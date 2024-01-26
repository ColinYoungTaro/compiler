#include <cassert>
#include <cstdio>
#include <exception>
#include <iostream>
#include <llvm/Support/raw_ostream.h>
#include <memory>
#include <string>

#include "ast.h"
#include "context.h"
#include "visitor.h"
#include "llvm/IR/LLVMContext.h"

using namespace std;

extern FILE *yyin;
extern int yyparse(unique_ptr<BaseAST> &ast);

int main(int argc, const char *argv[]) {

  auto input = argv[1];
  // 打开输入文件, 并且指定 lexer 在解析的时候读取这个文件
  yyin = fopen(input, "r");
  assert(yyin);

  // 调用 parser 函数, parser 函数会进一步调用 lexer 解析输入文件的
  unique_ptr<BaseAST> ast;
  auto ret = yyparse(ast);
  assert(!ret);

  ctx = new CodeGenContext();
  auto& context = ctx->getContextRef();
  std::vector<Type*> printf_arg_types;
  printf_arg_types.push_back(Type::getInt32Ty(context)); //printf要求的参数类型
  FunctionType* printf_type = FunctionType::get(Type::getVoidTy(context), printf_arg_types, true);
  ctx->theModule->getOrInsertFunction("printInt", printf_type);

  // 插入对runtime中函数的声明
  FunctionType* scanf_type = FunctionType::get(Type::getInt32Ty(context), true);
  ctx->theModule->getOrInsertFunction("getInt", scanf_type);
  FunctionType* printLnTy = FunctionType::get(Type::getVoidTy(context), true);
  ctx->theModule->getOrInsertFunction("printLn", printLnTy);
  try {
    // ast->dump();
    ast->codeGen();
    ctx->theModule->print(outs(), nullptr);

  } catch (std::string e) {
    errs() << e << "\n";
  } 
  delete ctx;
  return 0;
}
