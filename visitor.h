#pragma once

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/raw_ostream.h"

#include <memory>

#include "ast.h"


using namespace llvm;

class CodeGenVisitor : public AstVisitor {
private:
  llvm::Value *value;
  std::unique_ptr<Module> theModule;
  std::unique_ptr<IRBuilder<>> builder;

public:
  CodeGenVisitor(LLVMContext &context) {
    theModule = std::make_unique<Module>("main", context);
    builder = std::make_unique<IRBuilder<>>(context);
  }

  LLVMContext &getContext() const { return theModule->getContext(); }
  Module& getModule() const {return *theModule;}

  void visit(BaseAST *) override;
  void visit(BinaryExprAST *) override;
  void visit(UnaryExprAST *) override;
  void visit(BlockAST *) override;
  void visit(CompUnitAST *) override;
  void visit(ExprAST *) override;
  void visit(FuncDefAST *) override;
  void visit(FuncTypeAST *) override;
  void visit(StmtAST *) override;
  void visit(NumberAST *) override;
}; 