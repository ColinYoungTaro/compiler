#ifndef __CONTEXT_H__
#define __CONTEXT_H__

#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"

#include <initializer_list>
#include <map>
#include <memory>
#include <stack>
#include <string>
#include <vector>

using namespace llvm;

enum LogIdent { INFO, WARNING, ERROR };

void logError(const std::string &logInfo);
void logWarning(const std::string &logInfo);

class CodeGenBlock {
public:
  BasicBlock *block;
  std::map<std::string, Value *> locals;
};

class Symbol {
public:
  llvm::Value *value;
  bool isConst;

  Symbol(Value *value, bool isConst) {
    this->value = value;
    this->isConst = isConst;
  }
  Symbol(Value *value) : Symbol(value, false) {}
};

using SymbolTable = std::map<std::string, Symbol>;
using SymbolTableStack = std::vector<SymbolTable>;
using LoopFrame = std::pair<BasicBlock *, BasicBlock *>;

class CodeGenContext {
public:
  std::unique_ptr<LLVMContext> ctx;
  std::unique_ptr<IRBuilder<>> builder;
  std::unique_ptr<Module> theModule;
  std::stack<LoopFrame> loopStack;
  Function *currentFunction = nullptr;

  Value *currentFunctionRetVal;
  SymbolTableStack symbolTableStack;

  CodeGenContext() {
    ctx = std::make_unique<LLVMContext>();
    theModule = std::make_unique<Module>("main", getContextRef());
    builder = std::make_unique<IRBuilder<>>(getContextRef());
    enterNewScope();
  }

  void enterNewScope() { symbolTableStack.push_back(SymbolTable()); }

  void exitCurrentScope() { symbolTableStack.pop_back(); }

  LLVMContext *getContext() { return ctx.get(); }

  LLVMContext &getContextRef() { return *ctx; }

  Symbol *getSymbol(const std::string &name) {
    for (auto it = symbolTableStack.rbegin(); it != symbolTableStack.rend();
         it++) {
      auto &symbolTable = *it;
      if (symbolTable.count(name)) {
        return &symbolTable.at(name);
      }
    }
    return nullptr;
  }

  void setSymbol(const std::string &name, Value *val, bool isConst = false) {
    if (Symbol *symbol = getSymbol(name)) {
      symbol->value = val;
      symbol->isConst = isConst;
    } else {
      symbolTableStack.back().emplace(name, Symbol(val, isConst));
    }
  }

  void createSymbol(const std::string &name, Value *val, bool isConst = false) {
    if (!getCurrentScopeSymbol(name)) {
      symbolTableStack.back().emplace(name, Symbol(val, isConst));
    } else {
      errs() << "duplicate\n";
    }
  }

  Symbol *getCurrentScopeSymbol(const std::string &name) {
    if (symbolTableStack.back().count(name)) {
      return &symbolTableStack.back().at(name);
    } else {
      return nullptr;
    }
  }

  void enterLoop(BasicBlock *header, BasicBlock *exit) {
    loopStack.emplace(header, exit);
  }

  const LoopFrame *getCurrentLoop() {
    if (loopStack.empty()) {
      return nullptr;
    }
    return &loopStack.top();
  }

  void exitLoop() {
    if (!loopStack.empty()) {
      loopStack.pop();
    } else {
      errs() << "Invalid exit from loop";
    }
  }

  bool isInGlobalScope() { return this->symbolTableStack.size() == 1; }

  void dumpStackInfo() {
    errs() << "STAK INFO:\n";
    for (auto &frame : symbolTableStack) {
      for (auto &[s, v] : frame) {
        errs() << s << " ";
        v.value->print(errs());
        errs() << "\n";
      }
      errs() << "-----------\n";
    }
    errs() << "\n\n";
  }
};
extern CodeGenContext *ctx;
#endif