#include "ast.h"

#include "context.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/Instruction.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/raw_ostream.h"
#include <iostream>
#include <llvm/IR/IRBuilder.h>
#include <memory>
#include <string>
#include <vector>

using namespace llvm;

static const char *opStr[] = {"+", "-",  "*", "/",  "%", "!",  "==", "!=",
                              "<", "<=", ">", ">=", ".", "&&", "||"};

std::ostream &operator<<(std::ostream &os, const BaseAST &ast) {
  ast.dump(os);
  return os;
}

llvm::Type *getLLVMType(LLVMContext &ctx, TypeIdent ty) {
  switch (ty) {
  case Int:
    return Type::getInt32Ty(ctx);
  case Float:
    return Type::getFloatTy(ctx);
  case Char:
    return Type::getInt8Ty(ctx);
  case Void:
    return Type::getVoidTy(ctx);
  default:
    return nullptr;
  }
}

const std::string getOpStr(AstOperator op) {
  int numSymbols = sizeof(opStr) / sizeof(char *);
  if (op >= 0 && op <= numSymbols) {
    return opStr[op];
  }
  return "";
}

Value *CompUnitAST::codeGen() {
  for (auto &func : item_list) {
    func->codeGen();
  }
  return nullptr;
}

Value *FuncDefAST::codeGen() {
  std::vector<Type *> v;
  for (auto &arg : this->arg_list) {
    DeclAST *decl = (DeclAST *)arg.get();
    v.push_back(getLLVMType(ctx->getContextRef(), decl->type));
  }

  auto *ty = getLLVMType(ctx->getContextRef(), func_type);
  FunctionType *FT = FunctionType::get(ty, v, false);
  auto &identifier = this->ident;
  Function *F = Function::Create(FT, Function::ExternalLinkage, identifier,
                                 *ctx->theModule);

  if (F->getName() != identifier) {
    logError("Duplicated function Name: " + identifier);
    F->eraseFromParent();
    F = ctx->theModule->getFunction(identifier);
  }
  verifyFunction(*F);
  ctx->enterNewScope();

  BasicBlock *BB =
      BasicBlock::Create(ctx->getContextRef(), identifier + ".entry", F);
  ctx->builder->SetInsertPoint(BB);
  int index = 0;
  for (auto &arg : this->arg_list) {
    DeclAST *decl = (DeclAST *)arg.get();
    F->getArg(index)->setName(decl->ident);
    Value *var = decl->codeGen();
    ctx->setSymbol(decl->ident, var);
    ctx->builder->CreateStore(F->getArg(index++), var);
  }
  this->block->codeGen();

  auto *bb = ctx->builder->GetInsertBlock();
  auto &inst = bb->getInstList().back();
  if (!isa<ReturnInst>(inst) && !isa<BranchInst>(inst)) {
    if (ty->getTypeID() != Type::VoidTyID) {
      auto *defaultRet = ConstantInt::get(ty, 0);
      ctx->builder->CreateRet(defaultRet);
    } else {
      ctx->builder->CreateRet(nullptr);
    }
  }
  ctx->exitCurrentScope();
  return F;
}

Value *FuncTypeAST::codeGen() { return BaseAST::codeGen(); }

Value *BlockAST::codeGen() {
  ctx->enterNewScope();
  for (auto &&item : blockItems) {
    item->codeGen();
  }
  ctx->exitCurrentScope();
  return nullptr;
}

Value *RetStmtAST::codeGen() {
  auto *currentBB = ctx->builder->GetInsertBlock();
  auto *parent = currentBB->getParent();
  auto *newBB = BasicBlock::Create(ctx->getContextRef(), "ret", parent);
  Value *retVal;
  if (this->ret_expr) {
    auto *retExpr = this->ret_expr->codeGen();
    retVal = ctx->builder->CreateRet(retExpr);
  } else {
    retVal = ctx->builder->CreateRet(nullptr);
  }
  ctx->builder->SetInsertPoint(newBB);
  return retVal;
}

Value *NumberAST::codeGen() {
  int val = this->val;
  return ConstantInt::get(Type::getInt32Ty(ctx->getContextRef()), val);
}

Value *BinaryExprAST::codeGen() {
  if (!ctx->isInGlobalScope()) {
    if (op == LOr) {
      Type *cmpType = Type::getInt1Ty(ctx->getContextRef());

      Value *result = ctx->builder->CreateAlloca(cmpType);
      ctx->builder->CreateStore(ConstantInt::get(cmpType, 1), result);
      Value *lor = lhs->codeGen();
      if (lor->getType() != cmpType) {
        lor =
            ctx->builder->CreateCast(Instruction::CastOps::SExt, lor, cmpType);
      }
      // lor != 0;
      lor =
          ctx->builder->CreateICmpNE(lor, ConstantInt::get(cmpType, 0), "lor");
      auto *parent = ctx->builder->GetInsertBlock()->getParent();
      auto *right = BasicBlock::Create(ctx->getContextRef(), "lor", parent);
      auto *merge = BasicBlock::Create(ctx->getContextRef(), "ror", parent);
      // if l == 1 then goto true branch
      ctx->builder->CreateCondBr(lor, merge, right);

      ctx->builder->SetInsertPoint(right);
      Value *ror = rhs->codeGen();
      if (ror->getType() != cmpType) {
        ror =
            ctx->builder->CreateCast(Instruction::CastOps::SExt, ror, cmpType);
      }
      ror =
          ctx->builder->CreateICmpNE(ror, ConstantInt::get(cmpType, 0), "ror");

      ctx->builder->CreateStore(ror, result);
      ctx->builder->CreateBr(merge);

      ctx->builder->SetInsertPoint(merge);
      result = ctx->builder->CreateLoad(cmpType, result);
      return result;
    } else if (op == LAnd) {
      Type *cmpType = Type::getInt1Ty(ctx->getContextRef());

      Value *result = ctx->builder->CreateAlloca(cmpType);
      ctx->builder->CreateStore(ConstantInt::get(cmpType, 0), result);
      Value *land = lhs->codeGen();
      if (land->getType() != cmpType) {
        land =
            ctx->builder->CreateCast(Instruction::CastOps::SExt, land, cmpType);
      }
      // lor != 0;
      land = ctx->builder->CreateICmpNE(land, ConstantInt::get(cmpType, 0),
                                        "land");
      auto *parent = ctx->builder->GetInsertBlock()->getParent();
      auto *right = BasicBlock::Create(ctx->getContextRef(), "land", parent);
      auto *merge = BasicBlock::Create(ctx->getContextRef(), "rand", parent);
      // if l == 1 then goto true branch
      ctx->builder->CreateCondBr(land, right, merge);

      ctx->builder->SetInsertPoint(right);
      Value *rand = rhs->codeGen();
      if (rand->getType() != cmpType) {
        rand =
            ctx->builder->CreateCast(Instruction::CastOps::SExt, rand, cmpType);
      }
      rand =
          ctx->builder->CreateICmpNE(rand, ConstantInt::get(cmpType, 0), "ror");

      ctx->builder->CreateStore(rand, result);
      ctx->builder->CreateBr(merge);

      ctx->builder->SetInsertPoint(merge);
      result = ctx->builder->CreateLoad(cmpType, result);
      return result;
    }
  }
  Value *l = lhs->codeGen();
  Value *r = rhs->codeGen();
  if (!l || !r) {
    return nullptr;
  }
  // errs() << "Visit bin expr" << "\n";
  Value* value;
  switch (op) {
  case Add:
    value = ctx->builder->CreateAdd(l, r);
    break;
  case Sub:
    value = ctx->builder->CreateSub(l, r);
    break;
  case Mul:
    value = ctx->builder->CreateMul(l, r);
    break;
  case Div:
    value = ctx->builder->CreateSDiv(l, r);
    break;
  case Lt:
    value = ctx->builder->CreateICmpSLT(l, r);
    break;
  case Lte:
    value = ctx->builder->CreateICmpSLE(l, r);
    break;
  case Gt:
    value = ctx->builder->CreateICmpSGT(l, r);
    break;
  case Gte:
    value = ctx->builder->CreateICmpSGE(l, r);
    break;
  case Mod:
    value = ctx->builder->CreateSRem(l, r);
    break;
  case Eq:
    value = ctx->builder->CreateICmpEQ(l, r);
    break;
  case Ne:
    value = ctx->builder->CreateICmpNE(l, r);
    break;
  case LAnd:
    value = ctx->builder->CreateAnd(l, r);
    break;
  case LOr:
    value = ctx->builder->CreateOr(l, r);
    break;
  default:
    logError("Unimplemented.");
    return nullptr;
  }
  if(ctx->isInGlobalScope()) {
    if(!isa<ConstantInt>(value)) {
      logError("Global initializer must be constant expr");
    }
  }
  return value;
}

Value *UnaryExprAST::codeGen() {
  Value *subExpr = expr->codeGen();
  switch (op) {
  case Add:
    return subExpr;
  case Sub:
    return ctx->builder->CreateNeg(subExpr);
  case Not:
    return ctx->builder->CreateNot(subExpr);
  default:
    return nullptr;
  }
}

Value *DeclStmtAST::codeGen() {
  for (auto &&decl : decls) {
    decl->codeGen();
  }
  return nullptr;
}

Value *DeclAST::codeGen() {
  const std::string &identifier = ident;
  if (ctx->getCurrentScopeSymbol(identifier) != nullptr) {
    logError("multiple definition, value named: " + identifier);
    return nullptr;
  }

  llvm::Type *allocTy = getLLVMType(ctx->getContextRef(), type);
  if (ctx->isInGlobalScope()) {
    ctx->theModule->getOrInsertGlobal(ident,
                                      Type::getInt32Ty(ctx->getContextRef()));
    GlobalVariable *globalVariable = ctx->theModule->getNamedGlobal(ident);
    globalVariable->setLinkage(GlobalValue::ExternalLinkage);
    globalVariable->setDSOLocal(true);
    if (this->expr) {
      auto *constExp = this->expr->codeGen();
      if (isa<Constant>(constExp)) {
        globalVariable->setInitializer(dyn_cast<Constant>(constExp));
      } else {
        logError("initializer element is not a compile-time constant");
      }
    } else {
      globalVariable->setInitializer(ConstantInt::get(allocTy, 0));
    }

    ctx->setSymbol(ident, globalVariable);
    return globalVariable;
  }

  Value *allocaInst = ctx->builder->CreateAlloca(allocTy);
  ctx->setSymbol(identifier, allocaInst, this->isConst);
  if (this->expr) {
    Value *llvmExpr = this->expr->codeGen();
    ctx->builder->CreateStore(llvmExpr, allocaInst);
  }
  return allocaInst;
}

llvm::Value *AssignAST::codeGen() {
  std::string varName = this->ident;
  auto *var = ctx->getSymbol(varName);

  if (!var) {
    logError("value named: " + varName + " not declared in this scope.");
    return nullptr;
  }
  if (var->isConst) {
    logError("Invalid assignment: can't assign an expr to a const.");
  }
  Value *e = this->expr->codeGen();
  if (e->getType()->getTypeID() == Type::VoidTyID) {
    logError("Cannot assign a void return value");
  }
  return ctx->builder->CreateStore(e, var->value);
}

Value *LValAST::codeGen() {
  Symbol *symbol = ctx->getSymbol(ident);
  if (!symbol) {
    logError("Undefined variable: " + ident + ".");
    return nullptr;
  }
	if(ctx->isInGlobalScope()) {
		return symbol->value;
	}
  Value *loadInst = ctx->builder->CreateLoad(symbol->value);
  return loadInst;
}

llvm::Value *IfStmtAST::codeGen() {
  Value *condition = this->expr->codeGen();

  if (condition->getType() != Type::getInt1Ty(ctx->getContextRef())) {
    condition = ctx->builder->CreateICmpNE(
        condition, ConstantInt::get(condition->getType(), 0), "if-cond");
  }

  Function *parentFunction = ctx->builder->GetInsertBlock()->getParent();
  auto *thenBB =
      BasicBlock::Create(ctx->getContextRef(), "if.then", parentFunction);
  auto *elseBB =
      BasicBlock::Create(ctx->getContextRef(), "if.else", parentFunction);
  auto *mergeBB =
      BasicBlock::Create(ctx->getContextRef(), "if.merge", parentFunction);
  ctx->builder->CreateCondBr(condition, thenBB, elseBB);

  ctx->builder->SetInsertPoint(thenBB);
  this->then_stmt->codeGen();
  ctx->builder->CreateBr(mergeBB);

  ctx->builder->SetInsertPoint(elseBB);
  if (this->else_stmt)
    this->else_stmt->codeGen();
  ctx->builder->CreateBr(mergeBB);

  ctx->builder->SetInsertPoint(mergeBB);
  return nullptr;
}

llvm::Value *WhileStmtAST::codeGen() {

  Function *parentFunction = ctx->builder->GetInsertBlock()->getParent();
  auto *header =
      BasicBlock::Create(ctx->getContextRef(), "loop.header", parentFunction);
  auto *body =
      BasicBlock::Create(ctx->getContextRef(), "loop.body", parentFunction);
  auto *exit =
      BasicBlock::Create(ctx->getContextRef(), "loop.exit", parentFunction);

  ctx->builder->CreateBr(header);
  ctx->builder->SetInsertPoint(header);

  ctx->builder->SetInsertPoint(header);
  Value *condition = this->expr->codeGen();
  if (condition->getType() != Type::getInt1Ty(ctx->getContextRef())) {
    condition = ctx->builder->CreateICmpNE(
        condition, ConstantInt::get(condition->getType(), 0), "loop-cond");
  }
  ctx->builder->CreateCondBr(condition, body, exit);
  ctx->builder->SetInsertPoint(body);

  ctx->enterLoop(header, exit);
  this->loop_stmt->codeGen();
  ctx->exitLoop();
  ctx->builder->CreateBr(header);

  ctx->builder->SetInsertPoint(exit);
  return nullptr;
}

llvm::Value *BreakStmtAST::codeGen() {
  if (auto frame = ctx->getCurrentLoop()) {
    auto *exit = frame->second;
    auto *currentBB = ctx->builder->GetInsertBlock();
    auto *func = currentBB->getParent();
    ctx->builder->CreateBr(exit);
    auto *newBB = BasicBlock::Create(ctx->getContextRef(), "loop.body", func);
    ctx->builder->SetInsertPoint(newBB);
  } else
    logError("Break is not in loop.");
  return nullptr;
}

llvm::Value *ContinueStmtAST::codeGen() {
  if (auto frame = ctx->getCurrentLoop()) {
    auto *header = frame->first;
    auto *currentBB = ctx->builder->GetInsertBlock();
    auto *func = currentBB->getParent();
    ctx->builder->CreateBr(header);
    auto *newBB = BasicBlock::Create(ctx->getContextRef(), "loop.body", func);
    ctx->builder->SetInsertPoint(newBB);
  } else
    logError("Continue is not in loop.");
  return nullptr;
}

llvm::Value *ForStmtAST::codeGen() { return BaseAST::codeGen(); }
llvm::Value *CallExprAST::codeGen() {
  std::vector<Value *> rargs;
  for (auto &expr : this->r_arg_list) {
    rargs.push_back(expr->codeGen());
  }
  std::string ident = this->ident;
  auto func = ctx->theModule->getFunction(ident);
  if (!func) {
    logWarning("No Such function: " + ident);
  }
  auto call_expr = ctx->builder->CreateCall(func, rargs);
  return call_expr;
}