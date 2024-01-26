
// #include "visitor.h"
// #include "llvm/IR/Constants.h"

// void CodeGenVisitor::visit(BaseAST *base) {
//   outs() << "base";
// }

// void CodeGenVisitor::visit(BinaryExprAST *binaryExpr) {
//   binaryExpr->lhs->accept(*this);
//   Value* l = value;
//   binaryExpr->rhs->accept(*this);
//   Value* r = value;
//   if(!l || !r) {
//     value = nullptr;
//     return;
//   }
//   switch (binaryExpr->op) {
//   case Add:
//     value = builder->CreateAdd(l, r);
//     break;
//   case Sub:
//     value = builder->CreateSub(l, r);
//     break;
//   case Mul:
//     value = builder->CreateMul(l, r);
//     break;
//   case Div:
//     value = builder->CreateFDiv(l, r);
//     break;
//   case Lt:
//     value = builder->CreateICmpSLT(l, r);
//     break;
//   case Lte:
//     value = builder->CreateICmpSLE(l, r);
//     break;
//   case Gt:
//     value = builder->CreateICmpSGT(l, r);
//     break;
//   case Gte:
//     value = builder->CreateICmpSGE(l, r);
//     break;
//   case Mod:
//     value = builder->CreateSRem(l, r);
//     break;
//   case LOr:
//     value = builder->CreateOr(l, r);
//     break;
//   case LAnd:
//     value = builder->CreateAnd(l, r);
//     break;
//   case Eq:
//     value = builder->CreateICmpEQ(l, r);
//     break;
//   case Ne:
//     value = builder->CreateICmpNE(l, r);
//     break;
//   default:
//     value = nullptr;
//   }
// }

// void CodeGenVisitor::visit(BlockAST *block) {
//   // block->stmt->accept(*this);
// }

// void CodeGenVisitor::visit(CompUnitAST *compUnit) {
//   compUnit->func_def->accept(*this);
// }

// void CodeGenVisitor::visit(ExprAST *expr) {
//   outs() << "Expr";
// }

// void CodeGenVisitor::visit(FuncDefAST *funcDef) {
//   FunctionType *FT = FunctionType::get(
//     Type::getInt32Ty(getContext()), false);
//   auto& identifier = funcDef->ident;
//   Function *F = Function::Create(FT, Function::ExternalLinkage, identifier,
//   *theModule); if(F->getName() != identifier) {
//     F->eraseFromParent();
//     F = theModule->getFunction(identifier);
//   }

//   BasicBlock *BB = BasicBlock::Create(getContext(), "main", F);
//   builder->SetInsertPoint(BB);
//   funcDef->block->accept(*this);
//   verifyFunction(*F);
//   value = F;
// }

// void CodeGenVisitor::visit(FuncTypeAST *funcType) {
//   return;
// }

// void CodeGenVisitor::visit(NumberAST *number) {
//   int val = number->val;
//   value = ConstantInt::get(Type::getInt32Ty(getContext()), val);
// }

// void CodeGenVisitor::visit(StmtAST *stmt) {
//   stmt->expr->accept(*this);
//   Value* expr = value;
//   builder->CreateRet(expr);
// }

// void CodeGenVisitor::visit(UnaryExprAST* unaryExpr) {
//   unaryExpr->expr->accept(*this);
//   Value* subExpr = value;
//   switch (unaryExpr->op) {
//   case Add:
//     value = subExpr;
//     break;
//   case Sub:
//     value = builder->CreateNeg(subExpr);
//     break;
//   case Not:
//     value = builder->CreateNot(subExpr);
//     break;
//   default:
//     value = nullptr;
//   }
// }
