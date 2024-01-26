#pragma once
#include <deque>
#include <llvm/IR/Value.h>

#include <iostream>
#include <memory>
#include <ostream>
#include <vector>

// 所有 AST 的基类

class BaseAST;
class BlockItemAST;
class StmtAST;
class FuncTypeAST;
class FuncDefAST;
class NumberAST;
class CompUnitAST;
class ExprAST;
class BinaryExprAST;
class UnaryExprAST;
class BlockAST;

template <typename T> using ElmList = std::vector<std::unique_ptr<T>>;

template <typename T> using RawAstPtrList = std::deque<T *>;

using AstItemList = ElmList<BaseAST>;
using BlockItemList = ElmList<BlockItemAST>;

using namespace llvm;

class AstVisitor {
public:
  virtual void visit(BaseAST *) {}
  virtual void visit(ExprAST *) {}
  virtual void visit(BinaryExprAST *) {}
  virtual void visit(UnaryExprAST *) {}
  virtual void visit(StmtAST *) {}
  virtual void visit(BlockAST *) {}
  virtual void visit(CompUnitAST *) {}
  virtual void visit(FuncTypeAST *) {}
  virtual void visit(FuncDefAST *) {}
  virtual void visit(NumberAST *) {}
  virtual ~AstVisitor() = default;
};

enum AstOperator {
  Add,
  Sub,
  Mul,
  Div,
  Mod,
  Not,
  Eq,
  Ne,
  Lt,
  Lte,
  Gt,
  Gte,
  Dot,
  LAnd,
  LOr
};

enum TypeIdent { Int, Char, Float, Void };

const std::string getOpStr(AstOperator op);

class BaseAST {
public:
  virtual ~BaseAST() = default;

  virtual llvm::Value *codeGen() { return nullptr; }

  void dump() const { dump(std::cout); }

  virtual void dump(std::ostream &out) const { out << "UnImplemented"; }
  friend std::ostream &operator<<(std::ostream &os, const BaseAST &ast);
};

// CompUnit 是 BaseAST
class CompUnitAST : public BaseAST {
public:
  // 用智能指针管理对象
  AstItemList item_list;
  void dump(std::ostream &out) const override {
    out << "CompUnit: { ";
    for (auto &def : item_list) {
      out << *def << " }";
    }
  }

  Value *codeGen() override;
};

// FuncDef 也是 BaseAST
class FuncDefAST : public BaseAST {
public:
  TypeIdent func_type;
  std::string ident;
  std::unique_ptr<BaseAST> block;
  AstItemList arg_list;

  void dump(std::ostream &out) const override {
    out << "FuncDef: { <" << func_type << ", " << ident << "> " << *block
        << " }";
  }

  Value *codeGen() override;
};

class FuncTypeAST : public BaseAST {
public:
  std::string type;

  void dump(std::ostream &out) const override { out << type; }

  Value *codeGen() override;
};

class BlockAST : public BaseAST {
public:
  AstItemList blockItems;

  void dump(std::ostream &out) const override {
    out << "Block: { ";
    out << "}";
  }
  Value *codeGen() override;
};

class StmtAST : public BaseAST {};

class RetStmtAST : public StmtAST {
public:
  std::unique_ptr<ExprAST> ret_expr;
  void dump(std::ostream &out) const override {
    // out << "Stmt: { " << *expr << " }";
  }

  Value *codeGen() override;
};

class DeclAST : public BaseAST {
public:
  TypeIdent type;
  std::string ident;
  bool isConst = false;
  std::unique_ptr<ExprAST> expr;

  llvm::Value *codeGen() override;
};

class DeclStmtAST : public StmtAST {
public:
  ElmList<BaseAST> decls;

  Value *codeGen() override;
};

class ExprAST : public StmtAST {};

class NumberAST : public ExprAST {
public:
  int val;
  void dump(std::ostream &out) const override { out << " " << val << " "; }
  Value *codeGen() override;
};

class VarRefAST : public ExprAST {
public:
  Value *getSymbolValue();
  std::string ident;
};

class LValAST : public VarRefAST {
public:
  bool is_left = false;
  void dump(std::ostream &out) const override { out << " " << ident << " "; }
  Value *codeGen() override;
};

class BinaryExprAST : public ExprAST {
public:
  AstOperator op;
  std::unique_ptr<ExprAST> lhs, rhs;

  void dump(std::ostream &out) const override {
    out << *lhs << " " << getOpStr(op) << " " << *rhs;
  }

  Value *codeGen() override;
};

class UnaryExprAST : public ExprAST {
public:
  AstOperator op;
  std::unique_ptr<ExprAST> expr;

  void dump(std::ostream &out) const override {
    out << getOpStr(op) << " " << *expr;
  }
  Value *codeGen() override;
};

class AssignAST : public StmtAST {

public:
  std::unique_ptr<LValAST> left_val;
  std::unique_ptr<ExprAST> right_val;

  llvm::Value *codeGen() override;
};

class IfStmtAST : public StmtAST {
public:
  std::unique_ptr<ExprAST> expr;
  std::unique_ptr<BaseAST> then_stmt, else_stmt;

  llvm::Value *codeGen() override;
};

class WhileStmtAST : public StmtAST {
public:
  std::unique_ptr<ExprAST> expr;
  std::unique_ptr<BaseAST> loop_stmt;

  llvm::Value *codeGen() override;
};

class ForStmtAST : public StmtAST {
public:
  std::unique_ptr<BaseAST> init_stmt, cmp_stmt, inc_stmt, loop_stmt;

  llvm::Value *codeGen();
};

class BreakStmtAST : public StmtAST {
  llvm::Value *codeGen() override;
  void dump(std::ostream &out) const override { out << "break;\n"; }
};

class ContinueStmtAST : public StmtAST {
  llvm::Value *codeGen() override;
  void dump(std::ostream &out) const override { out << "continue;\n"; }
};

class CallExprAST : public ExprAST {
public:
  AstItemList r_arg_list;
  std::string ident;
  llvm::Value *codeGen() override;
};

// class ArgAST: public ExprAST {
// public:
//   std::string ident;
//   TypeIdent type;
// };

class ArrayDeclAST : public DeclAST {
public:
  std::unique_ptr<ExprAST> arr_size;
  ElmList<ExprAST> initializer;
  llvm::Value *codeGen() override;
};
// class ArrayInitAST : public ExprAST {
// public:

//   llvm::Value * codeGen() override;
// };

class ArrayDerefAST : public LValAST {
public:
  std::unique_ptr<ExprAST> offset;
  Value *codeGen() override;
};
