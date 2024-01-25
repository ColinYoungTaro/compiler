%code requires {
#include <memory>
#include <string>
#include "../ast.h"
}

%{

#include <iostream>
#include <memory>
#include <string>
#include <vector>
#include <deque>
#include "../ast.h"

int yydebug=1;

// 声明 lexer 函数和错误处理函数
int yylex();
void yyerror(std::unique_ptr<BaseAST> &ast, const char *s);
using namespace std;

template<typename T>
RawAstPtrList<T>* initRawPtrList() {
  return new RawAstPtrList<T>();
}

template<typename T>
void append_front(RawAstPtrList<T>* list, T* item) {
  list->push_front(item);
}

template<typename T, typename K>
void copyList(ElmList<T>& target, AstRawAstPtrList<K>* list) {
  for(K* ptr: list) {
    target->push_back(unique_ptr<T>(ptr));
  }
}

%}

// 定义 parser 函数和错误处理函数的附加参数
// 我们需要返回一个字符串作为 AST, 所以我们把附加参数定义成字符串的智能指针
// 解析完成后, 我们要手动修改这个参数, 把它设置成解析得到的字符串
%parse-param { 
  std::unique_ptr<BaseAST> &ast 
}

// yylval 的定义, 我们把它定义成了一个联合体 (union)
// 因为 token 的值有的是字符串指针, 有的是整数
// 之前我们在 lexer 中用到的 str_val 和 int_val 就是在这里被定义的
// 至于为什么要用字符串指针而不直接用 string 或者 unique_ptr<string>?
// 请自行 STFW 在 union 里写一个带析构函数的类会出现什么情况
%union {
  BaseAST* ast_val;
  RawAstPtrList<BaseAST> *item_list;
  DeclAST* decl_val;
  RawAstPtrList<DeclAST> *decl_list;
  StmtAST* stmt_val;
  ExprAST* expr_val;
  
  std::string *str_val;
  int int_val;
  double float_val;
  char char_val;
  AstOperator op;
  TypeIdent type_val;
  
}

// lexer 返回的所有 token 种类的声明
// 注意 IDENT 和 INT_CONST 会返回 token 的值, 分别对应 str_val 和 int_val
%token CONST INT VOID RETURN 
%token ADD SUB MUL DIV MOD NOT EQ LT GT DOT
%token L_OR L_AND

%token IF ELSE FOR WHILE CONTINUE BREAK 
%token <char_val> LPAREN RPAREN LBRACE RBRACE 
%token <str_val> LTE GTE NE 
%token <str_val> IDENT STRING
%token <int_val> INT_CONST

// 非终结符的类型定义

%type <type_val> BType FuncType
%type <ast_val> FuncDef Block CompElm
%type <decl_list> ConstDefList VarDefList ArgList
%type <decl_val> ConstDef VarDef Arg
%type <item_list> BlockItemList RArgList CompElmList CompUnit NonEmptyRArgList
%type <ast_val> BlockItem

%type <ast_val> Stmt OpenStmt CloseStmt SimpleStmt

%type <ast_val> Decl VarDecl ConstDecl  
%type <expr_val> Exp PrimaryExp Number ConstInitVal InitialVal LVal
%type <expr_val> UnaryExp MulExp AddExp RelExp EqExp LAndExp LOrExp ConstExp
%type <op>  CompOp UnaryOp EqOp AddOp MulOp

%%

// 开始符, CompUnit ::= FuncDef, 大括号后声明了解析完成后 parser 要做的事情
// 之前我们定义了 FuncDef 会返回一个 str_val, 也就是字符串指针
// 而 parser 一旦解析完 CompUnit, 就说明所有的 token 都被解析了, 即解析结束了
// 此时我们应该把 FuncDef 返回的结果收集起来, 作为 AST 传给调用 parser 的函数
// $1 指代规则里第一个符号的返回值, 也就是 FuncDef 的返回值
CompUnit
  : CompElmList {
    auto comp_unit = make_unique<CompUnitAST>();
    auto list = unique_ptr<RawAstPtrList<BaseAST>>($1);
    for(auto item: *list) {
      comp_unit->item_list.push_back(unique_ptr<BaseAST>(item));
    }
    ast = std::move(comp_unit);
  }

CompElmList
  : CompElm CompElmList {
    $$ = $2;
    $$->push_front($1);
  }
  | CompElm {
    $$ = new RawAstPtrList<BaseAST>();
    $$->push_front($1);
  }
  ;
CompElm
  : Decl {
    $$ = $1;
  }
  | FuncDef {
    $$ = $1;
  }
  ;

// FuncDef ::= FuncType IDENT '(' ')' Block;
// 我们这里可以直接写 '(' 和 ')', 因为之前在 lexer 里已经处理了单个字符的情况
// 解析完成后, 把这些符号的结果收集起来, 然后拼成一个新的字符串, 作为结果返回
// $$ 表示非终结符的返回值, 我们可以通过给这个符号赋值的方法来返回结果
// 你可能会问, FuncType, IDENT 之类的结果已经是字符串指针了
// 为什么还要用 unique_ptr 接住它们, 然后再解引用, 把它们拼成另一个字符串指针呢
// 因为所有的字符串指针都是我们 new 出来的, new 出来的内存一定要 delete
// 否则会发生内存泄漏, 而 unique_ptr 这种智能指针可以自动帮我们 delete
// 虽然此处你看不出用 unique_ptr 和手动 d`elete 的区别, 但当我们定义了 AST 之后
// 这种写法会省下很多内存管理的负担
FuncDef
  : BType IDENT '(' ')' Block {
    auto func_def = new FuncDefAST();
    func_def->ident = *unique_ptr<std::string>($2);
    func_def->block = unique_ptr<BaseAST>($5);
    func_def->func_type = $1;
    $$ = func_def;
  }
  | BType IDENT '(' ArgList ')' Block {
    auto func_def = new FuncDefAST();
    func_def->ident = *unique_ptr<std::string>($2);
    func_def->block = unique_ptr<BaseAST>($6);
    func_def->func_type = $1;
    auto arg_list = unique_ptr<RawAstPtrList<DeclAST>>($4);
    for(auto* arg: *arg_list) {
      func_def->arg_list.push_back(unique_ptr<BaseAST>(arg));
    }
    $$ = func_def;
  }
  ;

ArgList
  : Arg ',' ArgList {
    $$ = $3;
    $$->push_front($1);
  }
  | Arg {
    auto arg_list = new RawAstPtrList<DeclAST>();
    arg_list->push_front($1);
    $$ = arg_list;
  }

Arg
  : BType IDENT {
    $$ = new DeclAST();
    $$->ident = *unique_ptr<std::string>($2);
    $$->type = $1;
  }
  ;

/* FuncType
  : INT {
    $$ = Int;
  }
  | VOID {
    $$ = Void;
  }
  ; */

Block
  : '{' '}' {
    $$ = new BlockAST();
  }
  | '{' BlockItemList '}' {
    auto block = new BlockAST();
    auto items = unique_ptr<RawAstPtrList<BaseAST>>($2);
    for(auto* ptr: *items) {
      block->blockItems.push_back(unique_ptr<BaseAST>(ptr));
    }
    $$ = block;
  }
  ;

BlockItemList
  : BlockItem BlockItemList {
    $$ = $2;
    $$->push_front($1);
  }
  | BlockItem {
    $$ = new RawAstPtrList<BaseAST>();
    $$->push_front($1);
  }
  ;

BlockItem
  : Stmt {
    $$ = $1;
  }
  | Decl {
    $$ = $1;
  }
  ;

Decl 
  : ConstDecl {
    $$ = $1;
  }
  | VarDecl {
    $$ = $1;
  }
  ;

Stmt
  : OpenStmt {
    $$ = $1;
  }
  | CloseStmt {
    $$ = $1;
  }
  ;

OpenStmt
  : IF '(' Exp ')' Stmt {
    auto if_stmt = new IfStmtAST();
    if_stmt->expr = unique_ptr<ExprAST>($3);
    if_stmt->then_stmt = unique_ptr<BaseAST>($5);
    $$ = if_stmt;
  }
  | IF '(' Exp ')' CloseStmt ELSE OpenStmt {
    auto if_stmt = new IfStmtAST();
    if_stmt->expr = unique_ptr<ExprAST>($3);
    if_stmt->then_stmt = unique_ptr<BaseAST>($5);
    if_stmt->else_stmt = unique_ptr<BaseAST>($7);
    $$ = if_stmt;
  }
  ;

CloseStmt 
  : IF '(' Exp ')' CloseStmt ELSE CloseStmt {
    auto if_stmt = new IfStmtAST();
    if_stmt->expr = unique_ptr<ExprAST>($3);
    if_stmt->then_stmt = unique_ptr<BaseAST>($5);
    if_stmt->else_stmt = unique_ptr<BaseAST>($7);
    $$ = if_stmt;
  }
  | WHILE '(' Exp ')' CloseStmt {
    auto while_stmt = new WhileStmtAST();
    while_stmt->expr = unique_ptr<ExprAST>($3);
    while_stmt->loop_stmt = unique_ptr<BaseAST>($5);
    $$ = while_stmt;
  }
  | SimpleStmt {
    $$ = $1;
  }

SimpleStmt
  : IDENT '=' Exp ';' {
    auto assign = new AssignAST();
    assign->ident = *unique_ptr<std::string>($1);
    assign->expr = unique_ptr<ExprAST>($3);
    $$ = assign;
  }
  | RETURN Exp ';' {
    auto stmt = new RetStmtAST();
    stmt->ret_expr = unique_ptr<ExprAST>($2);
    $$ = stmt;
  }
  | RETURN ';' {
    $$ = new RetStmtAST();
  }
  | BREAK ';' {
    auto brk = new BreakStmtAST();
    $$ = brk;
  }
  | CONTINUE ';' {
    auto cont = new ContinueStmtAST();
    $$ = cont;
  }
  | Block {
    $$ = $1;
  }
  | Exp ';' {
    $$ = $1;
  }
  | ';' {
    $$ = new StmtAST();
  }
  ;

LVal
  : IDENT {
    auto l_val = new LValAST();
    l_val->ident = *unique_ptr<std::string>($1);
    $$ = l_val;
  }

VarDecl
  : BType VarDefList ';' {
    auto var_decls = new DeclStmtAST();
    auto def_list = unique_ptr<RawAstPtrList<DeclAST>>($2);
    for(auto* def: *def_list ) {
      def->type = $1;
      var_decls->decls.push_back(unique_ptr<BaseAST>(def));
    }
    $$ = var_decls;
  }
  ;
  
ConstDecl
  : CONST BType ConstDefList ';' {
    auto const_decls = new DeclStmtAST();
    auto def_list = unique_ptr<RawAstPtrList<DeclAST>>($3);
    for(auto* def: *def_list) {
      def->type = $2;
      const_decls->decls.push_back(unique_ptr<BaseAST>(def));
    }
    $$ = const_decls;
  }
  ;

BType
  : INT {
    $$ = Int;
  }
  | VOID {
    $$ = Void;
  }
  ;

ConstDefList
  : ConstDef ',' ConstDefList {
    $3->push_front($1);
    $$ = $3;
  }
  | ConstDef {
    auto *declStmt = new RawAstPtrList<DeclAST>();
    declStmt->push_front($1);
    $$ = declStmt;
  }
  ;

VarDefList
  : VarDef ',' VarDefList {
    $3->push_front($1);
    $$ = $3;
  }
  | VarDef {
    auto *declStmt = new RawAstPtrList<DeclAST>();
    declStmt->push_front($1);
    $$ = declStmt;
  }

ConstDef
  : IDENT '=' ConstInitVal {
    auto const_decl = new DeclAST();
    const_decl->expr = unique_ptr<ExprAST>($3);
    const_decl->isConst = true;
    const_decl->ident = *unique_ptr<std::string>($1);
    $$ = const_decl;
  }
  ;

VarDef
  : IDENT {
    auto val_decl = new DeclAST();
    NumberAST *number = new NumberAST();
    number->val = 0;
    val_decl->expr = unique_ptr<ExprAST>(number);
    val_decl->ident = *unique_ptr<std::string>($1);
    $$ = val_decl;
  }
  | IDENT '=' InitialVal {
    auto val_decl = new DeclAST();
    val_decl->expr = unique_ptr<ExprAST>($3);
    val_decl->ident = *unique_ptr<std::string>($1);
    $$ = val_decl;
  }
  | IDENT '[' ConstExp ']'
  ;

ConstInitVal
  : ConstExp {
    $$ = $1;
  }
  ;

InitialVal
  : Exp {
    $$ = $1;
  }
  ;

ConstExp
  : Exp {
    $$ = $1;
  }
  ;


Exp
  : LOrExp { $$ = $1; }

PrimaryExp 
  : '(' Exp ')' { $$ = $2; }
  | Number {
    $$ = $1;
  }
  | LVal {
    $$ = $1;
  }
  ;

Number
  : INT_CONST {
    auto number = new NumberAST();
    number->val = $1;
    $$ = number;
  }
  ;

UnaryExp
  : PrimaryExp {
    $$ = $1;
  }
  | IDENT '(' RArgList ')' {
    auto call_expr = new CallExprAST();
    call_expr->ident = *unique_ptr<std::string>($1);
    auto r_arg_list = unique_ptr<RawAstPtrList<BaseAST>>($3);

    for(auto r_arg: *r_arg_list) {
      call_expr->r_arg_list.push_back(unique_ptr<BaseAST>(r_arg));
    }
    $$ = call_expr;
  }
  | UnaryOp UnaryExp {
    auto u_expr = new UnaryExprAST();
    u_expr->op = $1;
    u_expr->expr = unique_ptr<ExprAST>($2);
    $$ = u_expr;
  }
  ;

RArgList
  : NonEmptyRArgList {
    $$ = $1;
  }
  | {
    $$ = new RawAstPtrList<BaseAST>();
  }

NonEmptyRArgList
  : Exp ',' RArgList {
    $$ = $3;
    $$->push_front($1);
  }
  | Exp {
    $$ = new RawAstPtrList<BaseAST>();
    $$->push_front($1);
  }


UnaryOp
  : ADD { $$ = Add; }
  | SUB { $$ = Sub; }
  | NOT { $$ = Not; }
  ;

CompOp
  : LT  { $$ = Lt;  }
  | LTE { $$ = Lte; }
  | GT  { $$ = Gt;  }
  | GTE { $$ = Gte; }
  ;

MulOp
  : MUL { $$ = Mul;}
  | DIV { $$ = Div;}
  | MOD { $$ = Mod;}
  ;

AddOp
  : ADD { $$ = Add; }
  | SUB { $$ = Sub; }
  ;

EqOp
  : EQ { $$ = Eq; }
  | NE { $$ = Ne; }
  ;
  
MulExp
  : UnaryExp {
    $$ = $1;
  }
  | MulExp MulOp UnaryExp {
    auto mul_expr = new BinaryExprAST();
    mul_expr->op = $2;
    mul_expr->lhs = unique_ptr<ExprAST>($1);
    mul_expr->rhs = unique_ptr<ExprAST>($3);
    $$ = mul_expr;
  }
  ;

AddExp
  : MulExp {
    $$ = $1;
  }
  | AddExp AddOp MulExp {
    auto add_expr = new BinaryExprAST();
    add_expr->op = $2;
    add_expr->lhs = unique_ptr<ExprAST>($1);
    add_expr->rhs = unique_ptr<ExprAST>($3);
    $$ = add_expr;
  }
  ; 

RelExp
  : AddExp { $$ = $1; }
  | RelExp CompOp AddExp {
    auto rel_expr = new BinaryExprAST();
    rel_expr->lhs = unique_ptr<ExprAST>($1);
    rel_expr->rhs = unique_ptr<ExprAST>($3);
    rel_expr->op = $2;
    $$ = rel_expr;
  }
  ;

EqExp
  : RelExp { $$ = $1; }
  | EqExp EqOp RelExp {
    auto eq_expr = new BinaryExprAST();
    eq_expr->lhs = unique_ptr<ExprAST>($1);
    eq_expr->rhs = unique_ptr<ExprAST>($3);
    eq_expr->op = $2;
    $$ = eq_expr;
  }
  ;

LAndExp
  : EqExp { $$ = $1; }
  | LAndExp L_AND EqExp {
    auto l_and_expr = new BinaryExprAST();
    l_and_expr->lhs = unique_ptr<ExprAST>($1);
    l_and_expr->rhs = unique_ptr<ExprAST>($3);
    l_and_expr->op = LAnd;
    $$ = l_and_expr;
  } 
  ;

LOrExp
  : LAndExp { $$ = $1; } 
  | LOrExp L_OR LAndExp {
    auto l_or_expr = new BinaryExprAST();
    l_or_expr->lhs = unique_ptr<ExprAST>($1);
    l_or_expr->rhs = unique_ptr<ExprAST>($3);
    l_or_expr->op = LOr;
    $$ = l_or_expr;
  }
  ;

%%

// 定义错误处理函数, 其中第二个参数是错误信息
// parser 如果发生错误 (例如输入的程序出现了语法错误), 就会调用这个函数
void yyerror(std::unique_ptr<BaseAST> &ast, const char *s) {
  cerr << "error: " << s << endl;
}
;