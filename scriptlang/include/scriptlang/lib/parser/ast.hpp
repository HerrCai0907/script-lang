#ifndef SCRIPTLANG_LIB_PARSER_AST_HPP
#define SCRIPTLANG_LIB_PARSER_AST_HPP

#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/SMLoc.h"
#include <memory>
#include <string>

namespace scriptlang {

class AST;

class TopDecls;

class Stmt;
class AssignStmt;
class BlockStmt;
class BreakStmt;
class ContinueStmt;
class DeclStmt;
class ExprStmt;
class IfStmt;
class ImportStmt;
class ReturnStmt;
class WhileStmt;

class Expr;
class BinaryExpr;
class CallExpr;
class FuncExpr;
class Identifier;
class LiteralExpr;
class MemberExpr;
class PrefixExpr;

class Type;
class FunctionType;
class NamedType;

class ASTVisitor {
public:
  virtual void visit(AST &) {}

  virtual void visit(TopDecls &) = 0;

  virtual void visit(Stmt &) {}
  virtual void visit(AssignStmt &) = 0;
  virtual void visit(BlockStmt &) = 0;
  virtual void visit(BreakStmt &) = 0;
  virtual void visit(ContinueStmt &) = 0;
  virtual void visit(DeclStmt &) = 0;
  virtual void visit(ExprStmt &) = 0;
  virtual void visit(IfStmt &) = 0;
  virtual void visit(ImportStmt &) = 0;
  virtual void visit(ReturnStmt &) = 0;
  virtual void visit(WhileStmt &) = 0;

  virtual void visit(Expr &) {}
  virtual void visit(BinaryExpr &) = 0;
  virtual void visit(CallExpr &) = 0;
  virtual void visit(FuncExpr &) = 0;
  virtual void visit(Identifier &) = 0;
  virtual void visit(LiteralExpr &) = 0;
  virtual void visit(MemberExpr &) = 0;
  virtual void visit(PrefixExpr &) = 0;

  virtual void visit(Type &) {}
  virtual void visit(FunctionType &) = 0;
  virtual void visit(NamedType &) = 0;

  virtual ~ASTVisitor() = default;
};

class AST {
public:
  explicit AST(llvm::SMRange range) : range_(range) {}
  virtual ~AST() = default;
  virtual void accept(ASTVisitor &) = 0;

  llvm::SMRange range() { return range_; }

  void dump();

protected:
  llvm::SMRange range_;
};

class TopDecls : public AST {
public:
  using TopDeclsVec = llvm::SmallVector<std::shared_ptr<Stmt>, 32U>;
  TopDecls(llvm::SMRange range, TopDeclsVec &&decls) : AST(range), decls_(decls) {}
  void accept(ASTVisitor &V) override { V.visit(*this); }

  TopDeclsVec::iterator begin() { return decls_.begin(); }
  TopDeclsVec::iterator end() { return decls_.end(); }

protected:
  TopDeclsVec decls_;
};

class Stmt : public AST {
public:
  Stmt(llvm::SMRange range) : AST(range) {}
};

class AssignStmt : public Stmt {
public:
  AssignStmt(llvm::SMRange range, std::shared_ptr<Expr> lhs, std::shared_ptr<Expr> rhs)
      : Stmt(range), lhs_(std::move(lhs)), rhs_(std::move(rhs)) {}
  void accept(ASTVisitor &V) override { V.visit(*this); }

  std::shared_ptr<Expr> lhs() { return lhs_; }
  std::shared_ptr<Expr> rhs() { return rhs_; }

private:
  std::shared_ptr<Expr> lhs_;
  std::shared_ptr<Expr> rhs_;
};
class BlockStmt : public Stmt {
public:
  using StmtsVec = llvm::SmallVector<std::shared_ptr<Stmt>, 32U>;
  BlockStmt(llvm::SMRange range, StmtsVec stmts) : Stmt(range), stmts_(std::move(stmts)) {}
  void accept(ASTVisitor &V) override { V.visit(*this); }

  StmtsVec::iterator begin() { return stmts_.begin(); }
  StmtsVec::iterator end() { return stmts_.end(); }

private:
  StmtsVec stmts_;
};
class BreakStmt : public Stmt {
public:
  BreakStmt(llvm::SMRange range) : Stmt(range) {}
  void accept(ASTVisitor &V) override { V.visit(*this); }
};
class ContinueStmt : public Stmt {
public:
  ContinueStmt(llvm::SMRange range) : Stmt(range) {}
  void accept(ASTVisitor &V) override { V.visit(*this); }
};
class DeclStmt : public Stmt {
public:
  enum class Kind : uint8_t { Normal, Const, Export, ExportConst };
  DeclStmt(llvm::SMRange range, Kind kind, std::string name, std::shared_ptr<Type> type,
           std::shared_ptr<Expr> expr)
      : Stmt(range), kind_(kind), name_(name), type_(type), expr_(expr) {}
  void accept(ASTVisitor &V) override { V.visit(*this); }
  Kind kind() { return kind_; }
  llvm::StringRef name() { return name_; }
  std::shared_ptr<Type> type() { return type_; }
  std::shared_ptr<Expr> expr() { return expr_; }

private:
  Kind kind_;
  std::string name_;
  std::shared_ptr<Type> type_;
  std::shared_ptr<Expr> expr_;
};
class ExprStmt : public Stmt {
public:
  ExprStmt(llvm::SMRange range, std::shared_ptr<Expr> expr) : Stmt(range), expr_(expr) {}
  void accept(ASTVisitor &V) override { V.visit(*this); }
  std::shared_ptr<Expr> &expr() { return expr_; }

private:
  std::shared_ptr<Expr> expr_;
};
class IfStmt : public Stmt {
public:
  IfStmt(llvm::SMRange range, std::shared_ptr<Expr> condition, std::shared_ptr<BlockStmt> thenBlock,
         std::shared_ptr<Stmt> elseBlock)
      : Stmt(range), condition_(condition), thenBlock_(thenBlock), elseBlock_(elseBlock) {}
  void accept(ASTVisitor &V) override { V.visit(*this); }
  std::shared_ptr<Expr> condition() { return condition_; }
  std::shared_ptr<BlockStmt> thenBlock() { return thenBlock_; }
  std::shared_ptr<Stmt> elseBlock() { return elseBlock_; }

private:
  std::shared_ptr<Expr> condition_;
  std::shared_ptr<BlockStmt> thenBlock_;
  std::shared_ptr<Stmt> elseBlock_;
};
class ImportStmt : public Stmt {
public:
  ImportStmt(llvm::SMRange range, std::string name, std::string importPath)
      : Stmt(range), name_(name), importPath_(importPath) {}
  void accept(ASTVisitor &V) override { V.visit(*this); }

  llvm::StringRef name() const { return name_; }
  llvm::StringRef importPath() const { return importPath_; }

private:
  std::string name_;
  std::string importPath_;
};
class ReturnStmt : public Stmt {
public:
  ReturnStmt(llvm::SMRange range, std::shared_ptr<Expr> value) : Stmt(range), value_(value) {}
  void accept(ASTVisitor &V) override { V.visit(*this); }
  std::shared_ptr<Expr> value() { return value_; }

private:
  std::shared_ptr<Expr> value_;
};
class WhileStmt : public Stmt {
public:
  WhileStmt(llvm::SMRange range, std::shared_ptr<Expr> condition, std::shared_ptr<BlockStmt> block)
      : Stmt(range), condition_(condition), block_(block) {}
  void accept(ASTVisitor &V) override { V.visit(*this); }
  std::shared_ptr<Expr> condition() { return condition_; }
  std::shared_ptr<BlockStmt> block() { return block_; }

private:
  std::shared_ptr<Expr> condition_;
  std::shared_ptr<BlockStmt> block_;
};

class Expr : public AST {
public:
  Expr(llvm::SMRange range) : AST(range) {}
};

class Identifier : public Expr {
public:
  Identifier(llvm::SMRange range, std::string name) : Expr(range), name_(name) {}
  void accept(ASTVisitor &V) override { V.visit(*this); }

  std::string name() { return name_; }

private:
  std::string name_;
};
class LiteralExpr : public Expr {
public:
  enum class Kind : uint8_t {
    Integer,
    Hex,
    Float,
    String,
  };
  LiteralExpr(llvm::SMRange range, Kind kind, std::string value)
      : Expr(range), kind_(kind), value_(value) {}
  void accept(ASTVisitor &V) override { V.visit(*this); }

  std::string value() { return value_; }
  Kind kind() { return kind_; }

private:
  Kind kind_;
  std::string value_;
};
class PrefixExpr : public Expr {
public:
  enum class Op : uint8_t {
    Not,
    Plus,
    Minus,
  };
  PrefixExpr(llvm::SMRange range, Op op, std::shared_ptr<Expr> expr)
      : Expr(range), op_(op), expr_(expr) {}
  void accept(ASTVisitor &V) override { V.visit(*this); }

  Op op() { return op_; }
  std::shared_ptr<Expr> expr() { return expr_; }

private:
  Op op_;
  std::shared_ptr<Expr> expr_;
};
class BinaryExpr : public Expr {
public:
  enum class Op : uint8_t {
    ADD,
    SUB,
    MUL,
    DIV,
    MOD,
    LEFT_SHIFT,
    RIGHT_SHIFT,
    LESS_THAN,
    GREATER_THAN,
    NO_LESS_THAN,
    NO_GREATER_THAN,
    EQUAL,
    NOT_EQUAL,
    AND,
    OR,
    XOR,
    LOGIC_AND,
    LOGIC_OR,
  };
  BinaryExpr(Op op, std::shared_ptr<Expr> lhs, std::shared_ptr<Expr> rhs)
      : Expr(llvm::SMRange{lhs->range().Start, rhs->range().End}), op_(op), lhs_(lhs), rhs_(rhs) {}
  void accept(ASTVisitor &V) override { V.visit(*this); }

  Op op() { return op_; }
  std::shared_ptr<Expr> lhs() { return lhs_; }
  std::shared_ptr<Expr> rhs() { return rhs_; }

private:
  Op op_;
  std::shared_ptr<Expr> lhs_;
  std::shared_ptr<Expr> rhs_;

  friend void appendExprInRhs(BinaryExpr &binaryExpr, BinaryExpr::Op op,
                              std::shared_ptr<Expr> expr);
};
class CallExpr : public Expr {
public:
  using ArgumentsVec = llvm::SmallVector<std::shared_ptr<Expr>, 4U>;
  CallExpr(llvm::SMRange range, std::shared_ptr<Expr> caller, ArgumentsVec &&arguments)
      : Expr(range), caller_(caller), arguments_(arguments) {}
  void accept(ASTVisitor &V) override { V.visit(*this); }
  std::shared_ptr<Expr> caller() { return caller_; }
  ArgumentsVec const &arguments() { return arguments_; }

private:
  std::shared_ptr<Expr> caller_;
  ArgumentsVec arguments_;
};
class MemberExpr : public Expr {
public:
  MemberExpr(llvm::SMRange range, std::shared_ptr<Expr> accessor,
             std::shared_ptr<Identifier> member)
      : Expr(range), accessor_(accessor), member_(member) {}
  void accept(ASTVisitor &V) override { V.visit(*this); }
  std::shared_ptr<Expr> accessor() const { return accessor_; }
  std::shared_ptr<Identifier> member() const { return member_; }

private:
  std::shared_ptr<Expr> accessor_;
  std::shared_ptr<Identifier> member_;
};
class FuncExpr : public Expr {
public:
  using ArgumentsVec = llvm::SmallVector<std::shared_ptr<Identifier>, 4U>;
  using ArgumentTypesVec = llvm::SmallVector<std::shared_ptr<Type>, 4U>;
  FuncExpr(llvm::SMRange range, ArgumentsVec arguments, ArgumentTypesVec argumentTypes,
           std::shared_ptr<BlockStmt> stmt)
      : Expr(range), arguments_(arguments), argumentTypes_(argumentTypes), stmt_(stmt) {}
  void accept(ASTVisitor &V) override { V.visit(*this); }

  ArgumentsVec const &arguments() const { return arguments_; }
  ArgumentTypesVec const &argumentTypes() const { return argumentTypes_; }
  std::shared_ptr<BlockStmt> stmt() const { return stmt_; }

private:
  ArgumentsVec arguments_;
  ArgumentTypesVec argumentTypes_;
  std::shared_ptr<BlockStmt> stmt_;
};

class Type : public AST {
public:
  Type(llvm::SMRange range) : AST(range) {}
};

class NamedType : public Type {
public:
  using NamedTypeVec = llvm::SmallVector<std::string, 2U>;
  NamedType(llvm::SMRange range, NamedTypeVec &&name) : Type(range), names_(name) {}
  void accept(ASTVisitor &V) override { V.visit(*this); }

  NamedTypeVec const &names() const { return names_; }

private:
  NamedTypeVec names_;
};
class FunctionType : public Type {
public:
  using FunctionTypeParameterVec = llvm::SmallVector<std::shared_ptr<Type>, 4U>;
  FunctionType(llvm::SMRange range, FunctionTypeParameterVec &&parameters,
               std::shared_ptr<Type> returnType)
      : Type(range), parameters_(parameters), returnType_(returnType) {}
  void accept(ASTVisitor &V) override { V.visit(*this); }

  FunctionTypeParameterVec const &parameters() const { return parameters_; }
  std::shared_ptr<Type> returnType() const { return returnType_; }

private:
  FunctionTypeParameterVec parameters_;
  std::shared_ptr<Type> returnType_;
};

} // namespace scriptlang

#endif /* SCRIPTLANG_LIB_PARSER_AST_HPP */
