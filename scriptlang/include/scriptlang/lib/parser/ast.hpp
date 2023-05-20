#ifndef SCRIPTLANG_LIB_PARSER_AST_HPP
#define SCRIPTLANG_LIB_PARSER_AST_HPP

#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/SMLoc.h"
#include <memory>
#include <string>

namespace scriptlang::ast {

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

class TypeNode;
class FunctionTypeNode;
class NamedTypeNode;

class Visitor {
public:
  virtual void visit(AST &);

  virtual void visit(TopDecls &);

  virtual void visit(Stmt &);
  virtual void visit(AssignStmt &);
  virtual void visit(BlockStmt &);
  virtual void visit(BreakStmt &);
  virtual void visit(ContinueStmt &);
  virtual void visit(DeclStmt &);
  virtual void visit(ExprStmt &);
  virtual void visit(IfStmt &);
  virtual void visit(ImportStmt &);
  virtual void visit(ReturnStmt &);
  virtual void visit(WhileStmt &);

  virtual void visit(Expr &);
  virtual void visit(BinaryExpr &);
  virtual void visit(CallExpr &);
  virtual void visit(FuncExpr &);
  virtual void visit(Identifier &);
  virtual void visit(LiteralExpr &);
  virtual void visit(MemberExpr &);
  virtual void visit(PrefixExpr &);

  virtual void visit(TypeNode &);
  virtual void visit(FunctionTypeNode &);
  virtual void visit(NamedTypeNode &);

  virtual ~Visitor() = default;
};

class AST {
public:
  explicit AST(llvm::SMRange range) : range_(range) {}
  virtual ~AST() = default;
  virtual void accept(Visitor &) = 0;

  llvm::SMRange range() { return range_; }

  void dump();

protected:
  llvm::SMRange range_;
};

class TopDecls : public AST {
public:
  using TopDeclsVec = llvm::SmallVector<std::shared_ptr<Stmt>, 32U>;
  TopDecls(llvm::SMRange range, TopDeclsVec &&decls) : AST(range), decls_(decls) {}
  void accept(Visitor &V) override { V.visit(*this); }

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
  void accept(Visitor &V) override { V.visit(*this); }

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
  void accept(Visitor &V) override { V.visit(*this); }

  StmtsVec::iterator begin() { return stmts_.begin(); }
  StmtsVec::iterator end() { return stmts_.end(); }

private:
  StmtsVec stmts_;
};
class BreakStmt : public Stmt {
public:
  BreakStmt(llvm::SMRange range) : Stmt(range) {}
  void accept(Visitor &V) override { V.visit(*this); }
};
class ContinueStmt : public Stmt {
public:
  ContinueStmt(llvm::SMRange range) : Stmt(range) {}
  void accept(Visitor &V) override { V.visit(*this); }
};
class DeclStmt : public Stmt {
public:
  enum class Kind : uint8_t { Normal, Const, Export, ExportConst };
  DeclStmt(llvm::SMRange range, Kind kind, std::string name, std::shared_ptr<TypeNode> type,
           std::shared_ptr<Expr> expr)
      : Stmt(range), kind_(kind), name_(name), type_(type), expr_(expr) {}
  void accept(Visitor &V) override { V.visit(*this); }
  Kind kind() { return kind_; }
  llvm::StringRef name() { return name_; }
  std::shared_ptr<TypeNode> type() { return type_; }
  std::shared_ptr<Expr> expr() { return expr_; }

private:
  Kind kind_;
  std::string name_;
  std::shared_ptr<TypeNode> type_;
  std::shared_ptr<Expr> expr_;
};
class ExprStmt : public Stmt {
public:
  ExprStmt(llvm::SMRange range, std::shared_ptr<Expr> expr) : Stmt(range), expr_(expr) {}
  void accept(Visitor &V) override { V.visit(*this); }
  std::shared_ptr<Expr> &expr() { return expr_; }

private:
  std::shared_ptr<Expr> expr_;
};
class IfStmt : public Stmt {
public:
  IfStmt(llvm::SMRange range, std::shared_ptr<Expr> condition, std::shared_ptr<BlockStmt> thenBlock,
         std::shared_ptr<Stmt> elseBlock)
      : Stmt(range), condition_(condition), thenBlock_(thenBlock), elseBlock_(elseBlock) {}
  void accept(Visitor &V) override { V.visit(*this); }
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
  void accept(Visitor &V) override { V.visit(*this); }

  llvm::StringRef name() const { return name_; }
  llvm::StringRef importPath() const { return importPath_; }

private:
  std::string name_;
  std::string importPath_;
};
class ReturnStmt : public Stmt {
public:
  ReturnStmt(llvm::SMRange range, std::shared_ptr<Expr> value) : Stmt(range), value_(value) {}
  void accept(Visitor &V) override { V.visit(*this); }
  std::shared_ptr<Expr> value() { return value_; }

private:
  std::shared_ptr<Expr> value_;
};
class WhileStmt : public Stmt {
public:
  WhileStmt(llvm::SMRange range, std::shared_ptr<Expr> condition, std::shared_ptr<BlockStmt> block)
      : Stmt(range), condition_(condition), block_(block) {}
  void accept(Visitor &V) override { V.visit(*this); }
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
  void accept(Visitor &V) override { V.visit(*this); }

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
  void accept(Visitor &V) override { V.visit(*this); }

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
  void accept(Visitor &V) override { V.visit(*this); }

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
  void accept(Visitor &V) override { V.visit(*this); }

  Op op() { return op_; }
  std::shared_ptr<Expr> lhs() { return lhs_; }
  std::shared_ptr<Expr> rhs() { return rhs_; }

  void appendExprInRhs(BinaryExpr::Op op, std::shared_ptr<Expr> expr);

private:
  Op op_;
  std::shared_ptr<Expr> lhs_;
  std::shared_ptr<Expr> rhs_;
};
class CallExpr : public Expr {
public:
  using ArgumentsVec = llvm::SmallVector<std::shared_ptr<Expr>, 4U>;
  CallExpr(llvm::SMRange range, std::shared_ptr<Expr> caller, ArgumentsVec &&arguments)
      : Expr(range), caller_(caller), arguments_(arguments) {}
  void accept(Visitor &V) override { V.visit(*this); }
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
  void accept(Visitor &V) override { V.visit(*this); }
  std::shared_ptr<Expr> accessor() const { return accessor_; }
  std::shared_ptr<Identifier> member() const { return member_; }

private:
  std::shared_ptr<Expr> accessor_;
  std::shared_ptr<Identifier> member_;
};
class FuncExpr : public Expr {
public:
  using ArgumentsVec = llvm::SmallVector<std::shared_ptr<Identifier>, 4U>;
  using ArgumentTypesVec = llvm::SmallVector<std::shared_ptr<TypeNode>, 4U>;
  FuncExpr(llvm::SMRange range, ArgumentsVec arguments, ArgumentTypesVec argumentTypes,
           std::shared_ptr<BlockStmt> stmt)
      : Expr(range), arguments_(arguments), argumentTypes_(argumentTypes), stmt_(stmt) {}
  void accept(Visitor &V) override { V.visit(*this); }

  ArgumentsVec const &arguments() const { return arguments_; }
  ArgumentTypesVec const &argumentTypes() const { return argumentTypes_; }
  std::shared_ptr<BlockStmt> stmt() const { return stmt_; }

private:
  ArgumentsVec arguments_;
  ArgumentTypesVec argumentTypes_;
  std::shared_ptr<BlockStmt> stmt_;
};

class TypeNode : public AST {
public:
  TypeNode(llvm::SMRange range) : AST(range) {}
};

class NamedTypeNode : public TypeNode {
public:
  using NamedTypeVec = llvm::SmallVector<std::string, 2U>;
  NamedTypeNode(llvm::SMRange range, NamedTypeVec &&name) : TypeNode(range), names_(name) {}
  void accept(Visitor &V) override { V.visit(*this); }

  NamedTypeVec const &names() const { return names_; }

private:
  NamedTypeVec names_;
};
class FunctionTypeNode : public TypeNode {
public:
  using FunctionTypeParameterVec = llvm::SmallVector<std::shared_ptr<TypeNode>, 4U>;
  FunctionTypeNode(llvm::SMRange range, FunctionTypeParameterVec &&parameters,
                   std::shared_ptr<TypeNode> returnType)
      : TypeNode(range), parameters_(parameters), returnType_(returnType) {}
  void accept(Visitor &V) override { V.visit(*this); }

  FunctionTypeParameterVec const &parameters() const { return parameters_; }
  std::shared_ptr<TypeNode> returnType() const { return returnType_; }

private:
  FunctionTypeParameterVec parameters_;
  std::shared_ptr<TypeNode> returnType_;
};

} // namespace scriptlang::ast

#endif /* SCRIPTLANG_LIB_PARSER_AST_HPP */
