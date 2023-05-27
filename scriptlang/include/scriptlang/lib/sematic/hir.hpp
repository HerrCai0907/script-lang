#ifndef SCRIPTLANG_LIB_SEMATIC_HIR_HPP
#define SCRIPTLANG_LIB_SEMATIC_HIR_HPP

#include "scriptlang/lib/parser/ast.hpp"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include <cassert>
#include <memory>
#include <utility>

namespace scriptlang::hir {

class HIR;
class Type;
class NamedType;
class FuncType;

class Decl;

class Value;
class IntegerLiteral;
class FloatLiteral;
class Variant;
class PrefixResult;
class BinaryResult;
class CallResult;

class Statement;
class AssignStatement;
class LoopStatement;
class JumpStatement;
class ReturnStatement;
class BlockStatement;
class BranchStatement;

class Visitor {
public:
  virtual void visit(HIR &type) {}

  virtual void visit(Type &type) {}
  virtual void visit(NamedType &type) {}
  virtual void visit(FuncType &type);

  virtual void visit(Decl &decl);

  virtual void visit(Value &value);
  virtual void visit(IntegerLiteral &value);
  virtual void visit(FloatLiteral &value);
  virtual void visit(Variant &value);
  virtual void visit(PrefixResult &value);
  virtual void visit(BinaryResult &value);
  virtual void visit(CallResult &value);

  virtual void visit(Statement &stmt);
  virtual void visit(AssignStatement &stmt);
  virtual void visit(LoopStatement &stmt);
  virtual void visit(JumpStatement &stmt);
  virtual void visit(ReturnStatement &stmt);
  virtual void visit(BlockStatement &stmt);
  virtual void visit(BranchStatement &stmt);
  virtual ~Visitor() = default;
};

class HIR {
public:
  void dump();
  virtual ~HIR() = default;
  virtual void accept(Visitor &V) = 0;
};

class Type : public HIR {
public:
  Type() {}
  virtual ~Type() = default;
  void accept(Visitor &V) override { V.visit(*this); }

  bool operator==(Type const &type) const { return equal(type); }
  bool operator!=(Type const &type) const { return !(*this == type); }

private:
  virtual bool equal(Type const &type) const = 0;
};
class NamedType : public Type {
public:
  explicit NamedType(llvm::SmallString<16U> name) : name_(name) {}
  void accept(Visitor &V) override { V.visit(*this); }

  llvm::SmallString<16U> name() { return name_; }

private:
  llvm::SmallString<16U> name_;
  bool equal(Type const &type) const override;
};
class FuncType : public Type {
public:
  using ArgumentTypes = llvm::SmallVector<std::shared_ptr<Type>, 4U>;
  FuncType(ArgumentTypes argumentTypes, std::shared_ptr<Type> returnType)
      : Type(), argumentTypes_(argumentTypes), returnType_(returnType) {}
  void accept(Visitor &V) override { V.visit(*this); }

  ArgumentTypes const &argumentTypes() const { return argumentTypes_; }
  std::shared_ptr<Type> returnType() const { return returnType_; }

private:
  ArgumentTypes argumentTypes_;
  std::shared_ptr<Type> returnType_;

  bool equal(Type const &type) const override;
};

class Decl : public HIR {
public:
  Decl(llvm::StringRef name, std::shared_ptr<Type> type) : name_(name), type_(std::move(type)) {}
  void accept(Visitor &V) override { V.visit(*this); }

  bool isConst() const { return isConst_; }
  void setConst(bool isConst) { isConst_ = isConst; }
  llvm::StringRef name() const { return name_; }
  std::shared_ptr<Type> type() const { return type_; }

private:
  llvm::SmallString<16U> name_;
  std::shared_ptr<Type> type_;

  bool isConst_;
};

class Value : public HIR {
public:
  explicit Value(std::shared_ptr<Type> type) : type_(type) {}
  virtual ~Value() = default;
  void accept(Visitor &V) override { V.visit(*this); }

  std::shared_ptr<Type> type() { return type_; }

protected:
  std::shared_ptr<Type> type_;
};
class IntegerLiteral : public Value {
public:
  explicit IntegerLiteral(std::shared_ptr<Type> type, uint64_t value)
      : Value(type), value_(value) {}
  void accept(Visitor &V) override { V.visit(*this); }

  uint64_t value() const { return value_; }

private:
  uint64_t value_;
};
class FloatLiteral : public Value {
public:
  explicit FloatLiteral(std::shared_ptr<Type> type, double value) : Value(type), value_(value) {}
  void accept(Visitor &V) override { V.visit(*this); }

  double value() const { return value_; }

private:
  double value_;
};
class Variant : public Value {
public:
  Variant(Decl *decl) : Value(decl->type()), decl_(decl) {}
  void accept(Visitor &V) override { V.visit(*this); }

  Decl *decl() const { return decl_; }

private:
  Decl *decl_;
};
class PrefixResult : public Value {
public:
  enum class Op { Not, Minus };
  PrefixResult(Op op, std::shared_ptr<Value> operand)
      : Value(operand->type()), op_(op), operand_(operand) {}
  void accept(Visitor &V) override { V.visit(*this); }

  Op getOp() const { return op_; }
  std::shared_ptr<Value> operand() const { return operand_; }

private:
  Op op_;
  std::shared_ptr<Value> operand_;
};
class BinaryResult : public Value {
public:
  BinaryResult(ast::BinaryExpr::Op op, std::shared_ptr<Type> type, std::shared_ptr<Value> lhs,
               std::shared_ptr<Value> rhs)
      : Value(type), op_(op), lhs_(lhs), rhs_(rhs) {}
  void accept(Visitor &V) override { V.visit(*this); }

  ast::BinaryExpr::Op op() const { return op_; }
  std::shared_ptr<Value> lhs() const { return lhs_; }
  std::shared_ptr<Value> rhs() const { return rhs_; }

private:
  ast::BinaryExpr::Op op_;
  std::shared_ptr<Value> lhs_;
  std::shared_ptr<Value> rhs_;
};
class CallResult : public Value {
public:
  using ArgumentVec = llvm::SmallVector<std::shared_ptr<Value>, 4U>;
  CallResult(std::shared_ptr<Type> type, std::shared_ptr<Value> func, ArgumentVec arguments)
      : Value(type), func_(func), arguments_(std::move(arguments)) {}
  void accept(Visitor &V) override { V.visit(*this); }

  std::shared_ptr<Value> func() const { return func_; }
  ArgumentVec const &arguments() const { return arguments_; }

private:
  std::shared_ptr<Value> func_;
  ArgumentVec arguments_;
};

class Statement : public HIR {
public:
  explicit Statement() : next_(nullptr), prev_(nullptr) {}
  virtual ~Statement() = default;
  void accept(Visitor &V) override { V.visit(*this); }

  void setNextStatement(std::shared_ptr<Statement> next) {
    assert(next_ == nullptr && "Statement already has next statement");
    next_ = next;
    next->prev_ = this;
  }
  std::shared_ptr<Statement> next() const { return next_; }
  Statement *prev() const { return prev_; }

protected:
  std::shared_ptr<Statement> next_;
  Statement *prev_;
};
class AssignStatement : public Statement {
public:
  AssignStatement(std::shared_ptr<Decl> decl, std::shared_ptr<Variant> variant,
                  std::shared_ptr<Value> value)
      : Statement(), decl_(decl), variant_(variant), value_(value) {}
  void accept(Visitor &V) override { V.visit(*this); }

  std::shared_ptr<Decl> decl() const { return decl_; }
  std::shared_ptr<Variant> variant() const { return variant_; }
  std::shared_ptr<Value> value() const { return value_; }

private:
  std::shared_ptr<Decl> decl_;       // maybe nullptr
  std::shared_ptr<Variant> variant_; // maybe nullptr
  std::shared_ptr<Value> value_;
};
class LoopStatement : public Statement {
public:
  LoopStatement(std::shared_ptr<Statement> body) : Statement(), body_(body) {}
  void accept(Visitor &V) override { V.visit(*this); }

  std::shared_ptr<Statement> body() const { return body_; }

private:
  std::shared_ptr<Statement> body_; // maybe nullptr
};
class JumpStatement : public Statement {
public:
  // TODO maybe more clear JumpStatement is needed.
  enum class Kind : uint8_t { Break, Continue };
  JumpStatement(Kind kind) : Statement(), kind_(kind) {}
  void accept(Visitor &V) override { V.visit(*this); }

  Kind getKind() const { return kind_; }

private:
  Kind kind_;
};
class ReturnStatement : public Statement {
public:
  ReturnStatement(std::shared_ptr<Value> value) : Statement(), value_(value) {}
  void accept(Visitor &V) override { V.visit(*this); }

  std::shared_ptr<Value> value() const { return value_; }

private:
  std::shared_ptr<Value> value_;
};
class BlockStatement : public Statement {
public:
  BlockStatement(std::shared_ptr<Statement> body) : Statement(), body_(body) {}
  void accept(Visitor &V) override { V.visit(*this); }

  std::shared_ptr<Statement> body() const { return body_; }

private:
  std::shared_ptr<Statement> body_; // maybe nullptr
};
class BranchStatement : public Statement {
public:
  BranchStatement(std::shared_ptr<Value> condition, std::shared_ptr<Statement> thenStatement,
                  std::shared_ptr<Statement> elseStatement)
      : Statement(), condition_(condition), thenStatement_(thenStatement),
        elseStatement_(elseStatement) {}
  void accept(Visitor &V) override { V.visit(*this); }

  std::shared_ptr<Value> condition() const { return condition_; }
  std::shared_ptr<Statement> thenStatement() const { return thenStatement_; }
  std::shared_ptr<Statement> elseStatement() const { return elseStatement_; }

private:
  std::shared_ptr<Value> condition_;
  std::shared_ptr<Statement> thenStatement_;
  std::shared_ptr<Statement> elseStatement_; // maybe nullptr
};

} // namespace scriptlang::hir

#endif /* SCRIPTLANG_LIB_SEMATIC_HIR_HPP */
