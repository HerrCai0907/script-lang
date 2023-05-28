#ifndef SCRIPTLANG_LIB_SEMATIC_HIR_HPP
#define SCRIPTLANG_LIB_SEMATIC_HIR_HPP

#include "scriptlang/lib/parser/ast.hpp"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/SMLoc.h"
#include <cassert>
#include <functional>
#include <list>
#include <memory>
#include <set>
#include <utility>

namespace scriptlang {
class TypeSystem;
}

namespace scriptlang::hir {

class HIR;

class Type;
class FuncType;
class NamedType;
class PendingResolvedType;

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
  virtual void visit(FuncType &type);
  virtual void visit(NamedType &type) {}
  virtual void visit(PendingResolvedType &type);

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

  virtual std::string toString() const = 0;

private:
  virtual bool equal(Type const &type) const = 0;
};
class NamedType : public Type {
public:
  void accept(Visitor &V) override { V.visit(*this); }

  llvm::SmallString<16U> name() const { return name_; }
  std::string toString() const override { return name_.str().str(); }

private:
  llvm::SmallString<16U> name_;

  explicit NamedType(llvm::SmallString<16U> name) : name_(name) {}
  bool equal(Type const &type) const override;

  friend class ::scriptlang::TypeSystem;
};
class PendingResolvedType : public Type, public std::enable_shared_from_this<PendingResolvedType> {
public:
  using TypeCandidates = std::set<std::shared_ptr<Type>>;
  enum class CallBackStatus { Resolved, Pending };
  using OnChangeCallback = std::function<void()>;
  TypeCandidates const &candidates() const { return candidates_; }
  std::string toString() const override;

  bool isInvalid() const { return candidates_.size() == 0; }
  bool canBeResolved() const { return candidates_.size() == 1; }

  std::shared_ptr<Type> const &getDefaultType() const { return defaultType_; }
  void setDefaultType(std::shared_ptr<Type> const &type);

  bool removeIf(std::function<bool(std::shared_ptr<Type> const &)> condition,
                std::list<OnChangeCallback> &onChanges);
  bool onlyKeepCandidate(std::shared_ptr<Type> const &type, std::list<OnChangeCallback> &onChanges);
  static void intersection(PendingResolvedType &lhs, PendingResolvedType &rhs,
                           std::list<OnChangeCallback> &onChanges);
  void applyDefaultType(std::list<OnChangeCallback> &onChanges);

  std::shared_ptr<Type> resolve();
  std::shared_ptr<Type> tryResolve();

  void registerOnChangeCallback(OnChangeCallback cb) {
    onChangeCallbacks_.push_back(std::move(cb));
  }
  void clearOnChangeCallback() { onChangeCallbacks_.clear(); }

private:
  TypeCandidates candidates_;
  std::list<OnChangeCallback> onChangeCallbacks_;
  std::shared_ptr<Type> defaultType_;

  explicit PendingResolvedType(TypeCandidates candidates)
      : candidates_(candidates), onChangeCallbacks_(), defaultType_(nullptr) {}
  bool equal(Type const &type) const override { return false; }
  void appendOnChangeToList(std::list<OnChangeCallback> &onChanges);

  friend class ::scriptlang::TypeSystem;
};
class FuncType : public Type {
public:
  using ArgumentTypes = llvm::SmallVector<std::shared_ptr<Type>, 4U>;
  void accept(Visitor &V) override { V.visit(*this); }

  ArgumentTypes const &argumentTypes() const { return argumentTypes_; }
  std::shared_ptr<Type> returnType() const { return returnType_; }

  std::string toString() const override;

private:
  ArgumentTypes argumentTypes_;
  std::shared_ptr<Type> returnType_;

  FuncType(ArgumentTypes argumentTypes, std::shared_ptr<Type> returnType)
      : Type(), argumentTypes_(std::move(argumentTypes)), returnType_(returnType) {}
  bool equal(Type const &type) const override;

  friend class ::scriptlang::TypeSystem;
};

class Decl : public HIR {
public:
  Decl(llvm::StringRef name, std::shared_ptr<Type> type, llvm::SMLoc typeDeclarationLoc)
      : name_(name), type_(std::move(type)), typeDeclarationLoc_(typeDeclarationLoc) {
    assert(type_ != nullptr);
  }
  void accept(Visitor &V) override { V.visit(*this); }

  bool isConst() const { return isConst_; }
  void setConst(bool isConst) { isConst_ = isConst; }
  llvm::StringRef name() const { return name_; }
  std::shared_ptr<Type> type() const { return type_; }

  llvm::SMLoc const &getTypeDeclarationLoc() const { return typeDeclarationLoc_; }

private:
  llvm::SmallString<16U> name_;
  std::shared_ptr<Type> type_;

  llvm::SMLoc typeDeclarationLoc_;

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
