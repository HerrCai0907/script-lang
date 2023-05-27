#include "scriptlang/lib/sematic/sematic.hpp"
#include "scriptlang/lib/parser/ast.hpp"
#include "scriptlang/lib/sematic/hir.hpp"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include <memory>
#include <string>
#include <utility>

namespace scriptlang {

class TypeSystem : public ast::Visitor {
public:
  TypeSystem() : hasError_(false) {
    addType(std::shared_ptr<hir::NamedType>(new hir::NamedType(llvm::StringRef("bool"))));
    addType(std::shared_ptr<hir::NamedType>(new hir::NamedType(llvm::StringRef("i8"))));
    addType(std::shared_ptr<hir::NamedType>(new hir::NamedType(llvm::StringRef("i16"))));
    addType(std::shared_ptr<hir::NamedType>(new hir::NamedType(llvm::StringRef("i32"))));
    addType(std::shared_ptr<hir::NamedType>(new hir::NamedType(llvm::StringRef("u8"))));
    addType(std::shared_ptr<hir::NamedType>(new hir::NamedType(llvm::StringRef("u16"))));
    addType(std::shared_ptr<hir::NamedType>(new hir::NamedType(llvm::StringRef("u32"))));
    addType(std::shared_ptr<hir::NamedType>(new hir::NamedType(llvm::StringRef("f32"))));
  }

  bool hasError() const { return hasError_; }

  std::shared_ptr<hir::NamedType> getTypeByName(llvm::StringRef name) const {
    auto it = typeMap_.find(name);
    if (it == typeMap_.end())
      return nullptr;
    return it->second;
  }

private:
  bool hasError_;
  llvm::StringMap<std::shared_ptr<hir::NamedType>> typeMap_;

  bool addType(std::shared_ptr<hir::NamedType> const &type) {
    return typeMap_.insert_or_assign(type->name(), type).second;
  }
};

class ToHIRConverter : public ast::Visitor {
public:
  ToHIRConverter(TypeSystem const &typeSystem) : hasError_(false), typeSystem_(typeSystem) {}

  std::shared_ptr<hir::Statement>
  visitAll(llvm::SmallVectorImpl<std::shared_ptr<ast::TopDecls>> const &tops) {
    // TODO
    llvm::SmallVector<std::shared_ptr<hir::Statement>, 32U> topHIRs;
    for (auto top : tops) {
      visit(*top);
      topHIRs.push_back(stmtResult_);
    }
    // TODO
    return topHIRs.front();
  }

  void visit(ast::TopDecls &topDecls) override {
    std::shared_ptr<hir::Statement> start;
    std::shared_ptr<hir::Statement> last;
    for (auto top : topDecls) {
      top->accept(*this);
      if (stmtResult_ == nullptr)
        continue;
      if (last)
        last->setNextStatement(stmtResult_);
      else
        start = stmtResult_;
      last = stmtResult_;
    }
    stmtResult_.reset(new hir::BlockStatement(start));
  }
  void visit(ast::AssignStmt &stmt) override {
    stmt.lhs()->accept(*this);
    if (!std::dynamic_pointer_cast<hir::Variant>(exprResult_))
      return stmtResult_.reset();
    auto variant = std::dynamic_pointer_cast<hir::Variant>(exprResult_);
    stmt.rhs()->accept(*this);
    auto value = exprResult_;
    if (variant == nullptr || value == nullptr)
      return stmtResult_.reset();
    stmtResult_.reset(new hir::AssignStatement(nullptr, variant, value));
  }
  void visit(ast::BlockStmt &stmt) override {
    std::shared_ptr<hir::Statement> start;
    std::shared_ptr<hir::Statement> last;
    for (auto subStmt : stmt) {
      subStmt->accept(*this);
      if (stmtResult_ == nullptr)
        continue;
      if (last)
        last->setNextStatement(stmtResult_);
      else
        start = stmtResult_;
      last = stmtResult_;
    }
    stmtResult_.reset(new hir::BlockStatement(start));
  }
  void visit(ast::BreakStmt &) override {
    stmtResult_.reset(new hir::JumpStatement(hir::JumpStatement::Kind::Break));
  }
  void visit(ast::ContinueStmt &) override {
    stmtResult_.reset(new hir::JumpStatement(hir::JumpStatement::Kind::Continue));
  }
  void visit(ast::DeclStmt &stmt) override {
    stmt.expr()->accept(*this);
    auto init = exprResult_;
    auto declType = handTypeNode(stmt.type());
    hasError_ = declType == nullptr;
    if (hasError_)
      return stmtResult_.reset();
    std::shared_ptr<hir::Decl> decl{new hir::Decl(stmt.name(), declType)};
    stmtResult_.reset(new hir::AssignStatement(
        decl, std::shared_ptr<hir::Variant>(new hir::Variant(decl.get())), init));
  }
  void visit(ast::ExprStmt &stmt) override {
    stmt.expr()->accept(*this);
    auto value = exprResult_;
    if (value == nullptr)
      return stmtResult_.reset();
    stmtResult_.reset(new hir::AssignStatement(nullptr, nullptr, value));
  }
  void visit(ast::IfStmt &stmt) override {
    stmt.condition()->accept(*this);
    auto condition = exprResult_;
    hasError_ = condition->type() != typeSystem_.getTypeByName("bool");
    if (hasError_)
      return stmtResult_.reset();
    stmt.thenBlock()->accept(*this);
    auto thenStatement = stmtResult_;
    std::shared_ptr<hir::Statement> elseStatement = nullptr;
    if (stmt.elseBlock()) {
      stmt.elseBlock()->accept(*this);
      elseStatement = stmtResult_;
    }
    stmtResult_.reset(new hir::BranchStatement(condition, thenStatement, elseStatement));
  }
  void visit(ast::ImportStmt &stmt) override {
    // TODO
  }
  void visit(ast::ReturnStmt &stmt) override {
    std::shared_ptr<hir::Value> returnValue{nullptr};
    if (stmt.value()) {
      stmt.value()->accept(*this);
      returnValue = exprResult_;
    }
    stmtResult_.reset(new hir::ReturnStatement(returnValue));
  }
  void visit(ast::WhileStmt &stmt) override {
    stmt.condition()->accept(*this);
    auto condition = exprResult_;
    if (condition == nullptr)
      return stmtResult_.reset();
    hasError_ = condition->type() != typeSystem_.getTypeByName("bool");
    if (hasError_)
      return stmtResult_.reset();
    stmt.block()->accept(*this);
    auto block = stmtResult_;
    // - Loop
    //   - Branch (condition)
    //     - Block
    //     - Jump Break
    stmtResult_.reset(new hir::LoopStatement(std::shared_ptr<hir::BranchStatement>(
        new hir::BranchStatement(condition, block,
                                 std::shared_ptr<hir::JumpStatement>(
                                     new hir::JumpStatement(hir::JumpStatement::Kind::Break))))));
  }

  std::shared_ptr<hir::Type> handTypeNode(std::shared_ptr<ast::TypeNode> const &typeNode) {
    if (std::dynamic_pointer_cast<ast::NamedTypeNode>(typeNode)) {
      auto const &names = std::dynamic_pointer_cast<ast::NamedTypeNode>(typeNode)->names();
      if (names.size() != 1U)
        return nullptr;
      return typeSystem_.getTypeByName(names[0]);
    }
    if (std::dynamic_pointer_cast<ast::FunctionTypeNode>(typeNode)) {
      auto funcTypeNode = std::dynamic_pointer_cast<ast::FunctionTypeNode>(typeNode);
      hir::FuncType::ArgumentTypes argumentTypes;
      for (auto parameter : funcTypeNode->parameters()) {
        auto argumentType = handTypeNode(parameter);
        if (argumentType == nullptr)
          return nullptr;
        argumentTypes.push_back(argumentType);
      }
      auto returnType = handTypeNode(funcTypeNode->returnType());
      if (returnType == nullptr)
        return nullptr;
      return std::shared_ptr<hir::FuncType>(new hir::FuncType(argumentTypes, returnType));
    }
    return nullptr;
  }

  void visit(ast::BinaryExpr &expr) override {
    if (hasError_)
      return exprResult_.reset();
    expr.lhs()->accept(*this);
    auto lhs = exprResult_;
    expr.rhs()->accept(*this);
    auto rhs = exprResult_;
    if (lhs == nullptr || rhs == nullptr)
      return exprResult_.reset();
    hasError_ = *lhs->type() != *rhs->type();
    if (hasError_)
      return exprResult_.reset();
    exprResult_.reset(new hir::BinaryResult(expr.op(), lhs->type(), lhs, rhs));
  }
  void visit(ast::CallExpr &expr) override {
    if (hasError_)
      return exprResult_.reset();
    expr.caller()->accept(*this);
    auto func = exprResult_;
    auto funcType = std::dynamic_pointer_cast<hir::FuncType>(func->type());
    hasError_ = funcType == nullptr;
    if (hasError_)
      return exprResult_.reset();
    hasError_ = expr.arguments().size() != funcType->argumentTypes().size();
    if (hasError_)
      return exprResult_.reset();
    hir::CallResult::ArgumentVec arguments;
    for (size_t i = 0; i < expr.arguments().size(); i++) {
      expr.arguments()[i]->accept(*this);
      arguments.push_back(exprResult_);
      hasError_ = *exprResult_->type() != *funcType->argumentTypes()[i];
      if (hasError_)
        return exprResult_.reset();
    }
    exprResult_.reset(new hir::CallResult(funcType->returnType(), func, arguments));
  }
  void visit(ast::FuncExpr &expr) override {}
  void visit(ast::Identifier &expr) override {
    if (expr.name() == "true") {
      exprResult_.reset(new hir::IntegerLiteral(typeSystem_.getTypeByName("bool"), 1));
      return;
    }
    if (expr.name() == "false") {
      exprResult_.reset(new hir::IntegerLiteral(typeSystem_.getTypeByName("bool"), 0));
      return;
    }
    auto stmt = stmtResult_.get();
    while (stmt) {
      auto assignStmt = dynamic_cast<hir::AssignStatement *>(stmt);
      if (assignStmt) {
        if (assignStmt->decl() && assignStmt->decl()->name() == expr.name()) {
          exprResult_.reset(new hir::Variant(assignStmt->decl().get()));
          return;
        }
      }
      stmt = stmt->prev();
    }
    hasError_ = true;
    return exprResult_.reset();
  }
  void visit(ast::LiteralExpr &expr) override {
    switch (expr.kind()) {
    case ast::LiteralExpr::Kind::Integer: {
      uint64_t value = std::stoull(expr.value());
      exprResult_.reset(new hir::IntegerLiteral(typeSystem_.getTypeByName("i32"), value));
      break;
    }
    case ast::LiteralExpr::Kind::Hex: {
      uint64_t value = std::stoull(expr.value(), nullptr, 16U);
      exprResult_.reset(new hir::IntegerLiteral(typeSystem_.getTypeByName("i32"), value));
      break;
    }
    case ast::LiteralExpr::Kind::Float: {
      double value = std::stold(expr.value());
      exprResult_.reset(new hir::FloatLiteral(typeSystem_.getTypeByName("f32"), value));
      break;
    }
    case ast::LiteralExpr::Kind::String:
      llvm_unreachable("");
      break;
    }
  }
  void visit(ast::MemberExpr &expr) override {
    // TODO
  }
  void visit(ast::PrefixExpr &expr) override {
    expr.expr()->accept(*this);
    auto operand = exprResult_;
    switch (expr.op()) {
    case ast::PrefixExpr::Op::Not:
      hasError_ = operand->type() != typeSystem_.getTypeByName("bool");
      if (hasError_)
        return exprResult_.reset();
      exprResult_.reset(new hir::PrefixResult(hir::PrefixResult::Op::Not, operand));
      break;
    case ast::PrefixExpr::Op::Plus:
      hasError_ = operand->type() != typeSystem_.getTypeByName("i32");
      if (hasError_)
        return exprResult_.reset();
      exprResult_ = operand;
      break;
    case ast::PrefixExpr::Op::Minus:
      hasError_ = operand->type() != typeSystem_.getTypeByName("i32");
      if (hasError_)
        return exprResult_.reset();
      exprResult_.reset(new hir::PrefixResult(hir::PrefixResult::Op::Minus, operand));
      break;
    }
  }

  bool hasError() const { return hasError_; }

private:
  bool hasError_;
  TypeSystem const &typeSystem_;

  std::shared_ptr<hir::Value> exprResult_;
  std::shared_ptr<hir::Statement> stmtResult_;
};

class Sema::Impl {
public:
  std::shared_ptr<hir::Statement>
  sematic(llvm::SmallVectorImpl<std::shared_ptr<ast::TopDecls>> &tops) {
    typeSystems_.reset(new TypeSystem());
    for (auto top : tops)
      typeSystems_->visit(*top);
    if (typeSystems_->hasError())
      return nullptr;
    hirConverter_.reset(new ToHIRConverter(*typeSystems_));
    auto hir = hirConverter_->visitAll(tops);
    if (hirConverter_->hasError())
      return nullptr;
    return hir;
  }

private:
  std::shared_ptr<TypeSystem> typeSystems_;
  std::shared_ptr<ToHIRConverter> hirConverter_;
};

Sema::Sema() : impl_(new Impl()) {}
std::shared_ptr<hir::Statement>
Sema::sematic(llvm::SmallVectorImpl<std::shared_ptr<ast::TopDecls>> &tops) {
  return impl_->sematic(tops);
}

} // namespace scriptlang
