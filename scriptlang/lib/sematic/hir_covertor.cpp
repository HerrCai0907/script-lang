#include "magic_enum.hpp"
#include "scriptlang/lib/parser/ast.hpp"
#include "scriptlang/lib/sematic/hir.hpp"
#include "scriptlang/lib/sematic/hir_coverter.hpp"
#include "llvm/Support/ErrorHandling.h"
#include <memory>

namespace scriptlang {

std::shared_ptr<hir::Statement>
HIRConverter::visitAll(llvm::SmallVectorImpl<std::shared_ptr<ast::TopDecls>> const &tops) {
  // TODO
  llvm::SmallVector<std::shared_ptr<hir::Statement>, 32U> topHIRs;
  for (auto top : tops) {
    visit(*top);
    typeSystem_.applyDefaultPendingType();
    topHIRs.push_back(stmtResult_);
  }
  // TODO
  return topHIRs.front();
}

void HIRConverter::visit(ast::TopDecls &topDecls) {
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
void HIRConverter::visit(ast::AssignStmt &stmt) {
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
void HIRConverter::visit(ast::BlockStmt &stmt) {
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
void HIRConverter::visit(ast::BreakStmt &) {
  stmtResult_.reset(new hir::JumpStatement(hir::JumpStatement::Kind::Break));
}
void HIRConverter::visit(ast::ContinueStmt &) {
  stmtResult_.reset(new hir::JumpStatement(hir::JumpStatement::Kind::Continue));
}
void HIRConverter::visit(ast::DeclStmt &stmt) {
  stmt.expr()->accept(*this);
  auto init = exprResult_;
  if (init == nullptr)
    return stmtResult_.reset();
  auto declType = handTypeNode(stmt.type());
  if (declType == nullptr)
    declType = init->type();
  std::shared_ptr<hir::Decl> decl{new hir::Decl(stmt.name(), declType, stmt.nameEndLoc())};
  decl->setConst(stmt.isConst());
  stmtResult_.reset(new hir::AssignStatement(
      decl, std::shared_ptr<hir::Variant>(new hir::Variant(decl.get())), init));
}
void HIRConverter::visit(ast::ExprStmt &stmt) {
  stmt.expr()->accept(*this);
  auto value = exprResult_;
  if (value == nullptr)
    return stmtResult_.reset();
  stmtResult_.reset(new hir::AssignStatement(nullptr, nullptr, value));
}
void HIRConverter::visit(ast::IfStmt &stmt) {
  stmt.condition()->accept(*this);
  auto condition = exprResult_;
  if (condition->type() != typeSystem_.boolTy()) {
    diag_.report(stmt.condition()->start(), Diag::unexpected_type, "bool",
                 condition->type()->toString());
    return stmtResult_.reset();
  }
  stmt.thenBlock()->accept(*this);
  auto thenStatement = stmtResult_;
  std::shared_ptr<hir::Statement> elseStatement = nullptr;
  if (stmt.elseBlock()) {
    stmt.elseBlock()->accept(*this);
    elseStatement = stmtResult_;
  }
  stmtResult_.reset(new hir::BranchStatement(condition, thenStatement, elseStatement));
}
void HIRConverter::visit(ast::ImportStmt &stmt) {
  // TODO
}
void HIRConverter::visit(ast::ReturnStmt &stmt) {
  std::shared_ptr<hir::Value> returnValue{nullptr};
  if (stmt.value()) {
    stmt.value()->accept(*this);
    returnValue = exprResult_;
  }
  stmtResult_.reset(new hir::ReturnStatement(returnValue));
}
void HIRConverter::visit(ast::WhileStmt &stmt) {
  stmt.condition()->accept(*this);
  auto condition = exprResult_;
  if (condition == nullptr)
    return stmtResult_.reset();
  if (condition->type() != typeSystem_.boolTy()) {
    diag_.report(stmt.condition()->start(), Diag::unexpected_type, "bool",
                 condition->type()->toString());
    return stmtResult_.reset();
  }
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

std::shared_ptr<hir::Type>
HIRConverter::handTypeNode(std::shared_ptr<ast::TypeNode> const &typeNode) {
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
    return typeSystem_.createFuncType(argumentTypes, returnType);
  }
  return nullptr;
}

void HIRConverter::visit(ast::BinaryExpr &expr) {
  expr.lhs()->accept(*this);
  auto lhs = exprResult_;
  expr.rhs()->accept(*this);
  auto rhs = exprResult_;
  if (lhs == nullptr || rhs == nullptr)
    return exprResult_.reset();
  std::shared_ptr<hir::Type> targetType;
  switch (expr.op()) {
  case ast::BinaryExpr::Op::ADD:
  case ast::BinaryExpr::Op::SUB:
  case ast::BinaryExpr::Op::MUL:
  case ast::BinaryExpr::Op::DIV: {
    targetType = typeSystem_.mergePendingResolvedType(TypeSystem::MergeKind::BinaryArithmetic,
                                                      lhs->type(), rhs->type());
    if (targetType == nullptr) {
      diag_.report(expr.lhs()->start(), Diag::binary_expression_expect_same_type,
                   lhs->type()->toString(), rhs->type()->toString());
      return exprResult_.reset();
    }
    break;
  }
  case ast::BinaryExpr::Op::LESS_THAN:
  case ast::BinaryExpr::Op::GREATER_THAN:
  case ast::BinaryExpr::Op::NO_LESS_THAN:
  case ast::BinaryExpr::Op::NO_GREATER_THAN:
  case ast::BinaryExpr::Op::EQUAL:
  case ast::BinaryExpr::Op::NOT_EQUAL: {
    auto mergedType = typeSystem_.mergePendingResolvedType(TypeSystem::MergeKind::BinaryCompare,
                                                           lhs->type(), rhs->type());
    if (mergedType == nullptr)
      diag_.report(expr.lhs()->start(), Diag::binary_expression_expect_same_type,
                   lhs->type()->toString(), rhs->type()->toString());
    targetType = typeSystem_.boolTy();
    break;
  }
  case ast::BinaryExpr::Op::MOD:
  case ast::BinaryExpr::Op::LEFT_SHIFT:
  case ast::BinaryExpr::Op::RIGHT_SHIFT:
  case ast::BinaryExpr::Op::AND:
  case ast::BinaryExpr::Op::OR:
  case ast::BinaryExpr::Op::XOR:
  case ast::BinaryExpr::Op::LOGIC_AND:
  case ast::BinaryExpr::Op::LOGIC_OR: {
    targetType = typeSystem_.mergePendingResolvedType(TypeSystem::MergeKind::BinaryLogic,
                                                      lhs->type(), rhs->type());
    if (targetType == nullptr)
      diag_.report(expr.lhs()->start(), Diag::binary_expression_expect_same_type,
                   lhs->type()->toString(), rhs->type()->toString());
    break;
  }
  }
  exprResult_.reset(new hir::BinaryResult(expr.op(), targetType, lhs, rhs));
}
void HIRConverter::visit(ast::CallExpr &expr) {
  expr.caller()->accept(*this);
  auto func = exprResult_;
  auto funcType = std::dynamic_pointer_cast<hir::FuncType>(func->type());
  if (funcType == nullptr) {
    diag_.report(expr.caller()->start(), Diag::unexpected_type, "Function",
                 func->type()->toString());
    return exprResult_.reset();
  }
  if (expr.arguments().size() != funcType->argumentTypes().size()) {
    diag_.report(expr.start(), Diag ::mismatched_arguments_amount, funcType->argumentTypes().size(),
                 expr.arguments().size());
    return exprResult_.reset();
  }
  hir::CallResult::ArgumentVec arguments;
  for (size_t i = 0; i < expr.arguments().size(); i++) {
    auto &argument = expr.arguments()[i];
    argument->accept(*this);
    auto argumentExpr = exprResult_;
    arguments.push_back(argumentExpr);
    if (*argumentExpr->type() != *funcType->argumentTypes()[i]) {
      diag_.report(expr.arguments()[i]->start(), Diag::unexpected_type,
                   funcType->argumentTypes()[i]->toString(), exprResult_->type()->toString());
      return exprResult_.reset();
    }
  }
  exprResult_.reset(new hir::CallResult(funcType->returnType(), func, arguments));
}
void HIRConverter::visit(ast::FuncExpr &expr) {}
void HIRConverter::visit(ast::Identifier &expr) {
  if (expr.name() == "true") {
    exprResult_.reset(new hir::IntegerLiteral(typeSystem_.boolTy(), 1));
    return;
  }
  if (expr.name() == "false") {
    exprResult_.reset(new hir::IntegerLiteral(typeSystem_.boolTy(), 0));
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
  diag_.report(expr.start(), Diag::undefined_identifier, expr.name());
  return exprResult_.reset();
}
void HIRConverter::visit(ast::LiteralExpr &expr) {
  switch (expr.kind()) {
  case ast::LiteralExpr::Kind::Integer: {
    uint64_t value = std::stoull(expr.value());
    auto type = typeSystem_.createPendingResolvedTypeFromInteger(value)->tryResolve();
    exprResult_.reset(new hir::IntegerLiteral(type, value));
    break;
  }
  case ast::LiteralExpr::Kind::Hex: {
    uint64_t value = std::stoull(expr.value(), nullptr, 16U);
    auto type = typeSystem_.createPendingResolvedTypeFromInteger(value)->tryResolve();
    exprResult_.reset(new hir::IntegerLiteral(type, value));
    break;
  }
  case ast::LiteralExpr::Kind::Float: {
    double value = std::stold(expr.value());
    exprResult_.reset(new hir::FloatLiteral(typeSystem_.getTypeByName("f32"), value));
    break;
  }
  case ast::LiteralExpr::Kind::String:
    llvm_unreachable("TODO");
    break;
  }
}
void HIRConverter::visit(ast::MemberExpr &expr) {
  // TODO
}
void HIRConverter::visit(ast::PrefixExpr &expr) {
  // `-{Integer}` should be combined as integer
  if (expr.op() == ast::PrefixExpr::Op::Minus) {
    auto literalOperand = std::dynamic_pointer_cast<ast::LiteralExpr>(expr.expr());
    if (literalOperand) {
      switch (literalOperand->kind()) {
      case ast::LiteralExpr::Kind::Integer: {
        uint64_t value = static_cast<uint64_t>(0) - std::stoull(literalOperand->value());
        auto type = typeSystem_.createPendingResolvedTypeFromInteger(static_cast<int64_t>(value))
                        ->tryResolve();
        exprResult_.reset(new hir::IntegerLiteral(type, value));
        return;
      }
      case ast::LiteralExpr::Kind::Hex: {
        uint64_t value =
            static_cast<uint64_t>(0) - std::stoull(literalOperand->value(), nullptr, 16U);
        auto type = typeSystem_.createPendingResolvedTypeFromInteger(static_cast<int64_t>(value))
                        ->tryResolve();
        exprResult_.reset(new hir::IntegerLiteral(type, value));
        return;
      }
      case ast::LiteralExpr::Kind::Float: {
        double value = static_cast<double>(0) - std::stold(literalOperand->value());
        exprResult_.reset(new hir::FloatLiteral(typeSystem_.f32Ty(), value));
        return;
      }
      case ast::LiteralExpr::Kind::String: {
        break;
      }
      }
    }
  }
  expr.expr()->accept(*this);
  auto operand = exprResult_;
  TypeSystem::ApplyKind kind;
  switch (expr.op()) {
  case ast::PrefixExpr::Op::Not:
    kind = TypeSystem::ApplyKind::PrefixNot;
    break;
  case ast::PrefixExpr::Op::Plus:
    kind = TypeSystem::ApplyKind::PrefixPlus;
    break;
  case ast::PrefixExpr::Op::Minus:
    kind = TypeSystem::ApplyKind::PrefixMinus;
    break;
  }
  auto targetType = typeSystem_.applyPendingResolvedType(kind, operand->type());
  if (targetType == nullptr) {
    diag_.report(expr.start(), Diag::invalid_operand_in_prefix_expression,
                 magic_enum::enum_name(expr.op()), operand->type()->toString());
    return exprResult_.reset();
  }
  switch (expr.op()) {
  case ast::PrefixExpr::Op::Not:
    exprResult_.reset(new hir::PrefixResult(hir::PrefixResult::Op::Not, operand));
    break;
  case ast::PrefixExpr::Op::Plus:
    exprResult_ = operand;
    break;
  case ast::PrefixExpr::Op::Minus:
    exprResult_.reset(new hir::PrefixResult(hir::PrefixResult::Op::Minus, operand));
    break;
  }
}

} // namespace scriptlang