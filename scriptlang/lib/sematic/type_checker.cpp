#include "scriptlang/lib/sematic/type_checker.hpp"
#include "scriptlang/lib/basic/diagnostic.hpp"
#include "scriptlang/lib/sematic/hir.hpp"
#include "llvm/Support/SMLoc.h"

namespace scriptlang {

static std::shared_ptr<hir::Type> resolveType(std::shared_ptr<hir::Type> const &type) {
  auto pendingResolvedType = std::dynamic_pointer_cast<hir::PendingResolvedType>(type);
  if (pendingResolvedType)
    return pendingResolvedType->resolve();
  else
    return type;
}

void PendingResolvedTypeChecker::resolve(std::shared_ptr<hir::Statement> const &stmt) {
  stmt->accept(*this);
}

void PendingResolvedTypeChecker::visit(hir::Decl &decl) {
  decl.type()->accept(*this);
  auto pendingResolvedType = std::dynamic_pointer_cast<hir::PendingResolvedType>(decl.type());
  if (pendingResolvedType) {
    if (pendingResolvedType->isInvalid()) {
      diag_.report(decl.getTypeDeclarationLoc(), Diag::expect_type_declaration);
    } else if (!pendingResolvedType->canBeResolved()) {
      diag_.report(decl.getTypeDeclarationLoc(), Diag::expect_type_declaration_with_options,
                   pendingResolvedType->toString());
    } else {
      decl.type_ = pendingResolvedType->resolve();
    }
  }
}

void PendingResolvedTypeChecker::visit(hir::Value &value) {
  value.type()->accept(*this);
  value.type_ = resolveType(value.type());
}

void PendingResolvedTypeChecker::visit(hir::FuncValue &value) {
  visit(static_cast<hir::Value &>(value));
  value.getBody()->accept(*this);
}

void PendingResolvedTypeChecker::visit(hir::FuncType &type) {
  type.returnType_ = resolveType(type.returnType());
}

void PendingResolvedTypeChecker::visit(hir::IntegerLiteral &value) {
  value.type_ = resolveType(value.type());
}

} // namespace scriptlang
