#include "scriptlang/lib/sematic/type_checker.hpp"
#include "scriptlang/lib/basic/diagnostic.hpp"
#include "scriptlang/lib/sematic/hir.hpp"

namespace scriptlang {

void PendingResolvedTypeChecker::visit(hir::Decl &decl) {
  auto pendingResolvedType = std::dynamic_pointer_cast<hir::PendingResolvedType>(decl.type());
  if (pendingResolvedType) {
    if (pendingResolvedType->isInvalid()) {
      diag_.report(decl.getTypeDeclarationLoc(), Diag::expect_type_declaration);
    } else if (!pendingResolvedType->canBeResolved()) {
      diag_.report(decl.getTypeDeclarationLoc(), Diag::expect_type_declaration_with_options,
                   pendingResolvedType->toString());
    }
  }
}

} // namespace scriptlang