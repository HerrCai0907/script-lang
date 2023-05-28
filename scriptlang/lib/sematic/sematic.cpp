#include "scriptlang/lib/sematic/sematic.hpp"
#include "scriptlang/lib/sematic/hir_coverter.hpp"
#include "scriptlang/lib/sematic/type_checker.hpp"
#include "scriptlang/lib/sematic/type_system.hpp"
#include "llvm/ADT/SmallVector.h"

namespace scriptlang {

class Sema::Impl {
public:
  Impl(DiagnosticsEngine &diag) : diag_(diag) {}

  std::shared_ptr<hir::Statement>
  sematic(llvm::SmallVectorImpl<std::shared_ptr<ast::TopDecls>> &tops) {
    TypeSystem typeSystems{diag_};
    for (auto top : tops)
      typeSystems.visit(*top);
    if (diag_.numError() > 0)
      return nullptr;

    HIRConverter hirConverter{diag_, typeSystems};
    auto hir = hirConverter.visitAll(tops);
    if (diag_.numError() > 0)
      return nullptr;

    PendingResolvedTypeChecker typeChecker{diag_};
    hir->accept(typeChecker);
    if (diag_.numError() > 0)
      return nullptr;

    return hir;
  }

private:
  DiagnosticsEngine &diag_;
};

Sema::Sema(DiagnosticsEngine &diag) : impl_(new Impl(diag)) {}
std::shared_ptr<hir::Statement>
Sema::sematic(llvm::SmallVectorImpl<std::shared_ptr<ast::TopDecls>> &tops) {
  return impl_->sematic(tops);
}

} // namespace scriptlang
