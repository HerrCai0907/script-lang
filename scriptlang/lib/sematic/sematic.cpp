#include "scriptlang/lib/sematic/sematic.hpp"
#include "scriptlang/lib/sematic/hir_coverter.hpp"
#include "scriptlang/lib/sematic/type_checker.hpp"
#include "scriptlang/lib/sematic/type_system.hpp"
#include "llvm/ADT/SmallVector.h"

namespace scriptlang {

class Sema::Impl {
public:
  Impl(DiagnosticsEngine &diag) : diag_(diag) {}

  std::pair<std::shared_ptr<hir::Statement>, std::shared_ptr<TypeSystem>>
  sematic(llvm::SmallVectorImpl<std::shared_ptr<ast::TopDecls>> &tops) {
    std::shared_ptr<TypeSystem> typeSystems{new TypeSystem(diag_)};
    for (auto top : tops)
      typeSystems->visit(*top);
    if (diag_.numError() > 0)
      return {nullptr, nullptr};

    HIRConverter hirConverter{diag_, *typeSystems};
    auto hir = hirConverter.visitAll(tops);
    if (diag_.numError() > 0)
      return {nullptr, nullptr};

    PendingResolvedTypeChecker typeChecker{diag_};
    hir->accept(typeChecker);
    if (diag_.numError() > 0)
      return {nullptr, nullptr};
    return {hir, typeSystems};
  }

private:
  DiagnosticsEngine &diag_;
};

Sema::Sema(DiagnosticsEngine &diag) : impl_(new Impl(diag)) {}
std::pair<std::shared_ptr<hir::Statement>, std::shared_ptr<TypeSystem>>
Sema::sematic(llvm::SmallVectorImpl<std::shared_ptr<ast::TopDecls>> &tops) {
  return impl_->sematic(tops);
}

} // namespace scriptlang
