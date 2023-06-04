#ifndef SCRIPTLANG_LIB_SEMATIC_TYPE_CHECKER_HPP
#define SCRIPTLANG_LIB_SEMATIC_TYPE_CHECKER_HPP

#include "scriptlang/lib/basic/diagnostic.hpp"
#include "scriptlang/lib/sematic/hir.hpp"
#include <map>

namespace scriptlang {

class PendingResolvedTypeChecker : public hir::Visitor {
public:
  PendingResolvedTypeChecker(DiagnosticsEngine &diag) : diag_(diag) {}
  void resolve(std::shared_ptr<hir::Statement> const &stmt);

private:
  DiagnosticsEngine &diag_;

  void visit(hir::Decl &decl) override;
  void visit(hir::Value &value) override;
  void visit(hir::FuncType &value) override;
};

} // namespace scriptlang

#endif /* SCRIPTLANG_LIB_SEMATIC_TYPE_CHECKER_HPP */
