#ifndef SCRIPTLANG_LIB_SEMATIC_TYPE_CHECKER_HPP
#define SCRIPTLANG_LIB_SEMATIC_TYPE_CHECKER_HPP

#include "scriptlang/lib/basic/diagnostic.hpp"
#include "scriptlang/lib/sematic/hir.hpp"

namespace scriptlang {

class PendingResolvedTypeChecker : public hir::Visitor {
public:
  PendingResolvedTypeChecker(DiagnosticsEngine &diag) : diag_(diag) {}
  void visit(hir::Decl &decl) override;
  void visit(hir::Value &value) override;

private:
  DiagnosticsEngine &diag_;
};

} // namespace scriptlang

#endif /* SCRIPTLANG_LIB_SEMATIC_TYPE_CHECKER_HPP */
