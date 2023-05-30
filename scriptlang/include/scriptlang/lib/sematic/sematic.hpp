#ifndef SCRIPTLANG_LIB_SEMATIC_SEMATIC_HPP
#define SCRIPTLANG_LIB_SEMATIC_SEMATIC_HPP

#include "scriptlang/lib/basic/diagnostic.hpp"
#include "scriptlang/lib/parser/ast.hpp"
#include "scriptlang/lib/sematic/hir.hpp"

namespace scriptlang {

class Sema {
public:
  class Impl;

  explicit Sema(DiagnosticsEngine &diag);
  std::pair<std::shared_ptr<hir::Statement>, std::shared_ptr<TypeSystem>>
  sematic(llvm::SmallVectorImpl<std::shared_ptr<ast::TopDecls>> &tops);

private:
  std::shared_ptr<Impl> impl_;
};

} // namespace scriptlang

#endif /* SCRIPTLANG_LIB_SEMATIC_SEMATIC_HPP */
