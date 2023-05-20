#ifndef SCRIPTLANG_LIB_SEMATIC_SEMATIC_HPP
#define SCRIPTLANG_LIB_SEMATIC_SEMATIC_HPP

#include "scriptlang/lib/parser/ast.hpp"
#include "llvm/ADT/SmallVector.h"
#include <memory>

namespace scriptlang {

class Sema {
public:
  class Impl;

  Sema();
  bool sematic(llvm::SmallVectorImpl<std::shared_ptr<ast::TopDecls>> &tops);

private:
  std::shared_ptr<Impl> impl_;
};

} // namespace scriptlang

#endif /* SCRIPTLANG_LIB_SEMATIC_SEMATIC_HPP */
