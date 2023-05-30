#ifndef SCRIPTLANG_LIB_CODEGEN_CODEGEN_HPP
#define SCRIPTLANG_LIB_CODEGEN_CODEGEN_HPP

#include "scriptlang/lib/sematic/hir.hpp"

namespace scriptlang {

class CodeGenImpl;

class CodeGen {
public:
  void compile(std::shared_ptr<hir::Statement> statement, std::shared_ptr<TypeSystem> typeSystem);

private:
  std::shared_ptr<CodeGenImpl> impl_;
};

} // namespace scriptlang

#endif /* SCRIPTLANG_LIB_CODEGEN_CODEGEN_HPP */
