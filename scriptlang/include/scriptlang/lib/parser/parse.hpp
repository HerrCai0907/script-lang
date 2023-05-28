#ifndef SCRIPTLANG_LIB_PARSER_PARSE_HPP
#define SCRIPTLANG_LIB_PARSER_PARSE_HPP

#include "scriptlang/lib/basic/diagnostic.hpp"
#include "scriptlang/lib/parser/ast.hpp"
#include "llvm/ADT/StringRef.h"
#include <memory>

namespace scriptlang {

class ParserImpl;

class Parser {
public:
  Parser(DiagnosticsEngine &diag, llvm::SourceMgr &Code);
  std::shared_ptr<ast::TopDecls> parse();

private:
  std::shared_ptr<ParserImpl> impl_;
};

} // namespace scriptlang

#endif /* SCRIPTLANG_LIB_PARSER_PARSE_HPP */
