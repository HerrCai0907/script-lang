#include "scriptlang/lib/basic/version.hpp"
#include "scriptlang/lib/parser/ast.hpp"
#include "scriptlang/lib/parser/parse.hpp"
#include "scriptlang/lib/sematic/sematic.hpp"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/raw_ostream.h"

int main(int argc_, const char **argv_) {
  llvm::InitLLVM X(argc_, argv_);
  llvm::outs() << "scriptlang version " << scriptlang::Version << "\n";

  scriptlang::Parser parser{R"(
    1 + 0x22 * 15;
  )"};
  llvm::SmallVector<std::shared_ptr<scriptlang::ast::TopDecls>, 16U> parseResults;
  auto parseResult = parser.parse();
  if (parseResult == nullptr)
    return 1;
  parseResults.push_back(parseResult);
  // parseResult->dump();

  scriptlang::Sema sema{};
  bool semaResult = sema.sematic(parseResults);
  if (semaResult == false)
    return 1;
  return 0;
}
