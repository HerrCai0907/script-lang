#include "scriptlang/lib/basic/version.hpp"
#include "scriptlang/lib/codegen/codegen.hpp"
#include "scriptlang/lib/parser/ast.hpp"
#include "scriptlang/lib/parser/parse.hpp"
#include "scriptlang/lib/sematic/sematic.hpp"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/raw_ostream.h"

int main(int argc_, const char **argv_) {
  llvm::InitLLVM X(argc_, argv_);
  llvm::outs() << "; scriptlang version " << scriptlang::Version << "\n";

  scriptlang::Parser parser{R"(
    let a:i32 = 10;
    while(true) {
      a = a + 1;
    }
    return a;
  )"};
  llvm::SmallVector<std::shared_ptr<scriptlang::ast::TopDecls>, 16U> parseResults;
  auto parseResult = parser.parse();
  if (parseResult == nullptr) {
    llvm::errs() << "parser error\n";
    return 1;
  }
  parseResults.push_back(parseResult);
  // parseResult->dump();

  scriptlang::Sema sema{};
  auto semaResult = sema.sematic(parseResults);
  if (semaResult == nullptr) {
    llvm::errs() << "sematic analysis error\n";
    return 1;
  }

  scriptlang::CodeGen codeGen{};
  codeGen.compile(semaResult);

  return 0;
}
