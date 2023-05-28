#include "scriptlang/lib/basic/diagnostic.hpp"
#include "scriptlang/lib/basic/version.hpp"
#include "scriptlang/lib/codegen/codegen.hpp"
#include "scriptlang/lib/parser/ast.hpp"
#include "scriptlang/lib/parser/parse.hpp"
#include "scriptlang/lib/sematic/sematic.hpp"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/SmallVectorMemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/raw_ostream.h"

int main(int argc_, const char **argv_) {
  llvm::InitLLVM X(argc_, argv_);
  llvm::outs() << "; scriptlang version " << scriptlang::Version << "\n";

  llvm::SourceMgr sourceMgr{};
  scriptlang::DiagnosticsEngine engine{sourceMgr};

  const char *source = R"(
    not 1;
  )";
  sourceMgr.AddNewSourceBuffer(llvm::MemoryBuffer::getMemBuffer(source), llvm::SMLoc{});

  scriptlang::Parser parser{engine, sourceMgr};
  llvm::SmallVector<std::shared_ptr<scriptlang::ast::TopDecls>, 16U> parseResults;
  auto parseResult = parser.parse();
  if (engine.numError() > 0) {
    llvm::errs() << "parser error\n";
    return 1;
  }
  assert(parseResult);
  parseResults.push_back(parseResult);

  scriptlang::Sema sema{engine};
  auto semaResult = sema.sematic(parseResults);
  if (engine.numError() > 0) {
    llvm::errs() << "sematic analysis error\n";
    return 1;
  }
  assert(semaResult);

  scriptlang::CodeGen codeGen{};
  codeGen.compile(semaResult);

  return 0;
}
