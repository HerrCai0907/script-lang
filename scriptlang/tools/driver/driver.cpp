#include "scriptlang/lib/basic/diagnostic.hpp"
#include "scriptlang/lib/basic/version.hpp"
#include "scriptlang/lib/codegen/codegen.hpp"
#include "scriptlang/lib/parser/ast.hpp"
#include "scriptlang/lib/parser/parse.hpp"
#include "scriptlang/lib/sematic/sematic.hpp"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/SmallVectorMemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"
#include "llvm/Support/WithColor.h"
#include "llvm/Support/raw_ostream.h"

static const char *Head = "scriptlang - scriptlang compiler";

static llvm::cl::list<std::string> inputFiles(llvm::cl::Positional,
                                              llvm::cl::desc("<input-files>"));

int main(int argc, const char **argv) {
  llvm::InitLLVM X(argc, argv);

  llvm::cl::SetVersionPrinter(
      [](llvm::raw_ostream &OS) { OS << "scriptlang version " << scriptlang::Version << "\n"; });
  llvm::cl::ParseCommandLineOptions(argc, argv, Head);

  llvm::SourceMgr sourceMgr{};
  scriptlang::DiagnosticsEngine engine{sourceMgr};

  for (const auto &file : inputFiles) {
    llvm::ErrorOr<std::unique_ptr<llvm::MemoryBuffer>> FileOrErr =
        llvm::MemoryBuffer::getFile(file);
    if (std::error_code BufferError = FileOrErr.getError()) {
      llvm::WithColor::error(llvm::errs(), argv[0])
          << "Error reading " << file << ": " << BufferError.message() << "\n";
      continue;
    }
    sourceMgr.AddNewSourceBuffer(std::move(*FileOrErr), llvm::SMLoc());
  }

  if (sourceMgr.getNumBuffers() > 0) {
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
    assert(semaResult.first);
    assert(semaResult.second);

    scriptlang::CodeGen codeGen{};
    codeGen.compile(semaResult.first, semaResult.second);
  }
  return 0;
}
