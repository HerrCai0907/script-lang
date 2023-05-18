#include "scriptlang/lib/basic/version.hpp"
#include "scriptlang/lib/parser/parse.hpp"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/raw_ostream.h"

int main(int argc_, const char **argv_) {
  llvm::InitLLVM X(argc_, argv_);
  llvm::outs() << "scriptlang version " << scriptlang::Version << "\n";

  scriptlang::Parser parser{R"(
    const c : i32 = 10;
    export let d = 20;
    d * (a:i32,b:str) => {
      a + b;
    }(1, 2.0) + c;
  )"};
  auto parseResult = parser.parse();
  if (parseResult == nullptr) {
    return -1;
  }
  parseResult->dump();
}