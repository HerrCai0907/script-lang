#include "scriptlang/lib/basic/version.hpp"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/raw_ostream.h"

int main(int argc_, const char **argv_) {
  llvm::InitLLVM X(argc_, argv_);
  llvm::outs() << "Hello, I am scriptlang " << scriptlang::Version << "\n";
}