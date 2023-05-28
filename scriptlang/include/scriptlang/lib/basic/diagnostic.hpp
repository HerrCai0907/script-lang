#ifndef SCRIPTLANG_LIB_BASIC_DIAGNOSTIC_HPP
#define SCRIPTLANG_LIB_BASIC_DIAGNOSTIC_HPP

#include "llvm/Support/FormatVariadic.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/SourceMgr.h"
#include <cstdint>
#include <utility>

namespace scriptlang {

enum class Diag : uint32_t {
#define DIAG(ID, Level, Msg) ID,
#include "scriptlang/lib/basic/diagnostic.def"
};

class DiagnosticsEngine {
public:
  DiagnosticsEngine(llvm::SourceMgr &sourceMgr) : sourceMgr_(sourceMgr), numErrors_(0) {}

  template <class... Args> void report(llvm::SMLoc loc, Diag id, Args &&...args) {
    auto msg = llvm::formatv(getDiagnosticText(id), std::forward<Args>(args)...).str();
    auto kind = getDiagnosticKind(id);
    sourceMgr_.PrintMessage(loc, kind, msg);
    numErrors_ += (kind == llvm::SourceMgr::DK_Error);
  }

  uint32_t numError() const { return numErrors_; }

private:
  llvm::SourceMgr &sourceMgr_;
  uint32_t numErrors_;

  static const char *getDiagnosticText(Diag id);
  static llvm::SourceMgr::DiagKind getDiagnosticKind(Diag id);
};

} // namespace scriptlang

#endif /* SCRIPTLANG_LIB_BASIC_DIAGNOSTIC_HPP */
