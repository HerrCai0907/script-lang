#include "scriptlang/lib/basic/diagnostic.hpp"
#include "llvm/Support/SourceMgr.h"

namespace scriptlang {

namespace {
static const char *diagnosticMessageText[] = {
#define DIAG(Id, Level, Msg) Msg,
#include "scriptlang/lib/basic/diagnostic.def"
};

static llvm::SourceMgr::DiagKind diagnosticKind[] = {
#define DIAG(Id, Level, Msg) llvm::SourceMgr::DK_##Level,
#include "scriptlang/lib/basic/diagnostic.def"
};

} // namespace

const char *DiagnosticsEngine::getDiagnosticText(Diag id) {
  return diagnosticMessageText[static_cast<size_t>(id)];
}
llvm::SourceMgr::DiagKind DiagnosticsEngine::getDiagnosticKind(Diag id) {
  return diagnosticKind[static_cast<size_t>(id)];
}

} // namespace scriptlang