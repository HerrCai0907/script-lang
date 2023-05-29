#ifndef SCRIPTLANG_LIB_SEMATIC_DECL_MANAGER_HPP
#define SCRIPTLANG_LIB_SEMATIC_DECL_MANAGER_HPP

#include "scriptlang/lib/sematic/hir.hpp"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"
#include <vector>

namespace scriptlang {

class DeclarationMgr {
public:
  DeclarationMgr() { enterScope(); }
  bool addDecl(std::shared_ptr<hir::Decl> decl);
  std::shared_ptr<hir::Decl> findDeclByName(llvm::StringRef name) const;
  void enterScope() { scopeTypesStack_.push_back({}); }
  void exitScope() { scopeTypesStack_.pop_back(); }

private:
  std::vector<llvm::StringMap<std::shared_ptr<hir::Decl>>> scopeTypesStack_;
};

} // namespace scriptlang

#endif /* SCRIPTLANG_LIB_SEMATIC_DECL_MANAGER_HPP */
