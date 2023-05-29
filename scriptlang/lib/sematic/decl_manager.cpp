#include "scriptlang/lib/sematic/decl_manager.hpp"

namespace scriptlang {

bool DeclarationMgr::addDecl(std::shared_ptr<hir::Decl> decl) {
  if (findDeclByName(decl->name())) {
    return false;
  }
  scopeTypesStack_.back().insert(std::make_pair(decl->name(), decl));
  return true;
}
std::shared_ptr<hir::Decl> DeclarationMgr::findDeclByName(llvm::StringRef name) const {
  for (auto it = scopeTypesStack_.rbegin(); it != scopeTypesStack_.rend(); ++it) {
    auto ret = it->find(name);
    if (ret != it->end())
      return ret->second;
  }
  return nullptr;
}

} // namespace scriptlang
