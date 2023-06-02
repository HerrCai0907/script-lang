#include "scriptlang/lib/sematic/type_system.hpp"
#include "scriptlang/lib/sematic/hir.hpp"
#include <cstdint>
#include <memory>

namespace scriptlang {

TypeSystem::TypeSystem(DiagnosticsEngine &diag)
    : diag_(diag), namedTypes_(), funcTypes_(), pendingResolvedTypes_(),
      pendingResolvedCallbacks_() {
  voidTy_ = std::shared_ptr<hir::NamedType>(new hir::NamedType(llvm::StringRef("void")));
  boolTy_ = std::shared_ptr<hir::NamedType>(new hir::NamedType(llvm::StringRef("bool")));
  i8Ty_ = std::shared_ptr<hir::NamedType>(new hir::NamedType(llvm::StringRef("i8")));
  i16Ty_ = std::shared_ptr<hir::NamedType>(new hir::NamedType(llvm::StringRef("i16")));
  i32Ty_ = std::shared_ptr<hir::NamedType>(new hir::NamedType(llvm::StringRef("i32")));
  i64Ty_ = std::shared_ptr<hir::NamedType>(new hir::NamedType(llvm::StringRef("i64")));
  u8Ty_ = std::shared_ptr<hir::NamedType>(new hir::NamedType(llvm::StringRef("u8")));
  u16Ty_ = std::shared_ptr<hir::NamedType>(new hir::NamedType(llvm::StringRef("u16")));
  u32Ty_ = std::shared_ptr<hir::NamedType>(new hir::NamedType(llvm::StringRef("u32")));
  u64Ty_ = std::shared_ptr<hir::NamedType>(new hir::NamedType(llvm::StringRef("u64")));
  f32Ty_ = std::shared_ptr<hir::NamedType>(new hir::NamedType(llvm::StringRef("f32")));
  f64Ty_ = std::shared_ptr<hir::NamedType>(new hir::NamedType(llvm::StringRef("f64")));
  addNamedType(voidTy_);
  addNamedType(boolTy_);
  addNamedType(i8Ty_);
  addNamedType(i16Ty_);
  addNamedType(i32Ty_);
  addNamedType(i64Ty_);
  addNamedType(u8Ty_);
  addNamedType(u16Ty_);
  addNamedType(u32Ty_);
  addNamedType(u64Ty_);
  addNamedType(f32Ty_);
  addNamedType(f64Ty_);
}

std::shared_ptr<hir::NamedType> TypeSystem::getTypeByName(llvm::StringRef name) const {
  auto it = namedTypes_.find(name);
  if (it == namedTypes_.end()) {
    (void)diag_; // TODO
    return nullptr;
  }
  return it->second;
}

std::shared_ptr<hir::FuncType>
TypeSystem::createFuncType(hir::FuncType::ArgumentTypes const &argumentTypes,
                           std::shared_ptr<hir::Type> returnType) {
  auto key = std::make_pair(argumentTypes, returnType);
  auto it = funcTypes_.find(key);
  if (it != funcTypes_.end())
    return it->second;
  std::shared_ptr<hir::FuncType> funcType{new hir::FuncType(argumentTypes, returnType)};
  funcTypes_.insert(std::make_pair(key, funcType));
  return funcType;
}
std::shared_ptr<hir::PendingResolvedType>
TypeSystem::createPendingResolvedTypeFromInteger(uint64_t num) {
  hir::PendingResolvedType::TypeCandidates candidates{u64Ty_};
  if (num <= static_cast<uint64_t>(INT64_MAX))
    candidates.insert(i64Ty_);
  if (num <= static_cast<uint64_t>(UINT32_MAX))
    candidates.insert(u32Ty_);
  if (num <= static_cast<uint64_t>(INT32_MAX))
    candidates.insert(i32Ty_);
  if (num <= static_cast<uint64_t>(UINT16_MAX))
    candidates.insert(u16Ty_);
  if (num <= static_cast<uint64_t>(INT16_MAX))
    candidates.insert(i16Ty_);
  if (num <= static_cast<uint64_t>(UINT8_MAX))
    candidates.insert(u8Ty_);
  if (num <= static_cast<uint64_t>(INT8_MAX))
    candidates.insert(i8Ty_);
  std::shared_ptr<hir::PendingResolvedType> type{new hir::PendingResolvedType(candidates)};
  if (num <= static_cast<uint64_t>(UINT32_MAX))
    type->setDefaultType(u32Ty_);
  else
    type->setDefaultType(u64Ty_);
  pendingResolvedTypes_.push_back(type);
  return type;
}
std::shared_ptr<hir::PendingResolvedType>
TypeSystem::createPendingResolvedTypeFromInteger(int64_t num) {
  if (num >= 0)
    return createPendingResolvedTypeFromInteger(static_cast<uint64_t>(num));
  hir::PendingResolvedType::TypeCandidates candidates{i64Ty_};
  if (num >= static_cast<int32_t>(INT32_MIN))
    candidates.insert(i32Ty_);
  if (num >= static_cast<int16_t>(INT16_MIN))
    candidates.insert(i16Ty_);
  if (num >= static_cast<int8_t>(INT8_MIN))
    candidates.insert(i8Ty_);
  std::shared_ptr<hir::PendingResolvedType> type{new hir::PendingResolvedType(candidates)};
  if (num >= static_cast<int32_t>(INT32_MIN))
    type->setDefaultType(i32Ty_);
  else
    type->setDefaultType(i64Ty_);
  pendingResolvedTypes_.push_back(type);
  return type;
}
std::shared_ptr<hir::PendingResolvedType> TypeSystem::createAnyType() {
  auto type = std::shared_ptr<hir::PendingResolvedType>{
      new hir::PendingResolvedType(hir::PendingResolvedType::AnyType{})};
  pendingResolvedTypes_.push_back(type);
  return type;
}

std::shared_ptr<hir::Type> TypeSystem::mergePendingResolvedType(MergeKind kind,
                                                                std::shared_ptr<hir::Type> lhs,
                                                                std::shared_ptr<hir::Type> rhs) {
  std::function<bool(const std::shared_ptr<hir::Type> &)> condition;
  switch (kind) {
  case MergeKind::ReturnValue:
    condition = [](const std::shared_ptr<hir::Type> &type) { return true; };
    break;
  case MergeKind::BinaryArithmetic:
  case MergeKind::BinaryCompare:
    condition = [this](const std::shared_ptr<hir::Type> &type) -> bool { return isNumber(type); };
    break;
  case MergeKind::BinaryLogic:
    condition = [this](const std::shared_ptr<hir::Type> &type) -> bool {
      return isSigned(type) || isUnsigned(type);
    };
    break;
  }
  auto pendingResolvedLhs = std::dynamic_pointer_cast<hir::PendingResolvedType>(lhs);
  auto pendingResolvedRhs = std::dynamic_pointer_cast<hir::PendingResolvedType>(rhs);
  if (pendingResolvedLhs && pendingResolvedRhs) {
    auto lhsPtr = pendingResolvedLhs.get();
    auto rhsPtr = pendingResolvedRhs.get();
    auto onChange = [this, lhsPtr, rhsPtr]() {
      hir::PendingResolvedType::intersection(*lhsPtr, *rhsPtr, pendingResolvedCallbacks_);
      if (lhsPtr->canBeResolved())
        lhsPtr->clearOnChangeCallback();
      if (rhsPtr->canBeResolved())
        rhsPtr->clearOnChangeCallback();
      return;
    };
    executeInfer([&] {
      pendingResolvedLhs->removeIf(
          [condition](const std::shared_ptr<hir::Type> &type) { return !condition(type); },
          pendingResolvedCallbacks_);
      pendingResolvedRhs->removeIf(
          [condition](const std::shared_ptr<hir::Type> &type) { return !condition(type); },
          pendingResolvedCallbacks_);
      onChange();
    });
    pendingResolvedLhs->registerOnChangeCallback(onChange);
    pendingResolvedRhs->registerOnChangeCallback(onChange);
  } else if (pendingResolvedLhs) {
    if (pendingResolvedLhs->onlyKeepCandidate(rhs, pendingResolvedCallbacks_) == false)
      return nullptr;
  } else if (pendingResolvedRhs) {
    if (pendingResolvedRhs->onlyKeepCandidate(lhs, pendingResolvedCallbacks_) == false)
      return nullptr;
  } else {
    if (lhs != rhs)
      return nullptr;
  }
  return lhs;
}
std::shared_ptr<hir::Type>
TypeSystem::applyPendingResolvedType(ApplyKind kind, std::shared_ptr<hir::Type> operand) {
  std::function<bool(const std::shared_ptr<hir::Type> &)> condition;
  switch (kind) {
  case ApplyKind::PrefixMinus:
    condition = [this](const std::shared_ptr<hir::Type> &type) {
      return isSigned(type) || isFloat(type);
    };
    break;
  case ApplyKind::PrefixPlus:
    condition = [this](const std::shared_ptr<hir::Type> &type) { return isNumber(type); };
    break;
  case ApplyKind::PrefixNot:
    condition = [this](const std::shared_ptr<hir::Type> &type) { return type == boolTy_; };
    break;
  }
  if (condition(operand))
    return operand;
  auto pendingResolvedLhs = std::dynamic_pointer_cast<hir::PendingResolvedType>(operand);
  if (pendingResolvedLhs) {
    executeInfer([&]() -> void {
      pendingResolvedLhs->removeIf(
          [condition](const std::shared_ptr<hir::Type> &type) { return !condition(type); },
          pendingResolvedCallbacks_);
    });
    return operand;
  }
  return nullptr;
}

void TypeSystem::executeInfer(hir::PendingResolvedType::OnChangeCallback cb) {
  cb();
  for (auto const &pendingResolvedCallback : pendingResolvedCallbacks_)
    pendingResolvedCallback();
  pendingResolvedCallbacks_.clear();
}
void TypeSystem::applyDefaultPendingType() {
  for (auto it = pendingResolvedTypes_.begin(); it != pendingResolvedTypes_.end();) {
    if ((*it)->canBeResolved()) {
      it = pendingResolvedTypes_.erase(it);
    } else {
      (*it)->applyDefaultType(pendingResolvedCallbacks_);
      ++it;
    }
  }
  for (auto const &pendingResolvedCallback : pendingResolvedCallbacks_)
    pendingResolvedCallback();
  pendingResolvedCallbacks_.clear();
}

} // namespace scriptlang
