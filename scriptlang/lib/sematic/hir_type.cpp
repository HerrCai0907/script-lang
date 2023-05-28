#include "scriptlang/lib/sematic/hir.hpp"
#include <cassert>

namespace scriptlang::hir {

bool NamedType::equal(Type const &type) const {
  auto other = dynamic_cast<const NamedType *>(&type);
  if (other == nullptr)
    return false;
  return name_ == other->name_;
}
bool FuncType::equal(Type const &type) const {
  auto other = dynamic_cast<const FuncType *>(&type);
  if (other == nullptr)
    return false;
  if (*returnType_ != *other->returnType_)
    return false;
  if (argumentTypes_.size() != other->argumentTypes_.size())
    return false;
  for (size_t i = 0; i < argumentTypes_.size(); i++)
    if (*argumentTypes_[i] != *other->argumentTypes_[i])
      return false;
  return true;
}

std::string PendingResolvedType::toString() const {
  std::string ret{};
  size_t candidateSize = candidates_.size();
  for (auto const &candidate : candidates_) {
    ret += candidate->toString();
    candidateSize--;
    if (0 != candidateSize)
      ret += " or ";
  }
  return ret;
}
std::string FuncType::toString() const {
  std::string ret{};
  ret += "(";
  for (auto const &argumentType : argumentTypes_) {
    ret += argumentType->toString();
    if (argumentType != argumentTypes_.back())
      ret += ", ";
  }
  ret += ") => ";
  ret += returnType_->toString();
  return ret;
}

void PendingResolvedType::setDefaultType(std::shared_ptr<Type> const &type) {
  for (auto const &candidate : candidates_) {
    if (*type == *candidate) {
      defaultType_ = candidate;
      return;
    }
  }
  assert(false && "default is not existed in candidates");
}

std::shared_ptr<Type> PendingResolvedType::resolve() {
  assert(canBeResolved());
  return *candidates_.begin();
}
std::shared_ptr<Type> PendingResolvedType::tryResolve() {
  if (canBeResolved())
    return resolve();
  return shared_from_this();
}

void PendingResolvedType::appendOnChangeToList(std::list<OnChangeCallback> &onChanges) {
  onChanges.insert(onChanges.end(), onChangeCallbacks_.begin(), onChangeCallbacks_.end());
}

/// @return true means PendingResolvedType is still valid
bool PendingResolvedType::removeIf(std::function<bool(std::shared_ptr<Type> const &)> condition,
                                   std::list<OnChangeCallback> &onChanges) {
  const size_t originSize = candidates_.size();
  for (auto it = candidates_.begin(); it != candidates_.end();) {
    if (condition(*it)) {
      it = candidates_.erase(it);
      if (*it == defaultType_)
        defaultType_ = nullptr;
    } else {
      ++it;
    }
  }
  const size_t currentSize = candidates_.size();
  if (currentSize != originSize)
    appendOnChangeToList(onChanges);
  return currentSize != 0;
}

/// @return true means PendingResolvedType is still valid
bool PendingResolvedType::onlyKeepCandidate(std::shared_ptr<Type> const &type,
                                            std::list<OnChangeCallback> &onChanges) {
  if (candidates_.count(type) == 0)
    return false;
  defaultType_ = nullptr;
  if (candidates_.size() == 1)
    return true;
  candidates_.clear();
  candidates_.insert(type);
  appendOnChangeToList(onChanges);
  return true;
}
void PendingResolvedType::intersection(PendingResolvedType &lhs, PendingResolvedType &rhs,
                                       std::list<OnChangeCallback> &onChanges) {
  auto lhsSize = lhs.candidates_.size();
  auto rhsSize = rhs.candidates_.size();
  for (auto it = lhs.candidates_.begin(); it != lhs.candidates_.end();) {
    if (rhs.candidates_.count(*it) == 0)
      it = lhs.candidates_.erase(it);
    else
      it++;
  }
  rhs.candidates_ = lhs.candidates_;

  if (lhs.defaultType_ != nullptr && lhs.candidates_.count(lhs.defaultType_) == 0)
    lhs.defaultType_ = nullptr;
  if (rhs.defaultType_ != nullptr && rhs.candidates_.count(lhs.defaultType_) == 0)
    rhs.defaultType_ = nullptr;

  if (lhsSize != lhs.candidates_.size())
    lhs.appendOnChangeToList(onChanges);
  if (rhsSize != rhs.candidates_.size())
    rhs.appendOnChangeToList(onChanges);
}
void PendingResolvedType::applyDefaultType(std::list<OnChangeCallback> &onChanges) {
  if (defaultType_) {
    assert(candidates_.count(defaultType_) > 0);
    candidates_.clear();
    candidates_.insert(defaultType_);
    appendOnChangeToList(onChanges);
  }
}

} // namespace scriptlang::hir
