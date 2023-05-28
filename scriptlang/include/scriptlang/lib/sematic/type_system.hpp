#ifndef SCRIPTLANG_LIB_SEMATIC_TYPE_SYSTEM_HPP
#define SCRIPTLANG_LIB_SEMATIC_TYPE_SYSTEM_HPP

#include "scriptlang/lib/basic/diagnostic.hpp"
#include "scriptlang/lib/sematic/hir.hpp"
#include "llvm/ADT/StringMap.h"
#include <list>
#include <unordered_map>

namespace scriptlang {

class TypeSystem : public ast::Visitor {
  struct FuncTypeHash {
    size_t
    operator()(std::pair<hir::FuncType::ArgumentTypes, std::shared_ptr<hir::Type>> const &v) const {
      size_t hash = 0;
      for (auto const &arg : v.first) {
        hash ^= std::hash<void *>()(arg.get());
      }
      constexpr const size_t returnTypeHashMask =
          (static_cast<size_t>(1) << (sizeof(hash) * 4)) - static_cast<size_t>(1);
      hash &= ~returnTypeHashMask;
      hash |= std::hash<void *>()(v.second.get()) & returnTypeHashMask;
      return hash;
    }
  };

  using NamedTypeMap = llvm::StringMap<std::shared_ptr<hir::NamedType>>;
  using FuncTypeMap =
      std::unordered_map<std::pair<hir::FuncType::ArgumentTypes, std::shared_ptr<hir::Type>>,
                         std::shared_ptr<hir::FuncType>, FuncTypeHash>;
  using PendingResolvedTypeList = std::list<std::shared_ptr<hir::PendingResolvedType>>;

public:
  explicit TypeSystem(DiagnosticsEngine &diag);

  std::shared_ptr<hir::NamedType> const &boolTy() const { return boolTy_; }
  std::shared_ptr<hir::NamedType> const &i8Ty() const { return i8Ty_; }
  std::shared_ptr<hir::NamedType> const &i16Ty() const { return i16Ty_; }
  std::shared_ptr<hir::NamedType> const &i32Ty() const { return i32Ty_; }
  std::shared_ptr<hir::NamedType> const &i64Ty() const { return i64Ty_; }
  std::shared_ptr<hir::NamedType> const &u8Ty() const { return u8Ty_; }
  std::shared_ptr<hir::NamedType> const &u16Ty() const { return u16Ty_; }
  std::shared_ptr<hir::NamedType> const &u32Ty() const { return u32Ty_; }
  std::shared_ptr<hir::NamedType> const &u64Ty() const { return u64Ty_; }
  std::shared_ptr<hir::NamedType> const &f32Ty() const { return f32Ty_; }
  std::shared_ptr<hir::NamedType> const &f64Ty() const { return f64Ty_; }

  bool isNumber(const std::shared_ptr<hir::Type> &type) const {
    return isUnsigned(type) || isSigned(type) || isFloat(type);
  }
  bool isUnsigned(const std::shared_ptr<hir::Type> &type) const {
    return type == u8Ty_ || type == u16Ty_ || type == u32Ty_ || type == u64Ty_;
  }
  bool isSigned(const std::shared_ptr<hir::Type> &type) const {
    return type == i8Ty_ || type == i16Ty_ || type == i32Ty_ || type == i64Ty_;
  }
  bool isFloat(const std::shared_ptr<hir::Type> &type) const {
    return type == f32Ty_ || type == f64Ty_;
  }

  std::shared_ptr<hir::FuncType> createFuncType(hir::FuncType::ArgumentTypes const &argumentTypes,
                                                std::shared_ptr<hir::Type> returnType);

  std::shared_ptr<hir::PendingResolvedType> createPendingResolvedTypeFromInteger(uint64_t num);
  std::shared_ptr<hir::PendingResolvedType> createPendingResolvedTypeFromInteger(int64_t num);

  std::shared_ptr<hir::NamedType> getTypeByName(llvm::StringRef name) const;

  enum class MergeKind { BinaryArithmetic, BinaryCompare, BinaryLogic };
  std::shared_ptr<hir::Type> mergePendingResolvedType(MergeKind kind,
                                                      std::shared_ptr<hir::Type> lhs,
                                                      std::shared_ptr<hir::Type> rhs);
  enum class ApplyKind { PrefixMinus, PrefixPlus, PrefixNot };
  std::shared_ptr<hir::Type> applyPendingResolvedType(ApplyKind kind,
                                                      std::shared_ptr<hir::Type> operand);
  void executeInfer(hir::PendingResolvedType::OnChangeCallback cb);
  void applyDefaultPendingType();

private:
  DiagnosticsEngine &diag_;
  NamedTypeMap namedTypes_;
  FuncTypeMap funcTypes_;
  PendingResolvedTypeList pendingResolvedTypes_;
  std::list<hir::PendingResolvedType::OnChangeCallback> pendingResolvedCallbacks_;

  std::shared_ptr<hir::NamedType> boolTy_;
  std::shared_ptr<hir::NamedType> i8Ty_;
  std::shared_ptr<hir::NamedType> i16Ty_;
  std::shared_ptr<hir::NamedType> i32Ty_;
  std::shared_ptr<hir::NamedType> i64Ty_;
  std::shared_ptr<hir::NamedType> u8Ty_;
  std::shared_ptr<hir::NamedType> u16Ty_;
  std::shared_ptr<hir::NamedType> u32Ty_;
  std::shared_ptr<hir::NamedType> u64Ty_;
  std::shared_ptr<hir::NamedType> f32Ty_;
  std::shared_ptr<hir::NamedType> f64Ty_;

  bool addNamedType(std::shared_ptr<hir::NamedType> const &type) {
    return namedTypes_.insert_or_assign(type->name(), type).second;
  }
};

} // namespace scriptlang

#endif /* SCRIPTLANG_LIB_SEMATIC_TYPE_SYSTEM_HPP */
