#include "scriptlang/lib/sematic/hir.hpp"
#include "scriptlang/lib/sematic/type_system.hpp"
#include "llvm/ADT/DenseMap.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"

namespace scriptlang {

class ToIRVisitor : public hir::Visitor {
public:
  ToIRVisitor(llvm::Module *m, TypeSystem *typeSystem);

  void run(hir::Statement *stmt);

  void visit(hir::FuncType &type) override;
  void visit(hir::NamedType &type) override;
  void visit(hir::PendingResolvedType &type) override;

  void visit(hir::IntegerLiteral &value) override;
  void visit(hir::FloatLiteral &value) override;
  void visit(hir::Variant &value) override;
  void visit(hir::PrefixResult &value) override;
  void visit(hir::BinaryResult &value) override;
  void visit(hir::CallResult &value) override;
  void visit(hir::FuncValue &value) override;

  void visit(hir::AssignStatement &stmt) override;
  void visit(hir::LoopStatement &stmt) override;
  void visit(hir::JumpStatement &stmt) override;
  void visit(hir::ReturnStatement &stmt) override;
  void visit(hir::BranchStatement &stmt) override;

private:
  TypeSystem *typeSystem_;

  llvm::Module *module_;

  llvm::Type *voidTy_;
  llvm::Type *int1Ty_;
  llvm::Type *int8Ty_;
  llvm::Type *int32Ty_;
  llvm::Type *int8PtrTy_;

  llvm::Constant *int1Zero;
  llvm::Constant *int32Zero_;

  llvm::IRBuilder<> builder_;

  llvm::Type *currentType_;
  llvm::Value *currentValue_;
  llvm::Function *currentFn_;

  llvm::DenseMap<hir::Decl *, llvm::AllocaInst *> declMap_;

  void handleNext(hir::Statement &stmt) {
    if (stmt.next())
      stmt.next()->accept(*this);
  }
};

} // namespace scriptlang