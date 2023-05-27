#include "scriptlang/lib/codegen/codegen.hpp"
#include "scriptlang/lib/sematic/hir.hpp"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constant.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/GlobalVariable.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/raw_ostream.h"
#include <cassert>
#include <utility>

namespace scriptlang {

class ToIRVisitor : public hir::Visitor {
public:
  explicit ToIRVisitor(llvm::Module *m)
      : module_(m), builder_(m->getContext()), currentType_(nullptr), currentValue_(nullptr) {
    voidTy_ = llvm::Type::getVoidTy(m->getContext());
    int1Ty_ = llvm::Type::getInt1Ty(m->getContext());
    int8Ty_ = llvm::Type::getInt8Ty(m->getContext());
    int32Ty_ = llvm::Type::getInt32Ty(m->getContext());
    int8PtrTy_ = llvm::Type::getInt8PtrTy(m->getContext());

    int1Zero = llvm::ConstantInt::get(int1Ty_, 0, true);
    int32Zero_ = llvm::ConstantInt::get(int32Ty_, 0, true);
  }

  void run(hir::Statement *stmt) {
    llvm::FunctionType *mainEntry =
        llvm::FunctionType::get(int32Ty_, {int32Ty_, int8PtrTy_->getPointerTo()}, false);
    llvm::Function *mainFn =
        llvm::Function::Create(mainEntry, llvm::GlobalValue::ExternalLinkage, "main", module_);
    currentFn_ = mainFn;
    llvm::BasicBlock *BB = llvm::BasicBlock::Create(module_->getContext(), "entry", mainFn);

    builder_.SetInsertPoint(BB);

    stmt->accept(*this);
  }

  void visit(hir::NamedType &type) override {
    currentType_ = llvm::StringSwitch<llvm::Type *>{type.name()}
                       .Case("bool", int1Ty_)
                       .Case("i32", int32Ty_)
                       .Case("i64", llvm::Type::getInt64Ty(module_->getContext()))
                       .Case("f32", llvm::Type::getFloatTy(module_->getContext()))
                       .Case("f64", llvm::Type::getDoubleTy(module_->getContext()))
                       .Default(nullptr);
  }
  // void visit(hir::FuncType &type) override {}

  // void visit(hir::Value &value) override {}
  void visit(hir::IntegerLiteral &value) override {
    value.type()->accept(*this);
    if (!currentType_)
      return;
    auto type = currentType_;
    currentType_ = nullptr;
    currentValue_ = llvm::ConstantInt::get(type, value.value(), true);
  }
  // void visit(hir::FloatLiteral &value) override {}
  void visit(hir::Variant &value) override {
    assert(nameMap_.contains(value.decl()->name()));
    value.type()->accept(*this);
    if (!currentType_)
      return;
    auto type = currentType_;
    currentType_ = nullptr;
    currentValue_ = builder_.CreateLoad(type, nameMap_.at(value.decl()->name()));
  }
  // void visit(hir::PrefixResult &value) override {}
  void visit(hir::BinaryResult &value) override {
    value.lhs()->accept(*this);
    if (!currentValue_)
      return;
    llvm::Value *lhs = currentValue_;
    currentValue_ = nullptr;
    value.rhs()->accept(*this);
    if (!currentValue_)
      return;
    llvm::Value *rhs = currentValue_;
    currentValue_ = nullptr;
    switch (value.op()) {
    case ast::BinaryExpr::Op::ADD:
      currentValue_ = builder_.CreateAdd(lhs, rhs);
      break;
    case ast::BinaryExpr::Op::SUB:
      currentValue_ = builder_.CreateSub(lhs, rhs);
      break;
    case ast::BinaryExpr::Op::MUL:
      currentValue_ = builder_.CreateMul(lhs, rhs);
      break;
    case ast::BinaryExpr::Op::DIV:
      currentValue_ = builder_.CreateSDiv(lhs, rhs);
      break;
    case ast::BinaryExpr::Op::MOD:
      currentValue_ = builder_.CreateSRem(lhs, rhs);
      break;
    case ast::BinaryExpr::Op::LEFT_SHIFT:
      currentValue_ = builder_.CreateShl(lhs, rhs);
      break;
    case ast::BinaryExpr::Op::RIGHT_SHIFT:
      currentValue_ = builder_.CreateLShr(lhs, rhs);
      break;
    case ast::BinaryExpr::Op::LESS_THAN:
      currentValue_ = builder_.CreateICmpSLT(lhs, rhs);
      break;
    case ast::BinaryExpr::Op::GREATER_THAN:
      currentValue_ = builder_.CreateICmpSGT(lhs, rhs);
      break;
    case ast::BinaryExpr::Op::NO_LESS_THAN:
      currentValue_ = builder_.CreateICmpSGE(lhs, rhs);
      break;
    case ast::BinaryExpr::Op::NO_GREATER_THAN:
      currentValue_ = builder_.CreateICmpSLE(lhs, rhs);
      break;
    case ast::BinaryExpr::Op::EQUAL:
      currentValue_ = builder_.CreateICmpEQ(lhs, rhs);
      break;
    case ast::BinaryExpr::Op::NOT_EQUAL:
      currentValue_ = builder_.CreateICmpNE(lhs, rhs);
      break;
    case ast::BinaryExpr::Op::AND:
      currentValue_ = builder_.CreateAnd(lhs, rhs);
      break;
    case ast::BinaryExpr::Op::OR:
      currentValue_ = builder_.CreateOr(lhs, rhs);
      break;
    case ast::BinaryExpr::Op::XOR:
      currentValue_ = builder_.CreateXor(lhs, rhs);
      break;
    case ast::BinaryExpr::Op::LOGIC_AND:
      currentValue_ = builder_.CreateLogicalAnd(lhs, rhs);
      break;
    case ast::BinaryExpr::Op::LOGIC_OR:
      currentValue_ = builder_.CreateLogicalOr(lhs, rhs);
      break;
    }
  }
  // void visit(hir::CallResult &value) override {}

  void visit(hir::AssignStatement &stmt) override {
    stmt.value()->accept(*this);
    auto value = currentValue_;
    if (!currentValue_)
      return;
    if (stmt.decl()) {
      stmt.decl()->type()->accept(*this);
      if (!currentType_)
        return;
      auto type = currentType_;
      currentType_ = nullptr;
      nameMap_.insert(std::make_pair(stmt.decl()->name(), builder_.CreateAlloca(type)));
    }
    if (stmt.variant()) {
      builder_.CreateStore(value, nameMap_.at(stmt.variant()->decl()->name()));
    }

    handleNext(stmt);
  }
  void visit(hir::LoopStatement &stmt) override {
    auto loopBlock = llvm::BasicBlock::Create(module_->getContext(), "loop.start", currentFn_);
    builder_.CreateBr(loopBlock);
    builder_.SetInsertPoint(loopBlock);
    stmt.body()->accept(*this);
    builder_.CreateBr(loopBlock);
    auto loopEndBlock = llvm::BasicBlock::Create(module_->getContext(), "loop.end", currentFn_);
    builder_.SetInsertPoint(loopEndBlock);
    handleNext(stmt);
  }
  // void visit(hir::JumpStatement &stmt) override {}
  void visit(hir::ReturnStatement &stmt) override {
    stmt.value()->accept(*this);
    if (!currentValue_)
      return;
    auto returnValue = currentValue_;
    currentValue_ = nullptr;

    builder_.CreateRet(returnValue);

    handleNext(stmt);
  }
  void visit(hir::BranchStatement &stmt) override {
    stmt.condition()->accept(*this);
    if (!currentValue_)
      return;

    auto condition = currentValue_;
    auto thenBlock = llvm::BasicBlock::Create(module_->getContext(), "if.then", currentFn_);
    auto elseBlock = stmt.elseStatement()
                         ? llvm::BasicBlock::Create(module_->getContext(), "if.else", currentFn_)
                         : nullptr;
    auto endBlock = llvm::BasicBlock::Create(module_->getContext(), "if.end", currentFn_);
    builder_.CreateCondBr(builder_.CreateICmpNE(condition, int1Zero), thenBlock, elseBlock);

    builder_.SetInsertPoint(thenBlock);
    stmt.thenStatement()->accept(*this);
    builder_.CreateBr(endBlock);

    if (stmt.elseStatement()) {
      builder_.SetInsertPoint(elseBlock);
      stmt.elseStatement()->accept(*this);
      builder_.CreateBr(endBlock);
    }

    builder_.SetInsertPoint(endBlock);
  }

private:
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

  llvm::StringMap<llvm::AllocaInst *> nameMap_;

  void handleNext(hir::Statement &stmt) {
    if (static_cast<hir::Statement &>(stmt).next())
      static_cast<hir::Statement &>(stmt).next()->accept(*this);
  }
};

class CodeGenImpl {
public:
  void compile(std::shared_ptr<hir::Statement> statement) {
    llvm::LLVMContext ctx;
    std::unique_ptr<llvm::Module> m{new llvm::Module("demo", ctx)};
    ToIRVisitor visitor{m.get()};
    visitor.run(statement.get());
    m->print(llvm::outs(), nullptr);
  }
};

void CodeGen::compile(std::shared_ptr<hir::Statement> statement) { impl_->compile(statement); }

} // namespace scriptlang
