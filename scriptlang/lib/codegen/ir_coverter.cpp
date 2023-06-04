#include "scriptlang/lib/codegen/ir_coverter.hpp"
#include "llvm/ADT/SmallVector.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/Casting.h"
#include <cassert>
#include <optional>
#include <string>

namespace scriptlang {

namespace {

static llvm::Type *typeToVariableType(llvm::Type *type) {
  if (llvm::isa<llvm::FunctionType>(type))
    return type->getPointerTo();
  else
    return type;
}

} // namespace

ToIRVisitor::ToIRVisitor(llvm::Module *m, TypeSystem *typeSystem)
    : typeSystem_(typeSystem), module_(m), builder_(m->getContext()), currentType_(nullptr),
      currentValue_(nullptr) {
  voidTy_ = llvm::Type::getVoidTy(m->getContext());
  int1Ty_ = llvm::Type::getInt1Ty(m->getContext());
  int8Ty_ = llvm::Type::getInt8Ty(m->getContext());
  int32Ty_ = llvm::Type::getInt32Ty(m->getContext());
  int8PtrTy_ = llvm::Type::getInt8PtrTy(m->getContext());

  int1Zero = llvm::ConstantInt::get(int1Ty_, 0, true);
  int32Zero_ = llvm::ConstantInt::get(int32Ty_, 0, true);
}

void ToIRVisitor::run(hir::Statement *stmt) {
  llvm::FunctionType *mainEntry =
      llvm::FunctionType::get(int32Ty_, {int32Ty_, int8PtrTy_->getPointerTo()}, false);
  llvm::Function *mainFn =
      llvm::Function::Create(mainEntry, llvm::GlobalValue::ExternalLinkage, "main", module_);
  currentFn_ = mainFn;
  llvm::BasicBlock *BB = llvm::BasicBlock::Create(module_->getContext(), "entry", mainFn);

  builder_.SetInsertPoint(BB);

  stmt->accept(*this);
}

void ToIRVisitor::visit(hir::FuncType &type) {
  type.returnType()->accept(*this);
  auto returnType = typeToVariableType(currentType_);
  assert(returnType != nullptr);
  llvm::SmallVector<llvm::Type *, 4u> argTypes;
  for (auto argType : type.argumentTypes()) {
    argType->accept(*this);
    assert(currentType_ != nullptr);
    argTypes.push_back(typeToVariableType(currentType_));
  }
  currentType_ = llvm::FunctionType::get(returnType, argTypes, false);
}
void ToIRVisitor::visit(hir::NamedType &type) {
  currentType_ = llvm::StringSwitch<llvm::Type *>{type.name()}
                     .Case("void", voidTy_)
                     .Case("bool", int1Ty_)
                     .Cases("i8", "u8", int8Ty_)
                     .Cases("i16", "u16", llvm::Type::getInt16Ty(module_->getContext()))
                     .Cases("i32", "u32", int32Ty_)
                     .Cases("i64", "u64", llvm::Type::getInt64Ty(module_->getContext()))
                     .Case("f32", llvm::Type::getFloatTy(module_->getContext()))
                     .Case("f64", llvm::Type::getDoubleTy(module_->getContext()))
                     .Default(nullptr);
  assert(currentType_ != nullptr);
}
void ToIRVisitor::visit(hir::PendingResolvedType &type) {
  llvm_unreachable("PendingResolvedType should not in code gen");
}

void ToIRVisitor::visit(hir::IntegerLiteral &value) {
  value.type()->accept(*this);
  auto type = currentType_;
  currentValue_ = llvm::ConstantInt::get(type, value.value(), typeSystem_->isSigned(value.type()));
}
void ToIRVisitor::visit(hir::FloatLiteral &value) {
  value.type()->accept(*this);
  auto type = currentType_;
  currentValue_ = llvm::ConstantFP::get(type, value.value());
}
void ToIRVisitor::visit(hir::Variant &value) {
  assert(declMap_.contains(value.decl().get()));
  value.type()->accept(*this);
  auto type = typeToVariableType(currentType_);
  currentValue_ =
      builder_.CreateLoad(type, declMap_.at(value.decl().get())); // FIXME: how to handle same name
}
void ToIRVisitor::visit(hir::PrefixResult &value) {
  value.operand()->accept(*this);
  assert(currentValue_);
  llvm::Value *operand = currentValue_;
  switch (value.getOp()) {
  case hir::PrefixResult::Op::Not:
    currentValue_ = builder_.CreateNot(operand);
    break;
  case hir::PrefixResult::Op::Minus:
    llvm::Value *lhs;
    if (typeSystem_->isSigned(value.operand()->type())) {
      lhs = llvm::ConstantInt::get(operand->getType(), 0, true);
    } else if (typeSystem_->isUnsigned(value.operand()->type())) {
      lhs = llvm::ConstantInt::get(operand->getType(), 0, false);
    } else if (typeSystem_->isFloat(value.operand()->type())) {
      lhs = llvm::ConstantFP::get(operand->getType(), 0.0);
    } else {
      llvm_unreachable("");
    }
    currentValue_ = builder_.CreateSub(lhs, operand);
    break;
  }
}
void ToIRVisitor::visit(hir::BinaryResult &value) {
  value.lhs()->accept(*this);
  assert(currentValue_);
  llvm::Value *lhs = currentValue_;
  currentValue_ = nullptr;
  value.rhs()->accept(*this);
  assert(currentValue_);
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
void ToIRVisitor::visit(hir::CallResult &value) {
  value.func()->type()->accept(*this);
  auto funcType = llvm::cast<llvm::FunctionType>(currentType_);

  value.func()->accept(*this);
  assert(currentValue_);
  auto callee = currentValue_;

  llvm::SmallVector<llvm::Value *, 4U> args{};
  for (auto const &arg : value.arguments()) {
    arg->accept(*this);
    assert(currentValue_);
    args.push_back(currentValue_);
  }

  llvm::Value *returnValue = builder_.CreateCall(funcType, callee, args);
  currentValue_ = returnValue;
}

static std::string getFuncName(std::optional<std::string> const &name) {
  static uint32_t lambdaIndex = 0;
  if (name.has_value())
    return name.value();
  else
    return "lambda#" + std::to_string(lambdaIndex++);
}

void ToIRVisitor::visit(hir::FuncValue &value) {
  llvm::Function *lastFn = currentFn_;
  llvm::BasicBlock *lastInsertBB = builder_.GetInsertBlock();
  llvm::BasicBlock::iterator lastInsertPoint = builder_.GetInsertPoint();

  value.type()->accept(*this);
  assert(currentType_);
  llvm::FunctionType *fnTy = llvm::cast<llvm::FunctionType>(currentType_);
  llvm::Function *fn = llvm::Function::Create(fnTy, llvm::GlobalValue::ExternalLinkage,
                                              getFuncName(value.getName()), module_);
  llvm::BasicBlock *BB = llvm::BasicBlock::Create(module_->getContext(), "entry", fn);

  currentFn_ = fn;
  builder_.SetInsertPoint(BB);

  value.getBody()->accept(*this);

  currentFn_ = lastFn;
  builder_.SetInsertPoint(lastInsertBB, lastInsertPoint);

  currentValue_ = llvm::ConstantExpr::getBitCast(fn, fnTy->getPointerTo());
}

void ToIRVisitor::visit(hir::AssignStatement &stmt) {
  stmt.value()->accept(*this);
  auto value = currentValue_;
  if (!currentValue_)
    return;
  if (stmt.isDecl()) {
    assert(stmt.variant());
    auto decl = stmt.variant()->decl();
    decl->type()->accept(*this);
    assert(currentType_);
    llvm::Type *type = typeToVariableType(currentType_);
    declMap_.insert(std::make_pair(decl.get(), builder_.CreateAlloca(type, nullptr, decl->name())));
  }
  if (stmt.variant()) {
    auto decl = stmt.variant()->decl();
    builder_.CreateStore(value, declMap_.at(decl.get()));
  }

  handleNext(stmt);
}
void ToIRVisitor::visit(hir::LoopStatement &stmt) {
  auto loopBlock = llvm::BasicBlock::Create(module_->getContext(), "loop.start", currentFn_);
  builder_.CreateBr(loopBlock);
  builder_.SetInsertPoint(loopBlock);
  stmt.body()->accept(*this);
  builder_.CreateBr(loopBlock);
  auto loopEndBlock = llvm::BasicBlock::Create(module_->getContext(), "loop.end", currentFn_);
  builder_.SetInsertPoint(loopEndBlock);
  handleNext(stmt);
}
void ToIRVisitor::visit(hir::JumpStatement &stmt) { llvm_unreachable("TODO"); }
void ToIRVisitor::visit(hir::ReturnStatement &stmt) {
  stmt.value()->accept(*this);
  if (!currentValue_)
    return;
  auto returnValue = currentValue_;
  currentValue_ = nullptr;

  builder_.CreateRet(returnValue);

  handleNext(stmt);
}
void ToIRVisitor::visit(hir::BranchStatement &stmt) {
  stmt.condition()->accept(*this);
  if (!currentValue_)
    return;

  auto condition = currentValue_;
  auto thenBlock = llvm::BasicBlock::Create(module_->getContext(), "if.then", currentFn_);
  auto elseBlock = stmt.elseStatement()
                       ? llvm::BasicBlock::Create(module_->getContext(), "if.else", currentFn_)
                       : nullptr;
  auto endBlock = llvm::BasicBlock::Create(module_->getContext(), "if.end", currentFn_);
  builder_.CreateCondBr(builder_.CreateICmpNE(condition, int1Zero), thenBlock,
                        stmt.elseStatement() ? elseBlock : endBlock);

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

} // namespace scriptlang
