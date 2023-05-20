#include "scriptlang/lib/sematic/hir.hpp"
#include "magic_enum.hpp"
#include "scriptlang/lib/basic/printer.hpp"
#include "llvm/Support/raw_ostream.h"

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

void Visitor::visit(FuncType &type) {
  for (auto argumentType : type.argumentTypes()) {
    argumentType->accept(*this);
  }
  type.returnType()->accept(*this);
}

void Visitor::visit(Decl &decl) { decl.type()->accept(*this); }

void Visitor::visit(Value &value) { value.type()->accept(*this); }
void Visitor::visit(IntegerLiteral &value) { visit(static_cast<Value &>(value)); }
void Visitor::visit(FloatLiteral &value) { visit(static_cast<Value &>(value)); }
void Visitor::visit(Variant &value) { visit(static_cast<Value &>(value)); }
void Visitor::visit(PrefixResult &value) {
  visit(static_cast<Value &>(value));
  value.operand()->accept(*this);
}
void Visitor::visit(BinaryResult &value) {
  visit(static_cast<Value &>(value));
  value.lhs()->accept(*this);
  value.rhs()->accept(*this);
}
void Visitor::visit(CallResult &value) {
  visit(static_cast<Value &>(value));
  value.func()->accept(*this);
  for (auto const &argument : value.arguments()) {
    argument->accept(*this);
  }
}

void Visitor::visit(Statement &stmt) {
  if (stmt.next())
    stmt.next()->accept(*this);
}
void Visitor::visit(AssignStatement &stmt) {
  if (stmt.decl())
    stmt.decl()->accept(*this);
  if (stmt.variant())
    stmt.variant()->accept(*this);
  stmt.value()->accept(*this);
  visit(static_cast<Statement &>(stmt));
}
void Visitor::visit(LoopStatement &stmt) {
  stmt.body()->accept(*this);
  visit(static_cast<Statement &>(stmt));
}
void Visitor::visit(JumpStatement &stmt) { visit(static_cast<Statement &>(stmt)); }
void Visitor::visit(ReturnStatement &stmt) {
  if (stmt.value())
    stmt.value()->accept(*this);
  visit(static_cast<Statement &>(stmt));
}
void Visitor::visit(BlockStatement &stmt) {
  if (stmt.body())
    stmt.body()->accept(*this);
  visit(static_cast<Statement &>(stmt));
}
void Visitor::visit(BranchStatement &stmt) {
  stmt.condition()->accept(*this);
  stmt.thenStatement()->accept(*this);
  if (stmt.elseStatement())
    stmt.elseStatement()->accept(*this);
  visit(static_cast<Statement &>(stmt));
}

void HIR::dump() {
  class Printer : public Visitor, public PrinterBase {
  public:
    void visit(NamedType &type) override {
      llvm::errs() << space() << type.name() << "\n";
      RttiIndent indent{this};
      Visitor::visit(type);
    }
    void visit(FuncType &type) override {
      llvm::errs() << space() << "FuncType\n";
      RttiIndent indent{this};
      for (auto argumentType : type.argumentTypes()) {
        argumentType->accept(*this);
      }
      llvm::errs() << space() << " => ";
      type.returnType()->accept(*this);
    }

    void visit(Decl &decl) override {
      llvm::errs() << space() << "Decl " << decl.name() << "\n";
      RttiIndent indent{this};
      Visitor::visit(decl);
    }

    void visit(IntegerLiteral &value) override {
      llvm::errs() << space() << "IntegerLiteral " << value.value() << "\n";
      RttiIndent indent{this};
      Visitor::visit(value);
    }
    void visit(FloatLiteral &value) override {
      llvm::errs() << space() << "FloatLiteral " << value.value() << "\n";
      RttiIndent indent{this};
      Visitor::visit(value);
    }
    void visit(Variant &value) override {
      llvm::errs() << space() << "Variant\n";
      RttiIndent indent{this};
      Visitor::visit(value);
    }
    void visit(PrefixResult &value) override {
      llvm::errs() << space() << "PrefixResult\n";
      RttiIndent indent{this};
      Visitor::visit(value);
    }
    void visit(BinaryResult &value) override {
      llvm::errs() << space() << "BinaryResult " << magic_enum::enum_name(value.op()) << "\n";
      RttiIndent indent{this};
      Visitor::visit(value);
    }
    void visit(CallResult &value) override {
      llvm::errs() << space() << "CallResult\n";
      RttiIndent indent{this};
      Visitor::visit(value);
    }

    void visit(AssignStatement &stmt) override {
      llvm::errs() << space() << "AssignStatement\n";
      RttiIndent indent{this};
      Visitor::visit(stmt);
    }
    void visit(LoopStatement &stmt) override {
      llvm::errs() << space() << "LoopStatement\n";
      RttiIndent indent{this};
      Visitor::visit(stmt);
    }
    void visit(JumpStatement &stmt) override {
      llvm::errs() << space() << "JumpStatement\n";
      RttiIndent indent{this};
      Visitor::visit(stmt);
    }
    void visit(ReturnStatement &stmt) override {
      llvm::errs() << space() << "ReturnStatement\n";
      RttiIndent indent{this};
      Visitor::visit(stmt);
    }
    void visit(BlockStatement &stmt) override {
      llvm::errs() << space() << "BlockStatement\n";
      RttiIndent indent{this};
      Visitor::visit(stmt);
    }
    void visit(BranchStatement &stmt) override {
      llvm::errs() << space() << "BranchStatement\n";
      RttiIndent indent{this};
      Visitor::visit(stmt);
    }

  } printer;

  accept(printer);
}

} // namespace scriptlang::hir
