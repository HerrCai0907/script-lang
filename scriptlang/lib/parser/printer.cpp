#include "magic_enum.hpp"
#include "scriptlang/lib/parser/ast.hpp"
#include "llvm/Support/raw_ostream.h"

namespace scriptlang {

class Printer : public ASTVisitor {
public:
  void visit(TopDecls &decls) override {
    llvm::errs() << space() << "TopDecls\n";
    RttiIndent indent{this};
    for (auto decl : decls) {
      decl->accept(*this);
    }
  }

  void visit(AssignStmt &stmt) override {
    llvm::errs() << space() << "AssignStmt\n";
    RttiIndent indent{this};
    stmt.lhs()->accept(*this);
    stmt.rhs()->accept(*this);
  }
  void visit(BlockStmt &stmt) override {
    llvm::errs() << space() << "BlockStmt\n";
    {
      RttiIndent indent{this};
      for (auto stmt : stmt)
        stmt->accept(*this);
    }
  }
  void visit(BreakStmt &) override { llvm::errs() << space() << "BreakStmt\n"; };
  void visit(ContinueStmt &) override { llvm::errs() << space() << "ContinueStmt\n"; };
  void visit(DeclStmt &stmt) override {
    llvm::errs() << space() << "DeclStmt " << magic_enum::enum_name(stmt.kind()) << " "
                 << stmt.name() << "\n";
    RttiIndent indent{this};
    if (stmt.type())
      stmt.type()->accept(*this);
    stmt.expr()->accept(*this);
  };
  void visit(ExprStmt &exprStmt) override {
    llvm::errs() << space() << "ExprStmt\n";
    RttiIndent indent{this};
    exprStmt.expr()->accept(*this);
  }

  void visit(IfStmt &stmt) override {
    llvm::errs() << space() << "IfStmt\n";
    RttiIndent indent{this};
    stmt.condition()->accept(*this);
    stmt.thenBlock()->accept(*this);
    if (stmt.elseBlock())
      stmt.elseBlock()->accept(*this);
  };
  void visit(ImportStmt &stmt) override {
    llvm::errs() << space() << "ImportStmt " << stmt.importPath() << " as " << stmt.name() << "\n";
    RttiIndent indent{this};
  };
  void visit(ReturnStmt &stmt) override {
    llvm::errs() << space() << "ReturnStmt\n";
    RttiIndent indent{this};
    stmt.value()->accept(*this);
  };
  void visit(WhileStmt &stmt) override {
    llvm::errs() << space() << "WhileStmt\n";
    RttiIndent indent{this};
    stmt.condition()->accept(*this);
    stmt.block()->accept(*this);
  };

  void visit(BinaryExpr &expr) override {
    llvm::errs() << space() << "BinaryExpr " << magic_enum::enum_name(expr.op()) << "\n";
    RttiIndent indent{this};
    expr.lhs()->accept(*this);
    expr.rhs()->accept(*this);
  }
  void visit(CallExpr &expr) override {
    llvm::errs() << space() << "CallExpr\n";
    RttiIndent indent{this};
    {
      llvm::errs() << space() << "Caller\n";
      RttiIndent indent{this};
      expr.caller()->accept(*this);
    }
    {
      llvm::errs() << space() << "Arguments\n";
      RttiIndent indent{this};
      for (auto arg : expr.arguments())
        arg->accept(*this);
    }
  }
  void visit(FuncExpr &expr) override {
    llvm::errs() << space() << "FuncExpr\n";
    RttiIndent indent{this};
    {
      llvm::errs() << space() << "Arguments\n";
      RttiIndent indent{this};
      for (size_t i = 0; i < expr.arguments().size(); i++) {
        expr.arguments()[i]->accept(*this);
        RttiIndent indent{this};
        if (expr.argumentTypes()[i])
          expr.argumentTypes()[i]->accept(*this);
      }
    }
    { expr.stmt()->accept(*this); }
  }
  void visit(Identifier &identifier) override {
    llvm::errs() << space() << "Identifier " << identifier.name() << "\n";
  }
  void visit(LiteralExpr &literal) override {
    llvm::errs() << space() << "Literal " << literal.value() << "\n";
  }
  void visit(MemberExpr &expr) override {
    llvm::errs() << space() << "MemberExpr\n";
    RttiIndent indent{this};
    expr.accessor()->accept(*this);
    expr.member()->accept(*this);
  }
  void visit(PrefixExpr &expr) override {
    llvm::errs() << space() << "PrefixExpr" << magic_enum::enum_name(expr.op()) << "\n";
    RttiIndent indent{this};
    expr.expr()->accept(*this);
  }

  void visit(NamedType &type) override {
    llvm::errs() << space() << "NamedType ";
    for (auto const &name : type.names()) {
      llvm::errs() << name;
    }
    llvm::errs() << "\n";
  }
  void visit(FunctionType &type) override {
    llvm::errs() << space() << "FunctionType\n";
    RttiIndent indent{this};
    {
      llvm::errs() << space() << "Parameters\n";
      RttiIndent indent{this};
      for (auto parameter : type.parameters()) {
        parameter->accept(*this);
      }
    }
    {
      llvm::errs() << space() << "Return\n";
      RttiIndent indent{this};
      type.returnType()->accept(*this);
    }
  }

  class RttiIndent {
  public:
    static constexpr const uint32_t INDENT_SIZE = 2U;
    explicit RttiIndent(Printer *printer) : printer_(printer) { printer_->indent_ += INDENT_SIZE; }
    ~RttiIndent() { printer_->indent_ -= INDENT_SIZE; }

  private:
    Printer *printer_;
  };

private:
  uint32_t indent_ = 0;
  std::string space() { return std::string(indent_, ' '); }
};

void AST::dump() {
  Printer printer{};
  accept(printer);
}

} // namespace scriptlang
