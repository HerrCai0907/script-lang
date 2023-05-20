#include "scriptlang/lib/parser/ast.hpp"
#include "magic_enum.hpp"
#include "scriptlang/lib/basic/printer.hpp"
#include "llvm/Support/raw_ostream.h"

namespace scriptlang::ast {

void Visitor::visit(AST &) {}

void Visitor::visit(TopDecls &decls) {
  for (auto decl : decls) {
    decl->accept(*this);
  }
}

void Visitor::visit(Stmt &) {}
void Visitor::visit(AssignStmt &stmt) {
  stmt.lhs()->accept(*this);
  stmt.rhs()->accept(*this);
}
void Visitor::visit(BlockStmt &stmt) {
  for (auto stmt : stmt)
    stmt->accept(*this);
}
void Visitor::visit(BreakStmt &) {}
void Visitor::visit(ContinueStmt &) {}
void Visitor::visit(DeclStmt &stmt) {
  if (stmt.type())
    stmt.type()->accept(*this);
  stmt.expr()->accept(*this);
}
void Visitor::visit(ExprStmt &stmt) { stmt.expr()->accept(*this); }
void Visitor::visit(IfStmt &stmt) {
  stmt.condition()->accept(*this);
  stmt.thenBlock()->accept(*this);
  if (stmt.elseBlock())
    stmt.elseBlock()->accept(*this);
}
void Visitor::visit(ImportStmt &) {}
void Visitor::visit(ReturnStmt &stmt) { stmt.value()->accept(*this); }
void Visitor::visit(WhileStmt &stmt) {
  stmt.condition()->accept(*this);
  stmt.block()->accept(*this);
}

void Visitor::visit(Expr &) {}
void Visitor::visit(BinaryExpr &expr) {
  expr.lhs()->accept(*this);
  expr.rhs()->accept(*this);
}
void Visitor::visit(CallExpr &expr) {
  expr.caller()->accept(*this);
  for (auto arg : expr.arguments())
    arg->accept(*this);
}
void Visitor::visit(FuncExpr &expr) {
  for (size_t i = 0; i < expr.arguments().size(); i++) {
    expr.arguments()[i]->accept(*this);
    if (expr.argumentTypes()[i])
      expr.argumentTypes()[i]->accept(*this);
  }
  expr.stmt()->accept(*this);
}
void Visitor::visit(Identifier &) {}
void Visitor::visit(LiteralExpr &) {}
void Visitor::visit(MemberExpr &expr) {
  expr.accessor()->accept(*this);
  expr.member()->accept(*this);
}
void Visitor::visit(PrefixExpr &expr) { expr.expr()->accept(*this); }

void Visitor::visit(TypeNode &) {}
void Visitor::visit(FunctionTypeNode &type) {
  for (auto parameter : type.parameters()) {
    parameter->accept(*this);
  }
  type.returnType()->accept(*this);
}
void Visitor::visit(NamedTypeNode &) {}

class Printer : public Visitor, public PrinterBase {
public:
  void visit(TopDecls &decls) override {
    llvm::errs() << space() << "TopDecls\n";
    RttiIndent indent{this};
    Visitor::visit(decls);
  }

  void visit(AssignStmt &stmt) override {
    llvm::errs() << space() << "AssignStmt\n";
    RttiIndent indent{this};
    Visitor::visit(stmt);
  }
  void visit(BlockStmt &stmt) override {
    llvm::errs() << space() << "BlockStmt\n";
    RttiIndent indent{this};
    Visitor::visit(stmt);
  }
  void visit(BreakStmt &stmt) override {
    llvm::errs() << space() << "BreakStmt\n";
    Visitor::visit(stmt);
  }
  void visit(ContinueStmt &stmt) override {
    llvm::errs() << space() << "ContinueStmt\n";
    Visitor::visit(stmt);
  }
  void visit(DeclStmt &stmt) override {
    llvm::errs() << space() << "DeclStmt " << magic_enum::enum_name(stmt.kind()) << " "
                 << stmt.name() << "\n";
    RttiIndent indent{this};
    Visitor::visit(stmt);
  }
  void visit(ExprStmt &stmt) override {
    llvm::errs() << space() << "ExprStmt\n";
    RttiIndent indent{this};
    Visitor::visit(stmt);
  }

  void visit(IfStmt &stmt) override {
    llvm::errs() << space() << "IfStmt\n";
    RttiIndent indent{this};
    Visitor::visit(stmt);
  };
  void visit(ImportStmt &stmt) override {
    llvm::errs() << space() << "ImportStmt " << stmt.importPath() << " as " << stmt.name() << "\n";
    RttiIndent indent{this};
    Visitor::visit(stmt);
  };
  void visit(ReturnStmt &stmt) override {
    llvm::errs() << space() << "ReturnStmt\n";
    RttiIndent indent{this};
    Visitor::visit(stmt);
  };
  void visit(WhileStmt &stmt) override {
    llvm::errs() << space() << "WhileStmt\n";
    RttiIndent indent{this};
    Visitor::visit(stmt);
  };

  void visit(BinaryExpr &expr) override {
    llvm::errs() << space() << "BinaryExpr " << magic_enum::enum_name(expr.op()) << "\n";
    RttiIndent indent{this};
    Visitor::visit(expr);
  }
  void visit(CallExpr &expr) override {
    llvm::errs() << space() << "CallExpr\n";
    RttiIndent indent{this};
    Visitor::visit(expr);
  }
  void visit(FuncExpr &expr) override {
    llvm::errs() << space() << "FuncExpr\n";
    RttiIndent indent{this};
    Visitor::visit(expr);
  }
  void visit(Identifier &expr) override {
    llvm::errs() << space() << "Identifier " << expr.name() << "\n";
    Visitor::visit(expr);
  }
  void visit(LiteralExpr &expr) override {
    llvm::errs() << space() << "Literal " << expr.value() << "\n";
    Visitor::visit(expr);
  }
  void visit(MemberExpr &expr) override {
    llvm::errs() << space() << "MemberExpr\n";
    RttiIndent indent{this};
    Visitor::visit(expr);
  }
  void visit(PrefixExpr &expr) override {
    llvm::errs() << space() << "PrefixExpr" << magic_enum::enum_name(expr.op()) << "\n";
    RttiIndent indent{this};
    Visitor::visit(expr);
  }

  void visit(NamedTypeNode &type) override {
    llvm::errs() << space() << "NamedTypeNode ";
    for (auto const &name : type.names()) {
      llvm::errs() << name;
    }
    llvm::errs() << "\n";
    Visitor::visit(type);
  }
  void visit(FunctionTypeNode &type) override {
    llvm::errs() << space() << "FunctionTypeNode\n";
    RttiIndent indent{this};
    Visitor::visit(type);
  }
};

void AST::dump() {
  Printer printer{};
  accept(printer);
}

void BinaryExpr::appendExprInRhs(BinaryExpr::Op op, std::shared_ptr<Expr> expr) {
  rhs_ = std::shared_ptr<ast::BinaryExpr>{new ast::BinaryExpr(op, rhs_, expr)};
  range_.End = expr->range().End;
}

} // namespace scriptlang::ast
