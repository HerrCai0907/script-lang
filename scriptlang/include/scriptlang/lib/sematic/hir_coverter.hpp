#ifndef SCRIPTLANG_LIB_SEMATIC_HIR_COVERTER_HPP
#define SCRIPTLANG_LIB_SEMATIC_HIR_COVERTER_HPP

#include "scriptlang/lib/parser/ast.hpp"
#include "scriptlang/lib/sematic/decl_manager.hpp"
#include "scriptlang/lib/sematic/hir.hpp"
#include "scriptlang/lib/sematic/sematic.hpp"
#include "scriptlang/lib/sematic/type_system.hpp"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"
#include <memory>
#include <string>
#include <utility>

namespace scriptlang {

class HIRConverter : public ast::Visitor {
public:
  HIRConverter(DiagnosticsEngine &diag, TypeSystem &typeSystem)
      : diag_(diag), typeSystem_(typeSystem), declarationMgr_(), exprResult_(), stmtResult_() {}

  std::shared_ptr<hir::Statement>
  visitAll(llvm::SmallVectorImpl<std::shared_ptr<ast::TopDecls>> const &tops);

  void visit(ast::TopDecls &topDecls) override;
  void visit(ast::AssignStmt &stmt) override;
  void visit(ast::BlockStmt &stmt) override;
  void visit(ast::BreakStmt &) override;
  void visit(ast::ContinueStmt &) override;
  void visit(ast::DeclStmt &stmt) override;
  void visit(ast::ExprStmt &stmt) override;
  void visit(ast::IfStmt &stmt) override;
  void visit(ast::ImportStmt &stmt) override;
  void visit(ast::ReturnStmt &stmt) override;
  void visit(ast::WhileStmt &stmt) override;

  std::shared_ptr<hir::Type> handTypeNode(std::shared_ptr<ast::TypeNode> const &typeNode);

  void visit(ast::BinaryExpr &expr) override;
  void visit(ast::CallExpr &expr) override;
  void visit(ast::FuncExpr &expr) override;
  void visit(ast::Identifier &expr) override;
  void visit(ast::LiteralExpr &expr) override;
  void visit(ast::MemberExpr &expr) override;
  void visit(ast::PrefixExpr &expr) override;

private:
  DiagnosticsEngine &diag_;

  TypeSystem &typeSystem_;
  DeclarationMgr declarationMgr_;

  std::shared_ptr<hir::Value> exprResult_;
  std::shared_ptr<hir::Statement> stmtResult_;
};

} // namespace scriptlang

#endif /* SCRIPTLANG_LIB_SEMATIC_HIR_COVERTER_HPP */
