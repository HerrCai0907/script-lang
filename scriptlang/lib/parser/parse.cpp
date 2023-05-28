#include "scriptlang/lib/parser/parse.hpp"
#include "antlr4_generated/scriptlangBaseListener.h"
#include "antlr4_generated/scriptlangLexer.h"
#include "scriptlang/lib/parser/ast.hpp"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/raw_ostream.h"
#include <memory>
#include <unordered_map>

namespace scriptlang {

static uint32_t getOpPriority(ast::BinaryExpr::Op op) {
  switch (op) {
  case ast::BinaryExpr::Op::MUL:
  case ast::BinaryExpr::Op::DIV:
  case ast::BinaryExpr::Op::MOD:
    return 12;
  case ast::BinaryExpr::Op::ADD:
  case ast::BinaryExpr::Op::SUB:
    return 11;
  case ast::BinaryExpr::Op::LEFT_SHIFT:
  case ast::BinaryExpr::Op::RIGHT_SHIFT:
    return 10;
  case ast::BinaryExpr::Op::LESS_THAN:
  case ast::BinaryExpr::Op::GREATER_THAN:
  case ast::BinaryExpr::Op::NO_LESS_THAN:
  case ast::BinaryExpr::Op::NO_GREATER_THAN:
    return 9;
  case ast::BinaryExpr::Op::EQUAL:
  case ast::BinaryExpr::Op::NOT_EQUAL:
    return 8;
  case ast::BinaryExpr::Op::AND:
    return 7;
  case ast::BinaryExpr::Op::XOR:
    return 6;
  case ast::BinaryExpr::Op::OR:
    return 5;
  case ast::BinaryExpr::Op::LOGIC_AND:
    return 4;
  case ast::BinaryExpr::Op::LOGIC_OR:
    return 3;
  }
  llvm_unreachable("unknown operator");
}

ast::BinaryExpr::Op getOpFromContext(scriptlangParser::BinaryOperatorContext *ctx) noexcept {
  if (ctx->Plus() != nullptr) {
    return ast::BinaryExpr::Op::ADD;
  } else if (ctx->Minus() != nullptr) {
    return ast::BinaryExpr::Op::SUB;
  } else if (ctx->Star() != nullptr) {
    return ast::BinaryExpr::Op::MUL;
  } else if (ctx->Div() != nullptr) {
    return ast::BinaryExpr::Op::DIV;
  } else if (ctx->Mod() != nullptr) {
    return ast::BinaryExpr::Op::MOD;
  } else if (ctx->LeftShift() != nullptr) {
    return ast::BinaryExpr::Op::LEFT_SHIFT;
  } else if (ctx->RightShift() != nullptr) {
    return ast::BinaryExpr::Op::RIGHT_SHIFT;
  } else if (ctx->Less() != nullptr) {
    return ast::BinaryExpr::Op::LESS_THAN;
  } else if (ctx->Greater() != nullptr) {
    return ast::BinaryExpr::Op::GREATER_THAN;
  } else if (ctx->LessEqual() != nullptr) {
    return ast::BinaryExpr::Op::NO_GREATER_THAN;
  } else if (ctx->GreaterEqual() != nullptr) {
    return ast::BinaryExpr::Op::NO_LESS_THAN;
  } else if (ctx->Equal() != nullptr) {
    return ast::BinaryExpr::Op::EQUAL;
  } else if (ctx->NotEqual() != nullptr) {
    return ast::BinaryExpr::Op::NOT_EQUAL;
  } else if (ctx->And() != nullptr) {
    return ast::BinaryExpr::Op::AND;
  } else if (ctx->Caret() != nullptr) {
    return ast::BinaryExpr::Op::XOR;
  } else if (ctx->Or() != nullptr) {
    return ast::BinaryExpr::Op::OR;
  } else if (ctx->AndAnd() != nullptr) {
    return ast::BinaryExpr::Op::LOGIC_AND;
  } else if (ctx->OrOr() != nullptr) {
    return ast::BinaryExpr::Op::LOGIC_OR;
  }
  llvm_unreachable("unknown op");
}

class AntlrListener : public scriptlangBaseListener {
public:
  AntlrListener(llvm::StringRef code) : code_(code) {}

  void exitScriptlang(scriptlangParser::ScriptlangContext *ctx) override {
    ast::TopDecls::TopDeclsVec vec;
    for (auto statement : ctx->statement())
      vec.push_back(getAstByContext<ast::Stmt>(statement));
    ast_.reset(new ast::TopDecls(getRange(ctx), std::move(vec)));
  }

  void exitLiteral(scriptlangParser::LiteralContext *ctx) override {
    ast::LiteralExpr::Kind kind;
    if (ctx->StringLiteral())
      kind = ast::LiteralExpr::Kind::String;
    else if (ctx->FloatNumber())
      kind = ast::LiteralExpr::Kind::Float;
    else if (ctx->HexNumber())
      kind = ast::LiteralExpr::Kind::Hex;
    else if (ctx->IntNumber())
      kind = ast::LiteralExpr::Kind::Integer;
    else
      llvm_unreachable("unknown literal");
    map_.emplace(ctx, std::shared_ptr<ast::LiteralExpr>{
                          new ast::LiteralExpr(getRange(ctx), kind, ctx->getText())});
  }

  void exitIdentifier(scriptlangParser::IdentifierContext *ctx) override {
    map_.emplace(ctx, std::shared_ptr<ast::Identifier>{
                          new ast::Identifier(getRange(ctx), ctx->Identifier()->getText())});
  }

  void exitType(scriptlangParser::TypeContext *ctx) override { handleCombineNode(ctx); }

  void exitBaseType(scriptlangParser::BaseTypeContext *ctx) override {
    ast::NamedTypeNode::NamedTypeVec vec;
    for (auto id : ctx->Identifier())
      vec.push_back(id->getText());
    map_.emplace(ctx, std::shared_ptr<ast::NamedTypeNode>{
                          new ast::NamedTypeNode(getRange(ctx), std::move(vec))});
  }

  void exitFunctionType(scriptlangParser::FunctionTypeContext *ctx) override {
    ast::FunctionTypeNode::FunctionTypeParameterVec vec;
    for (auto parameter : ctx->parameterWithType())
      vec.push_back(getAstByContext<ast::TypeNode>(parameter->type()));
    map_.emplace(ctx,
                 std::shared_ptr<ast::FunctionTypeNode>{new ast::FunctionTypeNode(
                     getRange(ctx), std::move(vec), getAstByContext<ast::TypeNode>(ctx->type()))});
  }

  void exitStatement(scriptlangParser::StatementContext *ctx) override { handleCombineNode(ctx); }

  void exitExpressionStatement(scriptlangParser::ExpressionStatementContext *ctx) override {
    map_.emplace(ctx, std::shared_ptr<ast::ExprStmt>{new ast::ExprStmt(
                          getRange(ctx), getAstByContext<ast::Expr>(ctx->expression()))});
  }

  void exitDeclareStatement(scriptlangParser::DeclareStatementContext *ctx) override {
    ast::DeclStmt::Kind kind = ast::DeclStmt::Kind::Normal;
    if (ctx->EXPORT() && ctx->CONST())
      kind = ast::DeclStmt::Kind::ExportConst;
    else if (ctx->EXPORT())
      kind = ast::DeclStmt::Kind::Export;
    else if (ctx->CONST())
      kind = ast::DeclStmt::Kind::Const;

    auto type = ctx->type() ? getAstByContext<ast::TypeNode>(ctx->type()) : nullptr;
    map_.emplace(ctx, std::shared_ptr<ast::DeclStmt>(
                          new ast::DeclStmt(getRange(ctx), kind, ctx->Identifier()->getText(), type,
                                            getAstByContext<ast::Expr>(ctx->expression()))));
  }

  void exitAssignStatement(scriptlangParser::AssignStatementContext *ctx) override {
    map_.emplace(ctx, std::shared_ptr<ast::AssignStmt>(new ast::AssignStmt(
                          getRange(ctx), getAstByContext<ast::Expr>(ctx->expression(0U)),
                          getAstByContext<ast::Expr>(ctx->expression(1U)))));
  }

  void exitReturnStatement(scriptlangParser::ReturnStatementContext *ctx) override {
    auto returnValue = ctx->expression() ? getAstByContext<ast::Expr>(ctx->expression()) : nullptr;
    map_.emplace(ctx,
                 std::shared_ptr<ast::ReturnStmt>(new ast::ReturnStmt(getRange(ctx), returnValue)));
  }

  void exitBlockStatement(scriptlangParser::BlockStatementContext *ctx) override {
    ast::BlockStmt::StmtsVec stmts;
    for (auto statementContext : ctx->statement())
      stmts.push_back(getAstByContext<ast::Stmt>(statementContext));
    map_.emplace(
        ctx, std::shared_ptr<ast::BlockStmt>{new ast::BlockStmt(getRange(ctx), std::move(stmts))});
  }

  void exitIfStatement(scriptlangParser::IfStatementContext *ctx) override {
    auto thenBranch = ctx->blockStatement().size() == 2U
                          ? getAstByContext<ast::Stmt>(ctx->blockStatement(1))
                      : ctx->ifStatement() ? getAstByContext<ast::Stmt>(ctx->ifStatement())
                                           : nullptr;
    map_.emplace(ctx, std::shared_ptr<ast::IfStmt>(new ast::IfStmt(
                          getRange(ctx), getAstByContext<ast::Expr>(ctx->expression()),
                          getAstByContext<ast::BlockStmt>(ctx->blockStatement(0)), thenBranch)));
  }

  void exitWhileStatement(scriptlangParser::WhileStatementContext *ctx) override {
    map_.emplace(ctx, std::shared_ptr<ast::WhileStmt>(new ast::WhileStmt(
                          getRange(ctx), getAstByContext<ast::Expr>(ctx->expression()),
                          getAstByContext<ast::BlockStmt>(ctx->blockStatement()))));
  }

  void exitBreakStatement(scriptlangParser::BreakStatementContext *ctx) override {
    map_.emplace(ctx, std::shared_ptr<ast::BreakStmt>(new ast::BreakStmt(getRange(ctx))));
  }

  void exitContinueStatement(scriptlangParser::ContinueStatementContext *ctx) override {
    map_.emplace(ctx, std::shared_ptr<ast::ContinueStmt>(new ast::ContinueStmt(getRange(ctx))));
  }

  void exitImportStatement(scriptlangParser::ImportStatementContext *ctx) override {
    map_.emplace(
        ctx, std::shared_ptr<ast::ImportStmt>(new ast::ImportStmt(
                 getRange(ctx), ctx->Identifier()->getText(), ctx->StringLiteral()->getText())));
  }

  void exitSimpleExpression(scriptlangParser::SimpleExpressionContext *ctx) override {
    handleCombineNode(ctx);
  }

  void exitPrefixExpressionOperand(scriptlangParser::PrefixExpressionOperandContext *ctx) override {
    handleCombineNode(ctx);
  }
  void exitPrefixExpression(scriptlangParser::PrefixExpressionContext *ctx) override {
    ast::PrefixExpr::Op kind;
    if (ctx->prefixOperator()->Minus())
      kind = ast::PrefixExpr::Op::Minus;
    else if (ctx->prefixOperator()->Plus())
      kind = ast::PrefixExpr::Op::Plus;
    else if (ctx->prefixOperator()->NOT())
      kind = ast::PrefixExpr::Op ::Not;
    else
      llvm_unreachable("unknown prefix op");
    map_.emplace(
        ctx, std::shared_ptr<ast::PrefixExpr>{new ast::PrefixExpr(
                 getRange(ctx), kind, getAstByContext<ast::Expr>(ctx->prefixExpressionOperand()))});
  }

  void exitParenthesesExpression(scriptlangParser::ParenthesesExpressionContext *ctx) override {
    map_.emplace(ctx, getAstByContext<ast::Expr>(ctx->expression()));
  }

  void exitBinaryExpressionLeft(scriptlangParser::BinaryExpressionLeftContext *ctx) override {
    handleCombineNode(ctx);
  }
  void exitBinaryExpressionRight(scriptlangParser::BinaryExpressionRightContext *ctx) override {
    handleCombineNode(ctx);
  }
  void exitBinaryExpression(scriptlangParser::BinaryExpressionContext *ctx) override {
    std::shared_ptr<ast::BinaryExpr> binaryExpr;
    for (auto binaryRightWithOp : ctx->binaryExpressionRightWithOp()) {
      ast::BinaryExpr::Op op = getOpFromContext(binaryRightWithOp->binaryOperator());
      std::shared_ptr<ast::Expr> rightExpr =
          getAstByContext<ast::Expr>(binaryRightWithOp->binaryExpressionRight());
      if (binaryExpr == nullptr) {
        binaryExpr = std::shared_ptr<ast::BinaryExpr>{new ast::BinaryExpr(
            op, getAstByContext<ast::Expr>(ctx->binaryExpressionLeft()), rightExpr)};
      } else {
        if (getOpPriority(binaryExpr->op()) >= getOpPriority(op)) {
          // 1 * 2 + 3 or 1 + 2 + 3
          binaryExpr =
              std::shared_ptr<ast::BinaryExpr>{new ast::BinaryExpr(op, binaryExpr, rightExpr)};
        } else {
          // 1 + 2 * 3
          binaryExpr->appendExprInRhs(op, rightExpr);
        }
      }
    }
    map_.emplace(ctx, binaryExpr);
  }

  void exitCallOrMemberExpressionLeft(
      scriptlangParser::CallOrMemberExpressionLeftContext *ctx) override {
    handleCombineNode(ctx);
  }
  ast::CallExpr::ArgumentsVec
  handleCallExpressionRight(scriptlangParser::CallExpressionRightContext *callRightContext) {
    ast::CallExpr::ArgumentsVec argument;
    for (auto exprContext : callRightContext->expression()) {
      argument.push_back(getAstByContext<ast::Expr>(exprContext));
    }
    return argument;
  }
  template <class T> std::shared_ptr<ast::Expr> handleCallOrMemberExpressionLeft(T *ctx) {
    static_assert(std::is_same_v<T, scriptlangParser::CallExpressionContext> ||
                  std::is_same_v<T, scriptlangParser::MemberExpressionContext>);
    std::shared_ptr<ast::Expr> callerOrAccessor =
        getAstByContext<ast::Expr>(ctx->callOrMemberExpressionLeft());
    for (auto *rightContext : ctx->callOrMemberExpressionRight()) {
      if (rightContext->callExpressionRight()) {
        callerOrAccessor = std::shared_ptr<ast::CallExpr>{new ast::CallExpr(
            llvm::SMRange{getRange(ctx).Start, getRange(rightContext).End}, callerOrAccessor,
            handleCallExpressionRight(rightContext->callExpressionRight()))};
      } else if (rightContext->memberExpressionRight()) {
        callerOrAccessor = std::shared_ptr<ast::MemberExpr>{new ast::MemberExpr(
            llvm::SMRange{getRange(ctx).Start, getRange(rightContext).End}, callerOrAccessor,
            getAstByContext<ast::Identifier>(rightContext->memberExpressionRight()->identifier()))};
      } else {
        llvm_unreachable("unknown callOrMemberExpressionRight");
      }
    }
    return callerOrAccessor;
  }
  void exitCallExpression(scriptlangParser::CallExpressionContext *ctx) override {
    map_.emplace(ctx, std::shared_ptr<ast::CallExpr>{new ast::CallExpr(
                          getRange(ctx), handleCallOrMemberExpressionLeft(ctx),
                          handleCallExpressionRight(ctx->callExpressionRight()))});
  }
  void exitMemberExpression(scriptlangParser::MemberExpressionContext *ctx) override {
    map_.emplace(
        ctx, std::shared_ptr<ast::MemberExpr>{new ast::MemberExpr(
                 getRange(ctx), handleCallOrMemberExpressionLeft(ctx),
                 getAstByContext<ast::Identifier>(ctx->memberExpressionRight()->identifier()))});
  }

  void exitFunctionExpression(scriptlangParser::FunctionExpressionContext *ctx) override {
    ast::FuncExpr::ArgumentsVec arguments;
    ast::FuncExpr::ArgumentTypesVec types;
    for (auto parameterContext : ctx->parameterWithIdentifier()) {
      arguments.push_back(getAstByContext<ast::Identifier>(parameterContext->identifier()));
      if (parameterContext->type()) {
        types.push_back(getAstByContext<ast::TypeNode>(parameterContext->type()));
      }
    }
    map_.emplace(ctx, std::shared_ptr<ast::FuncExpr>{new ast::FuncExpr(
                          getRange(ctx), std::move(arguments), std::move(types),
                          getAstByContext<ast::BlockStmt>(ctx->blockStatement()))});
  }

  void exitExpression(scriptlangParser::ExpressionContext *ctx) override { handleCombineNode(ctx); }

  operator std::shared_ptr<ast::TopDecls>() { return std::move(ast_); }

private:
  llvm::StringRef code_;
  std::shared_ptr<ast::TopDecls> ast_;
  std::unordered_map<antlr4::ParserRuleContext const *, std::shared_ptr<ast::AST>> map_;

  template <class TargetType>
  std::shared_ptr<TargetType> getAstByContext(antlr4::ParserRuleContext const *ctx) const {
    auto it = map_.find(ctx);
    assert(it != map_.end() && "not find ctx in map_");
    return std::dynamic_pointer_cast<TargetType>(it->second);
  }
  void handleCombineNode(antlr4::ParserRuleContext const *ctx) {
    auto *child = dynamic_cast<antlr4::ParserRuleContext *>(ctx->children.at(0));
    map_.emplace(ctx, getAstByContext<ast::AST>(child));
  }
  llvm::SMRange getRange(antlr4::ParserRuleContext const *ctx) const {
    return llvm::SMRange{
        llvm::SMLoc::getFromPointer(&code_.data()[ctx->getStart()->getStartIndex()]),
        llvm::SMLoc::getFromPointer(&code_.data()[ctx->getStop()->getStopIndex()])};
  }
};

class ParserImpl {
public:
  ParserImpl(DiagnosticsEngine &diag, llvm::SourceMgr &sourceMgr)
      : diag_(diag), sourceMgr_(sourceMgr) {}

  std::shared_ptr<ast::TopDecls> parse() {
    auto mainSource = sourceMgr_.getMemoryBuffer(sourceMgr_.getMainFileID())->getBuffer();

    antlr4::ANTLRInputStream inputStream{mainSource.data(), mainSource.size()};
    scriptlangLexer lexer{&inputStream};
    antlr4::CommonTokenStream tokens{&lexer};
    scriptlangParser parser(&tokens);

    // TODO(better error message)
    class ErrorListener : public antlr4::BaseErrorListener {
    public:
      explicit ErrorListener(DiagnosticsEngine &diag, llvm::StringRef source)
          : diag_(diag), source_(source) {}
      void syntaxError(antlr4::Recognizer *recognizer, antlr4::Token *offendingSymbol, size_t line,
                       size_t charPositionInLine, const std::string &msg,
                       std::exception_ptr e) override {
        llvm::SMLoc loc{};
        if (offendingSymbol)
          loc = llvm::SMLoc::getFromPointer(&source_.data()[offendingSymbol->getStartIndex()]);
        diag_.report(loc, Diag::unknown_token, msg.c_str());
      }
      void reportAmbiguity(antlr4::Parser *recognizer, const antlr4::dfa::DFA &dfa,
                           size_t startIndex, size_t stopIndex, bool exact,
                           const antlrcpp::BitSet &ambigAlts,
                           antlr4::atn::ATNConfigSet *configs) override {
        assert(false);
      }

    private:
      DiagnosticsEngine &diag_;
      llvm::StringRef source_;
    };
    ErrorListener errorListener{diag_, mainSource};

    parser.removeErrorListeners();
    parser.addErrorListener(&errorListener);

    AntlrListener listener{mainSource};
    try {
      antlr4::tree::ParseTreeWalker::DEFAULT.walk(&listener, parser.scriptlang());
    } catch (...) {
    }
    return listener;
  }

private:
  DiagnosticsEngine &diag_;
  llvm::SourceMgr &sourceMgr_;
};

Parser::Parser(DiagnosticsEngine &diag, llvm::SourceMgr &sourceMgr)
    : impl_(new ParserImpl(diag, sourceMgr)) {}

std::shared_ptr<ast::TopDecls> Parser::parse() { return impl_->parse(); }

} // namespace scriptlang
