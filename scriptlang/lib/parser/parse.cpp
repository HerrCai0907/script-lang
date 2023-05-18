#include "scriptlang/lib/parser/parse.hpp"
#include "antlr4_generated/scriptlangBaseListener.h"
#include "antlr4_generated/scriptlangLexer.h"
#include "scriptlang/lib/parser/ast.hpp"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/SMLoc.h"
#include "llvm/Support/raw_ostream.h"
#include <exception>
#include <memory>
#include <type_traits>
#include <unordered_map>

namespace scriptlang {

static uint32_t getOpPriority(BinaryExpr::Op op) {
  switch (op) {
  case BinaryExpr::Op::MUL:
  case BinaryExpr::Op::DIV:
  case BinaryExpr::Op::MOD:
    return 12;
  case BinaryExpr::Op::ADD:
  case BinaryExpr::Op::SUB:
    return 11;
  case BinaryExpr::Op::LEFT_SHIFT:
  case BinaryExpr::Op::RIGHT_SHIFT:
    return 10;
  case BinaryExpr::Op::LESS_THAN:
  case BinaryExpr::Op::GREATER_THAN:
  case BinaryExpr::Op::NO_LESS_THAN:
  case BinaryExpr::Op::NO_GREATER_THAN:
    return 9;
  case BinaryExpr::Op::EQUAL:
  case BinaryExpr::Op::NOT_EQUAL:
    return 8;
  case BinaryExpr::Op::AND:
    return 7;
  case BinaryExpr::Op::XOR:
    return 6;
  case BinaryExpr::Op::OR:
    return 5;
  case BinaryExpr::Op::LOGIC_AND:
    return 4;
  case BinaryExpr::Op::LOGIC_OR:
    return 3;
  }
  llvm_unreachable("unknown operator");
}

BinaryExpr::Op getOpFromContext(scriptlangParser::BinaryOperatorContext *ctx) noexcept {
  if (ctx->Plus() != nullptr) {
    return BinaryExpr::Op::ADD;
  } else if (ctx->Minus() != nullptr) {
    return BinaryExpr::Op::SUB;
  } else if (ctx->Star() != nullptr) {
    return BinaryExpr::Op::MUL;
  } else if (ctx->Div() != nullptr) {
    return BinaryExpr::Op::DIV;
  } else if (ctx->Mod() != nullptr) {
    return BinaryExpr::Op::MOD;
  } else if (ctx->LeftShift() != nullptr) {
    return BinaryExpr::Op::LEFT_SHIFT;
  } else if (ctx->RightShift() != nullptr) {
    return BinaryExpr::Op::RIGHT_SHIFT;
  } else if (ctx->Less() != nullptr) {
    return BinaryExpr::Op::LESS_THAN;
  } else if (ctx->Greater() != nullptr) {
    return BinaryExpr::Op::GREATER_THAN;
  } else if (ctx->LessEqual() != nullptr) {
    return BinaryExpr::Op::NO_GREATER_THAN;
  } else if (ctx->GreaterEqual() != nullptr) {
    return BinaryExpr::Op::NO_LESS_THAN;
  } else if (ctx->Equal() != nullptr) {
    return BinaryExpr::Op::EQUAL;
  } else if (ctx->NotEqual() != nullptr) {
    return BinaryExpr::Op::NOT_EQUAL;
  } else if (ctx->And() != nullptr) {
    return BinaryExpr::Op::AND;
  } else if (ctx->Caret() != nullptr) {
    return BinaryExpr::Op::XOR;
  } else if (ctx->Or() != nullptr) {
    return BinaryExpr::Op::OR;
  } else if (ctx->AndAnd() != nullptr) {
    return BinaryExpr::Op::LOGIC_AND;
  } else if (ctx->OrOr() != nullptr) {
    return BinaryExpr::Op::LOGIC_OR;
  }
  llvm_unreachable("unknown op");
}

void appendExprInRhs(BinaryExpr &binaryExpr, BinaryExpr::Op op, std::shared_ptr<Expr> expr) {
  binaryExpr.rhs_ = std::shared_ptr<BinaryExpr>{new BinaryExpr(op, binaryExpr.rhs_, expr)};
  binaryExpr.range_.End = expr->range().End;
}

class AntlrListener : public scriptlangBaseListener {
public:
  AntlrListener(llvm::StringRef code) : code_(code) {}

  void exitScriptlang(scriptlangParser::ScriptlangContext *ctx) override {
    TopDecls::TopDeclsVec vec;
    for (auto statement : ctx->statement())
      vec.push_back(getAstByContext<Stmt>(statement));
    ast_.reset(new TopDecls(getRange(ctx), std::move(vec)));
  }

  void exitLiteral(scriptlangParser::LiteralContext *ctx) override {
    LiteralExpr::Kind kind;
    if (ctx->StringLiteral())
      kind = LiteralExpr::Kind::String;
    else if (ctx->FloatNumber())
      kind = LiteralExpr::Kind::Float;
    else if (ctx->HexNumber())
      kind = LiteralExpr::Kind::Hex;
    else if (ctx->IntNumber())
      kind = LiteralExpr::Kind::Integer;
    else
      llvm_unreachable("unknown literal");
    map_.emplace(
        ctx, std::shared_ptr<LiteralExpr>{new LiteralExpr(getRange(ctx), kind, ctx->getText())});
  }

  void exitIdentifier(scriptlangParser::IdentifierContext *ctx) override {
    map_.emplace(ctx, std::shared_ptr<Identifier>{
                          new Identifier(getRange(ctx), ctx->Identifier()->getText())});
  }

  void exitType(scriptlangParser::TypeContext *ctx) override { handleCombineNode(ctx); }

  void exitBaseType(scriptlangParser::BaseTypeContext *ctx) override {
    NamedType::NamedTypeVec vec;
    for (auto id : ctx->Identifier())
      vec.push_back(id->getText());
    map_.emplace(ctx, std::shared_ptr<NamedType>{new NamedType(getRange(ctx), std::move(vec))});
  }

  void exitFunctionType(scriptlangParser::FunctionTypeContext *ctx) override {
    FunctionType::FunctionTypeParameterVec vec;
    for (auto parameter : ctx->parameterWithType())
      vec.push_back(getAstByContext<Type>(parameter->type()));
    map_.emplace(ctx, std::shared_ptr<FunctionType>{new FunctionType(
                          getRange(ctx), std::move(vec), getAstByContext<Type>(ctx->type()))});
  }

  void exitStatement(scriptlangParser::StatementContext *ctx) override { handleCombineNode(ctx); }

  void exitExpressionStatement(scriptlangParser::ExpressionStatementContext *ctx) override {
    map_.emplace(ctx, std::shared_ptr<ExprStmt>{
                          new ExprStmt(getRange(ctx), getAstByContext<Expr>(ctx->expression()))});
  }

  void exitDeclareStatement(scriptlangParser::DeclareStatementContext *ctx) override {
    DeclStmt::Kind kind = DeclStmt::Kind::Normal;
    if (ctx->EXPORT() && ctx->CONST())
      kind = DeclStmt::Kind::ExportConst;
    else if (ctx->EXPORT())
      kind = DeclStmt::Kind::Export;
    else if (ctx->CONST())
      kind = DeclStmt::Kind::Const;

    auto type = ctx->type() ? getAstByContext<Type>(ctx->type()) : nullptr;
    map_.emplace(ctx, std::shared_ptr<DeclStmt>(
                          new DeclStmt(getRange(ctx), kind, ctx->Identifier()->getText(), type,
                                       getAstByContext<Expr>(ctx->expression()))));
  }

  void exitAssignStatement(scriptlangParser::AssignStatementContext *ctx) override {
    map_.emplace(ctx, std::shared_ptr<AssignStmt>(
                          new AssignStmt(getRange(ctx), getAstByContext<Expr>(ctx->expression(0U)),
                                         getAstByContext<Expr>(ctx->expression(1U)))));
  }

  void exitReturnStatement(scriptlangParser::ReturnStatementContext *ctx) override {
    auto returnValue = ctx->expression() ? getAstByContext<Expr>(ctx->expression()) : nullptr;
    map_.emplace(ctx, std::shared_ptr<ReturnStmt>(new ReturnStmt(getRange(ctx), returnValue)));
  }

  void exitBlockStatement(scriptlangParser::BlockStatementContext *ctx) override {
    BlockStmt::StmtsVec stmts;
    for (auto statementContext : ctx->statement())
      stmts.push_back(getAstByContext<Stmt>(statementContext));
    map_.emplace(ctx, std::shared_ptr<BlockStmt>{new BlockStmt(getRange(ctx), std::move(stmts))});
  }

  void exitIfStatement(scriptlangParser::IfStatementContext *ctx) override {
    auto thenBranch = ctx->blockStatement().size() == 2U
                          ? getAstByContext<Stmt>(ctx->blockStatement(1))
                      : ctx->ifStatement() ? getAstByContext<Stmt>(ctx->ifStatement())
                                           : nullptr;
    map_.emplace(ctx, std::shared_ptr<IfStmt>(new IfStmt(
                          getRange(ctx), getAstByContext<Expr>(ctx->expression()),
                          getAstByContext<BlockStmt>(ctx->blockStatement(0)), thenBranch)));
  }

  void exitWhileStatement(scriptlangParser::WhileStatementContext *ctx) override {
    map_.emplace(ctx, std::shared_ptr<WhileStmt>(
                          new WhileStmt(getRange(ctx), getAstByContext<Expr>(ctx->expression()),
                                        getAstByContext<BlockStmt>(ctx->blockStatement()))));
  }

  void exitBreakStatement(scriptlangParser::BreakStatementContext *ctx) override {
    map_.emplace(ctx, std::shared_ptr<BreakStmt>(new BreakStmt(getRange(ctx))));
  }

  void exitContinueStatement(scriptlangParser::ContinueStatementContext *ctx) override {
    map_.emplace(ctx, std::shared_ptr<ContinueStmt>(new ContinueStmt(getRange(ctx))));
  }

  void exitImportStatement(scriptlangParser::ImportStatementContext *ctx) override {
    map_.emplace(
        ctx, std::shared_ptr<ImportStmt>(new ImportStmt(getRange(ctx), ctx->Identifier()->getText(),
                                                        ctx->StringLiteral()->getText())));
  }

  void exitSimpleExpression(scriptlangParser::SimpleExpressionContext *ctx) override {
    handleCombineNode(ctx);
  }

  void exitPrefixExpression(scriptlangParser::PrefixExpressionContext *ctx) override {
    PrefixExpr::Op kind;
    if (ctx->prefixOperator()->Minus())
      kind = PrefixExpr::Op::Minus;
    else if (ctx->prefixOperator()->Plus())
      kind = PrefixExpr::Op::Plus;
    else if (ctx->prefixOperator()->NOT())
      kind = PrefixExpr::Op ::Not;
    else
      llvm_unreachable("unknown prefix op");
    map_.emplace(ctx, std::shared_ptr<PrefixExpr>{new PrefixExpr(
                          getRange(ctx), kind, getAstByContext<Expr>(ctx->expression()))});
  }

  void exitParenthesesExpression(scriptlangParser::ParenthesesExpressionContext *ctx) override {
    map_.emplace(ctx, getAstByContext<Expr>(ctx->expression()));
  }

  void exitBinaryExpressionLeft(scriptlangParser::BinaryExpressionLeftContext *ctx) override {
    handleCombineNode(ctx);
  }
  void exitBinaryExpressionRight(scriptlangParser::BinaryExpressionRightContext *ctx) override {
    handleCombineNode(ctx);
  }
  void exitBinaryExpression(scriptlangParser::BinaryExpressionContext *ctx) override {
    std::shared_ptr<BinaryExpr> binaryExpr;
    for (auto binaryRightWithOp : ctx->binaryExpressionRightWithOp()) {
      BinaryExpr::Op op = getOpFromContext(binaryRightWithOp->binaryOperator());
      std::shared_ptr<Expr> rightExpr =
          getAstByContext<Expr>(binaryRightWithOp->binaryExpressionRight());
      if (binaryExpr == nullptr) {
        binaryExpr = std::shared_ptr<BinaryExpr>{
            new BinaryExpr(op, getAstByContext<Expr>(ctx->binaryExpressionLeft()), rightExpr)};
      } else {
        if (getOpPriority(binaryExpr->op()) >= getOpPriority(op)) {
          // 1 * 2 + 3 or 1 + 2 + 3
          binaryExpr = std::shared_ptr<BinaryExpr>{new BinaryExpr(op, binaryExpr, rightExpr)};
        } else {
          // 1 + 2 * 3
          appendExprInRhs(*binaryExpr, op, rightExpr);
        }
      }
    }
    map_.emplace(ctx, binaryExpr);
  }

  void exitCallOrMemberExpressionLeft(
      scriptlangParser::CallOrMemberExpressionLeftContext *ctx) override {
    handleCombineNode(ctx);
  }
  CallExpr::ArgumentsVec
  handleCallExpressionRight(scriptlangParser::CallExpressionRightContext *callRightContext) {
    CallExpr::ArgumentsVec argument;
    for (auto exprContext : callRightContext->expression()) {
      argument.push_back(getAstByContext<Expr>(exprContext));
    }
    return argument;
  }
  template <class T> std::shared_ptr<Expr> handleCallOrMemberExpressionLeft(T *ctx) {
    static_assert(std::is_same_v<T, scriptlangParser::CallExpressionContext> ||
                  std::is_same_v<T, scriptlangParser::MemberExpressionContext>);
    std::shared_ptr<Expr> callerOrAccessor =
        getAstByContext<Expr>(ctx->callOrMemberExpressionLeft());
    for (auto *rightContext : ctx->callOrMemberExpressionRight()) {
      if (rightContext->callExpressionRight()) {
        callerOrAccessor = std::shared_ptr<CallExpr>{new CallExpr(
            llvm::SMRange{getRange(ctx).Start, getRange(rightContext).End}, callerOrAccessor,
            handleCallExpressionRight(rightContext->callExpressionRight()))};
      } else if (rightContext->memberExpressionRight()) {
        callerOrAccessor = std::shared_ptr<MemberExpr>{new MemberExpr(
            llvm::SMRange{getRange(ctx).Start, getRange(rightContext).End}, callerOrAccessor,
            getAstByContext<Identifier>(rightContext->memberExpressionRight()->identifier()))};
      } else {
        llvm_unreachable("unknown callOrMemberExpressionRight");
      }
    }
    return callerOrAccessor;
  }
  void exitCallExpression(scriptlangParser::CallExpressionContext *ctx) override {
    map_.emplace(ctx, std::shared_ptr<CallExpr>{
                          new CallExpr(getRange(ctx), handleCallOrMemberExpressionLeft(ctx),
                                       handleCallExpressionRight(ctx->callExpressionRight()))});
  }
  void exitMemberExpression(scriptlangParser::MemberExpressionContext *ctx) override {
    map_.emplace(ctx,
                 std::shared_ptr<MemberExpr>{new MemberExpr(
                     getRange(ctx), handleCallOrMemberExpressionLeft(ctx),
                     getAstByContext<Identifier>(ctx->memberExpressionRight()->identifier()))});
  }

  void exitFunctionExpression(scriptlangParser::FunctionExpressionContext *ctx) override {
    FuncExpr::ArgumentsVec arguments;
    FuncExpr::ArgumentTypesVec types;
    for (auto parameterContext : ctx->parameterWithIdentifier()) {
      arguments.push_back(getAstByContext<Identifier>(parameterContext->identifier()));
      if (parameterContext->type()) {
        types.push_back(getAstByContext<Type>(parameterContext->type()));
      }
    }
    map_.emplace(ctx, std::shared_ptr<FuncExpr>{
                          new FuncExpr(getRange(ctx), std::move(arguments), std::move(types),
                                       getAstByContext<BlockStmt>(ctx->blockStatement()))});
  }

  void exitExpression(scriptlangParser::ExpressionContext *ctx) override { handleCombineNode(ctx); }

  operator std::shared_ptr<TopDecls>() { return std::move(ast_); }

private:
  llvm::StringRef code_;
  std::shared_ptr<TopDecls> ast_;
  std::unordered_map<antlr4::ParserRuleContext const *, std::shared_ptr<AST>> map_;

  template <class TargetType>
  std::shared_ptr<TargetType> getAstByContext(antlr4::ParserRuleContext const *ctx) const {
    auto it = map_.find(ctx);
    assert(it != map_.end() && "not find ctx in map_");
    return std::dynamic_pointer_cast<TargetType>(it->second);
  }
  void handleCombineNode(antlr4::ParserRuleContext const *ctx) {
    auto *child = dynamic_cast<antlr4::ParserRuleContext *>(ctx->children.at(0));
    map_.emplace(ctx, getAstByContext<AST>(child));
  }
  llvm::SMRange getRange(antlr4::ParserRuleContext const *ctx) const {
    return llvm::SMRange{
        llvm::SMLoc::getFromPointer(&code_.data()[ctx->getStart()->getStartIndex()]),
        llvm::SMLoc::getFromPointer(&code_.data()[ctx->getStop()->getStopIndex()])};
  }
};

class ParserImpl {
public:
  explicit ParserImpl(llvm::StringRef code) : code_(code) {}
  std::shared_ptr<TopDecls> parse() {
    antlr4::ANTLRInputStream inputStream{code_.data(), code_.size()};
    scriptlangLexer lexer{&inputStream};
    antlr4::CommonTokenStream tokens{&lexer};
    scriptlangParser parser(&tokens);

    // TODO(better error message)
    class ErrorListener : public antlr4::BaseErrorListener {
    public:
      virtual void syntaxError(antlr4::Recognizer *recognizer, antlr4::Token *offendingSymbol,
                               size_t line, size_t charPositionInLine, const std::string &msg,
                               std::exception_ptr e) override {
        llvm::errs() << "syntaxError: " << msg << "(" << line << ":" << charPositionInLine << ")\n";
      }
    } errorListener;
    parser.removeErrorListeners();
    parser.addErrorListener(&errorListener);

    AntlrListener listener{code_};
    try {
      antlr4::tree::ParseTreeWalker::DEFAULT.walk(&listener, parser.scriptlang());
    } catch (...) {
    }
    return listener;
  }

private:
  llvm::StringRef code_;
};

Parser::Parser(llvm::StringRef Code) : impl_(new ParserImpl(Code)) {}

std::shared_ptr<TopDecls> Parser::parse() { return impl_->parse(); }

} // namespace scriptlang
