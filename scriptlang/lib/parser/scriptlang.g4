grammar scriptlang;

scriptlang: statement* EOF;

literal: IntNumber | HexNumber | FloatNumber | StringLiteral;
identifier: Identifier;

// type
type: baseType | functionType;

baseType: Identifier;

parameter: Identifier ':' type;
parameterList: (parameter (',' parameter)*)?;
functionType: '(' parameterList ')' '=>' type;

// statement
statement:
	expressionStatement
	| declareStatement
	| assignStatement
	| returnStatement
	| blockStatement
	| ifStatement
	| whileStatement
	| breakStatement
	| continueStatement
	| importStatement
	| exportStatement;

// basis statement
expressionStatement: expression ';';
declareStatement:
	('const' | 'let') Identifier (':' type)? '=' expression ';';
assignStatement: expression '=' expression ';';

// flow statement
returnStatement: 'return' expression ';';
blockStatement: '{' statement* '}';
ifStatement:
	'if' '(' expression ')' blockStatement (
		'else' (blockStatement | ifStatement)
	)?;
whileStatement: 'while' '(' expression ')' blockStatement;
breakStatement: 'break' ';';
continueStatement: 'continue' ';';

importStatement:
	'import' '*' 'as' Identifier 'from' StringLiteral;
exportStatement: 'export' declareStatement;

// expression
prefixOperator: 'not' | '+' | '-';
binaryOperator:
	'*'
	| '/'
	| '%'
	| '+'
	| '-'
	| '<<'
	| '>>'
	| '<'
	| '>'
	| '<='
	| '>='
	| '=='
	| '!='
	| '&'
	| '^'
	| '|'
	| '&&'
	| '||';

prefixExpression: prefixOperator expression;

parenthesesExpression: '(' expression ')';

binaryExpressionRightWithOp:
	binaryOperator binaryExpressionRight;
binaryExpressionLeft:
	identifier
	| prefixExpression
	| parenthesesExpression
	// binaryExpression
	| callExpression
	| memberExpression;
binaryExpressionRight:
	identifier
	| prefixExpression
	| parenthesesExpression
	| binaryExpression
	| callExpression
	| memberExpression;
binaryExpression:
	binaryExpressionLeft binaryExpressionRightWithOp+;

callOrMemberExpressionLeft: identifier | parenthesesExpression;
callExpressionRight: '(' (expression (',' expression)*)? ')';
memberExpressionRight: '.' Identifier;
callOrMemberExpressionRight:
	callExpressionRight
	| memberExpressionRight;
callExpression:
	callOrMemberExpressionLeft callOrMemberExpressionRight* callExpressionRight;
memberExpression:
	callOrMemberExpressionLeft callOrMemberExpressionRight* memberExpressionRight;

functionExpression: '(' parameterList ')' '=>' blockStatement;

expression:
	identifier
	| prefixExpression
	| parenthesesExpression
	| binaryExpression
	| callExpression
	| memberExpression
	| functionExpression;

// Keyword

IF: 'if';
ELSE: 'else';
WHILE: 'while';
BREAK: 'break';
CONTINUE: 'continue';
LET: 'let';
CONST: 'const';
NOT: 'not';
FUNCTION: 'function';
DECLARE: 'declare';
CLASS: 'class';
STRUCT: 'struct';
RETURN: 'return';
IMPORT: 'import';
EXPORT: 'export';
FROM: 'from';
AS: 'as';

// Operator
LParenthesis: '(';
RParenthesis: ')';
LBrace: '{';
RBrace: '}';
LBracket: '[';
RBracket: ']';

Less: '<';
LessEqual: '<=';
Greater: '>';
GreaterEqual: '>=';
LeftShift: '<<';
RightShift: '>>';
EqualGreater: '=>';

Plus: '+';
Minus: '-';
Star: '*';
Div: '/';
Mod: '%';

And: '&';
Or: '|';
AndAnd: '&&';
OrOr: '||';
Caret: '^';

Question: '?';
Colon: ':';
Semi: ';';

Assign: '=';

Equal: '==';
NotEqual: '!=';

Dot: '.';

At: '@';

// Token

Identifier: NonDigit ( NonDigit | Digit)*;

IntNumber: Digit+;
HexNumber: '0' [xX] HexDigit+;
FloatNumber: Digit+ '.' Digit+;
StringLiteral: '"' (Char+)? '"';

fragment Digit: [0-9];
fragment HexDigit: [0-9a-fA-F];
fragment NonDigit: [a-zA-Z_] | UniversalCharacterName;

fragment UniversalCharacterName:
	'\\u' HexDigit HexDigit HexDigit HexDigit
	| '\\U' HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit;
fragment SimpleEscapeSequence: '\\' ['"?abfnrtv\\];
fragment HexadecimalEscapeSequence: '\\x' HexDigit+;
fragment EscapeSequence:
	SimpleEscapeSequence
	| HexadecimalEscapeSequence
	| UniversalCharacterName;

fragment Char: ~["\\\r\n] | EscapeSequence;

Whitespace: [ \t]+ -> skip;
Newline: ( '\r' '\n'? | '\n') -> skip;
BlockComment: '/*' .*? '*/' -> skip;
LineComment: '//' ~[\r\n]* -> skip;

