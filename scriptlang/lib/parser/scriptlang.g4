grammar scriptlang;

scriptlang: statement* EOF;

literal: IntNumber | HexNumber | FloatNumber | StringLiteral;
identifier: Identifier;

// type
type: baseType | functionType;

baseType: Identifier ('.' Identifier)*;

parameterWithType: (Identifier ':')? type;
functionType:
	'(' (parameterWithType (',' parameterWithType)*)? ')' '=>' type;

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
	| importStatement;

// basis statement
expressionStatement: expression ';';
declareStatement:
	'export'? ('const' | 'let') Identifier (':' type)? '=' expression ';';
assignStatement: expression '=' expression ';';

// flow statement
returnStatement: 'return' expression? ';';
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

simpleExpression: identifier | literal;

prefixExpressionOperand:
	simpleExpression
	// | prefixExpression
	| parenthesesExpression
	// | binaryExpression
	| callExpression
	| memberExpression;
prefixExpression: prefixOperator prefixExpressionOperand;

parenthesesExpression: '(' expression ')';

binaryExpressionRightWithOp:
	binaryOperator binaryExpressionRight;
binaryExpressionLeft:
	simpleExpression
	| prefixExpression
	| parenthesesExpression
	// binaryExpression
	| callExpression
	| memberExpression;
binaryExpressionRight:
	simpleExpression
	| prefixExpression
	| parenthesesExpression
	| binaryExpression
	| callExpression
	| memberExpression;
binaryExpression:
	binaryExpressionLeft binaryExpressionRightWithOp+;

callOrMemberExpressionLeft:
	simpleExpression
	| parenthesesExpression
	| functionExpression;
callExpressionRight: '(' (expression (',' expression)*)? ')';
memberExpressionRight: '.' identifier;
callOrMemberExpressionRight:
	callExpressionRight
	| memberExpressionRight;
callExpression:
	callOrMemberExpressionLeft callOrMemberExpressionRight* callExpressionRight;
memberExpression:
	callOrMemberExpressionLeft callOrMemberExpressionRight* memberExpressionRight;

parameterWithIdentifier: identifier (':' type)?;
functionExpression:
	'(' (parameterWithIdentifier (',' parameterWithIdentifier)*)? ')' '=>' blockStatement;

expression:
	simpleExpression
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

