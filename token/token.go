// Package token defines lexical tokens for the Charlang language.
//
// # Overview
//
// This package defines the Token type and all token constants used by the
// Charlang scanner and parser. Tokens represent the atomic units of the
// language's syntax, including:
//   - Literals: identifiers, numbers (int, uint, float), characters, strings
//   - Operators: arithmetic, logical, bitwise, comparison, assignment
//   - Delimiters: parentheses, brackets, braces, punctuation
//   - Keywords: control flow, declarations, exception handling
//
// # Token Categories
//
// Tokens are organized into categories using marker constants:
//   - Literals: Ident, Int, Uint, Float, Char, String
//   - Operators: Add through Question (all operators and delimiters)
//   - Keywords: Break through Throw (reserved words)
//
// Use IsLiteral(), IsOperator(), and IsKeyword() to check token category.
//
// # Operator Precedence
//
// The Precedence() method returns operator precedence for expression parsing:
//
//	5: *, /, %, <<, >>, &, &^ (multiplicative)
//	4: +, -, |, ^ (additive)
//	3: ==, !=, <, <=, >, >= (comparison)
//	2: && (logical AND)
//	1: || (logical OR)
//
// # Usage
//
//	// Check if a token is a keyword
//	if tok.IsKeyword() { ... }
//
//	// Look up identifier
//	tok := token.Lookup("for")  // returns token.For
//	tok := token.Lookup("myVar") // returns token.Ident
//
//	// Get string representation
//	s := token.Add.String()  // returns "+"
package token

import "strconv"

// keywords maps keyword strings to their Token values.
// Initialized in init() by scanning the keyword range.
var keywords map[string]Token

// Token represents a lexical token from Charlang source code.
// Token values are integers that can be compared for equality
// and used as map keys.
type Token int

// Token constants define all lexical tokens for Charlang.
// Tokens are organized into groups with marker constants for range checks.
const (
	// Illegal represents an invalid or unrecognized token.
	Illegal Token = iota

	// EOF marks the end of file.
	EOF

	// Comment represents a comment token (when ScanComments is enabled).
	Comment

	// _literalBeg marks the start of literal tokens.
	_literalBeg

	// Ident represents an identifier (variable, function, or type name).
	Ident
	// Int represents a signed integer literal.
	Int
	// Uint represents an unsigned integer literal.
	Uint
	// Float represents a floating-point literal.
	Float
	// Char represents a character literal.
	Char
	// String represents a string literal.
	String

	// _literalEnd marks the end of literal tokens.
	_literalEnd

	// _operatorBeg marks the start of operator and delimiter tokens.
	_operatorBeg

	// Arithmetic operators
	Add // +
	Sub // -
	Mul // *
	Quo // /
	Rem // %

	// Bitwise operators
	And    // &
	Or     // |
	Xor    // ^
	Shl    // <<
	Shr    // >>
	AndNot // &^

	// Compound assignment operators
	AddAssign    // +=
	SubAssign    // -=
	MulAssign    // *=
	QuoAssign    // /=
	RemAssign    // %=
	AndAssign    // &=
	OrAssign     // |=
	XorAssign    // ^=
	ShlAssign    // <<=
	ShrAssign    // >>=
	AndNotAssign // &^=

	// Logical operators
	LAnd // &&
	LOr  // ||

	// Increment/decrement
	Inc // ++
	Dec // --

	// Comparison operators
	Equal    // ==
	Less     // <
	Greater  // >
	Assign   // =
	Not      // !
	NotEqual // !=
	LessEq   // <=
	GreaterEq // >=

	// Declaration
	Define // :=

	// Special
	Ellipsis // ...

	// Delimiters
	LParen    // (
	LBrack    // [
	LBrace    // {
	Comma     // ,
	Period    // .
	RParen    // )
	RBrack    // ]
	RBrace    // }
	Semicolon // ;
	Colon     // :
	Question  // ?

	// _operatorEnd marks the end of operator tokens.
	_operatorEnd

	// _keywordBeg marks the start of keyword tokens.
	_keywordBeg

	// Control flow keywords
	Break
	Continue
	Else
	For
	Func    // function declaration
	If
	Return

	// Literal keywords
	True
	False
	Undefined

	// Iteration
	In

	// Module system
	Import

	// Declaration keywords
	Param  // function parameter declaration
	Global // global variable declaration
	Var    // variable declaration
	Const  // constant declaration

	// Exception handling
	Try
	Catch
	Finally
	Throw

	// _keywordEnd marks the end of keyword tokens.
	_keywordEnd
)

// tokens maps Token values to their string representations.
// Used by Token.String() for displaying tokens.
var tokens = [...]string{
	Illegal:      "ILLEGAL",
	EOF:          "EOF",
	Comment:      "COMMENT",
	Ident:        "IDENT",
	Int:          "INT",
	Uint:         "UINT",
	Float:        "FLOAT",
	Char:         "CHAR",
	String:       "STRING",
	Add:          "+",
	Sub:          "-",
	Mul:          "*",
	Quo:          "/",
	Rem:          "%",
	And:          "&",
	Or:           "|",
	Xor:          "^",
	Shl:          "<<",
	Shr:          ">>",
	AndNot:       "&^",
	AddAssign:    "+=",
	SubAssign:    "-=",
	MulAssign:    "*=",
	QuoAssign:    "/=",
	RemAssign:    "%=",
	AndAssign:    "&=",
	OrAssign:     "|=",
	XorAssign:    "^=",
	ShlAssign:    "<<=",
	ShrAssign:    ">>=",
	AndNotAssign: "&^=",
	LAnd:         "&&",
	LOr:          "||",
	Inc:          "++",
	Dec:          "--",
	Equal:        "==",
	Less:         "<",
	Greater:      ">",
	Assign:       "=",
	Not:          "!",
	NotEqual:     "!=",
	LessEq:       "<=",
	GreaterEq:    ">=",
	Define:       ":=",
	Ellipsis:     "...",
	LParen:       "(",
	LBrack:       "[",
	LBrace:       "{",
	Comma:        ",",
	Period:       ".",
	RParen:       ")",
	RBrack:       "]",
	RBrace:       "}",
	Semicolon:    ";",
	Colon:        ":",
	Question:     "?",
	Break:        "break",
	Continue:     "continue",
	Else:         "else",
	For:          "for",
	Func:         "func",
	If:           "if",
	Return:       "return",
	True:         "true",
	False:        "false",
	In:           "in",
	Undefined:    "undefined",
	Import:       "import",
	Param:        "param",
	Global:       "global",
	Var:          "var",
	Const:        "const",
	Try:          "try",
	Catch:        "catch",
	Finally:      "finally",
	Throw:        "throw",
}

// String returns the string representation of the token.
// For operators and delimiters, this is the operator symbol.
// For keywords, this is the keyword itself.
// For invalid tokens, returns "token(N)" where N is the numeric value.
func (tok Token) String() string {
	s := ""

	if 0 <= tok && tok < Token(len(tokens)) {
		s = tokens[tok]
	}

	if s == "" {
		s = "token(" + strconv.Itoa(int(tok)) + ")"
	}

	return s
}

// LowestPrec represents the lowest operator precedence.
// Non-operator tokens have this precedence.
const LowestPrec = 0

// Precedence returns the operator precedence for the token.
// Higher values bind more tightly. Non-operators return LowestPrec.
//
// Precedence levels:
//
//	5: *, /, %, <<, >>, &, &^ (multiplicative)
//	4: +, -, |, ^ (additive)
//	3: ==, !=, <, <=, >, >= (comparison)
//	2: && (logical AND)
//	1: || (logical OR)
func (tok Token) Precedence() int {
	switch tok {
	case LOr:
		return 1
	case LAnd:
		return 2
	case Equal, NotEqual, Less, LessEq, Greater, GreaterEq:
		return 3
	case Add, Sub, Or, Xor:
		return 4
	case Mul, Quo, Rem, Shl, Shr, And, AndNot:
		return 5
	}
	return LowestPrec
}

// IsLiteral returns true if the token is a literal value.
// Literal tokens are: Ident, Int, Uint, Float, Char, String.
func (tok Token) IsLiteral() bool {
	return _literalBeg < tok && tok < _literalEnd
}

// IsOperator returns true if the token is an operator or delimiter.
// This includes all arithmetic, logical, comparison operators and
// punctuation like parentheses, brackets, etc.
func (tok Token) IsOperator() bool {
	return _operatorBeg < tok && tok < _operatorEnd
}

// IsBinaryOperator returns true if the token is a binary operator.
// Binary operators take two operands (e.g., +, -, *, /, ==, <).
// This excludes unary operators (!, ++, --) and assignment operators.
func (tok Token) IsBinaryOperator() bool {
	switch tok {
	case Add,
		Sub,
		Mul,
		Quo,
		Rem,
		Less,
		LessEq,
		Greater,
		GreaterEq,
		And,
		Or,
		Xor,
		AndNot,
		Shl,
		Shr,
		Equal,
		NotEqual:
		return true
	}
	return false
}

// IsKeyword returns true if the token is a reserved keyword.
// Keywords cannot be used as identifiers.
func (tok Token) IsKeyword() bool {
	return _keywordBeg < tok && tok < _keywordEnd
}

// Lookup returns the token corresponding to the given identifier.
// If the identifier is a keyword, returns the keyword token.
// Otherwise, returns Ident.
//
// Example:
//
//	Lookup("for")     // returns For
//	Lookup("myVar")   // returns Ident
func Lookup(ident string) Token {
	if tok, isKeyword := keywords[ident]; isKeyword {
		return tok
	}
	return Ident
}

// init initializes the keywords map by scanning the keyword token range.
func init() {
	keywords = make(map[string]Token)
	for i := _keywordBeg + 1; i < _keywordEnd; i++ {
		keywords[tokens[i]] = i
	}
}
