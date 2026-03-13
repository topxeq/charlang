// Package parser implements a parser for Charlang source code.
// This file defines the expression AST node types.
//
// # Expression Types
//
// Expressions in Charlang can be categorized as:
//
// Literals:
//   - Ident: Identifier (variable or function name)
//   - IntLit, UintLit, FloatLit: Numeric literals
//   - StringLit, CharLit: String and character literals
//   - BoolLit: Boolean literal (true/false)
//   - UndefinedLit: Undefined value
//   - ArrayLit: Array literal [a, b, c]
//   - MapLit: Map literal {key: value}
//   - FuncLit: Function literal func(params) { body }
//
// Operators:
//   - BinaryExpr: Binary operations (a + b, a && b)
//   - UnaryExpr: Unary operations (-a, !b)
//   - CondExpr: Ternary conditional (a ? b : c)
//
// Access and Calls:
//   - CallExpr: Function call f(a, b)
//   - IndexExpr: Index access a[i]
//   - SliceExpr: Slice expression a[low:high]
//   - SelectorExpr: Member access a.b
//   - ParenExpr: Parenthesized expression (a)
//   - ImportExpr: Import expression import("module")
//
// Error Handling:
//   - BadExpr: Placeholder for syntax errors
package parser

import (
	"strings"

	"github.com/topxeq/charlang/token"
)

// Expr represents an expression node in the AST.
// All expression types must implement this interface.
// The exprNode() method ensures type safety - only expression
// nodes can be assigned to Expr.
type Expr interface {
	Node
	exprNode()
}

// ArrayLit represents an array literal expression.
// Array literals are written as [element1, element2, ...].
//
// Example:
//
//	[1, 2, 3]           // array of integers
//	["a", "b", "c"]     // array of strings
//	[]                  // empty array
type ArrayLit struct {
	Elements []Expr // Element expressions
	LBrack   Pos    // Position of opening '['
	RBrack   Pos    // Position of closing ']'
}

func (e *ArrayLit) exprNode() {}

// Pos returns the position of first character belonging to the node.
func (e *ArrayLit) Pos() Pos {
	return e.LBrack
}

// End returns the position of first character immediately after the node.
func (e *ArrayLit) End() Pos {
	return e.RBrack + 1
}

func (e *ArrayLit) String() string {
	var elements []string
	for _, m := range e.Elements {
		elements = append(elements, m.String())
	}
	return "[" + strings.Join(elements, ", ") + "]"
}

// BadExpr represents a malformed expression that could not be parsed.
// It serves as a placeholder for error recovery, allowing parsing to
// continue after encountering syntax errors.
//
// The From and To fields define the range of source text that
// contains the error.
type BadExpr struct {
	From Pos // Start position of the bad expression
	To   Pos // End position of the bad expression
}

func (e *BadExpr) exprNode() {}

// Pos returns the position of first character belonging to the node.
func (e *BadExpr) Pos() Pos {
	return e.From
}

// End returns the position of first character immediately after the node.
func (e *BadExpr) End() Pos {
	return e.To
}

func (e *BadExpr) String() string {
	return "<bad expression>"
}

// BinaryExpr represents a binary operator expression.
// Binary operators include arithmetic (+, -, *, /, %),
// comparison (==, !=, <, >, <=, >=), and logical (&&, ||).
//
// Example:
//
//	a + b       // arithmetic
//	x == y      // comparison
//	p && q      // logical
type BinaryExpr struct {
	LHS      Expr         // Left-hand side operand
	RHS      Expr         // Right-hand side operand
	Token    token.Token  // Operator token (Add, Sub, EQL, LSS, AND, etc.)
	TokenPos Pos          // Position of the operator
}

func (e *BinaryExpr) exprNode() {}

// Pos returns the position of first character belonging to the node.
func (e *BinaryExpr) Pos() Pos {
	return e.LHS.Pos()
}

// End returns the position of first character immediately after the node.
func (e *BinaryExpr) End() Pos {
	return e.RHS.End()
}

func (e *BinaryExpr) String() string {
	return "(" + e.LHS.String() + " " + e.Token.String() +
		" " + e.RHS.String() + ")"
}

// BoolLit represents a boolean literal (true or false).
type BoolLit struct {
	Value    bool   // Boolean value
	ValuePos Pos    // Position of the literal
	Literal  string // Raw literal text ("true" or "false")
}

func (e *BoolLit) exprNode() {}

// Pos returns the position of first character belonging to the node.
func (e *BoolLit) Pos() Pos {
	return e.ValuePos
}

// End returns the position of first character immediately after the node.
func (e *BoolLit) End() Pos {
	return Pos(int(e.ValuePos) + len(e.Literal))
}

func (e *BoolLit) String() string {
	return e.Literal
}

// CallExpr represents a function call expression.
// Function calls can include optional variadic arguments using ...
//
// Example:
//
//	func(a, b)         // regular call
//	func(args...)      // variadic call
type CallExpr struct {
	Func     Expr   // Function expression being called
	LParen   Pos    // Position of opening '('
	Args     []Expr // Argument expressions
	Ellipsis Pos    // Position of '...' if variadic (NoPos if not)
	RParen   Pos    // Position of closing ')'
}

func (e *CallExpr) exprNode() {}

// Pos returns the position of first character belonging to the node.
func (e *CallExpr) Pos() Pos {
	return e.Func.Pos()
}

// End returns the position of first character immediately after the node.
func (e *CallExpr) End() Pos {
	return e.RParen + 1
}

func (e *CallExpr) String() string {
	var args []string
	for _, e := range e.Args {
		args = append(args, e.String())
	}
	if len(args) > 0 && e.Ellipsis.IsValid() {
		args[len(args)-1] = args[len(args)-1] + "..."
	}
	return e.Func.String() + "(" + strings.Join(args, ", ") + ")"
}

// CharLit represents a character literal (rune).
// Character literals are written with single quotes: 'a', '\n', '\u0041'.
type CharLit struct {
	Value    rune   // Rune value
	ValuePos Pos    // Position of the literal
	Literal  string // Raw literal text including quotes
}

func (e *CharLit) exprNode() {}

// Pos returns the position of first character belonging to the node.
func (e *CharLit) Pos() Pos {
	return e.ValuePos
}

// End returns the position of first character immediately after the node.
func (e *CharLit) End() Pos {
	return Pos(int(e.ValuePos) + len(e.Literal))
}

func (e *CharLit) String() string {
	return e.Literal
}

// CondExpr represents a ternary conditional expression (a ? b : c).
// The Cond expression is evaluated first; if truthy, True is returned,
// otherwise False is returned.
//
// Example:
//
//	x >= 0 ? x : -x    // absolute value
type CondExpr struct {
	Cond        Expr // Condition expression
	True        Expr // Expression if condition is truthy
	False       Expr // Expression if condition is falsy
	QuestionPos Pos  // Position of '?'
	ColonPos    Pos  // Position of ':'
}

func (e *CondExpr) exprNode() {}

// Pos returns the position of first character belonging to the node.
func (e *CondExpr) Pos() Pos {
	return e.Cond.Pos()
}

// End returns the position of first character immediately after the node.
func (e *CondExpr) End() Pos {
	return e.False.End()
}

func (e *CondExpr) String() string {
	return "(" + e.Cond.String() + " ? " + e.True.String() +
		" : " + e.False.String() + ")"
}

// FloatLit represents a floating-point numeric literal.
// Float literals can be written in decimal or exponent notation.
//
// Example:
//
//	3.14159
//	2.5e10
//	0.5
type FloatLit struct {
	Value    float64 // Parsed float value
	ValuePos Pos     // Position of the literal
	Literal  string  // Raw literal text
}

func (e *FloatLit) exprNode() {}

// Pos returns the position of first character belonging to the node.
func (e *FloatLit) Pos() Pos {
	return e.ValuePos
}

// End returns the position of first character immediately after the node.
func (e *FloatLit) End() Pos {
	return Pos(int(e.ValuePos) + len(e.Literal))
}

func (e *FloatLit) String() string {
	return e.Literal
}

// FuncLit represents a function literal (anonymous function).
// Function literals define a function body that can be assigned
// to variables or passed as arguments.
//
// Example:
//
//	func(x, y) { return x + y }
//	func() { print("hello") }
type FuncLit struct {
	Type *FuncType  // Function signature (params)
	Body *BlockStmt // Function body
}

func (e *FuncLit) exprNode() {}

// Pos returns the position of first character belonging to the node.
func (e *FuncLit) Pos() Pos {
	return e.Type.Pos()
}

// End returns the position of first character immediately after the node.
func (e *FuncLit) End() Pos {
	return e.Body.End()
}

func (e *FuncLit) String() string {
	return "func" + e.Type.Params.String() + " " + e.Body.String()
}

// FuncType represents a function type definition (signature).
// It contains the function keyword position and parameter list.
type FuncType struct {
	FuncPos Pos        // Position of 'func' keyword
	Params  *IdentList // Parameter list
}

func (e *FuncType) exprNode() {}

// Pos returns the position of first character belonging to the node.
func (e *FuncType) Pos() Pos {
	return e.FuncPos
}

// End returns the position of first character immediately after the node.
func (e *FuncType) End() Pos {
	return e.Params.End()
}

func (e *FuncType) String() string {
	return "func" + e.Params.String()
}

// Ident represents an identifier (variable, function, or type name).
// Identifiers are names that refer to declared entities in the program.
//
// Example:
//
//	myVar
//	calculateSum
//	_privateFunc
type Ident struct {
	Name    string // Identifier name
	NamePos Pos    // Position of the identifier
}

func (e *Ident) exprNode() {}

// Pos returns the position of first character belonging to the node.
func (e *Ident) Pos() Pos {
	return e.NamePos
}

// End returns the position of first character immediately after the node.
func (e *Ident) End() Pos {
	return Pos(int(e.NamePos) + len(e.Name))
}

func (e *Ident) String() string {
	if e != nil {
		return e.Name
	}
	return nullRep
}

// ImportExpr represents a module import expression.
// Import expressions dynamically load modules at runtime.
//
// Example:
//
//	import("math")     // import a module
//	import("./util")   // import from relative path
type ImportExpr struct {
	ModuleName string       // Name or path of the module to import
	Token      token.Token  // Import token
	TokenPos   Pos          // Position of 'import' keyword
}

func (e *ImportExpr) exprNode() {}

// Pos returns the position of first character belonging to the node.
func (e *ImportExpr) Pos() Pos {
	return e.TokenPos
}

// End returns the position of first character immediately after the node.
func (e *ImportExpr) End() Pos {
	// import("moduleName")
	return Pos(int(e.TokenPos) + 10 + len(e.ModuleName))
}

func (e *ImportExpr) String() string {
	return `import("` + e.ModuleName + `")"`
}

// IndexExpr represents an index expression for element access.
// Index expressions access elements by numeric or key index.
//
// Example:
//
//	arr[0]       // array element access
//	map["key"]   // map value access
type IndexExpr struct {
	Expr   Expr // Expression being indexed
	LBrack Pos  // Position of opening '['
	Index  Expr // Index expression
	RBrack Pos  // Position of closing ']'
}

func (e *IndexExpr) exprNode() {}

// Pos returns the position of first character belonging to the node.
func (e *IndexExpr) Pos() Pos {
	return e.Expr.Pos()
}

// End returns the position of first character immediately after the node.
func (e *IndexExpr) End() Pos {
	return e.RBrack + 1
}

func (e *IndexExpr) String() string {
	var index string
	if e.Index != nil {
		index = e.Index.String()
	}
	return e.Expr.String() + "[" + index + "]"
}

// IntLit represents a signed integer literal.
// Integer literals can be in decimal, hexadecimal (0x), or octal (0) format.
//
// Example:
//
//	42
//	0xFF
//	0755
type IntLit struct {
	Value    int64  // Parsed integer value
	ValuePos Pos    // Position of the literal
	Literal  string // Raw literal text
}

func (e *IntLit) exprNode() {}

// Pos returns the position of first character belonging to the node.
func (e *IntLit) Pos() Pos {
	return e.ValuePos
}

// End returns the position of first character immediately after the node.
func (e *IntLit) End() Pos {
	return Pos(int(e.ValuePos) + len(e.Literal))
}

func (e *IntLit) String() string {
	return e.Literal
}

// UintLit represents an unsigned integer literal.
// Unsigned integers use the 'u' suffix or are larger than int64 max.
//
// Example:
//
//	42u
//	0xFFFFFFFFu
type UintLit struct {
	Value    uint64 // Parsed unsigned integer value
	ValuePos Pos    // Position of the literal
	Literal  string // Raw literal text
}

func (e *UintLit) exprNode() {}

// Pos returns the position of first character belonging to the node.
func (e *UintLit) Pos() Pos {
	return e.ValuePos
}

// End returns the position of first character immediately after the node.
func (e *UintLit) End() Pos {
	return Pos(int(e.ValuePos) + len(e.Literal))
}

func (e *UintLit) String() string {
	return e.Literal
}

// MapElementLit represents a key-value pair in a map literal.
// Each element consists of a string key and a value expression.
//
// Example:
//
//	name: "John"
//	age: 30
type MapElementLit struct {
	Key      string // Element key
	KeyPos   Pos    // Position of the key
	ColonPos Pos    // Position of the ':' separator
	Value    Expr   // Element value expression
}

func (e *MapElementLit) exprNode() {}

// Pos returns the position of first character belonging to the node.
func (e *MapElementLit) Pos() Pos {
	return e.KeyPos
}

// End returns the position of first character immediately after the node.
func (e *MapElementLit) End() Pos {
	return e.Value.End()
}

func (e *MapElementLit) String() string {
	return e.Key + ": " + e.Value.String()
}

// MapLit represents a map literal expression.
// Map literals are written as {key1: value1, key2: value2, ...}.
// Keys must be string literals, values can be any expression.
//
// Example:
//
//	{name: "John", age: 30}
//	{}  // empty map
type MapLit struct {
	LBrace   Pos             // Position of opening '{'
	Elements []*MapElementLit // Key-value pairs
	RBrace   Pos             // Position of closing '}'
}

func (e *MapLit) exprNode() {}

// Pos returns the position of first character belonging to the node.
func (e *MapLit) Pos() Pos {
	return e.LBrace
}

// End returns the position of first character immediately after the node.
func (e *MapLit) End() Pos {
	return e.RBrace + 1
}

func (e *MapLit) String() string {
	var elements []string
	for _, m := range e.Elements {
		elements = append(elements, m.String())
	}
	return "{" + strings.Join(elements, ", ") + "}"
}

// ParenExpr represents a parenthesized expression.
// Parentheses can be used to override operator precedence.
//
// Example:
//
//	(a + b) * c
//	(x)
type ParenExpr struct {
	Expr   Expr // Inner expression
	LParen Pos  // Position of opening '('
	RParen Pos  // Position of closing ')'
}

func (e *ParenExpr) exprNode() {}

// Pos returns the position of first character belonging to the node.
func (e *ParenExpr) Pos() Pos {
	return e.LParen
}

// End returns the position of first character immediately after the node.
func (e *ParenExpr) End() Pos {
	return e.RParen + 1
}

func (e *ParenExpr) String() string {
	return "(" + e.Expr.String() + ")"
}

// SelectorExpr represents a member access expression (x.sel).
// Used for accessing fields of objects or calling methods.
//
// Example:
//
//	obj.field
//	module.function()
type SelectorExpr struct {
	Expr Expr // Object expression
	Sel  Expr // Selector (typically an Ident)
}

func (e *SelectorExpr) exprNode() {}

// Pos returns the position of first character belonging to the node.
func (e *SelectorExpr) Pos() Pos {
	return e.Expr.Pos()
}

// End returns the position of first character immediately after the node.
func (e *SelectorExpr) End() Pos {
	return e.Sel.End()
}

func (e *SelectorExpr) String() string {
	return e.Expr.String() + "." + e.Sel.String()
}

// SliceExpr represents a slice expression for extracting a sub-array.
// Slice expressions use the syntax expr[low:high].
// Both low and high bounds are optional.
//
// Example:
//
//	arr[1:4]    // elements from index 1 to 3
//	arr[:5]     // elements from start to index 4
//	arr[2:]     // elements from index 2 to end
//	arr[:]      // copy of entire array
type SliceExpr struct {
	Expr   Expr // Expression being sliced
	LBrack Pos  // Position of opening '['
	Low    Expr // Lower bound (nil if omitted)
	High   Expr // Upper bound (nil if omitted)
	RBrack Pos  // Position of closing ']'
}

func (e *SliceExpr) exprNode() {}

// Pos returns the position of first character belonging to the node.
func (e *SliceExpr) Pos() Pos {
	return e.Expr.Pos()
}

// End returns the position of first character immediately after the node.
func (e *SliceExpr) End() Pos {
	return e.RBrack + 1
}

func (e *SliceExpr) String() string {
	var low, high string
	if e.Low != nil {
		low = e.Low.String()
	}
	if e.High != nil {
		high = e.High.String()
	}
	return e.Expr.String() + "[" + low + ":" + high + "]"
}

// StringLit represents a string literal.
// String literals can be written with double quotes or backticks.
// Double-quoted strings support escape sequences.
//
// Example:
//
//	"hello, world"
//	`raw string`
//	"line1\nline2"
type StringLit struct {
	Value    string // Parsed string value
	ValuePos Pos    // Position of the literal
	Literal  string // Raw literal text including quotes
}

func (e *StringLit) exprNode() {}

// Pos returns the position of first character belonging to the node.
func (e *StringLit) Pos() Pos {
	return e.ValuePos
}

// End returns the position of first character immediately after the node.
func (e *StringLit) End() Pos {
	return Pos(int(e.ValuePos) + len(e.Literal))
}

func (e *StringLit) String() string {
	return e.Literal
}

// UnaryExpr represents a unary operator expression.
// Unary operators include -, !, and ^ (bitwise NOT).
//
// Example:
//
//	-x       // negation
//	!flag    // logical NOT
//	^bits    // bitwise complement
type UnaryExpr struct {
	Expr     Expr        // Operand expression
	Token    token.Token // Operator token (Sub, Not, Xor)
	TokenPos Pos         // Position of the operator
}

func (e *UnaryExpr) exprNode() {}

// Pos returns the position of first character belonging to the node.
func (e *UnaryExpr) Pos() Pos {
	return e.Expr.Pos()
}

// End returns the position of first character immediately after the node.
func (e *UnaryExpr) End() Pos {
	return e.Expr.End()
}

func (e *UnaryExpr) String() string {
	return "(" + e.Token.String() + e.Expr.String() + ")"
}

// UndefinedLit represents the undefined literal.
// Undefined is a special value indicating the absence of a value.
// It's similar to null/nil in other languages.
//
// Example:
//
//	undefined
//	x := undefined
type UndefinedLit struct {
	TokenPos Pos // Position of 'undefined' keyword
}

func (e *UndefinedLit) exprNode() {}

// Pos returns the position of first character belonging to the node.
func (e *UndefinedLit) Pos() Pos {
	return e.TokenPos
}

// End returns the position of first character immediately after the node.
func (e *UndefinedLit) End() Pos {
	return e.TokenPos + 9 // len(undefined) == 9
}

func (e *UndefinedLit) String() string {
	return "undefined"
}
