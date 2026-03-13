// Package parser implements a parser for Charlang source code.
// This file defines the statement AST node types.
//
// # Statement Types
//
// Statements in Charlang can be categorized as:
//
// Declarations:
//   - DeclStmt: Declaration statement (var)
//   - GenDecl: Generic declaration
//
// Assignments:
//   - AssignStmt: Assignment (x = y)
//   - IncDecStmt: Increment/decrement (x++, x--)
//
// Control Flow:
//   - IfStmt: If-else statement
//   - ForStmt: C-style for loop
//   - ForInStmt: For-in iteration
//   - ReturnStmt: Function return
//   - BranchStmt: Break/continue
//
// Exception Handling:
//   - TryStmt: Try block
//   - CatchStmt: Catch clause
//   - FinallyStmt: Finally clause
//   - ThrowStmt: Throw exception
//
// Other:
//   - BlockStmt: Block of statements
//   - ExprStmt: Expression as statement
//   - EmptyStmt: Empty statement
//   - BadStmt: Placeholder for errors
package parser

import (
	"strings"

	"github.com/topxeq/charlang/token"
)

// Stmt represents a statement node in the AST.
// All statement types must implement this interface.
// The stmtNode() method ensures type safety - only statement
// nodes can be assigned to Stmt.
type Stmt interface {
	Node
	stmtNode()
}

// IsStatement returns true if the given value implements the Stmt interface.
// This is a utility function for type checking.
func IsStatement(v interface{}) bool {
	_, ok := v.(interface {
		stmtNode()
	})
	return ok
}

// AssignStmt represents an assignment statement.
// Assignments can be simple (=) or compound (+=, -=, etc.).
// Multiple assignments are supported (a, b = b, a).
//
// Example:
//
//	x = 10
//	a, b = b, a
//	count += 1
type AssignStmt struct {
	LHS      []Expr        // Left-hand side expressions
	RHS      []Expr        // Right-hand side expressions
	Token    token.Token   // Assignment operator (Assign, AddAssign, etc.)
	TokenPos Pos           // Position of the operator
}

func (s *AssignStmt) stmtNode() {}

// Pos returns the position of first character belonging to the node.
func (s *AssignStmt) Pos() Pos {
	return s.LHS[0].Pos()
}

// End returns the position of first character immediately after the node.
func (s *AssignStmt) End() Pos {
	return s.RHS[len(s.RHS)-1].End()
}

func (s *AssignStmt) String() string {
	var lhs, rhs []string
	for _, e := range s.LHS {
		lhs = append(lhs, e.String())
	}
	for _, e := range s.RHS {
		rhs = append(rhs, e.String())
	}
	return strings.Join(lhs, ", ") + " " + s.Token.String() +
		" " + strings.Join(rhs, ", ")
}

// BadStmt represents a malformed statement that could not be parsed.
// It serves as a placeholder for error recovery.
type BadStmt struct {
	From Pos // Start position of the bad statement
	To   Pos // End position of the bad statement
}

func (s *BadStmt) stmtNode() {}

// Pos returns the position of first character belonging to the node.
func (s *BadStmt) Pos() Pos {
	return s.From
}

// End returns the position of first character immediately after the node.
func (s *BadStmt) End() Pos {
	return s.To
}

func (s *BadStmt) String() string {
	return "<bad statement>"
}

// BlockStmt represents a block of statements enclosed in braces.
// Blocks create a new scope for local variables.
//
// Example:
//
//	{
//	    x := 1
//	    y := 2
//	}
type BlockStmt struct {
	Stmts  []Stmt // Statements in the block
	LBrace Pos    // Position of opening '{'
	RBrace Pos    // Position of closing '}'
}

func (s *BlockStmt) stmtNode() {}

// Pos returns the position of first character belonging to the node.
func (s *BlockStmt) Pos() Pos {
	return s.LBrace
}

// End returns the position of first character immediately after the node.
func (s *BlockStmt) End() Pos {
	return s.RBrace + 1
}

func (s *BlockStmt) String() string {
	var list []string
	for _, e := range s.Stmts {
		list = append(list, e.String())
	}
	return "{" + strings.Join(list, "; ") + "}"
}

// BranchStmt represents a branch statement (break or continue).
// Break exits the innermost loop, continue skips to the next iteration.
// Labels can be used for breaking out of nested loops.
//
// Example:
//
//	break
//	continue
//	break outerLoop
type BranchStmt struct {
	Token    token.Token // Break or Continue
	TokenPos Pos         // Position of the keyword
	Label    *Ident      // Optional label (nil if none)
}

func (s *BranchStmt) stmtNode() {}

// Pos returns the position of first character belonging to the node.
func (s *BranchStmt) Pos() Pos {
	return s.TokenPos
}

// End returns the position of first character immediately after the node.
func (s *BranchStmt) End() Pos {
	if s.Label != nil {
		return s.Label.End()
	}

	return Pos(int(s.TokenPos) + len(s.Token.String()))
}

func (s *BranchStmt) String() string {
	var label string
	if s.Label != nil {
		label = " " + s.Label.Name
	}
	return s.Token.String() + label
}

// EmptyStmt represents an empty statement (just a semicolon).
// Empty statements can be explicit (;) or implicit (inserted by scanner).
type EmptyStmt struct {
	Semicolon Pos  // Position of the semicolon
	Implicit  bool // Whether the semicolon was implicitly inserted
}

func (s *EmptyStmt) stmtNode() {}

// Pos returns the position of first character belonging to the node.
func (s *EmptyStmt) Pos() Pos {
	return s.Semicolon
}

// End returns the position of first character immediately after the node.
func (s *EmptyStmt) End() Pos {
	if s.Implicit {
		return s.Semicolon
	}
	return s.Semicolon + 1
}

func (s *EmptyStmt) String() string {
	return ";"
}

// ExprStmt represents an expression used as a statement.
// Function calls, assignments, and other expressions can be statements.
//
// Example:
//
//	print("hello")
//	x + y      // evaluated but result discarded
type ExprStmt struct {
	Expr Expr // The expression
}

func (s *ExprStmt) stmtNode() {}

// Pos returns the position of first character belonging to the node.
func (s *ExprStmt) Pos() Pos {
	return s.Expr.Pos()
}

// End returns the position of first character immediately after the node.
func (s *ExprStmt) End() Pos {
	return s.Expr.End()
}

func (s *ExprStmt) String() string {
	return s.Expr.String()
}

// ForInStmt represents a for-in iteration statement.
// For-in iterates over elements of an array or keys/values of a map.
//
// Example:
//
//	for k, v in arr { ... }    // array: k is index, v is element
//	for k, v in map { ... }    // map: k is key, v is value
//	for k in arr { ... }       // iterate with only key/index
type ForInStmt struct {
	ForPos   Pos       // Position of 'for' keyword
	Key      *Ident    // Key or index variable
	Value    *Ident    // Value variable (nil if not specified)
	Iterable Expr      // Expression to iterate over
	Body     *BlockStmt // Loop body
}

func (s *ForInStmt) stmtNode() {}

// Pos returns the position of first character belonging to the node.
func (s *ForInStmt) Pos() Pos {
	return s.ForPos
}

// End returns the position of first character immediately after the node.
func (s *ForInStmt) End() Pos {
	return s.Body.End()
}

func (s *ForInStmt) String() string {
	if s.Value != nil {
		return "for " + s.Key.String() + ", " + s.Value.String() +
			" in " + s.Iterable.String() + " " + s.Body.String()
	}
	return "for " + s.Key.String() + " in " + s.Iterable.String() +
		" " + s.Body.String()
}

// ForStmt represents a C-style for loop statement.
// All three components (init, cond, post) are optional.
//
// Example:
//
//	for i := 0; i < 10; i++ { ... }
//	for ; i < 10; { ... }      // equivalent to while loop
//	for { ... }                // infinite loop
type ForStmt struct {
	ForPos Pos        // Position of 'for' keyword
	Init   Stmt       // Initialization statement (may be nil)
	Cond   Expr       // Condition expression (may be nil)
	Post   Stmt       // Post iteration statement (may be nil)
	Body   *BlockStmt // Loop body
}

func (s *ForStmt) stmtNode() {}

// Pos returns the position of first character belonging to the node.
func (s *ForStmt) Pos() Pos {
	return s.ForPos
}

// End returns the position of first character immediately after the node.
func (s *ForStmt) End() Pos {
	return s.Body.End()
}

func (s *ForStmt) String() string {
	var init, cond, post string
	if s.Init != nil {
		init = s.Init.String()
	}
	if s.Cond != nil {
		cond = s.Cond.String() + " "
	}
	if s.Post != nil {
		post = s.Post.String()
	}

	if init != "" || post != "" {
		return "for " + init + " ; " + cond + " ; " + post + s.Body.String()
	}
	return "for " + cond + s.Body.String()
}

// IfStmt represents an if-else statement.
// An optional initialization statement can precede the condition.
//
// Example:
//
//	if x > 0 { ... }
//	if x > 0 { ... } else { ... }
//	if n := len(arr); n > 0 { ... }    // with init statement
type IfStmt struct {
	IfPos Pos        // Position of 'if' keyword
	Init  Stmt       // Optional initialization statement
	Cond  Expr       // Condition expression
	Body  *BlockStmt // If body
	Else  Stmt       // Else branch (BlockStmt or another IfStmt, may be nil)
}

func (s *IfStmt) stmtNode() {}

// Pos returns the position of first character belonging to the node.
func (s *IfStmt) Pos() Pos {
	return s.IfPos
}

// End returns the position of first character immediately after the node.
func (s *IfStmt) End() Pos {
	if s.Else != nil {
		return s.Else.End()
	}
	return s.Body.End()
}

func (s *IfStmt) String() string {
	var initStmt, elseStmt string
	if s.Init != nil {
		initStmt = s.Init.String() + "; "
	}
	if s.Else != nil {
		elseStmt = " else " + s.Else.String()
	}
	return "if " + initStmt + s.Cond.String() + " " +
		s.Body.String() + elseStmt
}

// IncDecStmt represents an increment or decrement statement.
// These are shorthand forms for assignment.
//
// Example:
//
//	i++
//	counter--
type IncDecStmt struct {
	Expr     Expr        // Expression to increment/decrement
	Token    token.Token // Inc (++) or Dec (--)
	TokenPos Pos         // Position of the operator
}

func (s *IncDecStmt) stmtNode() {}

// Pos returns the position of first character belonging to the node.
func (s *IncDecStmt) Pos() Pos {
	return s.Expr.Pos()
}

// End returns the position of first character immediately after the node.
func (s *IncDecStmt) End() Pos {
	return Pos(int(s.TokenPos) + 2)
}

func (s *IncDecStmt) String() string {
	return s.Expr.String() + s.Token.String()
}

// ReturnStmt represents a return statement in a function.
// Return can optionally include a value to return.
//
// Example:
//
//	return
//	return x
//	return x + y
type ReturnStmt struct {
	ReturnPos Pos  // Position of 'return' keyword
	Result    Expr // Return value expression (may be nil)
}

func (s *ReturnStmt) stmtNode() {}

// Pos returns the position of first character belonging to the node.
func (s *ReturnStmt) Pos() Pos {
	return s.ReturnPos
}

// End returns the position of first character immediately after the node.
func (s *ReturnStmt) End() Pos {
	if s.Result != nil {
		return s.Result.End()
	}
	return s.ReturnPos + 6
}

func (s *ReturnStmt) String() string {
	if s.Result != nil {
		return "return " + s.Result.String()
	}
	return "return"
}

// TryStmt represents a try-catch-finally statement for exception handling.
// Catch and finally clauses are optional but at least one must be present.
//
// Example:
//
//	try { ... }
//	try { ... } catch e { ... }
//	try { ... } finally { ... }
//	try { ... } catch e { ... } finally { ... }
type TryStmt struct {
	TryPos  Pos         // Position of 'try' keyword
	Body    *BlockStmt  // Try block
	Catch   *CatchStmt  // Catch clause (may be nil)
	Finally *FinallyStmt // Finally clause (may be nil)
}

func (s *TryStmt) stmtNode() {}

// Pos returns the position of first character belonging to the node.
func (s *TryStmt) Pos() Pos {
	return s.TryPos
}

// End returns the position of first character immediately after the node.
func (s *TryStmt) End() Pos {
	if s.Finally != nil {
		return s.Finally.End()
	}
	if s.Catch != nil {
		return s.Catch.End()
	}
	return s.Body.End()
}

func (s *TryStmt) String() string {
	var catchStmt, finallyStmt string
	if s.Catch != nil {
		catchStmt = s.Catch.String()
	}
	if s.Finally != nil {
		finallyStmt = s.Finally.String()
	}
	return "try " + s.Body.String() + " " + catchStmt + " " + finallyStmt
}

// CatchStmt represents a catch clause for exception handling.
// The caught exception can be bound to an optional identifier.
//
// Example:
//
//	catch { ... }       // catch without binding
//	catch e { ... }     // catch with binding
type CatchStmt struct {
	CatchPos Pos        // Position of 'catch' keyword
	Ident    *Ident     // Variable to bind the exception (may be nil)
	Body     *BlockStmt // Catch block
}

func (s *CatchStmt) stmtNode() {}

// Pos returns the position of first character belonging to the node.
func (s *CatchStmt) Pos() Pos {
	return s.CatchPos
}

// End returns the position of first character immediately after the node.
func (s *CatchStmt) End() Pos {
	return s.Body.End()
}

func (s *CatchStmt) String() string {
	var ident string
	if s.Ident != nil {
		ident = s.Ident.String()
	}
	return "catch " + ident + " " + s.Body.String()
}

// FinallyStmt represents a finally clause for cleanup code.
// Finally blocks are always executed, regardless of exceptions.
//
// Example:
//
//	finally { ... }
type FinallyStmt struct {
	FinallyPos Pos        // Position of 'finally' keyword
	Body       *BlockStmt // Finally block
}

func (s *FinallyStmt) stmtNode() {}

// Pos returns the position of first character belonging to the node.
func (s *FinallyStmt) Pos() Pos {
	return s.FinallyPos
}

// End returns the position of first character immediately after the node.
func (s *FinallyStmt) End() Pos {
	return s.Body.End()
}

func (s *FinallyStmt) String() string {
	return "finally " + s.Body.String()
}

// ThrowStmt represents a throw statement for raising exceptions.
// Throw can re-throw a caught exception or throw a new error.
//
// Example:
//
//	throw "error message"
//	throw error("something went wrong")
//	throw                   // re-throw current exception
type ThrowStmt struct {
	ThrowPos Pos  // Position of 'throw' keyword
	Expr     Expr // Expression to throw (may be nil for re-throw)
}

func (s *ThrowStmt) stmtNode() {}

// Pos returns the position of first character belonging to the node.
func (s *ThrowStmt) Pos() Pos {
	return s.ThrowPos
}

// End returns the position of first character immediately after the node.
func (s *ThrowStmt) End() Pos {
	return s.Expr.End()
}

func (s *ThrowStmt) String() string {
	var expr string
	if s.Expr != nil {
		expr = s.Expr.String()
	}
	return "throw " + expr
}
