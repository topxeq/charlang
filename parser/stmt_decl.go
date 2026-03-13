// Package parser implements a parser for Charlang source code.
// This file defines the declaration AST node types.
//
// # Declaration Types
//
// Declarations in Charlang include:
//   - Spec: Specification for a single variable or parameter
//   - ValueSpec: Variable declaration with optional initial value
//   - ParamSpec: Parameter declaration with optional variadic flag
//   - GenDecl: Generic declaration (var)
//   - DeclStmt: Declaration used as a statement
//   - BadDecl: Placeholder for declaration errors
package parser

import (
	"fmt"
	"strings"

	"github.com/topxeq/charlang/token"
)

// ----------------------------------------------------------------------------
// Declarations

type (
	// Spec represents a single variable or parameter specification.
	// The Spec type is the interface for *ParamSpec or *ValueSpec.
	Spec interface {
		Node
		specNode()
	}

	// ValueSpec represents a variable declaration with optional initial values.
	// Used within GenDecl for var statements.
	//
	// Example:
	//
	//	var x = 1
	//	var x, y = 1, 2
	ValueSpec struct {
		Idents []*Ident    // Variable names
		Values []Expr      // Initial values; or nil
		Data   interface{} // Additional data (e.g., iota)
	}

	// ParamSpec represents a function parameter declaration.
	// Parameters can be regular or variadic (...).
	//
	// Example:
	//
	//	x          // regular parameter
	//	...args    // variadic parameter
	ParamSpec struct {
		Ident    *Ident // Parameter name
		Variadic bool   // True if variadic (...)
	}
)

// Pos returns the position of first character belonging to the spec.
func (s *ParamSpec) Pos() Pos { return s.Ident.Pos() }

// Pos returns the position of first character belonging to the spec.
func (s *ValueSpec) Pos() Pos { return s.Idents[0].Pos() }

// End returns the position of first character immediately after the spec.
func (s *ParamSpec) End() Pos {
	return s.Ident.End()
}

// End returns the position of first character immediately after the spec.
func (s *ValueSpec) End() Pos {
	if n := len(s.Values); n > 0 && s.Values[n-1] != nil {
		return s.Values[n-1].End()
	}
	return s.Idents[len(s.Idents)-1].End()
}

func (s *ParamSpec) String() string {
	str := s.Ident.String()
	if s.Variadic {
		str = token.Ellipsis.String() + str
	}
	return str
}
func (s *ValueSpec) String() string {
	vals := make([]string, 0, len(s.Idents))
	for i := range s.Idents {
		if s.Values[i] != nil {
			vals = append(vals, fmt.Sprintf("%s = %v", s.Idents[i], s.Values[i]))
		} else {
			vals = append(vals, s.Idents[i].String())
		}
	}
	return strings.Join(vals, ", ")
}

// specNode() ensures that only spec nodes can be assigned to a Spec.
func (*ParamSpec) specNode() {}

// specNode() ensures that only spec nodes can be assigned to a Spec.
func (*ValueSpec) specNode() {}

// Decl represents a declaration node in the AST.
// All declaration types must implement this interface.
type Decl interface {
	Node
	declNode()
}

// DeclStmt represents a declaration used as a statement.
// This wraps a GenDecl (var statement) in statement contexts.
//
// Example:
//
//	var x = 1    // this is a DeclStmt
type DeclStmt struct {
	Decl // *GenDecl with VAR token
}

func (*DeclStmt) stmtNode() {}

// BadDecl represents a malformed declaration that could not be parsed.
// It serves as a placeholder for error recovery.
type BadDecl struct {
	From, To Pos // Position range of the bad declaration
}

// GenDecl represents a generic declaration (var statement).
// Declarations can be parenthesized or single-line.
//
// Example:
//
//	var x = 1              // single declaration
//	var (                  // parenthesized declarations
//	    a = 1
//	    b = 2
//	)
type GenDecl struct {
	TokPos Pos         // Position of the declaration keyword
	Tok    token.Token // Token (Var)
	Lparen Pos         // Position of '(', if parenthesized
	Specs  []Spec      // Variable specifications
	Rparen Pos         // Position of ')', if parenthesized
}

// Pos returns the position of first character belonging to the node.
func (d *BadDecl) Pos() Pos { return d.From }

// Pos returns the position of first character belonging to the node.
func (d *GenDecl) Pos() Pos { return d.TokPos }

// End returns the position of first character immediately after the node.
func (d *BadDecl) End() Pos { return d.To }

// End returns the position of first character immediately after the node.
func (d *GenDecl) End() Pos {
	if d.Rparen.IsValid() {
		return d.Rparen + 1
	}
	return d.Specs[0].End()
}

func (*BadDecl) declNode() {}
func (*GenDecl) declNode() {}

func (*BadDecl) String() string { return "<bad declaration>" }
func (d *GenDecl) String() string {
	var sb strings.Builder
	sb.WriteString(d.Tok.String())
	if d.Lparen > 0 {
		sb.WriteString(" (")
	} else {
		sb.WriteString(" ")
	}
	last := len(d.Specs) - 1
	for i := range d.Specs {
		sb.WriteString(d.Specs[i].String())
		if i != last {
			sb.WriteString(", ")
		}
	}
	if d.Rparen > 0 {
		sb.WriteString(")")
	}
	return sb.String()
}
