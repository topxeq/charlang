// Package parser implements a parser for Charlang source code.
// This file defines the File AST node, which is the root of a parsed source file.
package parser

import (
	"strings"
)

// File represents a parsed source file as the root of the AST.
// It contains the source file metadata, parsed statements, and optional comments.
//
// This is the top-level node returned by Parser.ParseFile().
type File struct {
	InputFile *SourceFile     // Source file metadata
	Stmts     []Stmt          // Top-level statements
	Comments  []*CommentGroup // Comment groups (if ParseComments mode)
}

// Pos returns the position of first character belonging to the node.
func (n *File) Pos() Pos {
	return Pos(n.InputFile.Base)
}

// End returns the position of first character immediately after the node.
func (n *File) End() Pos {
	return Pos(n.InputFile.Base + n.InputFile.Size)
}

func (n *File) String() string {
	var stmts []string
	for _, e := range n.Stmts {
		stmts = append(stmts, e.String())
	}
	return strings.Join(stmts, "; ")
}
