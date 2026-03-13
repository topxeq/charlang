// Package parser implements a parser for Charlang source code.
// This file defines the Pos type for compact position representation.
//
// # Position Types
//
// The parser uses two position types:
//   - Pos: Compact integer position for efficient comparison and storage
//   - SourceFilePos: Full position with filename, line, and column for display
//
// Pos values are only meaningful within a SourceFileSet, which provides
// the context to convert them to SourceFilePos values.
package parser

// Pos represents a compact position in a source file set.
// It is an integer offset that uniquely identifies a location across
// all files in the set.
//
// Pos values can be compared (<, >, ==) to determine relative positions.
// Use SourceFileSet.Position() to convert to human-readable form.
type Pos int

// NoPos represents an invalid or unknown position.
// It is the zero value for Pos.
const NoPos Pos = 0

// IsValid returns true if the position is valid (non-zero).
func (p Pos) IsValid() bool {
	return p != NoPos
}
