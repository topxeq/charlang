// Package parser implements a parser for Charlang source code.
// This file defines source file and position tracking types.
//
// # Position Tracking
//
// The parser tracks positions using two types:
//   - Pos: Compact integer position within a file set
//   - SourceFilePos: Human-readable position with filename, line, column
//
// # File Sets
//
// SourceFileSet manages a collection of source files with unique position
// ranges. This allows positions from multiple files to be compared and sorted.
//
// # Usage
//
//	fset := NewFileSet()
//	file := fset.AddFile("example.ch", -1, len(source))
//	// After parsing, convert positions:
//	pos := fset.Position(somePos)  // returns SourceFilePos
package parser

import (
	"fmt"
	"sort"
)

// SourceFilePos represents a human-readable position in a source file.
// It includes filename, offset, line, and column information for error messages.
type SourceFilePos struct {
	Filename string // Filename, if any
	Offset   int    // Byte offset, starting at 0
	Line     int    // Line number, starting at 1
	Column   int    // Column number, starting at 1 (byte count)
}

// IsValid returns true if the position is valid.
func (p SourceFilePos) IsValid() bool {
	return p.Line > 0
}

// String returns a string in one of several forms:
//
//	file:line:column    valid position with file name
//	file:line           valid position with file name but no column (column == 0)
//	line:column         valid position without file name
//	line                valid position without file name and no column (column == 0)
//	file                invalid position with file name
//	-                   invalid position without file name
func (p SourceFilePos) String() string {
	s := p.Filename
	if p.IsValid() {
		if s != "" {
			s += ":"
		}
		s += fmt.Sprintf("%d", p.Line)
		if p.Column != 0 {
			s += fmt.Sprintf(":%d", p.Column)
		}
	}
	if s == "" {
		s = "-"
	}
	return s
}

// SourceFileSet represents a set of source files with position management.
// It assigns unique position ranges to each file, allowing positions
// from different files to be distinguished.
//
// Position values are integers that encode both the file and offset.
// The Base field determines the starting position for the next added file.
type SourceFileSet struct {
	Base     int           // Base offset for the next file
	Files    []*SourceFile // List of files in order added
	LastFile *SourceFile   // Cache of last file looked up
}

// NewFileSet creates a new empty file set.
// The initial base is 1, as 0 represents NoPos (invalid position).
func NewFileSet() *SourceFileSet {
	return &SourceFileSet{
		Base: 1, // 0 == NoPos
	}
}

// AddFile adds a new file to the file set and returns it.
//
// Parameters:
//   - filename: Name of the file (for error messages)
//   - base: Base position (-1 to auto-assign)
//   - size: Size of the file in bytes
//
// Returns the SourceFile for position lookups.
// Panics if base is invalid or size is negative.
func (s *SourceFileSet) AddFile(filename string, base, size int) *SourceFile {
	if base < 0 {
		base = s.Base
	}
	if base < s.Base || size < 0 {
		panic("illegal base or size")
	}
	f := &SourceFile{
		set:   s,
		Name:  filename,
		Base:  base,
		Size:  size,
		Lines: []int{0},
	}
	base += size + 1 // +1 because EOF also has a position
	if base < 0 {
		panic("offset overflow (> 2G of source code in file set)")
	}

	// add the file to the file set
	s.Base = base
	s.Files = append(s.Files, f)
	s.LastFile = f
	return f
}

// File returns the file that contains the given position.
// Returns nil if no matching file is found (e.g., for NoPos).
func (s *SourceFileSet) File(p Pos) (f *SourceFile) {
	if p != NoPos {
		f = s.file(p)
	}
	return
}

// Position converts a Pos value to a human-readable SourceFilePos.
// Returns a zero SourceFilePos for NoPos.
func (s *SourceFileSet) Position(p Pos) (pos SourceFilePos) {
	if p != NoPos {
		if f := s.file(p); f != nil {
			return f.position(p)
		}
	}
	return
}

func (s *SourceFileSet) file(p Pos) *SourceFile {
	// common case: p is in last file
	f := s.LastFile
	if f != nil && f.Base <= int(p) && int(p) <= f.Base+f.Size {
		return f
	}

	// p is not in last file - search all files
	if i := searchFiles(s.Files, int(p)); i >= 0 {
		f := s.Files[i]

		// f.base <= int(p) by definition of searchFiles
		if int(p) <= f.Base+f.Size {
			s.LastFile = f // race is ok - s.last is only a cache
			return f
		}
	}
	return nil
}

func searchFiles(a []*SourceFile, x int) int {
	return sort.Search(len(a), func(i int) bool { return a[i].Base > x }) - 1
}

// SourceFile represents a single source file with position tracking.
// It maintains a mapping between byte offsets and line numbers.
type SourceFile struct {
	// SourceFile set for the file
	set *SourceFileSet
	// SourceFile name as provided to AddFile
	Name string
	// Pos value range for this file is [base...base+size]
	Base int
	// SourceFile size as provided to AddFile
	Size int
	// Lines contains the offset of the first character for each line
	// (the first entry is always 0)
	Lines []int
}

// Set returns SourceFileSet.
func (f *SourceFile) Set() *SourceFileSet {
	return f.set
}

// LineCount returns the current number of lines.
func (f *SourceFile) LineCount() int {
	return len(f.Lines)
}

// AddLine adds a new line.
func (f *SourceFile) AddLine(offset int) {
	i := len(f.Lines)
	if (i == 0 || f.Lines[i-1] < offset) && offset < f.Size {
		f.Lines = append(f.Lines, offset)
	}
}

// LineStart returns the position of the first character in the line.
func (f *SourceFile) LineStart(line int) Pos {
	if line < 1 {
		panic("illegal line number (line numbering starts at 1)")
	}
	if line > len(f.Lines) {
		panic("illegal line number")
	}
	return Pos(f.Base + f.Lines[line-1])
}

// FileSetPos returns the position in the file set.
func (f *SourceFile) FileSetPos(offset int) Pos {
	if offset > f.Size {
		panic("illegal file offset")
	}
	return Pos(f.Base + offset)
}

// Offset translates the file set position into the file offset.
func (f *SourceFile) Offset(p Pos) int {
	if int(p) < f.Base || int(p) > f.Base+f.Size {
		panic("illegal SourcePos value")
	}
	return int(p) - f.Base
}

// Line returns the line of given position.
func (f *SourceFile) Line(p Pos) int {
	return f.Position(p).Line
}

// Position translates the file set position into the file position.
func (f *SourceFile) Position(p Pos) (pos SourceFilePos) {
	if p != NoPos {
		if int(p) < f.Base || int(p) > f.Base+f.Size {
			panic("illegal SourcePos value")
		}
		pos = f.position(p)
	}
	return
}

func (f *SourceFile) position(p Pos) (pos SourceFilePos) {
	offset := int(p) - f.Base
	pos.Offset = offset
	pos.Filename, pos.Line, pos.Column = f.unpack(offset)
	return
}

func (f *SourceFile) unpack(offset int) (filename string, line, column int) {
	filename = f.Name
	if i := searchInts(f.Lines, offset); i >= 0 {
		line, column = i+1, offset-f.Lines[i]+1
	}
	return
}

func searchInts(a []int, x int) int {
	// This function body is a manually inlined version of:
	//   return sort.Search(len(a), func(i int) bool { return a[i] > x }) - 1
	i, j := 0, len(a)
	for i < j {
		h := i + (j-i)/2 // avoid overflow when computing h
		// i ≤ h < j
		if a[h] <= x {
			i = h + 1
		} else {
			j = h
		}
	}
	return i - 1
}
