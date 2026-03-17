package charlang

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestSymbolTable(t *testing.T) {
	st := NewSymbolTable()
	require.NotNil(t, st)

	// Test defining and resolving
	st.DefineLocal("x")
	sym, ok := st.Resolve("x")
	require.True(t, ok)
	require.Equal(t, "x", sym.Name)

	// Test String representation of Symbol
	str := sym.String()
	require.NotEmpty(t, str)
	require.Contains(t, str, "x")
}

func TestSymbolTableParent(t *testing.T) {
	parent := NewSymbolTable()
	_, err := parent.DefineGlobal("globalVar")
	require.NoError(t, err)

	child := parent.Fork(false)

	// Parent should be accessible
	p := child.Parent(false)
	require.Equal(t, parent, p)
}

func TestSymbolTableParentSkipBlock(t *testing.T) {
	grandparent := NewSymbolTable()
	parent := grandparent.Fork(true) // block scope
	child := parent.Fork(false)

	// With skipBlock=false, should return direct parent
	p := child.Parent(false)
	require.Equal(t, parent, p)

	// With skipBlock=true on a non-block child should still return parent
	p = child.Parent(true)
	require.Equal(t, parent, p)
}

func TestSymbolTableDefineLocal(t *testing.T) {
	st := NewSymbolTable()

	// First definition should not be redeclared
	sym, redeclared := st.DefineLocal("testVar")
	require.False(t, redeclared)
	require.Equal(t, "testVar", sym.Name)
	require.Equal(t, ScopeLocal, sym.Scope)

	// Second definition should return redeclared=true
	sym2, redeclared2 := st.DefineLocal("testVar")
	require.True(t, redeclared2)
	require.Equal(t, sym, sym2)
}

func TestSymbolTableDefineGlobal(t *testing.T) {
	st := NewSymbolTable()

	// Define global in top scope
	sym, err := st.DefineGlobal("globalVar")
	require.NoError(t, err)
	require.Equal(t, "globalVar", sym.Name)
	require.Equal(t, ScopeGlobal, sym.Scope)

	// Try to define global in child scope (should fail)
	child := st.Fork(false)
	sym, err = child.DefineGlobal("childGlobal")
	require.Error(t, err)
	require.Equal(t, "global declaration can be at top scope", err.Error())
	require.Nil(t, sym)

	// Define global again in top scope (should return existing)
	sym, err = st.DefineGlobal("globalVar")
	require.NoError(t, err)
	require.Equal(t, "globalVar", sym.Name)
}

func TestSymbolTableSetParams(t *testing.T) {
	st := NewSymbolTable()

	// Set params for the first time
	err := st.SetParams("param1", "param2")
	require.NoError(t, err)
	require.Equal(t, 2, st.NumParams())

	// Try to set params again (should fail)
	err = st.SetParams("param3")
	require.Error(t, err)
	require.Equal(t, "parameters already defined", err.Error())

	// Try to set params when disabled
	st2 := NewSymbolTable()
	st2.EnableParams(false)
	err = st2.SetParams("param1")
	require.Error(t, err)
	require.Equal(t, "parameters disabled", err.Error())

	// Try to set duplicate params
	st3 := NewSymbolTable()
	st3.DefineLocal("param1")
	err = st3.SetParams("param1")
	require.Error(t, err)
	require.Contains(t, err.Error(), "redeclared in this block")
}

func TestSymbolTableDisableBuiltin(t *testing.T) {
	st := NewSymbolTable()

	// Initially no disabled builtins
	disabled := st.DisabledBuiltins()
	require.Nil(t, disabled)

	// Disable a builtin
	st.DisableBuiltin("print")
	disabled = st.DisabledBuiltins()
	require.Len(t, disabled, 1)
	require.Contains(t, disabled, "print")

	// Disable multiple builtins
	st.DisableBuiltin("len", "append")
	disabled = st.DisabledBuiltins()
	require.Len(t, disabled, 3)

	// Check isBuiltinDisabled (internal method tested via public API)
	require.True(t, st.isBuiltinDisabled("print"))
	require.False(t, st.isBuiltinDisabled("notDisabled"))
}

func TestSymbolTableDisabledBuiltinsInChild(t *testing.T) {
	parent := NewSymbolTable()
	parent.DisableBuiltin("print")

	child := parent.Fork(false)

	// Child should see parent's disabled builtins
	disabled := child.DisabledBuiltins()
	require.Len(t, disabled, 1)
	require.Contains(t, disabled, "print")

	// isBuiltinDisabled should delegate to parent
	require.True(t, child.isBuiltinDisabled("print"))
}

func TestSymbolTableShadowedBuiltins(t *testing.T) {
	st := NewSymbolTable()

	// Define a symbol that shadows a builtin
	st.DefineLocal("len")

	// Should be in shadowed builtins
	shadowed := st.ShadowedBuiltins()
	require.Len(t, shadowed, 1)
	require.Contains(t, shadowed, "len")
}

func TestSymbolTableFork(t *testing.T) {
	st := NewSymbolTable()
	st.DefineLocal("parentVar")

	// Fork a non-block scope
	child := st.Fork(false)
	require.False(t, child.InBlock())

	// Fork a block scope
	blockChild := st.Fork(true)
	require.True(t, blockChild.InBlock())
}

func TestSymbolTableSymbols(t *testing.T) {
	st := NewSymbolTable()
	st.DefineLocal("var1")
	st.DefineLocal("var2")

	symbols := st.Symbols()
	require.Len(t, symbols, 2)
}

func TestSymbolTableFreeSymbols(t *testing.T) {
	parent := NewSymbolTable()
	parent.DefineLocal("parentVar")

	child := parent.Fork(false)

	// Resolve to trigger free symbol creation
	child.Resolve("parentVar")

	freeSymbols := child.FreeSymbols()
	require.Len(t, freeSymbols, 1)
}

func TestSymbolTableMaxSymbols(t *testing.T) {
	st := NewSymbolTable()
	require.Equal(t, 0, st.MaxSymbols())

	st.DefineLocal("var1")
	st.DefineLocal("var2")

	require.Equal(t, 2, st.MaxSymbols())
}

func TestSymbolTableEnableParams(t *testing.T) {
	st := NewSymbolTable()

	// Enable params (default)
	result := st.EnableParams(true)
	require.Equal(t, st, result)

	// Disable params
	result = st.EnableParams(false)
	require.Equal(t, st, result)
}
