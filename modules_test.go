package charlang

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func TestModuleMap(t *testing.T) {
	mm := NewModuleMap()
	require.NotNil(t, mm)

	// Add a builtin module
	builtinMod := Map{"test": Int(1)}
	mm.AddBuiltinModule("testmod", builtinMod)

	// Get the module
	retrieved := mm.Get("testmod")
	require.NotNil(t, retrieved)

	// Test Fork with module name
	forked := mm.Fork("testmod")
	require.NotNil(t, forked)

	// Verify forked has same modules
	forkedMod := forked.Get("testmod")
	require.NotNil(t, forkedMod)

	// Remove module
	mm.Remove("testmod")
	retrieved = mm.Get("testmod")
	require.Nil(t, retrieved)
}

func TestModuleMapCopy(t *testing.T) {
	mm := NewModuleMap()

	mod1Attrs := Map{"a": Int(1)}
	mod2Attrs := Map{"b": Int(2)}

	mm.AddBuiltinModule("mod1", mod1Attrs)
	mm.AddBuiltinModule("mod2", mod2Attrs)

	// Copy should work without error
	copied := mm.Copy()
	require.NotNil(t, copied)

	// Verify copied modules exist
	m1 := copied.Get("mod1")
	require.NotNil(t, m1)

	m2 := copied.Get("mod2")
	require.NotNil(t, m2)
}

func TestBuiltinModule(t *testing.T) {
	mm := NewModuleMap()

	// Add builtin module
	builtinMod := Map{"builtin": True}
	mm.AddBuiltinModule("builtin", builtinMod)

	// Should be retrievable
	retrieved := mm.Get("builtin")
	require.NotNil(t, retrieved)

	// Test importing the module
	result, err := retrieved.(*BuiltinModule).Import("builtin")
	require.NoError(t, err)
	require.NotNil(t, result)
}

func TestSourceModuleDirect(t *testing.T) {
	// Add source module
	src := []byte(`return 42`)
	sourceMod := &SourceModule{Src: src}

	// Test importing the source module
	result, err := sourceMod.Import("sourcemod")
	require.NoError(t, err)
	require.Equal(t, src, result)
}

func TestBuiltinModuleImport(t *testing.T) {
	// Test BuiltinModule.Import directly
	mod := &BuiltinModule{Attrs: Map{"x": Int(100)}}
	result, err := mod.Import("testmod")
	require.NoError(t, err)
	require.NotNil(t, result)

	// Should contain the __module_name__ attribute
	resultMap := result.(Map)
	require.Equal(t, String("testmod"), resultMap[AttrModuleName])
	require.Equal(t, Int(100), resultMap["x"])
}

func TestBuiltinModuleImportNilAttrs(t *testing.T) {
	// Test BuiltinModule.Import with nil attrs
	mod := &BuiltinModule{Attrs: nil}
	result, err := mod.Import("testmod")
	require.Error(t, err)
	require.Nil(t, result)
	require.Equal(t, "module attributes not set", err.Error())
}

func TestModuleMapSetExtImporter(t *testing.T) {
	mm := NewModuleMap()
	require.NotNil(t, mm)

	// SetExtImporter should be callable (we don't have a concrete ExtImporter, but just testing it doesn't panic)
	result := mm.SetExtImporter(nil)
	require.Equal(t, mm, result)
}

func TestModuleMapForkNil(t *testing.T) {
	// Test Fork on nil ModuleMap
	var mm *ModuleMap
	result := mm.Fork("test")
	require.Nil(t, result)
}

func TestModuleMapGetNil(t *testing.T) {
	// Test Get on nil ModuleMap
	var mm *ModuleMap
	result := mm.Get("test")
	require.Nil(t, result)
}
