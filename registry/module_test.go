package registry_test

import (
	"fmt"
	"reflect"
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/topxeq/charlang/registry"
)

// Custom type for testing converters
type CustomType struct {
	Value string
}

// Another custom type for testing
type AnotherType struct {
	Number int
}

func TestBuiltinObjects(t *testing.T) {
	// Test case when no converter is registered
	rs, ok := registry.ToObject(true)
	require.False(t, ok, "Should return false when no converter registered")
	require.Nil(t, rs, "Should return nil when no converter registered")

	rs2, ok := registry.ToInterface(true)
	require.False(t, ok, "Should return false when no converter registered")
	require.Nil(t, rs2, "Should return nil when no converter registered")

	rs3, ok := registry.ToObject(3)
	require.False(t, ok, "Should return false when no converter registered")
	require.Nil(t, rs3, "Should return nil when no converter registered")
}

func TestRegisterObjectConverter(t *testing.T) {
	// Register a converter for CustomType
	registry.RegisterObjectConverter(reflect.TypeOf(CustomType{}), func(in interface{}) (out interface{}, ok bool) {
		if ct, ok := in.(CustomType); ok {
			return fmt.Sprintf("converted:%s", ct.Value), true
		}
		return nil, false
	})

	// Test successful conversion
	t.Run("successful conversion", func(t *testing.T) {
		ct := CustomType{Value: "test"}
		result, ok := registry.ToObject(ct)
		require.True(t, ok, "Should return true for registered type")
		require.Equal(t, "converted:test", result)
	})

	// Test with nil input
	t.Run("nil input", func(t *testing.T) {
		result, ok := registry.ToObject(nil)
		require.False(t, ok, "Should return false for nil input")
		require.Nil(t, result)
	})

	// Test with unregistered type
	t.Run("unregistered type", func(t *testing.T) {
		at := AnotherType{Number: 42}
		result, ok := registry.ToObject(at)
		require.False(t, ok, "Should return false for unregistered type")
		require.Nil(t, result)
	})
}

func TestRegisterAnyConverter(t *testing.T) {
	// Register a converter for AnotherType
	registry.RegisterAnyConverter(reflect.TypeOf(AnotherType{}), func(in interface{}) (out interface{}, ok bool) {
		if at, ok := in.(AnotherType); ok {
			return map[string]interface{}{"number": at.Number}, true
		}
		return nil, false
	})

	// Test successful conversion
	t.Run("successful conversion", func(t *testing.T) {
		at := AnotherType{Number: 42}
		result, ok := registry.ToInterface(at)
		require.True(t, ok, "Should return true for registered type")
		require.Equal(t, map[string]interface{}{"number": 42}, result)
	})

	// Test with nil input
	t.Run("nil input", func(t *testing.T) {
		result, ok := registry.ToInterface(nil)
		require.False(t, ok, "Should return false for nil input")
		require.Nil(t, result)
	})

	// Test with unregistered type (using a new type not registered)
	t.Run("unregistered type", func(t *testing.T) {
		type UnregisteredType struct {
			Data string
		}
		ut := UnregisteredType{Data: "test"}
		result, ok := registry.ToInterface(ut)
		require.False(t, ok, "Should return false for unregistered type")
		require.Nil(t, result)
	})
}

func TestToObjectWithPointerType(t *testing.T) {
	// Register converter for pointer type
	registry.RegisterObjectConverter(reflect.TypeOf(&CustomType{}), func(in interface{}) (out interface{}, ok bool) {
		if ct, ok := in.(*CustomType); ok && ct != nil {
			return fmt.Sprintf("ptr:%s", ct.Value), true
		}
		return nil, false
	})

	t.Run("pointer type conversion", func(t *testing.T) {
		ct := &CustomType{Value: "pointer"}
		result, ok := registry.ToObject(ct)
		require.True(t, ok, "Should return true for pointer type")
		require.Equal(t, "ptr:pointer", result)
	})

	t.Run("nil pointer", func(t *testing.T) {
		var ct *CustomType = nil
		result, ok := registry.ToObject(ct)
		// When pointer is nil, reflect.TypeOf returns nil, so no converter is found
		require.False(t, ok, "Nil pointer should not match converter (reflect.TypeOf returns nil)")
		require.Nil(t, result)
	})
}

func TestToInterfaceWithPointerType(t *testing.T) {
	// Register converter for pointer type
	registry.RegisterAnyConverter(reflect.TypeOf(&AnotherType{}), func(in interface{}) (out interface{}, ok bool) {
		if at, ok := in.(*AnotherType); ok && at != nil {
			return []interface{}{at.Number}, true
		}
		return nil, false
	})

	t.Run("pointer type conversion", func(t *testing.T) {
		at := &AnotherType{Number: 100}
		result, ok := registry.ToInterface(at)
		require.True(t, ok, "Should return true for pointer type")
		require.Equal(t, []interface{}{100}, result)
	})
}

func TestConverterOverwrite(t *testing.T) {
	// Define a unique type for this test
	type OverwriteType struct {
		Val int
	}

	// Register first converter
	registry.RegisterObjectConverter(reflect.TypeOf(OverwriteType{}), func(in interface{}) (out interface{}, ok bool) {
		return "first", true
	})

	// Verify first converter works
	ot := OverwriteType{Val: 1}
	result, ok := registry.ToObject(ot)
	require.True(t, ok)
	require.Equal(t, "first", result)

	// Overwrite with second converter
	registry.RegisterObjectConverter(reflect.TypeOf(OverwriteType{}), func(in interface{}) (out interface{}, ok bool) {
		return "second", true
	})

	// Verify second converter is used
	result, ok = registry.ToObject(ot)
	require.True(t, ok)
	require.Equal(t, "second", result, "Converter should be overwritten")
}

func TestAnyConverterOverwrite(t *testing.T) {
	// Define a unique type for this test
	type OverwriteAnyType struct {
		Val string
	}

	// Register first converter
	registry.RegisterAnyConverter(reflect.TypeOf(OverwriteAnyType{}), func(in interface{}) (out interface{}, ok bool) {
		return "first-any", true
	})

	// Verify first converter works
	oat := OverwriteAnyType{Val: "test"}
	result, ok := registry.ToInterface(oat)
	require.True(t, ok)
	require.Equal(t, "first-any", result)

	// Overwrite with second converter
	registry.RegisterAnyConverter(reflect.TypeOf(OverwriteAnyType{}), func(in interface{}) (out interface{}, ok bool) {
		return "second-any", true
	})

	// Verify second converter is used
	result, ok = registry.ToInterface(oat)
	require.True(t, ok)
	require.Equal(t, "second-any", result, "Converter should be overwritten")
}

func TestToObjectWithBuiltInTypes(t *testing.T) {
	// Built-in types don't have converters by default in registry
	// These should return false

	testCases := []struct {
		name  string
		input interface{}
	}{
		{"string", "hello"},
		{"int", 42},
		{"float64", 3.14},
		{"bool", true},
		{"slice", []int{1, 2, 3}},
		{"map", map[string]int{"a": 1}},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			result, ok := registry.ToObject(tc.input)
			require.False(t, ok, "Built-in types should not have converters by default")
			require.Nil(t, result)
		})
	}
}

func TestToInterfaceWithBuiltInTypes(t *testing.T) {
	// Built-in types don't have converters by default in registry
	// These should return false

	testCases := []struct {
		name  string
		input interface{}
	}{
		{"string", "hello"},
		{"int", 42},
		{"float64", 3.14},
		{"bool", true},
		{"slice", []int{1, 2, 3}},
		{"map", map[string]int{"a": 1}},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			result, ok := registry.ToInterface(tc.input)
			require.False(t, ok, "Built-in types should not have converters by default")
			require.Nil(t, result)
		})
	}
}

func TestConverterReturningFalse(t *testing.T) {
	type RejectType struct {
		ShouldReject bool
	}

	registry.RegisterObjectConverter(reflect.TypeOf(RejectType{}), func(in interface{}) (out interface{}, ok bool) {
		if rt, ok := in.(RejectType); ok {
			if rt.ShouldReject {
				return nil, false
			}
			return "accepted", true
		}
		return nil, false
	})

	t.Run("converter accepts", func(t *testing.T) {
		rt := RejectType{ShouldReject: false}
		result, ok := registry.ToObject(rt)
		require.True(t, ok)
		require.Equal(t, "accepted", result)
	})

	t.Run("converter rejects", func(t *testing.T) {
		rt := RejectType{ShouldReject: true}
		result, ok := registry.ToObject(rt)
		require.False(t, ok, "Converter should reject based on input value")
		require.Nil(t, result)
	})
}
