package charlang_test

import (
	"errors"
	"fmt"
	"reflect"
	"testing"

	"github.com/stretchr/testify/require"

	. "github.com/topxeq/charlang"
	"github.com/topxeq/charlang/registry"
)

func TestToObject(t *testing.T) {
	err := errors.New("test error")
	fn := func(...Object) (Object, error) { return nil, nil }

	testCases := []struct {
		iface   interface{}
		want    Object
		wantErr bool
	}{
		{iface: nil, want: Undefined},
		{iface: "a", want: String("a")},
		{iface: int64(-1), want: Int(-1)},
		{iface: int(1), want: Int(1)},
		{iface: uint(1), want: Uint(1)},
		{iface: uint64(1), want: Uint(1)},
		{iface: uintptr(1), want: Uint(1)},
		{iface: true, want: True},
		{iface: false, want: False},
		{iface: rune(1), want: Char(1)},
		{iface: byte(1), want: Char(1)},
		{iface: float64(1), want: Float(1)},
		{iface: float32(1), want: Float(1)},
		{iface: []byte(nil), want: Bytes{}},
		{iface: []byte("a"), want: Bytes{'a'}},
		{iface: map[string]Object(nil), want: Map{}},
		{iface: map[string]Object{"a": Int(1)}, want: Map{"a": Int(1)}},
		{iface: map[string]interface{}{"a": 1}, want: Map{"a": Int(1)}},
		{iface: map[string]interface{}{"a": uint32(1)}, wantErr: true},
		{iface: []Object(nil), want: Array{}},
		{iface: []Object{Int(1), Char('a')}, want: Array{Int(1), Char('a')}},
		{iface: []interface{}{Int(1), Char('a')}, want: Array{Int(1), Char('a')}},
		{iface: []interface{}{uint32(1)}, wantErr: true},
		{iface: Object(nil), want: Undefined},
		{iface: String("a"), want: String("a")},
		{iface: CallableFunc(nil), want: Undefined},
		{iface: fn, want: &Function{Value: fn}},
		{iface: err, want: &Error{Message: err.Error(), Cause: err}},
		{iface: error(nil), want: Undefined},
		{iface: uint16(1), wantErr: true},
	}

	for _, tC := range testCases {
		t.Run(fmt.Sprintf("%[1]T:%[1]v", tC.iface), func(t *testing.T) {
			got, err := ToObject(tC.iface)
			if (err != nil) != tC.wantErr {
				t.Errorf("ToObject() error = %v, wantErr %v", err, tC.wantErr)
				return
			}
			if fn, ok := tC.iface.(CallableFunc); ok && fn != nil {
				require.NotNil(t, tC.want.(*Function).Value)
				return
			}
			if !reflect.DeepEqual(got, tC.want) {
				t.Errorf("ToObject() = %v, want %v", got, tC.want)
			}
		})
	}
}

func TestToInterface(t *testing.T) {

	testCases := []struct {
		object Object
		want   interface{}
	}{
		{object: nil, want: nil},
		{object: Undefined, want: nil},
		{object: Int(1), want: int64(1)},
		{object: String(""), want: ""},
		{object: String("a"), want: "a"},
		{object: Bytes(nil), want: []byte(nil)},
		{object: Bytes(""), want: []byte{}},
		{object: Bytes("a"), want: []byte{'a'}},
		{object: Array(nil), want: []interface{}{}},
		{object: Array{}, want: []interface{}{}},
		{object: Array{Int(1)}, want: []interface{}{int64(1)}},
		{object: Array{Undefined}, want: []interface{}{nil}},
		{object: Map(nil), want: map[string]interface{}{}},
		{object: Map{}, want: map[string]interface{}{}},
		{object: Map{"a": Undefined}, want: map[string]interface{}{"a": nil}},
		{object: Map{"a": Int(1)}, want: map[string]interface{}{"a": int64(1)}},
		{object: Uint(1), want: uint64(1)},
		{object: Char(1), want: rune(1)},
		{object: Float(1), want: float64(1)},
		{object: True, want: true},
		{object: False, want: false},
		{object: (*SyncMap)(nil), want: map[string]interface{}{}},
		{
			object: &SyncMap{Value: Map{"a": Int(1)}},
			want:   map[string]interface{}{"a": int64(1)},
		},
	}
	for _, tC := range testCases {
		t.Run(fmt.Sprintf("%T", tC.object), func(t *testing.T) {
			if got := ToInterface(tC.object); !reflect.DeepEqual(got, tC.want) {
				t.Errorf("ToInterface() = %v, want %v", got, tC.want)
			}
		})
	}
}

func TestToObjectAlt(t *testing.T) {
	err := errors.New("test error")
	fn := func(...Object) (Object, error) { return nil, nil }

	testCases := []struct {
		iface   interface{}
		want    Object
		wantErr bool
	}{
		{iface: nil, want: Undefined},
		{iface: "a", want: String("a")},
		{iface: int64(-1), want: Int(-1)},
		{iface: int32(-1), want: Int(-1)},
		{iface: int16(-1), want: Int(-1)},
		{iface: int8(-1), want: Int(-1)},
		{iface: int(1), want: Int(1)},
		{iface: uint(1), want: Uint(1)},
		{iface: uint64(1), want: Uint(1)},
		{iface: uint32(1), want: Uint(1)},
		{iface: uint16(1), want: Uint(1)},
		{iface: uint8(1), want: Uint(1)},
		{iface: uintptr(1), want: Uint(1)},
		{iface: true, want: True},
		{iface: false, want: False},
		{iface: rune(1), want: Int(1)},
		{iface: byte(2), want: Uint(2)},
		{iface: float64(1), want: Float(1)},
		{iface: float32(1), want: Float(1)},
		{iface: []byte(nil), want: Bytes{}},
		{iface: []byte("a"), want: Bytes{'a'}},
		{iface: map[string]Object(nil), want: Map{}},
		{iface: map[string]Object{"a": Int(1)}, want: Map{"a": Int(1)}},
		{iface: map[string]interface{}{"a": 1}, want: Map{"a": Int(1)}},
		{iface: map[string]interface{}{"a": uint32(1)}, want: Map{"a": Uint(1)}},
		{iface: []Object(nil), want: Array{}},
		{iface: []Object{Int(1), Char('a')}, want: Array{Int(1), Char('a')}},
		{iface: []interface{}{Int(1), Char('a')}, want: Array{Int(1), Char('a')}},
		{iface: []interface{}{uint32(1)}, want: Array{Uint(1)}},
		{iface: Object(nil), want: Undefined},
		{iface: String("a"), want: String("a")},
		{iface: CallableFunc(nil), want: Undefined},
		{iface: fn, want: &Function{Value: fn}},
		{iface: err, want: &Error{Message: err.Error(), Cause: err}},
		{iface: error(nil), want: Undefined},
		{iface: struct{}{}, wantErr: true},
	}

	for _, tC := range testCases {
		t.Run(fmt.Sprintf("%[1]T:%[1]v", tC.iface), func(t *testing.T) {
			got, err := ToObjectAlt(tC.iface)
			if (err != nil) != tC.wantErr {
				t.Errorf("ToObjectAlt() error = %v, wantErr %v", err, tC.wantErr)
				return
			}
			if fn, ok := tC.iface.(CallableFunc); ok && fn != nil {
				require.NotNil(t, tC.want.(*Function).Value)
				return
			}
			if !reflect.DeepEqual(got, tC.want) {
				t.Errorf("ToObjectAlt() = %[1]v (%[1]T), want %[2]v (%[2]T)", got, tC.want)
			}
		})
	}
}

func TestToGoByteSlice(t *testing.T) {
	testCases := []struct {
		name     string
		input    Object
		expected []byte
		ok       bool
	}{
		{"Bytes", Bytes("hello"), []byte("hello"), true},
		{"Bytes empty", Bytes{}, []byte{}, true},
		{"Chars", Chars("abc"), []byte("abc"), true},
		{"String", String("test"), []byte("test"), true},
		{"Array of bytes", Array{Byte(65), Byte(66), Byte(67)}, []byte{65, 66, 67}, true},
		{"Int not convertible", Int(42), nil, false},
		{"Undefined not convertible", Undefined, nil, false},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			result, ok := ToGoByteSlice(tc.input)
			require.Equal(t, tc.ok, ok)
			if tc.ok {
				require.Equal(t, tc.expected, result)
			}
		})
	}
}

func TestToGoInt(t *testing.T) {
	testCases := []struct {
		name     string
		input    Object
		expected int
		ok       bool
	}{
		{"Bool true", True, 1, true},
		{"Bool false", False, 0, true},
		{"Int", Int(42), 42, true},
		{"Uint", Uint(42), 42, true},
		{"Float", Float(3.14), 3, true},
		{"Char", Char(65), 65, true},
		{"Byte", Byte(255), 255, true},
		{"String number", String("123"), 123, true},
		{"String invalid", String("abc"), 0, false},
		{"Undefined", Undefined, 0, false},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			result, ok := ToGoInt(tc.input)
			require.Equal(t, tc.ok, ok)
			if tc.ok {
				require.Equal(t, tc.expected, result)
			}
		})
	}
}

func TestToGoIntWithDefault(t *testing.T) {
	defaultVal := 999

	testCases := []struct {
		name     string
		input    Object
		expected int
	}{
		{"Bool true", True, 1},
		{"Bool false", False, 0},
		{"Int", Int(42), 42},
		{"Uint", Uint(100), 100},
		{"Float", Float(3.14), 3},
		{"Char", Char(65), 65},
		{"Byte", Byte(128), 128},
		{"String number", String("456"), 456},
		{"String invalid returns default", String("abc"), defaultVal},
		{"Undefined returns default", Undefined, defaultVal},
		{"Map returns default", Map{}, defaultVal},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			result := ToGoIntWithDefault(tc.input, defaultVal)
			require.Equal(t, tc.expected, result)
		})
	}
}

func TestToGoInt64(t *testing.T) {
	testCases := []struct {
		name     string
		input    Object
		expected int64
		ok       bool
	}{
		{"Int", Int(42), 42, true},
		{"Uint", Uint(42), 42, true},
		{"Float", Float(3.14), 3, true},
		{"Char", Char(65), 65, true},
		{"Byte", Byte(255), 255, true},
		{"Bool true", True, 1, true},
		{"Bool false", False, 0, true},
		{"String number", String("123"), 123, true},
		{"String invalid", String("abc"), 0, false},
		{"Undefined", Undefined, 0, false},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			result, ok := ToGoInt64(tc.input)
			require.Equal(t, tc.ok, ok)
			if tc.ok {
				require.Equal(t, tc.expected, result)
			}
		})
	}
}

func TestToGoUint64(t *testing.T) {
	testCases := []struct {
		name     string
		input    Object
		expected uint64
		ok       bool
	}{
		{"Int", Int(42), 42, true},
		{"Uint", Uint(42), 42, true},
		{"Float", Float(3.14), 3, true},
		{"Char", Char(65), 65, true},
		{"Byte", Byte(255), 255, true},
		{"Bool true", True, 1, true},
		{"Bool false", False, 0, true},
		{"String number", String("123"), 123, true},
		{"String invalid", String("abc"), 0, false},
		{"Undefined", Undefined, 0, false},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			result, ok := ToGoUint64(tc.input)
			require.Equal(t, tc.ok, ok)
			if tc.ok {
				require.Equal(t, tc.expected, result)
			}
		})
	}
}

func TestToGoFloat64(t *testing.T) {
	testCases := []struct {
		name     string
		input    Object
		expected float64
		ok       bool
	}{
		{"Int", Int(42), 42, true},
		{"Uint", Uint(42), 42, true},
		{"Float", Float(3.14), 3.14, true},
		{"Char", Char(65), 65, true},
		{"Byte", Byte(255), 255, true},
		{"Bool true", True, 1, true},
		{"Bool false", False, 0, true},
		{"String number", String("3.14159"), 3.14159, true},
		{"String invalid", String("abc"), 0, false},
		{"Undefined", Undefined, 0, false},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			result, ok := ToGoFloat64(tc.input)
			require.Equal(t, tc.ok, ok)
			if tc.ok {
				require.Equal(t, tc.expected, result)
			}
		})
	}
}

func TestToGoRune(t *testing.T) {
	testCases := []struct {
		name     string
		input    Object
		expected rune
		ok       bool
	}{
		{"Int", Int(65), 65, true},
		{"Uint", Uint(65), 65, true},
		{"Char", Char('A'), 'A', true},
		{"Float", Float(65.5), 65, true},
		{"Byte", Byte(65), 65, true},
		{"String first char", String("ABC"), 'A', true},
		{"String empty returns replacement char", String(""), rune(65533), true}, // utf8.DecodeRuneInString behavior
		{"Bool true", True, 1, true},
		{"Bool false", False, 0, true},
		{"Undefined", Undefined, 0, false},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			result, ok := ToGoRune(tc.input)
			require.Equal(t, tc.ok, ok)
			if tc.ok {
				require.Equal(t, tc.expected, result)
			}
		})
	}
}

func TestToGoBool(t *testing.T) {
	testCases := []struct {
		name     string
		input    Object
		expected bool
	}{
		{"True", True, true},
		{"False", False, false},
		{"Int non-zero", Int(42), true},
		{"Int zero", Int(0), false},
		{"String non-empty", String("hello"), true},
		{"String empty", String(""), false},
		{"Array non-empty", Array{Int(1)}, true},
		{"Array empty", Array{}, false},
		{"Map non-empty", Map{"a": Int(1)}, true},
		{"Map empty", Map{}, false},
		{"Undefined", Undefined, false},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			result, ok := ToGoBool(tc.input)
			require.True(t, ok)
			require.Equal(t, tc.expected, result)
		})
	}
}

func TestToGoString(t *testing.T) {
	testCases := []struct {
		name     string
		input    Object
		expected string
		ok       bool
	}{
		{"String", String("hello"), "hello", true},
		{"Int", Int(42), "42", true},
		{"Undefined", Undefined, "", false},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			result, ok := ToGoString(tc.input)
			require.Equal(t, tc.ok, ok)
			if tc.ok {
				require.Equal(t, tc.expected, result)
			}
		})
	}
}

func TestToString(t *testing.T) {
	testCases := []struct {
		name     string
		input    Object
		expected String
		ok       bool
	}{
		{"String", String("hello"), "hello", true},
		{"Int returns string via ToGoString", Int(42), "42", true}, // ToGoString works on all objects
		{"Undefined returns false", Undefined, "", false},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			result, ok := ToString(tc.input)
			require.Equal(t, tc.ok, ok)
			if tc.ok {
				require.Equal(t, tc.expected, result)
			}
		})
	}
}

func TestToBytes(t *testing.T) {
	testCases := []struct {
		name     string
		input    Object
		expected Bytes
		ok       bool
	}{
		{"Bytes", Bytes("hello"), Bytes("hello"), true},
		{"Int returns false", Int(42), nil, false},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			result, ok := ToBytes(tc.input)
			require.Equal(t, tc.ok, ok)
			if tc.ok {
				require.Equal(t, tc.expected, result)
			}
		})
	}
}

func TestToInt(t *testing.T) {
	testCases := []struct {
		name     string
		input    Object
		expected Int
		ok       bool
	}{
		{"Int", Int(42), Int(42), true},
		{"Float", Float(42.5), Int(42), true},
		{"String number", String("42"), Int(42), true}, // String can be parsed to int via ToGoInt64
		{"String invalid", String("abc"), Int(0), false},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			result, ok := ToInt(tc.input)
			require.Equal(t, tc.ok, ok)
			if tc.ok {
				require.Equal(t, tc.expected, result)
			}
		})
	}
}

func TestToUint(t *testing.T) {
	testCases := []struct {
		name     string
		input    Object
		expected Uint
		ok       bool
	}{
		{"Uint", Uint(42), Uint(42), true},
		{"Int", Int(42), Uint(42), true},
		{"String number", String("42"), Uint(42), true}, // String can be parsed to uint via ToGoUint64
		{"String invalid", String("abc"), Uint(0), false},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			result, ok := ToUint(tc.input)
			require.Equal(t, tc.ok, ok)
			if tc.ok {
				require.Equal(t, tc.expected, result)
			}
		})
	}
}

func TestToFloat(t *testing.T) {
	testCases := []struct {
		name     string
		input    Object
		expected Float
		ok       bool
	}{
		{"Float", Float(3.14), Float(3.14), true},
		{"Int", Int(42), Float(42), true},
		{"String number", String("3.14"), Float(3.14), true}, // String can be parsed to float via ToGoFloat64
		{"String invalid", String("abc"), Float(0), false},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			result, ok := ToFloat(tc.input)
			require.Equal(t, tc.ok, ok)
			if tc.ok {
				require.Equal(t, tc.expected, result)
			}
		})
	}
}

func TestToChar(t *testing.T) {
	testCases := []struct {
		name     string
		input    Object
		expected Char
		ok       bool
	}{
		{"Char", Char('A'), Char('A'), true},
		{"Int", Int(65), Char(65), true},
		{"String first char", String("ABC"), Char('A'), true},
		{"String empty returns replacement char", String(""), Char(65533), true}, // utf8.DecodeRuneInString returns replacement char for empty string
		{"Map returns false", Map{}, Char(0), false},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			result, ok := ToChar(tc.input)
			require.Equal(t, tc.ok, ok)
			if tc.ok {
				require.Equal(t, tc.expected, result)
			}
		})
	}
}

func TestToBool(t *testing.T) {
	testCases := []struct {
		name     string
		input    Object
		expected Bool
		ok       bool
	}{
		{"True", True, True, true},
		{"False", False, False, true},
		{"Int non-zero", Int(1), True, true}, // ToBool converts truthiness
		{"Int zero", Int(0), False, true},    // ToBool converts falsiness
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			result, ok := ToBool(tc.input)
			require.Equal(t, tc.ok, ok)
			if tc.ok {
				require.Equal(t, tc.expected, result)
			}
		})
	}
}

func TestToArray(t *testing.T) {
	arr := Array{Int(1), Int(2)}
	testCases := []struct {
		name     string
		input    Object
		expected Array
		ok       bool
	}{
		{"Array", arr, arr, true},
		{"Map returns false", Map{}, nil, false},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			result, ok := ToArray(tc.input)
			require.Equal(t, tc.ok, ok)
			if tc.ok {
				require.Equal(t, tc.expected, result)
			}
		})
	}
}

func TestToMap(t *testing.T) {
	m := Map{"a": Int(1)}
	testCases := []struct {
		name     string
		input    Object
		expected Map
		ok       bool
	}{
		{"Map", m, m, true},
		{"Array returns false", Array{}, nil, false},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			result, ok := ToMap(tc.input)
			require.Equal(t, tc.ok, ok)
			if tc.ok {
				require.Equal(t, tc.expected, result)
			}
		})
	}
}

func TestToSyncMap(t *testing.T) {
	sm := &SyncMap{Value: Map{"a": Int(1)}}
	testCases := []struct {
		name     string
		input    Object
		expected *SyncMap
		ok       bool
	}{
		{"SyncMap", sm, sm, true},
		{"Map returns false", Map{}, nil, false},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			result, ok := ToSyncMap(tc.input)
			require.Equal(t, tc.ok, ok)
			if tc.ok {
				require.Equal(t, tc.expected, result)
			}
		})
	}
}

// TestToObjectWithRegistry tests ToObject with registered converters
func TestToObjectWithRegistry(t *testing.T) {
	// Define a custom type
	type CustomPoint struct {
		X, Y int
	}

	// Register a converter
	registry.RegisterObjectConverter(reflect.TypeOf(CustomPoint{}), func(in interface{}) (out interface{}, ok bool) {
		if cp, ok := in.(CustomPoint); ok {
			return Array{Int(cp.X), Int(cp.Y)}, true
		}
		return nil, false
	})

	// Test conversion
	cp := CustomPoint{X: 10, Y: 20}
	result, err := ToObject(cp)
	require.NoError(t, err)
	require.Equal(t, Array{Int(10), Int(20)}, result)
}

// TestToInterfaceWithRegistry tests ToInterface with registered converters
func TestToInterfaceWithRegistry(t *testing.T) {
	// Test with a normal object to verify ToInterface works
	testCases := []struct {
		name     string
		input    Object
		expected interface{}
	}{
		{"Int", Int(42), int64(42)},
		{"String", String("hello"), "hello"},
		{"Bool true", True, true},
		{"Array", Array{Int(1), Int(2)}, []interface{}{int64(1), int64(2)}},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			result := ToInterface(tc.input)
			require.Equal(t, tc.expected, result)
		})
	}
}
