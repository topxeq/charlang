package charlang_test

import (
	"testing"

	. "github.com/topxeq/charlang"
)

// TestZFuncWrappers tests the generated wrapper functions in zfuncs.go
// by executing scripts that use various builtins.
func TestZFuncWrappers(t *testing.T) {
	// Test funcPOsReEx and funcPOsRe patterns (Object, string -> error)
	expectRun(t, `
		m := {}
		delete(m, "key")
		return m
	`, nil, Map{})

	// Test funcPOROEx and funcPORO patterns (Object -> Object)
	expectRun(t, `
		return typeOf(123)
	`, nil, String("int"))

	expectRun(t, `
		return bool(1)
	`, nil, True)

	expectRun(t, `
		return int(true)
	`, nil, Int(1))

	expectRun(t, `
		return float(1)
	`, nil, Float(1))

	expectRun(t, `
		return char(65)
	`, nil, Char('A'))

	expectRun(t, `
		return string(65)
	`, nil, String("65"))

	expectRun(t, `
		return byte(65)
	`, nil, Byte(65))

	// Test funcPOiROeEx and funcPOiROe patterns (Object, int -> Object, error)
	expectRun(t, `
		arr := [1, 2, 3]
		return append(arr, 4)
	`, nil, Array{Int(1), Int(2), Int(3), Int(4)})

	// Test funcPiOROeEx and funcPiOROe patterns (int, Object -> Object, error)
	expectRun(t, `
		return repeat("x", 3)
	`, nil, String("xxx"))

	// Test funcPOOROeEx and funcPOOROe patterns (Object, Object -> Object, error)
	expectRun(t, `
		arr := [1, 2, 3]
		return contains(arr, 2)
	`, nil, True)

	// Test funcPi64ROEx and funcPi64RO patterns (int64 -> Object)
	expectRun(t, `
		return isInt(123)
	`, nil, True)

	expectRun(t, `
		return isString("test")
	`, nil, True)

	expectRun(t, `
		return isBool(true)
	`, nil, True)

	expectRun(t, `
		return isArray([1, 2])
	`, nil, True)

	expectRun(t, `
		return isMap({a: 1})
	`, nil, True)

	expectRun(t, `
		return isFunction(func() {})
	`, nil, True)

	expectRun(t, `
		return isError(error("test"))
	`, nil, True)

	expectRun(t, `
		return isUndefined(undefined)
	`, nil, True)

	expectRun(t, `
		return isChar('A')
	`, nil, True)
}

// TestVariousBuiltins tests more builtins to cover more zfuncs.go paths
func TestVariousBuiltins(t *testing.T) {
	// Type checking builtins
	expectRun(t, `
		return isInt("not int")
	`, nil, False)

	expectRun(t, `
		return isString(123)
	`, nil, False)

	expectRun(t, `
		return isBool(123)
	`, nil, False)

	expectRun(t, `
		return isArray("not array")
	`, nil, False)

	expectRun(t, `
		return isMap("not map")
	`, nil, False)

	expectRun(t, `
		return isFunction("not function")
	`, nil, False)

	expectRun(t, `
		return isError("not error")
	`, nil, False)

	expectRun(t, `
		return isUndefined("not undefined")
	`, nil, False)

	expectRun(t, `
		return isChar("not char")
	`, nil, False)

	// More type conversions
	expectRun(t, `
		return bool(0)
	`, nil, False)

	expectRun(t, `
		return bool("")
	`, nil, False)

	expectRun(t, `
		return bool("hello")
	`, nil, True)

	expectRun(t, `
		return int(false)
	`, nil, Int(0))

	expectRun(t, `
		return float(false)
	`, nil, Float(0))

	expectRun(t, `
		return string(true)
	`, nil, String("true"))

	// String/char conversions
	expectRun(t, `
		return char('B')
	`, nil, Char('B'))

	expectRun(t, `
		return byte(90)
	`, nil, Byte(90))
}
