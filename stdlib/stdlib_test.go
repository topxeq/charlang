package stdlib_test

import (
	"testing"

	"github.com/stretchr/testify/require"

	. "github.com/topxeq/charlang"
	. "github.com/topxeq/charlang/stdlib"
)

func TestFuncPOROEx(t *testing.T) {
	// Test with a function that returns the input object
	fn := FuncPOROEx(func(o Object) Object {
		return o
	})

	// Test with valid argument
	ret, err := fn(NewCall(nil, []Object{Int(42)}))
	require.NoError(t, err)
	require.Equal(t, Int(42), ret)

	// Test with string
	ret, err = fn(NewCall(nil, []Object{String("hello")}))
	require.NoError(t, err)
	require.Equal(t, String("hello"), ret)

	// Test with no arguments - should error
	_, err = fn(NewCall(nil, []Object{}))
	require.Error(t, err)
}

func TestFuncPiROEx(t *testing.T) {
	fn := FuncPiROEx(func(i int) Object {
		return Int(i * 2)
	})

	// Test with valid int
	ret, err := fn(NewCall(nil, []Object{Int(5)}))
	require.NoError(t, err)
	require.Equal(t, Int(10), ret)

	// Test with float (converts to int)
	ret, err = fn(NewCall(nil, []Object{Float(3.7)}))
	require.NoError(t, err)
	require.Equal(t, Int(6), ret) // 3.7 truncates to 3, then 3*2=6

	// Test with string number
	ret, err = fn(NewCall(nil, []Object{String("7")}))
	require.NoError(t, err)
	require.Equal(t, Int(14), ret)

	// Test with no arguments
	_, err = fn(NewCall(nil, []Object{}))
	require.Error(t, err)
}

func TestFuncPi64ROEx(t *testing.T) {
	fn := FuncPi64ROEx(func(i int64) Object {
		return Int(i + 1)
	})

	// Test with valid int64
	ret, err := fn(NewCall(nil, []Object{Int(100)}))
	require.NoError(t, err)
	require.Equal(t, Int(101), ret)

	// Test with uint
	ret, err = fn(NewCall(nil, []Object{Uint(50)}))
	require.NoError(t, err)
	require.Equal(t, Int(51), ret)
}

func TestFuncPi64REx(t *testing.T) {
	var called bool
	var receivedValue int64

	fn := FuncPi64REx(func(i int64) {
		called = true
		receivedValue = i
	})

	// Test with valid int64
	called = false
	receivedValue = 0
	ret, err := fn(NewCall(nil, []Object{Int(42)}))
	require.NoError(t, err)
	require.Equal(t, Undefined, ret)
	require.True(t, called)
	require.Equal(t, int64(42), receivedValue)
}

func TestFuncPsROeEx(t *testing.T) {
	fn := FuncPsROeEx(func(s string) (Object, error) {
		return String("processed: " + s), nil
	})

	// Test with valid string
	ret, err := fn(NewCall(nil, []Object{String("test")}))
	require.NoError(t, err)
	require.Equal(t, String("processed: test"), ret)

	// Test with int (converts to string via String() method)
	ret, err = fn(NewCall(nil, []Object{Int(123)}))
	require.NoError(t, err)
	require.Equal(t, String("processed: 123"), ret)
}

func TestFuncPsiROEx(t *testing.T) {
	fn := FuncPsiROEx(func(s string, i int) Object {
		if i <= len(s) {
			return String(s[:i])
		}
		return String(s)
	})

	// Test with valid arguments
	ret, err := fn(NewCall(nil, []Object{String("hello"), Int(2)}))
	require.NoError(t, err)
	require.Equal(t, String("he"), ret)

	// Test with string and int
	ret, err = fn(NewCall(nil, []Object{String("test"), Int(3)}))
	require.NoError(t, err)
	require.Equal(t, String("tes"), ret)
}

func TestFuncPROEx(t *testing.T) {
	fn := FuncPROEx(func() Object {
		return Int(42)
	})

	// Test with no arguments
	ret, err := fn(NewCall(nil, []Object{}))
	require.NoError(t, err)
	require.Equal(t, Int(42), ret)

	// Test with extra arguments - should error
	_, err = fn(NewCall(nil, []Object{Int(1), Int(2)}))
	require.Error(t, err)
}

func TestFuncPi64i64ROEx(t *testing.T) {
	fn := FuncPi64i64ROEx(func(i1, i2 int64) Object {
		return Int(i1 + i2)
	})

	// Test with valid arguments
	ret, err := fn(NewCall(nil, []Object{Int(10), Int(20)}))
	require.NoError(t, err)
	require.Equal(t, Int(30), ret)

	// Test with uints
	ret, err = fn(NewCall(nil, []Object{Uint(5), Uint(3)}))
	require.NoError(t, err)
	require.Equal(t, Int(8), ret)
}

func TestFuncPb2ROEx(t *testing.T) {
	fn := FuncPb2ROEx(func(b []byte) Object {
		return Int(len(b))
	})

	// Test with bytes
	ret, err := fn(NewCall(nil, []Object{Bytes("hello")}))
	require.NoError(t, err)
	require.Equal(t, Int(5), ret)

	// Test with string (converts to bytes)
	ret, err = fn(NewCall(nil, []Object{String("test")}))
	require.NoError(t, err)
	require.Equal(t, Int(4), ret)
}

func TestFuncPOssROEx(t *testing.T) {
	fn := FuncPOssROEx(func(o Object, s1, s2 string) Object {
		return String(s1 + s2)
	})

	// Test with valid arguments
	ret, err := fn(NewCall(nil, []Object{Int(1), String("a"), String("b")}))
	require.NoError(t, err)
	require.Equal(t, String("ab"), ret)
}

func TestFuncPb2bROEx(t *testing.T) {
	fn := FuncPb2bROEx(func(p []byte, b bool) Object {
		if b {
			return Int(len(p))
		}
		return Int(-1)
	})

	// Test with bytes and true
	ret, err := fn(NewCall(nil, []Object{Bytes("test"), True}))
	require.NoError(t, err)
	require.Equal(t, Int(4), ret)

	// Test with bytes and false
	ret, err = fn(NewCall(nil, []Object{Bytes("test"), False}))
	require.NoError(t, err)
	require.Equal(t, Int(-1), ret)
}

func TestFuncPb2ssROEx(t *testing.T) {
	fn := FuncPb2ssROEx(func(p []byte, s1, s2 string) Object {
		return String(string(p) + s1 + s2)
	})

	// Test with valid arguments
	ret, err := fn(NewCall(nil, []Object{Bytes("a"), String("b"), String("c")}))
	require.NoError(t, err)
	require.Equal(t, String("abc"), ret)
}

func TestFuncPssROEx(t *testing.T) {
	fn := FuncPssROEx(func(s1, s2 string) Object {
		return String(s1 + s2)
	})

	// Test with valid strings
	ret, err := fn(NewCall(nil, []Object{String("hello"), String("world")}))
	require.NoError(t, err)
	require.Equal(t, String("helloworld"), ret)

	// Test with ints (converts to strings)
	ret, err = fn(NewCall(nil, []Object{Int(1), Int(2)}))
	require.NoError(t, err)
	require.Equal(t, String("12"), ret)
}

func TestFuncPsROEx(t *testing.T) {
	fn := FuncPsROEx(func(s string) Object {
		return String("processed: " + s)
	})

	// Test with valid string
	ret, err := fn(NewCall(nil, []Object{String("test")}))
	require.NoError(t, err)
	require.Equal(t, String("processed: test"), ret)

	// Test with int (converts to string)
	ret, err = fn(NewCall(nil, []Object{Int(42)}))
	require.NoError(t, err)
	require.Equal(t, String("processed: 42"), ret)
}

func TestFuncPsrROEx(t *testing.T) {
	fn := FuncPsrROEx(func(s string, r rune) Object {
		return Int(len(s) + int(r))
	})

	// Test with valid arguments
	ret, err := fn(NewCall(nil, []Object{String("hello"), Char('A')}))
	require.NoError(t, err)
	require.Equal(t, Int(5+65), ret) // "hello" has 5 chars, 'A' is 65
}

func TestFuncPAsROEx(t *testing.T) {
	fn := FuncPAsROEx(func(arr Array, s string) Object {
		return Int(len(arr))
	})

	// Test with array and string
	ret, err := fn(NewCall(nil, []Object{Array{Int(1), Int(2)}, String("ignored")}))
	require.NoError(t, err)
	require.Equal(t, Int(2), ret)
}

func TestFuncPOi64ROeEx(t *testing.T) {
	fn := FuncPOi64ROeEx(func(o Object, i int64) (Object, error) {
		return Int(i * 2), nil
	})

	// Test with valid arguments
	ret, err := fn(NewCall(nil, []Object{Int(1), Int(5)}))
	require.NoError(t, err)
	require.Equal(t, Int(10), ret)
}

// Tests for non-Ex versions (CallableFunc versions)
func TestFuncPORO(t *testing.T) {
	fn := FuncPORO(func(o Object) Object {
		return o
	})

	// Test with valid argument
	ret, err := fn(Int(42))
	require.NoError(t, err)
	require.Equal(t, Int(42), ret)

	// Test with string
	ret, err = fn(String("hello"))
	require.NoError(t, err)
	require.Equal(t, String("hello"), ret)
}

func TestFuncPiRO(t *testing.T) {
	fn := FuncPiRO(func(i int) Object {
		return Int(i * 2)
	})

	// Test with valid int
	ret, err := fn(Int(5))
	require.NoError(t, err)
	require.Equal(t, Int(10), ret)
}

func TestFuncPi64RO(t *testing.T) {
	fn := FuncPi64RO(func(i int64) Object {
		return Int(i + 1)
	})

	// Test with valid int64
	ret, err := fn(Int(100))
	require.NoError(t, err)
	require.Equal(t, Int(101), ret)
}

func TestFuncPi64R(t *testing.T) {
	var called bool
	var receivedValue int64

	fn := FuncPi64R(func(i int64) {
		called = true
		receivedValue = i
	})

	// Test with valid int64
	called = false
	receivedValue = 0
	ret, err := fn(Int(42))
	require.NoError(t, err)
	require.Equal(t, Undefined, ret)
	require.True(t, called)
	require.Equal(t, int64(42), receivedValue)
}

func TestFuncPRO(t *testing.T) {
	fn := FuncPRO(func() Object {
		return Int(42)
	})

	// Test with no arguments
	ret, err := fn()
	require.NoError(t, err)
	require.Equal(t, Int(42), ret)
}

func TestFuncPssRO(t *testing.T) {
	fn := FuncPssRO(func(s1, s2 string) Object {
		return String(s1 + s2)
	})

	// Test with valid strings
	ret, err := fn(String("hello"), String("world"))
	require.NoError(t, err)
	require.Equal(t, String("helloworld"), ret)
}

func TestFuncPsRO(t *testing.T) {
	fn := FuncPsRO(func(s string) Object {
		return String("processed: " + s)
	})

	// Test with valid string
	ret, err := fn(String("test"))
	require.NoError(t, err)
	require.Equal(t, String("processed: test"), ret)
}

func TestFuncPAsRO(t *testing.T) {
	fn := FuncPAsRO(func(arr Array, s string) Object {
		return Int(len(arr))
	})

	// Test with array and string
	ret, err := fn(Array{Int(1), Int(2)}, String("ignored"))
	require.NoError(t, err)
	require.Equal(t, Int(2), ret)
}