// Code generated by 'go generate'; DO NOT EDIT.

package stdlib

import (
	"strconv"

	"github.com/topxeq/charlang"
)

// FuncPOROEx is a generated function to make charlang.CallableExFunc.
// Source: func(o charlang.Object) (ret charlang.Object)
func FuncPOROEx(fn func(charlang.Object) charlang.Object) charlang.CallableExFunc {
	return func(args charlang.Call) (ret charlang.Object, err error) {
		if err := args.CheckLen(1); err != nil {
			return charlang.Undefined, err
		}

		o := args.Get(0)

		ret = fn(o)
		return
	}
}

// FuncPiROEx is a generated function to make charlang.CallableExFunc.
// Source: func(i1 int) (ret charlang.Object)
func FuncPiROEx(fn func(int) charlang.Object) charlang.CallableExFunc {
	return func(args charlang.Call) (ret charlang.Object, err error) {
		if err := args.CheckLen(1); err != nil {
			return charlang.Undefined, err
		}

		i1, ok := charlang.ToGoInt(args.Get(0))
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("1st", "int", args.Get(0).TypeName())
		}

		ret = fn(i1)
		return
	}
}

// FuncPi64ROEx is a generated function to make charlang.CallableExFunc.
// Source: func(i1 int64) (ret charlang.Object)
func FuncPi64ROEx(fn func(int64) charlang.Object) charlang.CallableExFunc {
	return func(args charlang.Call) (ret charlang.Object, err error) {
		if err := args.CheckLen(1); err != nil {
			return charlang.Undefined, err
		}

		i1, ok := charlang.ToGoInt64(args.Get(0))
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("1st", "int", args.Get(0).TypeName())
		}

		ret = fn(i1)
		return
	}
}

// FuncPi64REx is a generated function to make charlang.CallableExFunc.
// Source: func(i1 int64)
func FuncPi64REx(fn func(int64)) charlang.CallableExFunc {
	return func(args charlang.Call) (ret charlang.Object, err error) {
		if err := args.CheckLen(1); err != nil {
			return charlang.Undefined, err
		}

		i1, ok := charlang.ToGoInt64(args.Get(0))
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("1st", "int", args.Get(0).TypeName())
		}

		fn(i1)
		ret = charlang.Undefined
		return
	}
}

// FuncPsROeEx is a generated function to make charlang.CallableExFunc.
// Source: func(s string) (ret charlang.Object, err error)
func FuncPsROeEx(fn func(string) (charlang.Object, error)) charlang.CallableExFunc {
	return func(args charlang.Call) (ret charlang.Object, err error) {
		if err := args.CheckLen(1); err != nil {
			return charlang.Undefined, err
		}

		s, ok := charlang.ToGoString(args.Get(0))
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("1st", "string", args.Get(0).TypeName())
		}

		ret, err = fn(s)
		return
	}
}

// FuncPsiROEx is a generated function to make charlang.CallableExFunc.
// Source: func(s string, i1 int) (ret charlang.Object)
func FuncPsiROEx(fn func(string, int) charlang.Object) charlang.CallableExFunc {
	return func(args charlang.Call) (ret charlang.Object, err error) {
		if err := args.CheckLen(2); err != nil {
			return charlang.Undefined, err
		}

		s, ok := charlang.ToGoString(args.Get(0))
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("1st", "string", args.Get(0).TypeName())
		}
		i1, ok := charlang.ToGoInt(args.Get(1))
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("2nd", "int", args.Get(1).TypeName())
		}

		ret = fn(s, i1)
		return
	}
}

// FuncPROEx is a generated function to make charlang.CallableExFunc.
// Source: func() (ret charlang.Object)
func FuncPROEx(fn func() charlang.Object) charlang.CallableExFunc {
	return func(args charlang.Call) (ret charlang.Object, err error) {
		if err := args.CheckLen(0); err != nil {
			return charlang.Undefined, err
		}

		ret = fn()
		return
	}
}

// FuncPi64i64ROEx is a generated function to make charlang.CallableExFunc.
// Source: func(i1 int64, i2 int64) (ret charlang.Object)
func FuncPi64i64ROEx(fn func(int64, int64) charlang.Object) charlang.CallableExFunc {
	return func(args charlang.Call) (ret charlang.Object, err error) {
		if err := args.CheckLen(2); err != nil {
			return charlang.Undefined, err
		}

		i1, ok := charlang.ToGoInt64(args.Get(0))
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("1st", "int", args.Get(0).TypeName())
		}
		i2, ok := charlang.ToGoInt64(args.Get(1))
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("2nd", "int", args.Get(1).TypeName())
		}

		ret = fn(i1, i2)
		return
	}
}

// FuncPb2ROEx is a generated function to make charlang.CallableExFunc.
// Source: func(b []byte) (ret charlang.Object)
func FuncPb2ROEx(fn func([]byte) charlang.Object) charlang.CallableExFunc {
	return func(args charlang.Call) (ret charlang.Object, err error) {
		if err := args.CheckLen(1); err != nil {
			return charlang.Undefined, err
		}

		b, ok := charlang.ToGoByteSlice(args.Get(0))
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("1st", "bytes", args.Get(0).TypeName())
		}

		ret = fn(b)
		return
	}
}

// FuncPOssROEx is a generated function to make charlang.CallableExFunc.
// Source: func(o charlang.Object, s1 string, s2 string) (ret charlang.Object)
func FuncPOssROEx(fn func(charlang.Object, string, string) charlang.Object) charlang.CallableExFunc {
	return func(args charlang.Call) (ret charlang.Object, err error) {
		if err := args.CheckLen(3); err != nil {
			return charlang.Undefined, err
		}

		o := args.Get(0)
		s1, ok := charlang.ToGoString(args.Get(1))
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("2nd", "string", args.Get(1).TypeName())
		}
		s2, ok := charlang.ToGoString(args.Get(2))
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("3rd", "string", args.Get(2).TypeName())
		}

		ret = fn(o, s1, s2)
		return
	}
}

// FuncPb2bROEx is a generated function to make charlang.CallableExFunc.
// Source: func(p []byte, b bool) (ret charlang.Object)
func FuncPb2bROEx(fn func([]byte, bool) charlang.Object) charlang.CallableExFunc {
	return func(args charlang.Call) (ret charlang.Object, err error) {
		if err := args.CheckLen(2); err != nil {
			return charlang.Undefined, err
		}

		p, ok := charlang.ToGoByteSlice(args.Get(0))
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("1st", "bytes", args.Get(0).TypeName())
		}
		b, ok := charlang.ToGoBool(args.Get(1))
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("2nd", "bool", args.Get(1).TypeName())
		}

		ret = fn(p, b)
		return
	}
}

// FuncPb2ssROEx is a generated function to make charlang.CallableExFunc.
// Source: func(p []byte, s1 string, s2 string) (ret charlang.Object)
func FuncPb2ssROEx(fn func([]byte, string, string) charlang.Object) charlang.CallableExFunc {
	return func(args charlang.Call) (ret charlang.Object, err error) {
		if err := args.CheckLen(3); err != nil {
			return charlang.Undefined, err
		}

		p, ok := charlang.ToGoByteSlice(args.Get(0))
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("1st", "bytes", args.Get(0).TypeName())
		}
		s1, ok := charlang.ToGoString(args.Get(1))
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("2nd", "string", args.Get(1).TypeName())
		}
		s2, ok := charlang.ToGoString(args.Get(2))
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("3rd", "string", args.Get(2).TypeName())
		}

		ret = fn(p, s1, s2)
		return
	}
}

// FuncPssROEx is a generated function to make charlang.CallableExFunc.
// Source: func(s1 string, s2 string) (ret charlang.Object)
func FuncPssROEx(fn func(string, string) charlang.Object) charlang.CallableExFunc {
	return func(args charlang.Call) (ret charlang.Object, err error) {
		if err := args.CheckLen(2); err != nil {
			return charlang.Undefined, err
		}

		s1, ok := charlang.ToGoString(args.Get(0))
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("1st", "string", args.Get(0).TypeName())
		}
		s2, ok := charlang.ToGoString(args.Get(1))
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("2nd", "string", args.Get(1).TypeName())
		}

		ret = fn(s1, s2)
		return
	}
}

// FuncPsROEx is a generated function to make charlang.CallableExFunc.
// Source: func(s string) (ret charlang.Object)
func FuncPsROEx(fn func(string) charlang.Object) charlang.CallableExFunc {
	return func(args charlang.Call) (ret charlang.Object, err error) {
		if err := args.CheckLen(1); err != nil {
			return charlang.Undefined, err
		}

		s, ok := charlang.ToGoString(args.Get(0))
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("1st", "string", args.Get(0).TypeName())
		}

		ret = fn(s)
		return
	}
}

// FuncPsrROEx is a generated function to make charlang.CallableExFunc.
// Source: func(s string, r rune) (ret charlang.Object)
func FuncPsrROEx(fn func(string, rune) charlang.Object) charlang.CallableExFunc {
	return func(args charlang.Call) (ret charlang.Object, err error) {
		if err := args.CheckLen(2); err != nil {
			return charlang.Undefined, err
		}

		s, ok := charlang.ToGoString(args.Get(0))
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("1st", "string", args.Get(0).TypeName())
		}
		r, ok := charlang.ToGoRune(args.Get(1))
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("2nd", "char", args.Get(1).TypeName())
		}

		ret = fn(s, r)
		return
	}
}

// FuncPAsROEx is a generated function to make charlang.CallableExFunc.
// Source: func(arr charlang.Array, s string) (ret charlang.Object)
func FuncPAsROEx(fn func(charlang.Array, string) charlang.Object) charlang.CallableExFunc {
	return func(args charlang.Call) (ret charlang.Object, err error) {
		if err := args.CheckLen(2); err != nil {
			return charlang.Undefined, err
		}

		arr, ok := charlang.ToArray(args.Get(0))
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("1st", "array", args.Get(0).TypeName())
		}
		s, ok := charlang.ToGoString(args.Get(1))
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("2nd", "string", args.Get(1).TypeName())
		}

		ret = fn(arr, s)
		return
	}
}

// FuncPOi64ROeEx is a generated function to make charlang.CallableExFunc.
// Source: func(o charlang.Object, i int64) (ret charlang.Object, err error)
func FuncPOi64ROeEx(fn func(charlang.Object, int64) (charlang.Object, error)) charlang.CallableExFunc {
	return func(args charlang.Call) (ret charlang.Object, err error) {
		if err := args.CheckLen(2); err != nil {
			return charlang.Undefined, err
		}

		o := args.Get(0)
		i, ok := charlang.ToGoInt64(args.Get(1))
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("2nd", "int", args.Get(1).TypeName())
		}

		ret, err = fn(o, i)
		return
	}
}

// FuncPORO is a generated function to make charlang.CallableFunc.
// Source: func(o charlang.Object) (ret charlang.Object)
func FuncPORO(fn func(charlang.Object) charlang.Object) charlang.CallableFunc {
	return func(args ...charlang.Object) (ret charlang.Object, err error) {
		if len(args) != 1 {
			return charlang.Undefined, charlang.ErrWrongNumArguments.NewError("want=1 got=" + strconv.Itoa(len(args)))
		}

		o := args[0]

		ret = fn(o)
		return
	}
}

// FuncPiRO is a generated function to make charlang.CallableFunc.
// Source: func(i1 int) (ret charlang.Object)
func FuncPiRO(fn func(int) charlang.Object) charlang.CallableFunc {
	return func(args ...charlang.Object) (ret charlang.Object, err error) {
		if len(args) != 1 {
			return charlang.Undefined, charlang.ErrWrongNumArguments.NewError("want=1 got=" + strconv.Itoa(len(args)))
		}

		i1, ok := charlang.ToGoInt(args[0])
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("1st", "int", args[0].TypeName())
		}

		ret = fn(i1)
		return
	}
}

// FuncPi64RO is a generated function to make charlang.CallableFunc.
// Source: func(i1 int64) (ret charlang.Object)
func FuncPi64RO(fn func(int64) charlang.Object) charlang.CallableFunc {
	return func(args ...charlang.Object) (ret charlang.Object, err error) {
		if len(args) != 1 {
			return charlang.Undefined, charlang.ErrWrongNumArguments.NewError("want=1 got=" + strconv.Itoa(len(args)))
		}

		i1, ok := charlang.ToGoInt64(args[0])
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("1st", "int", args[0].TypeName())
		}

		ret = fn(i1)
		return
	}
}

// FuncPi64R is a generated function to make charlang.CallableFunc.
// Source: func(i1 int64)
func FuncPi64R(fn func(int64)) charlang.CallableFunc {
	return func(args ...charlang.Object) (ret charlang.Object, err error) {
		if len(args) != 1 {
			return charlang.Undefined, charlang.ErrWrongNumArguments.NewError("want=1 got=" + strconv.Itoa(len(args)))
		}

		i1, ok := charlang.ToGoInt64(args[0])
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("1st", "int", args[0].TypeName())
		}

		fn(i1)
		ret = charlang.Undefined
		return
	}
}

// FuncPsROe is a generated function to make charlang.CallableFunc.
// Source: func(s string) (ret charlang.Object, err error)
func FuncPsROe(fn func(string) (charlang.Object, error)) charlang.CallableFunc {
	return func(args ...charlang.Object) (ret charlang.Object, err error) {
		if len(args) != 1 {
			return charlang.Undefined, charlang.ErrWrongNumArguments.NewError("want=1 got=" + strconv.Itoa(len(args)))
		}

		s, ok := charlang.ToGoString(args[0])
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("1st", "string", args[0].TypeName())
		}

		ret, err = fn(s)
		return
	}
}

// FuncPsiRO is a generated function to make charlang.CallableFunc.
// Source: func(s string, i1 int) (ret charlang.Object)
func FuncPsiRO(fn func(string, int) charlang.Object) charlang.CallableFunc {
	return func(args ...charlang.Object) (ret charlang.Object, err error) {
		if len(args) != 2 {
			return charlang.Undefined, charlang.ErrWrongNumArguments.NewError("want=2 got=" + strconv.Itoa(len(args)))
		}

		s, ok := charlang.ToGoString(args[0])
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("1st", "string", args[0].TypeName())
		}
		i1, ok := charlang.ToGoInt(args[1])
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("2nd", "int", args[1].TypeName())
		}

		ret = fn(s, i1)
		return
	}
}

// FuncPRO is a generated function to make charlang.CallableFunc.
// Source: func() (ret charlang.Object)
func FuncPRO(fn func() charlang.Object) charlang.CallableFunc {
	return func(args ...charlang.Object) (ret charlang.Object, err error) {
		if len(args) != 0 {
			return charlang.Undefined, charlang.ErrWrongNumArguments.NewError("want=0 got=" + strconv.Itoa(len(args)))
		}

		ret = fn()
		return
	}
}

// FuncPi64i64RO is a generated function to make charlang.CallableFunc.
// Source: func(i1 int64, i2 int64) (ret charlang.Object)
func FuncPi64i64RO(fn func(int64, int64) charlang.Object) charlang.CallableFunc {
	return func(args ...charlang.Object) (ret charlang.Object, err error) {
		if len(args) != 2 {
			return charlang.Undefined, charlang.ErrWrongNumArguments.NewError("want=2 got=" + strconv.Itoa(len(args)))
		}

		i1, ok := charlang.ToGoInt64(args[0])
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("1st", "int", args[0].TypeName())
		}
		i2, ok := charlang.ToGoInt64(args[1])
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("2nd", "int", args[1].TypeName())
		}

		ret = fn(i1, i2)
		return
	}
}

// FuncPb2RO is a generated function to make charlang.CallableFunc.
// Source: func(b []byte) (ret charlang.Object)
func FuncPb2RO(fn func([]byte) charlang.Object) charlang.CallableFunc {
	return func(args ...charlang.Object) (ret charlang.Object, err error) {
		if len(args) != 1 {
			return charlang.Undefined, charlang.ErrWrongNumArguments.NewError("want=1 got=" + strconv.Itoa(len(args)))
		}

		b, ok := charlang.ToGoByteSlice(args[0])
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("1st", "bytes", args[0].TypeName())
		}

		ret = fn(b)
		return
	}
}

// FuncPOssRO is a generated function to make charlang.CallableFunc.
// Source: func(o charlang.Object, s1 string, s2 string) (ret charlang.Object)
func FuncPOssRO(fn func(charlang.Object, string, string) charlang.Object) charlang.CallableFunc {
	return func(args ...charlang.Object) (ret charlang.Object, err error) {
		if len(args) != 3 {
			return charlang.Undefined, charlang.ErrWrongNumArguments.NewError("want=3 got=" + strconv.Itoa(len(args)))
		}

		o := args[0]
		s1, ok := charlang.ToGoString(args[1])
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("2nd", "string", args[1].TypeName())
		}
		s2, ok := charlang.ToGoString(args[2])
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("3rd", "string", args[2].TypeName())
		}

		ret = fn(o, s1, s2)
		return
	}
}

// FuncPb2bRO is a generated function to make charlang.CallableFunc.
// Source: func(p []byte, b bool) (ret charlang.Object)
func FuncPb2bRO(fn func([]byte, bool) charlang.Object) charlang.CallableFunc {
	return func(args ...charlang.Object) (ret charlang.Object, err error) {
		if len(args) != 2 {
			return charlang.Undefined, charlang.ErrWrongNumArguments.NewError("want=2 got=" + strconv.Itoa(len(args)))
		}

		p, ok := charlang.ToGoByteSlice(args[0])
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("1st", "bytes", args[0].TypeName())
		}
		b, ok := charlang.ToGoBool(args[1])
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("2nd", "bool", args[1].TypeName())
		}

		ret = fn(p, b)
		return
	}
}

// FuncPb2ssRO is a generated function to make charlang.CallableFunc.
// Source: func(p []byte, s1 string, s2 string) (ret charlang.Object)
func FuncPb2ssRO(fn func([]byte, string, string) charlang.Object) charlang.CallableFunc {
	return func(args ...charlang.Object) (ret charlang.Object, err error) {
		if len(args) != 3 {
			return charlang.Undefined, charlang.ErrWrongNumArguments.NewError("want=3 got=" + strconv.Itoa(len(args)))
		}

		p, ok := charlang.ToGoByteSlice(args[0])
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("1st", "bytes", args[0].TypeName())
		}
		s1, ok := charlang.ToGoString(args[1])
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("2nd", "string", args[1].TypeName())
		}
		s2, ok := charlang.ToGoString(args[2])
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("3rd", "string", args[2].TypeName())
		}

		ret = fn(p, s1, s2)
		return
	}
}

// FuncPssRO is a generated function to make charlang.CallableFunc.
// Source: func(s1 string, s2 string) (ret charlang.Object)
func FuncPssRO(fn func(string, string) charlang.Object) charlang.CallableFunc {
	return func(args ...charlang.Object) (ret charlang.Object, err error) {
		if len(args) != 2 {
			return charlang.Undefined, charlang.ErrWrongNumArguments.NewError("want=2 got=" + strconv.Itoa(len(args)))
		}

		s1, ok := charlang.ToGoString(args[0])
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("1st", "string", args[0].TypeName())
		}
		s2, ok := charlang.ToGoString(args[1])
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("2nd", "string", args[1].TypeName())
		}

		ret = fn(s1, s2)
		return
	}
}

// FuncPsRO is a generated function to make charlang.CallableFunc.
// Source: func(s string) (ret charlang.Object)
func FuncPsRO(fn func(string) charlang.Object) charlang.CallableFunc {
	return func(args ...charlang.Object) (ret charlang.Object, err error) {
		if len(args) != 1 {
			return charlang.Undefined, charlang.ErrWrongNumArguments.NewError("want=1 got=" + strconv.Itoa(len(args)))
		}

		s, ok := charlang.ToGoString(args[0])
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("1st", "string", args[0].TypeName())
		}

		ret = fn(s)
		return
	}
}

// FuncPsrRO is a generated function to make charlang.CallableFunc.
// Source: func(s string, r rune) (ret charlang.Object)
func FuncPsrRO(fn func(string, rune) charlang.Object) charlang.CallableFunc {
	return func(args ...charlang.Object) (ret charlang.Object, err error) {
		if len(args) != 2 {
			return charlang.Undefined, charlang.ErrWrongNumArguments.NewError("want=2 got=" + strconv.Itoa(len(args)))
		}

		s, ok := charlang.ToGoString(args[0])
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("1st", "string", args[0].TypeName())
		}
		r, ok := charlang.ToGoRune(args[1])
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("2nd", "char", args[1].TypeName())
		}

		ret = fn(s, r)
		return
	}
}

// FuncPAsRO is a generated function to make charlang.CallableFunc.
// Source: func(arr charlang.Array, s string) (ret charlang.Object)
func FuncPAsRO(fn func(charlang.Array, string) charlang.Object) charlang.CallableFunc {
	return func(args ...charlang.Object) (ret charlang.Object, err error) {
		if len(args) != 2 {
			return charlang.Undefined, charlang.ErrWrongNumArguments.NewError("want=2 got=" + strconv.Itoa(len(args)))
		}

		arr, ok := charlang.ToArray(args[0])
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("1st", "array", args[0].TypeName())
		}
		s, ok := charlang.ToGoString(args[1])
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("2nd", "string", args[1].TypeName())
		}

		ret = fn(arr, s)
		return
	}
}

// FuncPOi64ROe is a generated function to make charlang.CallableFunc.
// Source: func(o charlang.Object, i int64) (ret charlang.Object, err error)
func FuncPOi64ROe(fn func(charlang.Object, int64) (charlang.Object, error)) charlang.CallableFunc {
	return func(args ...charlang.Object) (ret charlang.Object, err error) {
		if len(args) != 2 {
			return charlang.Undefined, charlang.ErrWrongNumArguments.NewError("want=2 got=" + strconv.Itoa(len(args)))
		}

		o := args[0]
		i, ok := charlang.ToGoInt64(args[1])
		if !ok {
			return charlang.Undefined, charlang.NewArgumentTypeError("2nd", "int", args[1].TypeName())
		}

		ret, err = fn(o, i)
		return
	}
}
