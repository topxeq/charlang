// Copyright (c) 2020-2023 Ozan Hacıbekiroğlu.
// Use of this source code is governed by a MIT License
// that can be found in the LICENSE file.

package fmt

import (
	"fmt"
	"strconv"

	"github.com/topxeq/charlang"
)

// Module represents fmt module.
var Module = map[string]charlang.Object{
	// char:doc
	// # fmt Module
	//
	// ## Scan Examples
	//
	// ```go
	// arg1 := fmt.ScanArg("string")
	// arg2 := fmt.ScanArg("int")
	// ret := fmt.Sscanf("abc123", "%3s%d", arg1, arg2)
	// if isError(ret) {
	//   // handle error
	//   fmt.Println(err)
	// } else {
	//   fmt.Println(ret)            // 2, number of scanned items
	//   fmt.Println(arg1.Value)     // abc
	//   fmt.Println(bool(arg1))     // true, reports whether arg1 is scanned
	//   fmt.Println(arg2.Value)     // 123
	//   fmt.Println(bool(arg2))     // true, reports whether arg2 is scanned
	// }
	// ```
	//
	// ```go
	// arg1 = fmt.ScanArg("string")
	// arg2 = fmt.ScanArg("int")
	// arg3 = fmt.ScanArg("float")
	// ret = fmt.Sscanf("abc 123", "%s%d%f", arg1, arg2, arg3)
	// fmt.Println(ret)         // error: EOF
	// fmt.Println(arg1.Value)  // abc
	// fmt.Println(bool(arg1))  // true
	// fmt.Println(arg2.Value)  // 123
	// fmt.Println(bool(arg2))  // true
	// fmt.Println(arg3.Value)  // undefined
	// fmt.Println(bool(arg2))  // false, not scanned
	//
	// // Use if statement or a ternary expression to get the scanned value or a default value.
	// v := arg1 ? arg1.Value : "default value"
	// ```

	// char:doc
	// ## Functions
	// Print(...any) -> int
	// Formats using the default formats for its operands and writes to standard
	// output. Spaces are added between operands when neither is a string.
	// It returns the number of bytes written and any encountered write error
	// throws a runtime error.
	"Print": &charlang.Function{
		Name: "Print",
		Value: func(args ...charlang.Object) (charlang.Object, error) {
			return newPrint(fmt.Print)(charlang.NewCall(nil, args))
		},
		ValueEx: newPrint(fmt.Print),
	},
	// char:doc
	// Printf(format string, ...any) -> int
	// Formats according to a format specifier and writes to standard output.
	// It returns the number of bytes written and any encountered write error
	// throws a runtime error.
	"Printf": &charlang.Function{
		Name: "Printf",
		Value: func(args ...charlang.Object) (charlang.Object, error) {
			return newPrintf(fmt.Printf)(charlang.NewCall(nil, args))
		},
		ValueEx: newPrintf(fmt.Printf),
	},
	// char:doc
	// Println(...any) -> int
	// Formats using the default formats for its operands and writes to standard
	// output. Spaces are always added between operands and a newline
	// is appended. It returns the number of bytes written and any encountered
	// write error throws a runtime error.
	"Println": &charlang.Function{
		Name: "Println",
		Value: func(args ...charlang.Object) (charlang.Object, error) {
			return newPrint(fmt.Println)(charlang.NewCall(nil, args))
		},
		ValueEx: newPrint(fmt.Println),
	},
	// char:doc
	// Sprint(...any) -> string
	// Formats using the default formats for its operands and returns the
	// resulting string. Spaces are added between operands when neither is a
	// string.
	"Sprint": &charlang.Function{
		Name: "Sprint",
		Value: func(args ...charlang.Object) (charlang.Object, error) {
			return newSprint(fmt.Sprint)(charlang.NewCall(nil, args))
		},
		ValueEx: newSprint(fmt.Sprint),
	},
	// char:doc
	// Sprintf(format string, ...any) -> string
	// Formats according to a format specifier and returns the resulting string.
	"Sprintf": &charlang.Function{
		Name: "Sprintf",
		Value: func(args ...charlang.Object) (charlang.Object, error) {
			return newSprintf(fmt.Sprintf)(charlang.NewCall(nil, args))
		},
		ValueEx: newSprintf(fmt.Sprintf),
	},
	// char:doc
	// Sprintln(...any) -> string
	// Formats using the default formats for its operands and returns the
	// resulting string. Spaces are always added between operands and a newline
	// is appended.
	"Sprintln": &charlang.Function{
		Name: "Sprintln",
		Value: func(args ...charlang.Object) (charlang.Object, error) {
			return newSprint(fmt.Sprintln)(charlang.NewCall(nil, args))
		},
		ValueEx: newSprint(fmt.Sprintln),
	},
	// char:doc
	// Sscan(str string, ScanArg[, ...ScanArg]) -> int | error
	// Scans the argument string, storing successive space-separated values into
	// successive ScanArg arguments. Newlines count as space. If no error is
	// encountered, it returns the number of items successfully scanned. If that
	// is less than the number of arguments, error will report why.
	"Sscan": &charlang.Function{
		Name: "Sscan",
		Value: func(args ...charlang.Object) (charlang.Object, error) {
			return newSscan(fmt.Sscan)(charlang.NewCall(nil, args))
		},
		ValueEx: newSscan(fmt.Sscan),
	},
	// char:doc
	// Sscanf(str string, format string, ScanArg[, ...ScanArg]) -> int | error
	// Scans the argument string, storing successive space-separated values into
	// successive ScanArg arguments as determined by the format. It returns the
	// number of items successfully parsed or an error.
	// Newlines in the input must match newlines in the format.
	"Sscanf": &charlang.Function{
		Name: "Sscanf",
		Value: func(args ...charlang.Object) (charlang.Object, error) {
			return newSscanf(fmt.Sscanf)(charlang.NewCall(nil, args))
		},
		ValueEx: newSscanf(fmt.Sscanf),
	},
	// Sscanln(str string, ScanArg[, ...ScanArg]) -> int | error
	// Sscanln is similar to Sscan, but stops scanning at a newline and after
	// the final item there must be a newline or EOF. It returns the number of
	// items successfully parsed or an error.
	"Sscanln": &charlang.Function{
		Name: "Sscanln",
		Value: func(args ...charlang.Object) (charlang.Object, error) {
			return newSscan(fmt.Sscanln)(charlang.NewCall(nil, args))
		},
		ValueEx: newSscan(fmt.Sscanln),
	},
	// char:doc
	// ScanArg(typeName string) -> scanArg
	// Returns a `scanArg` object to scan a value of given type name in scan
	// functions.
	// Supported type names are `"string", "int", "uint", "float", "char",
	// "bool", "bytes"`.
	// It throws a runtime error if type name is not supported.
	// Alternatively, `string, int, uint, float, char, bool, bytes` builtin
	// functions can be provided to get the type name from the BuiltinFunction's
	// Name field.
	"ScanArg": &charlang.Function{
		Name: "ScanArg",
		Value: func(args ...charlang.Object) (charlang.Object, error) {
			return newScanArgFunc(charlang.NewCall(nil, args))
		},
		ValueEx: newScanArgFunc,
	},
}

func newPrint(fn func(...interface{}) (int, error)) charlang.CallableExFunc {
	return func(c charlang.Call) (ret charlang.Object, err error) {
		vargs := toPrintArgs(0, c)
		n, err := fn(vargs...)
		return charlang.Int(n), err
	}
}

func newPrintf(fn func(string, ...interface{}) (int, error)) charlang.CallableExFunc {
	return func(c charlang.Call) (ret charlang.Object, err error) {
		if c.Len() < 1 {
			return charlang.Undefined, charlang.ErrWrongNumArguments.NewError(
				"want>=1 got=" + strconv.Itoa(c.Len()))
		}
		vargs := toPrintArgs(1, c)
		n, err := fn(c.Get(0).String(), vargs...)
		return charlang.Int(n), err
	}
}

func newSprint(fn func(...interface{}) string) charlang.CallableExFunc {
	return func(c charlang.Call) (ret charlang.Object, err error) {
		vargs := toPrintArgs(0, c)
		return charlang.String(fn(vargs...)), nil
	}
}

func newSprintf(fn func(string, ...interface{}) string) charlang.CallableExFunc {
	return func(c charlang.Call) (ret charlang.Object, err error) {
		if c.Len() < 1 {
			return charlang.Undefined, charlang.ErrWrongNumArguments.NewError(
				"want>=1 got=" + strconv.Itoa(c.Len()))
		}
		vargs := toPrintArgs(1, c)
		return charlang.String(fn(c.Get(0).String(), vargs...)), nil
	}
}

func newSscan(fn func(string, ...interface{}) (int, error)) charlang.CallableExFunc {
	return func(c charlang.Call) (ret charlang.Object, err error) {
		if c.Len() < 2 {
			return charlang.Undefined, charlang.ErrWrongNumArguments.NewError(
				"want>=2 got=" + strconv.Itoa(c.Len()))
		}
		vargs, err := toScanArgs(1, c)
		if err != nil {
			return charlang.Undefined, err
		}
		n, err := fn(c.Get(0).String(), vargs...)
		return postScan(1, n, err, c), nil
	}
}

func newSscanf(
	fn func(string, string, ...interface{}) (int, error),
) charlang.CallableExFunc {
	return func(c charlang.Call) (ret charlang.Object, err error) {
		if c.Len() < 3 {
			return charlang.Undefined, charlang.ErrWrongNumArguments.NewError(
				"want>=3 got=" + strconv.Itoa(c.Len()))
		}
		vargs, err := toScanArgs(2, c)
		if err != nil {
			return charlang.Undefined, err
		}
		n, err := fn(c.Get(0).String(), c.Get(1).String(), vargs...)
		return postScan(2, n, err, c), nil
	}
}

func toScanArgs(offset int, c charlang.Call) ([]interface{}, error) {
	size := c.Len()
	vargs := make([]interface{}, 0, size-offset)
	for i := offset; i < size; i++ {
		v, ok := c.Get(i).(ScanArg)
		if !ok {
			return nil, charlang.NewArgumentTypeError(strconv.Itoa(i),
				"ScanArg interface", c.Get(i).TypeName())
		}
		v.Set(false)
		vargs = append(vargs, v.Arg())
	}
	return vargs, nil
}

func toPrintArgs(offset int, c charlang.Call) []interface{} {
	size := c.Len()
	vargs := make([]interface{}, 0, size-offset)
	for i := offset; i < size; i++ {
		vargs = append(vargs, c.Get(i))
	}
	return vargs
}

// args are always of ScanArg interface type.
func postScan(offset, n int, err error, c charlang.Call) charlang.Object {
	for i := offset; i < n+offset; i++ {
		c.Get(i).(ScanArg).Set(true)
	}
	if err != nil {
		return &charlang.Error{
			Message: err.Error(),
			Cause:   err,
		}
	}
	return charlang.Int(n)
}
