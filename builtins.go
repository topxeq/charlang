package charlang

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"os"
	"sort"
	"strconv"
	"strings"
	"time"
	"unicode/utf8"

	"github.com/topxeq/charlang/token"
	"github.com/topxeq/tk"
)

var (
	// PrintWriter is the default writer for printf and println builtins.
	PrintWriter io.Writer = os.Stdout
)

// BuiltinType represents a builtin type
type BuiltinType byte

// Builtins
const (
	BuiltinAppend BuiltinType = iota

	// char add start
	BuiltinToTime
	BuiltinNewAny
	BuiltinTime
	BuiltinSleep
	BuiltinExit
	BuiltinSystemCmd
	BuiltinIsErrX
	BuiltinToJSON
	BuiltinFromJSON
	BuiltinPlo
	BuiltinGetParam
	BuiltinGetSwitch
	BuiltinIfSwitchExists
	BuiltinTypeCode
	// BuiltinTypeName
	BuiltinPl
	BuiltinPln
	BuiltinTestByText
	BuiltinTestByStartsWith
	BuiltinTestByReg
	BuiltinGetSeq
	BuiltinPass

	// char add end

	BuiltinDelete
	BuiltinCopy
	BuiltinRepeat
	BuiltinContains
	BuiltinLen
	BuiltinSort
	BuiltinSortReverse
	BuiltinError
	BuiltinTypeName
	BuiltinBool
	BuiltinInt
	BuiltinUint
	BuiltinFloat
	BuiltinChar
	BuiltinString
	BuiltinBytes
	BuiltinChars
	BuiltinPrintf
	BuiltinPrintln
	BuiltinSprintf
	BuiltinGlobals

	BuiltinIsError
	BuiltinIsInt
	BuiltinIsUint
	BuiltinIsFloat
	BuiltinIsChar
	BuiltinIsBool
	BuiltinIsString
	BuiltinIsBytes
	BuiltinIsMap
	BuiltinIsSyncMap
	BuiltinIsArray
	BuiltinIsUndefined
	BuiltinIsFunction
	BuiltinIsCallable
	BuiltinIsIterable

	BuiltinWrongNumArgumentsError
	BuiltinInvalidOperatorError
	BuiltinIndexOutOfBoundsError
	BuiltinNotIterableError
	BuiltinNotIndexableError
	BuiltinNotIndexAssignableError
	BuiltinNotCallableError
	BuiltinNotImplementedError
	BuiltinZeroDivisionError
	BuiltinTypeError

	BuiltinMakeArray
	BuiltinCap
)

// BuiltinsMap is list of builtin types, exported for REPL.
var BuiltinsMap = map[string]BuiltinType{
	// char add start

	// internal & debug related
	"testByText":       BuiltinTestByText,
	"testByStartsWith": BuiltinTestByStartsWith,
	"testByReg":        BuiltinTestByReg,

	// data type related
	"typeCode": BuiltinTypeCode,
	// "typeName": BuiltinTypeName,
	"time":   BuiltinTime,
	"newAny": BuiltinNewAny,

	"toTime": BuiltinToTime,

	// time related

	// control related
	"exit": BuiltinExit,

	// print related
	"pl":  BuiltinPl,
	"pln": BuiltinPln,
	"plo": BuiltinPlo,

	// error related
	"isErrX": BuiltinIsErrX,

	// encode/decode related
	"toJSON":   BuiltinToJSON,
	"toJson":   BuiltinToJSON,
	"fromJSON": BuiltinFromJSON,
	"fromJson": BuiltinFromJSON,

	// command-line related
	"ifSwitchExists": BuiltinIfSwitchExists,
	"getSwitch":      BuiltinGetSwitch,
	"getParam":       BuiltinGetParam,

	// thread related
	"sleep": BuiltinSleep,

	// os/system related
	"systemCmd": BuiltinSystemCmd,

	// misc related
	"getSeq": BuiltinGetSeq,
	"pass":   BuiltinPass,

	// char add end

	"append":      BuiltinAppend,
	"delete":      BuiltinDelete,
	"copy":        BuiltinCopy,
	"repeat":      BuiltinRepeat,
	"contains":    BuiltinContains,
	"len":         BuiltinLen,
	"sort":        BuiltinSort,
	"sortReverse": BuiltinSortReverse,
	"error":       BuiltinError,
	"typeName":    BuiltinTypeName,
	"bool":        BuiltinBool,
	"int":         BuiltinInt,
	"uint":        BuiltinUint,
	"float":       BuiltinFloat,
	"char":        BuiltinChar,
	"string":      BuiltinString,
	"bytes":       BuiltinBytes,
	"chars":       BuiltinChars,
	"printf":      BuiltinPrintf,
	"println":     BuiltinPrintln,
	"sprintf":     BuiltinSprintf,
	"globals":     BuiltinGlobals,

	"isError":     BuiltinIsError,
	"isInt":       BuiltinIsInt,
	"isUint":      BuiltinIsUint,
	"isFloat":     BuiltinIsFloat,
	"isChar":      BuiltinIsChar,
	"isBool":      BuiltinIsBool,
	"isString":    BuiltinIsString,
	"isBytes":     BuiltinIsBytes,
	"isMap":       BuiltinIsMap,
	"isSyncMap":   BuiltinIsSyncMap,
	"isArray":     BuiltinIsArray,
	"isUndefined": BuiltinIsUndefined,
	"isFunction":  BuiltinIsFunction,
	"isCallable":  BuiltinIsCallable,
	"isIterable":  BuiltinIsIterable,

	"WrongNumArgumentsError":  BuiltinWrongNumArgumentsError,
	"InvalidOperatorError":    BuiltinInvalidOperatorError,
	"IndexOutOfBoundsError":   BuiltinIndexOutOfBoundsError,
	"NotIterableError":        BuiltinNotIterableError,
	"NotIndexableError":       BuiltinNotIndexableError,
	"NotIndexAssignableError": BuiltinNotIndexAssignableError,
	"NotCallableError":        BuiltinNotCallableError,
	"NotImplementedError":     BuiltinNotImplementedError,
	"ZeroDivisionError":       BuiltinZeroDivisionError,
	"TypeError":               BuiltinTypeError,

	":makeArray": BuiltinMakeArray,
	"cap":        BuiltinCap,
}

// BuiltinObjects is list of builtins, exported for REPL.
var BuiltinObjects = [...]Object{
	// :makeArray is a private builtin function to help destructuring array assignments
	BuiltinMakeArray: &BuiltinFunction{
		Name:    ":makeArray",
		Value:   funcPiOROe(builtinMakeArrayFunc),
		ValueEx: funcPiOROeEx(builtinMakeArrayFunc),
	},
	// char add start
	BuiltinNewAny: &BuiltinFunction{
		Name:  "newAny",
		Value: builtinNewAnyFunc,
	},
	BuiltinTime: &BuiltinFunction{
		Name:  "time", // new a Time object
		Value: builtinTimeFunc,
	},
	BuiltinToTime: &BuiltinFunction{
		Name:  "toTime", // new a Time object
		Value: builtinTimeFunc,
	},
	BuiltinExit: &BuiltinFunction{
		Name:  "exit", // usage: exit() or exit(1)
		Value: builtinExitFunc,
	},
	BuiltinSleep: &BuiltinFunction{
		Name:  "sleep", // usage: sleep(1.2) sleep for 1.2 seconds
		Value: builtinSleepFunc,
	},
	BuiltinIsErrX: &BuiltinFunction{
		Name:    "isErrX", // usage: isErrX(err1), check if err1 is error or error string(which starts with TXERROR:)
		Value:   CallExAdapter(builtinIsErrXFunc),
		ValueEx: builtinIsErrXFunc,
	},
	BuiltinToJSON: &BuiltinFunction{
		Name:    "toJSON",
		Value:   CallExAdapter(builtinToJSONFunc),
		ValueEx: builtinToJSONFunc,
	},
	BuiltinFromJSON: &BuiltinFunction{
		Name:    "fromJSON",
		Value:   CallExAdapter(builtinFromJSONFunc),
		ValueEx: builtinFromJSONFunc,
	},
	BuiltinPlo: &BuiltinFunction{
		Name: "plo",
		Value: func(args ...Object) (Object, error) {
			return fnAVAR(tk.Plo)(NewCall(nil, args))
		},
		ValueEx: fnAVAR(tk.Plo),
	},
	BuiltinIfSwitchExists: &BuiltinFunction{
		Name:    "ifSwitchExists", // usage: if ifSwitchExists(argsG, "-verbose") {...}
		Value:   CallExAdapter(builtinIfSwitchExistsFunc),
		ValueEx: builtinIfSwitchExistsFunc,
	},
	BuiltinGetSwitch: &BuiltinFunction{
		Name:    "getSwitch",
		Value:   CallExAdapter(builtinGetSwitchFunc),
		ValueEx: builtinGetSwitchFunc,
	},
	BuiltinGetParam: &BuiltinFunction{
		Name:    "getParam", // usage: getParam(argsG, 1, "default")
		Value:   CallExAdapter(builtinGetParamFunc),
		ValueEx: builtinGetParamFunc,
	},
	BuiltinPln: &BuiltinFunction{
		Name: "pln",
		Value: func(args ...Object) (Object, error) {
			return fnAVAR(tk.Pln)(NewCall(nil, args))
		},
		ValueEx: fnAVAR(tk.Pln),
	},
	BuiltinTypeCode: &BuiltinFunction{
		Name:    "typeCode",
		Value:   CallExAdapter(builtinTypeCodeFunc),
		ValueEx: builtinTypeCodeFunc,
	},
	// BuiltinTypeName: &BuiltinFunction{
	// 	Name:    "typeName",
	// 	Value:   CallExAdapter(builtinTypeNameFunc),
	// 	ValueEx: builtinTypeNameFunc,
	// },
	BuiltinPl: &BuiltinFunction{
		Name:    "pl", // usage: the same as printf, but with a line-end(\n) at the end
		Value:   CallExAdapter(builtinPlFunc),
		ValueEx: builtinPlFunc,
	},
	BuiltinPass: &BuiltinFunction{
		Name:    "pass",
		Value:   CallExAdapter(builtinPassFunc),
		ValueEx: builtinPassFunc,
	},
	BuiltinGetSeq: &BuiltinFunction{
		Name: "getSeq",
		Value: func(args ...Object) (Object, error) {
			return fnARI(tk.GetSeq)(NewCall(nil, args))
		},
		ValueEx: fnARI(tk.GetSeq),
	},
	BuiltinTestByText: &BuiltinFunction{
		Name:    "testByText",
		Value:   CallExAdapter(builtinTestByTextFunc),
		ValueEx: builtinTestByTextFunc,
	},
	BuiltinTestByStartsWith: &BuiltinFunction{
		Name:    "testByStartsWith",
		Value:   CallExAdapter(builtinTestByStartsWithFunc),
		ValueEx: builtinTestByStartsWithFunc,
	},
	BuiltinTestByReg: &BuiltinFunction{
		Name:    "testByReg",
		Value:   CallExAdapter(builtinTestByRegFunc),
		ValueEx: builtinTestByRegFunc,
	},
	BuiltinSystemCmd: &BuiltinFunction{
		Name: "systemCmd",
		Value: func(args ...Object) (Object, error) {
			return fnASVSRS(tk.SystemCmd)(NewCall(nil, args))
		},
		ValueEx: fnASVSRS(tk.SystemCmd),
	},
	// char add end
	BuiltinAppend: &BuiltinFunction{
		Name:    "append",
		Value:   CallExAdapter(builtinAppendFunc),
		ValueEx: builtinAppendFunc,
	},
	BuiltinDelete: &BuiltinFunction{
		Name:    "delete",
		Value:   funcPOsRe(builtinDeleteFunc),
		ValueEx: funcPOsReEx(builtinDeleteFunc),
	},
	BuiltinCopy: &BuiltinFunction{
		Name:    "copy",
		Value:   funcPORO(builtinCopyFunc),
		ValueEx: funcPOROEx(builtinCopyFunc),
	},
	BuiltinRepeat: &BuiltinFunction{
		Name:    "repeat",
		Value:   funcPOiROe(builtinRepeatFunc),
		ValueEx: funcPOiROeEx(builtinRepeatFunc),
	},
	BuiltinContains: &BuiltinFunction{
		Name:    "contains",
		Value:   funcPOOROe(builtinContainsFunc),
		ValueEx: funcPOOROeEx(builtinContainsFunc),
	},
	BuiltinLen: &BuiltinFunction{
		Name:    "len",
		Value:   funcPORO(builtinLenFunc),
		ValueEx: funcPOROEx(builtinLenFunc),
	},
	BuiltinCap: &BuiltinFunction{
		Name:    "cap",
		Value:   funcPORO(builtinCapFunc),
		ValueEx: funcPOROEx(builtinCapFunc),
	},
	BuiltinSort: &BuiltinFunction{
		Name:    "sort",
		Value:   funcPOROe(builtinSortFunc),
		ValueEx: funcPOROeEx(builtinSortFunc),
	},
	BuiltinSortReverse: &BuiltinFunction{
		Name:    "sortReverse",
		Value:   funcPOROe(builtinSortReverseFunc),
		ValueEx: funcPOROeEx(builtinSortReverseFunc),
	},
	BuiltinError: &BuiltinFunction{
		Name:    "error",
		Value:   funcPORO(builtinErrorFunc),
		ValueEx: funcPOROEx(builtinErrorFunc),
	},
	BuiltinTypeName: &BuiltinFunction{
		Name:    "typeName",
		Value:   funcPORO(builtinTypeNameFunc),
		ValueEx: funcPOROEx(builtinTypeNameFunc),
	},
	BuiltinBool: &BuiltinFunction{
		Name:    "bool",
		Value:   funcPORO(builtinBoolFunc),
		ValueEx: funcPOROEx(builtinBoolFunc),
	},
	BuiltinInt: &BuiltinFunction{
		Name:    "int",
		Value:   funcPi64RO(builtinIntFunc),
		ValueEx: funcPi64ROEx(builtinIntFunc),
	},
	BuiltinUint: &BuiltinFunction{
		Name:    "uint",
		Value:   funcPu64RO(builtinUintFunc),
		ValueEx: funcPu64ROEx(builtinUintFunc),
	},
	BuiltinFloat: &BuiltinFunction{
		Name:    "float",
		Value:   funcPf64RO(builtinFloatFunc),
		ValueEx: funcPf64ROEx(builtinFloatFunc),
	},
	BuiltinChar: &BuiltinFunction{
		Name:    "char",
		Value:   funcPOROe(builtinCharFunc),
		ValueEx: funcPOROeEx(builtinCharFunc),
	},
	BuiltinString: &BuiltinFunction{
		Name:    "string",
		Value:   funcPORO(builtinStringFunc),
		ValueEx: funcPOROEx(builtinStringFunc),
	},
	BuiltinBytes: &BuiltinFunction{
		Name:    "bytes",
		Value:   CallExAdapter(builtinBytesFunc),
		ValueEx: builtinBytesFunc,
	},
	BuiltinChars: &BuiltinFunction{
		Name:    "chars",
		Value:   funcPOROe(builtinCharsFunc),
		ValueEx: funcPOROeEx(builtinCharsFunc),
	},
	BuiltinPrintf: &BuiltinFunction{
		Name:    "printf",
		Value:   CallExAdapter(builtinPrintfFunc),
		ValueEx: builtinPrintfFunc,
	},
	BuiltinPrintln: &BuiltinFunction{
		Name:    "println",
		Value:   CallExAdapter(builtinPrintlnFunc),
		ValueEx: builtinPrintlnFunc,
	},
	BuiltinSprintf: &BuiltinFunction{
		Name:    "sprintf",
		Value:   CallExAdapter(builtinSprintfFunc),
		ValueEx: builtinSprintfFunc,
	},
	BuiltinGlobals: &BuiltinFunction{
		Name:    "globals",
		Value:   CallExAdapter(builtinGlobalsFunc),
		ValueEx: builtinGlobalsFunc,
	},
	BuiltinIsError: &BuiltinFunction{
		Name:    "isError",
		Value:   CallExAdapter(builtinIsErrorFunc),
		ValueEx: builtinIsErrorFunc,
	},
	BuiltinIsInt: &BuiltinFunction{
		Name:    "isInt",
		Value:   funcPORO(builtinIsIntFunc),
		ValueEx: funcPOROEx(builtinIsIntFunc),
	},
	BuiltinIsUint: &BuiltinFunction{
		Name:    "isUint",
		Value:   funcPORO(builtinIsUintFunc),
		ValueEx: funcPOROEx(builtinIsUintFunc),
	},
	BuiltinIsFloat: &BuiltinFunction{
		Name:    "isFloat",
		Value:   funcPORO(builtinIsFloatFunc),
		ValueEx: funcPOROEx(builtinIsFloatFunc),
	},
	BuiltinIsChar: &BuiltinFunction{
		Name:    "isChar",
		Value:   funcPORO(builtinIsCharFunc),
		ValueEx: funcPOROEx(builtinIsCharFunc),
	},
	BuiltinIsBool: &BuiltinFunction{
		Name:    "isBool",
		Value:   funcPORO(builtinIsBoolFunc),
		ValueEx: funcPOROEx(builtinIsBoolFunc),
	},
	BuiltinIsString: &BuiltinFunction{
		Name:    "isString",
		Value:   funcPORO(builtinIsStringFunc),
		ValueEx: funcPOROEx(builtinIsStringFunc),
	},
	BuiltinIsBytes: &BuiltinFunction{
		Name:    "isBytes",
		Value:   funcPORO(builtinIsBytesFunc),
		ValueEx: funcPOROEx(builtinIsBytesFunc),
	},
	BuiltinIsMap: &BuiltinFunction{
		Name:    "isMap",
		Value:   funcPORO(builtinIsMapFunc),
		ValueEx: funcPOROEx(builtinIsMapFunc),
	},
	BuiltinIsSyncMap: &BuiltinFunction{
		Name:    "isSyncMap",
		Value:   funcPORO(builtinIsSyncMapFunc),
		ValueEx: funcPOROEx(builtinIsSyncMapFunc),
	},
	BuiltinIsArray: &BuiltinFunction{
		Name:    "isArray",
		Value:   funcPORO(builtinIsArrayFunc),
		ValueEx: funcPOROEx(builtinIsArrayFunc),
	},
	BuiltinIsUndefined: &BuiltinFunction{
		Name:    "isUndefined",
		Value:   funcPORO(builtinIsUndefinedFunc),
		ValueEx: funcPOROEx(builtinIsUndefinedFunc),
	},
	BuiltinIsFunction: &BuiltinFunction{
		Name:    "isFunction",
		Value:   funcPORO(builtinIsFunctionFunc),
		ValueEx: funcPOROEx(builtinIsFunctionFunc),
	},
	BuiltinIsCallable: &BuiltinFunction{
		Name:  "isCallable",
		Value: funcPORO(builtinIsCallableFunc),
		//ValueEx: funcPOROEx(builtinIsCallableFunc),
	},
	BuiltinIsIterable: &BuiltinFunction{
		Name:    "isIterable",
		Value:   funcPORO(builtinIsIterableFunc),
		ValueEx: funcPOROEx(builtinIsIterableFunc),
	},

	BuiltinWrongNumArgumentsError:  ErrWrongNumArguments,
	BuiltinInvalidOperatorError:    ErrInvalidOperator,
	BuiltinIndexOutOfBoundsError:   ErrIndexOutOfBounds,
	BuiltinNotIterableError:        ErrNotIterable,
	BuiltinNotIndexableError:       ErrNotIndexable,
	BuiltinNotIndexAssignableError: ErrNotIndexAssignable,
	BuiltinNotCallableError:        ErrNotCallable,
	BuiltinNotImplementedError:     ErrNotImplemented,
	BuiltinZeroDivisionError:       ErrZeroDivision,
	BuiltinTypeError:               ErrType,
}

func builtinMakeArrayFunc(n int, arg Object) (Object, error) {
	if n <= 0 {
		return arg, nil
	}

	arr, ok := arg.(Array)
	if !ok {
		ret := make(Array, n)
		for i := 1; i < n; i++ {
			ret[i] = Undefined
		}
		ret[0] = arg
		return ret, nil
	}

	length := len(arr)
	if n <= length {
		return arr[:n], nil
	}

	ret := make(Array, n)
	x := copy(ret, arr)
	for i := x; i < n; i++ {
		ret[i] = Undefined
	}
	return ret, nil
}

func builtinAppendFunc(c Call) (Object, error) {
	target, ok := c.shift()
	if !ok {
		return Undefined, ErrWrongNumArguments.NewError("want>=1 got=0")
	}

	switch obj := target.(type) {
	case Array:
		obj = append(obj, c.args...)
		obj = append(obj, c.vargs...)
		return obj, nil
	case Bytes:
		n := 0
		for _, args := range [][]Object{c.args, c.vargs} {
			for _, v := range args {
				n++
				switch vv := v.(type) {
				case Int:
					obj = append(obj, byte(vv))
				case Uint:
					obj = append(obj, byte(vv))
				case Char:
					obj = append(obj, byte(vv))
				default:
					return Undefined, NewArgumentTypeError(
						strconv.Itoa(n),
						"int|uint|char",
						vv.TypeName(),
					)
				}
			}
		}
		return obj, nil
	case *UndefinedType:
		ret := make(Array, 0, c.Len())
		ret = append(ret, c.args...)
		ret = append(ret, c.vargs...)
		return ret, nil
	default:
		return Undefined, NewArgumentTypeError(
			"1st",
			"array",
			obj.TypeName(),
		)
	}
}

func builtinDeleteFunc(arg Object, key string) (err error) {
	if v, ok := arg.(IndexDeleter); ok {
		err = v.IndexDelete(ToStringObject(key))
	} else {
		err = NewArgumentTypeError(
			"1st",
			"map|syncMap|IndexDeleter",
			arg.TypeName(),
		)
	}
	return
}

func builtinCopyFunc(arg Object) Object {
	if v, ok := arg.(Copier); ok {
		return v.Copy()
	}
	return arg
}

func builtinRepeatFunc(arg Object, count int) (ret Object, err error) {
	if count < 0 {
		return nil, NewArgumentTypeError(
			"2nd",
			"non-negative integer",
			"negative integer",
		)
	}

	switch v := arg.(type) {
	case Array:
		out := make(Array, 0, len(v)*count)
		for i := 0; i < count; i++ {
			out = append(out, v...)
		}
		ret = out
	case String:
		ret = ToStringObject(strings.Repeat(v.String(), count))
	case Bytes:
		ret = Bytes(bytes.Repeat(v, count))
	default:
		err = NewArgumentTypeError(
			"1st",
			"array|string|bytes",
			arg.TypeName(),
		)
	}
	return
}

func builtinContainsFunc(arg0, arg1 Object) (Object, error) {
	var ok bool
	switch obj := arg0.(type) {
	case Map:
		_, ok = obj[arg1.String()]
	case *SyncMap:
		_, ok = obj.Get(arg1.String())
	case Array:
		for _, item := range obj {
			if item.Equal(arg1) {
				ok = true
				break
			}
		}
	case String:
		ok = strings.Contains(obj.String(), arg1.String())
	case Bytes:
		switch v := arg1.(type) {
		case Int:
			ok = bytes.Contains(obj, []byte{byte(v)})
		case Uint:
			ok = bytes.Contains(obj, []byte{byte(v)})
		case Char:
			ok = bytes.Contains(obj, []byte{byte(v)})
		case String:
			ok = bytes.Contains(obj, []byte(v.String()))
		case Bytes:
			ok = bytes.Contains(obj, v)
		default:
			return Undefined, NewArgumentTypeError(
				"2nd",
				"int|uint|string|char|bytes",
				arg1.TypeName(),
			)
		}
	case *UndefinedType:
	default:
		return Undefined, NewArgumentTypeError(
			"1st",
			"map|array|string|bytes",
			arg0.TypeName(),
		)
	}
	return Bool(ok), nil
}

func builtinLenFunc(arg Object) Object {
	var n int
	if v, ok := arg.(LengthGetter); ok {
		n = v.Len()
	}
	return Int(n)
}

func builtinCapFunc(arg Object) Object {
	var n int
	switch v := arg.(type) {
	case Array:
		n = cap(v)
	case Bytes:
		n = cap(v)
	}
	return Int(n)
}

func builtinSortFunc(arg Object) (ret Object, err error) {
	switch obj := arg.(type) {
	case Array:
		sort.Slice(obj, func(i, j int) bool {
			v, e := obj[i].BinaryOp(token.Less, obj[j])
			if e != nil && err == nil {
				err = e
				return false
			}
			if v != nil {
				return !v.IsFalsy()
			}
			return false
		})
		ret = arg
	case String:
		s := []rune(obj.String())
		sort.Slice(s, func(i, j int) bool {
			return s[i] < s[j]
		})
		ret = ToStringObject(s)
	case Bytes:
		sort.Slice(obj, func(i, j int) bool {
			return obj[i] < obj[j]
		})
		ret = arg
	case *UndefinedType:
		ret = Undefined
	default:
		ret = Undefined
		err = NewArgumentTypeError(
			"1st",
			"array|string|bytes",
			arg.TypeName(),
		)
	}
	return
}

func builtinSortReverseFunc(arg Object) (Object, error) {
	switch obj := arg.(type) {
	case Array:
		var err error
		sort.Slice(obj, func(i, j int) bool {
			v, e := obj[j].BinaryOp(token.Less, obj[i])
			if e != nil && err == nil {
				err = e
				return false
			}
			if v != nil {
				return !v.IsFalsy()
			}
			return false
		})

		if err != nil {
			return nil, err
		}
		return obj, nil
	case String:
		s := []rune(obj.String())
		sort.Slice(s, func(i, j int) bool {
			return s[j] < s[i]
		})
		return ToStringObject(s), nil
	case Bytes:
		sort.Slice(obj, func(i, j int) bool {
			return obj[j] < obj[i]
		})
		return obj, nil
	case *UndefinedType:
		return Undefined, nil
	}

	return Undefined, NewArgumentTypeError(
		"1st",
		"array|string|bytes",
		arg.TypeName(),
	)
}

func builtinErrorFunc(arg Object) Object {
	return &Error{Name: "error", Message: arg.String()}
}

func builtinTypeNameFunc(arg Object) Object { return ToStringObject(arg.TypeName()) }

func builtinBoolFunc(arg Object) Object { return Bool(!arg.IsFalsy()) }

func builtinIntFunc(v int64) Object { return Int(v) }

func builtinUintFunc(v uint64) Object { return Uint(v) }

func builtinFloatFunc(v float64) Object { return Float(v) }

func builtinCharFunc(arg Object) (Object, error) {
	v, ok := ToChar(arg)
	if ok && v != utf8.RuneError {
		return v, nil
	}
	if v == utf8.RuneError || arg == Undefined {
		return Undefined, nil
	}
	return Undefined, NewArgumentTypeError(
		"1st",
		"numeric|string|bool",
		arg.TypeName(),
	)
}

func builtinStringFunc(arg Object) Object { return ToStringObject(arg.String()) }

func builtinBytesFunc(c Call) (Object, error) {
	size := c.Len()

	switch size {
	case 0:
		return Bytes{}, nil
	case 1:
		if v, ok := ToBytes(c.Get(0)); ok {
			return v, nil
		}
	}

	out := make(Bytes, 0, size)
	for _, args := range [][]Object{c.args, c.vargs} {
		for i, obj := range args {
			switch v := obj.(type) {
			case Int:
				out = append(out, byte(v))
			case Uint:
				out = append(out, byte(v))
			case Char:
				out = append(out, byte(v))
			default:
				return Undefined, NewArgumentTypeError(
					strconv.Itoa(i+1),
					"int|uint|char",
					args[i].TypeName(),
				)
			}
		}
	}
	return out, nil
}

func builtinCharsFunc(arg Object) (ret Object, err error) {
	switch obj := arg.(type) {
	case String:
		s := obj.Value
		ret = make(Array, 0, utf8.RuneCountInString(s))
		sz := len(obj.Value)
		i := 0

		for i < sz {
			r, w := utf8.DecodeRuneInString(s[i:])
			if r == utf8.RuneError {
				return Undefined, nil
			}
			ret = append(ret.(Array), Char(r))
			i += w
		}
	case Bytes:
		ret = make(Array, 0, utf8.RuneCount(obj))
		sz := len(obj)
		i := 0

		for i < sz {
			r, w := utf8.DecodeRune(obj[i:])
			if r == utf8.RuneError {
				return Undefined, nil
			}
			ret = append(ret.(Array), Char(r))
			i += w
		}
	default:
		ret = Undefined
		err = NewArgumentTypeError(
			"1st",
			"string|bytes",
			arg.TypeName(),
		)
	}
	return
}

func builtinPrintfFunc(c Call) (ret Object, err error) {
	ret = Undefined
	switch size := c.Len(); size {
	case 0:
		err = ErrWrongNumArguments.NewError("want>=1 got=0")
	case 1:
		_, err = fmt.Fprint(PrintWriter, c.Get(0).String())
	default:
		format, _ := c.shift()
		vargs := make([]interface{}, 0, size-1)
		for i := 0; i < size-1; i++ {
			vargs = append(vargs, c.Get(i))
		}
		_, err = fmt.Fprintf(PrintWriter, format.String(), vargs...)
	}
	return
}

// char add start
func builtinSystemCmdFunc(c Call) (ret Object, err error) {
	ret = Undefined
	switch size := c.Len(); size {
	case 0:
		_, err = fmt.Fprintln(PrintWriter)
	case 1:
		_, err = fmt.Fprintln(PrintWriter, c.Get(0))
	default:
		vargs := make([]interface{}, 0, size)
		for i := 0; i < size; i++ {
			vargs = append(vargs, c.Get(i))
		}
		_, err = fmt.Fprintln(PrintWriter, vargs...)
	}
	return
}

// char add end

func builtinPrintlnFunc(c Call) (ret Object, err error) {
	ret = Undefined
	switch size := c.Len(); size {
	case 0:
		_, err = fmt.Fprintln(PrintWriter)
	case 1:
		_, err = fmt.Fprintln(PrintWriter, c.Get(0))
	default:
		vargs := make([]interface{}, 0, size)
		for i := 0; i < size; i++ {
			vargs = append(vargs, c.Get(i))
		}
		_, err = fmt.Fprintln(PrintWriter, vargs...)
	}
	return
}

func builtinSprintfFunc(c Call) (ret Object, err error) {
	ret = Undefined
	switch size := c.Len(); size {
	case 0:
		err = ErrWrongNumArguments.NewError("want>=1 got=0")
	case 1:
		ret = ToStringObject(c.Get(0).String())
	default:
		format, _ := c.shift()
		vargs := make([]interface{}, 0, size-1)
		for i := 0; i < size-1; i++ {
			vargs = append(vargs, c.Get(i))
		}
		ret = ToStringObject(fmt.Sprintf(format.String(), vargs...))
	}
	return
}

func builtinGlobalsFunc(c Call) (Object, error) {
	return c.VM().GetGlobals(), nil
}

func builtinIsErrorFunc(c Call) (ret Object, err error) {
	ret = False
	switch c.Len() {
	case 1:
		// We have Error, BuiltinError and also user defined error types.
		if _, ok := c.Get(0).(error); ok {
			ret = True
		}
	case 2:
		if err, ok := c.Get(0).(error); ok {
			if target, ok := c.Get(1).(error); ok {
				ret = Bool(errors.Is(err, target))
			}
		}
	default:
		err = ErrWrongNumArguments.NewError(
			"want=1..2 got=", strconv.Itoa(c.Len()))
	}
	return
}

func builtinIsIntFunc(arg Object) Object {
	_, ok := arg.(Int)
	return Bool(ok)
}

func builtinIsUintFunc(arg Object) Object {
	_, ok := arg.(Uint)
	return Bool(ok)
}

func builtinIsFloatFunc(arg Object) Object {
	_, ok := arg.(Float)
	return Bool(ok)
}

func builtinIsCharFunc(arg Object) Object {
	_, ok := arg.(Char)
	return Bool(ok)
}

func builtinIsBoolFunc(arg Object) Object {
	_, ok := arg.(Bool)
	return Bool(ok)
}

func builtinIsStringFunc(arg Object) Object {
	_, ok := arg.(String)
	return Bool(ok)
}

func builtinIsBytesFunc(arg Object) Object {
	_, ok := arg.(Bytes)
	return Bool(ok)
}

func builtinIsMapFunc(arg Object) Object {
	_, ok := arg.(Map)
	return Bool(ok)
}

func builtinIsSyncMapFunc(arg Object) Object {
	_, ok := arg.(*SyncMap)
	return Bool(ok)
}

func builtinIsArrayFunc(arg Object) Object {
	_, ok := arg.(Array)
	return Bool(ok)
}

func builtinIsUndefinedFunc(arg Object) Object {
	_, ok := arg.(*UndefinedType)
	return Bool(ok)
}

func builtinIsFunctionFunc(arg Object) Object {
	_, ok := arg.(*CompiledFunction)
	if ok {
		return True
	}

	_, ok = arg.(*BuiltinFunction)
	if ok {
		return True
	}

	_, ok = arg.(*Function)
	return Bool(ok)
}

func builtinIsCallableFunc(arg Object) Object { return Bool(arg.CanCall()) }

func builtinIsIterableFunc(arg Object) Object { return Bool(arg.CanIterate()) }

func CallExAdapter(fn CallableExFunc) CallableFunc {
	// tk.Pl("CallExAdapter: %v", fn)
	return func(args ...Object) (Object, error) {
		return fn(Call{args: args})
	}
}

// char add start
func toArgsA(offset int, c Call) []interface{} {
	size := c.Len()
	vargs := make([]interface{}, 0, size-offset)
	for i := offset; i < size; i++ {
		vargs = append(vargs, c.Get(i))
	}
	return vargs
}

func toArgsS(offset int, c Call) []string {
	size := c.Len()
	vargs := make([]string, 0, size-offset)
	for i := offset; i < size; i++ {
		vargs = append(vargs, c.Get(i).String())
	}
	return vargs
}

// like tk.GetOSName
func fnARS(fn func() string) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		rs := fn()
		return ToStringObject(rs), nil
	}
}

// like tk.GetSeq
func fnARI(fn func() int) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		rs := fn()
		return ToIntObject(rs), nil
	}
}

// like tk.SystemCmd
func fnASVSRS(fn func(string, ...string) string) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		if c.Len() < 1 {
			return Undefined, ErrWrongNumArguments.NewError(
				"want>=1 got=" + strconv.Itoa(c.Len()))
		}
		vargs := toArgsS(1, c)
		rs := fn(c.Get(0).String(), vargs...)
		return ToStringObject(rs), nil
	}
}

// like tk.Pln
func fnAVAR(fn func(...interface{})) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		vargs := toArgsA(0, c)
		fn(vargs...)
		return nil, nil
	}
}

// like fmt.printf
func fnASVARIE(fn func(string, ...interface{}) (int, error)) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		if c.Len() < 1 {
			return Undefined, ErrWrongNumArguments.NewError(
				"want>=1 got=" + strconv.Itoa(c.Len()))
		}
		vargs := toArgsA(1, c)
		n, err := fn(c.Get(0).String(), vargs...)
		return Int(n), err
	}
}

func builtinTestByTextFunc(c Call) (ret Object, err error) {
	argsA := c.GetArgs()

	lenT := len(argsA)

	if lenT < 2 {
		return nil, fmt.Errorf("not enough parameters")
	}

	v1 := argsA[0]
	v2 := argsA[1]

	var v3 string
	var v4 string

	if lenT > 3 {
		v3 = tk.ToStr(argsA[2])
		v4 = "(" + tk.ToStr(argsA[3]) + ")"
	} else if lenT > 2 {
		v3 = tk.ToStr(argsA[2])
	} else {
		v3 = tk.ToStr(tk.GetSeq())
	}

	nv1, ok := v1.(String)

	if !ok {
		return nil, fmt.Errorf("test %v%v failed(invalid type v1): %#v <-> %#v\n-----\n%v\n-----\n%v", v3, v4, v1, v2, v1, v2)
	}

	nv2, ok := v2.(String)

	if !ok {
		return nil, fmt.Errorf("test %v%v failed(invalid type v2): %#v <-> %#v\n-----\n%v\n-----\n%v", v3, v4, v1, v2, v1, v2)
	}

	if nv1.Value == nv2.Value {
		tk.Pl("test %v%v passed", v3, v4)
	} else {
		return nil, fmt.Errorf("test %v%v failed: %#v <-> %#v\n-----\n%v\n-----\n%v", v3, v4, v1, v2, v1, v2)
	}

	return nil, nil
}

func builtinTestByStartsWithFunc(c Call) (Object, error) {
	argsA := c.GetArgs()
	lenT := len(argsA)

	if lenT < 2 {
		return nil, fmt.Errorf("not enough parameters")
	}

	v1 := argsA[0]
	v2 := argsA[1]

	var v3 string
	var v4 string

	if lenT > 3 {
		v3 = tk.ToStr(argsA[2])
		v4 = "(" + tk.ToStr(argsA[3]) + ")"
	} else if lenT > 2 {
		v3 = tk.ToStr(argsA[2])
	} else {
		v3 = tk.ToStr(tk.GetSeq())
	}

	nv1, ok := v1.(String)

	if !ok {
		return nil, fmt.Errorf("test %v%v failed(invalid type v1): %#v <-> %#v\n-----\n%v\n-----\n%v", v3, v4, v1, v2, v1, v2)
	}

	nv2, ok := v2.(String)

	if !ok {
		return nil, fmt.Errorf("test %v%v failed(invalid type v2): %#v <-> %#v\n-----\n%v\n-----\n%v", v3, v4, v1, v2, v1, v2)
	}

	if strings.HasPrefix(nv1.Value, nv2.Value) {
		tk.Pl("test %v%v passed", v3, v4)
	} else {
		return nil, fmt.Errorf("test %v%v failed: %#v <-> %#v\n-----\n%v\n-----\n%v", v3, v4, v1, v2, v1, v2)
	}

	return nil, nil
}

func builtinTestByRegFunc(c Call) (Object, error) {
	argsA := c.GetArgs()
	lenT := len(argsA)

	if lenT < 2 {
		return nil, fmt.Errorf("not enough parameters")
	}

	v1 := argsA[0]
	v2 := argsA[1]

	var v3 string
	var v4 string

	if lenT > 3 {
		v3 = tk.ToStr(argsA[2])
		v4 = "(" + tk.ToStr(argsA[3]) + ")"
	} else if lenT > 2 {
		v3 = tk.ToStr(argsA[2])
	} else {
		v3 = tk.ToStr(tk.GetSeq())
	}

	nv1, ok := v1.(String)

	if !ok {
		return nil, fmt.Errorf("test %v%v failed(invalid type v1): %#v <-> %#v\n-----\n%v\n-----\n%v", v3, v4, v1, v2, v1, v2)
	}

	nv2, ok := v2.(String)

	if !ok {
		return nil, fmt.Errorf("test %v%v failed(invalid type v2): %#v <-> %#v\n-----\n%v\n-----\n%v", v3, v4, v1, v2, v1, v2)
	}

	if tk.RegMatchX(nv1.Value, nv2.Value) {
		tk.Pl("test %v%v passed", v3, v4)
	} else {
		return nil, fmt.Errorf("test %v%v failed: %#v <-> %#v\n-----\n%v\n-----\n%v", v3, v4, v1, v2, v1, v2)
	}

	return nil, nil
}

func builtinPassFunc(c Call) (Object, error) {
	return Undefined, nil
}

func builtinPlFunc(c Call) (Object, error) {
	args := c.GetArgs()
	if len(args) < 1 {
		return Undefined, nil
	}

	v, ok := args[0].(String)
	if !ok {
		return Undefined, NewCommonError("required format string")
	}

	tk.Pl(v.String(), ObjectsToI(args[1:])...)

	return Undefined, nil
}

func builtinTypeCodeFunc(c Call) (Object, error) {
	args := c.GetArgs()
	return ToIntObject(args[0].TypeCode()), nil
}

// func builtinTypeNameFunc(c Call) (Object, error) {
// 	args := c.GetArgs()
// 	return ToString(args[0].TypeName()), nil
// }

func builtinGetParamFunc(c Call) (Object, error) {
	argsA := c.GetArgs()

	defaultT := ToStringObject("")
	idxT := 1

	if argsA == nil {
		return defaultT, nil
	}

	if len(argsA) < 1 {
		return defaultT, nil
	}

	if len(argsA) > 1 {
		idxT = tk.StrToInt(argsA[1].String(), 1)
	}

	if len(argsA) > 2 {
		defaultT = ToStringObject(argsA[2].String())
	}

	listT, ok := argsA[0].(Array)

	if !ok {
		return defaultT, nil
	}

	var cnt int
	for _, v := range listT {
		argT := v.String()
		if tk.StartsWith(argT, "-") {
			continue
		}

		if cnt == idxT {
			if _, ok := v.(String); ok {
				if tk.StartsWith(argT, "\"") && tk.EndsWith(argT, "\"") {
					return ToStringObject(argT[1 : len(argT)-1]), nil
				}
			}

			return v, nil
		}

		cnt++

	}

	return defaultT, nil
}

func builtinGetSwitchFunc(c Call) (Object, error) {
	argsA := c.GetArgs()
	defaultT := ToStringObject("")

	if argsA == nil {
		return defaultT, nil
	}

	if len(argsA) < 2 {
		return defaultT, nil
	}

	if len(argsA) > 2 {
		defaultT = ToStringObject(argsA[2].String())
	}

	switchStrT := argsA[1].String()

	tmpStrT := ""

	listT, ok := argsA[0].(Array)

	if !ok {
		return defaultT, nil
	}

	for _, v := range listT {
		argOT, ok := v.(String)
		if !ok {
			continue
		}

		argT := FromStringObject(argOT)
		if tk.StartsWith(argT, switchStrT) {
			tmpStrT = argT[len(switchStrT):]
			if tk.StartsWith(tmpStrT, "\"") && tk.EndsWith(tmpStrT, "\"") {
				return ToStringObject(tmpStrT[1 : len(tmpStrT)-1]), nil
			}

			return ToStringObject(tmpStrT), nil
		}

	}

	return defaultT, nil
}

func builtinIfSwitchExistsFunc(c Call) (Object, error) {
	argsA := c.GetArgs()
	if len(argsA) < 2 {
		return Bool(false), nil
	}

	argListT, ok := argsA[0].(Array)

	if !ok {
		return Bool(false), nil
	}

	listT := ObjectsToS(argListT)

	if listT == nil {
		return Bool(false), nil
	}

	return Bool(tk.IfSwitchExistsWhole(listT, argsA[1].String())), nil

}

func builtinToJSONFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return Undefined, NewCommonError("not enough parameters")
	}

	cObjT := ConvertFromObject(args[0])

	rsT := tk.ToJSONX(cObjT, ObjectsToS(args[1:])...)

	return ToStringObject(rsT), nil
}

func builtinFromJSONFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return Undefined, NewCommonError("not enough parameters")
	}

	jsonTextT := args[0].String()

	jObjT := tk.FromJSONWithDefault(jsonTextT, nil)

	cObjT := ConvertToObject(jObjT)

	return cObjT, nil
}

func builtinIsErrXFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return Undefined, NewCommonError("not enough parameters")
	}

	switch nv := args[0].(type) {
	case *Error, *RuntimeError:
		return True, nil
	case String:
		if strings.HasPrefix(nv.Value, "TXERROR:") {
			return True, nil
		}
	case *Any:
		_, ok := nv.Value.(error)

		if ok {
			return True, nil
		}

		s1, ok := nv.Value.(string)

		if ok {
			if strings.HasPrefix(s1, "TXERROR:") {
				return True, nil
			}

			return True, nil
		}

		return False, nil
	}

	return False, nil
}

func builtinSleepFunc(args ...Object) (Object, error) {
	if len(args) < 1 {
		return Undefined, NewCommonError("not enough parameters")
	}

	v := args[0].String()

	f := tk.StrToFloat64WithDefaultValue(v, 0)

	if f <= 0 {
		return Undefined, NewCommonError("invalid time")
	}

	tk.Sleep(f)

	return Undefined, nil
}

func builtinExitFunc(args ...Object) (Object, error) {
	resultT := 0

	if len(args) > 0 {
		resultT = tk.StrToIntWithDefaultValue(args[0].String(), 0)
	}

	os.Exit(resultT)

	return Undefined, nil
}

func builtinTimeFunc(args ...Object) (Object, error) {
	if len(args) < 1 {
		return &Time{Value: time.Now()}, nil
	}

	switch obj := args[0].(type) {
	case Int:
		return &Time{Value: tk.GetTimeFromUnixTimeStampMid(obj.String())}, nil
	// case Float:
	// 	return DateTime{Value: float64(obj)}, nil
	case *Time:
		return &Time{Value: obj.Value}, nil
	case String:
		rsT := tk.ToTime(obj.Value, ObjectsToI(args[1:])...)

		if tk.IsError(rsT) {
			return Undefined, NewCommonError("failed to convert time: %v", rsT)
		}
		return &Time{Value: rsT.(time.Time)}, nil
	default:
		return Undefined, NewCommonError("failed to convert time")
	}
}

func builtinNewAnyFunc(args ...Object) (Object, error) {
	if len(args) < 1 {
		return &Any{Value: nil}, nil
	}

	var s1s string

	s1, ok := args[0].(String)

	if !ok {
		s1s = args[0].String()
	} else {
		s1s = s1.Value
	}

	switch s1s {
	case "strings.Builder", "*strings.Builder", "stringBuilder":
		return builtinMakeStringBuilderFunc(args[1:]...)
		// case "mux":
		// 	return Any{
		// 		Value:        http.NewServeMux(),
		// 		OriginalType: "*http.ServeMux",
		// 	}, nil
	}

	return &Any{Value: nil}, nil
}

func builtinMakeStringBuilderFunc(args ...Object) (Object, error) {
	return &Any{
		Value:        new(strings.Builder),
		OriginalType: "StringBuilder",
	}, nil
}

// func builtinToTimeFunc(args ...Object) (Object, error) {
// 	if len(args) < 1 {
// 		return &Time{Value: time.Now()}, nil
// 	}

// 	switch obj := args[0].(type) {
// 	case Int:
// 		return &Time{Value: tk.GetTimeFromUnixTimeStampMid(obj.String())}, nil
// 	// case Float:
// 	// 	return DateTime{Value: float64(obj)}, nil
// 	case Time:
// 		return &Time{Value: obj.Value}, nil
// 	case String:
// 		rsT := tk.ToTime(obj)

// 		if tk.IsError(rsT) {
// 			return Undefined, NewCommonError("failed to convert time")
// 		}
// 		return &Time{Value: rsT.(time.Time)}, nil
// 	default:
// 		return Undefined, NewCommonError("failed to convert time")
// 	}
// }

// char add end
