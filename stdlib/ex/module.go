// Package ex provides ex module implementing some extra functions.
package ex

import (
	"fmt"
	"io"
	"math"
	"runtime/debug"
	"sort"

	"github.com/topxeq/charlang"
	"github.com/topxeq/tk"
)

// func quickCompile(codeA string) interface{} {
// 	// moduleMap := charlang.NewModuleMap()
// 	// moduleMap.AddBuiltinModule("ex", Module)

// 	// opts := &charlang.CompilerOptions{
// 	// 	ModulePath:        "", //"(repl)",
// 	// 	ModuleMap:         moduleMap,
// 	// 	SymbolTable:       charlang.NewSymbolTable(),
// 	// 	OptimizerMaxCycle: charlang.TraceCompilerOptions.OptimizerMaxCycle,
// 	// 	// TraceParser:       traceParser,
// 	// 	// TraceOptimizer:    traceOptimizer,
// 	// 	// TraceCompiler:     traceCompiler,
// 	// 	// OptimizeConst:     !noOptimizer,
// 	// 	// OptimizeExpr:      !noOptimizer,
// 	// }

// 	bytecodeT, errT := charlang.Compile([]byte(codeA), charlang.DefaultCompilerOptions) // opts) // DefaultCompilerOptions)
// 	if errT != nil {
// 		return errT
// 	}

// 	return bytecodeT
// }

// func callExAdapter(fn charlang.CallableExFunc) charlang.CallableFunc {
// 	return func(argsA ...charlang.Object) (charlang.Object, error) {
// 		return fn(charlang.Call{args: argsA})
// 	}
// }

func quickCompileFunc(c charlang.Call) (charlang.Object, error) {
	argsA := c.GetArgs()

	lenT := len(argsA)

	if lenT < 1 {
		return charlang.NewCommonError("not enough parameters"), nil
		// return nil, fmt.Errorf("not enough parameters")
	}

	codeT := charlang.NewCharCode(argsA[0].String(), c.VM().GetCompilerOptions())

	byteCodeT := charlang.QuickCompile(codeT.Source, codeT.CompilerOptions)

	if tk.IsError(byteCodeT) {
		codeT.LastError = fmt.Sprintf("%v", byteCodeT)
		return charlang.NewCommonError("%v", byteCodeT), nil
		// return nil, byteCodeT.(error)
	}

	codeT.Value = byteCodeT.(*charlang.Bytecode)

	return codeT, nil

	// byteCodeT := charlang.QuickCompile(tk.ToStr(argsA[0]), c.VM().GetCompilerOptions()) // quickCompile(tk.ToStr(argsA[0])) //

	// if tk.IsError(byteCodeT) {
	// 	return charlang.NewCommonError("%v", byteCodeT), nil
	// 	// return nil, byteCodeT.(error)
	// }

	// return charlang.NewAny(byteCodeT), nil
}

func quickCompileGelFunc(c charlang.Call) (charlang.Object, error) {
	argsA := c.GetArgs()

	lenT := len(argsA)

	if lenT < 1 {
		return charlang.NewCommonError("not enough parameters"), nil
		// return nil, fmt.Errorf("not enough parameters")
	}

	codeT := charlang.NewCharCode(argsA[0].String(), c.VM().GetCompilerOptions())

	byteCodeT := charlang.QuickCompile(codeT.Source, codeT.CompilerOptions)

	if tk.IsError(byteCodeT) {
		codeT.LastError = fmt.Sprintf("%v", byteCodeT)
		return charlang.NewCommonError("%v", byteCodeT), nil
		// return nil, byteCodeT.(error)
	}

	codeT.Value = byteCodeT.(*charlang.Bytecode)

	return &charlang.Gel{Value: codeT}, nil

	// byteCodeT := charlang.QuickCompile(tk.ToStr(argsA[0]), c.VM().GetCompilerOptions()) // quickCompile(tk.ToStr(argsA[0])) //

	// if tk.IsError(byteCodeT) {
	// 	return charlang.NewCommonError("%v", byteCodeT), nil
	// 	// return nil, byteCodeT.(error)
	// }

	// return charlang.NewAny(byteCodeT), nil
}

func runCompiledFunc(argsA ...charlang.Object) (charlang.Object, error) {
	lenT := len(argsA)

	if lenT < 1 {
		return charlang.NewCommonError("not enough parameters"), nil
	}

	codeT, ok := argsA[0].(*charlang.CharCode)

	if !ok {
		return charlang.NewCommonError("invalid code type: (%T)%v", argsA[0], argsA[0]), nil
		// return nil, fmt.Errorf("invalid code type: (%T)%v", argsA[0], argsA[0])
	}

	if codeT.Value == nil {
		return charlang.NewCommonError("not compiled"), nil
	}

	var globalsA map[string]interface{} = nil
	var additionsA []charlang.Object = nil

	envT := charlang.NewBaseEnv(globalsA) // Map{}

	if lenT > 1 {
		additionsA = argsA[1:]
	}
	// for i:=1; i< lenT; i ++ {
	// 	envT["v"]
	// }

	retT, errT := charlang.NewVM(codeT.Value).Run(envT, additionsA...)

	if errT != nil {
		return charlang.NewCommonError("%v", errT), nil
	}

	return retT, nil
}

func threadRunCompiledFunc(argsA ...charlang.Object) (charlang.Object, error) {
	lenT := len(argsA)

	if lenT < 1 {
		return charlang.NewCommonError("not enough parameters"), nil
	}

	codeT, ok := argsA[0].(*charlang.CharCode)

	if !ok {
		return charlang.NewCommonError("invalid code type: (%T)%v", argsA[0], argsA[0]), nil
	}

	// valueT := anyT.Value

	// byteCodeT, ok := valueT.(*charlang.Bytecode)

	// if !ok {
	// 	return charlang.NewCommonError("invalid code type: (%T)%v", argsA[0], argsA[0]), nil
	// }

	if codeT.Value == nil {
		return charlang.NewCommonError("not compiled"), nil
	}

	var globalsA map[string]interface{} = nil
	var additionsA []charlang.Object = nil

	envT := charlang.NewBaseEnv(globalsA) // Map{}

	if lenT > 1 {
		additionsA = argsA[1:]
	}
	// for i:=1; i< lenT; i ++ {
	// 	envT["v"]
	// }

	go charlang.NewVM(codeT.Value).Run(envT, additionsA...)

	// if errT != nil {
	// 	return nil, errT
	// }

	return nil, nil
}

func builtinSortByFuncQuickFunc(c charlang.Call) (ret charlang.Object, err error) {
	defer func() {
		if r1 := recover(); r1 != nil {
			ret = charlang.Undefined
			err = fmt.Errorf("runtime exception while sorting: %v\n%v", r1, string(debug.Stack()))
			return
		}
	}()

	args := c.GetArgs()

	arg0 := args[0]

	arg1, ok := args[1].(*charlang.CompiledFunction)

	if !ok {
		return charlang.NewCommonErrorWithPos(c, "invalid type: (%T)%v", args[0], args[0]), nil
	}

	sort.SliceStable(arg0, func(i, j int) bool {
		retT, errT := charlang.NewInvoker(c.VM(), arg1).Invoke(charlang.Int(i), charlang.Int(j))

		if errT != nil {
			return false
		}

		nv1, ok := retT.(charlang.Bool)

		if !ok {
			return false
		}

		return bool(nv1)
	})

	ret = arg0
	err = nil
	return
}

func builtinSortByFuncFunc(c charlang.Call) (ret charlang.Object, err error) {
	defer func() {
		if r1 := recover(); r1 != nil {
			ret = charlang.Undefined
			err = fmt.Errorf("runtime exception while sorting: %v\n%v", r1, string(debug.Stack()))
			return
		}
	}()

	args := c.GetArgs()

	arg0 := args[0]

	arg1, ok := args[1].(*charlang.CompiledFunction)

	if !ok {
		return charlang.NewCommonErrorWithPos(c, "invalid type: (%T)%v", args[0], args[0]), nil
	}

	sort.SliceStable(arg0, func(i, j int) bool {
		retT, errT := charlang.NewInvoker(c.VM(), arg1).Invoke(arg0, charlang.Int(i), charlang.Int(j))

		if errT != nil {
			return false
		}

		nv1, ok := retT.(charlang.Bool)

		if !ok {
			return false
		}

		return bool(nv1)
	})

	ret = arg0
	err = nil

	// switch obj := arg0.(type) {
	// case charlang.Array:
	// 	sort.SliceStable(obj, func(i, j int) bool {
	// 		retT, errT := charlang.NewInvoker(c.VM(), arg1).Invoke(obj, charlang.Int(i), charlang.Int(j))

	// 		if errT != nil {
	// 			return false
	// 		}

	// 		nv1, ok := retT.(charlang.Bool)

	// 		if !ok {
	// 			return false
	// 		}

	// 		return bool(nv1)
	// 	})

	// 	ret = arg0
	// case charlang.String:
	// 	s := []rune(obj.String())
	// 	sort.SliceStable(s, func(i, j int) bool {
	// 		retT, errT := charlang.NewInvoker(c.VM(), arg1).Invoke(obj, charlang.ToStringObject(obj.Value[i]), charlang.ToStringObject(obj.Value[j]))

	// 		if errT != nil {
	// 			return false
	// 		}

	// 		nv1, ok := retT.(charlang.Bool)

	// 		if !ok {
	// 			return false
	// 		}

	// 		return bool(nv1)
	// 	})

	// 	ret = charlang.ToStringObject(s)
	// case *charlang.MutableString:
	// 	s := []rune(obj.String())
	// 	sort.SliceStable(s, func(i, j int) bool {
	// 		retT, errT := charlang.NewInvoker(c.VM(), arg1).Invoke(obj, charlang.ToStringObject(obj.Value[i]), charlang.ToStringObject(obj.Value[j]))

	// 		if errT != nil {
	// 			return false
	// 		}

	// 		nv1, ok := retT.(charlang.Bool)

	// 		if !ok {
	// 			return false
	// 		}

	// 		return bool(nv1)
	// 	})

	// 	ret = charlang.ToMutableStringObject(s)
	// case charlang.Bytes:
	// 	sort.SliceStable(obj, func(i, j int) bool {
	// 		retT, errT := charlang.NewInvoker(c.VM(), arg1).Invoke(obj, charlang.ToStringObject(obj[i]), charlang.ToStringObject(obj[j]))

	// 		if errT != nil {
	// 			return false
	// 		}

	// 		nv1, ok := retT.(charlang.Bool)

	// 		if !ok {
	// 			return false
	// 		}

	// 		return bool(nv1)
	// 	})

	// 	ret = arg0
	// case *charlang.UndefinedType:
	// 	ret = charlang.Undefined
	// default:
	// 	sort.SliceStable(obj, func(i, j int) bool {
	// 		nv1, errT := obj.IndexGet(i)
	// 		if errT != nil {
	// 			nv1 = charlang.NewCommonError("%v", errT)
	// 		}

	// 		nv2, errT := obj.IndexGet(j)
	// 		if errT != nil {
	// 			nv2 = charlang.NewCommonError("%v", errT)
	// 		}

	// 		retT, errT := charlang.NewInvoker(c.VM(), arg1).Invoke(nv1, nv2)

	// 		if errT != nil {
	// 			return false
	// 		}

	// 		nv1, ok := retT.(charlang.Bool)

	// 		if !ok {
	// 			return false
	// 		}

	// 		return bool(nv1)
	// 	})

	// 	ret = charlang.Undefined
	// 	err = charlang.NewArgumentTypeError(
	// 		"1st",
	// 		"array|string|bytes",
	// 		arg0.TypeName(),
	// 	)
	// }

	return
}

func builtinNewFuncFunc(c charlang.Call) (ret charlang.Object, err error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return charlang.NewCommonError("not enough parameters"), nil
	}

	arg0 := args[0].String()

	switch arg0 {
	case "lessFunc":
		arg1, ok := args[1].(*charlang.CompiledFunction)

		if !ok {
			return charlang.NewCommonErrorWithPos(c, "invalid type: (%T)%v", args[1], args[1]), nil
		}

		rs := func(i, j int) bool {

			var retT charlang.Object
			var errT error

			if len(args) > 2 {
				retT, errT = charlang.NewInvoker(c.VM(), arg1).Invoke(args[2], charlang.Int(i), charlang.Int(j))
			} else {
				retT, errT = charlang.NewInvoker(c.VM(), arg1).Invoke(charlang.Int(i), charlang.Int(j))
			}

			if errT != nil {
				return false
			}

			nv1, ok := retT.(charlang.Bool)

			if !ok {
				return false
			}

			return bool(nv1)
		}

		return &charlang.Any{Value: rs, OriginalType: fmt.Sprintf("%T", rs), OriginalCode: -1}, nil
	}

	return charlang.NewCommonErrorWithPos(c, "unsupported type: %v", args[0]), nil

	// arg1, ok := args[1].(*charlang.CompiledFunction)

	// if !ok {
	// 	return charlang.NewCommonErrorWithPos(c, "invalid type: (%T)%v", args[0], args[0]), nil
	// }

	// sort.SliceStable(arg0, func(i, j int) bool {
	// 	retT, errT := charlang.NewInvoker(c.VM(), arg1).Invoke(charlang.Int(i), charlang.Int(j))

	// 	if errT != nil {
	// 		return false
	// 	}

	// 	nv1, ok := retT.(charlang.Bool)

	// 	if !ok {
	// 		return false
	// 	}

	// 	return bool(nv1)
	// })

	// ret = arg0
	// err = nil
	// return
}

func builtinCloseFunc(c charlang.Call) (charlang.Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return charlang.NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	// r1, ok := args[0].(*charlang.Reader)

	// if ok {
	// 	return r1.CallMethod("close")
	// 	// rs := r1.GetMember(strT)

	// 	// if !charlang.IsUndefInternal(rs) {
	// 	// 	f1 := rs.(*Function)
	// 	// 	return (*f1).CallEx(charlang.Call{Args: append([]charlang.Object{o}, argsA[1:]...)}), nil
	// 	// }

	// 	// charlang.CallObjectMethodFunc(r1, "close")
	// }

	typeNameT := args[0].TypeName()

	if typeNameT == "reader" {
		r1 := args[0].(*charlang.Reader)

		rs := r1.GetMember("close")

		if !charlang.IsUndefInternal(rs) {
			f1 := rs.(*charlang.Function)
			return (*f1).CallEx(charlang.Call{Args: []charlang.Object{}})
		}

		return r1.CallMethod("close")
	} else if typeNameT == "any" {
		vT := args[0].(*charlang.Any)
		switch nv := vT.Value.(type) {
		case io.Closer:
			nv.Close()

			return charlang.Undefined, nil

		default:
			return charlang.NewCommonErrorWithPos(c, "unsupported any type: %T", vT.Value), nil

		}
	}

	return charlang.NewCommonErrorWithPos(c, "unsupported type: %v", typeNameT), nil
}

// Module represents ex module.
var Module = map[string]charlang.Object{
	// modules
	"math": charlang.Map{
		// constants
		"Pi": charlang.Float(math.Pi),
		"pi": charlang.Float(math.Pi),

		// funcs
		"sqrt": &charlang.Function{
			Name:    "compile", // compile a piece of code
			Value:   charlang.FnAFRF(math.Sqrt),
			ValueEx: charlang.FnAFRFex(math.Sqrt),
		},
	},
	// "big": charlang.Map{
	// 	// constants
	// 	"Pi": charlang.Float(math.Pi),
	// 	"pi": charlang.Float(math.Pi),

	// 	// funcs
	// 	"sqrt": &charlang.Function{
	// 		Name:    "compile", // compile a piece of code
	// 		Value:   charlang.FnAFRF(math.Sqrt),
	// 		ValueEx: charlang.FnAFRFex(math.Sqrt),
	// 	},
	// },
	// funcs start

	// compile/run/thread related
	"compile": &charlang.Function{
		Name:    "compile", // compile a piece of code
		Value:   charlang.CallExAdapter(quickCompileFunc),
		ValueEx: quickCompileFunc,
	},
	"runCompiled": &charlang.Function{
		Name:  "runCompiled", // run compiled code
		Value: runCompiledFunc,
	},
	"threadRunCompiled": &charlang.Function{
		Name:  "threadRunCompiled", // run compiled code in a new thread
		Value: threadRunCompiledFunc,
	},
	// "threadRunCompiled": &charlang.Function{
	// 	Name:  "threadRunCompiled", // run compiled code in a new thread
	// 	Value: threadRunCompiledFunc,
	// },
	"loadGel": &charlang.Function{
		Name:    "loadGel", // compile a piece of code and turn it to Gel
		Value:   charlang.CallExAdapter(quickCompileGelFunc),
		ValueEx: quickCompileGelFunc,
	},
	"sortByFunc": &charlang.Function{
		Name:    "sortByFunc", // sort by compiled function
		Value:   charlang.CallExAdapter(builtinSortByFuncFunc),
		ValueEx: builtinSortByFuncFunc,
	},
	"sortByFuncQuick": &charlang.Function{
		Name:    "sortByFuncQuick", // sort by compiled function
		Value:   charlang.CallExAdapter(builtinSortByFuncQuickFunc),
		ValueEx: builtinSortByFuncQuickFunc,
	},
	"newFunc": &charlang.Function{
		Name:    "newFunc", // new a go function
		Value:   charlang.CallExAdapter(builtinNewFuncFunc),
		ValueEx: builtinNewFuncFunc,
	},
	"close": &charlang.Function{
		Name:    "close", // close objects that can close
		Value:   charlang.CallExAdapter(builtinCloseFunc),
		ValueEx: builtinCloseFunc,
	},
	// funcs end
}
