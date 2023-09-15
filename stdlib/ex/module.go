// Package ex provides ex module implementing some extra functions.
package ex

import (
	"fmt"

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
		return nil, fmt.Errorf("not enough parameters")
	}

	byteCodeT := charlang.QuickCompile(tk.ToStr(argsA[0]), c.VM().GetCompilerOptions()) // quickCompile(tk.ToStr(argsA[0])) //

	if tk.IsError(byteCodeT) {
		return nil, byteCodeT.(error)
	}

	return charlang.NewAny(byteCodeT), nil
}

func runCompiledFunc(argsA ...charlang.Object) (charlang.Object, error) {
	lenT := len(argsA)

	if lenT < 1 {
		return nil, fmt.Errorf("not enough parameters")
	}

	anyT, ok := argsA[0].(*charlang.Any)

	if !ok {
		return nil, fmt.Errorf("invalid code type: (%T)%v", argsA[0], argsA[0])
	}

	valueT := anyT.Value

	byteCodeT, ok := valueT.(*charlang.Bytecode)

	if !ok {
		return nil, fmt.Errorf("invalid code type in Any: (%T)%v", valueT, valueT)
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

	retT, errT := charlang.NewVM(byteCodeT).Run(envT, additionsA...)

	if errT != nil {
		return nil, errT
	}

	return retT, nil
}

func goRunCompiledFunc(argsA ...charlang.Object) (charlang.Object, error) {
	lenT := len(argsA)

	if lenT < 1 {
		return nil, fmt.Errorf("not enough parameters")
	}

	anyT, ok := argsA[0].(*charlang.Any)

	if !ok {
		return nil, fmt.Errorf("invalid code type: (%T)%v", argsA[0], argsA[0])
	}

	valueT := anyT.Value

	byteCodeT, ok := valueT.(*charlang.Bytecode)

	if !ok {
		return nil, fmt.Errorf("invalid code type in Any: (%T)%v", valueT, valueT)
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

	go charlang.NewVM(byteCodeT).Run(envT, additionsA...)

	// if errT != nil {
	// 	return nil, errT
	// }

	return nil, nil
}

// Module represents time module.
var Module = map[string]charlang.Object{
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
	"goRunCompiled": &charlang.Function{
		Name:  "goRunCompiled", // run compiled code in a new thread
		Value: goRunCompiledFunc,
	},
	"threadRunCompiled": &charlang.Function{
		Name:  "threadRunCompiled", // run compiled code in a new thread
		Value: goRunCompiledFunc,
	},
	// funcs end
}
