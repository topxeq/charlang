package ex

import (
	"fmt"
	"math"
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/topxeq/charlang"
//	"github.com/topxeq/charlang/stdlib/ex"
)

func TestBuiltinObjects(t *testing.T) {
	codeT := charlang.NewCharCode("return 1+2", nil)

	byteCodeT := charlang.QuickCompile(codeT.Source, codeT.CompilerOptions)

	codeT.Value = byteCodeT.(*charlang.Bytecode)

	vmT := charlang.NewVM(byteCodeT.(*charlang.Bytecode))

	rs, _ := quickCompileFunc(charlang.Call{Args: []charlang.Object{charlang.ToStringObject("return 1+2")}})

	require.Equal(t, "error: [pos: ]VM is nil", fmt.Sprintf("%v", rs))

	rs, _ = quickCompileFunc(charlang.Call{Vm: vmT, Args: []charlang.Object{charlang.ToStringObject("return 1+2")}})

	require.Equal(t, "(charCode)Bytec", fmt.Sprintf("%v", rs)[0:15])

	rs, _ = quickCompileFunc(charlang.Call{Vm: vmT, Args: []charlang.Object{}})

	require.Equal(t, "error: [pos: 0]not enough parameters", fmt.Sprintf("%v", rs))

	rs, _ = quickCompileFunc(charlang.Call{Vm: vmT, Args: []charlang.Object{charlang.ToStringObject("retu rn 1+2")}})

	require.Equal(t, "error: [pos: 0]Parse Error: expected ';', found rn\n\tat (main):1:6", fmt.Sprintf("%v", rs))

	rs2, _ := quickCompileGelFunc(charlang.Call{Args: []charlang.Object{charlang.ToStringObject("return 1+2")}})

	require.Equal(t, "error: [pos: ]VM is nil", fmt.Sprintf("%v", rs2))

	rs2, _ = quickCompileGelFunc(charlang.Call{Args: []charlang.Object{}})

	require.Equal(t, "error: [pos: ]not enough parameters", fmt.Sprintf("%v", rs2))

	rs2, _ = quickCompileGelFunc(charlang.Call{Vm: vmT, Args: []charlang.Object{charlang.ToStringObject("return 1+2")}})

	require.Equal(t, "(gel)(charCode)", fmt.Sprintf("%v", rs2)[0:len("(gel)(charCode)")])

	rs3, _ := runCompiledFunc(codeT)

	require.Equal(t, "3", fmt.Sprintf("%v", rs3))

	rs4, _ := threadRunCompiledFunc(codeT)

	require.Equal(t, "<nil>", fmt.Sprintf("%v", rs4))

	rs5, _ := threadRunFuncFunc(charlang.Call{Args: []charlang.Object{charlang.Int(1)}})

	require.Equal(t, "error: invalid type: 1", fmt.Sprintf("%v", rs5))

	rs5, _ = builtinSortByFuncQuickFunc(charlang.Call{Args: []charlang.Object{charlang.Int(1)}})

	require.Equal(t, "undefined", fmt.Sprintf("%v", rs5))

	rs5, _ = builtinSortByFuncFunc(charlang.Call{Args: []charlang.Object{charlang.Int(1)}})

	require.Equal(t, "undefined", fmt.Sprintf("%v", rs5))

	rs5, _ = builtinNewFuncFunc(charlang.Call{Args: []charlang.Object{charlang.Int(1)}})

	require.Equal(t, "error: not enough parameters", fmt.Sprintf("%v", rs5))

	rs5, _ = builtinCloseFunc(charlang.Call{Args: []charlang.Object{charlang.Int(1)}})

	require.Equal(t, "error: [pos: ]unsupported type: int", fmt.Sprintf("%v", rs5))

	rs6 := fnAFRF(math.Sqrt)

	require.Equal(t, "func(...charlang.Object) (charlang.Object, error)", fmt.Sprintf("%T", rs6))

	rs7 := fnAFRFex(math.Sqrt)

	require.Equal(t, "func(charlang.Call) (charlang.Object, error)", fmt.Sprintf("%T", rs7))

	require.Equal(t, "3.141592653589793", fmt.Sprintf("%v", Module["math"].(charlang.Map)["Pi"]))

	require.Equal(t, "<function:runCompiled>", fmt.Sprintf("%v", Module["runCompiled"]))
}

// TestMathFunctions tests math module functions
func TestMathFunctions(t *testing.T) {
	mathModule := Module["math"].(charlang.Map)

	// Test constants
	require.Equal(t, charlang.Float(math.Pi), mathModule["Pi"])

	// Test sqrt function (lowercase name in module)
	sqrtFn, ok := mathModule["sqrt"].(*charlang.Function)
	require.True(t, ok)
	require.NotNil(t, sqrtFn.Value)

	result, err := sqrtFn.Value(charlang.Float(4.0))
	require.NoError(t, err)
	require.Equal(t, charlang.Float(2.0), result)

	// Test sqrt with int argument
	result, err = sqrtFn.Value(charlang.Int(16))
	require.NoError(t, err) // Should convert int to float
	require.Equal(t, charlang.Float(4.0), result)
}

// TestSortByFunc tests sortByFunc function
func TestSortByFunc(t *testing.T) {
	// Test with array of maps
	arr := charlang.Array{
		charlang.Map{"name": charlang.String("Alice"), "age": charlang.Int(30)},
		charlang.Map{"name": charlang.String("Bob"), "age": charlang.Int(25)},
		charlang.Map{"name": charlang.String("Charlie"), "age": charlang.Int(35)},
	}

	// Create a simple sort function
	sortFn := func(args ...charlang.Object) (charlang.Object, error) {
		// Simple comparison
		return charlang.Int(0), nil
	}

	result, err := builtinSortByFuncQuickFunc(charlang.Call{
		Args: []charlang.Object{arr, charlang.String("age"), &charlang.Function{Value: sortFn}},
	})

	// The function should return an array
	require.NoError(t, err)
	require.NotNil(t, result)
}

// TestRunCompiled tests runCompiled function more thoroughly
func TestRunCompiled(t *testing.T) {
	// Create a simple bytecode
	code := charlang.NewCharCode("return 10 + 20", nil)
	byteCode := charlang.QuickCompile(code.Source, code.CompilerOptions)
	code.Value = byteCode.(*charlang.Bytecode)

	// Run compiled
	result, err := runCompiledFunc(code)
	require.NoError(t, err)
	require.Equal(t, charlang.Int(30), result)
}

// TestFnAFRF tests the fnAFRF helper function
func TestFnAFRF(t *testing.T) {
	// Test with Sqrt
	sqrtFunc := fnAFRF(math.Sqrt)
	require.NotNil(t, sqrtFunc)

	result, err := sqrtFunc(charlang.Float(16.0))
	require.NoError(t, err)
	require.Equal(t, charlang.Float(4.0), result)

	// Test with non-float argument
	result, err = sqrtFunc(charlang.Int(16))
	require.NoError(t, err) // Should convert int to float
}

// TestFnAFRFex tests the fnAFRFex helper function
func TestFnAFRFex(t *testing.T) {
	// Test with Sqrt
	sqrtFunc := fnAFRFex(math.Sqrt)
	require.NotNil(t, sqrtFunc)

	result, err := sqrtFunc(charlang.Call{Args: []charlang.Object{charlang.Float(25.0)}})
	require.NoError(t, err)
	require.Equal(t, charlang.Float(5.0), result)
}


// TestModuleConstants tests that Module constants are accessible
func TestModuleConstants(t *testing.T) {
	// Check that Module is not nil
	require.NotNil(t, Module)

	// Check math module exists
	mathModule, ok := Module["math"].(charlang.Map)
	require.True(t, ok)
	require.NotNil(t, mathModule)

	// Check Pi constant (uppercase)
	pi, ok := mathModule["Pi"]
	require.True(t, ok)
	require.Equal(t, charlang.Float(math.Pi), pi)

	// Check pi constant (lowercase)
	piLower, ok := mathModule["pi"]
	require.True(t, ok)
	require.Equal(t, charlang.Float(math.Pi), piLower)
}

// TestMathModuleFunctions tests various math module functions
func TestMathModuleFunctions(t *testing.T) {
	mathModule := Module["math"].(charlang.Map)

	// Test sqrt function
	sqrtFn, ok := mathModule["sqrt"].(*charlang.Function)
	require.True(t, ok)
	result, err := sqrtFn.Value(charlang.Float(4.0))
	require.NoError(t, err)
	require.Equal(t, charlang.Float(2.0), result)

	// Test sqrt with larger value
	result, err = sqrtFn.Value(charlang.Float(16.0))
	require.NoError(t, err)
	require.Equal(t, charlang.Float(4.0), result)
}

