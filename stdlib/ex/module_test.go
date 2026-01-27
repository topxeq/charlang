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

