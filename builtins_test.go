package charlang

import (
	"bytes"
	"fmt"
	"math"
	"os"
	"reflect"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/topxeq/charlang/tests"
	//	. "github.com/topxeq/charlang"
	tk "github.com/topxeq/tkc"

	"github.com/topxeq/charlang/token"
	"github.com/topxeq/charlang/internal/compat"
)

func TestBuiltinTypes(t *testing.T) {
	for k, v := range BuiltinsMap {
		if BuiltinObjects[v] == nil {
			t.Fatalf("builtin '%s' is missing", k)
		}
		if _, ok := BuiltinObjects[v].(*BuiltinFunction); !ok {
			if _, ok := BuiltinObjects[v].(*Error); !ok {
				t.Fatalf("builtin '%s' is not *BuiltinFunction or *Error type", k)
			}
		}
	}
	if _, ok := BuiltinObjects[BuiltinGlobals].(*BuiltinFunction); !ok {
		t.Fatal("builtin 'global' is not *BuiltinFunction type")
	}
}

func TestBuiltinFuncs(t *testing.T) {
	if false {
		fmt.Println()
	}

	expectRun(t, `return len("abc")`, nil, Int(3))

	require.Equal(t, NewAny(1), &Any{
		Value:        1,
		OriginalType: "",
		OriginalCode: -1,
	})
}

type Struct1 struct {
	
}

func (o *Struct1) Write(p []byte) (n int, err error) {
	return 0, nil
}

func (o *Struct1) Width() (wid int, ok bool) {
	return 0, true
}

func (o *Struct1) Precision() (prec int, ok bool) {
	return 0, true
}

func (o *Struct1) Flag(c int) bool {
	return true
}

func isResultBool(strA string, optsA ...interface{}) bool {
	return tk.ToBool(strA)
}

func TestBuiltinObjects(t *testing.T) {
	var tmpr Object
	var tmpi interface{}
	var err, errT error
	var ok bool
	var gob1, gob2 bool
	var c Call
	
	c = Call{Args: Array{Int(0), Int(1), Int(2)}}
	
	require.True(t, Char(99) == Char('c'))

	expectRun(t, `return spr("%v", bytes(1, 2, 3))`, nil, String{Value: "[1 2 3]"})

	expectRun(t, `return spt(bytesBuffer(bytes(1, 2, 3)).bytes())`, nil, String{Value: "[1 2 3]"})

	expectRun(t, `o1 := image("-height=200"); return spt(o1.width(), o1.height())`, nil, String{Value: "100 200"})

	o1, errT := NewImage(Call{Args: []Object{String{Value: "-width=300"}}})

	if errT != nil {
		t.Fatalf("error occur: %v", errT)
	}

	o1c, ok := o1.(NameCallerObject)

	if !ok {
		t.Fatalf("failed to create object: %v", "image")
	}

	r1, errT := o1c.CallName("width", Call{})

	if errT != nil {
		t.Fatalf("error occur: %v", errT)
	}

	r2, errT := o1c.CallName("height", Call{})

	if errT != nil {
		t.Fatalf("error occur: %v", errT)
	}

	require.Equal(t, "300,100", fmt.Sprintf("%v,%v", r1, r2))

	o1, err = builtinResetFunc(Call{Args: Array{ToStringObject("abcd")}})

	require.Equal(t, `-<nil>`, fmt.Sprintf("%v-%v", o1, err))

	require.Equal(t, `[]`, fmt.Sprintf("%v", QuickRun(`a1 := [1, "a", true]; return reset(a1)`, nil)))

	require.Equal(t, `[]`, fmt.Sprintf("%v", QuickRun(`a1 := [1, "a", true]; return reset(a1)`, nil)))

	ary1 := Array{ToStringObject("a"), ToStringObject("b"), ToStringObject("c"), ToStringObject("d")}
	o2, _ := builtinRemoveItemsFunc(Call{Args: Array{ary1, Int(1), Int(2)}})

	rs := fmt.Sprintf("%v", o2)

	require.Equal(t, `["a", "d"]`, rs)

	o2, _ = builtinRemoveItemFunc(Call{Args: Array{ary1, Int(3)}})

	rs = fmt.Sprintf("%v", o2)

	require.Equal(t, `["a", "b", "c"]`, rs)

	ary2 := Array{ToStringObject("1"), ToStringObject("2"), ToStringObject("3")}

	o2, _ = builtinAppendListFunc(Call{Args: Array{ary1, ary2}})

	require.Equal(t, `["a", "b", "c", "d", "1", "2", "3"]`, fmt.Sprintf("%v", o2))

	o2, _ = builtinGetArrayItemFunc(Call{Args: Array{ary1, Int(1)}})

	require.Equal(t, `b`, fmt.Sprintf("%v", o2))

	m1 := Map{"f1": ToStringObject("v1"), "f2": ToStringObject("v2")}

	o2, _ = builtinGetMapItemFunc(Call{Args: Array{m1, ToStringObject("f2")}})

	require.Equal(t, `v2`, fmt.Sprintf("%v", o2))

	builtinSetMapItemFunc(Call{Args: Array{m1, ToStringObject("field3"), ToStringObject("value3")}})

	require.Equal(t, `{"f1":"v1","f2":"v2","field3":"value3"}`, fmt.Sprintf("%v", tk.ToJSONX(m1, "-sort")))

	o2, _ = builtinByteFunc(Call{Args: Array{Int(255)}})

	require.Equal(t, `255`, fmt.Sprintf("%v", o2))

	o2, _ = builtinByteFunc(Call{Args: Array{Int(256)}})

	require.Equal(t, `0`, fmt.Sprintf("%v", o2))

	o2, _ = builtinMutableStringFunc(Call{Args: Array{ToStringObject("field3")}})

	o2.IndexSet(ToStringObject("value"), ToStringObject("changed"))

	require.Equal(t, `changed`, fmt.Sprintf("%v", o2))

	by1, _ := builtinBytesWithSizeFunc(Call{Args: Array{Int(8)}})

	by1.IndexSet(Int(3), Int(5))

	require.Equal(t, `[0 0 0 5 0 0 0 0]`, fmt.Sprintf("%v", by1))

	by2, _ := builtinBytesWithCapFunc(Call{Args: Array{Int(5)}})

	by2.IndexSet(Int(3), Int(5))

	require.Equal(t, `[]`, fmt.Sprintf("%v", by2))

	by3, _ := by2.BinaryOp(token.Add, ToStringObject("abc"))

	require.Equal(t, `[97 98 99]`, fmt.Sprintf("%v", by3))

	s1, _ := builtinSystemCmdFunc(Call{Args: Array{ToStringObject("dir")}})

	require.Equal(t, `undefined`, fmt.Sprintf("%v", s1.String()))

	bt1, _ := builtinByteFunc(Call{Args: Array{Int(100)}})

	b1 := builtinIsByteFunc(bt1).(Bool)

	require.True(t, b1.BoolValue())

	require.True(t, bool(b1))

	chars1, _ := builtinCharsFunc(ToStringObject("abc今天"))

	b1 = builtinIsCharsFunc(chars1).(Bool)
	
	b1.Format(&Struct1{}, 3)
	
	require.Equal(t, "FmtFormatString: % +-#00.0\x03", fmt.Sprintf("FmtFormatString: %v", compat.FmtFormatString(&Struct1{}, 3)))
	
	require.Equal(t, `bool: true`, fmt.Sprintf("bool: %v", b1))

	require.Equal(t, `bool: %!d(bool=true)`, fmt.Sprintf("bool: %d", b1))

	require.True(t, bool(b1))

	ia1 := toArgsA(0, Call{Args: Array{ary1, Int(3)}})

	require.Equal(t, `[["a", "b", "c", "d"] 3]`, fmt.Sprintf("%v", ia1))

	sa1 := toArgsS(0, Call{Args: Array{ary1, Int(3)}})

	require.Equal(t, `[["a", "b", "c", "d"] 3]`, fmt.Sprintf("%v", sa1))

	sn1 := toArgsN(0, Call{Args: Array{ary1, Int(3)}})

	require.Equal(t, `[0 3]`, fmt.Sprintf("%v", sn1))

	f1 := fnAFRF(math.Sqrt)

	require.Equal(t, `func(...charlang.Object) (charlang.Object, error)`, fmt.Sprintf("%T", f1))

	f2 := fnAFRFex(math.Sqrt)

	require.Equal(t, `func(charlang.Call) (charlang.Object, error)`, fmt.Sprintf("%T", f2))

	require.Equal(t, `func(...charlang.Object) (charlang.Object, error)`, fmt.Sprintf("%T", fnASSRF(tk.CalTextSimilarity)))

	require.Equal(t, `func(charlang.Call) (charlang.Object, error)`, fmt.Sprintf("%T", fnASSRFex(tk.CalTextSimilarity)))

	require.Equal(t, `func(...charlang.Object) (charlang.Object, error)`, fmt.Sprintf("%T", fnARI(tk.GetSeq)))

	require.Equal(t, `charlang.Int`, fmt.Sprintf("%T", OneResultCallAdapter(fnARI(tk.GetSeq))()))

	require.Equal(t, `func(...charlang.Object) (charlang.Object, error)`, fmt.Sprintf("%T", fnALyRB(tk.IsDataEncryptedByTXDEF)))

	require.Equal(t, `func(charlang.Call) (charlang.Object, error)`, fmt.Sprintf("%T", fnALyRBex(tk.IsDataEncryptedByTXDEF)))

	require.Equal(t, `func(...charlang.Object) (charlang.Object, error)`, fmt.Sprintf("%T", fnASVaRB(isResultBool)))

	require.Equal(t, `func(charlang.Call) (charlang.Object, error)`, fmt.Sprintf("%T", fnASVaRBex(isResultBool)))

	require.Equal(t, `func(charlang.Call) (charlang.Object, error)`, fmt.Sprintf("%T", fnASVaRIEex(fmt.Printf)))

	require.Equal(t, `func(...charlang.Object) charlang.Object`, fmt.Sprintf("%T", OneResultCallExAdapter(builtinTestByTextFunc)))

	require.Equal(t, `*charlang.Error`, fmt.Sprintf("%T", OneResultCallExAdapter(builtinTestByTextFunc)()))

	require.True(t, tk.StartsWith(fmt.Sprintf("%v", OneResultCallExAdapter(builtinTestByTextFunc)(Int(1), Int(1))), "error: test 2 failed"))

	require.True(t, tk.StartsWith(fmt.Sprintf("%v", OneResultCallExAdapter(builtinTestByTextFunc)(ToStringObject("aa"), Int(1))), "error: test 3 failed"))

	require.Equal(t, `undefined`, fmt.Sprintf("%v", OneResultCallExAdapter(builtinTestByTextFunc)(ToStringObject("aa"), ToStringObject("aa"))))

	require.True(t, tk.StartsWith(fmt.Sprintf("%v", OneResultCallExAdapter(builtinTestByTextFunc)(ToStringObject("aa"), ToStringObject("ab"))), "error: test "))

	require.Equal(t, `abc`, fmt.Sprintf("%v", GetSwitchFromObjects([]Object{ToStringObject("-p1=1"), ToStringObject("2"), ToStringObject("-p2=abc")}, "-p2=", "")))

	require.Equal(t, ``, fmt.Sprintf("%v", GetSwitchFromObjects(nil, "-p2=", "")))

	require.Equal(t, ``, fmt.Sprintf("%v", GetSwitchFromObjects([]Object{}, "-p2=", "")))

	require.Equal(t, `abc3`, fmt.Sprintf("%v", GetSwitchFromObjects([]Object{ToStringObject("-p1=1"), ToStringObject("2"), ToStringObject("-p2=\"abc3\"")}, "-p2=", "")))

	require.Equal(t, `999`, fmt.Sprintf("%v", GetSwitchFromObjects([]Object{ToStringObject("-p1=1"), ToStringObject("2"), ToStringObject("-p2=\"abc3\"")}, "-a1=", "999")))

	codeT := NewCharCode("a := 1; return a+2", nil)

	byteCodeT := QuickCompile(codeT.Source, codeT.CompilerOptions)
	
	codeT.Value = byteCodeT.(*Bytecode)

	vmT := NewVM(byteCodeT.(*Bytecode))

	require.Equal(t, `0`, fmt.Sprintf("%v", vmT.GetSrcPos()))

	vmT.Run(nil)
	
	require.Equal(t, `0`, fmt.Sprintf("%v", vmT.GetSrcPos()))

	require.Equal(t, `[1]`, fmt.Sprintf("%v", vmT.GetLocalsQuick()))

	require.Equal(t, `12 39 RETURN`, fmt.Sprintf("%v", vmT.GetCurInstr()))

	require.Equal(t, `<compiledFunction>`, fmt.Sprintf("%v", vmT.GetCurFunc()))

	require.Equal(t, `*charlang.CompilerOptions`, fmt.Sprintf("%T", vmT.GetCompilerOptions()))

	require.Equal(t, `string`, fmt.Sprintf("%T", vmT.GetBytecodeInfo()))

	require.Equal(t, `*charlang.Bytecode`, fmt.Sprintf("%T", vmT.GetBytecode()))

	evalT := NewEvalQuick(map[string]interface{}{"scriptPathG": "", "runModeG": "repl"}, MainCompilerOptions)
	
	r, e := evalT.RunByteCode(nil, codeT.Value)

	require.Equal(t, `3-<nil>`, fmt.Sprintf("%v-%v", r, e))

	rr := NewModuleMap()
	rr.Add("abc", nil)
	rr.Remove("abc")
	
	require.Equal(t, `&{map[] <nil>}`, fmt.Sprintf("%v", rr))

	rr2 := rr.Fork("module2")
	
	require.Equal(t, `&{map[] <nil>}`, fmt.Sprintf("%v", rr2))

	rr2.SetExtImporter(nil)

	require.Equal(t, `&{map[] <nil>}`, fmt.Sprintf("%v", rr2))

	require.Equal(t, `<nil>`, fmt.Sprintf("%v", (&CompilerError{}).Unwrap()))
	
	gob1, gob2 = ToGoBool(Int(1))

	require.Equal(t, `true - true`, fmt.Sprintf("%v - %v", gob1, gob2))

	require.Equal(t, `18`, fmt.Sprintf("%v", ToGoIntWithDefault(Float(18.8), 0)))

	require.Equal(t, `18`, fmt.Sprintf("%v", ToGoIntWithDefault(Float(18.8), 0)))
	
	tmpr, ok = ToSyncMap(Float(18.8))

	require.Equal(t, `<nil> - false`, fmt.Sprintf("%v - %v", tmpr, ok))

	tmpr, ok = ToMap(Float(18.8))

	require.Equal(t, `{} - false`, fmt.Sprintf("%v - %v", tmpr, ok))

	tmpr, ok = ToArray(Float(18.8))

	require.Equal(t, `[] - false`, fmt.Sprintf("%v - %v", tmpr, ok))

	tmpr, ok = ToBool(Float(18.8))

	require.Equal(t, `true - true`, fmt.Sprintf("%v - %v", tmpr, ok))

	tmpr, ok = ToFloat(Float(18.8))

	require.Equal(t, `18.8 - true`, fmt.Sprintf("%v - %v", tmpr, ok))

	tmpr, ok = ToUint(Float(18.8))

	require.Equal(t, `18 - true`, fmt.Sprintf("%v - %v", tmpr, ok))

	tmpr, ok = ToInt(Float(18.8))

	require.Equal(t, `18 - true`, fmt.Sprintf("%v - %v", tmpr, ok))

	tmpr, ok = ToString(Float(18.8))

	require.Equal(t, `18.8 - true`, fmt.Sprintf("%v - %v", tmpr, ok))

	require.Equal(t, `18.8`, fmt.Sprintf("%v", ToFloatQuick(Float(18.8))))

	require.Equal(t, `1`, fmt.Sprintf("%v", ToFloatQuick(Bool(true))))

	require.Equal(t, `1`, fmt.Sprintf("%v", ToFloatQuick(Byte(1))))

	require.Equal(t, `1`, fmt.Sprintf("%v", ToFloatQuick(Char(1))))

	require.Equal(t, `1`, fmt.Sprintf("%v", ToFloatQuick(Int(1))))

	require.Equal(t, `1`, fmt.Sprintf("%v", ToFloatQuick(Uint(1))))

	require.Equal(t, `1`, fmt.Sprintf("%v", ToFloatQuick(ToStringObject(1))))

	require.Equal(t, `1`, fmt.Sprintf("%v", ToFloatQuick(&MutableString{Value: "1"})))

	require.Equal(t, `0`, fmt.Sprintf("%v", ToFloatQuick(m1)))

	require.Equal(t, `TXERROR:invalid magic number`, fmt.Sprintf("%v", GetMagic(-1)))

	require.Equal(t, `TXERROR:response status: 404`, fmt.Sprintf("%v", GetMagic(111)))

	require.Equal(t, `<nil>`, fmt.Sprintf("%v", WrapError(nil)))

	require.Equal(t, `Error: error: test`, fmt.Sprintf("%v", WrapError(NewCommonError("test"))))

	require.Equal(t, `<nil>`, fmt.Sprintf("%v", NewFromError(nil)))

	require.Equal(t, `error: error: test`, fmt.Sprintf("%v", NewFromError(NewCommonError("test"))))

	require.Equal(t, `test: 1`, fmt.Sprintf("%v", NewError("test", "%v", "1")))

	require.Equal(t, `error: [pos: ]1`, fmt.Sprintf("%v", NewCommonErrorWithPos(c, "%v", "1")))

	tmpi, err = NewChar("return 1")

	require.Equal(t, `*charlang.Bytecode - <nil>`, fmt.Sprintf("%T - %v", tmpi, err))

	require.Equal(t, ``, fmt.Sprintf("%v", RemoveCfgString("abc")))

	require.Equal(t, `TXERROR:failed to get config string: file not exists`, fmt.Sprintf("%v", GetCfgString("abc")))

	require.Equal(t, ``, fmt.Sprintf("%v", SetCfgString("abc", "123")))

	require.Equal(t, `123`, fmt.Sprintf("%v", GetCfgString("abc")))

	require.Equal(t, `TXERROR:invalid ssh config: `, fmt.Sprintf("%v", DownloadStringFromSSH("abc", "abc")))

	require.Equal(t, `[]byte{0x8, 0x7, 0x9}`, fmt.Sprintf("%#v", ObjectsToBytes(Array{Int(8), Int(7), Int(9)})))

	require.Equal(t, `[]interface {}{}`, fmt.Sprintf("%#v", AnysToOriginal(Array{Int(8), Int(7), Int(9)})))

	require.Equal(t, `[]int{8, 7, 9}`, fmt.Sprintf("%#v", ObjectsToN(Array{Int(8), Int(7), Int(9)})))

	require.Equal(t, `[]interface {}{8, 7, 9}`, fmt.Sprintf("%#v", ObjectsToO(Array{Int(8), Int(7), Int(9)})))

}

type testopts struct {
	globals       Object
	args          []Object
	moduleMap     *ModuleMap
	skip2pass     bool
	isCompilerErr bool
	noPanic       bool
}

func newOpts() *testopts {
	return &testopts{}
}

func (t *testopts) Globals(globals Object) *testopts {
	t.globals = globals
	return t
}

func (t *testopts) Args(args ...Object) *testopts {
	t.args = args
	return t
}

func (t *testopts) Skip2Pass() *testopts {
	t.skip2pass = true
	return t
}

func (t *testopts) CompilerError() *testopts {
	t.isCompilerErr = true
	return t
}

func (t *testopts) NoPanic() *testopts {
	t.noPanic = true
	return t
}

func (t *testopts) Module(name string, module interface{}) *testopts {
	if t.moduleMap == nil {
		t.moduleMap = NewModuleMap()
	}
	switch v := module.(type) {
	case []byte:
		t.moduleMap.AddSourceModule(name, v)
	case string:
		t.moduleMap.AddSourceModule(name, []byte(v))
	case map[string]Object:
		t.moduleMap.AddBuiltinModule(name, v)
	case Map:
		t.moduleMap.AddBuiltinModule(name, v)
	case Importable:
		t.moduleMap.Add(name, v)
	default:
		panic(fmt.Errorf("invalid module type: %T", module))
	}
	return t
}

func assertCompiledFunctionsEqual(t *testing.T,
	expected, got *CompiledFunction, checkSourceMap bool) bool {
	t.Helper()
	if expected.NumParams != got.NumParams {
		t.Errorf("NumParams not equal expected %d, got %d\n",
			expected.NumParams, got.NumParams)
		return false
	}
	if expected.Variadic != got.Variadic {
		t.Errorf("Variadic not equal expected %t, got %t\n",
			expected.Variadic, got.Variadic)
		return false
	}
	if expected.NumLocals != got.NumLocals {
		t.Errorf("NumLocals not equal expected %d, got %d\n",
			expected.NumLocals, got.NumLocals)
		return false
	}
	if string(expected.Instructions) != string(got.Instructions) {
		var buf bytes.Buffer
		buf.WriteString("Expected:\n")
		expected.Fprint(&buf)
		buf.WriteString("\nGot:\n")
		got.Fprint(&buf)
		t.Fatalf("Instructions not equal\n%s", buf.String())
	}
	if len(expected.Free) != len(got.Free) {
		t.Errorf("Free not equal expected %d, got %d\n",
			len(expected.Free), len(got.Free))
		return false
	}
	if checkSourceMap &&
		!reflect.DeepEqual(got.SourceMap, expected.SourceMap) {
		t.Errorf("sourceMaps not equal\n"+
			"Expected:\n%s\nGot:\n%s\n"+
			"Bytecode dump:\n%s\n",
			tests.Sdump(expected.SourceMap), tests.Sdump(got.SourceMap), got)
		return false
	}
	return true
}

func testBytecodesEqual(t *testing.T,
	expected, got *Bytecode, checkSourceMap bool) {

	t.Helper()
	if expected.NumModules != got.NumModules {
		t.Fatalf("NumModules not equal expected %d, got %d\n",
			expected.NumModules, got.NumModules)
	}
	if len(expected.Constants) != len(got.Constants) {
		var buf bytes.Buffer
		got.Fprint(&buf)
		t.Fatalf("Constants not equal\nDump:\n%s\n"+
			"Expected Constants:\n%s\nGot Constants:\n%s\n",
			buf.String(), tests.Sdump(expected.Constants), tests.Sdump(got.Constants))
	}
	if !assertCompiledFunctionsEqual(t,
		expected.Main, got.Main, checkSourceMap) {
		t.Fatal("Main functions not equal")
	}
	for i, gotObj := range got.Constants {
		expectObj := expected.Constants[i]

		switch g := gotObj.(type) {
		case *CompiledFunction:
			ex, ok := expectObj.(*CompiledFunction)
			if !ok {
				t.Fatalf("%T expected at index %d but got %T",
					expectObj, i, gotObj)
			}
			if !assertCompiledFunctionsEqual(t, ex, g, checkSourceMap) {
				t.Fatalf("CompiledFunctions not equal at %d\nExpected:\n"+
					"%s\nGot:\n%s\n", i, ex, g)
			}
			continue
		}
		if !reflect.DeepEqual(expectObj, gotObj) {
			t.Fatalf("Constants not equal at %d\nExpected:\n%s\nGot:\n%s\n",
				i, expectObj, gotObj)
		}
	}
}

func expectRun(t *testing.T, script string, opts *testopts, expect Object) {
	t.Helper()
	if opts == nil {
		opts = newOpts()
	}
	type testCase struct {
		name   string
		opts   CompilerOptions
		tracer bytes.Buffer
	}
	testCases := []testCase{
		{
			name: "default",
			opts: CompilerOptions{
				ModuleMap:      opts.moduleMap,
				OptimizeConst:  true,
				TraceParser:    true,
				TraceOptimizer: true,
				TraceCompiler:  true,
			},
		},
		{
			name: "unoptimized",
			opts: CompilerOptions{
				ModuleMap:      opts.moduleMap,
				TraceParser:    true,
				TraceOptimizer: true,
				TraceCompiler:  true,
			},
		},
	}
	if opts.skip2pass {
		testCases = testCases[:1]
	}
	for _, tC := range testCases {
		t.Run(tC.name, func(t *testing.T) {
			t.Helper()
			tC.opts.Trace = &tC.tracer // nolint exportloopref
			gotBc, err := Compile([]byte(script), &tC.opts)
			require.NoError(t, err)
			// create a copy of the bytecode before execution to test bytecode
			// change after execution
			expectBc := *gotBc
			expectBc.Main = gotBc.Main.Copy().(*CompiledFunction)
			expectBc.Constants = Array(gotBc.Constants).Copy().(Array)
			vm := NewVM(gotBc)
			defer func() {
				if r := recover(); r != nil {
					fmt.Fprintf(os.Stderr, "------- Start Trace -------\n%s"+
						"\n------- End Trace -------\n", tC.tracer.String())
					gotBc.Fprint(os.Stderr)
					panic(r)
				}
			}()
			got, err := vm.SetRecover(opts.noPanic).Run(
				opts.globals,
				opts.args...,
			)
			if !assert.NoErrorf(t, err, "Code:\n%s\n", script) {
				gotBc.Fprint(os.Stderr)
			}
			if !reflect.DeepEqual(expect, got) {
				var buf bytes.Buffer
				gotBc.Fprint(&buf)
				t.Fatalf("Objects not equal:\nExpected:\n%s\nGot:\n%s\nScript:\n%s\n%s\n",
					tests.Sdump(expect), tests.Sdump(got), script, buf.String())
			}
			testBytecodesEqual(t, &expectBc, gotBc, true)
		})
	}
}
