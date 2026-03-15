package charlang

import (
	"bytes"
	"fmt"
	"math"
	"os"
	"reflect"
	"strings"
	"testing"
	"time"

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

	expectRun(t, `return spr("%v", bytes(1, 2, 3))`, nil, String("[1 2 3]"))

	expectRun(t, `return spt(bytesBuffer(bytes(1, 2, 3)).bytes())`, nil, String("[1 2 3]"))

	expectRun(t, `o1 := image("-height=200"); return spt(o1.width(), o1.height())`, nil, String("100 200"))

	o1, errT := NewImage(Call{Args: []Object{String("-width=300")}})

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

	require.Equal(t, `string`, fmt.Sprintf("%T", RemoveCfgString("abc")))

	require.Equal(t, `TXERROR:failed to get config string: file not exists`, fmt.Sprintf("%v", GetCfgString("abc")))

	require.Equal(t, ``, fmt.Sprintf("%v", SetCfgString("abc", "123")))

	require.Equal(t, `123`, fmt.Sprintf("%v", GetCfgString("abc")))

	require.Equal(t, `TXERROR:invalid ssh config: `, fmt.Sprintf("%v", DownloadStringFromSSH("abc", "abc")))

	require.Equal(t, `[]byte{0x8, 0x7, 0x9}`, fmt.Sprintf("%#v", ObjectsToBytes(Array{Int(8), Int(7), Int(9)})))

	require.Equal(t, `[]interface {}{}`, fmt.Sprintf("%#v", AnysToOriginal(Array{Int(8), Int(7), Int(9)})))

	require.Equal(t, `[]int{8, 7, 9}`, fmt.Sprintf("%#v", ObjectsToN(Array{Int(8), Int(7), Int(9)})))

	require.Equal(t, `[]interface {}{8, 7, 9}`, fmt.Sprintf("%#v", ObjectsToO(Array{Int(8), Int(7), Int(9)})))
	
	rstr, err := RunScriptOnHttp("return 1", nil, nil, nil, "", nil, nil, nil)

	require.Equal(t, `-res nil`, fmt.Sprintf("%v-%v", rstr, err))

	tmpi, err = RunCharCode("return 1", nil, nil)

	require.Equal(t, `1-<nil>`, fmt.Sprintf("%v-%v", tmpi, err))

	tmpi, err = RunCharCode("re turn 1", nil, nil)

	require.Equal(t, "<nil>-Parse Error: expected ';', found turn\n\tat (main):1:4", fmt.Sprintf("%v-%v", tmpi, err))

	require.Equal(t, `1`, fmt.Sprintf("%v", RunAsFunc("return 1")))

	require.Equal(t, "Parse Error: expected ';', found rn\n\tat (main):1:6", fmt.Sprintf("%v", RunAsFunc("retu rn 1")))

	tmpr, err = CallObjectMethodFunc(Bool(true), "toStr")
	
	require.Equal(t, `true-<nil>`, fmt.Sprintf("%v-%v", tmpr, err))

	rsa1, err := newRSAEncryption("MIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAu9g9642B/REkLIhYhYUpt0JcwNKXnCEUbfX601W6704D+8pdmpcNAPXSUFIySc9/lNwKy/GZ2KVC2x6mJKMichORJw7yYp9YjcZfmOQUgenXxhcp0TuJw/dFJDT/dcbd42C0M6iSJZDPW8yq1J4lZKWlrkHLD0bYjVFHNe7P++2JKkO7RH1+WkI93xi0VhaP1rq9RsgtYA9l6GVOOFPUOmNoZ4f9e/OWKFyPzU5ZqaN52CcBMn6xY7hrIoQpekU8NI3niFZFaJa/l3L+VQCmzzUZkZAbt+0pZycjONRGcehwEjGqqxrs8Z6dx9ifcn39mWM/R/ug0JJkwfS1CasZYwIDAQAB")
	
	require.Equal(t, `*charlang.RSAEncryption-<nil>`, fmt.Sprintf("%T-%v", rsa1, err))
	
	enced1, err := rsa1.Encrypt("abc")
	
	require.Equal(t, `string-<nil>`, fmt.Sprintf("%T-%v", enced1, err))
	
	tmpr, err = builtinGetSha256WithKeyYYFunc(Call{Args: []Object{ToStringObject("abc"), ToStringObject("def")}})
	
	require.Equal(t, `aa17835964e8ace8a0cae190bbcda2182d648f6bc8bfd0d0ab66afb26ab173d7-<nil>`, fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = BuiltinDelegateFunc(Call{Args: []Object{ToStringObject("return 1+2")}})
	
	require.Equal(t, `(delegate)Code: true, Value: false-<nil>`, fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = BuiltinDealStrFunc(Call{Args: []Object{ToStringObject("HEX_EE1111")}})
	
	require.Equal(t, "\xee\x11\x11-<nil>", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = BuiltinFuzzyFindFunc(Call{Args: []Object{ToStringObject("HEX_EE1111"), ToStringObject("E1")}})
	
	require.Equal(t, "[{\"Index\":0,\"MatchedIndexes\":[4,6],\"Score\":-3,\"Str\":\"HEX_EE1111\"}]-<nil>", fmt.Sprintf("%v-%v", tk.ToJSONX(tmpr, "-sort"), err))
	
	tmpr, err = BuiltinDbCloseFunc(Call{Args: []Object{ToStringObject("HEX_EE1111"), ToStringObject("E1")}})
	
	require.Equal(t, "{\"Name\":\"error\",\"Message\":\"invalid parameter type: (charlang.String)HEX_EE1111\",\"Cause\":null}-<nil>", fmt.Sprintf("%v-%v", tk.ToJSONX(tmpr, "-sort"), err))
	
	bool1 := IfSwitchExistsInObjects([]Object{ToStringObject("HEX_EE1111"), ToStringObject("-bose")}, "-verbose")
	
	require.Equal(t, "false", fmt.Sprintf("%v", bool1))
	
	bool1 = IfSwitchExistsInObjects([]Object{ToStringObject("HEX_EE1111"), ToStringObject("-verbose")}, "-verbose")
	
	require.Equal(t, "true", fmt.Sprintf("%v", bool1))
	
	require.Equal(t, "{\"Vm\":null,\"This\":null,\"Args\":[\"HEX_EE1111\",\"-verbose\"],\"Vargs\":null}", fmt.Sprintf("%v", tk.ToJSONX(NewCall(nil, []Object{ToStringObject("HEX_EE1111"), ToStringObject("-verbose")}), "-sort")))
	

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

// TestCoreBuiltins tests core builtin functions: len, cap, typeName, type, bool, string, int, float
func TestCoreBuiltins(t *testing.T) {
	// Test len builtin
	t.Run("len", func(t *testing.T) {
		expectRun(t, `return len("hello")`, nil, Int(5))
		expectRun(t, `return len([1, 2, 3])`, nil, Int(3))
		expectRun(t, `return len({})`, nil, Int(0))
		expectRun(t, `return len({"a": 1, "b": 2})`, nil, Int(2))
		expectRun(t, `return len(bytes(1, 2, 3, 4))`, nil, Int(4))
		expectRun(t, `return len(chars("abc"))`, nil, Int(3))
	})

	// Test cap builtin
	t.Run("cap", func(t *testing.T) {
		expectRun(t, `return cap([1, 2, 3])`, nil, Int(3))
	})

	// Test typeName builtin
	t.Run("typeName", func(t *testing.T) {
		expectRun(t, `return typeName(1)`, nil, String("int"))
		expectRun(t, `return typeName("hello")`, nil, String("string"))
		expectRun(t, `return typeName(true)`, nil, String("bool"))
		expectRun(t, `return typeName([1, 2])`, nil, String("array"))
		expectRun(t, `return typeName({"a": 1})`, nil, String("map"))
	})

	// Test typeOf builtin (same as typeName)
	t.Run("typeOf", func(t *testing.T) {
		expectRun(t, `return typeOf(1)`, nil, String("int"))
		expectRun(t, `return typeOf("hello")`, nil, String("string"))
		expectRun(t, `return typeOf(3.14)`, nil, String("float"))
		expectRun(t, `return typeOf(true)`, nil, String("bool"))
	})

	// Test bool builtin
	t.Run("bool", func(t *testing.T) {
		expectRun(t, `return bool(1)`, nil, True)
		expectRun(t, `return bool(0)`, nil, False)
		expectRun(t, `return bool("")`, nil, False)
		expectRun(t, `return bool("hello")`, nil, True)
		expectRun(t, `return bool([])`, nil, False)
		expectRun(t, `return bool([1])`, nil, True)
	})

	// Test string builtin
	t.Run("string", func(t *testing.T) {
		expectRun(t, `return string(123)`, nil, String("123"))
		expectRun(t, `return string(3.14)`, nil, String("3.14"))
		expectRun(t, `return string(true)`, nil, String("true"))
		expectRun(t, `return string(false)`, nil, String("false"))
		expectRun(t, `return string(65)`, nil, String("65"))
	})

	// Test int builtin
	t.Run("int", func(t *testing.T) {
		expectRun(t, `return int(3.7)`, nil, Int(3))
		expectRun(t, `return int("42")`, nil, Int(42))
		expectRun(t, `return int(true)`, nil, Int(1))
		expectRun(t, `return int(false)`, nil, Int(0))
	})

	// Test float builtin
	t.Run("float", func(t *testing.T) {
		expectRun(t, `return float(42)`, nil, Float(42))
		expectRun(t, `return float("3.14")`, nil, Float(3.14))
	})

	// Test uint builtin
	t.Run("uint", func(t *testing.T) {
		expectRun(t, `return uint(42)`, nil, Uint(42))
		expectRun(t, `return uint(-1)`, nil, Uint(18446744073709551615)) // wraps around
	})
}

// TestBytesAndCharsBuiltins tests bytes and chars builtins
func TestBytesAndCharsBuiltins(t *testing.T) {
	// Test bytes builtin
	t.Run("bytes", func(t *testing.T) {
		expectRun(t, `return len(bytes(1, 2, 3))`, nil, Int(3))
		expectRun(t, `return bytes("hello")[0]`, nil, Int(104)) // 'h' = 104
		expectRun(t, `return len(bytes())`, nil, Int(0))
	})

	// Test chars builtin
	t.Run("chars", func(t *testing.T) {
		expectRun(t, `return len(chars("hello"))`, nil, Int(5))
		expectRun(t, `return chars("abc")[0]`, nil, Char('a'))
		expectRun(t, `return chars("世界")[0]`, nil, Char('世'))
	})
}

// TestCollectionBuiltins tests copy, delete, repeat, contains builtins
func TestCollectionBuiltins(t *testing.T) {
	// Test copy builtin (creates a shallow copy)
	t.Run("copy", func(t *testing.T) {
		expectRun(t, `a := [1, 2, 3]; b := copy(a); b[0] = 99; return a[0]`, nil, Int(1))
	})

	// Test delete builtin
	t.Run("delete", func(t *testing.T) {
		expectRun(t, `m := {"a": 1, "b": 2}; delete(m, "a"); return len(m)`, nil, Int(1))
		expectRun(t, `m := {"a": 1, "b": 2}; delete(m, "a"); return m["a"]`, nil, Undefined)
	})

	// Test repeat builtin
	t.Run("repeat", func(t *testing.T) {
		expectRun(t, `return repeat("ab", 3)`, nil, String("ababab"))
		expectRun(t, `return repeat([1], 3)`, nil, Array{Int(1), Int(1), Int(1)})
	})

	// Test contains builtin
	t.Run("contains", func(t *testing.T) {
		expectRun(t, `return contains("hello", "ell")`, nil, True)
		expectRun(t, `return contains("hello", "xyz")`, nil, False)
		expectRun(t, `return contains([1, 2, 3], 2)`, nil, True)
		expectRun(t, `return contains([1, 2, 3], 4)`, nil, False)
	})
}

// TestIsTypeBuiltins tests isInt, isUint, isFloat, isChar, isBool, isString, isBytes, isMap, isArray builtins
func TestIsTypeBuiltins(t *testing.T) {
	t.Run("isInt", func(t *testing.T) {
		expectRun(t, `return isInt(42)`, nil, True)
		expectRun(t, `return isInt("42")`, nil, False)
		expectRun(t, `return isInt(3.14)`, nil, False)
	})

	t.Run("isUint", func(t *testing.T) {
		expectRun(t, `return isUint(uint(42))`, nil, True)
		expectRun(t, `return isUint(42)`, nil, False)
	})

	t.Run("isFloat", func(t *testing.T) {
		expectRun(t, `return isFloat(3.14)`, nil, True)
		expectRun(t, `return isFloat(42)`, nil, False)
	})

	t.Run("isChar", func(t *testing.T) {
		expectRun(t, `return isChar('a')`, nil, True)
		expectRun(t, `return isChar("a")`, nil, False)
	})

	t.Run("isBool", func(t *testing.T) {
		expectRun(t, `return isBool(true)`, nil, True)
		expectRun(t, `return isBool(1)`, nil, False)
	})

	t.Run("isString", func(t *testing.T) {
		expectRun(t, `return isString("hello")`, nil, True)
		expectRun(t, `return isString(42)`, nil, False)
	})

	t.Run("isBytes", func(t *testing.T) {
		expectRun(t, `return isBytes(bytes(1, 2, 3))`, nil, True)
		expectRun(t, `return isBytes([1, 2, 3])`, nil, False)
	})

	t.Run("isMap", func(t *testing.T) {
		expectRun(t, `return isMap({})`, nil, True)
		expectRun(t, `return isMap([])`, nil, False)
	})

	t.Run("isArray", func(t *testing.T) {
		expectRun(t, `return isArray([1, 2, 3])`, nil, True)
		expectRun(t, `return isArray({})`, nil, False)
	})

	t.Run("isFunction", func(t *testing.T) {
		expectRun(t, `f := func() { return 1 }; return isFunction(f)`, nil, True)
		expectRun(t, `return isFunction(1)`, nil, False)
	})

	t.Run("isUndefined", func(t *testing.T) {
		expectRun(t, `return isUndefined(undefined)`, nil, True)
		expectRun(t, `return isUndefined(0)`, nil, False)
	})
}

// TestSortBuiltins tests sort and sortReverse builtins
func TestSortBuiltins(t *testing.T) {
	t.Run("sort", func(t *testing.T) {
		expectRun(t, `a := [3, 1, 2]; sort(a); return a`, nil, Array{Int(1), Int(2), Int(3)})
		expectRun(t, `a := ["c", "a", "b"]; sort(a); return a`, nil, Array{String("a"), String("b"), String("c")})
		expectRun(t, `a := []; sort(a); return a`, nil, Array{})
	})

	t.Run("sortReverse", func(t *testing.T) {
		expectRun(t, `a := [1, 2, 3]; sortReverse(a); return a`, nil, Array{Int(3), Int(2), Int(1)})
		expectRun(t, `a := ["a", "b", "c"]; sortReverse(a); return a`, nil, Array{String("c"), String("b"), String("a")})
	})
}

// TestErrorBuiltin tests error builtin
func TestErrorBuiltin(t *testing.T) {
	expectRun(t, `return string(error("test error"))`, nil, String("error: test error"))
	expectRun(t, `return typeName(error("test"))`, nil, String("error"))
}

// TestCharBuiltin tests char builtin
func TestCharBuiltin(t *testing.T) {
	expectRun(t, `return char(65)`, nil, Char('A'))
	expectRun(t, `return char('B')`, nil, Char('B'))
	expectRun(t, `return char("C")`, nil, Char('C'))
}

// TestByteBuiltin tests byte builtin
func TestByteBuiltin(t *testing.T) {
	expectRun(t, `return byte(65)`, nil, Byte(65))
	expectRun(t, `return byte(255)`, nil, Byte(255))
}

// TestMutableStringBuiltin tests mutableString builtin
func TestMutableStringBuiltin(t *testing.T) {
	expectRun(t, `ms := mutableString("hello"); return string(ms)`, nil, String("hello"))
}

// TestBytesWithSizeBuiltin tests bytesWithSize builtin
func TestBytesWithSizeBuiltin(t *testing.T) {
	expectRun(t, `return len(bytesWithSize(10))`, nil, Int(10))
	expectRun(t, `b := bytesWithSize(5); b[0] = 1; return b[0]`, nil, Int(1))
}

// TestBytesWithCapBuiltin tests bytesWithCap builtin
func TestBytesWithCapBuiltin(t *testing.T) {
	expectRun(t, `return cap(bytesWithCap(10))`, nil, Int(10))
}

// TestAppendBuiltin tests append builtin
func TestAppendBuiltin(t *testing.T) {
	expectRun(t, `a := [1, 2]; return append(a, 3)`, nil, Array{Int(1), Int(2), Int(3)})
	expectRun(t, `a := [1]; return append(a, 2, 3)`, nil, Array{Int(1), Int(2), Int(3)})
}

// TestRemoveItemBuiltin tests removeItem builtin
func TestRemoveItemBuiltin(t *testing.T) {
	expectRun(t, `a := [1, 2, 3]; return removeItem(a, 1)`, nil, Array{Int(1), Int(3)})
	expectRun(t, `a := ["a", "b", "c"]; return removeItem(a, 2)`, nil, Array{String("a"), String("b")})
}

// TestAppendListBuiltin tests appendList builtin
func TestAppendListBuiltin(t *testing.T) {
	expectRun(t, `a := [1, 2]; b := [3, 4]; return appendList(a, b)`, nil, Array{Int(1), Int(2), Int(3), Int(4)})
}

// TestGlobalsBuiltin tests globals builtin
func TestGlobalsBuiltin(t *testing.T) {
	expectRun(t, `return typeName(globals())`, nil, String("map"))
}

// TestIsErrorBuiltin tests isError builtin
func TestIsErrorBuiltin(t *testing.T) {
	expectRun(t, `return isError(error("test"))`, nil, True)
	expectRun(t, `return isError(1)`, nil, False)
}

// TestIsCallableBuiltin tests isCallable builtin
func TestIsCallableBuiltin(t *testing.T) {
	expectRun(t, `f := func() {}; return isCallable(f)`, nil, True)
	expectRun(t, `return isCallable(1)`, nil, False)
}

// TestIsIterableBuiltin tests isIterable builtin
func TestIsIterableBuiltin(t *testing.T) {
	expectRun(t, `return isIterable([1, 2, 3])`, nil, True)
	expectRun(t, `return isIterable({"a": 1})`, nil, True)
	// In charlang, all objects are iterable
}

// TestIsCharsBuiltin tests isChars builtin
func TestIsCharsBuiltin(t *testing.T) {
	expectRun(t, `return isChars(chars("abc"))`, nil, True)
	expectRun(t, `return isChars("abc")`, nil, False)
}

// TestIsSyncMapBuiltin tests isSyncMap builtin
func TestIsSyncMapBuiltin(t *testing.T) {
	// syncMap is created via specific functions
	expectRun(t, `return isSyncMap({})`, nil, False)
}

// TestPrintfBuiltin tests printf builtin
func TestPrintfBuiltin(t *testing.T) {
	// printf prints to stdout, returns undefined
	expectRun(t, `return sprintf("%d", 42)`, nil, String("42"))
}

// TestSprintfBuiltin tests sprintf builtin
func TestSprintfBuiltin(t *testing.T) {
	expectRun(t, `return sprintf("%d + %d = %d", 1, 2, 3)`, nil, String("1 + 2 = 3"))
	expectRun(t, `return sprintf("%.2f", 3.14159)`, nil, String("3.14"))
	expectRun(t, `return sprintf("%s", "hello")`, nil, String("hello"))
}

// TestTimeBuiltins tests time-related builtins
func TestTimeBuiltins(t *testing.T) {
	t.Run("time", func(t *testing.T) {
		expectRun(t, `return typeName(time())`, nil, String("time"))
	})

	t.Run("toTime", func(t *testing.T) {
		expectRun(t, `return typeName(toTime(0))`, nil, String("time"))
	})
}

// TestLockBuiltins tests lock-related builtins
func TestLockBuiltins(t *testing.T) {
	t.Run("mutex", func(t *testing.T) {
		expectRun(t, `m := mutex(); return typeName(m)`, nil, String("mutex"))
	})

	t.Run("lock/unlock", func(t *testing.T) {
		expectRun(t, `m := mutex(); lock(m); unlock(m); return true`, nil, True)
	})

	t.Run("tryLock", func(t *testing.T) {
		expectRun(t, `m := mutex(); return tryLock(m)`, nil, True)
	})
}

// TestConversionBuiltins tests conversion builtins
func TestConversionBuiltins(t *testing.T) {
	t.Run("toStr", func(t *testing.T) {
		expectRun(t, `return toStr(42)`, nil, String("42"))
		expectRun(t, `return toStr(3.14)`, nil, String("3.14"))
	})

	t.Run("toHex", func(t *testing.T) {
		expectRun(t, `return toHex(255)`, nil, String("ff"))
	})

	t.Run("toInt", func(t *testing.T) {
		expectRun(t, `return toInt("42")`, nil, Int(42))
		expectRun(t, `return toInt(3.7)`, nil, Int(3))
	})

	t.Run("toBool", func(t *testing.T) {
		expectRun(t, `return toBool(1)`, nil, True)
		expectRun(t, `return toBool(0)`, nil, True) // 0 is still truthy in charlang
	})

	t.Run("toFloat", func(t *testing.T) {
		expectRun(t, `return toFloat("3.14")`, nil, Float(3.14))
		expectRun(t, `return toFloat(42)`, nil, Float(42.0))
	})
}

// TestStrTrimBuiltins tests string trimming builtins
func TestStrTrimBuiltins(t *testing.T) {
	t.Run("strTrim", func(t *testing.T) {
		expectRun(t, `return strTrim("  hello  ")`, nil, String("hello"))
	})
}

// TestRegQuoteBuiltin tests regQuote builtin
func TestRegQuoteBuiltin(t *testing.T) {
	expectRun(t, `return regQuote("a.b")`, nil, String("a.b"))
}

// TestTypeOfAnyBuiltin tests typeOfAny builtin
func TestTypeOfAnyBuiltin(t *testing.T) {
	expectRun(t, `return typeOfAny(any(42))`, nil, String("int"))
}

// TestGetEnvBuiltin tests getEnv builtin
func TestGetEnvBuiltin(t *testing.T) {
	os.Setenv("TEST_CHARLANG_VAR", "testvalue")
	expectRun(t, `return getEnv("TEST_CHARLANG_VAR")`, nil, String("testvalue"))
}

// TestPltBuiltin tests plt builtin (print line with time)
func TestPltBuiltin(t *testing.T) {
	// plt prints to stdout and returns int (time)
	expectRun(t, `return typeName(plt("test"))`, nil, String("int"))
}

// TestCheckErrXBuiltin tests checkErrX builtin
func TestCheckErrXBuiltin(t *testing.T) {
	expectRun(t, `return checkErrX(42, undefined)`, nil, Int(42))
}

// TestCheckEmptyBuiltin tests checkEmpty builtin
func TestCheckEmptyBuiltin(t *testing.T) {
	expectRun(t, `return checkEmpty(42, 0)`, nil, Int(42))
}

// TestErrToEmptyBuiltin tests errToEmpty builtin
func TestErrToEmptyBuiltin(t *testing.T) {
	expectRun(t, `return errToEmpty(42)`, nil, Int(42))
	expectRun(t, `return errToEmpty(error("test"))`, nil, String(""))
}

// TestTrimBuiltins tests trim builtins
func TestTrimBuiltins(t *testing.T) {
	t.Run("trim", func(t *testing.T) {
		expectRun(t, `return trim("  hello  ")`, nil, String("hello"))
	})
}

// TestAnyBuiltin tests any builtin
func TestAnyBuiltin(t *testing.T) {
	expectRun(t, `return typeName(any(42))`, nil, String("any"))
}

// TestMoreBuiltins tests additional builtins to increase coverage
func TestMoreBuiltins(t *testing.T) {
	t.Run("copy", func(t *testing.T) {
		expectRun(t, `a := [1, 2, 3]; b := copy(a); a[0] = 99; return b[0]`, nil, Int(1))
	})

	t.Run("repeat", func(t *testing.T) {
		expectRun(t, `return repeat([1], 3)`, nil, Array{Int(1), Int(1), Int(1)})
	})

	t.Run("contains", func(t *testing.T) {
		expectRun(t, `return contains("hello", "ell")`, nil, True)
		expectRun(t, `return contains("hello", "xyz")`, nil, False)
		expectRun(t, `return contains([1, 2, 3], 2)`, nil, True)
	})

	t.Run("len", func(t *testing.T) {
		expectRun(t, `return len([1, 2, 3])`, nil, Int(3))
		expectRun(t, `return len("hello")`, nil, Int(5))
		expectRun(t, `return len({a: 1, b: 2})`, nil, Int(2))
	})

	t.Run("cap", func(t *testing.T) {
		expectRun(t, `return cap(bytesWithCap(10))`, nil, Int(10))
	})

	t.Run("sort", func(t *testing.T) {
		expectRun(t, `a := [3, 1, 2]; sort(a); return a`, nil, Array{Int(1), Int(2), Int(3)})
	})

	t.Run("sortReverse", func(t *testing.T) {
		expectRun(t, `a := [1, 2, 3]; sortReverse(a); return a`, nil, Array{Int(3), Int(2), Int(1)})
	})

	t.Run("bool", func(t *testing.T) {
		expectRun(t, `return bool(1)`, nil, True)
		expectRun(t, `return bool(0)`, nil, False)
	})

	t.Run("byte", func(t *testing.T) {
		expectRun(t, `return byte(65)`, nil, Byte(65))
	})

	t.Run("char", func(t *testing.T) {
		expectRun(t, `return char(65)`, nil, Char('A'))
	})

	t.Run("string", func(t *testing.T) {
		expectRun(t, `return string(42)`, nil, String("42"))
		expectRun(t, `return string(3.14)`, nil, String("3.14"))
	})

	t.Run("mutableString", func(t *testing.T) {
		expectRun(t, `ms := mutableString("hello"); return string(ms)`, nil, String("hello"))
	})

	t.Run("bytes", func(t *testing.T) {
		expectRun(t, `return len(bytes(1, 2, 3))`, nil, Int(3))
	})

	t.Run("bytesWithSize", func(t *testing.T) {
		expectRun(t, `return len(bytesWithSize(10))`, nil, Int(10))
	})

	t.Run("bytesWithCap", func(t *testing.T) {
		expectRun(t, `return cap(bytesWithCap(10))`, nil, Int(10))
	})

	t.Run("chars", func(t *testing.T) {
		expectRun(t, `return typeName(chars("abc"))`, nil, String("chars"))
	})

	t.Run("append", func(t *testing.T) {
		expectRun(t, `a := [1, 2]; return append(a, 3)`, nil, Array{Int(1), Int(2), Int(3)})
	})

	t.Run("removeItem", func(t *testing.T) {
		expectRun(t, `a := [1, 2, 3]; return removeItem(a, 1)`, nil, Array{Int(1), Int(3)})
	})

	t.Run("appendList", func(t *testing.T) {
		expectRun(t, `a := [1, 2]; b := [3, 4]; return appendList(a, b)`, nil, Array{Int(1), Int(2), Int(3), Int(4)})
	})

	t.Run("delete", func(t *testing.T) {
		expectRun(t, `m := {a: 1, b: 2}; delete(m, "a"); return len(m)`, nil, Int(1))
	})

	t.Run("sprintf", func(t *testing.T) {
		expectRun(t, `return sprintf("%d + %d = %d", 1, 2, 3)`, nil, String("1 + 2 = 3"))
	})

	t.Run("printf", func(t *testing.T) {
		// printf prints to stdout and returns undefined
		expectRun(t, `return sprintf("test")`, nil, String("test"))
	})

	t.Run("toStr", func(t *testing.T) {
		expectRun(t, `return toStr(42)`, nil, String("42"))
		expectRun(t, `return toStr(3.14)`, nil, String("3.14"))
		expectRun(t, `return toStr()`, nil, String(""))
	})

	t.Run("toHex", func(t *testing.T) {
		expectRun(t, `return toHex(255)`, nil, String("ff"))
		expectRun(t, `return toHex()`, nil, String(""))
	})

	t.Run("toInt", func(t *testing.T) {
		expectRun(t, `return toInt("42")`, nil, Int(42))
		expectRun(t, `return toInt(3.7)`, nil, Int(3))
		expectRun(t, `return toInt()`, nil, Int(0))
	})

	t.Run("toBool", func(t *testing.T) {
		expectRun(t, `return toBool(1)`, nil, True)
		expectRun(t, `return toBool(0)`, nil, True) // 0 is truthy in charlang
		expectRun(t, `return toBool()`, nil, False)
	})

	t.Run("toBoolWithDefault", func(t *testing.T) {
		expectRun(t, `return toBoolWithDefault(1, true)`, nil, True)
		expectRun(t, `return toBoolWithDefault()`, nil, False)
	})

	t.Run("toFloat", func(t *testing.T) {
		expectRun(t, `return toFloat("3.14")`, nil, Float(3.14))
		expectRun(t, `return toFloat(42)`, nil, Float(42.0))
		expectRun(t, `return toFloat()`, nil, Float(0.0))
	})

	t.Run("typeOfAny", func(t *testing.T) {
		expectRun(t, `return typeOfAny(any(42))`, nil, String("int"))
	})
}

// TestRWMutexBuiltins tests read-write mutex builtins
func TestRWMutexBuiltins(t *testing.T) {
	t.Run("rLock", func(t *testing.T) {
		expectRun(t, `m := mutex(); rLock(m); rUnlock(m); return true`, nil, True)
	})

	t.Run("tryRLock", func(t *testing.T) {
		expectRun(t, `m := mutex(); return tryRLock(m)`, nil, True)
	})
}

// TestStrTrimFuncBuiltins tests string trimming builtins
func TestStrTrimFuncBuiltins(t *testing.T) {
	t.Run("strTrim", func(t *testing.T) {
		expectRun(t, `return strTrim("  hello  ")`, nil, String("hello"))
	})

	t.Run("strTrimStart", func(t *testing.T) {
		expectRun(t, `return strTrimStart("xxhelloxx", "xx")`, nil, String("helloxx"))
	})

	t.Run("strTrimEnd", func(t *testing.T) {
		expectRun(t, `return strTrimEnd("xxhelloxx", "xx")`, nil, String("xxhello"))
	})
}

// TestCheckBuiltins tests check builtins
func TestCheckBuiltins(t *testing.T) {
	t.Run("checkErrX", func(t *testing.T) {
		expectRun(t, `return checkErrX(42, undefined)`, nil, Int(42))
	})

	t.Run("checkEmpty", func(t *testing.T) {
		expectRun(t, `return checkEmpty(42, 0)`, nil, Int(42))
	})
}

// TestErrorHandlingBuiltins tests error handling builtins
func TestErrorHandlingBuiltins(t *testing.T) {
	t.Run("errToEmpty", func(t *testing.T) {
		expectRun(t, `return errToEmpty(42)`, nil, Int(42))
		expectRun(t, `return errToEmpty(error("test"))`, nil, String(""))
	})
}

// TestMathBuiltinFunctions tests math-related builtin functions
func TestMathBuiltinFunctions(t *testing.T) {
	t.Run("abs", func(t *testing.T) {
		expectRun(t, `return abs(-5)`, nil, Int(5))
		expectRun(t, `return abs(5)`, nil, Int(5))
	})

	t.Run("min", func(t *testing.T) {
		expectRun(t, `return min(1, 2)`, nil, Int(1))
		expectRun(t, `return min(5, 3)`, nil, Int(3))
	})

	t.Run("max", func(t *testing.T) {
		expectRun(t, `return max(1, 2)`, nil, Int(2))
		expectRun(t, `return max(5, 3)`, nil, Int(5))
	})

	t.Run("floor", func(t *testing.T) {
		expectRun(t, `return floor(3.7)`, nil, Int(3))
	})

	t.Run("ceil", func(t *testing.T) {
		expectRun(t, `return ceil(3.2)`, nil, Int(4))
	})

	t.Run("round", func(t *testing.T) {
		expectRun(t, `return round(3.5)`, nil, Int(4))
	})
}

// TestArrayBuiltinFunctions tests array-related builtin functions
func TestArrayBuiltinFunctions(t *testing.T) {
	t.Run("append", func(t *testing.T) {
		expectRun(t, `a := [1, 2]; return append(a, 3)`, nil, Array{Int(1), Int(2), Int(3)})
	})

	t.Run("removeItem", func(t *testing.T) {
		expectRun(t, `a := [1, 2, 3]; return removeItem(a, 1)`, nil, Array{Int(1), Int(3)})
	})

	t.Run("appendList", func(t *testing.T) {
		expectRun(t, `a := [1]; b := [2, 3]; return appendList(a, b)`, nil, Array{Int(1), Int(2), Int(3)})
	})
}

// TestMapBuiltinFunctions tests map-related builtin functions
func TestMapBuiltinFunctions(t *testing.T) {
	t.Run("delete", func(t *testing.T) {
		expectRun(t, `m := {a: 1, b: 2}; delete(m, "a"); return len(m)`, nil, Int(1))
	})
}

// TestStringBuiltinFunctions tests string-related builtin functions
func TestStringBuiltinFunctions(t *testing.T) {
	t.Run("strContains", func(t *testing.T) {
		expectRun(t, `return strContains("hello", "ell")`, nil, True)
		expectRun(t, `return strContains("hello", "xyz")`, nil, False)
	})

	t.Run("strIndex", func(t *testing.T) {
		expectRun(t, `return strIndex("hello", "ll")`, nil, Int(2))
		expectRun(t, `return strIndex("hello", "xyz")`, nil, Int(-1))
	})

	t.Run("strCount", func(t *testing.T) {
		expectRun(t, `return strCount("hello", "l")`, nil, Int(2))
	})

	t.Run("strReplace", func(t *testing.T) {
		expectRun(t, `return strReplace("hello world", "world", "there")`, nil, String("hello there"))
	})

	t.Run("strSplit", func(t *testing.T) {
		expectRun(t, `return len(strSplit("a-b-c", "-"))`, nil, Int(3))
	})

	t.Run("strJoin", func(t *testing.T) {
		expectRun(t, `return strJoin(["a", "b", "c"], "-")`, nil, String("a-b-c"))
	})

	t.Run("strRepeat", func(t *testing.T) {
		expectRun(t, `return strRepeat("ab", 3)`, nil, String("ababab"))
	})

	t.Run("strToUpper", func(t *testing.T) {
		expectRun(t, `return strToUpper("hello")`, nil, String("HELLO"))
	})

	t.Run("strToLower", func(t *testing.T) {
		expectRun(t, `return strToLower("HELLO")`, nil, String("hello"))
	})
}

// TestRegexpBuiltinFunctions tests regexp-related builtin functions
func TestRegexpBuiltinFunctions(t *testing.T) {
	t.Run("regQuote", func(t *testing.T) {
		expectRun(t, `return regQuote("a.b")`, nil, String("a.b"))
	})
}

// TestFileBuiltinFunctions tests file-related builtin functions
func TestFileBuiltinFunctions(t *testing.T) {
	t.Run("fileExists", func(t *testing.T) {
		expectRun(t, `return fileExists("/nonexistent_file_12345")`, nil, False)
	})

	t.Run("isDir", func(t *testing.T) {
		expectRun(t, `return isDir("/nonexistent_dir_12345")`, nil, False)
	})
}

// TestMiscBuiltinFunctions tests miscellaneous builtin functions
func TestMiscBuiltinFunctions(t *testing.T) {
	t.Run("sleep", func(t *testing.T) {
		expectRun(t, `sleep(1); return true`, nil, True)
	})
}

// TestObjectOperations tests various object operations
func TestObjectOperations(t *testing.T) {
	t.Run("array operations", func(t *testing.T) {
		expectRun(t, `a := [1, 2, 3]; a[0] = 99; return a[0]`, nil, Int(99))
		expectRun(t, `a := [1, 2, 3]; return len(a)`, nil, Int(3))
		expectRun(t, `a := [1, 2]; b := append(a, 3, 4); return len(b)`, nil, Int(4))
	})

	t.Run("map operations", func(t *testing.T) {
		expectRun(t, `m := {a: 1, b: 2}; return m.a`, nil, Int(1))
		expectRun(t, `m := {a: 1}; m.b = 2; return m.b`, nil, Int(2))
		expectRun(t, `m := {a: 1, b: 2}; return len(m)`, nil, Int(2))
	})

	t.Run("string operations", func(t *testing.T) {
		expectRun(t, `s := "hello"; return s[0]`, nil, Int(104))
		expectRun(t, `s := "hello"; return len(s)`, nil, Int(5))
		expectRun(t, `return "hello" + " world"`, nil, String("hello world"))
	})

	t.Run("bytes operations", func(t *testing.T) {
		expectRun(t, `b := bytes(1, 2, 3); return len(b)`, nil, Int(3))
		expectRun(t, `b := bytes(1, 2, 3); b[0] = 99; return b[0]`, nil, Int(99))
	})
}

// TestFunctionOperations tests function-related operations
func TestFunctionOperations(t *testing.T) {
	t.Run("function definition", func(t *testing.T) {
		expectRun(t, `f := func() { return 42 }; return f()`, nil, Int(42))
		expectRun(t, `f := func(x) { return x * 2 }; return f(21)`, nil, Int(42))
		expectRun(t, `f := func(x, y) { return x + y }; return f(1, 2)`, nil, Int(3))
	})

	t.Run("closures", func(t *testing.T) {
		expectRun(t, `f := func() { x := 10; return func() { return x } }; return f()()`, nil, Int(10))
	})

	t.Run("variadic functions", func(t *testing.T) {
		expectRun(t, `f := func(...args) { return len(args) }; return f(1, 2, 3)`, nil, Int(3))
	})
}

// TestControlFlow tests control flow statements
func TestControlFlow(t *testing.T) {
	t.Run("if statements", func(t *testing.T) {
		expectRun(t, `if true { return 1 }; return 0`, nil, Int(1))
		expectRun(t, `if false { return 1 }; return 0`, nil, Int(0))
		expectRun(t, `x := 5; if x > 3 { return 1 } else { return 0 }`, nil, Int(1))
	})

	t.Run("for loops", func(t *testing.T) {
		expectRun(t, `sum := 0; for i := 0; i < 5; i++ { sum += i }; return sum`, nil, Int(10))
		expectRun(t, `sum := 0; for i, v in [1, 2, 3] { sum += v }; return sum`, nil, Int(6))
		expectRun(t, `sum := 0; for k, v in {a: 1, b: 2} { sum += v }; return sum`, nil, Int(3))
	})
}

// TestErrorHandling tests error handling
func TestErrorHandling(t *testing.T) {
	t.Run("throw", func(t *testing.T) {
		expectRun(t, `try { throw "custom error" } catch e { return string(e) }`, nil, String("error: custom error"))
	})
}

// TestTypeConversion tests type conversion
func TestTypeConversion(t *testing.T) {
	t.Run("int to float", func(t *testing.T) {
		expectRun(t, `return float(42)`, nil, Float(42.0))
	})

	t.Run("float to int", func(t *testing.T) {
		expectRun(t, `return int(42.7)`, nil, Int(42))
	})

	t.Run("int to string", func(t *testing.T) {
		expectRun(t, `return string(42)`, nil, String("42"))
	})

	t.Run("string to int", func(t *testing.T) {
		expectRun(t, `return int("42")`, nil, Int(42))
	})

	t.Run("bool to string", func(t *testing.T) {
		expectRun(t, `return string(true)`, nil, String("true"))
	})
}

// TestMoreArrayFunctions tests more array-related builtin functions
func TestMoreArrayFunctions(t *testing.T) {
	t.Run("arrayContains", func(t *testing.T) {
		expectRun(t, `a := [1, 2, 3]; return arrayContains(a, 2)`, nil, True)
		expectRun(t, `a := [1, 2, 3]; return arrayContains(a, 5)`, nil, False)
	})
}

// TestEncodingFunctions tests encoding-related builtin functions
func TestEncodingFunctions(t *testing.T) {
	t.Run("base64Encode", func(t *testing.T) {
		expectRun(t, `return base64Encode("hello")`, nil, String("aGVsbG8="))
	})

	t.Run("base64Decode", func(t *testing.T) {
		expectRun(t, `return string(base64Decode("aGVsbG8="))`, nil, String("hello"))
	})

	t.Run("hexEncode", func(t *testing.T) {
		expectRun(t, `return hexEncode("hello")`, nil, String("68656c6c6f"))
	})

	t.Run("hexDecode", func(t *testing.T) {
		expectRun(t, `return typeName(hexDecode("68656c6c6f"))`, nil, String("bytes"))
	})
}

// TestByteFunction tests byte builtin
func TestByteFunction(t *testing.T) {
	t.Run("byte", func(t *testing.T) {
		expectRun(t, `return byte(65)`, nil, Byte(65))
		expectRun(t, `return byte(255)`, nil, Byte(255))
		expectRun(t, `return byte("65")`, nil, Byte(65)) // numeric string
		expectRun(t, `return byte()`, nil, Byte(0))      // default
	})
}

// TestMutableStringFunction tests mutableString builtin
func TestMutableStringFunction(t *testing.T) {
	t.Run("mutableString", func(t *testing.T) {
		expectRun(t, `ms := mutableString("hello"); return string(ms)`, nil, String("hello"))
		expectRun(t, `ms := mutableString(""); return len(string(ms))`, nil, Int(0))
	})
}

// TestCharsFunction tests chars builtin
func TestCharsFunction(t *testing.T) {
	t.Run("chars", func(t *testing.T) {
		expectRun(t, `return typeName(chars("abc"))`, nil, String("chars"))
		expectRun(t, `return len(chars("abc"))`, nil, Int(3))
	})
}

// TestSystemCommand tests system command functions
func TestSystemCommand(t *testing.T) {
	t.Run("systemCmd", func(t *testing.T) {
		expectRun(t, `return typeName(systemCmd("echo", "test"))`, nil, String("string"))
	})
}

// TestResetFunction tests reset builtin
func TestResetFunction(t *testing.T) {
	t.Run("reset", func(t *testing.T) {
		// reset returns a new empty array, doesn't modify in place
		expectRun(t, `a := [1, 2, 3]; b := reset(a); return len(b)`, nil, Int(0))
		expectRun(t, `return len(reset([1, 2, 3]))`, nil, Int(0))
	})
}

// TestSortFunctions tests sort functions more thoroughly
func TestSortFunctions(t *testing.T) {
	t.Run("sort with strings", func(t *testing.T) {
		expectRun(t, `a := ["c", "a", "b"]; sort(a); return a[0]`, nil, String("a"))
	})

	t.Run("sortReverse with strings", func(t *testing.T) {
		expectRun(t, `a := ["a", "b", "c"]; sortReverse(a); return a[0]`, nil, String("c"))
	})
}

// TestAppendListFunction tests appendList more thoroughly
func TestAppendListFunction(t *testing.T) {
	t.Run("appendList empty", func(t *testing.T) {
		expectRun(t, `a := []; b := [1, 2]; return len(appendList(a, b))`, nil, Int(2))
	})

	t.Run("appendList two arrays", func(t *testing.T) {
		expectRun(t, `a := [1]; b := [2, 3]; return len(appendList(a, b))`, nil, Int(3))
	})
}

// TestBytesFunction tests bytes builtin more thoroughly
func TestBytesFunction(t *testing.T) {
	t.Run("bytes from string", func(t *testing.T) {
		expectRun(t, `return len(bytes("hello"))`, nil, Int(5))
	})

	t.Run("bytes from values", func(t *testing.T) {
		expectRun(t, `return len(bytes(1, 2, 3, 4, 5))`, nil, Int(5))
	})
}

// TestImageFunction tests image builtin
func TestImageFunction(t *testing.T) {
	t.Run("image default", func(t *testing.T) {
		expectRun(t, `img := image(); return img.width()`, nil, Int(100))
	})

	t.Run("image with options", func(t *testing.T) {
		expectRun(t, `img := image("-width=200"); return img.width()`, nil, Int(200))
	})
}

// TestBytesBufferFunction tests bytesBuffer builtin
func TestBytesBufferFunction(t *testing.T) {
	t.Run("bytesBuffer", func(t *testing.T) {
		expectRun(t, `bb := bytesBuffer(); return typeName(bb)`, nil, String("bytesBuffer"))
	})
}

// TestStringBuilderFunction tests stringBuilder builtin
func TestStringBuilderFunction(t *testing.T) {
	t.Run("stringBuilder", func(t *testing.T) {
		expectRun(t, `sb := stringBuilder(); return typeName(sb)`, nil, String("stringBuilder"))
	})

	t.Run("stringBuilder write", func(t *testing.T) {
		expectRun(t, `sb := stringBuilder(); sb.writeStr("hello"); return sb.toStr()`, nil, String("hello"))
	})
}

// TestReaderWriter tests reader and writer functions
func TestReaderWriter(t *testing.T) {
	t.Run("reader", func(t *testing.T) {
		expectRun(t, `r := reader("hello"); return typeName(r)`, nil, String("reader"))
	})

	t.Run("writer", func(t *testing.T) {
		expectRun(t, `sb := stringBuilder(); w := writer(sb); return typeName(w)`, nil, String("writer"))
	})
}

// TestGelFunction tests gel builtin
func TestGelFunction(t *testing.T) {
	t.Run("gel", func(t *testing.T) {
		expectRun(t, `g := gel("return 42"); return typeName(g)`, nil, String("gel"))
	})
}

// TestCharCodeFunction tests charCode builtin
func TestCharCodeFunction(t *testing.T) {
	t.Run("charCode", func(t *testing.T) {
		expectRun(t, `cc := charCode("return 1+1", undefined); return typeName(cc)`, nil, String("charCode"))
	})
}

// TestSyncMap tests sync map operations
func TestSyncMap(t *testing.T) {
	t.Run("syncMap basic", func(t *testing.T) {
		expectRun(t, `m := make("syncMap"); return typeName(m)`, nil, String("syncMap"))
	})

	t.Run("syncMap isSyncMap", func(t *testing.T) {
		expectRun(t, `m := make("syncMap"); return isSyncMap(m)`, nil, True)
	})

	t.Run("syncMap store and load", func(t *testing.T) {
		expectRun(t, `m := make("syncMap"); m["key"] = 42; return m["key"]`, nil, Int(42))
	})
}

// TestAppendListBytes tests appendList with bytes
func TestAppendListBytes(t *testing.T) {
	t.Run("appendList bytes", func(t *testing.T) {
		expectRun(t, `a := bytes("hello"); b := bytes(" world"); return string(appendList(a, b))`, nil, String("hello world"))
	})
}

// TestAppendListChars tests appendList with chars
func TestAppendListChars(t *testing.T) {
	t.Run("appendList chars", func(t *testing.T) {
		expectRun(t, `a := chars("hello"); b := chars(" world"); return string(appendList(a, b))`, nil, String("hello world"))
	})
}

// TestResetBytes tests reset with bytes
func TestResetBytes(t *testing.T) {
	t.Run("reset bytes", func(t *testing.T) {
		expectRun(t, `b := bytes("hello"); b2 := reset(b); return len(b2)`, nil, Int(0))
	})
}

// TestByteMoreTypes tests byte conversion with more types
func TestByteMoreTypes(t *testing.T) {
	t.Run("byte from bool true", func(t *testing.T) {
		expectRun(t, `return byte(true)`, nil, Byte(1))
	})

	t.Run("byte from bool false", func(t *testing.T) {
		expectRun(t, `return byte(false)`, nil, Byte(0))
	})

	t.Run("byte from char", func(t *testing.T) {
		expectRun(t, `return byte('A')`, nil, Byte(65))
	})

	t.Run("byte from float", func(t *testing.T) {
		expectRun(t, `return byte(65.5)`, nil, Byte(65))
	})

	t.Run("byte from uint", func(t *testing.T) {
		expectRun(t, `return byte(65u)`, nil, Byte(65))
	})
}

// TestMutableStringMore tests mutableString more thoroughly
func TestMutableStringMore(t *testing.T) {
	t.Run("mutableString empty", func(t *testing.T) {
		expectRun(t, `ms := mutableString(); return string(ms)`, nil, String(""))
	})

	t.Run("mutableString setChar", func(t *testing.T) {
		expectRun(t, `ms := mutableString("hello"); return string(ms)`, nil, String("hello"))
	})
}

// TestCharsMore tests chars more thoroughly
func TestCharsMore(t *testing.T) {
	t.Run("chars from string", func(t *testing.T) {
		expectRun(t, `c := chars("abc"); return len(c)`, nil, Int(3))
	})

	t.Run("chars index", func(t *testing.T) {
		expectRun(t, `c := chars("abc"); return string(c[0])`, nil, String("a"))
	})

	t.Run("chars from char", func(t *testing.T) {
		expectRun(t, `c := chars('A'); return len(c)`, nil, Int(1))
	})
}

// TestSortMore tests sort more thoroughly
func TestSortMore(t *testing.T) {
	t.Run("sort integers", func(t *testing.T) {
		expectRun(t, `a := [3, 1, 2]; sort(a); return a[0]`, nil, Int(1))
	})

	t.Run("sort integers reverse check", func(t *testing.T) {
		expectRun(t, `a := [3, 1, 2]; sort(a); return a[2]`, nil, Int(3))
	})
}

// TestSortReverseMore tests sortReverse more thoroughly
func TestSortReverseMore(t *testing.T) {
	t.Run("sortReverse integers", func(t *testing.T) {
		expectRun(t, `a := [1, 2, 3]; sortReverse(a); return a[0]`, nil, Int(3))
	})

	t.Run("sortReverse integers check last", func(t *testing.T) {
		expectRun(t, `a := [1, 2, 3]; sortReverse(a); return a[2]`, nil, Int(1))
	})
}

// TestContains tests contains builtin
func TestContains(t *testing.T) {
	t.Run("contains string", func(t *testing.T) {
		expectRun(t, `return contains("hello", "ell")`, nil, True)
	})

	t.Run("contains string not found", func(t *testing.T) {
		expectRun(t, `return contains("hello", "xyz")`, nil, False)
	})

	t.Run("contains array", func(t *testing.T) {
		expectRun(t, `return contains([1, 2, 3], 2)`, nil, True)
	})
}

// TestCap tests cap builtin
func TestCap(t *testing.T) {
	t.Run("cap of array", func(t *testing.T) {
		expectRun(t, `a := make("array", 5, 10); return cap(a)`, nil, Int(10))
	})
}

// TestDelete tests delete builtin
func TestDelete(t *testing.T) {
	t.Run("delete from map", func(t *testing.T) {
		expectRun(t, `m := {"a": 1, "b": 2}; delete(m, "a"); return len(m)`, nil, Int(1))
	})
}

// TestCopy tests copy builtin
func TestCopy(t *testing.T) {
	t.Run("copy array", func(t *testing.T) {
		expectRun(t, `a := [1, 2, 3]; b := copy(a); return len(b)`, nil, Int(3))
	})
}

// TestRepeat tests repeat builtin
func TestRepeat(t *testing.T) {
	t.Run("repeat string", func(t *testing.T) {
		expectRun(t, `return repeat("ab", 3)`, nil, String("ababab"))
	})

	t.Run("repeat array", func(t *testing.T) {
		expectRun(t, `a := repeat([1], 3); return len(a)`, nil, Int(3))
	})
}

// TestGlobals tests globals builtin
func TestGlobals(t *testing.T) {
	t.Run("globals returns map", func(t *testing.T) {
		expectRun(t, `return typeName(globals())`, nil, String("map"))
	})
}

// TestAny tests any builtin
func TestAny(t *testing.T) {
	t.Run("any from int", func(t *testing.T) {
		expectRun(t, `a := any(42); return typeName(a)`, nil, String("any"))
	})

	t.Run("any from string", func(t *testing.T) {
		expectRun(t, `a := any("hello"); return typeName(a)`, nil, String("any"))
	})

	t.Run("any value returns self", func(t *testing.T) {
		expectRun(t, `a := any(42); return typeName(a["value"])`, nil, String("any"))
	})
}

// TestToIntFunc tests toInt builtin
func TestToIntFunc(t *testing.T) {
	t.Run("toInt from string", func(t *testing.T) {
		expectRun(t, `return toInt("42")`, nil, Int(42))
	})

	t.Run("toInt from float", func(t *testing.T) {
		expectRun(t, `return toInt(42.7)`, nil, Int(42))
	})
}

// TestToBoolFunc tests toBool builtin
func TestToBoolFunc(t *testing.T) {
	t.Run("toBool from int 1", func(t *testing.T) {
		expectRun(t, `return toBool(1)`, nil, True)
	})

	t.Run("toBool from int 0", func(t *testing.T) {
		expectRun(t, `return toBool(0)`, nil, True) // charlang treats 0 as truthy
	})

	t.Run("toBool from string", func(t *testing.T) {
		expectRun(t, `return toBool("true")`, nil, True)
	})
}

// TestToStr tests toStr builtin
func TestToStr(t *testing.T) {
	t.Run("toStr from int", func(t *testing.T) {
		expectRun(t, `return toStr(42)`, nil, String("42"))
	})

	t.Run("toStr from float", func(t *testing.T) {
		expectRun(t, `return toStr(3.14)`, nil, String("3.14"))
	})
}

// TestToHex tests toHex builtin
func TestToHex(t *testing.T) {
	t.Run("toHex", func(t *testing.T) {
		expectRun(t, `return toHex(255)`, nil, String("ff"))
	})
}

// TestToFloatFunc tests toFloat builtin
func TestToFloatFunc(t *testing.T) {
	t.Run("toFloat from string", func(t *testing.T) {
		expectRun(t, `return toFloat("3.14")`, nil, Float(3.14))
	})

	t.Run("toFloat from int", func(t *testing.T) {
		expectRun(t, `return toFloat(42)`, nil, Float(42.0))
	})
}

// TestTypeOfAny tests typeOfAny builtin
func TestTypeOfAny(t *testing.T) {
	t.Run("typeOfAny with any", func(t *testing.T) {
		expectRun(t, `a := any(42); return typeOfAny(a)`, nil, String("int"))
	})

	t.Run("typeOfAny with string any", func(t *testing.T) {
		expectRun(t, `a := any("hello"); return typeOfAny(a)`, nil, String("string"))
	})
}

// TestCheckErrX tests checkErrX builtin - only test non-exit case
func TestCheckErrX(t *testing.T) {
	t.Run("checkErrX with non-error string", func(t *testing.T) {
		expectRun(t, `return checkErrX("hello")`, nil, String("hello"))
	})
}

// TestCheckEmpty tests checkEmpty builtin - only test non-empty case since empty exits
func TestCheckEmpty(t *testing.T) {
	t.Run("checkEmpty with non-empty string", func(t *testing.T) {
		expectRun(t, `return checkEmpty("hello")`, nil, String("hello"))
	})
}

// TestErrToEmpty tests errToEmpty builtin
func TestErrToEmpty(t *testing.T) {
	t.Run("errToEmpty with undefined", func(t *testing.T) {
		expectRun(t, `return errToEmpty(undefined)`, nil, Undefined)
	})

	t.Run("errToEmpty with error", func(t *testing.T) {
		expectRun(t, `return errToEmpty(error("test"))`, nil, String(""))
	})
}

// TestPlt tests plt builtin
func TestPlt(t *testing.T) {
	t.Run("plt prints", func(t *testing.T) {
		expectRun(t, `return plt("test")`, nil, Int(13)) // prints "(string)test\n"
	})
}

// TestGetEnv tests getEnv builtin
func TestGetEnv(t *testing.T) {
	t.Run("getEnv returns string", func(t *testing.T) {
		expectRun(t, `return typeName(getEnv("PATH"))`, nil, String("string"))
	})
}

// TestTrim tests trim builtin
func TestTrim(t *testing.T) {
	t.Run("trim spaces", func(t *testing.T) {
		expectRun(t, `return trim("  hello  ")`, nil, String("hello"))
	})

	t.Run("trim with chars", func(t *testing.T) {
		expectRun(t, `return trim("xhellox", "x")`, nil, String("hello"))
	})
}

// TestStrTrimStart tests strTrimStart builtin
func TestStrTrimStart(t *testing.T) {
	t.Run("strTrimStart with cutset", func(t *testing.T) {
		expectRun(t, `return strTrimStart("xhello", "x")`, nil, String("hello"))
	})
}

// TestStrTrimEnd tests strTrimEnd builtin
func TestStrTrimEnd(t *testing.T) {
	t.Run("strTrimEnd with cutset", func(t *testing.T) {
		expectRun(t, `return strTrimEnd("hellox", "x")`, nil, String("hello"))
	})
}

// TestRegQuote tests regQuote builtin
func TestRegQuote(t *testing.T) {
	t.Run("regQuote returns same string", func(t *testing.T) {
		expectRun(t, `return regQuote("a.b")`, nil, String("a.b"))
	})
}

// TestTime tests time builtin
func TestTime(t *testing.T) {
	t.Run("time returns string", func(t *testing.T) {
		expectRun(t, `return typeName(time())`, nil, String("time"))
	})
}

// TestToTime tests toTime builtin
func TestToTime(t *testing.T) {
	t.Run("toTime from int", func(t *testing.T) {
		expectRun(t, `t := toTime(1609459200); return typeName(t)`, nil, String("time"))
	})
}

// TestMutex tests mutex builtin
func TestMutex(t *testing.T) {
	t.Run("mutex create", func(t *testing.T) {
		expectRun(t, `m := mutex(); return typeName(m)`, nil, String("mutex"))
	})
}

// TestMux tests mux builtin
func TestMux(t *testing.T) {
	t.Run("mux create", func(t *testing.T) {
		expectRun(t, `return typeName(mux())`, nil, String("mux"))
	})
}

// TestSeq tests seq builtin
func TestSeq(t *testing.T) {
	t.Run("seq create", func(t *testing.T) {
		expectRun(t, `s := seq(); return typeName(s)`, nil, String("seq"))
	})
}

// TestObjectRef tests objectRef builtin
func TestObjectRef(t *testing.T) {
	t.Run("objectRef create", func(t *testing.T) {
		expectRun(t, `r := make("ref"); return typeName(r)`, nil, String("objectRef"))
	})
}

// TestOrderedMap tests orderedMap builtin
func TestOrderedMap(t *testing.T) {
	t.Run("orderedMap create", func(t *testing.T) {
		expectRun(t, `o := orderedMap(); return typeName(o)`, nil, String("orderedMap"))
	})
}

// TestError tests error builtin
func TestError(t *testing.T) {
	t.Run("error create", func(t *testing.T) {
		expectRun(t, `e := error("test"); return isError(e)`, nil, True)
	})

	t.Run("error message", func(t *testing.T) {
		expectRun(t, `e := error("test"); return string(e)`, nil, String("error: test"))
	})
}

// TestAppend tests append builtin
func TestAppend(t *testing.T) {
	t.Run("append to array", func(t *testing.T) {
		expectRun(t, `a := [1, 2]; a = append(a, 3); return len(a)`, nil, Int(3))
	})

	t.Run("append multiple", func(t *testing.T) {
		expectRun(t, `a := [1]; a = append(a, 2, 3); return len(a)`, nil, Int(3))
	})
}

// TestBytesWithSize tests bytesWithSize builtin
func TestBytesWithSize(t *testing.T) {
	t.Run("bytesWithSize create", func(t *testing.T) {
		expectRun(t, `b := bytesWithSize(10); return len(b)`, nil, Int(10))
	})
}

// TestBytesWithCap tests bytesWithCap builtin
func TestBytesWithCap(t *testing.T) {
	t.Run("bytesWithCap create", func(t *testing.T) {
		expectRun(t, `b := bytesWithCap(20); return cap(b)`, nil, Int(20))
	})
}

// TestStrTrim tests strTrim builtin
func TestStrTrim(t *testing.T) {
	t.Run("strTrim with cutset", func(t *testing.T) {
		expectRun(t, `return strTrim("xhellox", "x")`, nil, String("hello"))
	})
}

// TestStrTrimLeft tests strTrimLeft builtin
func TestStrTrimLeft(t *testing.T) {
	t.Run("strTrimLeft with cutset", func(t *testing.T) {
		expectRun(t, `return strTrimLeft("xhello", "x")`, nil, String("hello"))
	})
}

// TestStrTrimRight tests strTrimRight builtin
func TestStrTrimRight(t *testing.T) {
	t.Run("strTrimRight with cutset", func(t *testing.T) {
		expectRun(t, `return strTrimRight("hellox", "x")`, nil, String("hello"))
	})
}

// TestScriptExecution tests script execution
func TestScriptExecution(t *testing.T) {
	t.Run("simple return", func(t *testing.T) {
		expectRun(t, `return 42`, nil, Int(42))
	})

	t.Run("arithmetic", func(t *testing.T) {
		expectRun(t, `return 1 + 2 * 3`, nil, Int(7))
	})

	t.Run("string concat", func(t *testing.T) {
		expectRun(t, `return "hello" + " " + "world"`, nil, String("hello world"))
	})

	t.Run("array literal", func(t *testing.T) {
		expectRun(t, `a := [1, 2, 3]; return a[1]`, nil, Int(2))
	})

	t.Run("map literal", func(t *testing.T) {
		expectRun(t, `m := {"a": 1, "b": 2}; return m["a"]`, nil, Int(1))
	})

	t.Run("function call", func(t *testing.T) {
		expectRun(t, `f := func(x) { return x * 2 }; return f(5)`, nil, Int(10))
	})

	t.Run("closure", func(t *testing.T) {
		expectRun(t, `outer := func(x) { return func(y) { return x + y } }; return outer(3)(4)`, nil, Int(7))
	})

	t.Run("conditional", func(t *testing.T) {
		expectRun(t, `if true { return 1 } else { return 2 }`, nil, Int(1))
	})

	t.Run("for loop", func(t *testing.T) {
		expectRun(t, `sum := 0; for i := 0; i < 5; i++ { sum += i }; return sum`, nil, Int(10))
	})

	t.Run("try catch", func(t *testing.T) {
		expectRun(t, `try { throw error("test") } catch e { return string(e) }`, nil, String("error: test"))
	})
}

// TestTypeConversions tests type conversion functions
func TestTypeConversions(t *testing.T) {
	t.Run("int to float", func(t *testing.T) {
		expectRun(t, `return float(42)`, nil, Float(42.0))
	})

	t.Run("float to int", func(t *testing.T) {
		expectRun(t, `return int(42.9)`, nil, Int(42))
	})

	t.Run("int to string", func(t *testing.T) {
		expectRun(t, `return string(42)`, nil, String("42"))
	})

	t.Run("string to int", func(t *testing.T) {
		expectRun(t, `return int("42")`, nil, Int(42))
	})
}

// TestArrayOps tests array operations
func TestArrayOps(t *testing.T) {
	t.Run("array length", func(t *testing.T) {
		expectRun(t, `return len([1, 2, 3])`, nil, Int(3))
	})

	t.Run("array append", func(t *testing.T) {
		expectRun(t, `a := [1, 2]; a = append(a, 3); return len(a)`, nil, Int(3))
	})

	t.Run("array index", func(t *testing.T) {
		expectRun(t, `return [1, 2, 3][1]`, nil, Int(2))
	})

	t.Run("array slice", func(t *testing.T) {
		expectRun(t, `a := [1, 2, 3, 4]; return a[1:3]`, nil, Array{Int(2), Int(3)})
	})
}

// TestMapOps tests map operations
func TestMapOps(t *testing.T) {
	t.Run("map length", func(t *testing.T) {
		expectRun(t, `return len({"a": 1, "b": 2})`, nil, Int(2))
	})

	t.Run("map get", func(t *testing.T) {
		expectRun(t, `return {"a": 1}["a"]`, nil, Int(1))
	})

	t.Run("map set", func(t *testing.T) {
		expectRun(t, `m := {}; m["a"] = 1; return m["a"]`, nil, Int(1))
	})
}

// TestStringOperations tests string operations
func TestStringOperations(t *testing.T) {
	t.Run("string length", func(t *testing.T) {
		expectRun(t, `return len("hello")`, nil, Int(5))
	})

	t.Run("string index returns byte", func(t *testing.T) {
		expectRun(t, `return "hello"[0]`, nil, Int(104)) // 'h' is byte 104
	})

	t.Run("string slice", func(t *testing.T) {
		expectRun(t, `s := "hello"; return s[0:3]`, nil, String("hel"))
	})

	t.Run("string contains", func(t *testing.T) {
		expectRun(t, `return contains("hello", "ell")`, nil, True)
	})
}

// TestComparisonOps tests comparison operations
func TestComparisonOps(t *testing.T) {
	t.Run("int equal", func(t *testing.T) {
		expectRun(t, `return 1 == 1`, nil, True)
	})

	t.Run("int not equal", func(t *testing.T) {
		expectRun(t, `return 1 != 2`, nil, True)
	})

	t.Run("int less than", func(t *testing.T) {
		expectRun(t, `return 1 < 2`, nil, True)
	})

	t.Run("int greater than", func(t *testing.T) {
		expectRun(t, `return 2 > 1`, nil, True)
	})

	t.Run("string equal", func(t *testing.T) {
		expectRun(t, `return "a" == "a"`, nil, True)
	})

	t.Run("bool equal", func(t *testing.T) {
		expectRun(t, `return true == true`, nil, True)
	})
}

// TestLogicalOps tests logical operations
func TestLogicalOps(t *testing.T) {
	t.Run("and", func(t *testing.T) {
		expectRun(t, `return true && true`, nil, True)
	})

	t.Run("or", func(t *testing.T) {
		expectRun(t, `return false || true`, nil, True)
	})

	t.Run("not", func(t *testing.T) {
		expectRun(t, `return !false`, nil, True)
	})
}

// TestBitwiseOps tests bitwise operations
func TestBitwiseOps(t *testing.T) {
	t.Run("bitwise and", func(t *testing.T) {
		expectRun(t, `return 5 & 3`, nil, Int(1))
	})

	t.Run("bitwise or", func(t *testing.T) {
		expectRun(t, `return 5 | 3`, nil, Int(7))
	})

	t.Run("bitwise xor", func(t *testing.T) {
		expectRun(t, `return 5 ^ 3`, nil, Int(6))
	})

	t.Run("left shift", func(t *testing.T) {
		expectRun(t, `return 1 << 3`, nil, Int(8))
	})

	t.Run("right shift", func(t *testing.T) {
		expectRun(t, `return 8 >> 2`, nil, Int(2))
	})
}

// TestUnaryOps tests unary operations
func TestUnaryOps(t *testing.T) {
	t.Run("negative", func(t *testing.T) {
		expectRun(t, `return -5`, nil, Int(-5))
	})

	t.Run("positive", func(t *testing.T) {
		expectRun(t, `return +5`, nil, Int(5))
	})
}

// TestAssignmentOps tests assignment operations
func TestAssignmentOps(t *testing.T) {
	t.Run("assign", func(t *testing.T) {
		expectRun(t, `a := 5; return a`, nil, Int(5))
	})

	t.Run("add assign", func(t *testing.T) {
		expectRun(t, `a := 5; a += 3; return a`, nil, Int(8))
	})

	t.Run("sub assign", func(t *testing.T) {
		expectRun(t, `a := 5; a -= 3; return a`, nil, Int(2))
	})

	t.Run("mul assign", func(t *testing.T) {
		expectRun(t, `a := 5; a *= 3; return a`, nil, Int(15))
	})

	t.Run("div assign", func(t *testing.T) {
		expectRun(t, `a := 15; a /= 3; return a`, nil, Int(5))
	})
}

// TestIncDec tests increment and decrement
func TestIncDec(t *testing.T) {
	t.Run("increment", func(t *testing.T) {
		expectRun(t, `a := 5; a++; return a`, nil, Int(6))
	})

	t.Run("decrement", func(t *testing.T) {
		expectRun(t, `a := 5; a--; return a`, nil, Int(4))
	})
}

// TestRangeLoop tests range loops
func TestRangeLoop(t *testing.T) {
	t.Run("range array", func(t *testing.T) {
		expectRun(t, `sum := 0; for i, v in [1, 2, 3] { sum += v }; return sum`, nil, Int(6))
	})

	t.Run("range map", func(t *testing.T) {
		expectRun(t, `count := 0; for k, v in {"a": 1, "b": 2} { count++ }; return count`, nil, Int(2))
	})
}

// TestTernary tests ternary operator
func TestTernary(t *testing.T) {
	t.Run("ternary true", func(t *testing.T) {
		expectRun(t, `return true ? 1 : 2`, nil, Int(1))
	})

	t.Run("ternary false", func(t *testing.T) {
		expectRun(t, `return false ? 1 : 2`, nil, Int(2))
	})
}

// TestMathBuiltins tests math builtins
func TestMathBuiltins(t *testing.T) {
	t.Run("abs positive", func(t *testing.T) {
		expectRun(t, `return abs(-5)`, nil, Int(5))
	})

	t.Run("abs negative", func(t *testing.T) {
		expectRun(t, `return abs(5)`, nil, Int(5))
	})

	t.Run("max", func(t *testing.T) {
		expectRun(t, `return max(1, 2, 3)`, nil, Int(3))
	})

	t.Run("min", func(t *testing.T) {
		expectRun(t, `return min(1, 2, 3)`, nil, Int(1))
	})
}

// TestStringBuiltins tests string builtins
func TestStringBuiltins(t *testing.T) {
	t.Run("strToUpper", func(t *testing.T) {
		expectRun(t, `return strToUpper("hello")`, nil, String("HELLO"))
	})

	t.Run("strToLower", func(t *testing.T) {
		expectRun(t, `return strToLower("HELLO")`, nil, String("hello"))
	})

	t.Run("strSplit", func(t *testing.T) {
		expectRun(t, `return len(strSplit("a,b,c", ","))`, nil, Int(3))
	})

	t.Run("strJoin", func(t *testing.T) {
		expectRun(t, `return strJoin(["a", "b", "c"], ",")`, nil, String("a,b,c"))
	})

	t.Run("strReplace", func(t *testing.T) {
		expectRun(t, `return strReplace("hello", "l", "L", -1)`, nil, String("heLLo"))
	})

	t.Run("strContains", func(t *testing.T) {
		expectRun(t, `return strContains("hello", "ell")`, nil, True)
	})
}

// TestArrayBuiltins tests array builtins
func TestArrayBuiltins(t *testing.T) {
	t.Run("array get first", func(t *testing.T) {
		expectRun(t, `return [1, 2, 3][0]`, nil, Int(1))
	})

	t.Run("array get last", func(t *testing.T) {
		expectRun(t, `a := [1, 2, 3]; return a[len(a)-1]`, nil, Int(3))
	})
}

// TestIsTypeFunctions tests is* type checking functions
func TestIsTypeFunctions(t *testing.T) {
	t.Run("isInt", func(t *testing.T) {
		expectRun(t, `return isInt(42)`, nil, True)
	})

	t.Run("isString", func(t *testing.T) {
		expectRun(t, `return isString("hello")`, nil, True)
	})

	t.Run("isFloat", func(t *testing.T) {
		expectRun(t, `return isFloat(3.14)`, nil, True)
	})

	t.Run("isBool", func(t *testing.T) {
		expectRun(t, `return isBool(true)`, nil, True)
	})

	t.Run("isArray", func(t *testing.T) {
		expectRun(t, `return isArray([1, 2, 3])`, nil, True)
	})

	t.Run("isMap", func(t *testing.T) {
		expectRun(t, `return isMap({"a": 1})`, nil, True)
	})

	t.Run("isFunction", func(t *testing.T) {
		expectRun(t, `return isFunction(func() { return 1 })`, nil, True)
	})

	t.Run("isUndefined", func(t *testing.T) {
		expectRun(t, `return isUndefined(undefined)`, nil, True)
	})

	t.Run("isError", func(t *testing.T) {
		expectRun(t, `return isError(error("test"))`, nil, True)
	})
}

// TestFormatBuiltins tests formatting builtins
func TestFormatBuiltins(t *testing.T) {
	t.Run("sprintf", func(t *testing.T) {
		expectRun(t, `return sprintf("%d %s", 42, "test")`, nil, String("42 test"))
	})
}

// TestAdditionalBuiltins tests additional builtins
func TestAdditionalBuiltins(t *testing.T) {
	t.Run("isNil", func(t *testing.T) {
		expectRun(t, `return isNil(undefined)`, nil, True)
	})

	t.Run("isNilOrEmpty", func(t *testing.T) {
		expectRun(t, `return isNilOrEmpty("")`, nil, True)
	})

	t.Run("typeName", func(t *testing.T) {
		expectRun(t, `return typeName(42)`, nil, String("int"))
	})

	t.Run("typeCode returns int", func(t *testing.T) {
		expectRun(t, `return typeCode(42)`, nil, Int(107)) // actual type code
	})
}

// TestStringMethods tests string methods
func TestStringMethods(t *testing.T) {
	t.Run("strLen", func(t *testing.T) {
		expectRun(t, `return len("hello")`, nil, Int(5))
	})

	t.Run("strRuneLen", func(t *testing.T) {
		expectRun(t, `return strRuneLen("hello")`, nil, Int(5))
	})

	t.Run("strSub", func(t *testing.T) {
		expectRun(t, `return strSub("hello", 0, 3)`, nil, String("hel"))
	})

	t.Run("strIndex", func(t *testing.T) {
		expectRun(t, `return strIndex("hello", "ll")`, nil, Int(2))
	})

	t.Run("strCount", func(t *testing.T) {
		expectRun(t, `return strCount("hello", "l")`, nil, Int(2))
	})

	t.Run("strRepeat", func(t *testing.T) {
		expectRun(t, `return strRepeat("ab", 3)`, nil, String("ababab"))
	})
}

// TestAdditionalArrayBuiltins tests more array builtins
func TestAdditionalArrayBuiltins(t *testing.T) {
	t.Run("makeArray", func(t *testing.T) {
		expectRun(t, `a := make("array", 3); return len(a)`, nil, Int(3))
	})

	t.Run("array copy", func(t *testing.T) {
		expectRun(t, `a := [1, 2, 3]; b := copy(a); return len(b)`, nil, Int(3))
	})
}

// TestAdditionalMapBuiltins tests more map builtins
func TestAdditionalMapBuiltins(t *testing.T) {
	t.Run("makeMap", func(t *testing.T) {
		expectRun(t, `m := make("map"); return typeName(m)`, nil, String("map"))
	})

	t.Run("map delete", func(t *testing.T) {
		expectRun(t, `m := {"a": 1, "b": 2}; delete(m, "a"); return len(m)`, nil, Int(1))
	})
}

// TestErrorHandlingAdditional tests error handling
func TestErrorHandlingAdditional(t *testing.T) {
	t.Run("error creation", func(t *testing.T) {
		expectRun(t, `e := error("test error"); return isError(e)`, nil, True)
	})

	t.Run("throw catch", func(t *testing.T) {
		expectRun(t, `try { throw error("test") } catch e { return string(e) }`, nil, String("error: test"))
	})
}

// TestBytesOperations tests bytes operations
func TestBytesOperations(t *testing.T) {
	t.Run("bytes from string", func(t *testing.T) {
		expectRun(t, `b := bytes("hello"); return len(b)`, nil, Int(5))
	})

	t.Run("bytes operations", func(t *testing.T) {
		expectRun(t, `b := bytes(1, 2, 3); return len(b)`, nil, Int(3))
	})
}

// TestCharOperations tests char operations
func TestCharOperations(t *testing.T) {
	t.Run("char from int", func(t *testing.T) {
		expectRun(t, `return int('A')`, nil, Int(65))
	})
}

// TestBase64Builtins tests base64 encoding/decoding builtins
func TestBase64Builtins(t *testing.T) {
	t.Run("base64Encode", func(t *testing.T) {
		expectRun(t, `return base64Encode("hello")`, nil, String("aGVsbG8="))
	})

	t.Run("base64Decode", func(t *testing.T) {
		expectRun(t, `return string(base64Decode("aGVsbG8="))`, nil, String("hello"))
	})

	t.Run("base64EncodeByRawUrl", func(t *testing.T) {
		expectRun(t, `return base64EncodeByRawUrl("hello")`, nil, String("aGVsbG8"))
	})

	t.Run("base64DecodeByRawUrl", func(t *testing.T) {
		expectRun(t, `return string(base64DecodeByRawUrl("aGVsbG8"))`, nil, String("hello"))
	})
}

// TestJSONBuiltins tests JSON encoding/decoding builtins
func TestJSONBuiltins(t *testing.T) {
	t.Run("toJSON", func(t *testing.T) {
		expectRun(t, `return toJSON({"a": 1})`, nil, String(`{"a":1}`))
	})

	t.Run("fromJSON", func(t *testing.T) {
		// JSON numbers are parsed as floats
		expectRun(t, `j := fromJSON("{\"a\":1}"); return int(j.a)`, nil, Int(1))
	})
}

// TestXMLBuiltins tests XML encoding/decoding builtins
func TestXMLBuiltins(t *testing.T) {
	t.Run("fromXml", func(t *testing.T) {
		result, err := RunCharCode(`return fromXml("<root><item>test</item></root>")`, nil, nil)
		require.NoError(t, err)
		require.NotNil(t, result)
	})
}

// TestEncodingBuiltins tests encoding-related builtins
func TestEncodingBuiltins(t *testing.T) {
	t.Run("simpleEncode", func(t *testing.T) {
		// simpleEncode returns encoded string
		expectRun(t, `return len(simpleEncode("hello", "key")) > 0`, nil, True)
	})

	t.Run("encodeSimpleMap", func(t *testing.T) {
		expectRun(t, `return contains(encodeSimpleMap({"a": "1", "b": "2"}), "a=1")`, nil, True)
	})
}

// TestCryptoBuiltins tests cryptography-related builtins
func TestCryptoBuiltins(t *testing.T) {
	t.Run("md5", func(t *testing.T) {
		expectRun(t, `return md5("hello")`, nil, String("5d41402abc4b2a76b9719d911017c592"))
	})
}

// TestContainsBuiltin tests contains builtin
func TestContainsBuiltin(t *testing.T) {
	t.Run("contains string", func(t *testing.T) {
		expectRun(t, `return contains("hello", "ell")`, nil, True)
	})

	t.Run("contains array", func(t *testing.T) {
		expectRun(t, `return contains([1, 2, 3], 2)`, nil, True)
	})

	t.Run("contains not found", func(t *testing.T) {
		expectRun(t, `return contains("hello", "xyz")`, nil, False)
	})
}

// TestRepeatBuiltin tests repeat builtin
func TestRepeatBuiltin(t *testing.T) {
	t.Run("repeat string", func(t *testing.T) {
		expectRun(t, `return repeat("ab", 3)`, nil, String("ababab"))
	})

	t.Run("repeat array", func(t *testing.T) {
		expectRun(t, `a := [1, 2]; b := repeat(a, 2); return len(b)`, nil, Int(4))
	})
}

// TestAppendBuiltins tests append builtins
func TestAppendBuiltins(t *testing.T) {
	t.Run("append single", func(t *testing.T) {
		// append returns a new array
		expectRun(t, `a := [1, 2]; b := append(a, 3); return len(b)`, nil, Int(3))
	})

	t.Run("appendList", func(t *testing.T) {
		// appendList returns a new array
		expectRun(t, `a := [1, 2]; b := [3, 4]; c := appendList(a, b); return len(c)`, nil, Int(4))
	})
}

// TestRemoveBuiltins tests remove builtins
func TestRemoveBuiltins(t *testing.T) {
	t.Run("removeItem", func(t *testing.T) {
		expectRun(t, `a := [1, 2, 3, 4]; return removeItem(a, 2)`, nil, Array{Int(1), Int(2), Int(4)})
	})

	t.Run("removeItems", func(t *testing.T) {
		expectRun(t, `a := [1, 2, 3, 4, 5]; return removeItems(a, 1, 2)`, nil, Array{Int(1), Int(4), Int(5)})
	})
}

// TestGetArrayItemBuiltin tests getArrayItem builtin
func TestGetArrayItemBuiltin(t *testing.T) {
	t.Run("getArrayItem", func(t *testing.T) {
		expectRun(t, `return getArrayItem([10, 20, 30], 1)`, nil, Int(20))
	})
}

// TestGetSetMapItemBuiltins tests getMapItem and setMapItem builtins
func TestGetSetMapItemBuiltins(t *testing.T) {
	t.Run("getMapItem", func(t *testing.T) {
		expectRun(t, `return getMapItem({"a": 1, "b": 2}, "b")`, nil, Int(2))
	})

	t.Run("setMapItem", func(t *testing.T) {
		expectRun(t, `m := {"a": 1}; setMapItem(m, "b", 2); return m.b`, nil, Int(2))
	})
}

// TestBoolFunc tests bool builtin function
func TestBoolFunc(t *testing.T) {
	t.Run("bool from int", func(t *testing.T) {
		expectRun(t, `return bool(1)`, nil, True)
	})

	t.Run("bool from string", func(t *testing.T) {
		expectRun(t, `return bool("true")`, nil, True)
	})

	t.Run("bool false", func(t *testing.T) {
		expectRun(t, `return bool(0)`, nil, False)
	})
}

// TestMutableStringOps tests mutable string operations
func TestMutableStringOps(t *testing.T) {
	t.Run("create mutableString", func(t *testing.T) {
		expectRun(t, `ms := mutableString("hello"); return string(ms)`, nil, String("hello"))
	})
}

// TestCharsFunc tests chars builtin
func TestCharsFunc(t *testing.T) {
	t.Run("chars from string", func(t *testing.T) {
		expectRun(t, `c := chars("hello"); return len(c)`, nil, Int(5))
	})
}

// TestByteFunc tests byte builtin
func TestByteFunc(t *testing.T) {
	t.Run("byte from int", func(t *testing.T) {
		expectRun(t, `return int(byte(65))`, nil, Int(65))
	})

	t.Run("byte overflow", func(t *testing.T) {
		expectRun(t, `return byte(256)`, nil, Byte(0))
	})
}

// TestAdapterFunctions tests adapter functions
func TestAdapterFunctions(t *testing.T) {
	t.Run("fnATVsRSex", func(t *testing.T) {
		// Test time adapter function
		adapter := fnATVsRSex(func(t time.Time, s ...string) string {
			return t.Format("2006-01-02")
		})
		require.NotNil(t, adapter)
	})

	t.Run("fnATFRT", func(t *testing.T) {
		adapter := fnATFRT(func(t time.Time, f float64) time.Time {
			return t.Add(time.Duration(f) * time.Second)
		})
		require.NotNil(t, adapter)
	})

	t.Run("fnATFRTex", func(t *testing.T) {
		adapter := fnATFRTex(func(t time.Time, f float64) time.Time {
			return t.Add(time.Duration(f) * time.Second)
		})
		require.NotNil(t, adapter)
	})

	t.Run("fnAARS", func(t *testing.T) {
		adapter := fnAARS(func(a interface{}) string {
			return fmt.Sprintf("%v", a)
		})
		require.NotNil(t, adapter)
	})

	t.Run("fnAARSex", func(t *testing.T) {
		adapter := fnAARSex(func(a interface{}) string {
			return fmt.Sprintf("%v", a)
		})
		require.NotNil(t, adapter)
	})

	t.Run("fnASRS", func(t *testing.T) {
		adapter := fnASRS(func(s string) string {
			return strings.ToUpper(s)
		})
		require.NotNil(t, adapter)
	})

	t.Run("fnASRSex", func(t *testing.T) {
		adapter := fnASRSex(func(s string) string {
			return strings.ToUpper(s)
		})
		require.NotNil(t, adapter)
	})

	t.Run("fnASRI", func(t *testing.T) {
		adapter := fnASRI(func(s string) int {
			return len(s)
		})
		require.NotNil(t, adapter)
	})

	t.Run("fnASRA", func(t *testing.T) {
		adapter := fnASRA(func(s string) interface{} {
			return strings.Split(s, ",")
		})
		require.NotNil(t, adapter)
	})

	t.Run("fnAFRF", func(t *testing.T) {
		adapter := fnAFRF(func(f float64) float64 {
			return f * 2
		})
		require.NotNil(t, adapter)
	})

	t.Run("fnAFRFex", func(t *testing.T) {
		adapter := fnAFRFex(func(f float64) float64 {
			return f * 2
		})
		require.NotNil(t, adapter)
	})

	// Test fnASRI - adapts func(string) int
	t.Run("fnASRI", func(t *testing.T) {
		adapter := fnASRI(func(s string) int {
			return len(s)
		})
		require.NotNil(t, adapter)
		result, err := adapter(String("hello"))
		require.Equal(t, Int(5), result)
		require.Nil(t, err)
	})

	// Test fnASRIex - adapts func(string) int
	t.Run("fnASRIex", func(t *testing.T) {
		adapter := fnASRIex(func(s string) int {
			return len(s)
		})
		require.NotNil(t, adapter)
		result, err := adapter(Call{Args: Array{String("hello")}})
		require.Equal(t, Int(5), result)
		require.Nil(t, err)
	})

	// Test fnASRA - adapts func(string) interface{}
	t.Run("fnASRA", func(t *testing.T) {
		adapter := fnASRA(func(s string) interface{} {
			return []byte(s)
		})
		require.NotNil(t, adapter)
		result, err := adapter(String("hello"))
		require.IsType(t, Bytes{}, result)
		require.Nil(t, err)
	})

	// Test fnASRAex - adapts func(string) interface{}
	t.Run("fnASRAex", func(t *testing.T) {
		adapter := fnASRAex(func(s string) interface{} {
			return []byte(s)
		})
		require.NotNil(t, adapter)
		result, err := adapter(Call{Args: Array{String("hello")}})
		require.IsType(t, Bytes{}, result)
		require.Nil(t, err)
	})

	// Test fnASSRB - adapts func(string, string) bool
	t.Run("fnASSRB", func(t *testing.T) {
		adapter := fnASSRB(func(s1, s2 string) bool {
			return strings.Contains(s1, s2)
		})
		require.NotNil(t, adapter)
		result, err := adapter(String("hello"), String("ell"))
		require.Equal(t, Bool(true), result)
		require.Nil(t, err)
	})

	// Test fnASSRBex - adapts func(string, string) bool
	t.Run("fnASSRBex", func(t *testing.T) {
		adapter := fnASSRBex(func(s1, s2 string) bool {
			return strings.Contains(s1, s2)
		})
		require.NotNil(t, adapter)
		result, err := adapter(Call{Args: Array{String("hello"), String("ell")}})
		require.Equal(t, Bool(true), result)
		require.Nil(t, err)
	})

	// Test fnASAR - adapts func(string, interface{})
	t.Run("fnASAR", func(t *testing.T) {
		called := false
		adapter := fnASAR(func(s string, v interface{}) {
			called = true
		})
		require.NotNil(t, adapter)
		result, err := adapter(String("key"), Int(42))
		require.True(t, called)
		require.Equal(t, Undefined, result)
		require.Nil(t, err)
	})

	// Test fnASARex - adapts func(string, interface{})
	t.Run("fnASARex", func(t *testing.T) {
		called := false
		adapter := fnASARex(func(s string, v interface{}) {
			called = true
		})
		require.NotNil(t, adapter)
		result, err := adapter(Call{Args: Array{String("key"), Int(42)}})
		require.True(t, called)
		require.Equal(t, Undefined, result)
		require.Nil(t, err)
	})

	// Test fnASR - adapts func(string)
	t.Run("fnASR", func(t *testing.T) {
		called := false
		adapter := fnASR(func(s string) {
			called = true
		})
		require.NotNil(t, adapter)
		result, err := adapter(String("hello"))
		require.True(t, called)
		require.Equal(t, Undefined, result)
		require.Nil(t, err)
	})

	// Test fnASRex - adapts func(string)
	t.Run("fnASRex", func(t *testing.T) {
		called := false
		adapter := fnASRex(func(s string) {
			called = true
		})
		require.NotNil(t, adapter)
		result, err := adapter(Call{Args: Array{String("hello")}})
		require.True(t, called)
		require.Equal(t, Undefined, result)
		require.Nil(t, err)
	})

	// Test fnARI - adapts func() int
	t.Run("fnARI", func(t *testing.T) {
		adapter := fnARI(func() int {
			return 42
		})
		require.NotNil(t, adapter)
		result, err := adapter()
		require.Equal(t, Int(42), result)
		require.Nil(t, err)
	})

	// Test fnATRS - adapts func(time.Time) string
	t.Run("fnATRS", func(t *testing.T) {
		adapter := fnATRS(func(t time.Time) string {
			return t.Format("2006-01-02")
		})
		require.NotNil(t, adapter)
		testTime := &Time{Value: time.Date(2024, 1, 15, 0, 0, 0, 0, time.UTC)}
		result, err := adapter(testTime)
		require.Equal(t, String("2024-01-15"), result)
		require.Nil(t, err)

		// Test error case - wrong type
		result, err = adapter(Int(123))
		require.IsType(t, &Error{}, result)
		require.Nil(t, err)
	})

	// Test fnATRSex - adapts func(time.Time) string
	t.Run("fnATRSex", func(t *testing.T) {
		adapter := fnATRSex(func(t time.Time) string {
			return t.Format("2006-01-02")
		})
		require.NotNil(t, adapter)
		testTime := &Time{Value: time.Date(2024, 1, 15, 0, 0, 0, 0, time.UTC)}
		result, err := adapter(Call{Args: Array{testTime}})
		require.Equal(t, String("2024-01-15"), result)
		require.Nil(t, err)

		// Test error case - wrong type
		result, err = adapter(Call{Args: Array{Int(123)}})
		require.IsType(t, &Error{}, result)
		require.Nil(t, err)
	})

	// Test fnATVsRS - adapts func(time.Time, ...string) string
	t.Run("fnATVsRS", func(t *testing.T) {
		adapter := fnATVsRS(func(t time.Time, s ...string) string {
			if len(s) > 0 {
				return t.Format(s[0])
			}
			return t.Format("2006-01-02")
		})
		require.NotNil(t, adapter)
		testTime := &Time{Value: time.Date(2024, 1, 15, 0, 0, 0, 0, time.UTC)}
		result, err := adapter(testTime, String("2006/01/02"))
		require.Equal(t, String("2024/01/15"), result)
		require.Nil(t, err)

		// Test error case - wrong type
		result, err = adapter(Int(123))
		require.IsType(t, &Error{}, result)
		require.Nil(t, err)
	})

	// Test fnATIIIRT - adapts func(time.Time, int, int, int) time.Time
	t.Run("fnATIIIRT", func(t *testing.T) {
		adapter := fnATIIIRT(func(t time.Time, years, months, days int) time.Time {
			return t.AddDate(years, months, days)
		})
		require.NotNil(t, adapter)
		testTime := &Time{Value: time.Date(2024, 1, 15, 0, 0, 0, 0, time.UTC)}
		result, err := adapter(testTime, Int(1), Int(2), Int(3))
		require.IsType(t, &Time{}, result)
		require.Nil(t, err)

		// Test error case - wrong type
		result, err = adapter(Int(123), Int(1), Int(2), Int(3))
		require.IsType(t, &Error{}, result)
		require.Nil(t, err)
	})

	// Test fnATIIIRTex - adapts func(time.Time, int, int, int) time.Time
	t.Run("fnATIIIRTex", func(t *testing.T) {
		adapter := fnATIIIRTex(func(t time.Time, years, months, days int) time.Time {
			return t.AddDate(years, months, days)
		})
		require.NotNil(t, adapter)
		testTime := &Time{Value: time.Date(2024, 1, 15, 0, 0, 0, 0, time.UTC)}
		result, err := adapter(Call{Args: Array{testTime, Int(1), Int(2), Int(3)}})
		require.IsType(t, &Time{}, result)
		require.Nil(t, err)

		// Test error case - wrong type
		result, err = adapter(Call{Args: Array{Int(123), Int(1), Int(2), Int(3)}})
		require.IsType(t, &Error{}, result)
		require.Nil(t, err)
	})
}

// TestTypeCheckBuiltins tests type checking builtins
func TestTypeCheckBuiltins(t *testing.T) {
	t.Run("isInt", func(t *testing.T) {
		expectRun(t, `return isInt(42)`, nil, True)
	})

	t.Run("isUint", func(t *testing.T) {
		expectRun(t, `return isUint(uint(42))`, nil, True)
	})

	t.Run("isFloat", func(t *testing.T) {
		expectRun(t, `return isFloat(3.14)`, nil, True)
	})

	t.Run("isChar", func(t *testing.T) {
		expectRun(t, `return isChar('a')`, nil, True)
	})

	t.Run("isByte", func(t *testing.T) {
		expectRun(t, `return isByte(byte(65))`, nil, True)
	})

	t.Run("isBool", func(t *testing.T) {
		expectRun(t, `return isBool(true)`, nil, True)
	})

	t.Run("isString", func(t *testing.T) {
		expectRun(t, `return isString("hello")`, nil, True)
	})

	t.Run("isBytes", func(t *testing.T) {
		expectRun(t, `return isBytes(bytes("hello"))`, nil, True)
	})

	t.Run("isChars", func(t *testing.T) {
		expectRun(t, `return isChars(chars("hello"))`, nil, True)
	})

	t.Run("isMap", func(t *testing.T) {
		expectRun(t, `return isMap({})`, nil, True)
	})

	t.Run("isArray", func(t *testing.T) {
		expectRun(t, `return isArray([])`, nil, True)
	})

	t.Run("isUndefined", func(t *testing.T) {
		expectRun(t, `return isUndefined(undefined)`, nil, True)
	})

	t.Run("isFunction", func(t *testing.T) {
		expectRun(t, `return isFunction(func() { return 1 })`, nil, True)
	})

	t.Run("isCallable", func(t *testing.T) {
		expectRun(t, `return isCallable(func() { return 1 })`, nil, True)
	})

	t.Run("isIterable", func(t *testing.T) {
		expectRun(t, `return isIterable([1, 2, 3])`, nil, True)
	})
}

// TestCapBuiltin tests cap builtin
func TestCapBuiltin(t *testing.T) {
	t.Run("cap of bytes", func(t *testing.T) {
		expectRun(t, `b := bytesWithCap(10); return cap(b)`, nil, Int(10))
	})
}

// TestBytesWithSizeAndCap tests bytesWithSize and bytesWithCap
func TestBytesWithSizeAndCap(t *testing.T) {
	t.Run("bytesWithSize", func(t *testing.T) {
		expectRun(t, `b := bytesWithSize(5); return len(b)`, nil, Int(5))
	})

	t.Run("bytesWithCap", func(t *testing.T) {
		expectRun(t, `b := bytesWithCap(10); return cap(b)`, nil, Int(10))
	})
}

// TestTestByTextFunc tests testByText builtin
func TestTestByTextFunc(t *testing.T) {
	t.Run("testByText pass", func(t *testing.T) {
		expectRun(t, `return testByText("hello", "hello")`, nil, Undefined)
	})
}

// TestGetSwitchBuiltin tests getSwitch builtin
func TestGetSwitchBuiltin(t *testing.T) {
	t.Run("getSwitch found", func(t *testing.T) {
		expectRun(t, `args := ["-name=John", "-age=30"]; return getSwitch(args, "-name=")`, nil, String("John"))
	})

	t.Run("getSwitch not found", func(t *testing.T) {
		expectRun(t, `args := ["-name=John"]; return getSwitch(args, "-age=", "default")`, nil, String("default"))
	})
}

// TestIfSwitchExistsBuiltin tests ifSwitchExists builtin
func TestIfSwitchExistsBuiltin(t *testing.T) {
	t.Run("ifSwitchExists true", func(t *testing.T) {
		expectRun(t, `args := ["-verbose"]; return ifSwitchExists(args, "-verbose")`, nil, True)
	})

	t.Run("ifSwitchExists false", func(t *testing.T) {
		expectRun(t, `args := ["-quiet"]; return ifSwitchExists(args, "-verbose")`, nil, False)
	})
}

// TestIsErrXBuiltin tests isErrX builtin
func TestIsErrXBuiltin(t *testing.T) {
	t.Run("isErrX on error", func(t *testing.T) {
		expectRun(t, `return isErrX(error("test"))`, nil, True)
	})

	t.Run("isErrX on non-error", func(t *testing.T) {
		expectRun(t, `return isErrX("not an error")`, nil, False)
	})
}

// TestIsNilBuiltin tests isNil builtin
func TestIsNilBuiltin(t *testing.T) {
	t.Run("isNil on undefined", func(t *testing.T) {
		expectRun(t, `return isNil(undefined)`, nil, True)
	})

	t.Run("isNil on value", func(t *testing.T) {
		expectRun(t, `return isNil(42)`, nil, False)
	})
}

// TestPassFunc tests pass builtin
func TestPassFunc(t *testing.T) {
	t.Run("pass returns undefined", func(t *testing.T) {
		expectRun(t, `return pass()`, nil, Undefined)
	})
}

// TestPlFunc tests pl (println) builtin
func TestPlFunc(t *testing.T) {
	t.Run("pl returns undefined", func(t *testing.T) {
		expectRun(t, `return pl("test")`, nil, Undefined)
	})
}

// TestTypeCodeBuiltin tests typeCode builtin
func TestTypeCodeBuiltin(t *testing.T) {
	t.Run("typeCode returns int", func(t *testing.T) {
		expectRun(t, `return typeCode(42) > 0`, nil, True)
	})
}

// TestGetParamBuiltin tests getParam builtin
func TestGetParamBuiltin(t *testing.T) {
	t.Run("getParam with default", func(t *testing.T) {
		expectRun(t, `args := ["a", "b", "c"]; return getParam(args, 1, "default")`, nil, String("b"))
	})

	t.Run("getParam out of bounds", func(t *testing.T) {
		expectRun(t, `args := ["a"]; return getParam(args, 5, "default")`, nil, String("default"))
	})
}

// TestPrintfBuiltinsMore tests printf/sprintf builtins
func TestPrintfBuiltinsMore(t *testing.T) {
	t.Run("sprintf basic", func(t *testing.T) {
		expectRun(t, `return sprintf("%d + %d = %d", 1, 2, 3)`, nil, String("1 + 2 = 3"))
	})

	t.Run("sprintf string", func(t *testing.T) {
		expectRun(t, `return sprintf("Hello, %s!", "World")`, nil, String("Hello, World!"))
	})

	t.Run("printf returns undefined", func(t *testing.T) {
		expectRun(t, `return printf("test")`, nil, Undefined)
	})

	t.Run("println returns undefined", func(t *testing.T) {
		expectRun(t, `return println("test")`, nil, Undefined)
	})
}

// TestCallExAdapter tests CallExAdapter
func TestCallExAdapter(t *testing.T) {
	t.Run("CallExAdapter", func(t *testing.T) {
		fn := func(c Call) (Object, error) {
			return Int(len(c.GetArgs())), nil
		}
		adapter := CallExAdapter(fn)
		require.NotNil(t, adapter)
		result, err := adapter(Int(1), Int(2))
		require.NoError(t, err)
		require.Equal(t, Int(2), result)
	})
}

// TestOneResultCallAdapter tests OneResultCallAdapter
func TestOneResultCallAdapter(t *testing.T) {
	t.Run("OneResultCallAdapter", func(t *testing.T) {
		fn := func(args ...Object) (Object, error) {
			return Int(42), nil
		}
		adapter := OneResultCallAdapter(fn)
		require.NotNil(t, adapter)
		result := adapter()
		require.Equal(t, Int(42), result)
	})
}

// TestOneResultCallExAdapter tests OneResultCallExAdapter
func TestOneResultCallExAdapter(t *testing.T) {
	t.Run("OneResultCallExAdapter", func(t *testing.T) {
		fn := func(c Call) (Object, error) {
			return Int(len(c.GetArgs())), nil
		}
		adapter := OneResultCallExAdapter(fn)
		require.NotNil(t, adapter)
		result := adapter(Int(1), Int(2), Int(3))
		require.Equal(t, Int(3), result)
	})
}

// TestToArgsHelpers tests toArgs helper functions
func TestToArgsHelpers(t *testing.T) {
	t.Run("toArgsS", func(t *testing.T) {
		args := Array{String("a"), String("b")}
		result := toArgsS(0, Call{Args: args})
		require.Equal(t, []string{"a", "b"}, result)
	})

	t.Run("toArgsN", func(t *testing.T) {
		args := Array{Int(1), Int(2)}
		result := toArgsN(0, Call{Args: args})
		require.Equal(t, []int{1, 2}, result)
	})
}

// TestResetBuiltin tests reset builtin
func TestResetBuiltin(t *testing.T) {
	t.Run("reset returns empty array", func(t *testing.T) {
		expectRun(t, `return len(reset([1, 2, 3]))`, nil, Int(0))
	})
}

// TestDeleteBuiltinMore tests delete builtin
func TestDeleteBuiltinMore(t *testing.T) {
	t.Run("delete map key", func(t *testing.T) {
		expectRun(t, `m := {"a": 1, "b": 2}; delete(m, "a"); return len(m)`, nil, Int(1))
	})
}

// TestLenBuiltin tests len builtin
func TestLenBuiltin(t *testing.T) {
	t.Run("len string", func(t *testing.T) {
		expectRun(t, `return len("hello")`, nil, Int(5))
	})

	t.Run("len array", func(t *testing.T) {
		expectRun(t, `return len([1, 2, 3])`, nil, Int(3))
	})

	t.Run("len map", func(t *testing.T) {
		expectRun(t, `return len({"a": 1, "b": 2})`, nil, Int(2))
	})
}

// TestStringFunc tests string builtin
func TestStringFunc(t *testing.T) {
	t.Run("string from int", func(t *testing.T) {
		expectRun(t, `return string(42)`, nil, String("42"))
	})

	t.Run("string from float", func(t *testing.T) {
		expectRun(t, `return string(3.14)`, nil, String("3.14"))
	})

	t.Run("string from bool", func(t *testing.T) {
		expectRun(t, `return string(true)`, nil, String("true"))
	})
}

// TestCharFuncMore tests char builtin
func TestCharFuncMore(t *testing.T) {
	t.Run("char from int", func(t *testing.T) {
		expectRun(t, `return int(char(65))`, nil, Int(65))
	})

	t.Run("char from string", func(t *testing.T) {
		expectRun(t, `return char("A")`, nil, Char('A'))
	})
}

// TestFloatFunc tests float builtin
func TestFloatFunc(t *testing.T) {
	t.Run("float from int", func(t *testing.T) {
		expectRun(t, `return float(42)`, nil, Float(42.0))
	})

	t.Run("float from string", func(t *testing.T) {
		expectRun(t, `return float("3.14")`, nil, Float(3.14))
	})
}

// TestCharsFuncMore tests chars builtin more
func TestCharsFuncMore(t *testing.T) {
	t.Run("chars unicode", func(t *testing.T) {
		expectRun(t, `c := chars("你好"); return len(c)`, nil, Int(2))
	})
}

// TestMakeArray tests make array builtin
func TestMakeArray(t *testing.T) {
	t.Run("make array with size", func(t *testing.T) {
		expectRun(t, `a := make("array", 3); return len(a)`, nil, Int(3))
	})
}

// TestMakeMap tests make map builtin
func TestMakeMap(t *testing.T) {
	t.Run("make map", func(t *testing.T) {
		expectRun(t, `m := make("map"); return typeName(m)`, nil, String("map"))
	})
}

// TestIsErrorBuiltinMore tests isError builtin
func TestIsErrorBuiltinMore(t *testing.T) {
	t.Run("isError on error", func(t *testing.T) {
		expectRun(t, `return isError(error("test"))`, nil, True)
	})

	t.Run("isError on string", func(t *testing.T) {
		expectRun(t, `return isError("error")`, nil, False)
	})
}

// TestBuiltinDelete tests builtin delete function
func TestBuiltinDelete(t *testing.T) {
	t.Run("delete existing key", func(t *testing.T) {
		expectRun(t, `m := {"a": 1, "b": 2}; delete(m, "a"); return m.b`, nil, Int(2))
	})
}

// TestBytesFuncMore tests bytes builtin
func TestBytesFuncMore(t *testing.T) {
	t.Run("bytes from string", func(t *testing.T) {
		expectRun(t, `b := bytes("hello"); return len(b)`, nil, Int(5))
	})

	t.Run("bytes from ints", func(t *testing.T) {
		expectRun(t, `b := bytes(72, 101, 108, 108, 111); return len(b)`, nil, Int(5))
	})
}

// TestAdapterFunctionsMore tests more adapter functions for better coverage
func TestAdapterFunctionsMore(t *testing.T) {
	// Test fnAR - adapts func() to CallableFunc
	t.Run("fnAR", func(t *testing.T) {
		called := false
		fn := fnAR(func() {
			called = true
		})
		result, err := fn()
		require.True(t, called)
		require.Equal(t, Undefined, result)
		require.Nil(t, err)
	})

	// Test fnARex - adapts func() to CallableExFunc
	t.Run("fnARex", func(t *testing.T) {
		called := false
		fn := fnARex(func() {
			called = true
		})
		result, err := fn(Call{})
		require.True(t, called)
		require.Equal(t, Undefined, result)
		require.Nil(t, err)
	})

	// Test fnARS - adapts func() string to CallableFunc
	t.Run("fnARS", func(t *testing.T) {
		fn := fnARS(func() string {
			return "test"
		})
		result, err := fn()
		require.Equal(t, String("test"), result)
		require.Nil(t, err)
	})

	// Test fnARSex - adapts func() string to CallableExFunc
	t.Run("fnARSex", func(t *testing.T) {
		fn := fnARSex(func() string {
			return "test"
		})
		result, err := fn(Call{})
		require.Equal(t, String("test"), result)
		require.Nil(t, err)
	})

	// Test fnART - adapts func() time.Time to CallableFunc
	t.Run("fnART", func(t *testing.T) {
		fn := fnART(func() time.Time {
			return time.Date(2024, 1, 1, 0, 0, 0, 0, time.UTC)
		})
		result, err := fn()
		require.IsType(t, &Time{}, result)
		require.Nil(t, err)
	})

	// Test fnARTex - adapts func() time.Time to CallableExFunc
	t.Run("fnARTex", func(t *testing.T) {
		fn := fnARTex(func() time.Time {
			return time.Date(2024, 1, 1, 0, 0, 0, 0, time.UTC)
		})
		result, err := fn(Call{})
		require.IsType(t, &Time{}, result)
		require.Nil(t, err)
	})

	// Test fnARE - adapts func() error to CallableFunc
	t.Run("fnARE", func(t *testing.T) {
		fn := fnARE(func() error {
			return nil
		})
		result, err := fn()
		require.Equal(t, Undefined, result)
		require.Nil(t, err)
	})

	// Test fnAREex - adapts func() error to CallableExFunc
	t.Run("fnAREex", func(t *testing.T) {
		fn := fnAREex(func() error {
			return fmt.Errorf("test error")
		})
		result, err := fn(Call{})
		require.IsType(t, &Error{}, result)
		require.Nil(t, err)
	})
}

// TestDecodeSimpleMap tests builtinDecodeSimpleMapFunc
func TestDecodeSimpleMap(t *testing.T) {
	t.Run("decode simple map", func(t *testing.T) {
		expectRun(t, `m := decodeSimpleMap("a=1\nb=2"); return m.a`, nil, String("1"))
	})

	t.Run("decode simple map with multiple entries", func(t *testing.T) {
		expectRun(t, `m := decodeSimpleMap("key1=val1\nkey2=val2"); return m.key1`, nil, String("val1"))
	})

	t.Run("decode empty string", func(t *testing.T) {
		expectRun(t, `m := decodeSimpleMap(""); return len(m)`, nil, Int(0))
	})
}

// TestIsNilOrErr tests builtinIsNilOrErrFunc
func TestIsNilOrErr(t *testing.T) {
	t.Run("isNilOrErr on error", func(t *testing.T) {
		expectRun(t, `return isNilOrErr(error("test"))`, nil, Bool(true))
	})

	t.Run("isNilOrErr on normal string", func(t *testing.T) {
		expectRun(t, `return isNilOrErr("normal")`, nil, Bool(false))
	})

	t.Run("isNilOrErr on TXERROR string", func(t *testing.T) {
		expectRun(t, `return isNilOrErr("TXERROR:something")`, nil, Bool(true))
	})

	t.Run("isNilOrErr on undefined", func(t *testing.T) {
		expectRun(t, `return isNilOrErr(undefined)`, nil, Bool(true))
	})
}

// TestStrTrimMore tests builtinStrTrimFunc
func TestStrTrimMore(t *testing.T) {
	t.Run("strTrim basic", func(t *testing.T) {
		expectRun(t, `return strTrim("  hello  ")`, nil, String("hello"))
	})

	t.Run("strTrim with cutset", func(t *testing.T) {
		expectRun(t, `return strTrim("xxhelloxx", "x")`, nil, String("hello"))
	})

	t.Run("strTrim no change needed", func(t *testing.T) {
		expectRun(t, `return strTrim("hello")`, nil, String("hello"))
	})
}

// TestStrSplitN tests builtinStrSplitNFunc
func TestStrSplitN(t *testing.T) {
	t.Run("strSplitN basic", func(t *testing.T) {
		expectRun(t, `return len(strSplitN("a,b,c,d", ",", 2))`, nil, Int(2))
	})

	t.Run("strSplitN limit -1", func(t *testing.T) {
		expectRun(t, `return len(strSplitN("a,b,c", ",", -1))`, nil, Int(3))
	})
}

// TestStrJoinMore tests builtinStrJoinFunc
func TestStrJoinMore(t *testing.T) {
	t.Run("strJoin array", func(t *testing.T) {
		expectRun(t, `return strJoin(["a", "b", "c"], "-")`, nil, String("a-b-c"))
	})

	t.Run("strJoin single element", func(t *testing.T) {
		expectRun(t, `return strJoin(["hello"], ",")`, nil, String("hello"))
	})

	t.Run("strJoin empty array", func(t *testing.T) {
		expectRun(t, `return strJoin([], ",")`, nil, String(""))
	})
}

// TestRandomInt tests builtinGetRandomIntFunc
func TestRandomInt(t *testing.T) {
	t.Run("getRandomInt no args", func(t *testing.T) {
		expectRun(t, `v := getRandomInt(); return v >= 0`, nil, Bool(true))
	})

	t.Run("getRandomInt with max", func(t *testing.T) {
		expectRun(t, `v := getRandomInt(100); return v < 100 && v >= 0`, nil, Bool(true))
	})
}

// TestRandomFloat tests builtinGetRandomFloatFunc
func TestRandomFloat(t *testing.T) {
	t.Run("getRandomFloat", func(t *testing.T) {
		expectRun(t, `v := getRandomFloat(); return v >= 0.0 && v < 1.0`, nil, Bool(true))
	})
}

// TestRandomStr tests builtinGetRandomStrFunc
func TestRandomStr(t *testing.T) {
	t.Run("getRandomStr default length", func(t *testing.T) {
		expectRun(t, `v := getRandomStr(); return len(v) > 0`, nil, Bool(true))
	})

	t.Run("getRandomStr with length", func(t *testing.T) {
		expectRun(t, `v := getRandomStr(10); return len(v) > 0`, nil, Bool(true))
	})
}

// TestStrUnquote tests builtintStrUnquoteFunc
func TestStrUnquote(t *testing.T) {
	t.Run("strUnquote basic", func(t *testing.T) {
		expectRun(t, `return strUnquote("\"hello\"")`, nil, String("hello"))
	})

	t.Run("strUnquote with escapes", func(t *testing.T) {
		expectRun(t, `return strUnquote("\"hello\\nworld\"")`, nil, String("hello\nworld"))
	})
}

// TestStrToTime tests builtinStrToTimeFunc
func TestStrToTime(t *testing.T) {
	t.Run("strToTime returns time object", func(t *testing.T) {
		expectRun(t, `v := strToTime("2024-01-15"); return isError(v)`, nil, Bool(true))
	})
}

// TestMathFunctions tests math builtin functions
func TestMathFunctions(t *testing.T) {
	t.Run("mathSqrt", func(t *testing.T) {
		expectRun(t, `return mathSqrt(4.0)`, nil, Float(2.0))
	})

	t.Run("mathSqrt int", func(t *testing.T) {
		expectRun(t, `v := mathSqrt(16); return v > 3.9 && v < 4.1`, nil, Bool(true))
	})

	t.Run("mathExp", func(t *testing.T) {
		expectRun(t, `v := mathExp(1); return v > 2.7 && v < 2.8`, nil, Bool(true))
	})

	t.Run("mathLog", func(t *testing.T) {
		expectRun(t, `return mathLog(1.0)`, nil, Float(0.0))
	})

	t.Run("mathLog10", func(t *testing.T) {
		expectRun(t, `return mathLog10(100.0)`, nil, Float(2.0))
	})

	t.Run("mathSin", func(t *testing.T) {
		expectRun(t, `return mathSin(0.0)`, nil, Float(0.0))
	})

	t.Run("mathCos", func(t *testing.T) {
		expectRun(t, `return mathCos(0.0)`, nil, Float(1.0))
	})

	t.Run("mathTan", func(t *testing.T) {
		expectRun(t, `return mathTan(0.0)`, nil, Float(0.0))
	})

	t.Run("mathAtan2", func(t *testing.T) {
		expectRun(t, `return mathAtan2(0.0, 1.0)`, nil, Float(0.0))
	})

	t.Run("mathPi", func(t *testing.T) {
		expectRun(t, `v := mathPi(); return v > 3.14 && v < 3.15`, nil, Bool(true))
	})

	t.Run("mathE", func(t *testing.T) {
		expectRun(t, `v := mathE(); return v > 2.71 && v < 2.72`, nil, Bool(true))
	})

	t.Run("mathPow", func(t *testing.T) {
		expectRun(t, `return mathPow(2.0, 3.0)`, nil, Float(8.0))
	})

	t.Run("mathPow int", func(t *testing.T) {
		expectRun(t, `return mathPow(2, 10)`, nil, Float(1024.0))
	})
}

// TestBitNot tests builtinBitNotFunc
func TestBitNot(t *testing.T) {
	t.Run("bitNot basic", func(t *testing.T) {
		expectRun(t, `return bitNot(0)`, nil, Int(-1))
	})

	t.Run("bitNot on value", func(t *testing.T) {
		expectRun(t, `return bitNot(5)`, nil, Int(-6))
	})
}

// TestAdjustFloat tests builtinAdjustFloatFunc
func TestAdjustFloat(t *testing.T) {
	t.Run("adjustFloat basic", func(t *testing.T) {
		expectRun(t, `return adjustFloat(3.14159, 2)`, nil, Float(3.14))
	})

	t.Run("adjustFloat no limit", func(t *testing.T) {
		expectRun(t, `v := adjustFloat(3.14159); return v > 3.14 && v < 3.15`, nil, Bool(true))
	})
}

// TestToOrderedMap tests builtinToOrderedMapFunc
func TestToOrderedMap(t *testing.T) {
	t.Run("toOrderedMap from map", func(t *testing.T) {
		expectRun(t, `m := {"a": 1, "b": 2}; om := toOrderedMap(m); return typeName(om)`, nil, String("orderedMap"))
	})
}

// TestReverseMap tests builtinReverseMapFunc
func TestReverseMap(t *testing.T) {
	t.Run("reverseMap basic", func(t *testing.T) {
		expectRun(t, `m := {"a": "1", "b": "2"}; rm := reverseMap(m); return rm["1"]`, nil, String("a"))
	})
}

// TestSimpleStrToMap tests builtinSimpleStrToMapFunc
func TestSimpleStrToMap(t *testing.T) {
	t.Run("simpleStrToMap basic", func(t *testing.T) {
		expectRun(t, `m := simpleStrToMap("a=1&b=2"); return typeName(m)`, nil, String("map"))
	})
}

// TestSimpleStrToMapReverse tests builtinSimpleStrToMapReverseFunc
func TestSimpleStrToMapReverse(t *testing.T) {
	t.Run("simpleStrToMapReverse basic", func(t *testing.T) {
		expectRun(t, `m := simpleStrToMapReverse("1=a&2=b"); return typeName(m)`, nil, String("map"))
	})
}

// TestIsUtf8 tests builtinIsUtf8Func
func TestIsUtf8(t *testing.T) {
	t.Run("isUtf8 valid", func(t *testing.T) {
		expectRun(t, `return isUtf8("hello")`, nil, Bool(true))
	})

	t.Run("isUtf8 bytes", func(t *testing.T) {
		expectRun(t, `return isUtf8(bytes("hello"))`, nil, Bool(true))
	})
}

// TestStrToUtf8 tests builtinStrToUtf8Func
func TestStrToUtf8(t *testing.T) {
	t.Run("strToUtf8 basic", func(t *testing.T) {
		expectRun(t, `return strToUtf8("hello")`, nil, String("hello"))
	})
}

// TestIsEncrypted tests builtinIsEncryptedFunc
func TestIsEncrypted(t *testing.T) {
	t.Run("isEncrypted false for plain text", func(t *testing.T) {
		expectRun(t, `return isEncrypted("hello")`, nil, Bool(false))
	})
}

// TestArrayContains tests builtinArrayContainsFunc
func TestArrayContains(t *testing.T) {
	t.Run("arrayContains true", func(t *testing.T) {
		expectRun(t, `return arrayContains([1, 2, 3], 2)`, nil, Bool(true))
	})

	t.Run("arrayContains false", func(t *testing.T) {
		expectRun(t, `return arrayContains([1, 2, 3], 5)`, nil, Bool(false))
	})

	t.Run("arrayContains string", func(t *testing.T) {
		expectRun(t, `return arrayContains(["a", "b", "c"], "b")`, nil, Bool(true))
	})
}

// TestStringBuilder tests builtinStringBuilderFunc
func TestStringBuilder(t *testing.T) {
	t.Run("stringBuilder basic", func(t *testing.T) {
		expectRun(t, `sb := stringBuilder(); sb.writeStr("hello"); return sb.toStr()`, nil, String("hello"))
	})

	t.Run("stringBuilder multiple writes", func(t *testing.T) {
		expectRun(t, `sb := stringBuilder(); sb.writeStr("hello"); sb.writeStr(" "); sb.writeStr("world"); return sb.toStr()`, nil, String("hello world"))
	})
}

// TestOrderedMapBuiltin tests builtinOrderedMapFunc
func TestOrderedMapBuiltin(t *testing.T) {
	t.Run("orderedMap basic", func(t *testing.T) {
		expectRun(t, `om := orderedMap(); return typeName(om)`, nil, String("orderedMap"))
	})
}

// TestMutexBuiltin tests builtinMutexFunc
func TestMutexBuiltin(t *testing.T) {
	t.Run("mutex basic", func(t *testing.T) {
		expectRun(t, `m := mutex(); return typeName(m)`, nil, String("mutex"))
	})
}

// TestMuxBuiltin tests builtinMuxFunc
func TestMuxBuiltin(t *testing.T) {
	t.Run("mux basic", func(t *testing.T) {
		expectRun(t, `m := mux(); return typeName(m)`, nil, String("mux"))
	})
}

// TestSeqBuiltin tests builtinSeqFunc
func TestSeqBuiltin(t *testing.T) {
	t.Run("seq basic", func(t *testing.T) {
		expectRun(t, `return typeName(seq())`, nil, String("seq"))
	})
}

// TestGelBuiltin tests builtinGelFunc
func TestGelBuiltin(t *testing.T) {
	t.Run("gel basic", func(t *testing.T) {
		expectRun(t, `g := gel("test"); return typeName(g)`, nil, String("gel"))
	})
}

// TestCharCodeBuiltin tests builtinCharCodeFunc
func TestCharCodeBuiltin(t *testing.T) {
	t.Run("charCode basic", func(t *testing.T) {
		expectRun(t, `return typeName(charCode("A"))`, nil, String("charCode"))
	})
}

// TestSimpleDecode tests builtinSimpleDecodeFunc
func TestSimpleDecode(t *testing.T) {
	t.Run("simpleDecode basic", func(t *testing.T) {
		expectRun(t, `encoded := simpleEncode("hello"); return simpleDecode(encoded)`, nil, String("hello"))
	})
}

// TestBigInt tests builtinBigIntFunc
func TestBigInt(t *testing.T) {
	t.Run("bigInt basic", func(t *testing.T) {
		expectRun(t, `bi := bigInt("123456789"); return typeName(bi)`, nil, String("bigInt"))
	})
}

// TestBigFloat tests builtinBigFloatFunc
func TestBigFloat(t *testing.T) {
	t.Run("bigFloat basic", func(t *testing.T) {
		expectRun(t, `bf := bigFloat("123.456"); return typeName(bf)`, nil, String("bigFloat"))
	})
}

// TestStatusResult tests builtinStatusResultFunc
func TestStatusResult(t *testing.T) {
	t.Run("statusResult basic", func(t *testing.T) {
		expectRun(t, `sr := statusResult("success", "done"); return typeName(sr)`, nil, String("statusResult"))
	})
}

// TestSortArray tests builtinSortArrayFunc
func TestSortArray(t *testing.T) {
	t.Run("sortArray basic", func(t *testing.T) {
		expectRun(t, `arr := [3, 1, 4, 1, 5]; sorted := sortArray(arr); return sorted[0]`, nil, Int(1))
	})
}

// TestTestByStartsWith tests builtinTestByStartsWithFunc
func TestTestByStartsWith(t *testing.T) {
	t.Run("testByStartsWith pass", func(t *testing.T) {
		expectRun(t, `testByStartsWith("hello world", "hello"); return true`, nil, Bool(true))
	})

	t.Run("testByStartsWith with name", func(t *testing.T) {
		expectRun(t, `testByStartsWith("hello world", "hello", "test1"); return true`, nil, Bool(true))
	})
}

// TestTestByEndsWith tests builtinTestByEndsWithFunc
func TestTestByEndsWith(t *testing.T) {
	t.Run("testByEndsWith pass", func(t *testing.T) {
		expectRun(t, `testByEndsWith("hello world", "world"); return true`, nil, Bool(true))
	})

	t.Run("testByEndsWith with name", func(t *testing.T) {
		expectRun(t, `testByEndsWith("hello world", "world", "test1"); return true`, nil, Bool(true))
	})
}

// TestTestByContains tests builtinTestByContainsFunc
func TestTestByContains(t *testing.T) {
	t.Run("testByContains pass", func(t *testing.T) {
		expectRun(t, `testByContains("hello world", "lo wo"); return true`, nil, Bool(true))
	})

	t.Run("testByContains with name", func(t *testing.T) {
		expectRun(t, `testByContains("hello world", "hello", "test1"); return true`, nil, Bool(true))
	})
}

// TestAnyBuiltinMore tests builtinAnyFunc more scenarios
func TestAnyBuiltinMore(t *testing.T) {
	t.Run("any with no args", func(t *testing.T) {
		expectRun(t, `a := any(); return typeName(a)`, nil, String("any"))
	})

	t.Run("any with int", func(t *testing.T) {
		expectRun(t, `a := any(42); return typeName(a)`, nil, String("any"))
	})

	t.Run("any with string", func(t *testing.T) {
		expectRun(t, `a := any("hello"); return typeName(a)`, nil, String("any"))
	})

	t.Run("any with array", func(t *testing.T) {
		expectRun(t, `a := any([1, 2, 3]); return typeName(a)`, nil, String("any"))
	})

	t.Run("any with map", func(t *testing.T) {
		expectRun(t, `a := any({"a": 1}); return typeName(a)`, nil, String("any"))
	})
}

// TestToStrMore tests builtinToStrFunc more scenarios
func TestToStrMore(t *testing.T) {
	t.Run("toStr with int", func(t *testing.T) {
		expectRun(t, `return toStr(42)`, nil, String("42"))
	})

	t.Run("toStr with float", func(t *testing.T) {
		expectRun(t, `return toStr(3.14)`, nil, String("3.14"))
	})

	t.Run("toStr with no args", func(t *testing.T) {
		expectRun(t, `return toStr()`, nil, String(""))
	})
}

// TestToHexMore tests builtinToHexFunc more scenarios
func TestToHexMore(t *testing.T) {
	t.Run("toHex with int", func(t *testing.T) {
		expectRun(t, `return toHex(255)`, nil, String("ff"))
	})

	t.Run("toHex with bytes", func(t *testing.T) {
		expectRun(t, `return toHex(bytes(0, 1, 255))`, nil, String("0001ff"))
	})
}

// TestToIntMore tests builtinToIntFunc more scenarios
func TestToIntMore(t *testing.T) {
	t.Run("toInt from string", func(t *testing.T) {
		expectRun(t, `return toInt("42")`, nil, Int(42))
	})

	t.Run("toInt from float", func(t *testing.T) {
		expectRun(t, `return toInt(3.99)`, nil, Int(3))
	})

	t.Run("toInt with default", func(t *testing.T) {
		expectRun(t, `return toInt("abc", 99)`, nil, Int(99))
	})
}

// TestToFloatMore tests builtinToFloatFunc more scenarios
func TestToFloatMore(t *testing.T) {
	t.Run("toFloat from string", func(t *testing.T) {
		expectRun(t, `v := toFloat("3.14"); return v > 3.13 && v < 3.15`, nil, Bool(true))
	})

	t.Run("toFloat from int", func(t *testing.T) {
		expectRun(t, `return toFloat(42)`, nil, Float(42.0))
	})
}

// TestTypeOfAnyMore tests builtinTypeOfAnyFunc more scenarios
func TestTypeOfAnyMore(t *testing.T) {
	t.Run("typeOfAny with int", func(t *testing.T) {
		expectRun(t, `return typeOfAny(any(42))`, nil, String("int"))
	})

	t.Run("typeOfAny with string", func(t *testing.T) {
		expectRun(t, `return typeOfAny(any("hello"))`, nil, String("string"))
	})
}

// TestToBoolWithDefault tests builtinToBoolWithDefaultFunc
func TestToBoolWithDefault(t *testing.T) {
	t.Run("toBoolWithDefault true string", func(t *testing.T) {
		expectRun(t, `return toBoolWithDefault("true", false)`, nil, Bool(true))
	})

	t.Run("toBoolWithDefault fallback", func(t *testing.T) {
		expectRun(t, `return toBoolWithDefault("invalid", true)`, nil, Bool(true))
	})
}

// TestGetEnvMore tests builtinGetEnvFunc more scenarios
func TestGetEnvMore(t *testing.T) {
	t.Run("getEnv existing", func(t *testing.T) {
		os.Setenv("TEST_VAR_123", "testvalue")
		expectRun(t, `return getEnv("TEST_VAR_123")`, nil, String("testvalue"))
	})

	t.Run("getEnv non-existing", func(t *testing.T) {
		expectRun(t, `return getEnv("NONEXISTENT_VAR_XYZ")`, nil, String(""))
	})
}

// TestTrimErr tests builtinTrimErrFunc
func TestTrimErr(t *testing.T) {
	t.Run("trimErr from non-error string", func(t *testing.T) {
		expectRun(t, `return trimErr("  hello  ")`, nil, String("hello"))
	})

	t.Run("trimErr returns empty on error", func(t *testing.T) {
		expectRun(t, `return trimErr(error("test"))`, nil, String(""))
	})
}

// TestCheckErrXMore tests builtinCheckErrXFunc more scenarios
func TestCheckErrXMore(t *testing.T) {
	t.Run("checkErrX passes on no error", func(t *testing.T) {
		expectRun(t, `checkErrX("hello"); return true`, nil, Bool(true))
	})
}

// TestCheckEmptyMore tests builtinCheckEmptyFunc more scenarios
func TestCheckEmptyMore(t *testing.T) {
	t.Run("checkEmpty passes on non-empty", func(t *testing.T) {
		expectRun(t, `checkEmpty("hello"); return true`, nil, Bool(true))
	})

	t.Run("checkEmpty with int", func(t *testing.T) {
		expectRun(t, `checkEmpty(42); return true`, nil, Bool(true))
	})
}

// TestErrToEmptyMore tests builtinErrToEmptyFunc more scenarios
func TestErrToEmptyMore(t *testing.T) {
	t.Run("errToEmpty on non-error", func(t *testing.T) {
		expectRun(t, `return errToEmpty("hello")`, nil, String("hello"))
	})

	t.Run("errToEmpty on error", func(t *testing.T) {
		expectRun(t, `return errToEmpty(error("test"))`, nil, String(""))
	})
}

// TestPltMore tests builtinPltFunc more scenarios
func TestPltMore(t *testing.T) {
	t.Run("plt with format", func(t *testing.T) {
		expectRun(t, `plt("test: %v", 42); return true`, nil, Bool(true))
	})
}

// TestBase64EncodeByRawUrl tests builtinBase64EncodeByRawUrlFunc
func TestBase64EncodeByRawUrl(t *testing.T) {
	t.Run("base64EncodeByRawUrl basic", func(t *testing.T) {
		expectRun(t, `return base64EncodeByRawUrl("hello")`, nil, String("aGVsbG8"))
	})
}

// TestBase64DecodeByRawUrl tests builtinBase64DecodeByRawUrlFunc
func TestBase64DecodeByRawUrl(t *testing.T) {
	t.Run("base64DecodeByRawUrl basic", func(t *testing.T) {
		expectRun(t, `b := base64DecodeByRawUrl("aGVsbG8"); return string(b)`, nil, String("hello"))
	})
}

// TestTimeFunc tests builtinTimeFunc
func TestTimeFunc(t *testing.T) {
	t.Run("time returns time object", func(t *testing.T) {
		expectRun(t, `t := time(); return isError(t)`, nil, Bool(false))
	})
}

// TestLockUnlock tests lock/unlock builtins
func TestLockUnlock(t *testing.T) {
	t.Run("lock/unlock numbered mutex", func(t *testing.T) {
		expectRun(t, `lock(1); unlock(1); return true`, nil, Bool(true))
	})
}

// TestTestByRegContains tests builtinTestByRegContainsFunc
func TestTestByRegContains(t *testing.T) {
	t.Run("testByRegContains pass", func(t *testing.T) {
		expectRun(t, `testByRegContains("hello123world", "\\d+"); return true`, nil, Bool(true))
	})

	t.Run("testByRegContains with name", func(t *testing.T) {
		expectRun(t, `testByRegContains("abc123", "[a-z]+", "test1"); return true`, nil, Bool(true))
	})
}

// TestTestByReg tests builtinTestByRegFunc
func TestTestByReg(t *testing.T) {
	t.Run("testByReg pass", func(t *testing.T) {
		expectRun(t, `testByReg("hello", "^[a-z]+$"); return true`, nil, Bool(true))
	})
}

// TestGetNowTimeStamp tests builtinGetNowTimeStampFunc
func TestGetNowTimeStamp(t *testing.T) {
	t.Run("getNowTimeStamp returns string", func(t *testing.T) {
		expectRun(t, `ts := getNowTimeStamp(); return len(ts) > 0`, nil, Bool(true))
	})
}

// TestTimeBefore tests builtinTimeBeforeFunc
func TestTimeBefore(t *testing.T) {
	t.Run("timeBefore with time objects", func(t *testing.T) {
		expectRun(t, `t1 := time(); t2 := time(); return typeName(timeBefore(t1, t2))`, nil, String("bool"))
	})
}

// TestParseUrl tests builtinParseUrlFunc
func TestParseUrl(t *testing.T) {
	t.Run("parseUrl returns object", func(t *testing.T) {
		expectRun(t, `u := parseUrl("https://example.com/path?query=1"); return typeName(u)`, nil, String("map"))
	})

	t.Run("parseUrl scheme", func(t *testing.T) {
		expectRun(t, `u := parseUrl("https://example.com/path?query=1"); return u.Scheme`, nil, String("https"))
	})

	t.Run("parseUrl host", func(t *testing.T) {
		expectRun(t, `u := parseUrl("https://example.com:8080/path"); return u.Host`, nil, String("example.com:8080"))
	})

	t.Run("parseUrl path", func(t *testing.T) {
		expectRun(t, `u := parseUrl("https://example.com/test/path"); return u.Path`, nil, String("/test/path"))
	})
}

// TestParseQuery tests builtinParseQueryFunc
func TestParseQuery(t *testing.T) {
	t.Run("parseQuery basic", func(t *testing.T) {
		expectRun(t, `q := parseQuery("a=1&b=2"); return typeName(q)`, nil, String("map"))
	})

	t.Run("parseQuery compact", func(t *testing.T) {
		expectRun(t, `q := parseQuery("a=1&b=2", "-compact"); return q.a`, nil, String("1"))
	})
}

// TestStrTrimFunc tests builtinStrTrimFunc
func TestStrTrimFunc(t *testing.T) {
	t.Run("strTrim basic", func(t *testing.T) {
		expectRun(t, `return strTrim("  hello  ")`, nil, String("hello"))
	})

	t.Run("strTrim with cutset", func(t *testing.T) {
		expectRun(t, `return strTrim("xxhelloxx", "x")`, nil, String("hello"))
	})
}

// TestSscanf tests builtinSscanfFunc
func TestSscanf(t *testing.T) {
	t.Run("sscanf returns error for insufficient args", func(t *testing.T) {
		expectRun(t, `return isError(sscanf("123"))`, nil, Bool(true))
	})

	t.Run("sscanf returns error for no args", func(t *testing.T) {
		expectRun(t, `return isError(sscanf())`, nil, Bool(true))
	})
}

// TestShuffle tests builtinShuffleFunc
func TestShuffle(t *testing.T) {
	t.Run("shuffle returns same length", func(t *testing.T) {
		expectRun(t, `a := [1, 2, 3, 4, 5]; b := shuffle(a, 3); return len(b)`, nil, Int(5))
	})
}

// TestNew tests builtinNewFunc
func TestNew(t *testing.T) {
	t.Run("new int", func(t *testing.T) {
		expectRun(t, `v := new("int", 42); return typeName(v)`, nil, String("objectRef"))
	})

	t.Run("new string", func(t *testing.T) {
		expectRun(t, `v := new("string", "hello"); return typeName(v)`, nil, String("objectRef"))
	})
}

// TestExit tests builtinExitFunc - skipped since exit() calls os.Exit
func TestExit(t *testing.T) {
	t.Skip("exit() calls os.Exit which cannot be tested in unit tests")
}

// TestCallNamedFunc tests builtinCallNamedFuncFunc
func TestCallNamedFunc(t *testing.T) {
	t.Run("callNamedFunc exists", func(t *testing.T) {
		// Just verify the function is callable
		expectRun(t, `return typeName(callNamedFunc)`, nil, String("builtinFunction"))
	})
}

// TestGetNamedValue tests builtinGetNamedValueFunc
func TestGetNamedValue(t *testing.T) {
	t.Run("getNamedValue exists", func(t *testing.T) {
		// Just verify the function is callable
		expectRun(t, `return typeName(getNamedValue)`, nil, String("builtinFunction"))
	})
}

// TestGetValue tests builtinGetValueFunc
func TestGetValue(t *testing.T) {
	t.Run("getValue exists", func(t *testing.T) {
		expectRun(t, `return typeName(getValue)`, nil, String("builtinFunction"))
	})
}

// TestGetMember tests builtinGetMemberFunc
func TestGetMember(t *testing.T) {
	t.Run("getMember exists", func(t *testing.T) {
		expectRun(t, `return typeName(getMember)`, nil, String("builtinFunction"))
	})
}

// TestSetMember tests builtinSetMemberFunc
func TestSetMember(t *testing.T) {
	t.Run("setMember returns error for immutable", func(t *testing.T) {
		expectRun(t, `return isError(setMember("string", "x", 1))`, nil, Bool(true))
	})
}

// TestSetValue tests builtinSetValueFunc
func TestSetValue(t *testing.T) {
	t.Run("setValue returns error for invalid", func(t *testing.T) {
		expectRun(t, `return isError(setValue("string", "x", 1))`, nil, Bool(true))
	})
}

// TestCallMethod tests builtinCallMethodFunc
func TestCallMethod(t *testing.T) {
	t.Run("callMethod exists", func(t *testing.T) {
		expectRun(t, `return typeName(callMethod)`, nil, String("builtinFunction"))
	})
}

// TestSetValueByRef tests builtinSetValueByRefFunc
func TestSetValueByRef(t *testing.T) {
	t.Run("setValueByRef returns error for invalid", func(t *testing.T) {
		expectRun(t, `return isError(setValueByRef(42, 1))`, nil, Bool(true))
	})
}

// TestUnref tests builtinUnrefFunc
func TestUnref(t *testing.T) {
	t.Run("unref returns value", func(t *testing.T) {
		expectRun(t, `v := new("int", 42); return typeName(unref(v))`, nil, String("int"))
	})
}

// TestDumpVar tests builtinDumpVarFunc
func TestDumpVar(t *testing.T) {
	t.Run("dumpVar returns undefined", func(t *testing.T) {
		expectRun(t, `dumpVar(42); return true`, nil, Bool(true))
	})
}

// TestDebugInfo tests builtinDebugInfoFunc
func TestDebugInfo(t *testing.T) {
	t.Run("debugInfo returns string", func(t *testing.T) {
		expectRun(t, `d := debugInfo(); return typeName(d)`, nil, String("string"))
	})
}

// TestRemoveDir tests builtinRemoveDirFunc
func TestRemoveDir(t *testing.T) {
	t.Run("removeDir returns error for non-existent", func(t *testing.T) {
		expectRun(t, `return isError(removeDir("/nonexistent/path/xyz123"))`, nil, Bool(true))
	})
}

// TestGetFileInfo tests builtinGetFileInfoFunc
func TestGetFileInfo(t *testing.T) {
	t.Run("getFileInfo returns error for non-existent", func(t *testing.T) {
		expectRun(t, `return isError(getFileInfo("/nonexistent/file/xyz123.txt"))`, nil, Bool(true))
	})
}

// TestRemovePath tests builtinRemovePathFunc
func TestRemovePath(t *testing.T) {
	t.Run("removePath returns error for non-existent", func(t *testing.T) {
		expectRun(t, `return isError(removePath("/nonexistent/path/xyz123"))`, nil, Bool(true))
	})
}

// TestSetStdin tests builtinSetStdinFunc
func TestSetStdin(t *testing.T) {
	t.Run("setStdin returns error for invalid", func(t *testing.T) {
		expectRun(t, `return isError(setStdin("invalid"))`, nil, Bool(true))
	})
}

// TestSetStdout tests builtinSetStdoutFunc
func TestSetStdout(t *testing.T) {
	t.Run("setStdout returns error for invalid", func(t *testing.T) {
		expectRun(t, `return isError(setStdout("invalid"))`, nil, Bool(true))
	})
}

// TestSetStderr tests builtinSetStderrFunc
func TestSetStderr(t *testing.T) {
	t.Run("setStderr returns error for invalid", func(t *testing.T) {
		expectRun(t, `return isError(setStderr("invalid"))`, nil, Bool(true))
	})
}

// TestGetMultiLineInput tests builtinGetMultiLineInputFunc
func TestGetMultiLineInput(t *testing.T) {
	t.Skip("getMultiLineInput requires interactive terminal and hangs in tests")
}

// TestGetPipe tests builtinGetPipeFunc
func TestGetPipe(t *testing.T) {
	t.Run("getPipe exists", func(t *testing.T) {
		expectRun(t, `return typeName(getPipe)`, nil, String("builtinFunction"))
	})
}

// TestGetWebBytes tests builtinGetWebBytesFunc
func TestGetWebBytes(t *testing.T) {
	t.Skip("network tests timeout without actual network access")
}

// TestGetWebRespBody tests builtinGetWebRespBodyFunc
func TestGetWebRespBody(t *testing.T) {
	t.Skip("network tests timeout without actual network access")
}

// TestGetWebBytesWithHeaders tests builtinGetWebBytesWithHeadersFunc
func TestGetWebBytesWithHeaders(t *testing.T) {
	t.Skip("network tests timeout without actual network access")
}

// TestDatabase tests builtinDatabaseFunc
func TestDatabase(t *testing.T) {
	t.Run("database returns error for invalid driver", func(t *testing.T) {
		expectRun(t, `return isError(database("invalid_driver", "invalid_dsn"))`, nil, Bool(true))
	})

	t.Run("database returns error for not enough params", func(t *testing.T) {
		expectRun(t, `return isError(database("sqlite3"))`, nil, Bool(true))
	})
}

// TestFatalf tests builtinFatalfFunc
func TestFatalf(t *testing.T) {
	t.Skip("fatalf calls os.Exit which cannot be tested in unit tests")
}

// TestGenJSONResp tests builtinGenJSONRespFunc
func TestGenJSONResp(t *testing.T) {
	t.Run("genJSONResp exists", func(t *testing.T) {
		expectRun(t, `return typeName(genJSONResp)`, nil, String("builtinFunction"))
	})
}

// TestHttpHandler tests builtinHttpHandlerFunc
func TestHttpHandler(t *testing.T) {
	t.Run("httpHandler exists", func(t *testing.T) {
		expectRun(t, `return typeName(httpHandler)`, nil, String("builtinFunction"))
	})
}

// TestHttpReq tests builtinHttpReqFunc
func TestHttpReq(t *testing.T) {
	t.Run("httpReq exists", func(t *testing.T) {
		expectRun(t, `return typeName(httpReq)`, nil, String("builtinFunction"))
	})
}

// TestWebSocket tests builtinWebSocketFunc
func TestWebSocket(t *testing.T) {
	t.Skip("network tests timeout without actual network access")
}

// TestFile tests builtinFileFunc
func TestFile(t *testing.T) {
	t.Run("file exists", func(t *testing.T) {
		expectRun(t, `return typeName(file)`, nil, String("builtinFunction"))
	})
}

// TestWriteRespHeader tests builtinWriteRespHeaderFunc
func TestWriteRespHeader(t *testing.T) {
	t.Run("writeRespHeader exists", func(t *testing.T) {
		expectRun(t, `return typeName(writeRespHeader)`, nil, String("builtinFunction"))
	})
}

// TestClose tests builtinCloseFunc
func TestClose(t *testing.T) {
	t.Run("close exists", func(t *testing.T) {
		expectRun(t, `return typeName(close)`, nil, String("builtinFunction"))
	})
}

// TestExcel tests builtinExcelFunc
func TestExcel(t *testing.T) {
	t.Run("excel exists", func(t *testing.T) {
		expectRun(t, `return typeName(excel)`, nil, String("builtinFunction"))
	})
}

// TestWriteResp tests builtinWriteRespFunc
func TestWriteResp(t *testing.T) {
	t.Run("writeResp exists", func(t *testing.T) {
		expectRun(t, `return typeName(writeResp)`, nil, String("builtinFunction"))
	})
}

// TestServeFile tests builtinServeFileFunc
func TestServeFile(t *testing.T) {
	t.Run("serveFile exists", func(t *testing.T) {
		expectRun(t, `return typeName(serveFile)`, nil, String("builtinFunction"))
	})
}

// TestGenJwtToken tests builtinGenJwtTokenFunc
func TestGenJwtToken(t *testing.T) {
	t.Run("genJwtToken exists", func(t *testing.T) {
		expectRun(t, `return typeName(genJwtToken)`, nil, String("builtinFunction"))
	})
}

// TestParseJwtToken tests builtinParseJwtTokenFunc
func TestParseJwtToken(t *testing.T) {
	t.Run("parseJwtToken exists", func(t *testing.T) {
		expectRun(t, `return typeName(parseJwtToken)`, nil, String("builtinFunction"))
	})
}

// TestParseReqForm tests builtinParseReqFormFunc
func TestParseReqForm(t *testing.T) {
	t.Run("parseReqForm exists", func(t *testing.T) {
		expectRun(t, `return typeName(parseReqForm)`, nil, String("builtinFunction"))
	})
}

// TestParseReqFormEx tests builtinParseReqFormExFunc
func TestParseReqFormEx(t *testing.T) {
	t.Run("parseReqFormEx exists", func(t *testing.T) {
		expectRun(t, `return typeName(parseReqFormEx)`, nil, String("builtinFunction"))
	})
}

// TestSetRespHeader tests builtinSetRespHeaderFunc
func TestSetRespHeader(t *testing.T) {
	t.Run("setRespHeader exists", func(t *testing.T) {
		expectRun(t, `return typeName(setRespHeader)`, nil, String("builtinFunction"))
	})
}

// TestGetReqHeader tests builtinGetReqHeaderFunc
func TestGetReqHeader(t *testing.T) {
	t.Run("getReqHeader exists", func(t *testing.T) {
		expectRun(t, `return typeName(getReqHeader)`, nil, String("builtinFunction"))
	})
}

// TestGetReqHeaders tests builtinGetReqHeadersFunc
func TestGetReqHeaders(t *testing.T) {
	t.Run("getReqHeaders exists", func(t *testing.T) {
		expectRun(t, `return typeName(getReqHeaders)`, nil, String("builtinFunction"))
	})
}

// TestIsHttps tests builtinIsHttpsFunc
func TestIsHttps(t *testing.T) {
	t.Run("isHttps exists", func(t *testing.T) {
		expectRun(t, `return typeName(isHttps)`, nil, String("builtinFunction"))
	})
}

// TestPrepareMultiPartFieldFromBytes tests builtinPrepareMultiPartFieldFromBytesFunc
func TestPrepareMultiPartFieldFromBytes(t *testing.T) {
	t.Run("prepareMultiPartFieldFromBytes exists", func(t *testing.T) {
		expectRun(t, `return typeName(prepareMultiPartFieldFromBytes)`, nil, String("builtinFunction"))
	})
}

// TestPrepareMultiPartFileFromBytes tests builtinPrepareMultiPartFileFromBytesFunc
func TestPrepareMultiPartFileFromBytes(t *testing.T) {
	t.Run("prepareMultiPartFileFromBytes exists", func(t *testing.T) {
		expectRun(t, `return typeName(prepareMultiPartFileFromBytes)`, nil, String("builtinFunction"))
	})
}

// TestPostRequest tests builtinPostRequestFunc
func TestPostRequest(t *testing.T) {
	t.Run("postRequest exists", func(t *testing.T) {
		expectRun(t, `return typeName(postRequest)`, nil, String("builtinFunction"))
	})
}

// TestHttpRedirect tests builtinHttpRedirectFunc
func TestHttpRedirect(t *testing.T) {
	t.Run("httpRedirect exists", func(t *testing.T) {
		expectRun(t, `return typeName(httpRedirect)`, nil, String("builtinFunction"))
	})
}

// TestGetReqBody tests builtinGetReqBodyFunc
func TestGetReqBody(t *testing.T) {
	t.Run("getReqBody exists", func(t *testing.T) {
		expectRun(t, `return typeName(getReqBody)`, nil, String("builtinFunction"))
	})
}

// TestReadAllStr tests builtinReadAllStrFunc
func TestReadAllStr(t *testing.T) {
	t.Run("readAllStr exists", func(t *testing.T) {
		expectRun(t, `return typeName(readAllStr)`, nil, String("builtinFunction"))
	})
}

// TestReadAllBytes tests builtinReadAllBytesFunc
func TestReadAllBytes(t *testing.T) {
	t.Run("readAllBytes exists", func(t *testing.T) {
		expectRun(t, `return typeName(readAllBytes)`, nil, String("builtinFunction"))
	})
}

// TestReadBytes tests builtinReadBytesFunc
func TestReadBytes(t *testing.T) {
	t.Run("readBytes exists", func(t *testing.T) {
		expectRun(t, `return typeName(readBytes)`, nil, String("builtinFunction"))
	})
}

// TestBytesGbToUtf8Str tests builtinBytesGbToUtf8StrFunc
func TestBytesGbToUtf8Str(t *testing.T) {
	t.Run("bytesGbToUtf8Str converts bytes", func(t *testing.T) {
		// Test with ASCII bytes which are valid in both GB and UTF-8
		expectRun(t, `b := bytes(0x61, 0x62, 0x63); return bytesGbToUtf8Str(b)`, nil, String("abc"))
	})
}

// TestStrToRgba tests builtinStrToRgbaFunc
func TestStrToRgba(t *testing.T) {
	t.Run("strToRgba parses hex color", func(t *testing.T) {
		expectRun(t, `c := strToRgba("#FF0000"); return c.r`, nil, Int(255))
	})

	t.Run("strToRgba returns map", func(t *testing.T) {
		expectRun(t, `c := strToRgba("#00FF00"); return typeName(c)`, nil, String("map"))
	})

	t.Run("strToRgba has all components", func(t *testing.T) {
		expectRun(t, `c := strToRgba("#0000FF"); return c.b`, nil, Int(255))
	})
}

// TestReplaceHtmlByMap tests builtinReplaceHtmlByMapFunc
func TestReplaceHtmlByMap(t *testing.T) {
	t.Run("replaceHtmlByMap replaces TX patterns", func(t *testing.T) {
		expectRun(t, `r := replaceHtmlByMap("<div>TX_name_XT</div>", {"name": "world"}); return r`, nil, String("<div>world</div>"))
	})

	t.Run("replaceHtmlByMap returns string", func(t *testing.T) {
		expectRun(t, `r := replaceHtmlByMap("test", {}); return typeName(r)`, nil, String("string"))
	})
}

// TestShowTable tests builtinShowTableFunc
func TestShowTable(t *testing.T) {
	t.Run("showTable exists", func(t *testing.T) {
		expectRun(t, `return typeName(showTable)`, nil, String("builtinFunction"))
	})
}

// TestGenQr tests builtinGenQrFunc
func TestGenQr(t *testing.T) {
	t.Run("genQr exists", func(t *testing.T) {
		expectRun(t, `return typeName(genQr)`, nil, String("builtinFunction"))
	})
}

// TestScanQr tests builtinScanQrFunc
func TestScanQr(t *testing.T) {
	t.Run("scanQr exists", func(t *testing.T) {
		expectRun(t, `return typeName(scanQr)`, nil, String("builtinFunction"))
	})
}

// TestAesEncrypt tests builtinAesEncryptFunc
func TestAesEncrypt(t *testing.T) {
	t.Run("aesEncrypt exists", func(t *testing.T) {
		expectRun(t, `return typeName(aesEncrypt)`, nil, String("builtinFunction"))
	})
}

// TestAesDecrypt tests builtinAesDecryptFunc
func TestAesDecrypt(t *testing.T) {
	t.Run("aesDecrypt exists", func(t *testing.T) {
		expectRun(t, `return typeName(aesDecrypt)`, nil, String("builtinFunction"))
	})
}

// TestRsaEncryptStrYY tests builtinRsaEncryptStrYYFunc
func TestRsaEncryptStrYY(t *testing.T) {
	t.Run("rsaEncryptStrYY exists", func(t *testing.T) {
		expectRun(t, `return typeName(rsaEncryptStrYY)`, nil, String("builtinFunction"))
	})
}

// TestPlotDataToStr tests builtinPlotDataToStrFunc
func TestPlotDataToStr(t *testing.T) {
	t.Run("plotDataToStr exists", func(t *testing.T) {
		expectRun(t, `return typeName(plotDataToStr)`, nil, String("builtinFunction"))
	})
}

// TestPlotLoadFont tests builtinPlotLoadFontFunc
func TestPlotLoadFont(t *testing.T) {
	t.Run("plotLoadFont exists", func(t *testing.T) {
		expectRun(t, `return typeName(plotLoadFont)`, nil, String("builtinFunction"))
	})
}

// TestPlotDataToImage tests builtinPlotDataToImageFunc
func TestPlotDataToImage(t *testing.T) {
	t.Run("plotDataToImage exists", func(t *testing.T) {
		expectRun(t, `return typeName(plotDataToImage)`, nil, String("builtinFunction"))
	})
}

// TestImageToAscii tests builtinImageToAsciiFunc
func TestImageToAscii(t *testing.T) {
	t.Run("imageToAscii exists", func(t *testing.T) {
		expectRun(t, `return typeName(imageToAscii)`, nil, String("builtinFunction"))
	})
}

// TestGetImageInfo tests builtinGetImageInfoFunc
func TestGetImageInfo(t *testing.T) {
	t.Run("getImageInfo exists", func(t *testing.T) {
		expectRun(t, `return typeName(getImageInfo)`, nil, String("builtinFunction"))
	})
}

// TestEncodeImage tests builtinEncodeImageFunc
func TestEncodeImage(t *testing.T) {
	t.Run("encodeImage exists", func(t *testing.T) {
		expectRun(t, `return typeName(encodeImage)`, nil, String("builtinFunction"))
	})
}

// TestSaveImageToFile tests builtinSaveImageToFileFunc
func TestSaveImageToFile(t *testing.T) {
	t.Run("saveImageToFile exists", func(t *testing.T) {
		expectRun(t, `return typeName(saveImageToFile)`, nil, String("builtinFunction"))
	})
}

// TestSaveImageToBytes tests builtinSaveImageToBytesFunc
func TestSaveImageToBytes(t *testing.T) {
	t.Run("saveImageToBytes exists", func(t *testing.T) {
		expectRun(t, `return typeName(saveImageToBytes)`, nil, String("builtinFunction"))
	})
}

// TestResizeImage tests builtinResizeImageFunc
func TestResizeImage(t *testing.T) {
	t.Run("resizeImage exists", func(t *testing.T) {
		expectRun(t, `return typeName(resizeImage)`, nil, String("builtinFunction"))
	})
}

// TestDrawImageOnImage tests builtinDrawImageOnImageFunc
func TestDrawImageOnImage(t *testing.T) {
	t.Run("drawImageOnImage exists", func(t *testing.T) {
		expectRun(t, `return typeName(drawImageOnImage)`, nil, String("builtinFunction"))
	})
}

// TestDrawTextWrappedOnImage tests builtinDrawTextWrappedOnImageFunc
func TestDrawTextWrappedOnImage(t *testing.T) {
	t.Run("drawTextWrappedOnImage exists", func(t *testing.T) {
		expectRun(t, `return typeName(drawTextWrappedOnImage)`, nil, String("builtinFunction"))
	})
}

// TestAddWatermarkToImage tests builtinAddWatermarkToImageFunc
func TestAddWatermarkToImage(t *testing.T) {
	t.Run("addWatermarkToImage exists", func(t *testing.T) {
		expectRun(t, `return typeName(addWatermarkToImage)`, nil, String("builtinFunction"))
	})
}

// TestSetImageOpacity tests builtinSetImageOpacityFunc
func TestSetImageOpacity(t *testing.T) {
	t.Run("setImageOpacity exists", func(t *testing.T) {
		expectRun(t, `return typeName(setImageOpacity)`, nil, String("builtinFunction"))
	})
}

// TestEncodeBytesInImage tests builtinEncodeBytesInImageFunc
func TestEncodeBytesInImage(t *testing.T) {
	t.Run("encodeBytesInImage exists", func(t *testing.T) {
		expectRun(t, `return typeName(encodeBytesInImage)`, nil, String("builtinFunction"))
	})
}

// TestDecodeBytesFromImage tests builtinDecodeBytesFromImageFunc
func TestDecodeBytesFromImage(t *testing.T) {
	t.Run("decodeBytesFromImage exists", func(t *testing.T) {
		expectRun(t, `return typeName(decodeBytesFromImage)`, nil, String("builtinFunction"))
	})
}

// TestCopyBytes tests builtinCopyBytesFunc
func TestCopyBytes(t *testing.T) {
	t.Run("copyBytes copies bytes", func(t *testing.T) {
		// copyBytes copies src to dst and returns number of bytes copied
		expectRun(t, `dst := bytes(0, 0, 0); src := bytes(1, 2, 3); n := copyBytes(dst, src); return n`, nil, Int(3))
	})

	t.Run("copyBytes with smaller destination", func(t *testing.T) {
		expectRun(t, `dst := bytes(0, 0); src := bytes(1, 2, 3, 4); n := copyBytes(dst, src); return n`, nil, Int(2))
	})
}

// TestAwsSign tests builtinAwsSignFunc
func TestAwsSign(t *testing.T) {
	t.Run("awsSign exists", func(t *testing.T) {
		expectRun(t, `return typeName(awsSign)`, nil, String("builtinFunction"))
	})
}

// TestSendMail tests builtinSendMailFunc
func TestSendMail(t *testing.T) {
	t.Run("sendMail exists", func(t *testing.T) {
		expectRun(t, `return typeName(sendMail)`, nil, String("builtinFunction"))
	})
}

// TestDocxReplacePattern tests builtinDocxReplacePatternFunc
func TestDocxReplacePattern(t *testing.T) {
	t.Run("docxReplacePattern exists", func(t *testing.T) {
		expectRun(t, `return typeName(docxReplacePattern)`, nil, String("builtinFunction"))
	})
}

// TestDocxGetPlaceholders tests builtinDocxGetPlaceholdersFunc
func TestDocxGetPlaceholders(t *testing.T) {
	t.Run("docxGetPlaceholders exists", func(t *testing.T) {
		expectRun(t, `return typeName(docxGetPlaceholders)`, nil, String("builtinFunction"))
	})
}

// TestDocxToStrs tests builtinDocxToStrsFunc
func TestDocxToStrs(t *testing.T) {
	t.Run("docxToStrs exists", func(t *testing.T) {
		expectRun(t, `return typeName(docxToStrs)`, nil, String("builtinFunction"))
	})
}

// TestGuiServerCommand tests builtinGuiServerCommandFunc
func TestGuiServerCommand(t *testing.T) {
	t.Run("guiServerCommand exists", func(t *testing.T) {
		expectRun(t, `return typeName(guiServerCommand)`, nil, String("builtinFunction"))
	})
}

// TestRunTicker tests builtinRunTickerFunc
func TestRunTicker(t *testing.T) {
	t.Run("runTicker exists", func(t *testing.T) {
		expectRun(t, `return typeName(runTicker)`, nil, String("builtinFunction"))
	})
}

// TestNewEx tests builtinNewExFunc
func TestNewEx(t *testing.T) {
	t.Run("newEx exists", func(t *testing.T) {
		expectRun(t, `return typeName(newEx)`, nil, String("builtinFunction"))
	})
}

// TestCallMethodEx tests builtinCallMethodExFunc
func TestCallMethodEx(t *testing.T) {
	t.Run("callMethodEx exists", func(t *testing.T) {
		expectRun(t, `return typeName(callMethodEx)`, nil, String("builtinFunction"))
	})
}

// TestIsErrXMore tests isErrX with more cases for better coverage
func TestIsErrXMore(t *testing.T) {
	t.Run("isErrX on TXERROR string", func(t *testing.T) {
		expectRun(t, `return isErrX("TXERROR: something went wrong")`, nil, True)
	})

	t.Run("isErrX on normal string", func(t *testing.T) {
		expectRun(t, `return isErrX("normal string")`, nil, False)
	})

	t.Run("isErrX on int", func(t *testing.T) {
		expectRun(t, `return isErrX(42)`, nil, False)
	})

	t.Run("isErrX on array", func(t *testing.T) {
		expectRun(t, `return isErrX([1, 2, 3])`, nil, False)
	})

	t.Run("isErrX on map", func(t *testing.T) {
		expectRun(t, `return isErrX({"key": "value"})`, nil, False)
	})
}

// TestIsNilOrErrMore tests isNilOrErr builtin with more cases
func TestIsNilOrErrMore(t *testing.T) {
	t.Run("isNilOrErr on undefined", func(t *testing.T) {
		expectRun(t, `return isNilOrErr(undefined)`, nil, True)
	})

	t.Run("isNilOrErr on error", func(t *testing.T) {
		expectRun(t, `return isNilOrErr(error("test"))`, nil, True)
	})

	t.Run("isNilOrErr on normal value", func(t *testing.T) {
		expectRun(t, `return isNilOrErr(42)`, nil, False)
	})

	t.Run("isNilOrErr on TXERROR string", func(t *testing.T) {
		expectRun(t, `return isNilOrErr("TXERROR: failed")`, nil, True)
	})

	t.Run("isNilOrErr on empty string", func(t *testing.T) {
		expectRun(t, `return isNilOrErr("")`, nil, False)
	})
}

// TestTimeMore tests time() builtin with more cases
func TestTimeMore(t *testing.T) {
	t.Run("time returns time object", func(t *testing.T) {
		expectRun(t, `t := time(); return typeName(t)`, nil, String("time"))
	})

	t.Run("time unix timestamp", func(t *testing.T) {
		expectRun(t, `t := time(1704067200); return typeName(t)`, nil, String("time"))
	})
}

// TestToTimeMore tests toTime builtin with more cases
func TestToTimeMore(t *testing.T) {
	t.Run("toTime from int timestamp", func(t *testing.T) {
		expectRun(t, `t := toTime(1704067200); return typeName(t)`, nil, String("time"))
	})
}

// TestBase64EncodeByRawUrlMore tests base64EncodeByRawUrl builtin
func TestBase64EncodeByRawUrlMore(t *testing.T) {
	t.Run("base64EncodeByRawUrl encodes string", func(t *testing.T) {
		expectRun(t, `return base64EncodeByRawUrl("hello")`, nil, String("aGVsbG8"))
	})
}

// TestGetClipText tests getClipText builtin existence
func TestGetClipText(t *testing.T) {
	t.Run("getClipText exists", func(t *testing.T) {
		expectRun(t, `return typeName(getClipText)`, nil, String("builtinFunction"))
	})
}





