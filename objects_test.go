package charlang_test

import (
//	"errors"
	"fmt"
	"math"
	"testing"

	"github.com/stretchr/testify/require"

	. "github.com/topxeq/charlang"
	"github.com/topxeq/charlang/token"
)

func TestObjects2(t *testing.T) {
	var tmpr Object
	var err error

	// ObjectImpl
	require.Panics(t, func(){ ObjectImpl{}.TypeCode() })
	
	require.False(t, ObjectImpl{}.HasMemeber())
	
	tmpr, err = ObjectImpl{}.CallMethod("value")
	
	require.Equal(t, "%!v(PANIC=String method: NotImplementedError: )-<nil>", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Panics(t, func(){ ObjectImpl{}.CallMethod("toStr") })
	
	require.Panics(t, func(){ ObjectImpl{}.CallMethod("a") })
	
	require.Equal(t, "%!v(PANIC=String method: NotImplementedError: )", fmt.Sprintf("%v", ObjectImpl{}.GetValue()))
	
	require.Equal(t, "undefined", fmt.Sprintf("%v", ObjectImpl{}.GetMember("a")))
	
	require.Equal(t, "unsupported action(set member)", fmt.Sprintf("%v", ObjectImpl{}.SetMember("a", ToStringObject("b"))))
	
	// UndefinedType
	require.Equal(t, "0", fmt.Sprintf("%v", (&UndefinedType{}).TypeCode()))
	
	require.False(t, Undefined.HasMemeber())
	
	tmpr, err = Undefined.CallMethod("value")
	
	require.Equal(t, "undefined-<nil>", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "undefined", fmt.Sprintf("%v", Undefined.GetValue()))
	
	require.Equal(t, "undefined", fmt.Sprintf("%v", Undefined.GetMember("a")))
	
	require.Equal(t, "unsupported action(set member)", fmt.Sprintf("%v", Undefined.SetMember("a", ToStringObject("b"))))

	// Bool
	require.False(t, Bool(true).HasMemeber())
	
	tmpr, err = Bool(true).CallMethod("value")
	
	require.Equal(t, "true-<nil>", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "true", fmt.Sprintf("%v", Bool(true).GetValue()))
	
	require.Equal(t, "undefined", fmt.Sprintf("%v", Bool(true).GetMember("a")))
	
	require.Equal(t, "unsupported action(set member)", fmt.Sprintf("%v", Bool(true).SetMember("a", ToStringObject("b"))))
	
	require.Equal(t, "NotIndexAssignableError: ", fmt.Sprintf("%v", Bool(true).IndexSet(Int(1), Int(1))))
	
	// Int
	require.Equal(t, "107", fmt.Sprintf("%v", Int(1).TypeCode()))
	
	require.False(t, Int(1).HasMemeber())
	
	tmpr, err = Int(1).CallMethod("value")
	
	require.Equal(t, "1-<nil>", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "1", fmt.Sprintf("%v", Int(1).GetValue()))
	
	require.Equal(t, "undefined", fmt.Sprintf("%v", Int(1).GetMember("a")))
	
	require.Equal(t, "unsupported action(set member)", fmt.Sprintf("%v", Int(1).SetMember("a", ToStringObject("b"))))
	
	require.Equal(t, "NotIndexAssignableError: ", fmt.Sprintf("%v", Int(1).IndexSet(Int(1), Int(1))))
	
	require.Equal(t, "&{1 0}", fmt.Sprintf("%v", Int(1).Iterate()))
	
	// Uint
	require.Equal(t, "111", fmt.Sprintf("%v", Uint(1).TypeCode()))
	
	require.False(t, Uint(1).HasMemeber())
	
	tmpr, err = Uint(1).CallMethod("value")
	
	require.Equal(t, "1-<nil>", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "1", fmt.Sprintf("%v", Uint(1).GetValue()))
	
	require.Equal(t, "undefined", fmt.Sprintf("%v", Uint(1).GetMember("a")))
	
	require.Equal(t, "unsupported action(set member)", fmt.Sprintf("%v", Uint(1).SetMember("a", ToStringObject("b"))))
	
	require.Equal(t, "NotIndexAssignableError: ", fmt.Sprintf("%v", Uint(1).IndexSet(Int(1), Int(1))))
	
	require.Equal(t, "&{1 0}", fmt.Sprintf("%v", Uint(1).Iterate()))
	
	// Float
	require.Equal(t, "115", fmt.Sprintf("%v", Float(1).TypeCode()))
	
	require.False(t, Float(1).HasMemeber())
	
	tmpr, err = Float(1).CallMethod("value")
	
	require.Equal(t, "1-<nil>", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "1", fmt.Sprintf("%v", Float(1).GetValue()))
	
	require.Equal(t, "undefined", fmt.Sprintf("%v", Float(1).GetMember("a")))
	
	require.Equal(t, "unsupported action(set member)", fmt.Sprintf("%v", Float(1).SetMember("a", ToStringObject("b"))))
	
	require.Equal(t, "NotIndexAssignableError: ", fmt.Sprintf("%v", Float(1).IndexSet(Int(1), Int(1))))
	
	require.Equal(t, "<nil>", fmt.Sprintf("%v", Float(1).Iterate()))
	
	// Char
	require.Equal(t, "113", fmt.Sprintf("%v", Char(1).TypeCode()))
	
	require.False(t, Char(1).HasMemeber())
	
	tmpr, err = Char(1).CallMethod("value")
	
	require.Equal(t, "1-<nil>", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "1", fmt.Sprintf("%v", Char(1).GetValue()))
	
	require.Equal(t, "undefined", fmt.Sprintf("%v", Char(1).GetMember("a")))
	
	require.Equal(t, "unsupported action(set member)", fmt.Sprintf("%v", Char(1).SetMember("a", ToStringObject("b"))))
	
	require.Equal(t, "NotIndexAssignableError: ", fmt.Sprintf("%v", Char(1).IndexSet(Int(1), Int(1))))
	
	require.Equal(t, "<nil>", fmt.Sprintf("%v", Char(1).Iterate()))
	
	// Byte
	require.Equal(t, "109", fmt.Sprintf("%v", Byte(1).TypeCode()))
	
	require.Equal(t, "byte", fmt.Sprintf("%v", Byte(1).TypeName()))
	
	require.False(t, Byte(1).HasMemeber())
	
	tmpr, err = Byte(1).CallMethod("value")
	
	require.Equal(t, "1-<nil>", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "1", fmt.Sprintf("%v", Byte(1).GetValue()))
	
	require.Equal(t, "undefined", fmt.Sprintf("%v", Byte(1).GetMember("a")))
	
	require.Equal(t, "unsupported action(set member)", fmt.Sprintf("%v", Byte(1).SetMember("a", ToStringObject("b"))))
	
	require.Equal(t, "true", fmt.Sprintf("%v", Byte(1).Equal(Int(1))))
	
	require.Equal(t, "false", fmt.Sprintf("%v", Byte(1).IsFalsy()))
	
	require.Equal(t, "false", fmt.Sprintf("%v", Byte(1).CanCall()))
	
	tmpr, err = Byte(1).Call(ToStringObject("value1"))
	
	require.Equal(t, "<nil>-NotCallableError: ", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "false", fmt.Sprintf("%v", Byte(1).CanIterate()))
	
	require.Equal(t, "<nil>", fmt.Sprintf("%v", Byte(1).Iterate()))
	
	require.Equal(t, "NotIndexAssignableError: ", fmt.Sprintf("%v", Byte(1).IndexSet(Int(1), Int(1))))
	
	tmpr, err = Byte(1).IndexGet(ToStringObject("value1"))
	
	require.Equal(t, "undefined-error: not indexable: byte", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = Byte(1).IndexGet(Int(2))
	
	require.Equal(t, "<nil>-NotIndexableError: ", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = Byte(1).BinaryOp(token.Add, Int(3))

	require.Equal(t, "4-<nil>", fmt.Sprintf("%v-%v", tmpr, err))

	// String
	ss1 := ToStringObject(1)
	
	require.Equal(t, "105", fmt.Sprintf("%v", ToStringObject(1).TypeCode()))
	
	require.Equal(t, "string", fmt.Sprintf("%v", ToStringObject(1).TypeName()))
	
	require.False(t, ToStringObject(1).HasMemeber())
	
	tmpr, err = ToStringObject(1).CallMethod("value")
	
	require.Equal(t, "1-<nil>", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "1", fmt.Sprintf("%v", ToStringObject(1).GetValue()))
	
	require.Equal(t, "undefined", fmt.Sprintf("%v", ss1.GetMember("a")))
	
//	tk.Pl("%T %#v %+v", ss1, ss1, ss1.Members)
	
	require.Equal(t, "unsupported action(set member)", fmt.Sprintf("%v", ss1.SetMember("a", ToStringObject("b"))))
	
//	require.Equal(t, "b", fmt.Sprintf("%v", ss1.GetMember("a")))
	
	require.Equal(t, "false", fmt.Sprintf("%v", ToStringObject(1).Equal(Int(1))))
	
	require.Equal(t, "false", fmt.Sprintf("%v", ToStringObject(1).IsFalsy()))
	
	require.Equal(t, "false", fmt.Sprintf("%v", ToStringObject(1).CanCall()))
	
	tmpr, err = ToStringObject(1).Call(ToStringObject("value1"))
	
	require.Equal(t, "<nil>-NotCallableError: ", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "true", fmt.Sprintf("%v", ToStringObject(1).CanIterate()))
	
	require.Equal(t, "&{1 0}", fmt.Sprintf("%v", ToStringObject(1).Iterate()))
	
	require.Equal(t, "NotIndexAssignableError: ", fmt.Sprintf("%v", ToStringObject(1).IndexSet(Int(1), Int(1))))
	
	tmpr, err = ToStringObject(1).IndexGet(ToStringObject("value1"))
	
	require.Equal(t, "undefined-error: not indexable: string", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = ToStringObject(1).IndexGet(Int(2))
	
	require.Equal(t, "<nil>-IndexOutOfBoundsError: ", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = String("abcdefg").IndexGet(Int(2))
	
	require.Equal(t, "99-<nil>", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = ToStringObject(12).BinaryOp(token.Add, Int(3))

	require.Equal(t, "123-<nil>", fmt.Sprintf("%v-%v", tmpr, err))

	require.Equal(t, "123-<nil>", fmt.Sprintf("%v-%v", FromStringObject(tmpr.(String)), err))

	require.Equal(t, "123-<nil>", fmt.Sprintf("%v-%v", ToByteObject(tmpr.(String)), err))

	// Bytes
	bo1 := Bytes([]byte{1, 2, 3})
	
	require.Equal(t, "137", fmt.Sprintf("%v", bo1.TypeCode()))
	
	require.Equal(t, "bytes", fmt.Sprintf("%v", bo1.TypeName()))
	
	require.False(t, bo1.HasMemeber())
	
	tmpr, err = bo1.CallMethod("value")
	
	require.Equal(t, "[1 2 3]-<nil>", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "[1 2 3]", fmt.Sprintf("%v", bo1.GetValue()))
	
	require.Equal(t, "undefined", fmt.Sprintf("%v", bo1.GetMember("a")))
	
	require.Equal(t, "unsupported action(set member)", fmt.Sprintf("%v", bo1.SetMember("a", ToStringObject("b"))))
	
	require.Equal(t, "false", fmt.Sprintf("%v", bo1.Equal(Int(1))))
	
	require.Equal(t, "false", fmt.Sprintf("%v", bo1.IsFalsy()))
	
	require.Equal(t, "false", fmt.Sprintf("%v", bo1.CanCall()))
	
	tmpr, err = bo1.Call(ToStringObject("value1"))
	
	require.Equal(t, "<nil>-NotCallableError: ", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "true", fmt.Sprintf("%v", bo1.CanIterate()))
	
	require.Equal(t, "&{[1 2 3] 0}", fmt.Sprintf("%v", bo1.Iterate()))
	
	require.Equal(t, "<nil>", fmt.Sprintf("%v", bo1.IndexSet(Int(1), Int(1))))
	
	tmpr, err = bo1.IndexGet(ToStringObject("value1"))
	
	require.Equal(t, "undefined-error: not indexable: bytes", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = bo1.IndexGet(Int(2))
	
	require.Equal(t, "3-<nil>", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = bo1.IndexGet(Int(5))
	
	require.Equal(t, "<nil>-IndexOutOfBoundsError: ", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = bo1.BinaryOp(token.Add, Int(3))

	require.Equal(t, "<nil>-TypeError: unsupported operand types for '+': 'bytes' and 'int'", fmt.Sprintf("%v-%v", tmpr, err))

	// Chars
	co1 := Chars([]rune{1, 2, 3})
	
	require.Equal(t, "139", fmt.Sprintf("%v", co1.TypeCode()))
	
	require.Equal(t, "chars", fmt.Sprintf("%v", co1.TypeName()))
	
	require.Equal(t, "\x01\x02\x03", fmt.Sprintf("%v", co1.String()))
	
	require.False(t, co1.HasMemeber())
	
	tmpr, err = co1.CallMethod("value")
	
	require.Equal(t, "[1 2 3]-<nil>", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "[1 2 3]", fmt.Sprintf("%v", co1.GetValue()))
	
	require.Equal(t, "undefined", fmt.Sprintf("%v", co1.GetMember("a")))
	
	require.Equal(t, "unsupported action(set member)", fmt.Sprintf("%v", co1.SetMember("a", ToStringObject("b"))))
	
	require.Equal(t, "[1 2 3]", fmt.Sprintf("%v", co1.Copy()))
	
	require.Equal(t, "false", fmt.Sprintf("%v", co1.Equal(Int(1))))
	
	require.Equal(t, "false", fmt.Sprintf("%v", co1.IsFalsy()))
	
	require.Equal(t, "false", fmt.Sprintf("%v", co1.CanCall()))
	
	tmpr, err = co1.Call(ToStringObject("value1"))
	
	require.Equal(t, "<nil>-NotCallableError: ", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "true", fmt.Sprintf("%v", co1.CanIterate()))
	
	require.Equal(t, "&{[1 2 3] 0}", fmt.Sprintf("%v", co1.Iterate()))
	
	require.Equal(t, "<nil>", fmt.Sprintf("%v", co1.IndexSet(Int(1), Int(1))))
	
	tmpr, err = co1.IndexGet(ToStringObject("value1"))
	
	require.Equal(t, "undefined-error: not indexable: chars", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = co1.IndexGet(Int(2))
	
	require.Equal(t, "3-<nil>", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = co1.IndexGet(Int(5))
	
	require.Equal(t, "<nil>-IndexOutOfBoundsError: ", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = co1.BinaryOp(token.Add, Int(3))

	require.Equal(t, "<nil>-TypeError: unsupported operand types for '+': 'chars' and 'int'", fmt.Sprintf("%v-%v", tmpr, err))

	// *CompiledFunction
	codeT := NewCharCode("a := 1; return a+2", nil)

	byteCodeT := QuickCompile(codeT.Source, codeT.CompilerOptions)
	
	codeT.Value = byteCodeT.(*Bytecode)

	vmT := NewVM(byteCodeT.(*Bytecode))

	vmT.Run(nil)
	
	cf1 := vmT.GetCurFunc()
	
//	fmt.Printf("cf1: %#v\n", cf1)
	
	require.Equal(t, `<compiledFunction>`, fmt.Sprintf("%v", cf1))
	
	require.Equal(t, "185", fmt.Sprintf("%v", cf1.TypeCode()))
	
	require.Equal(t, "compiledFunction", fmt.Sprintf("%v", cf1.TypeName()))
	
	require.Equal(t, "<compiledFunction>", fmt.Sprintf("%v", cf1.String()))
	
	require.True(t, cf1.HasMemeber())
	
	tmpr, err = cf1.CallMethod("value")
	
	require.Equal(t, "<compiledFunction>-<nil>", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "<compiledFunction>", fmt.Sprintf("%v", cf1.GetValue()))
	
	require.Equal(t, "error: nil object", fmt.Sprintf("%v", cf1.GetMember("a")))
	
	require.Equal(t, "nil object", fmt.Sprintf("%v", cf1.SetMember("a", ToStringObject("b"))))
	
//	require.Equal(t, "[1 2 3]", fmt.Sprintf("%v", cf1.Copy()))
	
	require.Equal(t, "false", fmt.Sprintf("%v", cf1.Equal(Int(1))))
	
	require.Equal(t, "true", fmt.Sprintf("%v", cf1.IsFalsy()))
	
	require.Equal(t, "true", fmt.Sprintf("%v", cf1.CanCall()))
	
	tmpr, err = cf1.Call(ToStringObject("value1"))
	
	require.Equal(t, "undefined-NotCallableError: ", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "false", fmt.Sprintf("%v", cf1.CanIterate()))
	
	require.Equal(t, "<nil>", fmt.Sprintf("%v", cf1.Iterate()))
	
	require.Equal(t, "NotIndexAssignableError: ", fmt.Sprintf("%v", cf1.IndexSet(Int(1), Int(1))))
	
	tmpr, err = cf1.IndexGet(ToStringObject("value1"))
	
	require.Equal(t, "error: nil object-<nil>", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = cf1.IndexGet(Int(2))
	
	require.Equal(t, "<nil>-NotIndexableError: ", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = cf1.IndexGet(Int(5))
	
	require.Equal(t, "<nil>-NotIndexableError: ", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = cf1.BinaryOp(token.Add, Int(3))

	require.Equal(t, "<nil>-InvalidOperatorError: ", fmt.Sprintf("%v-%v", tmpr, err))

	// *Function
	f1 := (*Function)(nil)
	
//	fmt.Printf("f1: %#v\n", f1)
	
	require.Equal(t, `<nil>`, fmt.Sprintf("%v", f1))
	
	require.Equal(t, "181", fmt.Sprintf("%v", f1.TypeCode()))
	
	require.Equal(t, "function", fmt.Sprintf("%v", f1.TypeName()))
	
	require.Equal(t, "<nil>", fmt.Sprintf("%v", f1.String()))
	
	require.True(t, f1.HasMemeber())
	
	tmpr, err = f1.CallMethod("value")
	
	require.Equal(t, "<nil>-<nil>", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "<nil>", fmt.Sprintf("%v", f1.GetValue()))
	
	require.Equal(t, "error: nil object", fmt.Sprintf("%v", f1.GetMember("a")))
	
	require.Equal(t, "nil object", fmt.Sprintf("%v", f1.SetMember("a", ToStringObject("b"))))
	
//	require.Equal(t, "[1 2 3]", fmt.Sprintf("%v", f1.Copy()))
	
	require.Equal(t, "false", fmt.Sprintf("%v", f1.Equal(Int(1))))
	
	require.Equal(t, "true", fmt.Sprintf("%v", f1.IsFalsy()))
	
	require.Equal(t, "true", fmt.Sprintf("%v", f1.CanCall()))
	
	tmpr, err = f1.Call(ToStringObject("value1"))
	
	require.Equal(t, "undefined-error: nil object", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "false", fmt.Sprintf("%v", f1.CanIterate()))
	
	require.Equal(t, "<nil>", fmt.Sprintf("%v", f1.Iterate()))
	
	require.Equal(t, "NotIndexAssignableError: ", fmt.Sprintf("%v", f1.IndexSet(Int(1), Int(1))))
	
	tmpr, err = f1.IndexGet(ToStringObject("value1"))
	
	require.Equal(t, "error: nil object-<nil>", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = f1.IndexGet(Int(2))
	
	require.Equal(t, "<nil>-NotIndexableError: ", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = f1.IndexGet(Int(5))
	
	require.Equal(t, "<nil>-NotIndexableError: ", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = f1.BinaryOp(token.Add, Int(3))

	require.Equal(t, "<nil>-InvalidOperatorError: ", fmt.Sprintf("%v-%v", tmpr, err))

	// *BuiltinFunction
	bf1 := (*BuiltinFunction)(nil)
	
//	fmt.Printf("bf1: %#v\n", bf1)
	
	require.Equal(t, `<nil>`, fmt.Sprintf("%v", bf1))
	
	require.Equal(t, "183", fmt.Sprintf("%v", bf1.TypeCode()))
	
	require.Equal(t, "builtinFunction", fmt.Sprintf("%v", bf1.TypeName()))
	
	require.Equal(t, "<nil>", fmt.Sprintf("%v", bf1.String()))
	
	require.True(t, bf1.HasMemeber())
	
	tmpr, err = bf1.CallMethod("value")
	
	require.Equal(t, "<nil>-<nil>", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "<nil>", fmt.Sprintf("%v", bf1.GetValue()))
	
	require.Equal(t, "error: nil object", fmt.Sprintf("%v", bf1.GetMember("a")))
	
	require.Equal(t, "nil object", fmt.Sprintf("%v", bf1.SetMember("a", ToStringObject("b"))))
	
	require.Equal(t, "error: nil object", fmt.Sprintf("%v", bf1.Copy()))
	
	require.Equal(t, "false", fmt.Sprintf("%v", bf1.Equal(Int(1))))
	
	require.Equal(t, "true", fmt.Sprintf("%v", bf1.IsFalsy()))
	
	require.Equal(t, "true", fmt.Sprintf("%v", bf1.CanCall()))
	
	tmpr, err = bf1.Call(ToStringObject("value1"))
	
	require.Equal(t, "undefined-error: nil object", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "false", fmt.Sprintf("%v", bf1.CanIterate()))
	
	require.Equal(t, "<nil>", fmt.Sprintf("%v", bf1.Iterate()))
	
	require.Equal(t, "NotIndexAssignableError: ", fmt.Sprintf("%v", bf1.IndexSet(Int(1), Int(1))))
	
	tmpr, err = bf1.IndexGet(ToStringObject("value1"))
	
	require.Equal(t, "undefined-error: nil object", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = bf1.IndexGet(Int(2))
	
	require.Equal(t, "undefined-error: nil object", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = bf1.IndexGet(Int(5))
	
	require.Equal(t, "undefined-error: nil object", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = bf1.BinaryOp(token.Add, Int(3))

	require.Equal(t, "<nil>-InvalidOperatorError: ", fmt.Sprintf("%v-%v", tmpr, err))

}

func TestObjects(t *testing.T) {
	// ensure basic type's Go equality and comparison
	require.True(t, True == Bool(true))
	require.True(t, False == Bool(false))
	require.True(t, True != False)
	// comparable objects
	comparables := []Object{
		True,
		False,
		Undefined,
		Int(-1),
		Int(0),
		Int(1),
		Uint(0),
		Uint(1),
		Char(0),
		Char(1),
		Char('x'),
		Float(0),
		Float(1),
//		String(""),
//		String("x"),
	}
	for i := range comparables {
		for j := range comparables {
			if i != j {
				require.True(t, comparables[i] != comparables[j],
					"%T and %T must be not equal", comparables[i], comparables[j])
			} else {
				require.True(t, comparables[i] == comparables[j],
					"%T and %T must be equal", comparables[i], comparables[j])
			}
		}
	}
}

func TestObjectIterable(t *testing.T) {
	require.True(t, Int(0).CanIterate())
	require.True(t, Uint(0).CanIterate())
	require.False(t, Char(0).CanIterate())
	require.False(t, Float(0).CanIterate())
	require.False(t, Bool(true).CanIterate())
	require.False(t, Undefined.CanIterate())
	require.False(t, (&Error{}).CanIterate())
	require.False(t, (&RuntimeError{}).CanIterate())
	require.False(t, (&Function{}).CanIterate())
	require.False(t, (&BuiltinFunction{}).CanIterate())
	require.False(t, (&CompiledFunction{}).CanIterate())

//	require.Nil(t, Int(0).Iterate())
//	require.Nil(t, Uint(0).Iterate())
	require.Nil(t, Char(0).Iterate())
	require.Nil(t, Float(0).Iterate())
	require.Nil(t, Bool(true).Iterate())
	require.Nil(t, Undefined.Iterate())
	require.Nil(t, (&Error{}).Iterate())
	require.Nil(t, (&RuntimeError{}).Iterate())
	require.Nil(t, (&Function{}).Iterate())
	require.Nil(t, (&BuiltinFunction{}).Iterate())
	require.Nil(t, (&CompiledFunction{}).Iterate())

	require.True(t, String("").CanIterate())
	require.True(t, Array{}.CanIterate())
	require.True(t, Bytes{}.CanIterate())
	require.True(t, Map{}.CanIterate())
	require.True(t, (&SyncMap{}).CanIterate())

	require.NotNil(t, String("").Iterate())
	require.NotNil(t, Array{}.Iterate())
	require.NotNil(t, Bytes{}.Iterate())
	require.NotNil(t, Map{}.Iterate())
	require.NotNil(t, (&SyncMap{}).Iterate())
}

func TestObjectCallable(t *testing.T) {
	require.False(t, Int(0).CanCall())
	require.False(t, Uint(0).CanCall())
	require.False(t, Char(0).CanCall())
	require.False(t, Float(0).CanCall())
	require.False(t, Bool(true).CanCall())
	require.False(t, Undefined.CanCall())
	require.False(t, (&Error{}).CanCall())
	require.False(t, (&RuntimeError{}).CanCall())
	require.False(t, String("").CanCall())
	require.False(t, Array{}.CanCall())
	require.False(t, Bytes{}.CanCall())
	require.False(t, Map{}.CanCall())
	require.False(t, (&SyncMap{}).CanCall())

	require.True(t, (&Function{}).CanCall())
	require.True(t, (&BuiltinFunction{}).CanCall())
	require.True(t, (&CompiledFunction{}).CanCall())

	v, err := Int(0).Call()
	require.Nil(t, v)
	require.Equal(t, ErrNotCallable, err)
	v, err = Uint(0).Call()
	require.Nil(t, v)
	require.Equal(t, ErrNotCallable, err)
	v, err = Char(0).Call()
	require.Nil(t, v)
	require.Equal(t, ErrNotCallable, err)
	v, err = Float(0).Call()
	require.Nil(t, v)
	require.Equal(t, ErrNotCallable, err)
	v, err = Bool(true).Call()
	require.Nil(t, v)
	require.Equal(t, ErrNotCallable, err)
	v, err = Undefined.Call()
	require.Nil(t, v)
	require.Equal(t, ErrNotCallable, err)
	v, err = (&Error{}).Call()
	require.Nil(t, v)
	require.Equal(t, ErrNotCallable, err)
	v, err = (&RuntimeError{}).Call()
	require.Nil(t, v)
	require.Equal(t, ErrNotCallable, err)
	v, err = String("").Call()
	require.Nil(t, v)
	require.Equal(t, ErrNotCallable, err)
	v, err = Array{}.Call()
	require.Nil(t, v)
	require.Equal(t, ErrNotCallable, err)
	v, err = Bytes{}.Call()
	require.Nil(t, v)
	require.Equal(t, ErrNotCallable, err)
	v, err = Map{}.Call()
	require.Nil(t, v)
	require.Equal(t, ErrNotCallable, err)
	v, err = (&SyncMap{}).Call()
	require.Nil(t, v)
	require.Equal(t, ErrNotCallable, err)
}

func TestObjectString(t *testing.T) {
	require.Equal(t, "0", Int(0).String())
	require.Equal(t, "0", Uint(0).String())
	require.Equal(t, "\x00", Char(0).String())
	require.Equal(t, "0", Float(0).String())
	require.Equal(t, "true", Bool(true).String())
	require.Equal(t, "false", Bool(false).String())
	require.Equal(t, "undefined", Undefined.String())

	require.Equal(t, "error: ", (&Error{}).String())
	require.Equal(t, "error: message", (&Error{Message: "message"}).String())
	require.Equal(t, "name: message", (&Error{Name: "name", Message: "message"}).String())

	require.Equal(t, "<nil>", (&RuntimeError{}).String())

	require.Equal(t, "", String("").String())
	require.Equal(t, "xyz", String("xyz").String())

	require.Equal(t, "[]", Array{}.String())
	require.Equal(t, `[1, "x", 1.1]`, Array{Int(1), String("x"), Float(1.1)}.String())

	require.Equal(t, "", Bytes{}.String())
	require.Equal(t, "\x00\x01", Bytes{0, 1}.String())
	require.Equal(t, "xyz", Bytes(String("xyz")).String())
	require.Equal(t, String("xyz").String(), Bytes(String("xyz")).String())

	require.Equal(t, "{}", Map{}.String())
	m := Map{"a": Int(1)}
	require.Equal(t, `{"a": 1}`, m.String())
	require.Equal(t, "{}", (&SyncMap{}).String())
	require.Equal(t, m.String(), (&SyncMap{Value: m}).String())
	require.Equal(t, "{}", (&SyncMap{Value: Map{}}).String())

	require.Equal(t, "<function:>", (&Function{}).String())
	require.Equal(t, "<function:xyz>", (&Function{Name: "xyz"}).String())
	require.Equal(t, "<builtinFunction:>", (&BuiltinFunction{}).String())
	require.Equal(t, "<builtinFunction:abc>", (&BuiltinFunction{Name: "abc"}).String())
	require.Equal(t, "<compiledFunction>", (&CompiledFunction{}).String())
}

func TestObjectTypeName(t *testing.T) {
	require.Equal(t, "int", Int(0).TypeName())
	require.Equal(t, "uint", Uint(0).TypeName())
	require.Equal(t, "char", Char(0).TypeName())
	require.Equal(t, "float", Float(0).TypeName())
	require.Equal(t, "bool", Bool(true).TypeName())
	require.Equal(t, "undefined", Undefined.TypeName())
	require.Equal(t, "error", (&Error{}).TypeName())
	require.Equal(t, "error", (&RuntimeError{}).TypeName())
	require.Equal(t, "string", String("").TypeName())
	require.Equal(t, "array", Array{}.TypeName())
	require.Equal(t, "bytes", Bytes{}.TypeName())
	require.Equal(t, "map", Map{}.TypeName())
	require.Equal(t, "syncMap", (&SyncMap{}).TypeName())
	require.Equal(t, "function", (&Function{}).TypeName())
	require.Equal(t, "builtinFunction", (&BuiltinFunction{}).TypeName())
	require.Equal(t, "compiledFunction", (&CompiledFunction{}).TypeName())
}

func TestObjectIsFalsy(t *testing.T) {
	require.True(t, Int(0).IsFalsy())
	require.True(t, Uint(0).IsFalsy())
	require.True(t, Char(0).IsFalsy())
	require.False(t, Float(0).IsFalsy())
	require.True(t, Float(math.NaN()).IsFalsy())
	require.False(t, Bool(true).IsFalsy())
	require.True(t, Bool(false).IsFalsy())
	require.True(t, Undefined.IsFalsy())
	require.True(t, (&Error{}).IsFalsy())
	require.True(t, (&RuntimeError{}).IsFalsy())
	require.True(t, String("").IsFalsy())
	require.False(t, String("x").IsFalsy())
	require.True(t, Array{}.IsFalsy())
	require.False(t, Array{Int(0)}.IsFalsy())
	require.True(t, Bytes{}.IsFalsy())
	require.False(t, Bytes{0}.IsFalsy())
	require.True(t, Map{}.IsFalsy())
	require.False(t, Map{"a": Int(1)}.IsFalsy())
	require.True(t, (&SyncMap{}).IsFalsy())
	require.False(t, (&SyncMap{Value: Map{"a": Int(1)}}).IsFalsy())
//	require.False(t, (&Function{}).IsFalsy())
//	require.False(t, (&BuiltinFunction{}).IsFalsy())
//	require.False(t, (&CompiledFunction{}).IsFalsy())
}

func TestObjectCopier(t *testing.T) {
	objects := []Object{
		Array{},
		Bytes{},
		Map{},
		&SyncMap{},
	}
	for _, o := range objects {
		if _, ok := o.(Copier); !ok {
			t.Fatalf("%T must implement Copier interface", o)
		}
	}
}

func TestObjectImpl(t *testing.T) {
	var o interface{} = ObjectImpl{}
	if _, ok := o.(Object); !ok {
		t.Fatal("ObjectImpl must implement Object interface")
	}
	impl := ObjectImpl{}
	require.Panics(t, func() { _ = impl.String() })
	require.Panics(t, func() { _ = impl.TypeName() })
	require.False(t, impl.Equal(impl))
	require.True(t, impl.IsFalsy())
	require.False(t, impl.CanCall())
	v, err := impl.Call()
	require.Nil(t, v)
	require.Equal(t, ErrNotCallable, err)
	require.False(t, impl.CanIterate())
	require.Nil(t, impl.Iterate())
	v, err = impl.BinaryOp(token.Add, Int(0))
	require.Nil(t, v)
	require.NotNil(t, err)
	require.Equal(t, ErrInvalidOperator, err)

	v, err = impl.IndexGet(Undefined)
	require.Nil(t, v)
	require.NotNil(t, err)
	require.Equal(t, ErrNotIndexable, err)

	err = impl.IndexSet(Undefined, Undefined)
	require.NotNil(t, err)
	require.Equal(t, ErrNotIndexAssignable, err)
}

func TestObjectIndexGet(t *testing.T) {
	v, err := Int(0).IndexGet(Undefined)
	require.Nil(t, v)
	require.Equal(t, ErrNotIndexable, err)

	v, err = Uint(0).IndexGet(Undefined)
	require.Nil(t, v)
	require.Equal(t, ErrNotIndexable, err)

	v, err = Char(0).IndexGet(Undefined)
	require.Nil(t, v)
	require.Equal(t, ErrNotIndexable, err)

	v, err = Float(0).IndexGet(Undefined)
	require.Nil(t, v)
	require.Equal(t, ErrNotIndexable, err)

	v, err = Bool(true).IndexGet(Undefined)
	require.Nil(t, v)
	require.Equal(t, ErrNotIndexable, err)

	v, err = (&Function{}).IndexGet(Undefined)
	require.Nil(t, v)
	require.Equal(t, ErrNotIndexable, err)

//	v, err = (&BuiltinFunction{}).IndexGet(Undefined)
//	require.Nil(t, v)
//	require.Equal(t, ErrNotIndexable, err)

	v, err = (&CompiledFunction{}).IndexGet(Undefined)
	require.Nil(t, v)
	require.Equal(t, ErrNotIndexable, err)

	v, err = Undefined.IndexGet(Undefined)
	require.Equal(t, Undefined, v)
	require.NoError(t, err)

	v, err = (&Error{}).IndexGet(Undefined)
	require.NoError(t, err)
	require.Equal(t, Undefined, v)

	v, err = (&Error{}).IndexGet(String("Name"))
	require.NoError(t, err)
	require.Equal(t, String(""), v)

	v, err = (&Error{Name: "x"}).IndexGet(String("Name"))
	require.NoError(t, err)
	require.Equal(t, String("x"), v)

	v, err = (&Error{}).IndexGet(String("Message"))
	require.NoError(t, err)
	require.Equal(t, String(""), v)

	v, err = (&Error{Message: "x"}).IndexGet(String("Message"))
	require.NoError(t, err)
	require.Equal(t, String("x"), v)

	v, err = (&RuntimeError{}).IndexGet(Undefined)
	require.Equal(t, Undefined, v)
	require.NoError(t, err)

	v, err = (&RuntimeError{Err: &Error{}}).IndexGet(String("Name"))
	require.NoError(t, err)
	require.Equal(t, String(""), v)

	v, err = (&RuntimeError{Err: &Error{Name: "x"}}).IndexGet(String("Name"))
	require.NoError(t, err)
	require.Equal(t, String("x"), v)

	v, err = (&RuntimeError{Err: &Error{}}).IndexGet(String("Message"))
	require.NoError(t, err)
	require.Equal(t, String(""), v)

	v, err = (&RuntimeError{Err: &Error{Message: "x"}}).IndexGet(String("Message"))
	require.NoError(t, err)
	require.Equal(t, String("x"), v)

	v, err = String("").IndexGet(Undefined)
	require.Nil(t, v)
	require.NotNil(t, err)
//	fmt.Printf("err is %#v -> %#v\n", err, ErrType)
//	require.True(t, errors.Is(err, ErrType))

	v, err = String("x").IndexGet(Int(0))
	require.NotNil(t, v)
	require.Nil(t, err)
	require.Equal(t, Int("x"[0]), v)

	v, err = String("x").IndexGet(Int(0))
	require.NotNil(t, v)
	require.Nil(t, err)
	require.Equal(t, Int("x"[0]), v)

	v, err = String("x").IndexGet(Int(1))
	require.Nil(t, v)
	require.Equal(t, ErrIndexOutOfBounds, err)

	v, err = Array{Int(1)}.IndexGet(Undefined)
	require.NotNil(t, err)
	require.Nil(t, v)
//	require.True(t, errors.Is(err, ErrType))

	v, err = Array{Int(1)}.IndexGet(Int(0))
	require.NotNil(t, v)
	require.Nil(t, err)
	require.Equal(t, Int(1), v)

	v, err = Array{Int(1)}.IndexGet(Int(1))
	require.Nil(t, v)
	require.NotNil(t, err)
	require.Equal(t, ErrIndexOutOfBounds, err)

	v, err = Bytes{1}.IndexGet(Undefined)
	require.NotNil(t, err)
	require.Nil(t, v)
//	require.True(t, err.Name == ErrType.Name)

	v, err = Bytes{1}.IndexGet(Int(0))
	require.NotNil(t, v)
	require.Nil(t, err)
	require.Equal(t, Int(1), v)

	v, err = Bytes{1}.IndexGet(Int(1))
	require.Nil(t, v)
	require.NotNil(t, err)
	require.Equal(t, ErrIndexOutOfBounds, err)

	v, err = Map{}.IndexGet(Undefined)
	require.Nil(t, err)
	require.Equal(t, Undefined, v)

	v, err = Map{"a": Int(1)}.IndexGet(Int(0))
	require.Nil(t, err)
	require.Equal(t, Undefined, v)

	v, err = Map{"a": Int(1)}.IndexGet(String("a"))
	require.Nil(t, err)
	require.Equal(t, Int(1), v)

	v, err = (&SyncMap{Value: Map{}}).IndexGet(Undefined)
	require.Nil(t, err)
	require.Equal(t, Undefined, v)

	v, err = (&SyncMap{Value: Map{"a": Int(1)}}).IndexGet(Int(0))
	require.Nil(t, err)
	require.Equal(t, Undefined, v)

	v, err = (&SyncMap{Value: Map{"a": Int(1)}}).IndexGet(String("a"))
	require.Nil(t, err)
	require.Equal(t, Int(1), v)
}

func TestObjectIndexSet(t *testing.T) {
	err := Int(0).IndexSet(Undefined, Undefined)
	require.Equal(t, ErrNotIndexAssignable, err)

	err = Uint(0).IndexSet(Undefined, Undefined)
	require.Equal(t, ErrNotIndexAssignable, err)

	err = Char(0).IndexSet(Undefined, Undefined)
	require.Equal(t, ErrNotIndexAssignable, err)

	err = Float(0).IndexSet(Undefined, Undefined)
	require.Equal(t, ErrNotIndexAssignable, err)

	err = Bool(true).IndexSet(Undefined, Undefined)
	require.Equal(t, ErrNotIndexAssignable, err)

	err = (&Function{}).IndexSet(Undefined, Undefined)
	require.Equal(t, ErrNotIndexAssignable, err)

	err = (&BuiltinFunction{}).IndexSet(Undefined, Undefined)
	require.Equal(t, ErrNotIndexAssignable, err)

	err = (&CompiledFunction{}).IndexSet(Undefined, Undefined)
	require.Equal(t, ErrNotIndexAssignable, err)

	err = Undefined.IndexSet(Undefined, Undefined)
	require.Equal(t, ErrNotIndexAssignable, err)

	err = (&Error{}).IndexSet(Undefined, Undefined)
	require.Equal(t, ErrNotIndexAssignable, err)

	err = (&RuntimeError{}).IndexSet(Undefined, Undefined)
	require.Equal(t, ErrNotIndexAssignable, err)

	err = (&RuntimeError{Err: &Error{}}).IndexSet(Undefined, Undefined)
	require.Equal(t, ErrNotIndexAssignable, err)

	err = String("x").IndexSet(Int(0), String("y"))
	require.Equal(t, ErrNotIndexAssignable, err)

	var v Object = Array{Int(1)}
	err = v.IndexSet(Int(0), Int(2))
	require.NoError(t, err)
	require.Equal(t, Int(2), v.(Array)[0])

	v = Array{Int(1)}
	err = v.IndexSet(Int(1), Int(3))
	require.Equal(t, ErrIndexOutOfBounds, err)
	require.Equal(t, Array{Int(1)}, v)

	v = Array{Int(1)}
	err = v.IndexSet(String("x"), Int(3))
	require.Error(t, err)
//	fmt.Printf("err is %#v\n", err)
//	require.True(t, errors.Is(err, ErrType))

	v = Bytes{1}
	err = v.IndexSet(Int(0), Int(2))
	require.NoError(t, err)
	require.Equal(t, byte(2), v.(Bytes)[0])

	v = Bytes{1}
	err = v.IndexSet(Int(1), Int(2))
	require.Error(t, err)
	require.Equal(t, ErrIndexOutOfBounds, err)

	v = Bytes{1}
	err = v.IndexSet(Int(0), String(""))
	require.Error(t, err)
//	fmt.Printf("err is %#v -> %#v\n", err, ErrType)
//	require.True(t, errors.Is(err, ErrType))

	v = Bytes{1}
	err = v.IndexSet(String("x"), Int(1))
	require.Error(t, err)
//	require.True(t, err.Name == ErrType.Name)

	v = Map{}
	err = v.IndexSet(Undefined, Undefined)
	require.Nil(t, err)
	require.Equal(t, Undefined, v.(Map)["undefined"])

	v = Map{"a": Int(1)}
	err = v.IndexSet(String("a"), Int(2))
	require.Nil(t, err)
	require.Equal(t, Int(2), v.(Map)["a"])

	v = &SyncMap{Value: Map{}}
	err = v.IndexSet(Undefined, Undefined)
	require.Nil(t, err)
	require.Equal(t, Undefined, v.(*SyncMap).Value["undefined"])

	v = &SyncMap{Value: Map{"a": Int(1)}}
	err = v.IndexSet(String("a"), Int(2))
	require.Nil(t, err)
	require.Equal(t, Int(2), v.(*SyncMap).Value["a"])
}
