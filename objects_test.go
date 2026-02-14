package charlang

import (
//	"errors"
	"bytes"
	"fmt"
	"math"
	"strings"
	"testing"
	"time"

	"github.com/stretchr/testify/require"

//	. "github.com/topxeq/charlang"
	"github.com/topxeq/charlang/token"
	tk "github.com/topxeq/tkc"
)

func TestObjects4(t *testing.T) {
	var tmpr Object
	var err error
//	var ok bool

	// *Any
	obj3 := &Any{Value: "Abc"}
	
	require.Equal(t, "999", fmt.Sprintf("%v", obj3.TypeCode()))
	
	require.Equal(t, "any", fmt.Sprintf("%v", obj3.TypeName()))
	
	require.Equal(t, "(any:string)Abc", fmt.Sprintf("%v", obj3.String()))
	
	require.True(t, obj3.HasMemeber())
	
	tmpr, err = obj3.CallMethod("value")
	
	require.Equal(t, "(any:string)Abc-<nil>", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "*charlang.Any", fmt.Sprintf("%T", obj3.GetValue()))
	
	require.Equal(t, "undefined", fmt.Sprintf("%v", obj3.GetMember("a")))
	
	require.Equal(t, "<nil>", fmt.Sprintf("%v", obj3.SetMember("a", ToStringObject("b"))))
	
	require.Equal(t, "false", fmt.Sprintf("%v", obj3.Equal(Int(1))))
	
	require.Equal(t, "false", fmt.Sprintf("%v", obj3.IsFalsy()))
	
	require.Equal(t, "false", fmt.Sprintf("%v", obj3.CanCall()))
	
	tmpr, err = obj3.Call(ToStringObject("value1"))
	
	require.Equal(t, "<nil>-NotCallableError: ", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "false", fmt.Sprintf("%v", obj3.CanIterate()))
	
	require.Equal(t, "<nil>", fmt.Sprintf("%T", obj3.Iterate()))
	
	require.Equal(t, "NotIndexAssignableError: ", fmt.Sprintf("%v", obj3.IndexSet(Int(1), Int(1))))
	
	tmpr, err = obj3.IndexGet(ToStringObject("value1"))
	
	require.Equal(t, "undefined-error: not indexable: any", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = obj3.BinaryOp(token.Add, Int(3))

	require.Equal(t, "<nil>-InvalidOperatorError: ", fmt.Sprintf("%v-%v", tmpr, err))

	require.Equal(t, "{}", tk.ToJSONX(tk.FromJSONX((fmt.Sprintf("%v", obj3.String()))), "-sort"))
	
	require.Equal(t, "(any:string)Abc", fmt.Sprintf("%v", obj3.Copy()))
	
	err = obj3.SetValue(String("abc"))

	require.Equal(t, "(any:string)abc-<nil>", fmt.Sprintf("%v-%v", obj3, err))

	tmpr, err = obj3.CallName("Format", Call{Args: []Object{String("abc")}})

	require.Equal(t, "error: [pos: ]method(Format) not found for type: *charlang.Any(string)-<nil>", fmt.Sprintf("%v-%v", tmpr, err))

	// *Database
	obj1 := &Database{Value: nil}
	
	require.Equal(t, "309", fmt.Sprintf("%v", obj1.TypeCode()))
	
	require.Equal(t, "database", fmt.Sprintf("%v", obj1.TypeName()))
	
	require.Equal(t, "(database)<nil>", fmt.Sprintf("%v", obj1.String()))
	
	require.True(t, obj1.HasMemeber())
	
	tmpr, err = obj1.CallMethod("value")
	
	require.Equal(t, "(database)<nil>-<nil>", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "*charlang.Database", fmt.Sprintf("%T", obj1.GetValue()))
	
	require.Equal(t, "undefined", fmt.Sprintf("%v", obj1.GetMember("a")))
	
	require.Equal(t, "<nil>", fmt.Sprintf("%v", obj1.SetMember("a", ToStringObject("b"))))
	
	require.Equal(t, "false", fmt.Sprintf("%v", obj1.Equal(Int(1))))
	
	require.Equal(t, "true", fmt.Sprintf("%v", obj1.IsFalsy()))
	
	require.Equal(t, "false", fmt.Sprintf("%v", obj1.CanCall()))
	
	tmpr, err = obj1.Call(ToStringObject("value1"))
	
	require.Equal(t, "<nil>-NotCallableError: ", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "false", fmt.Sprintf("%v", obj1.CanIterate()))
	
	require.Equal(t, "<nil>", fmt.Sprintf("%T", obj1.Iterate()))
	
	require.Equal(t, "NotIndexAssignableError: ", fmt.Sprintf("%v", obj1.IndexSet(Int(1), Int(1))))
	
	tmpr, err = obj1.IndexGet(ToStringObject("value1"))
	
	require.Equal(t, "undefined-error: not indexable: database", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = obj1.BinaryOp(token.Add, Int(3))

	require.Equal(t, "<nil>-InvalidOperatorError: ", fmt.Sprintf("%v-%v", tmpr, err))

	require.Equal(t, "{}", tk.ToJSONX(tk.FromJSONX((fmt.Sprintf("%v", obj1.String()))), "-sort"))
	
	// *StatusResult
	obj2 := &StatusResult{Status: "success", Value: ""}
	
	require.Equal(t, "303", fmt.Sprintf("%v", obj2.TypeCode()))
	
	require.Equal(t, "statusResult", fmt.Sprintf("%v", obj2.TypeName()))
	
	require.Equal(t, "{\"Status\": \"success\", \"Value\": \"\"}", fmt.Sprintf("%v", obj2.String()))
	
	require.True(t, obj2.HasMemeber())
	
	tmpr, err = obj2.CallMethod("value")
	
	require.Equal(t, "{\"Status\": \"success\", \"Value\": \"\"}-<nil>", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "*charlang.StatusResult", fmt.Sprintf("%T", obj2.GetValue()))
	
	require.Equal(t, "undefined", fmt.Sprintf("%v", obj2.GetMember("a")))
	
	require.Equal(t, "<nil>", fmt.Sprintf("%v", obj2.SetMember("a", ToStringObject("b"))))
	
	require.Equal(t, "false", fmt.Sprintf("%v", obj2.Equal(Int(1))))
	
	require.Equal(t, "true", fmt.Sprintf("%v", obj2.IsFalsy()))
	
	require.Equal(t, "false", fmt.Sprintf("%v", obj2.CanCall()))
	
	tmpr, err = obj2.Call(ToStringObject("value1"))
	
	require.Equal(t, "<nil>-NotCallableError: ", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "false", fmt.Sprintf("%v", obj2.CanIterate()))
	
	require.Equal(t, "<nil>", fmt.Sprintf("%T", obj2.Iterate()))
	
	require.Equal(t, "NotIndexAssignableError: ", fmt.Sprintf("%v", obj2.IndexSet(Int(1), Int(1))))
	
	tmpr, err = obj2.IndexGet(ToStringObject("value1"))
	
	require.Equal(t, "undefined-error: not indexable: statusResult", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = obj2.BinaryOp(token.Add, Int(3))

	require.Equal(t, "<nil>-InvalidOperatorError: ", fmt.Sprintf("%v-%v", tmpr, err))

	require.Equal(t, "{\"Status\":\"success\",\"Value\":\"\"}", tk.ToJSONX(tk.FromJSONX((fmt.Sprintf("%v", obj2.String()))), "-sort"))
	
	tmpr, err = GenStatusResult(String(`{"Status": "fail", "Value": "quick reason"}`))

	require.Equal(t, "{\"Status\": \"fail\", \"Value\": \"quick reason\"}-<nil>", fmt.Sprintf("%v-%v", tmpr, err))

	// *StringBuilder
	obj4 := &StringBuilder{Value: new(strings.Builder)}
	
	require.Equal(t, "307", fmt.Sprintf("%v", obj4.TypeCode()))
	
	require.Equal(t, "stringBuilder", fmt.Sprintf("%v", obj4.TypeName()))
	
	require.Equal(t, "(stringBuilder)", fmt.Sprintf("%v", obj4.String()))
	
	require.True(t, obj4.HasMemeber())
	
	tmpr, err = obj4.CallMethod("value")
	
	require.Equal(t, "-<nil>", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "charlang.String", fmt.Sprintf("%T", obj4.GetValue()))
	
	require.Equal(t, "undefined", fmt.Sprintf("%v", obj4.GetMember("a")))
	
	require.Equal(t, "<nil>", fmt.Sprintf("%v", obj4.SetMember("a", ToStringObject("b"))))
	
	require.Equal(t, "false", fmt.Sprintf("%v", obj4.Equal(Int(1))))
	
	require.Equal(t, "false", fmt.Sprintf("%v", obj4.IsFalsy()))
	
	require.Equal(t, "false", fmt.Sprintf("%v", obj4.CanCall()))
	
	tmpr, err = obj4.Call(ToStringObject("value1"))
	
	require.Equal(t, "<nil>-NotCallableError: ", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "false", fmt.Sprintf("%v", obj4.CanIterate()))
	
	require.Equal(t, "<nil>", fmt.Sprintf("%T", obj4.Iterate()))
	
	require.Equal(t, "NotIndexAssignableError: ", fmt.Sprintf("%v", obj4.IndexSet(Int(1), Int(1))))
	
	tmpr, err = obj4.IndexGet(ToStringObject("value1"))
	
	require.Equal(t, "undefined-error: method(value1) not found for type: stringBuilder", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = obj4.BinaryOp(token.Add, Int(3))

	require.Equal(t, "<nil>-InvalidOperatorError: ", fmt.Sprintf("%v-%v", tmpr, err))

	require.Equal(t, "{}", tk.ToJSONX(tk.FromJSONX((fmt.Sprintf("%v", obj4.String()))), "-sort"))
	
	tmpr, err = obj4.CallName("writeStr", Call{Args: []Object{String("abc")}})

	require.Equal(t, "3-<nil>", fmt.Sprintf("%v-%v", tmpr, err))

	require.Equal(t, "(stringBuilder)abc", fmt.Sprintf("%v", obj4.Copy()))
	
	// *BytesBuffer
	obj5 := &BytesBuffer{Value: new(bytes.Buffer)}
	
	require.Equal(t, "308", fmt.Sprintf("%v", obj5.TypeCode()))
	
	require.Equal(t, "bytesBuffer", fmt.Sprintf("%v", obj5.TypeName()))
	
	require.Equal(t, "{[] 0 0}", fmt.Sprintf("%v", obj5.String()))
	
	require.True(t, obj5.HasMemeber())
	
	tmpr, err = obj5.CallMethod("value")
	
	require.Equal(t, "{[] 0 0}-<nil>", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "charlang.Bytes", fmt.Sprintf("%T", obj5.GetValue()))
	
	require.Equal(t, "undefined", fmt.Sprintf("%v", obj5.GetMember("a")))
	
	require.Equal(t, "<nil>", fmt.Sprintf("%v", obj5.SetMember("a", ToStringObject("b"))))
	
	require.Equal(t, "false", fmt.Sprintf("%v", obj5.Equal(Int(1))))
	
	require.Equal(t, "false", fmt.Sprintf("%v", obj5.IsFalsy()))
	
	require.Equal(t, "false", fmt.Sprintf("%v", obj5.CanCall()))
	
	tmpr, err = obj5.Call(ToStringObject("value1"))
	
	require.Equal(t, "<nil>-NotCallableError: ", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "false", fmt.Sprintf("%v", obj5.CanIterate()))
	
	require.Equal(t, "<nil>", fmt.Sprintf("%T", obj5.Iterate()))
	
	require.Equal(t, "NotIndexAssignableError: ", fmt.Sprintf("%v", obj5.IndexSet(Int(1), Int(1))))
	
	tmpr, err = obj5.IndexGet(ToStringObject("value1"))
	
	require.Equal(t, "undefined-error: method(value1) not found for type: bytesBuffer", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = obj5.BinaryOp(token.Add, Int(3))

	require.Equal(t, "<nil>-InvalidOperatorError: ", fmt.Sprintf("%v-%v", tmpr, err))

	require.Equal(t, "{}", tk.ToJSONX(tk.FromJSONX((fmt.Sprintf("%v", obj5.String()))), "-sort"))
	
//	tmpr, err = obj5.CallName("writeStr", Call{Args: []Object{String("abc")}})

//	require.Equal(t, "3-<nil>", fmt.Sprintf("%v-%v", tmpr, err))

	require.Equal(t, "{[] 0 0}", fmt.Sprintf("%v", obj5.Copy()))
	
	// *ObjectRef
	var container1 Object = String("abc")
	
	obj6 := &ObjectRef{Value: &container1}
	
	require.Equal(t, "152", fmt.Sprintf("%v", obj6.TypeCode()))
	
	require.Equal(t, "objectRef", fmt.Sprintf("%v", obj6.TypeName()))
	
	require.Equal(t, "<objectRef:abc>", fmt.Sprintf("%v", obj6.String()))
	
	require.True(t, obj6.HasMemeber())
	
	tmpr, err = obj6.CallMethod("value")
	
	require.Equal(t, "<objectRef:abc>-<nil>", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "*charlang.ObjectRef", fmt.Sprintf("%T", obj6.GetValue()))
	
	require.Equal(t, "undefined", fmt.Sprintf("%v", obj6.GetMember("a")))
	
	require.Equal(t, "<nil>", fmt.Sprintf("%v", obj6.SetMember("a", ToStringObject("b"))))
	
	require.Equal(t, "false", fmt.Sprintf("%v", obj6.Equal(Int(1))))
	
	require.Equal(t, "false", fmt.Sprintf("%v", obj6.IsFalsy()))
	
	require.Equal(t, "false", fmt.Sprintf("%v", obj6.CanCall()))
	
	tmpr, err = obj6.Call(ToStringObject("value1"))
	
	require.Equal(t, "<nil>-NotCallableError: ", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "false", fmt.Sprintf("%v", obj6.CanIterate()))
	
	require.Equal(t, "<nil>", fmt.Sprintf("%T", obj6.Iterate()))
	
	require.Equal(t, "NotIndexAssignableError: ", fmt.Sprintf("%v", obj6.IndexSet(Int(1), Int(1))))
	
	tmpr, err = obj6.IndexGet(ToStringObject("value1"))
	
	require.Equal(t, "<nil>-NotIndexableError: ", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = obj6.BinaryOp(token.Add, Int(3))

	require.Equal(t, "abc3-<nil>", fmt.Sprintf("%v-%v", tmpr, err))

	require.Equal(t, "{}", tk.ToJSONX(tk.FromJSONX((fmt.Sprintf("%v", obj6.String()))), "-sort"))
	
//	tmpr, err = obj6.CallName("writeStr", Call{Args: []Object{String("abc")}})

//	require.Equal(t, "3-<nil>", fmt.Sprintf("%v-%v", tmpr, err))

	require.Equal(t, "<objectRef:abc>", fmt.Sprintf("%v", obj6.Copy()))
	
	// *MutableString
	obj7 := &MutableString{Value: "abc"}
	
	require.Equal(t, "106", fmt.Sprintf("%v", obj7.TypeCode()))
	
	require.Equal(t, "mutableString", fmt.Sprintf("%v", obj7.TypeName()))
	
	require.Equal(t, "abc", fmt.Sprintf("%v", obj7.String()))
	
	require.True(t, obj7.HasMemeber())
	
	tmpr, err = obj7.CallMethod("value")
	
	require.Equal(t, "abc-<nil>", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "*charlang.MutableString", fmt.Sprintf("%T", obj7.GetValue()))
	
	require.Equal(t, "undefined", fmt.Sprintf("%v", obj7.GetMember("a")))
	
	require.Equal(t, "<nil>", fmt.Sprintf("%v", obj7.SetMember("a", ToStringObject("b"))))
	
	require.Equal(t, "false", fmt.Sprintf("%v", obj7.Equal(Int(1))))
	
	require.Equal(t, "false", fmt.Sprintf("%v", obj7.IsFalsy()))
	
	require.Equal(t, "false", fmt.Sprintf("%v", obj7.CanCall()))
	
	tmpr, err = obj7.Call(ToStringObject("value1"))
	
	require.Equal(t, "<nil>-NotCallableError: ", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "true", fmt.Sprintf("%v", obj7.CanIterate()))
	
	require.Equal(t, "*charlang.MutableStringIterator", fmt.Sprintf("%T", obj7.Iterate()))
	
	require.Equal(t, "NotIndexAssignableError: ", fmt.Sprintf("%v", obj7.IndexSet(Int(1), Int(1))))
	
	tmpr, err = obj7.IndexGet(ToStringObject("value1"))
	
	require.Equal(t, "undefined-error: method(value1) not found for type: mutableString", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = obj7.BinaryOp(token.Add, Int(3))

	require.Equal(t, "abc3-<nil>", fmt.Sprintf("%v-%v", tmpr, err))

	require.Equal(t, "{}", tk.ToJSONX(tk.FromJSONX((fmt.Sprintf("%v", obj7.String()))), "-sort"))
	
	tmpr, err = obj7.CallName("writeStr", Call{Args: []Object{String("abc")}})

	require.Equal(t, "<nil>-error: unknown method: writeStr", fmt.Sprintf("%v-%v", tmpr, err))

	require.Equal(t, "abc", fmt.Sprintf("%v", obj7.Copy()))
	
	bb1, err := obj7.MarshalJSON()

	require.Equal(t, "[]uint8-<nil>", fmt.Sprintf("%T-%v", bb1, err))

	err = obj7.SetValue(String("123"))

	require.Equal(t, "123-<nil>", fmt.Sprintf("%v-%v", obj7, err))

	require.Equal(t, "3-<nil>", fmt.Sprintf("%v-%v", obj7.Len(), err))

	// *Seq
	obj8 := &Seq{Value: tk.NewSeq()}
	
	require.Equal(t, "315", fmt.Sprintf("%v", obj8.TypeCode()))
	
	require.Equal(t, "seq", fmt.Sprintf("%v", obj8.TypeName()))
	
	require.Equal(t, "0", fmt.Sprintf("%v", obj8.String()))
	
	require.True(t, obj8.HasMemeber())
	
	tmpr, err = obj8.CallMethod("value")
	
	require.Equal(t, "1-<nil>", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "2", fmt.Sprintf("%v", obj8.GetValue()))
	
	require.Equal(t, "2", fmt.Sprintf("%v", obj8.GetCurrentValue()))
	
	require.Equal(t, "undefined", fmt.Sprintf("%v", obj8.GetMember("a")))
	
	require.Equal(t, "<nil>", fmt.Sprintf("%v", obj8.SetMember("a", ToStringObject("b"))))
	
	require.Equal(t, "false", fmt.Sprintf("%v", obj8.Equal(Int(1))))
	
	require.Equal(t, "false", fmt.Sprintf("%v", obj8.IsFalsy()))
	
	require.Equal(t, "false", fmt.Sprintf("%v", obj8.CanCall()))
	
	tmpr, err = obj8.Call(ToStringObject("value1"))
	
	require.Equal(t, "<nil>-NotCallableError: ", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "false", fmt.Sprintf("%v", obj8.CanIterate()))
	
	require.Equal(t, "<nil>", fmt.Sprintf("%T", obj8.Iterate()))
	
	require.Equal(t, "NotIndexAssignableError: ", fmt.Sprintf("%v", obj8.IndexSet(Int(1), Int(1))))
	
	tmpr, err = obj8.IndexGet(ToStringObject("value1"))
	
	require.Equal(t, "undefined-error: method(value1) not found for type: seq", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = obj8.BinaryOp(token.Add, Int(3))

	require.Equal(t, "<nil>-TypeError: unsupported operand types for '+': 'seq' and 'int'", fmt.Sprintf("%v-%v", tmpr, err))

	require.Equal(t, "2", tk.ToJSONX(tk.FromJSONX((fmt.Sprintf("%v", obj8.String()))), "-sort"))
	
//	tmpr, err = obj8.CallName("writeStr", Call{Args: []Object{String("abc")}})
//
//	require.Equal(t, "<nil>-error: unknown method: writeStr", fmt.Sprintf("%v-%v", tmpr, err))

//	require.Equal(t, "abc", fmt.Sprintf("%v", obj8.Copy()))
	
//	bb1, err = obj8.MarshalJSON()
//
//	require.Equal(t, "[]uint8-<nil>", fmt.Sprintf("%T-%v", bb1, err))

	err = obj8.SetValue(String("123"))

	require.Equal(t, "123-<nil>", fmt.Sprintf("%v-%v", obj8, err))

//	require.Equal(t, "3-<nil>", fmt.Sprintf("%v-%v", obj8.Len(), err))

	// *Mutex
	obj9 := NewMutex() // &Mutex{Value: tk.NewSeq()}
	
	require.Equal(t, "317", fmt.Sprintf("%v", obj9.TypeCode()))
	
	require.Equal(t, "mutex", fmt.Sprintf("%v", obj9.TypeName()))
	
	require.Equal(t, "string", fmt.Sprintf("%T", obj9.String()))
	
	require.True(t, obj9.HasMemeber())
	
	tmpr, err = obj9.CallMethod("value")
	
	require.Equal(t, "charlang.String-<nil>", fmt.Sprintf("%T-%v", tmpr, err))
	
	require.Equal(t, "undefined", fmt.Sprintf("%v", obj9.GetValue()))
	
//	require.Equal(t, "2", fmt.Sprintf("%v", obj9.GetCurrentValue()))
	
	require.Equal(t, "undefined", fmt.Sprintf("%v", obj9.GetMember("a")))
	
	require.Equal(t, "<nil>", fmt.Sprintf("%v", obj9.SetMember("a", ToStringObject("b"))))
	
	require.Equal(t, "false", fmt.Sprintf("%v", obj9.Equal(Int(1))))
	
	require.Equal(t, "false", fmt.Sprintf("%v", obj9.IsFalsy()))
	
	require.Equal(t, "false", fmt.Sprintf("%v", obj9.CanCall()))
	
	tmpr, err = obj9.Call(ToStringObject("value1"))
	
	require.Equal(t, "<nil>-NotCallableError: ", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "false", fmt.Sprintf("%v", obj9.CanIterate()))
	
	require.Equal(t, "<nil>", fmt.Sprintf("%T", obj9.Iterate()))
	
	require.Equal(t, "NotIndexAssignableError: ", fmt.Sprintf("%v", obj9.IndexSet(Int(1), Int(1))))
	
	tmpr, err = obj9.IndexGet(ToStringObject("value1"))
	
	require.Equal(t, "undefined-error: method(value1) not found for type: mutex", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = obj9.BinaryOp(token.Add, Int(3))

	require.Equal(t, "<nil>-TypeError: unsupported operand types for '+': 'mutex' and 'int'", fmt.Sprintf("%v-%v", tmpr, err))

	require.Equal(t, "{}", tk.ToJSONX(tk.FromJSONX((fmt.Sprintf("%v", obj9.String()))), "-sort"))
	
//	tmpr, err = obj9.CallName("writeStr", Call{Args: []Object{String("abc")}})
//
//	require.Equal(t, "<nil>-error: unknown method: writeStr", fmt.Sprintf("%v-%v", tmpr, err))

//	require.Equal(t, "abc", fmt.Sprintf("%v", obj9.Copy()))
	
//	bb1, err = obj9.MarshalJSON()
//
//	require.Equal(t, "[]uint8-<nil>", fmt.Sprintf("%T-%v", bb1, err))

//	err = obj9.SetValue(String("123"))

//	require.Equal(t, "123-<nil>", fmt.Sprintf("%v-%v", obj9, err))

//	require.Equal(t, "3-<nil>", fmt.Sprintf("%v-%v", obj9.Len(), err))

	
}

func TestObjects3(t *testing.T) {
	var tmpr Object
	var err error
	var ok bool
	
	// Array
	ary1 := Array{Int(1), Int(2), Int(3)}
	
	require.Equal(t, "131", fmt.Sprintf("%v", ary1.TypeCode()))
	
	require.Equal(t, "array", fmt.Sprintf("%v", ary1.TypeName()))
	
	require.Equal(t, "[1, 2, 3]", fmt.Sprintf("%v", ary1.String()))
	
	require.False(t, ary1.HasMemeber())
	
	tmpr, err = ary1.CallMethod("value")
	
	require.Equal(t, "[1, 2, 3]-<nil>", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "[1, 2, 3]", fmt.Sprintf("%v", ary1.GetValue()))
	
	require.Equal(t, "undefined", fmt.Sprintf("%v", ary1.GetMember("a")))
	
	require.Equal(t, "unsupported action(set member)", fmt.Sprintf("%v", ary1.SetMember("a", ToStringObject("b"))))
	
	require.Equal(t, "[1, 2, 3]", fmt.Sprintf("%v", ary1.Copy()))
	
	require.Equal(t, "false", fmt.Sprintf("%v", ary1.Equal(Int(1))))
	
	require.Equal(t, "false", fmt.Sprintf("%v", ary1.IsFalsy()))
	
	require.Equal(t, "false", fmt.Sprintf("%v", ary1.CanCall()))
	
	tmpr, err = ary1.Call(ToStringObject("value1"))
	
	require.Equal(t, "<nil>-NotCallableError: ", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "true", fmt.Sprintf("%v", ary1.CanIterate()))
	
	require.Equal(t, "&{[1, 2, 3] 0}", fmt.Sprintf("%v", ary1.Iterate()))
	
	require.Equal(t, "<nil>", fmt.Sprintf("%v", ary1.IndexSet(Int(1), Int(1))))
	
	tmpr, err = ary1.IndexGet(ToStringObject("value1"))
	
	require.Equal(t, "undefined-error: not indexable: array", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = ary1.IndexGet(Int(2))
	
	require.Equal(t, "3-<nil>", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = ary1.IndexGet(Int(5))
	
	require.Equal(t, "<nil>-IndexOutOfBoundsError: ", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = ary1.BinaryOp(token.Add, Int(3))

	require.Equal(t, "[1, 1, 3, 3]-<nil>", fmt.Sprintf("%v-%v", tmpr, err))

	// *ObjectPtr
	objP1 := &ObjectPtr{Value: &tmpr}
	
	require.Equal(t, "151", fmt.Sprintf("%v", objP1.TypeCode()))
	
	require.Equal(t, "objectPtr", fmt.Sprintf("%v", objP1.TypeName()))
	
	require.Equal(t, "<objectPtr:[1, 1, 3, 3]>", fmt.Sprintf("%v", objP1.String()))
	
	require.True(t, objP1.HasMemeber())
	
	tmpr, err = objP1.CallMethod("value")
	
	require.Equal(t, "[1, 1, 3, 3]-<nil>", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "<objectPtr:[1, 1, 3, 3]>", fmt.Sprintf("%v", objP1.GetValue()))
	
	require.Equal(t, "undefined", fmt.Sprintf("%v", objP1.GetMember("a")))
	
	require.Equal(t, "<nil>", fmt.Sprintf("%v", objP1.SetMember("a", ToStringObject("b"))))
	
	require.Equal(t, "<objectPtr:[1, 1, 3, 3]>", fmt.Sprintf("%v", objP1.Copy()))
	
	require.Equal(t, "false", fmt.Sprintf("%v", objP1.Equal(Int(1))))
	
	require.Equal(t, "false", fmt.Sprintf("%v", objP1.IsFalsy()))
	
	require.Equal(t, "false", fmt.Sprintf("%v", objP1.CanCall()))
	
	tmpr, err = objP1.Call(ToStringObject("value1"))
	
	require.Equal(t, "<nil>-error: not callable", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "false", fmt.Sprintf("%v", objP1.CanIterate()))
	
	require.Equal(t, "<nil>", fmt.Sprintf("%v", objP1.Iterate()))
	
	require.Equal(t, "NotIndexAssignableError: ", fmt.Sprintf("%v", objP1.IndexSet(Int(1), Int(1))))
	
	tmpr, err = objP1.IndexGet(ToStringObject("value1"))
	
	require.Equal(t, "<nil>-NotIndexableError: ", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = objP1.IndexGet(Int(2))
	
	require.Equal(t, "<nil>-NotIndexableError: ", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = objP1.IndexGet(Int(5))
	
	require.Equal(t, "<nil>-NotIndexableError: ", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = objP1.BinaryOp(token.Add, Int(3))

	require.Equal(t, "<nil>-error: nil object pointer", fmt.Sprintf("%v-%v", tmpr, err))

	// Map
	map1 := Map{"k1": String("v1"), "k2": String("v2")}
	
	require.Equal(t, "133", fmt.Sprintf("%v", map1.TypeCode()))
	
	require.Equal(t, "map", fmt.Sprintf("%v", map1.TypeName()))
	
	require.Equal(t, "{\"k1\":\"v1\",\"k2\":\"v2\"}", tk.ToJSONX(tk.FromJSONX((fmt.Sprintf("%v", map1.String()))), "-sort"))
	
	require.False(t, map1.HasMemeber())
	
	tmpr, err = map1.CallMethod("value")
	
	require.Equal(t, "undefined-InvalidIndexError: value", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "charlang.Map", fmt.Sprintf("%T", map1.GetValue()))
	
	require.Equal(t, "undefined", fmt.Sprintf("%v", map1.GetMember("a")))
	
	require.Equal(t, "unsupported action(set member)", fmt.Sprintf("%v", map1.SetMember("a", ToStringObject("b"))))
	
	require.Equal(t, "charlang.Map", fmt.Sprintf("%T", map1.Copy()))
	
	require.Equal(t, "false", fmt.Sprintf("%v", map1.Equal(Int(1))))
	
	require.Equal(t, "false", fmt.Sprintf("%v", map1.IsFalsy()))
	
	require.Equal(t, "false", fmt.Sprintf("%v", map1.CanCall()))
	
	tmpr, err = map1.Call(ToStringObject("value1"))
	
	require.Equal(t, "<nil>-NotCallableError: ", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "true", fmt.Sprintf("%v", map1.CanIterate()))
	
	require.Equal(t, "*charlang.MapIterator", fmt.Sprintf("%T", map1.Iterate()))
	
	require.Equal(t, "<nil>", fmt.Sprintf("%v", map1.IndexSet(Int(1), Int(1))))
	
	tmpr, err = map1.IndexGet(ToStringObject("value1"))
	
	require.Equal(t, "undefined-<nil>", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = map1.IndexGet(ToStringObject("k1"))
	
	require.Equal(t, "v1-<nil>", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = map1.IndexGet(Int(2))
	
	require.Equal(t, "undefined-<nil>", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = map1.IndexGet(Int(5))
	
	require.Equal(t, "undefined-<nil>", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = map1.BinaryOp(token.Add, Int(3))

	require.Equal(t, "<nil>-TypeError: unsupported operand types for '+': 'map' and 'int'", fmt.Sprintf("%v-%v", tmpr, err))

	// *SyncMap
	smap1 := &SyncMap{Value: Map{"k1": String("v1")}}
	
	require.Equal(t, "153", fmt.Sprintf("%v", smap1.TypeCode()))
	
	require.Equal(t, "syncMap", fmt.Sprintf("%v", smap1.TypeName()))
	
	require.Equal(t, "{\"k1\": \"v1\"}", fmt.Sprintf("%v", smap1.String()))
	
	require.True(t, smap1.HasMemeber())
	
	tmpr, err = smap1.CallMethod("value")
	
	require.Equal(t, "{\"k1\": \"v1\"}-<nil>", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "*charlang.SyncMap", fmt.Sprintf("%T", smap1.GetValue()))
	
	require.Equal(t, "undefined", fmt.Sprintf("%v", smap1.GetMember("a")))
	
	require.Equal(t, "<nil>", fmt.Sprintf("%v", smap1.SetMember("a", ToStringObject("b"))))
	
	require.Equal(t, "*charlang.SyncMap", fmt.Sprintf("%T", smap1.Copy()))
	
	require.Equal(t, "false", fmt.Sprintf("%v", smap1.Equal(Int(1))))
	
	require.Equal(t, "false", fmt.Sprintf("%v", smap1.IsFalsy()))
	
	require.Equal(t, "false", fmt.Sprintf("%v", smap1.CanCall()))
	
	tmpr, err = smap1.Call(ToStringObject("value1"))
	
	require.Equal(t, "<nil>-NotCallableError: ", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "true", fmt.Sprintf("%v", smap1.CanIterate()))
	
	require.Equal(t, "*charlang.SyncIterator", fmt.Sprintf("%T", smap1.Iterate()))
	
	require.Equal(t, "<nil>", fmt.Sprintf("%v", smap1.IndexSet(Int(1), Int(1))))
	
	tmpr, err = smap1.IndexGet(ToStringObject("value1"))
	
	require.Equal(t, "undefined-<nil>", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = smap1.IndexGet(ToStringObject("k1"))
	
	require.Equal(t, "v1-<nil>", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = smap1.IndexGet(Int(2))
	
	require.Equal(t, "undefined-<nil>", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = smap1.IndexGet(Int(5))
	
	require.Equal(t, "undefined-<nil>", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = smap1.BinaryOp(token.Add, Int(3))

	require.Equal(t, "<nil>-TypeError: unsupported operand types for '+': 'map' and 'int'", fmt.Sprintf("%v-%v", tmpr, err))

	smap1.Lock()
	
	smap1.Unlock()
	
	require.Equal(t, "{\"1\":1,\"k1\":\"v1\"}", tk.ToJSONX(tk.FromJSONX((fmt.Sprintf("%v", smap1.String()))), "-sort"))
	
	// *Error
	err1 := NewCommonError("failed to proceed")
	
	require.Equal(t, "155", fmt.Sprintf("%v", err1.TypeCode()))
	
	require.Equal(t, "error", fmt.Sprintf("%v", err1.TypeName()))
	
	require.Equal(t, "error: failed to proceed", fmt.Sprintf("%v", err1.String()))
	
	require.True(t, err1.HasMemeber())
	
	tmpr, err = err1.CallMethod("value")
	
	require.Equal(t, "error: failed to proceed-<nil>", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "*charlang.Error", fmt.Sprintf("%T", err1.GetValue()))
	
	require.Equal(t, "undefined", fmt.Sprintf("%v", err1.GetMember("a")))
	
	require.Equal(t, "<nil>", fmt.Sprintf("%v", err1.SetMember("a", ToStringObject("b"))))
	
	require.Equal(t, "*charlang.Error", fmt.Sprintf("%T", err1.Copy()))
	
	require.Equal(t, "false", fmt.Sprintf("%v", err1.Equal(Int(1))))
	
	require.Equal(t, "true", fmt.Sprintf("%v", err1.IsFalsy()))
	
	require.Equal(t, "false", fmt.Sprintf("%v", err1.CanCall()))
	
	tmpr, err = err1.Call(ToStringObject("value1"))
	
	require.Equal(t, "<nil>-NotCallableError: ", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "false", fmt.Sprintf("%v", err1.CanIterate()))
	
	require.Equal(t, "<nil>", fmt.Sprintf("%T", err1.Iterate()))
	
	require.Equal(t, "NotIndexAssignableError: ", fmt.Sprintf("%v", err1.IndexSet(Int(1), Int(1))))
	
	tmpr, err = err1.IndexGet(ToStringObject("value1"))
	
	require.Equal(t, "undefined-error: not indexable: error", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = err1.IndexGet(ToStringObject("k1"))
	
	require.Equal(t, "undefined-error: not indexable: error", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = err1.IndexGet(Int(2))
	
	require.Equal(t, "undefined-error: not indexable: error", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = err1.BinaryOp(token.Add, Int(3))

	require.Equal(t, "<nil>-InvalidOperatorError: ", fmt.Sprintf("%v-%v", tmpr, err))

	require.Equal(t, "{}", tk.ToJSONX(tk.FromJSONX((fmt.Sprintf("%v", err1.String()))), "-sort"))
	
	// *RuntimeError
	rerr1 := &(RuntimeError{Err: NewCommonError("failed to proceed")})
	
	require.Equal(t, "157", fmt.Sprintf("%v", rerr1.TypeCode()))
	
	require.Equal(t, "error", fmt.Sprintf("%v", rerr1.TypeName()))
	
	require.Equal(t, "error: failed to proceed", fmt.Sprintf("%v", rerr1.String()))
	
	require.True(t, rerr1.HasMemeber())
	
	tmpr, err = rerr1.CallMethod("value")
	
	require.Equal(t, "error: failed to proceed-<nil>", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "*charlang.RuntimeError", fmt.Sprintf("%T", rerr1.GetValue()))
	
	require.Equal(t, "undefined", fmt.Sprintf("%v", rerr1.GetMember("a")))
	
	require.Equal(t, "<nil>", fmt.Sprintf("%v", rerr1.SetMember("a", ToStringObject("b"))))
	
	require.Equal(t, "*charlang.RuntimeError", fmt.Sprintf("%T", rerr1.Copy()))
	
	require.Equal(t, "false", fmt.Sprintf("%v", rerr1.Equal(Int(1))))
	
	require.Equal(t, "true", fmt.Sprintf("%v", rerr1.IsFalsy()))
	
	require.Equal(t, "false", fmt.Sprintf("%v", rerr1.CanCall()))
	
	tmpr, err = rerr1.Call(ToStringObject("value1"))
	
	require.Equal(t, "<nil>-NotCallableError: ", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "false", fmt.Sprintf("%v", rerr1.CanIterate()))
	
	require.Equal(t, "<nil>", fmt.Sprintf("%T", rerr1.Iterate()))
	
	require.Equal(t, "NotIndexAssignableError: ", fmt.Sprintf("%v", rerr1.IndexSet(Int(1), Int(1))))
	
	tmpr, err = rerr1.IndexGet(ToStringObject("value1"))
	
	require.Equal(t, "undefined-error: not indexable: error", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = rerr1.IndexGet(ToStringObject("k1"))
	
	require.Equal(t, "undefined-error: not indexable: error", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = rerr1.IndexGet(Int(2))
	
	require.Equal(t, "undefined-error: not indexable: error", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = rerr1.BinaryOp(token.Add, Int(3))

	require.Equal(t, "<nil>-InvalidOperatorError: ", fmt.Sprintf("%v-%v", tmpr, err))

	require.Equal(t, "{}", tk.ToJSONX(tk.FromJSONX((fmt.Sprintf("%v", rerr1.String()))), "-sort"))
	
	require.Equal(t, "error: abc-InvalidOperatorError: ", fmt.Sprintf("%v-%v", rerr1.NewError("abc"), err))

	// *Time
	time1 := &(Time{Value: time.Now()})
	
	require.Equal(t, "311", fmt.Sprintf("%v", time1.TypeCode()))
	
	require.Equal(t, "time", fmt.Sprintf("%v", time1.TypeName()))
	
	require.Equal(t, "string", fmt.Sprintf("%T", time1.String()))
	
	require.True(t, time1.HasMemeber())
	
	tmpr, err = time1.CallMethod("value")
	
	require.Equal(t, "*charlang.Time-<nil>", fmt.Sprintf("%T-%v", tmpr, err))
	
	require.Equal(t, "*charlang.Time", fmt.Sprintf("%T", time1.GetValue()))
	
	require.Equal(t, "undefined", fmt.Sprintf("%v", time1.GetMember("a")))
	
	require.Equal(t, "<nil>", fmt.Sprintf("%v", time1.SetMember("a", ToStringObject("b"))))
	
	require.Equal(t, "*charlang.Time", fmt.Sprintf("%T", time1.Copy()))
	
	require.Equal(t, "false", fmt.Sprintf("%v", time1.Equal(Int(1))))
	
	require.Equal(t, "false", fmt.Sprintf("%v", time1.IsFalsy()))
	
	require.Equal(t, "false", fmt.Sprintf("%v", time1.CanCall()))
	
	tmpr, err = time1.Call(ToStringObject("value1"))
	
	require.Equal(t, "<nil>-NotCallableError: ", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "false", fmt.Sprintf("%v", time1.CanIterate()))
	
	require.Equal(t, "<nil>", fmt.Sprintf("%T", time1.Iterate()))
	
	require.Equal(t, "NotIndexAssignableError: ", fmt.Sprintf("%v", time1.IndexSet(Int(1), Int(1))))
	
	tmpr, err = time1.IndexGet(ToStringObject("value1"))
	
	require.Equal(t, "undefined-error: method(value1) not found for type: time", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = time1.IndexGet(ToStringObject("k1"))
	
	require.Equal(t, "undefined-error: method(k1) not found for type: time", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = time1.IndexGet(Int(2))
	
	require.Equal(t, "undefined-TypeError: index type expected string, found int", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = time1.BinaryOp(token.Add, Int(3))

	require.Equal(t, "*charlang.Time-<nil>", fmt.Sprintf("%T-%v", tmpr, err))

	require.Equal(t, "{}", tk.ToJSONX(tk.FromJSONX((fmt.Sprintf("%T", time1.String()))), "-sort"))
	
	tmpr, err = time1.CallName("Format", Call{Args: []Object{String("abc")}})

	require.Equal(t, "abc-<nil>", fmt.Sprintf("%v-%v", tmpr, err))

	bb1, err := time1.MarshalBinary()

	require.Equal(t, "[]uint8-<nil>", fmt.Sprintf("%T-%v", bb1, err))
	
	err = time1.UnmarshalBinary(bb1)

	require.Equal(t, "[]uint8-<nil>", fmt.Sprintf("%T-%v", bb1, err))
	
	bb1, err = time1.MarshalJSON()

	require.Equal(t, "[]uint8-<nil>", fmt.Sprintf("%T-%v", bb1, err))

	err = time1.UnmarshalJSON(bb1)

	require.Equal(t, "[]uint8-<nil>", fmt.Sprintf("%T-%v", bb1, err))
	
	require.Equal(t, "*charlang.Time-<nil>", fmt.Sprintf("%T-%v", timeAdd(time1, 10), err))

	require.Equal(t, "charlang.Int-<nil>", fmt.Sprintf("%T-%v", timeSub(time1, time1), err))

	require.Equal(t, "*charlang.Time-<nil>", fmt.Sprintf("%T-%v", timeAddDate(time1, 1, 2, 3), err))

	require.Equal(t, "false-<nil>", fmt.Sprintf("%v-%v", timeAfter(time1, time1), err))

	require.Equal(t, "false-<nil>", fmt.Sprintf("%v-%v", timeBefore(time1, time1), err))

	require.Equal(t, "true-<nil>", fmt.Sprintf("%v-%v", timeEqual(time1, time1), err))

	require.Equal(t, "abc-<nil>", fmt.Sprintf("%v-%v", timeFormat(time1, "abc"), err))

	require.Equal(t, "[97 98 99 97 98 99]-<nil>", fmt.Sprintf("%v-%v", timeAppendFormat(time1, []byte("abc"), "abc"), err))

	require.Equal(t, "*charlang.Time-<nil>", fmt.Sprintf("%T-%v", timeIn(time1, &Location{Value: time.Local}), err))

	require.Equal(t, "*charlang.Time-<nil>", fmt.Sprintf("%T-%v", timeRound(time1, 12), err))

	require.Equal(t, "*charlang.Time-<nil>", fmt.Sprintf("%T-%v", timeTruncate(time1, 12), err))

	tmpr, err = newArgTypeErr("aa", "bb", "cc")

	require.Equal(t, "*charlang.UndefinedType-TypeError: invalid type for argument 'aa': expected bb, found cc", fmt.Sprintf("%T-%v", tmpr, err))

	tmpr, ok = ToTime(Int(11))

	require.Equal(t, "*charlang.Time-true", fmt.Sprintf("%T-%v", tmpr, ok))

	tmpr, ok = ToLocation(String("abc"))

	require.Equal(t, "*charlang.Location-false", fmt.Sprintf("%T-%v", tmpr, ok))

	// *Location
	loc1 := &(Location{Value: time.UTC})
	
	require.Equal(t, "313", fmt.Sprintf("%v", loc1.TypeCode()))
	
	require.Equal(t, "location", fmt.Sprintf("%v", loc1.TypeName()))
	
	require.Equal(t, "UTC", fmt.Sprintf("%v", loc1.String()))
	
	require.True(t, loc1.HasMemeber())
	
	tmpr, err = loc1.CallMethod("value")
	
	require.Equal(t, "UTC-<nil>", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "*charlang.Location", fmt.Sprintf("%T", loc1.GetValue()))
	
	require.Equal(t, "undefined", fmt.Sprintf("%v", loc1.GetMember("a")))
	
	require.Equal(t, "<nil>", fmt.Sprintf("%v", loc1.SetMember("a", ToStringObject("b"))))
	
	require.Equal(t, "false", fmt.Sprintf("%v", loc1.Equal(Int(1))))
	
	require.Equal(t, "false", fmt.Sprintf("%v", loc1.IsFalsy()))
	
	require.Equal(t, "false", fmt.Sprintf("%v", loc1.CanCall()))
	
	tmpr, err = loc1.Call(ToStringObject("value1"))
	
	require.Equal(t, "<nil>-NotCallableError: ", fmt.Sprintf("%v-%v", tmpr, err))
	
	require.Equal(t, "false", fmt.Sprintf("%v", loc1.CanIterate()))
	
	require.Equal(t, "<nil>", fmt.Sprintf("%T", loc1.Iterate()))
	
	require.Equal(t, "NotIndexAssignableError: ", fmt.Sprintf("%v", loc1.IndexSet(Int(1), Int(1))))
	
	tmpr, err = loc1.IndexGet(ToStringObject("value1"))
	
	require.Equal(t, "<nil>-NotIndexableError: ", fmt.Sprintf("%v-%v", tmpr, err))
	
	tmpr, err = loc1.BinaryOp(token.Add, Int(3))

	require.Equal(t, "<nil>-InvalidOperatorError: ", fmt.Sprintf("%v-%v", tmpr, err))

	require.Equal(t, "{}", tk.ToJSONX(tk.FromJSONX((fmt.Sprintf("%v", loc1.String()))), "-sort"))
	
}

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
