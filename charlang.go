package charlang

//go:generate go run ./cmd/mkcallable -output zfuncs.go charlang.go

import (
	"fmt"
	"strconv"
	"unicode/utf8"

	"github.com/topxeq/charlang/registry"
	tk "github.com/topxeq/tkc"
)

const (
	// AttrModuleName is a special attribute injected into modules to identify
	// the modules by name.
	AttrModuleName = "__module_name__"
)

// CallableFunc is a function signature for a callable function.
type CallableFunc = func(args ...Object) (ret Object, err error)

// CallableExFunc is a function signature for a callable function that accepts
// a Call struct.
type CallableExFunc = func(Call) (ret Object, err error)

// ToObject will try to convert an interface{} v to an Object.
func ToObject(v interface{}) (ret Object, err error) {
	switch v := v.(type) {
	case nil:
		ret = Undefined
	case string:
		ret = ToStringObject(v)
	case int64:
		ret = Int(v)
	case int:
		ret = Int(v)
	case uint:
		ret = Uint(v)
	case uint64:
		ret = Uint(v)
	case uintptr:
		ret = Uint(v)
	case bool:
		if v {
			ret = True
		} else {
			ret = False
		}
	case rune:
		ret = Char(v)
	case byte:
		ret = Char(v)
	case float64:
		ret = Float(v)
	case float32:
		ret = Float(v)
	case []byte:
		if v != nil {
			ret = Bytes(v)
		} else {
			ret = Bytes{}
		}
	case map[string]Object:
		if v != nil {
			ret = Map(v)
		} else {
			ret = Map{}
		}
	case map[string]interface{}:
		m := make(Map, len(v))
		for vk, vv := range v {
			vo, err := ToObject(vv)
			if err != nil {
				return nil, err
			}
			m[vk] = vo
		}
		ret = m
	case []Object:
		if v != nil {
			ret = Array(v)
		} else {
			ret = Array{}
		}
	case []interface{}:
		arr := make(Array, len(v))
		for i, vv := range v {
			obj, err := ToObject(vv)
			if err != nil {
				return nil, err
			}
			arr[i] = obj
		}
		ret = arr
	case Object:
		ret = v
	case CallableFunc:
		if v != nil {
			ret = &Function{Value: v}
		} else {
			ret = Undefined
		}
	case error:
		ret = &Error{Message: v.Error(), Cause: v}
	default:
		if out, ok := registry.ToObject(v); ok {
			ret, ok = out.(Object)
			if ok {
				return
			}
		}
		err = fmt.Errorf("cannot convert to object: %T", v)
	}
	return
}

// ToObjectAlt is analogous to ToObject but it will always convert signed integers to
// Int and unsigned integers to Uint. It is an alternative to ToObject.
// Note that, this function is subject to change in the future.
func ToObjectAlt(v interface{}) (ret Object, err error) {
	switch v := v.(type) {
	case nil:
		ret = Undefined
	case string:
		ret = ToStringObject(v)
	case bool:
		if v {
			ret = True
		} else {
			ret = False
		}
	case int:
		ret = Int(v)
	case int64:
		ret = Int(v)
	case uint64:
		ret = Uint(v)
	case float64:
		ret = Float(v)
	case float32:
		ret = Float(v)
	case int32:
		ret = Int(v)
	case int16:
		ret = Int(v)
	case int8:
		ret = Int(v)
	case uint:
		ret = Uint(v)
	case uint32:
		ret = Uint(v)
	case uint16:
		ret = Uint(v)
	case uint8:
		ret = Uint(v)
	case uintptr:
		ret = Uint(v)
	case []byte:
		if v != nil {
			ret = Bytes(v)
		} else {
			ret = Bytes{}
		}
	case map[string]interface{}:
		m := make(Map, len(v))
		for vk, vv := range v {
			vo, err := ToObjectAlt(vv)
			if err != nil {
				return nil, err
			}
			m[vk] = vo
		}
		ret = m
	case map[string]Object:
		if v != nil {
			ret = Map(v)
		} else {
			ret = Map{}
		}
	case []interface{}:
		arr := make(Array, len(v))
		for i, vv := range v {
			obj, err := ToObjectAlt(vv)
			if err != nil {
				return nil, err
			}
			arr[i] = obj
		}
		ret = arr
	case []Object:
		if v != nil {
			ret = Array(v)
		} else {
			ret = Array{}
		}
	case Object:
		ret = v
	case CallableFunc:
		if v != nil {
			ret = &Function{Value: v}
		} else {
			ret = Undefined
		}
	case error:
		ret = &Error{Message: v.Error(), Cause: v}
	default:
		if out, ok := registry.ToObject(v); ok {
			ret, ok = out.(Object)
			if ok {
				return
			}
		}
		err = fmt.Errorf("cannot convert to object: %T", v)
	}
	return
}

// ToInterface tries to convert an Object o to an interface{} value.
func ToInterface(o Object) (ret interface{}) {
	switch o := o.(type) {
	case Int:
		ret = int64(o)
	case String:
		ret = o.Value
	case Bytes:
		ret = []byte(o)
	case Array:
		arr := make([]interface{}, len(o))
		for i, val := range o {
			arr[i] = ToInterface(val)
		}
		ret = arr
	case Map:
		m := make(map[string]interface{}, len(o))
		for key, v := range o {
			m[key] = ToInterface(v)
		}
		ret = m
	case Uint:
		ret = uint64(o)
	case Char:
		ret = rune(o)
	case Float:
		ret = float64(o)
	case Bool:
		ret = bool(o)
	case *SyncMap:
		if o == nil {
			return map[string]interface{}{}
		}
		o.RLock()
		defer o.RUnlock()
		m := make(map[string]interface{}, len(o.Value))
		for key, v := range o.Value {
			m[key] = ToInterface(v)
		}
		ret = m
	case *UndefinedType:
		ret = nil
	default:
		if out, ok := registry.ToInterface(o); ok {
			ret = out
		} else {
			ret = o
		}
	}
	return
}

// ToString will try to convert an Object to Charlang string value.
func ToString(o Object) (v String, ok bool) {
	if v, ok = o.(String); ok {
		return
	}
	vv, ok := ToGoString(o)
	if ok {
		v = ToStringObject(vv)
	}
	return
}

// ToBytes will try to convert an Object to Charlang bytes value.
func ToBytes(o Object) (v Bytes, ok bool) {
	if v, ok = o.(Bytes); ok {
		return
	}

	vv, ok := ToGoByteSlice(o)
	if ok {
		v = Bytes(vv)
	}
	return
}

// ToInt will try to convert an Object to Charlang int value.
func ToInt(o Object) (v Int, ok bool) {
	if v, ok = o.(Int); ok {
		return
	}
	vv, ok := ToGoInt64(o)
	if ok {
		v = Int(vv)
	}
	return
}

// ToUint will try to convert an Object to Charlang uint value.
func ToUint(o Object) (v Uint, ok bool) {
	if v, ok = o.(Uint); ok {
		return
	}
	vv, ok := ToGoUint64(o)
	if ok {
		v = Uint(vv)
	}
	return
}

// ToFloat will try to convert an Object to Charlang float value.
func ToFloat(o Object) (v Float, ok bool) {
	if v, ok = o.(Float); ok {
		return
	}
	vv, ok := ToGoFloat64(o)
	if ok {
		v = Float(vv)
	}
	return
}

// ToChar will try to convert an Object to Charlang char value.
func ToChar(o Object) (v Char, ok bool) {
	if v, ok = o.(Char); ok {
		return
	}
	vv, ok := ToGoRune(o)
	if ok {
		v = Char(vv)
	}
	return
}

// ToBool will try to convert an Object to Charlang bool value.
func ToBool(o Object) (v Bool, ok bool) {
	if v, ok = o.(Bool); ok {
		return
	}
	vv, ok := ToGoBool(o)
	v = Bool(vv)
	return
}

// ToArray will try to convert an Object to Charlang array value.
func ToArray(o Object) (v Array, ok bool) {
	v, ok = o.(Array)
	return
}

// ToMap will try to convert an Object to Charlang map value.
func ToMap(o Object) (v Map, ok bool) {
	v, ok = o.(Map)
	return
}

// ToSyncMap will try to convert an Object to Charlang syncMap value.
func ToSyncMap(o Object) (v *SyncMap, ok bool) {
	v, ok = o.(*SyncMap)
	return
}

// ToGoString will try to convert an Object to Go string value.
func ToGoString(o Object) (v string, ok bool) {
	if o == Undefined {
		return
	}
	v, ok = o.String(), true
	return
}

// ToGoByteSlice will try to convert an Object to Go byte slice.
func ToGoByteSlice(o Object) (v []byte, ok bool) {
	switch nv := o.(type) {
	case Bytes:
		v, ok = nv, true
	case Chars:
		v, ok = []byte(string(nv)), true
	case String:
		v, ok = make([]byte, len(nv.Value)), true
		copy(v, nv.Value)
	case Array:
		lenT := len(nv)
		bufT := make([]byte, lenT)

		for i := 0; i < lenT; i++ {
			rs1, errT := builtinByteFunc(Call{Args: []Object{nv[i]}})

			if errT != nil {
				return nil, false
			}

			bufT[i] = byte(rs1.(Byte))
		}

		return bufT, true
	case *Any:
		rs := tk.DataToBytes(nv.Value)

		if tk.IsError(rs) {
			return nil, false
		}

		return rs.([]byte), true

	}

	return
}

// ToGoInt will try to convert a numeric, bool or string Object to Go int value.
func ToGoInt(o Object) (v int, ok bool) {
	switch o := o.(type) {
	case Bool:
		ok = true
		if o {
			v = 1
		}
	case Byte:
		v, ok = int(o), true
	case Char:
		v, ok = int(o), true
	case Int:
		v, ok = int(o), true
	case Uint:
		v, ok = int(o), true
	case Float:
		v, ok = int(o), true
	case String:
		if vv, err := strconv.ParseInt(o.Value, 0, 0); err == nil {
			v = int(vv)
			ok = true
		}
	case *MutableString:
		if vv, err := strconv.ParseInt(o.Value, 0, 0); err == nil {
			v = int(vv)
			ok = true
		}
	}

	return
}

func ToGoIntWithDefault(o Object, defaultA int) int {
	switch o := o.(type) {
	case Bool:
		if o {
			return 1
		} else {
			return 0
		}
	case Byte:
		return int(o)
	case Char:
		return int(o)
	case Int:
		return int(o)
	case Uint:
		return int(o)
	case Float:
		return int(o)
	case String:
		if vv, err := strconv.ParseInt(o.Value, 0, 0); err == nil {
			return int(vv)
		}
	case *MutableString:
		if vv, err := strconv.ParseInt(o.Value, 0, 0); err == nil {
			return int(vv)
		}
	}

	return defaultA
}

// func ToGoIntQuick(o Object) int {
// 	switch o := o.(type) {
// 	case Bool:
// 		if o {
// 			return 1
// 		} else {
// 			return 0
// 		}
// 	case Byte:
// 		return int(o)
// 	case Char:
// 		return int(o)
// 	case Int:
// 		return int(o)
// 	case Uint:
// 		return int(o)
// 	case Float:
// 		return int(o)
// 	case String:
// 		if vv, err := strconv.ParseInt(o.Value, 0, 0); err == nil {
// 			return int(vv)
// 		}
// 	case *MutableString:
// 		if vv, err := strconv.ParseInt(o.Value, 0, 0); err == nil {
// 			return int(vv)
// 		}
// 	}

// 	return 0
// }

func ToIntQuick(o Object) int {
	switch o := o.(type) {
	case Bool:
		if o {
			return 1
		} else {
			return 0
		}
	case Byte:
		return int(o)
	case Char:
		return int(o)
	case Int:
		return int(o)
	case Uint:
		return int(o)
	case Float:
		return int(o)
	case String:
		if vv, err := strconv.ParseInt(o.Value, 0, 0); err == nil {
			return int(vv)
		}
	case *MutableString:
		if vv, err := strconv.ParseInt(o.Value, 0, 0); err == nil {
			return int(vv)
		}
	}

	return 0
}

func ToFloatQuick(o Object) float64 {
	switch o := o.(type) {
	case Bool:
		if o {
			return 1.0
		} else {
			return 0.0
		}
	case Byte:
		return float64(o)
	case Char:
		return float64(o)
	case Int:
		return float64(o)
	case Uint:
		return float64(o)
	case Float:
		return float64(o)
	case String:
		if vv, err := strconv.ParseFloat(o.Value, 64); err == nil {
			return float64(vv)
		}
	case *MutableString:
		if vv, err := strconv.ParseFloat(o.Value, 64); err == nil {
			return float64(vv)
		}
	}

	return 0.0
}

// ToGoInt64 will try to convert a numeric, bool or string Object to Go int64
// value.
func ToGoInt64(o Object) (v int64, ok bool) {
	switch o := o.(type) {
	case Int:
		v, ok = int64(o), true
	case Uint:
		v, ok = int64(o), true
	case Float:
		v, ok = int64(o), true
	case Char:
		v, ok = int64(o), true
	case Byte:
		v, ok = int64(o), true
	case Bool:
		ok = true
		if o {
			v = 1
		}
	case String:
		if vv, err := strconv.ParseInt(o.Value, 0, 64); err == nil {
			v = vv
			ok = true
		}
	}
	return
}

// ToGoUint64 will try to convert a numeric, bool or string Object to Go uint64
// value.
func ToGoUint64(o Object) (v uint64, ok bool) {
	switch o := o.(type) {
	case Int:
		v, ok = uint64(o), true
	case Uint:
		v, ok = uint64(o), true
	case Float:
		v, ok = uint64(o), true
	case Char:
		v, ok = uint64(o), true
	case Byte:
		v, ok = uint64(o), true
	case Bool:
		ok = true
		if o {
			v = 1
		}
	case String:
		if vv, err := strconv.ParseUint(o.Value, 0, 64); err == nil {
			v = vv
			ok = true
		}
	}
	return
}

// ToGoFloat64 will try to convert a numeric, bool or string Object to Go
// float64 value.
func ToGoFloat64(o Object) (v float64, ok bool) {
	switch o := o.(type) {
	case Int:
		v, ok = float64(o), true
	case Uint:
		v, ok = float64(o), true
	case Float:
		v, ok = float64(o), true
	case Char:
		v, ok = float64(o), true
	case Byte:
		v, ok = float64(o), true
	case Bool:
		ok = true
		if o {
			v = 1
		}
	case String:
		if vv, err := strconv.ParseFloat(o.Value, 64); err == nil {
			v = vv
			ok = true
		}
	}
	return
}

// ToGoRune will try to convert a int like Object to Go rune value.
func ToGoRune(o Object) (v rune, ok bool) {
	switch o := o.(type) {
	case Int:
		v, ok = rune(o), true
	case Uint:
		v, ok = rune(o), true
	case Char:
		v, ok = rune(o), true
	case Float:
		v, ok = rune(o), true
	case Byte:
		v, ok = rune(o), true
	case String:
		ok = true
		v, _ = utf8.DecodeRuneInString(o.Value)
	case Bool:
		ok = true
		if o {
			v = 1
		}
	}
	return
}

// ToGoBool will try to convert an Object to Go bool value.
func ToGoBool(o Object) (v bool, ok bool) {
	v, ok = !o.IsFalsy(), true
	return
}

// functions to generate with mkcallable

// builtin delete
//
//charlang:callable func(o Object, k string) (err error)

// builtin copy, len, error, typeName, bool, string, isInt, isUint
// isFloat, isChar, isBool, isString, isBytes, isMap, isSyncMap, isArray
// isUndefined, isFunction, isCallable, isIterable
//
//charlang:callable func(o Object) (ret Object)

// builtin repeat
//
//charlang:callable func(o Object, n int) (ret Object, err error)

// builtin :makeArray
//
//charlang:callable func(n int, o Object) (ret Object, err error)

// builtin contains
//
//charlang:callable func(o Object, v Object) (ret Object, err error)

// builtin sort, sortReverse, int, uint, float, char, chars
//
//charlang:callable func(o Object) (ret Object, err error)

// builtin int
//
//charlang:callable func(v int64) (ret Object)

// builtin uint
//
//charlang:callable func(v uint64) (ret Object)

// builtin float
//
//charlang:callable func(v float64) (ret Object)
