// Copyright (c) 2020 Ozan Hacıbekiroğlu.
// Use of this source code is governed by a MIT License
// that can be found in the LICENSE file.

package charlang

import (
	"fmt"
	"time"
)

var VersionG = "0.2a"

// CallableFunc is a function signature for a callable function.
type CallableFunc = func(args ...Object) (ret Object, err error)

func ConvertToObject(vA interface{}) Object {
	if vA == nil {
		return Undefined
	}

	switch nv := vA.(type) {
	case error:
		return WrapError(nv)
	case string:
		return ToString(nv)
	case bool:
		return Bool(nv)
	case int:
		return Int(nv)
	case int16:
		return Int(nv)
	case rune:
		return Char(nv)
	case int64:
		return Int(nv)
	case byte:
		return Byte(nv)
	case uint16:
		return Uint(nv)
	case uint32:
		return Uint(nv)
	case uint64:
		return Uint(nv)
	case float32:
		return Float(nv)
	case float64:
		return Float(nv)
	case []byte:
		return Bytes(nv)
	case []uint32:
		if nv == nil {
			return Undefined
		}

		rsT := make(Array, 0, len(nv))

		for _, v := range nv {
			rsT = append(rsT, Char(v))
		}

		return rsT
	case []int:
		if nv == nil {
			return Undefined
		}

		rsT := make(Array, 0, len(nv))

		for _, v := range nv {
			rsT = append(rsT, Int(v))
		}

		return rsT
	case []int64:
		if nv == nil {
			return Undefined
		}

		rsT := make(Array, 0, len(nv))

		for _, v := range nv {
			rsT = append(rsT, Int(v))
		}

		return rsT
	case []string:
		if nv == nil {
			return Undefined
		}

		rsT := make(Array, 0, len(nv))

		for _, v := range nv {
			rsT = append(rsT, ToString(v))
		}

		return rsT
	case [][]string:
		if nv == nil {
			return Undefined
		}

		rsT := make(Array, 0, len(nv))

		for _, v := range nv {
			lineListT := make(Array, 0, len(v))
			for _, jv := range v {
				lineListT = append(lineListT, ToString(jv))
			}

			rsT = append(rsT, lineListT)
		}

		return rsT
	case [][]int:
		if nv == nil {
			return Undefined
		}

		rsT := make(Array, 0, len(nv))

		for _, v := range nv {
			lineListT := make(Array, 0, len(v))
			for _, jv := range v {
				lineListT = append(lineListT, ToInt(jv))
			}

			rsT = append(rsT, lineListT)
		}

		return rsT
	case []interface{}:
		if nv == nil {
			return Undefined
		}

		rsT := make(Array, 0, len(nv))

		for _, v := range nv {
			rsT = append(rsT, ConvertToObject(v))
		}

		return rsT
	case map[string]string:
		if nv == nil {
			return Undefined
		}

		rsT := make(Map, len(nv))

		for k, v := range nv {
			rsT[k] = ToString(v)
		}

		return rsT
	case map[string]interface{}:
		if nv == nil {
			return Undefined
		}

		rsT := make(Map, len(nv))

		for k, v := range nv {
			rsT[k] = ConvertToObject(v)
		}

		return rsT
	case map[string]map[string]string:
		if nv == nil {
			return Undefined
		}

		rsT := make(Map, len(nv))

		for k, v := range nv {
			mapT := make(Map, len(nv))
			for jk, jv := range v {
				mapT[jk] = ToString(jv)
			}

			rsT[k] = mapT
		}

		return rsT
	case []map[string]string:
		if nv == nil {
			return Undefined
		}

		rsT := make(Array, 0, len(nv))

		for _, v := range nv {
			rsT = append(rsT, ConvertToObject(v))
		}

		return rsT
	case []map[string]interface{}:
		if nv == nil {
			return Undefined
		}

		rsT := make(Array, 0, len(nv))

		for _, v := range nv {
			rsT = append(rsT, ConvertToObject(v))
		}

		return rsT
	// case time.Time:
	// 	return Any{Value: nv, OriginalType: "time.Time"}
	case Function:
		return &nv
	case String:
		return nv

	default:
		return Any{Value: nv, OriginalType: fmt.Sprintf("%T", nv)}
		// tk.Pl("Unknown type: %T, %#v, %v", vA, vA, vA)
		// return Undefined
	}
}

func ConvertFromObject(vA Object) interface{} {
	// if vA.TypeName() == "int" {
	// 	return int(vA)
	// }
	switch nv := vA.(type) {
	case Bool:
		return bool(nv)
	case Byte:
		return byte(nv)
	case Char:
		return rune(nv)
	case Int:
		return int(nv)
	case Uint:
		return uint64(nv)
	case Float:
		return float64(nv)
	case String:
		return nv.Value
	case *String:
		return nv.Value
	case Bytes:
		return []byte(nv)
	case *Error:
		return nv.Unwrap()
	case Any:
		return nv.Value
	case Map:
		if nv == nil {
			return nil
		}

		rsT := make(map[string]interface{}, len(nv))

		for k, v := range nv {
			rsT[k] = ConvertFromObject(v)
		}

		return rsT
	case Array:
		if nv == nil {
			return nil
		}

		rsT := make([]interface{}, 0, len(nv))

		for _, v := range nv {
			rsT = append(rsT, ConvertFromObject(v))
		}

		return rsT
	}

	if vA == Undefined {
		return nil
	}

	return vA
}

func ObjectsToI(aryA []Object) []interface{} {
	if aryA == nil {
		return nil
	}

	rs := make([]interface{}, 0, len(aryA))

	for _, v := range aryA {
		rs = append(rs, ConvertFromObject(v))
	}

	return rs
}

func ObjectsToN(aryA []Object) []int {
	if aryA == nil {
		return nil
	}

	rs := make([]int, 0, len(aryA))

	for _, v := range aryA {
		vT := ConvertFromObject(v)
		if nv, ok := vT.(int); ok {
			rs = append(rs, nv)
		}
	}

	return rs
}

func ObjectsToS(aryA []Object) []string {
	if aryA == nil {
		return nil
	}

	rs := make([]string, 0, len(aryA))

	for _, v := range aryA {
		rs = append(rs, v.String())
	}

	return rs
}

func ObjectsToBytes(aryA []Object) []byte {
	if aryA == nil {
		return nil
	}

	rs := make([]byte, 0, len(aryA))

	for _, v := range aryA {
		rs = append(rs, byte(v.(Byte)))
	}

	return rs
}

func NewChar(codeA string) (interface{}, error) {
	bytecodeT, errT := Compile([]byte(codeA), DefaultCompilerOptions)
	// if errT != nil {
	// 	return nil, errT
	// }

	return bytecodeT, errT
}

var TkFunction = &Function{
	Name: "tk",
	Value: func(args ...Object) (Object, error) {

		if len(args) < 1 {
			return Undefined, NewCommonError("not enough paramters")
		}

		if args[0].TypeName() != "string" {
			return Undefined, NewCommonError("invalid type for command")
		}

		cmdT := args[0].String()

		switch cmdT {
		case "test":
			fmt.Printf("args: %v\n", args[1:])
			return ConvertToObject("Response!"), nil

		case "getNowTime":
			return ConvertToObject(time.Now()), nil

		default:
			return Undefined, NewCommonError("unknown comman")
		}

		return Undefined, nil
	},
}

func RunChar(charA *Bytecode, envA map[string]interface{}, paraMapA map[string]string, argsA ...Object) (interface{}, error) {
	envT := Map{}

	envT["tk"] = TkFunction
	// envT["argsG"] = ConvertToObject(os.Args)
	envT["versionG"] = ToString(VersionG)

	for k, v := range envA {
		envT[k] = ConvertToObject(v)
	}

	inParasT := make(Map, len(paraMapA))
	for k, v := range paraMapA {
		inParasT[k] = ToString(v)
	}

	paramsT := make([]Object, 0, 2+len(argsA))

	paramsT = append(paramsT, inParasT)
	paramsT = append(paramsT, argsA...)

	retT, errT := NewVM(charA).Run(
		envT, paramsT...,
	)

	return ConvertFromObject(retT), errT
}

func RunCharCode(codeA string, envA map[string]interface{}, paraMapA map[string]string, argsA ...Object) (interface{}, error) {
	bytecodeT, errT := Compile([]byte(codeA), DefaultCompilerOptions)
	if errT != nil {
		return nil, errT
	}

	envT := Map{}

	envT["tk"] = TkFunction
	// envT["argsG"] = ConvertToObject(os.Args)
	envT["versionG"] = ToString(VersionG)

	for k, v := range envA {
		envT[k] = ConvertToObject(v)
	}

	inParasT := make(Map, len(paraMapA))
	for k, v := range paraMapA {
		inParasT[k] = ToString(v)
	}

	paramsT := make([]Object, 0, 2+len(argsA))

	paramsT = append(paramsT, inParasT)
	paramsT = append(paramsT, argsA...)

	retT, errT := NewVM(bytecodeT).Run(
		envT, paramsT...,
	)

	return ConvertFromObject(retT), errT
}

func QuickCompile(codeA string) interface{} {
	bytecodeT, errT := Compile([]byte(codeA), DefaultCompilerOptions)
	if errT != nil {
		return errT
	}

	return bytecodeT
}

func QuickRun(codeA interface{}, argsA ...Object) interface{} {
	var errT error
	nv, ok := codeA.(*Bytecode)

	if !ok {
		codeT, ok := codeA.(string)
		if !ok {
			return fmt.Errorf("invalid parameter")
		}

		nv, errT = Compile([]byte(codeT), DefaultCompilerOptions)
		if errT != nil {
			return errT
		}
	}

	envT := Map{}

	envT["tk"] = TkFunction
	envT["argsG"] = Array{}
	envT["versionG"] = ToString(VersionG)

	paramsT := make([]Object, 0, 2+len(argsA))

	paramsT = append(paramsT, argsA...)

	retT, errT := NewVM(nv).Run(
		envT, paramsT...,
	)

	if errT != nil {
		return errT
	}

	return ConvertFromObject(retT)
}
