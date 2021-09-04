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
	switch nv := vA.(type) {
	case string:
		return String(nv)
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
	case []string:
		if nv == nil {
			return nil
		}

		rsT := make(Array, 0, len(nv))

		for _, v := range nv {
			rsT = append(rsT, String(v))
		}

		return rsT
	case []interface{}:
		if nv == nil {
			return nil
		}

		rsT := make(Array, 0, len(nv))

		for _, v := range nv {
			rsT = append(rsT, ConvertToObject(v))
		}

		return rsT
	case map[string]string:
		if nv == nil {
			return nil
		}

		rsT := make(Map, len(nv))

		for k, v := range nv {
			rsT[k] = String(v)
		}

		return rsT
	case map[string]interface{}:
		if nv == nil {
			return nil
		}

		rsT := make(Map, len(nv))

		for k, v := range nv {
			rsT[k] = ConvertToObject(v)
		}

		return rsT
	// case time.Time:
	// 	return Any{Value: nv, OriginalType: "time.Time"}
	case Function:
		return &nv

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
		return string(nv)
	case Bytes:
		return []byte(nv)
	case Any:
		return nv.Value
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

func RunChar(charA *Bytecode, envA map[string]string, paraMapA map[string]string, argsA ...Object) (interface{}, error) {
	envT := Map{}

	envT["tk"] = TkFunction

	for k, v := range envA {
		envT[k] = String(v)
	}

	inParasT := make(Map, len(paraMapA))
	for k, v := range paraMapA {
		inParasT[k] = String(v)
	}

	paramsT := make([]Object, 0, 2+len(argsA))

	paramsT = append(paramsT, inParasT)
	paramsT = append(paramsT, argsA...)

	retT, errT := NewVM(charA).Run(
		envT, paramsT...,
	)

	return ConvertFromObject(retT), errT
}

func RunCharCode(codeA string, envA map[string]string, paraMapA map[string]string, argsA ...Object) (interface{}, error) {
	bytecodeT, errT := Compile([]byte(codeA), DefaultCompilerOptions)
	if errT != nil {
		return nil, errT
	}

	envT := Map{}

	envT["tk"] = TkFunction

	for k, v := range envA {
		envT[k] = String(v)
	}

	inParasT := make(Map, len(paraMapA))
	for k, v := range paraMapA {
		inParasT[k] = String(v)
	}

	paramsT := make([]Object, 0, 2+len(argsA))

	paramsT = append(paramsT, inParasT)
	paramsT = append(paramsT, argsA...)

	retT, errT := NewVM(bytecodeT).Run(
		envT, paramsT...,
	)

	return ConvertFromObject(retT), errT
}
