// Copyright (c) 2020 Ozan Hacıbekiroğlu.
// Use of this source code is governed by a MIT License
// that can be found in the LICENSE file.

package charlang

import (
	"fmt"
	"time"
)

var VersionG = "0.1a"

// CallableFunc is a function signature for a callable function.
type CallableFunc = func(args ...Object) (ret Object, err error)

func ConvertToObject(vA interface{}) Object {
	switch nv := vA.(type) {
	case string:
		return String(nv)
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
	case Int:
		return int(nv)
	}

	if nv, ok := vA.(Int); ok {
		return int(nv)
	}

	if nv, ok := vA.(Float); ok {
		return float64(nv)
	}

	if nv, ok := vA.(Bool); ok {
		return bool(nv)
	}

	if vA.TypeName() == "string" {
		return vA.String()
	}

	if vA.TypeName() == "any" {
		nv := vA.(Any)

		// if nv.OriginalType == "time.Time" {
		// 	return nv.Value(.(time.Time))
		// }

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
