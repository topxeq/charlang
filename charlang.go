// Copyright (c) 2020 Ozan Hacıbekiroğlu.
// Use of this source code is governed by a MIT License
// that can be found in the LICENSE file.

package charlang

// CallableFunc is a function signature for a callable function.
type CallableFunc = func(args ...Object) (ret Object, err error)

func NewChar(codeA string) (interface{}, error) {
	bytecodeT, errT := Compile([]byte(codeA), DefaultCompilerOptions)
	// if errT != nil {
	// 	return nil, errT
	// }

	return bytecodeT, errT
}

func RunChar(charA *Bytecode, envA map[string]string, paraMapA map[string]string, argsA ...Object) (interface{}, error) {
	envT := Map{}

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

	return retT, errT
}

func RunCharCode(codeA string, envA map[string]string, paraMapA map[string]string, argsA ...Object) (interface{}, error) {
	bytecodeT, errT := Compile([]byte(codeA), DefaultCompilerOptions)
	if errT != nil {
		return nil, errT
	}

	envT := Map{}

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

	return retT, errT
}
