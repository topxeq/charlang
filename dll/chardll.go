package main

import "C"

import (
	"strings"
	"github.com/topxeq/tkc"
	"github.com/topxeq/charlang"
)

//export SumInt
func SumInt(a, b int) int {
    return a + b
}

//export SumFloat
func SumFloat(a, b float64) float64 {
    return a + b
}

//export SumStr
func SumStr(a, b *C.char) *C.char {
	a1 := C.GoString(a)
	b1 := C.GoString(b)
    return C.CString(a1 + b1)
}

//export DealStr
func DealStr(strA, codeA *C.char) *C.char {
	a1 := C.GoString(strA)
	b1 := C.GoString(codeA)
    return C.CString(tkc.DealString(a1, b1))
}

//export QuickRunChar
func QuickRunChar(codeA, paramA, secureCodeA, injectA *C.char) *C.char {
	codeT := C.GoString(codeA)

	secureCodeT := strings.TrimSpace(C.GoString(secureCodeA))
	
	if secureCodeT != "" {
		codeT = tkc.DealString(codeT, secureCodeT)
	}
	
	injectT := C.GoString(injectA)
	
	codeT = injectT + codeT

	nv, errT := charlang.Compile([]byte(codeT), &charlang.DefaultCompilerOptions)
	if errT != nil {
		return C.CString("TXERROR:" + errT.Error())
	}

	envT := charlang.Map{}

	envT["versionG"] = charlang.String{Value: charlang.VersionG}
	envT["scriptPathG"] = charlang.String{Value: ""}
	envT["runModeG"] = charlang.String{Value: "dll"}

	retT, errT := charlang.NewVM(nv).Run(envT, charlang.String{Value: C.GoString(paramA)})

	if errT != nil {
		return C.CString("TXERROR:" + errT.Error())
	}
	
	if charlang.IsUndefInternal(retT) {
		return C.CString("TXERROR:undefined")
	}
	
	return C.CString(retT.String())
}

// 编译命令
// go build -buildmode=c-shared -ldflags="-s -w" -o char.dll chardll.go
func main() {
//	fmt.Println("abc")
}