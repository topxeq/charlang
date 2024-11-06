package main

import "C"

import (
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

//export QuickRunChar
func QuickRunChar(codeA, paramA *C.char) *C.char {
	nv, errT := charlang.Compile([]byte(C.GoString(codeA)), &charlang.DefaultCompilerOptions)
	if errT != nil {
		return C.CString("TXERROR:" + errT.Error())
	}

	envT := charlang.Map{}

	retT, errT := charlang.NewVM(nv).Run(envT, charlang.String{Value: C.GoString(paramA)})

	if errT != nil {
		return C.CString("TXERROR:" + errT.Error())
	}
	
	return C.CString(retT.String())
}

// 编译命令
// go build -buildmode=c-shared -ldflags="-s -w" -o char.dll chardll.go
func main() {
//	fmt.Println("abc")
}