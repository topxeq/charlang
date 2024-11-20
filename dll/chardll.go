package main

/*
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

extern char *QuickRunChar(char *codeA, char *paramA, char *secureCodeA, char *injectA, char *globalsA, char *comBufA);

static void SayHello(char* s) {
	char *buf;

	buf = (char *) malloc(1 + 4 + 65536*256 + 1);
	
	QuickRunChar("a", "b", "c", "d", "e", buf);
	
    printf("%s\n", s);

    printf("0: %d\n", buf[0]);

    puts("_");

    printf("1: %d\n", buf[1]);

    puts("_");

	printf("2: %d\n", buf[2]);
	
    puts("_");

	printf("3: %d\n", buf[3]);
	
    puts("_");

	printf("4: %d\n", buf[4]);
	
    puts("_");
	
	int lenT = buf[1] * 65536 * 256 + buf[2] * 65536 + buf[3] * 256 + buf[4];

	printf("len: %d\n", lenT);
	
    puts("_");

	printf("5: %s\n", buf+5);
	
    puts("_");

	printf("end: %d\n", buf[1+4+lenT]);
	
	free(buf);
}
*/
import "C"

import (
	"encoding/binary"
	"strings"
	"unsafe"
	"fmt"
	"time"
	"github.com/topxeq/tkc"
	"github.com/topxeq/charlang"
	
	_ "github.com/denisenkom/go-mssqldb"
	_ "github.com/go-sql-driver/mysql"

	_ "github.com/glebarez/go-sqlite"
	_ "github.com/jackc/pgx/v5/stdlib"
	_ "github.com/sijms/go-ora/v2"
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

//var CurrentVM *charlang.VM = nil

var GlobalVars map[string]interface{} = nil;

//export QuickRunChar
func QuickRunChar(codeA, paramA, secureCodeA, injectA, globalsA, comBufA *C.char) *C.char {
	codeT := C.GoString(codeA)

	secureCodeT := strings.TrimSpace(C.GoString(secureCodeA))
	
	if true { // secureCodeT != "" {
		codeT = tkc.DealString(tkc.DealString(tkc.DealString(codeT, secureCodeT), secureCodeT), secureCodeT)
	}
	
//	tkc.AppendStringToFile(tkc.Spr("\n codeT: %v\n", codeT), `c:\test\test.log`)
	
	injectT := C.GoString(injectA)
	
	codeT = injectT + codeT

	nv, errT := charlang.Compile([]byte(codeT), &charlang.DefaultCompilerOptions)
	if errT != nil {
		return C.CString("TXERROR:" + errT.Error())
	}
	
	allowLenT := 16777216

	cgsmHandler := func(argsA ...interface{}) interface{} {
		actionA := tkc.ToStr(argsA[0])

		// paramsA := argsA[1:]

		switch actionA {
		case "getGlobalVar":
			if GlobalVars == nil {
				GlobalVars = make(map[string]interface{}, 10)
				return "empty"
			}
			
			strT := tkc.ToStr(argsA[1])
			
			return fmt.Sprintf("%#v", GlobalVars[strT])
		case "setGlobalVar":
			if GlobalVars == nil {
				GlobalVars = make(map[string]interface{}, 10)
				return "empty"
			}
			
			GlobalVars[tkc.ToStr(argsA[1])] = tkc.ToStr(argsA[2])
			
			return fmt.Sprintf("%#v", "done")
		case "setReturnInfo":
			strT := tkc.ToStr(argsA[1])
			
			strBufT := []byte(strT)
			
			lenT := len(strBufT)
			
			bufT := make([]byte, 4)
			
			binary.BigEndian.PutUint32(bufT, uint32(lenT))
			
			C.memset(unsafe.Pointer(comBufA), C.int(1), C.size_t(1))

			C.memcpy(unsafe.Add(unsafe.Pointer(comBufA), 1), C.CBytes(bufT), C.size_t(4))

			C.memcpy(unsafe.Add(unsafe.Pointer(comBufA), 5), C.CBytes(strBufT), C.size_t(lenT))
			
			C.memset(unsafe.Add(unsafe.Pointer(comBufA), 5+lenT), C.int(0), C.size_t(1))

			return ""
		case "send":
			strT := tkc.ToStr(argsA[1])
			
			strBufT := []byte(strT)
			
			lenT := len(strBufT)
			
			for {
				recvBufT := C.GoBytes(unsafe.Pointer(comBufA), 5)
				
				if recvBufT[0] == 99 {
					return ""
				}
				
				if recvBufT[0] == 2 {
					C.memset(unsafe.Pointer(comBufA), C.int(0), C.size_t(1))

					return fmt.Errorf("unexpected return left, cleared")
				}
				
				if recvBufT[0] != 0 {
					time.Sleep(time.Millisecond * 10)
					continue
				}
				
//				allowLenT := int(recvBufT[1]) * 65536 * 256 + int(recvBufT[2]) * 65536 + int(recvBufT[3]) * 256 + int(recvBufT[4])
//				
				if lenT > allowLenT {
					return fmt.Errorf("length exceeds limit")
				}
				
				break
			}

			bufT := make([]byte, 4)
			
			binary.BigEndian.PutUint32(bufT, uint32(lenT))
			
			C.memset(unsafe.Pointer(comBufA), C.int(1), C.size_t(1))

			C.memcpy(unsafe.Add(unsafe.Pointer(comBufA), 1), C.CBytes(bufT), C.size_t(4))

			C.memcpy(unsafe.Add(unsafe.Pointer(comBufA), 5), C.CBytes(strBufT), C.size_t(lenT))
			
			C.memset(unsafe.Add(unsafe.Pointer(comBufA), 5+lenT), C.int(0), C.size_t(1))
			
			return ""
		case "talk":
			strT := tkc.ToStr(argsA[1])
			
			strBufT := []byte(strT)
			
			lenT := len(strBufT)
			
			for {
				recvBufT := C.GoBytes(unsafe.Pointer(comBufA), 5)
				
				if recvBufT[0] == 99 {
					return ""
				}
				
				if recvBufT[0] == 2 {
					C.memset(unsafe.Pointer(comBufA), C.int(0), C.size_t(1))

					return fmt.Errorf("unexpected return left, cleared")
				}
				
				if recvBufT[0] != 0 {
					time.Sleep(time.Millisecond * 10)
					continue
				}
				
//				allowLenT := int(recvBufT[1]) * 65536 * 256 + int(recvBufT[2]) * 65536 + int(recvBufT[3]) * 256 + int(recvBufT[4])
				
				if lenT > allowLenT {
					return fmt.Errorf("length exceeds limit")
				}
				
				break
			}

			bufT := make([]byte, 4)
			
			binary.BigEndian.PutUint32(bufT, uint32(lenT))
			
			C.memset(unsafe.Pointer(comBufA), C.int(1), C.size_t(1))

			C.memcpy(unsafe.Add(unsafe.Pointer(comBufA), 1), C.CBytes(bufT), C.size_t(4))

			C.memcpy(unsafe.Add(unsafe.Pointer(comBufA), 5), C.CBytes(strBufT), C.size_t(lenT))
			
			C.memset(unsafe.Add(unsafe.Pointer(comBufA), 5+lenT), C.int(0), C.size_t(1))
			
			for {
				recvBufT := C.GoBytes(unsafe.Pointer(comBufA), 5)
				
				if recvBufT[0] == 99 {
					return ""
				}
				
				if recvBufT[0] == 0 {
					return fmt.Errorf("unexpected no return")
				}
				
				if recvBufT[0] == 2 {
					len2T := int(recvBufT[1]) * 65536 * 256 + int(recvBufT[2]) * 65536 + int(recvBufT[3]) * 256 + int(recvBufT[4])
					
					recvStrBufT :=  C.GoBytes(unsafe.Add(unsafe.Pointer(comBufA), 5), C.int(len2T))
					
					rs := string(recvStrBufT)
					
					C.memset(unsafe.Pointer(comBufA), C.int(0), C.size_t(1))

					return rs
				
//					break;
				}
			
				time.Sleep(time.Millisecond * 10)
			}

			return ""
		}
		// fmt.Printf("%v\n", "GUI engined disabled")
		return fmt.Errorf("GUI engine disabled")
	}


	envT := charlang.Map{}

	envT["versionG"] = charlang.String{Value: charlang.VersionG}
	envT["scriptPathG"] = charlang.String{Value: ""}
	envT["runModeG"] = charlang.String{Value: "dll"}
	envT["secureCodeG"] = charlang.String{Value: tkc.MD5Encrypt(secureCodeT)}
	envT["guiHandlerG"] = charlang.NewExternalDelegate(cgsmHandler)
//	envT["argsG"] = charlang.Array{}

	globalsT := strings.TrimSpace(C.GoString(globalsA))
	
//	tkc.AppendStringToFile(tkc.Spr("\nglobalsT: %v\n", globalsT), `c:\test\test.log`)
	
	if globalsT != "" {
		mapT, errT := tkc.MSSFromJSON(globalsT)
		
//		tkc.AppendStringToFile(tkc.Spr("\nglobalsT: %#v -- #v\n", mapT, errT), `c:\test\test.log`)
	
		if errT == nil {
			for k, v := range mapT {
				envT[k] = charlang.String{Value: v}
			}
		}
	}

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