package charlang

import (
	"bytes"
	"database/sql"
	"fmt"
	"io"
	"net/http"
	"os"
	"strings"
	"sync"
	"time"

	"github.com/topxeq/tk"

	_ "github.com/denisenkom/go-mssqldb"
	// _ "github.com/godror/godror"
	_ "github.com/sijms/go-ora/v2"

	// full version related end

	_ "github.com/go-sql-driver/mysql"
	_ "github.com/mattn/go-sqlite3"
	// ugofmt "github.com/topxeq/charlang/stdlib/fmt"
)

// global vars
var VersionG = "0.6.2"

var CodeTextG = ""

var VerboseG = false

var ScriptPathG string

var ServerModeG = false

var RandomGeneratorG *tk.RandomX = nil

var ErrCommon = &Error{Name: "error"}

// var TkFunction = &Function{
// 	Name: "tk",
// 	Value: func(args ...Object) (Object, error) {

// 		if len(args) < 1 {
// 			return Undefined, NewCommonError("not enough paramters")
// 		}

// 		if args[0].TypeName() != "string" {
// 			return Undefined, NewCommonError("invalid type for command")
// 		}

// 		cmdT := args[0].String()

// 		switch cmdT {
// 		case "test":
// 			fmt.Printf("args: %v\n", args[1:])
// 			return ConvertToObject("Response!"), nil

// 		case "getNowTime":
// 			return ConvertToObject(time.Now()), nil

// 		default:
// 			return Undefined, NewCommonError("unknown comman")
// 		}

// 		return Undefined, nil
// 	},
// }

var namedFuncMapG = map[string]interface{}{
	"fmt.Fprintf": fmt.Fprintf,
}

var namedValueMapG = map[string]interface{}{
	"tk.TimeFormat":            tk.TimeFormat,            // "2006-01-02 15:04:05"
	"tk.TimeFormatMS":          tk.TimeFormatMS,          // "2006-01-02 15:04:05.000"
	"tk.TimeFormatMSCompact":   tk.TimeFormatMSCompact,   // "20060102150405.000"
	"tk.TimeFormatCompact":     tk.TimeFormatCompact,     // "20060102150405"
	"tk.TimeFormatCompact2":    tk.TimeFormatCompact2,    // "2006/01/02 15:04:05"
	"tk.TimeFormatDateCompact": tk.TimeFormatDateCompact, // "20060102"

	"time.Layout":   time.Layout,
	"time.RFC3339":  time.RFC3339,
	"time.DateTime": time.DateTime,
	"time.DateOnly": time.DateOnly,
	"time.TimeOnly": time.TimeOnly,
}

// // first arg of each func is the object reference
// var setterFuncMapG = map[int]CallableExFunc{
// 	999: func(c Call) (Object, error) {
// 		args := c.GetArgs()

// 		fNameT := args[1].String()

// 		switch fNameT {
// 		case "value":
// 			nv := args[0].(*Any)

// 			rs1, errT := builtinAnyFunc(Call{args: args[2:]})

// 			if errT != nil {
// 				return Undefined, errT
// 			}

// 			nv2 := rs1.(*Any)

// 			nv.Value = nv2.Value
// 			nv.OriginalCode = nv2.OriginalCode
// 			nv.OriginalType = nv2.OriginalType
// 		}

// 		return Int(-1), nil // indicates method not found
// 	},
// }

// first arg of each func is the object reference
var methodFuncMapG = map[int]map[string]*Function{
	103: map[string]*Function{ // Bool
		"toStr": &Function{
			Name: "toStr",
			ValueEx: func(c Call) (Object, error) {
				// tk.Pl("bool.toStr: %#v", c)

				nv, ok := c.This.(Bool)

				if !ok {
					return Undefined, fmt.Errorf("invalid type: %#v", c.This)
				}

				return ToStringObject(nv.String()), nil
			},
		},
	},
	105: map[string]*Function{ // String
		"toStr": &Function{
			Name: "toStr",
			ValueEx: func(c Call) (Object, error) {
				nv, ok := c.This.(String)

				if !ok {
					return Undefined, fmt.Errorf("invalid type: %#v", c.This)
				}

				return ToStringObject(nv.Value), nil
			},
		},
		"trim": &Function{
			Name: "trim",
			ValueEx: func(c Call) (Object, error) {
				nv, ok := c.This.(String)
				if !ok {
					return Undefined, fmt.Errorf("invalid type: %#v", c.This)
				}

				args := toArgsS(0, c)

				return ToStringObject(tk.Trim(nv.Value, args...)), nil
			},
		},
		"contains": &Function{
			Name: "contains",
			ValueEx: func(c Call) (Object, error) {
				nv, ok := c.This.(String)
				if !ok {
					return Undefined, fmt.Errorf("invalid type: %#v", c.This)
				}

				if c.Len() < 1 {
					return Undefined, fmt.Errorf("not enough parameters")
				}

				return Bool(strings.Contains(nv.Value, c.Get(0).String())), nil
			},
		},
	},
	106: map[string]*Function{ // *MutableString
		"toStr": &Function{
			Name: "toStr",
			ValueEx: func(c Call) (Object, error) {
				nv, ok := c.This.(*MutableString)

				if !ok {
					return Undefined, fmt.Errorf("invalid type: %#v", c.This)
				}

				return ToStringObject(nv.String()), nil
			},
		},
		"trim": &Function{
			Name: "trim",
			ValueEx: func(c Call) (Object, error) {
				nv, ok := c.This.(*MutableString)
				if !ok {
					return Undefined, fmt.Errorf("invalid type: %#v", c.This)
				}

				args := toArgsS(0, c)

				nv.Value = tk.Trim(nv.Value, args...)
				// tk.Pl("h1: %#v %#v %#v", c, args, nv.Value)

				return ToStringObject(nv.Value), nil
			},
		},
	},
	191: map[string]*Function{ // *CharCode
		"toStr": &Function{
			Name: "toStr",
			ValueEx: func(c Call) (Object, error) {
				nv, ok := c.This.(*CharCode)

				if !ok {
					return Undefined, fmt.Errorf("invalid type: %#v", c.This)
				}

				return ToStringObject(nv.String()), nil
			},
		},
		"compile": &Function{
			Name: "compile",
			ValueEx: func(c Call) (Object, error) {
				nv, ok := c.This.(*CharCode)

				if !ok {
					return NewCommonError("invalid type: %#v", c.This), nil
				}

				byteCodeT := QuickCompile(nv.Source, nv.CompilerOptions) // quickCompile(tk.ToStr(argsA[0])) //

				if tk.IsError(byteCodeT) {
					nv.LastError = fmt.Sprintf("%v", byteCodeT)
					return NewCommonError("%v", byteCodeT), nil
				}

				nv.Value = byteCodeT.(*Bytecode)

				return nv, nil
			},
		},
		"run": &Function{
			Name: "run",
			ValueEx: func(c Call) (Object, error) {
				nv, ok := c.This.(*CharCode)

				if !ok {
					return NewCommonError("invalid type: %#v", c.This), nil
				}

				if nv.Value == nil {
					return NewCommonError("code not compiled"), nil
					// _, errT := nv.CallMethod("compile")

					// if errT != nil {
					// 	return NewCommonError("failed to compile code: %v", errT), nil
					// }
				}

				argsT := c.GetArgs()

				// lenT := len(argsT)

				// if lenT < 1 {
				// 	return NewCommonError("not enough parameters"), nil
				// }

				var globalsA map[string]interface{} = nil
				// var additionsA []Object = nil

				envT := NewBaseEnv(globalsA) // Map{}

				// if lenT > 1 {
				// 	additionsA = argsT
				// }
				// for i:=1; i< lenT; i ++ {
				// 	envT["v"]
				// }

				retT, errT := NewVM(nv.Value).Run(envT, argsT...)

				if errT != nil {
					return NewCommonError("%v", errT), nil
				}

				return retT, nil
			},
		},
		"goRun": &Function{
			Name: "goRun",
			ValueEx: func(c Call) (Object, error) {
				nv, ok := c.This.(*CharCode)

				if !ok {
					return NewCommonError("invalid type: %#v", c.This), nil
				}

				if nv.Value == nil {
					return NewCommonError("code not compiled"), nil
					// 	_, errT := nv.CallMethod("compile")

					// 	if errT != nil {
					// 		return NewCommonError("failed to compile code: %v", errT), nil
					// 	}
				}

				argsT := c.GetArgs()

				// lenT := len(argsT)

				// if lenT < 1 {
				// 	return NewCommonError("not enough parameters"), nil
				// }

				var globalsA map[string]interface{} = nil
				// var additionsA []Object = nil

				envT := NewBaseEnv(globalsA) // Map{}

				// if lenT > 1 {
				// 	additionsA = argsT
				// }
				// for i:=1; i< lenT; i ++ {
				// 	envT["v"]
				// }

				go NewVM(nv.Value).Run(envT, argsT...)

				return Undefined, nil
			},
		},
	},
	307: map[string]*Function{ // *StringBuilder
		"toStr": &Function{
			Name: "toStr",
			Value: func(args ...Object) (Object, error) {
				return ToStringObject(((*strings.Builder)(args[0].(*StringBuilder).Value)).String()), nil
			},
		},
		"write": &Function{
			Name: "write",
			Value: func(args ...Object) (Object, error) {
				var errT error

				o := args[0].(*StringBuilder)

				argsT := args[1:]

				countT := 0

				for _, v := range argsT {
					tmpCountT := 0
					switch nv := v.(type) {
					case String:
						tmpCountT, errT = o.Value.WriteString(nv.Value)

						if errT != nil {
							tmpCountT = 0
						}
					case Bytes:
						tmpCountT, errT = o.Value.Write([]byte(nv))

						if errT != nil {
							tmpCountT = 0
						}
					case Char:
						tmpCountT, errT = o.Value.WriteRune(rune(nv))

						if errT != nil {
							tmpCountT = 0
						}
					case Byte:
						errT = o.Value.WriteByte(byte(nv))

						if errT != nil {
							tmpCountT = 0
						} else {
							tmpCountT = 1
						}
					default:
						tmpCountT, errT = o.Value.WriteString(nv.String())

						if errT != nil {
							tmpCountT = 0
						}

					}

					countT += tmpCountT
				}

				return Int(countT), nil
			},
		},
		"writeStr": &Function{
			Name: "writeStr",
			Value: func(args ...Object) (Object, error) {
				var errT error

				o := args[0].(*StringBuilder)

				argsT := args[1:]

				if len(argsT) < 1 {
					return NewCommonError("not enough parameters"), nil
				}

				rsT, errT := o.Value.WriteString(argsT[0].String())

				if errT != nil {
					return NewFromError(errT), nil
				}

				return Int(rsT), nil
			},
		},
		"writeBytes": &Function{
			Name: "writeBytes",
			Value: func(argsA ...Object) (Object, error) {
				var errT error

				o := argsA[0].(*StringBuilder)

				args := argsA[1:]

				if len(args) < 1 {
					return NewCommonError("not enough parameters"), nil
				}

				nv, ok := args[0].(Bytes)
				if !ok {
					return NewCommonError("invalid parameter type: %v", args[0].TypeName()), nil
				}

				rsT, errT := o.Value.Write([]byte(nv))
				if errT != nil {
					return NewFromError(errT), nil
				}

				return Int(rsT), nil
			},
		},
		"clear": &Function{
			Name: "clear",
			Value: func(argsA ...Object) (Object, error) {
				o := argsA[0].(*StringBuilder)

				o.Value.Reset()
				return Undefined, nil
			},
		},
		"reset": &Function{
			Name: "reset",
			Value: func(argsA ...Object) (Object, error) {
				o := argsA[0].(*StringBuilder)

				o.Value.Reset()
				return Undefined, nil
			},
		},
	},
	308: map[string]*Function{ // *BytesBuffer
		"toStr": &Function{
			Name: "toStr",
			Value: func(args ...Object) (Object, error) {
				return ToStringObject(((*bytes.Buffer)(args[0].(*BytesBuffer).Value)).String()), nil
			},
		},
		"write": &Function{
			Name: "write",
			Value: func(args ...Object) (Object, error) {
				var errT error

				o := args[0].(*StringBuilder)

				argsT := args[1:]

				countT := 0

				for _, v := range argsT {
					tmpCountT := 0
					switch nv := v.(type) {
					case String:
						tmpCountT, errT = o.Value.WriteString(nv.Value)

						if errT != nil {
							tmpCountT = 0
						}
					case Bytes:
						tmpCountT, errT = o.Value.Write([]byte(nv))

						if errT != nil {
							tmpCountT = 0
						}
					case Char:
						tmpCountT, errT = o.Value.WriteRune(rune(nv))

						if errT != nil {
							tmpCountT = 0
						}
					case Byte:
						errT = o.Value.WriteByte(byte(nv))

						if errT != nil {
							tmpCountT = 0
						} else {
							tmpCountT = 1
						}
					default:
						tmpCountT, errT = o.Value.WriteString(nv.String())

						if errT != nil {
							tmpCountT = 0
						}

					}

					countT += tmpCountT
				}

				return Int(countT), nil
			},
		},
		"writeStr": &Function{
			Name: "writeStr",
			Value: func(args ...Object) (Object, error) {
				var errT error

				o := args[0].(*StringBuilder)

				argsT := args[1:]

				if len(argsT) < 1 {
					return NewCommonError("not enough parameters"), nil
				}

				rsT, errT := o.Value.WriteString(argsT[0].String())

				if errT != nil {
					return NewFromError(errT), nil
				}

				return Int(rsT), nil
			},
		},
		"writeBytes": &Function{
			Name: "writeBytes",
			Value: func(argsA ...Object) (Object, error) {
				var errT error

				o := argsA[0].(*StringBuilder)

				args := argsA[1:]

				if len(args) < 1 {
					return NewCommonError("not enough parameters"), nil
				}

				nv, ok := args[0].(Bytes)
				if !ok {
					return NewCommonError("invalid parameter type: %v", args[0].TypeName()), nil
				}

				rsT, errT := o.Value.Write([]byte(nv))
				if errT != nil {
					return NewFromError(errT), nil
				}

				return Int(rsT), nil
			},
		},
		"clear": &Function{
			Name: "clear",
			Value: func(argsA ...Object) (Object, error) {
				o := argsA[0].(*StringBuilder)

				o.Value.Reset()
				return Undefined, nil
			},
		},
		"reset": &Function{
			Name: "reset",
			Value: func(argsA ...Object) (Object, error) {
				o := argsA[0].(*StringBuilder)

				o.Value.Reset()
				return Undefined, nil
			},
		},
	},
	315: map[string]*Function{ // *Seq
		"toStr": &Function{
			Name: "toStr",
			Value: func(args ...Object) (Object, error) {
				return ToStringObject(((*tk.Seq)(args[0].(*Seq).Value)).String()), nil
			},
		},
		"get": &Function{
			Name: "get",
			Value: func(args ...Object) (Object, error) {
				return ToIntObject(((*tk.Seq)(args[0].(*Seq).Value)).Get()), nil
			},
		},
		"getCurrent": &Function{
			Name: "getCurrent",
			Value: func(args ...Object) (Object, error) {
				return ToIntObject(((*tk.Seq)(args[0].(*Seq).Value)).GetCurrent()), nil
			},
		},
		"reset": &Function{
			Name: "reset",
			Value: func(argsA ...Object) (Object, error) {
				o := argsA[0].(*Seq)

				if len(argsA) > 1 {
					o.Value.Reset(int(ToIntObject(argsA[1])))
				} else {
					o.Value.Reset()
				}
				return Undefined, nil
			},
		},
	},
	317: map[string]*Function{ // *Seq
		"toStr": &Function{
			Name: "toStr",
			Value: func(args ...Object) (Object, error) {
				return ToStringObject(fmt.Sprintf("%v", ((*sync.RWMutex)(args[0].(*Mutex).Value)))), nil
			},
		},
		"lock": &Function{
			Name: "lock",
			Value: func(args ...Object) (Object, error) {
				objT := (*sync.RWMutex)(args[0].(*Mutex).Value)

				objT.Lock()

				return Undefined, nil
			},
		},
		"unlock": &Function{
			Name: "unlock",
			Value: func(args ...Object) (Object, error) {
				objT := (*sync.RWMutex)(args[0].(*Mutex).Value)

				objT.Unlock()

				return Undefined, nil
			},
		},
		"rLock": &Function{
			Name: "rLock",
			Value: func(args ...Object) (Object, error) {
				objT := (*sync.RWMutex)(args[0].(*Mutex).Value)

				objT.RLock()

				return Undefined, nil
			},
		},
		"rUnlock": &Function{
			Name: "rUnlock",
			Value: func(args ...Object) (Object, error) {
				objT := (*sync.RWMutex)(args[0].(*Mutex).Value)

				objT.RUnlock()

				return Undefined, nil
			},
		},
		"tryLock": &Function{
			Name: "tryLock",
			Value: func(args ...Object) (Object, error) {
				objT := (*sync.RWMutex)(args[0].(*Mutex).Value)

				return Bool(objT.TryLock()), nil

			},
		},
		"tryRLock": &Function{
			Name: "tryRLock",
			Value: func(args ...Object) (Object, error) {
				objT := (*sync.RWMutex)(args[0].(*Mutex).Value)

				return Bool(objT.TryRLock()), nil

			},
		},
		"getCurrent": &Function{
			Name: "getCurrent",
			Value: func(args ...Object) (Object, error) {
				return ToIntObject(((*tk.Seq)(args[0].(*Seq).Value)).GetCurrent()), nil
			},
		},
		"reset": &Function{
			Name: "reset",
			Value: func(argsA ...Object) (Object, error) {
				o := argsA[0].(*Seq)

				if len(argsA) > 1 {
					o.Value.Reset(int(ToIntObject(argsA[1])))
				} else {
					o.Value.Reset()
				}
				return Undefined, nil
			},
		},
	},
	319: map[string]*Function{ // *Mux
		"toStr": &Function{
			Name: "toStr",
			Value: func(args ...Object) (Object, error) {
				return ToStringObject(fmt.Sprintf("%v", ((*sync.RWMutex)(args[0].(*Mutex).Value)))), nil
			},
		},
		"setHandler": &Function{
			Name: "setHandler",
			ValueEx: func(c Call) (Object, error) {
				lenT := c.Len()
				if lenT < 2 {
					return NewCommonError("not enough paramters"), nil
				}
				// tk.Plv("%#v", c.GetArgs())

				objT := (*http.ServeMux)(c.This.(*Mux).Value)

				pathT := c.Get(0).String()

				fnObjT := c.Get(1)

				fnT, ok := fnObjT.(*CompiledFunction)

				if ok {
					objT.HandleFunc(pathT, func(w http.ResponseWriter, req *http.Request) {
						retT, errT := NewInvoker(c.VM(), fnT).Invoke(ConvertToObject(req), ConvertToObject(w))

						if errT != nil {
							tk.Pl("failed to invoke handler: %v", errT)
							return
						}

						rs := retT.String()

						if rs != "TX_END_RESPONSE_XT" {
							w.Write([]byte(rs))
						}

					})

					return Undefined, nil
				}

				fn2T, ok := fnObjT.(*CharCode)

				if ok {
					var additionsA []Object = make([]Object, 0, lenT-2)

					for i := 2; i < lenT; i++ {
						additionsA = append(additionsA, c.Get(i))
					}

					// tk.Plv("additionsA", additionsA)

					objT.HandleFunc(pathT, func(w http.ResponseWriter, req *http.Request) {
						var globalsA map[string]interface{} = map[string]interface{}{
							"requestG":  req,
							"responseG": w,
						}

						envT := NewBaseEnv(globalsA) // Map{}

						// if lenT > 1 {
						// 	additionsA = argsA[1:]
						// }
						retT, errT := NewVM(fn2T.Value).Run(envT, additionsA...)

						if errT != nil {
							tk.Pl("failed to run handler: %v", errT)
							return
						}

						rs := retT.String()

						if rs != "TX_END_RESPONSE_XT" {
							w.Write([]byte(rs))
						}

					})
				}

				return NewCommonError("invalid paramter type: (%T)%v", fnObjT, fnObjT.TypeName()), nil

			},
		},
		"startHttpServer": &Function{
			Name: "startHttpServer",
			ValueEx: func(c Call) (Object, error) {
				portT := ":80"

				muxT := (*http.ServeMux)(c.This.(*Mux).Value)

				errT := http.ListenAndServe(portT, muxT)

				if errT != nil {
					return NewCommonError("failed to start server: %v", errT), nil
				}

				return Undefined, nil
			},
		},
		"reset": &Function{
			Name: "reset",
			Value: func(argsA ...Object) (Object, error) {
				o := argsA[0].(*Seq)

				if len(argsA) > 1 {
					o.Value.Reset(int(ToIntObject(argsA[1])))
				} else {
					o.Value.Reset()
				}
				return Undefined, nil
			},
		},
	},
}

// objects

// common funcs

func IsUndefInternal(o Object) bool {
	_, ok := o.(*UndefinedType)

	return ok
}

// func GetObjectMember(c Call) (Object, error) {

// 	if c.Len() < 2 {
// 		return Undefined, fmt.Errorf("not enough parameters")
// 	}

// 	rs1 := c.Get(0).GetMember(c.Get(1).String())

// 	return rs1, nil
// 	// 	// tk.Pl("member search result: %#v", rs1)
// 	// 	if !IsUndefInternal(rs1) {
// 	// 		tk.Pl("member got: %#v %#v", o, rs1)
// 	// 		return rs1, nil
// 	// 	}
// 	// }

// 	// map1, ok := methodFuncMapG[o.TypeCode()]

// 	// if !ok {
// 	// 	return Undefined, nil // ErrNotIndexable
// 	// }

// 	// f1, ok := map1[index]

// 	// if !ok {
// 	// 	return Undefined, nil
// 	// }

// 	// // tk.Pl("f1: %#v", f1)

// 	// if f1.ValueEx != nil {
// 	// 	fn1 := &Function{
// 	// 		Name: f1.Name,
// 	// 		ValueEx: func(c Call) (Object, error) {
// 	// 			c.This = o
// 	// 			return (*f1).CallEx(c)
// 	// 		}}

// 	// 	if ok0 {
// 	// 		// tk.Pl("set member fn1: %#v %#v", index, fn1)

// 	// 		nv1.SetMember(index, fn1)
// 	// 	}

// 	// 	return fn1, nil
// 	// }

// 	// if f1.Value == nil {
// 	// 	return Undefined, nil
// 	// }

// 	// fn2 := &Function{
// 	// 	Name: f1.Name,
// 	// 	Value: func(args ...Object) (Object, error) {
// 	// 		return (*f1).Call(append([]Object{o}, args...)...)
// 	// 	}}

// 	// if ok0 {
// 	// 	// tk.Pl("set member fn2: %#v %#v", index, fn2)
// 	// 	nv1.SetMember(index, fn2)
// 	// }

// 	// return fn2, nil

// }

func GetObjectMethodFunc(o Object, idxA string) (Object, error) {
	// tk.Pln("GetObjectMethodFunc:", index, o, o.TypeCode())

	map1, ok := methodFuncMapG[o.TypeCode()]

	if !ok {
		return nil, NewCommonError("not indexable: %v", o.TypeName())
	}

	f1, ok := map1[idxA]

	if !ok {
		return nil, NewCommonError("method(%v) not found for type: %v", idxA, o.TypeName())
	}

	// return &Function{
	// 	Name: f1.Name,
	// 	Value: func(args ...Object) (Object, error) {
	// 		return (*f1).Call(append([]Object{o}, args...)...)
	// 	}}, nil

	ok0 := o.HasMemeber()

	if f1.ValueEx != nil {
		fn1 := &Function{
			Name: f1.Name,
			ValueEx: func(c Call) (Object, error) {
				c.This = o
				return (*f1).CallEx(c)
			}}

		if ok0 {
			o.SetMember(idxA, fn1)
		}

		return fn1, nil
	}

	if f1.Value == nil {
		return Undefined, nil
	}

	fn2 := &Function{
		Name: f1.Name,
		Value: func(args ...Object) (Object, error) {
			return (*f1).Call(append([]Object{o}, args...)...)
		}}

	if ok0 {
		o.SetMember(idxA, fn2)
	}

	return fn2, nil

}

func CallObjectMethodFunc(o Object, idxA string, argsA ...Object) (Object, error) {
	// tk.Pln("CallObjectMethodFunc:", index, o, o.TypeCode())

	map1, ok := methodFuncMapG[o.TypeCode()]

	if !ok {
		return nil, NewCommonError("unknown method: %v", idxA)
	}

	f1, ok := map1[idxA]

	if !ok {
		return nil, NewCommonError("unknown method: %v", idxA)
	}

	if f1.ValueEx != nil {
		return (*f1).CallEx(Call{This: o, args: argsA})
	}

	if f1.Value == nil {
		return nil, NewCommonError("unknown method: %v", idxA)
	}

	return (*f1).Call(append([]Object{o}, argsA...)...)
}

func QuickCompile(codeA string, compilerOptionsA ...*CompilerOptions) interface{} {
	var compilerOptionsT *CompilerOptions
	if len(compilerOptionsA) > 0 {
		compilerOptionsT = compilerOptionsA[0]
	} else {
		compilerOptionsT = &DefaultCompilerOptions
	}

	bytecodeT, errT := Compile([]byte(codeA), compilerOptionsT)
	if errT != nil {
		return errT
	}

	return bytecodeT
}

func NewBaseEnv(varsA map[string]interface{}, additionsA ...Object) *Map {
	envT := Map{}

	// envT["tk"] = TkFunction
	envT["argsG"] = Array{}
	envT["versionG"] = ToStringObject(VersionG)

	for k, v := range varsA {
		envT[k] = ConvertToObject(v)
	}

	for i, v := range additionsA {
		envT[tk.IntToStr(i+1)] = v
	}

	return &envT
}

func QuickRun(codeA interface{}, globalsA map[string]interface{}, additionsA ...Object) interface{} {
	var errT error
	nv, ok := codeA.(*Bytecode)

	if !ok {
		codeT, ok := codeA.(string)
		if !ok {
			return fmt.Errorf("invalid parameter")
		}

		nv, errT = Compile([]byte(codeT), &DefaultCompilerOptions)
		if errT != nil {
			return errT
		}
	}

	envT := NewBaseEnv(globalsA) // Map{}

	// envT["tk"] = TkFunction
	// envT["argsG"] = Array{}
	// envT["versionG"] = ToString(VersionG)

	// for k, v := range globalsA {
	// 	envT[k] = ConvertToObject(v)
	// }

	// for i, v := range additionsA {
	// 	envT[tk.IntToStr(i+1)] = v
	// }

	retT, errT := NewVM(nv).Run(envT, additionsA...)

	if errT != nil {
		return errT
	}

	return ConvertFromObject(retT)
}

func NewEvalQuick(globalsA map[string]interface{}, optsA *CompilerOptions, localsA ...Object) *Eval {
	// moduleMap := NewModuleMap()
	// // moduleMap.AddBuiltinModule("time", ugotime.Module).
	// // 	AddBuiltinModule("strings", ugostrings.Module).
	// moduleMap.AddBuiltinModule("fmt", ugofmt.Module)

	var optsT CompilerOptions

	if optsA != nil {
		optsT = *optsA
	} else {
		optsT = CompilerOptions{
			ModulePath:        "",
			ModuleMap:         nil,
			SymbolTable:       NewSymbolTable(),
			OptimizerMaxCycle: TraceCompilerOptions.OptimizerMaxCycle,
			// TraceParser:       false,
			// TraceOptimizer:    false,
			// TraceCompiler:     false,
			// OptimizeConst:     !noOptimizer,
			// OptimizeExpr:      !noOptimizer,
		}
	}

	envT := NewBaseEnv(globalsA) // Map{}

	// if globals == nil {
	// 	globals = Map{}
	// }

	if optsT.SymbolTable == nil {
		optsT.SymbolTable = NewSymbolTable()
	}

	// if optsT.ModuleIndexes == nil {
	// 	optsT.ModuleIndexes = NewModuleIndexes()
	// }

	return &Eval{
		Locals:  localsA,
		Globals: *envT,
		Opts:    optsT,
		VM:      NewVM(nil).SetRecover(true),
	}
}

func RunScriptOnHttp(codeA string, compilerOptionsA *CompilerOptions, res http.ResponseWriter, req *http.Request, inputA string, argsA []string, parametersA map[string]string, optionsA ...string) (string, error) {
	if tk.IfSwitchExists(optionsA, "-verbose") {
		tk.Pl("Starting...")
	}

	if tk.StartsWith(codeA, "//TXDEF#") {
		tmps := tk.DecryptStringByTXDEF(codeA, "topxeq")

		if !tk.IsErrStr(tmps) {
			codeA = tmps
		}
	}

	if res != nil {
		res.Header().Set("Access-Control-Allow-Origin", "*")
		res.Header().Set("Access-Control-Allow-Headers", "*")
		res.Header().Set("Content-Type", "text/html; charset=utf-8")
	}

	if req != nil {
		req.ParseForm()
	}

	reqT := tk.GetFormValueWithDefaultValue(req, "charms", "")
	// if req.RequestURI != "/charms/ed01/addAccessLog" {
	// 	tk.Pl("RequestURI: %v", req.RequestURI)
	// }

	if reqT == "" {
		if tk.StartsWith(req.RequestURI, "/charms") {
			reqT = req.RequestURI[7:]
		}
	}

	tmps := tk.Split(reqT, "?")
	if len(tmps) > 1 {
		reqT = tmps[0]
	}

	if tk.StartsWith(reqT, "/") {
		reqT = reqT[1:]
	}

	// tk.Pl("charms: %v", reqT)

	var paraMapT map[string]string
	var errT error

	retT := ""

	vo := tk.GetFormValueWithDefaultValue(req, "vo", "")

	if vo == "" {
		paraMapT = tk.FormToMap(req.Form)
	} else {
		paraMapT, errT = tk.MSSFromJSON(vo)

		if errT != nil {
			res.Write([]byte(tk.ErrStrf("%v", "invalid vo format")))
			// res.Write([]byte(genFailCompact("操作失败", "invalid vo format", "-compact")))
			return retT, nil
		}
	}

	// if !tk.ContainsIn(req.RequestURI, "/charms/ed01/addAccessLog", "/charms/ed01/getRuleColor.js", "/charms/ed01/getLangList.js", "/charms/ed01/getKnowListByBoard") {
	// 	tk.Pl("[%v] REQ: %#v (%#v)", tk.GetNowTimeStringFormal(), reqT, paraMapT)
	// }

	toWriteT := ""

	fileNameT := reqT

	if !tk.EndsWith(fileNameT, ".char") {
		fileNameT += ".char"
	}

	fcT := codeA

	var compilerOptionsT *CompilerOptions

	if compilerOptionsA == nil {
		compilerOptionsT = &DefaultCompilerOptions
	} else {
		compilerOptionsT = compilerOptionsA
	}

	bytecodeT, errT := Compile([]byte(fcT), compilerOptionsT)
	if errT != nil {
		res.Write([]byte(tk.ErrStrf("%v", errT.Error())))
		// res.Write([]byte(genFailCompact("操作失败", errT.Error(), "-compact")))
		return retT, nil
	}

	inParasT := make(Map, len(paraMapT))
	for k, v := range paraMapT {
		inParasT[k] = ToStringObject(v)
	}

	envT := NewBaseEnv(nil) // Map{}

	// envT["tk"] = TkFunction
	(*envT)["argsG"] = ConvertToObject(argsA)

	(*envT)["requestG"] = ConvertToObject(req)
	(*envT)["responseG"] = ConvertToObject(res)
	(*envT)["reqNameG"] = ConvertToObject(reqT)
	(*envT)["inputG"] = ConvertToObject(inputA)
	(*envT)["basePathG"] = ConvertToObject(tk.GetSwitch(optionsA, "-base=", ""))
	(*envT)["paraMapG"] = ConvertToObject(parametersA)

	retObjectT, errT := NewVM(bytecodeT).Run(
		envT,
		inParasT,
	)

	if errT != nil {
		res.Write([]byte(tk.ErrStrf("%v", errT.Error())))
		return retObjectT.String(), nil
	}

	toWriteT = retObjectT.String()

	if toWriteT == "TX_END_RESPONSE_XT" {
		return retObjectT.String(), nil
	}

	res.Header().Set("Content-Type", "text/html; charset=utf-8")

	res.Write([]byte(toWriteT))

	return retObjectT.String(), nil
}

func ConvertToObject(vA interface{}) Object {
	if vA == nil {
		return Undefined
	}

	switch nv := vA.(type) {
	case error:
		return NewCommonError(nv.Error())
	case string:
		return ToStringObject(nv)
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
	case []rune:
		return Chars(nv)
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
			rsT = append(rsT, ToStringObject(v))
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
				lineListT = append(lineListT, ToStringObject(jv))
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
				lineListT = append(lineListT, ToIntObject(jv))
			}

			rsT = append(rsT, lineListT)
		}

		return rsT
	case []Object:
		if nv == nil {
			return Undefined
		}

		rsT := make(Array, 0, len(nv))

		for _, v := range nv {
			rsT = append(rsT, v)
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
			rsT[k] = ToStringObject(v)
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
				mapT[jk] = ToStringObject(jv)
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
	case time.Time:
		return &Time{Value: nv}
	case *tk.Seq:
		return &Seq{Value: nv}
	case *sync.RWMutex:
		return &Mutex{Value: nv}
	case *http.ServeMux:
		return &Mux{Value: nv}
	case *http.Request:
		return &HttpReq{Value: nv}
	case http.ResponseWriter:
		return &HttpResp{Value: nv}
	case *io.Reader:
		return &Reader{Value: nv}
	case *sql.DB:
		return &Database{Value: nv}
	case *Bytecode:
		return &CharCode{Value: nv}
	case *CharCode:
		return nv
	case *HttpReq:
		return nv
	case *HttpResp:
		return nv
	case Function:
		return &nv
	case String:
		return nv

	default:
		originalCodeT := -1
		nv1, ok := vA.(Object)

		if ok {
			originalCodeT = nv1.TypeCode()
		}

		return &Any{Value: nv, OriginalType: fmt.Sprintf("%T", nv), OriginalCode: originalCodeT}
		// tk.Pl("Unknown type: %T, %#v, %v", vA, vA, vA)
		// return Undefined
	}
}

func ConvertFromObject(vA Object) interface{} {
	// tk.Plo("ConvertFromObject:", vA)
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
	case *MutableString:
		return nv.Value
	case *StringBuilder:
		return nv.Value
	case *BytesBuffer:
		return nv.Value
	case Bytes:
		return []byte(nv)
	case Chars:
		return []rune(nv)
	case *Error:
		if nv == nil {
			return nil
		}

		return nv.Unwrap() // fmt.Errorf("%v", nv.Message)
	case *RuntimeError:
		if nv == nil {
			return nil
		}

		return nv.Unwrap()
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
	case *Time:
		return nv.Value
	case *Seq:
		return nv.Value
	case *Mutex:
		return nv.Value
	case *Mux:
		return nv.Value
	case *HttpReq:
		return nv.Value
	case *HttpResp:
		return nv.Value
	case *Reader:
		return nv.Value
	case *CharCode:
		return nv.Value
	case *Gel:
		return nv.Value
	case *Database:
		return nv.Value
	case *Any:
		return nv.Value
	}

	if vA.TypeCode() == 0 {
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

func DownloadStringFromSSH(sshA string, filePathA string) string {
	aryT := tk.Split(sshA, ":")

	basePathT := tk.EnsureBasePathInHome("char")

	if strings.HasPrefix(basePathT, "TXERROR:") {
		return tk.ErrStrf("failed to find base path: %v", basePathT[8:])
	}

	if len(aryT) != 5 {
		aryT = tk.Split(tk.LoadStringFromFile(tk.JoinPath(basePathT, "ssh.cfg"))+filePathA, ":")

		if len(aryT) != 5 {
			return tk.ErrStrF("invalid ssh config: %v", "")
		}

	}

	clientT, errT := tk.NewSSHClient(aryT[0], tk.StrToIntWithDefaultValue(aryT[1], 22), aryT[2], aryT[3])

	if errT != nil {
		return tk.ErrToStrF("failed to create SSH client:", errT)
	}

	tmpPathT := tk.JoinPath(basePathT, "tmp")

	errT = tk.EnsureMakeDirsE(tmpPathT)

	if errT != nil {
		return tk.ErrToStrF("failed to create tmp dir:", errT)
	}

	tmpFileT, errT := tk.CreateTempFile(tmpPathT, "")

	if errT != nil {
		return tk.ErrToStrF("failed to create tmp dir:", errT)
	}

	defer os.Remove(tmpFileT)

	errT = clientT.Download(aryT[4], tmpFileT)

	if errT != nil {
		return tk.ErrToStrF("failed to download file:", errT)
	}

	fcT := tk.LoadStringFromFile(tmpFileT)

	return fcT
}

func GetCfgString(fileNameA string) string {
	basePathT := tk.EnsureBasePathInHome("char")

	if !strings.HasPrefix(basePathT, "TXERROR:") {
		cfgPathT := tk.JoinPath(basePathT, fileNameA)

		cfgStrT := tk.Trim(tk.LoadStringFromFile(cfgPathT))

		if !tk.IsErrorString(cfgStrT) {
			return cfgStrT
		}

		return tk.ErrStrF("failed to get config string: %v", tk.GetErrorString(cfgStrT))

	}

	return tk.ErrStrF("failed to get config string: %v", basePathT[8:])
}

func SetCfgString(fileNameA string, strA string) string {
	basePathT := tk.EnsureBasePathInHome("char")

	if !strings.HasPrefix(basePathT, "TXERROR:") {
		cfgPathT := tk.JoinPath(basePathT, fileNameA)

		rsT := tk.SaveStringToFile(strA, cfgPathT)

		if tk.IsErrorString(rsT) {
			return tk.ErrStrF("failed to save config string: %v", tk.GetErrorString(rsT))
		}

		return ""

	}

	return tk.ErrStrf("failed to save config string: %v", basePathT[8:])
}

func NewChar(codeA string) (interface{}, error) {
	bytecodeT, errT := Compile([]byte(codeA), &DefaultCompilerOptions)
	// if errT != nil {
	// 	return nil, errT
	// }

	return bytecodeT, errT
}

func NewCommonError(formatA string, argsA ...interface{}) *Error {
	return &Error{Name: "error", Message: fmt.Sprintf(formatA, argsA...)}
}

func NewCommonErrorWithPos(c Call, formatA string, argsA ...interface{}) *Error {
	return &Error{Name: "error", Message: fmt.Sprintf(fmt.Sprintf("[pos: %v]", c.VM().GetSrcPos())+formatA, argsA...)}
}

func NewError(nameA string, formatA string, argsA ...interface{}) *Error {
	errT := ErrCommon.NewError(fmt.Sprintf(formatA, argsA...))

	errT.Name = nameA

	return errT
}

func NewFromError(errA error) *Error {
	if errA == nil {
		return nil
	}

	errT := ErrCommon.NewError(errA.Error())

	errT.Name = "error"

	return errT
}

func WrapError(errA error) *Error {
	if errA == nil {
		return nil
	}

	return &Error{Name: "Error", Message: errA.Error(), Cause: errA}
}
