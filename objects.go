// Copyright (c) 2020 Ozan Hacıbekiroğlu.
// Use of this source code is governed by a MIT License
// that can be found in the LICENSE file.

package charlang

import (
	"bytes"
	"database/sql"
	"errors"
	"fmt"
	"io"
	"net/http"
	"strconv"
	"strings"
	"sync"
	"time"

	"github.com/topxeq/charlang/parser"
	"github.com/topxeq/charlang/token"
	"github.com/topxeq/sqltk"
	"github.com/topxeq/tk"
)

const (
	// True represents a true value.
	True = Bool(true)

	// False represents a false value.
	False = Bool(false)
)

var (
	// Undefined represents undefined value.
	Undefined Object = undefined{}
)

// Object represents an object in the VM.
type Object interface {
	// TypeName should return the name of the type.
	TypeName() string

	// String should return a string of the type's value.
	String() string

	// BinaryOp handles +,-,*,/,%,<<,>>,<=,>=,<,> operators.
	// Returned error stops VM execution if not handled with an error handler
	// and VM.Run returns the same error as wrapped.
	BinaryOp(tok token.Token, right Object) (Object, error)

	// IsFalsy returns true if value is falsy otherwise false.
	IsFalsy() bool

	// Equal checks equality of objects.
	Equal(right Object) bool

	// Call is called from VM if CanCall() returns true. Check the number of
	// arguments provided and their types in the method. Returned error stops VM
	// execution if not handled with an error handler and VM.Run returns the
	// same error as wrapped.
	Call(args ...Object) (Object, error)

	// CanCall returns true if type can be called with Call() method.
	// VM returns an error if one tries to call a noncallable object.
	CanCall() bool

	// Iterate should return an Iterator for the type.
	Iterate() Iterator

	// CanIterate should return whether the Object can be Iterated.
	CanIterate() bool

	// IndexGet should take an index Object and return a result Object or an
	// error for indexable objects. Indexable is an object that can take an
	// index and return an object. Returned error stops VM execution if not
	// handled with an error handler and VM.Run returns the same error as
	// wrapped. If Object is not indexable, ErrNotIndexable should be returned
	// as error.
	IndexGet(index Object) (value Object, err error)

	// IndexSet should take an index Object and a value Object for index
	// assignable objects. Index assignable is an object that can take an index
	// and a value on the left-hand side of the assignment statement. If Object
	// is not index assignable, ErrNotIndexAssignable should be returned as
	// error. Returned error stops VM execution if not handled with an error
	// handler and VM.Run returns the same error as wrapped.
	IndexSet(index, value Object) error
}

// Copier wraps the Copy method to create a deep copy of the object.
type Copier interface {
	Copy() Object
}

// ObjectImpl is the basic Object implementation and it does not nothing, and
// helps to implement Object interface by embedding and overriding methods in
// custom implementations. String and TypeName must be implemented otherwise
// calling these methods causes panic.
type ObjectImpl struct {
	Members map[string]Object
	Methods map[string]*Function
}

var _ Object = ObjectImpl{}

// TypeName implements Object interface.
func (ObjectImpl) TypeName() string {
	panic(ErrNotImplemented)
}

// String implements Object interface.
func (ObjectImpl) String() string {
	panic(ErrNotImplemented)
}

// Equal implements Object interface.
func (ObjectImpl) Equal(Object) bool { return false }

// IsFalsy implements Object interface.
func (ObjectImpl) IsFalsy() bool { return true }

// CanCall implements Object interface.
func (ObjectImpl) CanCall() bool { return false }

// Call implements Object interface.
func (ObjectImpl) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

// CanIterate implements Object interface.
func (ObjectImpl) CanIterate() bool { return false }

// Iterate implements Object interface.
func (ObjectImpl) Iterate() Iterator { return nil }

// IndexGet implements Object interface.
func (ObjectImpl) IndexGet(index Object) (value Object, err error) {
	return nil, ErrNotIndexable
}

// IndexSet implements Object interface.
func (ObjectImpl) IndexSet(index, value Object) error {
	return ErrNotIndexAssignable
}

// BinaryOp implements Object interface.
func (ObjectImpl) BinaryOp(_ token.Token, _ Object) (Object, error) {
	return nil, ErrInvalidOperator
}

type undefined struct {
	ObjectImpl
}

// TypeName implements Object interface.
func (o undefined) TypeName() string {
	return "undefined"
}

// String implements Object interface.
func (o undefined) String() string {
	return "undefined"
}

// Call implements Object interface.
func (undefined) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

// Equal implements Object interface.
func (o undefined) Equal(right Object) bool {
	return right == Undefined
}

func (o undefined) BinaryOp(tok token.Token, right Object) (Object, error) {
	switch right.(type) {
	case undefined:
		switch tok {
		case token.Less, token.Greater:
			return False, nil
		case token.LessEq, token.GreaterEq:
			return True, nil
		}
	default:
		switch tok {
		case token.Less, token.LessEq:
			return True, nil
		case token.Greater, token.GreaterEq:
			return False, nil
		}
	}
	return nil, NewOperandTypeError(
		tok.String(),
		Undefined.TypeName(),
		right.TypeName())
}

func (undefined) IndexGet(key Object) (Object, error) {
	return Undefined, nil
}

func (undefined) IndexSet(key, value Object) error {
	return ErrNotIndexAssignable
}

type StatusResult struct {
	ObjectImpl
	Status string
	Value  string
}

var StatusResultInvalid = StatusResult{Status: "", Value: ""}
var StatusResultSuccess = StatusResult{Status: "success", Value: ""}
var StatusResultFail = StatusResult{Status: "fail", Value: ""}

func (o StatusResult) TypeName() string {
	return "statusResult"
}

func (o StatusResult) String() string {
	return `{"Status": ` + tk.ObjectToJSON(o.Status) + `, "Value": ` + tk.ObjectToJSON(o.Value) + `}`
}

func (StatusResult) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

func (o StatusResult) Equal(right Object) bool {
	nv, ok := right.(StatusResult)
	if !ok {
		return false
	}

	return ((nv.Status == o.Status) && (nv.Value == o.Value))
}

func (o StatusResult) BinaryOp(tok token.Token, right Object) (Object, error) {
	return nil, ErrInvalidOperator
}

func GenStatusResult(args ...Object) (Object, error) {
	if len(args) < 1 {
		return StatusResultInvalid, nil
	}

	if len(args) == 1 {
		nv, ok := args[0].(String)

		if !ok {
			return StatusResultInvalid, nil
		}

		mapT := tk.JSONToMapStringString(nv.Value)
		if mapT == nil {
			return StatusResultInvalid, nil
		}

		statusT, ok := mapT["Status"]
		if !ok {
			return StatusResultInvalid, nil
		}

		return StatusResult{Status: statusT, Value: mapT["Value"]}, nil
	}

	nv0, ok := args[0].(String)

	if !ok {
		return StatusResultInvalid, nil
	}

	nv1, ok := args[1].(String)

	if !ok {
		return StatusResultInvalid, nil
	}

	return StatusResult{Status: nv0.Value, Value: nv1.Value}, nil

}

func (o StatusResult) IndexGet(index Object) (Object, error) {
	nv, ok := index.(String)
	if !ok {
		return nil, ErrNotIndexable
	}

	fNameT := nv.Value

	if o.Members == nil {
		o.Members = map[string]Object{}
	}

	if o.Methods == nil {
		o.Methods = map[string]*Function{}
	}

	switch fNameT {
	case "status":
		fT, ok := o.Methods["status"]
		if !ok {
			o.Methods["status"] = &Function{
				Name: "status",
				Value: func(args ...Object) (Object, error) {
					if len(args) < 1 {
						return ToString(o.Status), nil
					}

					nv, ok := args[0].(String)

					if !ok {
						return StatusResultInvalid, nil
					}

					o.Status = nv.Value
					return StatusResultSuccess, nil
				}}
			fT = o.Methods["status"]
		}
		return fT, nil
	case "value":
		fT, ok := o.Methods["value"]
		if !ok {
			o.Methods["value"] = &Function{
				Name: "value",
				Value: func(args ...Object) (Object, error) {
					if len(args) < 1 {
						return ToString(o.Value), nil
					}

					nv, ok := args[0].(String)

					if !ok {
						return StatusResultInvalid, nil
					}

					o.Value = nv.Value
					return StatusResultSuccess, nil
				}}
			fT = o.Methods["value"]
		}
		return fT, nil
	case "isValid":
		fT, ok := o.Methods["isValid"]
		if !ok {
			o.Methods["isValid"] = &Function{
				Name: "isValid",
				Value: func(args ...Object) (Object, error) {
					return Bool(o.Status != ""), nil
				}}
			fT = o.Methods["isValid"]
		}
		return fT, nil
	case "isSuccess":
		fT, ok := o.Methods["isSuccess"]
		if !ok {
			o.Methods["isSuccess"] = &Function{
				Name: "isSuccess",
				Value: func(args ...Object) (Object, error) {
					return Bool(o.Status == "success"), nil
				}}
			fT = o.Methods["isSuccess"]
		}
		return fT, nil
	case "toString", "toJSON":
		fT, ok := o.Methods["toString"]
		if !ok {
			o.Methods["toString"] = &Function{
				Name: "toString",
				Value: func(args ...Object) (Object, error) {
					return ToString(o.String()), nil
				}}
			fT = o.Methods["toString"]
		}
		return fT, nil
	case "fromString", "set":
		fT, ok := o.Methods["fromString"]
		if !ok {
			o.Methods["fromString"] = &Function{
				Name: "fromString",
				Value: func(args ...Object) (Object, error) {
					if len(args) < 1 {
						o.Status = ""
						o.Value = ""
						return StatusResultInvalid, nil
					}

					if len(args) == 1 {
						nv, ok := args[0].(String)

						if !ok {
							o.Status = ""
							o.Value = ""
							return StatusResultInvalid, nil
						}

						mapT := tk.JSONToMapStringString(nv.Value)
						if mapT == nil {
							o.Status = ""
							o.Value = ""
							return StatusResultInvalid, nil
						}

						statusT, ok := mapT["Status"]
						if !ok {
							o.Status = ""
							o.Value = ""
							return StatusResultInvalid, nil
						}

						o.Status = statusT
						o.Value = mapT["Value"]
						return StatusResultSuccess, nil
					}

					nv0, ok := args[0].(String)

					if !ok {
						o.Status = ""
						o.Value = ""
						return StatusResultInvalid, nil
					}

					nv1, ok := args[1].(String)

					if !ok {
						o.Status = ""
						o.Value = ""
						return StatusResultInvalid, nil
					}

					o.Status = nv0.Value
					o.Value = nv1.Value
					return StatusResultSuccess, nil
				}}
			fT = o.Methods["fromString"]
		}
		return fT, nil
	}

	return nil, ErrNotIndexable
}

func (StatusResult) IndexSet(key, value Object) error {
	return ErrNotIndexAssignable
}

type Any struct {
	ObjectImpl
	Value        interface{}
	OriginalType string
}

var _ Object = Any{}

func NewAny(vA interface{}, argsA ...string) *Any {
	originalT := ""

	if len(argsA) > 0 {
		originalT = argsA[0]
	}

	return &Any{
		Value:        vA,
		OriginalType: originalT,
	}
}

func NewAnyValue(vA interface{}, argsA ...string) Any {
	originalT := ""

	if len(argsA) > 0 {
		originalT = argsA[0]
	}

	return Any{
		Value:        vA,
		OriginalType: originalT,
	}
}

func (o Any) WriteResp(bytesA []byte) error {
	v, ok := o.Value.(http.ResponseWriter)

	if !ok {
		return ErrType
	}

	_, err := v.Write(bytesA)

	return err
}

func (o Any) TypeName() string {
	return "any"
}

func (o Any) String() string {
	switch nv := o.Value.(type) {
	case *strings.Builder:
		return nv.String()
	}

	return fmt.Sprintf("%v", o.Value)
}

func (o Any) Equal(right Object) bool {
	if v, ok := right.(Any); ok {
		return o.Value == v.Value
	}

	return false
}

func (o Any) IsFalsy() bool { return false }

func (Any) CanCall() bool { return false }

func (Any) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

func (Any) CanIterate() bool { return false }

func (Any) Iterate() Iterator { return nil }

func (o Any) IndexGet(index Object) (value Object, err error) {
	nv, ok := index.(String)
	if !ok {
		return nil, ErrNotIndexable
	}

	fNameT := nv.Value

	if o.Members == nil {
		o.Members = map[string]Object{}
	}

	if o.Methods == nil {
		o.Methods = map[string]*Function{}
	}

	switch fNameT {
	case "isValid":
		fT, ok := o.Methods["isValid"]
		if !ok {
			o.Methods["isValid"] = &Function{
				Name: "isValid",
				Value: func(args ...Object) (Object, error) {
					return Bool(o.Value != nil), nil
				}}
			fT = o.Methods["isValid"]
		}
		return fT, nil
	case "type":
		fT, ok := o.Methods["type"]
		if !ok {
			o.Methods["type"] = &Function{
				Name: "type",
				Value: func(args ...Object) (Object, error) {
					return ToString(fmt.Sprintf("%T", o.Value)), nil
				}}
			fT = o.Methods["type"]
		}
		return fT, nil
	case "toString":
		fT, ok := o.Methods["toString"]
		if !ok {
			o.Methods["toString"] = &Function{
				Name: "toString",
				Value: func(args ...Object) (Object, error) {
					return ToString(fmt.Sprintf("%v", o.Value)), nil
				}}
			fT = o.Methods["toString"]
		}
		return fT, nil
	case "writeString":
		fT, ok := o.Methods["writeString"]
		if !ok {
			o.Methods["writeString"] = &Function{
				Name: "writeString",
				Value: func(args ...Object) (Object, error) {
					aryT := []Object{o}

					aryT = append(aryT, args...)

					return builtinWriteStringFunc(aryT...)
				}}
			fT = o.Methods["writeString"]
		}
		return fT, nil
	}

	return nil, ErrNotIndexable
}

func (Any) IndexSet(index, value Object) error {
	return ErrNotIndexAssignable
}

func (o Any) BinaryOp(tok token.Token, right Object) (Object, error) {
	return nil, ErrInvalidOperator
}

func (o Any) Copy() Object {
	return Any{
		Value:        o.Value,
		OriginalType: o.OriginalType,
	}
}

type DateTime struct {
	ObjectImpl
	Value time.Time
}

var _ Object = DateTime{}

func NewDateTime(vA interface{}) *DateTime {
	return &DateTime{
		Value: tk.ToTime(vA),
	}
}

func NewDateTimeValue(vA interface{}) DateTime {
	return DateTime{
		Value: tk.ToTime(vA),
	}
}

func (o DateTime) TypeName() string {
	return "datetime"
}

func (o DateTime) String() string {
	return fmt.Sprintf("%v", o.Value)
}

func (o DateTime) Equal(right Object) bool {
	if v, ok := right.(DateTime); ok {
		return o.Value == v.Value
	}

	return false
}

func (o DateTime) IsFalsy() bool { return false }

func (DateTime) CanCall() bool { return false }

func (DateTime) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

func (DateTime) CanIterate() bool { return false }

func (DateTime) Iterate() Iterator { return nil }

func (o DateTime) IndexGet(index Object) (value Object, err error) {
	nv, ok := index.(String)
	if !ok {
		return nil, ErrNotIndexable
	}

	fNameT := nv.Value

	if o.Members == nil {
		o.Members = map[string]Object{}
	}

	if o.Methods == nil {
		o.Methods = map[string]*Function{}
	}

	switch fNameT {
	case "format":
		fT, ok := o.Methods["format"]
		if !ok {
			o.Methods["format"] = &Function{
				Name: "format",
				Value: func(args ...Object) (Object, error) {
					rsT := tk.FormatTime(o.Value, ObjectsToS(args)...)

					return ToString(rsT), nil
				}}
			fT = o.Methods["format"]
		}
		return fT, nil
	case "add":
		fT, ok := o.Methods["add"]
		if !ok {
			o.Methods["add"] = &Function{
				Name: "add",
				Value: func(args ...Object) (Object, error) {
					if len(args) < 1 {
						return Undefined, NewCommonError("not enough parameters")
					}

					nv := ToInt(args[0], 0)
					if nv == 0 {
						return Undefined, NewCommonError("invalid parameter")
					}

					rsT := o.Value.Add(time.Second * time.Duration(nv))

					return NewDateTimeValue(rsT), nil
				}}
			fT = o.Methods["add"]
		}
		return fT, nil
	case "before":
		fT, ok := o.Methods["before"]
		if !ok {
			o.Methods["before"] = &Function{
				Name: "before",
				Value: func(args ...Object) (Object, error) {
					if len(args) < 1 {
						return Undefined, NewCommonError("not enough parameters")
					}

					nv, ok := args[0].(DateTime)
					if !ok {
						return Undefined, NewCommonError("invalid parameter")
					}

					rsT := o.Value.Before(nv.Value)

					return Bool(rsT), nil
				}}
			fT = o.Methods["before"]
		}
		return fT, nil
	}

	return nil, ErrNotIndexable
}

func (DateTime) IndexSet(index, value Object) error {
	return ErrNotIndexAssignable
}

func (o DateTime) BinaryOp(tok token.Token, right Object) (Object, error) {
	return nil, ErrInvalidOperator
}

func (o DateTime) Copy() Object {
	return DateTime{
		Value: o.Value,
	}
}

type StringBuilder struct {
	ObjectImpl
	Value *strings.Builder
}

var _ Object = StringBuilder{}

// func NewStringBuilder() *StringBuilder {
// 	return &StringBuilder{}
// }

// func NewStringBuilderValue(vA interface{}) StringBuilder {
// 	return StringBuilder{}
// }

func (o StringBuilder) TypeName() string {
	return "stringBuilder"
}

func (o StringBuilder) String() string {
	return fmt.Sprintf("%v", *(o.Value))
}

func (o StringBuilder) Equal(right Object) bool {
	if v, ok := right.(StringBuilder); ok {
		return o.Value.String() == v.Value.String()
	}

	return false
}

func (o StringBuilder) IsFalsy() bool { return false }

func (StringBuilder) CanCall() bool { return false }

func (StringBuilder) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

func (StringBuilder) CanIterate() bool { return false }

func (StringBuilder) Iterate() Iterator { return nil }

func (o StringBuilder) IndexGet(index Object) (value Object, err error) {
	nv, ok := index.(String)
	if !ok {
		return nil, ErrNotIndexable
	}

	fNameT := nv.Value

	if o.Members == nil {
		o.Members = map[string]Object{}
	}

	if o.Methods == nil {
		o.Methods = map[string]*Function{}
	}

	switch fNameT {
	case "write":
		fT, ok := o.Methods["write"]
		if !ok {
			o.Methods["write"] = &Function{
				Name: "write",
				Value: func(args ...Object) (Object, error) {

					countT := 0
					var errT error

					for _, v := range args {
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
				}}
			fT = o.Methods["write"]
		}
		return fT, nil
	case "writeString":
		fT, ok := o.Methods["writeString"]
		if !ok {
			o.Methods["writeString"] = &Function{
				Name: "writeString",
				Value: func(args ...Object) (Object, error) {
					if len(args) < 1 {
						return Int(0), NewCommonError("not enough parameters")
					}

					rsT, errT := o.Value.WriteString(args[0].String())

					if errT != nil {
						return Int(rsT), NewFromError(errT)
					}

					return Int(rsT), nil
				}}
			fT = o.Methods["writeString"]
		}
		return fT, nil
	case "writeBytes":
		fT, ok := o.Methods["writeBytes"]
		if !ok {
			o.Methods["writeBytes"] = &Function{
				Name: "writeBytes",
				Value: func(args ...Object) (Object, error) {
					if len(args) < 1 {
						return Int(0), NewCommonError("not enough parameters")
					}

					nv, ok := args[0].(Bytes)
					if !ok {
						return Int(0), NewCommonError("invalid parameter")
					}

					rsT, errT := o.Value.Write([]byte(nv))
					if errT != nil {
						return Int(rsT), NewFromError(errT)
					}

					return Int(rsT), nil
				}}
			fT = o.Methods["writeBytes"]
		}
		return fT, nil
	case "toString":
		fT, ok := o.Methods["toString"]
		if !ok {
			o.Methods["toString"] = &Function{
				Name: "toString",
				Value: func(args ...Object) (Object, error) {
					return ToString(o.Value.String()), nil
				}}
			fT = o.Methods["toString"]
		}
		return fT, nil
	case "reset", "clear":
		fT, ok := o.Methods["reset"]
		if !ok {
			o.Methods["reset"] = &Function{
				Name: "reset",
				Value: func(args ...Object) (Object, error) {
					o.Value.Reset()
					return Undefined, nil
				}}
			fT = o.Methods["reset"]
		}
		return fT, nil
	}

	return nil, ErrNotIndexable
}

func (StringBuilder) IndexSet(index, value Object) error {
	return ErrNotIndexAssignable
}

func (o StringBuilder) BinaryOp(tok token.Token, right Object) (Object, error) {
	return nil, ErrInvalidOperator
}

func (o StringBuilder) Copy() Object {
	rsT := StringBuilder{Value: new(strings.Builder)}

	rsT.Value.WriteString(o.Value.String())

	return rsT
}

type Database struct {
	ObjectImpl
	Value           *sql.DB
	DBType          string
	DBConnectString string
}

var _ Object = Database{}

// func NewStringBuilder() *StringBuilder {
// 	return &StringBuilder{}
// }

// func NewStringBuilderValue(vA interface{}) StringBuilder {
// 	return StringBuilder{}
// }

func (o Database) TypeName() string {
	return "database"
}

func (o Database) String() string {
	return fmt.Sprintf("%v", o)
}

func (o Database) Equal(right Object) bool {
	if v, ok := right.(Database); ok {
		return o.Value == v.Value
	}

	return false
}

func (o Database) IsFalsy() bool { return false }

func (Database) CanCall() bool { return false }

func (Database) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

func (Database) CanIterate() bool { return false }

func (Database) Iterate() Iterator { return nil }

func (o Database) IndexGet(index Object) (value Object, err error) {
	nv, ok := index.(String)
	if !ok {
		return nil, ErrNotIndexable
	}

	fNameT := nv.Value

	if o.Members == nil {
		o.Members = map[string]Object{}
	}

	if o.Methods == nil {
		o.Methods = map[string]*Function{}
	}

	switch fNameT {
	case "toAny":
		fT, ok := o.Methods["toAny"]
		if !ok {
			o.Methods["toAny"] = &Function{
				Name: "toAny",
				Value: func(args ...Object) (Object, error) {
					return NewAnyValue(o.Value), nil
				},
			}

			fT = o.Methods["toAny"]
		}
		return fT, nil
	case "connect":
		fT, ok := o.Methods["connect"]
		if !ok {
			o.Methods["connect"] = &Function{
				Name: "connect",
				Value: func(args ...Object) (Object, error) {
					aryT := ObjectsToS(args)

					rsT := sqltk.ConnectDBX(aryT[0], aryT[1])
					if tk.IsError(rsT) {
						return NewFromError(rsT.(error)), nil
					}

					o.Value = rsT.(*sql.DB)

					return Undefined, nil
				},
			}

			fT = o.Methods["connect"]
		}
		return fT, nil
	case "query":
		fT, ok := o.Methods["query"]
		if !ok {
			o.Methods["query"] = &Function{
				Name: "query",
				Value: func(args ...Object) (Object, error) {
					if len(args) < 1 {
						return NewCommonError("not enough parameters"), nil
					}

					s0, ok := args[0].(String)
					if !ok {
						return NewArgumentTypeError("first", "string", args[0].TypeName()), nil
					}

					objsT := ObjectsToI(args[1:])

					return ConvertToObject(sqltk.QueryDBX(o.Value, s0.Value, objsT...)), nil
				},
			}

			fT = o.Methods["query"]
		}
		return fT, nil
	case "queryRecs":
		fT, ok := o.Methods["queryRecs"]
		if !ok {
			o.Methods["queryRecs"] = &Function{
				Name: "queryRecs",
				Value: func(args ...Object) (Object, error) {
					if len(args) < 1 {
						return NewCommonError("not enough parameters"), nil
					}

					s0, ok := args[0].(String)
					if !ok {
						return NewArgumentTypeError("first", "string", args[0].TypeName()), nil
					}

					objsT := ObjectsToI(args[1:])

					return ConvertToObject(sqltk.QueryDBRecsX(o.Value, s0.Value, objsT...)), nil
				},
			}

			fT = o.Methods["queryRecs"]
		}
		return fT, nil
	case "queryMap":
		fT, ok := o.Methods["queryMap"]
		if !ok {
			o.Methods["queryMap"] = &Function{
				Name: "queryMap",
				Value: func(args ...Object) (Object, error) {
					if len(args) < 1 {
						return NewCommonError("not enough parameters"), nil
					}

					s0, ok := args[0].(String)
					if !ok {
						return NewArgumentTypeError("first", "string", args[0].TypeName()), nil
					}

					s1, ok := args[1].(String)
					if !ok {
						return NewArgumentTypeError("second", "string",
							args[1].TypeName()), nil
					}

					objsT := ObjectsToI(args[2:])

					return ConvertToObject(sqltk.QueryDBMapX(o.Value, s0.Value, s1.Value, objsT...)), nil
				},
			}

			fT = o.Methods["queryMap"]
		}
		return fT, nil
	case "queryMapArray":
		fT, ok := o.Methods["queryMapArray"]
		if !ok {
			o.Methods["queryMapArray"] = &Function{
				Name: "queryMapArray",
				Value: func(args ...Object) (Object, error) {
					if len(args) < 1 {
						return NewCommonError("not enough parameters"), nil
					}

					s0, ok := args[0].(String)
					if !ok {
						return NewArgumentTypeError("first", "string", args[0].TypeName()), nil
					}

					s1, ok := args[1].(String)
					if !ok {
						return NewArgumentTypeError("second", "string",
							args[1].TypeName()), nil
					}

					objsT := ObjectsToI(args[2:])

					return ConvertToObject(sqltk.QueryDBMapArrayX(o.Value, s0.Value, s1.Value, objsT...)), nil
				},
			}

			fT = o.Methods["queryMapArray"]
		}
		return fT, nil
	case "queryCount":
		fT, ok := o.Methods["queryCount"]
		if !ok {
			o.Methods["queryCount"] = &Function{
				Name: "queryCount",
				Value: func(args ...Object) (Object, error) {
					if len(args) < 1 {
						return NewCommonError("not enough parameters"), nil
					}

					s0, ok := args[0].(String)
					if !ok {
						return NewArgumentTypeError("first", "string", args[0].TypeName()), nil
					}

					objsT := ObjectsToI(args[1:])

					return ConvertToObject(sqltk.QueryCountX(o.Value, s0.Value, objsT...)), nil
				},
			}

			fT = o.Methods["queryCount"]
		}
		return fT, nil
	case "queryFloat":
		fT, ok := o.Methods["queryFloat"]
		if !ok {
			o.Methods["queryFloat"] = &Function{
				Name: "queryFloat",
				Value: func(args ...Object) (Object, error) {
					if len(args) < 1 {
						return NewCommonError("not enough parameters"), nil
					}

					s0, ok := args[0].(String)
					if !ok {
						return NewArgumentTypeError("first", "string", args[0].TypeName()), nil
					}

					objsT := ObjectsToI(args[1:])

					return ConvertToObject(sqltk.QueryFloatX(o.Value, s0.Value, objsT...)), nil
				},
			}

			fT = o.Methods["queryFloat"]
		}
		return fT, nil
	case "queryString":
		fT, ok := o.Methods["queryString"]
		if !ok {
			o.Methods["queryString"] = &Function{
				Name: "queryString",
				Value: func(args ...Object) (Object, error) {
					if len(args) < 1 {
						return NewCommonError("not enough parameters"), nil
					}

					s0, ok := args[0].(String)
					if !ok {
						return NewArgumentTypeError("first", "string", args[0].TypeName()), nil
					}

					objsT := ObjectsToI(args[1:])

					return ConvertToObject(sqltk.QueryStringX(o.Value, s0.Value, objsT...)), nil
				},
			}

			fT = o.Methods["queryString"]
		}
		return fT, nil
	case "exec":
		fT, ok := o.Methods["exec"]
		if !ok {
			o.Methods["exec"] = &Function{
				Name: "exec",
				Value: func(args ...Object) (Object, error) {
					if len(args) < 1 {
						return NewCommonError("not enough parameters"), nil
					}

					s0, ok := args[0].(String)
					if !ok {
						return NewArgumentTypeError("first", "string", args[0].TypeName()), nil
					}

					objsT := ObjectsToI(args[1:])

					return ConvertToObject(sqltk.ExecDBX(o.Value, s0.Value, objsT...)), nil
				},
			}

			fT = o.Methods["exec"]
		}
		return fT, nil
	case "close":
		fT, ok := o.Methods["close"]
		if !ok {
			o.Methods["close"] = &Function{
				Name: "close",
				Value: func(args ...Object) (Object, error) {
					o.Value.Close()
					return Undefined, nil
				},
			}

			fT = o.Methods["close"]
		}
		return fT, nil
	}
	return nil, ErrNotIndexable
}

func (Database) IndexSet(index, value Object) error {
	return ErrNotIndexAssignable
}

func (o Database) BinaryOp(tok token.Token, right Object) (Object, error) {
	return nil, ErrInvalidOperator
}

// Bool represents boolean values and implements Object interface.
type Bool bool

// TypeName implements Object interface.
func (Bool) TypeName() string {
	return "bool"
}

// String implements Object interface.
func (o Bool) String() string {
	if o {
		return "true"
	}
	return "false"
}

// Equal implements Object interface.
func (o Bool) Equal(right Object) bool {
	if v, ok := right.(Bool); ok {
		return o == v
	}

	if v, ok := right.(Int); ok {
		return bool((o && v == 1) || (!o && v == 0))
	}

	if v, ok := right.(Uint); ok {
		return bool((o && v == 1) || (!o && v == 0))
	}

	if v, ok := right.(Byte); ok {
		return bool((o && v == 1) || (!o && v == 0))
	}

	return false
}

// IsFalsy implements Object interface.
func (o Bool) IsFalsy() bool { return bool(!o) }

// CanCall implements Object interface.
func (Bool) CanCall() bool { return false }

// Call implements Object interface.
func (Bool) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

// CanIterate implements Object interface.
func (Bool) CanIterate() bool { return false }

// Iterate implements Object interface.
func (Bool) Iterate() Iterator { return nil }

// IndexGet implements Object interface.
func (Bool) IndexGet(index Object) (value Object, err error) {
	return nil, ErrNotIndexable
}

// IndexSet implements Object interface.
func (Bool) IndexSet(index, value Object) error {
	return ErrNotIndexAssignable
}

// BinaryOp implements Object interface.
func (o Bool) BinaryOp(tok token.Token, right Object) (Object, error) {
	bval := Int(0)
	if o {
		bval = Int(1)
	}
switchpos:
	switch v := right.(type) {
	case Int:
		switch tok {
		case token.Add:
			return bval + v, nil
		case token.Sub:
			return bval - v, nil
		case token.Mul:
			return bval * v, nil
		case token.Quo:
			if v == 0 {
				return nil, ErrZeroDivision
			}
			return bval / v, nil
		case token.Rem:
			return bval % v, nil
		case token.And:
			return bval & v, nil
		case token.Or:
			return bval | v, nil
		case token.Xor:
			return bval ^ v, nil
		case token.AndNot:
			return bval &^ v, nil
		case token.Shl:
			return bval << v, nil
		case token.Shr:
			return bval >> v, nil
		case token.Less:
			return Bool(bval < v), nil
		case token.LessEq:
			return Bool(bval <= v), nil
		case token.Greater:
			return Bool(bval > v), nil
		case token.GreaterEq:
			return Bool(bval >= v), nil
		}
	case Uint:
		bval := Uint(bval)
		switch tok {
		case token.Add:
			return bval + v, nil
		case token.Sub:
			return bval - v, nil
		case token.Mul:
			return bval * v, nil
		case token.Quo:
			if v == 0 {
				return nil, ErrZeroDivision
			}
			return bval / v, nil
		case token.Rem:
			return bval % v, nil
		case token.And:
			return bval & v, nil
		case token.Or:
			return bval | v, nil
		case token.Xor:
			return bval ^ v, nil
		case token.AndNot:
			return bval &^ v, nil
		case token.Shl:
			return bval << v, nil
		case token.Shr:
			return bval >> v, nil
		case token.Less:
			return Bool(bval < v), nil
		case token.LessEq:
			return Bool(bval <= v), nil
		case token.Greater:
			return Bool(bval > v), nil
		case token.GreaterEq:
			return Bool(bval >= v), nil
		}
	case Byte:
		bval := Byte(bval)
		switch tok {
		case token.Add:
			return bval + v, nil
		case token.Sub:
			return bval - v, nil
		case token.Mul:
			return bval * v, nil
		case token.Quo:
			if v == 0 {
				return nil, ErrZeroDivision
			}
			return bval / v, nil
		case token.Rem:
			return bval % v, nil
		case token.And:
			return bval & v, nil
		case token.Or:
			return bval | v, nil
		case token.Xor:
			return bval ^ v, nil
		case token.AndNot:
			return bval &^ v, nil
		case token.Shl:
			return bval << v, nil
		case token.Shr:
			return bval >> v, nil
		case token.Less:
			return Bool(bval < v), nil
		case token.LessEq:
			return Bool(bval <= v), nil
		case token.Greater:
			return Bool(bval > v), nil
		case token.GreaterEq:
			return Bool(bval >= v), nil
		}
	case Bool:
		if v {
			right = Int(1)
		} else {
			right = Int(0)
		}
		goto switchpos
	case undefined:
		switch tok {
		case token.Less, token.LessEq:
			return False, nil
		case token.Greater, token.GreaterEq:
			return True, nil
		}
	}
	return nil, NewOperandTypeError(
		tok.String(),
		o.TypeName(),
		right.TypeName())
}

// String represents string values and implements Object interface.
type String struct {
	ObjectImpl
	Value string
	// Members map[string]Object
	// Methods map[string]*Function
}

// TypeName implements Object interface.
func (String) TypeName() string {
	return "string"
}

func (o String) String() string {
	return o.Value
}

// CanIterate implements Object interface.
func (String) CanIterate() bool { return true }

// Iterate implements Object interface.
func (o String) Iterate() Iterator {
	return &StringIterator{V: o.Value}
}

// IndexSet implements Object interface.
func (String) IndexSet(index, value Object) error {
	return ErrNotIndexAssignable
}

func dealStringMethods(o String, fNameA string) (Object, error) {

	if o.Members == nil {
		o.Members = map[string]Object{}
	}

	if o.Methods == nil {
		o.Methods = map[string]*Function{}
	}

	switch fNameA {
	case "trim":
		fT, ok := o.Methods["trim"]
		if !ok {
			o.Methods["trim"] = &Function{
				Name: "trim",
				Value: func(args ...Object) (Object, error) {
					if len(args) < 1 {
						return ToString(strings.TrimSpace(FromString(o))), nil
					}

					return ToString(strings.Trim(FromString(o), args[0].String())), nil
				}}
			fT = o.Methods["trim"]
		}
		return fT, nil
	case "startsWith":
		fT, ok := o.Methods["startsWith"]
		if !ok {
			o.Methods["startsWith"] = &Function{
				Name: "startsWith",
				Value: func(args ...Object) (Object, error) {
					if len(args) < 1 {
						return Bool(false), nil
					}

					return Bool(strings.HasPrefix(FromString(o), args[0].String())), nil
				}}
			fT = o.Methods["startsWith"]
		}
		return fT, nil
	case "endsWith":
		fT, ok := o.Methods["endsWith"]
		if !ok {
			o.Methods["endsWith"] = &Function{
				Name: "endsWith",
				Value: func(args ...Object) (Object, error) {
					if len(args) < 1 {
						return Bool(false), nil
					}

					return Bool(strings.HasSuffix(FromString(o), args[0].String())), nil
				}}
			fT = o.Methods["endsWith"]
		}
		return fT, nil
	case "isEmpty":
		fT, ok := o.Methods["isEmpty"]
		if !ok {
			o.Methods["isEmpty"] = &Function{
				Name: "isEmpty",
				Value: func(args ...Object) (Object, error) {
					return Bool(FromString(o) == ""), nil
				}}
			fT = o.Methods["isEmpty"]
		}
		return fT, nil
	case "isEmptyTrim":
		fT, ok := o.Methods["isEmptyTrim"]
		if !ok {
			o.Methods["isEmptyTrim"] = &Function{
				Name: "isEmptyTrim",
				Value: func(args ...Object) (Object, error) {
					return Bool(strings.TrimSpace(FromString(o)) == ""), nil
				}}
			fT = o.Methods["isEmptyTrim"]
		}
		return fT, nil
		// return &Function{
		// 	Name: "isEmptyTrim",
		// 	Value: func(args ...Object) (Object, error) {
		// 		return Bool(strings.TrimSpace(FromString(o)) == ""), nil
		// 	}}, nil
	case "contains":
		fT, ok := o.Methods["contains"]
		if !ok {
			o.Methods["contains"] = &Function{
				Name: "contains",
				Value: func(args ...Object) (Object, error) {
					if len(args) < 1 {
						return Bool(false), nil
					}

					return Bool(strings.Contains(FromString(o), args[0].String())), nil
				}}
			fT = o.Methods["contains"]
		}
		return fT, nil

	case "regMatch":
		fT, ok := o.Methods["regMatch"]
		if !ok {
			o.Methods["regMatch"] = &Function{
				Name: "regMatch",
				Value: func(args ...Object) (Object, error) {
					if len(args) < 1 {
						return Bool(false), nil
					}

					return Bool(tk.RegMatchX(FromString(o), args[0].String())), nil
				}}
			fT = o.Methods["regMatch"]
		}
		return fT, nil
	case "regContains":
		fT, ok := o.Methods["regContains"]
		if !ok {
			o.Methods["regContains"] = &Function{
				Name: "regContains",
				Value: func(args ...Object) (Object, error) {
					if len(args) < 1 {
						return Bool(false), nil
					}

					return Bool(tk.RegContainsX(FromString(o), args[0].String())), nil
				}}
			fT = o.Methods["regContains"]
		}
		return fT, nil
	}

	return nil, ErrNotIndexable
}

// IndexGet represents string values and implements Object interface.
func (o String) IndexGet(index Object) (Object, error) {
	var idx int
	switch v := index.(type) {
	case Int:
		idx = int(v)
	case Uint:
		idx = int(v)
	case Byte:
		idx = int(v)
	case Char:
		idx = int(v)
	case String:
		return dealStringMethods(o, v.String())
	default:
		return nil, NewIndexTypeError("int|uint|char", index.TypeName())
	}
	if idx >= 0 && idx < len(o.Value) {
		return Int(o.Value[idx]), nil
	}
	return nil, ErrIndexOutOfBounds
}

// Equal implements Object interface.
func (o String) Equal(right Object) bool {
	if v, ok := right.(String); ok {
		return o.Value == v.Value
	}
	if v, ok := right.(Bytes); ok {
		return string(o.Value) == string(v)
	}
	return false
}

// IsFalsy implements Object interface.
func (o String) IsFalsy() bool { return len(o.Value) == 0 }

// CanCall implements Object interface.
func (o String) CanCall() bool { return false }

// Call implements Object interface.
func (o String) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

func ToString(argA interface{}) String {
	switch nv := argA.(type) {
	case String:
		return String{Value: nv.Value}
	case Object:
		return String{Value: nv.String()}
	case string:
		return String{Value: nv}
	case nil:
		return String{Value: ""}
	case []byte:
		return String{Value: string(nv)}
	}
	return String{Value: fmt.Sprintf("%v", argA)}
}

func ToInt(argA interface{}, defaultA ...int) Int {
	defaultT := 0
	if len(defaultA) > 0 {
		defaultT = defaultA[0]
	}
	switch nv := argA.(type) {
	case String:
		return Int(tk.StrToInt(nv.Value, defaultT))
	case Object:
		return Int(tk.StrToInt(nv.String(), defaultT))
	case string:
		return Int(tk.StrToInt(nv, defaultT))
	case nil:
		return Int(defaultT)
	case []byte:
		return Int(tk.StrToInt(string(nv), defaultT))
	}
	return Int(defaultT)
}

// func ToString(argA interface{}) Object {
// 	switch nv := argA.(type) {
// 	case String:
// 		return &String{Value: nv.Value}
// 	case Object:
// 		return &String{Value: nv.String()}
// 	case string:
// 		return &String{Value: nv}
// 	case nil:
// 		return &String{Value: ""}
// 	case []byte:
// 		return &String{Value: string(nv)}
// 	}
// 	return &String{Value: fmt.Sprintf("%v", argA)}
// }

func FromString(argA String) string {
	return argA.Value
}

// BinaryOp implements Object interface.
func (o String) BinaryOp(tok token.Token, right Object) (Object, error) {
	switch v := right.(type) {
	case String:
		switch tok {
		case token.Add:
			return String{Value: o.Value + v.Value}, nil
		case token.Less:
			return Bool(o.Value < v.Value), nil
		case token.LessEq:
			return Bool(o.Value <= v.Value), nil
		case token.Greater:
			return Bool(o.Value > v.Value), nil
		case token.GreaterEq:
			return Bool(o.Value >= v.Value), nil
		}
	case Bytes:
		switch tok {
		case token.Add:
			var sb strings.Builder
			sb.WriteString(o.Value)
			sb.Write(v)
			return ToString(sb.String()), nil
		case token.Less:
			return Bool(o.Value < string(v)), nil
		case token.LessEq:
			return Bool(o.Value <= string(v)), nil
		case token.Greater:
			return Bool(o.Value > string(v)), nil
		case token.GreaterEq:
			return Bool(o.Value >= string(v)), nil
		}
	case undefined:
		switch tok {
		case token.Less, token.LessEq:
			return False, nil
		case token.Greater, token.GreaterEq:
			return True, nil
		}
	}

	if tok == token.Add {
		return ToString(o.Value + right.String()), nil
	}

	return nil, NewOperandTypeError(
		tok.String(),
		o.TypeName(),
		right.TypeName())
}

// Bytes represents byte slice and implements Object interface.
type Bytes []byte

var _ Object = Bytes{}

// TypeName implements Object interface.
func (Bytes) TypeName() string {
	return "bytes"
}

func (o Bytes) String() string {
	return string(o)
}

// Copy implements Copier interface.
func (o Bytes) Copy() Object {
	cp := make(Bytes, len(o))
	copy(cp, o)
	return cp
}

// CanIterate implements Object interface.
func (Bytes) CanIterate() bool { return true }

// Iterate implements Object interface.
func (o Bytes) Iterate() Iterator {
	return &BytesIterator{V: o}
}

// IndexSet implements Object interface.
func (o Bytes) IndexSet(index, value Object) error {
	var idx int
	switch v := index.(type) {
	case Int:
		idx = int(v)
	case Uint:
		idx = int(v)
	case Byte:
		idx = int(v)
	default:
		return NewIndexTypeError("int|uint|byte", index.TypeName())
	}

	if idx >= 0 && idx < len(o) {
		switch v := value.(type) {
		case Int:
			o[idx] = byte(v)
		case Uint:
			o[idx] = byte(v)
		case Byte:
			o[idx] = byte(v)
		default:
			return NewIndexValueTypeError("int|uint|byte", value.TypeName())
		}
		return nil
	}
	return ErrIndexOutOfBounds
}

// IndexGet represents string values and implements Object interface.
func (o Bytes) IndexGet(index Object) (Object, error) {
	var idx int
	switch v := index.(type) {
	case Int:
		idx = int(v)
	case Uint:
		idx = int(v)
	case Byte:
		idx = int(v)
	default:
		return nil, NewIndexTypeError("int|uint|char|byte", index.TypeName())
	}

	if idx >= 0 && idx < len(o) {
		return Int(o[idx]), nil
	}
	return nil, ErrIndexOutOfBounds
}

// Equal implements Object interface.
func (o Bytes) Equal(right Object) bool {
	if v, ok := right.(Bytes); ok {
		return string(o) == string(v)
	}

	if v, ok := right.(String); ok {
		return string(o) == FromString(v)
	}
	return false
}

// IsFalsy implements Object interface.
func (o Bytes) IsFalsy() bool { return len(o) == 0 }

// CanCall implements Object interface.
func (o Bytes) CanCall() bool { return false }

// Call implements Object interface.
func (o Bytes) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

// BinaryOp implements Object interface.
func (o Bytes) BinaryOp(tok token.Token, right Object) (Object, error) {
	switch v := right.(type) {
	case Bytes:
		switch tok {
		case token.Add:
			return append(o, v...), nil
		case token.Less:
			return Bool(bytes.Compare([]byte(o), []byte(v)) == -1), nil
		case token.LessEq:
			cmp := bytes.Compare([]byte(o), []byte(v))
			return Bool(cmp == 0 || cmp == -1), nil
		case token.Greater:
			return Bool(bytes.Compare([]byte(o), []byte(v)) == 1), nil
		case token.GreaterEq:
			cmp := bytes.Compare([]byte(o), []byte(v))
			return Bool(cmp == 0 || cmp == 1), nil
		}
	case String:
		switch tok {
		case token.Add:
			return append(o, FromString(v)...), nil
		case token.Less:
			return Bool(string(o) < FromString(v)), nil
		case token.LessEq:
			return Bool(string(o) <= FromString(v)), nil
		case token.Greater:
			return Bool(string(o) > FromString(v)), nil
		case token.GreaterEq:
			return Bool(string(o) >= FromString(v)), nil
		}
	case undefined:
		switch tok {
		case token.Less, token.LessEq:
			return False, nil
		case token.Greater, token.GreaterEq:
			return True, nil
		}
	}
	return nil, NewOperandTypeError(
		tok.String(),
		o.TypeName(),
		right.TypeName())
}

// Function represents a function object and implements Object interface.
type Function struct {
	ObjectImpl
	Name  string
	Value func(args ...Object) (Object, error)
	Self  Object
}

var _ Object = (*Function)(nil)

// TypeName implements Object interface.
func (*Function) TypeName() string {
	return "function"
}

// String implements Object interface.
func (o *Function) String() string {
	return fmt.Sprintf("<function:%s(%p)>", o.Name, o.Value)
}

// Copy implements Copier interface.
func (o *Function) Copy() Object {
	return &Function{
		Name:  o.Name,
		Value: o.Value,
	}
}

// Equal implements Object interface.
func (o *Function) Equal(right Object) bool {
	v, ok := right.(*Function)
	if !ok {
		return false
	}
	return v == o
}

// IsFalsy implements Object interface.
func (*Function) IsFalsy() bool { return false }

// CanCall implements Object interface.
func (*Function) CanCall() bool { return true }

// Call implements Object interface.
func (o *Function) Call(args ...Object) (Object, error) {
	return o.Value(args...)
}

// BuiltinFunction represents a builtin function object and implements Object interface.
type BuiltinFunction struct {
	ObjectImpl
	Name    string
	Value   func(args ...Object) (Object, error)
	Remark  string
	Members map[string]Object
	Methods map[string]*Function
}

var _ Object = (*BuiltinFunction)(nil)

// TypeName implements Object interface.
func (*BuiltinFunction) TypeName() string {
	return "builtin-function"
}

// String implements Object interface.
func (o *BuiltinFunction) String() string {
	return fmt.Sprintf("<builtin-function:%s>", o.Name)
}

// Copy implements Copier interface.
func (o *BuiltinFunction) Copy() Object {
	return &BuiltinFunction{
		Name:  o.Name,
		Value: o.Value,
	}
}

// Equal implements Object interface.
func (o *BuiltinFunction) Equal(right Object) bool {
	v, ok := right.(*BuiltinFunction)
	if !ok {
		return false
	}
	return v == o
}

// IsFalsy implements Object interface.
func (*BuiltinFunction) IsFalsy() bool { return false }

// CanCall implements Object interface.
func (*BuiltinFunction) CanCall() bool { return true }

// Call implements Object interface.
func (o *BuiltinFunction) Call(args ...Object) (Object, error) {
	return o.Value(args...)
}

func (o *BuiltinFunction) IndexGet(index Object) (value Object, err error) {
	nv, ok := index.(String)
	if !ok {
		return nil, ErrNotIndexable
	}

	fNameT := o.Name + "." + FromString(nv)

	if o.Members == nil {
		o.Members = map[string]Object{}
	}

	if o.Methods == nil {
		o.Methods = map[string]*Function{}
	}

	switch fNameT {
	case "any.new":
		fT, ok := o.Methods["any.new"]
		if !ok {
			o.Methods["any.new"] = &Function{
				Name: "any.new",
				Value: func(args ...Object) (Object, error) {
					return builtinNewAnyFunc(args...)
				}}
			fT = o.Methods["any.new"]
		}

		return fT, nil
	case "database.connect":
		fT, ok := o.Methods["database.connect"]
		if !ok {
			o.Methods["database.connect"] = &Function{
				Name: "database.connect",
				Value: func(args ...Object) (Object, error) {

					nv0, ok := args[0].(String)

					if !ok {
						return NewCommonError("invalid paramter 1"), nil
					}

					nv1, ok := args[1].(String)

					if !ok {
						return NewCommonError("invalid paramter 2"), nil
					}

					rsT := sqltk.ConnectDBX(nv0.Value, nv1.Value)
					if tk.IsError(rsT) {
						return NewFromError(rsT.(error)), nil
					}

					return Database{DBType: nv0.Value, DBConnectString: nv1.String(), Value: rsT.(*sql.DB)}, nil
				}}
			fT = o.Methods["database.connect"]
		}

		return fT, nil
	case "database.formatSQLValue", "database.format":
		fT, ok := o.Methods["database.formatSQLValue"]
		if !ok {
			o.Methods["database.formatSQLValue"] = &Function{
				Name: "database.formatSQLValue",
				Value: func(args ...Object) (Object, error) {
					if len(args) < 1 {
						return NewCommonError("not enough paramters"), nil
					}

					nv0 := args[0].String()

					return ToString(sqltk.FormatSQLValue(nv0)), nil
				}}
			fT = o.Methods["database.formatSQLValue"]
		}

		return fT, nil
	case "statusResult.success":
		fT, ok := o.Methods["statusResult.success"]
		if !ok {
			o.Methods["statusResult.success"] = &Function{
				Name: "statusResult.success",
				Value: func(args ...Object) (Object, error) {
					if len(args) < 1 {
						return StatusResultSuccess, nil
					}

					return StatusResult{Status: "success", Value: args[0].String()}, nil
				}}
			fT = o.Methods["statusResult.success"]
		}

		return fT, nil
	case "statusResult.fail":
		fT, ok := o.Methods["statusResult.fail"]
		if !ok {
			o.Methods["statusResult.fail"] = &Function{
				Name: "statusResult.fail",
				Value: func(args ...Object) (Object, error) {
					if len(args) < 1 {
						return StatusResultFail, nil
					}

					return StatusResult{Status: "fail", Value: args[0].String()}, nil
				}}
			fT = o.Methods["statusResult.fail"]
		}

		return fT, nil
	case "dateTime.format":
		fT, ok := o.Methods["dateTime.format"]
		if !ok {
			o.Methods["dateTime.format"] = &Function{
				Name: "dateTime.format",
				Value: func(args ...Object) (Object, error) {
					if len(args) < 1 {
						return ToString(tk.FormatTime(time.Now(), ObjectsToS(args)...)), nil
					}

					return ToString(tk.FormatTime(args[0].(DateTime).Value, ObjectsToS(args[1:])...)), nil
				}}
			fT = o.Methods["dateTime.format"]
		}

		return fT, nil
	case "dateTime.now":
		fT, ok := o.Methods["dateTime.now"]
		if !ok {
			o.Methods["dateTime.now"] = &Function{
				Name: "dateTime.now",
				Value: func(args ...Object) (Object, error) {
					return DateTime{Value: time.Now()}, nil
				}}
			fT = o.Methods["dateTime.now"]
		}

		return fT, nil
	case "dateTime.timeFormatRFC1123":
		mT, ok := o.Members["dateTime.timeFormatRFC1123"]
		if !ok {
			o.Members["dateTime.timeFormatRFC1123"] = ToString(time.RFC1123)
			mT = o.Members["dateTime.timeFormatRFC1123"]
		}

		return mT, nil
	case "dateTime.second":
		mT, ok := o.Members["dateTime.second"]
		if !ok {
			o.Members["dateTime.second"] = Int(time.Second)
			mT = o.Members["dateTime.second"]
		}

		return mT, nil
	case "dateTime.timeFormatCompact":
		mT, ok := o.Members["dateTime.timeFormatCompact"]
		if !ok {
			o.Members["dateTime.timeFormatCompact"] = ToString(tk.TimeFormatCompact)
			mT = o.Members["dateTime.timeFormatCompact"]
		}

		return mT, nil
	case "dateTime.timeFormat":
		mT, ok := o.Members["dateTime.timeFormat"]
		if !ok {
			o.Members["dateTime.timeFormat"] = ToString(tk.TimeFormat)
			mT = o.Members["dateTime.timeFormat"]
		}

		return mT, nil
	case "dateTime.timeFormatMS":
		mT, ok := o.Members["dateTime.timeFormatMS"]
		if !ok {
			o.Members["dateTime.timeFormatMS"] = ToString(tk.TimeFormatMS)
			mT = o.Members["dateTime.timeFormatMS"]
		}

		return mT, nil
	case "dateTime.timeFormatMSCompact":
		mT, ok := o.Members["dateTime.timeFormatMSCompact"]
		if !ok {
			o.Members["dateTime.timeFormatMSCompact"] = ToString(tk.TimeFormatMSCompact)
			mT = o.Members["dateTime.timeFormatMSCompact"]
		}

		return mT, nil
	}

	return nil, ErrNotIndexable
}

// Array represents array of objects and implements Object interface.
type Array []Object

var _ Object = Array{}

// TypeName implements Object interface.
func (Array) TypeName() string {
	return "array"
}

// String implements Object interface.
func (o Array) String() string {
	var sb strings.Builder
	sb.WriteString("[")
	last := len(o) - 1

	for i := range o {
		switch v := o[i].(type) {
		case String:
			sb.WriteString(strconv.Quote(v.String()))
		case Char:
			sb.WriteString(strconv.QuoteRune(rune(v)))
		case Bytes:
			sb.WriteString(fmt.Sprint([]byte(v)))
		default:
			sb.WriteString(v.String())
		}
		if i != last {
			sb.WriteString(", ")
		}
	}

	sb.WriteString("]")
	return sb.String()
}

// Copy implements Copier interface.
func (o Array) Copy() Object {
	cp := make(Array, len(o))
	for i, v := range o {
		if vv, ok := v.(Copier); ok {
			cp[i] = vv.Copy()
		} else {
			cp[i] = v
		}
	}
	return cp
}

// IndexSet implements Object interface.
func (o Array) IndexSet(index, value Object) error {
	switch v := index.(type) {
	case Int:
		idx := int(v)
		if idx >= 0 && idx < len(o) {
			o[v] = value
			return nil
		}
		return ErrIndexOutOfBounds
	case Uint:
		idx := int(v)
		if idx >= 0 && idx < len(o) {
			o[v] = value
			return nil
		}
		return ErrIndexOutOfBounds
	case Byte:
		idx := int(v)
		if idx >= 0 && idx < len(o) {
			o[v] = value
			return nil
		}
		return ErrIndexOutOfBounds
	}
	return NewIndexTypeError("int|uint", index.TypeName())
}

// IndexGet implements Object interface.
func (o Array) IndexGet(index Object) (Object, error) {
	switch v := index.(type) {
	case Int:
		idx := int(v)
		if idx >= 0 && idx < len(o) {
			return o[v], nil
		}
		return nil, ErrIndexOutOfBounds
	case Uint:
		idx := int(v)
		if idx >= 0 && idx < len(o) {
			return o[v], nil
		}
		return nil, ErrIndexOutOfBounds
	case Byte:
		idx := int(v)
		if idx >= 0 && idx < len(o) {
			return o[v], nil
		}
		return nil, ErrIndexOutOfBounds
	}
	return nil, NewIndexTypeError("int|uint", index.TypeName())
}

// Equal implements Object interface.
func (o Array) Equal(right Object) bool {
	v, ok := right.(Array)
	if !ok {
		return false
	}

	if len(o) != len(v) {
		return false
	}

	for i := range o {
		if !o[i].Equal(v[i]) {
			return false
		}
	}
	return true
}

// IsFalsy implements Object interface.
func (o Array) IsFalsy() bool { return len(o) == 0 }

// CanCall implements Object interface.
func (Array) CanCall() bool { return false }

// Call implements Object interface.
func (Array) Call(...Object) (Object, error) {
	return nil, ErrNotCallable
}

// BinaryOp implements Object interface.
func (o Array) BinaryOp(tok token.Token, right Object) (Object, error) {
	switch tok {
	case token.Add:
		if v, ok := right.(Array); ok {
			arr := make(Array, 0, len(o)+len(v))
			arr = append(arr, o...)
			arr = append(arr, v...)
			return arr, nil
		}

		arr := make(Array, 0, len(o)+1)
		arr = append(arr, o...)
		arr = append(arr, right)
		return arr, nil
	}
	if right == Undefined {
		switch tok {
		case token.Less, token.LessEq:
			return False, nil
		case token.Greater, token.GreaterEq:
			return True, nil
		}
	}
	return nil, NewOperandTypeError(
		tok.String(),
		o.TypeName(),
		right.TypeName())
}

// CanIterate implements Object interface.
func (Array) CanIterate() bool { return true }

// Iterate implements Iterable interface.
func (o Array) Iterate() Iterator {
	return &ArrayIterator{V: o}
}

// ObjectPtr represents a pointer variable.
type ObjectPtr struct {
	ObjectImpl
	Value *Object
}

var _ Object = (*ObjectPtr)(nil)

// TypeName implements Object interface.
func (o *ObjectPtr) TypeName() string {
	return "object-ptr"
}

// String implements Object interface.
func (o *ObjectPtr) String() string {
	var v Object
	if o.Value != nil {
		v = *o.Value
	}
	return fmt.Sprintf("<object-ptr:%v>", v)
}

// Copy implements Copier interface.
func (o *ObjectPtr) Copy() Object {
	return o
}

// IsFalsy implements Object interface.
func (o *ObjectPtr) IsFalsy() bool {
	return o.Value == nil
}

// Equal implements Object interface.
func (o *ObjectPtr) Equal(x Object) bool {
	return o == x
}

// BinaryOp implements Object interface.
func (o *ObjectPtr) BinaryOp(tok token.Token, right Object) (Object, error) {
	if o.Value == nil {
		return nil, errors.New("nil pointer")
	}
	return (*o.Value).BinaryOp(tok, right)
}

// CanCall implements Object interface.
func (o *ObjectPtr) CanCall() bool {
	if o.Value == nil {
		return false
	}
	return (*o.Value).CanCall()
}

// Call implements Object interface.
func (o *ObjectPtr) Call(args ...Object) (Object, error) {
	if o.Value == nil {
		return nil, errors.New("nil pointer")
	}
	return (*o.Value).Call(args...)
}

// Map represents map of objects and implements Object interface.
type Map map[string]Object

var _ Object = Map{}

func MssToMap(vA map[string]string) Map {
	inParasT := make(Map, len(vA))
	for k, v := range vA {
		inParasT[k] = ToString(v)
	}

	return inParasT
}

func MsiToMap(vA map[string]interface{}) Map {
	inParasT := make(Map, len(vA))
	for k, v := range vA {
		inParasT[k] = ToString(fmt.Sprintf("%v", v))
	}

	return inParasT
}

// TypeName implements Object interface.
func (Map) TypeName() string {
	return "map"
}

// String implements Object interface.
func (o Map) String() string {
	var sb strings.Builder
	sb.WriteString("{")
	last := len(o) - 1
	i := 0

	for k := range o {
		sb.WriteString(strconv.Quote(k))
		sb.WriteString(": ")
		switch v := o[k].(type) {
		case String:
			sb.WriteString(strconv.Quote(v.String()))
		case Char:
			sb.WriteString(strconv.QuoteRune(rune(v)))
		case Bytes:
			sb.WriteString(fmt.Sprint([]byte(v)))
		default:
			sb.WriteString(v.String())
		}
		if i != last {
			sb.WriteString(", ")
		}
		i++
	}

	sb.WriteString("}")
	return sb.String()
}

// Copy implements Copier interface.
func (o Map) Copy() Object {
	cp := make(Map, len(o))
	for k, v := range o {
		if vv, ok := v.(Copier); ok {
			cp[k] = vv.Copy()
		} else {
			cp[k] = v
		}
	}
	return cp
}

// IndexSet implements Object interface.
func (o Map) IndexSet(index, value Object) error {
	o[index.String()] = value
	return nil
}

// IndexGet implements Object interface.
func (o Map) IndexGet(index Object) (Object, error) {
	v, ok := o[index.String()]
	if ok {
		return v, nil
	}
	return Undefined, nil
}

// Equal implements Object interface.
func (o Map) Equal(right Object) bool {
	v, ok := right.(Map)
	if !ok {
		return false
	}

	if len(o) != len(v) {
		return false
	}

	for k := range o {
		right, ok := v[k]
		if !ok {
			return false
		}
		if !o[k].Equal(right) {
			return false
		}
	}
	return true
}

// IsFalsy implements Object interface.
func (o Map) IsFalsy() bool { return len(o) == 0 }

// CanCall implements Object interface.
func (Map) CanCall() bool { return false }

// Call implements Object interface.
func (Map) Call(...Object) (Object, error) {
	return nil, ErrNotCallable
}

// BinaryOp implements Object interface.
func (o Map) BinaryOp(tok token.Token, right Object) (Object, error) {
	if right == Undefined {
		switch tok {
		case token.Less, token.LessEq:
			return False, nil
		case token.Greater, token.GreaterEq:
			return True, nil
		}
	}

	return nil, NewOperandTypeError(
		tok.String(),
		o.TypeName(),
		right.TypeName())
}

// CanIterate implements Object interface.
func (Map) CanIterate() bool { return true }

// Iterate implements Iterable interface.
func (o Map) Iterate() Iterator {
	keys := make([]string, 0, len(o))
	for k := range o {
		keys = append(keys, k)
	}
	return &MapIterator{V: o, keys: keys}
}

// SyncMap represents map of objects and implements Object interface.
type SyncMap struct {
	mu sync.RWMutex
	Map
}

var _ Object = (*SyncMap)(nil)

// TypeName implements Object interface.
func (*SyncMap) TypeName() string {
	return "sync-map"
}

// String implements Object interface.
func (o *SyncMap) String() string {
	o.mu.RLock()
	defer o.mu.RUnlock()
	if o.Map != nil {
		return o.Map.String()
	}
	return ""
}

// Copy implements Copier interface.
func (o *SyncMap) Copy() Object {
	if o.Map == nil {
		return &SyncMap{
			Map: Map{},
		}
	}

	return &SyncMap{
		Map: o.Map.Copy().(Map),
	}
}

// IndexSet implements Object interface.
func (o *SyncMap) IndexSet(index, value Object) error {
	o.mu.Lock()
	defer o.mu.Unlock()

	if o.Map != nil {
		return o.Map.IndexSet(index, value)
	}
	return nil
}

// IndexGet implements Object interface.
func (o *SyncMap) IndexGet(index Object) (Object, error) {
	o.mu.RLock()
	defer o.mu.RUnlock()

	if o.Map != nil {
		return o.Map.IndexGet(index)
	}
	return Undefined, nil
}

// Equal implements Object interface.
func (o *SyncMap) Equal(right Object) bool {
	o.mu.RLock()
	defer o.mu.RUnlock()

	if o.Map != nil {
		return o.Map.Equal(right)
	}
	return false
}

// IsFalsy implements Object interface.
func (o *SyncMap) IsFalsy() bool {
	o.mu.RLock()
	defer o.mu.RUnlock()

	if o.Map != nil {
		return o.Map.IsFalsy()
	}
	return true
}

// CanIterate implements Object interface.
func (o *SyncMap) CanIterate() bool { return true }

// Iterate implements Iterable interface.
func (o *SyncMap) Iterate() Iterator {
	o.mu.RLock()
	defer o.mu.RUnlock()

	if o.Map == nil {
		return &MapIterator{V: Map{}, keys: []string{}}
	}
	return &SyncIterator{Iterator: o.Map.Iterate()}
}

// Get returns Object in map if exists.
func (o *SyncMap) Get(index string) (value Object, exists bool) {
	o.mu.RLock()
	defer o.mu.RUnlock()

	value, exists = o.Map[index]
	return
}

// Error represents Error Object and implements error and Object interfaces.
type Error struct {
	Name    string
	Message string
	Cause   error
}

var _ Object = (*Error)(nil)

func (o *Error) Unwrap() error {
	if o.Cause == nil {
		if o.Message != "" {
			return fmt.Errorf("%v: %v", o.Name, o.Message)
		}
	}
	return o.Cause
}

func WrapError(errA error) *Error {
	return &Error{Name: "Error", Message: errA.Error(), Cause: errA}
}

// TypeName implements Object interface.
func (*Error) TypeName() string {
	return "error"
}

// String implements Object interface.
func (o *Error) String() string {
	return o.Error()
}

// Copy implements Copier interface.
func (o *Error) Copy() Object {
	return &Error{
		Name:    o.Name,
		Message: o.Message,
		Cause:   o.Cause,
	}
}

// Error implements error interface.
func (o *Error) Error() string {
	name := o.Name
	if name == "" {
		name = "error"
	}
	return fmt.Sprintf("%s: %s", name, o.Message)
}

// Equal implements Object interface.
func (o *Error) Equal(right Object) bool {
	if v, ok := right.(*Error); ok {
		return v == o
	}
	return false
}

// IsFalsy implements Object interface.
func (o *Error) IsFalsy() bool { return true }

// IndexGet implements Object interface.
func (o *Error) IndexGet(index Object) (Object, error) {
	s := index.String()
	if s == "Name" {
		return ToString(o.Name), nil
	}

	if s == "Message" {
		return ToString(o.Message), nil
	}

	if s == "New" {
		return &Function{
			Name: "New",
			Value: func(args ...Object) (Object, error) {
				switch len(args) {
				case 1:
					return o.NewError(args[0].String()), nil
				case 0:
					return o.NewError(o.Message), nil
				default:
					msgs := make([]string, len(args))
					for i := range args {
						msgs[i] = args[0].String()
					}
					return o.NewError(msgs...), nil
				}
			},
		}, nil
	}
	return Undefined, nil
}

// NewError creates a new Error and sets original Error as its cause which can be unwrapped.
func (o *Error) NewError(messages ...string) *Error {
	cp := o.Copy().(*Error)
	cp.Message = strings.Join(messages, " ")
	cp.Cause = o
	return cp
}

// IndexSet implements Object interface.
func (*Error) IndexSet(index, value Object) error {
	return ErrNotIndexAssignable
}

// BinaryOp implements Object interface.
func (o *Error) BinaryOp(tok token.Token, right Object) (Object, error) {
	return nil, ErrInvalidOperator
}

// CanCall implements Object interface.
func (*Error) CanCall() bool { return false }

// Call implements Object interface.
func (*Error) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

// CanIterate implements Object interface.
func (*Error) CanIterate() bool { return false }

// Iterate implements Object interface.
func (*Error) Iterate() Iterator { return nil }

// RuntimeError represents a runtime error that wraps Error and includes trace information.
type RuntimeError struct {
	Err     *Error
	fileSet *parser.SourceFileSet
	Trace   []parser.Pos
}

func (o *RuntimeError) Unwrap() error {
	if o.Err != nil {
		return o.Err
	}
	return nil
}

func (o *RuntimeError) addTrace(pos parser.Pos) {
	if len(o.Trace) > 0 {
		if o.Trace[len(o.Trace)-1] == pos {
			return
		}
	}
	o.Trace = append(o.Trace, pos)
}

// TypeName implements Object interface.
func (*RuntimeError) TypeName() string {
	return "error"
}

// String implements Object interface.
func (o *RuntimeError) String() string {
	return o.Error()
}

// Copy implements Copier interface.
func (o *RuntimeError) Copy() Object {
	var err *Error
	if o.Err != nil {
		err = o.Err.Copy().(*Error)
	}

	return &RuntimeError{
		Err:     err,
		fileSet: o.fileSet,
		Trace:   append([]parser.Pos{}, o.Trace...),
	}
}

// Error implements error interface.
func (o *RuntimeError) Error() string {
	if o.Err == nil {
		return "<nil>"
	}
	return o.Err.Error()
}

// Equal implements Object interface.
func (o *RuntimeError) Equal(right Object) bool {
	if o.Err != nil {
		return o.Err.Equal(right)
	}
	return false
}

// IsFalsy implements Object interface.
func (o *RuntimeError) IsFalsy() bool { return true }

// IndexGet implements Object interface.
func (o *RuntimeError) IndexGet(index Object) (Object, error) {
	if o.Err != nil {
		s := index.String()
		if s == "New" {
			return &Function{
				Name: "New",
				Value: func(args ...Object) (Object, error) {
					switch len(args) {
					case 1:
						return o.NewError(args[0].String()), nil
					case 0:
						return o.NewError(o.Err.Message), nil
					default:
						msgs := make([]string, len(args))
						for i := range args {
							msgs[i] = args[0].String()
						}
						return o.NewError(msgs...), nil
					}
				},
			}, nil
		}
		return o.Err.IndexGet(index)
	}

	return Undefined, nil
}

// NewError creates a new Error and sets original Error as its cause which can be unwrapped.
func (o *RuntimeError) NewError(messages ...string) *RuntimeError {
	cp := o.Copy().(*RuntimeError)
	cp.Err.Message = strings.Join(messages, " ")
	cp.Err.Cause = o
	return cp
}

// IndexSet implements Object interface.
func (*RuntimeError) IndexSet(index, value Object) error {
	return ErrNotIndexAssignable
}

// BinaryOp implements Object interface.
func (o *RuntimeError) BinaryOp(tok token.Token, right Object) (Object, error) {
	return nil, ErrInvalidOperator
}

// CanCall implements Object interface.
func (*RuntimeError) CanCall() bool { return false }

// Call implements Object interface.
func (*RuntimeError) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

// CanIterate implements Object interface.
func (*RuntimeError) CanIterate() bool { return false }

// Iterate implements Object interface.
func (*RuntimeError) Iterate() Iterator { return nil }

// StackTrace returns stack trace if set otherwise returns nil.
func (o *RuntimeError) StackTrace() StackTrace {
	if o.fileSet == nil {
		if o.Trace != nil {
			sz := len(o.Trace)
			trace := make(StackTrace, sz)
			j := 0
			for i := sz - 1; i >= 0; i-- {
				trace[j] = parser.SourceFilePos{
					Offset: int(o.Trace[i]),
				}
				j++
			}
			return trace
		}
		return nil
	}

	sz := len(o.Trace)
	trace := make(StackTrace, sz)
	j := 0
	for i := sz - 1; i >= 0; i-- {
		trace[j] = o.fileSet.Position(o.Trace[i])
		j++
	}
	return trace
}

// Format implements fmt.Formater interface.
func (o *RuntimeError) Format(s fmt.State, verb rune) {
	switch verb {
	case 'v', 's':
		switch {
		case s.Flag('+'):
			_, _ = io.WriteString(s, o.String())
			if len(o.Trace) > 0 {
				if v := o.StackTrace(); v != nil {
					_, _ = io.WriteString(s, fmt.Sprintf("%+v", v))
				} else {
					_, _ = io.WriteString(s, "<nil stack trace>")
				}
			} else {
				_, _ = io.WriteString(s, "<no stack trace>")
			}
			e := o.Unwrap()
			for e != nil {
				if e, ok := e.(*RuntimeError); ok && o != e {
					_, _ = fmt.Fprintf(s, "\n\t%+v", e)
				}
				if err, ok := e.(interface{ Unwrap() error }); ok {
					e = err.Unwrap()
				} else {
					break
				}
			}
		default:
			_, _ = io.WriteString(s, o.String())
		}
	case 'q':
		_, _ = io.WriteString(s, strconv.Quote(o.String()))
	}
}

// StackTrace is the stack of source file positions.
type StackTrace []parser.SourceFilePos

// Format formats the StackTrace to the fmt.Formatter interface.
func (st StackTrace) Format(s fmt.State, verb rune) {
	switch verb {
	case 'v', 's':
		switch {
		case s.Flag('+'):
			for i, f := range st {
				if i > 0 {
					_, _ = io.WriteString(s, "\n\t   ")
				} else {
					_, _ = io.WriteString(s, "\n\tat ")
				}
				_, _ = fmt.Fprintf(s, "%+v", parser.SourceFilePos(f))
			}
		default:
			_, _ = fmt.Fprintf(s, "%v", []parser.SourceFilePos(st))
		}
	}
}
