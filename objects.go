package charlang

import (
	"bytes"
	"context"
	"database/sql"
	"encoding/json"
	"errors"
	"fmt"
	"image"
	"io"
	"math"
	"math/big"
	"net/http"
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"sync"
	"time"

	"github.com/topxeq/charlang/internal/compat"
	"github.com/topxeq/charlang/parser"
	"github.com/topxeq/charlang/token"
	"github.com/topxeq/sqltk"
	tk "github.com/topxeq/tkc"

	"github.com/dop251/goja"
	"github.com/xuri/excelize/v2"
	//	"modernc.org/quickjs"
	"github.com/gorilla/websocket"
)

const (
	// True represents a true value.
	True = Bool(true)

	// False represents a false value.
	False = Bool(false)
)

var (
	// Undefined represents undefined value.
	Undefined Object = &UndefinedType{}
)

// when create a new object(class)
// choose a unique type code
// a builtin function to create/new the object
// builtinMakeFunc
// getValue/setValue, getMember/setMember, IndexGet/IndexSet
// ConvertToObject
// ConvertFromObject
// BuiltinAny
// ToIntObject
// ToStringObject

// TypeCodes: -1: unknown, undefined: 0, ObjectImpl: 101, Bool: 103, String: 105, *MutableString: 106, Int: 107, Byte: 109, Uint: 111, Char: 113, Float: 115, Array: 131, Map: 133, *OrderedMap: 135, *MapArray: 136, Bytes: 137, Chars: 139, *ObjectPtr: 151, *ObjectRef: 152, *SyncMap: 153, *Error: 155, *RuntimeError: 157, *Stack: 161, *Queue: 163, *Function: 181, *BuiltinFunction: 183, *CompiledFunction: 185, *CharCode: 191, *Gel: 193, *BigInt: 201, *BigFloat: 203, StatusResult: 303, *StringBuilder: 307, *BytesBuffer: 308, *Database: 309, *Time: 311, *Location: 313, *Seq: 315, *Mutex: 317, *Mux: 319, *HttpReq: 321, *HttpResp: 323, *HttpHandler: 325, *WebSocket: 327, *Reader: 331, *Writer: 333, *File: 401, *Image: 501, *Delegate: 601, *JsVm: 711, *QjsVm(not completed): 712, *EvalMachine: 888, *Any: 999, *Etable: 1001, *Excel: 1003

// Object represents an object in the VM.
type Object interface {
	// TypeName should return the name of the type.
	TypeName() string

	// TypeCode should return the code of the type.
	TypeCode() int

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

	GetValue() Object
	HasMemeber() bool
	GetMember(string) Object
	SetMember(string, Object) error
	CallMethod(string, ...Object) (Object, error)
}

// Copier wraps the Copy method to create a deep copy of the object.
type Copier interface {
	Copy() Object
}

// IndexDeleter wraps the IndexDelete method to delete an index of an object.
type IndexDeleter interface {
	IndexDelete(Object) error
}

// LengthGetter wraps the Len method to get the number of elements of an object.
type LengthGetter interface {
	Len() int
}

type ValueSetter interface {
	SetValue(Object) error
}

// type MemberHolder interface {
// 	GetMember(string) Object
// 	SetMember(string, Object) error
// }

// ExCallerObject is an interface for objects that can be called with CallEx
// method. It is an extended version of the Call method that can be used to
// call an object with a Call struct. Objects implementing this interface is
// called with CallEx method instead of Call method.
// Note that CanCall() should return true for objects implementing this
// interface.
type ExCallerObject interface {
	Object
	CallEx(c Call) (Object, error)
}

// NameCallerObject is an interface for objects that can be called with CallName
// method to call a method of an object. Objects implementing this interface can
// reduce allocations by not creating a callable object for each method call.
type NameCallerObject interface {
	Object
	CallName(name string, c Call) (Object, error)
}

// Call is a struct to pass arguments to CallEx and CallName methods.
// It provides VM for various purposes.
//
// Call struct intentionally does not provide access to normal and variadic
// arguments directly. Using Len() and Get() methods is preferred. It is safe to
// create Call with a nil VM as long as VM is not required by the callee.
type Call struct {
	Vm    *VM
	This  Object
	Args  []Object
	Vargs []Object
}

// NewCall creates a new Call struct with the given arguments.
func NewCall(vm *VM, args []Object, vargs ...Object) Call {
	return Call{
		Vm:    vm,
		Args:  args,
		Vargs: vargs,
	}
}

// VM returns the VM of the call.
func (c *Call) VM() *VM {
	return c.Vm
}

// Get returns the nth argument. If n is greater than the number of arguments,
// it returns the nth variadic argument.
// If n is greater than the number of arguments and variadic arguments, it
// panics!
func (c *Call) Get(n int) Object {
	if n < len(c.Args) {
		return c.Args[n]
	}
	return c.Vargs[n-len(c.Args)]
}

// Len returns the number of arguments including variadic arguments.
func (c *Call) Len() int {
	return len(c.Args) + len(c.Vargs)
}

// CheckLen checks the number of arguments and variadic arguments. If the number
// of arguments is not equal to n, it returns an error.
func (c *Call) CheckLen(n int) error {
	if n != c.Len() {
		return ErrWrongNumArguments.NewError(
			fmt.Sprintf("want=%d got=%d", n, c.Len()),
		)
	}
	return nil
}

// shift returns the first argument and removes it from the arguments.
// It updates the arguments and variadic arguments accordingly.
// If it cannot shift, it returns nil and false.
func (c *Call) shift() (Object, bool) {
	if len(c.Args) == 0 {
		if len(c.Vargs) == 0 {
			return nil, false
		}
		v := c.Vargs[0]
		c.Vargs = c.Vargs[1:]
		return v, true
	}
	v := c.Args[0]
	c.Args = c.Args[1:]
	return v, true
}

func (c *Call) callArgs() []Object {
	if len(c.Args) == 0 {
		return c.Vargs
	}
	args := make([]Object, 0, c.Len())
	args = append(args, c.Args...)
	args = append(args, c.Vargs...)
	return args
}

func (c *Call) GetArgs() []Object {
	if len(c.Args) == 0 {
		return c.Vargs
	}
	args := make([]Object, 0, c.Len())
	args = append(args, c.Args...)
	args = append(args, c.Vargs...)
	return args
}

// ObjectImpl is the basic Object implementation and it does not nothing, and
// helps to implement Object interface by embedding and overriding methods in
// custom implementations. String and TypeName must be implemented otherwise
// calling these methods causes panic.
type ObjectImpl struct {
	// Members map[string]Object
	// Methods map[string]*Function
}

var _ Object = ObjectImpl{}

func (ObjectImpl) TypeCode() int {
	panic(ErrNotImplemented)
	// return 101
}

// TypeName implements Object interface.
func (ObjectImpl) TypeName() string {
	panic(ErrNotImplemented)
}

// String implements Object interface.
func (o ObjectImpl) String() string {
//	return fmt.Sprintf("(objectImpl)")
	 panic(ErrNotImplemented)
}

func (o ObjectImpl) HasMemeber() bool {
	return false
}

func (o ObjectImpl) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return o, nil
	case "toStr":
		return ToStringObject(o), nil
	}

	return CallObjectMethodFunc(o, nameA, argsA...)
}

func (o ObjectImpl) GetValue() Object {
	return o
}

func (o ObjectImpl) GetMember(idxA string) Object {
	return Undefined
}

func (o ObjectImpl) SetMember(idxA string, valueA Object) error {
	return fmt.Errorf("unsupported action(set member)")
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
func (o ObjectImpl) IndexGet(index Object) (value Object, err error) {
	return nil, ErrNotIndexable
}

// IndexSet implements Object interface.
func (o ObjectImpl) IndexSet(index, value Object) error {
	return ErrNotIndexAssignable
}

// BinaryOp implements Object interface.
func (ObjectImpl) BinaryOp(_ token.Token, _ Object) (Object, error) {
	return nil, ErrInvalidOperator
}

// UndefinedType represents the type of global Undefined Object. One should use
// the UndefinedType in type switches only.
type UndefinedType struct {
	ObjectImpl
}

func (o *UndefinedType) TypeCode() int {
	return 0
}

// TypeName implements Object interface.
func (o *UndefinedType) TypeName() string {
	return "undefined"
}

// String implements Object interface.
func (o *UndefinedType) String() string {
	return "undefined"
}

func (o *UndefinedType) HasMemeber() bool {
	return false
}

func (o *UndefinedType) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return o, nil
	case "toStr":
		return ToStringObject(o), nil
	}

	return CallObjectMethodFunc(o, nameA, argsA...)
}

func (o *UndefinedType) GetValue() Object {
	return o
}

func (o *UndefinedType) GetMember(idxA string) Object {
	return Undefined
}

func (o *UndefinedType) SetMember(idxA string, valueA Object) error {
	return fmt.Errorf("unsupported action(set member)")
}

// Call implements Object interface.
func (*UndefinedType) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

// Equal implements Object interface.
func (o *UndefinedType) Equal(right Object) bool {
	return right == Undefined
}

// BinaryOp implements Object interface.
func (o *UndefinedType) BinaryOp(tok token.Token, right Object) (Object, error) {
	switch right.(type) {
	case *UndefinedType:
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

// IndexGet implements Object interface.
func (*UndefinedType) IndexGet(key Object) (Object, error) {
	return Undefined, nil
}

// IndexSet implements Object interface.
func (*UndefinedType) IndexSet(key, value Object) error {
	return ErrNotIndexAssignable
}

// Bool represents boolean values and implements Object interface.
type Bool bool

func (Bool) TypeCode() int {
	return 103
}

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

func (o Bool) HasMemeber() bool {
	return false
}

func (o Bool) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return o, nil
	case "toStr":
		return ToStringObject(o), nil
	}

	return CallObjectMethodFunc(o, nameA, argsA...)
}

func (o Bool) GetValue() Object {
	return o
}

func (o Bool) GetMember(idxA string) Object {
	return Undefined
}

func (o Bool) SetMember(idxA string, valueA Object) error {
	return fmt.Errorf("unsupported action(set member)")
}

// Equal implements Object interface.
func (o Bool) Equal(right Object) bool {
	if v, ok := right.(Bool); ok {
		return o == v
	}

	if v, ok := right.(Int); ok {
		return bool((o && v == 1) || (!o && v == 0))
	}

	if v, ok := right.(Byte); ok {
		return bool((o && v == 1) || (!o && v == 0))
	}

	if v, ok := right.(Char); ok {
		return bool((o && v == 1) || (!o && v == 0))
	}

	if v, ok := right.(Uint); ok {
		return bool((o && v == 1) || (!o && v == 0))
	}

	if v, ok := right.(Float); ok {
		return bool((o && v == 1) || (!o && v == 0))
	}

	if v, ok := right.(*BigInt); ok {
		return bool((o && v.Value.Int64() == 1) || (!o && v.Value.Int64() == 0))
	}

	return false
}

// IsFalsy implements Object interface.
func (o Bool) IsFalsy() bool { return bool(!o) }

func (o Bool) BoolValue() bool { return bool(o) }

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
func (o Bool) IndexGet(index Object) (value Object, err error) {
	switch v := index.(type) {
	case String:
		strT := v.String()

		if strT == "value" {
			return o, nil
		}

		rs := o.GetMember(strT)

		if !IsUndefInternal(rs) {
			return rs, nil
		}

		// return nil, ErrIndexOutOfBounds
		return GetObjectMethodFunc(o, strT)
	}

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
	case Bool:
		if v {
			right = Int(1)
		} else {
			right = Int(0)
		}

		goto switchpos
	case *UndefinedType:
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

// Format implements fmt.Formatter interface.
func (o Bool) Format(s fmt.State, verb rune) {
	format := compat.FmtFormatString(s, verb)
	fmt.Fprintf(s, format, bool(o))
}

// Int represents signed integer values and implements Object interface.
type Int int64

func (Int) TypeCode() int {
	return 107
}

// TypeName implements Object interface.
func (Int) TypeName() string {
	return "int"
}

// String implements Object interface.
func (o Int) String() string {
	return strconv.FormatInt(int64(o), 10)
}

func (o Int) HasMemeber() bool {
	return false
}

func (o Int) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return o, nil
	case "toStr":
		return ToStringObject(o), nil
	}

	return CallObjectMethodFunc(o, nameA, argsA...)
}

func (o Int) GetValue() Object {
	return o
}

func (o Int) GetMember(idxA string) Object {
	return Undefined
}

func (o Int) SetMember(idxA string, valueA Object) error {
	return fmt.Errorf("unsupported action(set member)")
}

// Equal implements Object interface.
func (o Int) Equal(right Object) bool {
	switch v := right.(type) {
	case Int:
		return o == v
	case Byte:
		return o == Int(v)
	case Char:
		return o == Int(v)
	case Uint:
		return Uint(o) == v
	case Float:
		return Float(o) == v
	case *BigInt:
		return big.NewInt(int64(o)).Cmp(v.Value) == 0
	case Bool:
		if v {
			return o == 1
		}
		return o == 0
	}
	return false
}

// IsFalsy implements Object interface.
func (o Int) IsFalsy() bool { return o == 0 }

// CanCall implements Object interface.
func (o Int) CanCall() bool { return false }

// Call implements Object interface.
func (o Int) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

// CanIterate implements Object interface.
func (Int) CanIterate() bool { return true }

// Iterate implements Object interface.
func (o Int) Iterate() Iterator {
	return &IntIterator{V: o}
	// return nil
}

// IndexSet implements Object interface.
func (Int) IndexSet(index, value Object) error {
	return ErrNotIndexAssignable
}

// IndexGet implements Object interface.
func (o Int) IndexGet(index Object) (Object, error) {
	switch v := index.(type) {
	case String:
		strT := v.String()

		if strT == "value" {
			return o, nil
		}

		rs := o.GetMember(strT)

		if !IsUndefInternal(rs) {
			return rs, nil
		}

		// return nil, ErrIndexOutOfBounds
		return GetObjectMethodFunc(o, strT)
	}

	return nil, ErrNotIndexable
}

// BinaryOp implements Object interface.
func (o Int) BinaryOp(tok token.Token, right Object) (Object, error) {
	// tk.Plo("Int) BinaryOp", right)
	switch v := right.(type) {
	case Int:
		switch tok {
		case token.Add:
			return o + v, nil
		case token.Sub:
			return o - v, nil
		case token.Mul:
			return o * v, nil
		case token.Quo:
			if v == 0 {
				return nil, ErrZeroDivision
			}
			return o / v, nil
		case token.Rem:
			return o % v, nil
		case token.And:
			return o & v, nil
		case token.Or:
			return o | v, nil
		case token.Xor:
			return o ^ v, nil
		case token.AndNot:
			return o &^ v, nil
		case token.Shl:
			return o << v, nil
		case token.Shr:
			return o >> v, nil
		case token.Less:
			return Bool(o < v), nil
		case token.LessEq:
			return Bool(o <= v), nil
		case token.Greater:
			return Bool(o > v), nil
		case token.GreaterEq:
			return Bool(o >= v), nil
		}
	case Byte:
		return o.BinaryOp(tok, Int(v))
		// switch tok {
		// case token.Add:
		// 	return o + Int(v), nil
		// case token.Sub:
		// 	return o - Int(v), nil
		// case token.Less:
		// 	return Bool(o < Int(v)), nil
		// case token.LessEq:
		// 	return Bool(o <= Int(v)), nil
		// case token.Greater:
		// 	return Bool(o > Int(v)), nil
		// case token.GreaterEq:
		// 	return Bool(o >= Int(v)), nil
		// }
	case Char:
		return o.BinaryOp(tok, Int(v))
	case Uint:
		return Uint(o).BinaryOp(tok, v)
	case Float:
		return Float(o).BinaryOp(tok, v)
	case Bool:
		if v {
			right = Int(1)
		} else {
			right = Int(0)
		}

		return o.BinaryOp(tok, right)
	case *BigInt:
		rs := &BigInt{Value: big.NewInt(int64(o))}
		return rs.BinaryOp(tok, v)
	case *UndefinedType:
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
		right.TypeName(),
	)
}

// Format implements fmt.Formatter interface.
func (o Int) Format(s fmt.State, verb rune) {
	format := compat.FmtFormatString(s, verb)
	fmt.Fprintf(s, format, int64(o))
}

// Uint represents unsigned integer values and implements Object interface.
type Uint uint64

func (Uint) TypeCode() int {
	return 111
}

// TypeName implements Object interface.
func (Uint) TypeName() string {
	return "uint"
}

// String implements Object interface.
func (o Uint) String() string {
	return strconv.FormatUint(uint64(o), 10)
}

func (o Uint) HasMemeber() bool {
	return false
}

func (o Uint) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return o, nil
	case "toStr":
		return ToStringObject(o), nil
	}

	return CallObjectMethodFunc(o, nameA, argsA...)
}

func (o Uint) GetValue() Object {
	return o
}

func (o Uint) GetMember(idxA string) Object {
	return Undefined
}

func (o Uint) SetMember(idxA string, valueA Object) error {
	return fmt.Errorf("unsupported action(set member)")
}

// Equal implements Object interface.
func (o Uint) Equal(right Object) bool {
	switch v := right.(type) {
	case Uint:
		return o == v
	case Int:
		return o == Uint(v)
	case Byte:
		return o == Uint(v)
	case Char:
		return o == Uint(v)
	case Float:
		return Float(o) == v
	case *BigInt:
		return big.NewInt(int64(o)).Cmp(v.Value) == 0
	case Bool:
		if v {
			return o == 1
		}
		return o == 0
	}
	return false
}

// IsFalsy implements Object interface.
func (o Uint) IsFalsy() bool { return o == 0 }

// CanCall implements Object interface.
func (o Uint) CanCall() bool { return false }

// Call implements Object interface.
func (o Uint) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

// CanIterate implements Object interface.
func (Uint) CanIterate() bool { return true }

// Iterate implements Object interface.
func (o Uint) Iterate() Iterator { 
	return &UintIterator{V: o}
//	return nil 
}

// IndexSet implements Object interface.
func (Uint) IndexSet(index, value Object) error {
	return ErrNotIndexAssignable
}

// IndexGet implements Object interface.
func (o Uint) IndexGet(index Object) (Object, error) {
	switch v := index.(type) {
	case String:
		strT := v.String()

		if strT == "value" {
			return o, nil
		}

		rs := o.GetMember(strT)

		if !IsUndefInternal(rs) {
			return rs, nil
		}

		// return nil, ErrIndexOutOfBounds
		return GetObjectMethodFunc(o, strT)
	}

	return nil, ErrNotIndexable
}

// BinaryOp implements Object interface.
func (o Uint) BinaryOp(tok token.Token, right Object) (Object, error) {
	switch v := right.(type) {
	case Uint:
		switch tok {
		case token.Add:
			return o + v, nil
		case token.Sub:
			return o - v, nil
		case token.Mul:
			return o * v, nil
		case token.Quo:
			if v == 0 {
				return nil, ErrZeroDivision
			}
			return o / v, nil
		case token.Rem:
			return o % v, nil
		case token.And:
			return o & v, nil
		case token.Or:
			return o | v, nil
		case token.Xor:
			return o ^ v, nil
		case token.AndNot:
			return o &^ v, nil
		case token.Shl:
			return o << v, nil
		case token.Shr:
			return o >> v, nil
		case token.Less:
			return Bool(o < v), nil
		case token.LessEq:
			return Bool(o <= v), nil
		case token.Greater:
			return Bool(o > v), nil
		case token.GreaterEq:
			return Bool(o >= v), nil
		}
	case Int:
		return o.BinaryOp(tok, Uint(v))
	case Byte:
		return o.BinaryOp(tok, Uint(v))
	case Char:
		return o.BinaryOp(tok, Uint(v))
	case Float:
		return Float(o).BinaryOp(tok, v)
	case Bool:
		if v {
			right = Uint(1)
		} else {
			right = Uint(0)
		}

		return o.BinaryOp(tok, v)
	case *BigInt:
		rs := &BigInt{Value: big.NewInt(int64(o))}
		return rs.BinaryOp(tok, v)
	case *UndefinedType:
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
		right.TypeName(),
	)
}

// Format implements fmt.Formatter interface.
func (o Uint) Format(s fmt.State, verb rune) {
	format := compat.FmtFormatString(s, verb)
	fmt.Fprintf(s, format, uint64(o))
}

// Float represents float values and implements Object interface.
type Float float64

func (Float) TypeCode() int {
	return 115
}

// TypeName implements Object interface.
func (Float) TypeName() string {
	return "float"
}

// String implements Object interface.
func (o Float) String() string {
	return strconv.FormatFloat(float64(o), 'g', -1, 64)
}

func (o Float) HasMemeber() bool {
	return false
}

func (o Float) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return o, nil
	case "toStr":
		return ToStringObject(o), nil
	}

	return CallObjectMethodFunc(o, nameA, argsA...)
}

func (o Float) GetValue() Object {
	return o
}

func (o Float) GetMember(idxA string) Object {
	return Undefined
}

func (o Float) SetMember(idxA string, valueA Object) error {
	return fmt.Errorf("unsupported action(set member)")
}

// Equal implements Object interface.
func (o Float) Equal(right Object) bool {
	switch v := right.(type) {
	case Float:
		return o == v
	case Int:
		return o == Float(v)
	case Uint:
		return o == Float(v)
	case Char:
		return o == Float(v)
	case Byte:
		return o == Float(v)
	case *BigInt:
		newNumT, _ := big.NewFloat(0).SetInt(v.Value).Float64()

		return o == Float(newNumT)
	case Bool:
		if v {
			return o == 1
		}
		return o == 0
	}
	return false
}

// IsFalsy implements Object interface.
func (o Float) IsFalsy() bool {
	// IEEE 754 says that only NaNs satisfy f != f.
	// See math.IsNan
	f := float64(o)
	return f != f
}

// CanCall implements Object interface.
func (o Float) CanCall() bool { return false }

// Call implements Object interface.
func (o Float) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

// CanIterate implements Object interface.
func (Float) CanIterate() bool { return false }

// Iterate implements Object interface.
func (Float) Iterate() Iterator { return nil }

// IndexSet implements Object interface.
func (Float) IndexSet(index, value Object) error {
	return ErrNotIndexAssignable
}

// IndexGet implements Object interface.
func (o Float) IndexGet(index Object) (Object, error) {
	switch v := index.(type) {
	case String:
		strT := v.String()

		if strT == "value" {
			return o, nil
		}

		rs := o.GetMember(strT)

		if !IsUndefInternal(rs) {
			return rs, nil
		}

		// return nil, ErrIndexOutOfBounds
		return GetObjectMethodFunc(o, strT)
	}

	return nil, ErrNotIndexable
}

// BinaryOp implements Object interface.
func (o Float) BinaryOp(tok token.Token, right Object) (Object, error) {
	switch v := right.(type) {
	case Float:
		switch tok {
		case token.Add:
			return o + v, nil
		case token.Sub:
			return o - v, nil
		case token.Mul:
			return o * v, nil
		case token.Quo:
			if v == 0.0 {
				return nil, ErrZeroDivision
			}
			return o / v, nil
		case token.Less:
			return Bool(o < v), nil
		case token.LessEq:
			return Bool(o <= v), nil
		case token.Greater:
			return Bool(o > v), nil
		case token.GreaterEq:
			return Bool(o >= v), nil
		}
	case Int:
		return o.BinaryOp(tok, Float(v))
	case Byte:
		return o.BinaryOp(tok, Float(v))
	case Char:
		return o.BinaryOp(tok, Float(v))
	case Uint:
		return o.BinaryOp(tok, Float(v))
	case Bool:
		if v {
			right = Float(1)
		} else {
			right = Float(0)
		}

		return o.BinaryOp(tok, right)
	case *BigInt:
		rs := &BigInt{Value: big.NewInt(int64(o))}
		return rs.BinaryOp(tok, right)
	case *UndefinedType:
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
		right.TypeName(),
	)
}

// Format implements fmt.Formatter interface.
func (o Float) Format(s fmt.State, verb rune) {
	format := compat.FmtFormatString(s, verb)
	fmt.Fprintf(s, format, float64(o))
}

// Char represents a rune and implements Object interface.
type Char rune

func (Char) TypeCode() int {
	return 113
}

// TypeName implements Object interface.
func (Char) TypeName() string {
	return "char"
}

// String implements Object interface.
func (o Char) String() string {
	return string(o)
}

func (o Char) HasMemeber() bool {
	return false
}

func (o Char) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return o, nil
	case "toStr":
		return ToStringObject(o), nil
	}

	return CallObjectMethodFunc(o, nameA, argsA...)
}

func (o Char) GetValue() Object {
	return o
}

func (o Char) GetMember(idxA string) Object {
	return Undefined
}

func (o Char) SetMember(idxA string, valueA Object) error {
	return fmt.Errorf("unsupported action(set member)")
}

// Equal implements Object interface.
func (o Char) Equal(right Object) bool {
	switch v := right.(type) {
	case Char:
		return o == v
	case Int:
		return Int(o) == v
	case Uint:
		return Uint(o) == v
	case Byte:
		return o == Char(v)
	case Float:
		return Float(o) == v
	case *BigInt:
		return big.NewInt(int64(o)).Cmp(v.Value) == 0
	case Bool:
		if v {
			return o == 1
		}
		return o == 0
	}
	return false
}

// IsFalsy implements Object interface.
func (o Char) IsFalsy() bool { return o == 0 }

// CanCall implements Object interface.
func (o Char) CanCall() bool { return false }

// Call implements Object interface.
func (o Char) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

// CanIterate implements Object interface.
func (Char) CanIterate() bool { return false }

// Iterate implements Object interface.
func (Char) Iterate() Iterator { return nil }

// IndexSet implements Object interface.
func (Char) IndexSet(index, value Object) error {
	return ErrNotIndexAssignable
}

// IndexGet implements Object interface.
func (o Char) IndexGet(index Object) (Object, error) {
	switch v := index.(type) {
	case String:
		strT := v.String()

		if strT == "value" {
			return o, nil
		}

		rs := o.GetMember(strT)

		if !IsUndefInternal(rs) {
			return rs, nil
		}

		// return nil, ErrIndexOutOfBounds
		return GetObjectMethodFunc(o, strT)
	}

	return nil, ErrNotIndexable
}

// BinaryOp implements Object interface.
func (o Char) BinaryOp(tok token.Token, right Object) (Object, error) {
	// tk.Plo("Char) BinaryOp", right)
	switch v := right.(type) {
	case Char:
		switch tok {
		case token.Add:
			return o + v, nil
		case token.Sub:
			return o - v, nil
		case token.Mul:
			return o * v, nil
		case token.Quo:
			if v == 0 {
				return nil, ErrZeroDivision
			}
			return o / v, nil
		case token.Rem:
			return o % v, nil
		case token.And:
			return o & v, nil
		case token.Or:
			return o | v, nil
		case token.Xor:
			return o ^ v, nil
		case token.AndNot:
			return o &^ v, nil
		case token.Shl:
			return o << v, nil
		case token.Shr:
			return o >> v, nil
		case token.Less:
			return Bool(o < v), nil
		case token.LessEq:
			return Bool(o <= v), nil
		case token.Greater:
			return Bool(o > v), nil
		case token.GreaterEq:
			return Bool(o >= v), nil
		}
	case Int:
		return Int(o).BinaryOp(tok, v)
	case Byte:
		return o.BinaryOp(tok, Char(v))
	case Uint:
		return Uint(o).BinaryOp(tok, v)
	case Bool:
		if v {
			right = Char(1)
		} else {
			right = Char(0)
		}

		return o.BinaryOp(tok, right)
	case *BigInt:
		rs := &BigInt{Value: big.NewInt(int64(o))}
		return rs.BinaryOp(tok, v)
	case String:
		if tok == token.Add {
			var sb strings.Builder
			sb.Grow(len(v.String()) + 4)
			sb.WriteRune(rune(o))
			sb.WriteString(v.String())
			return ToStringObject(sb.String()), nil
		}
	case *UndefinedType:
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
		right.TypeName(),
	)
}

// Format implements fmt.Formatter interface.
func (o Char) Format(s fmt.State, verb rune) {
	format := compat.FmtFormatString(s, verb)
	fmt.Fprintf(s, format, rune(o))
}

type Byte byte

func (Byte) TypeCode() int {
	return 109
}

// TypeName implements Object interface.
func (Byte) TypeName() string {
	return "byte"
}

// String implements Object interface.
func (o Byte) String() string {
	return strconv.FormatInt(int64(o), 10)
}

func (o Byte) HasMemeber() bool {
	return false
}

func (o Byte) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return o, nil
	case "toStr":
		return ToStringObject(o), nil
	}

	return CallObjectMethodFunc(o, nameA, argsA...)
}

func (o Byte) GetValue() Object {
	return o
}

func (o Byte) GetMember(idxA string) Object {
	return Undefined
}

func (o Byte) SetMember(idxA string, valueA Object) error {
	return fmt.Errorf("unsupported action(set member)")
}

// Equal implements Object interface.
func (o Byte) Equal(right Object) bool {
	switch v := right.(type) {
	case Byte:
		return o == v
	case Int:
		return Int(o) == v
	case Uint:
		return Uint(o) == v
	case Float:
		return Float(o) == v
	case Char:
		return Char(o) == v
	case *BigInt:
		return big.NewInt(int64(o)).Cmp(v.Value) == 0
	case Bool:
		if v {
			return o == 1
		}
		return o == 0
	}
	return false
}

// IsFalsy implements Object interface.
func (o Byte) IsFalsy() bool { return o == 0 }

// CanCall implements Object interface.
func (o Byte) CanCall() bool { return false }

// Call implements Object interface.
func (o Byte) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

// CanIterate implements Object interface.
func (Byte) CanIterate() bool { return false }

// Iterate implements Object interface.
func (Byte) Iterate() Iterator { return nil }

// IndexSet implements Object interface.
func (Byte) IndexSet(index, value Object) error {
	return ErrNotIndexAssignable
}

// IndexGet implements Object interface.
func (o Byte) IndexGet(index Object) (Object, error) {
	switch v := index.(type) {
	case String:
		strT := v.String()

		if strT == "value" {
			return o, nil
		}

		rs := o.GetMember(strT)

		if !IsUndefInternal(rs) {
			return rs, nil
		}

		// return nil, ErrIndexOutOfBounds
		return GetObjectMethodFunc(o, strT)
	}

	return nil, ErrNotIndexable
}

// BinaryOp implements Object interface.
func (o Byte) BinaryOp(tok token.Token, right Object) (Object, error) {
	switch v := right.(type) {
	case Byte:
		switch tok {
		case token.Add:
			return o + v, nil
		case token.Sub:
			return o - v, nil
		case token.Mul:
			return o * v, nil
		case token.Quo:
			if v == 0 {
				return nil, ErrZeroDivision
			}
			return o / v, nil
		case token.Rem:
			return o % v, nil
		case token.And:
			return o & v, nil
		case token.Or:
			return o | v, nil
		case token.Xor:
			return o ^ v, nil
		case token.AndNot:
			return o &^ v, nil
		case token.Shl:
			return o << v, nil
		case token.Shr:
			return o >> v, nil
		case token.Less:
			return Bool(o < v), nil
		case token.LessEq:
			return Bool(o <= v), nil
		case token.Greater:
			return Bool(o > v), nil
		case token.GreaterEq:
			return Bool(o >= v), nil
		}
	case Int:
		return Int(o).BinaryOp(tok, right)
	case Char:
		return Char(o).BinaryOp(tok, right)
	case Uint:
		return Uint(o).BinaryOp(tok, right)
	case Float:
		return Float(o).BinaryOp(tok, right)
	case Bool:
		if v {
			right = Int(1)
		} else {
			right = Int(0)
		}
		return o.BinaryOp(tok, right)
	case *BigInt:
		rs := &BigInt{Value: big.NewInt(int64(o))}
		return rs.BinaryOp(tok, v)
	case *UndefinedType:
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
		right.TypeName(),
	)
}

// Format implements fmt.Formatter interface.
func (o Byte) Format(s fmt.State, verb rune) {
	format := compat.FmtFormatString(s, verb)
	fmt.Fprintf(s, format, byte(o))
}

// String represents string values and implements Object interface.
type String string
//type String struct {
//	ObjectImpl
//	Value string
//
////	Members map[string]Object `json:"-"`
//	// Methods map[string]*Function
//}

var _ LengthGetter = ToStringObject("")

func (String) TypeCode() int {
	return 105
}

// TypeName implements Object interface.
func (String) TypeName() string {
	return "string"
}

func (o String) String() string {
	return string(o) // tk.ToJSONX(o)
}

func (o String) HasMemeber() bool {
	return false
}

func (o String) CallName(nameA string, c Call) (Object, error) {
	switch nameA {
	case "value":
		return o, nil
	case "toStr":
		return ToStringObject(o), nil
	case "contains":
		args := c.GetArgs()

		if len(args) < 1 {
			return NewCommonErrorWithPos(c, "not enough parameters"), nil
		}

		return Bool(strings.Contains(o.String(), args[0].String())), nil
	case "testByFunc":
		args := c.GetArgs()

		lenT := len(args)

		if lenT < 1 {
			return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
		}

		v2 := args[0]

		var v3 string
		var v4 string

		if lenT > 2 {
			v3 = tk.ToStr(args[1])
			v4 = "(" + tk.ToStr(args[2]) + ")"
		} else if lenT > 1 {
			v3 = tk.ToStr(args[1])
		} else {
			v3 = tk.ToStr(tk.GetSeq())
		}

		v1 := o
		nv1 := o

		nv2, ok := v2.(*CompiledFunction)

		if !ok {
			return nil, NewCommonErrorWithPos(c, "test %v%v failed(invalid type v2): %#v <-> %#v\n-----\n%v\n-----\n%v", v3, v4, v1, v2, v1, v2)
		}

		retT, errT := NewInvoker(c.VM(), nv2).Invoke(nv1)

		if errT != nil {
			return nil, NewCommonErrorWithPos(c, "test %v%v failed(compareFunc returns error: %v): %#v <-> %#v\n-----\n%v\n-----\n%v", v3, v4, errT, v1, v2, v1, v2)
		}

		b1, ok := retT.(Bool)
		if !ok {
			return nil, NewCommonErrorWithPos(c, "test %v%v failed(compareFunc returns invalid type: %#v): %#v <-> %#v\n-----\n%v\n-----\n%v", v3, v4, retT, v1, v2, v1, v2)
		}

		if !bool(b1) {
			return nil, NewCommonErrorWithPos(c, "test %v%v failed(compareFunc returns false): %v -> %#v\n-----\n%v\n", v3, v4, retT, v1, v1)
		}

		tk.Pl("test %v%v passed", v3, v4)

		return nil, nil
	}

	args := c.GetArgs()

	rs1, errT := CallObjectMethodFunc(o, nameA, args...)

	//	if errT != nil || tk.IsError(rs1) {
	//		rs3 := tk.ReflectCallMethodCompact(o.Value, nameA, ObjectsToI(args)...)
	//		return ConvertToObject(rs3), nil
	//	}

	return rs1, errT

	//	return Undefined, NewCommonErrorWithPos(c, "method not found: %v", nameA)
}

func (o String) CallMethod(nameA string, argsA ...Object) (Object, error) {
	return o.CallName(nameA, Call{Args: argsA})
	//	switch nameA {
	//	case "value":
	//		return o, nil
	//	case "toStr":
	//		return ToStringObject(o), nil
	//	case "contains":
	//		if len(argsA) < 1 {
	//			return NewCommonError("not enough parameters"), nil
	//		}
	//
	//		return Bool(strings.Contains(o.Value, argsA[0].String())), nil
	//	}
	//
	//	return CallObjectMethodFunc(o, nameA, argsA...)

	// return Undefined, NewCommonError("unknown method: %v", nameA)
}

func (o String) GetValue() Object {
	return o
}

func (o String) GetMember(idxA string) Object {
	return Undefined
//	if o.Members == nil {
//		return Undefined
//	}
//
//	v1, ok := o.Members[idxA]
//
//	if !ok {
//		return Undefined
//	}
//
//	return v1
}

func (o String) SetMember(idxA string, valueA Object) error {
//	if o.Members == nil {
//		o.Members = map[string]Object{}
//	}
//
//	if IsUndefInternal(valueA) {
//		delete(o.Members, idxA)
//		return nil
//	}
//
//	o.Members[idxA] = valueA
//
////	tk.Pln(idxA, valueA, o.Members)
//	return nil
	 return fmt.Errorf("unsupported action(set member)")
}

func (o String) Copy() Object {
	if DebugModeG {
		tk.Pl("string copy: %#v", o)
	}
	
	return String(o.String()) // , Methods: o.Methods, Members: o.Members
}

// CanIterate implements Object interface.
func (String) CanIterate() bool { return true }

// Iterate implements Object interface.
func (o String) Iterate() Iterator {
	return &StringIterator{V: o}
}

// IndexSet implements Object interface.
func (o String) IndexSet(index, value Object) error {
	// idxT, ok := index.(String)

	// if ok {
	// 	strT := idxT.Value
	// 	if strT == "value" {
	// 		o.Value = value.String()
	// 		return nil
	// 	}

	// 	return o.SetMember(idxT.Value, value)
	// }

	return ErrNotIndexAssignable
}

// IndexGet represents string values and implements Object interface.
func (o String) IndexGet(index Object) (Object, error) {
	if IsUndefInternal(index) {
		return nil, NewIndexTypeError("int|uint", index.TypeName())
	}

	var idx int
	switch v := index.(type) {
//	case UndefinedType:
//		return nil, NewIndexTypeError("int|uint", index.TypeName())
	case Byte:
		idx = int(v)
	case Int:
		idx = int(v)
	case Uint:
		idx = int(v)
	case Char:
		idx = int(v)
	case String:
		strT := v.String()

		if strT == "value" {
			return ToStringObject(o.String()), nil
		}

		rs := o.GetMember(strT)

		if !IsUndefInternal(rs) {
			return rs, nil
		}

		return GetObjectMethodFunc(o, strT)
	default:
		return nil, NewIndexTypeError("int|uint|char|string", index.TypeName())
	}

	if idx >= 0 && idx < len(o.String()) {
		return Int(o.String()[idx]), nil
	}

	return nil, ErrIndexOutOfBounds
}

// Equal implements Object interface.
func (o String) Equal(right Object) bool {
	if v, ok := right.(String); ok {
		return o.String() == v.String()
	}

	if v, ok := right.(Bytes); ok {
		return o.String() == string(v)
	}
	return false
}

// IsFalsy implements Object interface.
func (o String) IsFalsy() bool { return len(o.String()) == 0 }

// CanCall implements Object interface.
func (o String) CanCall() bool { return false }

// Call implements Object interface.
func (o String) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

// BinaryOp implements Object interface.
func (o String) BinaryOp(tok token.Token, right Object) (Object, error) {
	switch v := right.(type) {
	case String:
		switch tok {
		case token.Add:
			return String(o.String() + v.String()), nil
		case token.Less:
			return Bool(o.String() < v.String()), nil
		case token.LessEq:
			return Bool(o.String() <= v.String()), nil
		case token.Greater:
			return Bool(o.String() > v.String()), nil
		case token.GreaterEq:
			return Bool(o.String() >= v.String()), nil
		}
	case Bytes:
		switch tok {
		case token.Add:
			var sb strings.Builder
			sb.WriteString(string(o))
			sb.Write(v)
			return ToStringObject(sb.String()), nil
		case token.Less:
			return Bool(o.String() < string(v)), nil
		case token.LessEq:
			return Bool(o.String() <= string(v)), nil
		case token.Greater:
			return Bool(o.String() > string(v)), nil
		case token.GreaterEq:
			return Bool(o.String() >= string(v)), nil
		}
	case *UndefinedType:
		switch tok {
		case token.Less, token.LessEq:
			return False, nil
		case token.Greater, token.GreaterEq:
			return True, nil
		}
	}

	if tok == token.Add {
		return String(o.String() + right.String()), nil
	}

	return nil, NewOperandTypeError(
		tok.String(),
		o.TypeName(),
		right.TypeName())
}

// Len implements LengthGetter interface.
func (o String) Len() int {
	return len(o.String())
}

// Format implements fmt.Formatter interface.
func (o String) Format(s fmt.State, verb rune) {
	format := compat.FmtFormatString(s, verb)
	fmt.Fprintf(s, format, o.String())
}

func (o String) MarshalJSON() ([]byte, error) {
	b1, err := json.Marshal(o.String())
	return b1, err
}

func ToStringObject(argA interface{}) String {
	switch nv := argA.(type) {
	case String:
		return String(nv.String())
	case Chars:
		return String(string(nv))
	case Bytes:
		return String(string(nv))
	case string:
		return String(nv)
	case *strings.Builder:
		return String(nv.String())
	case strings.Builder:
		return String(nv.String())
	case *bytes.Buffer:
		return String(string(nv.Bytes()))
	case bytes.Buffer:
		return String(string(nv.Bytes()))
	case nil:
		return String("")
	case []byte:
		return String(string(nv))
	case []rune:
		return String(string(nv))
	case *big.Int:
		return String(nv.String())
	case *big.Float:
		return String(nv.String())
	case *BigInt:
		return String(nv.Value.String())
	case *BigFloat:
		return String(nv.Value.String())
	case *tk.Seq:
		return String(nv.String())
	case *sync.RWMutex:
		return String(fmt.Sprintf("%v", nv))
	case *Image:
		return String(nv.String())
	case Object:
		return String(nv.String())
	}

	return String(fmt.Sprintf("%v", argA))
}

func FromStringObject(argA String) string {
	return argA.String()
}

func ToIntObject(argA interface{}, defaultA ...int) Int {
	defaultT := 0
	if len(defaultA) > 0 {
		defaultT = defaultA[0]
	}
	switch nv := argA.(type) {
	case Bool:
		if nv {
			return Int(1)
		}

		return Int(0)
	case Byte:
		return Int(nv)
	case Char:
		return Int(nv)
	case Int:
		return nv
	case Uint:
		return Int(nv)
	case Float:
		return Int(nv)
	case String:
		return Int(tk.StrToInt(nv.String(), defaultT))
	case Object:
		return Int(tk.StrToInt(nv.String(), defaultT))
	case bool:
		if nv {
			return Int(1)
		}

		return Int(0)
	case byte:
		return Int(nv)
	case int:
		return Int(nv)
	case rune:
		return Int(nv)
	case int64:
		return Int(nv)
	case uint:
		return Int(nv)
	case uint64:
		return Int(nv)
	case float64:
		return Int(nv)
	case float32:
		return Int(nv)
	case *big.Int:
		return Int(nv.Int64())
	case *big.Float:
		n1, _ := nv.Int64()
		return Int(n1)
	case string:
		return Int(tk.StrToInt(nv, defaultT))
	case nil:
		return Int(defaultT)
	case []byte:
		return Int(tk.StrToInt(string(nv), defaultT))
	case time.Duration:
		return Int(nv)
	case *tk.Seq:
		return Int(nv.GetCurrent())
	}

	return Int(defaultT)
}

func ToByteObject(argA interface{}, defaultA ...byte) Byte {
	var defaultT byte = 0
	
	if len(defaultA) > 0 {
		defaultT = defaultA[0]
	}
	
	switch nv := argA.(type) {
	case Bool:
		if nv {
			return Byte(1)
		}

		return Byte(0)
	case Byte:
		return nv
	case Char:
		return Byte(nv)
	case Int:
		return Byte(nv)
	case Uint:
		return Byte(nv)
	case Float:
		return Byte(nv)
	case String:
		return Byte(tk.StrToInt(nv.String(), int(defaultT)))
	case Object:
		return Byte(tk.StrToInt(nv.String(), int(defaultT)))
	case bool:
		if nv {
			return Byte(1)
		}

		return Byte(0)
	case byte:
		return Byte(nv)
	case int:
		return Byte(nv)
	case rune:
		return Byte(nv)
	case int64:
		return Byte(nv)
	case uint:
		return Byte(nv)
	case uint64:
		return Byte(nv)
	case float64:
		return Byte(nv)
	case float32:
		return Byte(nv)
	case *big.Int:
		return Byte(nv.Int64())
	case *big.Float:
		n1, _ := nv.Int64()
		return Byte(n1)
	case string:
		return Byte(tk.StrToInt(nv, int(defaultT)))
	case nil:
		return Byte(defaultT)
	case []byte:
		return Byte(tk.StrToInt(string(nv), int(defaultT)))
	case time.Duration:
		return Byte(nv)
	case *tk.Seq:
		return Byte(nv.GetCurrent())
	}

	return Byte(defaultT)
}

// Bytes represents byte slice and implements Object interface.
type Bytes []byte

var (
	_ Object       = Bytes{}
	_ Copier       = Bytes{}
	_ LengthGetter = Bytes{}
)

func (Bytes) TypeCode() int {
	return 137
}

// TypeName implements Object interface.
func (Bytes) TypeName() string {
	return "bytes"
}

func (o Bytes) String() string {
	return string(o)
}

func (o Bytes) HasMemeber() bool {
	return false
}

func (o Bytes) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return o, nil
	case "toStr":
		return ToStringObject(o), nil
	}

	return CallObjectMethodFunc(o, nameA, argsA...)
	// return Undefined, NewCommonError("unknown method: %v", nameA)
}

func (o Bytes) GetValue() Object {
	return o
}

func (o Bytes) GetMember(idxA string) Object {
	return Undefined
}

func (o Bytes) SetMember(idxA string, valueA Object) error {
	return fmt.Errorf("unsupported action(set member)")
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
	default:
		return NewIndexTypeError("int|uint", index.TypeName())
	}

	if idx >= 0 && idx < len(o) {
		switch v := value.(type) {
		case Int:
			o[idx] = byte(v)
		case Uint:
			o[idx] = byte(v)
		default:
			return NewIndexValueTypeError("int|uint", value.TypeName())
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
	case String:
		strT := v.String()

		if strT == "value" {
			return o, nil
		}

		rs := o.GetMember(strT)

		if !IsUndefInternal(rs) {
			return rs, nil
		}

		return GetObjectMethodFunc(o, strT)
	default:
		return nil, NewIndexTypeError("int|uint|char", index.TypeName())
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
		return string(o) == v.String()
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
			return Bool(bytes.Compare(o, v) == -1), nil
		case token.LessEq:
			cmp := bytes.Compare(o, v)
			return Bool(cmp == 0 || cmp == -1), nil
		case token.Greater:
			return Bool(bytes.Compare(o, v) == 1), nil
		case token.GreaterEq:
			cmp := bytes.Compare(o, v)
			return Bool(cmp == 0 || cmp == 1), nil
		}
	case String:
		switch tok {
		case token.Add:
			return append(o, v.String()...), nil
		case token.Less:
			return Bool(string(o) < v.String()), nil
		case token.LessEq:
			return Bool(string(o) <= v.String()), nil
		case token.Greater:
			return Bool(string(o) > v.String()), nil
		case token.GreaterEq:
			return Bool(string(o) >= v.String()), nil
		}
	case *UndefinedType:
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

// Len implements LengthGetter interface.
func (o Bytes) Len() int {
	return len(o)
}

// Format implements fmt.Formatter interface.
func (o Bytes) Format(s fmt.State, verb rune) {
	format := compat.FmtFormatString(s, verb)
	fmt.Fprintf(s, format, []byte(o))
}

// Chars represents byte slice and implements Object interface.
type Chars []rune

var (
	_ Object       = Chars{}
	_ Copier       = Chars{}
	_ LengthGetter = Chars{}
)

func (Chars) TypeCode() int {
	return 139
}

// TypeName implements Object interface.
func (Chars) TypeName() string {
	return "chars"
}

func (o Chars) String() string {
	return string(o)
}

func (o Chars) HasMemeber() bool {
	return false
}

func (o Chars) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return o, nil
	case "toStr":
		return ToStringObject(o), nil
	}

	return CallObjectMethodFunc(o, nameA, argsA...)
	// return Undefined, NewCommonError("unknown method: %v", nameA)
}

func (o Chars) GetValue() Object {
	return o
}

func (o Chars) GetMember(idxA string) Object {
	return Undefined
}

func (o Chars) SetMember(idxA string, valueA Object) error {
	return fmt.Errorf("unsupported action(set member)")
}

// Copy implements Copier interface.
func (o Chars) Copy() Object {
	cp := make(Chars, len(o))
	copy(cp, o)
	return cp
}

// CanIterate implements Object interface.
func (Chars) CanIterate() bool { return true }

// Iterate implements Object interface.
func (o Chars) Iterate() Iterator {
	return &CharsIterator{V: o}
}

// IndexSet implements Object interface.
func (o Chars) IndexSet(index, value Object) error {
	var idx int
	switch v := index.(type) {
	case Int:
		idx = int(v)
	case Uint:
		idx = int(v)
	default:
		return NewIndexTypeError("int|uint", index.TypeName())
	}

	if idx >= 0 && idx < len(o) {
		switch v := value.(type) {
		case Byte:
			o[idx] = rune(v)
		case Int:
			o[idx] = rune(v)
		case Char:
			o[idx] = rune(v)
		case Uint:
			o[idx] = rune(v)
		default:
			return NewIndexValueTypeError("int|uint", value.TypeName())
		}
		return nil
	}
	return ErrIndexOutOfBounds
}

// IndexGet represents string values and implements Object interface.
func (o Chars) IndexGet(index Object) (Object, error) {
	var idx int
	switch v := index.(type) {
	case Int:
		idx = int(v)
	case Uint:
		idx = int(v)
	case String:
		strT := v.String()

		if strT == "value" {
			return o, nil
		}

		rs := o.GetMember(strT)

		if !IsUndefInternal(rs) {
			return rs, nil
		}

		return GetObjectMethodFunc(o, strT)
	default:
		return nil, NewIndexTypeError("int|uint|char", index.TypeName())
	}

	if idx >= 0 && idx < len(o) {
		return Char(o[idx]), nil
	}
	return nil, ErrIndexOutOfBounds
}

// Equal implements Object interface.
func (o Chars) Equal(right Object) bool {
	if v, ok := right.(Chars); ok {
		return string(o) == string(v)
	}

	if v, ok := right.(String); ok {
		return string(o) == v.String()
	}
	return false
}

// IsFalsy implements Object interface.
func (o Chars) IsFalsy() bool { return len(o) == 0 }

// CanCall implements Object interface.
func (o Chars) CanCall() bool { return false }

// Call implements Object interface.
func (o Chars) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

// BinaryOp implements Object interface.
func (o Chars) BinaryOp(tok token.Token, right Object) (Object, error) {
	switch v := right.(type) {
	case Chars:
		switch tok {
		case token.Add:
			return append(o, v...), nil
		case token.Less:
			for i1, v1 := range o {
				if v1 < v[i1] {
					return Bool(true), nil
				}
			}

			return Bool(false), nil // Bool(bytes.Compare(o, v) == -1), nil
		case token.LessEq:
			for i1, v1 := range o {
				if v1 > v[i1] {
					return Bool(false), nil
				}
			}

			return Bool(true), nil
			// cmp := bytes.Compare(o, v)
			// return Bool(cmp == 0 || cmp == -1), nil
		case token.Greater:
			for i1, v1 := range o {
				if v1 > v[i1] {
					return Bool(true), nil
				}
			}

			return Bool(false), nil
			// return Bool(bytes.Compare(o, v) == 1), nil
		case token.GreaterEq:
			for i1, v1 := range o {
				if v1 < v[i1] {
					return Bool(false), nil
				}
			}

			return Bool(true), nil
			// cmp := bytes.Compare(o, v)
			// return Bool(cmp == 0 || cmp == 1), nil
		}
	// case Bytes:
	// 	switch tok {
	// 	case token.Add:
	// 		return append(o, v...), nil
	// 	case token.Less:
	// 		return Bool(bytes.Compare(o, v) == -1), nil
	// 	case token.LessEq:
	// 		cmp := bytes.Compare(o, v)
	// 		return Bool(cmp == 0 || cmp == -1), nil
	// 	case token.Greater:
	// 		return Bool(bytes.Compare(o, v) == 1), nil
	// 	case token.GreaterEq:
	// 		cmp := bytes.Compare(o, v)
	// 		return Bool(cmp == 0 || cmp == 1), nil
	// 	}
	case String:
		switch tok {
		case token.Add:
			return append(o, ([]rune(FromStringObject(v)))...), nil
			// return append(o, v.Value...), nil
		case token.Less:
			return Bool(string(o) < FromStringObject(v)), nil
			// return Bool(string(o) < v.Value), nil
		case token.LessEq:
			return Bool(string(o) <= FromStringObject(v)), nil
			// return Bool(string(o) <= v.Value), nil
		case token.Greater:
			return Bool(string(o) > FromStringObject(v)), nil
			// return Bool(string(o) > v.Value), nil
		case token.GreaterEq:
			return Bool(string(o) >= FromStringObject(v)), nil
			// return Bool(string(o) >= v.Value), nil
		}
	case *UndefinedType:
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

// Len implements LengthGetter interface.
func (o Chars) Len() int {
	return len(o)
}

// Format implements fmt.Formatter interface.
func (o Chars) Format(s fmt.State, verb rune) {
	format := compat.FmtFormatString(s, verb)
	fmt.Fprintf(s, format, []rune(o))
}

// CompiledFunction holds the constants and instructions to pass VM.
type CompiledFunction struct {
	// number of parameters
	NumParams int
	// number of local variabls including parameters NumLocals>=NumParams
	NumLocals    int
	Instructions []byte
	Variadic     bool
	Free         []*ObjectPtr
	// SourceMap holds the index of instruction and token's position.
	SourceMap map[int]int

	Members map[string]Object `json:"-"`
}

var _ Object = (*CompiledFunction)(nil)

func (*CompiledFunction) TypeCode() int {
	return 185
}

// TypeName implements Object interface
func (*CompiledFunction) TypeName() string {
	return "compiledFunction"
}

func (o *CompiledFunction) String() string {
	return "<compiledFunction>"
}

func (o *CompiledFunction) HasMemeber() bool {
	return true
}

func (o *CompiledFunction) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return o, nil
	case "toStr":
		return ToStringObject(o), nil
	}

	return Undefined, NewCommonError("unknown method: %v", nameA)
}

func (o *CompiledFunction) GetValue() Object {
	return o
}

func (o *CompiledFunction) GetMember(idxA string) Object {
	if o.Members == nil {
		return Undefined
	}

	v1, ok := o.Members[idxA]

	if !ok {
		return Undefined
	}

	return v1
}

func (o *CompiledFunction) SetMember(idxA string, valueA Object) error {
	if o.Members == nil {
		o.Members = map[string]Object{}
	}

	o.Members[idxA] = valueA

	// return fmt.Errorf("unsupported action")
	return nil
}

// Copy implements the Copier interface.
func (o *CompiledFunction) Copy() Object {
	var insts []byte
	if o.Instructions != nil {
		insts = make([]byte, len(o.Instructions))
		copy(insts, o.Instructions)
	}

	var free []*ObjectPtr
	if o.Free != nil {
		// DO NOT Copy() elements; these are variable pointers
		free = make([]*ObjectPtr, len(o.Free))
		copy(free, o.Free)
	}

	var sourceMap map[int]int
	if o.SourceMap != nil {
		sourceMap = make(map[int]int, len(o.SourceMap))
		for k, v := range o.SourceMap {
			sourceMap[k] = v
		}
	}

	return &CompiledFunction{
		NumParams:    o.NumParams,
		NumLocals:    o.NumLocals,
		Instructions: insts,
		Variadic:     o.Variadic,
		Free:         free,
		SourceMap:    sourceMap,
	}
}

// CanIterate implements Object interface.
func (*CompiledFunction) CanIterate() bool { return false }

// Iterate implements Object interface.
func (*CompiledFunction) Iterate() Iterator { return nil }

// IndexGet represents string values and implements Object interface.
func (o *CompiledFunction) IndexGet(index Object) (Object, error) {
	switch v := index.(type) {
	case String:
		strT := v.String()

		rs := o.GetMember(strT)

		if !IsUndefInternal(rs) {
			return rs, nil
		}

		return GetObjectMethodFunc(o, strT)
	}

	return nil, ErrNotIndexable
}

// IndexSet implements Object interface.
func (*CompiledFunction) IndexSet(index, value Object) error {
	return ErrNotIndexAssignable
}

// CanCall implements Object interface.
func (*CompiledFunction) CanCall() bool { return true }

// Call implements Object interface. CompiledFunction is not directly callable.
// You should use Invoker to call it with a Virtual Machine. Because of this, it
// always returns an error.
func (*CompiledFunction) Call(...Object) (Object, error) {
	return Undefined, ErrNotCallable
}

// BinaryOp implements Object interface.
func (*CompiledFunction) BinaryOp(token.Token, Object) (Object, error) {
	return nil, ErrInvalidOperator
}

// IsFalsy implements Object interface.
func (o *CompiledFunction) IsFalsy() bool { return o.Instructions == nil }

// Equal implements Object interface.
func (o *CompiledFunction) Equal(right Object) bool {
	v, ok := right.(*CompiledFunction)
	return ok && o == v
}

// SourcePos returns the source position of the instruction at ip.
func (o *CompiledFunction) SourcePos(ip int) parser.Pos {
begin:
	if ip >= 0 {
		if p, ok := o.SourceMap[ip]; ok {
			return parser.Pos(p)
		}
		ip--
		goto begin
	}
	return parser.NoPos
}

// Fprint writes constants and instructions to given Writer in a human readable form.
func (o *CompiledFunction) Fprint(w io.Writer) {
	_, _ = fmt.Fprintf(w, "Params:%d Variadic:%t Locals:%d\n", o.NumParams, o.Variadic, o.NumLocals)
	_, _ = fmt.Fprintf(w, "Instructions:\n")

	i := 0
	var operands []int

	for i < len(o.Instructions) {

		op := o.Instructions[i]
		numOperands := OpcodeOperands[op]
		operands, offset := ReadOperands(numOperands, o.Instructions[i+1:], operands)
		_, _ = fmt.Fprintf(w, "%04d %-12s", i, OpcodeNames[op])

		if len(operands) > 0 {
			for _, r := range operands {
				_, _ = fmt.Fprint(w, "    ", strconv.Itoa(r))
			}
		}

		_, _ = fmt.Fprintln(w)
		i += offset + 1
	}

	if o.Free != nil {
		_, _ = fmt.Fprintf(w, "Free:%v\n", o.Free)
	}
	_, _ = fmt.Fprintf(w, "SourceMap:%v\n", o.SourceMap)
}

func (o *CompiledFunction) identical(other *CompiledFunction) bool {
	if o.NumParams != other.NumParams ||
		o.NumLocals != other.NumLocals ||
		o.Variadic != other.Variadic ||
		len(o.Instructions) != len(other.Instructions) ||
		len(o.Free) != len(other.Free) ||
		string(o.Instructions) != string(other.Instructions) {
		return false
	}
	for i := range o.Free {
		if o.Free[i].Equal(other.Free[i]) {
			return false
		}
	}
	return true
}

func (o *CompiledFunction) equalSourceMap(other *CompiledFunction) bool {
	if len(o.SourceMap) != len(other.SourceMap) {
		return false
	}
	for k, v := range o.SourceMap {
		vv, ok := other.SourceMap[k]
		if !ok || vv != v {
			return false
		}
	}
	return true
}

func (o *CompiledFunction) hash32() uint32 {
	hash := hashData32(2166136261, []byte{byte(o.NumParams)})
	hash = hashData32(hash, []byte{byte(o.NumLocals)})
	if o.Variadic {
		hash = hashData32(hash, []byte{1})
	} else {
		hash = hashData32(hash, []byte{0})
	}
	hash = hashData32(hash, o.Instructions)
	return hash
}

func hashData32(hash uint32, data []byte) uint32 {
	for _, c := range data {
		hash *= 16777619 // prime32
		hash ^= uint32(c)
	}
	return hash
}

// Function represents a function object and implements Object interface.
type Function struct {
	ObjectImpl
	Name    string
	Value   func(args ...Object) (Object, error)
	ValueEx func(Call) (Object, error)

	Members map[string]Object `json:"-"`
}

var _ Object = (*Function)(nil)

func (*Function) TypeCode() int {
	return 181
}

// TypeName implements Object interface.
func (*Function) TypeName() string {
	return "function"
}

// String implements Object interface.
func (o *Function) String() string {
	return fmt.Sprintf("<function:%s>", o.Name)
}

func (o *Function) HasMemeber() bool {
	return true
}

func (o *Function) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return o, nil
	case "toStr":
		return ToStringObject(o), nil
	}

	return CallObjectMethodFunc(o, nameA, argsA...)
	// return Undefined, NewCommonError("unknown method: %v", nameA)
}

func (o *Function) GetValue() Object {
	return o
}

func (o *Function) GetMember(idxA string) Object {
	if o.Members == nil {
		return Undefined
	}

	v1, ok := o.Members[idxA]

	if !ok {
		return Undefined
	}

	return v1
}

func (o *Function) SetMember(idxA string, valueA Object) error {
	if o.Members == nil {
		o.Members = map[string]Object{}
	}

	if IsUndefInternal(valueA) {
		delete(o.Members, idxA)
		return nil
	}

	o.Members[idxA] = valueA

	// return fmt.Errorf("unsupported action(set member)")
	return nil
}

// Copy implements Copier interface.
func (o *Function) Copy() Object {
	return &Function{
		Name:    o.Name,
		Value:   o.Value,
		ValueEx: o.ValueEx,
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
func (o *Function) IsFalsy() bool { return o.Value == nil && o.ValueEx == nil }

// CanCall implements Object interface.
func (*Function) CanCall() bool { return true }

// Call implements Object interface.
func (o *Function) Call(args ...Object) (Object, error) {
	return o.Value(args...)
}

func (o *Function) CallEx(call Call) (Object, error) {
	if o.ValueEx != nil {
		return o.ValueEx(call)
	}
	return o.Value(call.callArgs()...)
}

// BuiltinFunction represents a builtin function object and implements Object interface.
type BuiltinFunction struct {
	ObjectImpl
	Name    string
	Value   func(args ...Object) (Object, error)
	ValueEx func(Call) (Object, error)

	Members map[string]Object `json:"-"`
	Methods map[string]*Function

	Remark string
}

var _ ExCallerObject = (*BuiltinFunction)(nil)

func (*BuiltinFunction) TypeCode() int {
	return 183
}

// TypeName implements Object interface.
func (*BuiltinFunction) TypeName() string {
	return "builtinFunction"
}

// String implements Object interface.
func (o *BuiltinFunction) String() string {
	return fmt.Sprintf("<builtinFunction:%s>", o.Name)
}

func (o *BuiltinFunction) HasMemeber() bool {
	return true
}

func (o *BuiltinFunction) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return o, nil
	case "toStr":
		return ToStringObject(o), nil
	}

	return CallObjectMethodFunc(o, nameA, argsA...)
	// return Undefined, NewCommonError("unknown method: %v", nameA)
}

func (o *BuiltinFunction) GetValue() Object {
	return o
}

func (o *BuiltinFunction) GetMember(idxA string) Object {
	if o.Members == nil {
		return Undefined
	}

	v1, ok := o.Members[idxA]

	if !ok {
		return Undefined
	}

	return v1
}

func (o *BuiltinFunction) SetMember(idxA string, valueA Object) error {
	if o.Members == nil {
		o.Members = map[string]Object{}
	}

	if IsUndefInternal(valueA) {
		delete(o.Members, idxA)
		return nil
	}

	o.Members[idxA] = valueA

	// return fmt.Errorf("unsupported action(set member)")
	return nil
}

// Copy implements Copier interface.
func (o *BuiltinFunction) Copy() Object {
	return &BuiltinFunction{
		Name:    o.Name,
		Value:   o.Value,
		ValueEx: o.ValueEx,
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
func (o *BuiltinFunction) IsFalsy() bool { return o.Value == nil && o.ValueEx == nil }

// CanCall implements Object interface.
func (*BuiltinFunction) CanCall() bool { return true }

// Call implements Object interface.
func (o *BuiltinFunction) Call(args ...Object) (Object, error) {
	// tk.Pl("*BuiltinFunction Call: %v", args)
	return o.Value(args...)
}

func (o *BuiltinFunction) CallEx(c Call) (Object, error) {
	// tk.Pl("*BuiltinFunction CallEx: %v", c)
	if o.ValueEx != nil {
		return o.ValueEx(c)
	}
	return o.Value(c.callArgs()...)
}

func (o *BuiltinFunction) IndexGet(index Object) (Object, error) {
	// tk.Pl("*BuiltinFunction IndexGet: %v", index)

	nv, ok := index.(String)
	if !ok {
		return Undefined, NewIndexTypeError("string", index.TypeName())
	}

	fNameT := o.Name + "." + string(nv)

	if o.Methods == nil {
		o.Methods = map[string]*Function{}
	}

	if o.Members == nil {
		o.Members = map[string]Object{}
	}

	switch fNameT {
	// case "any.new":
	// 	fT, ok := o.Methods["any.new"]
	// 	if !ok {
	// 		o.Methods["any.new"] = &Function{
	// 			Name: "any.new",
	// 			ValueEx: func(c Call) (Object, error) {
	// 				return builtinAnyFunc(c)
	// 			}}
	// 		fT = o.Methods["any.new"]
	// 	}

	// 	return fT, nil
	case "database.connect":
		fT, ok := o.Methods["database.connect"]
		if !ok {
			o.Methods["database.connect"] = &Function{
				Name: "database.connect",
				Value: func(args ...Object) (Object, error) {

					nv0, ok := args[0].(String)

					if !ok {
						return NewCommonError("invalid parameter 1"), nil
					}

					nv1, ok := args[1].(String)

					if !ok {
						return NewCommonError("invalid parameter 2"), nil
					}

					rsT := sqltk.ConnectDBX(string(nv0), string(nv1))
					if tk.IsError(rsT) {
						return NewFromError(rsT.(error)), nil
					}

					return &Database{DBType: string(nv0), DBConnectString: nv1.String(), Value: rsT.(*sql.DB)}, nil
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
						return NewCommonError("not enough parameters"), nil
					}

					nv0 := args[0].String()

					return ToStringObject(tk.FormatSQLValue(nv0)), nil
				}}
			fT = o.Methods["database.formatSQLValue"]
		}

		return fT, nil
	case "database.oneColumnToArray", "database.oneColToAry":
		fT, ok := o.Methods["database.oneColumnToArray"]
		if !ok {
			o.Methods["database.oneColumnToArray"] = &Function{
				Name: "database.oneColumnToArray",
				Value: func(args ...Object) (Object, error) {
					if len(args) < 1 {
						return NewCommonError("not enough parameters"), nil
					}

					nv0, ok := args[0].(Array)
					if !ok {
						return NewCommonError("invalid parameter 1"), nil
					}

					aryT := Array{}
					for i, v := range nv0 {
						if i == 0 {
							continue
						}

						aryT = append(aryT, v.(Array)[0])
					}

					return aryT, nil
				}}
			fT = o.Methods["database.oneColumnToArray"]
		}

		return fT, nil
	// case "statusResult.success":
	// 	fT, ok := o.Methods["statusResult.success"]
	// 	if !ok {
	// 		o.Methods["statusResult.success"] = &Function{
	// 			Name: "statusResult.success",
	// 			Value: func(args ...Object) (Object, error) {
	// 				if len(args) < 1 {
	// 					return &StatusResultSuccess, nil
	// 				}

	// 				return &StatusResult{Status: "success", Value: args[0].String()}, nil
	// 			}}
	// 		fT = o.Methods["statusResult.success"]
	// 	}

	// 	return fT, nil
	// case "statusResult.fail":
	// 	fT, ok := o.Methods["statusResult.fail"]
	// 	if !ok {
	// 		o.Methods["statusResult.fail"] = &Function{
	// 			Name: "statusResult.fail",
	// 			Value: func(args ...Object) (Object, error) {
	// 				if len(args) < 1 {
	// 					return &StatusResultFail, nil
	// 				}

	// 				return &StatusResult{Status: "fail", Value: args[0].String()}, nil
	// 			}}
	// 		fT = o.Methods["statusResult.fail"]
	// 	}

	// 	return fT, nil
	case "time.format":
		fT, ok := o.Methods["time.format"]
		if !ok {
			o.Methods["time.format"] = &Function{
				Name: "time.format",
				Value: func(args ...Object) (Object, error) {
					if len(args) < 1 {
						return ToStringObject(tk.FormatTime(time.Now(), ObjectsToS(args)...)), nil
					}

					return ToStringObject(tk.FormatTime(args[0].(*Time).Value, ObjectsToS(args[1:])...)), nil
				}}
			fT = o.Methods["time.format"]
		}

		return fT, nil
	case "time.now":
		fT, ok := o.Methods["time.now"]
		if !ok {
			o.Methods["time.now"] = &Function{
				Name: "time.now",
				Value: func(args ...Object) (Object, error) {
					return &Time{Value: time.Now()}, nil
				}}
			fT = o.Methods["time.now"]
		}

		return fT, nil
	case "time.timeFormatRFC1123":
		mT, ok := o.Members["time.timeFormatRFC1123"]
		if !ok {
			o.Members["time.timeFormatRFC1123"] = ToStringObject(time.RFC1123)
			mT = o.Members["time.timeFormatRFC1123"]
		}

		return mT, nil
	case "time.second":
		mT, ok := o.Members["time.second"]
		if !ok {
			o.Members["time.second"] = Int(time.Second)
			mT = o.Members["time.second"]
		}

		return mT, nil
	case "time.timeFormatCompact":
		mT, ok := o.Members["time.timeFormatCompact"]
		if !ok {
			o.Members["time.timeFormatCompact"] = ToStringObject(tk.TimeFormatCompact)
			mT = o.Members["time.timeFormatCompact"]
		}

		return mT, nil
	case "time.timeFormat":
		mT, ok := o.Members["time.timeFormat"]
		if !ok {
			o.Members["time.timeFormat"] = ToStringObject(tk.TimeFormat)
			mT = o.Members["time.timeFormat"]
		}

		return mT, nil
	case "time.timeFormatMS":
		mT, ok := o.Members["time.timeFormatMS"]
		if !ok {
			o.Members["time.timeFormatMS"] = ToStringObject(tk.TimeFormatMS)
			mT = o.Members["time.timeFormatMS"]
		}

		return mT, nil
	case "time.timeFormatMSCompact":
		mT, ok := o.Members["time.timeFormatMSCompact"]
		if !ok {
			o.Members["time.timeFormatMSCompact"] = ToStringObject(tk.TimeFormatMSCompact)
			mT = o.Members["time.timeFormatMSCompact"]
		}

		return mT, nil
	}

	strT := string(nv)

	if strT == "value" {
		return o, nil
	}

	rs := o.GetMember(strT)

	if !IsUndefInternal(rs) {
		return rs, nil
	}

	r, e := GetObjectMethodFunc(o, strT)

	if e != nil {
		fmt.Println("fNameT:", fNameT)
	}

	return r, e

	// return nil, ErrNotIndexable

	// return Undefined, nil
}

// Array represents array of objects and implements Object interface.
type Array []Object

var (
	_ Object       = Array{}
	_ LengthGetter = Array{}
)

func (Array) TypeCode() int {
	return 131
}

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
		if o[i] == nil {
			sb.WriteString("nil")
		} else {
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
		}

		if i != last {
			sb.WriteString(", ")
		}
	}

	sb.WriteString("]")
	return sb.String()
}

func (o Array) HasMemeber() bool {
	return false
}

func (o Array) CallMethod(nameA string, argsA ...Object) (Object, error) {
	//	switch nameA {
	//	case "value":
	//		return o, nil
	//	case "toStr":
	//		return ToStringObject(o), nil
	//	}

	return o.CallName(nameA, Call{Args: argsA})
	// return Undefined, NewCommonError("unknown method: %v", nameA)
}

func (o Array) CallName(nameA string, c Call) (Object, error) {
	switch nameA {
	case "toStr":
		return ToStringObject(o), nil
	case "value":
		return o, nil
	case "size":
		return ToIntObject(len(o)), nil
	case "remove":
		args := c.GetArgs()

		if len(args) < 1 {
			return NewCommonErrorWithPos(c, "not enough parameters"), nil
		}

		idxT := ToIntQuick(args[0])

		rs := append(o[0:idxT], o[idxT+1:]...)

		return rs, nil
	case "sort":
		var err error

		//		args := c.Args

		vs := ObjectsToS(c.GetArgs())

		keyT := tk.GetSwitch(vs, "-key=", "")
		//		if len(args) > 0 {
		//			keyT = args[0].String()
		//		}

		//		descT := false
		descT := tk.IfSwitchExists(vs, "-desc")
		//		if len(args) > 1 {
		//			optT := args[1].String()
		//			descT = (optT == "desc") // || (optT != "asc" && !(args[1].IsFalsy()))
		//		}

		//		obj := o.(Array)

		if keyT != "" {
			sort.Slice(o, func(i, j int) bool {
				// v1 := Undefined
				// v2 := Undefined

				m1, ok := o[i].(Map)

				if ok {
					m2, ok := o[j].(Map)
					if ok {
						v1, ok := m1[keyT]
						if ok {
							v2, ok := m2[keyT]

							if ok {
								v, e := v1.BinaryOp(tk.IfThenElse(descT, token.Greater, token.Less).(token.Token), v2)
								if e != nil && err == nil {
									err = e
									return false
								}
								if v != nil {
									return !v.IsFalsy()
								}
								return false
							}
						}
					}
				} else {
					sort.Slice(o, func(i, j int) bool {
						v, e := o[i].BinaryOp(tk.IfThenElse(descT, token.Greater, token.Less).(token.Token), o[j])
						if e != nil && err == nil {
							//					err = e
							return false
						}

						if v != nil {
							return !v.IsFalsy()
						}

						return false
					})
				}

				return false
			})
		} else {
			sort.Slice(o, func(i, j int) bool {
				v, e := o[i].BinaryOp(tk.IfThenElse(descT, token.Greater, token.Less).(token.Token), o[j])
				if e != nil && err == nil {
					//					err = e
					return false
				}

				if v != nil {
					return !v.IsFalsy()
				}

				return false
			})
		}

		if err != nil {
			return NewCommonErrorWithPos(c, "error: %v", err), nil
		}

		return o, nil

	case "sortByFunc":
		argsT := c.GetArgs()

		if c.Len() < 1 {
			return Undefined, fmt.Errorf("not enough parameters")
		}

		if true {
			//			fn, ok := argsT[0].(*Function)
			//
			//			if ok {
			//				return fn.CallEx(c)
			//			}

			cfn, ok := argsT[0].(*CompiledFunction)

			//			tk.Plv(cfn, ok)

			if ok {
				lenT := len(o)

				var tmpv Object

				ivkT := NewInvoker(c.VM(), cfn)

				for i := 0; i < lenT; i++ {
					for j := i + 1; j < lenT; j++ {
						retT, errT := ivkT.Invoke(Int(i), Int(j))
						//						tk.Plv(retT, errT)

						if errT == nil {
							bv1, ok := retT.(Bool)

							if ok {
								if !bool(bv1) {
									tmpv = o[i]
									o[i] = o[j]
									o[j] = tmpv
								}
							}
						}
					}
				}

				return o, nil
			}

			//			return NewCommonErrorWithPos(c, "member is not a function: %v", nameA), nil
		}

		nv1, ok := c.Get(0).(*Delegate)

		if !ok {
			return NewCommonErrorWithPos(c, "invalid type: %#v", c.Get(0)), nil
		}

		if nv1.Value == nil {
			return NewCommonErrorWithPos(c, "func code not compiled"), nil
		}

		func1 := func(i, j interface{}) bool {
			rsT := nv1.Value(i, j)

			nvT, okT := rsT.(bool)

			//			tk.Plv(rsT, nvT, okT)

			if !okT {
				return false
			}

			return bool(nvT)
		}

		lenT := len(o)

		var tmpv Object

		for i := 0; i < lenT; i++ {
			for j := i + 1; j < lenT; j++ {
				if !func1(o[i], o[j]) {
					tmpv = o[i]
					o[i] = o[j]
					o[j] = tmpv
				}
			}
		}

		return o, nil

	}

	rs1, errT := CallObjectMethodFunc(o, nameA, c.GetArgs()...)

	//	if errT != nil || tk.IsError(rs1) {
	//		rs3 := tk.ReflectCallMethodCompact(o, nameA, ObjectsToI(c.GetArgs())...)
	//		return ConvertToObject(rs3), nil
	//	}

	if errT != nil {
		return NewCommonErrorWithPos(c, "failed to call method(%v): %v", nameA, errT), nil
	}

	return rs1, errT

	//	return Undefined, ErrInvalidIndex.NewError(nameA)
}

func (o Array) GetValue() Object {
	return o
}

func (o Array) GetMember(idxA string) Object {
	return Undefined
}

func (o Array) SetMember(idxA string, valueA Object) error {
	return fmt.Errorf("unsupported action(set member)")
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

		if idx < 0 {
			idx = len(o) + idx
		}

		if idx >= 0 && idx < len(o) {
			o[idx] = value
			return nil
		}
		return ErrIndexOutOfBounds
	case Uint:
		idx := int(v)
		if idx < 0 {
			idx = len(o) + idx
		}

		if idx >= 0 && idx < len(o) {
			o[idx] = value
			return nil
		}
		return ErrIndexOutOfBounds
	}
	
	return NewIndexTypeError("int|uint", index.TypeName())
}

// IndexGet implements Object interface.
func (o Array) IndexGet(index Object) (Object, error) {
	//	tk.Pln("Array", "IndexGet", index)
	switch v := index.(type) {
	case Int:
		idx := int(v)
		if idx < 0 {
			idx = len(o) + idx
		}

		if idx >= 0 && idx < len(o) {
			return o[idx], nil
		}
		return nil, ErrIndexOutOfBounds
	case Uint:
		idx := int(v)
		if idx < 0 {
			idx = len(o) + idx
		}

		if idx >= 0 && idx < len(o) {
			return o[idx], nil
		}
		return nil, ErrIndexOutOfBounds
	case String:
		strT := v.String()

		if strT == "value" {
			return o, nil
		}

		rs := o.GetMember(strT)

		if !IsUndefInternal(rs) {
			return rs, nil
		}

		return GetObjectMethodFunc(o, strT)
	}

	return nil, NewIndexTypeError("int|uint|string", index.TypeName())
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
	case token.Less, token.LessEq:
		if right == Undefined {
			return False, nil
		}
	case token.Greater, token.GreaterEq:
		if right == Undefined {
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

// Len implements LengthGetter interface.
func (o Array) Len() int {
	return len(o)
}

// ObjectPtr represents a pointer variable.
// this class is for internal use only, for common purpose, use ObjectRef instead
type ObjectPtr struct {
	ObjectImpl
	Value *Object

	Members map[string]Object `json:"-"`
}

var (
	_ Object = (*ObjectPtr)(nil)
	_ Copier = (*ObjectPtr)(nil)
)

func (o *ObjectPtr) TypeCode() int {
	return 151
}

// TypeName implements Object interface.
func (o *ObjectPtr) TypeName() string {
	return "objectPtr"
}

// String implements Object interface.
func (o *ObjectPtr) String() string {
	var v Object
	if o.Value != nil {
		v = *o.Value
	}
	return fmt.Sprintf("<objectPtr:%v>", v)
}

func (o *ObjectPtr) HasMemeber() bool {
	return true
}

func (o *ObjectPtr) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return o, nil
	case "toStr":
		return ToStringObject(o), nil
	}

	return CallObjectMethodFunc(o, nameA, argsA...)
}

func (o *ObjectPtr) GetValue() Object {
	return o
}

func (o *ObjectPtr) GetMember(idxA string) Object {
	if o.Members == nil {
		return Undefined
	}

	v1, ok := o.Members[idxA]

	if !ok {
		return Undefined
	}

	return v1
}

func (o *ObjectPtr) SetMember(idxA string, valueA Object) error {
	if o.Members == nil {
		o.Members = map[string]Object{}
	}

	if IsUndefInternal(valueA) {
		delete(o.Members, idxA)
		return nil
	}

	o.Members[idxA] = valueA

	// return fmt.Errorf("unsupported action(set member)")
	return nil
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

var (
	_ Object       = Map{}
	_ Copier       = Map{}
	_ IndexDeleter = Map{}
	_ LengthGetter = Map{}
)

func (Map) TypeCode() int {
	return 133
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

func (o Map) HasMemeber() bool {
	return false
}

func (o Map) CallName(nameA string, c Call) (Object, error) {
	//	tk.Pl("call: %#v", c)
	rs, ok := o[nameA]

	if ok {
		fn, ok := rs.(*Function)

		if ok {
			return fn.CallEx(c)
		}

		cfn, ok := rs.(*CompiledFunction)

		if ok {
			retT, errT := NewInvoker(c.VM(), cfn).Invoke(c.GetArgs()...)

			return retT, errT
		}

		return NewCommonErrorWithPos(c, "member is not a function: %v", nameA), nil
	}

	switch nameA {
	case "remove":
		args := c.GetArgs()

		if len(args) < 1 {
			return NewCommonErrorWithPos(c, "not enough parameters"), nil
		}

		delete(o, args[0].String())

		return Undefined, nil

	case "Set", "set":
		args := c.GetArgs()

		if len(args) < 2 {
			return NewCommonErrorWithPos(c, "not enough parameters"), nil
		}

		o[args[0].String()] = args[1]

		return Undefined, nil

	case "Get", "get":
		args := c.GetArgs()

		if len(args) < 1 {
			return NewCommonErrorWithPos(c, "not enough parameters"), nil
		}

		rs := o[args[0].String()]

		if rs == nil {
			return Undefined, nil
		}

		return rs, nil
	case "size":
		return ToIntObject(len(o)), nil
	case "toOrdered":
		lenT := len(o)

		rs := tk.NewOrderedMap()

		if lenT < 1 {
			return &OrderedMap{Value: rs}, nil
		}

		for k, v := range o {
			rs.Set(k, v)
		}

		return &OrderedMap{Value: rs}, nil
	case "hasKey":
		args := c.GetArgs()

		if len(args) < 1 {
			return NewCommonErrorWithPos(c, "not enough parameters"), nil
		}

		_, ok := o[args[0].String()]

		if ok {
			return Bool(true), nil
		}

		return Bool(false), nil
	case "keys":
		lenT := len(o)

		rs := Array{}

		if lenT < 1 {
			return rs, nil
		}

		for k := range o {
			rs = append(rs, String(k))
		}

		return rs, nil
	case "values":
		lenT := len(o)

		rs := Array{}

		if lenT < 1 {
			return rs, nil
		}

		for _, v := range o {
			rs = append(rs, v)
		}

		return rs, nil
	}

	//	rs1, errT := CallObjectMethodFunc(o, nameA, c.GetArgs()...)
	//
	//	if errT != nil || tk.IsError(rs1) {
	//		rs3 := tk.ReflectCallMethodCompact(o.Value, nameA, ObjectsToI(c.GetArgs())...)
	//		return ConvertToObject(rs3), nil
	//	}
	//
	//	return rs1, errT

	return Undefined, ErrInvalidIndex.NewError(nameA)
}

func (o Map) CallMethod(nameA string, argsA ...Object) (Object, error) {
	return o.CallName(nameA, Call{Args: argsA})
	//	switch nameA {
	//	case "value":
	//		return o, nil
	//	case "toStr":
	//		return ToStringObject(o), nil
	//	}
	//
	//	return CallObjectMethodFunc(o, nameA, argsA...)
}

func (o Map) GetValue() Object {
	return o
}

func (o Map) GetMember(idxA string) Object {
	return Undefined
}

func (o Map) SetMember(idxA string, valueA Object) error {
	return fmt.Errorf("unsupported action(set member)")
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

// IndexDelete tries to delete the string value of key from the map.
// IndexDelete implements IndexDeleter interface.
func (o Map) IndexDelete(key Object) error {
	delete(o, key.String())
	return nil
}

// Len implements LengthGetter interface.
func (o Map) Len() int {
	return len(o)
}

// SyncMap represents map of objects and implements Object interface.
type SyncMap struct {
	mu    sync.RWMutex
	Value Map

	Members map[string]Object `json:"-"`
}

var (
	_ Object       = (*SyncMap)(nil)
	_ Copier       = (*SyncMap)(nil)
	_ IndexDeleter = (*SyncMap)(nil)
	_ LengthGetter = (*SyncMap)(nil)
)

func (*SyncMap) TypeCode() int {
	return 153
}

// RLock locks the underlying mutex for reading.
func (o *SyncMap) RLock() {
	o.mu.RLock()
}

// RUnlock unlocks the underlying mutex for reading.
func (o *SyncMap) RUnlock() {
	o.mu.RUnlock()
}

// Lock locks the underlying mutex for writing.
func (o *SyncMap) Lock() {
	o.mu.Lock()
}

// Unlock unlocks the underlying mutex for writing.
func (o *SyncMap) Unlock() {
	o.mu.Unlock()
}

// TypeName implements Object interface.
func (*SyncMap) TypeName() string {
	return "syncMap"
}

// String implements Object interface.
func (o *SyncMap) String() string {
	o.mu.RLock()
	defer o.mu.RUnlock()

	return o.Value.String()
}

func (o *SyncMap) HasMemeber() bool {
	return true
}

func (o *SyncMap) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return o, nil
	case "toStr":
		return ToStringObject(o), nil
	}

	return CallObjectMethodFunc(o, nameA, argsA...)
}

func (o *SyncMap) GetValue() Object {
	return o
}

func (o *SyncMap) GetMember(idxA string) Object {
	if o.Members == nil {
		return Undefined
	}

	v1, ok := o.Members[idxA]

	if !ok {
		return Undefined
	}

	return v1
}

func (o *SyncMap) SetMember(idxA string, valueA Object) error {
	if o.Members == nil {
		o.Members = map[string]Object{}
	}

	if IsUndefInternal(valueA) {
		delete(o.Members, idxA)
		return nil
	}

	o.Members[idxA] = valueA

	// return fmt.Errorf("unsupported action(set member)")
	return nil
}

// Copy implements Copier interface.
func (o *SyncMap) Copy() Object {
	o.mu.RLock()
	defer o.mu.RUnlock()

	return &SyncMap{
		Value: o.Value.Copy().(Map),
	}
}

// IndexSet implements Object interface.
func (o *SyncMap) IndexSet(index, value Object) error {
	o.mu.Lock()
	defer o.mu.Unlock()

	if o.Value == nil {
		o.Value = Map{}
	}
	return o.Value.IndexSet(index, value)
}

// IndexGet implements Object interface.
func (o *SyncMap) IndexGet(index Object) (Object, error) {
	o.mu.RLock()
	defer o.mu.RUnlock()

	return o.Value.IndexGet(index)
}

// Equal implements Object interface.
func (o *SyncMap) Equal(right Object) bool {
	o.mu.RLock()
	defer o.mu.RUnlock()

	return o.Value.Equal(right)
}

// IsFalsy implements Object interface.
func (o *SyncMap) IsFalsy() bool {
	o.mu.RLock()
	defer o.mu.RUnlock()

	return o.Value.IsFalsy()
}

// CanIterate implements Object interface.
func (o *SyncMap) CanIterate() bool { return true }

// Iterate implements Iterable interface.
func (o *SyncMap) Iterate() Iterator {
	o.mu.RLock()
	defer o.mu.RUnlock()

	return &SyncIterator{Iterator: o.Value.Iterate()}
}

// Get returns Object in map if exists.
func (o *SyncMap) Get(index string) (value Object, exists bool) {
	o.mu.RLock()
	value, exists = o.Value[index]
	o.mu.RUnlock()
	return
}

// Len returns the number of items in the map.
// Len implements LengthGetter interface.
func (o *SyncMap) Len() int {
	o.mu.RLock()
	n := len(o.Value)
	o.mu.RUnlock()
	return n
}

// IndexDelete tries to delete the string value of key from the map.
func (o *SyncMap) IndexDelete(key Object) error {
	o.mu.Lock()
	defer o.mu.Unlock()

	return o.Value.IndexDelete(key)
}

// BinaryOp implements Object interface.
func (o *SyncMap) BinaryOp(tok token.Token, right Object) (Object, error) {
	o.mu.RLock()
	defer o.mu.RUnlock()

	return o.Value.BinaryOp(tok, right)
}

// CanCall implements Object interface.
func (*SyncMap) CanCall() bool { return false }

// Call implements Object interface.
func (*SyncMap) Call(...Object) (Object, error) {
	return nil, ErrNotCallable
}

// Error represents Error Object and implements error and Object interfaces.
type Error struct {
	Name    string
	Message string
	Cause   error

	Members map[string]Object `json:"-"`
}

var (
	_ Object = (*Error)(nil)
	_ Copier = (*Error)(nil)
)

func (o *Error) Unwrap() error {
	if o.Cause == nil {
		return fmt.Errorf("%v", o.Message)
	}

	return o.Cause
}

func (*Error) TypeCode() int {
	return 155
}

// TypeName implements Object interface.
func (*Error) TypeName() string {
	return "error"
}

// String implements Object interface.
func (o *Error) String() string {
	return o.Error()
}

func (o *Error) HasMemeber() bool {
	return true
}

func (o *Error) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return o, nil
	case "toStr":
		return ToStringObject(o), nil
	}

	return CallObjectMethodFunc(o, nameA, argsA...)
}

func (o *Error) GetValue() Object {
	return o
}

func (o *Error) GetMember(idxA string) Object {
	if o.Members == nil {
		return Undefined
	}

	v1, ok := o.Members[idxA]

	if !ok {
		return Undefined
	}

	return v1
}

func (o *Error) SetMember(idxA string, valueA Object) error {
	if o.Members == nil {
		o.Members = map[string]Object{}
	}

	if IsUndefInternal(valueA) {
		delete(o.Members, idxA)
		return nil
	}

	o.Members[idxA] = valueA

	// return fmt.Errorf("unsupported action(set member)")
	return nil
}

// Copy implements Copier interface.
func (o *Error) Copy() Object {
	// tk.Pl("*Error copy")
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
	if IsUndefInternal(index) {
		return Undefined, nil
	}

	s := index.String()
	if s == "Name" {
		return ToStringObject(o.Name), nil
	}

	if s == "Message" {
		return ToStringObject(o.Message), nil
	}

	if s == "value" {
		return o, nil
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

	strT := s

	if strT == "value" {
		return o, nil
	}

	rs := o.GetMember(strT)

	if !IsUndefInternal(rs) {
		return rs, nil
	}

	return GetObjectMethodFunc(o, strT)

	// return Undefined, nil
}

// NewError creates a new Error and sets original Error as its cause which can be unwrapped.
func (o *Error) NewError(messages ...string) *Error {
	cp := o.Copy().(*Error)
	cp.Message = strings.Join(messages, " ")
	cp.Cause = nil
	return cp
}

// IndexSet implements Object interface.
func (o *Error) IndexSet(index, value Object) error {
	idxT, ok := index.(String)

	if ok {
		strT := idxT.String()
		if strT == "value" {
			if nv, ok := value.(*Error); ok {
				o.Name = nv.Name
				o.Message = nv.Message
				o.Cause = nv.Cause
				return nil
			}

			return ErrNotIndexAssignable
		}

		return o.SetMember(strT, value)
	}

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

	Members map[string]Object `json:"-"`
}

var (
	_ Object = (*RuntimeError)(nil)
	_ Copier = (*RuntimeError)(nil)
)

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

func (*RuntimeError) TypeCode() int {
	return 157
}

// TypeName implements Object interface.
func (*RuntimeError) TypeName() string {
	return "error"
}

// String implements Object interface.
func (o *RuntimeError) String() string {
	return o.Error()
}

func (o *RuntimeError) HasMemeber() bool {
	return true
}

func (o *RuntimeError) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return o, nil
	case "toStr":
		return ToStringObject(o), nil
	}

	return CallObjectMethodFunc(o, nameA, argsA...)
}

func (o *RuntimeError) GetValue() Object {
	return o
}

func (o *RuntimeError) GetMember(idxA string) Object {
	if o.Members == nil {
		return Undefined
	}

	v1, ok := o.Members[idxA]

	if !ok {
		return Undefined
	}

	return v1
}

func (o *RuntimeError) SetMember(idxA string, valueA Object) error {
	if o.Members == nil {
		o.Members = map[string]Object{}
	}

	if IsUndefInternal(valueA) {
		delete(o.Members, idxA)
		return nil
	}

	o.Members[idxA] = valueA

	// return fmt.Errorf("unsupported action(set member)")
	return nil
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
	if IsUndefInternal(index) {
		return Undefined, nil
	}

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

	strT := index.String()

	if strT == "value" {
		return o, nil
	}

	rs := o.GetMember(strT)

	if !IsUndefInternal(rs) {
		return rs, nil
	}

	return GetObjectMethodFunc(o, strT)

	// return Undefined, nil
}

// NewError creates a new Error and sets original Error as its cause which can be unwrapped.
func (o *RuntimeError) NewError(messages ...string) *RuntimeError {
	cp := o.Copy().(*RuntimeError)
	cp.Err.Message = strings.Join(messages, " ")
	cp.Err.Cause = o
	return cp
}

// IndexSet implements Object interface.
func (o *RuntimeError) IndexSet(index, value Object) error {
	idxT, ok := index.(String)

	if ok {
		strT := idxT.String()
		if strT == "value" {
			return ErrNotIndexAssignable
		}

		return o.SetMember(strT, value)
	}

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
				_, _ = fmt.Fprintf(s, "%+v", f)
			}
		default:
			_, _ = fmt.Fprintf(s, "%v", []parser.SourceFilePos(st))
		}
	}
}

// Time represents time values and implements Object interface.
type Time struct {
	Value time.Time

	Members map[string]Object `json:"-"`
}

var _ NameCallerObject = (*Time)(nil)

func (*Time) TypeCode() int {
	return 311
}

// TypeName implements Object interface.
func (*Time) TypeName() string {
	return "time"
}

// String implements Object interface.
func (o *Time) String() string {
	return o.Value.String()
}

func (o *Time) HasMemeber() bool {
	return true
}

func (o *Time) CallMethod(nameA string, argsA ...Object) (Object, error) {
	// tk.Pl("*Time CallMethod: %v", nameA)

	switch nameA {
	case "value":
		return o, nil
	case "toStr":
		return ToStringObject(o), nil
	}

	fn, ok := methodTableForTime[nameA]
	if !ok {
		rs3 := tk.ReflectCallMethodCompact(o.Value, nameA, ObjectsToI(argsA)...)

		return ConvertToObject(rs3), nil
		// return CallObjectMethodFunc(o, nameA, argsA...)
		// return Undefined, ErrInvalidIndex.NewError(nameA)
	}

	return fn(o, &Call{Args: argsA})

	// return Undefined, NewCommonError("unknown method: %v", nameA)
}

func (o *Time) GetValue() Object {
	return o
}

func (o *Time) GetMember(idxA string) Object {
	if o.Members == nil {
		return Undefined
	}

	v1, ok := o.Members[idxA]

	if !ok {
		return Undefined
	}

	return v1
}

func (o *Time) SetMember(idxA string, valueA Object) error {
	if o.Members == nil {
		o.Members = map[string]Object{}
	}

	if IsUndefInternal(valueA) {
		delete(o.Members, idxA)
		return nil
	}

	o.Members[idxA] = valueA

	// return fmt.Errorf("unsupported action(set member)")
	return nil
}

// IsFalsy implements Object interface.
func (o *Time) IsFalsy() bool {
	return o.Value.IsZero()
}

// Equal implements Object interface.
func (o *Time) Equal(right Object) bool {
	if v, ok := right.(*Time); ok {
		return o.Value.Equal(v.Value)
	}
	return false
}

// CanCall implements Object interface.
func (*Time) CanCall() bool { return false }

// Call implements Object interface.
func (*Time) Call(args ...Object) (Object, error) {
	return nil, ErrNotCallable
}

// CanIterate implements Object interface.
func (*Time) CanIterate() bool { return false }

// Iterate implements Object interface.
func (*Time) Iterate() Iterator { return nil }

// char:doc
// #### Overloaded time Operators
//
// - `time + int` -> time
// - `time - int` -> time
// - `time - time` -> int
// - `time < time` -> bool
// - `time > time` -> bool
// - `time <= time` -> bool
// - `time >= time` -> bool
//
// Note that, `int` values as duration must be the right hand side operand.

// BinaryOp implements Object interface.
func (o *Time) BinaryOp(tok token.Token,
	right Object) (Object, error) {

	switch v := right.(type) {
	case Int:
		switch tok {
		case token.Add:
			return &Time{Value: o.Value.Add(time.Duration(v))}, nil
		case token.Sub:
			return &Time{Value: o.Value.Add(time.Duration(-v))}, nil
		}
	case Byte:
		switch tok {
		case token.Add:
			return &Time{Value: o.Value.Add(time.Duration(v))}, nil
		case token.Sub:
			return &Time{Value: o.Value.Add(time.Duration(-v))}, nil
		}
	case Char:
		switch tok {
		case token.Add:
			return &Time{Value: o.Value.Add(time.Duration(v))}, nil
		case token.Sub:
			return &Time{Value: o.Value.Add(time.Duration(-v))}, nil
		}
	case Uint:
		switch tok {
		case token.Add:
			return &Time{Value: o.Value.Add(time.Duration(v))}, nil
		case token.Sub:
			return &Time{Value: o.Value.Add(time.Duration(-v))}, nil
		}
	case Float:
		switch tok {
		case token.Add:
			return &Time{Value: o.Value.Add(time.Duration(v))}, nil
		case token.Sub:
			return &Time{Value: o.Value.Add(time.Duration(-v))}, nil
		}
	case *BigInt:
		switch tok {
		case token.Add:
			return &Time{Value: o.Value.Add(time.Duration(v.Value.Int64()))}, nil
		case token.Sub:
			return &Time{Value: o.Value.Add(time.Duration(-v.Value.Int64()))}, nil
		}
	case *Time:
		switch tok {
		case token.Sub:
			return Int(o.Value.Sub(v.Value)), nil
		case token.Less:
			return Bool(o.Value.Before(v.Value)), nil
		case token.LessEq:
			return Bool(o.Value.Before(v.Value) || o.Value.Equal(v.Value)), nil
		case token.Greater:
			return Bool(o.Value.After(v.Value)), nil
		case token.GreaterEq:
			return Bool(o.Value.After(v.Value) || o.Value.Equal(v.Value)),
				nil
		}
	case *UndefinedType:
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

// IndexSet implements Object interface.
func (o *Time) IndexSet(index, value Object) error {
	idxT, ok := index.(String)

	if ok {
		strT := idxT.String()
		if strT == "value" {
			if nv, ok := value.(*Time); ok {
				o.Value = nv.Value
				return nil
			}

			return ErrNotIndexAssignable
		}

		return o.SetMember(strT, value)
	}

	return ErrNotIndexAssignable
}

// char:doc
// #### time Getters
//
// Deprecated: Use method call. These selectors will return a callable object in
// the future. See methods.
//
// Dynamically calculated getters for a time value are as follows:
//
// | Selector  | Return Type                                     |
// |:----------|:------------------------------------------------|
// |.Date      | {"year": int, "month": int, "day": int}         |
// |.Clock     | {"hour": int, "minute": int, "second": int}     |
// |.UTC       | time                                            |
// |.Unix      | int                                             |
// |.UnixNano  | int                                             |
// |.Year      | int                                             |
// |.Month     | int                                             |
// |.Day       | int                                             |
// |.Hour      | int                                             |
// |.Minute    | int                                             |
// |.Second    | int                                             |
// |.NanoSecond| int                                             |
// |.IsZero    | bool                                            |
// |.Local     | time                                            |
// |.Location  | location                                        |
// |.YearDay   | int                                             |
// |.Weekday   | int                                             |
// |.ISOWeek   | {"year": int, "week": int}                      |
// |.Zone      | {"name": string, "offset": int}                 |

// IndexGet implements Object interface.
func (o *Time) IndexGet(index Object) (Object, error) {
	v, ok := index.(String)
	if !ok {
		return Undefined, NewIndexTypeError("string", index.TypeName())
	}

	// For simplicity, we use method call for now. As getters are deprecated, we
	// will return callable object in the future here.

	strT := v.String()

	switch strT {
	case "Date", "Clock", "UTC", "Unix", "UnixNano", "Year", "Month", "Day",
		"Hour", "Minute", "Second", "Nanosecond", "IsZero", "Local", "Location",
		"YearDay", "Weekday", "ISOWeek", "Zone", "AddDate":
		return o.CallName(strT, Call{})
	case "value":
		return o, nil
	}

	if strT == "value" {
		return ToStringObject(o.Value), nil
	}

	rs := o.GetMember(strT)

	if !IsUndefInternal(rs) {
		return rs, nil
	}

	return GetObjectMethodFunc(o, strT)

	// return Undefined, nil
}

// char:doc
// #### time Methods
//
// | Method                               | Return Type                                 |
// |:-------------------------------------|:--------------------------------------------|
// |.Add(duration int)                    | time                                        |
// |.Sub(t2 time)                         | int                                         |
// |.AddDate(year int, month int, day int)| int                                         |
// |.After(t2 time)                       | bool                                        |
// |.Before(t2 time)                      | bool                                        |
// |.Format(layout string)                | string                                      |
// |.AppendFormat(b bytes, layout string) | bytes                                       |
// |.In(loc location)                     | time                                        |
// |.Round(duration int)                  | time                                        |
// |.Truncate(duration int)               | time                                        |
// |.Equal(t2 time)                       | bool                                        |
// |.Date()                               | {"year": int, "month": int, "day": int}     |
// |.Clock()                              | {"hour": int, "minute": int, "second": int} |
// |.UTC()                                | time                                        |
// |.Unix()                               | int                                         |
// |.UnixNano()                           | int                                         |
// |.Year()                               | int                                         |
// |.Month()                              | int                                         |
// |.Day()                                | int                                         |
// |.Hour()                               | int                                         |
// |.Minute()                             | int                                         |
// |.Second()                             | int                                         |
// |.NanoSecond()                         | int                                         |
// |.IsZero()                             | bool                                        |
// |.Local()                              | time                                        |
// |.Location()                           | location                                    |
// |.YearDay()                            | int                                         |
// |.Weekday()                            | int                                         |
// |.ISOWeek()                            | {"year": int, "week": int}                  |
// |.Zone()                               | {"name": string, "offset": int}             |

func (o *Time) CallName(name string, c Call) (Object, error) {
	// tk.Pl("Time CallName: %v", name)

	fn, ok := methodTableForTime[name]
	if !ok {
		rs3 := tk.ReflectCallMethodCompact(o.Value, name, ObjectsToI(c.GetArgs())...)

		return ConvertToObject(rs3), nil

		// return Undefined, ErrInvalidIndex.NewError(name)
	}

	return fn(o, &c)
}

var methodTableForTime = map[string]func(*Time, *Call) (Object, error){
	"add": func(o *Time, c *Call) (Object, error) {
		if err := c.CheckLen(1); err != nil {
			return Undefined, err
		}

		d, ok := ToGoFloat64(c.Get(0))
		if !ok {
			return newArgTypeErr("1st", "float", c.Get(0).TypeName())
		}

		addDurT := time.Duration(d * float64(time.Second))

		return &Time{Value: o.Value.Add(addDurT)}, nil

		// return timeAdd(o, d), nil
	},
	"sub": func(o *Time, c *Call) (Object, error) {
		if err := c.CheckLen(1); err != nil {
			return Undefined, err
		}
		t2, ok := ToTime(c.Get(0))
		if !ok {
			return newArgTypeErr("1st", "time", c.Get(0).TypeName())
		}
		return timeSub(o, t2), nil
	},
	"addDate": func(o *Time, c *Call) (Object, error) {
		if err := c.CheckLen(3); err != nil {
			return Undefined, err
		}
		year, ok := ToGoInt(c.Get(0))
		if !ok {
			return newArgTypeErr("1st", "int", c.Get(0).TypeName())
		}
		month, ok := ToGoInt(c.Get(1))
		if !ok {
			return newArgTypeErr("2nd", "int", c.Get(1).TypeName())
		}
		day, ok := ToGoInt(c.Get(2))
		if !ok {
			return newArgTypeErr("3rd", "int", c.Get(2).TypeName())
		}
		return timeAddDate(o, year, month, day), nil
	},
	"after": func(o *Time, c *Call) (Object, error) {
		if err := c.CheckLen(1); err != nil {
			return Undefined, err
		}
		t2, ok := ToTime(c.Get(0))
		if !ok {
			return newArgTypeErr("1st", "time", c.Get(0).TypeName())
		}
		return timeAfter(o, t2), nil
	},
	"before": func(o *Time, c *Call) (Object, error) {
		if err := c.CheckLen(1); err != nil {
			return Undefined, err
		}
		t2, ok := ToTime(c.Get(0))
		if !ok {
			return newArgTypeErr("1st", "time", c.Get(0).TypeName())
		}
		return timeBefore(o, t2), nil
	},
	"format": func(o *Time, c *Call) (Object, error) {
		// if err := c.CheckLen(1); err != nil {
		// 	return Undefined, err
		// }

		// var format string = ""
		// var ok bool
		// if c.Len() < 1 {
		// 	format = tk.TimeFormat
		// } else {
		// 	format, ok = ToGoString(c.Get(0))
		// 	if !ok {
		// 		return newArgTypeErr("1st", "string", c.Get(0).TypeName())
		// 	}
		// }
		return String(tk.FormatTime(o.Value, ObjectsToS(c.GetArgs())...)), nil
		// return timeFormat(o, format), nil
	},
	"appendFormat": func(o *Time, c *Call) (Object, error) {
		if err := c.CheckLen(2); err != nil {
			return Undefined, err
		}
		b, ok := ToGoByteSlice(c.Get(0))
		if !ok {
			return newArgTypeErr("1st", "bytes", c.Get(0).TypeName())
		}
		format, ok := ToGoString(c.Get(1))
		if !ok {
			return newArgTypeErr("2nd", "string", c.Get(1).TypeName())
		}
		return timeAppendFormat(o, b, format), nil
	},
	"in": func(o *Time, c *Call) (Object, error) {
		if err := c.CheckLen(1); err != nil {
			return Undefined, err
		}
		loc, ok := ToLocation(c.Get(0))
		if !ok {
			return newArgTypeErr("1st", "location", c.Get(0).TypeName())
		}
		return timeIn(o, loc), nil
	},
	"round": func(o *Time, c *Call) (Object, error) {
		if err := c.CheckLen(1); err != nil {
			return Undefined, err
		}
		d, ok := ToGoInt64(c.Get(0))
		if !ok {
			return newArgTypeErr("1st", "int", c.Get(0).TypeName())
		}
		return timeRound(o, d), nil
	},
	"truncate": func(o *Time, c *Call) (Object, error) {
		if err := c.CheckLen(1); err != nil {
			return Undefined, err
		}
		d, ok := ToGoInt64(c.Get(0))
		if !ok {
			return newArgTypeErr("1st", "int", c.Get(0).TypeName())
		}
		return timeTruncate(o, d), nil
	},
	"equal": func(o *Time, c *Call) (Object, error) {
		if err := c.CheckLen(1); err != nil {
			return Undefined, err
		}
		t2, ok := ToTime(c.Get(0))
		if !ok {
			return newArgTypeErr("1st", "time", c.Get(0).TypeName())
		}
		return timeEqual(o, t2), nil
	},
	"date": func(o *Time, c *Call) (Object, error) {
		if err := c.CheckLen(0); err != nil {
			return Undefined, err
		}
		y, m, d := o.Value.Date()
		return Map{"year": Int(y), "month": Int(m),
			"day": Int(d)}, nil
	},
	"getInfo": func(o *Time, c *Call) (Object, error) {
		if err := c.CheckLen(0); err != nil {
			return Undefined, err
		}
		zoneT, offsetT := o.Value.Zone()
		return Map{"Time": o, "Formal": String(o.Value.Format(tk.TimeFormat)), "Compact": String(o.Value.Format(tk.TimeFormatCompact)), "Full": String(fmt.Sprintf("%v", o.Value)), "Year": Int(o.Value.Year()), "Month": Int(o.Value.Month()), "Day": Int(o.Value.Day()), "Hour": Int(o.Value.Hour()), "Minute": Int(o.Value.Minute()), "Second": Int(o.Value.Second()), "Zone": ConvertToObject(zoneT), "Offset": ConvertToObject(offsetT), "UnixNano": Int(o.Value.UnixNano()), "WeekDay": Int(o.Value.Weekday()), "NanoSec": Int(o.Value.Nanosecond()), "MilliSec": Int(o.Value.Nanosecond() / 1000000)}, nil
	},
	"clock": func(o *Time, c *Call) (Object, error) {
		if err := c.CheckLen(0); err != nil {
			return Undefined, err
		}
		h, m, s := o.Value.Clock()
		return Map{"hour": Int(h), "minute": Int(m),
			"second": Int(s)}, nil
	},
	"utc": func(o *Time, c *Call) (Object, error) {
		if err := c.CheckLen(0); err != nil {
			return Undefined, err
		}
		return &Time{Value: o.Value.UTC()}, nil
	},
	"unix": func(o *Time, c *Call) (Object, error) {
		if err := c.CheckLen(0); err != nil {
			return Undefined, err
		}
		return Int(o.Value.Unix()), nil
	},
	"unixNano": func(o *Time, c *Call) (Object, error) {
		if err := c.CheckLen(0); err != nil {
			return Undefined, err
		}
		return Int(o.Value.UnixNano()), nil
	},
	"year": func(o *Time, c *Call) (Object, error) {
		if err := c.CheckLen(0); err != nil {
			return Undefined, err
		}
		return Int(o.Value.Year()), nil
	},
	"month": func(o *Time, c *Call) (Object, error) {
		if err := c.CheckLen(0); err != nil {
			return Undefined, err
		}
		return Int(o.Value.Month()), nil
	},
	"day": func(o *Time, c *Call) (Object, error) {
		if err := c.CheckLen(0); err != nil {
			return Undefined, err
		}
		return Int(o.Value.Day()), nil
	},
	"hour": func(o *Time, c *Call) (Object, error) {
		if err := c.CheckLen(0); err != nil {
			return Undefined, err
		}
		return Int(o.Value.Hour()), nil
	},
	"minute": func(o *Time, c *Call) (Object, error) {
		if err := c.CheckLen(0); err != nil {
			return Undefined, err
		}
		return Int(o.Value.Minute()), nil
	},
	"second": func(o *Time, c *Call) (Object, error) {
		if err := c.CheckLen(0); err != nil {
			return Undefined, err
		}
		return Int(o.Value.Second()), nil
	},
	"nanoSecond": func(o *Time, c *Call) (Object, error) {
		if err := c.CheckLen(0); err != nil {
			return Undefined, err
		}
		return Int(o.Value.Nanosecond()), nil
	},
	"isZero": func(o *Time, c *Call) (Object, error) {
		if err := c.CheckLen(0); err != nil {
			return Undefined, err
		}
		return Bool(o.Value.IsZero()), nil
	},
	"local": func(o *Time, c *Call) (Object, error) {
		if err := c.CheckLen(0); err != nil {
			return Undefined, err
		}
		return &Time{Value: o.Value.Local()}, nil
	},
	"location": func(o *Time, c *Call) (Object, error) {
		if err := c.CheckLen(0); err != nil {
			return Undefined, err
		}
		return &Location{Value: o.Value.Location()}, nil
	},
	"yearDay": func(o *Time, c *Call) (Object, error) {
		if err := c.CheckLen(0); err != nil {
			return Undefined, err
		}
		return Int(o.Value.YearDay()), nil
	},
	"weekDay": func(o *Time, c *Call) (Object, error) {
		if err := c.CheckLen(0); err != nil {
			return Undefined, err
		}
		return Int(o.Value.Weekday()), nil
	},
	"isoWeek": func(o *Time, c *Call) (Object, error) {
		if err := c.CheckLen(0); err != nil {
			return Undefined, err
		}
		y, w := o.Value.ISOWeek()
		return Map{"year": Int(y), "week": Int(w)}, nil
	},
	"zone": func(o *Time, c *Call) (Object, error) {
		if err := c.CheckLen(0); err != nil {
			return Undefined, err
		}
		name, offset := o.Value.Zone()
		return Map{"name": ToStringObject(name), "offset": Int(offset)}, nil
	},
}

// MarshalBinary implements encoding.BinaryMarshaler interface.
func (o *Time) MarshalBinary() ([]byte, error) {
	return o.Value.MarshalBinary()
}

// MarshalJSON implements json.JSONMarshaler interface.
func (o *Time) MarshalJSON() ([]byte, error) {
	return o.Value.MarshalJSON()
}

// UnmarshalBinary implements encoding.BinaryUnmarshaler interface.
func (o *Time) UnmarshalBinary(data []byte) error {
	var t time.Time
	if err := t.UnmarshalBinary(data); err != nil {
		return err
	}
	o.Value = t
	return nil
}

// UnmarshalJSON implements json.JSONUnmarshaler interface.
func (o *Time) UnmarshalJSON(data []byte) error {
	var t time.Time
	if err := t.UnmarshalJSON(data); err != nil {
		return err
	}
	o.Value = t
	return nil
}

func timeAdd(t *Time, duration int64) Object {
	return &Time{Value: t.Value.Add(time.Duration(duration))}
}

func timeSub(t1, t2 *Time) Object {
	return Int(t1.Value.Sub(t2.Value))
}

func timeAddDate(t *Time, years, months, days int) Object {
	return &Time{Value: t.Value.AddDate(years, months, days)}
}

func timeAfter(t1, t2 *Time) Object {
	return Bool(t1.Value.After(t2.Value))
}

func timeBefore(t1, t2 *Time) Object {
	return Bool(t1.Value.Before(t2.Value))
}

func timeFormat(t *Time, layout string) Object {
	return ToStringObject(t.Value.Format(layout))
}

func timeAppendFormat(t *Time, b []byte, layout string) Object {
	return Bytes(t.Value.AppendFormat(b, layout))
}

func timeIn(t *Time, loc *Location) Object {
	return &Time{Value: t.Value.In(loc.Value)}
}

func timeRound(t *Time, duration int64) Object {
	return &Time{Value: t.Value.Round(time.Duration(duration))}
}

func timeTruncate(t *Time, duration int64) Object {
	return &Time{Value: t.Value.Truncate(time.Duration(duration))}
}

func timeEqual(t1, t2 *Time) Object {
	return Bool(t1.Value.Equal(t2.Value))
}

func newArgTypeErr(pos, want, got string) (Object, error) {
	return Undefined, NewArgumentTypeError(pos, want, got)
}

// ToTime will try to convert given given Object to *Time value.
func ToTime(o Object) (ret *Time, ok bool) {
	switch o := o.(type) {
	case *Time:
		ret, ok = o, true
	case Int:
		v := time.Unix(int64(o), 0)
		ret, ok = &Time{Value: v}, true
	case String:
		v, err := time.Parse(time.RFC3339Nano, o.String())
		if err != nil {
			v, err = time.Parse(time.RFC3339, o.String())
		}
		if err == nil {
			ret, ok = &Time{Value: v}, true
		}
	}
	return
}

// ToLocation will try to convert given Object to *Location value.
func ToLocation(o Object) (ret *Location, ok bool) {
	if v, isString := o.(String); isString {
		var err error
		o, err = loadLocationFunc(v.String())
		if err != nil {
			return
		}
	}
	ret, ok = o.(*Location)
	return
}

// Location represents location values and implements Object interface.
type Location struct {
	ObjectImpl
	Value *time.Location

	Members map[string]Object `json:"-"`
}

func (*Location) TypeCode() int {
	return 313
}

// TypeName implements Object interface.
func (*Location) TypeName() string {
	return "location"
}

// String implements Object interface.
func (o *Location) String() string {
	return o.Value.String()
}

func (o *Location) HasMemeber() bool {
	return true
}

func (o *Location) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return o, nil
	case "toStr":
		return ToStringObject(o), nil
	}

	return CallObjectMethodFunc(o, nameA, argsA...)
}

func (o *Location) GetValue() Object {
	return o
}

func (o *Location) GetMember(idxA string) Object {
	if o.Members == nil {
		return Undefined
	}

	v1, ok := o.Members[idxA]

	if !ok {
		return Undefined
	}

	return v1
}

func (o *Location) SetMember(idxA string, valueA Object) error {
	if o.Members == nil {
		o.Members = map[string]Object{}
	}

	if IsUndefInternal(valueA) {
		delete(o.Members, idxA)
		return nil
	}

	o.Members[idxA] = valueA

	// return fmt.Errorf("unsupported action(set member)")
	return nil
}

// IsFalsy implements Object interface.
func (o *Location) IsFalsy() bool {
	return o.Value == nil
}

// Equal implements Object interface.
func (o *Location) Equal(right Object) bool {
	if v, ok := right.(*Location); ok {
		return v == o || v.String() == o.String()
	}
	if v, ok := right.(String); ok {
		return o.String() == v.String()
	}
	return false
}

func loadLocationFunc(name string) (Object, error) {
	l, err := time.LoadLocation(name)
	if err != nil {
		return Undefined, err
	}
	return &Location{Value: l}, nil
}

type Database struct {
	ObjectImpl
	Value           *sql.DB
	DBType          string
	DBConnectString string

	Members map[string]Object    `json:"-"`
	Methods map[string]*Function `json:"-"`
}

var _ Object = &Database{}

// func NewStringBuilder() *StringBuilder {
// 	return &StringBuilder{}
// }

// func NewStringBuilderValue(vA interface{}) StringBuilder {
// 	return StringBuilder{}
// }

func (o *Database) TypeCode() int {
	return 309
}

func (o *Database) TypeName() string {
	return "database"
}

func (o *Database) String() string {
	return fmt.Sprintf("(database)%v", o.Value)
}

func (o *Database) HasMemeber() bool {
	return true
}

func (o *Database) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return o, nil
	case "toStr":
		return ToStringObject(o), nil
	}

	return CallObjectMethodFunc(o, nameA, argsA...)
}

func (o *Database) GetValue() Object {
	return o
}

func (o *Database) GetMember(idxA string) Object {
	if o.Members == nil {
		return Undefined
	}

	v1, ok := o.Members[idxA]

	if !ok {
		return Undefined
	}

	return v1
}

func (o *Database) SetMember(idxA string, valueA Object) error {
	if o.Members == nil {
		o.Members = map[string]Object{}
	}

	if IsUndefInternal(valueA) {
		delete(o.Members, idxA)
		return nil
	}

	o.Members[idxA] = valueA

	// return fmt.Errorf("unsupported action(set member)")
	return nil
}

func (o Database) Equal(right Object) bool {
	if v, ok := right.(*Database); ok {
		return o.Value == v.Value
	}

	return false
}

func (o *Database) IsFalsy() bool { return o.Value == nil }

func (*Database) CanCall() bool { return false }

func (*Database) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

func (*Database) CanIterate() bool { return false }

func (*Database) Iterate() Iterator { return nil }

func (o *Database) IndexGet(index Object) (value Object, err error) {
	nv, ok := index.(String)
	if !ok {
		return nil, ErrNotIndexable
	}

	strT := nv.String()

	if strT == "value" {
		return o, nil
	}

	fNameT := nv.String()

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
					return NewAny(o.Value), nil
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
					if len(args) < 2 {
						return NewCommonError("not enough parameters"), nil
					}

					nv0, ok := args[0].(String)

					if !ok {
						return NewCommonError("invalid parameter 1"), nil
					}

					nv1, ok := args[1].(String)

					if !ok {
						return NewCommonError("invalid parameter 2"), nil
					}

					rsT := sqltk.ConnectDBX(string(nv0), nv1.String())
					if tk.IsError(rsT) {
						return NewFromError(rsT.(error)), nil
					}

					o.DBType = string(nv0)
					o.DBConnectString = nv1.String()
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

					return ConvertToObject(sqltk.QueryDBX(o.Value, s0.String(), objsT...)), nil
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

					return ConvertToObject(sqltk.QueryDBRecsX(o.Value, s0.String(), objsT...)), nil
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

					return ConvertToObject(sqltk.QueryDBMapX(o.Value, s0.String(), s1.String(), objsT...)), nil
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

					return ConvertToObject(sqltk.QueryDBMapArrayX(o.Value, s0.String(), s1.String(), objsT...)), nil
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

					return ConvertToObject(sqltk.QueryCountX(o.Value, s0.String(), objsT...)), nil
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

					return ConvertToObject(sqltk.QueryFloatX(o.Value, s0.String(), objsT...)), nil
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

					return ConvertToObject(sqltk.QueryStringX(o.Value, s0.String(), objsT...)), nil
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

					return ConvertToObject(sqltk.ExecDBX(o.Value, s0.String(), objsT...)), nil
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

	// if strT == "value" {
	// 	return o, nil
	// }

	rs := o.GetMember(strT)

	if !IsUndefInternal(rs) {
		return rs, nil
	}

	return GetObjectMethodFunc(o, strT)

	// return nil, ErrNotIndexable
}

func (o *Database) IndexSet(index, value Object) error {
	idxT, ok := index.(String)

	if ok {
		strT := idxT.String()
		if strT == "value" {
			if nv, ok := value.(*Database); ok {
				o.Value = nv.Value
				return nil
			}

			return ErrNotIndexAssignable
		}

		return o.SetMember(strT, value)
	}

	return ErrNotIndexAssignable
}

func (o *Database) BinaryOp(tok token.Token, right Object) (Object, error) {
	return nil, ErrInvalidOperator
}

type StatusResult struct {
	ObjectImpl
	Status  string
	Value   string
	Objects interface{}

	Members map[string]Object    `json:"-"`
	Methods map[string]*Function `json:"-"`
}

var StatusResultInvalid = StatusResult{Status: "", Value: ""}
var StatusResultSuccess = StatusResult{Status: "success", Value: ""}
var StatusResultFail = StatusResult{Status: "fail", Value: ""}

func (o *StatusResult) TypeCode() int {
	return 303
}

func (o *StatusResult) TypeName() string {
	return "statusResult"
}

func (o *StatusResult) String() string {
	return `{"Status": ` + tk.ObjectToJSON(o.Status) + `, "Value": ` + tk.ObjectToJSON(o.Value) + `}`
}

func (o *StatusResult) HasMemeber() bool {
	return true
}

func (o *StatusResult) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return o, nil
	case "toStr":
		return ToStringObject(o), nil
	}

	return CallObjectMethodFunc(o, nameA, argsA...)
}

func (o *StatusResult) GetValue() Object {
	return o
}

func (o *StatusResult) GetMember(idxA string) Object {
	if o.Members == nil {
		return Undefined
	}

	v1, ok := o.Members[idxA]

	if !ok {
		return Undefined
	}

	return v1
}

func (o *StatusResult) SetMember(idxA string, valueA Object) error {
	if o.Members == nil {
		o.Members = map[string]Object{}
	}

	if IsUndefInternal(valueA) {
		delete(o.Members, idxA)
		return nil
	}

	o.Members[idxA] = valueA

	// return fmt.Errorf("unsupported action(set member)")
	return nil
}

func (*StatusResult) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

func (o *StatusResult) Equal(right Object) bool {
	nv, ok := right.(*StatusResult)
	if !ok {
		return false
	}

	return ((nv.Status == o.Status) && (nv.Value == o.Value))
}

func (o *StatusResult) BinaryOp(tok token.Token, right Object) (Object, error) {
	return nil, ErrInvalidOperator
}

func GenStatusResult(args ...Object) (Object, error) {
	if len(args) < 1 {
		return &StatusResultInvalid, nil
	}

	if len(args) == 1 {
		nv, ok := args[0].(String)

		if !ok {
			return &StatusResultInvalid, nil
		}

		mapT := tk.JSONToMapStringString(nv.String())
		if mapT == nil {
			return &StatusResultInvalid, nil
		}

		statusT, ok := mapT["Status"]
		if !ok {
			return &StatusResultInvalid, nil
		}

		return &StatusResult{Status: statusT, Value: mapT["Value"]}, nil
	}

	nv0, ok := args[0].(String)

	if !ok {
		return &StatusResultInvalid, nil
	}

	nv1, ok := args[1].(String)

	if !ok {
		return &StatusResultInvalid, nil
	}

	return &StatusResult{Status: string(nv0), Value: nv1.String()}, nil

}

func (o *StatusResult) IndexGet(index Object) (Object, error) {
	nv, ok := index.(String)
	if !ok {
		return nil, ErrNotIndexable
	}

	fNameT := nv.String()

	if fNameT == "v" || fNameT == "value" {
		return o, nil
	}

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
						return ToStringObject(o.Status), nil
					}

					nv, ok := args[0].(String)

					if !ok {
						return &StatusResultInvalid, nil
					}

					o.Status = nv.String()
					return &StatusResultSuccess, nil
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
						return ToStringObject(o.Value), nil
					}

					nv, ok := args[0].(String)

					if !ok {
						return &StatusResultInvalid, nil
					}

					o.Value = nv.String()
					return &StatusResultSuccess, nil
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
					return ToStringObject(o.String()), nil
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
						return &StatusResultInvalid, nil
					}

					if len(args) == 1 {
						nv, ok := args[0].(String)

						if !ok {
							o.Status = ""
							o.Value = ""
							return &StatusResultInvalid, nil
						}

						mapT := tk.JSONToMapStringString(nv.String())
						if mapT == nil {
							o.Status = ""
							o.Value = ""
							return &StatusResultInvalid, nil
						}

						statusT, ok := mapT["Status"]
						if !ok {
							o.Status = ""
							o.Value = ""
							return &StatusResultInvalid, nil
						}

						o.Status = statusT
						o.Value = mapT["Value"]
						return &StatusResultSuccess, nil
					}

					nv0, ok := args[0].(String)

					if !ok {
						o.Status = ""
						o.Value = ""
						return &StatusResultInvalid, nil
					}

					nv1, ok := args[1].(String)

					if !ok {
						o.Status = ""
						o.Value = ""
						return &StatusResultInvalid, nil
					}

					o.Status = string(nv0)
					o.Value = string(nv1)
					return &StatusResultSuccess, nil
				}}
			fT = o.Methods["fromString"]
		}
		return fT, nil
	}

	strT := nv.String()

	// if strT == "value" {
	// 	return o, nil
	// }

	rs := o.GetMember(strT)

	if !IsUndefInternal(rs) {
		return rs, nil
	}

	return GetObjectMethodFunc(o, strT)

	// return nil, ErrNotIndexable
}

func (o *StatusResult) IndexSet(key, value Object) error {
	idxT, ok := key.(String)

	if ok {
		strT := idxT.String()
		if strT == "value" {
			if nv, ok := value.(*StatusResult); ok {
				o.Status = nv.Status
				o.Value = nv.Value
				o.Objects = nv.Objects

				return nil
			}

			return ErrNotIndexAssignable
		}

		return o.SetMember(strT, value)
	}

	return ErrNotIndexAssignable
}

// Any represents container object and implements the Object interfaces.
// Any is used to hold some data which is not an Object in Charlang, such as structured data from Golang function call
type Any struct {
	ObjectImpl
	Value        interface{}
	OriginalType string
	OriginalCode int

	Members map[string]Object `json:"-"`
}

var (
	_ Object = (*Any)(nil)
	_ Copier = (*Any)(nil)
)

func (o *Any) TypeCode() int {
	return 999
}

// TypeName implements Object interface.
func (*Any) TypeName() string {
	return "any"
}

// String implements Object interface.
func (o *Any) String() string {
	return fmt.Sprintf("(any:%T)%v", o.Value, o.Value)
}

func (o *Any) CallName(nameA string, c Call) (Object, error) {
	// tk.Pl("Any CallName: %v", name)

	fn, errT := GetObjectMethodFunc(o, nameA)

	if errT == nil && !tk.IsError(fn) && !IsUndefInternal(fn) {
		fnT := fn.(*Function)
		return (*fnT).Call(append([]Object{o}, c.GetArgs()...)...)
	}

	if !tk.ReflectHasMethod(o.Value, nameA) {
		return NewCommonErrorWithPos(c, "method(%v) not found for type: %T(%T)", nameA, o, o.Value), nil
	}

	rs3 := tk.ReflectCallMethodCompact(o.Value, nameA, ObjectsToI(c.GetArgs())...)

	return ConvertToObject(rs3), nil

}

func (o *Any) SetValue(valueA Object) error {
	rs, errT := builtinAnyFunc(Call{Args: []Object{valueA}})

	if errT != nil {
		return errT
	}

	nv1 := rs.(*Any)

	o.Value = nv1.Value
	o.OriginalCode = nv1.OriginalCode
	o.OriginalType = nv1.OriginalType

	// o.Members = nv1.Members

	return nil
}

func (o *Any) HasMemeber() bool {
	return true
}

func (o *Any) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return o, nil
	case "toStr":
		return ToStringObject(o), nil
	}

	return CallObjectMethodFunc(o, nameA, argsA...)
}

func (o *Any) GetValue() Object {
	return o
}

func (o *Any) GetMember(idxA string) Object {
	if o.Members == nil {
		return Undefined
	}

	v1, ok := o.Members[idxA]

	if !ok {
		return Undefined
	}

	return v1
}

func (o *Any) SetMember(idxA string, valueA Object) error {
	if o.Members == nil {
		o.Members = map[string]Object{}
	}

	if IsUndefInternal(valueA) {
		delete(o.Members, idxA)
		return nil
	}

	o.Members[idxA] = valueA

	// return fmt.Errorf("unsupported action(set member)")
	return nil
}

// Copy implements Copier interface.
func (o *Any) Copy() Object {
	return &Any{
		Value:        o.Value,
		OriginalType: o.OriginalType,
		OriginalCode: o.OriginalCode,
	}
}

// Equal implements Object interface.
func (o *Any) Equal(right Object) bool {
	if v, ok := right.(*Any); ok {
		return v.Value == o.Value
	}

	return false
}

// IsFalsy implements Object interface.
func (o *Any) IsFalsy() bool { return o.Value == nil }

// IndexGet implements Object interface.
func (o *Any) IndexGet(index Object) (Object, error) {
	switch v := index.(type) {
	case String:
		strT := v.String()

		if strT == "value" {
			return o, nil
		}

		rs := o.GetMember(strT)

		if !IsUndefInternal(rs) {
			return rs, nil
		}

		rs1, errT := GetObjectMethodFunc(o, strT)

		if errT == nil && !tk.IsError(rs1) {
			return rs1, nil
		}

		rs5 := tk.ReflectGetMember(o.Value, strT)

		if !tk.IsError(rs5) {
			rs6 := ConvertToObject(rs5)
			// tk.Plo(rs6)
			if nv, ok := rs6.(*Any); ok {
				if strings.HasPrefix(nv.OriginalType, "func(") {
					rs2 := &Function{
						Name: strT,
						ValueEx: func(c Call) (Object, error) {
							rs3 := tk.ReflectCallMethodCompact(o.Value, strT, ObjectsToI(c.GetArgs())...)

							return ConvertToObject(rs3), nil
						},
					}

					o.SetMember(strT, rs2)

					return rs2, nil

				}
			}
			o.SetMember(strT, rs6)
			return rs6, nil
		}

		if errT != nil {
			if tk.ReflectHasMethod(o.Value, strT) {
				rs2 := &Function{
					Name: strT,
					ValueEx: func(c Call) (Object, error) {
						rs3 := tk.ReflectCallMethodCompact(o.Value, strT, ObjectsToI(c.GetArgs())...)

						return ConvertToObject(rs3), nil
					},
				}

				o.SetMember(strT, rs2)

				return rs2, nil
			}
		}

		return rs1, errT
	}

	return nil, ErrNotIndexable
	// s := index.String()
	// if s == "type" {
	// 	return ToStringObject(o.OriginalType), nil
	// }

	// if s == "code" {
	// 	return Int(o.OriginalCode), nil
	// }

	// if s == "value" {
	// 	return o, nil
	// }

	// if s == "New" {
	// 	return &Function{
	// 		Name: "New",
	// 		Value: func(args ...Object) (Object, error) {
	// 			switch len(args) {
	// 			case 1:
	// 				return o.NewError(args[0].String()), nil
	// 			case 0:
	// 				return o.NewError(o.Message), nil
	// 			default:
	// 				msgs := make([]string, len(args))
	// 				for i := range args {
	// 					msgs[i] = args[0].String()
	// 				}
	// 				return o.NewError(msgs...), nil
	// 			}
	// 		},
	// 	}, nil
	// }
	// return Undefined, nil
}

// // NewError creates a new Error and sets original Error as its cause which can be unwrapped.
// func (o *Error) NewError(messages ...string) *Error {
// 	cp := o.Copy().(*Error)
// 	cp.Message = strings.Join(messages, " ")
// 	cp.Cause = o
// 	return cp
// }

// IndexSet implements Object interface.
func (o *Any) IndexSet(index, value Object) error {
	idxT, ok := index.(String)

	if ok {
		strT := idxT.String()
		if strT == "value" {
			return o.SetValue(value)
		}

		return o.SetMember(strT, value)
	}

	// fnT, ok := setterFuncMapG[o.TypeCode()]

	// if ok {
	// 	nv, ok := index.(String)
	// 	if !ok {
	// 		return ErrNotIndexAssignable
	// 	}

	// 	rs1, errT := fnT(Call{Args: Array{o, nv, value}})

	// 	if errT != nil {
	// 		return errT
	// 	}

	// 	nv1, ok := rs1.(Int)

	// 	if ok && nv1 == Int(-1) { // not found

	// 	} else {
	// 		return errT
	// 	}
	// }

	// // nv2, ok := o.(ObjectImpl)

	// // if ok {
	// if o.Members == nil {
	// 	o.Members = map[string]Object{}
	// }

	// o.Members[index.String()] = value
	// return nil
	// // }

	return ErrNotIndexAssignable
}

// BinaryOp implements Object interface.
func (o *Any) BinaryOp(tok token.Token, right Object) (Object, error) {
	return nil, ErrInvalidOperator
}

// CanCall implements Object interface.
func (*Any) CanCall() bool { return false }

// Call implements Object interface.
func (*Any) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

// CanIterate implements Object interface.
func (*Any) CanIterate() bool { return false }

// Iterate implements Object interface.
func (*Any) Iterate() Iterator { return nil }

func NewAny(vA interface{}, argsA ...string) *Any {
	originalT := ""

	if len(argsA) > 0 {
		originalT = argsA[0]
	}

	originalCodeT := -1

	nv, ok := vA.(Object)

	if ok {
		originalCodeT = nv.TypeCode()
	}

	return &Any{
		Value:        vA,
		OriginalType: originalT,
		OriginalCode: originalCodeT,
	}
}

type StringBuilder struct {
	ObjectImpl
	Value *strings.Builder

	Members map[string]Object `json:"-"`
}

var _ Object = &StringBuilder{}

// func NewStringBuilder() *StringBuilder {
// 	return &StringBuilder{}
// }

// func NewStringBuilderValue(vA interface{}) StringBuilder {
// 	return StringBuilder{}
// }

func (o *StringBuilder) TypeCode() int {
	return 307
}

func (o *StringBuilder) TypeName() string {
	return "stringBuilder"
}

func (o *StringBuilder) String() string {
	return fmt.Sprintf("(stringBuilder)%v", o.Value.String())
}

func (o *StringBuilder) HasMemeber() bool {
	return true
}

func (o *StringBuilder) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return ToStringObject(o.Value.String()), nil
	case "toStr":
		return ToStringObject(o), nil
	}

	return CallObjectMethodFunc(o, nameA, argsA...)
}

func (o *StringBuilder) GetValue() Object {
	return ToStringObject(o.Value.String())
}

func (o *StringBuilder) GetMember(idxA string) Object {
	if o.Members == nil {
		return Undefined
	}

	v1, ok := o.Members[idxA]

	if !ok {
		return Undefined
	}

	return v1
}

func (o *StringBuilder) SetMember(idxA string, valueA Object) error {
	if o.Members == nil {
		o.Members = map[string]Object{}
	}

	if IsUndefInternal(valueA) {
		delete(o.Members, idxA)
		return nil
	}

	o.Members[idxA] = valueA

	// return fmt.Errorf("unsupported action(set member)")
	return nil
}

func (o *StringBuilder) Equal(right Object) bool {
	if v, ok := right.(*StringBuilder); ok {
		return o.Value.String() == v.Value.String()
	}

	return false
}

func (o *StringBuilder) IsFalsy() bool { return o.Value == nil }

func (*StringBuilder) CanCall() bool { return false }

func (*StringBuilder) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

func (*StringBuilder) CanIterate() bool { return false }

func (*StringBuilder) Iterate() Iterator { return nil }

func (o *StringBuilder) CallName(nameA string, c Call) (Object, error) {
	//	tk.Pl("EvalMachine call: %#v", c)
	switch nameA {
	case "toStr":
		return ToStringObject(o.Value.String()), nil
	case "String": // to keep Golang compatibility
		return ToStringObject(o.Value.String()), nil
	case "writeStr", "WriteString":
		args := c.GetArgs()

		if len(args) < 1 {
			return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
		}

		rsT, errT := o.Value.WriteString(args[0].String())

		if errT != nil {
			return NewCommonErrorWithPos(c, "%v", errT), nil
		}

		return Int(rsT), nil
	case "writeBytes":
		var errT error

		args := c.GetArgs()

		if len(args) < 1 {
			return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
		}

		nv, ok := args[0].(Bytes)
		if ok {
			rsT, errT := o.Value.Write([]byte(nv))
			if errT != nil {
				return NewCommonErrorWithPos(c, "%v", errT), nil
			}

			return Int(rsT), nil
		}

		nv1, ok := args[0].(Array)
		if ok {
			countT := 0

			for _, jv := range nv1 {
				jnv := byte(ToIntQuick(jv))

				errT = o.Value.WriteByte(jnv)

				if errT != nil {
					return NewCommonErrorWithPos(c, "failed to write byte: %v", errT), nil
				} else {
					countT++
				}
			}

			return Int(countT), nil
		}

		nv2, ok := args[0].(String)
		if ok {
			rsT, errT := o.Value.Write([]byte(nv2.String()))
			if errT != nil {
				return NewCommonErrorWithPos(c, "%v", errT), nil
			}

			return Int(rsT), nil
		}

		return NewCommonError("invalid parameter type: %v", args[0].TypeName()), nil

	case "write":
		var errT error

		args := c.GetArgs()

		if len(args) < 1 {
			return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
		}

		countT := 0

		for _, v := range args {
			tmpCountT := 0
			switch nv := v.(type) {
			case String:
				tmpCountT, errT = o.Value.WriteString(nv.String())

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
			case Int:
				tmpCountT, errT = o.Value.WriteRune(rune(nv))

				if errT != nil {
					tmpCountT = 0
				}
			case Array:
				for _, jv := range nv {
					switch jnv := jv.(type) {
					case String:
						tmpCountT, errT = o.Value.WriteString(jnv.String())

						if errT != nil {
							tmpCountT = 0
						}
					case Bytes:
						tmpCountT, errT = o.Value.Write([]byte(jnv))

						if errT != nil {
							tmpCountT = 0
						}
					case Byte:
						errT = o.Value.WriteByte(byte(jnv))

						if errT != nil {
							tmpCountT = 0
						} else {
							tmpCountT = 1
						}
					case Char:
						tmpCountT, errT = o.Value.WriteRune(rune(jnv))

						if errT != nil {
							tmpCountT = 0
						}
					case Int:
						tmpCountT, errT = o.Value.WriteRune(rune(jnv))

						if errT != nil {
							tmpCountT = 0
						}
					}

					countT += tmpCountT
				}

				tmpCountT = 0
			default:
				tmpCountT, errT = o.Value.WriteString(nv.String())

				if errT != nil {
					tmpCountT = 0
				}

			}

			countT += tmpCountT
		}

		return Int(countT), nil
	case "clear", "reset":
		o.Value.Reset()
		return Undefined, nil
	}

	args := c.GetArgs()

	rs1, errT := CallObjectMethodFunc(o, nameA, args...)

	if errT != nil || tk.IsError(rs1) {
		rs3 := tk.ReflectCallMethodCompact(o.Value, nameA, ObjectsToI(args)...)
		return ConvertToObject(rs3), nil
	}

	return rs1, errT

	//	return Undefined, NewCommonErrorWithPos(c, "method not found: %v", nameA)
}

func (o *StringBuilder) IndexGet(index Object) (value Object, err error) {
	switch v := index.(type) {
	case String:
		strT := v.String()

		if strT == "value" {
			return ToStringObject(o.Value.String()), nil
		}

		rs := o.GetMember(strT)

		if !IsUndefInternal(rs) {
			return rs, nil
		}

		return GetObjectMethodFunc(o, strT)
	}

	// switch v := index.(type) {
	// case String:
	// 	strT := v.Value

	// 	if strT == "value" {
	// 		return ToStringObject(o.Value.String()), nil
	// 	}

	// 	// return GetObjectMember(Call{This: o, args: []Object{o, ToStringObject(v.Value)}})
	// }

	// return GetObjectMember(Call{This: o, args: []Object{o, index}}) // GetObjectMethodFunc(o, index)
	// 	nv, ok := index.(String)
	// 	if !ok {
	// 		return nil, ErrNotIndexable
	// 	}

	// 	fNameT := nv.Value

	// 	if o.Members == nil {
	// 		o.Members = map[string]Object{}
	// 	}

	// 	if o.Methods == nil {
	// 		o.Methods = map[string]*Function{}
	// 	}

	// 	switch fNameT {
	// 	case "write":
	// 		fT, ok := o.Methods["write"]
	// 		if !ok {
	// 			o.Methods["write"] = &Function{
	// 				Name: "write",
	// 				Value: func(args ...Object) (Object, error) {

	// 					countT := 0
	// 					var errT error

	// 					for _, v := range args {
	// 						tmpCountT := 0
	// 						switch nv := v.(type) {
	// 						case String:
	// 							tmpCountT, errT = o.Value.WriteString(nv.Value)

	// 							if errT != nil {
	// 								tmpCountT = 0
	// 							}
	// 						case Bytes:
	// 							tmpCountT, errT = o.Value.Write([]byte(nv))

	// 							if errT != nil {
	// 								tmpCountT = 0
	// 							}
	// 						case Char:
	// 							tmpCountT, errT = o.Value.WriteRune(rune(nv))

	// 							if errT != nil {
	// 								tmpCountT = 0
	// 							}
	// 						case Byte:
	// 							errT = o.Value.WriteByte(byte(nv))

	// 							if errT != nil {
	// 								tmpCountT = 0
	// 							} else {
	// 								tmpCountT = 1
	// 							}
	// 						default:
	// 							tmpCountT, errT = o.Value.WriteString(nv.String())

	// 							if errT != nil {
	// 								tmpCountT = 0
	// 							}

	// 						}

	// 						countT += tmpCountT
	// 					}

	// 					return Int(countT), nil
	// 				}}
	// 			fT = o.Methods["write"]
	// 		}
	// 		return fT, nil
	// 	case "writeString":
	// 		fT, ok := o.Methods["writeString"]
	// 		if !ok {
	// 			o.Methods["writeString"] = &Function{
	// 				Name: "writeString",
	// 				Value: func(args ...Object) (Object, error) {
	// 					if len(args) < 1 {
	// 						return Int(0), NewCommonError("not enough parameters")
	// 					}

	// 					rsT, errT := o.Value.WriteString(args[0].String())

	// 					if errT != nil {
	// 						return Int(rsT), NewFromError(errT)
	// 					}

	// 					return Int(rsT), nil
	// 				}}
	// 			fT = o.Methods["writeString"]
	// 		}
	// 		return fT, nil
	// 	case "writeBytes":
	// 		fT, ok := o.Methods["writeBytes"]
	// 		if !ok {
	// 			o.Methods["writeBytes"] = &Function{
	// 				Name: "writeBytes",
	// 				Value: func(args ...Object) (Object, error) {
	// 					if len(args) < 1 {
	// 						return Int(0), NewCommonError("not enough parameters")
	// 					}

	// 					nv, ok := args[0].(Bytes)
	// 					if !ok {
	// 						return Int(0), NewCommonError("invalid parameter")
	// 					}

	// 					rsT, errT := o.Value.Write([]byte(nv))
	// 					if errT != nil {
	// 						return Int(rsT), NewFromError(errT)
	// 					}

	// 					return Int(rsT), nil
	// 				}}
	// 			fT = o.Methods["writeBytes"]
	// 		}
	// 		return fT, nil
	// 	case "toString":
	// 		fT, ok := o.Methods["toString"]
	// 		if !ok {
	// 			o.Methods["toString"] = &Function{
	// 				Name: "toString",
	// 				Value: func(args ...Object) (Object, error) {
	// 					return ToStringObject(o.Value.String()), nil
	// 				}}
	// 			fT = o.Methods["toString"]
	// 		}
	// 		return fT, nil
	// 	case "reset", "clear":
	// 		fT, ok := o.Methods["reset"]
	// 		if !ok {
	// 			o.Methods["reset"] = &Function{
	// 				Name: "reset",
	// 				Value: func(args ...Object) (Object, error) {
	// 					o.Value.Reset()
	// 					return Undefined, nil
	// 				}}
	// 			fT = o.Methods["reset"]
	// 		}
	// 		return fT, nil
	// 	}

	return nil, ErrNotIndexable
}

func (o *StringBuilder) IndexSet(index, value Object) error {
	idxT, ok := index.(String)

	if ok {
		strT := idxT.String()
		if strT == "value" {
			if nv, ok := value.(String); ok {
				o.Value.Reset()
				o.Value.WriteString(nv.String())
				return nil
			}

			if nv, ok := value.(*MutableString); ok {
				o.Value.Reset()
				o.Value.WriteString(nv.Value)
				return nil
			}

			return ErrNotIndexAssignable
		}

		return o.SetMember(strT, value)
	}

	return ErrNotIndexAssignable
}

func (o *StringBuilder) BinaryOp(tok token.Token, right Object) (Object, error) {
	return nil, ErrInvalidOperator
}

func (o *StringBuilder) Copy() Object {
	rsT := StringBuilder{Value: new(strings.Builder)}

	rsT.Value.WriteString(o.Value.String())

	return &rsT
}

type BytesBuffer struct {
	ObjectImpl
	Value *bytes.Buffer

	Members map[string]Object `json:"-"`
}

var _ Object = &BytesBuffer{}

func (o *BytesBuffer) TypeCode() int {
	return 308
}

func (o *BytesBuffer) TypeName() string {
	return "bytesBuffer"
}

func (o *BytesBuffer) String() string {
	return fmt.Sprintf("%v", *(o.Value))
}

func (o *BytesBuffer) HasMemeber() bool {
	return true
}

func (o *BytesBuffer) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return o, nil
	case "toStr":
		return ToStringObject(o), nil
	}

	return CallObjectMethodFunc(o, nameA, argsA...)
}

func (o *BytesBuffer) GetValue() Object {
	return Bytes(o.Value.Bytes())
}

func (o *BytesBuffer) GetMember(idxA string) Object {
	if o.Members == nil {
		return Undefined
	}

	v1, ok := o.Members[idxA]

	if !ok {
		return Undefined
	}

	return v1
}

func (o *BytesBuffer) SetMember(idxA string, valueA Object) error {
	if o.Members == nil {
		o.Members = map[string]Object{}
	}

	if IsUndefInternal(valueA) {
		delete(o.Members, idxA)
		return nil
	}

	o.Members[idxA] = valueA

	// return fmt.Errorf("unsupported action(set member)")
	return nil
}

func (o *BytesBuffer) Equal(right Object) bool {
	if v, ok := right.(*BytesBuffer); ok {
		buf1 := o.Value.Bytes()
		buf2 := v.Value.Bytes()

		r1 := bytes.Compare(buf1, buf2)

		if r1 != 0 {
			return false
		}

		return true
	}

	return false
}

func (o *BytesBuffer) IsFalsy() bool { return o.Value == nil }

func (*BytesBuffer) CanCall() bool { return false }

func (*BytesBuffer) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

func (*BytesBuffer) CanIterate() bool { return false }

func (*BytesBuffer) Iterate() Iterator { return nil }

func (o *BytesBuffer) IndexGet(index Object) (value Object, err error) {
	switch v := index.(type) {
	case String:
		strT := v.String()

		if strT == "value" {
			return Bytes(o.Value.Bytes()), nil
		}

		rs := o.GetMember(strT)

		if !IsUndefInternal(rs) {
			return rs, nil
		}

		return GetObjectMethodFunc(o, strT)
	}
	return nil, ErrNotIndexable
	// switch v := index.(type) {
	// case String:
	// 	strT := v.Value

	// 	if strT == "value" {
	// 		return o, nil
	// 	}

	// 	return GetObjectMember(Call{This: o, args: []Object{o, v}})
	// }

	// return GetObjectMember(Call{This: o, args: []Object{o, index}})

	// return GetObjectMethodFunc(o, index)
}

func (o *BytesBuffer) IndexSet(index, value Object) error {
	idxT, ok := index.(String)

	if ok {
		strT := idxT.String()
		if strT == "value" {
			if nv, ok := value.(Bytes); ok {
				o.Value.Reset()
				o.Value.Write(nv)
				return nil
			}

			return ErrNotIndexAssignable
		}

		return o.SetMember(strT, value)
	}

	return ErrNotIndexAssignable
}

func (o *BytesBuffer) BinaryOp(tok token.Token, right Object) (Object, error) {
	return nil, ErrInvalidOperator
}

func (o *BytesBuffer) Copy() Object {
	rsT := &BytesBuffer{Value: new(bytes.Buffer)}

	rsT.Value.WriteString(o.Value.String())

	return rsT
}

func NewBytesBuffer(c Call) (Object, error) {
	argsA := c.GetArgs()

	if len(argsA) > 0 {
		nv, ok := argsA[0].(Bytes)
		if !ok {
			return NewCommonError("unsupport type: %T", argsA[0]), nil
		}

		return &BytesBuffer{Value: bytes.NewBuffer([]byte(nv))}, nil
	}

	return &BytesBuffer{Value: new(bytes.Buffer)}, nil
}

// ObjectRef represents a reference variable.
// always refer to an Object(i.e. *Object)
type ObjectRef struct {
	ObjectImpl
	Value *Object

	Members map[string]Object `json:"-"`
}

var (
	_ Object = (*ObjectRef)(nil)
	_ Copier = (*ObjectRef)(nil)
)

func (o *ObjectRef) TypeCode() int {
	return 152
}

// TypeName implements Object interface.
func (o *ObjectRef) TypeName() string {
	return "objectRef"
}

// String implements Object interface.
func (o *ObjectRef) String() string {
	var v Object
	if o.Value != nil {
		v = *o.Value
	}
	return fmt.Sprintf("<objectRef:%v>", v)
}

func (o *ObjectRef) HasMemeber() bool {
	return true
}

func (o *ObjectRef) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return o, nil
	case "toStr":
		return ToStringObject(o), nil
	}

	return CallObjectMethodFunc(o, nameA, argsA...)
}

func (o *ObjectRef) GetValue() Object {
	return o
}

func (o *ObjectRef) GetMember(idxA string) Object {
	if o.Members == nil {
		return Undefined
	}

	v1, ok := o.Members[idxA]

	if !ok {
		return Undefined
	}

	return v1
}

func (o *ObjectRef) SetMember(idxA string, valueA Object) error {
	if o.Members == nil {
		o.Members = map[string]Object{}
	}

	if IsUndefInternal(valueA) {
		delete(o.Members, idxA)
		return nil
	}

	o.Members[idxA] = valueA

	// return fmt.Errorf("unsupported action(set member)")
	return nil
}

// Copy implements Copier interface.
func (o *ObjectRef) Copy() Object {
	return o
}

// IsFalsy implements Object interface.
func (o *ObjectRef) IsFalsy() bool {
	return o.Value == nil
}

// Equal implements Object interface.
func (o *ObjectRef) Equal(x Object) bool {
	return o == x
}

// BinaryOp implements Object interface.
func (o *ObjectRef) BinaryOp(tok token.Token, right Object) (Object, error) {
	if o.Value == nil {
		return nil, errors.New("nil pointer")
	}
	return (*o.Value).BinaryOp(tok, right)
}

// CanCall implements Object interface.
func (o *ObjectRef) CanCall() bool {
	if o.Value == nil {
		return false
	}
	return (*o.Value).CanCall()
}

// Call implements Object interface.
func (o *ObjectRef) Call(args ...Object) (Object, error) {
	if o.Value == nil {
		return nil, errors.New("nil pointer")
	}
	return (*o.Value).Call(args...)
}

// MutableString represents string values and implements Object interface, compare to String, it supports setValue method.
type MutableString struct {
	ObjectImpl
	Value string

	Members map[string]Object `json:"-"`
	// Methods map[string]*Function
}

var _ LengthGetter = ToMutableStringObject("")
var _ ValueSetter = ToMutableStringObject("")

func (*MutableString) TypeCode() int {
	return 106
}

// TypeName implements Object interface.
func (*MutableString) TypeName() string {
	return "mutableString"
}

func (o *MutableString) String() string {
	return o.Value // fmt.Sprintf("%v (%#v, %#v)", o, o.Members, o.Methods) //
}

func (o *MutableString) MarshalJSON() ([]byte, error) {
	b1, err := json.Marshal(o.Value)
	return b1, err
}

func (o *MutableString) SetValue(valueA Object) error {
	// nv1, ok := valueA.(*MutableString)

	// if ok {
	// 	o = nv1
	// 	return nil
	// }

	o.Value = valueA.String()

	return nil
}

func (o *MutableString) HasMemeber() bool {
	return true
}

func (o *MutableString) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return o, nil
	case "toStr":
		return ToStringObject(o), nil
	}

	return CallObjectMethodFunc(o, nameA, argsA...)
}

func (o *MutableString) GetValue() Object {
	return o
}

func (o *MutableString) GetMember(idxA string) Object {
	if o.Members == nil {
		return Undefined
	}

	v1, ok := o.Members[idxA]

	if !ok {
		return Undefined
	}

	return v1
}

func (o *MutableString) SetMember(idxA string, valueA Object) error {
	if o.Members == nil {
		o.Members = map[string]Object{}
	}

	if IsUndefInternal(valueA) {
		delete(o.Members, idxA)
		return nil
	}

	o.Members[idxA] = valueA

	// return fmt.Errorf("unsupported action(set member)")
	return nil
}

// CanIterate implements Object interface.
func (*MutableString) CanIterate() bool { return true }

// Iterate implements Object interface.
func (o *MutableString) Iterate() Iterator {
	return &MutableStringIterator{V: o}
}

// IndexSet implements Object interface.
func (o *MutableString) IndexSet(index, value Object) error {
	idxT, ok := index.(String)

	if ok {
		strT := idxT.String()
		if strT == "value" {
			o.Value = value.String()
			return nil
		}

		return o.SetMember(strT, value)
	}

	return ErrNotIndexAssignable
}

// IndexGet represents string values and implements Object interface.
func (o *MutableString) IndexGet(index Object) (Object, error) {
	var idx int
	switch v := index.(type) {
	case Int:
		idx = int(v)
	case Uint:
		idx = int(v)
	case Char:
		idx = int(v)
	case String:
		strT := v.String()

		if strT == "value" {
			return ToStringObject(o.Value), nil
		}

		rs := o.GetMember(strT)

		if !IsUndefInternal(rs) {
			return rs, nil
		}

		// return nil, ErrIndexOutOfBounds
		return GetObjectMethodFunc(o, strT) // Call{This: o, args: []Object{o, v}}
	default:
		return nil, NewIndexTypeError("int|uint|char|string", index.TypeName())
	}

	if idx >= 0 && idx < len(o.Value) {
		return Int(o.Value[idx]), nil
	}

	return nil, ErrIndexOutOfBounds
}

// Equal implements Object interface.
func (o *MutableString) Equal(right Object) bool {
	if v, ok := right.(String); ok {
		return o.Value == v.String()
	}
	if v, ok := right.(Bytes); ok {
		return o.Value == string(v)
	}
	return false
}

// IsFalsy implements Object interface.
func (o *MutableString) IsFalsy() bool { return len(o.Value) == 0 }

// CanCall implements Object interface.
func (o *MutableString) CanCall() bool { return false }

// Call implements Object interface.
func (o *MutableString) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

// BinaryOp implements Object interface.
func (o *MutableString) BinaryOp(tok token.Token, right Object) (Object, error) {
	// tk.Pl("*MutableString BinaryOp: %v, %v", tok, right)
	switch v := right.(type) {
	case String:
		switch tok {
		case token.Add:
			return String(o.Value + v.String()), nil
		case token.Less:
			return Bool(o.Value < v.String()), nil
		case token.LessEq:
			return Bool(o.Value <= v.String()), nil
		case token.Greater:
			return Bool(o.Value > v.String()), nil
		case token.GreaterEq:
			return Bool(o.Value >= v.String()), nil
		}
	case *MutableString:
		switch tok {
		case token.Add:
			return &MutableString{Value: o.Value + v.Value}, nil
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
			sb.WriteString(string(o.Value))
			sb.Write(v)
			return ToStringObject(sb.String()), nil
		case token.Less:
			return Bool(o.Value < string(v)), nil
		case token.LessEq:
			return Bool(o.Value <= string(v)), nil
		case token.Greater:
			return Bool(o.Value > string(v)), nil
		case token.GreaterEq:
			return Bool(o.Value >= string(v)), nil
		}
	case *UndefinedType:
		switch tok {
		case token.Less, token.LessEq:
			return False, nil
		case token.Greater, token.GreaterEq:
			return True, nil
		}
	}

	if tok == token.Add {
		return String(o.Value + right.String()), nil
	}

	return nil, NewOperandTypeError(
		tok.String(),
		o.TypeName(),
		right.TypeName())
}

// Len implements LengthGetter interface.
func (o *MutableString) Len() int {
	return len(o.Value)
}

// Format implements fmt.Formatter interface.
func (o *MutableString) Format(s fmt.State, verb rune) {
	format := compat.FmtFormatString(s, verb)
	fmt.Fprintf(s, format, o.Value)
}

func ToMutableStringObject(argA interface{}) *MutableString {
	switch nv := argA.(type) {
	case String:
		return &MutableString{Value: nv.String()}
	case Object:
		return &MutableString{Value: nv.String()}
	case string:
		return &MutableString{Value: nv}
	case nil:
		return &MutableString{Value: ""}
	case []byte:
		return &MutableString{Value: string(nv)}
	}
	return &MutableString{Value: fmt.Sprintf("%v", argA)}
}

// Seq object is used for generate unique sequence number(integer)
type Seq struct {
	ObjectImpl
	Value *(tk.Seq)

	Members map[string]Object `json:"-"`
	// Methods map[string]*Function
}

var _ Object = NewSeq()
var _ ValueSetter = NewSeq()

func (*Seq) TypeCode() int {
	return 315
}

// TypeName implements Object interface.
func (*Seq) TypeName() string {
	return "seq"
}

func (o *Seq) String() string {
	return fmt.Sprintf("%v", o.Value)
}

func (o *Seq) SetValue(valueA Object) error {
	o.Value.Reset(int(ToIntObject(valueA)))

	return nil
}

func (o *Seq) HasMemeber() bool {
	return true
}

func (o *Seq) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return builtinAnyFunc(Call{Args: []Object{o}})
	case "toStr":
		return ToStringObject(o), nil
	}

	return CallObjectMethodFunc(o, nameA, argsA...)
}

func (o *Seq) GetValue() Object {
	return ToIntObject(o.Value.Get())
}

func (o *Seq) GetMember(idxA string) Object {
	if o.Members == nil {
		return Undefined
	}

	v1, ok := o.Members[idxA]

	if !ok {
		return Undefined
	}

	return v1
}

func (o *Seq) SetMember(idxA string, valueA Object) error {
	if o.Members == nil {
		o.Members = map[string]Object{}
	}

	if IsUndefInternal(valueA) {
		delete(o.Members, idxA)
		return nil
	}

	o.Members[idxA] = valueA

	// return fmt.Errorf("unsupported action(set member)")
	return nil
}

func (*Seq) CanIterate() bool { return false }

func (o *Seq) Iterate() Iterator {
	return nil
}

func (o *Seq) IndexSet(index, value Object) error {
	idxT, ok := index.(String)

	if ok {
		strT := idxT.String()
		if strT == "value" {
			o.Value.Reset(int(ToIntObject(value)))
			return nil
		}

		return o.SetMember(strT, value)
	}

	return ErrNotIndexAssignable
}

func (o *Seq) IndexGet(index Object) (Object, error) {
	switch v := index.(type) {
	case String:
		strT := v.String()

		if strT == "value" {
			// return builtinAnyFunc(Call{Args: []Object{o}})
			return ToIntObject(o.Value), nil
		}

		rs := o.GetMember(strT)

		if !IsUndefInternal(rs) {
			return rs, nil
		}

		// return nil, ErrIndexOutOfBounds
		return GetObjectMethodFunc(o, strT)
	}

	return nil, ErrNotIndexable
}

func (o *Seq) Equal(right Object) bool {
	if v, ok := right.(*Seq); ok {
		return v == o
	}

	return false
}

func (o *Seq) IsFalsy() bool { return o.Value == nil }

func (o *Seq) CanCall() bool { return false }

func (o *Seq) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

func (o *Seq) BinaryOp(tok token.Token, right Object) (Object, error) {
	return nil, NewOperandTypeError(
		tok.String(),
		o.TypeName(),
		right.TypeName())
}

func NewSeq(argsA ...Object) *Seq {
	if len(argsA) > 0 {
		arg1 := argsA[0].String()

		if arg1 == "-global" {
			return &Seq{Value: tk.AutoSeq}
		}
	}

	return &Seq{Value: tk.NewSeq()}
}

// Mutex object is used for thread-safe actions
// note that the members set/get operations are not thread-safe
type Mutex struct {
	ObjectImpl
	Value *(sync.RWMutex)

	Members map[string]Object `json:"-"`
	// Methods map[string]*Function
}

var _ Object = NewMutex()

// var _ ValueSetter = NewMutex()

func (*Mutex) TypeCode() int {
	return 317
}

// TypeName implements Object interface.
func (*Mutex) TypeName() string {
	return "mutex"
}

func (o *Mutex) String() string {
	return fmt.Sprintf("%v", o.Value)
}

// func (o *Mutex) SetValue(valueA Object) error {
// 	// o.Value.Reset(int(ToIntObject(valueA)))

// 	return NewCommonError("unsupported action(set value)")
// }

func (o *Mutex) HasMemeber() bool {
	return true
}

func (o *Mutex) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return builtinAnyFunc(Call{Args: []Object{o}})
	case "toStr":
		return ToStringObject(o), nil
	}

	return CallObjectMethodFunc(o, nameA, argsA...)
}

func (o *Mutex) GetValue() Object {
	return Undefined
}

func (o *Mutex) GetMember(idxA string) Object {
	if o.Members == nil {
		return Undefined
	}

	v1, ok := o.Members[idxA]

	if !ok {
		return Undefined
	}

	return v1
}

func (o *Mutex) SetMember(idxA string, valueA Object) error {
	if o.Members == nil {
		o.Members = map[string]Object{}
	}

	if IsUndefInternal(valueA) {
		delete(o.Members, idxA)
		return nil
	}

	o.Members[idxA] = valueA

	// return fmt.Errorf("unsupported action(set member)")
	return nil
}

func (*Mutex) CanIterate() bool { return false }

func (o *Mutex) Iterate() Iterator {
	return nil
}

func (o *Mutex) IndexSet(index, value Object) error {
	idxT, ok := index.(String)

	if ok {
		strT := idxT.String()
		// if strT == "value" {
		// 	o.Value.Reset(int(ToIntObject(value)))
		// 	return nil
		// }

		return o.SetMember(strT, value)
	}

	return ErrNotIndexAssignable
}

func (o *Mutex) IndexGet(index Object) (Object, error) {
	switch v := index.(type) {
	case String:
		strT := v.String()

		if strT == "value" {
			return builtinAnyFunc(Call{Args: []Object{o}})
			// return ToIntObject(o.Value), nil
		}

		switch strT {
		case "toStr":
			return &Function{
				Name: "toStr",
				Value: func(args ...Object) (Object, error) {
					return ToStringObject(fmt.Sprintf("%v", o.Value)), nil
				},
			}, nil
		case "lock":
			return &Function{
				Name: "lock",
				Value: func(args ...Object) (Object, error) {
					o.Value.Lock()
					return Undefined, nil
				},
			}, nil
		case "unlock":
			return &Function{
				Name: "unlock",
				Value: func(args ...Object) (Object, error) {
					o.Value.Unlock()
					return Undefined, nil
				},
			}, nil
		case "rLock":
			return &Function{
				Name: "rLock",
				Value: func(args ...Object) (Object, error) {
					o.Value.RLock()

					return Undefined, nil
				},
			}, nil
		case "rUnlock":
			return &Function{
				Name: "rUnlock",
				Value: func(args ...Object) (Object, error) {

					o.Value.RUnlock()

					return Undefined, nil
				},
			}, nil
		case "tryLock":
			return &Function{
				Name: "tryLock",
				Value: func(args ...Object) (Object, error) {
					return Bool(o.Value.TryLock()), nil

				},
			}, nil
		case "tryRLock":
			return &Function{
				Name: "tryRLock",
				Value: func(args ...Object) (Object, error) {
					return Bool(o.Value.TryRLock()), nil
				},
			}, nil
		}

		rs := o.GetMember(strT)

		if !IsUndefInternal(rs) {
			return rs, nil
		}

		// return nil, ErrIndexOutOfBounds
		return GetObjectMethodFunc(o, strT)
	}

	return nil, ErrNotIndexable
}

func (o *Mutex) Equal(right Object) bool {
	if v, ok := right.(*Mutex); ok {
		return v == o
	}

	return false
}

func (o *Mutex) IsFalsy() bool { return o.Value == nil }

func (o *Mutex) CanCall() bool { return false }

func (o *Mutex) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

func (o *Mutex) BinaryOp(tok token.Token, right Object) (Object, error) {
	return nil, NewOperandTypeError(
		tok.String(),
		o.TypeName(),
		right.TypeName())
}

func NewMutex(argsA ...Object) *Mutex {
	return &Mutex{Value: &sync.RWMutex{}}
}

// Mux object is used for http handlers
type Mux struct {
	ObjectImpl
	Value *http.ServeMux

	Members map[string]Object `json:"-"`
}

var _ Object = NewMux()

func (*Mux) TypeCode() int {
	return 319
}

// TypeName implements Object interface.
func (*Mux) TypeName() string {
	return "mux"
}

func (o *Mux) String() string {
	return fmt.Sprintf("%v", o.Value)
}

// func (o *Mux) SetValue(valueA Object) error {
// 	// o.Value.Reset(int(ToIntObject(valueA)))

// 	return NewCommonError("unsupported action(set value)")
// }

func (o *Mux) HasMemeber() bool {
	return true
}

func (o *Mux) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return builtinAnyFunc(Call{Args: []Object{o}})
	case "toStr":
		return ToStringObject(o), nil
	}

	return CallObjectMethodFunc(o, nameA, argsA...)
}

func (o *Mux) GetValue() Object {
	return Undefined
}

func (o *Mux) GetMember(idxA string) Object {
	if o.Members == nil {
		return Undefined
	}

	v1, ok := o.Members[idxA]

	if !ok {
		return Undefined
	}

	return v1
}

func (o *Mux) SetMember(idxA string, valueA Object) error {
	if o.Members == nil {
		o.Members = map[string]Object{}
	}

	if IsUndefInternal(valueA) {
		delete(o.Members, idxA)
		return nil
	}

	o.Members[idxA] = valueA

	// return fmt.Errorf("unsupported action(set member)")
	return nil
}

func (*Mux) CanIterate() bool { return false }

func (o *Mux) Iterate() Iterator {
	return nil
}

func (o *Mux) IndexSet(index, value Object) error {
	idxT, ok := index.(String)

	if ok {
		strT := idxT.String()
		// if strT == "value" {
		// 	o.Value.Reset(int(ToIntObject(value)))
		// 	return nil
		// }

		return o.SetMember(strT, value)
	}

	return ErrNotIndexAssignable
}

func (o *Mux) IndexGet(index Object) (Object, error) {
	switch v := index.(type) {
	case String:
		strT := v.String()

		if strT == "value" {
			return builtinAnyFunc(Call{Args: []Object{o}})
			// return ToIntObject(o.Value), nil
		}

		rs := o.GetMember(strT)

		if !IsUndefInternal(rs) {
			return rs, nil
		}

		// return nil, ErrIndexOutOfBounds
		return GetObjectMethodFunc(o, strT)
	}

	return nil, NewCommonError("not indexable: %v", o.TypeName())
}

func (o *Mux) Equal(right Object) bool {
	if v, ok := right.(*Mux); ok {
		return v == o
	}

	return false
}

func (o *Mux) IsFalsy() bool { return o.Value == nil }

func (o *Mux) CanCall() bool { return false }

func (o *Mux) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

func (o *Mux) BinaryOp(tok token.Token, right Object) (Object, error) {
	return nil, NewOperandTypeError(
		tok.String(),
		o.TypeName(),
		right.TypeName())
}

func NewMux(argsA ...Object) *Mux {
	return &Mux{Value: http.NewServeMux()}
}

// HttpReq object is used to represent the http request
type HttpReq struct {
	ObjectImpl
	Value *http.Request

	Members map[string]Object `json:"-"`
}

// var _ Object = NewHttpReq()

func NewHttpReq(c Call) (Object, error) {
	return &HttpReq{Value: nil}, nil
}

func (*HttpReq) TypeCode() int {
	return 321
}

// TypeName implements Object interface.
func (*HttpReq) TypeName() string {
	return "httpReq"
}

func (o *HttpReq) String() string {
	return fmt.Sprintf("%v", o.Value)
}

// func (o *Mux) SetValue(valueA Object) error {
// 	// o.Value.Reset(int(ToIntObject(valueA)))

// 	return NewCommonError("unsupported action(set value)")
// }

func (o *HttpReq) HasMemeber() bool {
	return true
}

func (o *HttpReq) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return builtinAnyFunc(Call{Args: []Object{o}})
	case "toStr":
		return ToStringObject(o), nil
	}

	return CallObjectMethodFunc(o, nameA, argsA...)
}

func (o *HttpReq) GetValue() Object {
	return Undefined
}

func (o *HttpReq) GetMember(idxA string) Object {
	if o.Members == nil {
		return Undefined
	}

	v1, ok := o.Members[idxA]

	if !ok {
		return Undefined
	}

	return v1
}

func (o *HttpReq) SetMember(idxA string, valueA Object) error {
	if o.Members == nil {
		o.Members = map[string]Object{}
	}

	if IsUndefInternal(valueA) {
		delete(o.Members, idxA)
		return nil
	}

	o.Members[idxA] = valueA

	// return fmt.Errorf("unsupported action(set member)")
	return nil
}

func (*HttpReq) CanIterate() bool { return false }

func (o *HttpReq) Iterate() Iterator {
	return nil
}

func (o *HttpReq) IndexSet(index, value Object) error {
	idxT, ok := index.(String)

	if ok {
		strT := idxT.String()
		// if strT == "value" {
		// 	o.Value.Reset(int(ToIntObject(value)))
		// 	return nil
		// }

		return o.SetMember(strT, value)
	}

	return ErrNotIndexAssignable
}

func (o *HttpReq) IndexGet(index Object) (Object, error) {
	switch v := index.(type) {
	case String:
		strT := v.String()

		if strT == "value" {
			return builtinAnyFunc(Call{Args: []Object{o}})
			// return ToIntObject(o.Value), nil
		}

		rs := o.GetMember(strT)

		if !IsUndefInternal(rs) {
			return rs, nil
		}

		// return nil, ErrIndexOutOfBounds
		var errT error

		rs, errT = GetObjectMethodFunc(o, strT)

		if errT == nil && !IsUndefInternal(rs) {
			return rs, nil
		}

		reqT := o.Value

		rs1 := tk.ReflectGetMember(reqT, strT)

		if !tk.IsError(rs1) {
			return ConvertToObject(rs1), nil
		}

		rs2 := tk.ReflectGetMethod(reqT, strT)

		if !tk.IsError(rs2) {
			rs2 := &Function{
				Name: strT,
				ValueEx: func(c Call) (Object, error) {
					rs3 := tk.ReflectCallMethodCompact(o.Value, strT, ObjectsToI(c.GetArgs())...)

					return ConvertToObject(rs3), nil
				},
			}

			return rs2, nil
		}

		return NewCommonError("%v", rs2), nil // rs1.(error)
	}

	return nil, NewCommonError("not indexable: %v", o.TypeName())
}

func (o *HttpReq) Equal(right Object) bool {
	if v, ok := right.(*HttpReq); ok {
		return v == o
	}

	return false
}

func (o *HttpReq) IsFalsy() bool { return o.Value == nil }

func (o *HttpReq) CanCall() bool { return false }

func (o *HttpReq) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

func (o *HttpReq) BinaryOp(tok token.Token, right Object) (Object, error) {
	return nil, NewOperandTypeError(
		tok.String(),
		o.TypeName(),
		right.TypeName())
}

// HttpResp object is used to represent the http response
type HttpResp struct {
	ObjectImpl
	Value http.ResponseWriter

	Members map[string]Object `json:"-"`
}

// var _ Object = NewHttpReq()
var _ io.Writer = &HttpResp{Value: nil}

func (*HttpResp) TypeCode() int {
	return 323
}

// TypeName implements Object interface.
func (*HttpResp) TypeName() string {
	return "httpResp"
}

func (o *HttpResp) String() string {
	return fmt.Sprintf("%v", o.Value)
}

// comply to io.Writer
func (o *HttpResp) Write(p []byte) (n int, err error) {
	nv, ok := o.Value.(io.Writer)

	if !ok {
		return 0, fmt.Errorf("unable to write")
	}

	return nv.Write(p)
}

// func (o *HttpResp) SetValue(valueA Object) error {
// 	// o.Value.Reset(int(ToIntObject(valueA)))

// 	return NewCommonError("unsupported action(set value)")
// }

func (o *HttpResp) HasMemeber() bool {
	return true
}

func (o *HttpResp) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return builtinAnyFunc(Call{Args: []Object{o}})
	case "toStr":
		return ToStringObject(o), nil
	}

	return CallObjectMethodFunc(o, nameA, argsA...)
}

func (o *HttpResp) GetValue() Object {
	return Undefined
}

func (o *HttpResp) GetMember(idxA string) Object {
	if o.Members == nil {
		return Undefined
	}

	v1, ok := o.Members[idxA]

	if !ok {
		return Undefined
	}

	return v1
}

func (o *HttpResp) SetMember(idxA string, valueA Object) error {
	if o.Members == nil {
		o.Members = map[string]Object{}
	}

	if IsUndefInternal(valueA) {
		delete(o.Members, idxA)
		return nil
	}

	o.Members[idxA] = valueA

	// return fmt.Errorf("unsupported action(set member)")
	return nil
}

func (*HttpResp) CanIterate() bool { return false }

func (o *HttpResp) Iterate() Iterator {
	return nil
}

func (o *HttpResp) IndexSet(index, value Object) error {
	idxT, ok := index.(String)

	if ok {
		strT := idxT.String()
		// if strT == "value" {
		// 	o.Value.Reset(int(ToIntObject(value)))
		// 	return nil
		// }

		return o.SetMember(strT, value)
	}

	return ErrNotIndexAssignable
}

func (o *HttpResp) IndexGet(index Object) (Object, error) {
	switch v := index.(type) {
	case String:
		strT := v.String()

		if strT == "value" {
			return builtinAnyFunc(Call{Args: []Object{o}})
			// return ToIntObject(o.Value), nil
		}

		rs := o.GetMember(strT)

		if !IsUndefInternal(rs) {
			return rs, nil
		}

		// return nil, ErrIndexOutOfBounds
		return GetObjectMethodFunc(o, strT)
	}

	return nil, NewCommonError("not indexable: %v", o.TypeName())
}

func (o *HttpResp) Equal(right Object) bool {
	if v, ok := right.(*HttpResp); ok {
		return v == o
	}

	return false
}

func (o *HttpResp) IsFalsy() bool { return o.Value == nil }

func (o *HttpResp) CanCall() bool { return false }

func (o *HttpResp) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

func (o *HttpResp) BinaryOp(tok token.Token, right Object) (Object, error) {
	return nil, NewOperandTypeError(
		tok.String(),
		o.TypeName(),
		right.TypeName())
}

// HttpHandler object is used to represent the http response
type HttpHandler struct {
	// ObjectImpl
	Value func(http.ResponseWriter, *http.Request)

	Members map[string]Object `json:"-"`
}

// var _ Object = NewHttpReq()

func (*HttpHandler) TypeCode() int {
	return 325
}

// TypeName implements Object interface.
func (*HttpHandler) TypeName() string {
	return "httpHandler"
}

func (o *HttpHandler) String() string {
	return fmt.Sprintf("(httpHandler)%v", &o.Value)
}

// func (o *HttpResp) SetValue(valueA Object) error {
// 	// o.Value.Reset(int(ToIntObject(valueA)))

// 	return NewCommonError("unsupported action(set value)")
// }

func (o *HttpHandler) HasMemeber() bool {
	return true
}

func (o *HttpHandler) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return builtinAnyFunc(Call{Args: []Object{o}})
	case "toStr":
		return ToStringObject(o), nil
	}

	return CallObjectMethodFunc(o, nameA, argsA...)
}

func (o *HttpHandler) GetValue() Object {
	return Undefined
}

func (o *HttpHandler) GetMember(idxA string) Object {
	if o.Members == nil {
		return Undefined
	}

	v1, ok := o.Members[idxA]

	if !ok {
		return Undefined
	}

	return v1
}

func (o *HttpHandler) SetMember(idxA string, valueA Object) error {
	if o.Members == nil {
		o.Members = map[string]Object{}
	}

	if IsUndefInternal(valueA) {
		delete(o.Members, idxA)
		return nil
	}

	o.Members[idxA] = valueA

	// return fmt.Errorf("unsupported action(set member)")
	return nil
}

func (*HttpHandler) CanIterate() bool { return false }

func (o *HttpHandler) Iterate() Iterator {
	return nil
}

func (o *HttpHandler) IndexSet(index, value Object) error {
	idxT, ok := index.(String)

	if ok {
		strT := idxT.String()
		// if strT == "value" {
		// 	o.Value.Reset(int(ToIntObject(value)))
		// 	return nil
		// }

		return o.SetMember(strT, value)
	}

	return ErrNotIndexAssignable
}

func (o *HttpHandler) IndexGet(index Object) (Object, error) {
	switch v := index.(type) {
	case String:
		strT := v.String()

		if strT == "value" {
			return builtinAnyFunc(Call{Args: []Object{o}})
			// return ToIntObject(o.Value), nil
		}

		rs := o.GetMember(strT)

		if !IsUndefInternal(rs) {
			return rs, nil
		}

		// return nil, ErrIndexOutOfBounds
		return GetObjectMethodFunc(o, strT)
	}

	return nil, NewCommonError("not indexable: %v", o.TypeName())
}

func (o *HttpHandler) Equal(right Object) bool {
	if v, ok := right.(*HttpHandler); ok {
		return v == o
	}

	return false
}

func (o *HttpHandler) IsFalsy() bool { return o.Value == nil }

func (o *HttpHandler) CanCall() bool { return false }

func (o *HttpHandler) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

func (o *HttpHandler) BinaryOp(tok token.Token, right Object) (Object, error) {
	return nil, NewOperandTypeError(
		tok.String(),
		o.TypeName(),
		right.TypeName())
}

func NewHttpHandler(c Call) (Object, error) {
	argsA := c.GetArgs()

	if len(argsA) > 1 {
		arg0 := argsA[0].String()

		if arg0 == "static" {
			rs := tk.NewStaticWebHandler(argsA[1].String())

			if tk.IsError(rs) {
				return NewCommonErrorWithPos(c, "failed to create static web handler:", rs), nil
			}

			return &HttpHandler{Value: rs.(func(http.ResponseWriter, *http.Request))}, nil

		}

		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	return &HttpHandler{Value: nil}, nil
	// argsA := c.GetArgs()

	// if len(argsA) < 1 {
	// 	return NewCommonErrorWithPos(c, "not enough parameters"), nil
	// }

	// arg0 := argsA[0].String()

	// switch arg0 {
	// case "static":
	// 	if len(argsA) < 2 {
	// 		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	// 	}

	// 	rs := tk.NewStaticWebHandler(argsA[1].String())

	// 	if tk.IsError(rs) {
	// 		return NewCommonErrorWithPos(c, "failed to create static web handler:", rs), nil
	// 	}

	// 	return &HttpHandler{Value: rs.(func(http.ResponseWriter, *http.Request))}, nil
	// case "code":
	// 	if len(argsA) < 2 {
	// 		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	// 	}

	// 	fnObjT := argsA[1]

	// 	fnT, ok := fnObjT.(*CompiledFunction)

	// 	if ok {
	// 		handlerT := func(w http.ResponseWriter, req *http.Request) {
	// 			retT, errT := NewInvoker(c.VM(), fnT).Invoke(ConvertToObject(req), ConvertToObject(w))

	// 			if errT != nil {
	// 				tk.Pl("failed to invoke handler: %v", errT)
	// 				return
	// 			}

	// 			rs := retT.String()

	// 			if rs != "TX_END_RESPONSE_XT" {
	// 				w.Write([]byte(rs))
	// 			}

	// 		}

	// 		return &HttpHandler{Value: handlerT}, nil
	// 	}

	// 	fn2T, ok := fnObjT.(*CharCode)

	// 	if ok {
	// 		lenT := len(argsA)

	// 		var additionsA []Object = make([]Object, 0, lenT-2)

	// 		for i := 2; i < lenT; i++ {
	// 			additionsA = append(additionsA, c.Get(i))
	// 		}

	// 		// tk.Plv("additionsA", additionsA)

	// 		handlerT := func(w http.ResponseWriter, req *http.Request) {
	// 			var globalsA map[string]interface{} = map[string]interface{}{
	// 				"requestG":  req,
	// 				"responseG": w,
	// 			}

	// 			envT := NewBaseEnv(globalsA) // Map{}

	// 			// if lenT > 1 {
	// 			// 	additionsA = argsA[1:]
	// 			// }
	// 			retT, errT := NewVM(fn2T.Value).Run(envT, additionsA...)

	// 			if errT != nil {
	// 				tk.Pl("failed to run handler: %v", errT)
	// 				return
	// 			}

	// 			rs := retT.String()

	// 			if rs != "TX_END_RESPONSE_XT" {
	// 				w.Write([]byte(rs))
	// 			}

	// 		}

	// 		return &HttpHandler{Value: handlerT}, nil
	// 	}

	// 	return NewCommonErrorWithPos(c, "unsupported handler type: %T", fnObjT), nil
	// }

	// return NewCommonErrorWithPos(c, "unsupported type: %T", arg0), nil
}

// Reader object is used for represent io.Reader type value
type Reader struct {
	ObjectImpl
	Value     io.Reader
	SizeM     int // may be not set
	CloseDele tk.QuickVarDelegate

	// CloseHandler io.Closer

	Members map[string]Object `json:"-"`
}

var _ io.ReadCloser = &Reader{Value: nil}

func (*Reader) TypeCode() int {
	return 331
}

// TypeName implements Object interface.
func (*Reader) TypeName() string {
	return "reader"
}

func (o *Reader) String() string {
	return fmt.Sprintf("%v", o.Value)
}

func (o *Reader) Size() int {
	return o.SizeM
}

func (o *Reader) SetSize(sizeA int) {
	o.SizeM = sizeA
}

// func (o *Reader) SetValue(valueA Object) error {
// 	// o.Value.Reset(int(ToIntObject(valueA)))

// 	return NewCommonError("unsupported action(set value)")
// }

// comply to io.Closer
func (o *Reader) Close() error {
	//	fmt.Printf("\n******* %#v ********\n", o)

	if o.CloseDele != nil {
		o.CloseDele() // rs :=
		//		fmt.Printf("\n -- close 1\n", rs)
	}

	methodT := o.GetMember("close")

	if !IsUndefInternal(methodT) {
		f1, ok := methodT.(*Function)

		if ok {
			rs, err := (*f1).Call()

			if err != nil {
				return err
			}

			if isErrX(rs) {
				return rs.(*Error).Unwrap()
			}

			//			fmt.Printf("\n -- close 2: %v\n", rs)
			return nil
		}

		return fmt.Errorf("unable to close")
	}

	nv, ok := o.Value.(io.Closer)

	if ok {
		errT := nv.Close()
		// errT := o.CloseHandler.Close()

		if errT != nil {
			return fmt.Errorf("failed to close: %v", errT)
		}

		//		fmt.Printf("\n -- close 3: %v\n", errT)
		return nil
	}

	// if o.CloseHandler == nil {
	// 	return fmt.Errorf("no close handler")
	// }

	return fmt.Errorf("unable to close")
}

// comply to io.Reader
func (o *Reader) Read(p []byte) (n int, err error) {
	nv, ok := o.Value.(io.Reader)

	if !ok {
		return 0, fmt.Errorf("unable to close")
	}

	return nv.Read(p)
}

//func (o *Reader) CallMethod(nameA string, argsA ...Object) (Object, error) {
//	switch nameA {
//	case "value":
//		return builtinAnyFunc(Call{Args: []Object{o}})
//	case "toStr":
//		return ToStringObject(o), nil
//	}
//
//	return CallObjectMethodFunc(o, nameA, argsA...)
//}

func (o *Reader) CallMethod(nameA string, argsA ...Object) (Object, error) {
	return o.CallName(nameA, Call{Args: argsA})
	//	switch nameA {
	//	case "value":
	//		return o, nil
	//	case "toStr":
	//		return ToStringObject(o), nil
	//	}
	//
	//	return CallObjectMethodFunc(o, nameA, argsA...)
}

func (o *Reader) CallName(nameA string, c Call) (Object, error) {
	switch nameA {
	case "value":
		return builtinAnyFunc(Call{Args: []Object{o}})
	case "toStr":
		return ToStringObject(o), nil
	case "close":
		rs := o.Close()

		return ConvertToObject(rs), nil
	}

	args := c.GetArgs()

	rs1, errT := CallObjectMethodFunc(o, nameA, args...)

	//	if errT != nil || tk.IsError(rs1) {
	//		rs3 := tk.ReflectCallMethodCompact(o.Value, nameA, ObjectsToI(args)...)
	//		return ConvertToObject(rs3), nil
	//	}

	return rs1, errT

	//	return Undefined, NewCommonErrorWithPos(c, "method not found: %v", nameA)
}

func (o *Reader) HasMemeber() bool {
	return true
}

func (o *Reader) GetValue() Object {
	return Undefined
}

func (o *Reader) GetMember(idxA string) Object {
	if o.Members == nil {
		return Undefined
	}

	v1, ok := o.Members[idxA]

	if !ok {
		return Undefined
	}

	return v1
}

func (o *Reader) SetMember(idxA string, valueA Object) error {
	if o.Members == nil {
		o.Members = map[string]Object{}
	}

	if IsUndefInternal(valueA) {
		delete(o.Members, idxA)
		return nil
	}

	o.Members[idxA] = valueA

	// return fmt.Errorf("unsupported action(set member)")
	return nil
}

func (*Reader) CanIterate() bool { return false }

func (o *Reader) Iterate() Iterator {
	return nil
}

func (o *Reader) IndexSet(index, value Object) error {
	idxT, ok := index.(String)

	if ok {
		strT := idxT.String()
		// if strT == "value" {
		// 	o.Value.Reset(int(ToIntObject(value)))
		// 	return nil
		// }

		return o.SetMember(strT, value)
	}

	return ErrNotIndexAssignable
}

func (o *Reader) IndexGet(index Object) (Object, error) {
	switch v := index.(type) {
	case String:
		strT := v.String()

		if strT == "close" {
			errT := o.Close()

			if errT != nil {
				return NewCommonError("failed to close reader: %v", errT), nil
			}

			return Undefined, nil
		} else if strT == "value" {
			return builtinAnyFunc(Call{Args: []Object{o}})

		}

		rs := o.GetMember(strT)

		if !IsUndefInternal(rs) {
			return rs, nil
		}

		// return nil, ErrIndexOutOfBounds
		return GetObjectMethodFunc(o, strT)
	}

	return nil, NewCommonError("not indexable: %v", o.TypeName())
}

func (o *Reader) Equal(right Object) bool {
	if v, ok := right.(*Reader); ok {
		return v == o
	}

	return false
}

func (o *Reader) IsFalsy() bool { return o.Value == nil }

func (o *Reader) CanCall() bool { return false }

func (o *Reader) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

func (o *Reader) BinaryOp(tok token.Token, right Object) (Object, error) {
	return nil, NewOperandTypeError(
		tok.String(),
		o.TypeName(),
		right.TypeName())
}

func NewReader(c Call) (Object, error) {
	argsA := c.GetArgs()

	if len(argsA) < 1 {
		return NewCommonError("not enough parameters"), nil
		// return Undefined, nil
	}

	optsT := ObjectsToS(argsA[1:])

	r1, ok := argsA[0].(*Reader)

	if ok {
		return &Reader{Value: r1.Value}, nil
	}

	b1, ok := argsA[0].(Bytes)

	if ok {
		return &Reader{Value: bytes.NewReader(b1)}, nil
	}

	s1, ok := argsA[0].(String)

	if ok {
		if tk.IfSwitchExists(optsT, "-file") {
			f, err := os.Open(s1.String())

			if err != nil {
				return NewCommonErrorWithPos(c, "failed to open file: %v", err), nil
			}

			// defer f.Close()

			rs := &Reader{Value: f}

			// rs.CloseHandler = f

			// funcT := &Function{
			// 	Name: "close",
			// 	ValueEx: func(c Call) (Object, error) {

			// 		errT := rs.Close()

			// 		if errT != nil {
			// 			return NewCommonErrorWithPos(c, "failed to close reader: %v", errT), nil
			// 		}

			// 		return Undefined, nil
			// 	},
			// }

			// rs.SetMember("close", funcT)

			return rs, nil
		}

		return &Reader{Value: strings.NewReader(s1.String())}, nil
	}

	return Undefined, nil
}

// Writer object is used for represent io.Writer type value
type Writer struct {
	ObjectImpl
	Value io.Writer

	Members map[string]Object `json:"-"`
}

var _ io.WriteCloser = &Writer{Value: nil}

func (*Writer) TypeCode() int {
	return 333
}

// TypeName implements Object interface.
func (*Writer) TypeName() string {
	return "writer"
}

func (o *Writer) String() string {
	return fmt.Sprintf("%v", o.Value)
}

// comply to io.Closer
func (o *Writer) Close() error {
	methodT := o.GetMember("close")

	if !IsUndefInternal(methodT) {
		f1, ok := methodT.(*Function)

		if ok {
			rs, err := (*f1).Call()

			if err != nil {
				return err
			}

			if isErrX(rs) {
				return rs.(*Error).Unwrap()
			}

			return nil
		}

		return fmt.Errorf("unable to close")
	}

	nv, ok := o.Value.(io.Closer)

	if ok {
		errT := nv.Close()

		if errT != nil {
			return fmt.Errorf("failed to close: %v", errT)
		}

		return nil
	}

	return fmt.Errorf("unable to close")

}

// comply to io.Writer
func (o *Writer) Write(p []byte) (n int, err error) {
	nv, ok := o.Value.(io.Writer)

	if !ok {
		return 0, fmt.Errorf("unable to write")
	}

	return nv.Write(p)
}

func (o *Writer) HasMemeber() bool {
	return true
}

//func (o *Writer) CallMethod(nameA string, argsA ...Object) (Object, error) {
//	switch nameA {
//	case "value":
//		return builtinAnyFunc(Call{Args: []Object{o}})
//	case "toStr":
//		return ToStringObject(o), nil
//	}
//
//	return CallObjectMethodFunc(o, nameA, argsA...)
//}

func (o *Writer) CallMethod(nameA string, argsA ...Object) (Object, error) {
	return o.CallName(nameA, Call{Args: argsA})
}

func (o *Writer) CallName(nameA string, c Call) (Object, error) {
	switch nameA {
	case "value":
		return builtinAnyFunc(Call{Args: []Object{o}})
	case "toStr":
		return ToStringObject(o), nil
	case "close":
		rs := o.Close()

		return ConvertToObject(rs), nil
	}

	args := c.GetArgs()

	rs1, errT := CallObjectMethodFunc(o, nameA, args...)

	//	if errT != nil || tk.IsError(rs1) {
	//		rs3 := tk.ReflectCallMethodCompact(o.Value, nameA, ObjectsToI(args)...)
	//		return ConvertToObject(rs3), nil
	//	}

	return rs1, errT

	//	return Undefined, NewCommonErrorWithPos(c, "method not found: %v", nameA)
}

func (o *Writer) GetValue() Object {
	return Undefined
}

func (o *Writer) GetMember(idxA string) Object {
	if o.Members == nil {
		return Undefined
	}

	v1, ok := o.Members[idxA]

	if !ok {
		return Undefined
	}

	return v1
}

func (o *Writer) SetMember(idxA string, valueA Object) error {
	if o.Members == nil {
		o.Members = map[string]Object{}
	}

	if IsUndefInternal(valueA) {
		delete(o.Members, idxA)
		return nil
	}

	o.Members[idxA] = valueA

	// return fmt.Errorf("unsupported action(set member)")
	return nil
}

func (*Writer) CanIterate() bool { return false }

func (o *Writer) Iterate() Iterator {
	return nil
}

func (o *Writer) IndexSet(index, value Object) error {
	idxT, ok := index.(String)

	if ok {
		strT := idxT.String()
		// if strT == "value" {
		// 	o.Value.Reset(int(ToIntObject(value)))
		// 	return nil
		// }

		return o.SetMember(strT, value)
	}

	return ErrNotIndexAssignable
}

func (o *Writer) IndexGet(index Object) (Object, error) {
	switch v := index.(type) {
	case String:
		strT := v.String()

		if strT == "close" {
			errT := o.Close()

			if errT != nil {
				return NewCommonError("failed to close reader: %v", errT), nil
			}

			return Undefined, nil
		} else if strT == "value" {
			return builtinAnyFunc(Call{Args: []Object{o}})

		}

		rs := o.GetMember(strT)

		if !IsUndefInternal(rs) {
			return rs, nil
		}

		// return nil, ErrIndexOutOfBounds
		return GetObjectMethodFunc(o, strT)
	}

	return nil, NewCommonError("not indexable: %v", o.TypeName())
}

func (o *Writer) Equal(right Object) bool {
	if v, ok := right.(*Writer); ok {
		return v == o
	}

	return false
}

func (o *Writer) IsFalsy() bool { return o.Value == nil }

func (o *Writer) CanCall() bool { return false }

func (o *Writer) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

func (o *Writer) BinaryOp(tok token.Token, right Object) (Object, error) {
	return nil, NewOperandTypeError(
		tok.String(),
		o.TypeName(),
		right.TypeName())
}

func NewWriter(c Call) (Object, error) {
	argsA := c.GetArgs()

	if len(argsA) < 1 {
		return NewCommonError("not enough parameters"), nil
	}

	optsT := ObjectsToS(argsA[1:])

	r1, ok := argsA[0].(*Writer)

	if ok {
		return &Writer{Value: r1.Value}, nil
	}

	nv2, ok := argsA[0].(*BytesBuffer)

	if ok {
		return &Writer{Value: nv2.Value}, nil
	}

	nv3, ok := argsA[0].(*StringBuilder)

	if ok {
		return &Writer{Value: nv3.Value}, nil
	}

	nv4, ok := argsA[0].(*File)

	if ok {
		return &Writer{Value: nv4.Value}, nil
	}

	nv1, ok := argsA[0].(io.Writer)

	if ok {
		return &Writer{Value: nv1}, nil
	}

	b1, ok := argsA[0].(Bytes)

	if ok {
		return &Writer{Value: bytes.NewBuffer(b1)}, nil
	}

	s1, ok := argsA[0].(String)

	if ok {
		if tk.IfSwitchExists(optsT, "-file") {
			flagT := os.O_RDWR

			if tk.IfSwitchExists(optsT, "-create") {
				flagT = flagT | os.O_CREATE
			}

			if tk.IfSwitchExists(optsT, "-append") {
				flagT = flagT | os.O_APPEND
			}

			if tk.IfSwitchExists(optsT, "-truncate") {
				flagT = flagT | os.O_TRUNC
			}

			f, err := os.OpenFile(s1.String(), flagT, os.FileMode(tk.OctetToInt(tk.GetSwitch(optsT, "-perm=", "0777"), 0777)))

			if err != nil {
				return NewCommonErrorWithPos(c, "failed to open file: %v", err), nil
			}

			rs := &Writer{Value: f}

			return rs, nil
		}
	}

	// var sb strings.Builder

	// // sb.WriteString(s1.Value)

	// return &Writer{Value: &sb}, nil
	return NewCommonError("unsupported type: %T", argsA[0]), nil
}

// File object is used for represent os.File type value
type File struct {
	ObjectImpl
	Value *os.File

	Members map[string]Object `json:"-"`
}

func (*File) TypeCode() int {
	return 401
}

// TypeName implements Object interface.
func (*File) TypeName() string {
	return "file"
}

func (o *File) String() string {
	return fmt.Sprintf("%v", o.Value)
}

// comply to io.Closer
func (o *File) Close() error {
	errT := o.Value.Close()

	if errT != nil {
		return fmt.Errorf("failed to close: %v", errT)
	}

	return nil
}

// comply to io.Reader
func (o *File) Read(p []byte) (n int, err error) {
	return o.Value.Read(p)
}

// comply to io.Writer
func (o *File) Writer(p []byte) (n int, err error) {
	return o.Value.Write(p)
}

func (o *File) HasMemeber() bool {
	return true
}

var methodTableForFile = map[string]func(*File, *Call) (Object, error){
	"getInfo": func(o *File, c *Call) (Object, error) {
		filePathT := ""

		filePathMemberT := o.GetMember("Path")
		filePathT = strings.TrimSpace(filePathMemberT.String())

		if IsUndefInternal(filePathMemberT) || filePathT == "" {
			fi, errT := o.Value.Stat()
			if errT != nil && !os.IsExist(errT) {
				return NewCommonErrorWithPos(*c, "%v", errT), nil
			}

			fileNameT := fi.Name()
			absPathT := ""

			mapT := Map{"Path": ToStringObject(filePathT), "Abs": ToStringObject(absPathT), "Name": ToStringObject(fileNameT), "Ext": ToStringObject(filepath.Ext(fileNameT)), "Size": ToStringObject(tk.Int64ToStr(fi.Size())), "IsDir": ToStringObject(tk.BoolToStr(fi.IsDir())), "Time": ToStringObject(tk.FormatTime(fi.ModTime(), tk.TimeFormatCompact)), "Mode": ToStringObject(fmt.Sprintf("%v", fi.Mode()))}

			return mapT, nil
		}

		fi, errT := os.Stat(filePathT)
		if errT != nil && !os.IsExist(errT) {
			return NewCommonErrorWithPos(*c, "%v", errT), nil
		}

		absPathT, errT := filepath.Abs(filePathT)
		if errT != nil {
			return NewCommonErrorWithPos(*c, "%v", errT), nil
		}

		fileNameT := filepath.Base(filePathT)

		mapT := Map{"Path": ToStringObject(filePathT), "Abs": ToStringObject(absPathT), "Name": ToStringObject(fileNameT), "Ext": ToStringObject(filepath.Ext(fileNameT)), "Size": ToStringObject(tk.Int64ToStr(fi.Size())), "IsDir": ToStringObject(tk.BoolToStr(fi.IsDir())), "Time": ToStringObject(tk.FormatTime(fi.ModTime(), tk.TimeFormatCompact)), "Mode": ToStringObject(fmt.Sprintf("%v", fi.Mode()))}

		return mapT, nil
		// fi, errT := o.Value.Stat()

		// if errT != nil {
		// 	return NewCommonErrorWithPos(*c, "%v", errT), nil
		// }

		// fileNameT := fi.Name()
		// filePathT := ""
		// absPathT := ""

		// mapT := Map{"Path": ToStringObject(filePathT), "Abs": ToStringObject(absPathT), "Name": ToStringObject(fileNameT), "Ext": ToStringObject(filepath.Ext(fileNameT)), "Size": ToStringObject(tk.Int64ToStr(fi.Size())), "IsDir": ToStringObject(tk.BoolToStr(fi.IsDir())), "Time": ToStringObject(tk.FormatTime(fi.ModTime(), tk.TimeFormatCompact)), "Mode": ToStringObject(fmt.Sprintf("%v", fi.Mode()))}

		// return mapT, nil
	},
}

func (o *File) CallName(name string, c Call) (Object, error) {
	// tk.Pl("File CallName: %v", name)

	fn, ok := methodTableForFile[name]
	if !ok {
		if !tk.ReflectHasMethod(o.Value, name) {
			return NewCommonErrorWithPos(c, "method(%v) not found for type: %T", name, o), nil
		}

		rs3 := tk.ReflectCallMethodCompact(o.Value, name, ObjectsToI(c.GetArgs())...)

		return ConvertToObject(rs3), nil

		// return Undefined, ErrInvalidIndex.NewError(name)
	}

	return fn(o, &c)
}

func (o *File) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return builtinAnyFunc(Call{Args: []Object{o}})
	case "toStr":
		return ToStringObject(o), nil
	}

	return CallObjectMethodFunc(o, nameA, argsA...)
}

func (o *File) GetValue() Object {
	return Undefined
}

func (o *File) GetMember(idxA string) Object {
	if o.Members == nil {
		return Undefined
	}

	v1, ok := o.Members[idxA]

	if !ok {
		return Undefined
	}

	return v1
}

func (o *File) SetMember(idxA string, valueA Object) error {
	if o.Members == nil {
		o.Members = map[string]Object{}
	}

	if IsUndefInternal(valueA) {
		delete(o.Members, idxA)
		return nil
	}

	o.Members[idxA] = valueA

	// return fmt.Errorf("unsupported action(set member)")
	return nil
}

func (*File) CanIterate() bool { return false }

func (o *File) Iterate() Iterator {
	return nil
}

func (o *File) IndexSet(index, value Object) error {
	idxT, ok := index.(String)

	if ok {
		strT := idxT.String()
		// if strT == "value" {
		// 	o.Value.Reset(int(ToIntObject(value)))
		// 	return nil
		// }

		return o.SetMember(strT, value)
	}

	return ErrNotIndexAssignable
}

func (o *File) IndexGet(index Object) (Object, error) {
	switch v := index.(type) {
	case String:
		strT := v.String()

		if strT == "close" {
			errT := o.Close()

			if errT != nil {
				return NewCommonError("failed to close reader: %v", errT), nil
			}

			return Undefined, nil
		} else if strT == "value" {
			return builtinAnyFunc(Call{Args: []Object{o}})

		}

		rs := o.GetMember(strT)

		if !IsUndefInternal(rs) {
			return rs, nil
		}

		// return nil, ErrIndexOutOfBounds
		return GetObjectMethodFunc(o, strT)
	}

	return nil, NewCommonError("not indexable: %v", o.TypeName())
}

func (o *File) Equal(right Object) bool {
	if v, ok := right.(*File); ok {
		return v == o
	}

	return false
}

func (o *File) IsFalsy() bool { return o.Value == nil }

func (o *File) CanCall() bool { return false }

func (o *File) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

func (o *File) BinaryOp(tok token.Token, right Object) (Object, error) {
	return nil, NewOperandTypeError(
		tok.String(),
		o.TypeName(),
		right.TypeName())
}

func NewFile(c Call) (Object, error) {
	argsA := c.GetArgs()

	if len(argsA) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	optsT := ObjectsToS(argsA[1:])

	filePathT := argsA[0].String()

	if filePathT == "stdin" {
		return &File{Value: os.Stdin}, nil
	} else if filePathT == "stdout" {
		return &File{Value: os.Stdout}, nil
	} else if filePathT == "stderr" {
		return &File{Value: os.Stderr}, nil
		// } else if filePathT == "memfile" {
		// 	return &File{Value: (*os.File)(tk.NewMemFile(nil))}, nil
	}

	rs := tk.OpenFile(filePathT, optsT...)

	if tk.IsErrX(rs) {
		return NewCommonErrorWithPos(c, "failed to open file: %v", tk.GetErrStrX(rs)), nil
	}

	objT := &File{Value: rs.(*os.File)}

	objT.SetMember("Path", ToStringObject(filePathT))

	return objT, nil

}

// CharCode object is used for represent thent io.Reader type value
type CharCode struct {
	ObjectImpl
	Source string
	Value  *Bytecode

	CompilerOptions *CompilerOptions

	LastError string

	Members map[string]Object `json:"-"`
}

var _ Object = NewCharCode("")

func (*CharCode) TypeCode() int {
	return 191
}

// TypeName implements Object interface.
func (*CharCode) TypeName() string {
	return "charCode"
}

func (o *CharCode) String() string {
	return fmt.Sprintf("(charCode)%v", o.Value)
}

// func (o *Reader) SetValue(valueA Object) error {
// 	// o.Value.Reset(int(ToIntObject(valueA)))

// 	return NewCommonError("unsupported action(set value)")
// }

func (o *CharCode) HasMemeber() bool {
	return true
}

func (o *CharCode) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return builtinAnyFunc(Call{Args: []Object{o}})
	case "toStr":
		return ToStringObject(o), nil
	}

	return CallObjectMethodFunc(o, nameA, argsA...)
}

func (o *CharCode) GetValue() Object {
	return Undefined
}

func (o *CharCode) GetMember(idxA string) Object {
	if o.Members == nil {
		return Undefined
	}

	v1, ok := o.Members[idxA]

	if !ok {
		return Undefined
	}

	return v1
}

func (o *CharCode) SetMember(idxA string, valueA Object) error {
	if o.Members == nil {
		o.Members = map[string]Object{}
	}

	if IsUndefInternal(valueA) {
		delete(o.Members, idxA)
		return nil
	}

	o.Members[idxA] = valueA

	// return fmt.Errorf("unsupported action(set member)")
	return nil
}

func (*CharCode) CanIterate() bool { return false }

func (o *CharCode) Iterate() Iterator {
	return nil
}

func (o *CharCode) IndexSet(index, value Object) error {
	idxT, ok := index.(String)

	if ok {
		strT := idxT.String()
		// if strT == "value" {
		// 	o.Value.Reset(int(ToIntObject(value)))
		// 	return nil
		// }

		return o.SetMember(strT, value)
	}

	return ErrNotIndexAssignable
}

func (o *CharCode) IndexGet(index Object) (Object, error) {
	switch v := index.(type) {
	case String:
		strT := v.String()

		if strT == "value" {
			return builtinAnyFunc(Call{Args: []Object{o}})
			// return ToIntObject(o.Value), nil
		}

		if strT == "lastError" {
			return ToStringObject(o.LastError), nil
			// return ToIntObject(o.Value), nil
		}

		rs := o.GetMember(strT)

		if !IsUndefInternal(rs) {
			return rs, nil
		}

		// return nil, ErrIndexOutOfBounds
		return GetObjectMethodFunc(o, strT)
	}

	return nil, NewCommonError("not indexable: %v", o.TypeName())
}

func (o *CharCode) Equal(right Object) bool {
	if v, ok := right.(*CharCode); ok {
		return v == o
	}

	return false
}

func (o *CharCode) IsFalsy() bool { return o.Value == nil }

func (o *CharCode) CanCall() bool { return true }

func (o *CharCode) Call(argsA ...Object) (Object, error) {
	rs := o.GetMember("run")

	var errT error

	if !IsUndefInternal(rs) {
		return rs, nil
	} else {
		rs, errT = GetObjectMethodFunc(o, "run")

		if errT != nil {
			return Undefined, errT
		}
	}

	nv1, ok := rs.(*Function)

	if !ok {
		return Undefined, NewCommonError("no valid function type")
	}

	if nv1.ValueEx != nil {
		return nv1.ValueEx(Call{Args: argsA})
	}

	return nv1.Value(argsA...)

	// return nil, ErrIndexOutOfBounds

	// return nil, ErrNotCallable
}

func (o *CharCode) BinaryOp(tok token.Token, right Object) (Object, error) {
	return nil, NewOperandTypeError(
		tok.String(),
		o.TypeName(),
		right.TypeName())
}

// func (o *CharCode) RunAsFunc(argsA ...Object) Object {
// 	var globalsA map[string]interface{} = nil
// 	// var additionsA []Object = nil

// 	envT := NewBaseEnv(globalsA) // Map{}

// 	retT, errT := NewVM(o.Value).Run(envT, argsA...)

// 	if errT != nil {
// 		return NewCommonError("%v", errT)
// 	}

// 	return retT
// }

func NewCharCode(srcA string, optsA ...*CompilerOptions) *CharCode {
	// byteCodeT := QuickCompile(srcA, optsA...) // quickCompile(tk.ToStr(argsA[0])) //

	// if tk.IsError(byteCodeT) {
	// 	return nil
	// }

	var compilerOptionsT *CompilerOptions
	if len(optsA) > 0 {
		compilerOptionsT = optsA[0]
	} else {
		if MainCompilerOptions != nil {
			compilerOptionsT = MainCompilerOptions
		} else {
			compilerOptionsT = &DefaultCompilerOptions
		}
	}

	if compilerOptionsT == nil {
		compilerOptionsT = &DefaultCompilerOptions
	}

	if strings.HasPrefix(srcA, "//TXRR#") {
		srcA = srcA[7:]

		if strings.HasPrefix(srcA, "//TXDEF#") {
			srcA = tk.DecryptStringByTXDEF(strings.TrimSpace(srcA[8:]), "char")
		}

		if strings.HasPrefix(srcA, "http") {
			srcA = tk.ToStr(tk.GetWeb(strings.TrimSpace(srcA)))
		}
	}

	if strings.HasPrefix(srcA, "//TXDEF#") {
		srcA = tk.DecryptStringByTXDEF(strings.TrimSpace(srcA[8:]), "char")
	}

	return &CharCode{Source: srcA, Value: nil, CompilerOptions: compilerOptionsT}

	// objT := &CharCode{Source: srcA, Value: nil, CompilerOptions: compilerOptionsT}

	// _, errT := objT.CallMethod("compile")

	// if errT != nil {
	// 	return nil
	// }

	// return objT

	// return &CharCode{Source: srcA, Value: byteCodeT.(*Bytecode)}
}

// Gel object is like modules based on CharCode object
type Gel struct {
	ObjectImpl
	// Source string
	Value *CharCode

	Members map[string]Object `json:"-"`
}

// var _ Object = NewGel()[0]

func (*Gel) TypeCode() int {
	return 193
}

// TypeName implements Object interface.
func (*Gel) TypeName() string {
	return "gel"
}

func (o *Gel) String() string {
	return fmt.Sprintf("(gel)%v", o.Value)
}

// func (o *Reader) SetValue(valueA Object) error {
// 	// o.Value.Reset(int(ToIntObject(valueA)))

// 	return NewCommonError("unsupported action(set value)")
// }

func (o *Gel) HasMemeber() bool {
	return true
}

func (o *Gel) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return builtinAnyFunc(Call{Args: []Object{o}})
	case "toStr":
		return ToStringObject(o), nil
	}

	return CallObjectMethodFunc(o, nameA, argsA...)
}

func (o *Gel) GetValue() Object {
	return Undefined
}

func (o *Gel) GetMember(idxA string) Object {
	if o.Members == nil {
		return Undefined
	}

	v1, ok := o.Members[idxA]

	if !ok {
		return Undefined
	}

	return v1
}

func (o *Gel) SetMember(idxA string, valueA Object) error {
	if o.Members == nil {
		o.Members = map[string]Object{}
	}

	if IsUndefInternal(valueA) {
		delete(o.Members, idxA)
		return nil
	}

	o.Members[idxA] = valueA

	// return fmt.Errorf("unsupported action(set member)")
	return nil
}

func (*Gel) CanIterate() bool { return false }

func (o *Gel) Iterate() Iterator {
	return nil
}

func (o *Gel) IndexSet(index, value Object) error {
	idxT, ok := index.(String)

	if ok {
		strT := idxT.String()
		// if strT == "value" {
		// 	o.Value.Reset(int(ToIntObject(value)))
		// 	return nil
		// }

		return o.SetMember(strT, value)
	}

	return ErrNotIndexAssignable
}

func (o *Gel) IndexGet(index Object) (Object, error) {
	switch v := index.(type) {
	case String:
		strT := v.String()

		if strT == "value" {
			return builtinAnyFunc(Call{Args: []Object{o}})
			// return ToIntObject(o.Value), nil
		}

		rs := o.GetMember(strT)

		if !IsUndefInternal(rs) {
			return rs, nil
		}

		// return nil, ErrIndexOutOfBounds
		rs1, errT := GetObjectMethodFunc(o, strT)

		if errT != nil || IsUndefInternal(rs1) {

			if o.Value == nil || o.Value.Value == nil {
				return Undefined, NewCommonError("not compiled")
			}

			var globalsA map[string]interface{} = nil
			// var additionsA []Object = make([]Object, 0, len(argsT)+1)

			envT := NewBaseEnv(globalsA) // Map{}

			// additionsA = append(additionsA, ToStringObject(strT))
			// additionsA = append(additionsA, argsT...)

			retT, errT := NewVM(o.Value.Value).Run(envT, ToStringObject(strT)) // , additionsA...)

			if errT != nil {
				o.SetMember(strT, NewCommonError("%v", errT))

				return NewCommonError("%v", errT), nil
			}

			o.SetMember(strT, retT)

			return retT, nil

			// fnT := &Function{
			// 	Name: strT,
			// 	ValueEx: func(c Call) (Object, error) {
			// 		argsT := c.GetArgs()

			// 		var globalsA map[string]interface{} = nil
			// 		var additionsA []Object = make([]Object, 0, len(argsT)+1)

			// 		envT := NewBaseEnv(globalsA) // Map{}

			// 		additionsA = append(additionsA, ToStringObject(strT))
			// 		additionsA = append(additionsA, argsT...)

			// 		retT, errT := NewVM(o.Value.Value).Run(envT, additionsA...)

			// 		if errT != nil {
			// 			return NewCommonError("%v", errT), nil
			// 		}

			// 		return retT, nil

			// 	},
			// }

			// o.SetMember(strT, fnT)

			// return fnT, nil

		}

		return rs1, nil
	}

	return nil, NewCommonError("not indexable: %v", o.TypeName())
}

func (o *Gel) Equal(right Object) bool {
	if v, ok := right.(*Gel); ok {
		return v == o
	}

	return false
}

func (o *Gel) IsFalsy() bool { return o.Value == nil }

func (o *Gel) CanCall() bool { return false }

func (o *Gel) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

func (o *Gel) BinaryOp(tok token.Token, right Object) (Object, error) {
	return nil, NewOperandTypeError(
		tok.String(),
		o.TypeName(),
		right.TypeName())
}

func NewGel(argsA ...Object) (Object, error) {

	if len(argsA) < 1 {
		return Undefined, fmt.Errorf("not enough parameters")
	}
	// tk.Plv(argsA[0])
	nv, ok := argsA[0].(*CharCode)

	vs := ObjectsToS(argsA[1:])

	if !ok {
		nv2, ok := argsA[0].(*Error)

		if ok {
			return nv2, nil
		}

		if !tk.IfSwitchExists(vs, "-inherit") {
			nv = NewCharCode(argsA[0].String(), nil)
		} else {
			nv = NewCharCode(argsA[0].String())
		}

		// return Undefined, fmt.Errorf("invalid input type")
	}

	if nv == nil {
		return NewCommonError("nil CharCode"), nil
	}

	return &Gel{Value: nv}, nil
}

// OrderedMap represents map of objects but has order(in the order of push-in) and implements Object interface.
type OrderedMap struct {
	ObjectImpl
	Value *tk.OrderedMap

	// Members map[string]Object
}

var (
	_ Object       = &OrderedMap{}
	_ Copier       = &OrderedMap{}
	_ IndexDeleter = &OrderedMap{}
	_ LengthGetter = &OrderedMap{}
)

func (*OrderedMap) TypeCode() int {
	return 135
}

// TypeName implements Object interface.
func (*OrderedMap) TypeName() string {
	return "orderedMap"
}

// String implements Object interface.
func (o *OrderedMap) String() string {
	var sb strings.Builder
	sb.WriteString("{")
	lenT := o.Value.Len()
	last := lenT - 1
	i := 0

	for _, k := range o.Value.GetStringKeys() {
		sb.WriteString(strconv.Quote(k))
		sb.WriteString(": ")
		switch v := o.Value.GetQuick(k).(type) {
		case String:
			sb.WriteString(strconv.Quote(v.String()))
		case Char:
			sb.WriteString(strconv.QuoteRune(rune(v)))
		case Bytes:
			sb.WriteString(fmt.Sprint([]byte(v)))
		default:
			sb.WriteString(v.(Object).String())
		}
		if i != last {
			sb.WriteString(", ")
		}
		i++
	}

	sb.WriteString("}")
	return sb.String()
}

func (o *OrderedMap) HasMemeber() bool {
	return true
}

func (o *OrderedMap) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return o, nil
	case "toStr":
		return ToStringObject(o), nil
	}

	return CallObjectMethodFunc(o, nameA, argsA...)
}

func (o *OrderedMap) CallName(nameA string, c Call) (Object, error) {
	//	tk.Pl("call: %#v", c)
	//	rs, ok := o.GetMember[nameA]
	//
	//	if ok {
	//		fn, ok := rs.(*Function)
	//
	//		if ok {
	//			return fn.CallEx(c)
	//		}
	//
	//		cfn, ok := rs.(*CompiledFunction)
	//
	//		if ok {
	//			retT, errT := NewInvoker(c.VM(), cfn).Invoke(c.GetArgs()...)
	//
	//			return retT, errT
	//		}
	//
	//		return NewCommonErrorWithPos(c, "member is not a function: %v", nameA), nil
	//	}

	switch nameA {
	case "toStr":
		return ToStringObject(o), nil
	case "value":
		return o, nil
	case "method":
		args := c.GetArgs()

		if len(args) < 1 {
			return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
		}

		return o.CallMethod(args[0].String(), args[1:]...)

	case "size":
		return ToIntObject(o.Value.Len()), nil
	case "Set", "set":
		args := c.GetArgs()

		if len(args) < 2 {
			return NewCommonErrorWithPos(c, "not enough parameters"), nil
		}

		o.Value.Set(args[0].String(), args[1])

		return Undefined, nil

	case "Get", "get":
		args := c.GetArgs()

		if len(args) < 1 {
			return NewCommonErrorWithPos(c, "not enough parameters"), nil
		}

		rs := o.Value.GetCompact(args[0].String())

		if rs == nil {
			return Undefined, nil
		}

		return ConvertToObject(rs), nil

	case "remove":
		args := c.GetArgs()

		if len(args) < 1 {
			return NewCommonErrorWithPos(c, "not enough parameters"), nil
		}

		rs := o.Value.DeleteQuick(args[0].String())

		return ConvertToObject(rs), nil

	case "sort", "sortKeys":
		args := c.GetArgs()

		errT := o.Value.SortStringKeys(ObjectsToS(args)...)

		if errT != nil {
			return NewCommonErrorWithPos(c, "failed to sort: %v", errT), nil
		}

		return o, nil

	case "sortReverse", "sortKeysReverse":
		args := c.GetArgs()

		errT := o.Value.SortStringKeysDesc(ObjectsToS(args)...)

		if errT != nil {
			return NewCommonErrorWithPos(c, "failed to sort: %v", errT), nil
		}

		return o, nil

	case "toMap":
		rs := make(Map)

		for _, k := range o.Value.GetStringKeys() {
			rs[k] = o.Value.GetCompact(k).(Object)
		}

		return rs, nil
	case "hasKey":
		args := c.GetArgs()

		if len(args) < 1 {
			return NewCommonErrorWithPos(c, "not enough parameters"), nil
		}

		ok := o.Value.HasKey(args[0].String())

		if ok {
			return Bool(true), nil
		}

		return Bool(false), nil
	case "keys", "getKeys":
		rs1 := o.Value.GetKeys()

		if rs1 == nil {
			return Undefined, nil
		}

		return ConvertToObject(rs1), nil
	case "values", "getValues":
		rs1 := o.Value.GetValues()

		if rs1 == nil {
			return Undefined, nil
		}

		return ConvertToObject(rs1), nil

	case "moveToFront":
		if c.Len() < 1 {
			return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
		}

		errT := o.Value.MoveToFront(c.Get(0).String())

		if errT != nil {
			return NewCommonErrorWithPos(c, "failed to move: %v", errT), nil
		}

		return o, nil

	case "moveToBack":
		if c.Len() < 1 {
			return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
		}

		errT := o.Value.MoveToBack(c.Get(0).String())

		if errT != nil {
			return NewCommonErrorWithPos(c, "failed to move: %v", errT), nil
		}

		return o, nil

	case "moveBefore":
		if c.Len() < 2 {
			return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
		}

		errT := o.Value.MoveBefore(c.Get(0).String(), c.Get(1).String())

		if errT != nil {
			return NewCommonErrorWithPos(c, "failed to move: %v", errT), nil
		}

		return o, nil

	case "moveAfter":
		if c.Len() < 2 {
			return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
		}

		errT := o.Value.MoveAfter(c.Get(0).String(), c.Get(1).String())

		if errT != nil {
			return NewCommonErrorWithPos(c, "failed to move: %v", errT), nil
		}

		return o, nil

	case "getItemByIndex":
		if c.Len() < 1 {
			return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
		}

		rs1 := o.Value.GetPairByIndex(ToIntQuick(c.Get(0)))

		if rs1 == nil {
			return Undefined, nil
		}

		return Array{ToStringObject(rs1.Key), ConvertToObject(rs1.Value)}, nil

	case "oldest", "first":
		rs1 := o.Value.Oldest()

		if rs1 == nil {
			return Undefined, nil
		}

		return Array{ToStringObject(rs1.Key), ConvertToObject(rs1.Value)}, nil

	case "newest", "last":
		rs1 := o.Value.Newest()

		if rs1 == nil {
			return Undefined, nil
		}

		return Array{ToStringObject(rs1.Key), ConvertToObject(rs1.Value)}, nil

	case "sortByFunc":
		if c.Len() < 1 {
			return Undefined, fmt.Errorf("not enough parameters")
		}

		nv1, ok := c.Get(0).(*Delegate)

		if !ok {
			return NewCommonErrorWithPos(c, "invalid type: %#v", c.Get(0)), nil
		}

		if nv1.Value == nil {
			return NewCommonErrorWithPos(c, "func code not compiled"), nil
		}

		func1 := func(i, j string) bool {
			rsT := nv1.Value(i, j)

			nvT, okT := rsT.(bool)

			//			tk.Plv(rsT, nvT, okT)

			if !okT {
				return false
			}

			return bool(nvT)
		}

		errT := o.Value.SortByFunc(func1)

		if errT != nil {
			return NewCommonErrorWithPos(c, "failed to sort: %v", errT), nil
		}

		return o, nil

	case "dump":
		return String(o.Value.Dump()), nil
	}

	rs1, errT := CallObjectMethodFunc(o, nameA, c.GetArgs()...)

	if errT != nil || tk.IsError(rs1) {
		rs3 := tk.ReflectCallMethodCompact(o.Value, nameA, ObjectsToI(c.GetArgs())...)
		return ConvertToObject(rs3), nil
	}

	return rs1, errT

	//	return Undefined, ErrInvalidIndex.NewError(nameA)
}

func (o *OrderedMap) GetValue() Object {
	return o
}

func (o *OrderedMap) GetMember(idxA string) Object {
	return Undefined
}

func (o *OrderedMap) SetMember(idxA string, valueA Object) error {
	return fmt.Errorf("unsupported action(set member)")
}

// Copy implements Copier interface.
func (o *OrderedMap) Copy() Object {
	cp, _ := NewOrderedMap()

	cpn := cp.(*OrderedMap)

	lenT := o.Value.Len()

	for i := 0; i < lenT; i++ {
		v, ok := o.Value.GetByIndex(i)

		if !ok {
			continue
		}

		vv, ok := v.(Object)

		if !ok {
			continue
		}

		k, ok := o.Value.GetKeyByIndex(i)

		if !ok {
			continue
		}

		vvv, ok := vv.(Copier)

		if ok {
			cpn.Value.Set(k, vvv.Copy())
		} else {
			cpn.Value.Set(k, vvv)
		}

	}

	// for _, k := range o.Value.GetStringKeys() {
	// 	if vv, ok := v..(Copier); ok {
	// 		cp[k] = vv.Copy()
	// 	} else {
	// 		cp[k] = v
	// 	}
	// }
	return cp
}

// IndexSet implements Object interface.
func (o *OrderedMap) IndexSet(index, value Object) error {
	o.Value.Set(index.String(), value)
	return nil
}

// IndexGet implements Object interface.
func (o *OrderedMap) IndexGet(index Object) (Object, error) {
	switch nv := index.(type) {
	case Int:
		rs1, ok := o.Value.GetByIndex(int(nv))

		if !ok {
			return NewCommonError("not found"), nil
		}

		return ConvertToObject(rs1), nil
	case Uint:
		rs1, ok := o.Value.GetByIndex(int(nv))

		if !ok {
			return NewCommonError("not found"), nil
		}

		return ConvertToObject(rs1), nil
	case Byte:
		rs1, ok := o.Value.GetByIndex(int(nv))

		if !ok {
			return NewCommonError("not found"), nil
		}

		return ConvertToObject(rs1), nil
	case Char:
		rs1, ok := o.Value.GetByIndex(int(nv))

		if !ok {
			return NewCommonError("not found"), nil
		}

		return ConvertToObject(rs1), nil
	}

	idxT := index.String()

	v, ok := o.Value.Get(idxT)
	if ok {
		nv, ok := v.(Object)

		if ok {
			return nv, nil
		}

		return ConvertToObject(v), nil
	}

	if idxT == "value" {
		return o, nil
	} else if idxT == "method" {
		return &Function{
			Name: "method",
			ValueEx: func(c Call) (Object, error) {
				args := c.GetArgs()

				if len(args) < 1 {
					return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
				}

				return o.CallMethod(args[0].String(), args[1:]...)
			},
		}, nil
	}

	return Undefined, nil
}

// Equal implements Object interface.
func (o *OrderedMap) Equal(right Object) bool {
	v, ok := right.(*OrderedMap)
	if !ok {
		return false
	}

	if o.Value.Len() != o.Value.Len() {
		return false
	}

	for _, k := range o.Value.GetStringKeys() {
		right1, ok := v.Value.Get(k)
		if !ok {
			return false
		}
		if !o.Value.GetQuick(k).(Object).Equal(right1.(Object)) {
			return false
		}
	}
	return true
}

// IsFalsy implements Object interface.
func (o *OrderedMap) IsFalsy() bool { return o.Value == nil || o.Value.Len() == 0 }

// CanCall implements Object interface.
func (*OrderedMap) CanCall() bool { return false }

// Call implements Object interface.
func (*OrderedMap) Call(...Object) (Object, error) {
	return nil, ErrNotCallable
}

// BinaryOp implements Object interface.
func (o *OrderedMap) BinaryOp(tok token.Token, right Object) (Object, error) {
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
func (*OrderedMap) CanIterate() bool { return true }

// Iterate implements Iterable interface.
func (o *OrderedMap) Iterate() Iterator {
	// keys := make([]string, 0, o.Value.Len())
	// for _, k := range o.Value.GetStringKeys() {
	// 	keys = append(keys, k)
	// }

	return &OrderedMapIterator{V: o, keys: o.Value.GetStringKeys()}
}

// IndexDelete tries to delete the string value of key from the map.
// IndexDelete implements IndexDeleter interface.
func (o *OrderedMap) IndexDelete(key Object) error {
	switch nv := key.(type) {
	case Int:
		return o.Value.DeleteByIndex(int(nv))
	case Uint:
		return o.Value.DeleteByIndex(int(nv))
	case Byte:
		return o.Value.DeleteByIndex(int(nv))
	case Char:
		return o.Value.DeleteByIndex(int(nv))
	}

	o.Value.Delete(key.String())
	return nil
}

// Len implements LengthGetter interface.
func (o *OrderedMap) Len() int {
	return o.Value.Len()
}

func NewOrderedMap(argsA ...Object) (Object, error) {
	lenT := len(argsA)

	rs := tk.NewOrderedMap()

	if lenT < 1 {
		return &OrderedMap{Value: rs}, nil
	}

	nv1, ok := argsA[0].(Map)

	if lenT == 1 && (!ok) {
		return Undefined, NewCommonError("unsupported type while creating orderedMap: (%T)%v", argsA[0], argsA[0])
	}

	for k, v := range nv1 {
		rs.Set(k, v)
	}

	len1T := lenT / 2

	for i := 0; i < len1T; i++ {
		rs.Set(argsA[i*2].String(), argsA[i*2+1])
	}

	return &OrderedMap{Value: rs}, nil
}

// BigInt represents large signed integer values and implements Object interface.
type BigInt struct {
	// ObjectImpl

	Value *big.Int

	Members map[string]Object `json:"-"`
}

func (*BigInt) TypeCode() int {
	return 201
}

func (*BigInt) TypeName() string {
	return "bigInt"
}

func (o *BigInt) String() string {
	return o.Value.String()
}

func (o *BigInt) HasMemeber() bool {
	return true
}

func (o *BigInt) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return o, nil
	case "toStr":
		return ToStringObject(o), nil
	}

	return CallObjectMethodFunc(o, nameA, argsA...)
}

func (o *BigInt) GetValue() Object {
	return o
}

func (o *BigInt) SetValue(valueA Object) error {
	switch nv := valueA.(type) {
	case *BigInt:
		o.Value.Set(nv.Value)
		return nil
	case Bool:
		if nv {
			o.Value.Set(big.NewInt(1))
		} else {
			o.Value.Set(big.NewInt(0))
		}

		return nil
	case Byte:
		o.Value.Set(big.NewInt(int64(nv)))
		return nil
	case Int:
		o.Value.Set(big.NewInt(int64(nv)))
		return nil
	case Char:
		o.Value.Set(big.NewInt(int64(nv)))
		return nil
	case Uint:
		o.Value.Set(big.NewInt(int64(nv)))
		return nil
	case Float:
		o.Value.Set(big.NewInt(int64(nv)))
	case String:
		rs, ok := big.NewInt(0).SetString(nv.String(), 10)
		if !ok {
			return NewCommonError("failed to parse bigInt: %v", nv)
		}

		o.Value.Set(rs)
		return nil
	}

	return ErrNotIndexAssignable
}

func (o *BigInt) GetMember(idxA string) Object {
	if o.Members == nil {
		return Undefined
	}

	v1, ok := o.Members[idxA]

	if !ok {
		return Undefined
	}

	return v1
}

func (o *BigInt) SetMember(idxA string, valueA Object) error {
	if o.Members == nil {
		o.Members = map[string]Object{}
	}

	if IsUndefInternal(valueA) {
		delete(o.Members, idxA)
		return nil
	}

	o.Members[idxA] = valueA

	// return fmt.Errorf("unsupported action(set member)")
	return nil
}

func (o *BigInt) Equal(right Object) bool {
	switch v := right.(type) {
	case *BigInt:
		return o.Value.Cmp(v.Value) == 0
	case Bool:
		if v {
			return o.Value.Cmp(big.NewInt(1)) == 0
		}

		return o.Value.Cmp(big.NewInt(0)) == 0
	case Byte:
		return o.Value.Cmp(big.NewInt(int64(v))) == 0
	case Int:
		return o.Value.Cmp(big.NewInt(int64(v))) == 0
	case Char:
		return o.Value.Cmp(big.NewInt(int64(v))) == 0
	case Uint:
		return o.Value.Cmp(big.NewInt(int64(v))) == 0
	case Float:
		return o.Value.Cmp(big.NewInt(int64(v))) == 0
	}

	return false
}

func (o *BigInt) IsFalsy() bool {
	return o.Value.Cmp(BigIntZero) == 0
}

func (o *BigInt) CanCall() bool {
	return false
}

func (o *BigInt) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

func (*BigInt) CanIterate() bool {
	return false
}

func (*BigInt) Iterate() Iterator {
	return nil
}

func (o *BigInt) IndexSet(index, value Object) error {
	idxT, ok := index.(String)

	if ok {
		strT := idxT.String()
		if strT == "value" {
			return o.SetValue(value)
		}

		return o.SetMember(strT, value)
	}

	return ErrNotIndexAssignable
}

func (o *BigInt) IndexGet(index Object) (Object, error) {
	switch v := index.(type) {
	case String:
		strT := v.String()

		if strT == "value" {
			return o.GetValue(), nil
		}

		rs := o.GetMember(strT)

		if !IsUndefInternal(rs) {
			return rs, nil
		}

		// return nil, ErrIndexOutOfBounds
		return GetObjectMethodFunc(o, strT)
	}

	return nil, ErrNotIndexable
}

func (o *BigInt) BinaryOp(tok token.Token, right Object) (Object, error) {
	switch v := right.(type) {
	case *BigInt:
		switch tok {
		case token.Add:
			return &BigInt{Value: big.NewInt(0).Add(o.Value, v.Value)}, nil
		case token.Sub:
			return &BigInt{Value: big.NewInt(0).Sub(o.Value, v.Value)}, nil
		case token.Mul:
			return &BigInt{Value: big.NewInt(0).Mul(o.Value, v.Value)}, nil
		case token.Quo:
			if v.Value.Cmp(BigIntZero) == 0 {
				return Undefined, ErrZeroDivision
			}

			return &BigInt{Value: big.NewInt(0).Quo(o.Value, v.Value)}, nil
		case token.Rem:
			return &BigInt{Value: big.NewInt(0).Rem(o.Value, v.Value)}, nil
		case token.And:
			return &BigInt{Value: big.NewInt(0).And(o.Value, v.Value)}, nil
		case token.Or:
			return &BigInt{Value: big.NewInt(0).Or(o.Value, v.Value)}, nil
		case token.Xor:
			return &BigInt{Value: big.NewInt(0).Xor(o.Value, v.Value)}, nil
		case token.AndNot:
			return &BigInt{Value: big.NewInt(0).AndNot(o.Value, v.Value)}, nil
		case token.Shl:
			return &BigInt{Value: big.NewInt(0).Lsh(o.Value, uint(v.Value.Uint64()))}, nil
		case token.Shr:
			return &BigInt{Value: big.NewInt(0).Rsh(o.Value, uint(v.Value.Uint64()))}, nil
		case token.Less:
			return Bool(o.Value.Cmp(v.Value) < 0), nil
		case token.LessEq:
			rs := o.Value.Cmp(v.Value)
			return Bool(rs == 0 || rs < 0), nil
		case token.Greater:
			return Bool(o.Value.Cmp(v.Value) > 0), nil
		case token.GreaterEq:
			rs := o.Value.Cmp(v.Value)
			return Bool(rs == 0 || rs > 0), nil
		}
	case Byte:
		switch tok {
		case token.Add:
			return &BigInt{Value: big.NewInt(0).Add(o.Value, big.NewInt(int64(v)))}, nil
		case token.Sub:
			return &BigInt{Value: big.NewInt(0).Sub(o.Value, big.NewInt(int64(v)))}, nil
		case token.Mul:
			return &BigInt{Value: big.NewInt(0).Mul(o.Value, big.NewInt(int64(v)))}, nil
		case token.Quo:
			if v == 0 {
				return Undefined, ErrZeroDivision
			}

			return &BigInt{Value: big.NewInt(0).Quo(o.Value, big.NewInt(int64(v)))}, nil
		case token.Rem:
			return &BigInt{Value: big.NewInt(0).Rem(o.Value, big.NewInt(int64(v)))}, nil
		case token.And:
			return &BigInt{Value: big.NewInt(0).And(o.Value, big.NewInt(int64(v)))}, nil
		case token.Or:
			return &BigInt{Value: big.NewInt(0).Or(o.Value, big.NewInt(int64(v)))}, nil
		case token.Xor:
			return &BigInt{Value: big.NewInt(0).Xor(o.Value, big.NewInt(int64(v)))}, nil
		case token.AndNot:
			return &BigInt{Value: big.NewInt(0).AndNot(o.Value, big.NewInt(int64(v)))}, nil
		case token.Shl:
			return &BigInt{Value: big.NewInt(0).Lsh(o.Value, uint(v))}, nil
		case token.Shr:
			return &BigInt{Value: big.NewInt(0).Rsh(o.Value, uint(v))}, nil
		case token.Less:
			return Bool(o.Value.Cmp(big.NewInt(int64(v))) < 0), nil
		case token.LessEq:
			rs := o.Value.Cmp(big.NewInt(int64(v)))
			return Bool(rs == 0 || rs < 0), nil
		case token.Greater:
			return Bool(o.Value.Cmp(big.NewInt(int64(v))) > 0), nil
		case token.GreaterEq:
			rs := o.Value.Cmp(big.NewInt(int64(v)))
			return Bool(rs == 0 || rs > 0), nil
		}
	case Int:
		switch tok {
		case token.Add:
			return &BigInt{Value: big.NewInt(0).Add(o.Value, big.NewInt(int64(v)))}, nil
		case token.Sub:
			return &BigInt{Value: big.NewInt(0).Sub(o.Value, big.NewInt(int64(v)))}, nil
		case token.Mul:
			return &BigInt{Value: big.NewInt(0).Mul(o.Value, big.NewInt(int64(v)))}, nil
		case token.Quo:
			if v == 0 {
				return Undefined, ErrZeroDivision
			}

			return &BigInt{Value: big.NewInt(0).Quo(o.Value, big.NewInt(int64(v)))}, nil
		case token.Rem:
			return &BigInt{Value: big.NewInt(0).Rem(o.Value, big.NewInt(int64(v)))}, nil
		case token.And:
			return &BigInt{Value: big.NewInt(0).And(o.Value, big.NewInt(int64(v)))}, nil
		case token.Or:
			return &BigInt{Value: big.NewInt(0).Or(o.Value, big.NewInt(int64(v)))}, nil
		case token.Xor:
			return &BigInt{Value: big.NewInt(0).Xor(o.Value, big.NewInt(int64(v)))}, nil
		case token.AndNot:
			return &BigInt{Value: big.NewInt(0).AndNot(o.Value, big.NewInt(int64(v)))}, nil
		case token.Shl:
			return &BigInt{Value: big.NewInt(0).Lsh(o.Value, uint(v))}, nil
		case token.Shr:
			return &BigInt{Value: big.NewInt(0).Rsh(o.Value, uint(v))}, nil
		case token.Less:
			return Bool(o.Value.Cmp(big.NewInt(int64(v))) < 0), nil
		case token.LessEq:
			rs := o.Value.Cmp(big.NewInt(int64(v)))
			return Bool(rs == 0 || rs < 0), nil
		case token.Greater:
			return Bool(o.Value.Cmp(big.NewInt(int64(v))) > 0), nil
		case token.GreaterEq:
			rs := o.Value.Cmp(big.NewInt(int64(v)))
			return Bool(rs == 0 || rs > 0), nil
		}
	case Char:
		switch tok {
		case token.Add:
			return &BigInt{Value: big.NewInt(0).Add(o.Value, big.NewInt(int64(v)))}, nil
		case token.Sub:
			return &BigInt{Value: big.NewInt(0).Sub(o.Value, big.NewInt(int64(v)))}, nil
		case token.Mul:
			return &BigInt{Value: big.NewInt(0).Mul(o.Value, big.NewInt(int64(v)))}, nil
		case token.Quo:
			if v == 0 {
				return Undefined, ErrZeroDivision
			}

			return &BigInt{Value: big.NewInt(0).Quo(o.Value, big.NewInt(int64(v)))}, nil
		case token.Rem:
			return &BigInt{Value: big.NewInt(0).Rem(o.Value, big.NewInt(int64(v)))}, nil
		case token.And:
			return &BigInt{Value: big.NewInt(0).And(o.Value, big.NewInt(int64(v)))}, nil
		case token.Or:
			return &BigInt{Value: big.NewInt(0).Or(o.Value, big.NewInt(int64(v)))}, nil
		case token.Xor:
			return &BigInt{Value: big.NewInt(0).Xor(o.Value, big.NewInt(int64(v)))}, nil
		case token.AndNot:
			return &BigInt{Value: big.NewInt(0).AndNot(o.Value, big.NewInt(int64(v)))}, nil
		case token.Shl:
			return &BigInt{Value: big.NewInt(0).Lsh(o.Value, uint(v))}, nil
		case token.Shr:
			return &BigInt{Value: big.NewInt(0).Rsh(o.Value, uint(v))}, nil
		case token.Less:
			return Bool(o.Value.Cmp(big.NewInt(int64(v))) < 0), nil
		case token.LessEq:
			rs := o.Value.Cmp(big.NewInt(int64(v)))
			return Bool(rs == 0 || rs < 0), nil
		case token.Greater:
			return Bool(o.Value.Cmp(big.NewInt(int64(v))) > 0), nil
		case token.GreaterEq:
			rs := o.Value.Cmp(big.NewInt(int64(v)))
			return Bool(rs == 0 || rs > 0), nil
		}
	case Uint:
		switch tok {
		case token.Add:
			return &BigInt{Value: big.NewInt(0).Add(o.Value, big.NewInt(int64(v)))}, nil
		case token.Sub:
			return &BigInt{Value: big.NewInt(0).Sub(o.Value, big.NewInt(int64(v)))}, nil
		case token.Mul:
			return &BigInt{Value: big.NewInt(0).Mul(o.Value, big.NewInt(int64(v)))}, nil
		case token.Quo:
			if v == 0 {
				return Undefined, ErrZeroDivision
			}

			return &BigInt{Value: big.NewInt(0).Quo(o.Value, big.NewInt(int64(v)))}, nil
		case token.Rem:
			return &BigInt{Value: big.NewInt(0).Rem(o.Value, big.NewInt(int64(v)))}, nil
		case token.And:
			return &BigInt{Value: big.NewInt(0).And(o.Value, big.NewInt(int64(v)))}, nil
		case token.Or:
			return &BigInt{Value: big.NewInt(0).Or(o.Value, big.NewInt(int64(v)))}, nil
		case token.Xor:
			return &BigInt{Value: big.NewInt(0).Xor(o.Value, big.NewInt(int64(v)))}, nil
		case token.AndNot:
			return &BigInt{Value: big.NewInt(0).AndNot(o.Value, big.NewInt(int64(v)))}, nil
		case token.Shl:
			return &BigInt{Value: big.NewInt(0).Lsh(o.Value, uint(v))}, nil
		case token.Shr:
			return &BigInt{Value: big.NewInt(0).Rsh(o.Value, uint(v))}, nil
		case token.Less:
			return Bool(o.Value.Cmp(big.NewInt(int64(v))) < 0), nil
		case token.LessEq:
			rs := o.Value.Cmp(big.NewInt(int64(v)))
			return Bool(rs == 0 || rs < 0), nil
		case token.Greater:
			return Bool(o.Value.Cmp(big.NewInt(int64(v))) > 0), nil
		case token.GreaterEq:
			rs := o.Value.Cmp(big.NewInt(int64(v)))
			return Bool(rs == 0 || rs > 0), nil
		}
	case Float:
		switch tok {
		case token.Add:
			return &BigInt{Value: big.NewInt(0).Add(o.Value, big.NewInt(int64(v)))}, nil
		case token.Sub:
			return &BigInt{Value: big.NewInt(0).Sub(o.Value, big.NewInt(int64(v)))}, nil
		case token.Mul:
			return &BigInt{Value: big.NewInt(0).Mul(o.Value, big.NewInt(int64(v)))}, nil
		case token.Quo:
			if v == 0 {
				return Undefined, ErrZeroDivision
			}

			return &BigInt{Value: big.NewInt(0).Quo(o.Value, big.NewInt(int64(v)))}, nil
		case token.Rem:
			return &BigInt{Value: big.NewInt(0).Rem(o.Value, big.NewInt(int64(v)))}, nil
		case token.And:
			return &BigInt{Value: big.NewInt(0).And(o.Value, big.NewInt(int64(v)))}, nil
		case token.Or:
			return &BigInt{Value: big.NewInt(0).Or(o.Value, big.NewInt(int64(v)))}, nil
		case token.Xor:
			return &BigInt{Value: big.NewInt(0).Xor(o.Value, big.NewInt(int64(v)))}, nil
		case token.AndNot:
			return &BigInt{Value: big.NewInt(0).AndNot(o.Value, big.NewInt(int64(v)))}, nil
		case token.Shl:
			return &BigInt{Value: big.NewInt(0).Lsh(o.Value, uint(v))}, nil
		case token.Shr:
			return &BigInt{Value: big.NewInt(0).Rsh(o.Value, uint(v))}, nil
		case token.Less:
			return Bool(o.Value.Cmp(big.NewInt(int64(v))) < 0), nil
		case token.LessEq:
			rs := o.Value.Cmp(big.NewInt(int64(v)))
			return Bool(rs == 0 || rs < 0), nil
		case token.Greater:
			return Bool(o.Value.Cmp(big.NewInt(int64(v))) > 0), nil
		case token.GreaterEq:
			rs := o.Value.Cmp(big.NewInt(int64(v)))
			return Bool(rs == 0 || rs > 0), nil
		}
	case Bool:
		var v1 int64
		if v {
			v1 = 1
		} else {
			v1 = 0
		}
		switch tok {
		case token.Add:
			return &BigInt{Value: big.NewInt(0).Add(o.Value, big.NewInt(int64(v1)))}, nil
		case token.Sub:
			return &BigInt{Value: big.NewInt(0).Sub(o.Value, big.NewInt(int64(v1)))}, nil
		case token.Mul:
			return &BigInt{Value: big.NewInt(0).Mul(o.Value, big.NewInt(int64(v1)))}, nil
		case token.Quo:
			if v1 == 0 {
				return Undefined, ErrZeroDivision
			}

			return &BigInt{Value: big.NewInt(0).Quo(o.Value, big.NewInt(int64(v1)))}, nil
		case token.Rem:
			return &BigInt{Value: big.NewInt(0).Rem(o.Value, big.NewInt(int64(v1)))}, nil
		case token.And:
			return &BigInt{Value: big.NewInt(0).And(o.Value, big.NewInt(int64(v1)))}, nil
		case token.Or:
			return &BigInt{Value: big.NewInt(0).Or(o.Value, big.NewInt(int64(v1)))}, nil
		case token.Xor:
			return &BigInt{Value: big.NewInt(0).Xor(o.Value, big.NewInt(int64(v1)))}, nil
		case token.AndNot:
			return &BigInt{Value: big.NewInt(0).AndNot(o.Value, big.NewInt(int64(v1)))}, nil
		case token.Shl:
			return &BigInt{Value: big.NewInt(0).Lsh(o.Value, uint(v1))}, nil
		case token.Shr:
			return &BigInt{Value: big.NewInt(0).Rsh(o.Value, uint(v1))}, nil
		case token.Less:
			return Bool(o.Value.Cmp(big.NewInt(int64(v1))) < 0), nil
		case token.LessEq:
			rs := o.Value.Cmp(big.NewInt(int64(v1)))
			return Bool(rs == 0 || rs < 0), nil
		case token.Greater:
			return Bool(o.Value.Cmp(big.NewInt(int64(v1))) > 0), nil
		case token.GreaterEq:
			rs := o.Value.Cmp(big.NewInt(int64(v1)))
			return Bool(rs == 0 || rs > 0), nil
		}
	case *UndefinedType:
		switch tok {
		case token.Less, token.LessEq:
			return False, nil
		case token.Greater, token.GreaterEq:
			return True, nil
		}
	}

	return Undefined, NewCommonError("unsupported type: %T", right)
}

func NewBigInt(c Call) (Object, error) {
	argsA := c.GetArgs()

	if len(argsA) < 1 {
		return &BigInt{Value: big.NewInt(0)}, nil
	}

	arg0 := argsA[0]

	switch nv := arg0.(type) {
	case Byte:
		return &BigInt{Value: big.NewInt(int64(nv))}, nil
	case Int:
		return &BigInt{Value: big.NewInt(int64(nv))}, nil
	case Char:
		return &BigInt{Value: big.NewInt(int64(nv))}, nil
	case Uint:
		return &BigInt{Value: big.NewInt(int64(nv))}, nil
	case Float:
		return &BigInt{Value: big.NewInt(int64(nv))}, nil
	case Bool:
		if nv {
			return &BigInt{Value: big.NewInt(1)}, nil
		}

		return &BigInt{Value: big.NewInt(0)}, nil
	case String:
		rs, ok := big.NewInt(0).SetString(nv.String(), 10)
		if !ok {
			return NewCommonErrorWithPos(c, "failed to parse bigInt: %v", nv), nil
		}

		return &BigInt{Value: rs}, nil
	case *BigInt:
		return &BigInt{Value: big.NewInt(0).Set(nv.Value)}, nil
	}

	return NewCommonErrorWithPos(c, "unsupported type: %T", arg0), nil
}

// BigFloat represents large signed float values and implements Object interface.
type BigFloat struct {
	// ObjectImpl

	Value *big.Float

	Members map[string]Object `json:"-"`
}

func (*BigFloat) TypeCode() int {
	return 203
}

func (*BigFloat) TypeName() string {
	return "bigFloat"
}

func (o *BigFloat) String() string {
	return o.Value.Text('f', 20) // fmt.Sprintf("%f", o.Value)
}

func (o *BigFloat) HasMemeber() bool {
	return true
}

func (o *BigFloat) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return o, nil
	case "toStr":
		return ToStringObject(o), nil
	}

	return CallObjectMethodFunc(o, nameA, argsA...)
}

func (o *BigFloat) GetValue() Object {
	return o
}

func (o *BigFloat) SetValue(valueA Object) error {
	switch nv := valueA.(type) {
	case *BigFloat:
		o.Value.Set(nv.Value)
		return nil
	case *BigInt:
		o.Value.SetInt(nv.Value)
		return nil
	case Bool:
		if nv {
			o.Value.Set(big.NewFloat(1))
		} else {
			o.Value.Set(big.NewFloat(0))
		}

		return nil
	case Byte:
		o.Value.Set(big.NewFloat(float64(nv)))
		return nil
	case Int:
		o.Value.Set(big.NewFloat(float64(nv)))
		return nil
	case Char:
		o.Value.Set(big.NewFloat(float64(nv)))
		return nil
	case Uint:
		o.Value.Set(big.NewFloat(float64(nv)))
		return nil
	case Float:
		o.Value.Set(big.NewFloat(float64(nv)))
	case String:
		rs, _, errT := big.NewFloat(0).Parse(nv.String(), 10)
		if errT != nil {
			return fmt.Errorf("failed to parse bigFloat: %v", errT)
		}

		o.Value.Set(rs)
		return nil
	}

	return ErrNotIndexAssignable
}

func (o *BigFloat) GetMember(idxA string) Object {
	if o.Members == nil {
		return Undefined
	}

	v1, ok := o.Members[idxA]

	if !ok {
		return Undefined
	}

	return v1
}

func (o *BigFloat) SetMember(idxA string, valueA Object) error {
	if o.Members == nil {
		o.Members = map[string]Object{}
	}

	if IsUndefInternal(valueA) {
		delete(o.Members, idxA)
		return nil
	}

	o.Members[idxA] = valueA

	// return fmt.Errorf("unsupported action(set member)")
	return nil
}

func (o *BigFloat) Equal(right Object) bool {
	switch v := right.(type) {
	case *BigFloat:
		return o.Value.Cmp(v.Value) == 0
	case *BigInt:
		return o.Value.Cmp(big.NewFloat(0).SetInt(v.Value)) == 0
	case Bool:
		if v {
			return o.Value.Cmp(big.NewFloat(1)) == 0
		}

		return o.Value.Cmp(big.NewFloat(0)) == 0
	case Byte:
		return o.Value.Cmp(big.NewFloat(float64(v))) == 0
	case Int:
		// tk.Plo(v, float64(v), big.NewFloat(float64(v)), o.Value, o.Value.Cmp(big.NewFloat(float64(v))))
		return o.Value.Cmp(big.NewFloat(float64(v))) == 0
	case Char:
		return o.Value.Cmp(big.NewFloat(float64(v))) == 0
	case Uint:
		return o.Value.Cmp(big.NewFloat(float64(v))) == 0
	case Float:
		return o.Value.Cmp(big.NewFloat(float64(v))) == 0
	}

	return false
}

func (o *BigFloat) IsFalsy() bool {
	return o.Value.Cmp(BigFloatZero) == 0
}

func (o *BigFloat) CanCall() bool {
	return false
}

func (o *BigFloat) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

func (*BigFloat) CanIterate() bool {
	return false
}

func (*BigFloat) Iterate() Iterator {
	return nil
}

func (o *BigFloat) IndexSet(index, value Object) error {
	idxT, ok := index.(String)

	if ok {
		strT := idxT.String()
		if strT == "value" {
			return o.SetValue(value)
		}

		return o.SetMember(strT, value)
	}

	return ErrNotIndexAssignable
}

func (o *BigFloat) IndexGet(index Object) (Object, error) {
	switch v := index.(type) {
	case String:
		strT := v.String()

		if strT == "value" {
			return o.GetValue(), nil
		}

		rs := o.GetMember(strT)

		if !IsUndefInternal(rs) {
			return rs, nil
		}

		// return nil, ErrIndexOutOfBounds
		return GetObjectMethodFunc(o, strT)
	}

	return nil, ErrNotIndexable
}

func (o *BigFloat) BinaryOp(tok token.Token, right Object) (Object, error) {
	switch v := right.(type) {
	case *BigFloat:
		switch tok {
		case token.Add:
			return &BigFloat{Value: big.NewFloat(0).Add(o.Value, v.Value)}, nil
		case token.Sub:
			return &BigFloat{Value: big.NewFloat(0).Sub(o.Value, v.Value)}, nil
		case token.Mul:
			return &BigFloat{Value: big.NewFloat(0).Mul(o.Value, v.Value)}, nil
		case token.Quo:
			if v.Value.Cmp(BigFloatZero) == 0 {
				return Undefined, ErrZeroDivision
			}

			return &BigFloat{Value: big.NewFloat(0).Quo(o.Value, v.Value)}, nil
		// case token.Rem:
		// 	return &BigFloat{Value: big.NewFloat(0).Rem(o.Value, v.Value)}, nil
		// case token.And:
		// 	return &BigFloat{Value: big.NewFloat(0).And(o.Value, v.Value)}, nil
		// case token.Or:
		// 	return &BigFloat{Value: big.NewFloat(0).Or(o.Value, v.Value)}, nil
		// case token.Xor:
		// 	return &BigFloat{Value: big.NewFloat(0).Xor(o.Value, v.Value)}, nil
		// case token.AndNot:
		// 	return &BigFloat{Value: big.NewFloat(0).AndNot(o.Value, v.Value)}, nil
		// case token.Shl:
		// 	return &BigFloat{Value: big.NewFloat(0).Lsh(o.Value, uint(v.Value.Uint64()))}, nil
		// case token.Shr:
		// 	return &BigFloat{Value: big.NewFloat(0).Rsh(o.Value, uint(v.Value.Uint64()))}, nil
		case token.Less:
			return Bool(o.Value.Cmp(v.Value) < 0), nil
		case token.LessEq:
			rs := o.Value.Cmp(v.Value)
			return Bool(rs == 0 || rs < 0), nil
		case token.Greater:
			return Bool(o.Value.Cmp(v.Value) > 0), nil
		case token.GreaterEq:
			rs := o.Value.Cmp(v.Value)
			return Bool(rs == 0 || rs > 0), nil
		}
	case *BigInt:
		return o.BinaryOp(tok, &BigFloat{Value: big.NewFloat(0).SetInt(v.Value)})
		// switch tok {
		// case token.Add:
		// 	return &BigInt{Value: big.NewInt(0).Add(o.Value, v.Value)}, nil
		// case token.Sub:
		// 	return &BigInt{Value: big.NewInt(0).Sub(o.Value, v.Value)}, nil
		// case token.Mul:
		// 	return &BigInt{Value: big.NewInt(0).Mul(o.Value, v.Value)}, nil
		// case token.Quo:
		// 	if v.Value.Cmp(BigIntZero) == 0 {
		// 		return Undefined, ErrZeroDivision
		// 	}

		// 	return &BigInt{Value: big.NewInt(0).Quo(o.Value, v.Value)}, nil
		// case token.Rem:
		// 	return &BigInt{Value: big.NewInt(0).Rem(o.Value, v.Value)}, nil
		// case token.And:
		// 	return &BigInt{Value: big.NewInt(0).And(o.Value, v.Value)}, nil
		// case token.Or:
		// 	return &BigInt{Value: big.NewInt(0).Or(o.Value, v.Value)}, nil
		// case token.Xor:
		// 	return &BigInt{Value: big.NewInt(0).Xor(o.Value, v.Value)}, nil
		// case token.AndNot:
		// 	return &BigInt{Value: big.NewInt(0).AndNot(o.Value, v.Value)}, nil
		// case token.Shl:
		// 	return &BigInt{Value: big.NewInt(0).Lsh(o.Value, uint(v.Value.Uint64()))}, nil
		// case token.Shr:
		// 	return &BigInt{Value: big.NewInt(0).Rsh(o.Value, uint(v.Value.Uint64()))}, nil
		// case token.Less:
		// 	return Bool(o.Value.Cmp(v.Value) < 0), nil
		// case token.LessEq:
		// 	rs := o.Value.Cmp(v.Value)
		// 	return Bool(rs == 0 || rs < 0), nil
		// case token.Greater:
		// 	return Bool(o.Value.Cmp(v.Value) > 0), nil
		// case token.GreaterEq:
		// 	rs := o.Value.Cmp(v.Value)
		// 	return Bool(rs == 0 || rs > 0), nil
		// }
	case Byte:
		return o.BinaryOp(tok, &BigFloat{Value: big.NewFloat(0).SetInt64(int64(v))})
	case Int:
		return o.BinaryOp(tok, &BigFloat{Value: big.NewFloat(0).SetInt64(int64(v))})
	case Char:
		return o.BinaryOp(tok, &BigFloat{Value: big.NewFloat(0).SetInt64(int64(v))})
	case Uint:
		return o.BinaryOp(tok, &BigFloat{Value: big.NewFloat(0).SetInt64(int64(v))})
	case Float:
		return o.BinaryOp(tok, &BigFloat{Value: big.NewFloat(float64(v))})
	case Bool:
		var v1 float64
		if v {
			v1 = 1
		} else {
			v1 = 0
		}

		return o.BinaryOp(tok, &BigFloat{Value: big.NewFloat(v1)})
	case *UndefinedType:
		switch tok {
		case token.Less, token.LessEq:
			return False, nil
		case token.Greater, token.GreaterEq:
			return True, nil
		}
	}

	return Undefined, NewCommonError("unsupported type: %T", right)
}

func NewBigFloat(c Call) (Object, error) {
	argsA := c.GetArgs()

	if len(argsA) < 1 {
		return &BigFloat{Value: big.NewFloat(0)}, nil
	}

	arg0 := argsA[0]

	switch nv := arg0.(type) {
	case Byte:
		return &BigFloat{Value: big.NewFloat(float64(nv))}, nil
	case Int:
		return &BigFloat{Value: big.NewFloat(float64(nv))}, nil
	case Char:
		return &BigFloat{Value: big.NewFloat(float64(nv))}, nil
	case Uint:
		return &BigFloat{Value: big.NewFloat(float64(nv))}, nil
	case Float:
		return &BigFloat{Value: big.NewFloat(float64(nv))}, nil
	case Bool:
		if nv {
			return &BigFloat{Value: big.NewFloat(1)}, nil
		}

		return &BigFloat{Value: big.NewFloat(0)}, nil
	case String:
		nv1 := nv.String()
		integerDigitCount := strings.Index(nv1, ".")

		fractionalDigitCount := len(nv1) - (integerDigitCount + 1) // exclude the decimal from the count

		if integerDigitCount < 1 || fractionalDigitCount < 1 {
			rs, _, errT := big.NewFloat(0).Parse(nv.String(), 10)
			if errT != nil {
				return NewCommonErrorWithPos(c, "failed to parse bigFloat: %v", errT), nil
			}

			return &BigFloat{Value: rs}, nil
		}

		bf := big.NewFloat(0.0)
		precision := uint(math.Ceil(float64(fractionalDigitCount)*math.Log2(10.0)) + float64(integerDigitCount)*math.Log2(10.0))
		bf, _ = bf.SetPrec(uint(precision * 2)).SetString(nv1)
		// ppi := bf.Text('f', fractionalDigitCount)
		// fmt.Printf("%s\n", ppi)

		return &BigFloat{Value: bf}, nil

	case *BigInt:
		return &BigFloat{Value: big.NewFloat(0).SetInt(nv.Value)}, nil
	case *BigFloat:
		return &BigFloat{Value: big.NewFloat(0).Set(nv.Value)}, nil
	}

	return NewCommonErrorWithPos(c, "unsupported type: %T", arg0), nil
}

// Image represents an image and implements Object interface.
type Image struct {
	// ObjectImpl

	Value image.Image

	Members map[string]Object `json:"-"`
}

func (*Image) TypeCode() int {
	return 501
}

func (*Image) TypeName() string {
	return "image"
}

func (o *Image) String() string {
	return fmt.Sprintf("%v", o.Value)
}

func (o *Image) HasMemeber() bool {
	return true
}

func (o *Image) CallMethod(nameA string, argsA ...Object) (Object, error) {
	return o.CallName(nameA, Call{Args: argsA})
}

func (o *Image) CallName(nameA string, c Call) (Object, error) {
	switch nameA {
	case "value":
		return o, nil
	case "toStr":
		return ToStringObject(o), nil
	case "width":
		rs := o.Value.Bounds().Max.X - o.Value.Bounds().Min.X
		
		if rs < 0 {
			rs = -rs
		}

		return Int(rs), nil
	case "height":
		rs := o.Value.Bounds().Max.Y - o.Value.Bounds().Min.Y
		
		if rs < 0 {
			rs = -rs
		}

		return Int(rs), nil
	}

	return Undefined, NewCommonErrorWithPos(c, "method not found: %v", nameA)
}

func (o *Image) GetValue() Object {
	return o
}

func (o *Image) SetValue(valueA Object) error {
	switch nv := valueA.(type) {
	case image.Image:
		o.Value = nv
		return nil
	}

	return ErrNotIndexAssignable
}

func (o *Image) GetMember(idxA string) Object {
	if o.Members == nil {
		return Undefined
	}

	v1, ok := o.Members[idxA]

	if !ok {
		return Undefined
	}

	return v1
}

func (o *Image) SetMember(idxA string, valueA Object) error {
	if o.Members == nil {
		o.Members = map[string]Object{}
	}

	if IsUndefInternal(valueA) {
		delete(o.Members, idxA)
		return nil
	}

	o.Members[idxA] = valueA

	// return fmt.Errorf("unsupported action(set member)")
	return nil
}

func (o *Image) Equal(right Object) bool {
	switch v := right.(type) {
	case *Image:
		return o.Value == v.Value
	}

	return false
}

func (o *Image) IsFalsy() bool {
	return o.Value == nil
}

func (o *Image) CanCall() bool {
	return false
}

func (o *Image) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

func (*Image) CanIterate() bool {
	return false
}

func (*Image) Iterate() Iterator {
	return nil
}

func (o *Image) IndexSet(index, value Object) error {
	idxT, ok := index.(String)

	if ok {
		strT := idxT.String()
		if strT == "value" {
			return o.SetValue(value)
		}

		return o.SetMember(strT, value)
	}

	return ErrNotIndexAssignable
}

func (o *Image) IndexGet(index Object) (Object, error) {
	switch v := index.(type) {
	case String:
		strT := v.String()

		if strT == "value" {
			return o.GetValue(), nil
		}

		rs := o.GetMember(strT)

		if !IsUndefInternal(rs) {
			return rs, nil
		}

		// return nil, ErrIndexOutOfBounds
		return GetObjectMethodFunc(o, strT)
	}

	return nil, ErrNotIndexable
}

func (o *Image) BinaryOp(tok token.Token, right Object) (Object, error) {
	return Undefined, NewCommonError("unsupported type: %T", right)
}

func NewImage(c Call) (Object, error) {
	argsA := c.GetArgs()

	imgPT := tk.NewImage(ObjectsToS(argsA)...)

	return &Image{Value: imgPT}, nil
}

// Delegate represents an function caller and implements Object interface.
type Delegate struct {
	// ObjectImpl
	Code  *CharCode
	Value tk.QuickVarDelegate

	Members map[string]Object `json:"-"`
}

func (*Delegate) TypeCode() int {
	return 601
}

func (*Delegate) TypeName() string {
	return "delegate"
}

func (o *Delegate) String() string {
	return fmt.Sprintf("(delegate)Code: %v, Value: %v", o.Code != nil, o.Value != nil)
}

func (o *Delegate) HasMemeber() bool {
	return true
}

func (o *Delegate) CallMethod(nameA string, argsA ...Object) (Object, error) {
	return o.CallName(nameA, Call{Args: argsA})
	//	switch nameA {
	//	case "value":
	//		return o, nil
	//	case "toStr":
	//		return ToStringObject(o), nil
	//	}
	//
	//	return CallObjectMethodFunc(o, nameA, argsA...)
}

func (o *Delegate) CallName(nameA string, c Call) (Object, error) {
	switch nameA {
	case "value":
		return o, nil
	case "toStr":
		return ToStringObject(o), nil
	case "call":
		args := c.GetArgs()

		rs := o.Value(ObjectsToI(args)...)

		return ConvertToObject(rs), nil
	}

	args := c.GetArgs()

	rs1, errT := CallObjectMethodFunc(o, nameA, args...)

	//	if errT != nil || tk.IsError(rs1) {
	////		fmt.Printf("%#v\n", o)
	//		rs3 := tk.ReflectCallMethodCompact(o.Value, nameA, ObjectsToI(args)...)
	//		return ConvertToObject(rs3), nil
	//	}
	if errT != nil || tk.IsError(rs1) {
		if o.Value == nil {
			return Undefined, NewCommonError("delegate nil: %v", nameA)
		}

		args := c.GetArgs()

		//		fmt.Printf("1args: %#v", args)

		aryT := make([]interface{}, 0, len(args)+1)

		aryT = append(aryT, nameA)

		for _, v := range args {
			aryT = append(aryT, ConvertFromObject(v))
		}

		rs := o.Value(aryT...)

		return ConvertToObject(rs), nil
	}

	return rs1, errT

	//	return Undefined, NewCommonErrorWithPos(c, "method not found: %v", nameA)
}

func (o *Delegate) GetValue() Object {
	return o
}

func (o *Delegate) SetValue(valueA Object) error {
	switch nv := valueA.(type) {
	case *Delegate:
		o.Value = nv.Value
		return nil
	}

	return ErrNotIndexAssignable
}

func (o *Delegate) GetMember(idxA string) Object {
	if o.Members == nil {
		return Undefined
	}

	v1, ok := o.Members[idxA]

	if !ok {
		return Undefined
	}

	return v1
}

func (o *Delegate) SetMember(idxA string, valueA Object) error {
	if o.Members == nil {
		o.Members = map[string]Object{}
	}

	if IsUndefInternal(valueA) {
		delete(o.Members, idxA)
		return nil
	}

	o.Members[idxA] = valueA

	// return fmt.Errorf("unsupported action(set member)")
	return nil
}

func (o *Delegate) Equal(right Object) bool {
	// switch v := right.(type) {
	// case *Delegate:
	// 	return o.Value == v.Value
	// }

	return false
}

func (o *Delegate) IsFalsy() bool {
	return o.Value == nil
}

func (o *Delegate) CanCall() bool {
	return true
}

func (o *Delegate) Call(argsA ...Object) (Object, error) {

	rs := o.Value(ObjectsToI(argsA)...)

	return ConvertToObject(rs), nil
}

func (*Delegate) CanIterate() bool {
	return false
}

func (*Delegate) Iterate() Iterator {
	return nil
}

func (o *Delegate) IndexSet(index, value Object) error {
	idxT, ok := index.(String)

	if ok {
		strT := idxT.String()
		if strT == "value" {
			return o.SetValue(value)
		}

		return o.SetMember(strT, value)
	}

	return ErrNotIndexAssignable
}

func (o *Delegate) IndexGet(index Object) (Object, error) {
	switch v := index.(type) {
	case String:
		strT := v.String()

		if strT == "value" {
			return o.GetValue(), nil
		}

		rs := o.GetMember(strT)

		if !IsUndefInternal(rs) {
			return rs, nil
		}

		// return nil, ErrIndexOutOfBounds
		rs, errT := GetObjectMethodFunc(o, strT)

		if errT != nil || IsUndefInternal(rs) {
			if o.Value == nil {
				return rs, errT
			}

			fn2 := &Function{
				Name: strT,
				Value: func(argsA ...Object) (Object, error) {
					aryT := make([]interface{}, 0, len(argsA)+1)

					aryT = append(aryT, strT)

					aryT = append(aryT, ObjectsToI(argsA)...)

					rs := o.Value(aryT...)

					return ConvertToObject(rs), nil
				}}

			o.SetMember(strT, fn2)

			return fn2, nil
		}

		return rs, nil
	}

	return nil, ErrNotIndexable
}

func (o *Delegate) BinaryOp(tok token.Token, right Object) (Object, error) {
	return Undefined, NewCommonError("unsupported type: %T", right)
}

func NewDelegate(c Call) (Object, error) {
	argsA := c.GetArgs()

	// if len(argsA) < 1 {
	// 	return Undefined, NewCommonErrorWithPos(c, "%v", "not enough parameters")
	// }

	if len(argsA) < 1 {
		return Undefined, fmt.Errorf("not enough parameters")
	}

	nv, ok := argsA[0].(*CharCode)

	if !ok {
		nv = NewCharCode(argsA[0].String())

		// return Undefined, fmt.Errorf("invalid input type")
	}

	// if nv == nil {
	// 	return Undefined, fmt.Errorf("nil CharCode")
	// }

	// deleT := func(argsA ...interface{}) interface{} {
	// 	var globalsA map[string]interface{} = nil
	// 	// var additionsA []Object = make([]Object, 0, len(argsT)+1)

	// 	envT := NewBaseEnv(globalsA) // Map{}

	// 	(*envT)["inputG"] = ConvertToObject(argsA)

	// 	// additionsA = append(additionsA, ToStringObject(strT))
	// 	// additionsA = append(additionsA, argsT...)

	// 	retT, errT := NewVM(argsA[0].(*Bytecode)).Run(envT) // , additionsA...)

	// 	if errT != nil {
	// 		return fmt.Errorf("%v", errT)
	// 	}

	// 	return ConvertFromObject(retT)
	// }

	return &Delegate{Code: nv}, nil
}

func NewExternalDelegate(funcA func(...interface{}) interface{}) Object {
	return &Delegate{Value: funcA}
}

// Excel represents an Excel(or compatible) file and implements Object interface.
type Excel struct {
	// ObjectImpl
	Value *excelize.File

	Members map[string]Object `json:"-"`
}

func (*Excel) TypeCode() int {
	return 1003
}

func (*Excel) TypeName() string {
	return "Excel"
}

func (o *Excel) String() string {
	return fmt.Sprintf("(Excel)Value: %v", o.Value)
}

func (o *Excel) HasMemeber() bool {
	return true
}

func (o *Excel) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return o, nil
	case "toStr":
		return ToStringObject(o), nil
	}

	return CallObjectMethodFunc(o, nameA, argsA...)
}

func (o *Excel) GetValue() Object {
	return o
}

func (o *Excel) SetValue(valueA Object) error {
	switch nv := valueA.(type) {
	case *Excel:
		o.Value = nv.Value
		return nil
	}

	return ErrNotIndexAssignable
}

func (o *Excel) GetMember(idxA string) Object {
	if o.Members == nil {
		return Undefined
	}

	v1, ok := o.Members[idxA]

	if !ok {
		return Undefined
	}

	return v1
}

func (o *Excel) SetMember(idxA string, valueA Object) error {
	if o.Members == nil {
		o.Members = map[string]Object{}
	}

	if IsUndefInternal(valueA) {
		delete(o.Members, idxA)
		return nil
	}

	o.Members[idxA] = valueA

	// return fmt.Errorf("unsupported action(set member)")
	return nil
}

func (o *Excel) Equal(right Object) bool {
	// switch v := right.(type) {
	// case *Delegate:
	// 	return o.Value == v.Value
	// }

	return false
}

func (o *Excel) IsFalsy() bool {
	return o.Value == nil
}

func (o *Excel) CanCall() bool {
	return false
}

func (o *Excel) Call(argsA ...Object) (Object, error) {
	return nil, ErrNotCallable
}

func (*Excel) CanIterate() bool {
	return false
}

func (*Excel) Iterate() Iterator {
	return nil
}

func (o *Excel) IndexSet(index, value Object) error {
	idxT, ok := index.(String)

	if ok {
		strT := idxT.String()
		if strT == "value" {
			return o.SetValue(value)
		}

		return o.SetMember(strT, value)
	}

	return ErrNotIndexAssignable
}

func (o *Excel) IndexGet(index Object) (Object, error) {
	switch v := index.(type) {
	case String:
		strT := v.String()

		if strT == "value" {
			return o.GetValue(), nil
		}

		rs := o.GetMember(strT)

		if !IsUndefInternal(rs) {
			return rs, nil
		}

		// return nil, ErrIndexOutOfBounds
		rs, errT := GetObjectMethodFunc(o, strT)

		if errT != nil || IsUndefInternal(rs) {
			if o.Value == nil {
				return rs, errT
			}

			rs2 := &Function{
				Name: strT,
				ValueEx: func(c Call) (Object, error) {
					rs3 := tk.ReflectCallMethodCompact(o.Value, strT, ObjectsToI(c.GetArgs())...)

					return ConvertToObject(rs3), nil
				},
			}

			o.SetMember(strT, rs2)

			return rs2, nil
		}

		return rs, nil
	}

	return nil, ErrNotIndexable
}

func (o *Excel) BinaryOp(tok token.Token, right Object) (Object, error) {
	return Undefined, NewCommonError("unsupported type: %T", right)
}

func NewExcel(c Call) (Object, error) {
	argsA := c.GetArgs()

	// // if len(argsA) < 1 {
	// // 	return Undefined, NewCommonErrorWithPos(c, "%v", "not enough parameters")
	// // }

	pathT := strings.TrimSpace(GetSwitchFromObjects(argsA, "-path=", ""))

	if pathT == "" {
		f := excelize.NewFile()
		return &Excel{Value: f}, nil
	}

	f, err := excelize.OpenFile(pathT)
	if err != nil {
		return NewCommonErrorWithPos(c, "failed to open file: %v", err), nil
	}

	return &Excel{Value: f}, nil
}

// Stack represents a generic stack object and implements Object interface.
type Stack struct {
	ObjectImpl
	Value *tk.SimpleStack

	Members map[string]Object `json:"-"`
}

var (
	_ Object       = &Stack{}
	_ LengthGetter = &Stack{}
)

func (*Stack) TypeCode() int {
	return 161
}

// TypeName implements Object interface.
func (*Stack) TypeName() string {
	return "stack"
}

// String implements Object interface.
func (o *Stack) String() string {
	return o.String()
}

func (o *Stack) HasMemeber() bool {
	return true
}

func (o *Stack) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return o, nil
	case "toStr":
		return ToStringObject(o), nil
	}

	return CallObjectMethodFunc(o, nameA, argsA...)
}

func (o *Stack) GetValue() Object {
	return o
}

// func (o *Stack) SetValue(valueA Object) error {
// 	return ErrNotSe
// }

func (o *Stack) GetMember(idxA string) Object {
	if o.Members == nil {
		return Undefined
	}

	v1, ok := o.Members[idxA]

	if !ok {
		return Undefined
	}

	return v1
}

func (o *Stack) SetMember(idxA string, valueA Object) error {
	if o.Members == nil {
		o.Members = map[string]Object{}
	}

	if IsUndefInternal(valueA) {
		delete(o.Members, idxA)
		return nil
	}

	o.Members[idxA] = valueA

	// return fmt.Errorf("unsupported action(set member)")
	return nil
}

// // Copy implements Copier interface.
// func (o *Stack) Copy() Object {
// 	cp, _ := NewStack()

// 	cpn := cp.(*Stack)

// 	lenT := o.Value.Len()

// 	for i := 0; i < lenT; i++ {
// 		v, ok := o.Value.GetByIndex(i)

// 		if !ok {
// 			continue
// 		}

// 		vv, ok := v.(Object)

// 		if !ok {
// 			continue
// 		}

// 		k, ok := o.Value.GetKeyByIndex(i)

// 		if !ok {
// 			continue
// 		}

// 		vvv, ok := vv.(Copier)

// 		if ok {
// 			cpn.Value.Set(k, vvv.Copy())
// 		} else {
// 			cpn.Value.Set(k, vvv)
// 		}

// 	}

// 	// for _, k := range o.Value.GetStringKeys() {
// 	// 	if vv, ok := v..(Copier); ok {
// 	// 		cp[k] = vv.Copy()
// 	// 	} else {
// 	// 		cp[k] = v
// 	// 	}
// 	// }
// 	return cp
// }

// IndexSet implements Object interface.
func (o *Stack) IndexSet(index, value Object) error {
	return o.Value.SetByIndex(ToIntQuick(index), ConvertFromObject(value))
}

// IndexGet implements Object interface.
func (o *Stack) IndexGet(index Object) (Object, error) {
	return ConvertToObject(o.Value.PeekLayer(ToIntQuick(index))), nil
}

// Equal implements Object interface.
func (o *Stack) Equal(right Object) bool {
	return false
}

// IsFalsy implements Object interface.
func (o *Stack) IsFalsy() bool { return o.Value == nil || o.Value.Size() == 0 }

// CanCall implements Object interface.
func (*Stack) CanCall() bool { return false }

// Call implements Object interface.
func (*Stack) Call(...Object) (Object, error) {
	return nil, ErrNotCallable
}

// BinaryOp implements Object interface.
func (o *Stack) BinaryOp(tok token.Token, right Object) (Object, error) {
	return nil, fmt.Errorf("unsupported action(BinaryOp)")
}

// CanIterate implements Object interface.
func (*Stack) CanIterate() bool { return true }

// Iterate implements Iterable interface.
func (o *Stack) Iterate() Iterator {
	// keys := make([]string, 0, o.Value.Len())
	// for _, k := range o.Value.GetStringKeys() {
	// 	keys = append(keys, k)
	// }

	return &StackIterator{V: o}
}

// // IndexDelete tries to delete the string value of key from the map.
// // IndexDelete implements IndexDeleter interface.
// func (o *Stack) IndexDelete(key Object) error {
// 	switch nv := key.(type) {
// 	case Int:
// 		return o.Value.DeleteByIndex(int(nv))
// 	case Uint:
// 		return o.Value.DeleteByIndex(int(nv))
// 	case Byte:
// 		return o.Value.DeleteByIndex(int(nv))
// 	case Char:
// 		return o.Value.DeleteByIndex(int(nv))
// 	}

// 	o.Value.Delete(key.String())
// 	return nil
// }

// Len implements LengthGetter interface.
func (o *Stack) Len() int {
	return o.Value.Size()
}

func NewStack(c Call) (Object, error) {
	argsA := c.GetArgs()

	lenT := len(argsA)

	rs := tk.NewSimpleStack()

	if lenT < 1 {
		return &Stack{Value: rs}, nil
	}

	for _, v := range argsA {
		rs.Push(v)
	}

	return &Stack{Value: rs}, nil
}

// Queue represents a generic FIFO queue object and implements Object interface.
type Queue struct {
	ObjectImpl
	Value *tk.AnyQueue

	Members map[string]Object `json:"-"`
}

var (
	_ Object       = &Queue{}
	_ LengthGetter = &Queue{}
)

func (*Queue) TypeCode() int {
	return 163
}

// TypeName implements Object interface.
func (*Queue) TypeName() string {
	return "queue"
}

// String implements Object interface.
func (o *Queue) String() string {
	return o.Value.String()
}

func (o *Queue) HasMemeber() bool {
	return true
}

func (o *Queue) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return o, nil
	case "toStr":
		return ToStringObject(o), nil
	}

	return CallObjectMethodFunc(o, nameA, argsA...)
}

func (o *Queue) GetValue() Object {
	return o
}

// func (o *Queue) SetValue(valueA Object) error {
// 	return ErrNotSe
// }

func (o *Queue) GetMember(idxA string) Object {
	if o.Members == nil {
		return Undefined
	}

	v1, ok := o.Members[idxA]

	if !ok {
		return Undefined
	}

	return v1
}

func (o *Queue) SetMember(idxA string, valueA Object) error {
	if o.Members == nil {
		o.Members = map[string]Object{}
	}

	if IsUndefInternal(valueA) {
		delete(o.Members, idxA)
		return nil
	}

	o.Members[idxA] = valueA

	// return fmt.Errorf("unsupported action(set member)")
	return nil
}

// // Copy implements Copier interface.
// func (o *Stack) Copy() Object {
// 	cp, _ := NewStack()

// 	cpn := cp.(*Stack)

// 	lenT := o.Value.Len()

// 	for i := 0; i < lenT; i++ {
// 		v, ok := o.Value.GetByIndex(i)

// 		if !ok {
// 			continue
// 		}

// 		vv, ok := v.(Object)

// 		if !ok {
// 			continue
// 		}

// 		k, ok := o.Value.GetKeyByIndex(i)

// 		if !ok {
// 			continue
// 		}

// 		vvv, ok := vv.(Copier)

// 		if ok {
// 			cpn.Value.Set(k, vvv.Copy())
// 		} else {
// 			cpn.Value.Set(k, vvv)
// 		}

// 	}

// 	// for _, k := range o.Value.GetStringKeys() {
// 	// 	if vv, ok := v..(Copier); ok {
// 	// 		cp[k] = vv.Copy()
// 	// 	} else {
// 	// 		cp[k] = v
// 	// 	}
// 	// }
// 	return cp
// }

// IndexSet implements Object interface.
func (o *Queue) IndexSet(index, value Object) error {
	return o.Value.SetByIndex(ToIntQuick(index), ConvertFromObject(value))
}

// IndexGet implements Object interface.
func (o *Queue) IndexGet(index Object) (Object, error) {
	switch v := index.(type) {
	case String:
		strT := v.String()

		if strT == "value" {
			return o, nil
		}

		rs := o.GetMember(strT)

		if !IsUndefInternal(rs) {
			return rs, nil
		}

		rs1, errT := GetObjectMethodFunc(o, strT)

		if errT == nil && !tk.IsError(rs1) {
			return rs1, nil
		}

		rs5 := tk.ReflectGetMember(o.Value, strT)

		if !tk.IsError(rs5) {
			rs6 := ConvertToObject(rs5)
			// tk.Plo(rs6)
			if nv, ok := rs6.(*Any); ok {
				if strings.HasPrefix(nv.OriginalType, "func(") {
					rs2 := &Function{
						Name: strT,
						ValueEx: func(c Call) (Object, error) {
							rs3 := tk.ReflectCallMethodCompact(o.Value, strT, ObjectsToI(c.GetArgs())...)

							return ConvertToObject(rs3), nil
						},
					}

					o.SetMember(strT, rs2)

					return rs2, nil

				}
			}
			o.SetMember(strT, rs6)
			return rs6, nil
		}

		if errT != nil {
			if tk.ReflectHasMethod(o.Value, strT) {
				rs2 := &Function{
					Name: strT,
					ValueEx: func(c Call) (Object, error) {
						rs3 := tk.ReflectCallMethodCompact(o.Value, strT, ObjectsToI(c.GetArgs())...)

						return ConvertToObject(rs3), nil
					},
				}

				o.SetMember(strT, rs2)

				return rs2, nil
			}
		}

		return rs1, errT
		// // return nil, ErrIndexOutOfBounds
	}

	return ConvertToObject(o.Value.Get(ToIntQuick(index))), nil
}

// Equal implements Object interface.
func (o *Stack) Queue(right Object) bool {
	return false
}

// IsFalsy implements Object interface.
func (o *Queue) IsFalsy() bool { return o.Value == nil || o.Value.Size() == 0 }

// CanCall implements Object interface.
func (*Queue) CanCall() bool { return false }

// Call implements Object interface.
func (*Queue) Call(...Object) (Object, error) {
	return nil, ErrNotCallable
}

// BinaryOp implements Object interface.
func (o *Queue) BinaryOp(tok token.Token, right Object) (Object, error) {
	return nil, fmt.Errorf("unsupported action(BinaryOp)")
}

// CanIterate implements Object interface.
func (*Queue) CanIterate() bool { return true }

// Iterate implements Iterable interface.
func (o *Queue) Iterate() Iterator {
	// keys := make([]string, 0, o.Value.Len())
	// for _, k := range o.Value.GetStringKeys() {
	// 	keys = append(keys, k)
	// }

	return &QueueIterator{V: o}
}

// // IndexDelete tries to delete the string value of key from the map.
// // IndexDelete implements IndexDeleter interface.
// func (o *Stack) IndexDelete(key Object) error {
// 	switch nv := key.(type) {
// 	case Int:
// 		return o.Value.DeleteByIndex(int(nv))
// 	case Uint:
// 		return o.Value.DeleteByIndex(int(nv))
// 	case Byte:
// 		return o.Value.DeleteByIndex(int(nv))
// 	case Char:
// 		return o.Value.DeleteByIndex(int(nv))
// 	}

// 	o.Value.Delete(key.String())
// 	return nil
// }

// Len implements LengthGetter interface.
func (o *Queue) Len() int {
	return o.Value.Size()
}

func NewQueue(c Call) (Object, error) {
	argsA := c.GetArgs()

	lenT := len(argsA)

	if lenT > 0 {
		rs := tk.NewAnyQueue(ToIntQuick(argsA[0]))
		return &Queue{Value: rs}, nil
	}

	rs := tk.NewAnyQueue()

	return &Queue{Value: rs}, nil
}

// JsVm represents an JavaScript Virtual Machine could run script once or more
type JsVm struct {
	// ObjectImpl

	Value *goja.Runtime

	Members map[string]Object `json:"-"`
}

func (*JsVm) TypeCode() int {
	return 711
}

func (*JsVm) TypeName() string {
	return "jsVm"
}

func (o *JsVm) String() string {
	return fmt.Sprintf("%v", o.Value)
}

func (o *JsVm) HasMemeber() bool {
	return true
}

func (o *JsVm) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return o, nil
	case "toStr":
		return ToStringObject(o), nil
	}

	return CallObjectMethodFunc(o, nameA, argsA...)
}

func (o *JsVm) GetValue() Object {
	return o
}

func (o *JsVm) SetValue(valueA Object) error {
	switch nv := valueA.(type) {
	case *JsVm:
		o.Value = nv.Value
		return nil
	}

	return ErrNotIndexAssignable
}

func (o *JsVm) GetMember(idxA string) Object {
	if o.Members == nil {
		return Undefined
	}

	v1, ok := o.Members[idxA]

	if !ok {
		return Undefined
	}

	return v1
}

func (o *JsVm) SetMember(idxA string, valueA Object) error {
	if o.Members == nil {
		o.Members = map[string]Object{}
	}

	if IsUndefInternal(valueA) {
		delete(o.Members, idxA)
		return nil
	}

	o.Members[idxA] = valueA

	// return fmt.Errorf("unsupported action(set member)")
	return nil
}

func (o *JsVm) Equal(right Object) bool {
	switch v := right.(type) {
	case *JsVm:
		return o.Value == v.Value
	}

	return false
}

func (o *JsVm) IsFalsy() bool {
	return o.Value == nil
}

func (o *JsVm) CanCall() bool {
	return false
}

func (o *JsVm) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

func (*JsVm) CanIterate() bool {
	return false
}

func (*JsVm) Iterate() Iterator {
	return nil
}

func (o *JsVm) IndexSet(index, value Object) error {
	return ErrNotIndexAssignable
}

func (o *JsVm) IndexGet(index Object) (Object, error) {
	switch v := index.(type) {
	case String:
		strT := v.String()

		if strT == "value" {
			return o, nil
		}

		rs := o.GetMember(strT)

		if !IsUndefInternal(rs) {
			return rs, nil
		}

		// return nil, ErrIndexOutOfBounds
		return GetObjectMethodFunc(o, strT)
	}

	return nil, ErrNotIndexable
}

func (o *JsVm) BinaryOp(tok token.Token, right Object) (Object, error) {
	return Undefined, NewCommonError("unsupported type: %T", right)
}

func (o *JsVm) CallName(nameA string, c Call) (Object, error) {
	//	tk.Pl("EvalMachine call: %#v", c)
	switch nameA {
	case "eval", "run":
		args := c.GetArgs()

		if len(args) < 1 {
			return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
		}

		rs, errT := o.Value.RunString(args[0].String())

		if errT != nil {
			return NewCommonErrorWithPos(c, "%v", errT), nil
		}

		if rs != nil {
			//			fmt.Println(lastResultT)
			return ConvertToObject(rs.Export()), nil
		}

		return Undefined, nil
	case "set":
		args := c.GetArgs()

		if len(args) < 2 {
			return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
		}

		errT := o.Value.Set(args[0].String(), ConvertFromObject(args[1]))

		if errT != nil {
			return NewCommonErrorWithPos(c, "%v", errT), nil
		}

		return Undefined, nil
	case "get":
		args := c.GetArgs()

		if len(args) < 1 {
			return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
		}

		rs := o.Value.Get(args[0].String()).Export()

		return ConvertToObject(rs), nil
	}

	rs1, errT := CallObjectMethodFunc(o, nameA, c.GetArgs()...)

	if errT != nil || tk.IsError(rs1) {
		rs3 := tk.ReflectCallMethodCompact(o.Value, nameA, ObjectsToI(c.GetArgs())...)
		return ConvertToObject(rs3), nil
	}

	return rs1, errT

	//	return Undefined, NewCommonErrorWithPos(c, "method not found: %v", nameA)
}

func NewJsVm(c Call) (Object, error) {
	//	argsA := c.GetArgs()

	jsVmT := goja.New()

	return &JsVm{Value: jsVmT}, nil
}

//// QjsVm represents an JavaScript Virtual Machine based on QuickJS engine which could run script once or more
//type QjsVm struct {
//	// ObjectImpl
//
//	Value *quickjs.VM
//
//	Members map[string]Object `json:"-"`
//}
//
//func (*QjsVm) TypeCode() int {
//	return 712
//}
//
//func (*QjsVm) TypeName() string {
//	return "qjsVm"
//}
//
//func (o *QjsVm) String() string {
//	return fmt.Sprintf("%v", o.Value)
//}
//
//func (o *QjsVm) HasMemeber() bool {
//	return true
//}
//
//func (o *QjsVm) CallMethod(nameA string, argsA ...Object) (Object, error) {
//	switch nameA {
//	case "value":
//		return o, nil
//	case "toStr":
//		return ToStringObject(o), nil
//	}
//
//	return CallObjectMethodFunc(o, nameA, argsA...)
//}
//
//func (o *QjsVm) GetValue() Object {
//	return o
//}
//
//func (o *QjsVm) SetValue(valueA Object) error {
//	switch nv := valueA.(type) {
//	case *QjsVm:
//		o.Value = nv.Value
//		return nil
//	}
//
//	return ErrNotIndexAssignable
//}
//
//func (o *QjsVm) GetMember(idxA string) Object {
//	if o.Members == nil {
//		return Undefined
//	}
//
//	v1, ok := o.Members[idxA]
//
//	if !ok {
//		return Undefined
//	}
//
//	return v1
//}
//
//func (o *QjsVm) SetMember(idxA string, valueA Object) error {
//	if o.Members == nil {
//		o.Members = map[string]Object{}
//	}
//
//	if IsUndefInternal(valueA) {
//		delete(o.Members, idxA)
//		return nil
//	}
//
//	o.Members[idxA] = valueA
//
//	// return fmt.Errorf("unsupported action(set member)")
//	return nil
//}
//
//func (o *QjsVm) Equal(right Object) bool {
//	switch v := right.(type) {
//	case *QjsVm:
//		return o.Value == v.Value
//	}
//
//	return false
//}
//
//func (o *QjsVm) IsFalsy() bool {
//	return o.Value == nil
//}
//
//func (o *QjsVm) CanCall() bool {
//	return false
//}
//
//func (o *QjsVm) Call(_ ...Object) (Object, error) {
//	return nil, ErrNotCallable
//}
//
//func (*QjsVm) CanIterate() bool {
//	return false
//}
//
//func (*QjsVm) Iterate() Iterator {
//	return nil
//}
//
//func (o *QjsVm) IndexSet(index, value Object) error {
//	return ErrNotIndexAssignable
//}
//
//func (o *QjsVm) IndexGet(index Object) (Object, error) {
//	switch v := index.(type) {
//	case String:
//		strT := v.Value
//
//		if strT == "value" {
//			return o, nil
//		}
//
//		rs := o.GetMember(strT)
//
//		if !IsUndefInternal(rs) {
//			return rs, nil
//		}
//
//		// return nil, ErrIndexOutOfBounds
//		return GetObjectMethodFunc(o, strT)
//	}
//
//	return nil, ErrNotIndexable
//}
//
//func (o *QjsVm) BinaryOp(tok token.Token, right Object) (Object, error) {
//	return Undefined, NewCommonError("unsupported type: %T", right)
//}
//
//func (o *QjsVm) CallName(nameA string, c Call) (Object, error) {
////	tk.Pl("EvalMachine call: %#v", c)
//	switch nameA {
//	case "eval", "run":
//		args := c.GetArgs()
//
//		if len(args) < 1 {
//			return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
//		}
//
////		rs, errT := o.Value.RunString(args[0].String())
//		rs, errT := o.Value.Eval(args[0].String(), quickjs.EvalGlobal)
//
//		if errT != nil {
//			return NewCommonErrorWithPos(c, "%v", errT), nil
//		}
//
//		if rs != nil {
////			fmt.Println(lastResultT)
//			return ConvertToObject(rs), nil
//		}
//
//		return Undefined, nil
//	case "set":
//		if o.Value == nil {
//			return NewCommonErrorWithPos(c, "invalid object"), nil
//		}
//
//		args := c.GetArgs()
//
//		if len(args) < 2 {
//			return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
//		}
//
//		globalObjT := o.Value.GlobalObject()
//
//		atomT, errT := o.Value.NewAtom(args[0].String())
//		if errT != nil {
//			return NewCommonErrorWithPos(c, "failed to new atom prop: %v", errT), nil
//		}
//
//		errT = o.Value.SetProperty(globalObjT, atomT, ConvertFromObject(args[1]))
//
//		if errT != nil {
//			return NewCommonErrorWithPos(c, "%v", errT), nil
//		}
//
//		return Undefined, nil
//	case "setFunc":
//		if o.Value == nil {
//			return NewCommonErrorWithPos(c, "invalid object"), nil
//		}
//
//		args := c.GetArgs()
//
//		if len(args) < 2 {
//			return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
//		}
//
//		f1 := ConvertFromObject(args[1])
//
////		fmt.Printf("%T %v\n", f1, f1)
//
////		var f2 = func(argsA ...interface{}) interface{} {
////			return f1.(tk.QuickVarDelegate)(argsA...)
////		}
//
////		var f2 = func(this any, argsA ...interface{}) int {
////			return a1 + a2
////		}
//
////		rs1 := f2(1, 2)
////		fmt.Printf("%T %v\n", rs1, rs1)
//
//
////		f2 = f1.(func(argsA ...interface{}) interface{})
//
//		errT := o.Value.RegisterFunc(args[0].String(), f1, false)
//
//		if errT != nil {
//			return NewCommonErrorWithPos(c, "%v", errT), nil
//		}
//
//		return Undefined, nil
//	case "get":
//		args := c.GetArgs()
//
//		if len(args) < 1 {
//			return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
//		}
//
//		globalObjT := o.Value.GlobalObject()
//
//		atomT, errT := o.Value.NewAtom(args[0].String())
//		if errT != nil {
//			return NewCommonErrorWithPos(c, "failed to new atom prop: %v", errT), nil
//		}
//
//		rs, errT := o.Value.GetProperty(globalObjT, atomT)
//		if errT != nil {
//			return NewCommonErrorWithPos(c, "failed to get prop: %v", errT), nil
//		}
//
//		return ConvertToObject(rs), nil
//	case "close":
//		if o.Value == nil {
//			return NewCommonErrorWithPos(c, "invalid object"), nil
//		}
//
//		errT := o.Value.Close()
//		if errT != nil {
//			return NewCommonErrorWithPos(c, "failed to close: %v", errT), nil
//		}
//
//		return Undefined, nil
//	}
//
//	rs1, errT := CallObjectMethodFunc(o, nameA, c.GetArgs()...)
//
//	if errT != nil || tk.IsError(rs1) {
//		rs3 := tk.ReflectCallMethodCompact(o.Value, nameA, ObjectsToI(c.GetArgs())...)
//		return ConvertToObject(rs3), nil
//	}
//
//	return rs1, errT
//
////	return Undefined, NewCommonErrorWithPos(c, "method not found: %v", nameA)
//}
//
//func NewQjsVm(c Call) (Object, error) {
////	argsA := c.GetArgs()
//
//	qjsVmT, errT := quickjs.NewVM()
//
//	if errT != nil {
//		return NewCommonError("faile to create QjsVm object: %v", errT), nil
//	}
//
//	return &QjsVm{Value: qjsVmT}, nil
//}

// EvalMachine represents an Charlang Virtual Machine could run script once or more
type EvalMachine struct {
	// ObjectImpl

	Value *Eval

	Members map[string]Object `json:"-"`
}

func (*EvalMachine) TypeCode() int {
	return 888
}

func (*EvalMachine) TypeName() string {
	return "evalMachine"
}

func (o *EvalMachine) String() string {
	return fmt.Sprintf("%v", o.Value)
}

func (o *EvalMachine) HasMemeber() bool {
	return true
}

func (o *EvalMachine) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return o, nil
	case "toStr":
		return ToStringObject(o), nil
	}

	return CallObjectMethodFunc(o, nameA, argsA...)
}

func (o *EvalMachine) GetValue() Object {
	return o
}

func (o *EvalMachine) SetValue(valueA Object) error {
	switch nv := valueA.(type) {
	case *EvalMachine:
		o.Value = nv.Value
		return nil
	}

	return ErrNotIndexAssignable
}

func (o *EvalMachine) GetMember(idxA string) Object {
	if o.Members == nil {
		return Undefined
	}

	v1, ok := o.Members[idxA]

	if !ok {
		return Undefined
	}

	return v1
}

func (o *EvalMachine) SetMember(idxA string, valueA Object) error {
	if o.Members == nil {
		o.Members = map[string]Object{}
	}

	if IsUndefInternal(valueA) {
		delete(o.Members, idxA)
		return nil
	}

	o.Members[idxA] = valueA

	// return fmt.Errorf("unsupported action(set member)")
	return nil
}

func (o *EvalMachine) Equal(right Object) bool {
	switch v := right.(type) {
	case *EvalMachine:
		return o.Value == v.Value
	}

	return false
}

func (o *EvalMachine) IsFalsy() bool {
	return o.Value == nil
}

func (o *EvalMachine) CanCall() bool {
	return false
}

func (o *EvalMachine) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

func (*EvalMachine) CanIterate() bool {
	return false
}

func (*EvalMachine) Iterate() Iterator {
	return nil
}

func (o *EvalMachine) IndexSet(index, value Object) error {
	return ErrNotIndexAssignable
}

func (o *EvalMachine) IndexGet(index Object) (Object, error) {
	return nil, ErrNotIndexable
}

func (o *EvalMachine) BinaryOp(tok token.Token, right Object) (Object, error) {
	return Undefined, NewCommonError("unsupported type: %T", right)
}

func (o *EvalMachine) CallName(nameA string, c Call) (Object, error) {
	//	tk.Pl("EvalMachine call: %#v", c)
	switch nameA {
	case "eval", "run":
		args := c.GetArgs()

		if len(args) < 1 {
			return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
		}

		codeT, ok := args[0].(*CharCode)

		if ok {
			ctx, cancel := context.WithCancel(context.Background())
			defer cancel()

			lastResultT, errT := o.Value.RunByteCode(ctx, codeT.Value)

			if errT != nil {
				return NewCommonErrorWithPos(c, "%v", errT), nil
			}

			if lastResultT != nil && lastResultT.TypeCode() != 0 {
				//			fmt.Println(lastResultT)
				return lastResultT, nil
			}

			return Undefined, nil
		}

		ctx, cancel := context.WithCancel(context.Background())
		defer cancel()

		lastResultT, _, errT := o.Value.Run(ctx, []byte(args[0].String()))

		if errT != nil {
			return NewCommonErrorWithPos(c, "%v", errT), nil
		}

		if lastResultT != nil && lastResultT.TypeCode() != 0 {
			//			fmt.Println(lastResultT)
			return lastResultT, nil
		}

		return Undefined, nil
	}

	//	rs1, errT := CallObjectMethodFunc(o, nameA, c.GetArgs()...)
	//
	//	if errT != nil || tk.IsError(rs1) {
	//		rs3 := tk.ReflectCallMethodCompact(o.Value, nameA, ObjectsToI(c.GetArgs())...)
	//		return ConvertToObject(rs3), nil
	//	}
	//
	//	return rs1, errT

	return Undefined, NewCommonErrorWithPos(c, "method not found: %v", nameA)
}

func NewEvalMachine(c Call) (Object, error) {
	argsA := c.GetArgs()

	moduleMap := NewModuleMap()

	compilerOptionsT := &CompilerOptions{
		ModulePath:        "", //"(repl)",
		ModuleMap:         moduleMap,
		SymbolTable:       NewSymbolTable(),
		OptimizerMaxCycle: TraceCompilerOptions.OptimizerMaxCycle,
		// TraceParser:       traceParser,
		// TraceOptimizer:    traceOptimizer,
		// TraceCompiler:     traceCompiler,
		// OptimizeConst:     !noOptimizer,
		// OptimizeExpr:      !noOptimizer,
	}

	argsT := ObjectsToS(argsA)

	evalT := NewEvalQuick(map[string]interface{}{"versionG": VersionG, "argsG": argsT, "inputG": argsA, "scriptPathG": "", "runModeG": "eval"}, compilerOptionsT)

	return &EvalMachine{Value: evalT}, nil
}

// MapArray represents an SimpleFlexObject which is an array with some items have keys
type MapArray struct {
	// ObjectImpl

	Value *tk.SimpleFlexObject
}

func (*MapArray) TypeCode() int {
	return 136
}

func (*MapArray) TypeName() string {
	return "mapArray"
}

func (o *MapArray) String() string {
	return fmt.Sprintf("%v", o.Value)
}

func (o *MapArray) HasMemeber() bool {
	return false
}

func (o *MapArray) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return o, nil
	case "toStr":
		return ToStringObject(o), nil
	}

	return CallObjectMethodFunc(o, nameA, argsA...)
}

func (o *MapArray) GetValue() Object {
	return o
}

func (o *MapArray) SetValue(valueA Object) error {
	switch nv := valueA.(type) {
	case *MapArray:
		o.Value = nv.Value
		return nil
	}

	return ErrNotIndexAssignable
}

func (o *MapArray) GetMember(idxA string) Object {
	return Undefined
	//	if o.Members == nil {
	//		return Undefined
	//	}
	//
	//	v1, ok := o.Members[idxA]
	//
	//	if !ok {
	//		return Undefined
	//	}
	//
	//	return v1
}

func (o *MapArray) SetMember(idxA string, valueA Object) error {
	//	if o.Members == nil {
	//		o.Members = map[string]Object{}
	//	}
	//
	//	if IsUndefInternal(valueA) {
	//		delete(o.Members, idxA)
	//		return nil
	//	}
	//
	//	o.Members[idxA] = valueA

	return fmt.Errorf("unsupported action(set member)")
	//	return nil
}

func (o *MapArray) Equal(right Object) bool {
	switch v := right.(type) {
	case *MapArray:
		return o.Value == v.Value
	}

	return false
}

func (o *MapArray) IsFalsy() bool {
	return o.Value == nil
}

func (o *MapArray) CanCall() bool {
	return false
}

func (o *MapArray) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

func (*MapArray) CanIterate() bool {
	return true
}

func (o *MapArray) Iterate() Iterator {
	return &MapArrayIterator{V: o}
}

// Len implements LengthGetter interface.
func (o *MapArray) Len() int {
	return o.Value.Size()
}

func (o *MapArray) IndexSet(index, value Object) error {
	var idxT int

	switch v := index.(type) {
	case Int:
		idxT = int(v)
	case Uint:
		idxT = int(v)
	default:
		keyT := index.String()

		idxT = o.Value.GetIndexByKey(keyT)
	}

	if idxT < 0 || idxT >= o.Value.Size() {
		return NewCommonError("index out of range: %v/%v", idxT, o.Value.Size())
	}

	o.Value.Items[idxT] = value.String()

	return nil
}

func (o *MapArray) IndexGet(index Object) (Object, error) {
	var idxT int

	switch v := index.(type) {
	case Int:
		idxT = int(v)
	case Uint:
		idxT = int(v)
	default:
		keyT := index.String()

		idxT = o.Value.GetIndexByKey(keyT)
	}

	if idxT < 0 || idxT >= o.Value.Size() {
		return nil, NewCommonError("index out of range: %v/%v", idxT, o.Value.Size())
	}

	return ToStringObject(o.Value.Items[idxT]), nil
}

func (o *MapArray) BinaryOp(tok token.Token, right Object) (Object, error) {
	return Undefined, NewCommonError("unsupported type: %T", right)
}

func (o *MapArray) CallName(nameA string, c Call) (Object, error) {
	//	tk.Pl("EvalMachine call: %#v", c)
	switch nameA {
	case "encode":
		args := c.GetArgs()

		//		if len(args) < 1 {
		//			return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
		//		}

		rs := o.Value.Encode(ObjectsToS(args)...)

		return ToStringObject(rs), nil
	}

	return Undefined, NewCommonErrorWithPos(c, "method not found: %v", nameA)
}

func NewMapArray(c Call) (Object, error) {
	argsA := c.GetArgs()

	return &MapArray{Value: tk.NewSimpleFlexObject(ObjectsToI(argsA))}, nil
}

// WebSocket object is used to represent the WebSocket connection
type WebSocket struct {
	ObjectImpl
	Value *websocket.Conn
}

// var _ Object = NewWebSocket()

func NewWebSocket(c Call) (Object, error) {
	argsA := c.GetArgs()

	if len(argsA) < 2 {
		if len(argsA) > 0 { // client mode
			nv1 := argsA[0].String()

			connT, _, errT := websocket.DefaultDialer.Dial(nv1, nil)
			if errT != nil {
				return NewCommonErrorWithPos(c, "failed to dial websocket server: %v", errT), nil
			}

			return &WebSocket{Value: connT}, nil
		}

		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	nv1, ok := argsA[0].(*HttpReq)

	if !ok {
		return NewCommonErrorWithPos(c, "unsupport type of parameter 1: %T", argsA[0]), nil
	}

	nv2, ok := argsA[1].(*HttpResp)

	if !ok {
		return NewCommonErrorWithPos(c, "unsupport type of parameter 2: %T", argsA[1]), nil
	}

	var upgraderT = websocket.Upgrader{}

	conT, errT := upgraderT.Upgrade(nv2.Value, nv1.Value, nil)

	if errT != nil {
		return NewCommonErrorWithPos(c, "failed to upgrade connection: %v", errT), nil
	}

	return &WebSocket{Value: conT}, nil
}

func (*WebSocket) TypeCode() int {
	return 327
}

// TypeName implements Object interface.
func (*WebSocket) TypeName() string {
	return "webSocket"
}

func (o *WebSocket) String() string {
	return fmt.Sprintf("%v", o.Value)
}

// func (o *Mux) SetValue(valueA Object) error {
// 	// o.Value.Reset(int(ToIntObject(valueA)))

// 	return NewCommonError("unsupported action(set value)")
// }

func (o *WebSocket) HasMemeber() bool {
	return false
}

func (o *WebSocket) CallMethod(nameA string, argsA ...Object) (Object, error) {
	return o.CallName(nameA, Call{Args: argsA})
}

func (o *WebSocket) CallName(nameA string, c Call) (Object, error) {
	//	tk.Pl("here0 nameA: %#v", 1)
	switch nameA {
	case "toStr":
		return ToStringObject(o), nil
	case "value":
		return o, nil
	case "localAddr":
		return ToStringObject(o.Value.LocalAddr().String()), nil
	case "readMsg", "getMsg":
		if o.Value == nil {
			return NewCommonErrorWithPos(c, "failed to read message: %v", "connection is nil"), nil
		}

		mt, message, errT := o.Value.ReadMessage()

		if errT != nil {
			return NewCommonErrorWithPos(c, "failed to read message: %v", errT), nil
		}

		return Array{ToIntObject(mt), Bytes(message)}, nil
	case "readTextMsg", "getTextMsg":
		if o.Value == nil {
			return NewCommonErrorWithPos(c, "failed to read message: %v", "connection is nil"), nil
		}

		mt, message, errT := o.Value.ReadMessage()

		if errT != nil {
			return NewCommonErrorWithPos(c, "failed to read message: %v", errT), nil
		}

		if mt == websocket.CloseMessage {
			return NewCommonErrorWithPos(c, "is a close message: %v", mt), nil
		}

		if mt != websocket.TextMessage {
			return NewCommonErrorWithPos(c, "not a text message: %v", mt), nil
		}

		return String(string(message)), nil
	case "readBinMsg", "getBinMsg":
		if o.Value == nil {
			return NewCommonErrorWithPos(c, "failed to read message: %v", "connection is nil"), nil
		}

		mt, message, errT := o.Value.ReadMessage()

		if errT != nil {
			return NewCommonErrorWithPos(c, "failed to read message: %v", errT), nil
		}

		if mt == websocket.CloseMessage {
			return NewCommonErrorWithPos(c, "is a close message: %v", mt), nil
		}

		if mt != websocket.BinaryMessage {
			return NewCommonErrorWithPos(c, "not a binary message: %v", mt), nil
		}

		return Bytes(message), nil
	case "writeMsg", "sendMsg":
		if o.Value == nil {
			return NewCommonErrorWithPos(c, "failed to write message: %v", "connection is nil"), nil
		}

		argsA := c.GetArgs()

		if len(argsA) < 2 {
			return NewCommonErrorWithPos(c, "not enough parameters"), nil
		}

		nv1, ok := argsA[0].(Int)

		if !ok {
			return NewCommonErrorWithPos(c, "unsupport type of parameter 1: %T", argsA[0]), nil
		}

		var nv2b []byte

		nv2, ok := argsA[1].(Bytes)

		if !ok {
			nv2s, ok := argsA[1].(String)

			if !ok {
				return NewCommonErrorWithPos(c, "unsupport type of parameter 2: %T", argsA[1]), nil
			}

			nv2b = []byte(nv2s.String())
		} else {
			nv2b = []byte(nv2)
		}

		errT := o.Value.WriteMessage(int(nv1), nv2b)

		if errT != nil {
			return NewCommonErrorWithPos(c, "failed to write message: %v", errT), nil
		}

		return Undefined, nil
	case "writeTextMsg", "sendTextMsg":
		if o.Value == nil {
			return NewCommonErrorWithPos(c, "failed to write message: %v", "connection is nil"), nil
		}

		argsA := c.GetArgs()

		if len(argsA) < 1 {
			return NewCommonErrorWithPos(c, "not enough parameters"), nil
		}

		nv1 := argsA[0].String()

		errT := o.Value.WriteMessage(websocket.TextMessage, []byte(nv1))

		if errT != nil {
			return NewCommonErrorWithPos(c, "failed to write message: %v", errT), nil
		}

		return Undefined, nil
	case "writeBinMsg", "sendBinMsg":
		if o.Value == nil {
			return NewCommonErrorWithPos(c, "failed to write message: %v", "connection is nil"), nil
		}

		argsA := c.GetArgs()

		if len(argsA) < 1 {
			return NewCommonErrorWithPos(c, "not enough parameters"), nil
		}

		var nv2b []byte

		nv1, ok := argsA[0].(Bytes)

		if !ok {
			nv2s, ok := argsA[0].(String)

			if !ok {
				return NewCommonErrorWithPos(c, "unsupport type of parameter 1: %T", argsA[0]), nil
			}

			nv2b = []byte(nv2s.String())
		} else {
			nv2b = []byte(nv1)
		}

		errT := o.Value.WriteMessage(websocket.BinaryMessage, []byte(nv2b))

		if errT != nil {
			return NewCommonErrorWithPos(c, "failed to write message: %v", errT), nil
		}

		return Undefined, nil
	case "writeCloseMsg", "sendCloseMsg":
		if o.Value == nil {
			return NewCommonErrorWithPos(c, "failed to write message: %v", "connection is nil"), nil
		}

		errT := o.Value.WriteMessage(websocket.CloseMessage, websocket.FormatCloseMessage(websocket.CloseNormalClosure, ""))

		if errT != nil {
			return NewCommonErrorWithPos(c, "failed to write message: %v", errT), nil
		}

		return Undefined, nil
	case "close":
		if o.Value == nil {
			return NewCommonErrorWithPos(c, "failed to close connection: %v", "connection is nil"), nil
		}

		errT := o.Value.Close()

		if errT != nil {
			return NewCommonErrorWithPos(c, "failed to close: %v", errT), nil
		}

		return Undefined, nil
	case "setReadTimeout": // 0 to no timeout
		if o.Value == nil {
			return NewCommonErrorWithPos(c, "connection is nil"), nil
		}

		argsA := c.GetArgs()

		if len(argsA) < 1 {
			return NewCommonErrorWithPos(c, "not enough parameters"), nil
		}

		nv1 := ToFloatQuick(argsA[0])

		errT := o.Value.SetReadDeadline(time.Now().Add(time.Duration(nv1 * float64(time.Second))))

		if errT != nil {
			return NewCommonErrorWithPos(c, "failed to set timeout: %v", errT), nil
		}

		return Undefined, nil
	case "setWriteTimeout": // 0 to no timeout
		if o.Value == nil {
			return NewCommonErrorWithPos(c, "connection is nil"), nil
		}

		argsA := c.GetArgs()

		if len(argsA) < 1 {
			return NewCommonErrorWithPos(c, "not enough parameters"), nil
		}

		nv1 := ToFloatQuick(argsA[0])

		errT := o.Value.SetWriteDeadline(time.Now().Add(time.Duration(nv1 * float64(time.Second))))

		if errT != nil {
			return NewCommonErrorWithPos(c, "failed to set timeout: %v", errT), nil
		}

		return Undefined, nil
	case "setReadLimit": // 0 to no limit
		if o.Value == nil {
			return NewCommonErrorWithPos(c, "connection is nil"), nil
		}

		argsA := c.GetArgs()

		if len(argsA) < 1 {
			return NewCommonErrorWithPos(c, "not enough parameters"), nil
		}

		nv1 := ToIntQuick(argsA[0])

		o.Value.SetReadLimit(int64(nv1))

		return Undefined, nil
	}

	//	rs1, errT := CallObjectMethodFunc(o, nameA, c.GetArgs()...)
	//
	//	if errT != nil || tk.IsError(rs1) {
	//	tk.Pl("here1 nameA: %#v", 2)
	rs3 := tk.ReflectCallMethodCompactWithError(o.Value, nameA, ObjectsToI(c.GetArgs())...)
	//	tk.Pl("here2 nameA: %#v", rs3)
	return ConvertToObject(rs3), nil
	//	}

	//	return rs1, errT
}

func (o *WebSocket) GetValue() Object {
	return Undefined
}

func (o *WebSocket) GetMember(idxA string) Object {
	return Undefined
}

func (o *WebSocket) SetMember(idxA string, valueA Object) error {
	return nil
}

func (*WebSocket) CanIterate() bool { return false }

func (o *WebSocket) Iterate() Iterator {
	return nil
}

func (o *WebSocket) IndexSet(index, value Object) error {

	return ErrNotIndexAssignable
}

func (o *WebSocket) IndexGet(index Object) (Object, error) {
	return Undefined, NewCommonError("not indexable: %v", o.TypeName())
}

func (o *WebSocket) Equal(right Object) bool {
	if v, ok := right.(*WebSocket); ok {
		return v == o
	}

	return false
}

func (o *WebSocket) IsFalsy() bool { return o.Value == nil }

func (o *WebSocket) CanCall() bool { return false }

func (o *WebSocket) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

func (o *WebSocket) BinaryOp(tok token.Token, right Object) (Object, error) {
	return nil, NewOperandTypeError(
		tok.String(),
		o.TypeName(),
		right.TypeName())
}

// comply to io.Closer
func (o *WebSocket) Close() error {
	errT := o.Value.Close()

	if errT != nil {
		return fmt.Errorf("failed to close: %v", errT)
	}

	return nil
}
