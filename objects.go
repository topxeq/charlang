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

	"github.com/topxeq/charlang/internal/compat"
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

// TypeCodes: -1: unknown, undefined: 0, ObjectImpl: 101, Bool: 103, String: 105, Int: 107, Byte: 109, Uint: 111, Char: 113, Float: 115, Array: 131, Map: 133, Bytes: 137, Chars: 139, *ObjectPtr: 151, *ObjectRef: 152, *SyncMap: 153, *Error: 155, *RuntimeError: 157, *Time: 159, *Function: 181, *BuiltinFunction: 183, *CompiledFunction: 185, *CharCode: 191, *Gel: 193, StatusResult: 303, DateTime: 305, *StringBuilder: 307, *BytesBuffer: 308, *Database: 309, *Time: 311, *Location: 313, *Seq: 315, *Mutex: 317, *Mux: 319, *HttpReq: 321, *HttpResp: 323, *Reader: 331, Any: 999

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
	vm    *VM
	This  Object
	args  []Object
	vargs []Object
}

// NewCall creates a new Call struct with the given arguments.
func NewCall(vm *VM, args []Object, vargs ...Object) Call {
	return Call{
		vm:    vm,
		args:  args,
		vargs: vargs,
	}
}

// VM returns the VM of the call.
func (c *Call) VM() *VM {
	return c.vm
}

// Get returns the nth argument. If n is greater than the number of arguments,
// it returns the nth variadic argument.
// If n is greater than the number of arguments and variadic arguments, it
// panics!
func (c *Call) Get(n int) Object {
	if n < len(c.args) {
		return c.args[n]
	}
	return c.vargs[n-len(c.args)]
}

// Len returns the number of arguments including variadic arguments.
func (c *Call) Len() int {
	return len(c.args) + len(c.vargs)
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
	if len(c.args) == 0 {
		if len(c.vargs) == 0 {
			return nil, false
		}
		v := c.vargs[0]
		c.vargs = c.vargs[1:]
		return v, true
	}
	v := c.args[0]
	c.args = c.args[1:]
	return v, true
}

func (c *Call) callArgs() []Object {
	if len(c.args) == 0 {
		return c.vargs
	}
	args := make([]Object, 0, c.Len())
	args = append(args, c.args...)
	args = append(args, c.vargs...)
	return args
}

func (c *Call) GetArgs() []Object {
	if len(c.args) == 0 {
		return c.vargs
	}
	args := make([]Object, 0, c.Len())
	args = append(args, c.args...)
	args = append(args, c.vargs...)
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
	return fmt.Sprintf("%v", o)
	// panic(ErrNotImplemented)
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

	if v, ok := right.(Uint); ok {
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
func (o Bool) IndexGet(index Object) (value Object, err error) {
	switch v := index.(type) {
	case String:
		strT := v.Value

		if strT == "v" || strT == "value" {
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
	case Uint:
		return Uint(o) == v
	case Float:
		return Float(o) == v
	case Char:
		return o == Int(v)
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
func (Int) CanIterate() bool { return false }

// Iterate implements Object interface.
func (Int) Iterate() Iterator { return nil }

// IndexSet implements Object interface.
func (Int) IndexSet(index, value Object) error {
	return ErrNotIndexAssignable
}

// IndexGet implements Object interface.
func (o Int) IndexGet(index Object) (Object, error) {
	switch v := index.(type) {
	case String:
		strT := v.Value

		if strT == "v" || strT == "value" {
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
	case Uint:
		return Uint(o).BinaryOp(tok, right)
	case Float:
		return Float(o).BinaryOp(tok, right)
	case Char:
		switch tok {
		case token.Add:
			return Char(o) + v, nil
		case token.Sub:
			return Char(o) - v, nil
		case token.Less:
			return Bool(o < Int(v)), nil
		case token.LessEq:
			return Bool(o <= Int(v)), nil
		case token.Greater:
			return Bool(o > Int(v)), nil
		case token.GreaterEq:
			return Bool(o >= Int(v)), nil
		}
	case Byte:
		switch tok {
		case token.Add:
			return Byte(o) + v, nil
		case token.Sub:
			return Byte(o) - v, nil
		case token.Less:
			return Bool(o < Int(v)), nil
		case token.LessEq:
			return Bool(o <= Int(v)), nil
		case token.Greater:
			return Bool(o > Int(v)), nil
		case token.GreaterEq:
			return Bool(o >= Int(v)), nil
		}
	case Bool:
		if v {
			right = Int(1)
		} else {
			right = Int(0)
		}
		return o.BinaryOp(tok, right)
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
	case Float:
		return Float(o) == v
	case Char:
		return o == Uint(v)
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
func (Uint) CanIterate() bool { return false }

// Iterate implements Object interface.
func (Uint) Iterate() Iterator { return nil }

// IndexSet implements Object interface.
func (Uint) IndexSet(index, value Object) error {
	return ErrNotIndexAssignable
}

// IndexGet implements Object interface.
func (o Uint) IndexGet(index Object) (Object, error) {
	switch v := index.(type) {
	case String:
		strT := v.Value

		if strT == "v" || strT == "value" {
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
	case Float:
		return Float(o).BinaryOp(tok, right)
	case Char:
		switch tok {
		case token.Add:
			return Char(o) + v, nil
		case token.Sub:
			return Char(o) - v, nil
		case token.Less:
			return Bool(o < Uint(v)), nil
		case token.LessEq:
			return Bool(o <= Uint(v)), nil
		case token.Greater:
			return Bool(o > Uint(v)), nil
		case token.GreaterEq:
			return Bool(o >= Uint(v)), nil
		}
	case Byte:
		switch tok {
		case token.Add:
			return Byte(o) + v, nil
		case token.Sub:
			return Byte(o) - v, nil
		case token.Less:
			return Bool(o < Uint(v)), nil
		case token.LessEq:
			return Bool(o <= Uint(v)), nil
		case token.Greater:
			return Bool(o > Uint(v)), nil
		case token.GreaterEq:
			return Bool(o >= Uint(v)), nil
		}
	case Bool:
		if v {
			right = Uint(1)
		} else {
			right = Uint(0)
		}
		return o.BinaryOp(tok, right)
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
		strT := v.Value

		if strT == "v" || strT == "value" {
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
			if v == 0 {
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
	case Uint:
		return o.BinaryOp(tok, Float(v))
	case Char:
		return o.BinaryOp(tok, Float(v))
	case Byte:
		return o.BinaryOp(tok, Float(v))
	case Bool:
		if v {
			right = Float(1)
		} else {
			right = Float(0)
		}
		return o.BinaryOp(tok, right)
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
		strT := v.Value

		if strT == "v" || strT == "value" {
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
		switch tok {
		case token.Add:
			return o + Char(v), nil
		case token.Sub:
			return o - Char(v), nil
		case token.Less:
			return Bool(Int(o) < v), nil
		case token.LessEq:
			return Bool(Int(o) <= v), nil
		case token.Greater:
			return Bool(Int(o) > v), nil
		case token.GreaterEq:
			return Bool(Int(o) >= v), nil
		}
	case Byte:
		switch tok {
		case token.Add:
			return Byte(o) + v, nil
		case token.Sub:
			return Byte(o) - v, nil
		case token.Less:
			return Bool(o < Char(v)), nil
		case token.LessEq:
			return Bool(o <= Char(v)), nil
		case token.Greater:
			return Bool(o > Char(v)), nil
		case token.GreaterEq:
			return Bool(o >= Char(v)), nil
		}
	case Uint:
		switch tok {
		case token.Add:
			return o + Char(v), nil
		case token.Sub:
			return o - Char(v), nil
		case token.Less:
			return Bool(Uint(o) < v), nil
		case token.LessEq:
			return Bool(Uint(o) <= v), nil
		case token.Greater:
			return Bool(Uint(o) > v), nil
		case token.GreaterEq:
			return Bool(Uint(o) >= v), nil
		}
	case Bool:
		if v {
			right = Char(1)
		} else {
			right = Char(0)
		}
		return o.BinaryOp(tok, right)
	case String:
		if tok == token.Add {
			var sb strings.Builder
			sb.Grow(len(v.Value) + 4)
			sb.WriteRune(rune(o))
			sb.WriteString(v.Value)
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
		strT := v.Value

		if strT == "v" || strT == "value" {
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
	case Uint:
		return Uint(o).BinaryOp(tok, right)
	case Float:
		return Float(o).BinaryOp(tok, right)
	case Char:
		switch tok {
		case token.Add:
			return Char(o) + v, nil
		case token.Sub:
			return Char(o) - v, nil
		case token.Less:
			return Bool(Char(o) < v), nil
		case token.LessEq:
			return Bool(Char(o) <= v), nil
		case token.Greater:
			return Bool(Char(o) > v), nil
		case token.GreaterEq:
			return Bool(Char(o) >= v), nil
		}
	case Bool:
		if v {
			right = Int(1)
		} else {
			right = Int(0)
		}
		return o.BinaryOp(tok, right)
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
type String struct {
	ObjectImpl
	Value string

	Members map[string]Object
	// Methods map[string]*Function
}

var _ LengthGetter = ToStringObject("")

func (String) TypeCode() int {
	return 105
}

// TypeName implements Object interface.
func (String) TypeName() string {
	return "string"
}

func (o String) String() string {
	return o.Value // tk.ToJSONX(o)
}

func (o String) HasMemeber() bool {
	return true
}

func (o String) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return o, nil
	case "toStr":
		return ToStringObject(o), nil
	case "contains":
		if len(argsA) < 1 {
			return NewCommonError("not enough parameters"), nil
		}

		return Bool(strings.Contains(o.Value, argsA[0].String())), nil
	}

	return CallObjectMethodFunc(o, nameA, argsA...)

	// return Undefined, NewCommonError("unknown method: %v", nameA)
}

func (o String) GetValue() Object {
	return o
}

func (o String) GetMember(idxA string) Object {
	if o.Members == nil {
		return Undefined
	}

	v1, ok := o.Members[idxA]

	if !ok {
		return Undefined
	}

	return v1
}

func (o String) SetMember(idxA string, valueA Object) error {
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

func (o String) Copy() Object {
	return String{Value: o.Value, Members: o.Members} // , Methods: o.Methods
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
	// 	if strT == "v" || strT == "value" {
	// 		o.Value = value.String()
	// 		return nil
	// 	}

	// 	return o.SetMember(idxT.Value, value)
	// }

	return ErrNotIndexAssignable
}

// IndexGet represents string values and implements Object interface.
func (o String) IndexGet(index Object) (Object, error) {
	var idx int
	switch v := index.(type) {
	case Byte:
		idx = int(v)
	case Int:
		idx = int(v)
	case Uint:
		idx = int(v)
	case Char:
		idx = int(v)
	case String:
		strT := v.Value

		if strT == "v" || strT == "value" {
			return ToStringObject(o.Value), nil
		}

		rs := o.GetMember(strT)

		if !IsUndefInternal(rs) {
			return rs, nil
		}

		return GetObjectMethodFunc(o, strT)
	default:
		return nil, NewIndexTypeError("int|uint|char|string", index.TypeName())
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
		return o.Value == string(v)
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
		return String{Value: o.Value + right.String()}, nil
	}

	return nil, NewOperandTypeError(
		tok.String(),
		o.TypeName(),
		right.TypeName())
}

// Len implements LengthGetter interface.
func (o String) Len() int {
	return len(o.Value)
}

// Format implements fmt.Formatter interface.
func (o String) Format(s fmt.State, verb rune) {
	format := compat.FmtFormatString(s, verb)
	fmt.Fprintf(s, format, o.Value)
}

func ToStringObject(argA interface{}) String {
	switch nv := argA.(type) {
	case String:
		return String{Value: nv.Value}
	case Object:
		return String{Value: nv.String()}
	case string:
		return String{Value: nv}
	case *strings.Builder:
		return String{Value: nv.String()}
	case strings.Builder:
		return String{Value: nv.String()}
	case *bytes.Buffer:
		return String{Value: string(nv.Bytes())}
	case bytes.Buffer:
		return String{Value: string(nv.Bytes())}
	case nil:
		return String{Value: ""}
	case []byte:
		return String{Value: string(nv)}
	case *tk.Seq:
		return String{Value: nv.String()}
	case *sync.RWMutex:
		return String{Value: fmt.Sprintf("%v", nv)}
	}

	return String{Value: fmt.Sprintf("%v", argA)}
}

func FromStringObject(argA String) string {
	return argA.Value
}

func ToIntObject(argA interface{}, defaultA ...int) Int {
	defaultT := 0
	if len(defaultA) > 0 {
		defaultT = defaultA[0]
	}
	switch nv := argA.(type) {
	case String:
		return Int(tk.StrToInt(nv.Value, defaultT))
	case Int:
		return nv
	case Float:
		return Int(nv)
	case Byte:
		return Int(nv)
	case Char:
		return Int(nv)
	case Object:
		return Int(tk.StrToInt(nv.String(), defaultT))
	case string:
		return Int(tk.StrToInt(nv, defaultT))
	case int:
		return Int(nv)
	case int64:
		return Int(nv)
	case int32:
		return Int(nv)
	case uint8:
		return Int(nv)
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
		strT := v.Value

		if strT == "v" || strT == "value" {
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
		return string(o) == v.Value
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
			return append(o, v.Value...), nil
		case token.Less:
			return Bool(string(o) < v.Value), nil
		case token.LessEq:
			return Bool(string(o) <= v.Value), nil
		case token.Greater:
			return Bool(string(o) > v.Value), nil
		case token.GreaterEq:
			return Bool(string(o) >= v.Value), nil
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
		strT := v.Value

		if strT == "v" || strT == "value" {
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
		return string(o) == v.Value
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

// Function represents a function object and implements Object interface.
type Function struct {
	ObjectImpl
	Name    string
	Value   func(args ...Object) (Object, error)
	ValueEx func(Call) (Object, error)

	Members map[string]Object
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
func (*Function) IsFalsy() bool { return false }

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

	Members map[string]Object
	Methods map[string]*Function
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
func (*BuiltinFunction) IsFalsy() bool { return false }

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

	fNameT := o.Name + "." + nv.Value

	if o.Methods == nil {
		o.Methods = map[string]*Function{}
	}

	if o.Members == nil {
		o.Members = map[string]Object{}
	}

	switch fNameT {
	case "any.new":
		fT, ok := o.Methods["any.new"]
		if !ok {
			o.Methods["any.new"] = &Function{
				Name: "any.new",
				ValueEx: func(c Call) (Object, error) {
					return builtinAnyFunc(c)
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

					return &Database{DBType: nv0.Value, DBConnectString: nv1.String(), Value: rsT.(*sql.DB)}, nil
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

					return ToStringObject(sqltk.FormatSQLValue(nv0)), nil
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
						return NewCommonError("not enough paramters"), nil
					}

					nv0, ok := args[0].(Array)
					if !ok {
						return NewCommonError("invalid paramter 1"), nil
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
	case "statusResult.success":
		fT, ok := o.Methods["statusResult.success"]
		if !ok {
			o.Methods["statusResult.success"] = &Function{
				Name: "statusResult.success",
				Value: func(args ...Object) (Object, error) {
					if len(args) < 1 {
						return &StatusResultSuccess, nil
					}

					return &StatusResult{Status: "success", Value: args[0].String()}, nil
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
						return &StatusResultFail, nil
					}

					return &StatusResult{Status: "fail", Value: args[0].String()}, nil
				}}
			fT = o.Methods["statusResult.fail"]
		}

		return fT, nil
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

	strT := nv.Value

	if strT == "v" || strT == "value" {
		return o, nil
	}

	rs := o.GetMember(strT)

	if !IsUndefInternal(rs) {
		return rs, nil
	}

	return GetObjectMethodFunc(o, strT)

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

func (o Array) HasMemeber() bool {
	return false
}

func (o Array) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return o, nil
	case "toStr":
		return ToStringObject(o), nil
	}

	return CallObjectMethodFunc(o, nameA, argsA...)
	// return Undefined, NewCommonError("unknown method: %v", nameA)
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
	case String:
		strT := v.Value

		if strT == "v" || strT == "value" {
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

	Members map[string]Object
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

func (o Map) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return o, nil
	case "toStr":
		return ToStringObject(o), nil
	}

	return CallObjectMethodFunc(o, nameA, argsA...)
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

	Members map[string]Object
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

	Members map[string]Object
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

	if strT == "v" || strT == "value" {
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
		strT := idxT.Value
		if strT == "v" || strT == "value" {
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

	Members map[string]Object
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

	if strT == "v" || strT == "value" {
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
		strT := idxT.Value
		if strT == "v" || strT == "value" {
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

	Members map[string]Object
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
	switch nameA {
	case "value":
		return o, nil
	case "toStr":
		return ToStringObject(o), nil
	}

	fn, ok := methodTableForTime[strings.ToLower(nameA)]
	if !ok {
		return CallObjectMethodFunc(o, nameA, argsA...)
		// return Undefined, ErrInvalidIndex.NewError(nameA)
	}

	return fn(o, &Call{args: argsA})

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
		strT := idxT.Value
		if strT == "v" || strT == "value" {
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
	// tk.Pl("Time IndexGet: %v", index)

	v, ok := index.(String)
	if !ok {
		return Undefined, NewIndexTypeError("string", index.TypeName())
	}

	// For simplicity, we use method call for now. As getters are deprecated, we
	// will return callable object in the future here.

	switch v.Value {
	case "Date", "Clock", "UTC", "Unix", "UnixNano", "Year", "Month", "Day",
		"Hour", "Minute", "Second", "Nanosecond", "IsZero", "Local", "Location",
		"YearDay", "Weekday", "ISOWeek", "Zone":
		return o.CallName(v.Value, Call{})
	case "value":
		return o, nil
	}

	strT := v.Value

	if strT == "v" || strT == "value" {
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

	fn, ok := methodTableForTime[strings.ToLower(name)]
	if !ok {
		return Undefined, ErrInvalidIndex.NewError(name)
	}
	return fn(o, &c)
}

var methodTableForTime = map[string]func(*Time, *Call) (Object, error){
	"add": func(o *Time, c *Call) (Object, error) {
		if err := c.CheckLen(1); err != nil {
			return Undefined, err
		}
		d, ok := ToGoInt64(c.Get(0))
		if !ok {
			return newArgTypeErr("1st", "int", c.Get(0).TypeName())
		}
		return timeAdd(o, d), nil
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
	"adddate": func(o *Time, c *Call) (Object, error) {
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

		var format string = ""
		var ok bool
		if c.Len() < 1 {
			format = tk.TimeFormat
		} else {
			format, ok = ToGoString(c.Get(0))
			if !ok {
				return newArgTypeErr("1st", "string", c.Get(0).TypeName())
			}
		}

		return timeFormat(o, format), nil
	},
	"appendformat": func(o *Time, c *Call) (Object, error) {
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
	"getinfo": func(o *Time, c *Call) (Object, error) {
		if err := c.CheckLen(0); err != nil {
			return Undefined, err
		}
		zoneT, offsetT := o.Value.Zone()
		return Map{"Time": o, "Formal": String{Value: o.Value.Format(tk.TimeFormat)}, "Compact": String{Value: o.Value.Format(tk.TimeFormatCompact)}, "Full": String{Value: fmt.Sprintf("%v", o.Value)}, "Year": Int(o.Value.Year()), "Month": Int(o.Value.Month()), "Day": Int(o.Value.Day()), "Hour": Int(o.Value.Hour()), "Minute": Int(o.Value.Minute()), "Second": Int(o.Value.Second()), "Zone": ConvertToObject(zoneT), "Offset": ConvertToObject(offsetT), "UnixNano": Int(o.Value.UnixNano()), "WeekDay": Int(o.Value.Weekday()), "NanoSec": Int(o.Value.Nanosecond()), "MilliSec": Int(o.Value.Nanosecond() / 1000000)}, nil
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
	"unixnano": func(o *Time, c *Call) (Object, error) {
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
	"nanosecond": func(o *Time, c *Call) (Object, error) {
		if err := c.CheckLen(0); err != nil {
			return Undefined, err
		}
		return Int(o.Value.Nanosecond()), nil
	},
	"iszero": func(o *Time, c *Call) (Object, error) {
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
	"yearday": func(o *Time, c *Call) (Object, error) {
		if err := c.CheckLen(0); err != nil {
			return Undefined, err
		}
		return Int(o.Value.YearDay()), nil
	},
	"weekday": func(o *Time, c *Call) (Object, error) {
		if err := c.CheckLen(0); err != nil {
			return Undefined, err
		}
		return Int(o.Value.Weekday()), nil
	},
	"isoweek": func(o *Time, c *Call) (Object, error) {
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
		v, err := time.Parse(time.RFC3339Nano, string(o.Value))
		if err != nil {
			v, err = time.Parse(time.RFC3339, string(o.Value))
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
		o, err = loadLocationFunc(v.Value)
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

	Members map[string]Object
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

	Members map[string]Object
	Methods map[string]*Function
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
	return fmt.Sprintf("%v", o)
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

func (o *Database) IsFalsy() bool { return false }

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

	strT := nv.Value

	if strT == "v" || strT == "value" {
		return o, nil
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
						return NewCommonError("not enough paramters"), nil
					}

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

					o.DBType = nv0.Value
					o.DBConnectString = nv1.Value
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

	// if strT == "v" || strT == "value" {
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
		strT := idxT.Value
		if strT == "v" || strT == "value" {
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

	Members map[string]Object
	Methods map[string]*Function
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

		mapT := tk.JSONToMapStringString(nv.Value)
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

	return &StatusResult{Status: nv0.Value, Value: nv1.Value}, nil

}

func (o *StatusResult) IndexGet(index Object) (Object, error) {
	nv, ok := index.(String)
	if !ok {
		return nil, ErrNotIndexable
	}

	fNameT := nv.Value

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

					o.Status = nv.Value
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

					o.Value = nv.Value
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

						mapT := tk.JSONToMapStringString(nv.Value)
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

					o.Status = nv0.Value
					o.Value = nv1.Value
					return &StatusResultSuccess, nil
				}}
			fT = o.Methods["fromString"]
		}
		return fT, nil
	}

	strT := nv.Value

	// if strT == "v" || strT == "value" {
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
		strT := idxT.Value
		if strT == "v" || strT == "value" {
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

	Members map[string]Object
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
	return tk.ToStr(o.Value)
}

func (o *Any) SetValue(valueA Object) error {
	rs, errT := builtinAnyFunc(Call{args: []Object{valueA}})

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
func (o *Any) IsFalsy() bool { return false }

// IndexGet implements Object interface.
func (o *Any) IndexGet(index Object) (Object, error) {
	switch v := index.(type) {
	case String:
		strT := v.Value

		if strT == "v" || strT == "value" {
			return o, nil
		}

		rs := o.GetMember(strT)

		if !IsUndefInternal(rs) {
			return rs, nil
		}

		return GetObjectMethodFunc(o, strT)
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
		strT := idxT.Value
		if strT == "v" || strT == "value" {
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

	// 	rs1, errT := fnT(Call{args: Array{o, nv, value}})

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

	Members map[string]Object
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
	return fmt.Sprintf("%v", *(o.Value))
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

func (o *StringBuilder) IsFalsy() bool { return false }

func (*StringBuilder) CanCall() bool { return false }

func (*StringBuilder) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

func (*StringBuilder) CanIterate() bool { return false }

func (*StringBuilder) Iterate() Iterator { return nil }

func (o *StringBuilder) IndexGet(index Object) (value Object, err error) {
	switch v := index.(type) {
	case String:
		strT := v.Value

		if strT == "v" || strT == "value" {
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

	// 	if strT == "v" || strT == "value" {
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
		strT := idxT.Value
		if strT == "v" || strT == "value" {
			if nv, ok := value.(String); ok {
				o.Value.Reset()
				o.Value.WriteString(nv.Value)
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

	Members map[string]Object
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

func (o *BytesBuffer) IsFalsy() bool { return false }

func (*BytesBuffer) CanCall() bool { return false }

func (*BytesBuffer) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

func (*BytesBuffer) CanIterate() bool { return false }

func (*BytesBuffer) Iterate() Iterator { return nil }

func (o *BytesBuffer) IndexGet(index Object) (value Object, err error) {
	switch v := index.(type) {
	case String:
		strT := v.Value

		if strT == "v" || strT == "value" {
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

	// 	if strT == "v" || strT == "value" {
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
		strT := idxT.Value
		if strT == "v" || strT == "value" {
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

// ObjectRef represents a reference variable.
// always refer to an Object(i.e. *Object)
type ObjectRef struct {
	ObjectImpl
	Value *Object

	Members map[string]Object
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

	Members map[string]Object
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
		strT := idxT.Value
		if strT == "v" || strT == "value" {
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
		strT := v.Value

		if strT == "v" || strT == "value" {
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
		return o.Value == v.Value
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
		return String{Value: o.Value + right.String()}, nil
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
		return &MutableString{Value: nv.Value}
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

	Members map[string]Object
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
		return builtinAnyFunc(Call{args: []Object{o}})
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
		strT := idxT.Value
		if strT == "v" || strT == "value" {
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
		strT := v.Value

		if strT == "v" || strT == "value" {
			// return builtinAnyFunc(Call{args: []Object{o}})
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
type Mutex struct {
	ObjectImpl
	Value *(sync.RWMutex)

	Members map[string]Object
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
		return builtinAnyFunc(Call{args: []Object{o}})
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
		strT := idxT.Value
		// if strT == "v" || strT == "value" {
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
		strT := v.Value

		if strT == "v" || strT == "value" {
			return builtinAnyFunc(Call{args: []Object{o}})
			// return ToIntObject(o.Value), nil
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

	Members map[string]Object
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
		return builtinAnyFunc(Call{args: []Object{o}})
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
		strT := idxT.Value
		// if strT == "v" || strT == "value" {
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
		strT := v.Value

		if strT == "v" || strT == "value" {
			return builtinAnyFunc(Call{args: []Object{o}})
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

	Members map[string]Object
}

// var _ Object = NewHttpReq()

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
		return builtinAnyFunc(Call{args: []Object{o}})
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
		strT := idxT.Value
		// if strT == "v" || strT == "value" {
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
		strT := v.Value

		if strT == "v" || strT == "value" {
			return builtinAnyFunc(Call{args: []Object{o}})
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

	Members map[string]Object
}

// var _ Object = NewHttpReq()

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
		return builtinAnyFunc(Call{args: []Object{o}})
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
		strT := idxT.Value
		// if strT == "v" || strT == "value" {
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
		strT := v.Value

		if strT == "v" || strT == "value" {
			return builtinAnyFunc(Call{args: []Object{o}})
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

// Reader object is used for represent io.Reader type value
type Reader struct {
	ObjectImpl
	Value *io.Reader

	Members map[string]Object
}

var _ Object = NewReader()

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

// func (o *Reader) SetValue(valueA Object) error {
// 	// o.Value.Reset(int(ToIntObject(valueA)))

// 	return NewCommonError("unsupported action(set value)")
// }

func (o *Reader) HasMemeber() bool {
	return true
}

func (o *Reader) CallMethod(nameA string, argsA ...Object) (Object, error) {
	switch nameA {
	case "value":
		return builtinAnyFunc(Call{args: []Object{o}})
	case "toStr":
		return ToStringObject(o), nil
	}

	return CallObjectMethodFunc(o, nameA, argsA...)
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
		strT := idxT.Value
		// if strT == "v" || strT == "value" {
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
		strT := v.Value

		if strT == "v" || strT == "value" {
			return builtinAnyFunc(Call{args: []Object{o}})
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

func NewReader(argsA ...Object) *Reader {
	if len(argsA) < 1 {
		return nil
	}

	return &Reader{Value: argsA[0].(*Reader).Value}
}

// CharCode object is used for represent thent io.Reader type value
type CharCode struct {
	ObjectImpl
	Source string
	Value  *Bytecode

	CompilerOptions *CompilerOptions

	LastError string

	Members map[string]Object
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
	return fmt.Sprintf("%v", o.Value)
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
		return builtinAnyFunc(Call{args: []Object{o}})
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
		strT := idxT.Value
		// if strT == "v" || strT == "value" {
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
		strT := v.Value

		if strT == "v" || strT == "value" {
			return builtinAnyFunc(Call{args: []Object{o}})
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

func (o *CharCode) CanCall() bool { return false }

func (o *CharCode) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

func (o *CharCode) BinaryOp(tok token.Token, right Object) (Object, error) {
	return nil, NewOperandTypeError(
		tok.String(),
		o.TypeName(),
		right.TypeName())
}

func NewCharCode(srcA string, optsA ...*CompilerOptions) *CharCode {
	// byteCodeT := QuickCompile(srcA, optsA...) // quickCompile(tk.ToStr(argsA[0])) //

	// if tk.IsError(byteCodeT) {
	// 	return nil
	// }

	var compilerOptionsT *CompilerOptions
	if len(optsA) > 0 {
		compilerOptionsT = optsA[0]
	} else {
		compilerOptionsT = &DefaultCompilerOptions
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
	Source string
	Value  *CharCode

	Members map[string]Object
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
	return fmt.Sprintf("%v", o.Value)
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
		return builtinAnyFunc(Call{args: []Object{o}})
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
		strT := idxT.Value
		// if strT == "v" || strT == "value" {
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
		strT := v.Value

		if strT == "v" || strT == "value" {
			return builtinAnyFunc(Call{args: []Object{o}})
			// return ToIntObject(o.Value), nil
		}

		rs := o.GetMember(strT)

		if !IsUndefInternal(rs) {
			return rs, nil
		}

		// return nil, ErrIndexOutOfBounds
		rs1, errT := GetObjectMethodFunc(o, strT)

		if errT != nil || IsUndefInternal(rs1) {

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

	nv, ok := argsA[0].(*CharCode)

	if !ok {
		return Undefined, fmt.Errorf("invalid input type")
	}

	if nv == nil {
		return Undefined, fmt.Errorf("not compiled")
	}

	return &Gel{Value: nv}, nil
}
