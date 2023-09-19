package charlang

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/topxeq/charlang/internal/compat"
	"github.com/topxeq/charlang/token"
)

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

	return Undefined, NewCommonError("unknown method: %v", nameA)
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

	return Undefined, NewCommonError("unknown method: %v", nameA)
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

	return Undefined, NewCommonError("unknown method: %v", nameA)
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

	return Undefined, NewCommonError("unknown method: %v", nameA)
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

	return Undefined, NewCommonError("unknown method: %v", nameA)
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
