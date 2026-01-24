package fmt

import (
	"reflect"
	"strconv"

	"github.com/topxeq/charlang"
	"github.com/topxeq/charlang/registry"
)

func init() {
	registry.RegisterAnyConverter(reflect.TypeOf((*scanArg)(nil)),
		func(in interface{}) (interface{}, bool) {
			sa := in.(*scanArg)
			if sa.argValue != nil {
				return sa.Arg(), true
			}
			return charlang.Undefined, false
		},
	)
}

// ScanArg is an interface that wraps methods required to scan argument with
// scan functions.
type ScanArg interface {
	// Set sets status of scanning. It is set false before scanning and true
	// after scanning if argument is scanned.
	Set(bool)
	// Arg must return either a pointer to a basic Go type or implementations of
	// fmt.Scanner interface.
	Arg() interface{}
	// Value must return scanned, non-nil Charlang Object.
	Value() charlang.Object
}

// argValue is an interface implemented by the basic scannable types and used by
// scanArg type.
type argValue interface {
	Arg() interface{}
	Value() charlang.Object
}

// scanArg implements charlang.Object and ScanArg interfaces to provide arguments to
// scan functions.
// "Value" selector in Charlang scripts gives the scanned value if scan was successful.
type scanArg struct {
	charlang.ObjectImpl
	argValue
	ok bool
}

var _ ScanArg = (*scanArg)(nil)

func (*scanArg) TypeName() string { return "scanArg" }

func (o *scanArg) String() string { return "<scanArg>" }

func (o *scanArg) IsFalsy() bool { return !o.ok }

func (o *scanArg) IndexGet(index charlang.Object) (charlang.Object, error) {
	if o.ok && index.String() == "Value" {
		return o.Value(), nil
	}
	return charlang.Undefined, nil
}

func (o *scanArg) Set(scanned bool) { o.ok = scanned }

func newScanArgFunc(c charlang.Call) (charlang.Object, error) {
	typ := "string"
	if c.Len() > 0 {
		v := c.Get(0)
		if b, ok := v.(*charlang.BuiltinFunction); ok {
			typ = b.Name
		} else {
			typ = v.String()
		}
	}
	var scan scanArg
	switch typ {
	case "string":
		scan.argValue = &stringType{}
	case "int":
		scan.argValue = &intType{}
	case "uint":
		scan.argValue = &uintType{}
	case "float":
		scan.argValue = &floatType{}
	case "bool":
		scan.argValue = &boolType{}
	case "char":
		scan.argValue = &charType{}
	case "bytes":
		scan.argValue = &bytesType{}
	default:
		return nil, charlang.ErrType.NewError(strconv.Quote(typ), "not implemented")
	}
	return &scan, nil
}

type stringType struct {
	v string
}

func (st *stringType) Arg() interface{} {
	return &st.v
}

func (st *stringType) Value() charlang.Object {
	return charlang.String{Value: st.v}
}

type bytesType struct {
	v []byte
}

func (bt *bytesType) Arg() interface{} {
	return &bt.v
}

func (bt *bytesType) Value() charlang.Object {
	return charlang.Bytes(bt.v)
}

type intType struct {
	v int64
}

func (it *intType) Arg() interface{} {
	return &it.v
}

func (it *intType) Value() charlang.Object {
	return charlang.Int(it.v)
}

type uintType struct {
	v uint64
}

func (ut *uintType) Arg() interface{} {
	return &ut.v
}

func (ut *uintType) Value() charlang.Object {
	return charlang.Uint(ut.v)
}

type floatType struct {
	v float64
}

func (ft *floatType) Arg() interface{} {
	return &ft.v
}

func (ft *floatType) Value() charlang.Object {
	return charlang.Float(ft.v)
}

type charType struct {
	v rune
}

func (ct *charType) Arg() interface{} {
	return &ct.v
}

func (ct *charType) Value() charlang.Object {
	return charlang.Char(ct.v)
}

type boolType struct {
	v bool
}

func (bt *boolType) Arg() interface{} {
	return &bt.v
}

func (bt *boolType) Value() charlang.Object {
	return charlang.Bool(bt.v)
}
