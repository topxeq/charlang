package json

import (
	"encoding/json"
	"fmt"
	"reflect"

	"github.com/topxeq/charlang"
	"github.com/topxeq/charlang/registry"
)

func init() {
	registry.RegisterObjectConverter(reflect.TypeOf(json.RawMessage(nil)),
		func(in interface{}) (interface{}, bool) {
			rm := in.(json.RawMessage)
			if rm == nil {
				return &RawMessage{Value: charlang.Bytes{}}, true
			}
			return &RawMessage{Value: rm}, true
		},
	)

	registry.RegisterAnyConverter(reflect.TypeOf((*RawMessage)(nil)),
		func(in interface{}) (interface{}, bool) {
			rm := in.(*RawMessage)
			return json.RawMessage(rm.Value), true
		},
	)
}

// char:doc
// ## Types
// ### encoderOptions
//
// Go Type
//
// ```go
// // EncoderOptions represents the encoding options (quote, html escape) to
// // Marshal any Object.
// type EncoderOptions struct {
// 	charlang.ObjectImpl
// 	Value      charlang.Object
// 	Quote      bool
// 	EscapeHTML bool
// }
// ```

// EncoderOptions represents the encoding options (quote, html escape) to
// Marshal any Object.
type EncoderOptions struct {
	charlang.ObjectImpl
	Value      charlang.Object
	Quote      bool
	EscapeHTML bool
}

// TypeName implements charlang.Object interface.
func (eo *EncoderOptions) TypeName() string {
	return "encoderOptions"
}

// String implements charlang.Object interface.
func (eo *EncoderOptions) String() string {
	return fmt.Sprintf("encoderOptions{Quote:%t EscapeHTML:%t Value:%s}",
		eo.Quote, eo.EscapeHTML, eo.Value)
}

// char:doc
// #### encoderOptions Getters
//
//
// | Selector  | Return Type |
// |:----------|:------------|
// |.Value     | any         |
// |.Quote     | bool        |
// |.EscapeHTML| bool        |

// IndexGet implements charlang.Object interface.
func (eo *EncoderOptions) IndexGet(index charlang.Object) (ret charlang.Object, err error) {
	switch index.String() {
	case "Value":
		ret = eo.Value
	case "Quote":
		ret = charlang.Bool(eo.Quote)
	case "EscapeHTML":
		ret = charlang.Bool(eo.EscapeHTML)
	default:
		ret = charlang.Undefined
	}
	return
}

// char:doc
// #### encoderOptions Setters
//
//
// | Selector  | Value Type  |
// |:----------|:------------|
// |.Value     | any         |
// |.Quote     | bool        |
// |.EscapeHTML| bool        |

// IndexSet implements charlang.Object interface.
func (eo *EncoderOptions) IndexSet(index, value charlang.Object) error {
	switch index.String() {
	case "Value":
		eo.Value = value
	case "Quote":
		eo.Quote = !value.IsFalsy()
	case "EscapeHTML":
		eo.EscapeHTML = !value.IsFalsy()
	default:
		return charlang.ErrInvalidIndex
	}
	return nil
}

// char:doc
// ## Types
// ### rawMessage
//
// Go Type
//
// ```go
// // RawMessage represents raw encoded json message to directly use value of
// // MarshalJSON without encoding.
// type RawMessage struct {
// 	charlang.ObjectImpl
// 	Value []byte
// }
// ```

// RawMessage represents raw encoded json message to directly use value of
// MarshalJSON without encoding.
type RawMessage struct {
	charlang.ObjectImpl
	Value []byte
}

var _ Marshaler = (*RawMessage)(nil)

// TypeName implements charlang.Object interface.
func (rm *RawMessage) TypeName() string {
	return "rawMessage"
}

// String implements charlang.Object interface.
func (rm *RawMessage) String() string {
	return string(rm.Value)
}

// MarshalJSON implements Marshaler interface and returns rm as the JSON
// encoding of rm.Value.
func (rm *RawMessage) MarshalJSON() ([]byte, error) {
	if rm == nil || rm.Value == nil {
		return []byte("null"), nil
	}
	return rm.Value, nil
}

// char:doc
// #### rawMessage Getters
//
//
// | Selector  | Return Type |
// |:----------|:------------|
// |.Value     | bytes       |

// IndexGet implements charlang.Object interface.
func (rm *RawMessage) IndexGet(index charlang.Object) (ret charlang.Object, err error) {
	switch index.String() {
	case "Value":
		ret = charlang.Bytes(rm.Value)
	default:
		ret = charlang.Undefined
	}
	return
}

// char:doc
// #### rawMessage Setters
//
//
// | Selector  | Value Type  |
// |:----------|:------------|
// |.Value     | bytes       |

// IndexSet implements charlang.Object interface.
func (rm *RawMessage) IndexSet(index, value charlang.Object) error {
	switch index.String() {
	case "Value":
		if v, ok := charlang.ToBytes(value); ok {
			rm.Value = v
		} else {
			return charlang.ErrType
		}
	default:
		return charlang.ErrInvalidIndex
	}
	return nil
}
