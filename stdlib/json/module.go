// Copyright (c) 2022-2023 Ozan Hacıbekiroğlu.
// Use of this source code is governed by a MIT License
// that can be found in the LICENSE file.

package json

import (
	"bytes"

	"github.com/topxeq/charlang"
	"github.com/topxeq/charlang/stdlib"
)

// Module represents json module.
var Module = map[string]charlang.Object{
	// char:doc
	// # json Module
	//
	// ## Functions
	// Marshal(v any) -> bytes
	// Returns the JSON encoding v or error.
	"Marshal": &charlang.Function{
		Name:    "Marshal",
		Value:   stdlib.FuncPORO(marshalFunc),
		ValueEx: stdlib.FuncPOROEx(marshalFunc),
	},
	// char:doc
	// MarshalIndent(v any, prefix string, indent string) -> bytes
	// MarshalIndent is like Marshal but applies Indent to format the output.
	"MarshalIndent": &charlang.Function{
		Name:    "MarshalIndent",
		Value:   stdlib.FuncPOssRO(marshalIndentFunc),
		ValueEx: stdlib.FuncPOssROEx(marshalIndentFunc),
	},
	// char:doc
	// Indent(src bytes, prefix string, indent string) -> bytes
	// Returns indented form of the JSON-encoded src or error.
	"Indent": &charlang.Function{
		Name:    "Indent",
		Value:   stdlib.FuncPb2ssRO(indentFunc),
		ValueEx: stdlib.FuncPb2ssROEx(indentFunc),
	},
	// char:doc
	// RawMessage(v bytes) -> rawMessage
	// Returns a wrapped bytes to provide raw encoded JSON value to Marshal
	// functions.
	"RawMessage": &charlang.Function{
		Name:    "RawMessage",
		Value:   stdlib.FuncPb2RO(rawMessageFunc),
		ValueEx: stdlib.FuncPb2ROEx(rawMessageFunc),
	},
	// char:doc
	// Compact(data bytes, escape bool) -> bytes
	// Returns elided insignificant space characters from data or error.
	"Compact": &charlang.Function{
		Name:    "Compact",
		Value:   stdlib.FuncPb2bRO(compactFunc),
		ValueEx: stdlib.FuncPb2bROEx(compactFunc),
	},
	// char:doc
	// Quote(v any) -> encoderOptions
	// Returns a wrapped object to provide Marshal functions to quote v.
	"Quote": &charlang.Function{
		Name:    "Quote",
		Value:   stdlib.FuncPORO(quoteFunc),
		ValueEx: stdlib.FuncPOROEx(quoteFunc),
	},
	// char:doc
	// NoQuote(v any) -> encoderOptions
	// Returns a wrapped object to provide Marshal functions not to quote while
	// encoding.
	// This can be used not to quote all array or map items.
	"NoQuote": &charlang.Function{
		Name:    "NoQuote",
		Value:   stdlib.FuncPORO(noQuoteFunc),
		ValueEx: stdlib.FuncPOROEx(noQuoteFunc),
	},
	// char:doc
	// NoEscape(v any) -> encoderOptions
	// Returns a wrapped object to provide Marshal functions not to escape html
	// while encoding.
	"NoEscape": &charlang.Function{
		Name:    "NoEscape",
		Value:   stdlib.FuncPORO(noEscapeFunc),
		ValueEx: stdlib.FuncPOROEx(noEscapeFunc),
	},
	// char:doc
	// Unmarshal(p bytes) -> any
	// Unmarshal parses the JSON-encoded p and returns the result or error.
	"Unmarshal": &charlang.Function{
		Name:    "Unmarshal",
		Value:   stdlib.FuncPb2RO(unmarshalFunc),
		ValueEx: stdlib.FuncPb2ROEx(unmarshalFunc),
	},
	// char:doc
	// Valid(p bytes) -> bool
	// Reports whether p is a valid JSON encoding.
	"Valid": &charlang.Function{
		Name:    "Valid",
		Value:   stdlib.FuncPb2RO(validFunc),
		ValueEx: stdlib.FuncPb2ROEx(validFunc),
	},
}

func marshalFunc(o charlang.Object) charlang.Object {
	b, err := Marshal(o)
	if err != nil {
		return &charlang.Error{Message: err.Error(), Cause: err}
	}
	return charlang.Bytes(b)
}

func marshalIndentFunc(o charlang.Object, prefix, indent string) charlang.Object {
	b, err := MarshalIndent(o, prefix, indent)
	if err != nil {
		return &charlang.Error{Message: err.Error(), Cause: err}
	}
	return charlang.Bytes(b)
}

func indentFunc(src []byte, prefix, indent string) charlang.Object {
	var buf bytes.Buffer
	err := indentBuffer(&buf, src, prefix, indent)
	if err != nil {
		return &charlang.Error{Message: err.Error(), Cause: err}
	}
	return charlang.Bytes(buf.Bytes())
}

func rawMessageFunc(b []byte) charlang.Object { return &RawMessage{Value: b} }

func compactFunc(data []byte, escape bool) charlang.Object {
	var buf bytes.Buffer
	err := compact(&buf, data, escape)
	if err != nil {
		return &charlang.Error{Message: err.Error(), Cause: err}
	}
	return charlang.Bytes(buf.Bytes())
}

func quoteFunc(o charlang.Object) charlang.Object {
	if v, ok := o.(*EncoderOptions); ok {
		v.Quote = true
		return v
	}
	return &EncoderOptions{Value: o, Quote: true, EscapeHTML: true}
}

func noQuoteFunc(o charlang.Object) charlang.Object {
	if v, ok := o.(*EncoderOptions); ok {
		v.Quote = false
		return v
	}
	return &EncoderOptions{Value: o, Quote: false, EscapeHTML: true}
}

func noEscapeFunc(o charlang.Object) charlang.Object {
	if v, ok := o.(*EncoderOptions); ok {
		v.EscapeHTML = false
		return v
	}
	return &EncoderOptions{Value: o}
}

func unmarshalFunc(b []byte) charlang.Object {
	v, err := Unmarshal(b)
	if err != nil {
		return &charlang.Error{Message: err.Error(), Cause: err}
	}
	return v
}

func validFunc(b []byte) charlang.Object { return charlang.Bool(valid(b)) }
