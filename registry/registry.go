package registry

import (
	"reflect"
)

// Converter is a function that converts a value of one type to another.
type Converter func(in interface{}) (out interface{}, ok bool)

var objectConverters = map[reflect.Type]Converter{}
var anyConverters = map[reflect.Type]Converter{}

// RegisterObjectConverter registers a converter for a specific type to be used
// with ToObject that converts to a charlang.Object.
func RegisterObjectConverter(typ reflect.Type, converter Converter) {
	objectConverters[typ] = converter
}

// RegisterAnyConverter registers a converter for a specific type to be used
// with ToAny that converts to any.
func RegisterAnyConverter(typ reflect.Type, converter Converter) {
	anyConverters[typ] = converter
}

// ToObject tries to convert any value to a charlang.Object using the registered
// converters.
// This should be called in charlang.ToObject.
func ToObject(in interface{}) (out interface{}, ok bool) {
	typ := reflect.TypeOf(in)
	if converter, ok := objectConverters[typ]; ok {
		return converter(in)
	}
	return nil, false
}

// ToInterface tries to convert any value to any using the registered converters.
// This should be called in charlang.ToInterface.
func ToInterface(in interface{}) (out interface{}, ok bool) {
	typ := reflect.TypeOf(in)
	if converter, ok := anyConverters[typ]; ok {
		return converter(in)
	}
	return nil, false
}
