// Copyright (c) 2020-2023 Ozan Hacıbekiroğlu.
// Use of this source code is governed by a MIT License
// that can be found in the LICENSE file.

package time

import (
	"time"

	"github.com/topxeq/charlang"
)

// char:doc
// ## Types
// ### location
//
// Go Type
//
// ```go
// // Location represents location values and implements charlang.Object interface.
// type Location struct {
//    charlang.ObjectImpl
//    Value *time.Location
// }
// ```

// Location represents location values and implements charlang.Object interface.
type Location struct {
	charlang.ObjectImpl
	Value *time.Location
}

// TypeName implements charlang.Object interface.
func (*Location) TypeName() string {
	return "location"
}

// String implements charlang.Object interface.
func (o *Location) String() string {
	return o.Value.String()
}

// IsFalsy implements charlang.Object interface.
func (o *Location) IsFalsy() bool {
	return o.Value == nil
}

// Equal implements charlang.Object interface.
func (o *Location) Equal(right charlang.Object) bool {
	if v, ok := right.(*Location); ok {
		return v == o || v.String() == o.String()
	}
	if v, ok := right.(charlang.String); ok {
		return o.String() == v.String()
	}
	return false
}
