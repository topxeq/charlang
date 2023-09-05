package time

import (
	"time"

	"github.com/topxeq/charlang"
)

//go:generate go run ../../cmd/mkcallable -output zfuncs.go funcs.go

//char:callable:convert *Location ToLocation
//char:callable:convert *Time ToTime

// ToLocation will try to convert given charlang.Object to *Location value.
func ToLocation(o charlang.Object) (ret *Location, ok bool) {
	if v, isString := o.(charlang.String); isString {
		var err error
		o, err = loadLocationFunc(string(v))
		if err != nil {
			return
		}
	}
	ret, ok = o.(*Location)
	return
}

// ToTime will try to convert given given charlang.Object to *Time value.
func ToTime(o charlang.Object) (ret *Time, ok bool) {
	switch o := o.(type) {
	case *Time:
		ret, ok = o, true
	case charlang.Int:
		v := time.Unix(int64(o), 0)
		ret, ok = &Time{Value: v}, true
	case charlang.String:
		v, err := time.Parse(time.RFC3339Nano, string(o))
		if err != nil {
			v, err = time.Parse(time.RFC3339, string(o))
		}
		if err == nil {
			ret, ok = &Time{Value: v}, true
		}
	}
	return
}

// Since, Until
//
//char:callable funcPTRO(t *Time) (ret charlang.Object)

// Add, Round, Truncate
//
//char:callable funcPTi64RO(t *Time, d int64) (ret charlang.Object)

// Sub, After, Before
//
//char:callable funcPTTRO(t1 *Time, t2 *Time) (ret charlang.Object)

// AddDate
//
//char:callable funcPTiiiRO(t *Time, i1 int, i2 int, i3 int) (ret charlang.Object)

// Format
//
//char:callable funcPTsRO(t *Time, s string) (ret charlang.Object)

// AppendFormat
//
//char:callable funcPTb2sRO(t *Time, b []byte, s string) (ret charlang.Object)

// In
//
//char:callable funcPTLRO(t *Time, loc *Location) (ret charlang.Object)
