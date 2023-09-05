package time

import (
	"reflect"
	"time"

	"github.com/topxeq/charlang"
	"github.com/topxeq/charlang/registry"
	"github.com/topxeq/charlang/token"
)

func init() {
	registry.RegisterObjectConverter(reflect.TypeOf(time.Duration(0)),
		func(in interface{}) (interface{}, bool) {
			return charlang.Int(in.(time.Duration)), true
		},
	)

	registry.RegisterObjectConverter(reflect.TypeOf(time.Time{}),
		func(in interface{}) (interface{}, bool) {
			return &Time{Value: in.(time.Time)}, true
		},
	)
	registry.RegisterObjectConverter(reflect.TypeOf((*time.Time)(nil)),
		func(in interface{}) (interface{}, bool) {
			v := in.(*time.Time)
			if v == nil {
				return charlang.Undefined, true
			}
			return &Time{Value: *v}, true
		},
	)
	registry.RegisterAnyConverter(reflect.TypeOf((*Time)(nil)),
		func(in interface{}) (interface{}, bool) {
			return in.(*Time).Value, true
		},
	)

	registry.RegisterObjectConverter(reflect.TypeOf((*time.Location)(nil)),
		func(in interface{}) (interface{}, bool) {
			v := in.(*time.Location)
			if v == nil {
				return charlang.Undefined, true
			}
			return &Location{Value: v}, true
		},
	)
	registry.RegisterAnyConverter(reflect.TypeOf((*Location)(nil)),
		func(in interface{}) (interface{}, bool) {
			return in.(*Location).Value, true
		},
	)
}

// char:doc
// ## Types
// ### time
//
// Go Type
//
// ```go
// // Time represents time values and implements charlang.Object interface.
// type Time struct {
//   Value time.Time
// }
// ```

// Time represents time values and implements charlang.Object interface.
type Time struct {
	Value time.Time
}

var _ charlang.NameCallerObject = (*Time)(nil)

// TypeName implements charlang.Object interface.
func (*Time) TypeName() string {
	return "time"
}

// String implements charlang.Object interface.
func (o *Time) String() string {
	return o.Value.String()
}

// IsFalsy implements charlang.Object interface.
func (o *Time) IsFalsy() bool {
	return o.Value.IsZero()
}

// Equal implements charlang.Object interface.
func (o *Time) Equal(right charlang.Object) bool {
	if v, ok := right.(*Time); ok {
		return o.Value.Equal(v.Value)
	}
	return false
}

// CanCall implements charlang.Object interface.
func (*Time) CanCall() bool { return false }

// Call implements charlang.Object interface.
func (*Time) Call(args ...charlang.Object) (charlang.Object, error) {
	return nil, charlang.ErrNotCallable
}

// CanIterate implements charlang.Object interface.
func (*Time) CanIterate() bool { return false }

// Iterate implements charlang.Object interface.
func (*Time) Iterate() charlang.Iterator { return nil }

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

// BinaryOp implements charlang.Object interface.
func (o *Time) BinaryOp(tok token.Token,
	right charlang.Object) (charlang.Object, error) {

	switch v := right.(type) {
	case charlang.Int:
		switch tok {
		case token.Add:
			return &Time{Value: o.Value.Add(time.Duration(v))}, nil
		case token.Sub:
			return &Time{Value: o.Value.Add(time.Duration(-v))}, nil
		}
	case *Time:
		switch tok {
		case token.Sub:
			return charlang.Int(o.Value.Sub(v.Value)), nil
		case token.Less:
			return charlang.Bool(o.Value.Before(v.Value)), nil
		case token.LessEq:
			return charlang.Bool(o.Value.Before(v.Value) || o.Value.Equal(v.Value)), nil
		case token.Greater:
			return charlang.Bool(o.Value.After(v.Value)), nil
		case token.GreaterEq:
			return charlang.Bool(o.Value.After(v.Value) || o.Value.Equal(v.Value)),
				nil
		}
	case *charlang.UndefinedType:
		switch tok {
		case token.Less, token.LessEq:
			return charlang.False, nil
		case token.Greater, token.GreaterEq:
			return charlang.True, nil
		}
	}
	return nil, charlang.NewOperandTypeError(
		tok.String(),
		o.TypeName(),
		right.TypeName())
}

// IndexSet implements charlang.Object interface.
func (*Time) IndexSet(_, _ charlang.Object) error { return charlang.ErrNotIndexAssignable }

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

// IndexGet implements charlang.Object interface.
func (o *Time) IndexGet(index charlang.Object) (charlang.Object, error) {
	v, ok := index.(charlang.String)
	if !ok {
		return charlang.Undefined, charlang.NewIndexTypeError("string", index.TypeName())
	}

	// For simplicity, we use method call for now. As getters are deprecated, we
	// will return callable object in the future here.

	switch v {
	case "Date", "Clock", "UTC", "Unix", "UnixNano", "Year", "Month", "Day",
		"Hour", "Minute", "Second", "Nanosecond", "IsZero", "Local", "Location",
		"YearDay", "Weekday", "ISOWeek", "Zone":
		return o.CallName(string(v), charlang.Call{})
	}
	return charlang.Undefined, nil
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

func (o *Time) CallName(name string, c charlang.Call) (charlang.Object, error) {
	fn, ok := methodTable[name]
	if !ok {
		return charlang.Undefined, charlang.ErrInvalidIndex.NewError(name)
	}
	return fn(o, &c)
}

var methodTable = map[string]func(*Time, *charlang.Call) (charlang.Object, error){
	"Add": func(o *Time, c *charlang.Call) (charlang.Object, error) {
		if err := c.CheckLen(1); err != nil {
			return charlang.Undefined, err
		}
		d, ok := charlang.ToGoInt64(c.Get(0))
		if !ok {
			return newArgTypeErr("1st", "int", c.Get(0).TypeName())
		}
		return timeAdd(o, d), nil
	},
	"Sub": func(o *Time, c *charlang.Call) (charlang.Object, error) {
		if err := c.CheckLen(1); err != nil {
			return charlang.Undefined, err
		}
		t2, ok := ToTime(c.Get(0))
		if !ok {
			return newArgTypeErr("1st", "time", c.Get(0).TypeName())
		}
		return timeSub(o, t2), nil
	},
	"AddDate": func(o *Time, c *charlang.Call) (charlang.Object, error) {
		if err := c.CheckLen(3); err != nil {
			return charlang.Undefined, err
		}
		year, ok := charlang.ToGoInt(c.Get(0))
		if !ok {
			return newArgTypeErr("1st", "int", c.Get(0).TypeName())
		}
		month, ok := charlang.ToGoInt(c.Get(1))
		if !ok {
			return newArgTypeErr("2nd", "int", c.Get(1).TypeName())
		}
		day, ok := charlang.ToGoInt(c.Get(2))
		if !ok {
			return newArgTypeErr("3rd", "int", c.Get(2).TypeName())
		}
		return timeAddDate(o, year, month, day), nil
	},
	"After": func(o *Time, c *charlang.Call) (charlang.Object, error) {
		if err := c.CheckLen(1); err != nil {
			return charlang.Undefined, err
		}
		t2, ok := ToTime(c.Get(0))
		if !ok {
			return newArgTypeErr("1st", "time", c.Get(0).TypeName())
		}
		return timeAfter(o, t2), nil
	},
	"Before": func(o *Time, c *charlang.Call) (charlang.Object, error) {
		if err := c.CheckLen(1); err != nil {
			return charlang.Undefined, err
		}
		t2, ok := ToTime(c.Get(0))
		if !ok {
			return newArgTypeErr("1st", "time", c.Get(0).TypeName())
		}
		return timeBefore(o, t2), nil
	},
	"Format": func(o *Time, c *charlang.Call) (charlang.Object, error) {
		if err := c.CheckLen(1); err != nil {
			return charlang.Undefined, err
		}
		format, ok := charlang.ToGoString(c.Get(0))
		if !ok {
			return newArgTypeErr("1st", "string", c.Get(0).TypeName())
		}
		return timeFormat(o, format), nil
	},
	"AppendFormat": func(o *Time, c *charlang.Call) (charlang.Object, error) {
		if err := c.CheckLen(2); err != nil {
			return charlang.Undefined, err
		}
		b, ok := charlang.ToGoByteSlice(c.Get(0))
		if !ok {
			return newArgTypeErr("1st", "bytes", c.Get(0).TypeName())
		}
		format, ok := charlang.ToGoString(c.Get(1))
		if !ok {
			return newArgTypeErr("2nd", "string", c.Get(1).TypeName())
		}
		return timeAppendFormat(o, b, format), nil
	},
	"In": func(o *Time, c *charlang.Call) (charlang.Object, error) {
		if err := c.CheckLen(1); err != nil {
			return charlang.Undefined, err
		}
		loc, ok := ToLocation(c.Get(0))
		if !ok {
			return newArgTypeErr("1st", "location", c.Get(0).TypeName())
		}
		return timeIn(o, loc), nil
	},
	"Round": func(o *Time, c *charlang.Call) (charlang.Object, error) {
		if err := c.CheckLen(1); err != nil {
			return charlang.Undefined, err
		}
		d, ok := charlang.ToGoInt64(c.Get(0))
		if !ok {
			return newArgTypeErr("1st", "int", c.Get(0).TypeName())
		}
		return timeRound(o, d), nil
	},
	"Truncate": func(o *Time, c *charlang.Call) (charlang.Object, error) {
		if err := c.CheckLen(1); err != nil {
			return charlang.Undefined, err
		}
		d, ok := charlang.ToGoInt64(c.Get(0))
		if !ok {
			return newArgTypeErr("1st", "int", c.Get(0).TypeName())
		}
		return timeTruncate(o, d), nil
	},
	"Equal": func(o *Time, c *charlang.Call) (charlang.Object, error) {
		if err := c.CheckLen(1); err != nil {
			return charlang.Undefined, err
		}
		t2, ok := ToTime(c.Get(0))
		if !ok {
			return newArgTypeErr("1st", "time", c.Get(0).TypeName())
		}
		return timeEqual(o, t2), nil
	},
	"Date": func(o *Time, c *charlang.Call) (charlang.Object, error) {
		if err := c.CheckLen(0); err != nil {
			return charlang.Undefined, err
		}
		y, m, d := o.Value.Date()
		return charlang.Map{"year": charlang.Int(y), "month": charlang.Int(m),
			"day": charlang.Int(d)}, nil
	},
	"Clock": func(o *Time, c *charlang.Call) (charlang.Object, error) {
		if err := c.CheckLen(0); err != nil {
			return charlang.Undefined, err
		}
		h, m, s := o.Value.Clock()
		return charlang.Map{"hour": charlang.Int(h), "minute": charlang.Int(m),
			"second": charlang.Int(s)}, nil
	},
	"UTC": func(o *Time, c *charlang.Call) (charlang.Object, error) {
		if err := c.CheckLen(0); err != nil {
			return charlang.Undefined, err
		}
		return &Time{Value: o.Value.UTC()}, nil
	},
	"Unix": func(o *Time, c *charlang.Call) (charlang.Object, error) {
		if err := c.CheckLen(0); err != nil {
			return charlang.Undefined, err
		}
		return charlang.Int(o.Value.Unix()), nil
	},
	"UnixNano": func(o *Time, c *charlang.Call) (charlang.Object, error) {
		if err := c.CheckLen(0); err != nil {
			return charlang.Undefined, err
		}
		return charlang.Int(o.Value.UnixNano()), nil
	},
	"Year": func(o *Time, c *charlang.Call) (charlang.Object, error) {
		if err := c.CheckLen(0); err != nil {
			return charlang.Undefined, err
		}
		return charlang.Int(o.Value.Year()), nil
	},
	"Month": func(o *Time, c *charlang.Call) (charlang.Object, error) {
		if err := c.CheckLen(0); err != nil {
			return charlang.Undefined, err
		}
		return charlang.Int(o.Value.Month()), nil
	},
	"Day": func(o *Time, c *charlang.Call) (charlang.Object, error) {
		if err := c.CheckLen(0); err != nil {
			return charlang.Undefined, err
		}
		return charlang.Int(o.Value.Day()), nil
	},
	"Hour": func(o *Time, c *charlang.Call) (charlang.Object, error) {
		if err := c.CheckLen(0); err != nil {
			return charlang.Undefined, err
		}
		return charlang.Int(o.Value.Hour()), nil
	},
	"Minute": func(o *Time, c *charlang.Call) (charlang.Object, error) {
		if err := c.CheckLen(0); err != nil {
			return charlang.Undefined, err
		}
		return charlang.Int(o.Value.Minute()), nil
	},
	"Second": func(o *Time, c *charlang.Call) (charlang.Object, error) {
		if err := c.CheckLen(0); err != nil {
			return charlang.Undefined, err
		}
		return charlang.Int(o.Value.Second()), nil
	},
	"Nanosecond": func(o *Time, c *charlang.Call) (charlang.Object, error) {
		if err := c.CheckLen(0); err != nil {
			return charlang.Undefined, err
		}
		return charlang.Int(o.Value.Nanosecond()), nil
	},
	"IsZero": func(o *Time, c *charlang.Call) (charlang.Object, error) {
		if err := c.CheckLen(0); err != nil {
			return charlang.Undefined, err
		}
		return charlang.Bool(o.Value.IsZero()), nil
	},
	"Local": func(o *Time, c *charlang.Call) (charlang.Object, error) {
		if err := c.CheckLen(0); err != nil {
			return charlang.Undefined, err
		}
		return &Time{Value: o.Value.Local()}, nil
	},
	"Location": func(o *Time, c *charlang.Call) (charlang.Object, error) {
		if err := c.CheckLen(0); err != nil {
			return charlang.Undefined, err
		}
		return &Location{Value: o.Value.Location()}, nil
	},
	"YearDay": func(o *Time, c *charlang.Call) (charlang.Object, error) {
		if err := c.CheckLen(0); err != nil {
			return charlang.Undefined, err
		}
		return charlang.Int(o.Value.YearDay()), nil
	},
	"Weekday": func(o *Time, c *charlang.Call) (charlang.Object, error) {
		if err := c.CheckLen(0); err != nil {
			return charlang.Undefined, err
		}
		return charlang.Int(o.Value.Weekday()), nil
	},
	"ISOWeek": func(o *Time, c *charlang.Call) (charlang.Object, error) {
		if err := c.CheckLen(0); err != nil {
			return charlang.Undefined, err
		}
		y, w := o.Value.ISOWeek()
		return charlang.Map{"year": charlang.Int(y), "week": charlang.Int(w)}, nil
	},
	"Zone": func(o *Time, c *charlang.Call) (charlang.Object, error) {
		if err := c.CheckLen(0); err != nil {
			return charlang.Undefined, err
		}
		name, offset := o.Value.Zone()
		return charlang.Map{"name": charlang.String(name), "offset": charlang.Int(offset)}, nil
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

func timeAdd(t *Time, duration int64) charlang.Object {
	return &Time{Value: t.Value.Add(time.Duration(duration))}
}

func timeSub(t1, t2 *Time) charlang.Object {
	return charlang.Int(t1.Value.Sub(t2.Value))
}

func timeAddDate(t *Time, years, months, days int) charlang.Object {
	return &Time{Value: t.Value.AddDate(years, months, days)}
}

func timeAfter(t1, t2 *Time) charlang.Object {
	return charlang.Bool(t1.Value.After(t2.Value))
}

func timeBefore(t1, t2 *Time) charlang.Object {
	return charlang.Bool(t1.Value.Before(t2.Value))
}

func timeFormat(t *Time, layout string) charlang.Object {
	return charlang.String(t.Value.Format(layout))
}

func timeAppendFormat(t *Time, b []byte, layout string) charlang.Object {
	return charlang.Bytes(t.Value.AppendFormat(b, layout))
}

func timeIn(t *Time, loc *Location) charlang.Object {
	return &Time{Value: t.Value.In(loc.Value)}
}

func timeRound(t *Time, duration int64) charlang.Object {
	return &Time{Value: t.Value.Round(time.Duration(duration))}
}

func timeTruncate(t *Time, duration int64) charlang.Object {
	return &Time{Value: t.Value.Truncate(time.Duration(duration))}
}

func timeEqual(t1, t2 *Time) charlang.Object {
	return charlang.Bool(t1.Value.Equal(t2.Value))
}

func newArgTypeErr(pos, want, got string) (charlang.Object, error) {
	return charlang.Undefined, charlang.NewArgumentTypeError(pos, want, got)
}
