// Package time provides time module for measuring and displaying time for Charlang
// script language. It wraps Go's time package functionalities.
// Note that: Charlang's int values are converted to Go's time.Duration values.
package time

import (
	"strconv"
	"time"

	"github.com/topxeq/charlang"
	"github.com/topxeq/charlang/stdlib"
)

var utcLoc charlang.Object = &Location{Value: time.UTC}
var localLoc charlang.Object = &Location{Value: time.Local}
var zeroTime charlang.Object = &charlang.Time{}

// Module represents time module.
var Module = map[string]charlang.Object{
	// char:doc
	// # time Module
	//
	// ## Constants
	// ### Months
	//
	// January
	// February
	// March
	// April
	// May
	// June
	// July
	// August
	// September
	// October
	// November
	// December
	"January":   charlang.Int(time.January),
	"February":  charlang.Int(time.February),
	"March":     charlang.Int(time.March),
	"April":     charlang.Int(time.April),
	"May":       charlang.Int(time.May),
	"June":      charlang.Int(time.June),
	"July":      charlang.Int(time.July),
	"August":    charlang.Int(time.August),
	"September": charlang.Int(time.September),
	"October":   charlang.Int(time.October),
	"November":  charlang.Int(time.November),
	"December":  charlang.Int(time.December),

	// char:doc
	// ### Weekdays
	//
	// Sunday
	// Monday
	// Tuesday
	// Wednesday
	// Thursday
	// Friday
	// Saturday
	"Sunday":    charlang.Int(time.Sunday),
	"Monday":    charlang.Int(time.Monday),
	"Tuesday":   charlang.Int(time.Tuesday),
	"Wednesday": charlang.Int(time.Wednesday),
	"Thursday":  charlang.Int(time.Thursday),
	"Friday":    charlang.Int(time.Friday),
	"Saturday":  charlang.Int(time.Saturday),

	// char:doc
	// ### Layouts
	//
	// ANSIC
	// UnixDate
	// RubyDate
	// RFC822
	// RFC822Z
	// RFC850
	// RFC1123
	// RFC1123Z
	// RFC3339
	// RFC3339Nano
	// Kitchen
	// Stamp
	// StampMilli
	// StampMicro
	// StampNano
	"ANSIC":       charlang.String{Value: time.ANSIC},
	"UnixDate":    charlang.String{Value: time.UnixDate},
	"RubyDate":    charlang.String{Value: time.RubyDate},
	"RFC822":      charlang.String{Value: time.RFC822},
	"RFC822Z":     charlang.String{Value: time.RFC822Z},
	"RFC850":      charlang.String{Value: time.RFC850},
	"RFC1123":     charlang.String{Value: time.RFC1123},
	"RFC1123Z":    charlang.String{Value: time.RFC1123Z},
	"RFC3339":     charlang.String{Value: time.RFC3339},
	"RFC3339Nano": charlang.String{Value: time.RFC3339Nano},
	"Kitchen":     charlang.String{Value: time.Kitchen},
	"Stamp":       charlang.String{Value: time.Stamp},
	"StampMilli":  charlang.String{Value: time.StampMilli},
	"StampMicro":  charlang.String{Value: time.StampMicro},
	"StampNano":   charlang.String{Value: time.StampNano},

	// char:doc
	// ### Durations
	//
	// Nanosecond
	// Microsecond
	// Millisecond
	// Second
	// Minute
	// Hour
	"Nanosecond":  charlang.Int(time.Nanosecond),
	"Microsecond": charlang.Int(time.Microsecond),
	"Millisecond": charlang.Int(time.Millisecond),
	"Second":      charlang.Int(time.Second),
	"Minute":      charlang.Int(time.Minute),
	"Hour":        charlang.Int(time.Hour),

	// char:doc
	// ## Functions
	// UTC() -> location
	// Returns Universal Coordinated Time (UTC) location.
	"UTC": &charlang.Function{
		Name:    "UTC",
		Value:   stdlib.FuncPRO(utcFunc),
		ValueEx: stdlib.FuncPROEx(utcFunc),
	},

	// char:doc
	// Local() -> location
	// Returns the system's local time zone location.
	"Local": &charlang.Function{
		Name:    "Local",
		Value:   stdlib.FuncPRO(localFunc),
		ValueEx: stdlib.FuncPROEx(localFunc),
	},

	// char:doc
	// MonthString(m int) -> month string
	// Returns English name of the month m ("January", "February", ...).
	"MonthString": &charlang.Function{
		Name:    "MonthString",
		Value:   stdlib.FuncPiRO(monthStringFunc),
		ValueEx: stdlib.FuncPiROEx(monthStringFunc),
	},

	// char:doc
	// WeekdayString(w int) -> weekday string
	// Returns English name of the int weekday w, note that 0 is Sunday.
	"WeekdayString": &charlang.Function{
		Name:    "WeekdayString",
		Value:   stdlib.FuncPiRO(weekdayStringFunc),
		ValueEx: stdlib.FuncPiROEx(weekdayStringFunc),
	},

	// char:doc
	// DurationString(d int) -> string
	// Returns a string representing the duration d in the form "72h3m0.5s".
	"DurationString": &charlang.Function{
		Name:    "DurationString",
		Value:   stdlib.FuncPi64RO(durationStringFunc),
		ValueEx: stdlib.FuncPi64ROEx(durationStringFunc),
	},
	// char:doc
	// DurationNanoseconds(d int) -> int
	// Returns the duration d as an int nanosecond count.
	"DurationNanoseconds": &charlang.Function{
		Name:    "DurationNanoseconds",
		Value:   stdlib.FuncPi64RO(durationNanosecondsFunc),
		ValueEx: stdlib.FuncPi64ROEx(durationNanosecondsFunc),
	},
	// char:doc
	// DurationMicroseconds(d int) -> int
	// Returns the duration d as an int microsecond count.
	"DurationMicroseconds": &charlang.Function{
		Name:    "DurationMicroseconds",
		Value:   stdlib.FuncPi64RO(durationMicrosecondsFunc),
		ValueEx: stdlib.FuncPi64ROEx(durationMicrosecondsFunc),
	},
	// char:doc
	// DurationMilliseconds(d int) -> int
	// Returns the duration d as an int millisecond count.
	"DurationMilliseconds": &charlang.Function{
		Name:    "DurationMilliseconds",
		Value:   stdlib.FuncPi64RO(durationMillisecondsFunc),
		ValueEx: stdlib.FuncPi64ROEx(durationMillisecondsFunc),
	},
	// char:doc
	// DurationSeconds(d int) -> float
	// Returns the duration d as a floating point number of seconds.
	"DurationSeconds": &charlang.Function{
		Name:    "DurationSeconds",
		Value:   stdlib.FuncPi64RO(durationSecondsFunc),
		ValueEx: stdlib.FuncPi64ROEx(durationSecondsFunc),
	},
	// char:doc
	// DurationMinutes(d int) -> float
	// Returns the duration d as a floating point number of minutes.
	"DurationMinutes": &charlang.Function{
		Name:    "DurationMinutes",
		Value:   stdlib.FuncPi64RO(durationMinutesFunc),
		ValueEx: stdlib.FuncPi64ROEx(durationMinutesFunc),
	},
	// char:doc
	// DurationHours(d int) -> float
	// Returns the duration d as a floating point number of hours.
	"DurationHours": &charlang.Function{
		Name:    "DurationHours",
		Value:   stdlib.FuncPi64RO(durationHoursFunc),
		ValueEx: stdlib.FuncPi64ROEx(durationHoursFunc),
	},
	// char:doc
	// Sleep(duration int) -> undefined
	// Pauses the current goroutine for at least the duration.
	"Sleep": &charlang.Function{
		Name: "Sleep",
		Value: stdlib.FuncPi64R(func(duration int64) {
			time.Sleep(time.Duration(duration))
		}),
		ValueEx: sleepFunc,
	},
	// char:doc
	// ParseDuration(s string) -> duration int
	// Parses duration s and returns duration as int or error.
	"ParseDuration": &charlang.Function{
		Name:    "ParseDuration",
		Value:   stdlib.FuncPsROe(parseDurationFunc),
		ValueEx: stdlib.FuncPsROeEx(parseDurationFunc),
	},
	// char:doc
	// DurationRound(duration int, m int) -> duration int
	// Returns the result of rounding duration to the nearest multiple of m.
	"DurationRound": &charlang.Function{
		Name:    "DurationRound",
		Value:   stdlib.FuncPi64i64RO(durationRoundFunc),
		ValueEx: stdlib.FuncPi64i64ROEx(durationRoundFunc),
	},
	// char:doc
	// DurationTruncate(duration int, m int) -> duration int
	// Returns the result of rounding duration toward zero to a multiple of m.
	"DurationTruncate": &charlang.Function{
		Name:    "DurationTruncate",
		Value:   stdlib.FuncPi64i64RO(durationTruncateFunc),
		ValueEx: stdlib.FuncPi64i64ROEx(durationTruncateFunc),
	},
	// char:doc
	// FixedZone(name string, sec int) -> location
	// Returns a Location that always uses the given zone name and offset
	// (seconds east of UTC).
	"FixedZone": &charlang.Function{
		Name:    "FixedZone",
		Value:   stdlib.FuncPsiRO(fixedZoneFunc),
		ValueEx: stdlib.FuncPsiROEx(fixedZoneFunc),
	},
	// char:doc
	// LoadLocation(name string) -> location
	// Returns the Location with the given name.
	"LoadLocation": &charlang.Function{
		Name:    "LoadLocation",
		Value:   stdlib.FuncPsROe(loadLocationFunc),
		ValueEx: stdlib.FuncPsROeEx(loadLocationFunc),
	},
	// char:doc
	// IsLocation(any) -> bool
	// Reports whether any value is of location type.
	"IsLocation": &charlang.Function{
		Name:    "IsLocation",
		Value:   stdlib.FuncPORO(isLocationFunc),
		ValueEx: stdlib.FuncPOROEx(isLocationFunc),
	},
	// char:doc
	// Time() -> time
	// Returns zero time.
	"Time": &charlang.Function{
		Name:    "Time",
		Value:   stdlib.FuncPRO(zerotimeFunc),
		ValueEx: stdlib.FuncPROEx(zerotimeFunc),
	},
	// char:doc
	// Since(t time) -> duration int
	// Returns the time elapsed since t.
	"Since": &charlang.Function{
		Name:    "Since",
		Value:   funcPTRO(sinceFunc),
		ValueEx: funcPTROEx(sinceFunc),
	},
	// char:doc
	// Until(t time) -> duration int
	// Returns the duration until t.
	"Until": &charlang.Function{
		Name:    "Until",
		Value:   funcPTRO(untilFunc),
		ValueEx: funcPTROEx(untilFunc),
	},
	// char:doc
	// Date(year int, month int, day int[, hour int, min int, sec int, nsec int, loc location]) -> time
	// Returns the Time corresponding to yyyy-mm-dd hh:mm:ss + nsec nanoseconds
	// in the appropriate zone for that time in the given location. Zero values
	// of optional arguments are used if not provided.
	"Date": &charlang.Function{
		Name:    "Date",
		Value:   dateFunc,
		ValueEx: dateFuncEx,
	},
	// char:doc
	// Now() -> time
	// Returns the current local time.
	"Now": &charlang.Function{
		Name:    "Now",
		Value:   stdlib.FuncPRO(nowFunc),
		ValueEx: stdlib.FuncPROEx(nowFunc),
	},
	// char:doc
	// Parse(layout string, value string[, loc location]) -> time
	// Parses a formatted string and returns the time value it represents.
	// If location is not provided, Go's `time.Parse` function is called
	// otherwise `time.ParseInLocation` is called.
	"Parse": &charlang.Function{
		Name:    "Parse",
		Value:   parseFunc,
		ValueEx: parseFuncEx,
	},
	// char:doc
	// Unix(sec int[, nsec int]) -> time
	// Returns the local time corresponding to the given Unix time,
	// sec seconds and nsec nanoseconds since January 1, 1970 UTC.
	// Zero values of optional arguments are used if not provided.
	"Unix": &charlang.Function{
		Name:    "Unix",
		Value:   unixFunc,
		ValueEx: unixFuncEx,
	},
	// char:doc
	// Add(t time, duration int) -> time
	// Deprecated: Use .Add method of time object.
	// Returns the time of t+duration.
	"Add": &charlang.Function{
		Name:  "Add",
		Value: funcPTi64RO(timeAdd),
	},
	// char:doc
	// Sub(t1 time, t2 time) -> int
	// Deprecated: Use .Sub method of time object.
	// Returns the duration of t1-t2.
	"Sub": &charlang.Function{
		Name:  "Sub",
		Value: funcPTTRO(timeSub),
	},
	// char:doc
	// AddDate(t time, years int, months int, days int) -> time
	// Deprecated: Use .AddDate method of time object.
	// Returns the time corresponding to adding the given number of
	// years, months, and days to t.
	"AddDate": &charlang.Function{
		Name:  "AddDate",
		Value: funcPTiiiRO(timeAddDate),
	},
	// char:doc
	// After(t1 time, t2 time) -> bool
	// Deprecated: Use .After method of time object.
	// Reports whether the time t1 is after t2.
	"After": &charlang.Function{
		Name:  "After",
		Value: funcPTTRO(timeAfter),
	},
	// char:doc
	// Before(t1 time, t2 time) -> bool
	// Deprecated: Use .Before method of time object.
	// Reports whether the time t1 is before t2.
	"Before": &charlang.Function{
		Name:  "Before",
		Value: funcPTTRO(timeBefore),
	},
	// char:doc
	// Format(t time, layout string) -> string
	// Deprecated: Use .Format method of time object.
	// Returns a textual representation of the time value formatted according
	// to layout.
	"Format": &charlang.Function{
		Name:  "Format",
		Value: funcPTsRO(timeFormat),
	},
	// char:doc
	// AppendFormat(t time, b bytes, layout string) -> bytes
	// Deprecated: Use .AppendFormat method of time object.
	// It is like `Format` but appends the textual representation to b and
	// returns the extended buffer.
	"AppendFormat": &charlang.Function{
		Name:  "AppendFormat", // funcPTb2sRO
		Value: funcPTb2sRO(timeAppendFormat),
	},
	// char:doc
	// In(t time, loc location) -> time
	// Deprecated: Use .In method of time object.
	// Returns a copy of t representing the same time t, but with the copy's
	// location information set to loc for display purposes.
	"In": &charlang.Function{
		Name:  "In",
		Value: funcPTLRO(timeIn),
	},
	// char:doc
	// Round(t time, duration int) -> time
	// Deprecated: Use .Round method of time object.
	// Round returns the result of rounding t to the nearest multiple of
	// duration.
	"Round": &charlang.Function{
		Name:  "Round",
		Value: funcPTi64RO(timeRound),
	},
	// char:doc
	// Truncate(t time, duration int) -> time
	// Deprecated: Use .Truncate method of time object.
	// Truncate returns the result of rounding t down to a multiple of duration.
	"Truncate": &charlang.Function{
		Name:  "Truncate",
		Value: funcPTi64RO(timeTruncate),
	},
	// char:doc
	// IsTime(any) -> bool
	// Reports whether any value is of time type.
	"IsTime": &charlang.Function{
		Name:    "IsTime",
		Value:   stdlib.FuncPORO(isTimeFunc),
		ValueEx: stdlib.FuncPOROEx(isTimeFunc),
	},
}

func utcFunc() charlang.Object { return utcLoc }

func localFunc() charlang.Object { return localLoc }

func monthStringFunc(m int) charlang.Object {
	return charlang.String(time.Month(m).String())
}

func weekdayStringFunc(w int) charlang.Object {
	return charlang.String(time.Weekday(w).String())
}

func durationStringFunc(d int64) charlang.Object {
	return charlang.String(time.Duration(d).String())
}

func durationNanosecondsFunc(d int64) charlang.Object {
	return charlang.Int(time.Duration(d).Nanoseconds())
}

func durationMicrosecondsFunc(d int64) charlang.Object {
	return charlang.Int(time.Duration(d).Microseconds())
}

func durationMillisecondsFunc(d int64) charlang.Object {
	return charlang.Int(time.Duration(d).Milliseconds())
}

func durationSecondsFunc(d int64) charlang.Object {
	return charlang.Float(time.Duration(d).Seconds())
}

func durationMinutesFunc(d int64) charlang.Object {
	return charlang.Float(time.Duration(d).Minutes())
}

func durationHoursFunc(d int64) charlang.Object {
	return charlang.Float(time.Duration(d).Hours())
}

func sleepFunc(c charlang.Call) (charlang.Object, error) {
	if err := c.CheckLen(1); err != nil {
		return charlang.Undefined, err
	}
	arg0 := c.Get(0)

	var dur time.Duration
	if v, ok := charlang.ToGoInt64(arg0); !ok {
		return newArgTypeErr("1st", "int", arg0.TypeName())
	} else {
		dur = time.Duration(v)
	}

	for {
		if dur <= 10*time.Millisecond {
			time.Sleep(dur)
			break
		}
		dur -= 10 * time.Millisecond
		time.Sleep(10 * time.Millisecond)
		if c.VM().Aborted() {
			return charlang.Undefined, charlang.ErrVMAborted
		}
	}
	return charlang.Undefined, nil
}

func parseDurationFunc(s string) (charlang.Object, error) {
	d, err := time.ParseDuration(s)
	if err != nil {
		return nil, err
	}
	return charlang.Int(d), nil
}

func durationRoundFunc(d, m int64) charlang.Object {
	return charlang.Int(time.Duration(d).Round(time.Duration(m)))
}

func durationTruncateFunc(d, m int64) charlang.Object {
	return charlang.Int(time.Duration(d).Truncate(time.Duration(m)))
}

func fixedZoneFunc(name string, sec int) charlang.Object {
	return &Location{Value: time.FixedZone(name, sec)}
}

func loadLocationFunc(name string) (charlang.Object, error) {
	l, err := time.LoadLocation(name)
	if err != nil {
		return charlang.Undefined, err
	}
	return &Location{Value: l}, nil
}

func isLocationFunc(o charlang.Object) charlang.Object {
	_, ok := o.(*Location)
	return charlang.Bool(ok)
}

func zerotimeFunc() charlang.Object { return zeroTime }

func sinceFunc(t *Time) charlang.Object { return charlang.Int(time.Since(t.Value)) }

func untilFunc(t *Time) charlang.Object { return charlang.Int(time.Until(t.Value)) }

func dateFunc(args ...charlang.Object) (charlang.Object, error) {
	return dateFuncEx(charlang.NewCall(nil, args))
}

func dateFuncEx(c charlang.Call) (charlang.Object, error) {
	size := c.Len()
	if size < 3 || size > 8 {
		return charlang.Undefined, charlang.ErrWrongNumArguments.NewError(
			"want=3..8 got=" + strconv.Itoa(size))
	}
	ymdHmsn := [7]int{}
	loc := &Location{Value: time.Local}
	var ok bool
	for i := 0; i < size; i++ {
		arg := c.Get(i)
		if i < 7 {
			ymdHmsn[i], ok = charlang.ToGoInt(arg)
			if !ok {
				return newArgTypeErr(strconv.Itoa(i+1), "int", arg.TypeName())
			}
			continue
		}
		loc, ok = arg.(*Location)
		if !ok {
			return newArgTypeErr(strconv.Itoa(i+1), "location", arg.TypeName())
		}
	}

	return &Time{
		Value: time.Date(ymdHmsn[0], time.Month(ymdHmsn[1]), ymdHmsn[2],
			ymdHmsn[3], ymdHmsn[4], ymdHmsn[5], ymdHmsn[6], loc.Value),
	}, nil
}

func nowFunc() charlang.Object { return &Time{Value: time.Now()} }

func parseFunc(args ...charlang.Object) (charlang.Object, error) {
	return parseFuncEx(charlang.NewCall(nil, args))
}

func parseFuncEx(c charlang.Call) (charlang.Object, error) {
	size := c.Len()
	if size != 2 && size != 3 {
		return charlang.Undefined, charlang.ErrWrongNumArguments.NewError(
			"want=2..3 got=" + strconv.Itoa(size))
	}
	layout, ok := charlang.ToGoString(c.Get(0))
	if !ok {
		return newArgTypeErr("1st", "string", c.Get(0).TypeName())
	}
	value, ok := charlang.ToGoString(c.Get(1))
	if !ok {
		return newArgTypeErr("2nd", "string", c.Get(1).TypeName())
	}
	if size == 2 {
		tm, err := time.Parse(layout, value)
		if err != nil {
			return charlang.Undefined, err
		}
		return &Time{Value: tm}, nil
	}
	loc, ok := ToLocation(c.Get(2))
	if !ok {
		return newArgTypeErr("3rd", "location", c.Get(2).TypeName())
	}
	tm, err := time.ParseInLocation(layout, value, loc.Value)
	if err != nil {
		return charlang.Undefined, err
	}
	return &Time{Value: tm}, nil
}

func unixFunc(args ...charlang.Object) (charlang.Object, error) {
	return unixFuncEx(charlang.NewCall(nil, args))
}

func unixFuncEx(c charlang.Call) (charlang.Object, error) {
	size := c.Len()
	if size != 1 && size != 2 {
		return charlang.Undefined, charlang.ErrWrongNumArguments.NewError(
			"want=1..2 got=" + strconv.Itoa(size))
	}

	sec, ok := charlang.ToGoInt64(c.Get(0))
	if !ok {
		return newArgTypeErr("1st", "int", c.Get(0).TypeName())
	}

	var nsec int64
	if size > 1 {
		nsec, ok = charlang.ToGoInt64(c.Get(1))
		if !ok {
			return newArgTypeErr("2nd", "int", c.Get(1).TypeName())
		}
	}
	return &Time{Value: time.Unix(sec, nsec)}, nil
}

func isTimeFunc(o charlang.Object) charlang.Object {
	_, ok := o.(*Time)
	return charlang.Bool(ok)
}
