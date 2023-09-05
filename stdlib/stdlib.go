package stdlib

//go:generate go run ../cmd/mkcallable -export -output zfuncs.go stdlib.go

// time module IsTime
// json module Marshal, Quote, NoQuote, NoEscape
//
//char:callable func(o charlang.Object) (ret charlang.Object)

// time module MountString, WeekdayString
//
//char:callable func(i1 int) (ret charlang.Object)

// time module DurationString, DurationHours, DurationMinutes, DurationSeconds
// DurationMilliseconds, DurationMicroseconds, DurationNanoseconds
//
//char:callable func(i1 int64) (ret charlang.Object)

// time module Sleep
//
//char:callable func(i1 int64)

// time module ParseDuration, LoadLocation
//
//char:callable func(s string) (ret charlang.Object, err error)

// time module FixedZone
// strings module Repeat
//
//char:callable func(s string, i1 int) (ret charlang.Object)

// time module Time, Now
//
//char:callable func() (ret charlang.Object)

// time module DurationRound, DurationTruncate
//
//char:callable func(i1 int64, i2 int64) (ret charlang.Object)

// json module Unmarshal, RawMessage, Valid
//
//char:callable func(b []byte) (ret charlang.Object)

// json module MarshalIndent
//
//char:callable func(o charlang.Object, s1 string, s2 string) (ret charlang.Object)

// json module Compact
//
//char:callable func(p []byte, b bool) (ret charlang.Object)

// json module Indent
//
//char:callable func(p []byte, s1 string, s2 string) (ret charlang.Object)

// strings module Contains, ContainsAny, Count, EqualFold, HasPrefix, HasSuffix
// Index, IndexAny, LastIndex, LastIndexAny, Trim, TrimLeft, TrimPrefix,
// TrimRight, TrimSuffix
//
//char:callable func(s1 string, s2 string) (ret charlang.Object)

// strings module Fields, Title, ToLower, ToTitle, ToUpper, TrimSpace
//
//char:callable func(s string) (ret charlang.Object)

// strings module ContainsChar, IndexByte, IndexChar, LastIndexByte
//
//char:callable func(s string, r rune) (ret charlang.Object)

// strings module Join
//
//char:callable func(arr charlang.Array, s string) (ret charlang.Object)

// misc. functions
//
//char:callable func(o charlang.Object, i int64) (ret charlang.Object, err error)
