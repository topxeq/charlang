// Package strings provides strings module implementing simple functions to
// manipulate UTF-8 encoded strings for Charlang script language. It wraps Go's
// strings package functionalities.
package strings

import (
	"strconv"
	"strings"
	"unicode/utf8"

	"github.com/topxeq/charlang"
	"github.com/topxeq/charlang/stdlib"
)

// Module represents time module.
var Module = map[string]charlang.Object{
	// char:doc
	// # strings Module
	//
	// ## Functions
	// Contains(s string, substr string) -> bool
	// Reports whether substr is within s.
	"Contains": &charlang.Function{
		Name:    "Contains",
		Value:   stdlib.FuncPssRO(containsFunc),
		ValueEx: stdlib.FuncPssROEx(containsFunc),
	},
	// char:doc
	// ContainsAny(s string, chars string) -> bool
	// Reports whether any char in chars are within s.
	"ContainsAny": &charlang.Function{
		Name:    "ContainsAny",
		Value:   stdlib.FuncPssRO(containsAnyFunc),
		ValueEx: stdlib.FuncPssROEx(containsAnyFunc),
	},
	// char:doc
	// ContainsChar(s string, c char) -> bool
	// Reports whether the char c is within s.
	"ContainsChar": &charlang.Function{
		Name:    "ContainsChar",
		Value:   stdlib.FuncPsrRO(containsCharFunc),
		ValueEx: stdlib.FuncPsrROEx(containsCharFunc),
	},
	// char:doc
	// Count(s string, substr string) -> int
	// Counts the number of non-overlapping instances of substr in s.
	"Count": &charlang.Function{
		Name:    "Count",
		Value:   stdlib.FuncPssRO(countFunc),
		ValueEx: stdlib.FuncPssROEx(countFunc),
	},
	// char:doc
	// EqualFold(s string, t string) -> bool
	// EqualFold reports whether s and t, interpreted as UTF-8 strings,
	// are equal under Unicode case-folding, which is a more general form of
	// case-insensitivity.
	"EqualFold": &charlang.Function{
		Name:    "EqualFold",
		Value:   stdlib.FuncPssRO(equalFoldFunc),
		ValueEx: stdlib.FuncPssROEx(equalFoldFunc),
	},
	// char:doc
	// Fields(s string) -> array
	// Splits the string s around each instance of one or more consecutive white
	// space characters, returning an array of substrings of s or an empty array
	// if s contains only white space.
	"Fields": &charlang.Function{
		Name:    "Fields",
		Value:   stdlib.FuncPsRO(fieldsFunc),
		ValueEx: stdlib.FuncPsROEx(fieldsFunc),
	},
	// char:doc
	// FieldsFunc(s string, f func(char) bool) -> array
	// Splits the string s at each run of Unicode code points c satisfying f(c),
	// and returns an array of slices of s. If all code points in s satisfy
	// f(c) or the string is empty, an empty array is returned.
	"FieldsFunc": &charlang.Function{
		Name: "FieldsFunc",
		Value: func(args ...charlang.Object) (charlang.Object, error) {
			return fieldsFuncInv(charlang.NewCall(nil, args))
		},
		ValueEx: fieldsFuncInv,
	},
	// char:doc
	// HasPrefix(s string, prefix string) -> bool
	// Reports whether the string s begins with prefix.
	"HasPrefix": &charlang.Function{
		Name:    "HasPrefix",
		Value:   stdlib.FuncPssRO(hasPrefixFunc),
		ValueEx: stdlib.FuncPssROEx(hasPrefixFunc),
	},
	// char:doc
	// HasSuffix(s string, suffix string) -> bool
	// Reports whether the string s ends with prefix.
	"HasSuffix": &charlang.Function{
		Name:    "HasSuffix",
		Value:   stdlib.FuncPssRO(hasSuffixFunc),
		ValueEx: stdlib.FuncPssROEx(hasSuffixFunc),
	},
	// char:doc
	// Index(s string, substr string) -> int
	// Returns the index of the first instance of substr in s, or -1 if substr
	// is not present in s.
	"Index": &charlang.Function{
		Name:    "Index",
		Value:   stdlib.FuncPssRO(indexFunc),
		ValueEx: stdlib.FuncPssROEx(indexFunc),
	},
	// char:doc
	// IndexAny(s string, chars string) -> int
	// Returns the index of the first instance of any char from chars in s, or
	// -1 if no char from chars is present in s.
	"IndexAny": &charlang.Function{
		Name:    "IndexAny",
		Value:   stdlib.FuncPssRO(indexAnyFunc),
		ValueEx: stdlib.FuncPssROEx(indexAnyFunc),
	},
	// char:doc
	// IndexByte(s string, c char|int) -> int
	// Returns the index of the first byte value of c in s, or -1 if byte value
	// of c is not present in s. c's integer value must be between 0 and 255.
	"IndexByte": &charlang.Function{
		Name:    "IndexByte",
		Value:   stdlib.FuncPsrRO(indexByteFunc),
		ValueEx: stdlib.FuncPsrROEx(indexByteFunc),
	},
	// char:doc
	// IndexChar(s string, c char) -> int
	// Returns the index of the first instance of the char c, or -1 if char is
	// not present in s.
	"IndexChar": &charlang.Function{
		Name:    "IndexChar",
		Value:   stdlib.FuncPsrRO(indexCharFunc),
		ValueEx: stdlib.FuncPsrROEx(indexCharFunc),
	},
	// char:doc
	// IndexFunc(s string, f func(char) bool) -> int
	// Returns the index into s of the first Unicode code point satisfying f(c),
	// or -1 if none do.
	"IndexFunc": &charlang.Function{
		Name: "IndexFunc",
		Value: func(args ...charlang.Object) (charlang.Object, error) {
			return newIndexFuncInv(strings.IndexFunc)(charlang.NewCall(nil, args))
		},
		ValueEx: newIndexFuncInv(strings.IndexFunc),
	},
	// char:doc
	// Join(arr array, sep string) -> string
	// Concatenates the string values of array arr elements to create a
	// single string. The separator string sep is placed between elements in the
	// resulting string.
	"Join": &charlang.Function{
		Name:    "Join",
		Value:   stdlib.FuncPAsRO(joinFunc),
		ValueEx: stdlib.FuncPAsROEx(joinFunc),
	},
	// char:doc
	// LastIndex(s string, substr string) -> int
	// Returns the index of the last instance of substr in s, or -1 if substr
	// is not present in s.
	"LastIndex": &charlang.Function{
		Name:    "LastIndex",
		Value:   stdlib.FuncPssRO(lastIndexFunc),
		ValueEx: stdlib.FuncPssROEx(lastIndexFunc),
	},
	// char:doc
	// LastIndexAny(s string, chars string) -> int
	// Returns the index of the last instance of any char from chars in s, or
	// -1 if no char from chars is present in s.
	"LastIndexAny": &charlang.Function{
		Name:    "LastIndexAny",
		Value:   stdlib.FuncPssRO(lastIndexAnyFunc),
		ValueEx: stdlib.FuncPssROEx(lastIndexAnyFunc),
	},
	// char:doc
	// LastIndexByte(s string, c char|int) -> int
	// Returns the index of byte value of the last instance of c in s, or -1
	// if c is not present in s. c's integer value must be between 0 and 255.
	"LastIndexByte": &charlang.Function{
		Name:    "LastIndexByte",
		Value:   stdlib.FuncPsrRO(lastIndexByteFunc),
		ValueEx: stdlib.FuncPsrROEx(lastIndexByteFunc),
	},
	// char:doc
	// LastIndexFunc(s string, f func(char) bool) -> int
	// Returns the index into s of the last Unicode code point satisfying f(c),
	// or -1 if none do.
	"LastIndexFunc": &charlang.Function{
		Name: "LastIndexFunc",
		Value: func(args ...charlang.Object) (charlang.Object, error) {
			return newIndexFuncInv(strings.LastIndexFunc)(charlang.NewCall(nil, args))
		},
		ValueEx: newIndexFuncInv(strings.LastIndexFunc),
	},
	// char:doc
	// Map(f func(char) char, s string) -> string
	// Returns a copy of the string s with all its characters modified
	// according to the mapping function f. If f returns a negative value, the
	// character is dropped from the string with no replacement.
	"Map": &charlang.Function{
		Name: "Map",
		Value: func(args ...charlang.Object) (charlang.Object, error) {
			return mapFuncInv(charlang.NewCall(nil, args))
		},
		ValueEx: mapFuncInv,
	},
	// char:doc
	// PadLeft(s string, padLen int[, padWith any]) -> string
	// Returns a string that is padded on the left with the string `padWith` until
	// the `padLen` length is reached. If padWith is not given, a white space is
	// used as default padding.
	"PadLeft": &charlang.Function{
		Name: "PadLeft",
		Value: func(args ...charlang.Object) (charlang.Object, error) {
			return pad(charlang.NewCall(nil, args), true)
		},
		ValueEx: func(c charlang.Call) (charlang.Object, error) {
			return pad(c, true)
		},
	},
	// char:doc
	// PadRight(s string, padLen int[, padWith any]) -> string
	// Returns a string that is padded on the right with the string `padWith` until
	// the `padLen` length is reached. If padWith is not given, a white space is
	// used as default padding.
	"PadRight": &charlang.Function{
		Name: "PadRight",
		Value: func(args ...charlang.Object) (charlang.Object, error) {
			return pad(charlang.NewCall(nil, args), false)
		},
		ValueEx: func(c charlang.Call) (charlang.Object, error) {
			return pad(c, false)
		},
	},
	// char:doc
	// Repeat(s string, count int) -> string
	// Returns a new string consisting of count copies of the string s.
	//
	// - If count is a negative int, it returns empty string.
	// - If (len(s) * count) overflows, it panics.
	"Repeat": &charlang.Function{
		Name:    "Repeat",
		Value:   stdlib.FuncPsiRO(repeatFunc),
		ValueEx: stdlib.FuncPsiROEx(repeatFunc),
	},
	// char:doc
	// Replace(s string, old string, new string[, n int]) -> string
	// Returns a copy of the string s with the first n non-overlapping instances
	// of old replaced by new. If n is not provided or -1, it replaces all
	// instances.
	"Replace": &charlang.Function{
		Name: "Replace",
		Value: func(args ...charlang.Object) (charlang.Object, error) {
			return replaceFunc(charlang.NewCall(nil, args))
		},
		ValueEx: replaceFunc,
	},
	// char:doc
	// Split(s string, sep string[, n int]) -> [string]
	// Splits s into substrings separated by sep and returns an array of
	// the substrings between those separators.
	//
	// n determines the number of substrings to return:
	//
	// - n < 0: all substrings (default)
	// - n > 0: at most n substrings; the last substring will be the unsplit remainder.
	// - n == 0: the result is empty array
	"Split": &charlang.Function{
		Name: "Split",
		Value: func(args ...charlang.Object) (charlang.Object, error) {
			return newSplitFunc(strings.SplitN)(charlang.NewCall(nil, args))
		},
		ValueEx: newSplitFunc(strings.SplitN),
	},
	// char:doc
	// SplitAfter(s string, sep string[, n int]) -> [string]
	// Slices s into substrings after each instance of sep and returns an array
	// of those substrings.
	//
	// n determines the number of substrings to return:
	//
	// - n < 0: all substrings (default)
	// - n > 0: at most n substrings; the last substring will be the unsplit remainder.
	// - n == 0: the result is empty array
	"SplitAfter": &charlang.Function{
		Name: "SplitAfter",
		Value: func(args ...charlang.Object) (charlang.Object, error) {
			return newSplitFunc(strings.SplitAfterN)(charlang.NewCall(nil, args))
		},
		ValueEx: newSplitFunc(strings.SplitAfterN),
	},
	// char:doc
	// Title(s string) -> string
	// Deprecated: Returns a copy of the string s with all Unicode letters that
	// begin words mapped to their Unicode title case.
	"Title": &charlang.Function{
		Name:    "Title",
		Value:   stdlib.FuncPsRO(titleFunc),
		ValueEx: stdlib.FuncPsROEx(titleFunc),
	},
	// char:doc
	// ToLower(s string) -> string
	// Returns s with all Unicode letters mapped to their lower case.
	"ToLower": &charlang.Function{
		Name:    "ToLower",
		Value:   stdlib.FuncPsRO(toLowerFunc),
		ValueEx: stdlib.FuncPsROEx(toLowerFunc),
	},
	// char:doc
	// ToTitle(s string) -> string
	// Returns a copy of the string s with all Unicode letters mapped to their
	// Unicode title case.
	"ToTitle": &charlang.Function{
		Name:    "ToTitle",
		Value:   stdlib.FuncPsRO(toTitleFunc),
		ValueEx: stdlib.FuncPsROEx(toTitleFunc),
	},
	// char:doc
	// ToUpper(s string) -> string
	// Returns s with all Unicode letters mapped to their upper case.
	"ToUpper": &charlang.Function{
		Name:    "ToUpper",
		Value:   stdlib.FuncPsRO(toUpperFunc),
		ValueEx: stdlib.FuncPsROEx(toUpperFunc),
	},
	// char:doc
	// ToValidUTF8(s string[, replacement string]) -> string
	// Returns a copy of the string s with each run of invalid UTF-8 byte
	// sequences replaced by the replacement string, which may be empty.
	"ToValidUTF8": &charlang.Function{
		Name: "ToValidUTF8",
		Value: func(args ...charlang.Object) (charlang.Object, error) {
			return toValidUTF8Func(charlang.NewCall(nil, args))
		},
		ValueEx: toValidUTF8Func,
	},
	// char:doc
	// Trim(s string, cutset string) -> string
	// Returns a slice of the string s with all leading and trailing Unicode
	// code points contained in cutset removed.
	"Trim": &charlang.Function{
		Name:    "Trim",
		Value:   stdlib.FuncPssRO(trimFunc),
		ValueEx: stdlib.FuncPssROEx(trimFunc),
	},
	// char:doc
	// TrimFunc(s string, f func(char) bool) -> string
	// Returns a slice of the string s with all leading and trailing Unicode
	// code points satisfying f removed.
	"TrimFunc": &charlang.Function{
		Name: "TrimFunc",
		Value: func(args ...charlang.Object) (charlang.Object, error) {
			return newTrimFuncInv(strings.TrimFunc)(charlang.NewCall(nil, args))
		},
		ValueEx: newTrimFuncInv(strings.TrimFunc),
	},
	// char:doc
	// TrimLeft(s string, cutset string) -> string
	// Returns a slice of the string s with all leading Unicode code points
	// contained in cutset removed.
	"TrimLeft": &charlang.Function{
		Name:    "TrimLeft",
		Value:   stdlib.FuncPssRO(trimLeftFunc),
		ValueEx: stdlib.FuncPssROEx(trimLeftFunc),
	},
	// char:doc
	// TrimLeftFunc(s string, f func(char) bool) -> string
	// Returns a slice of the string s with all leading Unicode code points
	// c satisfying f(c) removed.
	"TrimLeftFunc": &charlang.Function{
		Name: "TrimLeftFunc",
		Value: func(args ...charlang.Object) (charlang.Object, error) {
			return newTrimFuncInv(strings.TrimLeftFunc)(charlang.NewCall(nil, args))
		},
		ValueEx: newTrimFuncInv(strings.TrimLeftFunc),
	},
	// char:doc
	// TrimPrefix(s string, prefix string) -> string
	// Returns s without the provided leading prefix string. If s doesn't start
	// with prefix, s is returned unchanged.
	"TrimPrefix": &charlang.Function{
		Name:    "TrimPrefix",
		Value:   stdlib.FuncPssRO(trimPrefixFunc),
		ValueEx: stdlib.FuncPssROEx(trimPrefixFunc),
	},
	// char:doc
	// TrimRight(s string, cutset string) -> string
	// Returns a slice of the string s with all trailing Unicode code points
	// contained in cutset removed.
	"TrimRight": &charlang.Function{
		Name:    "TrimRight",
		Value:   stdlib.FuncPssRO(trimRightFunc),
		ValueEx: stdlib.FuncPssROEx(trimRightFunc),
	},
	// char:doc
	// TrimRightFunc(s string, f func(char) bool) -> string
	// Returns a slice of the string s with all trailing Unicode code points
	// c satisfying f(c) removed.
	"TrimRightFunc": &charlang.Function{
		Name: "TrimRightFunc",
		Value: func(args ...charlang.Object) (charlang.Object, error) {
			return newTrimFuncInv(strings.TrimRightFunc)(charlang.NewCall(nil, args))
		},
		ValueEx: newTrimFuncInv(strings.TrimRightFunc),
	},
	// char:doc
	// TrimSpace(s string) -> string
	// Returns a slice of the string s, with all leading and trailing white
	// space removed, as defined by Unicode.
	"TrimSpace": &charlang.Function{
		Name:    "TrimSpace",
		Value:   stdlib.FuncPsRO(trimSpaceFunc),
		ValueEx: stdlib.FuncPsROEx(trimSpaceFunc),
	},
	// char:doc
	// TrimSuffix(s string, suffix string) -> string
	// Returns s without the provided trailing suffix string. If s doesn't end
	// with suffix, s is returned unchanged.
	"TrimSuffix": &charlang.Function{
		Name:    "TrimSuffix",
		Value:   stdlib.FuncPssRO(trimSuffixFunc),
		ValueEx: stdlib.FuncPssROEx(trimSuffixFunc),
	},
}

func containsFunc(s, substr string) charlang.Object {
	return charlang.Bool(strings.Contains(s, substr))
}

func containsAnyFunc(s, chars string) charlang.Object {
	return charlang.Bool(strings.ContainsAny(s, chars))
}

func containsCharFunc(s string, c rune) charlang.Object {
	return charlang.Bool(strings.ContainsRune(s, c))
}

func countFunc(s, substr string) charlang.Object {
	return charlang.Int(strings.Count(s, substr))
}

func equalFoldFunc(s, t string) charlang.Object {
	return charlang.Bool(strings.EqualFold(s, t))
}

func fieldsFunc(s string) charlang.Object {
	fields := strings.Fields(s)
	out := make(charlang.Array, 0, len(fields))
	for _, s := range fields {
		out = append(out, charlang.String{Value: s})
	}
	return out
}

func fieldsFuncInv(c charlang.Call) (charlang.Object, error) {
	return stringInvoke(c, 0, 1,
		func(s string, inv *charlang.Invoker) (charlang.Object, error) {
			var err error
			fields := strings.FieldsFunc(s, func(r rune) bool {
				if err != nil {
					return false
				}
				var ret charlang.Object
				ret, err = inv.Invoke(charlang.Char(r))
				if err != nil {
					return false
				}
				return !ret.IsFalsy()
			})
			if err != nil {
				return charlang.Undefined, err
			}
			out := make(charlang.Array, 0, len(fields))
			for _, s := range fields {
				out = append(out, charlang.String{Value: s})
			}
			return out, nil
		},
	)
}

func hasPrefixFunc(s, prefix string) charlang.Object {
	return charlang.Bool(strings.HasPrefix(s, prefix))
}

func hasSuffixFunc(s, suffix string) charlang.Object {
	return charlang.Bool(strings.HasSuffix(s, suffix))
}

func indexFunc(s, substr string) charlang.Object {
	return charlang.Int(strings.Index(s, substr))
}

func indexAnyFunc(s, chars string) charlang.Object {
	return charlang.Int(strings.IndexAny(s, chars))
}

func indexByteFunc(s string, c rune) charlang.Object {
	if c > 255 || c < 0 {
		return charlang.Int(-1)
	}
	return charlang.Int(strings.IndexByte(s, byte(c)))
}

func indexCharFunc(s string, c rune) charlang.Object {
	return charlang.Int(strings.IndexRune(s, c))
}

func joinFunc(arr charlang.Array, sep string) charlang.Object {
	elems := make([]string, len(arr))
	for i := range arr {
		elems[i] = arr[i].String()
	}
	return charlang.String{Value: strings.Join(elems, sep)})
}

func lastIndexFunc(s, substr string) charlang.Object {
	return charlang.Int(strings.LastIndex(s, substr))
}

func lastIndexAnyFunc(s, chars string) charlang.Object {
	return charlang.Int(strings.LastIndexAny(s, chars))
}

func lastIndexByteFunc(s string, c rune) charlang.Object {
	if c > 255 || c < 0 {
		return charlang.Int(-1)
	}
	return charlang.Int(strings.LastIndexByte(s, byte(c)))
}

func mapFuncInv(c charlang.Call) (charlang.Object, error) {
	return stringInvoke(c, 1, 0,
		func(s string, inv *charlang.Invoker) (charlang.Object, error) {
			var err error
			out := strings.Map(func(r rune) rune {
				if err != nil {
					return utf8.RuneError
				}
				var ret charlang.Object
				ret, err = inv.Invoke(charlang.Char(r))
				if err != nil {
					return 0
				}
				r, ok := charlang.ToGoRune(ret)
				if !ok {
					return utf8.RuneError
				}
				return r
			}, s)
			return charlang.String{Value: out}, err
		},
	)
}

func pad(c charlang.Call, left bool) (charlang.Object, error) {
	size := c.Len()
	if size != 2 && size != 3 {
		return charlang.Undefined,
			charlang.ErrWrongNumArguments.NewError("want=2..3 got=" + strconv.Itoa(size))
	}
	s := c.Get(0).String()
	padLen, ok := charlang.ToGoInt(c.Get(1))
	if !ok {
		return charlang.Undefined,
			charlang.NewArgumentTypeError("2nd", "int", c.Get(1).TypeName())
	}
	diff := padLen - len(s)
	if diff <= 0 {
		return charlang.String{Value: s}, nil
	}
	padWith := " "
	if size > 2 {
		if padWith = c.Get(2).String(); len(padWith) == 0 {
			return charlang.String{Value: s}, nil
		}
	}
	r := (diff-len(padWith))/len(padWith) + 2
	if r <= 0 {
		return charlang.String{Value: s}, nil
	}
	var sb strings.Builder
	sb.Grow(padLen)
	if left {
		sb.WriteString(strings.Repeat(padWith, r)[:diff])
		sb.WriteString(s)
	} else {
		sb.WriteString(s)
		sb.WriteString(strings.Repeat(padWith, r)[:diff])
	}
	return charlang.String{Value: sb.String()}, nil
}

func repeatFunc(s string, count int) charlang.Object {
	// if n is negative strings.Repeat function panics
	if count < 0 {
		return charlang.String{Value: ""}
	}
	return charlang.String{Value: strings.Repeat(s, count)}
}

func replaceFunc(c charlang.Call) (charlang.Object, error) {
	size := c.Len()
	if size != 3 && size != 4 {
		return charlang.Undefined,
			charlang.ErrWrongNumArguments.NewError("want=3..4 got=" + strconv.Itoa(size))
	}
	s := c.Get(0).String()
	old := c.Get(1).String()
	news := c.Get(2).String()
	n := -1
	if size == 4 {
		v, ok := charlang.ToGoInt(c.Get(3))
		if !ok {
			return charlang.Undefined,
				charlang.NewArgumentTypeError("4th", "int", c.Get(3).TypeName())
		}
		n = v
	}
	return charlang.String{Value: strings.Replace(s, old, news, n)}, nil
}

func titleFunc(s string) charlang.Object {
	//lint:ignore SA1019 Keep it for backward compatibility.
	return charlang.String{Value: strings.Title(s)} //nolint staticcheck Keep it for backward compatibility
}

func toLowerFunc(s string) charlang.Object { return charlang.String{Value: strings.ToLower(s)} }

func toTitleFunc(s string) charlang.Object { return charlang.String{Value: strings.ToTitle(s)} }

func toUpperFunc(s string) charlang.Object { return charlang.String{Value: strings.ToUpper(s)} }

func toValidUTF8Func(c charlang.Call) (charlang.Object, error) {
	size := c.Len()
	if size != 1 && size != 2 {
		return charlang.Undefined,
			charlang.ErrWrongNumArguments.NewError("want=1..2 got=" + strconv.Itoa(size))
	}
	s := c.Get(0).String()
	var repl string
	if size == 2 {
		repl = c.Get(1).String()
	}
	return charlang.String{Value: strings.ToValidUTF8(s, repl)}, nil
}

func trimFunc(s, cutset string) charlang.Object {
	return charlang.String{Value: strings.Trim(s, cutset)}
}

func trimLeftFunc(s, cutset string) charlang.Object {
	return charlang.String{Value: strings.TrimLeft(s, cutset)}
}

func trimPrefixFunc(s, prefix string) charlang.Object {
	return charlang.String{Value: strings.TrimPrefix(s, prefix)}
}

func trimRightFunc(s, cutset string) charlang.Object {
	return charlang.String{Value: strings.TrimRight(s, cutset)}
}

func trimSpaceFunc(s string) charlang.Object {
	return charlang.String{Value: strings.TrimSpace(s)}
}

func trimSuffixFunc(s, suffix string) charlang.Object {
	return charlang.String{Value: strings.TrimSuffix(s, suffix)}
}

func newSplitFunc(fn func(string, string, int) []string) charlang.CallableExFunc {
	return func(c charlang.Call) (charlang.Object, error) {
		size := c.Len()
		if size != 2 && size != 3 {
			return charlang.Undefined,
				charlang.ErrWrongNumArguments.NewError("want=2..3 got=" + strconv.Itoa(size))
		}
		s := c.Get(0).String()
		sep := c.Get(1).String()
		n := -1
		if size == 3 {
			v, ok := charlang.ToGoInt(c.Get(2))
			if !ok {
				return charlang.Undefined,
					charlang.NewArgumentTypeError("3rd", "int", c.Get(2).TypeName())
			}
			n = v
		}
		strs := fn(s, sep, n)
		out := make(charlang.Array, 0, len(strs))
		for _, s := range strs {
			out = append(out, charlang.String{Value: s})
		}
		return out, nil
	}
}

func newIndexFuncInv(fn func(string, func(rune) bool) int) charlang.CallableExFunc {
	return func(c charlang.Call) (charlang.Object, error) {
		return stringInvoke(c, 0, 1,
			func(s string, inv *charlang.Invoker) (charlang.Object, error) {
				var err error
				out := fn(s, func(r rune) bool {
					if err != nil {
						return false
					}
					var ret charlang.Object
					ret, err = inv.Invoke(charlang.Char(r))
					if err != nil {
						return false
					}
					return !ret.IsFalsy()
				})
				return charlang.Int(out), err
			},
		)
	}
}

func newTrimFuncInv(fn func(string, func(rune) bool) string) charlang.CallableExFunc {
	return func(c charlang.Call) (charlang.Object, error) {
		return stringInvoke(c, 0, 1,
			func(s string, inv *charlang.Invoker) (charlang.Object, error) {
				var err error
				out := fn(s, func(r rune) bool {
					if err != nil {
						return false
					}
					var ret charlang.Object
					ret, err = inv.Invoke(charlang.Char(r))
					if err != nil {
						return false
					}
					return !ret.IsFalsy()
				})
				return charlang.String{Value: out}, err
			},
		)
	}
}

func stringInvoke(
	c charlang.Call,
	sidx int,
	cidx int,
	fn func(string, *charlang.Invoker) (charlang.Object, error),
) (charlang.Object, error) {
	err := c.CheckLen(2)
	if err != nil {
		return charlang.Undefined, err
	}

	str := c.Get(sidx).String()
	callee := c.Get(cidx)
	if !callee.CanCall() {
		return charlang.Undefined, charlang.ErrNotCallable
	}
	if c.VM() == nil {
		if _, ok := callee.(*charlang.CompiledFunction); ok {
			return charlang.Undefined, charlang.ErrNotCallable
		}
	}

	inv := charlang.NewInvoker(c.VM(), callee)
	inv.Acquire()
	defer inv.Release()
	return fn(str, inv)
}
