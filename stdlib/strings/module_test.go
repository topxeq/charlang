package strings_test

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/require"

	. "github.com/topxeq/charlang"
	. "github.com/topxeq/charlang/stdlib/strings"
)

func TestModuleStrings(t *testing.T) {
	contains := Module["Contains"]
	ret, err := contains.Call(String{Value: "abc"}, String{Value: "b"})
	require.NoError(t, err)
	require.EqualValues(t, true, ret)
	ret, err = contains.Call(String{Value: "abc"}, String{Value: "d"})
	require.NoError(t, err)
	require.EqualValues(t, false, ret)
	_, err = contains.Call(String{Value: "abc"}, String{Value: "d"}, String{Value: "x"})
	require.Error(t, err)
	_, err = contains.Call(String{Value: "abc"})
	require.Error(t, err)
	_, err = contains.Call()
	require.Error(t, err)

	containsAny := Module["ContainsAny"]
	ret, err = containsAny.Call(String{Value: "abc"}, String{Value: "ax"})
	require.NoError(t, err)
	require.EqualValues(t, true, ret)
	ret, err = containsAny.Call(String{Value: "abc"}, String{Value: "d"})
	require.NoError(t, err)
	require.EqualValues(t, false, ret)

	containsChar := Module["ContainsChar"]
	ret, err = containsChar.Call(String{Value: "abc"}, Char('a'))
	require.NoError(t, err)
	require.EqualValues(t, true, ret)
	ret, err = containsChar.Call(String{Value: "abc"}, Char('d'))
	require.NoError(t, err)
	require.EqualValues(t, false, ret)

	count := Module["Count"]
	ret, err = count.Call(String{Value: "cheese"}, String{Value: "e"})
	require.NoError(t, err)
	require.EqualValues(t, 3, ret)
	ret, err = count.Call(String{Value: "cheese"}, String{Value: "d"})
	require.NoError(t, err)
	require.EqualValues(t, 0, ret)

	equalFold := Module["EqualFold"]
	ret, err = equalFold.Call(String{Value: "CHAR"}, String{Value: "char"})
	require.NoError(t, err)
	require.EqualValues(t, true, ret)
	ret, err = equalFold.Call(String{Value: "CHAR"}, String{Value: "chars"})
	require.NoError(t, err)
	require.EqualValues(t, false, ret)

	fields := Module["Fields"]
	ret, err = fields.Call(String{Value: "\tfoo bar\nbaz"})
	require.NoError(t, err)
	require.Equal(t, 3, len(ret.(Array)))
	require.EqualValues(t, "foo", ret.(Array)[0].(String).Value)
	require.EqualValues(t, "bar", ret.(Array)[1].(String).Value)
	require.EqualValues(t, "baz", ret.(Array)[2].(String).Value)

	hasPrefix := Module["HasPrefix"]
	ret, err = hasPrefix.Call(String{Value: "foobarbaz"}, String{Value: "foo"})
	require.NoError(t, err)
	require.EqualValues(t, true, ret)
	ret, err = hasPrefix.Call(String{Value: "foobarbaz"}, String{Value: "baz"})
	require.NoError(t, err)
	require.EqualValues(t, false, ret)

	hasSuffix := Module["HasSuffix"]
	ret, err = hasSuffix.Call(String{Value: "foobarbaz"}, String{Value: "baz"})
	require.NoError(t, err)
	require.EqualValues(t, true, ret)
	ret, err = hasSuffix.Call(String{Value: "foobarbaz"}, String{Value: "foo"})
	require.NoError(t, err)
	require.EqualValues(t, false, ret)

	index := Module["Index"]
	ret, err = index.Call(String{Value: "foobarbaz"}, String{Value: "bar"})
	require.NoError(t, err)
	require.EqualValues(t, 3, ret)
	ret, err = index.Call(String{Value: "foobarbaz"}, String{Value: "x"})
	require.NoError(t, err)
	require.EqualValues(t, -1, ret)

	indexAny := Module["IndexAny"]
	ret, err = indexAny.Call(String{Value: "foobarbaz"}, String{Value: "xz"})
	require.NoError(t, err)
	require.EqualValues(t, 8, ret)
	ret, err = indexAny.Call(String{Value: "foobarbaz"}, String{Value: "x"})
	require.NoError(t, err)
	require.EqualValues(t, -1, ret)

	indexByte := Module["IndexByte"]
	ret, err = indexByte.Call(String{Value: "foobarbaz"}, Char('z'))
	require.NoError(t, err)
	require.EqualValues(t, 8, ret)
	ret, err = indexByte.Call(String{Value: "foobarbaz"}, Int('z'))
	require.NoError(t, err)
	require.EqualValues(t, 8, ret)
	ret, err = indexByte.Call(String{Value: "foobarbaz"}, Char('x'))
	require.NoError(t, err)
	require.EqualValues(t, -1, ret)

	indexChar := Module["IndexChar"]
	ret, err = indexChar.Call(String{Value: "foobarbaz"}, Char('z'))
	require.NoError(t, err)
	require.EqualValues(t, 8, ret)
	ret, err = indexChar.Call(String{Value: "foobarbaz"}, Char('x'))
	require.NoError(t, err)
	require.EqualValues(t, -1, ret)

	join := Module["Join"]
	ret, err = join.Call(Array{String{Value: "foo"}, String{Value: "bar"}}, String{Value: ";"})
	require.NoError(t, err)
	require.EqualValues(t, "foo;bar", ret.String())

	lastIndex := Module["LastIndex"]
	ret, err = lastIndex.Call(String{Value: "zfoobarbaz"}, String{Value: "z"})
	require.NoError(t, err)
	require.EqualValues(t, 9, ret)
	ret, err = lastIndex.Call(String{Value: "zfoobarbaz"}, String{Value: "x"})
	require.NoError(t, err)
	require.EqualValues(t, -1, ret)

	lastIndexAny := Module["LastIndexAny"]
	ret, err = lastIndexAny.Call(String{Value: "zfoobarbaz"}, String{Value: "xz"})
	require.NoError(t, err)
	require.EqualValues(t, 9, ret)
	ret, err = lastIndexAny.Call(String{Value: "foobarbaz"}, String{Value: "o"})
	require.NoError(t, err)
	require.EqualValues(t, 2, ret)
	ret, err = lastIndexAny.Call(String{Value: "foobarbaz"}, String{Value: "p"})
	require.NoError(t, err)
	require.EqualValues(t, -1, ret)

	lastIndexByte := Module["LastIndexByte"]
	ret, err = lastIndexByte.Call(String{Value: "zfoobarbaz"}, Char('z'))
	require.NoError(t, err)
	require.EqualValues(t, 9, ret)
	ret, err = lastIndexByte.Call(String{Value: "zfoobarbaz"}, Int('z'))
	require.NoError(t, err)
	require.EqualValues(t, 9, ret)
	ret, err = lastIndexByte.Call(String{Value: "zfoobarbaz"}, Char('x'))
	require.NoError(t, err)
	require.EqualValues(t, -1, ret)

	padLeft := Module["PadLeft"]
	ret, err = padLeft.Call(String{Value: "abc"}, Int(3))
	require.NoError(t, err)
	require.EqualValues(t, "abc", ret.(String).Value)
	ret, err = padLeft.Call(String{Value: "abc"}, Int(4))
	require.NoError(t, err)
	require.EqualValues(t, " abc", ret.(String).Value)
	ret, err = padLeft.Call(String{Value: "abc"}, Int(5))
	require.NoError(t, err)
	require.EqualValues(t, "  abc", ret.(String).Value)
	ret, err = padLeft.Call(String{Value: "abc"}, Int(5), String{Value: "="})
	require.NoError(t, err)
	require.EqualValues(t, "==abc", ret.(String).Value)
	ret, err = padLeft.Call(String{Value: ""}, Int(6), String{Value: "="})
	require.NoError(t, err)
	require.EqualValues(t, "======", ret.(String).Value)

	padRight := Module["PadRight"]
	ret, err = padRight.Call(String{Value: "abc"}, Int(3))
	require.NoError(t, err)
	require.EqualValues(t, "abc", ret.(String).Value)
	ret, err = padRight.Call(String{Value: "abc"}, Int(4))
	require.NoError(t, err)
	require.EqualValues(t, "abc ", ret.(String).Value)
	ret, err = padRight.Call(String{Value: "abc"}, Int(5))
	require.NoError(t, err)
	require.EqualValues(t, "abc  ", ret.(String).Value)
	ret, err = padRight.Call(String{Value: "abc"}, Int(5), String{Value: "="})
	require.NoError(t, err)
	require.EqualValues(t, "abc==", ret.(String).Value)
	ret, err = padRight.Call(String{Value: ""}, Int(6), String{Value: "="})
	require.NoError(t, err)
	require.EqualValues(t, "======", ret.(String).Value)

	repeat := Module["Repeat"]
	ret, err = repeat.Call(String{Value: "abc"}, Int(3))
	require.NoError(t, err)
	require.EqualValues(t, "abcabcabc", ret.(String).Value)
	ret, err = repeat.Call(String{Value: "abc"}, Int(-1))
	require.NoError(t, err)
	require.EqualValues(t, "", ret.(String).Value)

	replace := Module["Replace"]
	ret, err = replace.Call(String{Value: "abcdefbc"}, String{Value: "bc"}, String{Value: "(bc)"})
	require.NoError(t, err)
	require.EqualValues(t, "a(bc)def(bc)", ret.(String).Value)
	ret, err = replace.Call(
		String{Value: "abcdefbc"}, String{Value: "bc"}, String{Value: "(bc)"}, Int(1))
	require.NoError(t, err)
	require.EqualValues(t, "a(bc)defbc", ret.(String).Value)

	split := Module["Split"]
	ret, err = split.Call(String{Value: "abc;def;"}, String{Value: ";"})
	require.NoError(t, err)
	require.Equal(t, 3, len(ret.(Array)))
	require.EqualValues(t, "abc", ret.(Array)[0].(String).Value)
	require.EqualValues(t, "def", ret.(Array)[1].(String).Value)
	require.EqualValues(t, "", ret.(Array)[2].(String).Value)
	ret, err = split.Call(String{Value: "abc;def;"}, String{Value: "!"}, Int(0))
	require.NoError(t, err)
	require.Equal(t, 0, len(ret.(Array)))
	ret, err = split.Call(String{Value: "abc;def;"}, String{Value: ";"}, Int(1))
	require.NoError(t, err)
	require.Equal(t, 1, len(ret.(Array)))
	require.EqualValues(t, "abc;def;", ret.(Array)[0].(String).Value)
	ret, err = split.Call(String{Value: "abc;def;"}, String{Value: ";"}, Int(2))
	require.NoError(t, err)
	require.Equal(t, 2, len(ret.(Array)))
	require.EqualValues(t, "abc", ret.(Array)[0].(String).Value)
	require.EqualValues(t, "def;", ret.(Array)[1].(String).Value)

	splitAfter := Module["SplitAfter"]
	ret, err = splitAfter.Call(String{Value: "abc;def;"}, String{Value: ";"})
	require.NoError(t, err)
	require.Equal(t, 3, len(ret.(Array)))
	require.EqualValues(t, "abc;", ret.(Array)[0].(String).Value)
	require.EqualValues(t, "def;", ret.(Array)[1].(String).Value)
	require.EqualValues(t, "", ret.(Array)[2].(String).Value)
	ret, err = splitAfter.Call(String{Value: "abc;def;"}, String{Value: "!"}, Int(0))
	require.NoError(t, err)
	require.Equal(t, 0, len(ret.(Array)))
	ret, err = splitAfter.Call(String{Value: "abc;def;"}, String{Value: ";"}, Int(1))
	require.NoError(t, err)
	require.Equal(t, 1, len(ret.(Array)))
	require.EqualValues(t, "abc;def;", ret.(Array)[0].(String).Value)
	ret, err = splitAfter.Call(String{Value: "abc;def;"}, String{Value: ";"}, Int(2))
	require.NoError(t, err)
	require.Equal(t, 2, len(ret.(Array)))
	require.EqualValues(t, "abc;", ret.(Array)[0].(String).Value)
	require.EqualValues(t, "def;", ret.(Array)[1].(String).Value)

	title := Module["Title"]
	ret, err = title.Call(String{Value: "хлеб"})
	require.NoError(t, err)
	require.EqualValues(t, "Хлеб", ret.(String).Value)

	toLower := Module["ToLower"]
	ret, err = toLower.Call(String{Value: "ÇİĞÖŞÜ"})
	require.NoError(t, err)
	require.EqualValues(t, "çiğöşü", ret.(String).Value)

	toTitle := Module["ToTitle"]
	ret, err = toTitle.Call(String{Value: "хлеб"})
	require.NoError(t, err)
	require.EqualValues(t, "ХЛЕБ", ret.(String).Value)

	toUpper := Module["ToUpper"]
	ret, err = toUpper.Call(String{Value: "çığöşü"})
	require.NoError(t, err)
	require.EqualValues(t, "ÇIĞÖŞÜ", ret.(String).Value)

	trim := Module["Trim"]
	ret, err = trim.Call(String{Value: "!!??abc?!"}, String{Value: "!?"})
	require.NoError(t, err)
	require.EqualValues(t, "abc", ret.(String).Value)

	trimLeft := Module["TrimLeft"]
	ret, err = trimLeft.Call(String{Value: "!!??abc?!"}, String{Value: "!?"})
	require.NoError(t, err)
	require.EqualValues(t, "abc?!", ret.(String).Value)

	trimPrefix := Module["TrimPrefix"]
	ret, err = trimPrefix.Call(String{Value: "abcdef"}, String{Value: "abc"})
	require.NoError(t, err)
	require.EqualValues(t, "def", ret.(String).Value)

	trimRight := Module["TrimRight"]
	ret, err = trimRight.Call(String{Value: "!!??abc?!"}, String{Value: "!?"})
	require.NoError(t, err)
	require.EqualValues(t, "!!??abc", ret.(String).Value)

	trimSpace := Module["TrimSpace"]
	ret, err = trimSpace.Call(String{Value: "\n \tabcdef\t \n"})
	require.NoError(t, err)
	require.EqualValues(t, "abcdef", ret.(String).Value)

	trimSuffix := Module["TrimSuffix"]
	ret, err = trimSuffix.Call(String{Value: "abcdef"}, String{Value: "def"})
	require.NoError(t, err)
	require.EqualValues(t, "abc", ret.(String).Value)
}

func TestScript(t *testing.T) {
	ret := func(s string) string {
		return fmt.Sprintf(`
		strings := import("strings")
		return %s`, s)
	}
	catch := func(s string) string {
		return fmt.Sprintf(`
		strings := import("strings")
		try {
			return %s
		} catch err {
			return string(err)
		}`, s)
	}
	wrongArgs := func(want, got int) String {
		return String{Value: ErrWrongNumArguments.NewError(
			fmt.Sprintf("want=%d got=%d", want, got),
		).String()}
	}
	nwrongArgs := func(want1, want2, got int) String {
		return String{Value: ErrWrongNumArguments.NewError(
			fmt.Sprintf("want=%d..%d got=%d", want1, want2, got),
		).String()}
	}
	typeErr := func(pos, expected, got string) String {
		return String{Value: NewArgumentTypeError(pos, expected, got).String()}
	}
	testCases := []struct {
		s string
		m func(string) string
		e Object
	}{
		{s: `strings.Contains()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.Contains(1)`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.Contains(1, 2, 3)`, m: catch, e: wrongArgs(2, 3)},
		{s: `strings.Contains(1, 2)`, e: False},
		{s: `strings.Contains("", 2)`, e: False},
		{s: `strings.Contains("acbdef", "de")`, e: True},
		{s: `strings.Contains("acbdef", "dex")`, e: False},

		{s: `strings.ContainsAny()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.ContainsAny(1)`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.ContainsAny(1, 2, 3)`, m: catch, e: wrongArgs(2, 3)},
		{s: `strings.ContainsAny(1, 2)`, e: False},
		{s: `strings.ContainsAny("", 2)`, e: False},
		{s: `strings.ContainsAny("acbdef", "de")`, e: True},
		{s: `strings.ContainsAny("acbdef", "xw")`, e: False},

		{s: `strings.ContainsChar()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.ContainsChar(1)`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.ContainsChar(1, 2, 3)`, m: catch, e: wrongArgs(2, 3)},
		{s: `strings.ContainsChar(1, 2)`, e: False},
		{s: `strings.ContainsChar("", 2)`, e: False},
		{s: `strings.ContainsChar("acbdef", 'd')`, e: True},
		{s: `strings.ContainsChar("acbdef", 'x')`, e: False},

		{s: `strings.Count()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.Count(1)`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.Count(1, 2, 3)`, m: catch, e: wrongArgs(2, 3)},
		{s: `strings.Count(1, 2)`, e: Int(0)},
		{s: `strings.Count("", 2)`, e: Int(0)},
		{s: `strings.Count("abcddef", "d")`, e: Int(2)},
		{s: `strings.Count("abcddef", "x")`, e: Int(0)},

		{s: `strings.EqualFold()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.EqualFold(1)`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.EqualFold(1, 2, 3)`, m: catch, e: wrongArgs(2, 3)},
		{s: `strings.EqualFold(1, 2)`, e: False},
		{s: `strings.EqualFold("", 2)`, e: False},
		{s: `strings.EqualFold("çğöşü", "ÇĞÖŞÜ")`, e: True},
		{s: `strings.EqualFold("x", "y")`, e: False},

		{s: `strings.Fields()`, m: catch, e: wrongArgs(1, 0)},
		{s: `strings.Fields(1)`, e: Array{String{Value: "1"}}},
		{s: `strings.Fields(1, 2)`, m: catch, e: wrongArgs(1, 2)},
		{s: `strings.Fields("a\nb c\td ")`,
			e: Array{String{Value: "a"}, String{Value: "b"}, String{Value: "c"}, String{Value: "d"}}},
		{s: `strings.Fields("")`, e: Array{}},

		{s: `strings.FieldsFunc()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.FieldsFunc("")`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.FieldsFunc("axbxcx", func(c){ return c == 'x' })`,
			e: Array{String{Value: "a"}, String{Value: "b"}, String{Value: "c"}}},
		{s: `strings.FieldsFunc("axbxcx", func(c){ return false })`,
			e: Array{String{Value: "axbxcx"}}},

		{s: `strings.HasPrefix()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.HasPrefix(1)`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.HasPrefix(1, 2, 3)`, m: catch, e: wrongArgs(2, 3)},
		{s: `strings.HasPrefix(1, 2)`, e: False},
		{s: `strings.HasPrefix("", 2)`, e: False},
		{s: `strings.HasPrefix("abcdef", "abcde")`, e: True},
		{s: `strings.HasPrefix("abcdef", "x")`, e: False},

		{s: `strings.HasSuffix()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.HasSuffix(1)`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.HasSuffix(1, 2, 3)`, m: catch, e: wrongArgs(2, 3)},
		{s: `strings.HasSuffix(1, 2)`, e: False},
		{s: `strings.HasSuffix("", 2)`, e: False},
		{s: `strings.HasSuffix("abcdef", "ef")`, e: True},
		{s: `strings.HasSuffix("abcdef", "abc")`, e: False},

		{s: `strings.Index()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.Index(1)`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.Index(1, 2, 3)`, m: catch, e: wrongArgs(2, 3)},
		{s: `strings.Index(1, 2)`, e: Int(-1)},
		{s: `strings.Index("", 2)`, e: Int(-1)},
		{s: `strings.Index("abcdef", "ef")`, e: Int(4)},
		{s: `strings.Index("abcdef", "x")`, e: Int(-1)},

		{s: `strings.IndexAny()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.IndexAny(1)`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.IndexAny(1, 2, 3)`, m: catch, e: wrongArgs(2, 3)},
		{s: `strings.IndexAny(1, 2)`, e: Int(-1)},
		{s: `strings.IndexAny("", 2)`, e: Int(-1)},
		{s: `strings.IndexAny("abcdef", "ef")`, e: Int(4)},
		{s: `strings.IndexAny("abcdef", "x")`, e: Int(-1)},
		{s: `strings.IndexAny("abcdef", "xa")`, e: Int(0)},

		{s: `strings.IndexByte()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.IndexByte(1)`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.IndexByte(1, 2, 3)`, m: catch, e: wrongArgs(2, 3)},
		{s: `strings.IndexByte(1, 2)`, e: Int(-1)},
		{s: `strings.IndexByte("", "")`, e: Int(-1)},
		{s: `strings.IndexByte("abcdef", 'b')`, e: Int(1)},
		{s: `strings.IndexByte("abcdef", int('c'))`, e: Int(2)},
		{s: `strings.IndexByte("abcdef", 'g')`, e: Int(-1)},
		{s: `strings.IndexByte("abcdef", int('g'))`, e: Int(-1)},

		{s: `strings.IndexChar()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.IndexChar(1)`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.IndexChar(1, 2, 3)`, m: catch, e: wrongArgs(2, 3)},
		{s: `strings.IndexChar(1, 2)`, e: Int(-1)},
		{s: `strings.IndexChar("", 1)`, e: Int(-1)},
		{s: `strings.IndexChar("abcdef", 'c')`, e: Int(2)},

		{s: `strings.IndexFunc()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.IndexFunc("")`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.IndexFunc("abcd", func(c){return c == 'c'})`, e: Int(2)},
		{s: `strings.IndexFunc("abcd", func(c){return c == 'e'})`, e: Int(-1)},

		{s: `strings.Join()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.Join(1)`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.Join(1, 2, 3)`, m: catch, e: wrongArgs(2, 3)},
		{s: `strings.Join(1, 2)`, m: catch,
			e: typeErr("1st", "array", "int")},
		{s: `strings.Join([], 1)`, e: String{Value: ""}},
		{s: `strings.Join(["a", "b", "c"], "\t")`, e: String{Value: "a\tb\tc"}},

		{s: `strings.LastIndex()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.LastIndex(1)`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.LastIndex(1, 2, 3)`, m: catch, e: wrongArgs(2, 3)},
		{s: `strings.LastIndex(1, 2)`, e: Int(-1)},
		{s: `strings.LastIndex("", 2)`, e: Int(-1)},
		{s: `strings.LastIndex("efabcdef", "ef")`, e: Int(6)},
		{s: `strings.LastIndex("abcdef", "g")`, e: Int(-1)},

		{s: `strings.LastIndexAny()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.LastIndexAny(1)`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.LastIndexAny(1, 2, 3)`, m: catch, e: wrongArgs(2, 3)},
		{s: `strings.LastIndexAny(1, 2)`, e: Int(-1)},
		{s: `strings.LastIndexAny("", 2)`, e: Int(-1)},
		{s: `strings.LastIndexAny("efabcdef", "xf")`, e: Int(7)},
		{s: `strings.LastIndexAny("abcdef", "g")`, e: Int(-1)},

		{s: `strings.LastIndexByte()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.LastIndexByte(1)`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.LastIndexByte(1, 2, 3)`, m: catch, e: wrongArgs(2, 3)},
		{s: `strings.LastIndexByte(1, 2)`, e: Int(-1)},
		{s: `strings.LastIndexByte("", "")`, e: Int(-1)},
		{s: `strings.LastIndexByte("efabcdef", 'f')`, e: Int(7)},
		{s: `strings.LastIndexByte("efabcdef", int('f'))`, e: Int(7)},
		{s: `strings.LastIndexByte("abcdef", 'g')`, e: Int(-1)},

		{s: `strings.LastIndexFunc()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.LastIndexFunc("")`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.LastIndexFunc("acbcd", func(c){return c=='c'})`, e: Int(3)},
		{s: `strings.LastIndexFunc("abcd", func(c){return c=='e'})`, e: Int(-1)},

		{s: `strings.Map()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.Map(func(){})`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.Map(
			func(c){
				if c == 'a' { return 'I' }
				if c == 'b' { return '❤' }
				if c == 'c' { return 'C' }
				if c == 'd' { return 'h' }
				if c == 'e' { return 'a' }
				if c == 'f' { return 'r' }
				return c
			},
			"abcdef")`, e: String{Value: "I❤Char"}},
		{s: `strings.Map(func(c){return c}, "test")`,
			m: catch, e: String{Value: "test"}},

		{s: `strings.PadLeft()`, m: catch, e: nwrongArgs(2, 3, 0)},
		{s: `strings.PadLeft(1)`, m: catch, e: nwrongArgs(2, 3, 1)},
		{s: `strings.PadLeft(1, 2, 3, 4)`, m: catch, e: nwrongArgs(2, 3, 4)},
		{s: `strings.PadLeft(1, 2, 3)`, e: String{Value: "31"}},
		{s: `strings.PadLeft("", "", "")`, m: catch,
			e: typeErr("2nd", "int", "string")},
		{s: `strings.PadLeft("", 0, "")`, e: String{Value: ""}},
		{s: `strings.PadLeft("", -1, "")`, e: String{Value: ""}},
		{s: `strings.PadLeft("", 1, "")`, e: String{Value: ""}},
		{s: `strings.PadLeft("", 1, "x")`, e: String{Value: "x"}},
		{s: `strings.PadLeft("abc", 3)`, e: String{Value: "abc"}},
		{s: `strings.PadLeft("abc", 4)`, e: String{Value: " abc"}},
		{s: `strings.PadLeft("abc", 5)`, e: String{Value: "  abc"}},
		{s: `strings.PadLeft("abc", 6)`, e: String{Value: "   abc"}},
		{s: `strings.PadLeft("abc", -1, "x")`, e: String{Value: "abc"}},
		{s: `strings.PadLeft("abc", 0, "x")`, e: String{Value: "abc"}},
		{s: `strings.PadLeft("abc", 1, "x")`, e: String{Value: "abc"}},
		{s: `strings.PadLeft("abc", 2, "x")`, e: String{Value: "abc"}},
		{s: `strings.PadLeft("abc", 3, "x")`, e: String{Value: "abc"}},
		{s: `strings.PadLeft("abc", 4, "x")`, e: String{Value: "xabc"}},
		{s: `strings.PadLeft("abc", 5, "x")`, e: String{Value: "xxabc"}},
		{s: `strings.PadLeft("abc", 5, "xy")`, e: String{Value: "xyabc"}},
		{s: `strings.PadLeft("abc", 6, "xy")`, e: String{Value: "xyxabc"}},
		{s: `strings.PadLeft("abc", 6, "wxyz")`, e: String{Value: "wxyabc"}},

		{s: `strings.PadRight()`, m: catch, e: nwrongArgs(2, 3, 0)},
		{s: `strings.PadRight(1)`, m: catch, e: nwrongArgs(2, 3, 1)},
		{s: `strings.PadRight(1, 2, 3, 4)`, m: catch, e: nwrongArgs(2, 3, 4)},
		{s: `strings.PadRight(1, 2, 3)`, e: String{Value: "13"}},
		{s: `strings.PadRight("", "", "")`, m: catch,
			e: typeErr("2nd", "int", "string")},
		{s: `strings.PadRight("", 0, "")`, e: String{Value: ""}},
		{s: `strings.PadRight("", -1, "")`, e: String{Value: ""}},
		{s: `strings.PadRight("", 1, "")`, e: String{Value: ""}},
		{s: `strings.PadRight("", 1, "x")`, e: String{Value: "x"}},
		{s: `strings.PadRight("abc", 3)`, e: String{Value: "abc"}},
		{s: `strings.PadRight("abc", 4)`, e: String{Value: "abc "}},
		{s: `strings.PadRight("abc", 5)`, e: String{Value: "abc  "}},
		{s: `strings.PadRight("abc", 6)`, e: String{Value: "abc   "}},
		{s: `strings.PadRight("abc", -1, "x")`, e: String{Value: "abc"}},
		{s: `strings.PadRight("abc", 0, "x")`, e: String{Value: "abc"}},
		{s: `strings.PadRight("abc", 1, "x")`, e: String{Value: "abc"}},
		{s: `strings.PadRight("abc", 2, "x")`, e: String{Value: "abc"}},
		{s: `strings.PadRight("abc", 3, "x")`, e: String{Value: "abc"}},
		{s: `strings.PadRight("abc", 4, "x")`, e: String{Value: "abcx"}},
		{s: `strings.PadRight("abc", 5, "x")`, e: String{Value: "abcxx"}},
		{s: `strings.PadRight("abc", 5, "xy")`, e: String{Value: "abcxy"}},
		{s: `strings.PadRight("abc", 6, "xy")`, e: String{Value: "abcxyx"}},
		{s: `strings.PadRight("abc", 6, "wxyz")`, e: String{Value: "abcwxy"}},

		{s: `strings.Repeat()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.Repeat(1)`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.Repeat(1, 2, 3)`, m: catch, e: wrongArgs(2, 3)},
		{s: `strings.Repeat(1, 2)`, e: String{Value: "11"}},
		{s: `strings.Repeat("", "")`, m: catch,
			e: typeErr("2nd", "int", "string")},
		{s: `strings.Repeat("a", -1)`, e: String{Value: ""}},
		{s: `strings.Repeat("a", 0)`, e: String{Value: ""}},
		{s: `strings.Repeat("a", 2)`, e: String{Value: "aa"}},

		{s: `strings.Replace()`, m: catch, e: nwrongArgs(3, 4, 0)},
		{s: `strings.Replace(1)`, m: catch, e: nwrongArgs(3, 4, 1)},
		{s: `strings.Replace(1, 2)`, m: catch, e: nwrongArgs(3, 4, 2)},
		{s: `strings.Replace(1, 2, 3, 4, 5)`, m: catch, e: nwrongArgs(3, 4, 5)},
		{s: `strings.Replace(1, 2, 3)`, e: String{Value: "1"}},
		{s: `strings.Replace("", 1, 3)`, e: String{Value: ""}},
		{s: `strings.Replace("", "", 1)`, e: String{Value: "1"}},
		{s: `strings.Replace("", "", "", "")`, m: catch,
			e: typeErr("4th", "int", "string")},
		{s: `strings.Replace("abc", "s", "ş")`, e: String{Value: "abc"}},
		{s: `strings.Replace("abbc", "b", "a")`, e: String{Value: "aaac"}},
		{s: `strings.Replace("abbc", "b", "a", 0)`, e: String{Value: "abbc"}},
		{s: `strings.Replace("abbc", "b", "a", 1)`, e: String{Value: "aabc"}},

		{s: `strings.Split()`, m: catch, e: nwrongArgs(2, 3, 0)},
		{s: `strings.Split(1)`, m: catch, e: nwrongArgs(2, 3, 1)},
		{s: `strings.Split(1, 2, 3, 4)`, m: catch, e: nwrongArgs(2, 3, 4)},
		{s: `strings.Split(1, 2)`, e: Array{String{Value: "1"}}},
		{s: `strings.Split("", 1, 3)`, e: Array{String{Value: ""}}},
		{s: `strings.Split("", "", "")`, m: catch,
			e: typeErr("3rd", "int", "string")},
		{s: `strings.Split("a.b.c", ".", 0)`, e: Array{}},
		{s: `strings.Split("a.b.c", ".", 1)`, e: Array{String{Value: "a.b.c"}}},
		{s: `strings.Split("a.b.c", ".", -1)`,
			e: Array{String{Value: "a"}, String{Value: "b"}, String{Value: "c"}}},
		{s: `strings.Split("a.b.c", ".")`,
			e: Array{String{Value: "a"}, String{Value: "b"}, String{Value: "c"}}},
		{s: `strings.Split("a.b.c.", ".")`,
			e: Array{String{Value: "a"}, String{Value: "b"}, String{Value: "c"}, String{Value: ""}}},
		{s: `strings.Split("a.b.c.", ".", 5)`,
			e: Array{String{Value: "a"}, String{Value: "b"}, String{Value: "c"}, String{Value: ""}}},

		{s: `strings.SplitAfter()`, m: catch, e: nwrongArgs(2, 3, 0)},
		{s: `strings.SplitAfter(1)`, m: catch, e: nwrongArgs(2, 3, 1)},
		{s: `strings.SplitAfter(1, 2, 3, 4)`, m: catch, e: nwrongArgs(2, 3, 4)},
		{s: `strings.SplitAfter(1, 2)`, e: Array{String{Value: "1"}}},
		{s: `strings.SplitAfter("", 1, 3)`, e: Array{String{Value: ""}}},
		{s: `strings.SplitAfter("", "", "")`, m: catch,
			e: typeErr("3rd", "int", "string")},
		{s: `strings.SplitAfter("a.b.c", ".", 0)`, e: Array{}},
		{s: `strings.SplitAfter("a.b.c", ".", 1)`, e: Array{String{Value: "a.b.c"}}},
		{s: `strings.SplitAfter("a.b.c", ".", -1)`,
			e: Array{String{Value: "a."}, String{Value: "b."}, String{Value: "c"}}},
		{s: `strings.SplitAfter("a.b.c", ".")`,
			e: Array{String{Value: "a."}, String{Value: "b."}, String{Value: "c"}}},
		{s: `strings.SplitAfter("a.b.c.", ".")`,
			e: Array{String{Value: "a."}, String{Value: "b."}, String{Value: "c."}, String{Value: ""}}},
		{s: `strings.SplitAfter("a.b.c.", ".", 5)`,
			e: Array{String{Value: "a."}, String{Value: "b."}, String{Value: "c."}, String{Value: ""}}},

		{s: `strings.Title()`, m: catch, e: wrongArgs(1, 0)},
		{s: `strings.Title(1, 2)`, m: catch, e: wrongArgs(1, 2)},
		{s: `strings.Title(1)`, e: String{Value: "1"}},
		{s: `strings.Title("")`, e: String{Value: ""}},
		{s: `strings.Title("abc def")`, e: String{Value: "Abc Def"}},

		{s: `strings.ToLower()`, m: catch, e: wrongArgs(1, 0)},
		{s: `strings.ToLower(1, 2)`, m: catch, e: wrongArgs(1, 2)},
		{s: `strings.ToLower(1)`, e: String{Value: "1"}},
		{s: `strings.ToLower("")`, e: String{Value: ""}},
		{s: `strings.ToLower("XYZ")`, e: String{Value: "xyz"}},

		{s: `strings.ToTitle()`, m: catch, e: wrongArgs(1, 0)},
		{s: `strings.ToTitle(1, 2)`, m: catch, e: wrongArgs(1, 2)},
		{s: `strings.ToTitle(1)`, e: String{Value: "1"}},
		{s: `strings.ToTitle("")`, e: String{Value: ""}},
		{s: `strings.ToTitle("çğ öşü")`, e: String{Value: "ÇĞ ÖŞÜ"}},

		{s: `strings.ToUpper()`, m: catch, e: wrongArgs(1, 0)},
		{s: `strings.ToUpper(1, 2)`, m: catch, e: wrongArgs(1, 2)},
		{s: `strings.ToUpper(1)`, e: String{Value: "1"}},
		{s: `strings.ToUpper("")`, e: String{Value: ""}},
		{s: `strings.ToUpper("çğ öşü")`, e: String{Value: "ÇĞ ÖŞÜ"}},

		{s: `strings.ToValidUTF8()`, m: catch, e: nwrongArgs(1, 2, 0)},
		{s: `strings.ToValidUTF8(1, 2, 2)`, m: catch, e: nwrongArgs(1, 2, 3)},
		{s: `strings.ToValidUTF8("a")`, e: String{Value: "a"}},
		{s: `strings.ToValidUTF8("a☺\xffb☺\xC0\xAFc☺\xff", "日本語")`, e: String{Value: "a☺日本語b☺日本語c☺日本語"}},
		{s: `strings.ToValidUTF8("a☺\xffb☺\xC0\xAFc☺\xff", "")`, e: String{Value: "a☺b☺c☺"}},

		{s: `strings.Trim()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.Trim(1)`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.Trim(1, 2, 3)`, m: catch, e: wrongArgs(2, 3)},
		{s: `strings.Trim(1, 2)`, e: String{Value: "1"}},
		{s: `strings.Trim("", 2)`, e: String{Value: ""}},
		{s: `strings.Trim("!!xyz!!", "")`, e: String{Value: "!!xyz!!"}},
		{s: `strings.Trim("!!xyz!!", "!")`, e: String{Value: "xyz"}},
		{s: `strings.Trim("!!xyz!!", "!?")`, e: String{Value: "xyz"}},

		{s: `strings.TrimFunc()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.TrimFunc("")`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.TrimFunc("xabcxx",
			func(c){return c=='x'})`, e: String{Value: "abc"}},

		{s: `strings.TrimLeft()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.TrimLeft(1)`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.TrimLeft(1, 2, 3)`, m: catch, e: wrongArgs(2, 3)},
		{s: `strings.TrimLeft(1, 2)`, e: String{Value: "1"}},
		{s: `strings.TrimLeft("", 2)`, e: String{Value: ""}},
		{s: `strings.TrimLeft("!!xyz!!", "")`, e: String{Value: "!!xyz!!"}},
		{s: `strings.TrimLeft("!!xyz!!", "!")`, e: String{Value: "xyz!!"}},
		{s: `strings.TrimLeft("!!?xyz!!", "!?")`, e: String{Value: "xyz!!"}},

		{s: `strings.TrimLeftFunc()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.TrimLeftFunc("")`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.TrimLeftFunc("xxabcxx",
			func(c){return c=='x'})`, e: String{Value: "abcxx"}},
		{s: `strings.TrimLeftFunc("abcxx",
			func(c){return c=='x'})`, e: String{Value: "abcxx"}},

		{s: `strings.TrimPrefix()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.TrimPrefix(1)`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.TrimPrefix(1, 2, 3)`, m: catch, e: wrongArgs(2, 3)},
		{s: `strings.TrimPrefix(1, 2)`, e: String{Value: "1"}},
		{s: `strings.TrimPrefix("", 2)`, e: String{Value: ""}},
		{s: `strings.TrimPrefix("!!xyz!!", "")`, e: String{Value: "!!xyz!!"}},
		{s: `strings.TrimPrefix("!!xyz!!", "!")`, e: String{Value: "!xyz!!"}},
		{s: `strings.TrimPrefix("!!xyz!!", "!!")`, e: String{Value: "xyz!!"}},
		{s: `strings.TrimPrefix("!!xyz!!", "!!x")`, e: String{Value: "yz!!"}},

		{s: `strings.TrimRight()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.TrimRight(1)`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.TrimRight(1, 2, 3)`, m: catch, e: wrongArgs(2, 3)},
		{s: `strings.TrimRight(1, 2)`, e: String{Value: "1"}},
		{s: `strings.TrimRight("", 2)`, e: String{Value: ""}},
		{s: `strings.TrimRight("!!xyz!!", "")`, e: String{Value: "!!xyz!!"}},
		{s: `strings.TrimRight("!!xyz!!", "!")`, e: String{Value: "!!xyz"}},
		{s: `strings.TrimRight("!!xyz?!!", "!?")`, e: String{Value: "!!xyz"}},

		{s: `strings.TrimRightFunc()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.TrimRightFunc("")`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.TrimRightFunc("xxabcxx",
			func(c){return c=='x'})`, e: String{Value: "xxabc"}},
		{s: `strings.TrimRightFunc("xxabc",
			func(c){return c=='x'})`, e: String{Value: "xxabc"}},

		{s: `strings.TrimSpace()`, m: catch, e: wrongArgs(1, 0)},
		{s: `strings.TrimSpace(1, 2)`, m: catch, e: wrongArgs(1, 2)},
		{s: `strings.TrimSpace(1)`, e: String{Value: "1"}},
		{s: `strings.TrimSpace(" \txyz\n\r")`, e: String{Value: "xyz"}},
		{s: `strings.TrimSpace("xyz")`, e: String{Value: "xyz"}},

		{s: `strings.TrimSuffix()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.TrimSuffix(1)`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.TrimSuffix(1, 2, 3)`, m: catch, e: wrongArgs(2, 3)},
		{s: `strings.TrimSuffix(1, 2)`, e: String{Value: "1"}},
		{s: `strings.TrimSuffix("", 2)`, e: String{Value: ""}},
		{s: `strings.TrimSuffix("!!xyz!!", "")`, e: String{Value: "!!xyz!!"}},
		{s: `strings.TrimSuffix("!!xyz!!", "!")`, e: String{Value: "!!xyz!"}},
		{s: `strings.TrimSuffix("!!xyz!!", "!!")`, e: String{Value: "!!xyz"}},
		{s: `strings.TrimSuffix("!!xyz!!", "z!!")`, e: String{Value: "!!xy"}},
	}
	for _, tt := range testCases {
		var s string
		if tt.m == nil {
			s = ret(tt.s)
		} else {
			s = catch(tt.s)
		}
		t.Run(tt.s, func(t *testing.T) {
			expectRun(t, s, tt.e)
		})
	}
}

func expectRun(t *testing.T, script string, expected Object) {
	t.Helper()
	mm := NewModuleMap()
	mm.AddBuiltinModule("strings", Module)
	c := DefaultCompilerOptions
	c.ModuleMap = mm
	bc, err := Compile([]byte(script), &c)
	require.NoError(t, err)
	ret, err := NewVM(bc).Run(nil)
	require.NoError(t, err)
	require.Equal(t, expected, ret)
}
