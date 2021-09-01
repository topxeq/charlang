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
	ret, err := contains.Call(String("abc"), String("b"))
	require.NoError(t, err)
	require.EqualValues(t, true, ret)
	ret, err = contains.Call(String("abc"), String("d"))
	require.NoError(t, err)
	require.EqualValues(t, false, ret)
	_, err = contains.Call(String("abc"), String("d"), String("x"))
	require.Error(t, err)
	_, err = contains.Call(String("abc"))
	require.Error(t, err)
	_, err = contains.Call()
	require.Error(t, err)

	containsAny := Module["ContainsAny"]
	ret, err = containsAny.Call(String("abc"), String("ax"))
	require.NoError(t, err)
	require.EqualValues(t, true, ret)
	ret, err = containsAny.Call(String("abc"), String("d"))
	require.NoError(t, err)
	require.EqualValues(t, false, ret)

	containsChar := Module["ContainsChar"]
	ret, err = containsChar.Call(String("abc"), Char('a'))
	require.NoError(t, err)
	require.EqualValues(t, true, ret)
	ret, err = containsChar.Call(String("abc"), Char('d'))
	require.NoError(t, err)
	require.EqualValues(t, false, ret)

	count := Module["Count"]
	ret, err = count.Call(String("cheese"), String("e"))
	require.NoError(t, err)
	require.EqualValues(t, 3, ret)
	ret, err = count.Call(String("cheese"), String("d"))
	require.NoError(t, err)
	require.EqualValues(t, 0, ret)

	equalFold := Module["EqualFold"]
	ret, err = equalFold.Call(String("UGO"), String("ugo"))
	require.NoError(t, err)
	require.EqualValues(t, true, ret)
	ret, err = equalFold.Call(String("UGO"), String("go"))
	require.NoError(t, err)
	require.EqualValues(t, false, ret)

	fields := Module["Fields"]
	ret, err = fields.Call(String("\tfoo bar\nbaz"))
	require.NoError(t, err)
	require.Equal(t, 3, len(ret.(Array)))
	require.EqualValues(t, "foo", ret.(Array)[0].(String))
	require.EqualValues(t, "bar", ret.(Array)[1].(String))
	require.EqualValues(t, "baz", ret.(Array)[2].(String))

	hasPrefix := Module["HasPrefix"]
	ret, err = hasPrefix.Call(String("foobarbaz"), String("foo"))
	require.NoError(t, err)
	require.EqualValues(t, true, ret)
	ret, err = hasPrefix.Call(String("foobarbaz"), String("baz"))
	require.NoError(t, err)
	require.EqualValues(t, false, ret)

	hasSuffix := Module["HasSuffix"]
	ret, err = hasSuffix.Call(String("foobarbaz"), String("baz"))
	require.NoError(t, err)
	require.EqualValues(t, true, ret)
	ret, err = hasSuffix.Call(String("foobarbaz"), String("foo"))
	require.NoError(t, err)
	require.EqualValues(t, false, ret)

	index := Module["Index"]
	ret, err = index.Call(String("foobarbaz"), String("bar"))
	require.NoError(t, err)
	require.EqualValues(t, 3, ret)
	ret, err = index.Call(String("foobarbaz"), String("x"))
	require.NoError(t, err)
	require.EqualValues(t, -1, ret)

	indexAny := Module["IndexAny"]
	ret, err = indexAny.Call(String("foobarbaz"), String("xz"))
	require.NoError(t, err)
	require.EqualValues(t, 8, ret)
	ret, err = indexAny.Call(String("foobarbaz"), String("x"))
	require.NoError(t, err)
	require.EqualValues(t, -1, ret)

	indexByte := Module["IndexByte"]
	ret, err = indexByte.Call(String("foobarbaz"), Char('z'))
	require.NoError(t, err)
	require.EqualValues(t, 8, ret)
	ret, err = indexByte.Call(String("foobarbaz"), Int('z'))
	require.NoError(t, err)
	require.EqualValues(t, 8, ret)
	ret, err = indexByte.Call(String("foobarbaz"), Char('x'))
	require.NoError(t, err)
	require.EqualValues(t, -1, ret)

	indexChar := Module["IndexChar"]
	ret, err = indexChar.Call(String("foobarbaz"), Char('z'))
	require.NoError(t, err)
	require.EqualValues(t, 8, ret)
	ret, err = indexChar.Call(String("foobarbaz"), Char('x'))
	require.NoError(t, err)
	require.EqualValues(t, -1, ret)

	join := Module["Join"]
	ret, err = join.Call(Array{String("foo"), String("bar")}, String(";"))
	require.NoError(t, err)
	require.EqualValues(t, "foo;bar", ret)

	lastIndex := Module["LastIndex"]
	ret, err = lastIndex.Call(String("zfoobarbaz"), String("z"))
	require.NoError(t, err)
	require.EqualValues(t, 9, ret)
	ret, err = lastIndex.Call(String("zfoobarbaz"), String("x"))
	require.NoError(t, err)
	require.EqualValues(t, -1, ret)

	lastIndexAny := Module["LastIndexAny"]
	ret, err = lastIndexAny.Call(String("zfoobarbaz"), String("xz"))
	require.NoError(t, err)
	require.EqualValues(t, 9, ret)
	ret, err = lastIndexAny.Call(String("foobarbaz"), String("o"))
	require.NoError(t, err)
	require.EqualValues(t, 2, ret)
	ret, err = lastIndexAny.Call(String("foobarbaz"), String("p"))
	require.NoError(t, err)
	require.EqualValues(t, -1, ret)

	lastIndexByte := Module["LastIndexByte"]
	ret, err = lastIndexByte.Call(String("zfoobarbaz"), Char('z'))
	require.NoError(t, err)
	require.EqualValues(t, 9, ret)
	ret, err = lastIndexByte.Call(String("zfoobarbaz"), Int('z'))
	require.NoError(t, err)
	require.EqualValues(t, 9, ret)
	ret, err = lastIndexByte.Call(String("zfoobarbaz"), Char('x'))
	require.NoError(t, err)
	require.EqualValues(t, -1, ret)

	padLeft := Module["PadLeft"]
	ret, err = padLeft.Call(String("abc"), Int(3))
	require.NoError(t, err)
	require.EqualValues(t, "abc", ret)
	ret, err = padLeft.Call(String("abc"), Int(4))
	require.NoError(t, err)
	require.EqualValues(t, " abc", ret)
	ret, err = padLeft.Call(String("abc"), Int(5))
	require.NoError(t, err)
	require.EqualValues(t, "  abc", ret)
	ret, err = padLeft.Call(String("abc"), Int(5), String("="))
	require.NoError(t, err)
	require.EqualValues(t, "==abc", ret)
	ret, err = padLeft.Call(String(""), Int(6), String("="))
	require.NoError(t, err)
	require.EqualValues(t, "======", ret)

	padRight := Module["PadRight"]
	ret, err = padRight.Call(String("abc"), Int(3))
	require.NoError(t, err)
	require.EqualValues(t, "abc", ret)
	ret, err = padRight.Call(String("abc"), Int(4))
	require.NoError(t, err)
	require.EqualValues(t, "abc ", ret)
	ret, err = padRight.Call(String("abc"), Int(5))
	require.NoError(t, err)
	require.EqualValues(t, "abc  ", ret)
	ret, err = padRight.Call(String("abc"), Int(5), String("="))
	require.NoError(t, err)
	require.EqualValues(t, "abc==", ret)
	ret, err = padRight.Call(String(""), Int(6), String("="))
	require.NoError(t, err)
	require.EqualValues(t, "======", ret)

	repeat := Module["Repeat"]
	ret, err = repeat.Call(String("abc"), Int(3))
	require.NoError(t, err)
	require.EqualValues(t, "abcabcabc", ret)
	ret, err = repeat.Call(String("abc"), Int(-1))
	require.NoError(t, err)
	require.EqualValues(t, "", ret)

	replace := Module["Replace"]
	ret, err = replace.Call(String("abcdefbc"), String("bc"), String("(bc)"))
	require.NoError(t, err)
	require.EqualValues(t, "a(bc)def(bc)", ret)
	ret, err = replace.Call(
		String("abcdefbc"), String("bc"), String("(bc)"), Int(1))
	require.NoError(t, err)
	require.EqualValues(t, "a(bc)defbc", ret)

	split := Module["Split"]
	ret, err = split.Call(String("abc;def;"), String(";"))
	require.NoError(t, err)
	require.Equal(t, 3, len(ret.(Array)))
	require.EqualValues(t, "abc", ret.(Array)[0])
	require.EqualValues(t, "def", ret.(Array)[1])
	require.EqualValues(t, "", ret.(Array)[2])
	ret, err = split.Call(String("abc;def;"), String("!"), Int(0))
	require.NoError(t, err)
	require.Equal(t, 0, len(ret.(Array)))
	ret, err = split.Call(String("abc;def;"), String(";"), Int(1))
	require.NoError(t, err)
	require.Equal(t, 1, len(ret.(Array)))
	require.EqualValues(t, "abc;def;", ret.(Array)[0])
	ret, err = split.Call(String("abc;def;"), String(";"), Int(2))
	require.NoError(t, err)
	require.Equal(t, 2, len(ret.(Array)))
	require.EqualValues(t, "abc", ret.(Array)[0])
	require.EqualValues(t, "def;", ret.(Array)[1])

	splitAfter := Module["SplitAfter"]
	ret, err = splitAfter.Call(String("abc;def;"), String(";"))
	require.NoError(t, err)
	require.Equal(t, 3, len(ret.(Array)))
	require.EqualValues(t, "abc;", ret.(Array)[0])
	require.EqualValues(t, "def;", ret.(Array)[1])
	require.EqualValues(t, "", ret.(Array)[2])
	ret, err = splitAfter.Call(String("abc;def;"), String("!"), Int(0))
	require.NoError(t, err)
	require.Equal(t, 0, len(ret.(Array)))
	ret, err = splitAfter.Call(String("abc;def;"), String(";"), Int(1))
	require.NoError(t, err)
	require.Equal(t, 1, len(ret.(Array)))
	require.EqualValues(t, "abc;def;", ret.(Array)[0])
	ret, err = splitAfter.Call(String("abc;def;"), String(";"), Int(2))
	require.NoError(t, err)
	require.Equal(t, 2, len(ret.(Array)))
	require.EqualValues(t, "abc;", ret.(Array)[0])
	require.EqualValues(t, "def;", ret.(Array)[1])

	title := Module["Title"]
	ret, err = title.Call(String("хлеб"))
	require.NoError(t, err)
	require.EqualValues(t, "Хлеб", ret)

	toLower := Module["ToLower"]
	ret, err = toLower.Call(String("ÇİĞÖŞÜ"))
	require.NoError(t, err)
	require.EqualValues(t, "çiğöşü", ret)

	toTitle := Module["ToTitle"]
	ret, err = toTitle.Call(String("хлеб"))
	require.NoError(t, err)
	require.EqualValues(t, "ХЛЕБ", ret)

	toUpper := Module["ToUpper"]
	ret, err = toUpper.Call(String("çığöşü"))
	require.NoError(t, err)
	require.EqualValues(t, "ÇIĞÖŞÜ", ret)

	trim := Module["Trim"]
	ret, err = trim.Call(String("!!??abc?!"), String("!?"))
	require.NoError(t, err)
	require.EqualValues(t, "abc", ret)

	trimLeft := Module["TrimLeft"]
	ret, err = trimLeft.Call(String("!!??abc?!"), String("!?"))
	require.NoError(t, err)
	require.EqualValues(t, "abc?!", ret)

	trimPrefix := Module["TrimPrefix"]
	ret, err = trimPrefix.Call(String("abcdef"), String("abc"))
	require.NoError(t, err)
	require.EqualValues(t, "def", ret)

	trimRight := Module["TrimRight"]
	ret, err = trimRight.Call(String("!!??abc?!"), String("!?"))
	require.NoError(t, err)
	require.EqualValues(t, "!!??abc", ret)

	trimSpace := Module["TrimSpace"]
	ret, err = trimSpace.Call(String("\n \tabcdef\t \n"))
	require.NoError(t, err)
	require.EqualValues(t, "abcdef", ret)

	trimSuffix := Module["TrimSuffix"]
	ret, err = trimSuffix.Call(String("abcdef"), String("def"))
	require.NoError(t, err)
	require.EqualValues(t, "abc", ret)
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
		return String(ErrWrongNumArguments.NewError(
			fmt.Sprintf("want=%d got=%d", want, got),
		).String())
	}
	nwrongArgs := func(want1, want2, got int) String {
		return String(ErrWrongNumArguments.NewError(
			fmt.Sprintf("want=%d..%d got=%d", want1, want2, got),
		).String())
	}
	typeErr := func(pos, expected, got string) String {
		return String(NewArgumentTypeError(pos, expected, got).String())
	}
	testCases := []struct {
		s string
		m func(string) string
		e Object
	}{
		{s: `strings.Contains()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.Contains(1)`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.Contains(1, 2, 3)`, m: catch, e: wrongArgs(2, 3)},
		{s: `strings.Contains(1, 2)`, m: catch,
			e: typeErr("first", "string", "int")},
		{s: `strings.Contains("", 2)`, m: catch,
			e: typeErr("second", "string", "int")},
		{s: `strings.Contains("acbdef", "de")`, e: True},
		{s: `strings.Contains("acbdef", "dex")`, e: False},

		{s: `strings.ContainsAny()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.ContainsAny(1)`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.ContainsAny(1, 2, 3)`, m: catch, e: wrongArgs(2, 3)},
		{s: `strings.ContainsAny(1, 2)`, m: catch,
			e: typeErr("first", "string", "int")},
		{s: `strings.ContainsAny("", 2)`, m: catch,
			e: typeErr("second", "string", "int")},
		{s: `strings.ContainsAny("acbdef", "de")`, e: True},
		{s: `strings.ContainsAny("acbdef", "xw")`, e: False},

		{s: `strings.ContainsChar()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.ContainsChar(1)`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.ContainsChar(1, 2, 3)`, m: catch, e: wrongArgs(2, 3)},
		{s: `strings.ContainsChar(1, 2)`, m: catch,
			e: typeErr("first", "string", "int")},
		{s: `strings.ContainsChar("", 2)`, m: catch,
			e: typeErr("second", "char", "int")},
		{s: `strings.ContainsChar("acbdef", 'd')`, e: True},
		{s: `strings.ContainsChar("acbdef", 'x')`, e: False},

		{s: `strings.Count()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.Count(1)`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.Count(1, 2, 3)`, m: catch, e: wrongArgs(2, 3)},
		{s: `strings.Count(1, 2)`, m: catch,
			e: typeErr("first", "string", "int")},
		{s: `strings.Count("", 2)`, m: catch,
			e: typeErr("second", "string", "int")},
		{s: `strings.Count("abcddef", "d")`, e: Int(2)},
		{s: `strings.Count("abcddef", "x")`, e: Int(0)},

		{s: `strings.EqualFold()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.EqualFold(1)`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.EqualFold(1, 2, 3)`, m: catch, e: wrongArgs(2, 3)},
		{s: `strings.EqualFold(1, 2)`, m: catch,
			e: typeErr("first", "string", "int")},
		{s: `strings.EqualFold("", 2)`, m: catch,
			e: typeErr("second", "string", "int")},
		{s: `strings.EqualFold("çğöşü", "ÇĞÖŞÜ")`, e: True},
		{s: `strings.EqualFold("x", "y")`, e: False},

		{s: `strings.Fields()`, m: catch, e: wrongArgs(1, 0)},
		{s: `strings.Fields(1)`, m: catch,
			e: typeErr("first", "string", "int")},
		{s: `strings.Fields(1, 2)`, m: catch, e: wrongArgs(1, 2)},
		{s: `strings.Fields(1)`, m: catch,
			e: typeErr("first", "string", "int")},
		{s: `strings.Fields("a\nb c\td ")`,
			e: Array{String("a"), String("b"), String("c"), String("d")}},
		{s: `strings.Fields("")`, e: Array{}},

		{s: `strings.HasPrefix()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.HasPrefix(1)`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.HasPrefix(1, 2, 3)`, m: catch, e: wrongArgs(2, 3)},
		{s: `strings.HasPrefix(1, 2)`, m: catch,
			e: typeErr("first", "string", "int")},
		{s: `strings.HasPrefix("", 2)`, m: catch,
			e: typeErr("second", "string", "int")},
		{s: `strings.HasPrefix("abcdef", "abcde")`, e: True},
		{s: `strings.HasPrefix("abcdef", "x")`, e: False},

		{s: `strings.HasSuffix()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.HasSuffix(1)`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.HasSuffix(1, 2, 3)`, m: catch, e: wrongArgs(2, 3)},
		{s: `strings.HasSuffix(1, 2)`, m: catch,
			e: typeErr("first", "string", "int")},
		{s: `strings.HasSuffix("", 2)`, m: catch,
			e: typeErr("second", "string", "int")},
		{s: `strings.HasSuffix("abcdef", "ef")`, e: True},
		{s: `strings.HasSuffix("abcdef", "abc")`, e: False},

		{s: `strings.Index()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.Index(1)`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.Index(1, 2, 3)`, m: catch, e: wrongArgs(2, 3)},
		{s: `strings.Index(1, 2)`, m: catch,
			e: typeErr("first", "string", "int")},
		{s: `strings.Index("", 2)`, m: catch,
			e: typeErr("second", "string", "int")},
		{s: `strings.Index("abcdef", "ef")`, e: Int(4)},
		{s: `strings.Index("abcdef", "x")`, e: Int(-1)},

		{s: `strings.IndexAny()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.IndexAny(1)`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.IndexAny(1, 2, 3)`, m: catch, e: wrongArgs(2, 3)},
		{s: `strings.IndexAny(1, 2)`, m: catch,
			e: typeErr("first", "string", "int")},
		{s: `strings.IndexAny("", 2)`, m: catch,
			e: typeErr("second", "string", "int")},
		{s: `strings.IndexAny("abcdef", "ef")`, e: Int(4)},
		{s: `strings.IndexAny("abcdef", "x")`, e: Int(-1)},
		{s: `strings.IndexAny("abcdef", "xa")`, e: Int(0)},

		{s: `strings.IndexByte()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.IndexByte(1)`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.IndexByte(1, 2, 3)`, m: catch, e: wrongArgs(2, 3)},
		{s: `strings.IndexByte(1, 2)`, m: catch,
			e: typeErr("first", "string", "int")},
		{s: `strings.IndexByte("", "")`, m: catch,
			e: typeErr("second", "char|int", "string")},
		{s: `strings.IndexByte("abcdef", 'b')`, e: Int(1)},
		{s: `strings.IndexByte("abcdef", int('c'))`, e: Int(2)},
		{s: `strings.IndexByte("abcdef", 'g')`, e: Int(-1)},
		{s: `strings.IndexByte("abcdef", int('g'))`, e: Int(-1)},

		{s: `strings.IndexChar()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.IndexChar(1)`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.IndexChar(1, 2, 3)`, m: catch, e: wrongArgs(2, 3)},
		{s: `strings.IndexChar(1, 2)`, m: catch,
			e: typeErr("first", "string", "int")},
		{s: `strings.IndexChar("", 1)`, m: catch,
			e: typeErr("second", "char", "int")},
		{s: `strings.IndexChar("abcdef", 'c')`, e: Int(2)},

		{s: `strings.Join()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.Join(1)`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.Join(1, 2, 3)`, m: catch, e: wrongArgs(2, 3)},
		{s: `strings.Join(1, 2)`, m: catch,
			e: typeErr("first", "array", "int")},
		{s: `strings.Join([], 1)`, m: catch,
			e: typeErr("second", "string", "int")},
		{s: `strings.Join(["a", "b", "c"], "\t")`, e: String("a\tb\tc")},

		{s: `strings.LastIndex()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.LastIndex(1)`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.LastIndex(1, 2, 3)`, m: catch, e: wrongArgs(2, 3)},
		{s: `strings.LastIndex(1, 2)`, m: catch,
			e: typeErr("first", "string", "int")},
		{s: `strings.LastIndex("", 2)`, m: catch,
			e: typeErr("second", "string", "int")},
		{s: `strings.LastIndex("efabcdef", "ef")`, e: Int(6)},
		{s: `strings.LastIndex("abcdef", "g")`, e: Int(-1)},

		{s: `strings.LastIndexAny()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.LastIndexAny(1)`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.LastIndexAny(1, 2, 3)`, m: catch, e: wrongArgs(2, 3)},
		{s: `strings.LastIndexAny(1, 2)`, m: catch,
			e: typeErr("first", "string", "int")},
		{s: `strings.LastIndexAny("", 2)`, m: catch,
			e: typeErr("second", "string", "int")},
		{s: `strings.LastIndexAny("efabcdef", "xf")`, e: Int(7)},
		{s: `strings.LastIndexAny("abcdef", "g")`, e: Int(-1)},

		{s: `strings.LastIndexByte()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.LastIndexByte(1)`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.LastIndexByte(1, 2, 3)`, m: catch, e: wrongArgs(2, 3)},
		{s: `strings.LastIndexByte(1, 2)`, m: catch,
			e: typeErr("first", "string", "int")},
		{s: `strings.LastIndexByte("", "")`, m: catch,
			e: typeErr("second", "char|int", "string")},
		{s: `strings.LastIndexByte("efabcdef", 'f')`, e: Int(7)},
		{s: `strings.LastIndexByte("efabcdef", int('f'))`, e: Int(7)},
		{s: `strings.LastIndexByte("abcdef", 'g')`, e: Int(-1)},

		{s: `strings.PadLeft()`, m: catch, e: nwrongArgs(2, 3, 0)},
		{s: `strings.PadLeft(1)`, m: catch, e: nwrongArgs(2, 3, 1)},
		{s: `strings.PadLeft(1, 2, 3, 4)`, m: catch, e: nwrongArgs(2, 3, 4)},
		{s: `strings.PadLeft(1, 2, 3)`, m: catch,
			e: typeErr("first", "string", "int")},
		{s: `strings.PadLeft("", "", "")`, m: catch,
			e: typeErr("second", "int", "string")},
		{s: `strings.PadLeft("", 0, "")`, e: String("")},
		{s: `strings.PadLeft("", -1, "")`, e: String("")},
		{s: `strings.PadLeft("", 1, "")`, e: String("")},
		{s: `strings.PadLeft("", 1, "x")`, e: String("x")},
		{s: `strings.PadLeft("abc", 3)`, e: String("abc")},
		{s: `strings.PadLeft("abc", 4)`, e: String(" abc")},
		{s: `strings.PadLeft("abc", 5)`, e: String("  abc")},
		{s: `strings.PadLeft("abc", 6)`, e: String("   abc")},
		{s: `strings.PadLeft("abc", -1, "x")`, e: String("abc")},
		{s: `strings.PadLeft("abc", 0, "x")`, e: String("abc")},
		{s: `strings.PadLeft("abc", 1, "x")`, e: String("abc")},
		{s: `strings.PadLeft("abc", 2, "x")`, e: String("abc")},
		{s: `strings.PadLeft("abc", 3, "x")`, e: String("abc")},
		{s: `strings.PadLeft("abc", 4, "x")`, e: String("xabc")},
		{s: `strings.PadLeft("abc", 5, "x")`, e: String("xxabc")},
		{s: `strings.PadLeft("abc", 5, "xy")`, e: String("xyabc")},
		{s: `strings.PadLeft("abc", 6, "xy")`, e: String("xyxabc")},
		{s: `strings.PadLeft("abc", 6, "wxyz")`, e: String("wxyabc")},

		{s: `strings.PadRight()`, m: catch, e: nwrongArgs(2, 3, 0)},
		{s: `strings.PadRight(1)`, m: catch, e: nwrongArgs(2, 3, 1)},
		{s: `strings.PadRight(1, 2, 3, 4)`, m: catch, e: nwrongArgs(2, 3, 4)},
		{s: `strings.PadRight(1, 2, 3)`, m: catch,
			e: typeErr("first", "string", "int")},
		{s: `strings.PadRight("", "", "")`, m: catch,
			e: typeErr("second", "int", "string")},
		{s: `strings.PadRight("", 0, "")`, e: String("")},
		{s: `strings.PadRight("", -1, "")`, e: String("")},
		{s: `strings.PadRight("", 1, "")`, e: String("")},
		{s: `strings.PadRight("", 1, "x")`, e: String("x")},
		{s: `strings.PadRight("abc", 3)`, e: String("abc")},
		{s: `strings.PadRight("abc", 4)`, e: String("abc ")},
		{s: `strings.PadRight("abc", 5)`, e: String("abc  ")},
		{s: `strings.PadRight("abc", 6)`, e: String("abc   ")},
		{s: `strings.PadRight("abc", -1, "x")`, e: String("abc")},
		{s: `strings.PadRight("abc", 0, "x")`, e: String("abc")},
		{s: `strings.PadRight("abc", 1, "x")`, e: String("abc")},
		{s: `strings.PadRight("abc", 2, "x")`, e: String("abc")},
		{s: `strings.PadRight("abc", 3, "x")`, e: String("abc")},
		{s: `strings.PadRight("abc", 4, "x")`, e: String("abcx")},
		{s: `strings.PadRight("abc", 5, "x")`, e: String("abcxx")},
		{s: `strings.PadRight("abc", 5, "xy")`, e: String("abcxy")},
		{s: `strings.PadRight("abc", 6, "xy")`, e: String("abcxyx")},
		{s: `strings.PadRight("abc", 6, "wxyz")`, e: String("abcwxy")},

		{s: `strings.Repeat()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.Repeat(1)`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.Repeat(1, 2, 3)`, m: catch, e: wrongArgs(2, 3)},
		{s: `strings.Repeat(1, 2)`, m: catch,
			e: typeErr("first", "string", "int")},
		{s: `strings.Repeat("", "")`, m: catch,
			e: typeErr("second", "int", "string")},
		{s: `strings.Repeat("a", -1)`, e: String("")},
		{s: `strings.Repeat("a", 0)`, e: String("")},
		{s: `strings.Repeat("a", 2)`, e: String("aa")},

		{s: `strings.Replace()`, m: catch, e: nwrongArgs(3, 4, 0)},
		{s: `strings.Replace(1)`, m: catch, e: nwrongArgs(3, 4, 1)},
		{s: `strings.Replace(1, 2)`, m: catch, e: nwrongArgs(3, 4, 2)},
		{s: `strings.Replace(1, 2, 3, 4, 5)`, m: catch, e: nwrongArgs(3, 4, 5)},
		{s: `strings.Replace(1, 2, 3)`, m: catch,
			e: typeErr("first", "string", "int")},
		{s: `strings.Replace("", 1, 3)`, m: catch,
			e: typeErr("second", "string", "int")},
		{s: `strings.Replace("", "", 1)`, m: catch,
			e: typeErr("third", "string", "int")},
		{s: `strings.Replace("", "", "", "")`, m: catch,
			e: typeErr("fourth", "int", "string")},
		{s: `strings.Replace("abc", "s", "ş")`, e: String("abc")},
		{s: `strings.Replace("abbc", "b", "a")`, e: String("aaac")},
		{s: `strings.Replace("abbc", "b", "a", 0)`, e: String("abbc")},
		{s: `strings.Replace("abbc", "b", "a", 1)`, e: String("aabc")},

		{s: `strings.Split()`, m: catch, e: nwrongArgs(2, 3, 0)},
		{s: `strings.Split(1)`, m: catch, e: nwrongArgs(2, 3, 1)},
		{s: `strings.Split(1, 2, 3, 4)`, m: catch, e: nwrongArgs(2, 3, 4)},
		{s: `strings.Split(1, 2)`, m: catch,
			e: typeErr("first", "string", "int")},
		{s: `strings.Split("", 1, 3)`, m: catch,
			e: typeErr("second", "string", "int")},
		{s: `strings.Split("", "", "")`, m: catch,
			e: typeErr("third", "int", "string")},
		{s: `strings.Split("a.b.c", ".", 0)`, e: Array{}},
		{s: `strings.Split("a.b.c", ".", 1)`, e: Array{String("a.b.c")}},
		{s: `strings.Split("a.b.c", ".", -1)`,
			e: Array{String("a"), String("b"), String("c")}},
		{s: `strings.Split("a.b.c", ".")`,
			e: Array{String("a"), String("b"), String("c")}},
		{s: `strings.Split("a.b.c.", ".")`,
			e: Array{String("a"), String("b"), String("c"), String("")}},
		{s: `strings.Split("a.b.c.", ".", 5)`,
			e: Array{String("a"), String("b"), String("c"), String("")}},

		{s: `strings.SplitAfter()`, m: catch, e: nwrongArgs(2, 3, 0)},
		{s: `strings.SplitAfter(1)`, m: catch, e: nwrongArgs(2, 3, 1)},
		{s: `strings.SplitAfter(1, 2, 3, 4)`, m: catch, e: nwrongArgs(2, 3, 4)},
		{s: `strings.SplitAfter(1, 2)`, m: catch,
			e: typeErr("first", "string", "int")},
		{s: `strings.SplitAfter("", 1, 3)`, m: catch,
			e: typeErr("second", "string", "int")},
		{s: `strings.SplitAfter("", "", "")`, m: catch,
			e: typeErr("third", "int", "string")},
		{s: `strings.SplitAfter("a.b.c", ".", 0)`, e: Array{}},
		{s: `strings.SplitAfter("a.b.c", ".", 1)`, e: Array{String("a.b.c")}},
		{s: `strings.SplitAfter("a.b.c", ".", -1)`,
			e: Array{String("a."), String("b."), String("c")}},
		{s: `strings.SplitAfter("a.b.c", ".")`,
			e: Array{String("a."), String("b."), String("c")}},
		{s: `strings.SplitAfter("a.b.c.", ".")`,
			e: Array{String("a."), String("b."), String("c."), String("")}},
		{s: `strings.SplitAfter("a.b.c.", ".", 5)`,
			e: Array{String("a."), String("b."), String("c."), String("")}},

		{s: `strings.Title()`, m: catch, e: wrongArgs(1, 0)},
		{s: `strings.Title(1, 2)`, m: catch, e: wrongArgs(1, 2)},
		{s: `strings.Title(1)`, m: catch,
			e: typeErr("first", "string", "int")},
		{s: `strings.Title("")`, e: String("")},
		{s: `strings.Title("abc def")`, e: String("Abc Def")},

		{s: `strings.ToLower()`, m: catch, e: wrongArgs(1, 0)},
		{s: `strings.ToLower(1, 2)`, m: catch, e: wrongArgs(1, 2)},
		{s: `strings.ToLower(1)`, m: catch,
			e: typeErr("first", "string", "int")},
		{s: `strings.ToLower("")`, e: String("")},
		{s: `strings.ToLower("XYZ")`, e: String("xyz")},

		{s: `strings.ToTitle()`, m: catch, e: wrongArgs(1, 0)},
		{s: `strings.ToTitle(1, 2)`, m: catch, e: wrongArgs(1, 2)},
		{s: `strings.ToTitle(1)`, m: catch,
			e: typeErr("first", "string", "int")},
		{s: `strings.ToTitle("")`, e: String("")},
		{s: `strings.ToTitle("çğ öşü")`, e: String("ÇĞ ÖŞÜ")},

		{s: `strings.ToUpper()`, m: catch, e: wrongArgs(1, 0)},
		{s: `strings.ToUpper(1, 2)`, m: catch, e: wrongArgs(1, 2)},
		{s: `strings.ToUpper(1)`, m: catch,
			e: typeErr("first", "string", "int")},
		{s: `strings.ToUpper("")`, e: String("")},
		{s: `strings.ToUpper("çğ öşü")`, e: String("ÇĞ ÖŞÜ")},

		{s: `strings.Trim()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.Trim(1)`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.Trim(1, 2, 3)`, m: catch, e: wrongArgs(2, 3)},
		{s: `strings.Trim(1, 2)`, m: catch,
			e: typeErr("first", "string", "int")},
		{s: `strings.Trim("", 2)`, m: catch,
			e: typeErr("second", "string", "int")},
		{s: `strings.Trim("!!xyz!!", "")`, e: String("!!xyz!!")},
		{s: `strings.Trim("!!xyz!!", "!")`, e: String("xyz")},
		{s: `strings.Trim("!!xyz!!", "!?")`, e: String("xyz")},

		{s: `strings.TrimLeft()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.TrimLeft(1)`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.TrimLeft(1, 2, 3)`, m: catch, e: wrongArgs(2, 3)},
		{s: `strings.TrimLeft(1, 2)`, m: catch,
			e: typeErr("first", "string", "int")},
		{s: `strings.TrimLeft("", 2)`, m: catch,
			e: typeErr("second", "string", "int")},
		{s: `strings.TrimLeft("!!xyz!!", "")`, e: String("!!xyz!!")},
		{s: `strings.TrimLeft("!!xyz!!", "!")`, e: String("xyz!!")},
		{s: `strings.TrimLeft("!!?xyz!!", "!?")`, e: String("xyz!!")},

		{s: `strings.TrimPrefix()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.TrimPrefix(1)`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.TrimPrefix(1, 2, 3)`, m: catch, e: wrongArgs(2, 3)},
		{s: `strings.TrimPrefix(1, 2)`, m: catch,
			e: typeErr("first", "string", "int")},
		{s: `strings.TrimPrefix("", 2)`, m: catch,
			e: typeErr("second", "string", "int")},
		{s: `strings.TrimPrefix("!!xyz!!", "")`, e: String("!!xyz!!")},
		{s: `strings.TrimPrefix("!!xyz!!", "!")`, e: String("!xyz!!")},
		{s: `strings.TrimPrefix("!!xyz!!", "!!")`, e: String("xyz!!")},
		{s: `strings.TrimPrefix("!!xyz!!", "!!x")`, e: String("yz!!")},

		{s: `strings.TrimRight()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.TrimRight(1)`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.TrimRight(1, 2, 3)`, m: catch, e: wrongArgs(2, 3)},
		{s: `strings.TrimRight(1, 2)`, m: catch,
			e: typeErr("first", "string", "int")},
		{s: `strings.TrimRight("", 2)`, m: catch,
			e: typeErr("second", "string", "int")},
		{s: `strings.TrimRight("!!xyz!!", "")`, e: String("!!xyz!!")},
		{s: `strings.TrimRight("!!xyz!!", "!")`, e: String("!!xyz")},
		{s: `strings.TrimRight("!!xyz?!!", "!?")`, e: String("!!xyz")},

		{s: `strings.TrimSpace()`, m: catch, e: wrongArgs(1, 0)},
		{s: `strings.TrimSpace(1, 2)`, m: catch, e: wrongArgs(1, 2)},
		{s: `strings.TrimSpace(1)`, m: catch,
			e: typeErr("first", "string", "int")},
		{s: `strings.TrimSpace(" \txyz\n\r")`, e: String("xyz")},
		{s: `strings.TrimSpace("xyz")`, e: String("xyz")},

		{s: `strings.TrimSuffix()`, m: catch, e: wrongArgs(2, 0)},
		{s: `strings.TrimSuffix(1)`, m: catch, e: wrongArgs(2, 1)},
		{s: `strings.TrimSuffix(1, 2, 3)`, m: catch, e: wrongArgs(2, 3)},
		{s: `strings.TrimSuffix(1, 2)`, m: catch,
			e: typeErr("first", "string", "int")},
		{s: `strings.TrimSuffix("", 2)`, m: catch,
			e: typeErr("second", "string", "int")},
		{s: `strings.TrimSuffix("!!xyz!!", "")`, e: String("!!xyz!!")},
		{s: `strings.TrimSuffix("!!xyz!!", "!")`, e: String("!!xyz!")},
		{s: `strings.TrimSuffix("!!xyz!!", "!!")`, e: String("!!xyz")},
		{s: `strings.TrimSuffix("!!xyz!!", "z!!")`, e: String("!!xy")},
	}
	for _, tC := range testCases {
		var s string
		if tC.m == nil {
			s = ret(tC.s)
		} else {
			s = catch(tC.s)
		}
		t.Run(tC.s, func(t *testing.T) {
			expectRun(t, s, tC.e)
		})
	}
}

func expectRun(t *testing.T, script string, expected Object) {
	t.Helper()
	mm := NewModuleMap()
	mm.AddBuiltinModule("strings", Module)
	c := DefaultCompilerOptions
	c.ModuleMap = mm
	bc, err := Compile([]byte(script), c)
	require.NoError(t, err)
	ret, err := NewVM(bc).Run(nil)
	require.NoError(t, err)
	require.Equal(t, expected, ret)
}
