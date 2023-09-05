package encoder_test

import (
	"bytes"
	"io"
	"io/ioutil"
	"testing"
	gotime "time"

	"github.com/stretchr/testify/require"

	"github.com/topxeq/charlang"
	"github.com/topxeq/charlang/stdlib/fmt"
	"github.com/topxeq/charlang/stdlib/json"
	"github.com/topxeq/charlang/stdlib/strings"
	"github.com/topxeq/charlang/stdlib/time"
	"github.com/topxeq/charlang/tests"

	. "github.com/topxeq/charlang/encoder"
)

var baz charlang.Object = charlang.String("baz")
var testObjects = []charlang.Object{
	charlang.Undefined,
	charlang.Int(-1), charlang.Int(0), charlang.Int(1),
	charlang.Uint(0), ^charlang.Uint(0),
	charlang.Char('x'),
	charlang.Bool(true), charlang.Bool(false),
	charlang.Float(0), charlang.Float(1.2),
	charlang.String(""), charlang.String("abc"),
	charlang.Bytes{}, charlang.Bytes("foo"),
	charlang.ErrIndexOutOfBounds,
	&charlang.RuntimeError{Err: charlang.ErrInvalidIndex},
	charlang.Map{"key": &charlang.Function{Name: "f"}},
	&charlang.SyncMap{Value: charlang.Map{"k": charlang.String("")}},
	charlang.Array{charlang.Undefined, charlang.True, charlang.False},
	&time.Time{Value: gotime.Time{}},
	&json.EncoderOptions{Value: charlang.Int(1)},
	&json.RawMessage{Value: charlang.Bytes("bar")},
	&charlang.ObjectPtr{Value: &baz},
}

func TestBytecode_Encode(t *testing.T) {
	testBytecodeSerialization(t, &charlang.Bytecode{Main: compFunc(nil)}, nil)

	testBytecodeSerialization(t,
		&charlang.Bytecode{Constants: testObjects,
			Main: compFunc(
				[]byte("test instructions"),
				withLocals(1), withParams(1), withVariadic(),
			),
		},
		nil,
	)
}

func TestBytecode_file(t *testing.T) {
	temp := t.TempDir()

	bc := &charlang.Bytecode{Constants: testObjects,
		Main: compFunc(
			[]byte("test instructions"),
			withLocals(4), withParams(0), withVariadic(),
			withSourceMap(map[int]int{0: 1, 1: 2}),
		),
	}
	f, err := ioutil.TempFile(temp, "mod.charlangc")
	require.NoError(t, err)
	defer f.Close()

	err = EncodeBytecodeTo(bc, f)
	require.NoError(t, err)

	_, err = f.Seek(0, io.SeekStart)
	require.NoError(t, err)

	got, err := DecodeBytecodeFrom(f, nil)
	require.NoError(t, err)
	testBytecodesEqual(t, bc, got)
}

func TestBytecode_full(t *testing.T) {
	src := `
fmt := import("fmt")
strings := import("strings")
time := import("time")
json := import("json")
srcmod := import("srcmod")

v := int(json.Unmarshal(json.Marshal(1)))
v = int(strings.Join([v], ""))
v = srcmod.Incr(v)
v = srcmod.Decr(v)
v = int(fmt.Sprintf("%d", v))
return v*time.Second/time.Second // 1
`

	opts := charlang.DefaultCompilerOptions
	opts.ModuleMap = charlang.NewModuleMap().
		AddBuiltinModule("fmt", fmt.Module).
		AddBuiltinModule("strings", strings.Module).
		AddBuiltinModule("time", time.Module).
		AddBuiltinModule("json", json.Module).
		AddSourceModule("srcmod", []byte(`
return {
	Incr: func(x) { return x + 1 },
	Decr: func(x) { return x - 1 },
}
		`))

	mmCopy := opts.ModuleMap.Copy()

	bc, err := charlang.Compile([]byte(src), opts)
	require.NoError(t, err)

	wantRet, err := charlang.NewVM(bc).Run(nil)
	require.NoError(t, err)
	require.Equal(t, charlang.Int(1), wantRet)

	temp := t.TempDir()
	f, err := ioutil.TempFile(temp, "program.charlangc")
	require.NoError(t, err)
	defer f.Close()

	var buf bytes.Buffer

	logmicros(t, "encode time: %d microsecs", func() {
		err = EncodeBytecodeTo(bc, &buf)
	})
	require.NoError(t, err)

	t.Logf("written size: %v bytes", buf.Len())

	_, err = buf.WriteTo(f)
	require.NoError(t, err)

	_, err = f.Seek(0, io.SeekStart)
	require.NoError(t, err)

	var gotBc *charlang.Bytecode
	logmicros(t, "decode time: %d microsecs", func() {
		gotBc, err = DecodeBytecodeFrom(f, mmCopy)
	})
	require.NoError(t, err)
	require.NotNil(t, gotBc)

	var gotRet charlang.Object
	logmicros(t, "run time: %d microsecs", func() {
		gotRet, err = charlang.NewVM(gotBc).Run(nil)
	})
	require.NoError(t, err)

	require.Equal(t, wantRet, gotRet)
}

func testBytecodeSerialization(t *testing.T, b *charlang.Bytecode, modules *charlang.ModuleMap) {
	t.Helper()

	var buf bytes.Buffer
	err := (*Bytecode)(b).Encode(&buf)
	require.NoError(t, err)

	r := &charlang.Bytecode{}
	err = (*Bytecode)(r).Decode(bytes.NewReader(buf.Bytes()), modules)
	require.NoError(t, err)

	testBytecodesEqual(t, b, r)
}

func testBytecodesEqual(t *testing.T, want, got *charlang.Bytecode) {
	t.Helper()

	require.Equal(t, want.FileSet, got.FileSet)
	require.Equal(t, want.Main, got.Main)
	require.Equalf(t, want.Constants, got.Constants,
		"expected:%s\nactual:%s", tests.Sdump(want.Constants), tests.Sdump(want.Constants))
	testBytecodeConstants(t, want.Constants, got.Constants)
	require.Equal(t, want.NumModules, got.NumModules)
}

func logmicros(t *testing.T, format string, f func()) {
	t0 := gotime.Now()
	f()
	t.Logf(format, gotime.Since(t0).Microseconds())
}
