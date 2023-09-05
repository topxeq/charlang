package importers_test

import (
	"bytes"
	"io/ioutil"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/topxeq/charlang"
	"github.com/topxeq/charlang/importers"
)

func TestFileImporter(t *testing.T) {
	buf := bytes.NewBuffer(nil)
	orig := charlang.PrintWriter
	charlang.PrintWriter = buf
	defer func() {
		charlang.PrintWriter = orig
	}()

	files := map[string]string{
		"./test1.char": `
import("./test2.char")
println("test1")
`,
		"./test2.char": `
import("./foo/test3.char")
println("test2")
`,
		"./foo/test3.char": `
import("./test4.char")
println("test3")
`,
		"./foo/test4.char": `
import("./bar/test5.char")
println("test4")
`,
		"./foo/bar/test5.char": `
import("../test6.char")
println("test5")
`,
		"./foo/test6.char": `
import("sourcemod")
println("test6")
`,
		"./test7.char": `
println("test7")
`,
	}

	script := `
import("test1.char")
println("main")

// modules have been imported already, so these imports will not trigger a print.
import("test1.char")
import("test2.char")
import("foo/test3.char")
import("foo/test4.char")
import("foo/bar/test5.char")
import("foo/test6.char")

func() {
	import("test1.char")
	import("test2.char")
	import("foo/test3.char")
	import("foo/test4.char")
	import("foo/bar/test5.char")
	import("foo/test6.char")
}()

`
	moduleMap := charlang.NewModuleMap().
		AddSourceModule("sourcemod", []byte(`
import("./test7.char")
println("sourcemod")`))

	t.Run("default", func(t *testing.T) {
		buf.Reset()

		tempDir := t.TempDir()

		createModules(t, tempDir, files)

		opts := charlang.DefaultCompilerOptions
		opts.ModuleMap = moduleMap.Copy()
		opts.ModuleMap.SetExtImporter(&importers.FileImporter{WorkDir: tempDir})
		bc, err := charlang.Compile([]byte(script), opts)
		require.NoError(t, err)
		ret, err := charlang.NewVM(bc).Run(nil)
		require.NoError(t, err)
		require.Equal(t, charlang.Undefined, ret)
		require.Equal(t,
			"test7\nsourcemod\ntest6\ntest5\ntest4\ntest3\ntest2\ntest1\nmain\n",
			strings.ReplaceAll(buf.String(), "\r", ""),
		)
	})

	t.Run("shebang", func(t *testing.T) {
		buf.Reset()

		const shebangline = "#!/usr/bin/char\n"

		mfiles := make(map[string]string)
		for k, v := range files {
			mfiles[k] = shebangline + v
		}

		tempDir := t.TempDir()

		createModules(t, tempDir, mfiles)

		opts := charlang.DefaultCompilerOptions
		opts.ModuleMap = moduleMap.Copy()
		opts.ModuleMap.SetExtImporter(
			&importers.FileImporter{
				WorkDir:    tempDir,
				FileReader: importers.ShebangReadFile,
			},
		)

		script := append([]byte(shebangline), script...)
		importers.Shebang2Slashes(script)

		bc, err := charlang.Compile(script, opts)
		require.NoError(t, err)
		ret, err := charlang.NewVM(bc).Run(nil)
		require.NoError(t, err)
		require.Equal(t, charlang.Undefined, ret)
		require.Equal(t,
			"test7\nsourcemod\ntest6\ntest5\ntest4\ntest3\ntest2\ntest1\nmain\n",
			strings.ReplaceAll(buf.String(), "\r", ""),
		)
	})

}

func createModules(t *testing.T, baseDir string, files map[string]string) {
	for file, data := range files {
		path := filepath.Join(baseDir, file)
		err := os.MkdirAll(filepath.Dir(path), 0755)
		require.NoError(t, err)
		err = ioutil.WriteFile(path, []byte(data), 0644)
		require.NoError(t, err)
	}
}
