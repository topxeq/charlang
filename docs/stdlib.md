# Standard Library

## Module List

* [fmt](stdlib-fmt.md) module at `github.com/topxeq/charlang/stdlib/fmt`
* [strings](stdlib-strings.md) module at `github.com/topxeq/charlang/stdlib/strings`
* [json](stdlib-json.md) module at `github.com/topxeq/charlang/stdlib/json`

## How-To

### Import Module

Each standard library module is imported separately. `Module` variable as
`map[string]Object` in modules holds module values to pass to module map which
is deeply copied then.

**Example**

```go
package main

import (
    "github.com/topxeq/charlang"
    "github.com/topxeq/charlang/stdlib/fmt"
    "github.com/topxeq/charlang/stdlib/json"
    "github.com/topxeq/charlang/stdlib/strings"
)

func main() {
    script := `
    const fmt = import("fmt")
    const strings = import("strings")
    const json = import("json")

    total := 0
    fn := func() {
        start := now()
        try {
            /* ... */
        } finally {
            total += time.Since(start)
        }
    }
    fn()
    /* ... */
    `
    moduleMap := ugo.NewModuleMap()
    moduleMap.AddBuiltinModule("fmt", fmt.Module)
    moduleMap.AddBuiltinModule("strings", strings.Module)
    moduleMap.AddBuiltinModule("json", json.Module)

    opts := ugo.DefaultCompilerOptions
    opts.ModuleMap = moduleMap

    byteCode, err := ugo.Compile([]byte(script), opts)
    ret, err := ugo.NewVM(byteCode).Run(nil)
    /* ... */
}
```
