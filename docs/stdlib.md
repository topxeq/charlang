# Standard Library

## Module List

* [fmt](stdlib-fmt.md) module at `github.com/ozanh/ugo/stdlib/fmt`
* [strings](stdlib-strings.md) module at `github.com/ozanh/ugo/stdlib/strings`
* [time](stdlib-time.md) module at `github.com/ozanh/ugo/stdlib/time`

## How-To

### Import Module

Each standard library module is imported separately. `Module` variable as
`map[string]Object` in modules holds module values to pass to module map which
is deeply copied then.

**Example**

```go
package main

import (
    ugo "github.com/topxeq/charlang"
    "github.com/topxeq/charlang/stdlib/fmt"
    "github.com/topxeq/charlang/stdlib/strings"
    "github.com/topxeq/charlang/stdlib/time"
)

func main() {
    script := `
    fmt := import("fmt")
    time := import("time")
    strings := import("strings")

    total := 0
    fn := func() {
        start := time.Now()
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
    moduleMap.AddBuiltinModule("time", time.Module)
    opts := ugo.DefaultCompilerOptions
    opts.ModuleMap = moduleMap
    bc, err := ugo.Compile([]byte(script), opts)
    ret, err := ugo.NewVM(bc).Run(nil)
    /* ... */
}
```
