# The Char Language (Charlang)

Charlang is based on uGo language with some minor modifications and some new builtin-functions. Thanks to ozanh(github.com/ozanh/ugo).

## new features

- Added new types Byte, Any.
- Added functions: NewCommonError, NewError.
- Added builtin functions: getRandomInt, writeResp, setRespHeader, writeRespHeader.
- Added a global funtion wrapper: tk (TkFunction)

# The Charlang Language

[Go Reference](https://pkg.go.dev/github.com/topxeq/charlang)

Charlang is a fast, dynamic scripting language to embed in Go applications.
Charlang is compiled and executed as bytecode on stack-based VM that's written
in native Go.

Charlang is inspired by awesome script language [uGo](https://github.com/topxeq/charlang). A special thanks to uGo's creater and contributors.

**Fibonacci Example**

```go
param arg0

var fib

fib = func(x) {
    if x == 0 {
        return 0
    } else if x == 1 {
        return 1
    }
    return fib(x-1) + fib(x-2)
}
return fib(arg0)
```

## Features

* Written in native Go (no cgo).
* `if else` statements.
* `for` and `for in` statements.
* `try catch finally` statements.
* `param`, `global`, `var` and `const` declarations.
* Rich builtins.
* Module support.
* Go like syntax with additions.

## Why Charlang

I needed a faster embedded scripting language with runtime error handling.

## Quick Start

`go get -u github.com/topxeq/charlang`

Charlang has a REPL application to learn and test Charlang language thanks to
`github.com/c-bata/go-prompt` library.

`go get -u github.com/topxeq/charlang/cmd/char`

`./char`

```go
package main

import (
    "fmt"

    ugo "github.com/topxeq/charlang"
)

func main() {
    script := `
param ...args

mapEach := func(seq, fn) {

    if !isArray(seq) {
        return error("want array, got " + typeName(seq))
    }

    var out = []

    if sz := len(seq); sz > 0 {
        out = repeat([0], sz)
    } else {
        return out
    }

    try {
        for i, v in seq {
            out[i] = fn(v)
        }
    } catch err {
        println(err)
    } finally {
        return out, err
    }
}

global multiplier

v, err := mapEach(args, func(x) { return x*multiplier })
if err != undefined {
    return err
}
return v
`

    bytecode, err := ugo.Compile([]byte(script), ugo.DefaultCompilerOptions)
    if err != nil {
        panic(err)
    }
    globals := ugo.Map{"multiplier": ugo.Int(2)}
    ret, err := ugo.NewVM(bytecode).Run(
        globals,
        ugo.Int(1), ugo.Int(2), ugo.Int(3), ugo.Int(4),
    )
    if err != nil {
        panic(err)
    }
    fmt.Println(ret) // [2, 4, 6, 8]
}
```

## Documentation

* [Tutorial](https://github.com/topxeq/charlang/blob/main/docs/tutorial.md)
* [Runtime Types](https://github.com/topxeq/charlang/blob/main/docs/runtime-types.md)
* [Builtins](https://github.com/topxeq/charlang/blob/main/docs/builtins.md)
* [Operators](https://github.com/topxeq/charlang/blob/main/docs/operators.md)
* [Error Handling](https://github.com/topxeq/charlang/blob/main/docs/error-handling.md)
* [Standard Library](https://github.com/topxeq/charlang/blob/main/docs/stdlib.md)
* [Optimizer](https://github.com/topxeq/charlang/blob/main/docs/optimizer.md)
* [Destructuring](https://github.com/topxeq/charlang/blob/main/docs/destructuring.md)

