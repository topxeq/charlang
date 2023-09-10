# The Char Language (Charlang)

[Charlang](http://topget.org/charlang) is a fast, dynamic scripting language to embed in Go applications.
Charlang is compiled and executed as bytecode on stack-based VM that's written
in native Go. Charlang has a more-common runtime error handling(try-catch-finally) than Golang.

Charlang is inspired by and based on awesome script language [uGo](https://github.com/ozanh/ugo). A special thanks to uGo's creater([ozanh](https://github.com/ozanh)) and contributors.

## Features

* Written in native Go (no cgo).
* `if else` statements.
* `for` and `for in` statements.
* `try catch finally` statements.
* `param`, `global`, `var` and `const` declarations.
* Rich builtins.
* Module support.
* Go like syntax with additions.

## new features

- Added new types such as Byte, Any...
- Added functions: NewCommonError, NewError and more...
- Added builtin functions: getRandomInt, writeResp, setRespHeader, writeRespHeader and much more...
- Added some global variables and resources.
- Added a new thread-model.
- Added runtime/dynamically script compiling and running capability.

[Go Reference](https://pkg.go.dev/github.com/topxeq/charlang)

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

## Quick Start

`go get -u github.com/topxeq/charlang`

Charlang has a REPL application to learn and test Charlang language.

`go get -u github.com/topxeq/charlang/cmd/char`

`./char`

```go
package main

import (
    "fmt"

    "github.com/topxeq/charlang"
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

    bytecode, err := charlang.Compile([]byte(script), charlang.DefaultCompilerOptions)
    if err != nil {
        panic(err)
    }
    globals := charlang.Map{"multiplier": charlang.Int(2)}
    ret, err := charlang.NewVM(bytecode).Run(
        globals,
        charlang.Int(1), charlang.Int(2), charlang.Int(3), charlang.Int(4),
    )
    if err != nil {
        panic(err)
    }
    fmt.Println(ret) // [2, 4, 6, 8]
}
```

## Documentation

### Get the binary

Download the binary release files according to your OS from the website: [Charlang Homepage](http://topget.org/charlang).

### Start running the shell or scripts

After download, extract the executable from the zip file, put it into a directory, better in the system path.

Then type 'char' in the terminal/console to start the interactive command-line shell interface. Also you can run some scripts using command like 'char test.char', or 'char -example basic.char'.

Using command-line switch '-view' will show the source code of the script instead of run it.

### Hello world!

```go
pln("Hello world!")

```

The function 'pln' is the same as 'println' in other languages.


