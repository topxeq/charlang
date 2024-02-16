<!-- |title: The Char Language (Charlang)| -->

- [The Char Language (Charlang)](#the-char-language-charlang)
  - [Features](#features)
  - [New Features](#new-features)
  - [Quick Links](#quick-links)
  - [Quick Start](#quick-start)
  - [Documentation](#documentation)
    - [Get the binary](#get-the-binary)
    - [Compile from source code](#compile-from-source-code)
    - [Start running the shell or scripts](#start-running-the-shell-or-scripts)
    - [Various ways to run Charlang scripts](#various-ways-to-run-charlang-scripts)
    - [Get the examples](#get-the-examples)
    - [Quick tour](#quick-tour)
      - [Hello world!](#hello-world)
      - [Define variables](#define-variables)

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

## New Features

- New types such as Byte, Any...
- New functions: NewCommonError, NewError and more...
- New builtin functions: getRandomInt, writeResp, setRespHeader, writeRespHeader and much more...
- New global variables and resources.
- A new thread-model.
- Runtime/dynamically script compiling and running capability.

**Fibonacci Example**

```go
var fib

fib = func(x) {
    if x == 0 {
        return 0
    } else if x == 1 {
        return 1
    }
    return fib(x-1) + fib(x-2)
}

return fib(35)

```

## Quick Links

[Go Reference](https://pkg.go.dev/github.com/topxeq/charlang)

[Builtin Functions](https://pkg.go.dev/github.com/topxeq/charlang)

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

### Compile from source code

```shell
go get -u github.com/topxeq/charlang
```

### Start running the shell or scripts

After download, extract the executable from the zip file, put it into a directory, better in the system path.

Then type 'char' in the terminal/console to start the interactive command-line shell interface. Also you can run some scripts using command like 'char test.char', or 'char -example basic.char'. Here 'test.char' is the source code file(in UTF-8 encoding, plain text format).

Using command-line switch '-view' will show the source code of the script instead of run it.

### Various ways to run Charlang scripts

Examples:

- Run from a source file: `char d:\scripts\test.char`
- Run the text in clipboard as script source: `char -clip`
- Run from the remote server: `char -remote http://replacewithyourdomain.com/script/abc.char`
- Run the example code: `char -example basic.char`
- Run from Golang source directory: `char -gopath basic.char`
- Run from local scripts directory: place a config file local.cfg in the subdirectory 'char' under the user's home directory, with text content such as `c:\scripts`, then `char -local basic.char` will run 'c:\script\basic.char'
- Run from cloud/network: place a config file cloud.cfg in the subdirectory 'char' under the user's home directory, with text content such as `http://script.my.com/`, then `char -cloud basic.char` will be the same as `char -remote http://script.my.com/basic.char`

### Get the examples

All the source code examples marked by file names in the document can be retrieved or run by the command line like:

```shell
C:\Users\Administrator>char -example -view basic.char
// do simple add operation
x := 1.2
y := x + 1

println(x + y)

pass()

C:\Users\Administrator>char -example basic.char
3.4000000000000004

C:\Users\Administrator>
```

You can browse to `http://topget.org/dc/c/charlang/example/basic.char` to view the source code in an online text editor.

Using command-line switch '-viewPage' with '-example' will show the online code page in system-default browser as well.

### Quick tour

#### Hello world!

file: [helloWorld.char](http://topget.org/dc/c/charlang/example/helloWorld.char)

```go
pln("Hello world!")

```

The function 'pln' is the same as 'println' in other languages. pln formats using the default formats for its operands and writes to standard output.

The result output:

```shell
C:\Users\Administrator>char -example helloWorld.char
Hello world!

C:\Users\Administrator>

```

#### Define variables

file: [var.char](http://topget.org/dc/c/charlang/example/var.char)

```go
// define a variable before using it
var a

a = 1

pln(a)

// assign a value of another type to a
a = "abc"

pln(a)

// define and assign in one step
b := true

pln(b)

```

The result:

```shell
C:\Users\Administrator>char -example var.char
1
abc
true

```


