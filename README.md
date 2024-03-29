<!-- |title: The Char Language (Charlang)| -->

- [The Char Language (Charlang)](#the-char-language-charlang)
  - [Features](#features)
  - [New Features](#new-features)
  - [Quick Links](#quick-links)
  - [Quick Start](#quick-start)
  - [Documentation](#documentation)
    - [Get the Binary](#get-the-binary)
    - [Compile from Source Code](#compile-from-source-code)
    - [Start Running the Shell or Scripts](#start-running-the-shell-or-scripts)
    - [Various Ways to Run Charlang Scripts](#various-ways-to-run-charlang-scripts)
    - [Get the Examples](#get-the-examples)
    - [Quick Tour](#quick-tour)
      - [Hello World!](#hello-world)
      - [Comments](#comments)
      - [Define Variables](#define-variables)
      - [Data Type Name](#data-type-name)
      - [Boolean Data Type](#boolean-data-type)
      - [Integer Data Type](#integer-data-type)
      - [Float Data Type](#float-data-type)
      - [String/Bytes/Chars Data Type](#stringbyteschars-data-type)

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

[Charlang Home](http://topget.org/charlang)

[Go Reference](https://pkg.go.dev/github.com/topxeq/charlang)

[Builtin Functions](http://topget.org/dc/charlang/funcs)

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

### Get the Binary

Download the binary release files according to your OS from the website: [Charlang Homepage](http://topget.org/charlang).

### Compile from Source Code

```shell
go get -u github.com/topxeq/charlang
```

### Start Running the Shell or Scripts

After download, extract the executable from the zip file, put it into a directory, better in the system path.

Then type 'char' in the terminal/console to start the interactive command-line shell interface. Also you can run some scripts using command like 'char test.char', or 'char -example basic.char'. Here 'test.char' is the source code file(in UTF-8 encoding, plain text format).

Using command-line switch '-view' will show the source code of the script instead of run it.

### Various Ways to Run Charlang Scripts

Examples:

- Run from a source file: `char d:\scripts\test.char`
- Run the text in clipboard as script source: `char -clip`
- Run from the remote server: `char -remote http://replacewithyourdomain.com/script/abc.char`
- Run the example code: `char -example basic.char`
- Run from Golang source directory: `char -gopath basic.char`
- Run from local scripts directory: place a config file local.cfg in the subdirectory 'char' under the user's home directory, with text content such as `c:\scripts`, then `char -local basic.char` will run 'c:\script\basic.char'
- Run from cloud/network: place a config file cloud.cfg in the subdirectory 'char' under the user's home directory, with text content such as `http://script.my.com/`, then `char -cloud basic.char` will be the same as `char -remote http://script.my.com/basic.char`

### Get the Examples

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

### Quick Tour

#### Hello World!

file: [example001.char](http://topget.org/dc/c/charlang/example/example001.char)

```go
// function 'pln' is the same as 'println' in other languages
pln("Hello world!")

```

The function 'pln' is the same as 'println' in other languages. pln formats using the default formats for its arguments and writes to standard output.

And in Charlang, comments are supported. You can use `//` or `/* ... */` to guide the comments. 

The resulting output of the script:

```shell
C:\Users\Administrator>char -example example001.char
Hello world!

C:\Users\Administrator>

```
#### Comments

file: [example002.char](http://topget.org/dc/c/charlang/example/example002.char)

As mentioned above, like Golang, Charlang supports line comments (//...) and block comments (/* ... */). Comments in the code will be ignored by the compiler and virtual machine. You can use Ctrl+/key combination in many text or source code editors to switch whether the line is commented or not.

```go
/*
  multi-line block comments - 1
  multi-line block comments - 2
  multi-line block comments - 3
*/

a := 8    // line comments

// line comments at the start of a new line
pln("a:", a)

```

The output:

```shell
C:\Users\Administrator>char -example example002.char
a: 8

```

#### Define Variables

file: [example003.char](http://topget.org/dc/c/charlang/example/example003.char)

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
C:\Users\Administrator>char -example example003.char
1
abc
true

```

Note that unlike Golang, a variable can be assigned a value of different types.

#### Data Type Name

file: [example004.char](http://topget.org/dc/c/charlang/example/example004.char)

```go

a := 3 // assign an integer(int) value to variable 'a'

// function 'pl' is equivalent to the printf function in other languages, 
// followed by an additional newline character "\n"
// and the conversion characters are the same as Golang, 
// '%T' is used to output the value's type, '%v' is the general output format for any value
pl("[%T] %v", a, a)

// Instead of using '%T', which will output the native type in Golang(in which Charlang is written)
// function 'typeOf' is often used to get the type name of a variable in Charlang
pl("[%v] %v", typeOf(a), a)

```

The output:

```shell
C:\Users\Administrator>char -example example004.char
[charlang.Int] 3
[int] 3

```

#### Boolean Data Type

file: [example005.char](http://topget.org/dc/c/charlang/example/example005.char)

```go

// Boolean values

b := true

// function 'prf' is the same as 'printf' in C/C++/Golang
prf("[%v] %v\n", typeOf(b), b)

c := false

prf("[%T] %v\n", c, c)

prf("!b = %v\n", !b) // the operators are the same as Golang and many other languages

prf("b == c: %v\n", b == c)

prf("1 > 14: %v\n", 1 > 14)

prf("b == true: %v\n", b == true)

prf("b && c: %v\n", b && c)

prf("b || c: %v\n", b || c)

```

The output:

```shell
C:\Users\Administrator>char -example example005.char
[bool] true
[charlang.Bool] false
!b = false
b == c: false
1 > 14: false
b == true: true
b && c: false
b || c: true

```

#### Integer Data Type

file: [example006.char](http://topget.org/dc/c/charlang/example/example006.char)

```go

// Integer

c1 := 19

c2 := 18

pln(c1 + c2/3)

pl("%v, %v", typeOf(c1), c1)

pl("%T, %v", c1+c2, c1+c2)
pl("%v, %v", typeOf(c2/3), c2/3)
pl("%v, %v", typeOf(c1+c2/3), c1+c2/3)
pl("%T, %v", (c1+c2/3)*6, (c1+c2/3)*6)

```

The output:

```shell
C:\Users\Administrator>char -example example006.char
25
int, 19
charlang.Int, 37
int, 6
int, 25
charlang.Int, 150

```

#### Float Data Type

file: [example007.char](http://topget.org/dc/c/charlang/example/example007.char)

```go

// Float

f1 := 1.32

pl("%v, %v", typeOf(f1), f1)

previus_f1 := f1

f1 = f1 * 0.8

// function 'pr' is the same as 'print' in other languages
pr(previus_f1, "*", 0.8, "=", f1)

pln()

f2 := 0.93
f2 /= 0.3

pr(0.93, "/", 0.3, "=", f2, "\n")

```

The output:

```shell
C:\Users\Administrator>char -example example007.char
float, 1.32
1.32*0.8=1.056
0.93/0.3=3.1

```

#### String/Bytes/Chars Data Type

file: [example008.char](http://topget.org/dc/c/charlang/example/example008.char)

```go

// String, Bytes and Chars

s1 := "abc"

// concatenate strings
s2 := s1 + "3"

// function 'plt' will output the value with its Charlang type
plt(s2)

pln(s1, "+", "3", "=", s2)

s5 := "上善若水"

// function 'plt' will output the value with its internal(Golang) type
plo(s5)

s6 := bytes(s5)

// s6 will be a bytes array
pln("s6:", s6)

// t will be a utf-8 character(rune in Golang)
t := char(5)

plo(t)

// s7 will be array of chars
// in this example, will be 4 unicode characters, each has 3 bytes
s7 := chars(s5)

plt(s7)

// slice of s5(string) will be a string with only one ASCII(0-255) character
pl("s5[1:2] = %v(%#v)", s5[1:2], s5[1:2])

// slice of s6(bytes, i.e. array of byte) will be a byte array contains only one item
pl("s6[1:2] = %v(%#v)", s6[1:2], s6[1:2])

// slice of s7(chars, i.e. array of char) will be a char array contains only one item
pl("s7[1:2] = %v(%#v)", s7[1:2], s7[1:2])

// covert utf-8 chars to string
pl("string(s7[1:3]) = %v(%#v)", string(s7[1:3]), string(s7[1:3]))

// covert utf-8 chars to bytes, then to string, has the same effect as above
pl("string(bytes(string(s7[1:3]))) = %v(%#v)", string(bytes(string(s7[1:3]))), string(bytes(string(s7[1:3]))))

// output the first item of string, bytes and chars, as a single character
pl("%c", s5[1])
pl("%c", s6[1])
pl("%c", s7[1])

// output the first item of string, bytes and chars, with its value and type
pl("%T, %#v", s5[1], s5[1])
pl("%v, %#v", typeOf(s6[1]), s6[1])
pl("%T, %#v", s7[1], s7[1])

// iterate the string using 'for' loop
for i := 0; i < len(s5); i++ {
	pl("%v: %v, %v", i, typeOf(s5[i]), s5[i])
}

// iterate the string using 'for-in' loop
for i, v in s5 {
	pl("%v: %v, %v", i, typeOf(v), v)
}

// iterate the chars
for i, v in s7 {
	// function 'typeName' is equivalent to 'typeOf'
	pl("%v: %v, %v", i, typeName(v), v)
}

```

The output:

```shell
C:\Users\Administrator>char -example example008.char
(string)abc3
abc + 3 = abc3
(charlang.String)"上善若水"
s6: [228 184 138 229 150 132 232 139 165 230 176 180]
(charlang.Char)5
(chars)[19978 21892 33509 27700]
s5[1:2] = �("\xb8")
s6[1:2] = [184]([]byte{0xb8})
s7[1:2] = [21892]([]int32{21892})
string(s7[1:3]) = 善若("善若")
string(bytes(string(s7[1:3]))) = 善若("善若")
¸
¸
善
charlang.Int, 184
int, 184
charlang.Char, 21892
0: int, 228
1: int, 184
2: int, 138
3: int, 229
4: int, 150
5: int, 132
6: int, 232
7: int, 139
8: int, 165
9: int, 230
10: int, 176
11: int, 180
0: byte, 228
1: byte, 184
2: byte, 138
3: byte, 229
4: byte, 150
5: byte, 132
6: byte, 232
7: byte, 139
8: byte, 165
9: byte, 230
10: byte, 176
11: byte, 180
0: char, 19978
1: char, 21892
2: char, 33509
3: char, 27700

```

