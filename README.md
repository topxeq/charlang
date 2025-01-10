<!-- |title: The Char Language (Charlang)| -->

- [The Char Language (Charlang)](#the-char-language-charlang)
  - [1. Features](#1-features)
  - [2. New Features](#2-new-features)
  - [3. Quick Links](#3-quick-links)
  - [4. Download](#4-download)
  - [5. Installation](#5-installation)
  - [6. Quick Start](#6-quick-start)
  - [7. Documentation](#7-documentation)
    - [7.1 Get the Binary](#71-get-the-binary)
    - [7.2 Compile from Source Code](#72-compile-from-source-code)
    - [7.3 Start Running the Shell or Scripts](#73-start-running-the-shell-or-scripts)
    - [7.4 Various Ways to Run Charlang Scripts](#74-various-ways-to-run-charlang-scripts)
    - [7.5 Get the Examples](#75-get-the-examples)
    - [7.6 Quick Tour](#76-quick-tour)
      - [Hello World!](#hello-world)
      - [Comments](#comments)
      - [Define Variables](#define-variables)
      - [Data Type Name](#data-type-name)
      - [Boolean Data Type](#boolean-data-type)
      - [Integer Data Type](#integer-data-type)
      - [Float Data Type](#float-data-type)
      - [String/Bytes/Chars Data Type](#stringbyteschars-data-type)
      - [Array](#array)
      - [Map](#map)
      - [Function Type(Declare Function)](#function-typedeclare-function)
      - [For Loop](#for-loop)
      - [If Statement](#if-statement)
      - [Predefined Global Variables](#predefined-global-variables)
      - [Error handling: Try-Catch-Finally](#error-handling-try-catch-finally)
      - [Run Charlang Script/Code](#run-charlang-scriptcode)
      - [Multi-Threading](#multi-threading)
      - [Gel](#gel)
      - [Eval Machine(Virtual Machine to Run More Than One Piece of Script)](#eval-machinevirtual-machine-to-run-more-than-one-piece-of-script)
      - [Implement a Common Web Server](#implement-a-common-web-server)
        - [Start a Simple Web Server(with SSL Support) to Serve Static Files](#start-a-simple-web-serverwith-ssl-support-to-serve-static-files)
        - [Start a Common Web Server](#start-a-common-web-server)
        - [Web Server with Multi-thread](#web-server-with-multi-thread)
        - [Request Handler Running in a New Virtual Machine](#request-handler-running-in-a-new-virtual-machine)
        - [Serve Static and Dynamic HTML Pages at the Same Time](#serve-static-and-dynamic-html-pages-at-the-same-time)
      - [Charlang's Embedded Fully Functional Web/Microservices/Application Server](#charlangs-embedded-fully-functional-webmicroservicesapplication-server)
      - [Charlang as System Service](#charlang-as-system-service)
    - [7.7 More Examples](#77-more-examples)
      - [Builtin Function: checkErr](#builtin-function-checkerr)
      - [Anonymous Function](#anonymous-function)
      - [More About Array](#more-about-array)
      - [More About Map](#more-about-map)
      - [Mutable String](#mutable-string)
      - [Big Int](#big-int)
      - [Big Float](#big-float)
      - [Bitwise Processing](#bitwise-processing)
      - [Calculate BMI](#calculate-bmi)
      - [Calculate Cosine Similarity of Two Vectors](#calculate-cosine-similarity-of-two-vectors)
      - [Calculate Cosine Similarity of Two Vectors by Big Floats](#calculate-cosine-similarity-of-two-vectors-by-big-floats)
      - [One More Example for Gels](#one-more-example-for-gels)
      - [Redirect Stdout to a File](#redirect-stdout-to-a-file)
      - [Get Named Value in Charlang](#get-named-value-in-charlang)
      - [Call Named Function in Golang](#call-named-function-in-golang)
      - [Compare Binary Files](#compare-binary-files)
      - [A Simple Text Editor](#a-simple-text-editor)
      - [Base64 Encoding of Images](#base64-encoding-of-images)
      - [Plot Data Graph in Console](#plot-data-graph-in-console)
      - [Plot Data Graph in Console with Realtime Data Update](#plot-data-graph-in-console-with-realtime-data-update)
      - [Plot Simple Data Graph to Image](#plot-simple-data-graph-to-image)
      - [Plot Simple Data Graph to Web View](#plot-simple-data-graph-to-web-view)
      - [Plot Line Charts](#plot-line-charts)
      - [Calculator in WebView2](#calculator-in-webview2)
      - [Byte Queue](#byte-queue)
    - [7.8 Advance Topics](#78-advance-topics)
      - [Language Considerations](#language-considerations)
      - [Run Script from Command Line](#run-script-from-command-line)
      - [Show Environment Information of Charlang](#show-environment-information-of-charlang)
      - [Compile a Script to One Executable](#compile-a-script-to-one-executable)
      - [Charlang as An Embedded Language in Golang](#charlang-as-an-embedded-language-in-golang)
      - [Variables Declaration and Scopes](#variables-declaration-and-scopes)
        - [param](#param)
        - [global](#global)
        - [var](#var)
        - [const](#const)
      - [Values and Value Types](#values-and-value-types)
        - [Error Values](#error-values)
      - [Charlang Runtime Types](#charlang-runtime-types)
        - [Basic types:](#basic-types)
        - [More types:](#more-types)
        - [Go Type Definitions](#go-type-definitions)
        - [Type Conversion/Coercion Table](#type-conversioncoercion-table)
        - [Object.IsFalsy()](#objectisfalsy)
      - [Builtin Errors](#builtin-errors)
        - [Undefined Values](#undefined-values)
        - [Array Values](#array-values)
        - [Map Values](#map-values)
        - [Function Values](#function-values)
      - [Type Conversions](#type-conversions)
      - [Order of evaluation](#order-of-evaluation)
      - [Operators](#operators)
        - [Unary Operators](#unary-operators)
        - [Binary Operators](#binary-operators)
        - [Ternary Operators](#ternary-operators)
        - [Assignment and Increment Operators](#assignment-and-increment-operators)
        - [Operator Precedences](#operator-precedences)
        - [Selector and Indexer](#selector-and-indexer)
      - [Statements](#statements)
        - [If Statement](#if-statement-1)
        - [For Statement](#for-statement)
        - [For-In Statement](#for-in-statement)
      - [Modules](#modules)
      - [Differences from Go](#differences-from-go)
      - [Interfaces](#interfaces)
        - [Object interface](#object-interface)
        - [Iterator interface](#iterator-interface)
        - [Copier interface](#copier-interface)
        - [IndexDeleter interface](#indexdeleter-interface)
        - [LengthGetter interface](#lengthgetter-interface)
        - [Object Interface Extensions](#object-interface-extensions)

# The Char Language (Charlang)

[Charlang](https://topget.org/charlang) is a fast, dynamic scripting language to embed in Go applications.
Charlang is compiled and executed as bytecode on stack-based VM that's written
in native Go. Charlang has a more-common runtime error handling(try-catch-finally) than Golang.

Charlang is inspired by and based on awesome script language [uGo](https://github.com/ozanh/ugo). Special thanks to uGo's creater([ozanh](https://github.com/ozanh)) and contributors.

## 1. Features

* Written in native Go (no cgo).
* `if else` statements.
* `for` and `for in` statements.
* `try catch finally` statements.
* `param`, `global`, `var` and `const` declarations.
* Rich builtins.
* Module support.
* Go like syntax with additions.

## 2. New Features

- New types such as Byte, Image, Any...
- New functions: NewCommonError, NewError and more...
- New builtin functions: getRandomInt, writeResp, setRespHeader, writeRespHeader and much more...
- New global variables and resources.
- A new thread-model.
- Runtime/dynamically script compiling and running capability.
- Server mode: quickly start a WEB and/or application server.
- Run as system service.

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

## 3. Quick Links

[Charlang Home](https://topget.org/charlang)

[Go Reference](https://pkg.go.dev/github.com/topxeq/charlang)

[Builtin Functions](https://topget.org/dc/charlang/funcs)

## 4. Download

- [Windows x64](https://topget.org/pub/char.zip)
- [Windows x64(No Console Version - for GUI Applications)](https://topget.org/pub/charw.zip)
- [Linux Amd64](https://topget.org/pub/char.tar.gz)
- [Linux Arm8(Termux)](https://topget.org/pub/charArm8.tar.gz)

Or download the package from [Charlang Official Site](https://topget.org/charlang)

## 5. Installation

Download the latest Charlang executable file or compressed package above or from the [official website](https://topget.org/charlang), and then put it in a directory, preferably within the system path(such as C:\Windows in Windows or /usr/bin). If you download a compressed package, decompress it first. Then it is ready to use, run it in any terminal or console application(Window CMD, PowerShell, Terminal or bash);

## 6. Quick Start

Install Charlang first according to the installation guide, or build from source code by:

`go get -u github.com/topxeq/charlang`

`go get -u github.com/topxeq/charlang/cmd/char`

Charlang has a REPL application to learn and test Charlang language, run Charlang's main executable file with no command-line arguments to start it.

`char.exe` or `./char`

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

## 7. Documentation

### 7.1 Get the Binary

Download the binary release files according to your OS from the website: [Charlang Homepage](https://topget.org/charlang).

### 7.2 Compile from Source Code

```shell
go get -u github.com/topxeq/charlang
```

or

```shell
cd $GOPATH/src/github.com/topxeq

git clone https://github.com/topxeq/tkc

git clone https://github.com/topxeq/charlang

cd $GOPATH/src/github.com/topxeq/charlang/cmd

go install
```

### 7.3 Start Running the Shell or Scripts

After download, extract the executable from the zip file, put it into a directory, better in the system path.

Then type 'char' in the terminal/console to start the interactive command-line shell interface. Also you can run some scripts using command like 'char test.char', or 'char -example basic.char'. Here 'test.char' is the source code file(in UTF-8 encoding, plain text format).

Using command-line switch '-view' will show the source code of the script instead of run it.

### 7.4 Various Ways to Run Charlang Scripts

Examples:

- Run from a source file: `char d:\scripts\test.char`
- Run the text in clipboard as script source: `char -clip`
- Run from the remote server: `char -remote http://replacewithyourdomain.com/script/abc.char`
- Run the example code: `char -example basic.char`
- Run from Golang source directory: `char -gopath basic.char`
- Run from local scripts directory: place a config file local.cfg in the subdirectory 'char' under the user's home directory, with text content such as `c:\scripts`, then `char -local basic.char` will run 'c:\script\basic.char'
- Run from cloud/network: place a config file cloud.cfg in the subdirectory 'char' under the user's home directory, with text content such as `http://script.my.com/`, then `char -cloud basic.char` will be the same as `char -remote http://script.my.com/basic.char`
- Select the script(or input the script file path in console mode) to run: `char -selectScript`
- Load the script: `char -edit d:\scripts\test.char`, then run it in the editor(click the 'Quick Run' button) on Windows(WebView2)
- Run it in the CharEd IDE(Lazarus), which can be downloaded from the official website, in the Windows package
- Use it as an embeded script language engine, in any Golang compatible languages
- Run it through other languages, with the compiled DLL of Charlang

### 7.5 Get the Examples

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

You can browse to `https://topget.org/dc/c/charlang/example/basic.char` to view the source code in an online text editor.

Using command-line switch '-viewPage' with '-example' will show the online code page in system-default browser as well.

### 7.6 Quick Tour

#### Hello World!

file: [example001.char](https://topget.org/dc/c/charlang/example/example001.char)

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

file: [example002.char](https://topget.org/dc/c/charlang/example/example002.char)

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

file: [example003.char](https://topget.org/dc/c/charlang/example/example003.char)

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

// can not use := again for the same variable
// the next line will cause error
// b := 1.1

```

The result:

```shell
C:\Users\Administrator>char -example example003.char
1
abc
true

```

Note that unlike Golang, a variable can be assigned a value of different types.

Declare more than 1 variable in one line within a pair of parentheses.

```go
var (a, b, c)

...
```

#### Data Type Name

file: [example004.char](https://topget.org/dc/c/charlang/example/example004.char)

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

file: [example005.char](https://topget.org/dc/c/charlang/example/example005.char)

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

file: [example006.char](https://topget.org/dc/c/charlang/example/example006.char)

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

file: [example007.char](https://topget.org/dc/c/charlang/example/example007.char)

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

file: [example008.char](https://topget.org/dc/c/charlang/example/example008.char)

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

#### Array

```go
// declare an array
a := [1, 2, 3, "abc", 12.3]

println("a:", a)

println("a[2]:", a[2])

println("length of a:", len(a))

// reassign the array variable
a = [1, 2]

// append values
a = append(a, "abc")

// array item can be any type, even another array
b := ["xyz", 16, a]

pln("b:", b)

```

output:

```shell
a: [1, 2, 3, "abc", 12.3]
a[2]: 3
length of a: 5
b: ["xyz", 16, [1, 2, "abc"]]
```

Refer to the array example in More Examples section for more information about array type.

#### Map

In Charlang, map is a set of key-value pairs where key is string and the value is
of any value types. Value of a map can be accessed using indexer `[]` or
selector '.' operators.

```go
// declare an empty map
a := {}

// all keys will be converted to string type, values keep their original types
a["Num"] = 3
a[5] = "abc"
a[-1] = true
a["ary"] = [1, "xyz", false]
a[false] = "not true"
a.Item1 = "item 1"

pln(a)

// length is the number of key-value pairs in the map
pl("length of a: %v", len(a))

// index by dot
pl("a.Num: %v", a.Num)

// index by square brackets
a["Num"]++
pln(a["Num"])

// slice
pln(a[5][2:3])

a[5] = a[5] + a[5]

// slice to end
a[5] = a[5][1:]
pl("a[5]: %v", a[5])

// slice from begining
pln(a[5][:2])

// iterate
for k, v in a {
	println("[", k, "]:", v)
}
  
pln("---")

// declare map with initial values
b := {"intItem": 12, "floatItem": 5.6, "boolItem": true, "stringItem": "str1", "arrayItem": ["array", "in", "map"], "mapItem": {"map": 1, "in": "map"}}

plt(b)

pln("---")

c := {}

// all keys will be converted to string type
c[3] = "3"
c[18] = "abc"
c[-198] = "true"

pl("c: %v", c)

v1 := c[18]

if v1 == undefined {
	println("v1:", v1)
}

// index with non-existent key
v2 := c[19]

if v2 == undefined {
	println("v2:", v2)
}

// remove key-value pair
delete(c, 18)

println("c:", c)

```  

output:

```shell
{"Num": 3, "5": "abc", "-1": true, "ary": [1, "xyz", false], "false": "not true", "Item1": "item 1"}
length of a: 6
a.Num: 3
4
c
a[5]: bcabc
bc
[ Num ]: 4
[ 5 ]: bcabc
[ -1 ]: true
[ ary ]: [1, "xyz", false]
[ false ]: not true
[ Item1 ]: item 1
---
(map){"intItem": 12, "floatItem": 5.6, "boolItem": true, "stringItem": "str1", "arrayItem": ["array", "in", "map"], "mapItem": {"map": 1, "in": "map"}}
---
c: {"3": "3", "18": "abc", "-198": "true"}
v2: undefined
c: {"3": "3", "-198": "true"}
```

Refer to the map example in More Examples section for more information about map type.

#### Function Type(Declare Function)

Function is a data type in Charlang.

```go
// declare a function with arguments
f1 := func(a, b, c, d) {
	return a*b + c/d
}

result1 := f1(3, 6, 8, 9)

pln("result1=", result1)

// variadic function(function accepts a variable number of arguments)
f2 := func(v0, v1, ...args) {
	sum := v0 + v1

	argsLen := len(args)

	for i := 0; i < argsLen; i++ {
		sum += args[i]
	}

	return sum
}

result2 := f2(3, 6, 8, 9, 7)

pln("result2=", result2)

```

#### For Loop

```go
c1 := 0

// classic for loop
for i := 0; i < 5; i++ {
	c1 += i
}

pln(c1 * 3)

// for loop with condition only
i := 0

for i < 5 {
	pl("i: %v", i)

	i = i + 1
}

// infinite loop(if no break)

c := 5
for {
	c = c + 5

	if c < 10 {
		continue
	}

	if c > 30 {
		break
	}
}

pln("c:", c)

```

Output:

```shell
30
i: 0
i: 1
i: 2
i: 3
i: 4
c: 35
```

#### If Statement

```go
var a

if !a {
	pln("!a")
}

pln("!a", !a)

if a {
	pln("a =", a)
} else {
	pln("a is not initialized")
}

b := 1

if b {
	pln("b =", b)
} else {
	pln("b is not initialized")
}

pln("!b", !b)

var c = "abc"

pln("!c", !c)

```

Output:

```shell
!a
!a true
a is not initialized
b = 1
!b false
!c false
```

#### Predefined Global Variables

There are some prefined global variables in Charlang, which can be refered by 'global' keyword. The following code will show the command-line arguments in order,

```go
global argsG

for i, v in argsG {
    pl("[%v] %v", i, v)
}
```

The global predefined variable 'argsG' holds the command-line parameters while running the Charlang's main program. The data type of argsG is array, so we could use it instantly without other declaration.

If using Charlang as a library in Golang, we can pass other global variables other than the predefined ones.

The common global predefined variables include:

- versionG: the current version of Charlang;
- basePathG: the base path of Charlang, will be the current user's home directory such as c:\Users\Administrator or the service root directory(c:\char in Windows and /char in Linux);
- argsG: the array typed variable holds the command-line parameters;
- scriptPathG: the path of the script file running currently;
- runModeG: running mode of Charlang(script, repl, service, charms, chp, chardc, ...)

And while runnin as a WEB/Application/Micro-service server, there are additional predefined global variables:

- requestG: the HTTP request object, holds the request information;
- responseG: the HTTP respone object that could write response to, or set response settings;
- reqUriG: the route of the request, such as 'static/images/img001.png'
- paraMapG: holds the GET/POST form values, in a map object, such as `{"auth": "xxxxx", "input1": "value1"}`

In Windows platform, there are some additional  predefined global variables:

- guiG: holds the object to interact with WebView2 component which could used to establish GUI applications.

#### Error handling: Try-Catch-Finally

```go
a := 0

var r

try {
	r = 3 / a

} catch e {
	pln("exception:", e)

} finally {
	
	pln("r:", r)
}

pln("done")
```

Output:

```shell
D:\tmp>char -example tryCatch.char
exception: ZeroDivisionError: 
r: undefined
done
```

#### Run Charlang Script/Code

- Run a piece of Charlang code(in format as string),

```go
sourceT := `
param (v1, v2)

return v1 + v2
`

codeT := charCode(sourceT)

codeT.compile()

resultT := codeT.run(12, 8.5)

pl("result: %v", resultT)


```

charCode is the object type to hold Charlang code to run, compile the source code before run it. Various parameters could be passed to the code object.

The output:

```shell
D:\tmp>char -exam runCode.char
result: 20.5
```

- Passing various length paramters,

```go
sourceT := `
param ...vargs

pln(toJson(vargs, "-sort"))

return vargs[2]

`

codeT := charCode(sourceT)

codeT.compile()

rs := codeT.run("abc", 123.5, true, {"name": "Tom", "age": 16})

pl("rs: %v", rs)

```

- Run code like a function call, and the fix part of parameters with various length paramters,

```go
sourceT := `
param (v1, v2, ...vargs)

pln("input:", v1, v2, ...vargs)

sum := v1 + v2

for i, v in vargs {
	sum += v
}

return sum
`

addAll := charCode(sourceT).compile()

resultT := addAll(12, 8.5, 2, 3, 16)

pl("result: %v", resultT)

```

#### Multi-Threading

- First way:

```go
func1 := func(v0) {
	for i := 0; i < 5; i++ {
		v0++

		pl("(thread) v0=%v", v0)

		sleep(1.0)
	}
}

a := 5

func1.threadRun(a)

sleep(0.15)

for i := 0; i < 5; i++ {
	a += 10

	pl("(main) a=%v", a)

	sleep(1.3)
}

```

- Second way, import the 'ex' module and use the ex.threadRunFunc function:

Note: here also demonstrate the usage of mutex and pass/set value by reference.

```go
ex := import("ex")

roundsT := 1000

mutex1 := mutex()

func1 := func(v0) {

	for i := 0; i < roundsT; i++ {
		lock(mutex1)

		setValueByRef(v0, unref(v0)+1)

		unlock(mutex1)

		pl("(thread) *v0=%v", unref(v0))

		sleep(0.05)
	}
}

a := new("int", 5)

ex.threadRunFunc(func1, a)

sleep(0.15)

for i := 0; i < roundsT; i++ {
	lock(mutex1)

	setValueByRef(a, unref(a)+10)

	unlock(mutex1)

	pl("(main) *a=%v", unref(a))

	sleep(0.065)
}


```

- The third way, run in another VM:

Note: this example also demonstrate how to pass parameters to the thread runs in another VM.

```go
// using Array or Map to pass parameters which may change in the thread

sourceT := `
param (v0)

for i := 0; i < 5; i++ {
	v0[0] ++

	pl("(thread) v0=%v", v0[0])

	sleep(1.0)
}

return

`

c1 := charCode(sourceT).compile()

if isErr(c1) {
	fatalf("failed to compile code: %v", c1)
}

a := [5]

c1.threadRun(a)

sleep(0.15)

for i := 0; i < 5; i++ {
	a[0] += 10

	pl("(main) a=%v", a[0])

	sleep(1.3)
}

```

#### Gel

Gel in Charlang is an object to encapsulate values or functions.

```go
sourceT := `
param (v0, ...vargs)

pln(v0, vargs)

add := func(a1, a2) {
	return a1 + a2
}

mul := func(a1, a2) {
	return a1 * a2
}

if v0 == "add" {
	return add
} else if v0 == "mul" {
	return mul
} else if v0 == "Pi" {
	return 3.1415926
}

return errStrf("member/method not found: %v", v0)
`

c1 := charCode(sourceT)

if isErr(c1.compile()) {
	pl("failed to compile: %v", c1.lastError)
	exit()
}

g1 := gel(c1)

rs := g1.add(1, 2)

plo(rs)

pl("g1.Pi: %v", g1.Pi)

pl("unknown member: %v", g1.var1)

pln(isErr(g1.var1))

pln(getErrStr(g1.var1))

try {
	pl("unknown func: %v", g1.func1())
} catch e {
	pl("unknown func(%v): %v", g1.func1, e)
}

rs2 := g1.mul(3.6, 8)

plo(rs2)

```

The output:

```shell
D:\tmp>char -example gel1.char
add []
(charlang.Int)3
Pi []
g1.Pi: 3.1415926
var1 []
unknown member: TXERROR:member/method not found: var1
true
member/method not found: var1
func1 []
unknown func(TXERROR:member/method not found: func1): NotCallableError: string
mul []
(charlang.Float)28.8
```

We can use 'ex' module to load gels as well, loaded gels are compiled already.

```go
ex := import("ex")

sourceT := `
param (v0, ...vargs)

pln(v0, vargs)

add := func(a1, a2) {
	return a1 + a2
}

mul := func(a1, a2) {
	return a1 * a2
}

if v0 == "add" {
	return add
} else if v0 == "mul" {
	return mul
} else if v0 == "Pi" {
	return 3.1415926
}

return errStrf("member/method not found: %v", v0)
`

g1 := ex.loadGel(sourceT)

rs := g1.add(1, 2)

plo(rs)

pl("g1.Pi: %v", g1.Pi)

pl("unknown member: %v", g1.var1)

pln(isErrX(g1.var1))

pln(getErrStrX(g1.var1))

try {
	pl("unknown func: %v", g1.func1())
} catch e {
	pl("unknown func(%v): %v", g1.func1, e)
}

rs2 := g1.mul(3.6, 8)

plo(rs2)

```

#### Eval Machine(Virtual Machine to Run More Than One Piece of Script)

Demonstrate how to create a new virtual machine to run scripts and/or eval the result value.

```go
ev1 := evalMachine("value1", "value2", 2, true)

// parameters could be passed in global variable inputG
// and argsG which all values will be converted to string
rs1 := ev1.eval(`
global inputG
global argsG

pln(inputG)
pln(argsG)

`)

plt(rs1)

rs := ev1.eval("3.6 * 12.5")

plt(rs)

rs = ev1.eval("a := 4")

plt(rs)

rs = ev1.eval("mathSqrt(16 * a)")

plt(rs)

// modify one of the paramter
ev1.eval("inputG[2] = 3.1415926")

rs = ev1.eval(`
return inputG
`)

plt(rs)

```

Running result:

```shell
D:\tmpx>char -exam eval1.char
["value1", "value2", 2, true]
["value1", "value2", "2", "true"]
(undefined)undefined
(float)45
(undefined)undefined
(float)8
(array)["value1", "value2", 3.1415926, true]
```

- to modify and/or add value to parameters

```go
// pass parameters through evaluations of an evalMachine
aryT := [[1, 2, 3]]

ev1 := evalMachine(1, 2, aryT)

rs1 := ev1.eval(`
global inputG

ary1 := inputG[2][0]

ary1[0] + ary1[1] + ary1[2]

`)

plt(rs1)

// change the value
aryT[0][2] = 5

rs := ev1.eval("return ary1[0] + ary1[1] + ary1[2]")

plt(rs)

// append a value
aryT[0] = append(aryT[0], 18)

rs = ev1.eval(`

ary1 = inputG[2][0]

plt(ary1)

return ary1[0] + ary1[1] + ary1[2] + ary1[3]
`)

plt(rs)

```

Output:

```shell
(int)6
(int)8
(array)[1, 2, 5, 18]
(int)26
```

#### Implement a Common Web Server

Demonstrate how to implement a more flexible Web server, with support of static files, dynamic pages(as PHP, JSP, ASPX...) and micro-services.

##### Start a Simple Web Server(with SSL Support) to Serve Static Files

```go
// quickly start a static web server to serve files under certain directory
// port and certificate directory could be set
handlerT := httpHandler("static", joinPath(getHomeDir(), "pages"))

if isErr(handlerT) {
	fatalf("failed to create httpHandler: %v", getErrStr(handlerT))
}

muxT := mux()

muxT.setHandler("/", handlerT)

pln("starting http server on port 80(default)...")
checkErr(muxT.threadStartHttpServer())

// the certificate files should be server.crt and server.key
pln("starting https(SSL) server on port 443...")
plErr(muxT.threadStartHttpsServer("-port=443", "-certDir=."))

for {
	pln(time().format("2006-01-02 15:04:05"), "heartbeat")
	sleep(5.0)
}

```

##### Start a Common Web Server

```go
// a very simple web server
// to test the server, open your browser, and browse to the URL address: http://127.0.0.1

muxT := mux()

handlerT := func(requestA, responseA) {
	// pl("req: %#v, res: %#v", requestA, responseA)

	writeResp(responseA, "This is a test!")

	return "TX_END_RESPONSE_XT"
}

muxT.setHandler("/", handlerT)

rs := muxT.startHttpServer()

pl("result: %v", rs)

```

Modify the code part in handlerT to fulfill customized requirement of the Web server. You can mix a static files handler with the same Mux object in different route, such as:

```go
muxT.setHandler("/", staticFileHandlerT)
muxT.setHandler("/common", commonHandlerT)
```

##### Web Server with Multi-thread

One more example to Demonstrate the usage of mutex to avoid conflicts for multi http requests:

```go
n1 := new("int", 0)

mutex1 := mutex()

muxT := mux()

handlerT := func(requestA, responseA) {
	pl("req: %#v, res: %#v", requestA, responseA)

	params1 := parseReqFormEx(requestA)

	plo(params1)

	setRespHeader(responseA, "Access-Control-Allow-Origin", "*")
	setRespHeader(responseA, "Access-Control-Allow-Headers", "*")
	setRespHeader(responseA, "Content-Type", "application/json;charset=utf-8")

	authT := trim(params1["auth"])

	if authT != "abc123" {
		writeResp(responseA, genJsonResp(requestA, "fail", "auth failed"))
		return "TX_END_RESPONSE_XT"
	}

	inputT := params1["input"]

	if inputT == undefined {
		writeResp(responseA, genJsonResp(requestA, "fail", "input could not be empty"))
		return "TX_END_RESPONSE_XT"
	}

	c1 := int(inputT)

	mutex1.lock()
	setValueByRef(n1, unref(n1)+c1)
	mutex1.unlock()

	writeResp(responseA, genJsonResp(requestA, "success", toStr(unref(n1))))

	return "TX_END_RESPONSE_XT"
}

muxT.setHandler("/test", handlerT)

rs := muxT.startHttpServer()

pl("result: %v", rs)

```

##### Request Handler Running in a New Virtual Machine

Another example to Demonstrate how to set handler runnin in a new VM, and the way to pass parameters to it:

```go
// handler runs in a seperate VM, more safe but with a little more system resource(and time) cost/overhead
n1 := new("int", 0)

mutex1 := mutex()

muxT := mux()

handlerT := httpHandler().set("code", `
param(n1, mutex1)

global requestG
global responseG

pl("req: %#v, res: %#v", requestG, responseG)

pl("n1: %#v, mutex1: %#v", n1, mutex1)

params1 := parseReqForm(requestG)

plo(params1)

setRespHeader(responseG, "Access-Control-Allow-Origin", "*")
setRespHeader(responseG, "Access-Control-Allow-Headers", "*")
setRespHeader(responseG, "Content-Type", "application/json;charset=utf-8")

authT := trim(params1["auth"])

if authT != "abc123" {
	writeResp(responseG, genJsonResp(requestG, "fail", "auth failed"))
	return "TX_END_RESPONSE_XT"
}

inputT := params1["input"]

if inputT == undefined {
	writeResp(responseG, genJsonResp(requestG, "fail", "input could not be empty"))
	return "TX_END_RESPONSE_XT"
}

c1 := int(inputT)

pln("c1:", c1)

mutex1.lock()

setValueByRef(n1, unref(n1)+c1)
mutex1.unlock()

writeResp(responseG, genJsonResp(requestG, "success", toStr(unref(n1))))

return "TX_END_RESPONSE_XT"

`, n1, mutex1)

if isErr(handlerT) {
	fatalf("failed to create http handler: %v", getErrStr(handlerT))
}

muxT.setHandler("/test", handlerT)

pln("starting web server on port 8080...")
rs := muxT.startHttpServer("-port=8080")

pl("result: %v", rs)

```

##### Serve Static and Dynamic HTML Pages at the Same Time

Read HTML template from the file according to the route(in the URL parameter).

```go
// demonstrate how to serve static and dynamic HTML pages, not this is not the only way

staticHandlerT := httpHandler("static", joinPath(getHomeDir(), "pages"))

if isErr(staticHandlerT) {
	fatalf("failed to create static handler: %v", getErrStr(staticHandlerT))
}

dynamicHandlerT := func(requestA, responseA) {
	// set the proper response header
	setRespHeader(responseA, "Access-Control-Allow-Origin", "*")
	setRespHeader(responseA, "Access-Control-Allow-Headers", "*")
	setRespHeader(responseA, "Content-Type", "text/html;charset=utf-8")

	// get the parameters from the HTTP request
	paramsT := parseReqForm(requestG)

	// get the 'req' parameter from query string or post form values
	// it will be used as route
	reqT := trim(params1["req"])

	// get HTML template from specified folder
	templateDirT := `c:\test\tmpl`

	templateFileT := joinPath(templateDirT, reqT+".html")

	templateHtmlT := loadText(templateFileT)

	// do some modifications on the template
	strReplace(templateHtmlT, "{{field1}}", getNowStr())

	// write the processed text to HTTP response
	writeResp(responseA, templateHtmlT)

	// write "TX_END_RESPONSE_XT" to end the response stream/output
	return "TX_END_RESPONSE_XT"
}

// serve dynamic pages
muxT.setHandler("/pages", dynamicHandlerT)

// serve other route as static pages
muxT.setHandler("/", staticHandlerT)

rs := muxT.startHttpServer()

pl("result: %v", rs)

```

#### Charlang's Embedded Fully Functional Web/Microservices/Application Server

&nbsp;

Charlang's main program comes with a server mode that supports a lightweight WEB/Application/API all-in-one server. You can start it using the following command line:

```shell
D:\tmp>char -server -dir=scripts
[2024/08/23 08:08:22] Charlang Server V1.3.3 -port=:80 -sslPort=:443 -dir=scripts -webDir=scripts -certDir=scripts
[2024/08/23 08:08:22] try starting ssl server on :443...
[2024/08/23 08:08:22] try starting server on :80 ...
[2024/08/23 08:08:22] failed to start https: open scripts\server.crt: The system cannot find the file specified.
```

As can be seen, the server mode of Charlang can be started with the '-server' parameter, and the '-port' parameter can be used to specify the HTTP service port (please add a colon), '-sslPort' can be used to specify the SSL port, '-certDir' can be used to specify the certificate file directory of the SSL service (which should be two files: server.crt and server.key), '-dir' can be used to specify the root directory of the service, and '-webDir' can be used to specify the web service for static pages and resources. These parameters have default values and can be seen without entering any parameters.

The error in the output information is because the SSL certificate was not provided, and the SSL service will not be able to start. Adding the certificate files will suffice.

Then open a browser to access the address http://127.0.0.1:80 to access the all-in-one web service written in Charlang.

Assuming that the specified directory contains three files: charmsIndex.char, charmsTmpl.html, and charmsApi.char, various modes supported by the application server established by Charlang can be displayed.

First, access it with a browser http://127.0.0.1/charmsTmpl.html This will be accessing general web services, as the web directory defaults to the same as the server root directory. Therefore, the static file xmsTmpl.html under the root directory will be displayed, which is an example web page.

![Screenshot](https://topget.org/dc/s/images/pic2719068761.png)

You can see that the "{{text1}}" tag after the text 'Please click the button ' in the webpage. This is the tag that we need to replace when displaying the dynamic webpage later. The content of the charmsTmpl.html file is as follows:

```html
<html>
<body>
    <script>
        function test() {
            let xhr = new XMLHttpRequest();

            xhr.open('POST', 'http://127.0.0.1:80/charms/charmsApi', true);

            xhr.setRequestHeader("Content-Type", "application/x-www-form-urlencoded")

            xhr.onload = function(){
                alert(xhr.responseText);
            }

            xhr.send("param1=abc&param2=123");
        }
    </script>

    <div>
        <span>Please click the button {{text1}}：</span><button onclick="javascript:test();">Button1</button>
    </div>

</body>
</html>
```

Then we try to perform dynamic web page output, which is similar to dynamically rendering of webpages in the background supported by PHP, ASP, JSP, or other similar frameworks. Then browse to 'http://127.0.0.1/charms/charmsIndex' . Add the charms path to the URL, which is a virtual path indicating that the server will search for the charmsIndex.char file in the root directory while starting the server. This code will output the webpage content. Let's take a look inside the charmsIndex.char file.

```go
// responseG is the predefined global variable which holds the HTTP response object to write to
global responseG

// basePathG is the predefined global variable which holds the specified base path while starting the server
global basePathG

// Set the default global return value variable outG to the string TX_END_RESPONSE_XT
// If the default Charlang server receives a function to process a request, the return result is TX_END_RESPONSE_XT
// The server processing(output of the HTTP response) of the page will be terminated, otherwise the return value will be output as a string to the webpage
outG := "TX_END_RESPONSE_XT"

// Obtain the corresponding web page template(HTML)
// The joinPath function will merge multiple file paths into one complete file path
// The first parameter represents the variable to be placed in the result, where $push represents stack pushing
// basePathG is a built-in global variable that represents the root directory of the service
templatePathT := joinPath(basePathG, `charmsTmpl.html`)

// Load the file as a text string(HTML)
templateTextT := loadText(templatePathT)

// Replace the {{text1}} tag to letter A
templateTextT = strReplace(templateTextT, "{{text1}}", "A")

// Set the corresponding response header
setRespHeader(responseG, "Content-Type", "text/html; charset=utf-8")

// Write the HTML to the web page output
// responseG is also a predefined global variable that represents the HTTP/webpage output object to be written to
writeResp(responseG, templateTextT)

// Return 'TX_END_RESPONSE_XT' to end the output
return outG

```

In Charlang server model, each HTTP request will be processed by a separate virtual machine, which can be seen as a microservice concept. The microservice in this example only replaces the specified tags in the loaded webpage template and outputs them to the webpage. Although it is simple, it has already demonstrated the basic principle of dynamic webpage, that is, it can perform necessary and controllable rendering before outputting the webpage.

Let's browse to 'http://127.0.0.1/charms/charmsIndex'. It will result in the following results:

![Screenshot](https://topget.org/dc/s/images/pic362253946.png)

We can found that the original tag has indeed been replaced with the uppercase letter A, verifying the effect of dynamic web pages.

Looking at the webpage template file charmsTmpl.html above, once the button is clicked, the JavaScript function `test` will be executed, where an AJAX request is made and the result of the request will be output using the `alert` function. This is a typical example of a client accessing a backend API service. Let's take a look at how to implement this backend API service. The following is the content of the charmsApi.char file also located in the server root directory:

```go
// declare the global variables to use in the context
global requestG
global reqNameG
global reqUriG
global responseG
global paraMapG

// Get the current time and put it into variable t
t := getNowStr()

// Output reference information(to server's local console, not the HTTP response)
// Where reqNameG is a predefined global variable that represents the service name, which is the last part of the access URL
// reqUriG is a predefined global variable that represents the service route/path
// paraMapG is also a global variable that represents the query string or form parameters contained in HTTP requests (which can be in GET or POST requests)
pl(`[%v] reqNameG: %v, reqUriG: %v, paraMapG: %v`, t, reqNameG, reqUriG, paraMapG)

// Set output response header information (to JSON format)
setRespHeader(responseG, "Content-Type", "text/json; charset=utf-8")

// Set the response status to 200 (HTTP_oK), indicating a successful request response
writeRespHeader(responseG, 200)

// Assembling response strings using spr instructions
str1 := spr("The request is: %v, uri: %v, parameters: %v", reqNameG, reqUriG, paraMapG)

// Generate encapsulated JSON responses using genJsonResp, or output strings in other formats on your own
respStr := genJsonResp(requestG, "success", str1)

// Write and output the response string (to a webpage)
writeResp(responseG, respStr)

// End processing and return 'TX_END_RESPONSE_XT' to terminate the continued output of the response stream
return "TX_END_RESPONSE_XT"

```

Then, if we click button `button1` on the webpage, we will get the following alert pop-up:

![Screenshot](https://topget.org/dc/s/images/pic1052413353.png)

This is because the webpage charmsTmpl.html called AJAX request to the service located at `http://127.0.0.1:80/charms/charmsApi`. And our Charlang server will find charmsApi.char (automatically added with the `.char` file name suffix) and execute it, so it will output the content we want.

Now, an example of a small but fully functional WEB/application/API all-in-one server has been fully demonstrated. It is already sufficient for a general and small application service, and has almost no external dependencies. Deployment is also very convenient, only requiring the main program of Charlang and corresponding HTML and script files in the specified directory.

&nbsp;

#### Charlang as System Service

Charlang can be started as a system service and supports operating systems such as Windows and Linux. As long as you add the command line parameter '-reinstallService' to run the Charlang main program, you can install a system service called charService in the system (which can be seen using the service management module in computer management under Windows). Note that installing services in the operating system generally requires administrator privileges. Under Windows, you need to open the CMD window as an administrator to execute this command, while under Linux, you need to execute it as root or with the sudo command.

After the service is started, a log will be recorded in the file charService.log in the service root directory (c:\char in Windows and /char in Linux). When the service starts for the first time, it will search for all files with names similar to taskXXX.char in the service root directory (such as task001.char, taskAbc.char, etc.) and run them one by one, and output their execution results (returned value) to the log. This type of code file is called a one-time-run task file, and is generally used in situations where it needs to be started and run once. It can also be manually run the command 'char -restartService' to restart the service and achieve the goal of task re-execution.

There are another kind of one-time-run task files which will be run in a seperated thread called 'thread tasks', their file name similar to threadTaskXXX.char in the service root directory (such as threadTask001.char, threadTaskAbc.char, etc.). These tasks is for those tasks need running continously, such as WEB servers, FRP server or client, ... If some error occur while running thread-tasks, the information will be logged in file 'runThreadTask.log' in service root directory.

In addition, during operation, the charService service checks the service root directory every 5 seconds. If there are files with names similar to autoRemoveTaskXXX.char (such as autoRemoveTask001.char, autoRemoveTaskAbc.char, etc.), the code in these files will be immediately executed and then deleted. This mechanism is similar to a task queue, allowing us to add tasks to the queue (placed in the service root directory) at any time, and Charlang service will execute these tasks at any time. And since the task will be deleted immediately after execution, it will not be executed repeatedly.

The command line parameters related to service installation, removal, start, stop, and restart of the Charlang main program also include '-installService', '-removeService', '-startService', '-stopService', '-restartService', and so on.

The task code can refer to examples such as task001.char, threadTask001.char, autoRemoveTask001.char, etc.

- One-time-task Example

file: [task001.char](https://topget.org/dc/c/charlang/example/task001.char)

```go
global basePathG

logPathT := joinPath(basePathG, "charService.log")

rs := appendText("\ntask001.char\n", logPathT)

return "task001 returns some result 000"

```

- Thread-task Example

file: [threadTask001.char](https://topget.org/dc/c/charlang/example/threadTask001.char)

```go
for {
	saveText(getNowStr(), `c:\char\task1.txt`)

	sleep(60.0)
}
```

It will be a continous loop which will write the current time string in the file every 60 seconds.

The following is the thread-task to run Frpc client when system started,

file: [threadTaskFrpc.char](https://topget.org/dc/c/charlang/example/threadTaskFrpc.char)

```go
appendText(spr("\n[%v] %v %v\n", getNowStr(), "threadTaskFrpc", "starting..."), `c:\logs\frpcTask.log`)

rs := systemCmd(`c:\tools\frp\frpc.exe`, `-c`, `c:\tools\frp\frpc.ini`)

appendText(spr("\n[%v] %v %v\n", getNowStr(), "threadTaskFrpc", rs), `c:\logs\frpcTask.log`)


```

- Auto-remove-task Example

file: [autoRemoveTask001.char](https://topget.org/dc/c/charlang/example/autoRemoveTask001.char)

```go
global basePathG

logPathT := joinPath(basePathG, "charService.log")

rs := appendText("\nautoRemoveTask001.char\n", logPathT)

```


### 7.7 More Examples

#### Builtin Function: checkErr

checkErr: check if the object is error or error string, if true, exit the program with output, usage: checkErr(result, "-format=Failed to process: %v\n"), the default format is "Error: %v\n"

file: [checkErr.char](https://topget.org/dc/c/charlang/example/checkErr.char)

#### Anonymous Function

file: [anonymousFunc.char](https://topget.org/dc/c/charlang/example/anonymousFunc.char)

#### More About Array

file: [array.char](https://topget.org/dc/c/charlang/example/array.char)

#### More About Map

file: [map.char](https://topget.org/dc/c/charlang/example/map.char)

#### Mutable String

file: [mutableString.char](https://topget.org/dc/c/charlang/example/mutableString.char)

#### Big Int

file: [bigInt.char](https://topget.org/dc/c/charlang/example/bigInt.char)

#### Big Float

file: [bigFloat.char](https://topget.org/dc/c/charlang/example/bigFloat.char)

#### Bitwise Processing

file: [bitwise.char](https://topget.org/dc/c/charlang/example/bitwise.char)

#### Calculate BMI

file: [bmi.char](https://topget.org/dc/c/charlang/example/bmi.char)

#### Calculate Cosine Similarity of Two Vectors

file: [calCosineSimilarity.char](https://topget.org/dc/c/charlang/example/calCosineSimilarity.char)

#### Calculate Cosine Similarity of Two Vectors by Big Floats

* more precision

file: [calCosineSimilarityBig.char](https://topget.org/dc/c/charlang/example/calCosineSimilarityBig.char)

#### One More Example for Gels

file: [gel3.char](https://topget.org/dc/c/charlang/example/gel3.char)

#### Redirect Stdout to a File

Note: Better run the example code with charw.exe(i.e. the GUI version of Charlang in Windows).

file: [guiRedirectStdout.char](https://topget.org/dc/c/charlang/example/guiRedirectStdout.char)

#### Get Named Value in Charlang

Note: These values should be defined in the Charlang source code first before using them. In charadd.go:

```go
...

var namedValueMapG = map[string]interface{}{
	"tk.TimeFormat":            tk.TimeFormat,            // "2006-01-02 15:04:05"
	"tk.TimeFormatMS":          tk.TimeFormatMS,          // "2006-01-02 15:04:05.000"
	"tk.TimeFormatMSCompact":   tk.TimeFormatMSCompact,   // "20060102150405.000"
	"tk.TimeFormatCompact":     tk.TimeFormatCompact,     // "20060102150405"
	"tk.TimeFormatCompact2":    tk.TimeFormatCompact2,    // "2006/01/02 15:04:05"
	"tk.TimeFormatDateCompact": tk.TimeFormatDateCompact, // "20060102"

	"time.Layout":   time.Layout,
	"time.RFC1123":  time.RFC1123,
	"time.RFC3339":  time.RFC3339,
	"time.DateTime": time.DateTime,
	"time.DateOnly": time.DateOnly,
	"time.TimeOnly": time.TimeOnly,

	"maxInt":   math.MaxInt,
	"minInt":   math.MinInt,
	"maxFloat": math.MaxFloat64,
	"minFloat": math.SmallestNonzeroFloat64,

...
```

Get these values by calling getNamedValue(getConst) function.

file: [namedValue.char](https://topget.org/dc/c/charlang/example/namedValue.char)

#### Call Named Function in Golang

Note: These functions should be defined in the Charlang source code first before calling them. In charadd.go:

```go
...

var namedFuncMapG = map[string]interface{}{
	"fmt.Fprintf": fmt.Fprintf,
	"tk.NewTK":    tk.NewTK,
}

...
```

Call these functions by calling callNamedValue function and passing the proper parameters.

file: [callNamedFunc.char](https://topget.org/dc/c/charlang/example/callNamedFunc.char)

With this method, in theory, we can add any functions in Golang's standard library or 3rd party repositories.

#### Compare Binary Files

file: [binCompare.char](https://topget.org/dc/c/charlang/example/binCompare.char)

#### A Simple Text Editor

file: [editFile.char](https://topget.org/dc/c/charlang/example/editFile.char)

A simple text editor with file load/save, JSON validate, code running features, in GUI.

#### Base64 Encoding of Images

file: [base64EncodeImage.char](https://topget.org/dc/c/charlang/example/base64EncodeImage.char)

Encode an image file to Base64 code to use in img tag in HTML, for example,

```shell
D:\tmp>char -exam base64EncodeImage.char -file=d:\downtemp\curtain-2757815_960_720.png
data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAA8AAAAHECAYAAAD22EOkAAABeGlDQ1BJQ0MgUHJvZmlsZQAAeJx1kL9LAmEcxh+1sB+GQRINDTdIQyiEDTWWDUKIiBlktdyddxqcetydRDQ2tDq4VLRk0X9QW/QPBEFQTUHU3FAQQcj1nCcood/jve+H5/0+7733AN60JpfMgTmgVLaMTCIubOQ2Bf8rhjGOSYSwIMqmvpxOJ9G3vh/hcfpD1Dmr/1zPGs0rpgx4hsiLsm5Y5CVyatfSHa6RQ3JRzJPPyRGDFyTfO7rk8rvDBZd/HDaymRXAGyALhS6WulguGiVyhBwuaVW5fR/nTwJKeX2Nfbq1TGSQQBwCJFSxAw0WouxlZt...
```

#### Plot Data Graph in Console

file: [asciiPlot.char](https://topget.org/dc/c/charlang/example/asciiPlot.char)

![Snapshot](https://topget.org/dc/s/images/pic3873474424.png)

#### Plot Data Graph in Console with Realtime Data Update

file: [asciiPlotRealTime.char](https://topget.org/dc/c/charlang/example/asciiPlotRealTime.char)

Demonstrate multi-thread, update one area of console...

#### Plot Simple Data Graph to Image

file: [basicPlot.char](https://topget.org/dc/c/charlang/example/basicPlot.char)

Demonstrate plotting a simple line chart with data in an array...

#### Plot Simple Data Graph to Web View

file: [basicPlotToWebView.char](https://topget.org/dc/c/charlang/example/basicPlotToWebView.char)

Demonstrate plotting a simple line chart with data in an array, then render it into an IMG element in WebView2...

#### Plot Line Charts

file: [imagePlot.char](https://topget.org/dc/c/charlang/example/imagePlot.char)

More examples of plotting line charts to images...

#### Calculator in WebView2

file: [calculatorInWebView.char](https://topget.org/dc/c/charlang/example/calculatorInWebView.char)

A tiny GUI calculator running in WebView2, use the HTML/CSS/JavaScript example code from github.com/WebDevSimplified/Vanilla-JavaScript-Calculator. Demonstrate how to embed WEB applications in Charlang.

#### Byte Queue

file: [byteQueue.char](https://topget.org/dc/c/charlang/example/byteQueue.char)

Demonstrate usage of a non-builtin data type "byteQueue", using 'newObj' function.

### 7.8 Advance Topics

#### Language Considerations

Charlang source code is compiled to bytecode and run in a Virtual Machine (VM). Compiled bytecode can be serialized/deserialized for wire to remove compilation step before execution for remote processes, and deserialization will solve version differences to a certain degree.

Main script and Charlang source modules in Charlang are all functions which have compiledFunction type name. Parameters can be defined for main function with param statement and main function returns a value with return statement as well. If return statement is missing, undefined value is returned by default. All functions return single value but thanks to destructuring feature Charlang allows to return multiple values as an array and set returning array elements to multiple variables.

Charlang relies on Go's garbage collector and there is no allocation limit for objects. Builtin objects can be disabled before compilation.

Charlang can handle runtime errors and Go panic with try-catch-finally statements which is similar to Ecmascript implementation with a few minor differences. Although Go developers are not fan of try-catch-finally, they are well known statements to work with.

Charlang is developed to be an embedded script language for Go applications also, and importing source modules from files will not be added in near future but one can implement a custom module to return file content to the compiler.

Charlang currently has a simple optimizer for constant folding and evaluating expressions which do not have side effects. Optimizer greedily evaluates expressions having only literals (int, uint, char, float, bool, string). Optimizer can be disabled with compiler options.

#### Run Script from Command Line

- One line

```shell
D:\tmp>char -cmd=pln(1+3)
4
```

- Multiple lines and/or with spaces

Original code:

```
a := 3
pln(a * 12)
```

Command-line after url-encode the parameter:

```
D:\tmp>char -cmd=a%20:=3%0Apln(a%20*%2012) -urlDecode
36
```

- Multiple lines and/or with spaces(alternative way)

Encrypt the script first:

```
D:\tmp>char
Charlang 1.2.2 by TopXeQ
> encryptText("a := 12\npln(a * 5)", "char") 
694B9F86EB4B1024390E25200A6264602C571C20282F29
>
```

Then prefix the string `//TXDEF#` before the parameter,

```shell
D:\tmp>char -cmd=//TXDEF#694B9F86EB4B1024390E25200A6264602C571C20282F29
60
```


#### Show Environment Information of Charlang

```shell
D:\tmp>char -env
Charlang by TopXeQ V1.2.3
basePath: C:\Users\Administrator\char
localPath: d:\scripts
cloudPath: https://script.example.com/scripts/
```

#### Compile a Script to One Executable

```shell
D:\tmp>char -compile -output=basic.exe -example basic.char

D:\tmpx>basic.exe
3.4000000000000004

D:\tmpx>basic
3.4000000000000004

```

Compile a script with charw.exe in Windows, to avoid the console window(CMD) to show while running. For example:

```shell

charw -compile -output=cal.exe -example guiCalculator.char

D:\tmpx>cal.exe

```

![Snapshot](https://topget.org/dc/s/attach/pic1110174843.png)


#### Charlang as An Embedded Language in Golang

To run a Charlang script in Golang, it must be compiled to create a `Bytecode` object then it is
provided to Virtual Machine (VM). Charlang has a simple optimizer enabled by default
in the compiler. Optimizer evaluates simple expressions not having side effects
to replace expressions with constant values. Note that, optimizer can be
disabled to speed up compilation process.

```go
package main

import (
  "fmt"

  "github.com/topxeq/charlang"
)

func main() {
  script := `
  param num

  var fib
  fib = func(n, a, b) {
    if n == 0 {
      return a
    } else if n == 1 {
      return b
    }
    return fib(n-1, b, a+b)
    }
  return fib(num, 0, 1)
  `
  bytecode, err := charlang.Compile([]byte(script), charlang.DefaultCompilerOptions)

  if err != nil {
    panic(err)
  }

  retValue, err := charlang.NewVM(bytecode).Run(nil,  charlang.Int(35))

  if err != nil {
    panic(err)
  }

  fmt.Println(retValue) // 9227465
}
```

Script above is pretty self explanatory, which calculates the fibonacci number
of given number.

Compiler options hold all customizable options for the compiler.
`TraceCompilerOptions` is used to trace parse-optimize-compile steps for
debugging and testing purposes like below;

```go
bytecode, err := charlang.Compile([]byte(script), charlang.TraceCompilerOptions)
// or change output and disable tracing parser
// opts := charlang.TraceCompilerOptions
// opts.Trace = os.Stderr
// opts.TraceParser = false
// bytecode, err := charlang.Compile([]byte(script), opts)
```

VM execution can be aborted by using `Abort` method which cause `Run` method to
return an error wrapping `ErrVMAborted` error. `Abort` must be called from a
different goroutine and it is safe to call multiple times.

Errors returned from `Run` method can be checked for specific error values with
Go's `errors.Is` function in `errors` package.

`VM` instances are reusable. `Clear` method of `VM` clears all references held
and ensures stack and module cache is cleaned.

```go
vm := charlang.NewVM(bytecode)
retValue, err := vm.Run(nil,  Charlang.Int(35))
/* ... */
// vm.Clear()
retValue, err := vm.Run(nil,  Charlang.Int(34))
/* ... */
```

Global variables can be provided to VM which are declared with `global` keyword. Globals are accessible to source modules as well. Map like objects should be used to get/set global variables as below.

```go
script := `
param num
global upperBound
return num > upperBound ? "big" : "small"
`
bytecode, err := charlang.Compile([]byte(script), charlang.DefaultCompilerOptions)

if err != nil {
  panic(err)
}

g := charlang.Map{"upperBound": charlang.Int(1984)}
retValue, err := charlang.NewVM(bytecode).Run(g, charlang.Int(2018))
// retValue == charlang.String("big")
```

There is a special type `SyncMap` in charlang to make goroutine safe map object where scripts/Go might need to interact with each other concurrently, e.g. one can collect statistics or data within maps. Underlying map of `SyncMap` is guarded with a `sync.RWMutex`.

```go
module := `
global stats

return func() {
  stats.fn2++
  /* ... */
}
`
script := `
global stats

fn1 := func() {
  stats.fn1++
  /* ... */
}

fn1()

fn2 := import("module")
fn2()
`
mm := charlang.NewModuleMap()
mm.AddSourceModule("module", []byte(module))

opts := charlang.DefaultCompilerOptions
opts.ModuleMap = mm

bytecode, err := charlang.Compile([]byte(script), opts)

if err != nil {
  panic(err)
}

g := &charlang.SyncMap{
    Map: charlang.Map{"stats": charlang.Map{"fn1": charlang.Int(0), "fn2": charlang.Int(0)}},
}
_, err = charlang.NewVM(bytecode).Run(g)
/* ... */
```

As can be seen from examples above, VM's `Run` method takes arguments and its signature is as below. A map like `globals` argument or `nil` value can be provided for globals parameter. `args` variadic parameter enables providing arbitrary number of arguments to VM which are accessed via `param` statement.

```go
func (vm *VM) Run(globals Object, args ...Object) (Object, error)
```

#### Variables Declaration and Scopes

##### param

`param` keyword is used to declare a parameter for main function (main script). Parenthesis is required for multiple declarations. Last argument can also be variadic. Unlike `var` keyword, initializing value is illegal. Variadic argument initialized as an empty array `[]`, and others are initialized as `undefined` if not provided. `param` keyword can be used only once in main function.

```go
param (arg0, arg1, ...vargs)
```

```go
param foo
param bar    // illegal, multiple param keyword is not allowed
```

```go
if condition  {
  param arg    // illegal, not allowed in this scope
}

func(){
    param (a, b)    // illegal, not allowed in this scope
}
```

##### global

`global` keyword is to declare global variables. Note that `var` statements or short variable declaration `:=` always creates local variables not global. Parenthesis is required for multiple declarations. Unlike `var`, initializing value is illegal. `global` statements can appear multiple times in the scripts.

`global` gives access to indexable `globals` argument with a variable name provided to Virtual Machine (VM).

If `nil` is passed to VM as globals, a temporary `map` assigned to globals.

Any assignment to a global variable creates or updates the globals element.

Note that global variables can be accessed by imported source modules which enables to export objects to scripts like `extern` in C.

```go
global foo
global (bar, baz)
```

```go
// "globals" builtin function returns "globals" provided to VM.
g := globals()
v := g["foo"]    // same as `global foo; v := foo`
```

```go
if condition {
  global x     // illegal, not allowed in this scope
}

func() {
  global y     // illegal, not allowed in this scope
}
```

##### var

`var` keyword is used to declare a local variable. Parenthesis is required for multiple declaration. Note: Tuple assignment is not supported with var statements.

```go
var foo               // foo == undefined
var (bar, baz = 1)    // bar == undefined, baz == 1
var (bar,
     baz = 1)         // valid
var (
    foo = 1
    bar
    baz = "baz"
)                     // valid
```

A value can be assigned to a variable using short variable declaration `:=` and assignment `=` operators.

* `:=` operator defines a new variable in the scope and assigns a value.
* `=` operator assigns a new value to an existing variable in the scope.

```go
                 // function scope A
a := "foo"       // define 'a' in local scope

func() {         // function scope B
  b := 52        // define 'b' in function scope B
  
  func() {       // function scope C
    c := 19.84   // define 'c' in function scope C

    a = "bee"    // ok: assign new value to 'a' from function scope A
    b = 20       // ok: assign new value to 'b' from function scope B

    b := true    // ok: define new 'b' in function scope C
                 //     (shadowing 'b' from function scope B)
  }
  
  a = "bar"      // ok: assign new value to 'a' from function scope A
  b = 10         // ok: assign new value to 'b'
  a := -100      // ok: define new 'a' in function scope B
                 //     (shadowing 'a' from function scope A)
  
  c = -9.1       // illegal: 'c' is not defined
  var b = [1, 2] // illegal: 'b' is already defined in the same scope
}

b = 25           // illegal: 'b' is not defined
var a = {d: 2}   // illegal: 'a' is already defined in the same scope
```

Following is illegal because variable is not defined when function is created.

In assignment statements right hand side is compiled before left hand side.

```go
f := func() {
  f()    // illegal: unresolved symbol "f"
}
```

```go
var f
f = func() {
  f()    // ok: "f" is declared before assignment.
}
```

Unlike Go, a variable can be assigned a value of different types.

```go
a := 123        // assigned    'int'
a = "123"       // reassigned 'string'
a = [1, 2, 3]   // reassigned 'array'
```

Capturing loop variables returns the last value of the variable set after last post statement of the for loop, like Go.

```go
var f

for i := 0; i < 3; i++ {
  f = func(){
    return i
  }  
}

println(f())  // 3
```

Like Go, to capture the variable define a new variable using same name or
different.

```go
var f

for i := 0; i < 3; i++ {
  i := i
  f = func(){
    return i
  }  
}

println(f())  // 2
```

##### const

`const` keyword is used to declare a local constant variable. Parenthesis is required for multiple declaration. Note: Tuple assignment is not supported.

The value of a constant can't be changed through reassignment.

Reassignment is checked during compilation and an error is thrown.

An initializer for a constant is required while declaring. The const declaration creates a read-only reference to a value. It does not mean the value it holds is immutable.

```go
const (
  a = 1
  b = {foo: "bar"}
)

const c       // illegal, no initializer

a = 2         // illegal, reassignment
b.foo = "baz" // legal
```

`iota` is supported as well.

```go
const (
  x = iota
  y
  z
)
println(x, y, z) // 0 1 2
```

```go
const (
  x = 1<<iota
  y
  z
)
println(x, y, z) // 1 2 4
```

```go
const (
  _ = 1<<iota
  x
  y
  z
)
println(x, y, z) // 2 4 8
```

```go
const (
  x = 1+iota
  _
  z
)
println(x, z) // 1 3
```

```go
const (
  x = func() { return iota }() // illegal, compile error
)
```

```go
const (
  iota = 1 // illegal, compile error
)
```

RHS of the assignment can be any expression so `iota` can be used with them as well.

```go
const (
  x = [iota]
  y
)
println(x) // [0]
println(y) // [1]
```

```go
const (
  _ = iota
  x = "string" + iota
  y
)
println(x) // string1
println(y) // string2
```

**Warning:** if a variable named `iota` is created before `const` assignments, `iota` is not used for enumeration and it is treated as normal variable.

```go
iota := "foo"

const (
  x = iota
  y
)
println(x) // foo
println(y) // foo
```

#### Values and Value Types

In Charlang, everything is a value, and, all values are associated with a type(object).

```go
19 + 84                 // int values
1u + 5u                 // uint values
"foo" + `bar`           // string values
-9.22 + 1e10            // float values
true || false           // bool values
'ç' > '9'               // char values
[1, false, "foo"]       // array value
{a: 12.34, "b": "bar"}  // map value
func() { /*...*/ }      // function value
```

Here's a list of fundamental value types in Charlang. 

| Charlang Type          | Description                          | Equivalent Type in Go |
|:------------------|:-------------------------------------|:----------------------|
| int               | signed 64-bit integer value          | `int64`               |
| uint              | unsigned 64-bit integer value        | `uint64`              |
| float             | 64-bit floating point value          | `float64`             |
| bool              | boolean value                        | `bool`                |
| char              | unicode character                    | `rune`                |
| string            | unicode string                       | `string`              |
| bytes             | byte array                           | `[]byte`              |
| error             | [error](#error-values) value         | -                     |
| array             | value array                          | `[]Object`            |
| map               | value map with string keys           | `map[string]Object`   |
| undefined         | [undefined](#undefined-values) value | -                     |
| compiledFunction  | [function](#function-values) value   | -                     |

##### Error Values

In Charlang, an error can be represented using "error" typed values. An error value is created using `error` builtin function, and, it has an underlying message. The underlying message of an error can be accessed using `.Message` selector. Error has also a name which is accessed using `.Name`. Errors created with `error` builtin have default name `error` but builtin errors have different names like `NotIterableError`, `ZeroDivisionError`.

First argument passed to `error` builtin function is converted to string as message.

```go
err1 := error("oops")
err2 := error(1+2+3)         // equivalent to err2 := error("6")
if isError(err1) {           // 'isError' is a builtin function
  name := err1.Name          // get underlying name
  message := err1.Message    // get underlying message
}  
```

#### Charlang Runtime Types

##### Basic types:

- **bool**: boolean (`bool` in Go)
- **byte**: unsigned 8bit integer (`uint8` in Go)
- **char**: character (`rune` in Go)
- **int**: signed 64bit integer (`int64` in Go)
- **uint**: unsigned 64bit integer (`uint64` in Go)
- **float**: 64bit floating point (`float64` in Go)
- **string**: string (`string` in Go)
- **bytes**: byte array (`[]byte` in Go)
- **chars**: character (`[]rune` in Go)
- **array**: objects array (`[]Object` in Go)
- **map**: objects map with string keys (`map[string]Object` in Go)
- **error**: an error with a string Name and Message
- **undefined**: undefined

##### More types:

- **bigInt**: holds an big integer value
- **bigFloat**: holds an big float value
- **mutableString**: mutable string 
- **orderedMap**: map with fix-ordered items
- **objectRef**: object reference to pass to functions
- **stack**: a common stack
- **queue**: a common queue
- **function**: callable function
- **compiledFunction**: compiled function
- **charCode**: holds the source code and runnable Charlang code
- **gel**: holds the source code and runnable Charlang code mainly for remote call
- **statusResult**: status object, usually return by server side, in form of: `{"Status": "success", "Value": "more info"}` or `{"Status": "fail", "Value": "error message"}`
- **stringBuilder**: holds a strings.Builder object in Golang
- **bytesBuffer**: holds a bytes.Buffer object in Golang
- **database**: holds a database object in Golang
- **time**: holds a time.Time object in Golang
- **location**: holds a time.Location object in Golang
- **seq**: an object to generate unique, sequential numbers
- **mutex**: mutex for multi-threading
- **mux**: mux(router) for http request
- **httpReq**: holds a http.Request object in Golang
- **httpResp**: holds a http.Response object in Golang
- **httpHandler**: holds a http.HandleFunc object in Golang
- **reader**: holds a io.Reader object in Golang
- **writer**: holds a io.Writer object in Golang
- **file**: holds a os.File object in Golang
- **image**: holds a image.Image object in Golang
- **delegate**: holds a delegate(callback) function for calling
- **etable**: holds an object with multi-sheet table data like Excel or CSV
- **excel**: holds an object with Excel data
- **any**: an object to hold any value types

##### Go Type Definitions

- `int`

```go
type Int int64
```

- `uint`

```go
type Uint uint64
```

Note: uint values can be represented by adding `u` suffix to integer values.

- `float`

```go
type Float float64
```

- `bool`

```go
type Bool bool
```

- `char`

```go
type Char rune
```

- `string`

```go
type String string
```

- `bytes`

```go
type Bytes []byte
```

- `error`

```go
type Error struct {
  Name    string
  Message string
  Cause   error
}
```

- `array`

```go
type Array []Object
```

- `map`

```go
type Map map[string]Object
```

- `syncMap`

```go
type SyncMap struct {
  mu sync.RWMutex
  Map
}
```

##### Type Conversion/Coercion Table

|           |    int    |    uint   |    float   |    bool    |             char            |      string      |   bytes   | array |  map  |   error  | undefined |
|-----------|:---------:|:---------:|:----------:|:----------:|:---------------------------:|:----------------:|:---------:|:-----:|:-----:|:--------:|:---------:|
| int       |     -     | uint64(v) | float64(v) | !IsFalsy() |           rune(v)           |     _strconv_    |   **X**   | **X** | **X** | String() |   **X**   |
| uint      |  int64(v) |     -     | float64(v) | !IsFalsy() |           rune(v)           |     _strconv_    |   **X**   | **X** | **X** | String() |   **X**   |
| float     |  int64(v) | uint64(v) |      -     | !IsFalsy() |           rune(v)           |     _strconv_    |   **X**   | **X** | **X** | String() |   **X**   |
| bool      |   1 / 0   |   1 / 0   |  1.0 / 0.0 |      -     |            1 / 0            | "true" / "false" |   **X**   | **X** | **X** | String() |   **X**   |
| char      |  int64(v) | uint64(v) | float64(v) | !IsFalsy() |              -              |     string(v)    |   **X**   | **X** | **X** | String() |   **X**   |
| string    | _strconv_ | _strconv_ |  _strconv_ | !IsFalsy() | utf8. DecodeRuneInString(v) |         -        | []byte(v) | **X** | **X** | String() |   **X**   |
| bytes     |   **X**   |   **X**   |    **X**   | !IsFalsy() |            **X**            |     string(v)    |     -     | **X** | **X** | String() |   **X**   |
| array     |   **X**   |   **X**   |    **X**   | !IsFalsy() |            **X**            |     String()     |   **X**   |   -   | **X** | String() |   **X**   |
| map       |   **X**   |   **X**   |    **X**   | !IsFalsy() |            **X**            |     String()     |   **X**   | **X** |   -   | String() |   **X**   |
| error     |   **X**   |   **X**   |    **X**   |    **X**   |            **X**            |     String()     |   **X**   | **X** | **X** |     -    |   **X**   |
| undefined |   **X**   |   **X**   |    **X**   | !IsFalsy() |            **X**            |     String()     |   **X**   | **X** | **X** |   **X**  |     -     |

- **X**: No conversion. Conversion function will throw runtime error, TypeError.
- strconv: converted using Go's conversion functions from `strconv` package.
- IsFalsy(): use [Object.IsFalsy()](#objectisfalsy) function.
- String(): use `Object.String()` function.

##### Object.IsFalsy()

`Object.IsFalsy()` interface method is used to determine if a given value
should evaluate to `false` (e.g. for condition expression of `if` statement).

- **int**: `v == 0`
- **uint**: `v == 0`
- **float**: `math.IsNaN(v)`
- **bool**: `!v`
- **char**: `v == 0`
- **string**: `len(v) == 0`
- **bytes**: `len(v) == 0`
- **array**: `len(v) == 0`
- **map**: `len(v) == 0`
- **error**: `true` _(error is always falsy)_
- **undefined**: `true` _(undefined is always falsy)_


#### Builtin Errors

Builtin errors do not have message but have name. With `.New(message)` function call on an error value creates a new error by wrapping the error.

* WrongNumArgumentsError
* InvalidOperatorError
* IndexOutOfBoundsError
* NotIterableError
* NotIndexableError
* NotIndexAssignableError
* NotCallableError
* NotImplementedError
* ZeroDivisionError
* TypeError

##### Undefined Values

In Charlang, an `undefined` value can be used to represent an unexpected or non-existing value:

* A function that does not return a value explicitly considered to return `undefined` value.
* Indexer or selector on composite value types may return `undefined` if the key or index does not exist.  
* Builtin functions may return `undefined`.

```go
a := func() { b := 4 }()    // a == undefined
c := {a: "foo"}["b"]        // c == undefined
d := sort(undefined)        // d == undefined
e := delete({}, "foo")      // "delete" always returns undefined
```

Builtin function `isUndefined`, `isUndef` or `==` operator can be used to check value is undefined.

##### Array Values

In Charlang, array is an ordered list of values of any types. Elements of an array can be accessed using indexer `[]`.

```go
[1, 2, 3][0]       // == 1
[1, 2, 3][2]       // == 3
[1, 2, 3][3]       // RuntimeError: IndexOutOfBoundsError

["foo", 'x', [1, 2, 3], {bar: 2u}, true, undefined, bytes()]   // ok
```

##### Map Values

In Charlang, map is a set of key-value pairs where key is string and the value is of any value types. Value of a map can be accessed using indexer `[]` or selector '.' operators.

```go
m := { a: 1, "b": false, c: "foo" }
m["b"]                                // == false
m.c                                   // == "foo"
m.x                                   // == undefined

{a: [1, 2, 3], b: {c: "foo", d: "bar"}} // ok
```  

##### Function Values

In Charlang, function is a callable value with a number of function arguments and a return value. Just like any other values, functions can be passed into or returned from another function.

```go
sum := func(arg1, arg2) {
  return arg1 + arg2
}

var mul = func(arg1, arg2) {
  return arg1 * arg2
}

adder := func(base) {
  return func(x) { return base + x }  // capturing 'base'
}

add5 := adder(5)
nine := add5(4)    // == 9
```

Unlike Go, Charlang does not have function declarations. All functions are anonymous functions. So the following code is illegal:

```go
func foo(arg1, arg2) {  // illegal
  return arg1 + arg2
}
```

Charlang also supports variadic functions:

```go
variadic := func (a, b, ...c) {
  return [a, b, c]
}
variadic(1, 2, 3, 4) // [1, 2, [3, 4]]

variadicClosure := func(a) {
  return func(b, ...c) {
    return [a, b, c]
  }
}
variadicClosure(1)(2, 3, 4) // [1, 2, [3, 4]]
```

Only the last parameter can be variadic. The following code is illegal:

```go
// illegal, because "a" is variadic and is not the last parameter
illegal := func(...a, b) {}
```

When calling a function, the number of passing arguments must match that of function definition.

```go
f := func(a, b) {}
f(1, 2, 3)    // RuntimeError: WrongNumArgumentsError
```

Like Go, you can use ellipsis `...` to pass value of array type as its last
parameter:

```go
f1 := func(a, b, c) { return a + b + c }
f1(...[1, 2, 3])    // => 6
f1(1, ...[2, 3])    // => 6
f1(1, 2, ...[3])    // => 6
f1(...[1, 2])       // RuntimeError: WrongNumArgumentsError

f2 := func(a, ...b) {}
f2(1)               // valid; a == 1, b == []
f2(1, 2)            // valid; a == 1, b == [2]
f2(1, 2, 3)         // valid; a == 1, b == [2, 3]
f2(...[1, 2, 3])    // valid; a == 1, b == [2, 3]
```

#### Type Conversions

Although the type is not directly specified in Charlang, one can use type conversion builtin functions to convert between value types.

```go
s1 := string(1984)    // "1984"
i2 := int("-999")     // -999
f3 := float(-51)      // -51.0
b4 := bool(1)         // true
c5 := char("X")       // 'X'
```

#### Order of evaluation

Expressions are evaluated from left to right but in assignments, right hand sideof the assignment is evaluated before left hand side.

```go
a := 1
f := func() {
  a*=10
  return a
}
g := func() {
  a++
  return a
}
h := func() {
  a+=2
  return a
}
d := {}
d[f()] = [g(), h()]
return d    // d == {"40": [2, 4]}
```

#### Operators

##### Unary Operators

| Operator | Operation               | Types(Results)                                            |
|:--------:|:-----------------------:|:---------------------------------------------------------:|
| `+`      | `0 + x`                 | int(int), uint(uint), char(char), float(float), bool(int) |
| `-`      | `0 - x`                 | int(int), uint(uint), char(int), float(float), bool(int)  |
| `^`      | bitwise complement `^x` | int(int), uint(uint), char(char), bool(int)               |
| `!`      | logical NOT             | all types*                                                |

_* In Charlang, all values can be either truthy or falsy._

##### Binary Operators

| Operator | Usage                    |
|:--------:|:------------------------:|
| `==`     | equal                    |
| `!=`     | not equal                |
| `&&`     | logical AND              |
| `\|\|`   | logical OR               |
| `+`      | add/concat               |
| `-`      | subtract                 |
| `*`      | multiply                 |
| `/`      | divide                   |
| `&`      | bitwise AND              |
| `\|`     | bitwise OR               |
| `^`      | bitwise XOR              |
| `&^`     | bitclear (AND NOT)       |
| `<<`     | shift left               |
| `>>`     | shift right              |
| `<`      | less than                |
| `<=`     | less than or equal to    |
| `>`      | greater than             |
| `>=`     | greater than or equal to |

##### Ternary Operators

Charlang has a ternary conditional operator `(condition expression) ? (true expression) : (false expression)`.

```go
a := true ? 1 : -1    // a == 1

min := func(a, b) {
  return a < b ? a : b
}
b := min(5, 10)      // b == 5
```

##### Assignment and Increment Operators

| Operator | Usage                     |
|:--------:|:-------------------------:|
| `+=`     | `(lhs) = (lhs) + (rhs)`   |
| `-=`     | `(lhs) = (lhs) - (rhs)`   |
| `*=`     | `(lhs) = (lhs) * (rhs)`   |
| `/=`     | `(lhs) = (lhs) / (rhs)`   |
| `%=`     | `(lhs) = (lhs) % (rhs)`   |
| `&=`     | `(lhs) = (lhs) & (rhs)`   |
| `\|=`    | `(lhs) = (lhs) \| (rhs)`  |
| `&^=`    | `(lhs) = (lhs) &^ (rhs)`  |
| `^=`     | `(lhs) = (lhs) ^ (rhs)`   |
| `<<=`    | `(lhs) = (lhs) << (rhs)`  |
| `>>=`    | `(lhs) = (lhs) >> (rhs)`  |
| `++`     | `(lhs) = (lhs) + 1`       |
| `--`     | `(lhs) = (lhs) - 1`       |

##### Operator Precedences

Unary operators have the highest precedence, and, ternary operator has the lowest precedence. There are five precedence levels for binary operators. Multiplication operators bind strongest, followed by addition operators, comparison operators, `&&` (logical AND), and finally `||` (logical OR):

| Precedence | Operator                             |
|:----------:|:------------------------------------:|
| 5          | `*`  `/`  `%`  `<<`  `>>`  `&`  `&^` |
| 4          | `+`  `-`  `\|`  `^`                  |
| 3          | `==`  `!=`  `<`  `<=`  `>`  `>=`     |
| 2          | `&&`                                 |
| 1          | `\|\|`                               |

Like Go, `++` and `--` operators form statements, not expressions, they fall outside the operator hierarchy.

##### Selector and Indexer

One can use selector (`.`) and indexer (`[]`) operators to read or write elements of composite types (array, map, string, bytes).

```go
["one", "two", "three"][1]  // == "two"

bytes(0, 1, 2, 3)[1]    // == 1

// Like Go, indexing string returns byte value of index as int value.
"foobarbaz"[4]    // == 97

m := {
  a: 1,
  b: [2, 3, 4],
  c: func() { return 10 }
}
m.a              // == 1
m["b"][1]        // == 3
m.c()            // == 10
m.x.y.z          // == undefined
m.x.y.z = 1      // RuntimeError: NotIndexAssignableError
m.x = 5          // add 'x' to map 'm'
```

Like Go, one can use slice operator `[:]` for sequence value types such as array, string, bytes. Negative indexes are illegal.

```go
a := [1, 2, 3, 4, 5][1:3]    // == [2, 3]
b := [1, 2, 3, 4, 5][3:]     // == [4, 5]
c := [1, 2, 3, 4, 5][:3]     // == [1, 2, 3]
d := "hello world"[2:10]     // == "llo worl"
e := [1, 2, 3, 4, 5][:]      // == [1, 2, 3, 4, 5]
f := [1, 2, 3, 4, 5][-1:]    // RuntimeError: InvalidIndexError
g := [1, 2, 3, 4, 5][10:]    // RuntimeError: IndexOutOfBoundsError
```

**Note: Keywords cannot be used as selectors.**

```go
a := {}
a.func = ""     // Parse Error: expected selector, found 'func'
```

Use double quotes and indexer to use keywords with maps.

```go
a := {}
a["func"] = ""
```

#### Statements

##### If Statement

"If" statement is very similar to Go.

```go
if a < 0 {
  // execute if 'a' is negative
} else if a == 0 {
  // execute if 'a' is zero
} else {
  // execute if 'a' is positive
}
```

Like Go, the condition expression may be preceded by a simple statement, which executes before the expression is evaluated.

```go
if a := foo(); a < 0 {
  // execute if 'a' is negative
}
```

##### For Statement

"For" statement is very similar to Go.

```go
// for (init); (condition); (post) {}
for a:=0; a<10; a++ {
  // ...
}

// for (condition) {}
for a < 10 {
  // ...
}

// for {}
for {
  // ...
}
```

##### For-In Statement

It's similar to Go's `for range` statement. "For-In" statement can iterate any iterable value types (array, map, bytes,
string).  

```go
for v in [1, 2, 3] {          // array: element
  // 'v' is array element value
}
for i, v in [1, 2, 3] {       // array: index and element
  // 'i' is index
  // 'v' is array element value
}
for k, v in {k1: 1, k2: 2} {  // map: key and value
  // 'k' is key
  // 'v' is map element value
}
for i, v in "foo" {           // array: index and element
  // 'i' is index
  // 'v' is char
}
```

#### Modules

Module is the basic compilation unit in Charlang. A module can import another module using `import` expression. There 3 types of modules. Source modules, builtin modules and custom modules. Source module is in the form Charlang code. Builtin module type is `map[string]Object`. Lastly, any value implementing Go `Importable` interface can be a module. `Import` method must return a valid Charlang Object or `[]byte`. Source module is called like a compiled function and returned value is stored for future use. Other module values are copied while importing in VM if `Copier` interface is implemented.

```go
type Importable interface {
  Import(moduleName string) (interface{}, error)
}
```

```go
type Copier interface {
  Copy() Object
}
```

Main module:

```go
sum := import("sum")    // load a module
println(sum(10))        // module function
```

Source module as `sum`:

```go
base := 5

return func(x) {
  return x + base
}
```

In Charlang, modules are very similar to functions.

* `import` expression loads the module code and execute it like a function.
* Module should return a value using `return` statement.
  * Module can return a value of any types: int, map, function, etc.
  * `return` in a module stops execution and return a value to the importing
    code.
  * If the module does not have any `return` statement, `import` expression
  simply returns `undefined`. _(Just like the function that has no `return`.)_  
* importing same module multiple times at different places or in different
  modules returns the same object so it preserves the state of imported object.
* Arguments cannot be provided to source modules while importing although it is
  allowed to use `param` statement in module.
* Modules can use `global` statements to access globally shared object.

#### Differences from Go

Unlike Go, Charlang does not have the following:

* Imaginary values
* Structs
* Pointers
* Channels
* Goroutines
* Tuple assignment (Charlang supports [destructuring](destructuring.md) array)
* Switch statement
* Goto statement
* Defer statement
* Panic and recover
* Type assertion

#### Interfaces

Charlang types implement `Object` interface. Any Go type implementing `Object` interface can be provided to Charlang VM.

##### Object interface

```go

// Object represents an object in the VM.
type Object interface {
  // TypeName should return the name of the type.
  TypeName() string

  // String should return a string of the type's value.
  String() string

  // BinaryOp handles +,-,*,/,%,<<,>>,<=,>=,<,> operators.
  // Returned error stops VM execution if not handled with an error handler
  // and VM.Run returns the same error as wrapped.
  BinaryOp(tok token.Token, right Object) (Object, error)

  // IsFalsy returns true if value is falsy otherwise false.
  IsFalsy() bool

  // Equal checks equality of objects.
  Equal(right Object) bool

  // Call is called from VM if CanCall() returns true. Check the number of
  // arguments provided and their types in the method. Returned error stops VM
  // execution if not handled with an error handler and VM.Run returns the
  // same error as wrapped.
  Call(args ...Object) (Object, error)

  // CanCall returns true if type can be called with Call() method.
  // VM returns an error if one tries to call a noncallable object.
  CanCall() bool

  // Iterate should return an Iterator for the type.
  Iterate() Iterator

  // CanIterate should return whether the Object can be Iterated.
  CanIterate() bool

  // IndexGet should take an index Object and return a result Object or an
  // error for indexable objects. Indexable is an object that can take an
  // index and return an object. Returned error stops VM execution if not
  // handled with an error handler and VM.Run returns the same error as
  // wrapped. If Object is not indexable, ErrNotIndexable should be returned
  // as error.
  IndexGet(index Object) (value Object, err error)

  // IndexSet should take an index Object and a value Object for index
  // assignable objects. Index assignable is an object that can take an index
  // and a value on the left-hand side of the assignment statement. If Object
  // is not index assignable, ErrNotIndexAssignable should be returned as
  // error. Returned error stops VM execution if not handled with an error
  // handler and VM.Run returns the same error as wrapped.
  IndexSet(index, value Object) error
}
```

##### Iterator interface

If an object's `CanIterate` method returns `true`, its `Iterate` method must return a value implementing `Iterator` interface to use in `for-in` loops.

```go
// Iterator wraps the methods required to iterate Objects in VM.
type Iterator interface {
  // Next returns true if there are more elements to iterate.
  Next() bool

  // Key returns the key or index value of the current element.
  Key() Object

  // Value returns the value of the current element.
  Value() Object
}
```

##### Copier interface

Assignments to Charlang values copy the values except array, map or bytes like Go. `copy` builtin function returns the copy of a value if Copier interface is implemented by object. If not implemented, same object is returned which copies the value under the hood by Go.

```go
// Copier wraps the Copy method to create a deep copy of an object.
type Copier interface {
  Copy() Object
}
```

##### IndexDeleter interface

`delete` builtin checks if the given object implements `IndexDeleter` interface to delete an element from the object. `map` and `syncMap` implement this interface.

```go
// IndexDeleter wraps the IndexDelete method to delete an index of an object.
type IndexDeleter interface {
    IndexDelete(Object) error
}
```

##### LengthGetter interface

`len` builtin checks if the given object implements `IndexDeleter` interface to get the length of an object. `array`, `bytes`, `string`, `map` and `syncMap` implement this interface.

```go
// LengthGetter wraps the Len method to get the number of elements of an object.
type LengthGetter interface {
    Len() int
}
```

##### Object Interface Extensions

Note that `ExCallerObject` will replace the existing Object interface in the uture.

```go
// ExCallerObject is an interface for objects that can be called with CallEx
// method. It is an extended version of the Call method that can be used to
// call an object with a Call struct. Objects implementing this interface is
// called with CallEx method instead of Call method.
// Note that CanCall() should return true for objects implementing this
// interface.
type ExCallerObject interface {
    Object
    CallEx(c Call) (Object, error)
}

// NameCallerObject is an interface for objects that can be called with CallName
// method to call a method of an object. Objects implementing this interface can
// reduce allocations by not creating a callable object for each method call.
type NameCallerObject interface {
    Object
    CallName(name string, c Call) (Object, error)
}
```

