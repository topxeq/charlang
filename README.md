<!-- |title: The Char Language (Charlang)| -->

- [The Char Language (Charlang)](#the-char-language-charlang)
  - [1. Features](#1-features)
  - [2. New Features](#2-new-features)
  - [3. Quick Links](#3-quick-links)
  - [4. Quick Start](#4-quick-start)
  - [5. Documentation](#5-documentation)
    - [5.1 Get the Binary](#51-get-the-binary)
    - [5.2 Compile from Source Code](#52-compile-from-source-code)
    - [5.3 Start Running the Shell or Scripts](#53-start-running-the-shell-or-scripts)
    - [5.4 Various Ways to Run Charlang Scripts](#54-various-ways-to-run-charlang-scripts)
    - [5.5 Get the Examples](#55-get-the-examples)
    - [5.6 Quick Tour](#56-quick-tour)
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
      - [For Loop](#for-loop)
      - [If Statement](#if-statement)
      - [Predefined Global Variables](#predefined-global-variables)
      - [Run Charlang Script/Code](#run-charlang-scriptcode)
      - [Charlang as System Service](#charlang-as-system-service)
    - [5.7 More Examples](#57-more-examples)
      - [Anonymous Function](#anonymous-function)
      - [More About Array](#more-about-array)
      - [More About Map](#more-about-map)
      - [A Simple Text Editor](#a-simple-text-editor)
      - [Base64 Encoding of Images](#base64-encoding-of-images)
      - [Plot Data Graph in Console](#plot-data-graph-in-console)
      - [Plot Data Graph in Console with Realtime Data Update](#plot-data-graph-in-console-with-realtime-data-update)
    - [5.8 Advance Topics](#58-advance-topics)
      - [Language Considerations](#language-considerations)
      - [Run Script from Command Line](#run-script-from-command-line)
      - [Show Environment Information of Charlang](#show-environment-information-of-charlang)
      - [Charlang as An Embedded Language in Golang](#charlang-as-an-embedded-language-in-golang)

# The Char Language (Charlang)

[Charlang](http://topget.org/charlang) is a fast, dynamic scripting language to embed in Go applications.
Charlang is compiled and executed as bytecode on stack-based VM that's written
in native Go. Charlang has a more-common runtime error handling(try-catch-finally) than Golang.

Charlang is inspired by and based on awesome script language [uGo](https://github.com/ozanh/ugo). A special thanks to uGo's creater([ozanh](https://github.com/ozanh)) and contributors.

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

## 3. Quick Links

[Charlang Home](http://topget.org/charlang)

[Go Reference](https://pkg.go.dev/github.com/topxeq/charlang)

[Builtin Functions](http://topget.org/dc/charlang/funcs)

## 4. Quick Start

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

## 5. Documentation

### 5.1 Get the Binary

Download the binary release files according to your OS from the website: [Charlang Homepage](http://topget.org/charlang).

### 5.2 Compile from Source Code

```shell
go get -u github.com/topxeq/charlang
```

### 5.3 Start Running the Shell or Scripts

After download, extract the executable from the zip file, put it into a directory, better in the system path.

Then type 'char' in the terminal/console to start the interactive command-line shell interface. Also you can run some scripts using command like 'char test.char', or 'char -example basic.char'. Here 'test.char' is the source code file(in UTF-8 encoding, plain text format).

Using command-line switch '-view' will show the source code of the script instead of run it.

### 5.4 Various Ways to Run Charlang Scripts

Examples:

- Run from a source file: `char d:\scripts\test.char`
- Run the text in clipboard as script source: `char -clip`
- Run from the remote server: `char -remote http://replacewithyourdomain.com/script/abc.char`
- Run the example code: `char -example basic.char`
- Run from Golang source directory: `char -gopath basic.char`
- Run from local scripts directory: place a config file local.cfg in the subdirectory 'char' under the user's home directory, with text content such as `c:\scripts`, then `char -local basic.char` will run 'c:\script\basic.char'
- Run from cloud/network: place a config file cloud.cfg in the subdirectory 'char' under the user's home directory, with text content such as `http://script.my.com/`, then `char -cloud basic.char` will be the same as `char -remote http://script.my.com/basic.char`
- Select the script(or input the script file path in console mode) to run: `char -selectScript`

### 5.5 Get the Examples

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

### 5.6 Quick Tour

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

#### Charlang as System Service

Charlang can be started as a system service and supports operating systems such as Windows and Linux. As long as you add the command line parameter '-reinstallService' to run the Charlang main program, you can install a system service called charService in the system (which can be seen using the service management module in computer management under Windows). Note that installing services in the operating system generally requires administrator privileges. Under Windows, you need to open the CMD window as an administrator to execute this command, while under Linux, you need to execute it as root or with the sudo command.

After the service is started, a log will be recorded in the file charService.log in the service root directory (c:\char in Windows and /char in Linux). When the service starts for the first time, it will search for all files with names similar to taskXXX.char in the service root directory (such as task001.char, taskAbc.char, etc.) and run them one by one, and output their execution results (returned value) to the log. This type of code file is called a one-time-run task file, and is generally used in situations where it needs to be started and run once. It can also be manually run the command 'char -restartService' to restart the service and achieve the goal of task re-execution.

There are another kind of one-time-run task files which will be run in a seperated thread called 'thread tasks', their file name similar to threadTaskXXX.char in the service root directory (such as threadTask001.char, threadTaskAbc.char, etc.). These tasks is for those tasks need running continously, such as WEB servers, FRP server or client, ... If some error occur while running thread-tasks, the information will be logged in file 'runThreadTask.log' in service root directory.

In addition, during operation, the charService service checks the service root directory every 5 seconds. If there are files with names similar to autoRemoveTaskXXX.char (such as autoRemoveTask001.char, autoRemoveTaskAbc.char, etc.), the code in these files will be immediately executed and then deleted. This mechanism is similar to a task queue, allowing us to add tasks to the queue (placed in the service root directory) at any time, and Charlang service will execute these tasks at any time. And since the task will be deleted immediately after execution, it will not be executed repeatedly.

The command line parameters related to service installation, removal, start, stop, and restart of the Charlang main program also include '-installService', '-removeService', '-startService', '-stopService', '-restartService', and so on.

The task code can refer to examples such as task001.char, threadTask001.char, autoRemoveTask001.char, etc.

- One-time-task Example

file: [task001.char](http://topget.org/dc/c/charlang/example/task001.char)

```go
global basePathG

logPathT := joinPath(basePathG, "charService.log")

rs := appendText("\ntask001.char\n", logPathT)

return "task001 returns some result 000"

```

- Thread-task Example

file: [threadTask001.char](http://topget.org/dc/c/charlang/example/threadTask001.char)

```go
for {
	saveText(getNowStr(), `c:\char\task1.txt`)

	sleep(60.0)
}
```

It will be a continous loop which will write the current time string in the file every 60 seconds.

The following is the thread-task to run Frpc client when system started,

file: [threadTaskFrpc.char](http://topget.org/dc/c/charlang/example/threadTaskFrpc.char)

```go
appendText(spr("\n[%v] %v %v\n", getNowStr(), "threadTaskFrpc", "starting..."), `c:\logs\frpcTask.log`)

rs := systemCmd(`c:\tools\frp\frpc.exe`, `-c`, `c:\tools\frp\frpc.ini`)

appendText(spr("\n[%v] %v %v\n", getNowStr(), "threadTaskFrpc", rs), `c:\logs\frpcTask.log`)


```

- Auto-remove-task Example

file: [autoRemoveTask001.char](http://topget.org/dc/c/charlang/example/autoRemoveTask001.char)

```go
global basePathG

logPathT := joinPath(basePathG, "charService.log")

rs := appendText("\nautoRemoveTask001.char\n", logPathT)

```


### 5.7 More Examples

#### Anonymous Function

file: [anonymousFunc.char](http://topget.org/dc/c/charlang/example/anonymousFunc.char)

#### More About Array

file: [array.char](http://topget.org/dc/c/charlang/example/array.char)

#### More About Map

file: [map.char](http://topget.org/dc/c/charlang/example/map.char)

#### A Simple Text Editor

file: [editFile.char](http://topget.org/dc/c/charlang/example/editFile.char)

A simple text editor with file load/save, JSON validate, code running features.

#### Base64 Encoding of Images

file: [base64EncodeImage.char](http://topget.org/dc/c/charlang/example/base64EncodeImage.char)

Encode an image file to Base64 code to use in img tag in HTML, for example,

```shell
D:\tmp>char -exam base64EncodeImage.char -file=d:\downtemp\curtain-2757815_960_720.png
data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAA8AAAAHECAYAAAD22EOkAAABeGlDQ1BJQ0MgUHJvZmlsZQAAeJx1kL9LAmEcxh+1sB+GQRINDTdIQyiEDTWWDUKIiBlktdyddxqcetydRDQ2tDq4VLRk0X9QW/QPBEFQTUHU3FAQQcj1nCcood/jve+H5/0+7733AN60JpfMgTmgVLaMTCIubOQ2Bf8rhjGOSYSwIMqmvpxOJ9G3vh/hcfpD1Dmr/1zPGs0rpgx4hsiLsm5Y5CVyatfSHa6RQ3JRzJPPyRGDFyTfO7rk8rvDBZd/HDaymRXAGyALhS6WulguGiVyhBwuaVW5fR/nTwJKeX2Nfbq1TGSQQBwCJFSxAw0WouxlZt...
```

#### Plot Data Graph in Console

file: [asciiPlot.char](http://topget.org/dc/c/charlang/example/asciiPlot.char)

![Snapshot](http://topget.org/dc/s/images/pic3873474424.png)

#### Plot Data Graph in Console with Realtime Data Update

file: [asciiPlotRealTime.char](http://topget.org/dc/c/charlang/example/asciiPlotRealTime.char)

Demonstrate multi-thread, update one area of console...

### 5.8 Advance Topics

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

#### Charlang as An Embedded Language in Golang

To run a script in Golang, it must be compiled to create a `Bytecode` object then it is
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



