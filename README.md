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
      - [For Loop](#for-loop)
      - [If Statement](#if-statement)
      - [Predefined Global Variables](#predefined-global-variables)
      - [Charlang as System Service](#charlang-as-system-service)
    - [5.7 More Examples](#57-more-examples)
      - [Anonymous Function](#anonymous-function)
      - [More About Array](#more-about-array)
      - [A Simple Text Editor](#a-simple-text-editor)

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
    pl("[%v] %v")
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

#### Charlang as System Service

Charlang can be started as a system service and supports operating systems such as Windows and Linux. As long as you add the command line parameter '-reinstallService' to run the Charlang main program, you can install a system service called charService in the system (which can be seen using the service management module in computer management under Windows). Note that installing services in the operating system generally requires administrator privileges. Under Windows, you need to open the CMD window as an administrator to execute this command, while under Linux, you need to execute it as root or with the sudo command.

After the service is started, a log will be recorded in the file charService.log in the service root directory (c:\char in Windows and /char in Linux). When the service starts for the first time, it will search for all files with names similar to taskXXX.char in the service root directory (such as task001.char, taskAbc.char, etc.) and run them one by one, and output their execution results (returned value) to the log. This type of code file is called a one-time-run task file, and is generally used in situations where it needs to be started and run once. It can also be manually run the command 'char -restartService' to restart the service and achieve the goal of task re-execution.

There are another kind of one-time-run task files which will be run in a seperated thread called 'thread tasks', their file name similar to threadTaskXXX.char in the service root directory (such as threadTask001.char, threadTaskAbc.char, etc.). These tasks is for those tasks need running continously, such as WEB servers, FRP server or client, ... If some error occur while running thread-tasks, the information will be logged in file 'runThreadTask.log' in service root directory.

In addition, during operation, the charService service checks the service root directory every 5 seconds. If there are files with names similar to autoRemoveTaskXXX.char (such as autoRemoveTask001.char, autoRemoveTaskAbc.char, etc.), the code in these files will be immediately executed and then deleted. This mechanism is similar to a task queue, allowing us to add tasks to the queue (placed in the service root directory) at any time, and Charlang service will execute these tasks at any time. And since the task will be deleted immediately after execution, it will not be executed repeatedly.

The command line parameters related to service installation, removal, start, stop, and restart of the Charlang main program also include '-installService', '-removeService', '-startService', '-stopService', '-restartService', and so on.

The task code can refer to examples such as task001.char, threadTask001.char, autoRemoveTask001.char, etc.

- One-time-task Example

```go
global basePathG

logPathT := joinPath(basePathG, "charService.log")

rs := appendText("\ntask001.char\n", logPathT)

return "task001 returns some result 000"

```

- Thread-task Example

```go
for {
	saveText(getNowStr(), `c:\char\task1.txt`)

	sleep(60.0)
}
```

It will be a continous loop which will write the current time string in the file every 60 seconds.

- Auto-remove-task Example

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

#### A Simple Text Editor

file: [editFile.char](http://topget.org/dc/c/charlang/example/editFile.char)

A simple text editor with file load/save, JSON validate, code running features.

