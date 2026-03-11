# Charlang Quick Start Guide

## Table of Contents

- [1. Introduction](#1-introduction)
- [2. Installation and Running](#2-installation-and-running)
- [3. First Program](#3-first-program)
- [4. Basic Syntax](#4-basic-syntax)
- [5. Data Types](#5-data-types)
- [6. Operators](#6-operators)
- [7. Control Flow](#7-control-flow)
- [8. Functions](#8-functions)
- [9. Arrays](#9-arrays)
- [10. Maps](#10-maps)
- [11. Error Handling](#11-error-handling)
- [12. Common Built-in Functions](#12-common-built-in-functions)
- [13. File Operations](#13-file-operations)
- [14. Command Line Arguments](#14-command-line-arguments)
- [15. Modules and Imports](#15-modules-and-imports)
- [16. Concurrent Programming](#16-concurrent-programming)
- [17. Dynamic Code Execution](#17-dynamic-code-execution)
- [18. Microservice Development](#18-microservice-development)
- [19. Next Steps](#19-next-steps)

---

## 1. Introduction

Charlang is a dynamic scripting language written in Go, with syntax similar to Go but without strong type restrictions, featuring a more flexible type system and error handling.

**Features:**
- Single file execution, no compilation needed
- Pure Go implementation, no CGO dependencies
- Cross-platform (Windows, Linux, Mac)
- Rich built-in functions
- Dynamic typing
- Support for concurrent programming
- Built-in microservice/Web server

---

## 2. Installation and Running

### Download and Install

Download `char.exe` (Windows) or `char` (Linux/Mac) from [GitHub](https://github.com/topxeq/charlang) or the [official website](https://topget.org/charlang).

### Running Modes

```bash
# REPL interactive mode
char

# Run script
char hello.char

# Run with arguments
char script.char arg1 arg2

# Start web server
char -server -port=:8080 -dir=.
```

---

## 3. First Program

Create file `hello.char`:

```charlang
pln("Hello, World!")
```

Run:

```bash
char hello.char
```

Output: `Hello, World!`

---

## 4. Basic Syntax

### Comments

```charlang
// Single line comment

/*
 * Multi-line comment
 */
```

### Variable Declaration

```charlang
// Method 1: Using := (recommended)
name := "John"
age := 25
isStudent := true

// Method 2: Using var + assignment
var x
x = 10

// Method 3: Using var with initial value
var y = 20
```

### Constants

```charlang
const PI = 3.14159
const (
    STATUS_OK = 200
    STATUS_ERR = 500
)

// iota for enumerations
const (
    RED = iota   // 0
    GREEN         // 1
    BLUE          // 2
)
```

### Scope

```charlang
a := "global"

func() {
    a := "local"  // creates new variable, shadows outer
    pln(a)        // prints: local
}()

pln(a)  // prints: global
```

---

## 5. Data Types

### Basic Types

```charlang
// Integer
n := 42           // int
u := 100u        // uint

// Float
f := 3.14        // float

// String
s := "Hello"     // string

// Boolean
b := true        // bool

// Character
c := 'A'         // char (actually int)

// Undefined
var u1           // undefined
```

### Arrays

```charlang
arr := [1, 2, 3, 4, 5]
arr2 := []int{}           // empty array
arr3 := []string{"a", "b"}
```

### Maps

```charlang
m := {"name": "John", "age": 25}
m2 := map[string]int{}
m2["key"] = 100
```

### Type Checking

```charlang
pln(typeName(123))        // "int"
pln(typeName("hello"))   // "string"
pln(typeName([1,2,3]))   // "array"
pln(typeName({"a":1}))   // "map"

// Type check functions
isInt(123)      // true
isString("x")  // true
isArray([1])   // true
isMap({})      // true
isUndefined(u) // true (for unassigned variables)
```

---

## 6. Operators

### Arithmetic Operators

```charlang
a := 10 + 5    // addition
b := 10 - 5    // subtraction
c := 10 * 5    // multiplication
d := 10 / 5    // division
e := 10 % 3    // modulo

// Compound assignment
x := 10
x += 5         // x = 15
x++            // x = 16
x--            // x = 15
```

### Comparison Operators

```charlang
a := 5 == 5    // true
b := 5 != 3    // true
c := 5 > 3     // true
d := 5 < 3     // false
e := 5 >= 5    // true
f := 5 <= 5    // true
```

### Logical Operators

```charlang
a := true && false   // false
b := true || false   // true
c := !true           // false
```

### Bitwise Operators

```charlang
a := 0xFF & 0x0F    // 0x0F (AND)
b := 0xF0 | 0x0F    // 0xFF (OR)
c := 0xFF ^ 0x0F    // 0xF0 (XOR)
d := 1 << 4          // 16 (left shift)
e := 16 >> 2         // 4 (right shift)
f := 0xFF &^ 0x0F   // 0xF0 (AND NOT)
```

### Ternary Operator

```charlang
result := age >= 18 ? "adult" : "minor"
```

---

## 7. Control Flow

### if-else

```charlang
score := 85

if score >= 90 {
    pln("Excellent")
} else if score >= 60 {
    pln("Passed")
} else {
    pln("Failed")
}
```

### for Loop

```charlang
// Classic for loop
for i := 0; i < 5; i++ {
    pln(i)
}

// Condition only
i := 0
for i < 5 {
    pln(i)
    i++
}

// Infinite loop
for {
    if condition {
        break
    }
}

// for-in over array
arr := ["a", "b", "c"]
for i, v in arr {
    pln(i, v)  // 0 a, 1 b, 2 c
}

// for-in over range
for i in 5 {  // 0,1,2,3,4
    pln(i)
}
```

### break and continue

```charlang
for i := 0; i < 10; i++ {
    if i == 3 {
        continue  // skip i=3
    }
    if i == 7 {
        break     // exit loop
    }
    pln(i)
}
```

---

## 8. Functions

### Basic Function

```charlang
// Define function
add := func(a, b) {
    return a + b
}

// Call
result := add(10, 20)  // 30
```

### Multiple Return Values

```charlang
divide := func(a, b) {
    if b == 0 {
        return 0, "division by zero"
    }
    return a / b, ""
}

result, err := divide(10, 2)
if err != "" {
    pln("Error:", err)
} else {
    pln("Result:", result)
}
```

### Variadic Parameters

```charlang
sum := func(...args) {
    total := 0
    for v in args {
        total += v
    }
    return total
}

pln(sum(1, 2, 3, 4, 5))  // 15
```

### Closures

```charlang
counter := func() {
    count := 0
    return func() {
        count++
        return count
    }
}

c := counter()
pln(c())  // 1
pln(c())  // 2
pln(c())  // 3
```

### Recursion

```charlang
factorial := func(n) {
    if n <= 1 {
        return 1
    }
    return n * factorial(n - 1)
}

pln(factorial(5))  // 120
```

---

## 9. Arrays

### Creation and Access

```charlang
arr := [1, 2, 3, 4, 5]

// Access element
pln(arr[0])    // 1
pln(arr[len(arr)-1])  // 5

// Modify element
arr[0] = 100
```

### Common Operations

```charlang
arr := [1, 2, 3]

// Append elements
arr = append(arr, 4, 5)

// Slice
slice := arr[1:3]  // [2, 3]

// Length
pln(len(arr))  // 5

// Contains check
has3 := contains(arr, 3)  // true

// Iterate
for i, v in arr {
    pln(i, v)
}
```

### Sorting

```charlang
arr := [3, 1, 4, 1, 5]
sort(arr)           // ascending
sortReverse(arr)    // descending
```

---

## 10. Maps

### Creation and Access

```charlang
m := {"name": "John", "age": 25}

// Access
pln(m["name"])    // John

// Add/modify
m["age"] = 26
m["city"] = "Beijing"

// Delete
delete(m, "city")

// Iterate
for k, v in m {
    pln(k, v)
}

// Get all keys
keys := m.keys()

// Get all values
values := m.values()
```

---

## 11. Error Handling

### try-catch-finally

```charlang
try {
    result := riskyOperation()
    pln("Success:", result)
} catch err {
    pln("Caught error:", err)
} finally {
    pln("Always executes")
}
```

### throw and error

```charlang
try {
    if invalid {
        throw("Something went wrong")
    }
} catch err {
    pln("Error:", err)
}

// Create error value
err := error("custom error message")
if isError(err) {
    pln("Is error")
}
```

---

## 12. Common Built-in Functions

### Output Functions

```charlang
pln("Hello")           // print with newline
pr("Hello")            // print without newline
pl("Value: %v", 123)   // formatted print
plt(x)                 // print with type
```

### Type Conversion

```charlang
int("42")              // string to int
float("3.14")          // string to float
string(123)            // to string
bool(1)                // to bool
char(65)               // int to char 'A'
uint(42)               // to unsigned int
bytes("ABC")           // string to byte array
chars("ABC")           // string to rune array
```

### String Functions

```charlang
s := "Hello World"

len(s)                   // length
strContains(s, "Hello")  // contains substring
strStartsWith(s, "Hello") // starts with
strEndsWith(s, "World")  // ends with
strIndex(s, "o")         // find position
strSplit(s, " ")         // split
strReplace(s, "World", "Charlang")  // replace
strToUpper(s)            // to uppercase
strToLower(s)            // to lowercase
strTrim(s)               // trim whitespace
strRepeat(s, 3)          // repeat
strJoin(["a","b"], "-")  // join
```

### Encoding Functions

```charlang
md5("hello")            // MD5 hash
sha256("hello")         // SHA256 hash
base64Encode("abc")     // Base64 encode
base64Decode("YWJj")    // Base64 decode
urlEncode("a b")        // URL encode
urlDecode("a%20b")     // URL decode
```

### Time Functions

```charlang
now()                   // current time
getNowStr()            // current time string
getNowTimeStamp()      // timestamp (seconds)
timeAddSecs(now(), 60) // add seconds
formatTime(now(), "2006-01-02 15:04:05")  // format
strToTime("2024-01-01") // string to time
```

---

## 13. File Operations

**Note: `saveText` parameter order is (content, path)**

```charlang
// Read file
content := loadText("file.txt")

// Write file (content first, path second!)
saveText("Hello World", "output.txt")

// Check file exists
if fileExists("file.txt") {
    pln("File exists")
}
```

---

## 14. Command Line Arguments

```charlang
global argsG

pln("Argument count:", len(argsG))

for i, v in argsG {
    pln(i, v)
}

// Safe parameter access
name := getParam(argsG, 1, "default")

// Get switch parameter
port := getSwitch(argsG, "-port=", "8080")
hasVerbose := ifSwitchExists(argsG, "-v")
```

---

## 15. Modules and Imports

### Import Built-in Modules

```charlang
ex := import("ex")

// Use module functions
compiled := ex.compile(sourceCode)
result := ex.runCompiled(compiled, args...)

// Thread execution
ex.threadRunFunc(myFunc, args)
```

---

## 16. Concurrent Programming

### Thread Basics

```charlang
// Define function to run in thread
worker := func(data) {
    for i := 0; i < 5; i++ {
        pln("Worker:", i)
        sleep(0.5)
    }
}

// Start thread
worker.threadRun(data)

// Main thread continues
pln("Main thread continues")
```

### Mutex (Thread Safety)

```charlang
// Create mutex
m := mutex()

// Create shared counter
counter := new("int", 0)

// Thread function
worker := charCode(`
    param (counter, mutex, iterations)
    for i := 0; i < iterations; i++ {
        lock(mutex)
        setValueByRef(counter, unref(counter) + 1)
        unlock(mutex)
    }
`).compile()

// Start multiple threads
worker.threadRun(counter, m, 100)
worker.threadRun(counter, m, 100)

// Wait for completion
sleep(2)

pln("Final count:", unref(counter))  // 200
```

---

## 17. Dynamic Code Execution

### charCode and compile

```charlang
// Create code from string
code := charCode(`
    param (a, b)
    return a + b
`).compile()

// Execute code
result := code.run(10, 20)  // 30
```

### Reference Types

```charlang
// Create reference
n := new("int", 42)

// Get value
val := unref(n)

// Set value
setValueByRef(n, 100)
```

---

## 18. Microservice Development

### Start Server

```bash
char -server -port=:8080 -dir=.
```

Access: `http://127.0.0.1:8080/charms/your-service-name`

### Microservice Template

Create file `myservice.char`:

```charlang
// Declare global variables
global requestG
global responseG
global paraMapG
global reqUriG

// Set response end marker
outG := "TX_END_RESPONSE_XT"

// Set response header
setRespHeader(responseG, "Content-Type", "application/json; charset=utf-8")

// Get parameters
text := trim(paraMapG["text"])

// Business logic
if text == "" {
    writeResp(responseG, genResp("fail", "missing text parameter", requestG))
    return outG
}

result := md5(text)

// Return success response
writeResp(responseG, genResp("success", result, requestG))

return outG
```

### Response Functions

```charlang
// Generate JSON response
genResp(status, value, request)

// Write response
writeResp(responseG, content)

// Set response header
setRespHeader(responseG, "Content-Type", "text/html; charset=utf-8")
```

---

## 19. Next Steps

- Read "Charlang Reference" for complete function list
- Check test examples in `genscripts/` directory
- Explore more examples in `reference/scripts/`
- Try writing your own microservices

**Resources:**
- Official Website: https://topget.org/charlang
- GitHub: https://github.com/topxeq/charlang
- Built-in Functions: https://topget.org/dc/charlang/funcs
