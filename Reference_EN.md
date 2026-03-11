# Charlang Reference

## Table of Contents

- [1. Introduction](#1-introduction)
- [2. Syntax](#2-syntax)
  - [2.1 Identifiers and Keywords](#21-identifiers-and-keywords)
  - [2.2 Variable Declaration](#22-variable-declaration)
  - [2.3 Constants and iota](#23-constants-and-iota)
  - [2.4 Scope](#24-scope)
- [3. Data Types](#3-data-types)
  - [3.1 Basic Types](#31-basic-types)
  - [3.2 Arrays](#32-arrays)
  - [3.3 Maps](#33-maps)
  - [3.4 Function Types](#34-function-types)
  - [3.5 Type Checking and Conversion](#35-type-checking-and-conversion)
- [4. Operators](#4-operators)
  - [4.1 Arithmetic Operators](#41-arithmetic-operators)
  - [4.2 Comparison Operators](#42-comparison-operators)
  - [4.3 Logical Operators](#43-logical-operators)
  - [4.4 Bitwise Operators](#44-bitwise-operators)
  - [4.5 Assignment Operators](#45-assignment-operators)
  - [4.6 Other Operators](#46-other-operators)
- [5. Control Flow](#5-control-flow)
  - [5.1 if Statement](#51-if-statement)
  - [5.2 for Statement](#52-for-statement)
  - [5.3 for-in Statement](#53-for-in-statement)
  - [5.4 break and continue](#54-break-and-continue)
- [6. Functions](#6-functions)
  - [6.1 Function Definition and Call](#61-function-definition-and-call)
  - [6.2 Parameters and Return Values](#62-parameters-and-return-values)
  - [6.3 Closures](#63-closures)
  - [6.4 Recursion](#64-recursion)
  - [6.5 Defer](#65-defer)
- [7. Error Handling](#7-error-handling)
  - [7.1 try-catch-finally](#71-try-catch-finally)
  - [7.2 throw Statement](#72-throw-statement)
  - [7.3 Error Values](#73-error-values)
- [8. Common Built-in Functions](#8-common-built-in-functions)
  - [8.1 Output Functions](#81-output-functions)
  - [8.2 String Functions](#82-string-functions)
  - [8.3 Array Functions](#83-array-functions)
  - [8.4 Map Functions](#84-map-functions)
  - [8.5 Type Conversion Functions](#85-type-conversion-functions)
  - [8.6 Encoding Functions](#86-encoding-functions)
  - [8.7 Math Functions](#87-math-functions)
  - [8.8 Time Functions](#88-time-functions)
  - [8.9 File Functions](#89-file-functions)
  - [8.10 Command Line Functions](#810-command-line-functions)
  - [8.11 Regex Functions](#811-regex-functions)
  - [8.12 JSON Functions](#812-json-functions)
  - [8.13 Other Functions](#813-other-functions)
- [9. Concurrent Programming](#9-concurrent-programming)
  - [9.1 Thread Basics](#91-thread-basics)
  - [9.2 Mutex](#92-mutex)
  - [9.3 Reference Types](#93-reference-types)
- [10. Dynamic Code Execution](#10-dynamic-code-execution)
  - [10.1 charCode Function](#101-charcode-function)
  - [10.2 compile](#102-compile)
  - [10.3 Code Reuse](#103-code-reuse)
- [11. Module System](#11-module-system)
  - [11.1 import](#111-import)
  - [11.2 ex Module](#112-ex-module)
- [12. Microservice Development](#12-microservice-development)
  - [12.1 Server Startup](#121-server-startup)
  - [12.2 Writing Microservices](#122-writing-microservices)
  - [12.3 Response Functions](#123-response-functions)
  - [12.4 Global Variables](#124-global-variables)
- [13. Command Line Reference](#13-command-line-reference)
  - [13.1 Common Options](#131-common-options)
  - [13.2 Server Mode](#132-server-mode)
- [14. Differences from Go](#14-differences-from-go)
- [15. Built-in Object Methods](#15-built-in-object-methods)
  - [15.1 Array Methods](#151-array-methods)
  - [15.2 Map Methods](#152-map-methods)
  - [15.3 String Methods](#153-string-methods)
  - [15.4 Time Methods](#154-time-methods)

---

# 1. Introduction

Charlang is a dynamic scripting language built on Go, designed to maintain Go's simplicity while providing more flexible type system and convenient error handling.

**Core Features:**

- **Dynamic Typing**: Variable types can change at runtime
- **Clean Syntax**: Removed some of Go's mandatory requirements
- **Error Handling**: Uses try-catch-finally instead of defer
- **Single File Execution**: No complex compilation needed
- **Built-in Server**: Web service/microservice development support
- **Concurrency**: Lightweight threads and mutexes
- **Rich Function Library**: Extensive built-in functions

**Official Resources:**
- Website: https://topget.org/charlang
- GitHub: https://github.com/topxeq/charlang

---

# 2. Syntax

## 2.1 Identifiers and Keywords

### Identifier Rules

- Must start with letter or underscore
- Can contain letters, numbers, and underscores
- Case sensitive

```charlang
name := "valid"
_name := "valid"
name123 := "valid"
_123 := "valid"
```

### Keywords

```
break     default     func     interface   select
case      defer       go       map         struct
chan      else        goto     package     switch
const     fallthrough if       range       type
continue  for         import   return      var
```

### Reserved Words

```
true    false    nil     iota    undefined
```

## 2.2 Variable Declaration

### Using `:=` (Recommended)

```charlang
// Basic types
name := "John"
age := 25
height := 1.75
isStudent := true

// Arrays
arr := [1, 2, 3]

// Maps
m := {"key": "value"}
```

### Using var

```charlang
// Declare then assign
var x
x = 10

// Declare with assignment
var y = 20

// Multiple variables
var (
    a = 1
    b = 2
)
```

### Type Inference

```charlang
// Type inferred from value
n := 100       // int
s := "hello"   // string
f := 3.14      // float
b := true      // bool
```

## 2.3 Constants and iota

### Constant Declaration

```charlang
const PI = 3.14159
const NAME = "Charlang"

// Constant group
const (
    STATUS_OK = 200
    STATUS_ERR = 500
)
```

### iota Enumeration

```charlang
// Starts from 0 and increments
const (
    RED = iota    // 0
    GREEN          // 1
    BLUE           // 2
)

// Skip values
const (
    _ = iota
    KB = 1 << iota  // 1
    MB              // 2
    GB              // 4
)

// In expressions
const (
    BIT0 = 1 << iota  // 1
    BIT1              // 2
    BIT2              // 4
)
```

## 2.4 Scope

### Function Scope

```charlang
a := "outer"

func() {
    a := "inner"    // shadows outer variable
    pln(a)          // prints: inner
}()

pln(a)              // prints: outer
```

### Assignment vs Declaration

```charlang
x := 10

func() {
    x = 20          // modifies outer variable
    pln(x)          // prints: 20
}()

pln(x)              // prints: 20
```

---

# 3. Data Types

## 3.1 Basic Types

| Type | Example | Description |
|------|---------|-------------|
| undefined | `var v` | Unassigned variable |
| bool | `true`, `false` | Boolean |
| int | `42`, `-10` | Signed integer |
| uint | `100u` | Unsigned integer |
| float | `3.14`, `-0.5` | Floating point |
| string | `"hello"` | String |
| char | `'A'` | Character (actually int) |
| error | `error("msg")` | Error value |

### Type Literals

```charlang
// Integers
42         // int
42i        // imaginary
100u       // uint
0xFF       // hexadecimal
0b1010     // binary
0o777      // octal

// Floats
3.14
.5         // 0.5
1e10       // scientific notation
```

## 3.2 Arrays

### Creating Arrays

```charlang
// Literal
arr := [1, 2, 3, 4, 5]

// With type
arr2 := []int{}

// Multi-dimensional
matrix := [[1, 2], [3, 4]]
```

### Array Slicing

```charlang
arr := [1, 2, 3, 4, 5]

a := arr[1:3]    // [2, 3]
b := arr[:3]     // [1, 2, 3]
c := arr[2:]     // [3, 4, 5]
d := arr[:]      // copy entire array
```

## 3.3 Maps

### Creating Maps

```charlang
// Literal
m := {"name": "John", "age": 25}

// With type
m2 := map[string]int{}
m2["a"] = 1

// Nested
nested := {"user": {"name": "John", "age": 25}}
```

## 3.4 Function Types

```charlang
// Function as value
add := func(a, b) {
    return a + b
}

// Function as parameter
apply := func(fn, a, b) {
    return fn(a, b)
}

result := apply(add, 10, 20)  // 30
```

## 3.5 Type Checking and Conversion

### Type Checking Functions

```charlang
isUndefined(v)   // Is undefined
isBool(v)        // Is boolean
isInt(v)         // Is integer
isUint(v)        // Is unsigned
isFloat(v)       // Is float
isString(v)      // Is string
isArray(v)       // Is array
isMap(v)         // Is map
isFunction(v)    // Is function
isError(v)       // Is error
```

### Type Information

```charlang
typeName(v)      // Type name string
typeCode(v)      // Type code integer
```

### Type Conversion

```charlang
int(v)           // To integer
float(v)         // To float
string(v)        // To string
bool(v)          // To boolean
char(v)          // To character
uint(v)          // To unsigned
bytes(v)         // String to byte array
chars(v)         // String to rune array
```

---

# 4. Operators

## 4.1 Arithmetic Operators

| Operator | Description | Example |
|----------|-------------|---------|
| + | Addition | `10 + 5 = 15` |
| - | Subtraction | `10 - 5 = 5` |
| * | Multiplication | `10 * 5 = 50` |
| / | Division | `10 / 5 = 2` |
| % | Modulo | `10 % 3 = 1` |

## 4.2 Comparison Operators

| Operator | Description | Example |
|----------|-------------|---------|
| == | Equal | `5 == 5 = true` |
| != | Not equal | `5 != 3 = true` |
| > | Greater than | `5 > 3 = true` |
| < | Less than | `5 < 3 = false` |
| >= | Greater or equal | `5 >= 5 = true` |
| <= | Less or equal | `5 <= 5 = true` |

## 4.3 Logical Operators

| Operator | Description | Example |
|----------|-------------|---------|
| && | Logical AND | `true && false = false` |
| \|\| | Logical OR | `true \|\| false = true` |
| ! | Logical NOT | `!true = false` |

## 4.4 Bitwise Operators

| Operator | Description | Example |
|----------|-------------|---------|
| & | Bitwise AND | `0xFF & 0x0F = 0x0F` |
| \| | Bitwise OR | `0xF0 \| 0x0F = 0xFF` |
| ^ | Bitwise XOR | `0xFF ^ 0x0F = 0xF0` |
| &^ | Bit clear | `0xFF &^ 0x0F = 0xF0` |
| << | Left shift | `1 << 4 = 16` |
| >> | Right shift | `16 >> 2 = 4` |

## 4.5 Assignment Operators

```charlang
x := 10
x += 5    // x = 15
x -= 3    // x = 12
x *= 2    // x = 24
x /= 4    // x = 6
x %= 5    // x = 1

x++       // x = 2
x--       // x = 1
```

## 4.6 Other Operators

### Ternary Operator

```charlang
result := condition ? value1 : value2
age := 20
status := age >= 18 ? "adult" : "minor"
```

### Index Access

```charlang
arr := [1, 2, 3]
first := arr[0]

m :={"a": 1, "b": 2}
val := m["a"]
```

---

# 5. Control Flow

## 5.1 if Statement

```charlang
if condition {
    // code
}

if a > b {
    // a > b
} else if a < b {
    // a < b
} else {
    // a == b
}
```

## 5.2 for Statement

```charlang
// Classic for
for i := 0; i < 10; i++ {
    pln(i)
}

// Condition only
for i < 10 {
    pln(i)
    i++
}

// Infinite loop
for {
    if condition {
        break
    }
}
```

## 5.3 for-in Statement

```charlang
// Iterate array
arr := ["a", "b", "c"]
for i, v in arr {
    pln(i, v)
}

// Iterate range
for i in 5 {  // 0,1,2,3,4
    pln(i)
}

// Iterate map
m := {"a": 1, "b": 2}
for k, v in m {
    pln(k, v)
}
```

## 5.4 break and continue

```charlang
for i := 0; i < 10; i++ {
    if i == 3 {
        continue  // skip this iteration
    }
    if i == 7 {
        break     // exit loop
    }
    pln(i)
}
```

---

# 6. Functions

## 6.1 Function Definition and Call

```charlang
// Basic function
greet := func(name) {
    return "Hello, " + name + "!"
}

pln(greet("World"))
```

## 6.2 Parameters and Return Values

### Parameter Types

```charlang
// Fixed parameters
add := func(a, b) {
    return a + b
}

// Variadic parameters
sum := func(...args) {
    total := 0
    for v in args {
        total += v
    }
    return total
}

// Mixed parameters
print := func(prefix, ...args) {
    pln(prefix, args)
}
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
}
```

## 6.3 Closures

```charlang
// Counter
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

## 6.4 Recursion

```charlang
factorial := func(n) {
    if n <= 1 {
        return 1
    }
    return n * factorial(n - 1)
}

pln(factorial(5))  // 120
```

## 6.5 Defer

```charlang
func() {
    defer pln("executes last")
    pln("executes first")
}()
```

---

# 7. Error Handling

## 7.1 try-catch-finally

```charlang
try {
    result := mightFail()
    pln("Success:", result)
} catch err {
    pln("Caught:", err)
} finally {
    pln("Cleanup")
}
```

## 7.2 throw Statement

```charlang
try {
    if invalid {
        throw("Something wrong")
    }
} catch err {
    pln("Error:", err)
}
```

## 7.3 Error Values

```charlang
err := error("error message")

if isError(err) {
    pln("Is error:", err)
}

// Function returning error
check := func(n) {
    if n < 0 {
        return 0, error("negative not allowed")
    }
    return n, ""
}

result, err := check(-1)
if isError(err) {
    pln("Error:", err)
}
```

---

# 8. Common Built-in Functions

## 8.1 Output Functions

```charlang
pln(...)        // Print with newline, space separated
pr(...)         // Print without newline
pl(format, ...) // Formatted print with newline
plt(v)          // Print value and type
sprintf(...)    // Format to string
spr(...)        // Alias for sprintf
```

## 8.2 String Functions

```charlang
len(s)                    // Length
strLen(s)                 // Unicode length
strContains(s, sub)       // Contains substring
strStartsWith(s, prefix)  // Starts with prefix
strEndsWith(s, suffix)    // Ends with suffix
strIndex(s, sub)          // Substring position
strLastIndex(s, sub)      // Last position
strReplace(s, old, new)   // Replace
strSplit(s, sep)          // Split
strJoin(arr, sep)         // Join
strToUpper(s)             // To uppercase
strToLower(s)             // To lowercase
strTrim(s)                // Trim whitespace
strTrimLeft(s)            // Trim left
strTrimRight(s)           // Trim right
strRepeat(s, n)           // Repeat
strCount(s, sub)          // Count occurrences
strReplaceAll(s, old, new) // Replace all
```

## 8.3 Array Functions

```charlang
len(arr)              // Array length
append(arr, items...) // Append elements
contains(arr, item)   // Contains element
copy(dst, src)        // Copy
delete(arr, index)    // Delete (note: not a built-in)
sort(arr)             // Sort ascending
sortReverse(arr)     // Sort descending
```

## 8.4 Map Functions

```charlang
len(m)                // Map size
delete(m, key)        // Delete key-value
m.keys()             // All keys
m.values()           // All values
contains(m, key)     // Contains key
```

## 8.5 Type Conversion Functions

```charlang
int(v)      // To integer
float(v)    // To float
string(v)   // To string
bool(v)     // To boolean
char(v)     // To character
uint(v)     // To unsigned
bytes(v)    // To byte array
chars(v)    // To rune array
typeName(v) // Type name
typeCode(v) // Type code
```

## 8.6 Encoding Functions

```charlang
md5(s)              // MD5 hash
sha1(s)             // SHA1 hash
sha256(s)           // SHA256 hash
base64Encode(s)     // Base64 encode
base64Decode(s)     // Base64 decode
urlEncode(s)        // URL encode
urlDecode(s)        // URL decode
hexEncode(s)        // Hex encode
hexDecode(s)       // Hex decode
```

## 8.7 Math Functions

```charlang
abs(n)              // Absolute value
min(a, b, ...)    // Minimum
max(a, b, ...)    // Maximum
pow(a, b)          // Power
sqrt(n)            // Square root
floor(n)           // Floor
ceil(n)            // Ceiling
round(n)           // Round
rand()            // Random float
randInt(min, max) // Random integer in range
```

## 8.8 Time Functions

```charlang
now()                    // Current time object
getNowStr()            // Current time string
getNowTimeStamp()       // Unix timestamp (seconds)
getNowTimeStampMs()     // Unix timestamp (milliseconds)
timeAddSecs(t, seconds) // Add seconds
timeAddMins(t, minutes) // Add minutes
timeAddHours(t, hours)  // Add hours
timeAddDays(t, days)    // Add days
formatTime(t, layout)   // Format time
strToTime(s)            // Parse time string
getNowStrCompact()      // Compact time string
```

**Time format layout:** Uses Go layout `2006-01-02 15:04:05`

## 8.9 File Functions

**Note: `saveText(content, path)` - content first, path second!**

```charlang
loadText(path)          // Read file
saveText(content, path) // Write file (content first!)
fileExists(path)        // Check if exists
getFileSize(path)       // Get file size
listFiles(dir)          // List files in directory
```

## 8.10 Command Line Functions

```charlang
global argsG            // Command line args array
getOSArgs()            // Get args array
getParam(args, i, default)   // Safe param access
getSwitch(args, name, default) // Get switch param
getSwitches(args)       // Get all switches
ifSwitchExists(args, name)   // Check switch exists
```

## 8.11 Regex Functions

```charlang
regMatch(s, pattern)      // Match
regContains(s, pattern)  // Contains match
regFindFirst(s, pattern)  // Find first
regFindAll(s, pattern)    // Find all
regReplace(s, pattern, repl) // Replace
regCount(s, pattern)      // Count matches
regSplit(s, pattern)     // Split by pattern
```

## 8.12 JSON Functions

```charlang
toJson(v)              // To JSON string
fromJson(s)            // Parse JSON
```

## 8.13 Other Functions

```charlang
exit(code)             // Exit program
sleep(seconds)         // Sleep
getEnv(name)           // Get environment variable
setEnv(name, value)    // Set environment variable
systemCmd(cmd, args...) // Execute system command
```

---

# 9. Concurrent Programming

## 9.1 Thread Basics

```charlang
worker := func(data) {
    for i := 0; i < 5; i++ {
        pln("Working:", i)
        sleep(0.5)
    }
}

// Start thread
worker.threadRun(data)

// Main thread continues
pln("Main continues")
```

## 9.2 Mutex

```charlang
// Create mutex
m := mutex()

// Lock
lock(m)
// critical section
unlock(m)

// Short form with defer
lock(m)
defer unlock(m)
```

### Thread-Safe Counter Example

```charlang
counter := new("int", 0)
lock := mutex()

worker := charCode(`
    param (counter, lock, n)
    for i := 0; i < n; i++ {
        lock(lock)
        setValueByRef(counter, unref(counter) + 1)
        unlock(lock)
    }
`).compile()

worker.threadRun(counter, lock, 100)
worker.threadRun(counter, lock, 100)
sleep(2)

pln(unref(counter))  // 200
```

## 9.3 Reference Types

```charlang
// Create reference
n := new("int", 0)
s := new("string", "hello")

// Get value
val := unref(n)

// Set value
setValueByRef(n, 100)
```

---

# 10. Dynamic Code Execution

## 10.1 charCode Function

```charlang
// Create code object from string
code := charCode(`return 42`)
```

## 10.2 compile

```charlang
// Compile code
compiled := charCode(`
    param (a, b)
    return a + b
`).compile()

// Execute
result := compiled.run(10, 20)  // 30
```

## 10.3 Code Reuse

```charlang
// Compile once, execute multiple times
addCode := charCode(`
    param (a, b)
    return a + b
`).compile()

r1 := addCode.run(1, 2)   // 3
r2 := addCode.run(100, 200)  // 300
```

---

# 11. Module System

## 11.1 import

```charlang
// Import built-in module
ex := import("ex")

// Module returns function
myModule := import("myModule")
```

## 11.2 ex Module

```charlang
ex := import("ex")

// Compile code
compiled := ex.compile(sourceCode)

// Run compiled code
result := ex.runCompiled(compiled, args...)

// Thread execution
ex.threadRunFunc(fn, args...)
ex.threadRunCompiled(compiled, args...)
```

---

# 12. Microservice Development

## 12.1 Server Startup

```bash
# Start server
char -server -port=:8080 -dir=microservice

# Access microservice
# http://127.0.0.1:8080/charms/service-name
```

## 12.2 Writing Microservices

```charlang
// 1. Declare global variables
global requestG
global responseG
global paraMapG
global reqUriG
global reqNameG

// 2. Set response end marker
outG := "TX_END_RESPONSE_XT"

// 3. Set response header
setRespHeader(responseG, "Content-Type", "application/json; charset=utf-8")

// 4. Get parameters
name := trim(paraMapG["name"])
age := trim(paraMapG["age"])

// 5. Business logic
if name == "" {
    writeResp(responseG, genResp("fail", "name required", requestG))
    return outG
}

// 6. Return response
result := {"name": name, "age": age}
writeResp(responseG, genResp("success", toJson(result), requestG))

// 7. End response
return outG
```

## 12.3 Response Functions

```charlang
genResp(status, value, request)  // Generate JSON response
genJsonResp(request, status, value) // Generate JSON response
writeResp(responseG, content)  // Write response content
writeRespHeader(responseG, code) // Write response status code
setRespHeader(responseG, key, value) // Set response header
```

## 12.4 Global Variables

```charlang
requestG      // HTTP request object
responseG     // HTTP response object
reqUriG       // Request URI
reqNameG      // Request name (last part of URL)
paraMapG      // Parameter map
basePathG     // Service root directory
runModeG      // Running mode
versionG      // Charlang version
```

---

# 13. Command Line Reference

## 13.1 Common Options

```bash
# Run script
char script.char

# With arguments
char script.char arg1 arg2

# Script arguments
char script.char -port=8080 -debug

# REPL mode
char

# Compile to executable
char -compile -output=app.exe script.char
```

## 13.2 Server Mode

```bash
# Start web server
char -server -port=:8080 -dir=.

# Specify directory and port
char -server -port=:9090 -dir=public -webDir=static

# With SSL
char -server -port=:443 -sslPort=:443 -certDir=cert
```

---

# 14. Differences from Go

| Feature | Charlang | Go |
|---------|----------|-----|
| Type System | Dynamic | Static |
| Variable Declaration | `:=`, `var` | `var`, `:=` |
| Error Handling | try-catch-finally | defer + error returns |
| Constant Enum | iota | iota |
| Multi-return | Supported | Supported |
| Concurrency | goroutine | goroutine |
| Interfaces | Implicit | Explicit |
| Pointers | None | Available |
| Generics | None | Available (1.18+) |

---

# 15. Built-in Object Methods

## 15.1 Array Methods

```charlang
arr := [1, 2, 3]

arr.len()         // Length (same as len(arr))
arr.contains(v)  // Contains element
arr.indexOf(v)    // Element position
arr.push(v)       // Push to end (assignment required)
arr.pop()         // Pop from end (assignment required)
```

## 15.2 Map Methods

```charlang
m := {"a": 1, "b": 2}

m.keys()         // All keys
m.values()      // All values
m.has(k)        // Has key
m.get(k)        // Get value
m.set(k, v)     // Set value (assignment required)
m.delete(k)    // Delete (assignment required)
```

## 15.3 String Methods

```charlang
s := "Hello"

s.len()              // Length
s.contains(sub)      // Contains
s.startsWith(prefix) // Starts with
s.endsWith(suffix)   // Ends with
s.toUpper()         // To uppercase
s.toLower()         // To lowercase
s.trim()            // Trim whitespace
s.split(sep)        // Split
s.replace(old, new) // Replace
```

## 15.4 Time Methods

```charlang
t := now()

t.year()           // Year
t.month()          // Month
t.day()            // Day
t.hour()           // Hour
t.minute()         // Minute
t.second()         // Second
t.unix()           // Unix timestamp
t.format(layout)   // Format
t.add(seconds)     // Add time
```

---

# 16. Built-in Object Types Reference

## 16.1 Object Types Overview

Charlang provides many built-in object types, each with specific methods.

| Type Name | Creation | Description |
|-----------|----------|-------------|
| `time` | `now()`, `strToTime()` | Time object |
| `array` | `[1, 2, 3]` | Array |
| `map` | `{"a": 1}` | Map |
| `string` | `"hello"` | String |
| `bytes` | `bytes(s)` | Byte array |
| `error` | `error(msg)` | Error object |
| `charCode` | `charCode(source)` | Compiled code object |
| `mutex` | `mutex()` | Mutex |
| `database` | `dbConnect()` | Database connection |
| `StatusResult` | `genResp()` | Response result |
| `StringBuilder` | `stringBuilder()` | String builder |
| `BytesBuffer` | `bytesBuffer()` | Byte buffer |
| `ObjectRef` | `new()` | Reference type |
| `MutableString` | `new("string", v)` | Mutable string |
| `Seq` | `seq()` | Sequence iterator |
| `HttpReq` | (microservice) | HTTP request |
| `HttpResp` | (microservice) | HTTP response |
| `Reader` | `openFile()` | File reader |
| `Writer` | `openFile()` | File writer |
| `File` | `openFile()` | File object |
| `Queue` | `new("queue")` | Queue |
| `Stack` | `new("stack")` | Stack |
| `OrderedMap` | `new("orderedmap")` | Ordered map |
| `MapArray` | `new("maparray")` | Key-value array |
| `BigInt` | `new("bigint", s)` | Big integer |
| `BigFloat` | `new("bigfloat", s)` | Big float |
| `WebSocket` | (WebSocket module) | WebSocket |
| `JsVm` | `newjs()` | JavaScript VM |
| `Gel` | `gel()` | GEL script engine |
| `Excel` | `new("excel")` | Excel handler |

## 16.2 time Object

Time objects are created using `now()` or `strToTime()`.

### Creation

```charlang
t := now()                           // Current time
t2 := strToTime("2024-01-01")        // Parse from string
t3 := getNowTimeStamp()              // Timestamp to time
```

### Properties

```charlang
t := now()
t.year()        // Year (int)
t.month()       // Month (int)
t.day()         // Day (int)
t.hour()        // Hour (int)
t.minute()      // Minute (int)
t.second()      // Second (int)
t.weekday()     // Day of week (int)
t.yearDay()     // Day of year (int)
t.unix()        // Unix timestamp (int)
t.unixMilli()   // Millisecond timestamp (int)
t.unixMicro()   // Microsecond timestamp (int)
t.isZero()      // Is zero time (bool)
```

### Method List

| Method | Parameters | Returns | Description |
|--------|------------|---------|-------------|
| `add` | `seconds` (float) | `time` | Add seconds |
| `sub` | `time` | `time`/`int` | Subtract time |
| `addDate` | `year, month, day` (int) | `time` | Add date |
| `after` | `time` | `bool` | After specified time |
| `before` | `time` | `bool` | Before specified time |
| `format` | `layout` (string) | `string` | Format time |
| `appendFormat` | `bytes, layout` | `bytes` | Format and append |
| `in` | `location` | `time` | Convert timezone |
| `round` | `duration` (int) | `time` | Round to precision |
| `truncate` | `duration` (int) | `time` | Truncate |
| `equal` | `time` | `bool` | Compare equality |
| `date` | - | `map` | Date components {year, month, day} |
| `clock` | - | `map` | Clock {hour, minute, second, nanosecond} |
| `year` | - | `int` | Get year |
| `month` | - | `int` | Get month |
| `day` | - | `int` | Get day |
| `hour` | - | `int` | Get hour |
| `minute` | - | `int` | Get minute |
| `second` | - | `int` | Get second |
| `weekday` | - | `int` | Get weekday (0=Sunday) |

### Time Operations

```charlang
t1 := now()
t2 := t1 + 3600      // Add 1 hour (seconds)
t3 := t1 - 3600      // Subtract 1 hour
t4 := t1 - t2        // Difference (seconds)

t5 := t1 > t2        // Compare
t6 := t1 < t2
t7 := t1 >= t2
t8 := t1 <= t2
```

## 16.3 array Object

### Creation

```charlang
arr := [1, 2, 3]
arr2 := []int{}
arr3 := new("array")
```

### Method List

| Method | Parameters | Returns | Description |
|--------|------------|---------|-------------|
| `len()` | - | `int` | Array length |
| `contains` | `value` | `bool` | Contains element |
| `indexOf` | `value` | `int` | Element index, -1 if not found |
| `push` | `value...` | `array` | Push elements |
| `pop` | - | `value` | Pop element |
| `unshift` | `value...` | `array` | Push to front |
| `shift` | - | `value` | Pop from front |
| `reverse` | - | `array` | Reverse |
| `clone` | - | `array` | Shallow copy |
| `sort` | - | `array` | Sort (assignment required) |
| `sortReverse` | - | `array` | Reverse sort |
| `join` | `separator` | `string` | Join to string |
| `slice` | `start, end` | `array` | Slice |

## 16.4 map Object

### Creation

```charlang
m := {"a": 1, "b": 2}
m2 := map[string]int{}
m3 := new("map")
```

### Method List

| Method | Parameters | Returns | Description |
|--------|------------|---------|-------------|
| `keys()` | - | `array` | All keys |
| `values()` | - | `array` | All values |
| `has` | `key` | `bool` | Has key |
| `get` | `key` | `value` | Get value |
| `set` | `key, value` | - | Set value |
| `delete` | `key` | - | Delete key |
| `len()` | - | `int` | Size |
| `clear` | - | - | Clear all |
| `clone` | - | `map` | Shallow copy |
| `merge` | `map` | `map` | Merge another map |

## 16.5 string Object

### Creation

```charlang
s := "hello"
s2 := new("string", "hello")
s3 := string(65)        // From ASCII
```

### Method List

| Method | Parameters | Returns | Description |
|--------|------------|---------|-------------|
| `len()` | - | `int` | String length |
| `contains` | `sub` | `bool` | Contains substring |
| `startsWith` | `prefix` | `bool` | Starts with |
| `endsWith` | `suffix` | `bool` | Ends with |
| `indexOf` | `sub` | `int` | Substring position |
| `lastIndexOf` | `sub` | `int` | Last position |
| `toUpper()` | - | `string` | To uppercase |
| `toLower()` | - | `string` | To lowercase |
| `trim()` | - | `string` | Trim whitespace |
| `trimLeft()` | - | `string` | Trim left |
| `trimRight()` | - | `string` | Trim right |
| `split` | `sep` | `array` | Split |
| `replace` | `old, new` | `string` | Replace first |
| `replaceAll` | `old, new` | `string` | Replace all |
| `substring` | `start, end` | `string` | Substring |
| `charAt` | `index` | `string` | Character at |
| `codePointAt` | `index` | `int` | Unicode code point |
| `repeat` | `count` | `string` | Repeat |
| `reverse` | - | `string` | Reverse |

## 16.6 error Object

### Creation

```charlang
err := error("error message")
err2 := error("ErrorName", "error message")
```

### Method List

| Method | Parameters | Returns | Description |
|--------|------------|---------|-------------|
| `toString()` | - | `string` | String representation |
| `getMessage()` | - | `string` | Get message |
| `getName()` | - | `string` | Get error name |

## 16.7 charCode Object

### Creation

```charlang
code := charCode(`return 42`)
compiled := code.compile()
```

### Method List

| Method | Parameters | Returns | Description |
|--------|------------|---------|-------------|
| `compile()` | - | `charCode` | Compile code |
| `run()` | `args...` | `value` | Execute code |
| `threadRun()` | `args...` | - | Execute in thread |

## 16.8 mutex Object

### Creation

```charlang
m := mutex()
```

### Methods

| Method | Description |
|--------|-------------|
| `lock()` | Lock |
| `unlock()` | Unlock |

## 16.9 database Object

### Creation

```charlang
db := dbConnect("sqlite3", "test.db")
db := dbConnect("mysql", "user:pass@tcp(localhost:3306)/dbname")
```

### Method List

| Method | Parameters | Returns | Description |
|--------|------------|---------|-------------|
| `query` | `sql` | `array` | Query returns array |
| `queryRecs` | `sql` | `array` | Query returns records |
| `queryMap` | `sql` | `map` | Query returns map |
| `queryMapArray` | `sql` | `array` | Query returns map array |
| `queryCount` | `sql` | `int` | Query count |
| `queryFloat` | `sql` | `float` | Query float |
| `queryString` | `sql` | `string` | Query string |
| `exec` | `sql` | `int` | Execute, returns affected rows |
| `close` | - | - | Close connection |

## 16.10 StringBuilder Object

### Creation

```charlang
sb := stringBuilder()
```

### Method List

| Method | Parameters | Returns | Description |
|--------|------------|---------|-------------|
| `write` | `value` | - | Write value |
| `writeString` | `s` | - | Write string |
| `writeBytes` | `b` | - | Write bytes |
| `toString()` | - | `string` | Convert to string |
| `reset` | - | - | Reset |

## 16.11 StatusResult Object

### Creation

```charlang
resp := genResp("success", data, requestG)
```

### Method List

| Method | Parameters | Returns | Description |
|--------|------------|---------|-------------|
| `status()` | - | `string` | Get status |
| `value()` | - | `value` | Get value |
| `isValid()` | - | `bool` | Is valid |
| `isSuccess()` | - | `bool` | Is success |
| `toString()` | - | `string` | String representation |

## 16.12 Queue Object

### Creation

```charlang
q := new("queue")
```

### Method List

| Method | Parameters | Returns | Description |
|--------|------------|---------|-------------|
| `push` | `value` | - | Enqueue |
| `pop` | - | `value` | Dequeue |
| `front` | - | `value` | Peek front |
| `len()` | - | `int` | Queue length |
| `isEmpty()` | - | `bool` | Is empty |

## 16.13 Stack Object

### Creation

```charlang
s := new("stack")
```

### Method List

| Method | Parameters | Returns | Description |
|--------|------------|---------|-------------|
| `push` | `value` | - | Push |
| `pop` | - | `value` | Pop |
| `top` | - | `value` | Peek |
| `len()` | - | `int` | Stack size |
| `isEmpty()` | - | `bool` | Is empty |

## 16.14 ObjectRef Object

### Creation

```charlang
ref := new("int", 0)
ref2 := new("string", "hello")
ref3 := new("bool", true)
ref4 := new("float", 3.14)
```

### Method List

| Method | Parameters | Returns | Description |
|--------|------------|---------|-------------|
| `unref()` | - | `value` | Get value |
| `setValueByRef()` | `value` | - | Set value |

## 16.15 BytesBuffer Object

### Creation

```charlang
buf := bytesBuffer()
```

### Method List

| Method | Parameters | Returns | Description |
|--------|------------|---------|-------------|
| `write` | `value` | - | Write |
| `writeString` | `s` | - | Write string |
| `writeBytes` | `b` | - | Write bytes |
| `toBytes()` | - | `bytes` | To byte array |
| `toString()` | - | `string` | To string |
| `reset` | - | - | Reset |
| `len()` | - | `int` | Length |

## 16.16 BigInt / BigFloat Objects

### Creation

```charlang
bi := new("bigint", "12345678901234567890")
bf := new("bigfloat", "3.14159265358979323846")
```

### Methods

| Object | Method | Description |
|--------|--------|-------------|
| BigInt | `add()`, `sub()`, `mul()`, `div()` | Arithmetic |
| BigInt | `mod()`, `pow()`, `sqrt()` | Mod, power, sqrt |
| BigInt | `toString()` | To string |
| BigFloat | `add()`, `sub()`, `mul()`, `div()` | Arithmetic |
| BigFloat | `pow()`, `sqrt()`, `abs()` | Power, sqrt, abs |
| BigFloat | `toString()` | To string |
| Both | `gt()`, `lt()`, `eq()`, `gte()`, `lte()` | Comparison |

## 16.17 HttpReq / HttpResp Objects

### HttpReq Properties

```charlang
global requestG

requestG.Method       // GET, POST, etc.
requestG.URL          // Request URL
requestG.RequestURI   // Request URI
requestG.Host         // Host
requestG.Header       // Headers
requestG.Body         // Body
requestG.Form         // Form data
requestG.PostForm     // POST form data
```

### HttpResp Methods

```charlang
global responseG

setRespHeader(responseG, "Content-Type", "text/html")
writeResp(responseG, "Hello")
writeRespHeader(responseG, 200)
```

## 16.18 File / Reader / Writer Objects

### Creation

```charlang
f := openFile("test.txt", "r")
```

### Reader Methods

| Method | Description |
|--------|-------------|
| `read(n)` | Read n bytes |
| `readAll()` | Read all |
| `readLine()` | Read line |
| `close()` | Close |

### Writer Methods

| Method | Description |
|--------|-------------|
| `write(data)` | Write |
| `writeString(s)` | Write string |
| `flush()` | Flush |
| `close()` | Close |

---

# 17. Appendix

## 17.1 Type Codes Reference

| Type Code | Type Name |
|-----------|-----------|
| 101 | undefined |
| 102 | bool |
| 103 | error |
| 104 | nil |
| 105 | string |
| 106 | array |
| 107 | int |
| 108 | char |
| 109 | uint |
| 110 | float |
| 111 | bytes |
| 112 | map |
| 115 | float (alias) |
| 301 | time |
| 302 | charCode |
| 303 | function |
| 304 | builtin_function |
| 311 | time (type code) |
| 312 | regexp |
| 313 | duration |
| 314 | file |
| 315 | bytes_buffer |
| 316 | string_builder |
| 317 | mutex |
| 318 | object_ref |
| 319 | sync_map |
| 320 | database |
| 321 | status_result |
| 322 | any |
| 323 | location |
| 324 | mutable_string |
| 325 | seq |
| 326 | mux |
| 327 | http_request |
| 328 | http_response |
| 329 | http_handler |
| 330 | reader |
| 331 | writer |
| 332 | charcode |
| 333 | gel |
| 334 | ordered_map |
| 335 | bigint |
| 336 | bigfloat |
| 337 | image |
| 338 | delegate |
| 339 | excel |
| 340 | stack |
| 341 | queue |
| 342 | jsvm |
| 343 | eval_machine |
| 344 | map_array |
| 345 | websocket |

## 17.2 Operator Precedence

From high to low:

| Precedence | Operators |
|------------|-----------|
| 1 | `()` function call `.` member `[]` index |
| 2 | `!` `~` `+` `-` (unary) |
| 3 | `*` `/` `%` |
| 4 | `+` `-` (binary) |
| 5 | `<<` `>>` |
| 6 | `<` `<=` `>` `>=` |
| 7 | `==` `!=` |
| 8 | `&` (bit and) |
| 9 | `^` (bit xor) |
| 10 | `\|` (bit or) |
| 11 | `&&` |
| 12 | `\|\|` |
| 13 | `?:` ternary |
| 14 | `=` `+=` `-=` `*=` `/=` `%=` `&=` `\|=` `^=` `<<=` `>>=` |
| 15 | `,` |

---

*For more information, visit https://topget.org/charlang*
