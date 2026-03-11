# 察语言参考

## 目录

- [1. 简介](#1-简介)
- [2. 语法](#2-语法)
  - [2.1 标识符与关键字](#21-标识符与关键字)
  - [2.2 变量声明](#22-变量声明)
  - [2.3 常量与iota](#23-常量与iota)
  - [2.4 作用域](#24-作用域)
- [3. 数据类型](#3-数据类型)
  - [3.1 基本类型](#31-基本类型)
  - [3.2 数组](#32-数组)
  - [3.3 Map映射](#33-map映射)
  - [3.4 函数类型](#34-函数类型)
  - [3.5 类型检查与转换](#35-类型检查与转换)
- [4. 运算符](#4-运算符)
  - [4.1 算术运算符](#41-算术运算符)
  - [4.2 比较运算符](#42-比较运算符)
  - [4.3 逻辑运算符](#43-逻辑运算符)
  - [4.4 位运算符](#44-位运算符)
  - [4.5 赋值运算符](#45-赋值运算符)
  - [4.6 其他运算符](#46-其他运算符)
- [5. 流程控制](#5-流程控制)
  - [5.1 if语句](#51-if语句)
  - [5.2 for语句](#52-for语句)
  - [5.3 for-in语句](#53-for-in语句)
  - [5.4 break与continue](#54-break与continue)
- [6. 函数](#6-函数)
  - [6.1 函数定义与调用](#61-函数定义与调用)
  - [6.2 参数与返回值](#62-参数与返回值)
  - [6.3 闭包](#63-闭包)
  - [6.4 递归](#64-递归)
  - [6.5 延迟执行defer](#65-延迟执行defer)
- [7. 错误处理](#7-错误处理)
  - [7.1 try-catch-finally](#71-try-catch-finally)
  - [7.2 throw语句](#72-throw语句)
  - [7.3 错误值](#73-错误值)
- [8. 常用内置函数](#8-常用内置函数)
  - [8.1 输出函数](#81-输出函数)
  - [8.2 字符串函数](#82-字符串函数)
  - [8.3 数组函数](#83-数组函数)
  - [8.4 Map函数](#84-map函数)
  - [8.5 类型转换函数](#85-类型转换函数)
  - [8.6 编码函数](#86-编码函数)
  - [8.7 数学函数](#87-数学函数)
  - [8.8 时间函数](#88-时间函数)
  - [8.9 文件函数](#89-文件函数)
  - [8.10 命令行函数](#810-命令行函数)
  - [8.11 正则表达式函数](#811-正则表达式函数)
  - [8.12 JSON函数](#812-json函数)
  - [8.13 其他函数](#813-其他函数)
- [9. 并发编程](#9-并发编程)
  - [9.1 线程基础](#91-线程基础)
  - [9.2 互斥锁Mutex](#92-互斥锁mutex)
  - [9.3 引用类型](#93-引用类型)
- [10. 动态代码执行](#10-动态代码执行)
  - [10.1 charCode函数](#101-charcode函数)
  - [10.2 compile编译](#102-compile编译)
  - [10.3 代码复用](#103-代码复用)
- [11. 模块系统](#11-模块系统)
  - [11.1 import导入](#111-import导入)
  - [11.2 ex模块](#112-ex模块)
- [12. 微服务开发](#12-微服务开发)
  - [12.1 服务器启动](#121-服务器启动)
  - [12.2 微服务编写](#122-微服务编写)
  - [12.3 响应函数](#123-响应函数)
  - [12.4 全局变量](#124-全局变量)
- [13. 命令行参考](#13-命令行参考)
  - [13.1 常用参数](#131-常用参数)
  - [13.2 服务模式](#132-服务模式)
- [14. 与Go语言的区别](#14-与go语言的区别)
- [15. 内置对象类型方法](#15-内置对象类型方法)
  - [15.1 数组方法](#151-数组方法)
  - [15.2 Map方法](#152-map方法)
  - [15.3 字符串方法](#153-字符串方法)
  - [15.4 时间方法](#154-时间方法)

---

# 1. 简介

察语言（Charlang）是一种基于Go语言开发的动态脚本语言，设计目标是在保持Go语言简洁性的同时，提供更灵活的类型系统和更便捷的错误处理机制。

**核心特性：**

- **动态类型**：变量类型可以随时改变
- **简洁语法**：去除了一些Go语言的强制性要求
- **错误处理**：使用try-catch-finally替代defer
- **单文件执行**：无需复杂的编译过程
- **内置服务器**：支持Web服务/微服务开发
- **并发支持**：轻量级线程和互斥锁
- **丰富函数库**：内置大量常用函数

**官方资源：**
- 官网：https://topget.org/charlang
- GitHub：https://github.com/topxeq/charlang

---

# 2. 语法

## 2.1 标识符与关键字

### 标识符规则

- 必须以字母或下划线开头
- 可以包含字母、数字和下划线
- 大小写敏感

```charlang
name := "valid"
_name := "valid"
name123 := "valid"
_123 := "valid"
```

### 关键字

```
break     default     func     interface   select
case      defer       go       map         struct
chan      else        goto     package     switch
const     fallthrough if       range       type
continue  for         import   return      var
```

### 保留字

```
true    false    nil     iota    undefined
```

## 2.2 变量声明

### 使用`:=`声明（推荐）

```charlang
// 基本类型
name := "张三"
age := 25
height := 1.75
isStudent := true

// 数组
arr := [1, 2, 3]

// Map
m := {"key": "value"}
```

### 使用var声明

```charlang
// 先声明后赋值
var x
x = 10

// 声明时赋值
var y = 20

// 多变量
var (
    a = 1
    b = 2
)
```

### 变量类型推导

```charlang
// 根据初始值推导类型
n := 100       // int
s := "hello"   // string
f := 3.14      // float
b := true      // bool
```

## 2.3 常量与iota

### 常量声明

```charlang
const PI = 3.14159
const NAME = "Charlang"

// 常量组
const (
    STATUS_OK = 200
    STATUS_ERR = 500
)
```

### iota枚举

```charlang
// 从0开始递增
const (
    RED = iota    // 0
    GREEN          // 1
    BLUE           // 2
)

// 跳过值
const (
    _ = iota
    KB = 1 << iota  // 1
    MB              // 2
    GB              // 4
)

// 表达式中使用
const (
    BIT0 = 1 << iota  // 1
    BIT1              // 2
    BIT2              // 4
)
```

## 2.4 作用域

### 函数作用域

```charlang
a := "outer"

func() {
    a := "inner"    // 遮蔽外层变量
    pln(a)          // 输出: inner
}()

pln(a)              // 输出: outer
```

### 赋值而非创建

```charlang
x := 10

func() {
    x = 20          // 修改外层变量
    pln(x)          // 输出: 20
}()

pln(x)              // 输出: 20
```

---

# 3. 数据类型

## 3.1 基本类型

| 类型 | 示例 | 说明 |
|------|------|------|
| undefined | `var v` | 未赋值的变量 |
| bool | `true`, `false` | 布尔值 |
| int | `42`, `-10` | 有符号整数 |
| uint | `100u` | 无符号整数 |
| float | `3.14`, `-0.5` | 浮点数 |
| string | `"hello"` | 字符串 |
| char | `'A'` | 字符(实际是int) |
| error | `error("msg")` | 错误值 |

### 类型字面量

```charlang
// 整数
42         // int
42i        // 虚数
100u       // uint
0xFF       // 十六进制
0b1010     // 二进制
0o777      // 八进制

// 浮点数
3.14
.5         // 0.5
1e10       // 科学计数
```

## 3.2 数组

### 创建数组

```charlang
// 字面量
arr := [1, 2, 3, 4, 5]

// 指定类型
arr2 := []int{}

// 多维数组
matrix := [[1, 2], [3, 4]]
```

### 数组切片

```charlang
arr := [1, 2, 3, 4, 5]

a := arr[1:3]    // [2, 3]
b := arr[:3]     // [1, 2, 3]
c := arr[2:]     // [3, 4, 5]
d := arr[:]      // 复制整个数组
```

## 3.3 Map映射

### 创建Map

```charlang
// 字面量
m := {"name": "张三", "age": 25}

// 指定类型
m2 := map[string]int{}
m2["a"] = 1

// 嵌套
nested := {"user": {"name": "张三", "age": 25}}
```

## 3.4 函数类型

```charlang
// 函数作为值
add := func(a, b) {
    return a + b
}

// 函数作为参数
apply := func(fn, a, b) {
    return fn(a, b)
}

result := apply(add, 10, 20)  // 30
```

## 3.5 类型检查与转换

### 类型检查函数

```charlang
isUndefined(v)   // 是否未定义
isBool(v)        // 是否布尔
isInt(v)         // 是否整数
isUint(v)        // 是否无符号整数
isFloat(v)       // 是否浮点
isString(v)      // 是否字符串
isArray(v)       // 是否数组
isMap(v)         // 是否Map
isFunction(v)    // 是否函数
isError(v)       // 是否错误
```

### 类型信息

```charlang
typeName(v)      // 类型名称字符串
typeCode(v)      // 类型代码整数
```

### 类型转换

```charlang
int(v)           // 转换为整数
float(v)         // 转换为浮点
string(v)        // 转换为字符串
bool(v)          // 转换为布尔
char(v)          // 转换为字符
uint(v)          // 转换为无符号整数
bytes(v)         // 字符串转字节数组
chars(v)         // 字符串转字符数组
```

---

# 4. 运算符

## 4.1 算术运算符

| 运算符 | 说明 | 示例 |
|--------|------|------|
| + | 加 | `10 + 5 = 15` |
| - | 减 | `10 - 5 = 5` |
| * | 乘 | `10 * 5 = 50` |
| / | 除 | `10 / 5 = 2` |
| % | 取余 | `10 % 3 = 1` |

## 4.2 比较运算符

| 运算符 | 说明 | 示例 |
|--------|------|------|
| == | 等于 | `5 == 5 = true` |
| != | 不等于 | `5 != 3 = true` |
| > | 大于 | `5 > 3 = true` |
| < | 小于 | `5 < 3 = false` |
| >= | 大于等于 | `5 >= 5 = true` |
| <= | 小于等于 | `5 <= 5 = true` |

## 4.3 逻辑运算符

| 运算符 | 说明 | 示例 |
|--------|------|------|
| && | 逻辑与 | `true && false = false` |
| \|\| | 逻辑或 | `true \|\| false = true` |
| ! | 逻辑非 | `!true = false` |

## 4.4 位运算符

| 运算符 | 说明 | 示例 |
|--------|------|------|
| & | 位与 | `0xFF & 0x0F = 0x0F` |
| \| | 位或 | `0xF0 \| 0x0F = 0xFF` |
| ^ | 位异或 | `0xFF ^ 0x0F = 0xF0` |
| &^ | 位清除 | `0xFF &^ 0x0F = 0xF0` |
| << | 左移 | `1 << 4 = 16` |
| >> | 右移 | `16 >> 2 = 4` |

## 4.5 赋值运算符

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

## 4.6 其他运算符

### 三元运算符

```charlang
result := condition ? value1 : value2
age := 20
status := age >= 18 ? "成年" : "未成年"
```

### 索引访问

```charlang
arr := [1, 2, 3]
first := arr[0]

m := {"a": 1, "b": 2}
val := m["a"]
```

---

# 5. 流程控制

## 5.1 if语句

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

## 5.2 for语句

```charlang
// 经典for
for i := 0; i < 10; i++ {
    pln(i)
}

// 条件循环
for i < 10 {
    pln(i)
    i++
}

// 无限循环
for {
    if condition {
        break
    }
}
```

## 5.3 for-in语句

```charlang
// 遍历数组
arr := ["a", "b", "c"]
for i, v in arr {
    pln(i, v)
}

// 遍历范围
for i in 5 {  // 0,1,2,3,4
    pln(i)
}

// 遍历Map
m := {"a": 1, "b": 2}
for k, v in m {
    pln(k, v)
}
```

## 5.4 break与continue

```charlang
for i := 0; i < 10; i++ {
    if i == 3 {
        continue  // 跳过本次循环
    }
    if i == 7 {
        break     // 退出循环
    }
    pln(i)
}
```

---

# 6. 函数

## 6.1 函数定义与调用

```charlang
// 基本函数
greet := func(name) {
    return "Hello, " + name + "!"
}

pln(greet("World"))
```

## 6.2 参数与返回值

### 参数类型

```charlang
// 固定参数
add := func(a, b) {
    return a + b
}

// 可变参数
sum := func(...args) {
    total := 0
    for v in args {
        total += v
    }
    return total
}

// 混合参数
print := func(prefix, ...args) {
    pln(prefix, args)
}
```

### 多返回值

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

## 6.3 闭包

```charlang
// 计数器
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

## 6.4 递归

```charlang
factorial := func(n) {
    if n <= 1 {
        return 1
    }
    return n * factorial(n - 1)
}

pln(factorial(5))  // 120
```

## 6.5 延迟执行defer

```charlang
func() {
    defer pln("最后执行")
    pln("先执行")
}()
```

---

# 7. 错误处理

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

## 7.2 throw语句

```charlang
try {
    if invalid {
        throw("Something wrong")
    }
} catch err {
    pln("Error:", err)
}
```

## 7.3 错误值

```charlang
err := error("error message")

if isError(err) {
    pln("Is error:", err)
}

// 函数返回错误
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

# 8. 常用内置函数

## 8.1 输出函数

```charlang
pln(...)        // 打印换行，空格分隔
pr(...)         // 打印不换行
pl(format, ...) // 格式化打印换行
plt(v)          // 打印值和类型
sprintf(...)    // 格式化字符串
spr(...)        // 同sprintf
```

## 8.2 字符串函数

```charlang
len(s)                    // 字符串长度
strLen(s)                 // 字符串长度(Unicode)
strContains(s, sub)       // 是否包含
strStartsWith(s, prefix)  // 是否开头
strEndsWith(s, suffix)    // 是否结尾
strIndex(s, sub)          // 子串位置
strLastIndex(s, sub)      // 最后位置
strReplace(s, old, new)   // 替换
strSplit(s, sep)          // 分割
strJoin(arr, sep)         // 拼接
strToUpper(s)             // 转大写
strToLower(s)             // 转小写
strTrim(s)                // 去除空白
strTrimLeft(s)           // 去除左空白
strTrimRight(s)          // 去除右空白
strRepeat(s, n)          // 重复
strCount(s, sub)         // 计数
strReplaceAll(s, old, new) // 全部替换
```

## 8.3 数组函数

```charlang
len(arr)              // 数组长度
append(arr, items...) // 添加元素
contains(arr, item)   // 是否包含
copy(dst, src)        // 复制
delete(arr, index)    // 删除(注意不是内置)
sort(arr)             // 升序排序
sortReverse(arr)     // 降序排序
```

## 8.4 Map函数

```charlang
len(m)                // Map大小
delete(m, key)        // 删除键值对
m.keys()             // 所有键
m.values()           // 所有值
contains(m, key)     // 是否包含键
```

## 8.5 类型转换函数

```charlang
int(v)      // 转整数
float(v)    // 转浮点
string(v)   // 转字符串
bool(v)     // 转布尔
char(v)     // 转字符
uint(v)     // 转无符号
bytes(v)    // 转字节数组
chars(v)    // 转字符数组
typeName(v) // 类型名
typeCode(v) // 类型代码
```

## 8.6 编码函数

```charlang
md5(s)              // MD5哈希
sha1(s)             // SHA1哈希
sha256(s)           // SHA256哈希
base64Encode(s)     // Base64编码
base64Decode(s)     // Base64解码
urlEncode(s)        // URL编码
urlDecode(s)        // URL解码
hexEncode(s)        // 十六进制编码
hexDecode(s)       // 十六进制解码
```

## 8.7 数学函数

```charlang
abs(n)              // 绝对值
min(a, b, ...)     // 最小值
max(a, b, ...)     // 最大值
pow(a, b)          // 幂运算
sqrt(n)            // 平方根
floor(n)           // 向下取整
ceil(n)            // 向上取整
round(n)           // 四舍五入
rand()             // 随机数
randInt(min, max)  // 范围随机整数
```

## 8.8 时间函数

```charlang
now()                    // 当前时间对象
getNowStr()             // 当前时间字符串
getNowTimeStamp()       // 当前时间戳(秒)
getNowTimeStampMs()     // 当前时间戳(毫秒)
timeAddSecs(t, seconds) // 加秒
timeAddMins(t, minutes) // 加分钟
timeAddHours(t, hours)  // 加小时
timeAddDays(t, days)    // 加天
formatTime(t, layout)   // 格式化时间
strToTime(s)            // 字符串转时间
getNowStrCompact()      // 紧凑时间字符串
```

**时间格式化布局：** 使用Go语言布局 `2006-01-02 15:04:05`

## 8.9 文件函数

**注意：`saveText(content, path)` - 内容在前，路径在后！**

```charlang
loadText(path)          // 读取文件
saveText(content, path) // 写入文件
fileExists(path)        // 文件是否存在
getFileSize(path)       // 文件大小
listFiles(dir)          // 列出文件
```

## 8.10 命令行函数

```charlang
global argsG            // 命令行参数数组
getOSArgs()            // 获取参数数组
getParam(args, i, default)   // 安全获取参数
getSwitch(args, name, default) // 获取开关参数
getSwitches(args)       // 获取所有开关
ifSwitchExists(args, name)   // 开关是否存在
```

## 8.11 正则表达式函数

```charlang
regMatch(s, pattern)      // 匹配
regContains(s, pattern)  // 包含匹配
regFindFirst(s, pattern)  // 查找第一个
regFindAll(s, pattern)    // 查找所有
regReplace(s, pattern, repl) // 替换
regCount(s, pattern)      // 计数
regSplit(s, pattern)     // 分割
```

## 8.12 JSON函数

```charlang
toJson(v)              // 转JSON字符串
fromJson(s)            // 解析JSON
```

## 8.13 其他函数

```charlang
exit(code)             // 退出程序
sleep(seconds)         // 休眠
exit(code)             // 退出
getEnv(name)           // 获取环境变量
setEnv(name, value)    // 设置环境变量
systemCmd(cmd, args...) // 执行系统命令
```

---

# 9. 并发编程

## 9.1 线程基础

```charlang
worker := func(data) {
    for i := 0; i < 5; i++ {
        pln("Working:", i)
        sleep(0.5)
    }
}

// 启动线程
worker.threadRun(data)

// 主线程继续
pln("Main continues")
```

## 9.2 互斥锁Mutex

```charlang
// 创建互斥锁
m := mutex()

// 加锁
lock(m)
// 临界区
unlock(m)

// 简写
lock(m)
defer unlock(m)  // defer在函数返回时解锁
```

### 线程安全计数器示例

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

## 9.3 引用类型

```charlang
// 创建引用
n := new("int", 0)
s := new("string", "hello")

// 获取值
val := unref(n)

// 设置值
setValueByRef(n, 100)
```

---

# 10. 动态代码执行

## 10.1 charCode函数

```charlang
// 从字符串创建代码对象
code := charCode(`return 42`)
```

## 10.2 compile编译

```charlang
// 编译代码
compiled := charCode(`
    param (a, b)
    return a + b
`).compile()

// 执行
result := compiled.run(10, 20)  // 30
```

## 10.3 代码复用

```charlang
// 编译一次，多次执行
addCode := charCode(`
    param (a, b)
    return a + b
`).compile()

r1 := addCode.run(1, 2)   // 3
r2 := addCode.run(100, 200)  // 300
```

---

# 11. 模块系统

## 11.1 import导入

```charlang
// 导入内置模块
ex := import("ex")

// 模块返回函数
myModule := import("myModule")
```

## 11.2 ex模块

```charlang
ex := import("ex")

// 编译代码
compiled := ex.compile(sourceCode)

// 运行编译后的代码
result := ex.runCompiled(compiled, args...)

// 线程执行
ex.threadRunFunc(fn, args...)
ex.threadRunCompiled(compiled, args...)
```

---

# 12. 微服务开发

## 12.1 服务器启动

```bash
# 启动服务器
char -server -port=:8080 -dir=microservice

# 访问微服务
# http://127.0.0.1:8080/charms/服务名
```

## 12.2 微服务编写

```charlang
// 1. 声明全局变量
global requestG
global responseG
global paraMapG
global reqUriG
global reqNameG

// 2. 设置响应结束标记
outG := "TX_END_RESPONSE_XT"

// 3. 设置响应头
setRespHeader(responseG, "Content-Type", "application/json; charset=utf-8")

// 4. 获取参数
name := trim(paraMapG["name"])
age := trim(paraMapG["age"])

// 5. 业务处理
if name == "" {
    writeResp(responseG, genResp("fail", "name required", requestG))
    return outG
}

// 6. 返回响应
result := {"name": name, "age": age}
writeResp(responseG, genResp("success", toJson(result), requestG))

// 7. 结束响应
return outG
```

## 12.3 响应函数

```charlang
genResp(status, value, request)  // 生成JSON响应
genJsonResp(request, status, value) // 生成JSON响应
writeResp(responseG, content)  // 写入响应内容
writeRespHeader(responseG, code) // 写入响应状态码
setRespHeader(responseG, key, value) // 设置响应头
```

## 12.4 全局变量

```charlang
requestG      // HTTP请求对象
responseG     // HTTP响应对象
reqUriG       // 请求URI
reqNameG      // 请求名称(URL最后部分)
paraMapG      // 参数映射
basePathG     // 服务根目录
runModeG      // 运行模式
versionG      // Charlang版本
```

---

# 13. 命令行参考

## 13.1 常用参数

```bash
# 运行脚本
char script.char

# 带参数
char script.char arg1 arg2

# 脚本内参数
char script.char -port=8080 -debug

# REPL模式
char

# 编译为可执行文件
char -compile -output=app.exe script.char
```

## 13.2 服务模式

```bash
# 启动Web服务器
char -server -port=:8080 -dir=.

# 指定目录和端口
char -server -port=:9090 -dir=public -webDir=static

# 带SSL
char -server -port=:443 -sslPort=:443 -certDir=cert
```

---

# 14. 与Go语言的区别

| 特性 | Charlang | Go |
|------|----------|-----|
| 类型系统 | 动态类型 | 静态类型 |
| 变量声明 | `:=`, `var` | `var`, `:=` |
| 错误处理 | try-catch-finally | defer + error返回值 |
| 常量枚举 | iota | iota |
| 函数多返回 | 支持 | 支持 |
| 并发 | goroutine | goroutine |
| 接口 | 隐式实现 | 显式实现 |
| 指针 | 无 | 有 |
| 泛型 | 无 | 有(1.18+) |

---

# 15. 内置对象类型方法

## 15.1 数组方法

```charlang
arr := [1, 2, 3]

arr.len()         // 长度(同len(arr))
arr.contains(v)  // 是否包含
arr.indexOf(v)    // 元素位置
arr.push(v)       // 末尾添加(需赋值)
arr.pop()         // 末尾删除(需赋值)
```

## 15.2 Map方法

```charlang
m := {"a": 1, "b": 2}

m.keys()         // 所有键
m.values()      // 所有值
m.has(k)        // 是否包含键
m.get(k)        // 获取值
m.set(k, v)     // 设置值(需赋值)
m.delete(k)    // 删除(需赋值)
```

## 15.3 字符串方法

```charlang
s := "Hello"

s.len()              // 长度
s.contains(sub)      // 包含
s.startsWith(prefix) // 开头
s.endsWith(suffix)   // 结尾
s.toUpper()         // 大写
s.toLower()         // 小写
s.trim()            // 去除空白
s.split(sep)        // 分割
s.replace(old, new) // 替换
```

## 15.4 时间方法

```charlang
t := now()

t.year()           // 年
t.month()          // 月
t.day()            // 日
t.hour()           // 时
t.minute()         // 分
t.second()         // 秒
t.unix()           // 时间戳
t.format(layout)   // 格式化
t.add(seconds)     // 加时间
```

---

# 16. 内置对象类型详解

## 16.1 对象类型概览

察语言内置了丰富的对象类型，每种类型都有特定的方法可供调用。

| 类型名称 | 创建方式 | 说明 |
|----------|----------|------|
| `time` | `now()`, `strToTime()` | 时间对象 |
| `array` | `[1, 2, 3]` | 数组 |
| `map` | `{"a": 1}` | 映射 |
| `string` | `"hello"` | 字符串 |
| `bytes` | `bytes(s)` | 字节数组 |
| `error` | `error(msg)` | 错误对象 |
| `charCode` | `charCode(source)` | 编译代码对象 |
| `mutex` | `mutex()` | 互斥锁 |
| `database` | `dbConnect()` | 数据库连接 |
| `StatusResult` | `genResp()` | 响应结果对象 |
| `StringBuilder` | `stringBuilder()` | 字符串构建器 |
| `BytesBuffer` | `bytesBuffer()` | 字节缓冲区 |
| `ObjectRef` | `new()` | 引用类型 |
| `MutableString` | `new("string", v)` | 可变字符串 |
| `Seq` | `seq()` | 序列迭代器 |
| `HttpReq` | (微服务内置) | HTTP请求对象 |
| `HttpResp` | (微服务内置) | HTTP响应对象 |
| `Reader` | `openFile()` | 文件读取器 |
| `Writer` | `openFile()` | 文件写入器 |
| `File` | `openFile()` | 文件对象 |
| `Queue` | `new("queue")` | 队列 |
| `Stack` | `new("stack")` | 栈 |
| `OrderedMap` | `new("orderedmap")` | 有序映射 |
| `MapArray` | `new("maparray")` | 键值对数组 |
| `BigInt` | `new("bigint", s)` | 大整数 |
| `BigFloat` | `new("bigfloat", s)` | 大浮点数 |
| `WebSocket` | (WebSocket模块) | WebSocket连接 |
| `JsVm` | `newjs()` | JavaScript虚拟机 |
| `Gel` | `gel()` | GEL脚本引擎 |
| `Excel` | `new("excel")` | Excel操作对象 |

## 16.2 time 对象

时间对象是使用 `now()` 或 `strToTime()` 创建的。

### 创建方式

```charlang
t := now()                           // 当前时间
t2 := strToTime("2024-01-01")        // 从字符串解析
t3 := getNowTimeStamp()              // 时间戳转时间
```

### 属性访问

```charlang
t := now()
t.year()        // 年 (int)
t.month()       // 月 (int)
t.day()         // 日 (int)
t.hour()        // 时 (int)
t.minute()      // 分 (int)
t.second()      // 秒 (int)
t.weekday()     // 星期几 (int)
t.yearDay()     // 一年中第几天 (int)
t.unix()        // Unix时间戳 (int)
t.unixMilli()   // 毫秒时间戳 (int)
t.unixMicro()   // 微秒时间戳 (int)
t.isZero()      // 是否零时间 (bool)
```

### 方法列表

| 方法 | 参数 | 返回值 | 说明 |
|------|------|--------|------|
| `add` | `seconds` (float) | `time` | 加秒数 |
| `sub` | `time` | `time`/`int` | 减去时间或秒数 |
| `addDate` | `year, month, day` (int) | `time` | 加日期 |
| `after` | `time` | `bool` | 是否在参数时间之后 |
| `before` | `time` | `bool` | 是否在参数时间之前 |
| `format` | `layout` (string) | `string` | 格式化时间 |
| `appendFormat` | `bytes, layout` | `bytes` | 格式化并追加到字节数组 |
| `in` | `location` | `time` | 转换为不同时区 |
| `round` | `duration` (int) | `time` | 四舍五入到指定精度 |
| `truncate` | `duration` (int) | `time` | 截断到指定精度 |
| `equal` | `time` | `bool` | 比较两个时间是否相等 |
| `date` | - | `map` | 返回日期组成 {year, month, day} |
| `clock` | - | `map` | 返回时钟组成 {hour, minute, second, nanosecond} |
| `year` | - | `int` | 获取年份 |
| `month` | - | `int` | 获取月份 |
| `day` | - | `int` | 获取日期 |
| `hour` | - | `int` | 获取小时 |
| `minute` | - | `int` | 获取分钟 |
| `second` | - | `int` | 获取秒数 |
| `weekday` | - | `int` | 获取星期几 (0=周日) |

### 时间运算

```charlang
t1 := now()
t2 := t1 + 3600      // 加1小时(秒)
t3 := t1 - 3600      // 减1小时
t4 := t1 - t2        // 差值(秒)

t5 := t1 > t2        // 比较
t6 := t1 < t2
t7 := t1 >= t2
t8 := t1 <= t2
```

## 16.3 array 对象

数组对象的创建方式和常用操作。

### 创建方式

```charlang
arr := [1, 2, 3]
arr2 := []int{}
arr3 := new("array")
```

### 方法列表

| 方法 | 参数 | 返回值 | 说明 |
|------|------|--------|------|
| `len()` | - | `int` | 数组长度 |
| `contains` | `value` | `bool` | 是否包含元素 |
| `indexOf` | `value` | `int` | 元素位置，不存在返回-1 |
| `push` | `value...` | `array` | 末尾添加元素 |
| `pop` | - | `value` | 末尾删除并返回 |
| `unshift` | `value...` | `array` | 头部添加元素 |
| `shift` | - | `value` | 头部删除并返回 |
| `reverse` | - | `array` | 反转数组 |
| `clone` | - | `array` | 浅拷贝 |
| `sort` | - | `array` | 排序(需赋值) |
| `sortReverse` | - | `array` | 降序排序(需赋值) |
| `join` | `separator` | `string` | 拼接为字符串 |
| `slice` | `start, end` | `array` | 切片 |

### 使用示例

```charlang
arr := [3, 1, 4, 1, 5]

// 排序
arr = sort(arr)           // [1, 1, 3, 4, 5]
arr = sortReverse(arr)    // [5, 4, 3, 1, 1]

// 添加元素
arr = append(arr, 6, 7)   // [3, 1, 4, 1, 5, 6, 7]
arr = arr.push(8)         // 另一种方式

// 查找
idx := arr.indexOf(4)     // 2
has := arr.contains(5)   // true

// 切片
sub := arr.slice(1, 3)    // [1, 4]
```

## 16.4 map 对象

映射对象的创建和操作。

### 创建方式

```charlang
m := {"a": 1, "b": 2}
m2 := map[string]int{}
m3 := new("map")
```

### 方法列表

| 方法 | 参数 | 返回值 | 说明 |
|------|------|--------|------|
| `keys()` | - | `array` | 所有键 |
| `values()` | - | `array` | 所有值 |
| `has` | `key` | `bool` | 是否包含键 |
| `get` | `key` | `value` | 获取值 |
| `set` | `key, value` | - | 设置值 |
| `delete` | `key` | - | 删除键值对 |
| `len()` | - | `int` | 键值对数量 |
| `clear` | - | - | 清空所有键值对 |
| `clone` | - | `map` | 浅拷贝 |
| `merge` | `map` | `map` | 合并另一个map |

### 使用示例

```charlang
m := {"name": "张三", "age": 25}

// 获取
name := m["name"]
keys := m.keys()
values := m.values()

// 设置
m["city"] = "北京"
m.set("country", "中国")

// 检查
hasName := m.has("name")
hasName2 := "name" in m

// 删除
delete(m, "age")
```

## 16.5 string 对象

字符串对象的创建和方法。

### 创建方式

```charlang
s := "hello"
s2 := new("string", "hello")
s3 := string(65)        // 从ASCII码
```

### 方法列表

| 方法 | 参数 | 返回值 | 说明 |
|------|------|--------|------|
| `len()` | - | `int` | 字符串长度 |
| `contains` | `sub` | `bool` | 是否包含子串 |
| `startsWith` | `prefix` | `bool` | 是否开头 |
| `endsWith` | `suffix` | `bool` | 是否结尾 |
| `indexOf` | `sub` | `int` | 子串位置 |
| `lastIndexOf` | `sub` | `int` | 最后出现位置 |
| `toUpper()` | - | `string` | 转大写 |
| `toLower()` | - | `string` | 转小写 |
| `trim()` | - | `string` | 去除两端空白 |
| `trimLeft()` | - | `string` | 去除左空白 |
| `trimRight()` | - | `string` | 去除右空白 |
| `split` | `sep` | `array` | 分割为数组 |
| `replace` | `old, new` | `string` | 替换第一个 |
| `replaceAll` | `old, new` | `string` | 替换所有 |
| `substring` | `start, end` | `string` | 子串 |
| `charAt` | `index` | `string` | 指定位置字符 |
| `codePointAt` | `index` | `int` | 字符Unicode码点 |
| `repeat` | `count` | `string` | 重复 |
| `reverse` | - | `string` | 反转 |

### 使用示例

```charlang
s := "Hello World"

// 大小写
upper := s.toUpper()     // "HELLO WORLD"
lower := s.toLower()     // "hello world"

// 查找
idx := s.indexOf("o")    // 4
last := s.lastIndexOf("o") // 7

// 截取
sub := s.substring(0, 5)  // "Hello"
char := s.charAt(0)       // "H"

// 替换
s2 := s.replace("World", "Charlang")  // "Hello Charlang"
s3 := s.replaceAll("o", "0")          // "Hell0 W0rld"
```

## 16.6 error 对象

错误对象的创建和使用。

### 创建方式

```charlang
err := error("错误信息")
err2 := error("错误名称", "错误信息")
```

### 方法列表

| 方法 | 参数 | 返回值 | 说明 |
|------|------|--------|------|
| `toString()` | - | `string` | 错误字符串表示 |
| `getMessage()` | - | `string` | 获取错误消息 |
| `getName()` | - | `string` | 获取错误名称 |

### 使用示例

```charlang
err := error("ValidationError", "name is required")

name := err.getName()    // "ValidationError"
msg := err.getMessage()  // "name is required"
str := err.toString()    // "ValidationError: name is required"

if isError(err) {
    pln("发生错误:", err)
}
```

## 16.7 charCode 对象

动态代码编译对象。

### 创建方式

```charlang
code := charCode(`return 42`)
compiled := code.compile()
```

### 方法列表

| 方法 | 参数 | 返回值 | 说明 |
|------|------|--------|------|
| `compile()` | - | `charCode` | 编译代码 |
| `run()` | `args...` | `value` | 执行代码 |
| `threadRun()` | `args...` | - | 线程中执行 |

### 使用示例

```charlang
// 基本使用
code := charCode(`return 10 + 20`).compile()
result := code.run()  // 30

// 带参数
addCode := charCode(`
    param (a, b)
    return a + b
`).compile()

sum := addCode.run(5, 7)  // 12

// 线程执行
code.threadRun(args...)
```

## 16.8 mutex 对象

互斥锁对象，用于并发控制。

### 创建方式

```charlang
m := mutex()
```

### 方法列表

| 方法 | 说明 |
|------|------|
| `lock()` | 加锁 |
| `unlock()` | 解锁 |

### 使用示例

```charlang
lock := mutex()
counter := new("int", 0)

// 方式1: 手动lock/unlock
lock.lock()
counter = counter + 1
lock.unlock()

// 方式2: defer自动解锁
lock.lock()
defer lock.unlock()
// 临界区代码
```

## 16.9 database 对象

数据库连接对象（需要数据库支持）。

### 创建方式

```charlang
db := dbConnect("sqlite3", "test.db")
db := dbConnect("mysql", "user:pass@tcp(localhost:3306)/dbname")
```

### 方法列表

| 方法 | 参数 | 返回值 | 说明 |
|------|------|--------|------|
| `query` | `sql` | `array` | 查询返回数组 |
| `queryRecs` | `sql` | `array` | 查询返回记录数组 |
| `queryMap` | `sql` | `map` | 查询返回map |
| `queryMapArray` | `sql` | `array` | 查询返回map数组 |
| `queryCount` | `sql` | `int` | 查询数量 |
| `queryFloat` | `sql` | `float` | 查询浮点数 |
| `queryString` | `sql` | `string` | 查询字符串 |
| `exec` | `sql` | `int` | 执行返回影响行数 |
| `close` | - | - | 关闭连接 |

### 使用示例

```charlang
db := dbConnect("sqlite3", "test.db")

// 查询
rows := db.query("SELECT * FROM users")
for row in rows {
    pln(row)
}

// 执行
count := db.exec("INSERT INTO users (name) VALUES ('张三')")

// 关闭
db.close()
```

## 16.10 StringBuilder 对象

高效字符串构建器。

### 创建方式

```charlang
sb := stringBuilder()
```

### 方法列表

| 方法 | 参数 | 返回值 | 说明 |
|------|------|--------|------|
| `write` | `value` | - | 写入值 |
| `writeString` | `s` | - | 写入字符串 |
| `writeBytes` | `b` | - | 写入字节 |
| `toString()` | - | `string` | 转换为字符串 |
| `reset` | - | - | 重置 |

### 使用示例

```charlang
sb := stringBuilder()

for i := 0; i < 10; i++ {
    sb.write(i)
    sb.writeString(",")
}

result := sb.toString()  // "0,1,2,3,4,5,6,7,8,9,"
```

## 16.11 StatusResult 对象

响应结果对象，常用于微服务。

### 创建方式

```charlang
resp := genResp("success", data, requestG)
```

### 方法列表

| 方法 | 参数 | 返回值 | 说明 |
|------|------|--------|------|
| `status()` | - | `string` | 获取状态 |
| `value()` | - | `value` | 获取值 |
| `isValid()` | - | `bool` | 是否有效 |
| `isSuccess()` | - | `bool` | 是否成功 |
| `toString()` | - | `string` | 字符串表示 |

## 16.12 Queue 对象

队列对象（先进先出）。

### 创建方式

```charlang
q := new("queue")
```

### 方法列表

| 方法 | 参数 | 返回值 | 说明 |
|------|------|--------|------|
| `push` | `value` | - | 入队 |
| `pop` | - | `value` | 出队 |
| `front` | - | `value` | 查看队首 |
| `len()` | - | `int` | 队列长度 |
| `isEmpty()` | - | `bool` | 是否为空 |

### 使用示例

```charlang
q := new("queue")

q.push(1)
q.push(2)
q.push(3)

val := q.pop()   // 1
val = q.pop()    // 2

empty := q.isEmpty()  // false
```

## 16.13 Stack 对象

栈对象（后进先出）。

### 创建方式

```charlang
s := new("stack")
```

### 方法列表

| 方法 | 参数 | 返回值 | 说明 |
|------|------|--------|------|
| `push` | `value` | - | 入栈 |
| `pop` | - | `value` | 出栈 |
| `top` | - | `value` | 查看栈顶 |
| `len()` | - | `int` | 栈大小 |
| `isEmpty()` | - | `bool` | 是否为空 |

## 16.14 ObjectRef 对象

引用类型对象，用于在函数间共享可变数据。

### 创建方式

```charlang
ref := new("int", 0)
ref2 := new("string", "hello")
ref3 := new("bool", true)
ref4 := new("float", 3.14)
```

### 方法列表

| 方法 | 参数 | 返回值 | 说明 |
|------|------|--------|------|
| `unref()` | - | `value` | 获取值 |
| `setValueByRef()` | `value` | - | 设置值 |

### 使用示例

```charlang
counter := new("int", 0)

increment := func(c) {
    setValueByRef(c, unref(c) + 1)
}

increment(counter)
increment(counter)
pln(unref(counter))  // 2
```

## 16.15 BytesBuffer 对象

字节缓冲区对象。

### 创建方式

```charlang
buf := bytesBuffer()
```

### 方法列表

| 方法 | 参数 | 返回值 | 说明 |
|------|------|--------|------|
| `write` | `value` | - | 写入 |
| `writeString` | `s` | - | 写入字符串 |
| `writeBytes` | `b` | - | 写入字节 |
| `toBytes()` | - | `bytes` | 转换为字节数组 |
| `toString()` | - | `string` | 转换为字符串 |
| `reset` | - | - | 重置 |
| `len()` | - | `int` | 长度 |

## 16.16 BigInt / BigFloat 对象

大整数和大浮点数对象。

### 创建方式

```charlang
bi := new("bigint", "12345678901234567890")
bf := new("bigfloat", "3.14159265358979323846")
```

### 方法列表

| 对象 | 方法 | 说明 |
|------|------|------|
| BigInt | `add()`, `sub()`, `mul()`, `div()` | 基本运算 |
| BigInt | `mod()`, `pow()`, `sqrt()` | 模运算、幂运算、平方根 |
| BigInt | `toString()` | 转换为字符串 |
| BigFloat | `add()`, `sub()`, `mul()`, `div()` | 基本运算 |
| BigFloat | `pow()`, `sqrt()`, `abs()` | 幂运算、平方根、绝对值 |
| BigFloat | `toString()` | 转换为字符串 |
| 两者 | `gt()`, `lt()`, `eq()`, `gte()`, `lte()` | 比较运算 |

## 16.17 HttpReq / HttpResp 对象

HTTP请求和响应对象（微服务内置）。

### HttpReq 属性

```charlang
global requestG

// 常用属性
requestG.Method       // 请求方法 GET, POST 等
requestG.URL          // 请求URL
requestG.RequestURI   // 请求URI
requestG.Host         // 主机
requestG.Header       // 请求头
requestG.Body         // 请求体
requestG.Form         // 表单数据
requestG.PostForm     // POST表单数据
```

### HttpResp 方法

```charlang
global responseG

setRespHeader(responseG, "Content-Type", "text/html")
writeResp(responseG, "Hello")
writeRespHeader(responseG, 200)
```

## 16.18 File / Reader / Writer 对象

文件操作对象。

### 创建方式

```charlang
f := openFile("test.txt", "r")
```

### Reader 方法

| 方法 | 说明 |
|------|------|
| `read(n)` | 读取n字节 |
| `readAll()` | 读取全部 |
| `readLine()` | 读取一行 |
| `close()` | 关闭 |

### Writer 方法

| 方法 | 说明 |
|------|------|
| `write(data)` | 写入 |
| `writeString(s)` | 写入字符串 |
| `flush()` | 刷新缓冲区 |
| `close()` | 关闭 |

### File 方法

| 方法 | 说明 |
|------|------|
| `read(n)` | 读取 |
| `write(data)` | 写入 |
| `seek(offset, whence)` | 移动指针 |
| `stat()` | 获取文件信息 |
| `close()` | 关闭 |

---

# 17. 附录

## 17.1 类型代码参考

| 类型代码 | 类型名称 |
|----------|----------|
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
| 115 | float (别名) |
| 301 | time |
| 302 | charCode |
| 303 | function |
| 304 | builtin_function |
| 311 | time (类型代码) |
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

## 17.2 运算符优先级

从高到低：

| 优先级 | 运算符 |
|--------|--------|
| 1 | `()` 函数调用 `.` 属性访问 `[]` 索引 |
| 2 | `!` `~` `+` `-` (一元) |
| 3 | `*` `/` `%` |
| 4 | `+` `-` (二元) |
| 5 | `<<` `>>` |
| 6 | `<` `<=` `>` `>=` |
| 7 | `==` `!=` |
| 8 | `&` (位与) |
| 9 | `^` (位异或) |
| 10 | `\|` (位或) |
| 11 | `&&` |
| 12 | `\|\|` |
| 13 | `?:` 三元 |
| 14 | `=` `+=` `-=` `*=` `/=` `%=` `&=` `\|=` `^=` `<<=` `>>=` |
| 15 | `,` |

---

*更多内容请参考官方文档 https://topget.org/charlang*
