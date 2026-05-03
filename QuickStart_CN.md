# 察语言快速入门

## 目录

- [1. 简介](#1-简介)
- [2. 安装与运行](#2-安装与运行)
- [3. 第一个程序](#3-第一个程序)
- [4. 基础语法](#4-基础语法)
- [5. 数据类型](#5-数据类型)
- [6. 运算符](#6-运算符)
- [7. 流程控制](#7-流程控制)
- [8. 函数](#8-函数)
- [9. 数组](#9-数组)
- [10. 映射Map](#10-映射map)
- [11. 错误处理](#11-错误处理)
- [12. 常用内置函数](#12-常用内置函数)
- [13. 文件文件操作)
-操作](#13- [14. 命令行参数](#14-命令行参数)
- [15. 模块与导入](#15-模块与导入)
- [16. 并发编程](#16-并发编程)
- [17. 动态代码执行](#17-动态代码执行)
- [18. 微服务开发](#18-微服务开发)
- [19. 下一步](#19-下一步)

---

## 1. 简介

察语言（Charlang）是一种基于Go语言编写的动态脚本语言，语法与Go相似，但去除了强类型限制，增加了更灵活的类型系统和错误处理机制。

**特点：**
- 单文件执行，无需编译
- 纯Go实现，无CGO依赖
- 跨平台支持（Windows、Linux、Mac）
- 丰富的内置函数
- 动态类型
- 支持并发编程
- 内置微服务/Web服务器

---

## 2. 安装与运行

### 下载与安装

从 [GitHub](https://github.com/topxeq/charlang) 或[官网](https://topget.org/charlang)下载对应平台的 `char.exe`（Windows）或 `char`（Linux/Mac）。

### 运行方式

```bash
# REPL交互模式
char

# 运行脚本
char hello.char

# 带参数运行
char script.char arg1 arg2

# 启动Web服务器
char -server -port=:8080 -dir=.
```

---

## 3. 第一个程序

创建文件 `hello.char`：

```charlang
pln("Hello, World!")
```

运行：

```bash
char hello.char
```

输出：`Hello, World!`

---

## 4. 基础语法

### 注释

```charlang
// 单行注释

/*
 * 多行注释
 */
```

### 变量声明

```charlang
// 方法1：使用 := 声明并赋值（推荐）
name := "张三"
age := 25
isStudent := true

// 方法2：使用 var 声明 + 赋值
var x
x = 10

// 方法3：使用 var 声明时直接赋值
var y = 20
```

### 常量

```charlang
const PI = 3.14159
const (
    STATUS_OK = 200
    STATUS_ERR = 500
)

// iota 用于枚举
const (
    RED = iota   // 0
    GREEN         // 1
    BLUE          // 2
)
```

### 作用域

```charlang
a := "全局"

func() {
    a := "局部"  // 创建新变量，遮蔽外层
    pln(a)       // 输出：局部
}()

pln(a)  // 输出：全局
```

---

## 5. 数据类型

### 基本类型

```charlang
// 整数
n := 42           // int
u := 100u        // uint

// 浮点数
f := 3.14        // float

// 字符串
s := "Hello"     // string

// 布尔值
b := true        // bool

// 字符
c := 'A'         // char (实际是int)

// 未定义
var u1           // undefined
```

### 数组

```charlang
arr := [1, 2, 3, 4, 5]
arr2 := []int{}           // 空数组
arr3 := []string{"a", "b"}
```

### 映射Map

```charlang
m := {"name": "张三", "age": 25}
m2 := map[string]int{}
m2["key"] = 100
```

### 类型判断

```charlang
pln(typeName(123))        // "int"
pln(typeName("hello"))    // "string"
pln(typeName([1,2,3]))   // "array"
pln(typeName({"a":1}))    // "map"

// 类型检查函数
isInt(123)      // true
isString("x")   // true
isArray([1])    // true
isMap({})       // true
isUndefined(u)  // true (变量未赋值时)
```

---

## 6. 运算符

### 算术运算符

```charlang
a := 10 + 5    // 加
b := 10 - 5    // 减
c := 10 * 5    // 乘
d := 10 / 5    // 除
e := 10 % 3    // 取余

// 复合赋值
x := 10
x += 5         // x = 15
x++            // x = 16
x--            // x = 15
```

### 比较运算符

```charlang
a := 5 == 5    // true
b := 5 != 3    // true
c := 5 > 3     // true
d := 5 < 3     // false
e := 5 >= 5    // true
f := 5 <= 5    // true
```

### 逻辑运算符

```charlang
a := true && false   // false
b := true || false   // true
c := !true           // false
```

### 位运算符

```charlang
a := 0xFF & 0x0F    // 0x0F (AND)
b := 0xF0 | 0x0F    // 0xFF (OR)
c := 0xFF ^ 0x0F    // 0xF0 (XOR)
d := 1 << 4          // 16 (左移)
e := 16 >> 2         // 4 (右移)
f := 0xFF &^ 0x0F   // 0xF0 (AND NOT)
```

### 三元运算符

```charlang
result := age >= 18 ? "成年" : "未成年"
```

---

## 7. 流程控制

### if-else

```charlang
score := 85

if score >= 90 {
    pln("优秀")
} else if score >= 60 {
    pln("及格")
} else {
    pln("不及格")
}
```

### for循环

```charlang
// 经典for循环
for i := 0; i < 5; i++ {
    pln(i)
}

// 条件循环
i := 0
for i < 5 {
    pln(i)
    i++
}

// 无限循环
for {
    if condition {
        break
    }
}

// for-in 遍历数组
arr := ["a", "b", "c"]
for i, v in arr {
    pln(i, v)  // 0 a, 1 b, 2 c
}

// for-in 遍历范围
for i in 5 {  // 0,1,2,3,4
    pln(i)
}
```

### break与continue

```charlang
for i := 0; i < 10; i++ {
    if i == 3 {
        continue  // 跳过i=3
    }
    if i == 7 {
        break     // 退出循环
    }
    pln(i)
}
```

---

## 8. 函数

### 基本函数

```charlang
// 定义函数
add := func(a, b) {
    return a + b
}

// 调用
result := add(10, 20)  // 30
```

### 多返回值

```charlang
divide := func(a, b) {
    if b == 0 {
        return 0, "除数不能为零"
    }
    return a / b, ""
}

result, err := divide(10, 2)
if err != "" {
    pln("错误:", err)
} else {
    pln("结果:", result)
}
```

### 可变参数

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

### 闭包

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

### 递归

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

## 9. 数组

### 创建与访问

```charlang
arr := [1, 2, 3, 4, 5]

// 访问元素
pln(arr[0])    // 1
pln(arr[len(arr)-1])  // 5

// 修改元素
arr[0] = 100
```

### 常用操作

```charlang
arr := [1, 2, 3]

// 添加元素
arr = append(arr, 4, 5)

// 切片
slice := arr[1:3]  // [2, 3]

// 长度
pln(len(arr))  // 5

// 包含检查
has3 := contains(arr, 3)  // true

// 遍历
for i, v in arr {
    pln(i, v)
}
```

### 排序

```charlang
arr := [3, 1, 4, 1, 5]
sort(arr)           // 升序
sortReverse(arr)    // 降序
```

---

## 10. 映射Map

### 创建与访问

```charlang
m := {"name": "张三", "age": 25}

// 访问
pln(m["name"])    // 张三

// 添加/修改
m["age"] = 26
m["city"] = "北京"

// 删除
delete(m, "city")

// 遍历
for k, v in m {
    pln(k, v)
}

// 获取所有键
keys := m.keys()

// 获取所有值
values := m.values()
```

---

## 11. 错误处理

### try-catch-finally

```charlang
try {
    result := riskyOperation()
    pln("成功:", result)
} catch err {
    pln("捕获错误:", err)
} finally {
    pln("总是执行")
}
```

### throw与error

```charlang
try {
    if invalid {
        throw("Something went wrong")
    }
} catch err {
    pln("错误:", err)
}

// 创建错误值
err := error("custom error message")
if isError(err) {
    pln("是错误")
}
```

---

## 12. 常用内置函数

### 输出函数

```charlang
pln("Hello")           // 打印换行
pr("Hello")            // 打印不换行
pl("Value: %v", 123)   // 格式化打印
plt(x)                 // 打印带类型
```

### 类型转换

```charlang
int("42")              // 字符串转整数
float("3.14")          // 字符串转浮点
string(123)            // 转字符串
bool(1)                // 转布尔
char(65)               // 整数转字符 'A'
uint(42)               // 转无符号整数
bytes("ABC")           // 字符串转字节数组
chars("ABC")           // 字符串转字符数组
```

### 字符串函数

```charlang
s := "Hello World"

len(s)                   // 长度
strContains(s, "Hello")  // 是否包含
strStartsWith(s, "Hello") // 是否开头
strEndsWith(s, "World")  // 是否结尾
strIndex(s, "o")         // 查找位置
strSplit(s, " ")         // 分割
strReplace(s, "World", "Charlang")  // 替换
strToUpper(s)            // 转大写
strToLower(s)            // 转小写
strTrim(s)               // 去除空白
strRepeat(s, 3)          // 重复
strJoin(["a","b"], "-")  // 拼接
```

### 编码函数

```charlang
md5("hello")            // MD5哈希
sha256("hello")         // SHA256哈希
base64Encode("abc")     // Base64编码
base64Decode("YWJj")    // Base64解码
urlEncode("a b")         // URL编码
urlDecode("a%20b")      // URL解码
```

### 时间函数

```charlang
now()                   // 当前时间
getNowStr()             // 当前时间字符串
getNowTimeStamp()       // 时间戳(秒)
timeAddSecs(now(), 60)  // 加秒
formatTime(now(), "2006-01-02 15:04:05")  // 格式化
strToTime("2024-01-01") // 字符串转时间
```

---

## 13. 文件操作

**注意：`saveText` 参数顺序是（内容，路径）**

```charlang
// 读取文件
content := loadText("file.txt")

// 写入文件（内容在前，路径在后！）
saveText("Hello World", "output.txt")

// 检查文件存在
if fileExists("file.txt") {
    pln("文件存在")
}
```

---

## 14. 命令行参数

```charlang
global argsG

pln("参数个数:", len(argsG))

for i, v in argsG {
    pln(i, v)
}

// 安全获取参数
name := getParam(argsG, 1, "默认值")

// 获取开关参数
port := getSwitch(argsG, "-port=", "8080")
hasVerbose := ifSwitchExists(argsG, "-v")
```

### 常用命令行开关

| 参数 | 说明 |
|------|------|
| `-e="code"` / `-e "code"` | 直接执行代码（不做额外处理） |
| `-check` / `-parse` | 语法检查（不执行） |
| `-cmd=code` | 执行代码（支持 `-urlDecode` 和加密代码） |
| `-clip` | 从剪贴板运行 |
| `-pipe` | 从标准输入运行 |
| `-shell` | 启动交互式Shell |
| `-server` | 启动Web服务器 |
| `-compile -output=app.exe` | 编译为可执行文件 |
| `-verbose` | 详细输出模式 |
| `-debug` | 调试模式 |

---

## 15. 模块与导入

### 导入内置模块

```charlang
ex := import("ex")

// 使用模块函数
compiled := ex.compile(sourceCode)
result := ex.runCompiled(compiled, args...)

// 线程执行
ex.threadRunFunc(myFunc, args)
```

---

## 16. 并发编程

### 线程基础

```charlang
// 定义要在线程中执行的函数
worker := func(data) {
    for i := 0; i < 5; i++ {
        pln("Worker:", i)
        sleep(0.5)
    }
}

// 启动线程
worker.threadRun(data)

// 主线程继续执行
pln("Main thread continues")
```

### 互斥锁（线程安全）

```charlang
// 创建互斥锁
m := mutex()

// 创建共享计数器
counter := new("int", 0)

// 线程函数
worker := charCode(`
    param (counter, mutex, iterations)
    for i := 0; i < iterations; i++ {
        lock(mutex)
        setValueByRef(counter, unref(counter) + 1)
        unlock(mutex)
    }
`).compile()

// 启动多个线程
worker.threadRun(counter, m, 100)
worker.threadRun(counter, m, 100)

// 等待完成
sleep(2)

pln("Final count:", unref(counter))  // 200
```

---

## 17. 动态代码执行

### charCode与compile

```charlang
// 从字符串创建代码
code := charCode(`
    param (a, b)
    return a + b
`).compile()

// 执行代码
result := code.run(10, 20)  // 30
```

### 引用类型

```charlang
// 创建引用
n := new("int", 42)

// 获取值
val := unref(n)

// 修改值
setValueByRef(n, 100)
```

---

## 18. 微服务开发

### 启动服务器

```bash
char -server -port=:8080 -dir=.
```

访问：`http://127.0.0.1:8080/charms/你的服务名`

### 微服务模板

创建文件 `myservice.char`：

```charlang
// 声明全局变量
global requestG
global responseG
global paraMapG
global reqUriG

// 设置响应结束标记
outG := "TX_END_RESPONSE_XT"

// 设置响应头
setRespHeader(responseG, "Content-Type", "application/json; charset=utf-8")

// 获取参数
text := trim(paraMapG["text"])

// 处理业务
if text == "" {
    writeResp(responseG, genResp("fail", "missing text parameter", requestG))
    return outG
}

result := md5(text)

// 返回成功响应
writeResp(responseG, genResp("success", result, requestG))

return outG
```

### 响应函数

```charlang
// 生成JSON响应
genResp(status, value, request)

// 写入响应
writeResp(responseG, content)

// 设置响应头
setRespHeader(responseG, "Content-Type", "text/html; charset=utf-8")
```

---

## 19. 下一步

- 阅读《察语言参考》了解完整函数列表
- 查看 `genscripts/` 目录下的测试示例
- 探索 `reference/scripts/` 下的更多示例
- 尝试编写自己的微服务

**资源链接：**
- 官网：https://topget.org/charlang
- GitHub：https://github.com/topxeq/charlang
- 内置函数：https://topget.org/dc/charlang/funcs
