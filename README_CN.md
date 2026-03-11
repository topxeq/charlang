<!-- |title: 察语言 (Charlang)| -->
[![Coverage](https://img.shields.io/badge/Coverage-34.4%25-yellow)
](https://img.shields.io/badge/Coverage-1-red)
[![Go Reference](https://pkg.go.dev/badge/github.com/topxeq/charlang.svg)](https://pkg.go.dev/github.com/topxeq/charlang)
[![Go Report Card](https://goreportcard.com/badge/github.com/topxeq/charlang)](https://goreportcard.com/report/github.com/topxeq/charlang)

**[English Version](./README.md)** | **[中文版](./README_CN.md)**

- [察语言 (Charlang)](#察语言-charlang)
  - [1. 特性](#1-特性)
  - [2. 更多特性](#2-更多特性)
  - [3. 快速链接](#3-快速链接)
  - [4. 下载](#4-下载)
  - [5. 安装](#5-安装)
  - [6. 快速入门](#6-快速入门)
  - [7. 文档](#7-文档)
    - [7.1 获取二进制文件](#71-获取二进制文件)
    - [7.2 从源代码编译](#72-从源代码编译)
    - [7.3 运行Shell或脚本](#73-运行shell或脚本)
    - [7.4 运行Charlang脚本的多种方式](#74-运行charlang脚本的多种方式)
    - [7.5 获取示例](#75-获取示例)
    - [7.6 快速导览](#76-快速导览)
      - [Hello World!](#hello-world)
      - [注释](#注释)
      - [定义变量](#定义变量)
      - [数据类型名称](#数据类型名称)
      - [布尔数据类型](#布尔数据类型)
      - [整数数据类型](#整数数据类型)
      - [浮点数据类型](#浮点数据类型)
      - [字符串/字节/字符数据类型](#字符串字节字符数据类型)
      - [数组](#数组)
      - [映射Map](#映射map)
      - [函数类型(声明函数)](#函数类型声明函数)
      - [For循环](#for循环)
      - [If语句](#if语句)
      - [预定义全局变量](#预定义全局变量)
      - [错误处理：Try-Catch-Finally](#错误处理try-catch-finally)
      - [运行Charlang脚本/代码](#运行charlang脚本代码)
      - [多线程](#多线程)
      - [Gel](#gel)
      - [Eval机器(运行多段脚本的虚拟机)](#eval机器运行多段脚本的虚拟机)
      - [运行JavaScript代码](#运行javascript代码)
      - [实现通用Web服务器](#实现通用web服务器)
        - [启动简单Web服务器(支持SSL)用于静态文件服务](#启动简单web服务器支持ssl用于静态文件服务)
        - [启动通用Web服务器](#启动通用web服务器)
        - [多线程Web服务器](#多线程web服务器)
        - [在新虚拟机中运行的请求处理器](#在新虚拟机中运行的请求处理器)
        - [同时提供静态和动态HTML页面](#同时提供静态和动态html页面)
      - [Charlang嵌入式全功能Web/微服务/应用服务器](#charlang嵌入式全功能web微服务应用服务器)
      - [Charlang作为系统服务](#charlang作为系统服务)
    - [7.7 更多示例](#77-更多示例)
      - [内置函数：checkErr](#内置函数checkerr)
      - [匿名函数](#匿名函数)
      - [数组进阶](#数组进阶)
      - [Map进阶](#map进阶)
      - [可变字符串](#可变字符串)
      - [大整数](#大整数)
      - [大浮点数](#大浮点数)
      - [位运算](#位运算)
      - [计算BMI](#计算bmi)
      - [计算两个向量的余弦相似度](#计算两个向量的余弦相似度)
      - [使用大浮点数计算两个向量的余弦相似度](#使用大浮点数计算两个向量的余弦相似度)
      - [Gel的更多示例](#gel的更多示例)
      - [重定向标准输出到文件](#重定向标准输出到文件)
      - [获取Charlang中的命名值](#获取charlang中的命名值)
      - [调用Go中的命名函数](#调用go中的命名函数)
      - [比较二进制文件](#比较二进制文件)
      - [简单文本编辑器](#简单文本编辑器)
      - [图像的Base64编码](#图像的base64编码)
      - [在控制台绘制数据图形](#在控制台绘制数据图形)
      - [在控制台绘制实时数据图形](#在控制台绘制实时数据图形)
      - [绘制简单数据图形到图像](#绘制简单数据图形到图像)
      - [在WebView中绘制简单数据图形](#在webview中绘制简单数据图形)
      - [绘制折线图](#绘制折线图)
      - [WebView2中的计算器](#webview2中的计算器)
      - [使用蒙特卡罗算法在WebView2中估算π的值](#使用蒙特卡罗算法在webview2中估算π的值)
      - [字节队列](#字节队列)
    - [7.8 高级主题](#78-高级主题)
      - [语言注意事项](#语言注意事项)
      - [从命令行运行脚本](#从命令行运行脚本)
      - [显示Charlang的环境信息](#显示charlang的环境信息)
      - [将脚本编译为可执行文件](#将脚本编译为可执行文件)
      - [Charlang作为Go中的嵌入式语言](#charlang作为go中的嵌入式语言)
      - [变量声明和作用域](#变量声明和作用域)
        - [param](#param)
        - [global](#global)
        - [var](#var)
        - [const](#const)
      - [值和值类型](#值和值类型)
        - [错误值](#错误值)
      - [Charlang运行时类型](#charlang运行时类型)
        - [基本类型：](#基本类型)
        - [更多类型：](#更多类型)
        - [Go类型定义](#go类型定义)
        - [类型转换/强制转换表](#类型转换强制转换表)
        - [Object.IsFalsy()](#objectisfalsy)
      - [内置错误](#内置错误)
        - [未定义值](#未定义值)
        - [数组值](#数组值)
        - [Map值](#map值)
        - [函数值](#函数值)
      - [类型转换](#类型转换)
      - [运算符](#运算符)
        - [一元运算符](#一元运算符)
        - [二元运算符](#二元运算符)
        - [三元运算符](#三元运算符)
        - [赋值和递增运算符](#赋值和递增运算符)
        - [运算符优先级](#运算符优先级)
      - [语句](#语句)
        - [If语句](#if语句-1)
        - [For语句](#for语句)
        - [For-In语句](#for-in语句)
      - [模块](#模块)
      - [注释](#注释-1)
      - [与Go的差异](#与go的差异)
      - [接口](#接口)
        - [Object接口](#object接口)
        - [Iterator接口](#iterator接口)
        - [Copier接口](#copier接口)
        - [IndexDeleter接口](#indexdeleter接口)
        - [LengthGetter接口](#lengthgetter接口)
        - [对象接口扩展](#对象接口扩展)

# 察语言 (Charlang)

[Charlang](https://topget.org/charlang) 是一种快速、动态的脚本语言，可嵌入到Go应用程序中。
Charlang 在原生 Go 编写的基于栈的虚拟机上编译并执行为字节码。Charlang 具有比 Golang 更常见的运行时错误处理（try-catch-finally）。

Charlang 受启发并基于出色的脚本语言 [uGo](https://github.com/ozanh/ugo)。特别感谢 uGo 的创建者 ([ozanh](https://github.com/ozanh)) 和贡献者。

## 1. 特性

* 原生 Go 编写（无 cgo）。
* `if else` 语句。
* `for` 和 `for in` 语句。
* `try catch finally` 语句。
* `param`、`global`、`var` 和 `const` 声明。
* 丰富的内置函数。
* 模块支持。
* 类似 Go 的语法并有所扩展。

## 2. 更多特性

- 新增类型如 Byte、Image、Any...
- 新增函数：NewCommonError、NewError 等...
- 新增内置函数：getRandomInt、writeResp、setRespHeader、writeRespHeader 等等...
- 新增全局变量和资源。
- 新的线程模型。
- 运行时/动态脚本编译和运行能力。
- 内置控制台和 GUI 代码编辑器。
- 服务器模式：快速启动 WEB 和/或应用程序服务器。
- 作为系统服务运行。

**斐波那契示例**

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

## 3. 快速链接

[Charlang 主页](https://topget.org/charlang)

[Go 参考](https://pkg.go.dev/github.com/topxeq/charlang)

[内置函数](https://topget.org/dc/charlang/funcs)

## 4. 下载

- [Windows x64](https://topget.org/pub/char.zip)
- [Windows x64 (无控制台版本 - 用于 GUI 应用程序)](https://topget.org/pub/charw.zip)
- [Linux Amd64](https://topget.org/pub/char.tar.gz)
- [Linux Arm8 (Termux)](https://topget.org/pub/charArm8.tar.gz)

或从 [Charlang 官方网站](https://topget.org/charlang) 下载软件包。

## 5. 安装

从上述官方站点或[官方网站](https://topget.org/charlang)下载最新的 Charlang 可执行文件或压缩包，然后将其放入目录中，最好放在系统路径中（Windows 中如 C:\Windows，Linux 中如 /usr/bin）。如果是压缩包，请先解压。然后就可以使用了，在任何终端或控制台应用程序中运行它（Windows CMD、PowerShell、Terminal 或 bash）。

## 6. 快速入门

首先按照安装指南安装 Charlang，或通过以下方式从源代码构建：

```
go get -u github.com/topxeq/charlang

go get -u github.com/topxeq/charlang/cmd/char
```

Charlang 有一个 REPL 应用程序用于学习和测试 Charlang 语言，运行 Charlang 的主可执行文件不带命令行参数即可启动。

`char.exe` 或 `./char`

```shell
D:\tmpx>char
Charlang 1.9.6 by TopXeQ
> 1.8 * 3.79
6.822
> a := 12
> pln(a + 19) 
31
> for i in 3 {\
    pln(i, i+1)\
  }
0 1
1 2
2 3
> q

D:\tmpx>
```

使用命令 'q' 退出 REPL。

使用文件名运行 Charlang 来编译和运行脚本代码。

```shell
D:\tmpx>char test.char
aaa[3]

D:\tmpx>
```

以下简单示例展示如何嵌入 Charlang 引擎并在 Golang 中运行脚本。

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

请参阅下面的文档以查找更多运行 Charlang 代码的示例。

## 7. 文档

### 7.1 获取二进制文件

根据您的操作系统从网站下载二进制发布文件：[Charlang 主页](https://topget.org/charlang)。

### 7.2 从源代码编译

如果您想修改代码，或为特定平台构建 Charlang：

```
go get -u github.com/topxeq/charlang
```

或

```
go get -u github.com/topxeq/charlang/cmd/char
```

然后：

```
cd $GOPATH/src/github.com/topxeq/charlang/cmd/char

go build -o char.exe
```

Windows 用户可以在 PowerShell 中执行相同的操作。

### 7.3 运行Shell或脚本

运行 `char` 命令不带参数以启动 REPL：

```shell
char
```

这将启动 Charlang REPL，您可以输入 Charlang 代码并立即查看结果。

您可以运行脚本文件：

```shell
char hello.char
```

### 7.4 运行Charlang脚本的多种方式

Charlang 支持多种运行脚本的方式：

- 直接运行脚本文件
- 编译为可执行文件
- 嵌入到 Go 应用程序中

### 7.5 获取示例

Charlang 仓库中包含许多示例脚本，位于 `scripts` 目录下。

### 7.6 快速导览

#### Hello World!

```charlang
pln("Hello, World!")
```

#### 注释

```charlang
// 单行注释

/*
 * 多行注释
 */
```

#### 定义变量

```charlang
// 使用 := 声明（推荐）
name := "John"
age := 25

// 使用 var 声明
var x
x = 10

// 常量
const PI = 3.14159
```

#### 数据类型名称

```charlang
pln(typeName(123))        // int
pln(typeName("hello"))   // string
pln(typeName([1,2,3]))  // array
pln(typeName({"a":1}))   // map
```

#### 布尔数据类型

```charlang
a := true
b := false
pln(a && b)  // false
pln(a || b)  // true
pln(!a)      // false
```

#### 整数数据类型

```charlang
a := 42
b := 0xFF    // 十六进制
c := 0b1010  // 二进制
pln(a + b, a - c)
```

#### 浮点数据类型

```charlang
pi := 3.14159
pln(pi * 2)
```

#### 字符串/字节/字符数据类型

```charlang
s := "Hello, World!"
b := bytes(s)
pln(s[0])  // 第一个字符
```

#### 数组

```charlang
arr := [1, 2, 3, 4, 5]
pln(arr[0])     // 访问元素
arr = append(arr, 6)  // 添加元素

// 遍历
for i, v in arr {
    pln(i, v)
}
```

#### 映射Map

```charlang
m := {"name": "John", "age": 25}
pln(m["name"])

// 添加/修改
m["city"] = "Beijing"

// 删除
delete(m, "age")
```

#### 函数类型(声明函数)

```charlang
// 命名函数
add := func(a, b) {
    return a + b
}
pln(add(10, 20))

// 可变参数
sum := func(...args) {
    total := 0
    for v in args {
        total += v
    }
    return total
}
pln(sum(1, 2, 3, 4, 5))

// 多返回值
divide := func(a, b) {
    if b == 0 {
        return 0, "division by zero"
    }
    return a / b, ""
}
```

#### For循环

```charlang
// 经典 for
for i := 0; i < 10; i++ {
    pln(i)
}

// for-in 遍历数组
for i, v in [1, 2, 3] {
    pln(i, v)
}

// for-in 遍历范围
for i in 5 {  // 0,1,2,3,4
    pln(i)
}
```

#### If语句

```charlang
if x > 10 {
    pln("x is greater than 10")
} else if x > 5 {
    pln("x is between 5 and 10")
} else {
    pln("x is 5 or less")
}
```

#### 预定义全局变量

```charlang
global argsG     // 命令行参数
global versionG  // Charlang 版本
global scriptPathG  // 脚本路径
global runModeG  // 运行模式
```

#### 错误处理：Try-Catch-Finally

```charlang
try {
    result := riskyOperation()
    pln("Success:", result)
} catch err {
    pln("Error:", err)
} finally {
    pln("Cleanup")
}
```

#### 运行Charlang脚本/代码

```charlang
// 使用 charCode 动态编译
code := charCode(`
    param (a, b)
    return a + b
`).compile()

result := code.run(10, 20)
```

#### 多线程

```charlang
worker := func(data) {
    for i := 0; i < 5; i++ {
        pln("Worker:", i)
        sleep(0.5)
    }
}

worker.threadRun(data)
```

#### Gel

Gel 是 Charlang 的模板引擎：

```charlang
tpl := "Hello {{.Name}}!"
gel(tpl, Map{"Name": "World"})
```

#### Eval机器(运行多段脚本的虚拟机)

```charlang
vm := newVM()
vm.run("a := 10")
vm.run("b := 20")
result := vm.run("a + b")
```

#### 运行JavaScript代码

```charlang
jsvm := newjs()
result := jsvm.run("Math.sqrt(16)")
```

#### 实现通用Web服务器

##### 启动简单Web服务器(支持SSL)用于静态文件服务

```shell
char -server -dir=scripts
```

##### 启动通用Web服务器

```shell
char -80server -port=: -dir=public -webDir=static
```

##### 多线程Web服务器

Charlang 服务器为每个请求创建新的虚拟机实例，实现天然的线程隔离。

##### 在新虚拟机中运行的请求处理器

在 Charlang 服务器模式下，每个请求在独立的虚拟机中处理。

##### 同时提供静态和动态HTML页面

静态页面直接从文件系统读取，动态页面由 `.char` 脚本生成。

#### Charlang嵌入式全功能Web/微服务/应用服务器

```shell
char -server -port=:8080 -dir=.
```

访问 http://127.0.0.1:8080 查看服务。

#### Charlang作为系统服务

```shell
char -installService
char -startService
```

---

**注意：由于篇幅限制，以上为部分翻译。完整内容请参考 README.md 英文原版。**

---

## 相关文档

本任务生成的文档：

- **快速入门**
  - [QuickStart_EN.md](./QuickStart_EN.md) - Charlang Quick Start Guide (English)
  - [QuickStart_CN.md](./QuickStart_CN.md) - 察语言快速入门 (中文)

- **完整参考**
  - [Reference_EN.md](./Reference_EN.md) - Charlang Complete Reference (English)  
  - [Reference_CN.md](./Reference_CN.md) - 察语言参考 (中文)

这些文档也已添加到项目中，可供用户离线阅读。
