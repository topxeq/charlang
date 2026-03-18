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
  - [性能基准测试](#性能基准测试)
  - [附加文档 (中英文双语)](#附加文档-中英文双语)
  - [4. 下载](#4-下载)
  - [5. 安装](#5-安装)
  - [6. 快速入门](#6-快速入门)
  - [7. 文档](#7-文档)
    - [7.1 获取二进制文件](#71-获取二进制文件)
    - [7.2 从源代码编译](#72-从源代码编译)
    - [7.3 开始运行Shell或脚本](#73-开始运行shell或脚本)
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
        - [选择器和索引器](#选择器和索引器)
      - [语句](#语句)
        - [If语句](#if语句-1)
        - [For语句](#for语句)
        - [For-In语句](#for-in语句)
      - [模块](#模块)
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

## 性能基准测试

Charlang 作为解释型脚本语言展现出优秀的性能表现。以下是与 Go 和 Python 的对比：

### 迭代斐波那契性能

| n | Charlang | Go | Python | 最优 |
|---|----------|-----|--------|------|
| fib(100000) | **6 ms** | 0 ms | 60 ms | Go |

**关键发现**: Charlang 迭代性能接近 Go，**比 Python 快 10 倍**。

### 递归斐波那契性能

| n | Charlang | Go | Python | 最优 |
|---|----------|-----|--------|------|
| fib(35) | 1299 ms | **32 ms** | 802 ms | Go |
| fib(40) | 14420 ms | **360 ms** | 8917 ms | Go |

**关键发现**: Go 递归比 Charlang 快约 40 倍（解释器开销）。

### 性能总结

| 场景 | Charlang 表现 |
|------|--------------|
| 迭代运算 | 接近 Go，比 Python 快 10 倍 |
| 字符串操作 | 与 Go 相当 |
| 数组/Map 操作 | 高效 |
| 递归运算 | 有解释器开销，建议使用迭代 |

📖 **详细报告**: [docs/benchmark/](./docs/benchmark/)

## 附加文档 (中英文双语)

- **[QuickStart_EN.md](./QuickStart_EN.md)** - Charlang 快速入门指南 (英文)
- **[QuickStart_CN.md](./QuickStart_CN.md)** - 察语言快速入门 (中文)
- **[Reference_EN.md](./Reference_EN.md)** - Charlang 完整参考 (英文)
- **[Reference_CN.md](./Reference_CN.md)** - 察语言参考 (中文)

## 4. 下载

- [Windows x64](https://topget.org/pub/char.zip)
- [Windows x64 (无控制台版本 - 用于 GUI 应用程序)](https://topget.org/pub/charw.zip)
- [Linux Amd64](https://topget.org/pub/char.tar.gz)
- [Linux Arm8 (Termux)](https://topget.org/pub/charArm8.tar.gz)

或从 [Charlang 官方网站](https://topget.org/charlang) 下载软件包。

## 5. 安装

### 5.1 使用脚本快速安装

**Linux/macOS:**

使用 curl:

```bash
curl -fsSL https://raw.githubusercontent.com/topxeq/charlang/main/scripts/install.sh | bash
```

或使用 wget:

```bash
wget -qO- https://raw.githubusercontent.com/topxeq/charlang/main/scripts/install.sh | bash
```

**Windows (PowerShell):**

下载安装程序:

```powershell
Invoke-WebRequest -Uri "https://raw.githubusercontent.com/topxeq/charlang/main/scripts/install.ps1" -OutFile "install.ps1"
```

运行安装程序（如需绕过执行策略）:

```powershell
Set-ExecutionPolicy -ExecutionPolicy Bypass -Scope Process -Force; .\install.ps1
```

或单行命令:

```powershell
Set-ExecutionPolicy -ExecutionPolicy Bypass -Scope Process -Force; iex (iwr "https://raw.githubusercontent.com/topxeq/charlang/main/scripts/install.ps1")
```

**注意:** 如果看到"在此系统上禁止运行脚本"的错误，上面的命令会临时绕过当前会话的执行策略。

### 5.2 手动安装

从上述链接或 [官方网站](https://topget.org/charlang) 下载最新的 Charlang 可执行文件或压缩包，然后将其放入目录中，最好放在系统路径中（Windows 中如 C:\Windows，Linux 中如 /usr/bin）。如果是压缩包，请先解压。然后就可以使用了，在任何终端或控制台应用程序中运行它（Windows CMD、PowerShell、Terminal 或 bash）；

### 5.3 更新 Charlang

安装后，您可以使用内置的自我更新功能轻松更新到最新版本：

```bash
char -updateSelf
```

这将自动：
1. 检查最新版本
2. 下载适合您操作系统/架构的二进制文件
3. 备份当前可执行文件
4. 替换为新版本

## 6. 快速入门

首先按照安装指南安装 Charlang，或通过以下方式从源代码构建：

`go get -u github.com/topxeq/charlang`

`go get -u github.com/topxeq/charlang/cmd/char`

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

如果您想修改代码，或为特定平台构建 Charlang，

```shell
go get -u github.com/topxeq/charlang
```

或

```shell
cd $GOPATH/src/github.com/topxeq

git clone https://github.com/topxeq/tkc

git clone https://github.com/topxeq/charlang

cd $GOPATH/src/github.com/topxeq/charlang/cmd

go install
```

### 7.3 开始运行Shell或脚本

下载后，从 zip 文件中解压可执行文件，放入目录中，最好放在系统路径中。

然后在终端/控制台中输入 'char' 启动交互式命令行 shell 界面。您也可以使用类似 'char test.char' 或 'char -example basic.char' 的命令运行某些脚本。这里的 'test.char' 是源代码文件（UTF-8 编码，纯文本格式）。

使用命令行开关 '-view' 将显示脚本的源代码而不是运行它。

### 7.4 运行Charlang脚本的多种方式

示例：

- 从源文件运行：`char d:\scripts\test.char`
- 将剪贴板中的文本作为脚本源运行：`char -clip`
- 从远程服务器运行：`char -remote http://replacewithyourdomain.com/script/abc.char`
- 运行示例代码：`char -example basic.char`
- 从 Golang 源目录运行：`char -gopath basic.char`
- 从本地脚本目录运行：在用户主目录下的 'char' 子目录中放置配置文件 local.cfg，内容如 `c:\scripts`，然后 `char -local basic.char` 将运行 'c:\script\basic.char'
- 从云端/网络运行：在用户主目录下的 'char' 子目录中放置配置文件 cloud.cfg，内容如 `http://script.my.com/`，然后 `char -cloud basic.char` 将等同于 `char -remote http://script.my.com/basic.char`
- 选择脚本（或在控制台模式下输入脚本文件路径）来运行：`char -selectScript`
- 在内置控制台（多行）编辑器中加载脚本：`char -cedit d:\scripts\test.char`，然后按 Ctrl-Q 运行，Ctrl-X 退出
- 在内置 GUI 编辑器中加载脚本：`char -edit d:\scripts\test.char`，然后在编辑器中修改或运行它（点击 'Quick Run' 按钮），在 Windows(WebView2) 上
- 在 CharEd IDE(Lazarus) 中运行，可从官方网站下载，在 Windows 软件包中
- 将其用作嵌入式脚本语言引擎，在任何 Golang 兼容的语言中
- 通过 Charlang 编译的 DLL，用其他语言运行

### 7.5 获取示例

文档中以文件名标记的所有源代码示例可以通过以下命令行获取或运行：

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

您可以浏览 `https://topget.org/dc/c/charlang/example/basic.char` 在在线文本编辑器中查看源代码。

使用命令行开关 '-viewPage' 和 '-example' 也会在系统默认浏览器中显示在线代码页面。

### 7.6 快速导览

#### Hello World!

文件：[example001.char](https://topget.org/dc/c/charlang/example/example001.char)

```go
// 函数 'pln' 与其他语言中的 'println' 相同
pln("Hello world!")

```

函数 'pln' 与其他语言中的 'println' 相同。pln 使用其参数的默认格式进行格式化并写入标准输出。

在 Charlang 中，支持注释。您可以使用 `//` 或 `/* ... */` 来引导注释。

脚本的输出结果：

```shell
C:\Users\Administrator>char -example example001.char
Hello world!

C:\Users\Administrator>

```

#### 注释

文件：[example002.char](https://topget.org/dc/c/charlang/example/example002.char)

如上所述，像 Golang 一样，Charlang 支持行注释 (//...) 和块注释 (/* ... */)。代码中的注释将被编译器和虚拟机忽略。您可以在许多文本或源代码编辑器中使用 Ctrl+/ 组合键来切换该行是否被注释。

```go
/*
  多行块注释 - 1
  多行块注释 - 2
  多行块注释 - 3
*/

a := 8    // 行注释

// 在新行开始的行注释
pln("a:", a)

```

输出：

```shell
C:\Users\Administrator>char -example example002.char
a: 8

```

#### 定义变量

文件：[example003.char](https://topget.org/dc/c/charlang/example/example003.char)

```go
// 在使用前定义变量
var a

a = 1

pln(a)

// 将另一种类型的值赋给 a
a = "abc"

pln(a)

// 在一步中定义并赋值
b := true

pln(b)

// 不能对同一变量再次使用 :=
// 下一行将导致错误
// b := 1.1

```

结果：

```shell
C:\Users\Administrator>char -example example003.char
1
abc
true

```

注意，与 Golang 不同，变量可以被赋予不同类型的值。

在一对括号内的一行中声明多个变量。

```go
var (a, b, c)

...
```

#### 数据类型名称

文件：[example004.char](https://topget.org/dc/c/charlang/example/example004.char)

```go

a := 3 // 将一个整数(int)值赋给变量 'a'

// 函数 'pl' 等同于其他语言中的 printf 函数，
// 后面跟着一个额外的换行符 "\n"
// 转换字符与 Golang 相同，
// '%T' 用于输出值的类型，'%v' 是任何值的通用输出格式
pl("[%T] %v", a, a)

// 不使用 '%T'（它将输出 Golang 中的原生类型，Charlang 是用 Golang 编写的）
// 函数 'typeOf' 通常用于在 Charlang 中获取变量的类型名称
pl("[%v] %v", typeOf(a), a)

```

输出：

```shell
C:\Users\Administrator>char -example example004.char
[charlang.Int] 3
[int] 3

```

#### 布尔数据类型

文件：[example005.char](https://topget.org/dc/c/charlang/example/example005.char)

```go

// 布尔值

b := true

// 函数 'prf' 与 C/C++/Golang 中的 'printf' 相同
prf("[%v] %v\n", typeOf(b), b)

c := false

prf("[%T] %v\n", c, c)

prf("!b = %v\n", !b) // 运算符与 Golang 和许多其他语言相同

prf("b == c: %v\n", b == c)

prf("1 > 14: %v\n", 1 > 14)

prf("b == true: %v\n", b == true)

prf("b && c: %v\n", b && c)

prf("b || c: %v\n", b || c)

```

输出：

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

#### 整数数据类型

文件：[example006.char](https://topget.org/dc/c/charlang/example/example006.char)

```go

// 整数

c1 := 19

c2 := 18

pln(c1 + c2/3)

pl("%v, %v", typeOf(c1), c1)

pl("%T, %v", c1+c2, c1+c2)
pl("%v, %v", typeOf(c2/3), c2/3)
pl("%v, %v", typeOf(c1+c2/3), c1+c2/3)
pl("%T, %v", (c1+c2/3)*6, (c1+c2/3)*6)

```

输出：

```shell
C:\Users\Administrator>char -example example006.char
25
int, 19
charlang.Int, 37
int, 6
int, 25
charlang.Int, 150

```

#### 浮点数据类型

文件：[example007.char](https://topget.org/dc/c/charlang/example/example007.char)

```go

// 浮点数

f1 := 1.32

pl("%v, %v", typeOf(f1), f1)

previus_f1 := f1

f1 = f1 * 0.8

// 函数 'pr' 与其他语言中的 'print' 相同
pr(previus_f1, "*", 0.8, "=", f1)

pln()

f2 := 0.93
f2 /= 0.3

pr(0.93, "/", 0.3, "=", f2, "\n")

```

输出：

```shell
C:\Users\Administrator>char -example example007.char
float, 1.32
1.32*0.8=1.056
0.93/0.3=3.1

```

#### 字符串/字节/字符数据类型

文件：[example008.char](https://topget.org/dc/c/charlang/example/example008.char)

```go

// 字符串、字节和字符

s1 := "abc"

// 连接字符串
s2 := s1 + "3"

// 函数 'plt' 将输出值及其 Charlang 类型
plt(s2)

pln(s1, "+", "3", "=", s2)

s5 := "上善若水"

// 函数 'plt' 将输出值及其内部(Golang)类型
plo(s5)

s6 := bytes(s5)

// s6 将是一个字节数组
pln("s6:", s6)

// t 将是一个 utf-8 字符（Golang 中的 rune）
t := char(5)

plo(t)

// s7 将是字符数组
// 在这个例子中，将是 4 个 unicode 字符，每个有 3 个字节
s7 := chars(s5)

plt(s7)

// s5(string) 的切片将是只有一个 ASCII(0-255) 字符的字符串
pl("s5[1:2] = %v(%#v)", s5[1:2], s5[1:2])

// s6(bytes, 即字节数组) 的切片将是只包含一个项的字节数组
pl("s6[1:2] = %v(%#v)", s6[1:2], s6[1:2])

// s7(chars, 即字符数组) 的切片将是只包含一个项的字符数组
pl("s7[1:2] = %v(%#v)", s7[1:2], s7[1:2])

// 将 utf-8 字符转换为字符串
pl("string(s7[1:3]) = %v(%#v)", string(s7[1:3]), string(s7[1:3]))

// 将 utf-8 字符转换为字节，然后转换为字符串，效果同上
pl("string(bytes(string(s7[1:3]))) = %v(%#v)", string(bytes(string(s7[1:3]))), string(bytes(string(s7[1:3]))))

// 输出 string、bytes 和 chars 的第一项，作为单个字符
pl("%c", s5[1])
pl("%c", s6[1])
pl("%c", s7[1])

// 输出 string、bytes 和 chars 的第一项，带有其值和类型
pl("%T, %#v", s5[1], s5[1])
pl("%v, %#v", typeOf(s6[1]), s6[1])
pl("%T, %#v", s7[1], s7[1])

// 使用 'for' 循环遍历字符串
for i := 0; i < len(s5); i++ {
	pl("%v: %v, %v", i, typeOf(s5[i]), s5[i])
}

// 使用 'for-in' 循环遍历字符串
for i, v in s5 {
	pl("%v: %v, %v", i, typeOf(v), v)
}

// 遍历字符
for i, v in s7 {
	// 函数 'typeName' 等同于 'typeOf'
	pl("%v: %v, %v", i, typeName(v), v)
}

```

输出：

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

#### 数组

```go
// 声明一个数组
a := [1, 2, 3, "abc", 12.3]

println("a:", a)

println("a[2]:", a[2])

println("length of a:", len(a))

// 重新赋值数组变量
a = [1, 2]

// 追加值
a = append(a, "abc")

// 数组项可以是任何类型，甚至是另一个数组
b := ["xyz", 16, a]

pln("b:", b)

```

输出：

```shell
a: [1, 2, 3, "abc", 12.3]
a[2]: 3
length of a: 5
b: ["xyz", 16, [1, 2, "abc"]]
```

有关数组类型的更多信息，请参阅更多示例部分中的数组示例。

#### 映射Map

在 Charlang 中，map 是一组键值对，其中键是字符串，值可以是任何值类型。可以使用索引器 `[]` 或选择器 '.' 运算符访问 map 的值。

```go
// 声明一个空 map
a := {}

// 所有键都将转换为字符串类型，值保持其原始类型
a["Num"] = 3
a[5] = "abc"
a[-1] = true
a["ary"] = [1, "xyz", false]
a[false] = "not true"
a.Item1 = "item 1"

pln(a)

// 长度是 map 中键值对的数量
pl("length of a: %v", len(a))

// 通过点索引
pl("a.Num: %v", a.Num)

// 通过方括号索引
a["Num"]++
pln(a["Num"])

// 切片
pln(a[5][2:3])

a[5] = a[5] + a[5]

// 切片到末尾
a[5] = a[5][1:]
pl("a[5]: %v", a[5])

// 从开头切片
pln(a[5][:2])

// 迭代
for k, v in a {
	println("[", k, "]:", v)
}
  
pln("---")

// 用初始值声明 map
b := {"intItem": 12, "floatItem": 5.6, "boolItem": true, "stringItem": "str1", "arrayItem": ["array", "in", "map"], "mapItem": {"map": 1, "in": "map"}}

plt(b)

pln("---")

c := {}

// 所有键都将转换为字符串类型
c[3] = "3"
c[18] = "abc"
c[-198] = "true"

pl("c: %v", c)

v1 := c[18]

if v1 == undefined {
	println("v1:", v1)
}

// 用不存在的键索引
v2 := c[19]

if v2 == undefined {
	println("v2:", v2)
}

// 删除键值对
delete(c, 18)

println("c:", c)

```  

输出：

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

有关 map 类型的更多信息，请参阅更多示例部分中的 map 示例。

#### 函数类型(声明函数)

函数是 Charlang 中的一种数据类型。

```go
// 声明带参数的函数
f1 := func(a, b, c, d) {
	return a*b + c/d
}

result1 := f1(3, 6, 8, 9)

pln("result1=", result1)

// 可变参数函数（函数接受可变数量的参数）
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

#### For循环

```go
c1 := 0

// 经典 for 循环
for i := 0; i < 5; i++ {
	c1 += i
}

pln(c1 * 3)

// 仅带条件的 for 循环
i := 0

for i < 5 {
	pl("i: %v", i)

	i = i + 1
}

// 无限循环（如果没有 break）

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

输出：

```shell
30
i: 0
i: 1
i: 2
i: 3
i: 4
c: 35
```

#### If语句

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

输出：

```shell
!a
!a true
a is not initialized
b = 1
!b false
!c false
```

#### 预定义全局变量

Charlang 中有一些预定义的全局变量，可以通过 'global' 关键字引用。以下代码将按顺序显示命令行参数：

```go
global argsG

for i, v in argsG {
    pl("[%v] %v", i, v)
}
```

全局预定义变量 'argsG' 在运行 Charlang 主程序时保存命令行参数。argsG 的数据类型是数组，因此我们可以直接使用它而无需其他声明。

如果在 Golang 中将 Charlang 用作库，我们可以传递除预定义之外的其他全局变量。

常见的全局预定义变量包括：

- versionG：Charlang 的当前版本；
- basePathG：Charlang 的基础路径，将是当前用户的主目录，如 c:\Users\Administrator 或服务根目录（Windows 中为 c:\char，Linux 中为 /char）；
- argsG：保存命令行参数的数组类型变量；
- scriptPathG：当前运行的脚本文件路径；
- runModeG：Charlang 的运行模式（script、repl、service、charms、chp、chardc 等）

当作为 WEB/应用/微服务服务器运行时，还有额外的预定义全局变量：

- requestG：HTTP 请求对象，保存请求信息；
- responseG：可以写入响应或设置响应设置的 HTTP 响应对象；
- reqUriG：请求的路由，如 'static/images/img001.png'
- paraMapG：以 map 对象保存 GET/POST 表单值，如 `{"auth": "xxxxx", "input1": "value1"}`

在 Windows 平台上，还有一些额外的预定义全局变量：

- guiG：保存与 WebView2 组件交互的对象，可用于建立 GUI 应用程序。

#### 错误处理：Try-Catch-Finally

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

输出：

```shell
D:\tmp>char -example tryCatch.char
exception: ZeroDivisionError: 
r: undefined
done
```

#### 运行Charlang脚本/代码

- 运行一段 Charlang 代码（以字符串格式），

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

charCode 是用于保存要运行的 Charlang 代码的对象类型，在运行之前编译源代码。可以向代码对象传递各种参数。

输出：

```shell
D:\tmp>char -exam runCode.char
result: 20.5
```

- 传递可变长度参数，

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

- 像函数调用一样运行代码，固定部分的参数带有可变长度参数，

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

#### 多线程

- 第一种方式：

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

- 第二种方式，导入 'ex' 模块并使用 ex.threadRunFunc 函数：

注意：这里还演示了互斥锁的用法以及通过引用传递/设置值。

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

- 第三种方式，在另一个 VM 中运行：

注意：此示例还演示了如何将参数传递给在另一个 VM 中运行的线程。

```go
// 使用 Array 或 Map 传递可能在线程中更改的参数

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

Charlang 中的 Gel 是一个用于封装值或函数的对象。

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

输出：

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

我们也可以使用 'ex' 模块来加载 gel，加载的 gel 已经编译过了。

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

#### Eval机器(运行多段脚本的虚拟机)

演示如何创建新的虚拟机来运行脚本和/或评估结果值。

```go
ev1 := evalMachine("value1", "value2", 2, true)

// 参数可以通过全局变量 inputG 传递
// 以及 argsG，所有值都将转换为字符串
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

// 修改其中一个参数
ev1.eval("inputG[2] = 3.1415926")

rs = ev1.eval(`
return inputG
`)

plt(rs)

```

运行结果：

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

- 修改和/或向参数添加值

```go
// 通过 evalMachine 的评估传递参数
aryT := [[1, 2, 3]]

ev1 := evalMachine(1, 2, aryT)

rs1 := ev1.eval(`
global inputG

ary1 := inputG[2][0]

ary1[0] + ary1[1] + ary1[2]

`)

plt(rs1)

// 更改值
aryT[0][2] = 5

rs := ev1.eval("return ary1[0] + ary1[1] + ary1[2]")

plt(rs)

// 追加值
aryT[0] = append(aryT[0], 18)

rs = ev1.eval(`

ary1 = inputG[2][0]

plt(ary1)

return ary1[0] + ary1[1] + ary1[2] + ary1[3]
`)

plt(rs)

```

输出：

```shell
(int)6
(int)8
(array)[1, 2, 5, 18]
(int)26
```

#### 运行JavaScript代码

演示如何创建 JavaScript 虚拟机来运行 JavaScript 代码和/或评估结果值。

```go
// 初始化 JavaScript 虚拟机
vmT := jsVm()

// 运行一段 JavaScript 代码，并返回最后的评估结果
rs := vmT.run(`
let a = 1.2

var b = a * 2.7

b
`)

// 获取并输出返回结果
pl("result: %v", rs)

// 设置全局值
vmT.set("c", "abc")

// eval 等同于 run
rs2 := vmT.eval(`let d = "" + b + c; d`)

pl("result2: %v", rs2)

// 从 VM 获取全局变量值
value_a := vmT.get("a")

pl("value a: %v(%v)", value_a, typeOf(value_a))

// 设置带参数的函数/委托/回调，然后调用它
sourceT := `
param ...vargs

global inputG

pln(inputG, vargs)

return vargs[0] + vargs[1] + inputG[0]
`

d1 := delegate(sourceT)

d1c := d1.compile(vmT.get("b")) // 传递另一个参数（通过全局变量 inputG，它将是一个数组）

vmT.set("f1", d1c)

rs3 := vmT.run(`let d3 = f1(3, 5); d3`)

pl("result3: %v", rs3)

```

运行结果：

```shell
D:\tmpx>char -exam runJavaScript.char
result: 3.24
result2: 3.24abc
value a: 1.2(float)
[3.24] [3, 5]
result3: 11.24

```

#### 实现通用Web服务器

演示如何实现更灵活的 Web 服务器，支持静态文件、动态页面（如 PHP、JSP、ASPX 等）和微服务。

##### 启动简单Web服务器(支持SSL)用于静态文件服务

```go
// 快速启动静态 web 服务器以服务特定目录下的文件
// 可以设置端口和证书目录
handlerT := httpHandler("static", joinPath(getHomeDir(), "pages"))

if isErr(handlerT) {
	fatalf("failed to create httpHandler: %v", getErrStr(handlerT))
}

muxT := mux()

muxT.setHandler("/", handlerT)

pln("starting http server on port 80(default)...")
checkErr(muxT.threadStartHttpServer())

// 证书文件应该是 server.crt 和 server.key
pln("starting https(SSL) server on port 443...")
plErr(muxT.threadStartHttpsServer("-port=443", "-certDir=."))

for {
	pln(time().format("2006-01-02 15:04:05"), "heartbeat")
	sleep(5.0)
}

```

##### 启动通用Web服务器

```go
// 一个非常简单的 web 服务器
// 要测试服务器，打开浏览器，浏览到 URL 地址：http://127.0.0.1

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

修改 handlerT 中的代码部分以满足 Web 服务器的定制需求。您可以在同一个 Mux 对象中混合静态文件处理器和不同的路由，例如：

```go
muxT.setHandler("/", staticFileHandlerT)
muxT.setHandler("/common", commonHandlerT)
```

##### 多线程Web服务器

另一个示例演示了如何使用互斥锁来避免多个 http 请求的冲突：

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

##### 在新虚拟机中运行的请求处理器

另一个示例演示如何设置处理器在新的 VM 中运行，以及向其传递参数的方式：

```go
// 处理器在单独的 VM 中运行，更安全但有更多的系统资源（和时间）成本/开销
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

##### 同时提供静态和动态HTML页面

根据路由（在 URL 参数中）从文件读取 HTML 模板。

```go
// 演示如何同时提供静态和动态 HTML 页面，注意这不是唯一的方式

staticHandlerT := httpHandler("static", joinPath(getHomeDir(), "pages"))

if isErr(staticHandlerT) {
	fatalf("failed to create static handler: %v", getErrStr(staticHandlerT))
}

dynamicHandlerT := func(requestA, responseA) {
	// 设置适当的响应头
	setRespHeader(responseA, "Access-Control-Allow-Origin", "*")
	setRespHeader(responseA, "Access-Control-Allow-Headers", "*")
	setRespHeader(responseA, "Content-Type", "text/html;charset=utf-8")

	// 从 HTTP 请求获取参数
	paramsT := parseReqForm(requestG)

	// 从查询字符串或 post 表单值获取 'req' 参数
	// 它将用作路由
	reqT := trim(params1["req"])

	// 从指定文件夹获取 HTML 模板
	templateDirT := `c:\test\tmpl`

	templateFileT := joinPath(templateDirT, reqT+".html")

	templateHtmlT := loadText(templateFileT)

	// 对模板进行一些修改
	strReplace(templateHtmlT, "{{field1}}", getNowStr())

	// 将处理后的文本写入 HTTP 响应
	writeResp(responseA, templateHtmlT)

	// 写入 "TX_END_RESPONSE_XT" 以结束响应流/输出
	return "TX_END_RESPONSE_XT"
}

// 提供动态页面
muxT.setHandler("/pages", dynamicHandlerT)

// 将其他路由作为静态页面提供
muxT.setHandler("/", staticHandlerT)

rs := muxT.startHttpServer()

pl("result: %v", rs)

```

#### Charlang嵌入式全功能Web/微服务/应用服务器

&nbsp;

Charlang 的主程序附带一个服务器模式，支持轻量级 WEB/应用/API 一体化服务器。您可以使用以下命令行启动它：

```shell
D:\tmp>char -server -dir=scripts
[2024/08/23 08:08:22] Charlang Server V1.3.3 -port=:80 -sslPort=:443 -dir=scripts -webDir=scripts -certDir=scripts
[2024/08/23 08:08:22] try starting ssl server on :443...
[2024/08/23 08:08:22] try starting server on :80 ...
[2024/08/23 08:08:22] failed to start https: open scripts\server.crt: The system cannot find the file specified.
```

可以看出，Charlang 的服务器模式可以用 '-server' 参数启动，'-port' 参数可用于指定 HTTP 服务端口（请加冒号），'-sslPort' 可用于指定 SSL 端口，'-certDir' 可用于指定 SSL 服务的证书文件目录（应该是两个文件：server.crt 和 server.key），'-dir' 可用于指定服务的根目录，'-webDir' 可用于指定静态页面和资源的 web 服务。这些参数都有默认值，可以在不输入任何参数的情况下查看。

输出信息中的错误是因为没有提供 SSL 证书，SSL 服务将无法启动。添加证书文件即可。

然后打开浏览器访问地址 http://127.0.0.1:80 即可访问用 Charlang 编写的一体化 web 服务。

假设指定目录包含三个文件：charmsIndex.char、charmsTmpl.html 和 charmsApi.char，可以展示 Charlang 建立的应用服务器支持的各种模式。

首先，用浏览器访问 http://127.0.0.1/charmsTmpl.html，这将是访问一般 web 服务，因为 web 目录默认与服务根目录相同。因此，根目录下的静态文件 charmsTmpl.html 将被显示，这是一个示例网页。

![截图](https://topget.org/dc/s/images/pic2719068761.png)

您可以看到网页中 'Please click the button ' 文本后的 "{{text1}}" 标签。这是我们稍后在显示动态网页时要替换的标签。charmsTmpl.html 文件的内容如下：

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

然后我们尝试进行动态网页输出，这类似于 PHP、ASP、JSP 或其他类似框架支持的后台动态渲染网页。然后浏览到 'http://127.0.0.1/charms/charmsIndex'。在 URL 中添加 charms 路径，这是一个虚拟路径，表示服务器在启动时将在根目录中搜索 charmsIndex.char 文件。此代码将输出网页内容。让我们看看 charmsIndex.char 文件内部。

```go
// responseG 是预定义的全局变量，它保存要写入的 HTTP 响应对象
global responseG

// basePathG 是预定义的全局变量，它在启动服务器时保存指定的基础路径
global basePathG

// 将默认的全局返回值变量 outG 设置为字符串 TX_END_RESPONSE_XT
// 如果默认的 Charlang 服务器接收到一个函数来处理请求，返回结果是 TX_END_RESPONSE_XT
// 页面的服务器处理（HTTP 响应的输出）将被终止，否则返回值将作为字符串输出到网页
outG := "TX_END_RESPONSE_XT"

// 获取相应的网页模板(HTML)
// joinPath 函数将多个文件路径合并为一个完整的文件路径
// 第一个参数表示要放入结果的变量，其中 $push 表示堆栈推送
// basePathG 是一个内置全局变量，表示服务的根目录
templatePathT := joinPath(basePathG, `charmsTmpl.html`)

// 将文件加载为文本字符串(HTML)
templateTextT := loadText(templatePathT)

// 将 {{text1}} 标签替换为字母 A
templateTextT = strReplace(templateTextT, "{{text1}}", "A")

// 设置相应的响应头
setRespHeader(responseG, "Content-Type", "text/html; charset=utf-8")

// 将 HTML 写入网页输出
// responseG 也是一个预定义的全局变量，表示要写入的 HTTP/网页输出对象
writeResp(responseG, templateTextT)

// 返回 'TX_END_RESPONSE_XT' 以结束输出
return outG

```

在 Charlang 服务器模型中，每个 HTTP 请求都由一个单独的虚拟机处理，这可以看作是微服务的概念。此示例中的微服务仅替换加载的网页模板中的指定标签并将它们输出到网页。虽然简单，但它已经展示了动态网页的基本原理，即在输出网页之前可以进行必要的和可控的渲染。

让我们浏览到 'http://127.0.0.1/charms/charmsIndex'。它将产生以下结果：

![截图](https://topget.org/dc/s/images/pic362253946.png)

我们可以发现原来的标签确实已被替换为大写字母 A，验证了动态网页的效果。

查看上面的网页模板文件 charmsTmpl.html，一旦点击按钮，将执行 JavaScript 函数 `test`，其中发出 AJAX 请求并使用 `alert` 函数输出请求的结果。这是客户端访问后端 API 服务的典型示例。让我们看看如何实现这个后端 API 服务。以下是同样位于服务器根目录中的 charmsApi.char 文件的内容：

```go
// 声明要在上下文中使用的全局变量
global requestG
global reqNameG
global reqUriG
global responseG
global paraMapG

// 获取当前时间并将其放入变量 t
t := getNowStr()

// 输出参考信息（到服务器的本地控制台，而不是 HTTP 响应）
// 其中 reqNameG 是一个预定义的全局变量，表示服务名称，即访问 URL 的最后一部分
// reqUriG 是一个预定义的全局变量，表示服务路由/路径
// paraMapG 也是一个全局变量，表示 HTTP 请求中包含的查询字符串或表单参数（可以在 GET 或 POST 请求中）
pl(`[%v] reqNameG: %v, reqUriG: %v, paraMapG: %v`, t, reqNameG, reqUriG, paraMapG)

// 设置输出响应头信息（为 JSON 格式）
setRespHeader(responseG, "Content-Type", "text/json; charset=utf-8")

// 将响应状态设置为 200 (HTTP_oK)，表示成功的请求响应
writeRespHeader(responseG, 200)

// 使用 spr 指令组装响应字符串
str1 := spr("The request is: %v, uri: %v, parameters: %v", reqNameG, reqUriG, paraMapG)

// 使用 genJsonResp 生成封装的 JSON 响应，或自行输出其他格式的字符串
respStr := genJsonResp(requestG, "success", str1)

// 写入并输出响应字符串（到网页）
writeResp(responseG, respStr)

// 结束处理并返回 'TX_END_RESPONSE_XT' 以终止响应流的继续输出
return "TX_END_RESPONSE_XT"

```

然后，如果我们点击网页上的按钮 `button1`，我们将得到以下弹出警告：

![截图](https://topget.org/dc/s/images/pic1052413353.png)

这是因为网页 charmsTmpl.html 向位于 `http://127.0.0.1:80/charms/charmsApi` 的服务发出了 AJAX 请求。我们的 Charlang 服务器将找到 charmsApi.char（自动添加 `.char` 文件名后缀）并执行它，因此它将输出我们想要的内容。

现在，一个小而全功能的 WEB/应用/API 一体化服务器的示例已经完全演示完毕。对于一般的小型应用服务来说已经足够了，而且几乎没有外部依赖。部署也非常方便，只需要 Charlang 的主程序和指定目录中的相应 HTML 和脚本文件。

&nbsp;

#### Charlang作为系统服务

Charlang 可以作为系统服务启动，支持 Windows 和 Linux 等操作系统。只要添加命令行参数 '-reinstallService' 来运行 Charlang 主程序，就可以在系统中安装名为 charService 的系统服务（在 Windows 下可以使用计算机管理中的服务管理模块查看）。注意，在操作系统中安装服务通常需要管理员权限。在 Windows 下，您需要以管理员身份打开 CMD 窗口来执行此命令，而在 Linux 下，您需要以 root 身份或使用 sudo 命令执行。

服务启动后，将在服务根目录（Windows 中为 c:\char，Linux 中为 /char）中的 charService.log 文件中记录日志。当服务首次启动时，它将在服务根目录中搜索所有名称类似于 taskXXX.char 的文件（如 task001.char、taskAbc.char 等）并逐个运行它们，并将它们的执行结果（返回值）输出到日志中。这种类型的代码文件称为一次性运行任务文件，通常用于需要启动并运行一次的情况。也可以手动运行命令 'char -restartService' 来重启服务并实现任务重新执行的目标。

还有另一种一次性运行任务文件，它们将在称为 '线程任务' 的单独线程中运行，它们的文件名类似于服务根目录中的 threadTaskXXX.char（如 threadTask001.char、threadTaskAbc.char 等）。这些任务用于需要连续运行的任务，如 WEB 服务器、FRP 服务器或客户端等。如果在运行线程任务时发生某些错误，信息将记录在服务根目录的 'runThreadTask.log' 文件中。

此外，在运行期间，charService 服务每 5 秒检查一次服务根目录。如果有名称类似于 autoRemoveTaskXXX.char 的文件（如 autoRemoveTask001.char、autoRemoveTaskAbc.char 等），这些文件中的代码将立即执行然后删除。这种机制类似于任务队列，允许我们随时将任务添加到队列（放入服务根目录），Charlang 服务将随时执行这些任务。而且由于任务在执行后立即被删除，因此不会重复执行。

与 Charlang 主程序的服务安装、删除、启动、停止和重启相关的命令行参数还包括 '-installService'、'-removeService'、'-startService'、'-stopService'、'-restartService' 等。

任务代码可以参考 task001.char、threadTask001.char、autoRemoveTask001.char 等示例。

- 一次性任务示例

文件：[task001.char](https://topget.org/dc/c/charlang/example/task001.char)

```go
global basePathG

logPathT := joinPath(basePathG, "charService.log")

rs := appendText("\ntask001.char\n", logPathT)

return "task001 returns some result 000"

```

- 线程任务示例

文件：[threadTask001.char](https://topget.org/dc/c/charlang/example/threadTask001.char)

```go
for {
	saveText(getNowStr(), `c:\char\task1.txt`)

	sleep(60.0)
}
```

这将是一个连续循环，每 60 秒将当前时间字符串写入文件。

以下是系统启动时运行 Frpc 客户端的线程任务，

文件：[threadTaskFrpc.char](https://topget.org/dc/c/charlang/example/threadTaskFrpc.char)

```go
appendText(spr("\n[%v] %v %v\n", getNowStr(), "threadTaskFrpc", "starting..."), `c:\logs\frpcTask.log`)

rs := systemCmd(`c:\tools\frp\frpc.exe`, `-c`, `c:\tools\frp\frpc.ini`)

appendText(spr("\n[%v] %v %v\n", getNowStr(), "threadTaskFrpc", rs), `c:\logs\frpcTask.log`)


```

- 自动删除任务示例

文件：[autoRemoveTask001.char](https://topget.org/dc/c/charlang/example/autoRemoveTask001.char)

```go
global basePathG

logPathT := joinPath(basePathG, "charService.log")

rs := appendText("\nautoRemoveTask001.char\n", logPathT)

```


### 7.7 更多示例

#### 内置函数：checkErr

checkErr：检查对象是否为错误或错误字符串，如果是，则输出并退出程序，用法：checkErr(result, "-format=Failed to process: %v\n")，默认格式为 "Error: %v\n"

文件：[checkErr.char](https://topget.org/dc/c/charlang/example/checkErr.char)

#### 匿名函数

文件：[anonymousFunc.char](https://topget.org/dc/c/charlang/example/anonymousFunc.char)

#### 数组进阶

文件：[array.char](https://topget.org/dc/c/charlang/example/array.char)

#### Map进阶

文件：[map.char](https://topget.org/dc/c/charlang/example/map.char)

#### 可变字符串

文件：[mutableString.char](https://topget.org/dc/c/charlang/example/mutableString.char)

#### 大整数

文件：[bigInt.char](https://topget.org/dc/c/charlang/example/bigInt.char)

#### 大浮点数

文件：[bigFloat.char](https://topget.org/dc/c/charlang/example/bigFloat.char)

#### 位运算

文件：[bitwise.char](https://topget.org/dc/c/charlang/example/bitwise.char)

#### 计算BMI

文件：[bmi.char](https://topget.org/dc/c/charlang/example/bmi.char)

#### 计算两个向量的余弦相似度

文件：[calCosineSimilarity.char](https://topget.org/dc/c/charlang/example/calCosineSimilarity.char)

#### 使用大浮点数计算两个向量的余弦相似度

* 更高精度

文件：[calCosineSimilarityBig.char](https://topget.org/dc/c/charlang/example/calCosineSimilarityBig.char)

#### Gel的更多示例

文件：[gel3.char](https://topget.org/dc/c/charlang/example/gel3.char)

#### 重定向标准输出到文件

注意：最好使用 charw.exe（即 Windows 中的 Charlang 的 GUI 版本）运行示例代码。

文件：[guiRedirectStdout.char](https://topget.org/dc/c/charlang/example/guiRedirectStdout.char)

#### 获取Charlang中的命名值

注意：在使用这些值之前，应首先在 Charlang 源代码中定义它们。在 charadd.go 中：

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
}
```

通过调用 getNamedValue(getConst) 函数获取这些值。

文件：[namedValue.char](https://topget.org/dc/c/charlang/example/namedValue.char)

#### 调用Go中的命名函数

注意：在调用这些函数之前，应首先在 Charlang 源代码中定义它们。在 charadd.go 中：

```go
...

var namedFuncMapG = map[string]interface{}{
	"fmt.Fprintf": fmt.Fprintf,
	"tk.NewTK":    tk.NewTK,
}

...
```

通过调用 callNamedValue 函数并传递适当的参数来调用这些函数。

文件：[callNamedFunc.char](https://topget.org/dc/c/charlang/example/callNamedFunc.char)

使用这种方法，理论上我们可以在 Golang 的标准库或第三方存储库中添加任何函数。

#### 比较二进制文件

文件：[binCompare.char](https://topget.org/dc/c/charlang/example/binCompare.char)

#### 简单文本编辑器

文件：[editFile.char](https://topget.org/dc/c/charlang/example/editFile.char)

一个简单的文本编辑器，具有文件加载/保存、JSON 验证、代码运行功能，在 GUI 中。

#### 图像的Base64编码

文件：[base64EncodeImage.char](https://topget.org/dc/c/charlang/example/base64EncodeImage.char)

将图像文件编码为 Base64 代码以在 HTML 的 img 标签中使用，例如，

```shell
D:\tmp>char -exam base64EncodeImage.char -file=d:\downtemp\curtain-2757815_960_720.png
data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAA8AAAAHECAYAAAD22EOkAAABeGlDQ1BJQ0MgUHJvZmlsZQAAeJx1kL9LAmEcxh+1sB+GQRINDTdIQyiEDTWWDUKIiBlktdyddxqcetydRDQ2tDq4VLRk0X9QW/QPBEFQTUHU3FAQQcj1nCcood/jve+H5/0+7733AN60JpfMgTmgVLaMTCIubOQ2Bf8rhjGOSYSwIMqmvpxOJ9G3vh/hcfpD1Dmr/1zPGs0rpgx4hsiLsm5Y5CVyatfSHa6RQ3JRzJPPyRGDFyTfO7rk8rvDBZd/HDaymRXAGyALhS6WulguGiVyhBwuaVW5fR/nTwJKeX2Nfbq1TGSQQBwCJFSxAw0WouxlZt...
```

#### 在控制台绘制数据图形

文件：[asciiPlot.char](https://topget.org/dc/c/charlang/example/asciiPlot.char)

![快照](https://topget.org/dc/s/images/pic3873474424.png)

#### 在控制台绘制实时数据图形

文件：[asciiPlotRealTime.char](https://topget.org/dc/c/charlang/example/asciiPlotRealTime.char)

演示多线程，更新控制台的一个区域...

#### 绘制简单数据图形到图像

文件：[basicPlot.char](https://topget.org/dc/c/charlang/example/basicPlot.char)

演示在数组中绘制数据的简单折线图...

#### 在WebView中绘制简单数据图形

文件：[basicPlotToWebView.char](https://topget.org/dc/c/charlang/example/basicPlotToWebView.char)

演示在数组中绘制数据的简单折线图，然后将其渲染到 WebView2 中的 IMG 元素...

#### 绘制折线图

文件：[imagePlot.char](https://topget.org/dc/c/charlang/example/imagePlot.char)

更多将折线图绘制到图像的示例...

#### WebView2中的计算器

文件：[calculatorInWebView.char](https://topget.org/dc/c/charlang/example/calculatorInWebView.char)

一个在 WebView2 中运行的小型 GUI 计算器，使用 github.com/WebDevSimplified/Vanilla-JavaScript-Calculator 中的 HTML/CSS/JavaScript 示例代码。演示如何在 Charlang 中嵌入 WEB 应用程序。

#### 使用蒙特卡罗算法在WebView2中估算π的值

文件：[guiCalPi.char](https://topget.org/dc/c/charlang/example/guiCalPi.char)

演示如何在 Charlang 的 WebView2 应用程序中运行线程以通过 JavaScript 持续更新 WEB 页面。

#### 字节队列

文件：[byteQueue.char](https://topget.org/dc/c/charlang/example/byteQueue.char)

演示如何使用 'newObj' 函数使用非内置数据类型 "byteQueue"。

### 7.8 高级主题

#### 语言注意事项

Charlang 源代码被编译为字节码并在虚拟机 (VM) 中运行。编译后的字节码可以序列化/反序列化，以便为远程进程在执行前移除编译步骤，反序列化将在一定程度上解决版本差异。

Charlang 中的主脚本和 Charlang 源模块都是具有 compiledFunction 类型名称的函数。可以使用 param 语句为主函数定义参数，主函数也可以使用 return 语句返回值。如果缺少 return 语句，默认返回 undefined 值。所有函数返回单个值，但由于解构功能，Charlang 允许将多个值作为数组返回，并将返回的数组元素设置为多个变量。

Charlang 依赖于 Go 的垃圾回收器，对象没有分配限制。内置对象可以在编译前禁用。

Charlang 可以使用 try-catch-finally 语句处理运行时错误和 Go panic，这与 Ecmascript 实现类似，只有一些细微差异。虽然 Go 开发人员不喜欢 try-catch-finally，但它们是众所周知的可用语句。

Charlang 也被开发为 Go 应用程序的嵌入式脚本语言，从文件导入源模块在不久的将来不会添加，但可以实现自定义模块以将文件内容返回给编译器。

Charlang 目前有一个简单的优化器，用于常量折叠和评估没有副作用的表达式。优化器贪婪地评估仅包含字面量（int、uint、char、float、bool、string）的表达式。可以使用编译器选项禁用优化器。

#### 从命令行运行脚本

- 单行

```shell
D:\tmp>char -cmd=pln(1+3)
4
```

- 多行和/或带空格

原始代码：

```
a := 3
pln(a * 12)
```

对参数进行 url 编码后的命令行：

```
D:\tmp>char -cmd=a%20:=3%0Apln(a%20*%2012) -urlDecode
36
```

- 多行和/或带空格（替代方式）

首先加密脚本：

```
D:\tmp>char
Charlang 1.2.2 by TopXeQ
> encryptText("a := 12\npln(a * 5)", "char") 
694B9F86EB4B1024390E25200A6264602C571C20282F29
>
```

然后在参数前加上字符串 `//TXDEF#`，

```shell
D:\tmp>char -cmd=//TXDEF#694B9F86EB4B1024390E25200A6264602C571C20282F29
60
```


#### 显示Charlang的环境信息

```shell
D:\tmp>char -env
Charlang by TopXeQ V1.2.3
basePath: C:\Users\Administrator\char
localPath: d:\scripts
cloudPath: https://script.example.com/scripts/
```

#### 查看版本和更新

**显示版本信息：**

```shell
char -version
```

输出：
```
Charlang by TopXeQ V2.1.4
Commit: d298bd9
Built: 2026-03-18T01:40:22Z
```

**自我更新到最新版本：**

```shell
char -updateSelf
```

这将自动检查更新、下载最新版本并安装。

#### 将脚本编译为可执行文件

```shell
D:\tmp>char -compile -output=basic.exe -example basic.char

D:\tmpx>basic.exe
3.4000000000000004

D:\tmpx>basic
3.4000000000000004

```

在 Windows 中使用 charw.exe 编译脚本，以避免运行时显示控制台窗口 (CMD)。例如：

```shell

charw -compile -output=cal.exe -example guiCalculator.char

D:\tmpx>cal.exe

```

![快照](https://topget.org/dc/s/attach/pic1110174843.png)


#### Charlang作为Go中的嵌入式语言

要在 Golang 中运行 Charlang 脚本，必须编译它以创建 `Bytecode` 对象，然后将其提供给虚拟机 (VM)。Charlang 在编译器中默认启用了一个简单的优化器。优化器评估没有副作用的简单表达式，以用常量值替换表达式。注意，可以禁用优化器以加快编译过程。

由于它处于积极开发阶段，为了成功编译，请先在您的 `$GOPATH/src/github.com/topxeq`（Windows 中为 `%GOPATH%/src/github.com/topxeq`）目录中 `git clone https://github.com/topxeq/tkc`。然后在 go.mod 文件中添加 `replace github.com/topxeq/tkc v0.0.0 => $GOPATH/src/github.com/topxeq/tkc`。确保将 `$GOPATH` 替换为您的实际 GOPATH。

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
  bytecode, err := charlang.Compile([]byte(script), &charlang.DefaultCompilerOptions)

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

上面的脚本非常不言自明，它计算给定数字的斐波那契数。

编译器选项保存编译器的所有可自定义选项。
`TraceCompilerOptions` 用于跟踪解析-优化-编译步骤，用于调试和测试目的，如下所示；

```go
bytecode, err := charlang.Compile([]byte(script), charlang.TraceCompilerOptions)
// or change output and disable tracing parser
// opts := charlang.TraceCompilerOptions
// opts.Trace = os.Stderr
// opts.TraceParser = false
// bytecode, err := charlang.Compile([]byte(script), opts)
```

可以使用 `Abort` 方法中止 VM 执行，这会导致 `Run` 方法返回包装 `ErrVMAborted` 错误的错误。`Abort` 必须从不同的 goroutine 调用，并且可以安全地多次调用。

可以使用 Go 的 `errors` 包中的 `errors.Is` 函数检查从 `Run` 方法返回的错误是否为特定错误值。

`VM` 实例是可重用的。`VM` 的 `Clear` 方法清除所有持有的引用，并确保清理堆栈和模块缓存。

```go
vm := charlang.NewVM(bytecode)
retValue, err := vm.Run(nil,  Charlang.Int(35))
/* ... */
// vm.Clear()
retValue, err := vm.Run(nil,  Charlang.Int(34))
/* ... */
```

可以向 VM 提供使用 `global` 关键字声明的全局变量。源模块也可以访问全局变量。应使用类似 map 的对象来获取/设置全局变量，如下所示。

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

Charlang 中有一个特殊类型 `SyncMap` 用于创建 goroutine 安全的 map 对象，其中脚本/Go 可能需要并发地相互交互，例如，可以在 map 中收集统计信息或数据。`SyncMap` 的底层 map 受 `sync.RWMutex` 保护。

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

从上面的示例可以看出，VM 的 `Run` 方法接受参数，其签名如下。可以为 globals 参数提供类似 map 的 `globals` 参数或 `nil` 值。`args` 可变参数允许向 VM 提供任意数量的参数，这些参数通过 `param` 语句访问。

```go
func (vm *VM) Run(globals Object, args ...Object) (Object, error)
```

#### 变量声明和作用域

##### param

`param` 关键字用于为主函数（主脚本）声明参数。多个声明需要括号。最后一个参数也可以是可变的。与 `var` 关键字不同，初始化值是非法的。可变参数初始化为空数组 `[]`，如果未提供，其他参数初始化为 `undefined`。`param` 关键字在主函数中只能使用一次。

```go
param (arg0, arg1, ...vargs)
```

```go
param foo
param bar    // 非法，不允许使用多个 param 关键字
```

```go
if condition  {
  param arg    // 非法，在此作用域中不允许
}

func(){
    param (a, b)    // 非法，在此作用域中不允许
}
```

##### global

`global` 关键字用于声明全局变量。注意，`var` 语句或短变量声明 `:=` 总是创建局部变量而不是全局变量。多个声明需要括号。与 `var` 不同，初始化值是非法的。`global` 语句可以在脚本中出现多次。

`global` 允许访问提供给虚拟机 (VM) 的具有变量名的可索引 `globals` 参数。

如果将 `nil` 作为 globals 传递给 VM，则会为 globals 分配一个临时 `map`。

对全局变量的任何赋值都会创建或更新 globals 元素。

注意，源模块可以访问全局变量，这使得可以将对象导出到脚本，就像 C 中的 `extern` 一样。

```go
global foo
global (bar, baz)
```

```go
// "globals" 内置函数返回提供给 VM 的 "globals"。
g := globals()
v := g["foo"]    // 与 `global foo; v := foo` 相同
```

```go
if condition {
  global x     // 非法，在此作用域中不允许
}

func() {
  global y     // 非法，在此作用域中不允许
}
```

##### var

`var` 关键字用于声明局部变量。多个声明需要括号。注意：var 语句不支持元组赋值。

```go
var foo               // foo == undefined
var (bar, baz = 1)    // bar == undefined, baz == 1
var (bar,
     baz = 1)         // 有效
var (
    foo = 1
    bar
    baz = "baz"
)                     // 有效
```

可以使用短变量声明 `:=` 和赋值 `=` 运算符将值赋给变量。

* `:=` 运算符在作用域中定义一个新变量并赋值。
* `=` 运算符为作用域中的现有变量赋新值。

```go
                  // 函数作用域 A
a := "foo"       // 在局部作用域中定义 'a'

func() {         // 函数作用域 B
  b := 52        // 在函数作用域 B 中定义 'b'
  
  func() {       // 函数作用域 C
    c := 19.84   // 在函数作用域 C 中定义 'c'

    a = "bee"    // ok: 从函数作用域 A 为 'a' 赋新值
    b = 20       // ok: 从函数作用域 B 为 'b' 赋新值

    b := true    // ok: 在函数作用域 C 中定义新的 'b'
                 //     （遮蔽来自函数作用域 B 的 'b'）
  }
  
  a = "bar"      // ok: 从函数作用域 A 为 'a' 赋新值
  b = 10         // ok: 为 'b' 赋新值
  a := -100      // ok: 在函数作用域 B 中定义新的 'a'
                 //     （遮蔽来自函数作用域 A 的 'a'）
  
  c = -9.1       // 非法: 'c' 未定义
  var b = [1, 2] // 非法: 'b' 已在同一作用域中定义
}

b = 25           // 非法: 'b' 未定义
var a = {d: 2}   // 非法: 'a' 已在同一作用域中定义
```

以下是非法的，因为函数创建时变量未定义。

在赋值语句中，右侧在左侧之前编译。

```go
f := func() {
  f()    // 非法: 未解析的符号 "f"
}
```

```go
var f
f = func() {
  f()    // ok: "f" 在赋值之前声明。
}
```

与 Go 不同，变量可以被赋予不同类型的值。

```go
a := 123        // 已赋值 'int'
a = "123"       // 重新赋值 'string'
a = [1, 2, 3]   // 重新赋值 'array'
```

捕获循环变量返回循环最后一次 post 语句之后设置的变量的最后一个值，就像 Go 一样。

```go
var f

for i := 0; i < 3; i++ {
  f = func(){
    return i
  }  
}

println(f())  // 3
```

像 Go 一样，要捕获变量，请使用相同名称或不同的名称定义新变量。

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

`const` 关键字用于声明局部常量变量。多个声明需要括号。注意：不支持元组赋值。

常量的值不能通过重新赋值来更改。

重新赋值在编译期间检查并抛出错误。

常量在声明时需要初始化器。const 声明创建对值的只读引用。这并不意味着它持有的值是不可变的。

```go
const (
  a = 1
  b = {foo: "bar"}
)

const c       // 非法，没有初始化器

a = 2         // 非法，重新赋值
b.foo = "baz" // 合法
```

也支持 `iota`。

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
  x = func() { return iota }() // 非法，编译错误
)
```

```go
const (
  iota = 1 // 非法，编译错误
)
```

赋值的 RHS 可以是任何表达式，因此 `iota` 也可以与它们一起使用。

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

**警告：** 如果在 `const` 赋值之前创建了名为 `iota` 的变量，则 `iota` 不用于枚举，它被视为普通变量。

```go
iota := "foo"

const (
  x = iota
  y
)
println(x) // foo
println(y) // foo
```

#### 值和值类型

在 Charlang 中，一切都是值，并且所有值都与一个类型（对象）相关联。

```go
19 + 84                 // int 值
1u + 5u                 // uint 值
"foo" + `bar`           // string 值
-9.22 + 1e10            // float 值
true || false           // bool 值
'ç' > '9'               // char 值
[1, false, "foo"]       // array 值
{a: 12.34, "b": "bar"}  // map 值
func() { /*...*/ }      // function 值
```

以下是 Charlang 中基本值类型的列表。

| Charlang 类型          | 描述                          | Go 中的等效类型 |
|:------------------|:-------------------------------------|:----------------------|
| int               | 有符号 64 位整数值          | `int64`               |
| uint              | 无符号 64 位整数值        | `uint64`              |
| float             | 64 位浮点值          | `float64`             |
| bool              | 布尔值                        | `bool`                |
| char              | unicode 字符                    | `rune`                |
| string            | unicode 字符串                       | `string`              |
| bytes             | 字节数组                           | `[]byte`              |
| error             | [error](#错误值) 值         | -                     |
| array             | 值数组                          | `[]Object`            |
| map               | 具有字符串键的值 map           | `map[string]Object`   |
| undefined         | [undefined](#未定义值) 值 | -                     |
| compiledFunction  | [function](#函数值) 值   | -                     |

##### 错误值

在 Charlang 中，可以使用 "error" 类型的值表示错误。使用 `error` 内置函数创建错误值，它具有底层消息。可以使用 `.Message` 选择器访问错误的底层消息。错误还有一个名称，可以使用 `.Name` 访问。使用 `error` 内置函数创建的错误具有默认名称 `error`，但内置错误具有不同的名称，如 `NotIterableError`、`ZeroDivisionError`。

传递给 `error` 内置函数的第一个参数将转换为字符串作为消息。

```go
err1 := error("oops")
err2 := error(1+2+3)         // 等同于 err2 := error("6")
if isError(err1) {           // 'isError' 是内置函数
  name := err1.Name          // 获取底层名称
  message := err1.Message    // 获取底层消息
}  
```

#### Charlang运行时类型

##### 基本类型：

- **bool**: 布尔值（Go 中的 `bool`）
- **byte**: 无符号 8 位整数（Go 中的 `uint8`）
- **char**: 字符（Go 中的 `rune`）
- **int**: 有符号 64 位整数（Go 中的 `int64`）
- **uint**: 无符号 64 位整数（Go 中的 `uint64`）
- **float**: 64 位浮点数（Go 中的 `float64`）
- **string**: 字符串（Go 中的 `string`）
- **bytes**: 字节数组（Go 中的 `[]byte`）
- **chars**: 字符（Go 中的 `[]rune`）
- **array**: 对象数组（Go 中的 `[]Object`）
- **map**: 具有字符串键的对象 map（Go 中的 `map[string]Object`）
- **error**: 带有字符串 Name 和 Message 的错误
- **undefined**: undefined

##### 更多类型：

- **bigInt**: 保存一个大整数值
- **bigFloat**: 保存一个大浮点值
- **mutableString**: 可变字符串
- **orderedMap**: 具有固定顺序项的 map
- **objectRef**: 传递给函数的对象引用
- **stack**: 通用堆栈
- **queue**: 通用队列
- **function**: 可调用函数
- **compiledFunction**: 编译函数
- **charCode**: 保存源代码和可运行的 Charlang 代码
- **gel**: 保存主要用于远程调用的源代码和可运行的 Charlang 代码
- **statusResult**: 状态对象，通常由服务器端返回，格式为：`{"Status": "success", "Value": "more info"}` 或 `{"Status": "fail", "Value": "error message"}`
- **stringBuilder**: 在 Golang 中保存一个 strings.Builder 对象
- **bytesBuffer**: 在 Golang 中保存一个 bytes.Buffer 对象
- **database**: 在 Golang 中保存一个数据库对象
- **time**: 在 Golang 中保存一个 time.Time 对象
- **location**: 在 Golang 中保存一个 time.Location 对象
- **seq**: 生成唯一、序列号的对象
- **mutex**: 多线程的互斥锁
- **mux**: http 请求的 mux(router)
- **httpReq**: 在 Golang 中保存一个 http.Request 对象
- **httpResp**: 在 Golang 中保存一个 http.Response 对象
- **httpHandler**: 在 Golang 中保存一个 http.HandleFunc 对象
- **reader**: 在 Golang 中保存一个 io.Reader 对象
- **writer**: 在 Golang 中保存一个 io.Writer 对象
- **file**: 在 Golang 中保存一个 os.File 对象
- **image**: 在 Golang 中保存一个 image.Image 对象
- **delegate**: 保存用于调用的委托（回调）函数
- **etable**: 保存具有多工作表表格数据的对象，如 Excel 或 CSV
- **excel**: 保存具有 Excel 数据的对象
- **any**: 保存任何值类型的对象

##### Go类型定义

- `int`

```go
type Int int64
```

- `uint`

```go
type Uint uint64
```

注意：可以通过在整数值后添加 `u` 后缀来表示 uint 值。

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

##### 类型转换/强制转换表

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

- **X**: 没有转换。转换函数将抛出运行时错误 TypeError。
- strconv: 使用 `strconv` 包中的 Go 转换函数转换。
- IsFalsy(): 使用 [Object.IsFalsy()](#objectisfalsy) 函数。
- String(): 使用 `Object.String()` 函数。

##### Object.IsFalsy()

`Object.IsFalsy()` 接口方法用于确定给定值是否应评估为 `false`（例如用于 `if` 语句的条件表达式）。

- **int**: `v == 0`
- **uint**: `v == 0`
- **float**: `math.IsNaN(v)`
- **bool**: `!v`
- **char**: `v == 0`
- **string**: `len(v) == 0`
- **bytes**: `len(v) == 0`
- **array**: `len(v) == 0`
- **map**: `len(v) == 0`
- **error**: `true` _(error 总是 falsy)_
- **undefined**: `true` _(undefined 总是 falsy)_


#### 内置错误

内置错误没有消息但有名称。通过对错误值调用 `.New(message)` 函数，通过包装错误创建新错误。

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

##### 未定义值

在 Charlang 中，`undefined` 值可用于表示意外或不存在的值：

* 不显式返回值的函数被认为返回 `undefined` 值。
* 复合值类型的索引器或选择器可能返回 `undefined`（如果键或索引不存在）。
* 内置函数可能返回 `undefined`。

```go
a := func() { b := 4 }()    // a == undefined
c := {a: "foo"}["b"]        // c == undefined
d := sort(undefined)        // d == undefined
e := delete({}, "foo")      // "delete" 总是返回 undefined
```

可以使用内置函数 `isUndefined`、`isUndef` 或 `==` 运算符来检查值是否为 undefined。

##### 数组值

在 Charlang 中，数组是任何类型值的有序列表。可以使用索引器 `[]` 访问数组的元素。

```go
[1, 2, 3][0]       // == 1
[1, 2, 3][2]       // == 3
[1, 2, 3][3]       // RuntimeError: IndexOutOfBoundsError

["foo", 'x', [1, 2, 3], {bar: 2u}, true, undefined, bytes()]   // ok
```

##### Map值

在 Charlang 中，map 是一组键值对，其中键是字符串，值可以是任何值类型。可以使用索引器 `[]` 或选择器 '.' 运算符访问 map 的值。

```go
m := { a: 1, "b": false, c: "foo" }
m["b"]                                // == false
m.c                                   // == "foo"
m.x                                   // == undefined

{a: [1, 2, 3], b: {c: "foo", d: "bar"}} // ok
```  

##### 函数值

在 Charlang 中，函数是具有若干函数参数和返回值的可调用值。就像任何其他值一样，函数可以传递到另一个函数中或从另一个函数返回。

```go
sum := func(arg1, arg2) {
  return arg1 + arg2
}

var mul = func(arg1, arg2) {
  return arg1 * arg2
}

adder := func(base) {
  return func(x) { return base + x }  // 捕获 'base'
}

add5 := adder(5)
nine := add5(4)    // == 9
```

与 Go 不同，Charlang 没有函数声明。所有函数都是匿名函数。所以以下代码是非法的：

```go
func foo(arg1, arg2) {  // 非法
  return arg1 + arg2
}
```

Charlang 还支持可变参数函数：

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

只有最后一个参数可以是可变的。以下代码是非法的：

```go
// 非法，因为 "a" 是可变的并且不是最后一个参数
illegal := func(...a, b) {}
```

调用函数时，传递的参数数量必须与函数定义的数量匹配。

```go
f := func(a, b) {}
f(1, 2, 3)    // RuntimeError: WrongNumArgumentsError
```

像 Go 一样，您可以使用省略号 `...` 将数组类型的值作为其最后一个参数传递：

```go
f1 := func(a, b, c) { return a + b + c }
f1(...[1, 2, 3])    // => 6
f1(1, ...[2, 3])    // => 6
f1(1, 2, ...[3])    // => 6
f1(...[1, 2])       // RuntimeError: WrongNumArgumentsError

f2 := func(a, ...b) {}
f2(1)               // 有效; a == 1, b == []
f2(1, 2)            // 有效; a == 1, b == [2]
f2(1, 2, 3)         // 有效; a == 1, b == [2, 3]
f2(...[1, 2, 3])    // 有效; a == 1, b == [2, 3]
```

#### 类型转换

虽然在 Charlang 中没有直接指定类型，但可以使用类型转换内置函数在值类型之间进行转换。

```go
s1 := string(1984)    // "1984"
i2 := int("-999")     // -999
f3 := float(-51)      // -51.0
b4 := bool(1)         // true
c5 := char("X")       // 'X'
```

#### 运算符

##### 一元运算符

| 运算符 | 操作               | 类型（结果）                                            |
|:--------:|:-----------------------:|:---------------------------------------------------------:|
| `+`      | `0 + x`                 | int(int), uint(uint), char(char), float(float), bool(int) |
| `-`      | `0 - x`                 | int(int), uint(uint), char(int), float(float), bool(int)  |
| `^`      | 按位取反 `^x` | int(int), uint(uint), char(char), bool(int)               |
| `!`      | 逻辑非             | 所有类型*                                                |

_* 在 Charlang 中，所有值都可以是真值或假值。_

##### 二元运算符

| 运算符 | 用法                    |
|:--------:|:------------------------:|
| `==`     | 等于                    |
| `!=`     | 不等于                |
| `&&`     | 逻辑与              |
| `\|\|`   | 逻辑或               |
| `+`      | 加/连接               |
| `-`      | 减                 |
| `*`      | 乘                   |
| `/`      | 除                   |
| `&`      | 按位与              |
| `\|`     | 按位或               |
| `^`      | 按位异或              |
| `&^`     | 位清除 (AND NOT)       |
| `<<`     | 左移               |
| `>>`     | 右移               |
| `<`      | 小于                |
| `<=`     | 小于等于    |
| `>`      | 大于             |
| `>=`     | 大于等于 |

##### 三元运算符

Charlang 有一个三元条件运算符 `(条件表达式) ? (真表达式) : (假表达式)`。

```go
a := true ? 1 : -1    // a == 1

min := func(a, b) {
  return a < b ? a : b
}
b := min(5, 10)      // b == 5
```

##### 赋值和递增运算符

| 运算符 | 用法                     |
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

##### 运算符优先级

一元运算符具有最高优先级，三元运算符具有最低优先级。二元运算符有五个优先级。乘法运算符结合最紧密，其次是加法运算符、比较运算符、`&&`（逻辑与），最后是 `||`（逻辑或）：

| 优先级 | 运算符                             |
|:----------:|:------------------------------------:|
| 5          | `*`  `/`  `%`  `<<`  `>>`  `&`  `&^` |
| 4          | `+`  `-`  `\|`  `^`                  |
| 3          | `==`  `!=`  `<`  `<=`  `>`  `>=`     |
| 2          | `&&`                                 |
| 1          | `\|\|`                               |

像 Go 一样，`++` 和 `--` 运算符构成语句，而不是表达式，它们不在运算符层次结构中。

##### 选择器和索引器

可以使用选择器 (`.`) 和索引器 (`[]`) 运算符来读取或写入复合类型（array、map、string、bytes）的元素。

```go
["one", "two", "three"][1]  // == "two"

bytes(0, 1, 2, 3)[1]    // == 1

// 像 Go 一样，索引字符串返回索引的字节值作为 int 值。
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
m.x = 5          // 将 'x' 添加到 map 'm'
```

像 Go 一样，可以对序列值类型（如 array、string、bytes）使用切片运算符 `[:]`。负索引是非法的。

```go
a := [1, 2, 3, 4, 5][1:3]    // == [2, 3]
b := [1, 2, 3, 4, 5][3:]     // == [4, 5]
c := [1, 2, 3, 4, 5][:3]     // == [1, 2, 3]
d := "hello world"[2:10]     // == "llo worl"
e := [1, 2, 3, 4, 5][:]      // == [1, 2, 3, 4, 5]
f := [1, 2, 3, 4, 5][-1:]    // RuntimeError: InvalidIndexError
g := [1, 2, 3, 4, 5][10:]    // RuntimeError: IndexOutOfBoundsError
```

**注意：关键字不能用作选择器。**

```go
a := {}
a.func = ""     // Parse Error: expected selector, found 'func'
```

使用双引号和索引器将关键字与 map 一起使用。

```go
a := {}
a["func"] = ""
```

#### 语句

##### If语句

"If" 语句与 Go 非常相似。

```go
if a < 0 {
  // 如果 'a' 为负数则执行
} else if a == 0 {
  // 如果 'a' 为零则执行
} else {
  // 如果 'a' 为正数则执行
}
```

像 Go 一样，条件表达式前面可以有一个简单语句，该语句在评估表达式之前执行。

```go
if a := foo(); a < 0 {
  // 如果 'a' 为负数则执行
}
```

##### For语句

"For" 语句与 Go 非常相似。

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

##### For-In语句

它类似于 Go 的 `for range` 语句。"For-In" 语句可以迭代任何可迭代的值类型（array、map、bytes、string）。

```go
for v in [1, 2, 3] {          // array: 元素
  // 'v' 是数组元素值
}
for i, v in [1, 2, 3] {       // array: 索引和元素
  // 'i' 是索引
  // 'v' 是数组元素值
}
for k, v in {k1: 1, k2: 2} {  // map: 键和值
  // 'k' 是键
  // 'v' 是 map 元素值
}
for i, v in "foo" {           // array: 索引和元素
  // 'i' 是索引
  // 'v' 是 char
}
```

#### 模块

模块是 Charlang 中的基本编译单元。模块可以使用 `import` 表达式导入另一个模块。有 3 种类型的模块。源模块、内置模块和自定义模块。源模块采用 Charlang 代码的形式。内置模块类型为 `map[string]Object`。最后，任何实现 Go `Importable` 接口的值都可以是模块。源模块像编译函数一样被调用，返回的值被存储以供将来使用。如果实现了 `Copier` 接口，其他模块值在 VM 中导入时会被复制。

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

主模块：

```go
sum := import("sum")    // 加载模块
println(sum(10))        // 模块函数
```

源模块作为 `sum`：

```go
base := 5

return func(x) {
  return x + base
}
```

在 Charlang 中，模块与函数非常相似。

* `import` 表达式加载模块代码并像函数一样执行它。
* 模块应使用 `return` 语句返回值。
  * 模块可以返回任何类型的值：int、map、function 等。
  * 模块中的 `return` 停止执行并将值返回给导入代码。
  * 如果模块没有任何 `return` 语句，`import` 表达式简单地返回 `undefined`。_(就像没有 `return` 的函数一样。)_  
* 在不同位置或不同模块中多次导入同一模块会返回相同的对象，因此它保留导入对象的状态。
* 尽管模块中允许使用 `param` 语句，但在导入时不能向源模块提供参数。
* 模块可以使用 `global` 语句访问全局共享对象。

#### 与Go的差异

与 Go 不同，Charlang 没有以下内容：

* 虚数值
* 结构体
* 指针
* 通道
* Goroutines
* 元组赋值（Charlang 支持[解构](destructuring.md)数组）
* Switch 语句
* Goto 语句
* Defer 语句
* Panic 和 recover
* 类型断言

#### 接口

Charlang 类型实现 `Object` 接口。任何实现 `Object` 接口的 Go 类型都可以提供给 Charlang VM。

##### Object接口

```go

// Object 表示 VM 中的对象。
type Object interface {
  // TypeName 应返回类型的名称。
  TypeName() string

  // String 应返回类型值的字符串。
  String() string

  // BinaryOp 处理 +,-,*,/,%,<<,>>,<=,>=,<,> 运算符。
  // 如果没有错误处理程序处理，返回的错误会停止 VM 执行
  // 并且 VM.Run 返回相同的错误作为包装。
  BinaryOp(tok token.Token, right Object) (Object, error)

  // IsFalsy 如果值为假则返回 true，否则返回 false。
  IsFalsy() bool

  // Equal 检查对象的相等性。
  Equal(right Object) bool

  // Call 如果 CanCall() 返回 true，则从 VM 调用。检查
  // 方法中提供的参数数量及其类型。如果没有错误处理程序处理，返回的错误会停止 VM
  // 执行，并且 VM.Run 返回相同的错误作为包装。
  Call(args ...Object) (Object, error)

  // CanCall 如果类型可以用 Call() 方法调用则返回 true。
  // 如果尝试调用不可调用对象，VM 返回错误。
  CanCall() bool

  // Iterate 应为类型返回一个 Iterator。
  Iterate() Iterator

  // CanIterate 应返回对象是否可以迭代。
  CanIterate() bool

  // IndexGet 应为可索引对象获取一个索引 Object 并返回一个结果 Object 或
  // 一个错误。可索引是可以接受索引并返回对象的对象。如果没有
  // 错误处理程序处理，返回的错误会停止 VM 执行，并且 VM.Run 返回相同的错误
  // 作为包装。如果对象不可索引，应返回 ErrNotIndexable
  // 作为错误。
  IndexGet(index Object) (value Object, err error)

  // IndexSet 应为可索引赋值对象获取一个索引 Object 和一个值 Object。
  // 可索引赋值是可以接受索引并在赋值语句左侧接受值的对象。如果对象
  // 不可索引赋值，应返回 ErrNotIndexAssignable 作为错误。如果没有错误处理程序处理，返回的错误会停止 VM
  // 执行，并且 VM.Run 返回相同的错误作为包装。
  IndexSet(index, value Object) error
}
```

##### Iterator接口

如果对象的 `CanIterate` 方法返回 `true`，则其 `Iterate` 方法必须返回一个实现 `Iterator` 接口的值，以便在 `for-in` 循环中使用。

```go
// Iterator 包装在 VM 中迭代对象所需的方法。
type Iterator interface {
  // Next 如果有更多元素要迭代则返回 true。
  Next() bool

  // Key 返回当前元素的键或索引值。
  Key() Object

  // Value 返回当前元素的值。
  Value() Object
}
```

##### Copier接口

对 Charlang 值的赋值会复制值，除了像 Go 那样的 array、map 或 bytes。`copy` 内置函数如果对象实现了 Copier 接口，则返回值的副本。如果未实现，则返回相同的对象，它由 Go 在底层复制值。

```go
// Copier 包装 Copy 方法以创建对象的深层副本。
type Copier interface {
  Copy() Object
}
```

##### IndexDeleter接口

`delete` 内置函数检查给定对象是否实现 `IndexDeleter` 接口以从对象中删除元素。`map` 和 `syncMap` 实现此接口。

```go
// IndexDeleter 包装 IndexDelete 方法以删除对象的索引。
type IndexDeleter interface {
    IndexDelete(Object) error
}
```

##### LengthGetter接口

`len` 内置函数检查给定对象是否实现 `IndexDeleter` 接口以获取对象的长度。`array`、`bytes`、`string`、`map` 和 `syncMap` 实现此接口。

```go
// LengthGetter 包装 Len 方法以获取对象的元素数量。
type LengthGetter interface {
    Len() int
}
```

##### 对象接口扩展

注意，`ExCallerObject` 将在未来替换现有的 Object 接口。

```go
// ExCallerObject 是可以用 CallEx 方法调用的对象的接口。
// 它是 Call 方法的扩展版本，可用于
// 使用 Call 结构调用对象。实现此接口的对象
// 使用 CallEx 方法而不是 Call 方法调用。
// 注意，CanCall() 应为实现此接口的对象返回 true。
type ExCallerObject interface {
    Object
    CallEx(c Call) (Object, error)
}

// NameCallerObject 是可以用 CallName 方法调用的对象的接口
// 以调用对象的方法。实现此接口的对象可以
// 通过不为每个方法调用创建可调用对象来减少分配。
type NameCallerObject interface {
    Object
    CallName(name string, c Call) (Object, error)
}
```
