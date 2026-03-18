# Charlang 发布与安装指南

[English Version](./README.md)

## 概述

本目录包含用于以下功能的脚本和工具：
- 构建时动态生成版本号
- GitHub Release 自动化
- 自我更新功能
- 首次用户安装脚本

## 动态版本控制

Charlang 使用构建时版本注入，而不是在源代码中硬编码版本号。

### 工作原理

版本信息在编译时通过 Go 链接器标志（`ldflags`）注入：

```bash
go build -ldflags "-X github.com/topxeq/charlang.VersionG=1.2.3 -X github.com/topxeq/charlang.CommitG=abc123 -X github.com/topxeq/charlang.BuildDateG=2024-01-01T12:00:00Z"
```

### 版本变量

- `VersionG`: 语义化版本（例如 "v1.2.3" 或 "2.1.3"）
- `CommitG`: 简短的 git commit 哈希（例如 "d298bd9"）
- `BuildDateG`: ISO 8601 UTC 构建时间戳（例如 "2026-03-18T01:40:22Z"）

### 使用 Make 构建

```bash
# 使用 git 自动检测的版本构建
make build

# 使用特定版本构建
VERSION=v1.2.3 make build

# 为所有平台构建
make build-all

# 安装到 GOPATH
make install
```

### 查看版本

```bash
char -version
```

输出：
```
Charlang by TopXeQ V2.1.3
Commit: d298bd9
Built: 2026-03-18T01:40:22Z
```

## GitHub Release 自动化

在 `.github/workflows/release.yaml` 提供了 GitHub Actions 工作流，它会：

1. 在标签推送（`v*` 模式）或手动调度时触发
2. 为多个平台构建二进制文件：
   - Linux (amd64)
   - Windows (amd64) - 控制台和 GUI 版本
   - macOS (amd64, arm64)
   - Android (arm64)
3. 创建带有构建产物的 GitHub Release
4. 自动生成发布说明

### 创建 Release

```bash
# 创建并推送标签
git tag -a v1.2.3 -m "Release v1.2.3"
git push origin v1.2.3
```

或使用 Makefile 助手：
```bash
make tag
# 按照提示输入版本号
```

## 自我更新功能

Charlang 已经通过 `-updateChar` 标志包含了自我更新功能：

```bash
char -updateChar
```

这将：
1. 在 `https://topget.org/pub/charVersion.txt` 检查最新版本
2. 与当前版本比较
3. 为您的操作系统/架构下载适当的二进制文件
4. 备份当前可执行文件
5. 替换为新版本

### 更新服务器

更新服务器应提供：
- `charVersion.txt`: 包含最新版本号的纯文本文件
- `char.gz` / `char.exe.gz` / 等：每个平台的压缩二进制文件

## 安装脚本

### Linux/macOS

```bash
# 下载并运行安装程序
curl -fsSL https://raw.githubusercontent.com/topxeq/charlang/main/scripts/install.sh | bash

# 或使用 wget
wget -qO- https://raw.githubusercontent.com/topxeq/charlang/main/scripts/install.sh | bash
```

该脚本将：
1. 检测您的操作系统和架构
2. 获取最新版本
3. 下载并安装到 `/usr/local/bin`
4. 如需要，添加到 PATH

### Windows

```powershell
# 下载并运行安装程序
Invoke-WebRequest -Uri "https://raw.githubusercontent.com/topxeq/charlang/main/scripts/install.ps1" -OutFile "install.ps1"
.\install.ps1
```

或单行命令：
```powershell
iex (iwr "https://raw.githubusercontent.com/topxeq/charlang/main/scripts/install.ps1")
```

该脚本将：
1. 检测 Windows 和架构
2. 获取最新版本
3. 下载并安装到 `%LOCALAPPDATA%\Charlang\bin`
4. 如需要，添加到用户 PATH

## 手动安装

### 下载预构建二进制文件

从 [Releases 页面](https://github.com/topxeq/charlang/releases) 下载适合您平台的二进制文件。

### 从源码构建

```bash
# 克隆仓库
git clone https://github.com/topxeq/charlang.git
cd charlang

# 构建
make build

# 安装
make install
```

## 文件结构

```
charlang/
├── .github/
│   └── workflows/
│       └── release.yaml      # GitHub Release 自动化
├── scripts/
│   ├── README.md              # 本文件
│   ├── README_CN.md           # 中文文档
│   ├── install.sh             # Linux/macOS 安装程序
│   └── install.ps1            # Windows 安装程序
├── Makefile                   # 带版本控制的构建目标
└── charadd.go                 # 版本变量（构建时设置）
```

## 故障排除

### Linux/macOS 上权限被拒绝

如果运行安装程序时出现"permission denied"：

```bash
# 使用 sudo 运行
curl -fsSL https://raw.githubusercontent.com/topxeq/charlang/main/scripts/install.sh | sudo bash
```

或安装到您拥有的目录：
```bash
mkdir -p ~/bin
curl -fsSL https://raw.githubusercontent.com/topxeq/charlang/main/scripts/install.sh | INSTALL_PATH=~/bin bash
```

### Windows 上 PATH 未更新

重启终端或运行：
```powershell
$env:Path = [System.Environment]::GetEnvironmentVariable("Path","User") + ";" + [System.Environment]::GetEnvironmentVariable("Path","Machine")
```

## 贡献

有关一般贡献指南，请参阅主 [README.md](../README_CN.md)。

对于发布/安装系统的更改：
1. 在本地测试安装脚本
2. 验证 GitHub Actions 工作流正常工作
3. 更新英文和中文文档

## 许可证

与主 Charlang 项目相同的许可证。
