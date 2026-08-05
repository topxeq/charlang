# Charlang 服务器监控与运维指南

> 适用版本：v0.6.112+（新增强 RequestInfo 脚本名显示 + pprof 诊断端口）

---

## 一、启动方式变更

### 旧命令

```bash
charlang -server -port=:80 -dir=/data/scripts
```

### 新命令（推荐）

```bash
charlang -server -port=:80 -dir=/data/scripts -adminToken=yourSecretToken -pprofPort=:6060
```

### 新增参数说明

| 参数 | 是否必须 | 默认值 | 说明 |
|------|----------|--------|------|
| `-adminToken` | 建议设置 | `""` | 访问管理接口的鉴权 token，与旧版一样 |
| `-pprofPort` | 可选 | `""`（不启用） | pprof 诊断端口，仅监听 `127.0.0.1`，外网不可达 |

### Windows 服务模式

服务模式（`-service`）不受此更新影响，无需修改。如果之前是通过脚本在服务内启动 HTTP 服务器，只需在脚本中增加 `-pprofPort` 参数。

---

## 二、监控手段

### 2.1 查看当前运行的微服务

```bash
curl "http://host:port/admin/status?token=yourSecretToken"
```

返回示例：

```json
{
  "count": 3,
  "vms": [
    {"id": "0xc0001a4000", "info": "[report.char] GET /charms/report?vo=...", "duration": "2m35s"},
    {"id": "0xc0001a4120", "info": "[route:/api/users] GET /api/users",           "duration": "12s"},
    {"id": "0xc0001a4240", "info": "[query.char] GET /charms/query?vo=...",      "duration": "45m0s"}
  ]
}
```

**字段含义：**

- `id` — VM 唯一标识，用于 kill 操作
- `info` — `[脚本名或路由] HTTP方法 URL`。新版本包含了脚本文件名（`[xxx.char]`）或注册路由（`[route:/xxx]`）
- `duration` — 该请求已运行时长。**长时间运行的请求通常是问题所在**

### 2.2 终止异常请求

```bash
curl -X POST "http://host:port/admin/kill?id=0xc0001a4240&token=yourSecretToken"
```

返回 `{"status":"killed"}` 表示已终止。被终止的请求会收到 `VM aborted` 错误。

### 2.3 查看所有 Goroutine 栈（确认谁在"死循环"）

浏览器直接访问：

```
http://127.0.0.1:6060/debug/pprof/goroutine?debug=2
```

或者：

```bash
curl "http://127.0.0.1:6060/debug/pprof/goroutine?debug=2"
```

输出示例：

```
goroutine 45 [running]:
charlang.(*VM).Run(...)
    D:/goprjs/src/github.com/topxeq/charlang/vm.go:850
charlang.RunScriptOnHttp(...)
    .../charadd.go:2806

goroutine 67 [running]:
charlang.(*VM).Run(...)
    .../vm.go:850
```

如果看到大量 goroutine 都在跑同一个脚本，该脚本可能存在死循环或慢查询。

### 2.4 CPU Profile（精确定位 CPU 热点）

```bash
# 采样 30 秒（期间保持正常负载）
curl -o cpu.prof "http://127.0.0.1:6060/debug/pprof/profile?seconds=30"
```

采样完成后用 Go 工具分析：

```bash
# 命令行 Top 20
go tool pprof -top cpu.prof

# 火焰图（推荐）
go tool pprof -http=:8081 cpu.prof
# 浏览器打开 http://localhost:8081
```

火焰图中可以直观看到哪些函数占据了 CPU 时间，并关联到具体的 `.char` 脚本行号。

### 2.5 Heap Profile（排查内存泄漏）

```bash
curl -o heap.prof "http://127.0.0.1:6060/debug/pprof/heap"
go tool pprof -http=:8081 heap.prof
```

### 2.6 命令行快速诊断脚本

```bash
# 一键查看：当前运行的 VM + Goroutine 概况
curl -s "http://host:port/admin/status?token=xxx" | python -m json.tool
curl -s "http://127.0.0.1:6060/debug/pprof/goroutine?debug=1" | head -50

# 30 秒 CPU profile + 自动打开火焰图
curl -o /tmp/cpu.prof "http://127.0.0.1:6060/debug/pprof/profile?seconds=30"
go tool pprof -http=:8081 /tmp/cpu.prof
```

---

## 三、日常运维流程

### 收到 "CPU 高" 告警时

```
1. curl /admin/status → 看哪些脚本在跑，跑了多久
                    → 特别关注 duration > 1 分钟的请求

2. curl pprof/goroutine?debug=2 → 确认是否有大量 goroutine 堆积

3. curl pprof/profile?seconds=30 → 拿 30 秒 CPU profile
                                  → go tool pprof 看火焰图

4. 定位到问题脚本后，kill 掉对应 VM
   curl -X POST /admin/kill?id=xxx

5. 修复脚本逻辑 → 重新部署
```

### 日常巡检

```bash
# 建议加入 cron / 定时任务
# 每小时记录一次 VM 状态
curl -s "http://127.0.0.1:port/admin/status?token=xxx" >> /var/log/charlang/vm_snapshot.log
```

---

## 四、安全须知

1. `-pprofPort` 仅绑定 `127.0.0.1`，外网不可达。确保服务器本身不被未授权用户登录。
2. `-adminToken` 应设置为强随机字符串，不要使用默认空值。
3. pprof 的 `/debug/pprof/cmdline` 会暴露启动命令行参数。不要在命令行中传递密码/密钥。
4. 不需要时不要加 `-pprofPort`，默认为关闭状态，零开销。

---

## 五、pprof 端口一览

| 端点 | 用途 |
|------|------|
| `/debug/pprof/` | 索引页 |
| `/debug/pprof/goroutine?debug=1` | 所有 goroutine 栈（含函数名） |
| `/debug/pprof/goroutine?debug=2` | goroutine 按状态分组 |
| `/debug/pprof/profile?seconds=30` | 30 秒 CPU profile |
| `/debug/pprof/heap` | 内存分配 profile |
| `/debug/pprof/allocs` | 内存分配采样 |
| `/debug/pprof/threadcreate` | 线程创建 profile |
| `/debug/pprof/block` | 阻塞 profile |
| `/debug/pprof/mutex` | 互斥锁竞争 profile |
| `/debug/pprof/trace?seconds=5` | 执行 trace |
| `/debug/pprof/cmdline` | 启动命令行参数 |
| `/debug/pprof/symbol` | 符号表查询 |
