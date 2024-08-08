//go:build !js
// +build !js

package main

import (
	"bufio"
	"bytes"
	"context"
	"fmt"
	"io"
	"math/rand"
	"net/http"
	"os"
	"os/exec"
	"path"
	"path/filepath"
	"regexp"
	"runtime"
	"runtime/debug"
	"strings"
	"time"

	"github.com/topxeq/charlang"

	// ugofmt "github.com/topxeq/charlang/stdlib/fmt"
	// ugotime "github.com/topxeq/charlang/stdlib/time"
	// ugostrings "github.com/topxeq/charlang/stdlib/strings"
	charex "github.com/topxeq/charlang/stdlib/ex"
	// charfmt "github.com/topxeq/charlang/stdlib/fmt"
	tk "github.com/topxeq/tkc"

	"github.com/kardianos/service"

	_ "github.com/denisenkom/go-mssqldb"
	_ "github.com/go-sql-driver/mysql"

	// _ "github.com/mattn/go-sqlite3"
	_ "github.com/glebarez/go-sqlite"
	_ "github.com/sijms/go-ora/v2"
	// _ "github.com/godror/godror"
)

// for charms
var muxG *http.ServeMux
var portG = ":80"
var sslPortG = ":443"
var basePathG = "."
var webPathG = "."
var certPathG = "."
var verboseG = false

var dummyG string

// global variables
var logo = `
Charlang V` + charlang.VersionG + `

`

const (
	title         = "Charlang"
	promptPrefix  = ">>> "
	promptPrefix2 = "... "
)

var (
	isMultiline    bool
	noOptimizer    bool
	traceEnabled   bool
	traceParser    bool
	traceOptimizer bool
	traceCompiler  bool
)

var (
	initialSuggLen int
)

var serviceNameG = "charService"
var configFileNameG = serviceNameG + ".cfg"
var serviceModeG = false
var currentOSG = ""
var scriptPathG = "" // for service only

var CurrentVM *charlang.VM = nil

type program struct {
	BasePath string
}

func (p *program) Start(s service.Service) error {
	// Start should not block. Do the actual work async.
	// basePathG = p.BasePath
	// logWithTime("basePath: %v", basePathG)
	serviceModeG = true

	go p.run()

	return nil
}

func (p *program) run() {
	go doWork()
}

func (p *program) Stop(s service.Service) error {
	// Stop should not block. Return with a few seconds.
	return nil
}

func initSvc() *service.Service {
	if tk.GetOSName() == "windows" {
		currentOSG = "win"
		if tk.Trim(basePathG) == "." || strings.TrimSpace(basePathG) == "" {
			basePathG = "c:\\" + "char" // serviceNameG
		}
		configFileNameG = serviceNameG + "win.cfg"
	} else {
		currentOSG = "linux"
		if tk.Trim(basePathG) == "." || strings.TrimSpace(basePathG) == "" {
			basePathG = "/" + "char" //  + serviceNameG
		}
		configFileNameG = serviceNameG + "linux.cfg"
	}

	if !tk.IfFileExists(basePathG) {
		os.MkdirAll(basePathG, 0777)
	}

	tk.SetLogFile(filepath.Join(basePathG, serviceNameG+".log"))

	svcConfigT := &service.Config{
		Name:        serviceNameG,
		DisplayName: serviceNameG,
		Description: serviceNameG + " V" + charlang.VersionG,
		Arguments:   []string{"-service"},
	}

	prgT := &program{BasePath: basePathG}
	var s, err = service.New(prgT, svcConfigT)

	if err != nil {
		tk.LogWithTimeCompact("%v unable to init servcie: %v\n", svcConfigT.DisplayName, err)
		return nil
	}

	return &s
}

func QuickRunChar(codeA string, scriptPathA string, argsA ...string) interface{} {
	moduleMap := charlang.NewModuleMap()
	moduleMap.AddBuiltinModule("ex", charex.Module)
	// moduleMap.AddBuiltinModule("fmt", charfmt.Module)

	charlang.MainCompilerOptions = &charlang.CompilerOptions{
		// ModulePath:        "", //"(repl)",
		ModuleMap: moduleMap,
		// SymbolTable:       charlang.NewSymbolTable(),
		// OptimizerMaxCycle: charlang.TraceCompilerOptions.OptimizerMaxCycle,
		// TraceParser:       true,
		// TraceOptimizer:    true,
		// TraceCompiler:     true,
		// OptimizeConst:     !noOptimizer,
		// OptimizeExpr:      !noOptimizer,

		// Trace:             os.Stdout,
		// TraceParser:       true,
		// TraceCompiler:     true,
		// TraceOptimizer:    true,
		// OptimizerMaxCycle: 1<<8 - 1,
		// OptimizeConst:     false,
		// OptimizeExpr:      false,
	}

	if charlang.DebugModeG {
		// opts = &charlang.CompilerOptions{
		// 	// ModulePath:        "", //"(repl)",
		// 	ModuleMap: moduleMap,
		// 	// SymbolTable:       charlang.NewSymbolTable(),
		// 	// OptimizerMaxCycle: charlang.TraceCompilerOptions.OptimizerMaxCycle,
		// 	// TraceParser:       true,
		// 	// TraceOptimizer:    true,
		// 	// TraceCompiler:     true,
		// 	// OptimizeConst:     !noOptimizer,
		// 	// OptimizeExpr:      !noOptimizer,

		// 	// Trace: os.Stdout,
		// 	// TraceParser: true,
		// 	// TraceCompiler: true,
		// 	// TraceOptimizer:    true,
		// 	// OptimizerMaxCycle: 1<<8 - 1,
		// 	// OptimizeConst:     false,
		// 	// OptimizeExpr:      false,
		// }

	}

	if tk.StartsWith(codeA, "//TXDEF#") {
		codeA = tk.DecryptStringByTXDEF(codeA, "char")
	}

	// tk.Pln(2.1)
	bytecodeT, errT := charlang.Compile([]byte(codeA), charlang.MainCompilerOptions) // charlang.DefaultCompilerOptions)
	if errT != nil {
		return errT
	}
	// tk.Pln(2.2)

	// inParasT := make(charlang.Map, len(paraMapT))
	// for k, v := range paraMapT {
	// 	inParasT[k] = charlang.ToString(v)
	// }

	// inParasT := make(charlang.Map, 0)

	envT := charlang.Map{}

	// envT["tk"] = charlang.TkFunction
	envT["argsG"] = charlang.ConvertToObject(os.Args)
	envT["versionG"] = charlang.ToStringObject(charlang.VersionG)
	envT["scriptPathG"] = charlang.ToStringObject(scriptPathA)
	envT["basePathG"] = charlang.ToStringObject(basePathG)
	envT["runModeG"] = charlang.ToStringObject(tk.GetSwitch(argsA, "-runMode=", "script"))

	vmT := charlang.NewVM(bytecodeT)

	retT, errT := vmT.Run(envT) // inParasT,

	if errT != nil {

		// tk.Pl()

		// f, l := QlVMG.Code.Line(QlVMG.Code.Reserve().Next())
		// tk.Pl("Next line: %v, %v", f, l)

		logFileT := tk.GetSwitch(argsA, "-logFile=", "")

		if logFileT != "nil" {
			tk.AppendStringToFile(fmt.Sprintf("\n[%v] %v %v\n", tk.GetNowTimeString(), scriptPathA, errT), logFileT)
		}

		return tk.Errf("failed to execute script(%v) error: %v\n", scriptPathA, errT)
	}

	return charlang.ConvertFromObject(retT)
}

func Svc() {
	if tk.GetOSName() == "windows" {
		currentOSG = "win"
		if tk.Trim(basePathG) == "." || strings.TrimSpace(basePathG) == "" {
			basePathG = "c:\\" + "char" // serviceNameG
		}
		configFileNameG = serviceNameG + "win.cfg"
	} else {
		currentOSG = "linux"
		if tk.Trim(basePathG) == "." || strings.TrimSpace(basePathG) == "" {
			basePathG = "/" + "char" //  + serviceNameG
		}
		configFileNameG = serviceNameG + "linux.cfg"
	}

	if !tk.IfFileExists(basePathG) {
		os.MkdirAll(basePathG, 0777)
	}

	tk.SetLogFile(filepath.Join(basePathG, serviceNameG+".log"))

	defer func() {
		if v := recover(); v != nil {
			tk.LogWithTimeCompact("panic in service: %v", v)
		}
	}()

	tk.DebugModeG = true

	tk.LogWithTimeCompact("%v V%v", serviceNameG, charlang.VersionG)
	tk.LogWithTimeCompact("os: %v, basePathG: %v, configFileNameG: %v, scriptPathG: %v", runtime.GOOS, basePathG, configFileNameG, scriptPathG)
	tk.LogWithTimeCompact("command-line args: %v", os.Args)

	// tk.Pl("os: %v, basePathG: %v, configFileNameG: %v", runtime.GOOS, basePathG, configFileNameG)

	cfgFileNameT := filepath.Join(basePathG, configFileNameG)
	if tk.IfFileExists(cfgFileNameT) {
		fileContentT := tk.LoadSimpleMapFromFile(cfgFileNameT)

		if fileContentT != nil {
			basePathG = fileContentT["charBasePath"]
		}
	}

	tk.LogWithTimeCompact("Service started.")
	// tk.LogWithTimeCompact("Using config file: %v", cfgFileNameT)

	runAutoRemoveTask := func() {
		for {
			taskFileListT := tk.GetFileList(basePathG, "-pattern=autoRemoveTask*.char", "-sort=asc", "-sortKey=Name")

			if len(taskFileListT) > 0 {
				for i, v := range taskFileListT {

					fcT := tk.LoadStringFromFile(v["Abs"])

					if tk.IsErrX(fcT) {
						tk.LogWithTimeCompact("failed to load run-then-remove task - [%v] %v: %v", i, v["Abs"], tk.GetErrStrX(fcT))
						continue
					}

					tk.LogWithTimeCompact("running run-then-remove task: %v ...", v["Abs"])

					scriptPathG = v["Abs"]

					rs := QuickRunChar(fcT, scriptPathG)
					if !tk.IsUndefined(rs) && !tk.IsNil(rs) {
						tk.LogWithTimeCompact("task result: %v", rs)
					}

					tk.RemoveFile(v["Abs"])
				}
			}

			tk.Sleep(5.0)

		}

	}

	go runAutoRemoveTask()

	runThreadTask := func() {
		taskFileListT := tk.GetFileList(basePathG, "-pattern=threadTask*.char", "-sort=asc", "-sortKey=Name")

		if len(taskFileListT) > 0 {
			for i, v := range taskFileListT {

				fcT := tk.LoadStringFromFile(v["Abs"])

				if tk.IsErrX(fcT) {
					tk.LogWithTimeCompact("failed to load thread task - [%v] %v: %v", i, v["Abs"], tk.GetErrStrX(fcT))
					continue
				}

				tk.LogWithTimeCompact("running thread task: %v ...", v["Abs"])

				scriptPathG = v["Abs"]

				go QuickRunChar(fcT, scriptPathG, "-logFile="+filepath.Join(basePathG, "runThreadTask.log"))
			}
		}

		tk.Sleep(5.0)

	}

	go runThreadTask()

	taskFileListT := tk.GetFileList(basePathG, "-pattern=task*.char", "-sort=asc", "-sortKey=Name")

	if len(taskFileListT) > 0 {
		for i, v := range taskFileListT {

			fcT := tk.LoadStringFromFile(v["Abs"])

			if tk.IsErrX(fcT) {
				tk.LogWithTimeCompact("failed to load auto task - [%v] %v: %v", i, v["Abs"], tk.GetErrStrX(fcT))
				continue
			}

			tk.LogWithTimeCompact("running task: %v ...", v["Abs"])

			scriptPathG = v["Abs"]

			rs := QuickRunChar(fcT, scriptPathG)
			if !tk.IsUndefined(rs) && !tk.IsNil(rs) {
				tk.LogWithTimeCompact("auto task result: %v", rs)
			}
		}
	}

	// c := 0
	for {
		tk.Sleep(60.0)

		// c++
		// tk.Pl("c: %v", c)
		// tk.LogWithTimeCompact("c: %v", c)
	}

}

var exitG = make(chan struct{})

func doWork() {
	serviceModeG = true

	go Svc()

	for {
		select {
		case <-exitG:
			os.Exit(0)
			return
		}
	}
}

// var scriptGlobals = &charlang.SyncMap{
// 	Map: charlang.Map{
// 		"Gosched": &charlang.Function{
// 			Name: "Gosched",
// 			Value: func(args ...charlang.Object) (charlang.Object, error) {
// 				runtime.Gosched()
// 				return charlang.Undefined, nil
// 			},
// 		},
// 		"versionG": charlang.ToString(charlang.VersionG),
// 		"argsG":    charlang.ConvertToObject(os.Args[1:]),
// 		"tk":       charlang.TkFunction,
// 		// "tk": &charlang.Function{
// 		// 	Name: "Do",
// 		// 	Value: func(args ...charlang.Object) (charlang.Object, error) {

// 		// 		if len(args) < 1 {
// 		// 			return charlang.Undefined, charlang.NewCommonError("not enough paramters")
// 		// 		}

// 		// 		if args[0].TypeName() != "string" {
// 		// 			return charlang.Undefined, charlang.NewCommonError("invalid type for command")
// 		// 		}

// 		// 		cmdT := args[0].String()

// 		// 		switch cmdT {
// 		// 		case "test":
// 		// 			tk.Pl("args: %v", args[1:])
// 		// 			return charlang.ConvertToObject("Response!"), nil

// 		// 		case "getNowTime":
// 		// 			return charlang.ConvertToObject(time.Now()), nil

// 		// 		default:
// 		// 			return charlang.Undefined, charlang.NewCommonError("unknown comman")
// 		// 		}

// 		// 		return charlang.Undefined, nil
// 		// 	},
// 		// },
// 	},
// }

// var grepl *repl

// type repl struct {
// 	ctx          context.Context
// 	eval         *charlang.Eval
// 	lastBytecode *charlang.Bytecode
// 	lastResult   charlang.Object
// 	multiline    string
// 	werr         prompt.ConsoleWriter
// 	wout         prompt.ConsoleWriter
// 	stdout       io.Writer
// 	commands     map[string]func()
// }

// func newREPL(ctx context.Context, stdout io.Writer, cw prompt.ConsoleWriter) *repl {
// 	moduleMap := charlang.NewModuleMap()
// 	// moduleMap.AddBuiltinModule("time", ugotime.Module).
// 	// 	AddBuiltinModule("strings", ugostrings.Module).
// 	moduleMap.AddBuiltinModule("ex", ugoex.Module)

// 	opts := charlang.CompilerOptions{
// 		ModulePath:        "(repl)",
// 		ModuleMap:         moduleMap,
// 		SymbolTable:       charlang.NewSymbolTable(),
// 		OptimizerMaxCycle: charlang.TraceCompilerOptions.OptimizerMaxCycle,
// 		TraceParser:       traceParser,
// 		TraceOptimizer:    traceOptimizer,
// 		TraceCompiler:     traceCompiler,
// 		OptimizeConst:     !noOptimizer,
// 		OptimizeExpr:      !noOptimizer,
// 	}

// 	if stdout == nil {
// 		stdout = os.Stdout
// 	}

// 	if traceEnabled {
// 		opts.Trace = stdout
// 	}

// 	r := &repl{
// 		ctx:    ctx,
// 		eval:   charlang.NewEval(opts, scriptGlobals),
// 		werr:   cw,
// 		wout:   cw,
// 		stdout: stdout,
// 	}

// 	r.commands = map[string]func(){
// 		".bytecode":      r.cmdBytecode,
// 		".builtins":      r.cmdBuiltins,
// 		".gc":            r.cmdGC,
// 		".globals":       r.cmdGlobals,
// 		".globals+":      r.cmdGlobalsVerbose,
// 		".locals":        r.cmdLocals,
// 		".locals+":       r.cmdLocalsVerbose,
// 		".return":        r.cmdReturn,
// 		".return+":       r.cmdReturnVerbose,
// 		".symbols":       r.cmdSymbols,
// 		".modules_cache": r.cmdModulesCache,
// 		".memory_stats":  r.cmdMemoryStats,
// 		".reset":         r.cmdReset,
// 		".exit":          func() { os.Exit(0) },
// 	}
// 	return r
// }

// func (r *repl) cmdBytecode() {
// 	_, _ = fmt.Fprintf(r.stdout, "%s\n", r.lastBytecode)
// }

// func (r *repl) cmdBuiltins() {
// 	builtins := make([]string, len(charlang.BuiltinsMap))

// 	for k, v := range charlang.BuiltinsMap {
// 		remarkT := ""
// 		if nv, ok := (charlang.BuiltinObjects[v]).(*charlang.BuiltinFunction); ok {
// 			remarkT = nv.Remark
// 		}
// 		builtins[v] = fmt.Sprint(charlang.BuiltinObjects[v].TypeName(), ":", k, remarkT)
// 	}
// 	_, _ = fmt.Fprintln(r.stdout, strings.Join(builtins, "\n"))
// }

// func (*repl) cmdGC() { runtime.GC() }

// func (r *repl) cmdGlobals() {
// 	_, _ = fmt.Fprintf(r.stdout, "%+v\n", r.eval.Globals)
// }

// func (r *repl) cmdGlobalsVerbose() {
// 	_, _ = fmt.Fprintf(r.stdout, "%#v\n", r.eval.Globals)
// }

// func (r *repl) cmdLocals() {
// 	_, _ = fmt.Fprintf(r.stdout, "%+v\n", r.eval.Locals)
// }

// func (r *repl) cmdLocalsVerbose() {
// 	fmt.Fprintf(r.stdout, "%#v\n", r.eval.Locals)
// }

// func (r *repl) cmdReturn() {
// 	_, _ = fmt.Fprintf(r.stdout, "%#v\n", r.lastResult)
// }

// func (r *repl) cmdReturnVerbose() {
// 	if r.lastResult != nil {
// 		_, _ = fmt.Fprintf(r.stdout,
// 			"GoType:%[1]T, TypeName:%[2]s, Value:%#[1]v\n",
// 			r.lastResult, r.lastResult.TypeName())
// 	} else {
// 		_, _ = fmt.Fprintln(r.stdout, "<nil>")
// 	}
// }

// func (r *repl) cmdReset() {
// 	grepl = newREPL(r.ctx, r.stdout, r.wout)
// }

// func (r *repl) cmdSymbols() {
// 	_, _ = fmt.Fprintf(r.stdout, "%v\n", r.eval.Opts.SymbolTable.Symbols())
// }

// func (r *repl) cmdMemoryStats() {
// 	// writeMemStats writes the formatted current, total and OS memory
// 	// being used. As well as the number of garbage collection cycles completed.
// 	var m runtime.MemStats
// 	runtime.ReadMemStats(&m)

// 	_, _ = fmt.Fprintf(r.stdout, "Go Memory Stats see: "+
// 		"https://golang.org/pkg/runtime/#MemStats\n\n")
// 	_, _ = fmt.Fprintf(r.stdout, "HeapAlloc = %s", humanFriendlySize(m.HeapAlloc))
// 	_, _ = fmt.Fprintf(r.stdout, "\tHeapObjects = %v", m.HeapObjects)
// 	_, _ = fmt.Fprintf(r.stdout, "\tSys = %s", humanFriendlySize(m.Sys))
// 	_, _ = fmt.Fprintf(r.stdout, "\tNumGC = %v\n", m.NumGC)
// }

// func (r *repl) cmdModulesCache() {
// 	_, _ = fmt.Fprintf(r.stdout, "%v\n", r.eval.ModulesCache)
// }

// func (r *repl) writeErrorStr(msg string) {
// 	r.werr.SetColor(prompt.Red, prompt.DefaultColor, true)
// 	r.werr.WriteStr(msg)
// 	_ = r.werr.Flush()
// }

// func (r *repl) writeStr(msg string) {
// 	r.wout.SetColor(prompt.Green, prompt.DefaultColor, false)
// 	r.wout.WriteStr(msg)
// 	_ = r.wout.Flush()
// }

// func (r *repl) executor(line string) {
// 	switch {
// 	case line == "":
// 		if !isMultiline {
// 			return
// 		}
// 	case line[0] == '.':
// 		if fn, ok := r.commands[line]; ok {
// 			fn()
// 			return
// 		}
// 	case strings.HasSuffix(line, "\\"):
// 		isMultiline = true
// 		r.multiline += line[:len(line)-1] + "\n"
// 		return
// 	}
// 	r.executeScript(line)
// }

// func (r *repl) executeScript(line string) {
// 	defer func() {
// 		isMultiline = false
// 		r.multiline = ""
// 	}()

// 	var err error
// 	r.lastResult, r.lastBytecode, err = r.eval.Run(r.ctx, []byte(r.multiline+line))
// 	if err != nil {
// 		r.writeErrorStr(fmt.Sprintf("\n%+v\n", err))
// 		return
// 	}

// 	if err != nil {
// 		r.writeErrorStr(fmt.Sprintf("VM:\n     %+v\n", err))
// 		return
// 	}

// 	switch v := r.lastResult.(type) {
// 	case charlang.String:
// 		r.writeStr(fmt.Sprintf("%q\n", v.Value))
// 	case charlang.Char:
// 		r.writeStr(fmt.Sprintf("%q\n", rune(v)))
// 	case charlang.Bytes:
// 		r.writeStr(fmt.Sprintf("%v\n", []byte(v)))
// 	default:
// 		r.writeStr(fmt.Sprintf("%v\n", r.lastResult))
// 	}

// 	symbols := r.eval.Opts.SymbolTable.Symbols()
// 	suggestions = suggestions[:initialSuggLen]

// 	for _, s := range symbols {
// 		if s.Scope != charlang.ScopeBuiltin {
// 			suggestions = append(suggestions,
// 				prompt.Suggest{
// 					Text:        s.Name,
// 					Description: string(s.Scope) + " variable",
// 				},
// 			)
// 		}
// 	}
// }

// func humanFriendlySize(b uint64) string {
// 	if b < 1024 {
// 		return fmt.Sprint(strconv.FormatUint(b, 10), " bytes")
// 	}

// 	if b >= 1024 && b < 1024*1024 {
// 		return fmt.Sprint(strconv.FormatFloat(
// 			float64(b)/1024, 'f', 1, 64), " KiB")
// 	}

// 	return fmt.Sprint(strconv.FormatFloat(
// 		float64(b)/1024/1024, 'f', 1, 64), " MiB")
// }

// func completer(in prompt.Document) []prompt.Suggest {
// 	// w := in.GetWordBeforeCursorWithSpace()
// 	return nil
// 	// return prompt.FilterHasPrefix(suggestions, w, true)
// }

// var suggestions = []prompt.Suggest{
// 	// Commands
// 	{Text: ".bytecode", Description: "Print Bytecode"},
// 	{Text: ".builtins", Description: "Print Builtins"},
// 	{Text: ".reset", Description: "Reset"},
// 	{Text: ".locals", Description: "Print Locals"},
// 	{Text: ".locals+", Description: "Print Locals (verbose)"},
// 	{Text: ".globals", Description: "Print Globals"},
// 	{Text: ".globals+", Description: "Print Globals (verbose)"},
// 	{Text: ".return", Description: "Print Last Return Result"},
// 	{Text: ".return+", Description: "Print Last Return Result (verbose)"},
// 	{Text: ".modules_cache", Description: "Print Modules Cache"},
// 	{Text: ".memory_stats", Description: "Print Memory Stats"},
// 	{Text: ".gc", Description: "Run Go GC"},
// 	{Text: ".symbols", Description: "Print Symbols"},
// 	{Text: ".exit", Description: "Exit"},
// }

// func init() {
// 	// add builtins to suggestions
// 	for k, v := range charlang.BuiltinsMap {
// 		remarkT := ""
// 		if nv, ok := (charlang.BuiltinObjects[v]).(*charlang.BuiltinFunction); ok {
// 			remarkT = nv.Remark
// 		}

// 		suggestions = append(suggestions,
// 			prompt.Suggest{
// 				Text:        k,
// 				Description: "Builtin " + k + remarkT,
// 			},
// 		)
// 	}

// 	for tok := token.Question + 3; tok.IsKeyword(); tok++ {
// 		s := tok.String()
// 		suggestions = append(suggestions, prompt.Suggest{
// 			Text:        s,
// 			Description: "keyword " + s,
// 		})
// 	}
// 	initialSuggLen = len(suggestions)
// }

// func newPrompt(
// 	executor func(s string),
// 	w io.Writer,
// 	poptions ...prompt.Option,
// ) *prompt.Prompt {

// 	// _, _ = fmt.Fprintln(w, "Copyright (c) 2020 Ozan Hacıbekiroğlu")
// 	// _, _ = fmt.Fprintln(w, "License: MIT")
// 	_, _ = fmt.Fprintln(w, "Press Ctrl+D to exit or use .exit command")
// 	_, _ = fmt.Fprintln(w, logo)

// 	options := []prompt.Option{
// 		prompt.OptionPrefix(promptPrefix),
// 		prompt.OptionHistory([]string{
// 			"a := 1",
// 			"sum := func(...a) { total:=0; for v in a { total+=v }; return total }",
// 			"func(a, b){ return a*b }(2, 3)",
// 			`println("")`,
// 			`var (x, y, z); if x { y } else { z }`,
// 			`var (x, y, z); x ? y : z`,
// 			`for i := 0; i < 3; i++ { }`,
// 			`m := {}; for k,v in m { printf("%s:%v\n", k, v) }`,
// 			`try { } catch err { } finally { }`,
// 		}),
// 		prompt.OptionLivePrefix(func() (string, bool) {
// 			if isMultiline {
// 				return promptPrefix2, true
// 			}
// 			return "", false
// 		}),
// 		prompt.OptionTitle(title),
// 		prompt.OptionPrefixTextColor(prompt.Yellow),
// 		prompt.OptionPreviewSuggestionTextColor(prompt.Blue),
// 		prompt.OptionSelectedSuggestionBGColor(prompt.LightGray),
// 		prompt.OptionSuggestionBGColor(prompt.DarkGray),
// 	}

// 	options = append(options, poptions...)
// 	return prompt.New(executor, completer, options...)
// }

// func parseFlags(
// 	flagset *flag.FlagSet,
// 	args []string,
// ) (filePath string, timeout time.Duration, err error) {

// 	var trace string
// 	flagset.StringVar(&trace, "dir", "",
// 		`directory to scan`)
// 	flagset.StringVar(&trace, "trace", "",
// 		`Comma separated units: -trace parser,optimizer,compiler`)
// 	flagset.BoolVar(&noOptimizer, "no-optimizer", false, `Disable optimization`)
// 	flagset.DurationVar(&timeout, "timeout", 0,
// 		"Program timeout. It is applicable if a script file is provided and "+
// 			"must be non-zero duration")

// 	flagset.Usage = func() {
// 		_, _ = fmt.Fprint(flagset.Output(),
// 			"Usage: ugo [flags] [uGO script file]\n\n",
// 			"If script file is not provided, REPL terminal application is started\n",
// 			"Use - to read from stdin\n\n",
// 			"\nFlags:\n",
// 		)
// 		flagset.PrintDefaults()
// 	}

// 	if err = flagset.Parse(args); err != nil {
// 		return
// 	}

// 	if trace != "" {
// 		traceEnabled = true
// 		trace = "," + trace + ","
// 		if strings.Contains(trace, ",parser,") {
// 			traceParser = true
// 		}
// 		if strings.Contains(trace, ",optimizer,") {
// 			traceOptimizer = true
// 		}
// 		if strings.Contains(trace, ",compiler,") {
// 			traceCompiler = true
// 		}
// 	}

// 	if flagset.NArg() != 1 {
// 		return
// 	}

// 	filePath = flagset.Arg(0)
// 	if filePath == "-" {
// 		return
// 	}

// 	if _, err = os.Stat(filePath); err != nil {
// 		return
// 	}
// 	return
// }

// func executeScript(ctx context.Context, scr []byte, traceOut io.Writer) error {
// 	opts := charlang.DefaultCompilerOptions
// 	if traceEnabled {
// 		opts.Trace = traceOut
// 		opts.TraceParser = traceParser
// 		opts.TraceCompiler = traceCompiler
// 		opts.TraceOptimizer = traceOptimizer
// 	}

// 	opts.ModuleMap = nil // charlang.NewModuleMap().
// 	// AddBuiltinModule("time", ugotime.Module).
// 	// AddBuiltinModule("strings", ugostrings.Module).
// 	// AddBuiltinModule("fmt", ugofmt.Module)

// 	bc, err := charlang.Compile(scr, opts)
// 	if err != nil {
// 		return err
// 	}

// 	vm := charlang.NewVM(bc)
// 	done := make(chan struct{})
// 	go func() {
// 		defer close(done)
// 		_, err = vm.Run(scriptGlobals)
// 	}()

// 	select {
// 	case <-done:
// 	case <-ctx.Done():
// 		vm.Abort()
// 		<-done
// 		if err == nil {
// 			err = ctx.Err()
// 		}
// 	}
// 	return err
// }

// func checkErr(err error, f func()) {
// 	if err == nil {
// 		return
// 	}

// 	defer os.Exit(1)
// 	_, _ = fmt.Fprintf(os.Stderr, "%+v\n", err)

// 	e := err.(*charlang.RuntimeError)
// 	fmt.Printf("Trace: %v\n", e.StackTrace())

// 	if f != nil {
// 		f()
// 	}
// }

// func main() {
// 	// filePath, timeout, err := parseFlags(flag.CommandLine, os.Args[1:])
// 	// checkErr(err, nil)

// 	filePath := tk.GetParameterByIndexWithDefaultValue(os.Args, 1, "")
// 	timeout := time.Duration(0)

// 	var err error = nil

// 	ctx, cancel := context.WithCancel(context.Background())
// 	defer cancel()

// 	if filePath != "" {
// 		if tk.IfSwitchExistsWhole(os.Args, "-gopath") {
// 			filePath = filepath.Join(tk.GetEnv("GOPATH"), "src", "github.com", "topxeq", "charlang", "cmd", "scripts", filePath)
// 		}
// 		if timeout > 0 {
// 			var c func()
// 			ctx, c = context.WithTimeout(ctx, timeout)
// 			defer c()
// 		}

// 		var script []byte
// 		if filePath == "-" {
// 			script, err = ioutil.ReadAll(os.Stdin)
// 		} else if strings.HasPrefix(filePath, "http") {
// 			rsT := tk.DownloadWebPageX(filePath)

// 			if tk.IsErrStr(rsT) {
// 				script = []byte("")
// 				err = tk.ErrStrToErr(rsT)
// 			} else {
// 				script = []byte(rsT)
// 				err = nil
// 			}

// 		} else {
// 			script, err = ioutil.ReadFile(filePath)
// 		}

// 		checkErr(err, cancel)
// 		err = executeScript(ctx, script, os.Stdout)
// 		checkErr(err, cancel)
// 		return
// 	}

// 	defer handlePromptExit()

// 	cw := prompt.NewStdoutWriter()
// 	grepl = newREPL(ctx, os.Stdout, cw)
// 	newPrompt(
// 		func(s string) { grepl.executor(s) },
// 		os.Stdout,
// 		prompt.OptionWriter(cw),
// 	).Run()
// }

func runInteractiveShell() int {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	// defer handlePromptExit()

	// cw := prompt.NewStdoutWriter()
	// grepl = newREPL(ctx, os.Stdout, cw)
	// newPrompt(
	// 	func(s string) { grepl.executor(s) },
	// 	os.Stdout,
	// 	prompt.OptionWriter(cw),
	// ).Run()

	// return 0

	moduleMap := charlang.NewModuleMap()
	// moduleMap.AddBuiltinModule("time", ugotime.Module).
	// 	AddBuiltinModule("strings", ugostrings.Module).
	// 	AddBuiltinModule("fmt", ugofmt.Module)
	moduleMap.AddBuiltinModule("ex", charex.Module)
	// moduleMap.AddBuiltinModule("fmt", charfmt.Module)

	charlang.MainCompilerOptions = &charlang.CompilerOptions{
		ModulePath:        "", //"(repl)",
		ModuleMap:         moduleMap,
		SymbolTable:       charlang.NewSymbolTable(),
		OptimizerMaxCycle: charlang.TraceCompilerOptions.OptimizerMaxCycle,
		// TraceParser:       traceParser,
		// TraceOptimizer:    traceOptimizer,
		// TraceCompiler:     traceCompiler,
		// OptimizeConst:     !noOptimizer,
		// OptimizeExpr:      !noOptimizer,
	}

	// evalT := charlang.NewEval(opts, scriptGlobals)

	evalT := charlang.NewEvalQuick(map[string]interface{}{"versionG": charlang.VersionG, "argsG": os.Args, "scriptPathG": "", "runModeG": "repl"}, charlang.MainCompilerOptions)

	var following bool
	var source string

	tk.Pl("Charlang %v by TopXeQ", charlang.VersionG)

	scanner := bufio.NewScanner(os.Stdin)

	for {
		if following {
			source += "\n"
			fmt.Print("  ")
		} else {
			fmt.Print("> ")
		}

		if !scanner.Scan() {
			break
		}
		source += scanner.Text()
		if source == "" {
			continue
		}
		if source == "quit()" {
			break
		}

		// stmts, err := parser.ParseSrc(source)

		// if e, ok := err.(*parser.Error); ok {
		// 	es := e.Error()
		// 	if strings.HasPrefix(es, "syntax error: unexpected") {
		// 		if strings.HasPrefix(es, "syntax error: unexpected $end,") {
		// 			following = true
		// 			continue
		// 		}
		// 	} else {
		// 		if e.Pos.Column == len(source) && !e.Fatal {
		// 			fmt.Fprintln(os.Stderr, e)
		// 			following = true
		// 			continue
		// 		}
		// 		if e.Error() == "unexpected EOF" {
		// 			following = true
		// 			continue
		// 		}
		// 	}
		// }

		// gox.RetG = gox.NotFoundG

		// err := gox.QlVMG.SafeEval(source)

		lastResultT, lastBytecodeT, errT := evalT.Run(ctx, []byte(source))

		if verboseG {
			tk.Pln("result:", lastResultT, lastBytecodeT, errT)
		}

		if errT != nil {
			fmt.Fprintln(os.Stderr, errT)
			following = false
			source = ""
			continue
		}

		if lastResultT != nil && lastResultT.TypeCode() != 0 {
			fmt.Println(lastResultT)
		}

		following = false
		source = ""
	}

	if err := scanner.Err(); err != nil {
		if err != io.EOF {
			fmt.Fprintln(os.Stderr, "ReadString error:", err)
			return 12
		}
	}

	return 0
}

func chpHandler(strA string, w http.ResponseWriter, r *http.Request) {
	var paraMapT map[string]string
	var errT error

	r.ParseForm()

	vo := tk.GetFormValueWithDefaultValue(r, "vo", "")

	if vo == "" {
		paraMapT = tk.FormToMap(r.Form)
	} else {
		paraMapT, errT = tk.MSSFromJSON(vo)

		if errT != nil {
			paraMapT = map[string]string{}
		}
	}

	evalT := charlang.NewEvalQuick(map[string]interface{}{"versionG": charlang.VersionG, "argsG": []string{}, "scriptPathG": "", "runModeG": "chp", "paraMapG": paraMapT, "requestG": r, "responseG": w, "reqUriG": r.RequestURI, "reqNameG": r.RequestURI}, charlang.MainCompilerOptions)

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	countT := 0

	replaceFuncT := func(str1A string) string {
		countT++
		// tk.Pl("found: %v", str1A)
		lastResultT, lastBytecodeT, errT := evalT.Run(ctx, []byte(str1A[5:len(str1A)-2]))

		if verboseG {
			tk.Pln("result:", lastResultT, lastBytecodeT, errT)
		}

		if errT != nil {
			return fmt.Sprintf("[%v] %v", countT, tk.ErrorToString(errT))
		}

		if lastResultT != nil && lastResultT.TypeCode() != 0 {
			return fmt.Sprintf("%v", lastResultT)
		}

		return ""
	}

	re := regexp.MustCompile(`(?sm)<\?chp.*?\?>`)

	strT := re.ReplaceAllStringFunc(strA, replaceFuncT)

	w.Header().Set("Access-Control-Allow-Origin", "*")
	w.Header().Set("Access-Control-Allow-Headers", "*")
	w.Header().Set("Content-Type", "text/html; charset=utf-8")

	w.Write([]byte(strT))
}

var staticFS http.Handler = nil

func serveStaticDirHandler(w http.ResponseWriter, r *http.Request) {
	if staticFS == nil {
		// tk.Pl("staticFS: %#v", staticFS)
		// staticFS = http.StripPrefix("/w/", http.FileServer(http.Dir(filepath.Join(basePathG, "w"))))
		hdl := http.FileServer(http.Dir(webPathG))
		// tk.Pl("hdl: %#v", hdl)
		staticFS = hdl
	}

	old := r.URL.Path

	tk.Pl("urlPath: %v", r.URL.Path)

	name := filepath.Join(webPathG, path.Clean(old))

	tk.Pl("name: %v", name)

	info, err := os.Lstat(name)
	if err == nil {
		if !info.IsDir() {
			if strings.HasSuffix(name, ".chp") {
				chpHandler(tk.LoadStringFromFile(name), w, r)

				return
			}

			staticFS.ServeHTTP(w, r)
			// http.ServeFile(w, r, name)
		} else {
			if tk.IfFileExists(filepath.Join(name, "index.html")) {
				staticFS.ServeHTTP(w, r)
			} else {
				http.NotFound(w, r)
			}
		}
	} else {
		http.NotFound(w, r)
	}

}

func startHttpsServer(portA string) {
	if !tk.StartsWith(portA, ":") {
		portA = ":" + portA
	}

	err := http.ListenAndServeTLS(portA, filepath.Join(certPathG, "server.crt"), filepath.Join(certPathG, "server.key"), muxG)
	if err != nil {
		tk.PlNow("failed to start https: %v", err)
	}

}

func doServer() {
	charlang.ServerModeG = true
	// charlang.RunModeG = "server"

	portG = tk.GetSwitch(os.Args, "-port=", portG)
	sslPortG = tk.GetSwitch(os.Args, "-sslPort=", sslPortG)

	verboseG = tk.IfSwitchExistsWhole(os.Args, "-verbose")

	if !tk.StartsWith(portG, ":") {
		portG = ":" + portG
	}

	if !tk.StartsWith(sslPortG, ":") {
		sslPortG = ":" + sslPortG
	}

	basePathG = tk.GetSwitch(os.Args, "-dir=", basePathG)
	webPathG = tk.GetSwitch(os.Args, "-webDir=", basePathG)
	certPathG = tk.GetSwitch(os.Args, "-certDir=", basePathG)

	muxG = http.NewServeMux()

	muxG.HandleFunc("/charms/", doCharms)
	muxG.HandleFunc("/charms", doCharms)

	// dynamic content
	muxG.HandleFunc("/dc/", doCharmsContent)
	muxG.HandleFunc("/dc", doCharmsContent)

	muxG.HandleFunc("/", serveStaticDirHandler)

	tk.PlNow("Charlang Server %v -port=%v -sslPort=%v -dir=%v -webDir=%v -certDir=%v", charlang.VersionG, portG, sslPortG, basePathG, webPathG, certPathG)

	if sslPortG != "" {
		tk.PlNow("try starting ssl server on %v...", sslPortG)
		go startHttpsServer(sslPortG)
	}

	tk.Pl("try starting server on %v ...", portG)
	err := http.ListenAndServe(portG, muxG)

	if err != nil {
		tk.PlNow("failed to start: %v", err)
	}

}

func genFailCompact(titleA, msgA string, optsA ...string) string {
	mapT := map[string]string{
		"msgTitle":    titleA,
		"msg":         msgA,
		"subMsg":      "",
		"actionTitle": "back",
		"actionHref":  "javascript:history.back();",
	}

	var fileNameT = "fail.html"

	if tk.IfSwitchExists(optsA, "-compact") {
		fileNameT = "failcompact.html"
	}

	tmplT := tk.LoadStringFromFile(filepath.Join(basePathG, "tmpl", fileNameT))

	if tk.IsErrStr(tmplT) {
		tmplT = `<!DOCTYPE html>
		<html>
		<head>
			<meta charset="utf-8">
			<meta http-equiv="content-type" content="text/html; charset=UTF-8" />
			<meta name='viewport' content='width=device-width; initial-scale=1.0; maximum-scale=4.0; user-scalable=1;' />
		</head>
		
		<body>
			<div>
				<h2>TX_msgTitle_XT</h2>
				<p>TX_msg_XT</p>
			</div>
			<div>
				<p>TX_subMsg_XT</p>
			</div>
			<div style="display: none;">
				<p>
					<a href="TX_actionHref_XT">TX_actionTitle_XT</a>
				</p>
			</div>
		</body>
		
		</html>`
	}

	tmplT = tk.ReplaceHtmlByMap(tmplT, mapT)

	return tmplT
}

func doCharms(res http.ResponseWriter, req *http.Request) {
	if res != nil {
		res.Header().Set("Access-Control-Allow-Origin", "*")
		res.Header().Set("Access-Control-Allow-Headers", "*")
		res.Header().Set("Content-Type", "text/html; charset=utf-8")
	}

	if req != nil {
		req.ParseForm()
	}

	reqT := tk.GetFormValueWithDefaultValue(req, "charms", "")
	if verboseG {
		tk.Pl("RequestURI: %v", req.RequestURI)
	}

	if reqT == "" {
		if tk.StartsWith(req.RequestURI, "/charms") {
			reqT = req.RequestURI[7:]
		}
	}

	tmps := tk.Split(reqT, "?")
	if len(tmps) > 1 {
		reqT = tmps[0]
	}

	if tk.StartsWith(reqT, "/") {
		reqT = reqT[1:]
	}

	var paraMapT map[string]string
	var errT error

	vo := tk.GetFormValueWithDefaultValue(req, "vo", "")

	if vo == "" {
		paraMapT = tk.FormToMap(req.Form)
	} else {
		paraMapT, errT = tk.MSSFromJSON(vo)

		if errT != nil {
			res.Write([]byte(tk.ErrStrf("action failed: %v", "invalid vo format")))
			return
		}
	}

	if verboseG {
		tk.Pl("[%v] REQ: %#v (%#v)", tk.GetNowTimeStringFormal(), reqT, paraMapT)
	}

	toWriteT := ""

	fileNameT := reqT

	if !tk.EndsWith(fileNameT, ".char") {
		fileNameT += ".char"
	}

	fullPathT := filepath.Join(basePathG, fileNameT)

	if verboseG {
		tk.Pl("[%v] file path: %#v", tk.GetNowTimeStringFormal(), fullPathT)
	}

	fcT := tk.LoadStringFromFile(fullPathT)
	if tk.IsErrStr(fcT) {
		res.Write([]byte(tk.ErrStrf("action failed: %v", tk.GetErrStr(fcT))))
		return
	}

	// paraMapT["_reqHost"] = req.Host
	// paraMapT["_reqInfo"] = fmt.Sprintf("%#v", req)

	toWriteT, errT = charlang.RunScriptOnHttp(fcT, nil, res, req, paraMapT["input"], nil, paraMapT, map[string]interface{}{"scriptPathG": fullPathT, "runModeG": "charms", "basePathG": basePathG}, "-base="+basePathG)

	if errT != nil {
		res.Header().Set("Content-Type", "text/html; charset=utf-8")

		errStrT := tk.ErrStrf("action failed: %v", errT)
		tk.Pln(errStrT)
		res.Write([]byte(errStrT))
		return
	}

	if toWriteT == "TX_END_RESPONSE_XT" {
		return
	}

	res.Header().Set("Content-Type", "text/html; charset=utf-8")

	res.Write([]byte(toWriteT))
}

func doCharmsContent(res http.ResponseWriter, req *http.Request) {
	if res != nil {
		res.Header().Set("Access-Control-Allow-Origin", "*")
		res.Header().Set("Access-Control-Allow-Headers", "*")
		res.Header().Set("Content-Type", "text/html; charset=utf-8")
	}

	if req != nil {
		req.ParseForm()
		req.ParseMultipartForm(1000000000000)
	}

	reqT := tk.GetFormValueWithDefaultValue(req, "dc", "")

	if charlang.GlobalsG.VerboseLevel > 0 {
		tk.Pl("RequestURI: %v", req.RequestURI)
	}

	if reqT == "" {
		if tk.StartsWith(req.RequestURI, "/dc") {
			reqT = req.RequestURI[3:]
		}
	}

	tmps := tk.Split(reqT, "?")
	if len(tmps) > 1 {
		reqT = tmps[0]
	}

	if tk.StartsWith(reqT, "/") {
		reqT = reqT[1:]
	}

	var paraMapT map[string]string
	var errT error

	vo := tk.GetFormValueWithDefaultValue(req, "vo", "")

	if vo == "" {
		paraMapT = tk.FormToMap(req.Form)
	} else {
		paraMapT, errT = tk.MSSFromJSON(vo)

		if errT != nil {
			res.Write([]byte(genFailCompact("action failed", "invalid parameter format", "-compact")))
			return
		}
	}

	if charlang.GlobalsG.VerboseLevel > 0 {
		tk.Pl("[%v] REQ: %#v (%#v)", tk.GetNowTimeStringFormal(), reqT, paraMapT)
	}

	toWriteT := ""

	fileNameT := "router"

	if !tk.EndsWith(fileNameT, ".char") {
		fileNameT += ".char"
	}

	// fcT := tk.LoadStringFromFile(filepath.Join(basePathG, "xms", fileNameT))
	// absT, _ := filepath.Abs(filepath.Join(basePathG, fileNameT))
	// tk.Pln("loading", absT)

	fullPathT := filepath.Join(basePathG, fileNameT)

	fcT := tk.LoadStringFromFile(fullPathT)
	if tk.IsErrStr(fcT) {
		res.Write([]byte(genFailCompact("action failed", tk.GetErrStr(fcT), "-compact")))
		return
	}

	// vmT := xie.NewVMQuick(nil)

	// vmT.SetVar(nil, "paraMapG", paraMapT)
	// vmT.SetVar(nil, "requestG", req)
	// vmT.SetVar(nil, "responseG", res)
	// vmT.SetVar(nil, "reqNameG", reqT)
	// vmT.SetVar(nil, "basePathG", basePathG)

	// vmT.SetVar("inputG", objA)

	// lrs := vmT.Load(nil, fcT)

	// contentTypeT := res.Header().Get("Content-Type")

	// if tk.IsErrX(lrs) {
	// 	if tk.StartsWith(contentTypeT, "text/json") {
	// 		res.Write([]byte(tk.GenerateJSONPResponse("fail", tk.Spr("action failed: %v", tk.GetErrStrX(lrs)), req)))
	// 		return
	// 	}

	// 	res.Write([]byte(genFailCompact("action failed", tk.GetErrStrX(lrs), "-compact")))
	// 	return
	// }

	// rs := vmT.Run()

	toWriteT, errT = charlang.RunScriptOnHttp(fcT, nil, res, req, paraMapT["input"], nil, paraMapT, map[string]interface{}{"scriptPathG": fullPathT, "runModeG": "chardc", "basePathG": basePathG}, "-base="+basePathG)

	if errT != nil {
		res.Header().Set("Content-Type", "text/html; charset=utf-8")

		errStrT := tk.ErrStrf("action failed: %v", errT)
		tk.Pln(errStrT)
		res.Write([]byte(errStrT))
		return
	}

	if toWriteT == "TX_END_RESPONSE_XT" {
		return
	}

	res.Header().Set("Content-Type", "text/html; charset=utf-8")

	res.Write([]byte(toWriteT))
	contentTypeT := res.Header().Get("Content-Type")

	if errT != nil {
		if tk.StartsWith(contentTypeT, "text/json") {
			res.Write([]byte(tk.GenerateJSONPResponse("fail", tk.Spr("action failed: %v", errT), req)))
			return
		}

		res.Write([]byte(genFailCompact("action failed", errT.Error(), "-compact")))
		return
	}

	if toWriteT == "TX_END_RESPONSE_XT" {
		return
	}

	res.Header().Set("Content-Type", "text/html; charset=utf-8")

	res.Write([]byte(toWriteT))

}

func runArgs(argsA ...string) interface{} {
	argsT := argsA

	if tk.IfSwitchExistsWhole(argsT, "-version") {
		tk.Pl("Charlang by TopXeQ V%v", charlang.VersionG)
		return nil // "github.com/topxeq/charlang"
	}

	if tk.IfSwitchExistsWhole(argsT, "-h") {
		// showHelp()
		tk.Pl("Charlang by TopXeQ V%v", charlang.VersionG)
		return nil
	}

	if tk.IfSwitchExistsWhole(argsT, "-env") {
		tk.Pl("Charlang by TopXeQ V%v", charlang.VersionG)

		basePathT := tk.EnsureBasePathInHome("char")

		tk.Pl("basePath: %v", basePathT)

		tk.Pl("localPath: %v", charlang.GetCfgString("localScriptPath.cfg"))

		tk.Pl("cloudPath: %v", charlang.GetCfgString("cloud.cfg"))

		return nil
	}

	scriptT := tk.GetParameterByIndexWithDefaultValue(argsT, 1, "")

	scriptPathT := ""

	// GUI related start

	// full version related start
	// if tk.IfSwitchExistsWhole(argsT, "-edit") {
	// 	// editFile(scriptT)
	// 	rs := gox.RunScriptX(gox.EditFileScriptG, argsT...)

	// 	if rs != gox.NotFoundG && rs != nil {
	// 		tk.Pl("%v", rs)
	// 	}

	// 	return nil
	// }
	// full version related end

	// GUI related end

	if tk.IfSwitchExistsWhole(argsT, "-initgui") {
		// applicationPathT := tk.GetApplicationPath()

		// osT := tk.GetOSName()

		// if tk.Contains(osT, "inux") {
		// 	tk.Pl("Please visit the following URL to find out how to make Sciter environment ready in Linux: ")

		// 	return nil
		// } else if tk.Contains(osT, "arwin") {
		// 	tk.Pl("Please visit the following URL to find out how to make Sciter environment ready in Linux: ")

		// 	return nil
		// } else {
		// 	// rs := tk.DownloadFile("http://scripts.frenchfriend.net/pub/sciterts.dll", applicationPathT, "sciterts.dll")

		// 	// if tk.IsErrorString(rs) {

		// 	// 	return tk.Errf("failed to download Sciter DLL file.")
		// 	// }

		// 	// tk.Pl("Sciter DLL downloaded to application path.")

		// 	// rs = tk.DownloadFile("http://scripts.frenchfriend.net/pub/webview.dll", applicationPathT, "webview.dll", false)

		// 	// if tk.IsErrorString(rs) {

		// 	// 	return tk.Errf("failed to download webview DLL file.")
		// 	// }

		// 	// rs = tk.DownloadFile("http://scripts.frenchfriend.net/pub/WebView2Loader.dll", applicationPathT, "WebView2Loader.dll", false)

		// 	// if tk.IsErrorString(rs) {

		// 	// 	return tk.Errf("failed to download webview DLL file.")
		// 	// }

		// 	// tk.Pl("webview DLL downloaded to application path.")

		// 	return nil
		// }
	}

	charlang.DebugModeG = tk.IfSwitchExistsWhole(argsT, "-debug")

	// ifXieT := tk.IfSwitchExistsWhole(argsT, "-xie")
	ifClipT := tk.IfSwitchExistsWhole(argsT, "-clip")
	ifEmbedT := (charlang.CodeTextG != "") && (!tk.IfSwitchExistsWhole(argsT, "-noembed"))
	ifSelectScriptT := tk.IfSwitchExistsWhole(argsT, "-selectScript")
	ifEditT := tk.IfSwitchExistsWhole(argsT, "-edit")
	ifExampleT := tk.IfSwitchExistsWhole(argsT, "-exam") || tk.IfSwitchExistsWhole(argsT, "-example")

	ifInExeT := false
	inExeCodeT := ""

	binNameT, errT := os.Executable()
	if errT != nil {
		binNameT = ""
	}

	baseBinNameT := filepath.Base(binNameT)

	text1T := tk.Trim(`740404`)
	text2T := tk.Trim(`690415`)
	text3T := tk.Trim(`040626`)

	if binNameT != "" {
		if !tk.StartsWith(baseBinNameT, "char") {
			buf1, errT := tk.LoadBytesFromFileE(binNameT)
			if errT == nil {
				re := regexp.MustCompile(text1T + text2T + text3T + `(.*?) *` + text3T + text2T + text1T)
				matchT := re.FindAllSubmatch(buf1, -1)

				if matchT != nil && len(matchT) > 0 {
					codeStrT := string(matchT[len(matchT)-1][1])

					decCodeT := tk.DecryptStringByTXDEF(codeStrT, "topxeq")
					if !tk.IsErrStr(decCodeT) {
						ifInExeT = true
						inExeCodeT = decCodeT
					}

				}
			}
		}
	}

	if tk.IfSwitchExistsWhole(argsT, "-shell") {
		// gox.InitQLVM()

		// var guiHandlerG tk.TXDelegate = guiHandler

		// gox.QlVMG.SetVar("argsG", argsT)
		// gox.QlVMG.SetVar("guiG", guiHandlerG)

		runInteractiveShell()

		// tk.Pl("not enough parameters")

		return nil
	}

	// if tk.IfSwitchExistsWhole(argsT, "-pipe") {
	// 	// gox.InitQLVM()

	// 	// var guiHandlerG tk.TXDelegate = guiHandler

	// 	// gox.QlVMG.SetVar("argsG", argsT)
	// 	// gox.QlVMG.SetVar("guiG", guiHandlerG)

	// 	runInteractiveShell()

	// 	// tk.Pl("not enough parameters")

	// 	return nil
	// }

	if tk.IfSwitchExistsWhole(argsT, "-server") {

		// gox.InitQLVM()
		doServer()

		// tk.Pl("not enough parameters")

		return nil
	}

	cmdT := tk.GetSwitchWithDefaultValue(argsT, "-cmd=", "")

	if cmdT != "" {
		scriptT = "CMD"
	}

	ifPipeT := tk.IfSwitchExistsWhole(argsT, "-pipe")

	if ifEditT {
		if !ifInExeT {
			ifExampleT = true
			if strings.TrimSpace(scriptT) != "" {
				argsT = append(argsT, "-fromFile="+scriptT)
			}

			scriptT = "editFile.char"
		}
	}

	if scriptT == "" && (!ifClipT) && (!ifSelectScriptT) && (!ifEditT) && (!ifEmbedT) && (!ifInExeT) && (!ifPipeT) {

		// autoPathT := filepath.Join(tk.GetApplicationPath(), "auto.gox")
		// autoGxbPathT := filepath.Join(tk.GetApplicationPath(), "auto.gxb")
		autoPathT := "auto.char"
		autoGxbPathT := "auto.charb"

		if tk.IfFileExists(autoPathT) {
			scriptT = autoPathT
		} else if tk.IfFileExists(autoGxbPathT) {
			scriptT = autoGxbPathT
		} else {
			// gox.InitQLVM()

			// var guiHandlerG tk.TXDelegate = guiHandler

			// gox.QlVMG.SetVar("argsG", argsT)
			// gox.QlVMG.SetVar("guiG", guiHandlerG)

			runInteractiveShell()

			// tk.Pl("not enough parameters")

			return nil
		}

	}

	encryptCodeT := tk.GetSwitchWithDefaultValue(argsT, "-encrypt=", "")

	if encryptCodeT != "" {
		fcT := tk.LoadStringFromFile(scriptT)

		if tk.IsErrorString(fcT) {

			return tk.Errf("failed to load file [%v]: %v", scriptT, tk.GetErrorString(fcT))
		}

		encStrT := tk.EncryptStringByTXDEF(fcT, encryptCodeT)

		if tk.IsErrorString(encStrT) {

			return tk.Errf("failed to encrypt content [%v]: %v", scriptT, tk.GetErrorString(encStrT))
		}

		rsT := tk.SaveStringToFile("//TXDEF#"+encStrT, scriptT+"e")

		if tk.IsErrorString(rsT) {

			return tk.Errf("failed to encrypt file [%v]: %v", scriptT, tk.GetErrorString(rsT))
		}

		return nil
	}

	decryptCodeT := tk.GetSwitchWithDefaultValue(argsT, "-decrypt=", "")

	if decryptCodeT != "" {
		fcT := tk.LoadStringFromFile(scriptT)

		if tk.IsErrorString(fcT) {

			return tk.Errf("failed to load file [%v]: %v", scriptT, tk.GetErrorString(fcT))
		}

		decStrT := tk.DecryptStringByTXDEF(fcT, decryptCodeT)

		if tk.IsErrorString(decStrT) {

			return tk.Errf("failed to decrypt content [%v]: %v", scriptT, tk.GetErrorString(decStrT))
		}

		rsT := tk.SaveStringToFile(decStrT, scriptT+"d")

		if tk.IsErrorString(rsT) {

			return tk.Errf("failed to decrypt file [%v]: %v", scriptT, tk.GetErrorString(rsT))
		}

		return nil
	}

	decryptRunCodeT := tk.GetSwitchWithDefaultValue(argsT, "-decrun=", "")

	ifBatchT := tk.IfSwitchExistsWhole(argsT, "-batch")

	if !ifBatchT {
		if tk.EndsWithIgnoreCase(scriptT, ".gxb") {
			ifBatchT = true
		}
	}

	// ifBinT := tk.IfSwitchExistsWhole(argsT, "-bin")
	// if ifBinT {
	// }

	ifRunT := tk.IfSwitchExistsWhole(argsT, "-run")
	ifGoPathT := tk.IfSwitchExistsWhole(argsT, "-gopath")
	ifLocalT := tk.IfSwitchExistsWhole(argsT, "-local")
	ifAppPathT := tk.IfSwitchExistsWhole(argsT, "-apppath")
	ifRemoteT := tk.IfSwitchExistsWhole(argsT, "-remote")
	ifCloudT := tk.IfSwitchExistsWhole(argsT, "-cloud")
	sshT := tk.GetSwitchWithDefaultValue(argsT, "-ssh=", "")
	ifViewT := tk.IfSwitchExistsWhole(argsT, "-view")
	ifViewPageT := tk.IfSwitchExistsWhole(argsT, "-viewPage")
	ifOpenT := tk.IfSwitchExistsWhole(argsT, "-open")
	ifCompileT := tk.IfSwitchExistsWhole(argsT, "-compile")

	charlang.VerboseG = tk.IfSwitchExistsWhole(argsT, "-verbose")

	// ifMagicT := false
	// magicNumberT, errT := tk.StrToIntE(scriptT)

	// if errT == nil {
	// 	ifMagicT = true
	// }

	if ifViewPageT {
		if !ifInExeT {
			tk.RunWinFileWithSystemDefault(fmt.Sprintf("http://topget.org/dc/c/charlang/example/%v", scriptT))
		}

		return nil
	}

	var fcT string

	if ifInExeT && inExeCodeT != "" && !tk.IfSwitchExistsWhole(argsT, "-noin") {
		fcT = inExeCodeT

		scriptPathT = ""
	} else if cmdT != "" {
		fcT = cmdT

		if tk.IfSwitchExistsWhole(argsT, "-urlDecode") {
			fcT = tk.UrlDecode(fcT)
		}

		scriptPathT = ""
		// } else if ifMagicT {
		// 	fcT = gox.GetMagic(magicNumberT)

		// 	scriptPathT = ""
	} else if ifRunT {
		if tk.IfSwitchExistsWhole(argsT, "-urlDecode") {
			fcT = tk.UrlDecode(scriptT)
		} else {
			fcT = scriptT
		}
		tk.Pl("run cmd(%v)", fcT)

		scriptPathT = ""
	} else if ifRemoteT {
		scriptPathT = scriptT
		fcT = tk.DownloadPageUTF8(scriptT, nil, "", 30)

	} else if ifSelectScriptT {
		scriptPathT = strings.TrimSpace(SelectScript())

		if scriptPathT == "" {
			return tk.Errf("no script selected: %v", scriptPathT)
		}

		if tk.IsErrX(scriptPathT) {
			return tk.Errf("failed to select script: %v", tk.GetErrStrX(scriptPathT))
		}

		fcT = tk.LoadStringFromFile(scriptPathT)

	} else if ifExampleT {
		scriptPathT = "http://topget.org/dc/t/charlang/example/" + scriptT

		fcT = tk.DownloadPageUTF8("http://topget.org/dc/t/charlang/example/"+scriptT, nil, "", 30)

	} else if ifClipT {
		fcT = tk.GetClipText()

		scriptPathT = ""
	} else if ifPipeT {
		// fmt.Println("pipe")
		bufT := bufio.NewReader(os.Stdin)

		b, err := io.ReadAll(bufT)
		if err != nil {
			return tk.Errf("failed to load script from stdin: %v", tk.GetErrorString(fcT))
		}

		// Prints the data in buffer
		// fmt.Println("s1T", string(b))

		scriptPathT = "#PIPE"

		fcT = string(b)

	} else if ifEmbedT {
		fcT = charlang.CodeTextG

		scriptPathT = ""
	} else if ifCloudT {
		basePathT := tk.EnsureBasePathInHome("char")

		gotT := false

		if !strings.HasPrefix(basePathT, "TXERROR:") {
			cfgPathT := tk.JoinPath(basePathT, "cloud.cfg")

			cfgStrT := tk.Trim(tk.LoadStringFromFile(cfgPathT))

			if !tk.IsErrorString(cfgStrT) {
				scriptPathT = cfgStrT + scriptT

				fcT = tk.DownloadPageUTF8(cfgStrT+scriptT, nil, "", 30)

				gotT = true
			}

		}

		if !gotT {
			scriptPathT = scriptT
			fcT = tk.DownloadPageUTF8(scriptT, nil, "", 30)
		}

	} else if sshT != "" {
		fcT = charlang.DownloadStringFromSSH(sshT, scriptT)

		if tk.IsErrorString(fcT) {

			return tk.Errf("failed to get script from SSH: %v", tk.GetErrorString(fcT))
		}

		scriptPathT = ""
	} else if ifGoPathT {
		scriptPathT = filepath.Join(tk.GetEnv("GOPATH"), "src", "github.com", "topxeq", "charlang", "cmd", "scripts", scriptT)

		fcT = tk.LoadStringFromFile(scriptPathT)
	} else if ifAppPathT {
		scriptPathT = filepath.Join(tk.GetApplicationPath(), scriptT)

		fcT = tk.LoadStringFromFile(scriptPathT)
	} else if ifLocalT {
		localPathT := charlang.GetCfgString("localScriptPath.cfg")

		if tk.IsErrorString(localPathT) {
			// tk.Pl("failed to get local path: %v", tk.GetErrorString(localPathT))

			return tk.Errf("failed to get local path: %v", tk.GetErrorString(localPathT))
		}

		// if tk.GetEnv("GOXVERBOSE") == "true" {
		// 	tk.Pl("Try to load script from %v", filepath.Join(localPathT, scriptT))
		// }

		scriptPathT = filepath.Join(localPathT, scriptT)

		fcT = tk.LoadStringFromFile(scriptPathT)
	} else {
		scriptPathT = scriptT
		fcT = tk.LoadStringFromFile(scriptT)

	}

	if tk.IsErrorString(fcT) {
		return tk.Errf("failed to load script from %v: %v", scriptT, tk.GetErrorString(fcT))
	}

	rrStrT := ""

	if strings.HasPrefix(fcT, "//TXRR#") {
		rrStrT = fcT

		fcT = fcT[7:]

		if strings.HasPrefix(fcT, "//TXDEF#") {
			fcT = tk.DecryptStringByTXDEF(strings.TrimSpace(fcT[8:]), "char")
		}

		if strings.HasPrefix(fcT, "http") {
			fcT = tk.ToStr(tk.GetWeb(strings.TrimSpace(fcT)))
		}
	}

	if tk.StartsWith(fcT, "//TXDEF#") {
		if decryptRunCodeT == "" {
			tmps := tk.DecryptStringByTXDEF(fcT, "char")

			if tk.IsErrStr(tmps) {
				tk.Prf("Password: ")
				decryptRunCodeT = tk.Trim(tk.GetInputBufferedScan())
			} else {
				fcT = tmps
			}

			// fcT = fcT[8:]
		}
	}

	if decryptRunCodeT != "" {
		fcT = tk.DecryptStringByTXDEF(fcT, decryptRunCodeT)
	}

	if ifViewT {
		if !ifInExeT {
			tk.Pl("%v", fcT)
		}

		return nil
	}

	if ifCompileT {
		appPathT, errT := os.Executable()
		tk.CheckError(errT)

		outputT := tk.Trim(tk.GetSwitch(argsT, "-output=", "output.exe"))

		if fcT == "" {
			tk.Fatalf("code empty")
		}

		buf1, errT := tk.LoadBytesFromFileE(appPathT)
		if errT != nil {
			tk.Fatalf("loading bin failed: %v", errT)
		}

		if rrStrT != "" {
			fcT = rrStrT
		}

		encTextT := tk.EncryptStringByTXDEF(fcT, "topxeq")

		encBytesT := []byte(encTextT)

		lenEncT := len(encBytesT)

		text1T := tk.Trim("740404")
		text2T := tk.Trim("690415")
		text3T := tk.Trim("040626")

		// tk.SaveBytesToFile(buf1, `d:\tmpx\tmpbytes.exe`)

		// tk.SaveBytesToFile([]byte(codeG), `d:\tmpx\tmpbytes2.txt`)

		re := regexp.MustCompile(text1T + text2T + text3T + `(.*)` + text3T + text2T + text1T)
		matchT := re.FindSubmatchIndex(buf1)
		if matchT == nil {
			tk.Fatalf("invalid bin: %v", appPathT)
		}

		bufCodeLenT := matchT[3] - matchT[2]

		var buf3 bytes.Buffer

		if bufCodeLenT < lenEncT {

			buf3.Write(buf1)
			buf3.Write([]byte("74040469" + "0415840215"))
			buf3.Write(encBytesT)
			buf3.Write([]byte("840215690" + "415740404"))
		} else {
			buf3.Write(buf1[:matchT[2]])
			buf3.Write(encBytesT)
			buf3.Write(buf1[matchT[2]+lenEncT:])
		}

		errT = tk.SaveBytesToFileE(buf3.Bytes(), outputT)
		tk.CheckError(errT)

		return nil

	}

	if ifOpenT {
		tk.RunWinFileWithSystemDefault(scriptPathT)

		return nil
	}

	// if ifCompileT {
	// 	initQLVM()

	// 	gox.QlVMG.SetVar("argsG", argsT)

	// 	retG = gox.NotFoundG

	// 	endT, errT := gox.QlVMG.SafeCl([]byte(fcT), "")
	// 	if errT != nil {

	// 		// tk.Pl()

	// 		// f, l := gox.QlVMG.Code.Line(gox.QlVMG.Code.Reserve().Next())
	// 		// tk.Pl("Next line: %v, %v", f, l)

	// 		return tk.Errf("failed to compile script(%v) error: %v\n", scriptT, errT)
	// 	}

	// 	tk.Pl("endT: %v", endT)

	// 	errT = gox.QlVMG.DumpEngine()

	// 	if errT != nil {
	// 		return tk.Errf("failed to dump engine: %v\n", errT)
	// 	}

	// 	tk.Plvsr(gox.QlVMG.Cpl.GetCode().Len(), gox.QlVMG.Run())

	// 	return nil
	// }

	if !ifBatchT {
		if tk.RegStartsWith(fcT, `//\s*(CHARB|charb)`) {
			ifBatchT = true
		}
	}

	// if ifBatchT {
	// 	listT := tk.SplitLinesRemoveEmpty(fcT)

	// 	// tk.Plv(fcT)
	// 	// tk.Plv(listT)

	// 	for _, v := range listT {
	// 		// tk.Pl("Run line: %#v", v)
	// 		v = tk.Trim(v)

	// 		if tk.StartsWith(v, "//") {
	// 			continue
	// 		}

	// 		rsT := runLine(v)

	// 		if rsT != nil {
	// 			valueT, ok := rsT.(error)

	// 			if ok {
	// 				return valueT
	// 			} else {
	// 				tk.Pl("%v", rsT)
	// 			}
	// 		}

	// 	}

	// 	return nil
	// }

	// if ifXieT {
	// 	var guiHandlerG tk.TXDelegate = guiHandler

	// 	rs := xie.RunCode(fcT, nil, map[string]interface{}{"guiG": guiHandlerG, "scriptPathG": gox.ScriptPathG, "basePathG": basePathG}, argsT...) // "guiG": guiHandlerG,
	// 	if !tk.IsUndefined(rs) {
	// 		tk.Pl("%v", rs)
	// 	}

	// 	return nil
	// }

	// gox.InitQLVM()

	// var guiHandlerG tk.TXDelegate = guiHandler

	// gox.QlVMG.SetVar("argsG", argsT)
	// gox.QlVMG.SetVar("guiG", guiHandlerG)

	// gox.RetG = gox.NotFoundG

	// errT = gox.QlVMG.SafeEval(fcT)
	moduleMap := charlang.NewModuleMap()
	moduleMap.AddBuiltinModule("ex", charex.Module)
	// moduleMap.AddBuiltinModule("fmt", charfmt.Module)

	charlang.MainCompilerOptions = &charlang.CompilerOptions{
		// ModulePath:        "", //"(repl)",
		ModuleMap: moduleMap,
		// SymbolTable:       charlang.NewSymbolTable(),
		// OptimizerMaxCycle: charlang.TraceCompilerOptions.OptimizerMaxCycle,
		// TraceParser:       true,
		// TraceOptimizer:    true,
		// TraceCompiler:     true,
		// OptimizeConst:     !noOptimizer,
		// OptimizeExpr:      !noOptimizer,

		// Trace:             os.Stdout,
		// TraceParser:       true,
		// TraceCompiler:     true,
		// TraceOptimizer:    true,
		// OptimizerMaxCycle: 1<<8 - 1,
		// OptimizeConst:     false,
		// OptimizeExpr:      false,
	}

	if charlang.DebugModeG {
		// opts = &charlang.CompilerOptions{
		// 	// ModulePath:        "", //"(repl)",
		// 	ModuleMap: moduleMap,
		// 	// SymbolTable:       charlang.NewSymbolTable(),
		// 	// OptimizerMaxCycle: charlang.TraceCompilerOptions.OptimizerMaxCycle,
		// 	// TraceParser:       true,
		// 	// TraceOptimizer:    true,
		// 	// TraceCompiler:     true,
		// 	// OptimizeConst:     !noOptimizer,
		// 	// OptimizeExpr:      !noOptimizer,

		// 	// Trace: os.Stdout,
		// 	// TraceParser: true,
		// 	// TraceCompiler: true,
		// 	// TraceOptimizer:    true,
		// 	// OptimizerMaxCycle: 1<<8 - 1,
		// 	// OptimizeConst:     false,
		// 	// OptimizeExpr:      false,
		// }

	}

	// tk.Pln(2.1)
	bytecodeT, errT := charlang.Compile([]byte(fcT), charlang.MainCompilerOptions) // charlang.DefaultCompilerOptions)
	if errT != nil {
		return errT
	}
	// tk.Pln(2.2)

	// inParasT := make(charlang.Map, len(paraMapT))
	// for k, v := range paraMapT {
	// 	inParasT[k] = charlang.ToString(v)
	// }

	// inParasT := make(charlang.Map, 0)

	envT := charlang.Map{}

	// envT["tk"] = charlang.TkFunction
	envT["argsG"] = charlang.ConvertToObject(argsT)
	envT["versionG"] = charlang.ToStringObject(charlang.VersionG)
	envT["scriptPathG"] = charlang.ToStringObject(scriptPathT)
	envT["runModeG"] = charlang.ToStringObject("script")
	envT["guiG"] = charlang.NewExternalDelegate(guiHandler)

	vmT := charlang.NewVM(bytecodeT)

	CurrentVM = vmT

	retT, errT := vmT.Run(envT) // inParasT,

	if errT != nil {

		// tk.Pl()

		// f, l := QlVMG.Code.Line(QlVMG.Code.Reserve().Next())
		// tk.Pl("Next line: %v, %v", f, l)

		return tk.Errf("failed to execute script(%v) error: %v\n", scriptT, errT)
	}

	return retT
}

func test(vA int) {
	if vA == 10000 {
		dummyG = fmt.Sprintf("codeG: %v\n", tk.LimitString(codeG, 50))
	}
}

func main() {
	// var errT error

	defer func() {
		err := recover()
		if err != nil {
			fmt.Println("Exception: ", err)
			fmt.Printf("runtime error: %v\n%v\n", err, string(debug.Stack()))
		}
	}()

	test(12)

	// errT := bluetoothAdapter.Enable()

	// if errT != nil {
	// 	tk.Pl("enable Bluetooth function failed: %v", errT)
	// 	// exit()
	// }

	rand.Seed(time.Now().Unix())

	// service route

	argsT := os.Args

	if tk.IfSwitchExistsWhole(argsT, "-help") {
		textT := tk.GetWeb(`http://topget.org/dc/t/charlang/intro.md`)

		tk.Pl("%v", tk.ToStr(textT))
		return
	}

	if tk.IfSwitchExistsWhole(argsT, "-helpFuncs") {
		textT := tk.GetWeb(`http://topget.org/dc/t/charlang/funcs.md`)

		tk.Pl("%v", tk.ToStr(textT))
		return
	}

	if tk.IfSwitchExistsWhole(argsT, "-service") {
		tk.Pl("%v V%v is running in service(server) mode. Running the application with argument \"-service\" will cause it running in service mode.\n", serviceNameG, charlang.VersionG)
		serviceModeG = true

		s := initSvc()

		if s == nil {
			tk.LogWithTimeCompact("Failed to init service")
			return
		}

		err := (*s).Run()
		if err != nil {
			tk.LogWithTimeCompact("Service \"%s\" failed to run.", (*s).String())
		}

		return
	}

	if tk.IfSwitchExistsWhole(argsT, "-installService") {
		s := initSvc()

		if s == nil {
			tk.Pl("failed to initialize service")
			return
		}

		tk.Pl("installing service \"%v\"...", (*s).String())

		errT := (*s).Install()
		if errT != nil {
			tk.Pl("failed to install service: %v", errT)
			return
		}

		tk.Pl("service installed - \"%s\" .", (*s).String())

		// tk.Pl("启动服务（starting service） \"%v\"...", (*s).String())

		// errT = (*s).Start()
		// if errT != nil {
		// 	tk.Pl("启动服务失败（failed to start）: %v", errT)
		// 	return
		// }

		// tk.Pl("服务已启动（service started） - \"%s\" .", (*s).String())

		return

	}

	if tk.IfSwitchExistsWhole(argsT, "-startService") {
		s := initSvc()

		if s == nil {
			tk.Pl("failed to init service")
			return
		}

		tk.Pl("starting service \"%v\"...", (*s).String())

		errT := (*s).Start()
		if errT != nil {
			tk.Pl("failed to start: %v", errT)
			return
		}

		tk.Pl("service started - \"%s\" ", (*s).String())

		return

	}

	if tk.IfSwitchExistsWhole(argsT, "-stopService") {
		s := initSvc()

		if s == nil {
			tk.Pl("failed to init service")
			return
		}

		errT := (*s).Stop()
		if errT != nil {
			tk.Pl("failed to stop service: %s", errT)
		} else {
			tk.Pl("service stopped - \"%s\" ", (*s).String())
		}

		return

	}

	if tk.IfSwitchExistsWhole(argsT, "-removeService") || tk.IfSwitchExistsWhole(argsT, "-uninstallService") {
		s := initSvc()

		if s == nil {
			tk.Pl("failed to init service")
			return
		}

		errT := (*s).Stop()
		if errT != nil {
			tk.Pl("failed to stop service: %s", errT)
		} else {
			tk.Pl("service stopped - \"%s\" ", (*s).String())
		}

		errT = (*s).Uninstall()
		if errT != nil {
			tk.Pl("failed to remove service: %v", errT)
			return
		}

		tk.Pl("service removed - \"%s\" ", (*s).String())

		return

	}

	if tk.IfSwitchExistsWhole(argsT, "-reinstallService") {
		s := initSvc()

		if s == nil {
			tk.Pl("failed to init service")
			return
		}

		errT := (*s).Stop()
		if errT != nil {
			tk.Pl("failed to stop service: %s", errT)
		} else {
			tk.Pl("service stopped - \"%s\" ", (*s).String())
		}

		errT = (*s).Uninstall()
		if errT != nil {
			tk.Pl("failed to remove service: %v", errT)
		} else {
			tk.Pl("service removed - \"%s\" ", (*s).String())
		}

		tk.Pl("installing service \"%v\"...", (*s).String())

		errT = (*s).Install()
		if errT != nil {
			tk.Pl("failed to install service: %v", errT)
			return
		}

		tk.Pl("service installed - \"%s\" .", (*s).String())

		tk.Pl("starting service \"%v\"...", (*s).String())

		errT = (*s).Start()
		if errT != nil {
			tk.Pl("failed to start: %v", errT)
			return
		}

		tk.Pl("service started - \"%s\" ", (*s).String())

		return

	}

	if tk.IfSwitchExistsWhole(argsT, "-restartService") {
		s := initSvc()

		if s == nil {
			tk.Pl("failed to init service")
			return
		}

		errT := (*s).Stop()
		if errT != nil {
			tk.Pl("failed to stop service: %s", errT)
		} else {
			tk.Pl("service stopped - \"%s\" ", (*s).String())
		}

		tk.Pl("starting service \"%v\"...", (*s).String())

		errT = (*s).Start()
		if errT != nil {
			tk.Pl("failed to start: %v", errT)
			return
		}

		tk.Pl("service started - \"%s\" ", (*s).String())

		return

	}

	// rs := runArgs(os.Args[1:]...)
	rs := runArgs(argsT...)

	if rs != nil {
		valueT, ok := rs.(error)

		if ok {
			tk.Pl("Error: %v", valueT)
			return
		}

		nv2, ok := rs.(charlang.Object)

		if ok {
			if nv2.TypeCode() == 0 {

			} else {
				tk.Pl("%v", nv2)
			}
			return
		}

		tk.Pl("(%T)%v", rs, rs)

	}

}

// Workaround for following issue.
// https://github.com/c-bata/go-prompt/issues/228
func handlePromptExit() {
	if runtime.GOOS != "linux" {
		return
	}

	if _, err := exec.LookPath("/bin/stty"); err != nil {
		return
	}

	rawModeOff := exec.Command("/bin/stty", "-raw", "echo")
	rawModeOff.Stdin = os.Stdin
	_ = rawModeOff.Run()
}
