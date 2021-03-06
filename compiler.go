// Copyright (c) 2020 Ozan Hacıbekiroğlu.
// Use of this source code is governed by a MIT License
// that can be found in the LICENSE file.

package charlang

import (
	"context"
	"fmt"
	"io"
	"os"
	"reflect"
	"time"

	"github.com/topxeq/charlang/parser"
	"github.com/topxeq/charlang/token"
)

// CompilerOptions represents customizable options for Compile().
type CompilerOptions struct {
	ModuleMap         *ModuleMap
	ModulePath        string
	ModuleIndexes     *ModuleIndexes
	Constants         []Object
	SymbolTable       *SymbolTable
	Trace             io.Writer
	TraceParser       bool
	TraceCompiler     bool
	TraceOptimizer    bool
	OptimizerMaxCycle int
	OptimizeConst     bool
	OptimizeExpr      bool
	constsCache       map[Object]int
}

var (
	// DefaultCompilerOptions holds default Compiler options.
	DefaultCompilerOptions = CompilerOptions{
		OptimizerMaxCycle: 100,
		OptimizeConst:     true,
		OptimizeExpr:      true,
	}
	// TraceCompilerOptions holds Compiler options to print trace output
	// to stdout for Parser, Optimizer, Compiler.
	TraceCompilerOptions = CompilerOptions{
		Trace:             os.Stdout,
		TraceParser:       true,
		TraceCompiler:     true,
		TraceOptimizer:    true,
		OptimizerMaxCycle: 1<<8 - 1,
		OptimizeConst:     true,
		OptimizeExpr:      true,
	}
)

// loopStmts represents a loopStmts construct that the compiler uses to track the current loopStmts.
type loopStmts struct {
	Continues         []int
	Breaks            []int
	lastTryCatchIndex int
}

// CompilerError represents a compiler error.
type CompilerError struct {
	FileSet *parser.SourceFileSet
	Node    parser.Node
	Err     error
}

func (e *CompilerError) Error() string {
	filePos := e.FileSet.Position(e.Node.Pos())
	return fmt.Sprintf("Compile Error: %s\n\tat %s", e.Err.Error(), filePos)
}

func (e *CompilerError) Unwrap() error {
	return e.Err
}

// ModuleIndex represents indexes of a single module.
type ModuleIndex struct {
	ConstantIndex int
	ModuleIndex   int
}

// ModuleIndexes represents modules indexes and total count that are defined while compiling.
type ModuleIndexes struct {
	Count   int
	Indexes map[string]ModuleIndex
}

// NewModuleIndexes returns a new ModuleIndexes object.
func NewModuleIndexes() *ModuleIndexes {
	return &ModuleIndexes{
		Indexes: make(map[string]ModuleIndex),
	}
}

// Reset resets ModuleIndexes to initial state to re-use.
func (mi *ModuleIndexes) Reset() *ModuleIndexes {
	mi.Count = 0
	for k := range mi.Indexes {
		delete(mi.Indexes, k)
	}
	return mi
}

// Compiler compiles the AST into a bytecode.
type Compiler struct {
	parent        *Compiler
	file          *parser.SourceFile
	constants     []Object
	constsCache   map[Object]int
	symbolTable   *SymbolTable
	instructions  []byte
	sourceMap     map[int]int
	moduleMap     *ModuleMap
	moduleIndexes *ModuleIndexes
	modulePath    string
	variadic      bool
	loops         []*loopStmts
	loopIndex     int
	tryCatchIndex int
	opts          CompilerOptions
	trace         io.Writer
	indent        int
}

// NewCompiler creates a new Compiler object.
func NewCompiler(file *parser.SourceFile, opts CompilerOptions) *Compiler {
	if opts.SymbolTable == nil {
		opts.SymbolTable = NewSymbolTable()
	}

	if opts.constsCache == nil {
		opts.constsCache = make(map[Object]int)
		for i := range opts.Constants {
			switch opts.Constants[i].(type) {
			case Int, Uint, Byte, Bool, Float, Char, undefined,
				*CompiledFunction:
				opts.constsCache[opts.Constants[i]] = i
			}
		}
	}

	if opts.ModuleIndexes == nil {
		opts.ModuleIndexes = NewModuleIndexes()
	}

	var trace io.Writer
	if opts.TraceCompiler {
		trace = opts.Trace
	}

	return &Compiler{
		file:          file,
		constants:     opts.Constants,
		constsCache:   opts.constsCache,
		symbolTable:   opts.SymbolTable,
		sourceMap:     make(map[int]int),
		moduleMap:     opts.ModuleMap,
		moduleIndexes: opts.ModuleIndexes,
		modulePath:    opts.ModulePath,
		loopIndex:     -1,
		tryCatchIndex: -1,
		opts:          opts,
		trace:         trace,
	}
}

// Compile compiles given script to Bytecode.
func Compile(script []byte, opts CompilerOptions) (*Bytecode, error) {
	fileSet := parser.NewFileSet()
	moduleName := opts.ModulePath

	if moduleName == "" {
		moduleName = "(main)"
	}

	srcFile := fileSet.AddFile(moduleName, -1, len(script))
	var trace io.Writer
	if opts.TraceParser {
		trace = opts.Trace
	}

	p := parser.NewParser(srcFile, script, trace)
	pf, err := p.ParseFile()
	if err != nil {
		return nil, err
	}

	compiler := NewCompiler(srcFile, opts)
	if opts.OptimizeConst || opts.OptimizeExpr {
		optim, err := compiler.optimize(pf)
		if err != nil {
			return nil, err
		}
		if optim != nil && opts.TraceCompiler && !opts.TraceOptimizer {
			_, _ = fmt.Fprintf(opts.Trace,
				"<Optimization Took: %s>\n", optim.Duration())
		}

	}

	if err := compiler.Compile(pf); err != nil {
		return nil, err
	}

	bc := compiler.Bytecode()
	if bc.Main.NumLocals > 256 {
		return nil, ErrSymbolLimit
	}
	return bc, nil
}

// optimize runs the Optimizer and returns Optimizer object and error from Optimizer.
// Note:If optimizer cannot run for some reason, all returned values will be nil.
func (c *Compiler) optimize(file *parser.File) (*SimpleOptimizer, error) {
	if c.opts.OptimizerMaxCycle < 1 {
		return nil, nil
	}

	ctx, cancel := context.WithTimeout(context.Background(), time.Second)
	defer cancel()

	o := NewOptimizer(
		ctx,
		file,
		c.symbolTable,
		c.opts,
	)

	if err := o.Optimize(); err != nil {
		return o, err
	}

	c.opts.OptimizerMaxCycle -= o.Total()
	return o, nil
}

// Bytecode returns compiled Bytecode ready to run in VM.
func (c *Compiler) Bytecode() *Bytecode {
	var lastOp Opcode
	var operands = make([]int, 0, 4)
	var jumpPos = make(map[int]struct{})
	var offset int
	var i int

	for i < len(c.instructions) {
		lastOp = c.instructions[i]
		numOperands := OpcodeOperands[lastOp]
		operands, offset = ReadOperands(
			numOperands,
			c.instructions[i+1:],
			operands,
		)

		if lastOp == OpJump || lastOp == OpJumpFalsy ||
			lastOp == OpAndJump || lastOp == OpOrJump {
			jumpPos[operands[0]] = struct{}{}
		}

		delete(jumpPos, i)
		i += offset + 1
	}

	if lastOp != OpReturn || len(jumpPos) > 0 {
		c.emit(nil, OpReturn, 0)
	}

	return &Bytecode{
		FileSet:   c.file.Set(),
		Constants: c.constants,
		Main: &CompiledFunction{
			NumParams:    c.symbolTable.NumParams(),
			NumLocals:    c.symbolTable.MaxSymbols(),
			Variadic:     c.variadic,
			Instructions: c.instructions,
			SourceMap:    c.sourceMap,
		},
		NumModules: c.moduleIndexes.Count,
	}
}

// Compile compiles parser.Node and builds Bytecode.
func (c *Compiler) Compile(node parser.Node) error {
	if c.trace != nil {
		if node != nil {
			defer untracec(tracec(c, fmt.Sprintf("%s (%s)",
				node.String(), reflect.TypeOf(node).Elem().Name())))
		} else {
			defer untracec(tracec(c, "<nil>"))
		}
	}

	switch node := node.(type) {
	case *parser.File:
		for _, stmt := range node.Stmts {
			if err := c.Compile(stmt); err != nil {
				return err
			}
		}
	case *parser.ExprStmt:
		if err := c.Compile(node.Expr); err != nil {
			return err
		}
		c.emit(node, OpPop)
	case *parser.IncDecStmt:
		op := token.AddAssign
		if node.Token == token.Dec {
			op = token.SubAssign
		}
		return c.compileAssignStmt(
			node,
			[]parser.Expr{node.Expr},
			[]parser.Expr{&parser.IntLit{Value: 1, ValuePos: node.TokenPos}},
			token.Var,
			op,
		)
	case *parser.ParenExpr:
		return c.Compile(node.Expr)
	case *parser.BinaryExpr:
		if node.Token == token.LAnd || node.Token == token.LOr {
			return c.compileLogical(node)
		}
		return c.compileBinaryExpr(node)
	case *parser.IntLit:
		c.emit(node, OpConstant, c.addConstant(Int(node.Value)))
	case *parser.UintLit:
		c.emit(node, OpConstant, c.addConstant(Uint(node.Value)))
	case *parser.ByteLit:
		c.emit(node, OpConstant, c.addConstant(Byte(node.Value)))
	case *parser.FloatLit:
		c.emit(node, OpConstant, c.addConstant(Float(node.Value)))
	case *parser.BoolLit:
		if node.Value {
			c.emit(node, OpTrue)
		} else {
			c.emit(node, OpFalse)
		}
	case *parser.StringLit:
		c.emit(node, OpConstant, c.addConstant(ToString(node.Value)))
	case *parser.CharLit:
		c.emit(node, OpConstant, c.addConstant(Char(node.Value)))
	case *parser.UndefinedLit:
		c.emit(node, OpNull)
	case *parser.UnaryExpr:
		return c.compileUnaryExpr(node)
	case *parser.IfStmt:
		return c.compileIfStmt(node)
	case *parser.TryStmt:
		return c.compileTryStmt(node)
	case *parser.CatchStmt:
		return c.compileCatchStmt(node)
	case *parser.FinallyStmt:
		return c.compileFinallyStmt(node)
	case *parser.ThrowStmt:
		return c.compileThrowStmt(node)
	case *parser.ForStmt:
		return c.compileForStmt(node)
	case *parser.ForInStmt:
		return c.compileForInStmt(node)
	case *parser.BranchStmt:
		return c.compileBranchStmt(node)
	case *parser.BlockStmt:
		return c.compileBlockStmt(node)
	case *parser.DeclStmt:
		return c.compileDeclStmt(node)
	case *parser.AssignStmt:
		return c.compileAssignStmt(node,
			node.LHS, node.RHS, token.Var, node.Token)
	case *parser.Ident:
		return c.compileIdent(node)
	case *parser.ArrayLit:
		return c.compileArrayLit(node)
	case *parser.MapLit:
		return c.compileMapLit(node)
	case *parser.SelectorExpr: // selector on RHS side
		return c.compileSelectorExpr(node)
	case *parser.IndexExpr:
		return c.compileIndexExpr(node)
	case *parser.SliceExpr:
		return c.compileSliceExpr(node)
	case *parser.FuncLit:
		return c.compileFuncLit(node)
	case *parser.ReturnStmt:
		return c.compileReturnStmt(node)
	case *parser.CallExpr:
		return c.compileCallExpr(node)
	case *parser.ImportExpr:
		return c.compileImportExpr(node)
	case *parser.CondExpr:
		return c.compileCondExpr(node)
	case *parser.EmptyStmt:
	case nil:
	default:
		return c.errorf(node, `%[1]T "%[1]v" not implemented`, node)
	}
	return nil
}

func (c *Compiler) changeOperand(opPos int, operand ...int) {
	op := c.instructions[opPos]
	inst := make([]byte, 8)
	inst, err := MakeInstruction(inst, op, operand...)
	if err != nil {
		panic(err)
	}
	c.replaceInstruction(opPos, inst)
}

func (c *Compiler) replaceInstruction(pos int, inst []byte) {
	copy(c.instructions[pos:], inst)
	if c.trace != nil {
		printTrace(c.indent, c.trace, fmt.Sprintf("REPLC %s",
			FormatInstructions(c.instructions[pos:], pos)[0]))
	}
}

func (c *Compiler) addConstant(obj Object) (index int) {
	defer func() {
		if c.trace != nil {
			printTrace(c.indent, c.trace,
				fmt.Sprintf("CONST %04d %s", index, obj))
		}
	}()

	switch obj.(type) {
	case Int, Uint, Byte, Bool, Float, Char, undefined:
		i, ok := c.constsCache[obj]
		if ok {
			index = i
			return
		}
	case String:
		c.constants = append(c.constants, obj)
		index = len(c.constants) - 1
		return
	case *CompiledFunction:
		for i, v := range c.constants {
			if f, ok := v.(*CompiledFunction); ok {
				if reflect.DeepEqual(f, obj) {
					index = i
					return
				}
			}
		}
	default:
		// unhashable types cannot be stored in constsCache, append them to constants slice
		// and return index
		c.constants = append(c.constants, obj)
		index = len(c.constants) - 1
		return
	}

	c.constants = append(c.constants, obj)
	index = len(c.constants) - 1
	c.constsCache[obj] = index
	return
}

func (c *Compiler) emit(node parser.Node, opcode Opcode, operands ...int) int {
	filePos := parser.NoPos
	if node != nil {
		filePos = node.Pos()
	}

	inst := make([]byte, 8)
	inst, err := MakeInstruction(inst, opcode, operands...)
	if err != nil {
		panic(err)
	}

	pos := c.addInstruction(inst)
	c.sourceMap[pos] = int(filePos)

	if c.trace != nil {
		printTrace(c.indent, c.trace, fmt.Sprintf("EMIT  %s",
			FormatInstructions(c.instructions[pos:], pos)[0]))
	}
	return pos
}

func (c *Compiler) addInstruction(b []byte) int {
	posNewIns := len(c.instructions)
	c.instructions = append(c.instructions, b...)
	return posNewIns
}

func (c *Compiler) checkCyclicImports(node parser.Node, modulePath string) error {
	if c.modulePath == modulePath {
		return c.errorf(node, "cyclic module import: %s", modulePath)
	} else if c.parent != nil {
		return c.parent.checkCyclicImports(node, modulePath)
	}
	return nil
}

func (c *Compiler) addModule(name string, constantIndex int) ModuleIndex {
	index := c.moduleIndexes.Count
	c.moduleIndexes.Count++
	c.moduleIndexes.Indexes[name] = ModuleIndex{
		ConstantIndex: constantIndex,
		ModuleIndex:   index,
	}
	return c.moduleIndexes.Indexes[name]
}

func (c *Compiler) getModule(name string) (ModuleIndex, bool) {
	indexes, ok := c.moduleIndexes.Indexes[name]
	return indexes, ok
}

func (c *Compiler) compileModule(
	node parser.Node,
	modulePath string,
	src []byte,
) (ModuleIndex, error) {
	var (
		modIndex ModuleIndex
		err      error
	)
	if err = c.checkCyclicImports(node, modulePath); err != nil {
		return modIndex, err
	}

	modIndex, exists := c.getModule(modulePath)
	if exists {
		return modIndex, nil
	}

	modFile := c.file.Set().AddFile(modulePath, -1, len(src))
	var trace io.Writer
	if c.opts.TraceParser {
		trace = c.trace
	}

	p := parser.NewParser(modFile, src, trace)
	var file *parser.File
	file, err = p.ParseFile()
	if err != nil {
		return modIndex, err
	}

	symbolTable := NewSymbolTable().
		DisableBuiltin(c.symbolTable.DisabledBuiltins()...)

	fork := c.fork(modFile, modulePath, symbolTable)
	_, err = fork.optimize(file)
	if err != nil {
		err = c.error(node, err)
		return modIndex, err
	}

	if err = fork.Compile(file); err != nil {
		return modIndex, err
	}

	bc := fork.Bytecode()
	if bc.Main.NumLocals > 256 {
		err = c.error(node, ErrSymbolLimit)
		return modIndex, err
	}

	c.constants = bc.Constants
	index := c.addConstant(bc.Main)
	return c.addModule(modulePath, index), nil
}

func (c *Compiler) enterLoop() *loopStmts {
	loop := &loopStmts{lastTryCatchIndex: c.tryCatchIndex}
	c.loops = append(c.loops, loop)
	c.loopIndex++

	if c.trace != nil {
		printTrace(c.indent, c.trace, "LOOPE", c.loopIndex)
	}
	return loop
}

func (c *Compiler) leaveLoop() {
	if c.trace != nil {
		printTrace(c.indent, c.trace, "LOOPL", c.loopIndex)
	}
	c.loops = c.loops[:len(c.loops)-1]
	c.loopIndex--
}

func (c *Compiler) currentLoop() *loopStmts {
	if c.loopIndex >= 0 {
		return c.loops[c.loopIndex]
	}
	return nil
}

func (c *Compiler) fork(
	file *parser.SourceFile,
	modulePath string,
	symbolTable *SymbolTable,
) *Compiler {
	child := NewCompiler(file, CompilerOptions{
		ModuleMap:         c.moduleMap,
		ModuleIndexes:     c.moduleIndexes,
		ModulePath:        modulePath,
		Constants:         c.constants,
		SymbolTable:       symbolTable,
		Trace:             c.trace,
		TraceParser:       c.opts.TraceParser,
		TraceCompiler:     c.opts.TraceCompiler,
		TraceOptimizer:    c.opts.TraceOptimizer,
		OptimizerMaxCycle: c.opts.OptimizerMaxCycle,
		OptimizeConst:     c.opts.OptimizeConst,
		OptimizeExpr:      c.opts.OptimizeExpr,
		constsCache:       c.constsCache,
	})

	child.parent = c
	if modulePath == c.modulePath {
		child.indent = c.indent
	}
	return child
}

func (c *Compiler) error(node parser.Node, err error) error {
	return &CompilerError{
		FileSet: c.file.Set(),
		Node:    node,
		Err:     err,
	}
}

func (c *Compiler) errorf(
	node parser.Node,
	format string,
	args ...interface{},
) error {
	return &CompilerError{
		FileSet: c.file.Set(),
		Node:    node,
		Err:     fmt.Errorf(format, args...),
	}
}

func printTrace(indent int, trace io.Writer, a ...interface{}) {
	const (
		dots = ". . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . "
		n    = len(dots)
	)

	i := 2 * indent
	for i > n {
		_, _ = fmt.Fprint(trace, dots)
		i -= n
	}

	_, _ = fmt.Fprint(trace, dots[0:i])
	_, _ = fmt.Fprintln(trace, a...)
}

func tracec(c *Compiler, msg string) *Compiler {
	printTrace(c.indent, c.trace, msg, "{")
	c.indent++
	return c
}

func untracec(c *Compiler) {
	c.indent--
	printTrace(c.indent, c.trace, "}")
}

// MakeInstruction returns a bytecode for an Opcode and the operands.
//
// Provide "buf" slice which is a returning value to reduce allocation or nil
// to create new byte slice. This is implemented to reduce compilation
// allocation that resulted in -15% allocation, +2% speed in compiler.
// It takes ~8ns/op with zero allocation.
//
// Returning error is required to identify bugs faster when VM and Opcodes are
// under heavy development.
//
// Warning: Unknown Opcode causes panic!
func MakeInstruction(buf []byte, op Opcode, args ...int) ([]byte, error) {
	operands := OpcodeOperands[op]
	if len(operands) != len(args) {
		return nil, fmt.Errorf(
			"MakeInstruction: %s expected %d operands, but got %d",
			OpcodeNames[op], len(operands), len(args),
		)
	}

	buf = append(buf[:0], op)
	switch op {
	case OpConstant, OpMap, OpArray, OpGetGlobal, OpSetGlobal, OpJump,
		OpJumpFalsy, OpAndJump, OpOrJump, OpStoreModule:
		buf = append(buf, byte(args[0]>>8))
		buf = append(buf, byte(args[0]))
		return buf, nil
	case OpLoadModule, OpSetupTry:
		buf = append(buf, byte(args[0]>>8))
		buf = append(buf, byte(args[0]))
		buf = append(buf, byte(args[1]>>8))
		buf = append(buf, byte(args[1]))
		return buf, nil
	case OpClosure:
		buf = append(buf, byte(args[0]>>8))
		buf = append(buf, byte(args[0]))
		buf = append(buf, byte(args[1]))
		return buf, nil
	case OpCall:
		buf = append(buf, byte(args[0]))
		buf = append(buf, byte(args[1]))
		return buf, nil
	case OpGetBuiltin, OpReturn, OpBinaryOp, OpUnary, OpGetIndex, OpGetLocal,
		OpSetLocal, OpGetFree, OpSetFree, OpGetLocalPtr, OpGetFreePtr, OpThrow,
		OpFinalizer, OpDefineLocal:
		buf = append(buf, byte(args[0]))
		return buf, nil
	case OpEqual, OpNotEqual, OpNull, OpTrue, OpFalse, OpPop, OpSliceIndex,
		OpSetIndex, OpIterInit, OpIterNext, OpIterKey, OpIterValue,
		OpSetupCatch, OpSetupFinally, OpNoOp:
		return buf, nil
	default:
		return nil, fmt.Errorf("MakeInstruction: unknown Opcode %d %s",
			op, OpcodeNames[op])
	}
}

// FormatInstructions returns string representation of bytecode instructions.
func FormatInstructions(b []byte, posOffset int) []string {
	var out []string
	var operands = make([]int, 0, 4)
	var offset int
	var i int

	for i < len(b) {
		numOperands := OpcodeOperands[b[i]]
		operands, offset = ReadOperands(numOperands, b[i+1:], operands)

		switch len(numOperands) {
		case 0:
			out = append(out, fmt.Sprintf("%04d %-7s",
				posOffset+i, OpcodeNames[b[i]]))
		case 1:
			out = append(out, fmt.Sprintf("%04d %-7s %-5d",
				posOffset+i, OpcodeNames[b[i]], operands[0]))
		case 2:
			out = append(out, fmt.Sprintf("%04d %-7s %-5d %-5d",
				posOffset+i, OpcodeNames[b[i]],
				operands[0], operands[1]))
		}
		i += 1 + offset
	}
	return out
}

// IterateInstructions iterate instructions and call given function for each instruction.
// Note: Do not use operands slice in callback, it is reused for less allocation.
func IterateInstructions(insts []byte,
	fn func(pos int, opcode Opcode, operands []int, offset int) bool) {
	operands := make([]int, 0, 4)
	var offset int

	for i := 0; i < len(insts); i++ {
		numOperands := OpcodeOperands[insts[i]]
		operands, offset = ReadOperands(numOperands, insts[i+1:], operands)
		if !fn(i, insts[i], operands, offset) {
			break
		}
		i += offset
	}
}
