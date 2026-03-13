// Package opcodes defines the bytecode instructions for the Charlang VM.
//
// # Opcode Overview
//
// Opcodes are single-byte instructions that the VM executes sequentially.
// Each opcode may have zero or more operands that provide additional data.
//
// # Instruction Format
//
// Instructions are encoded as: [opcode][operand1][operand2]...
// Operands are either 1 byte (0-255) or 2 bytes (0-65535, big-endian).
//
// # Opcode Categories
//
//   - Constants: OpConstant, OpNull, OpTrue, OpFalse
//   - Variables: OpGetGlobal, OpSetGlobal, OpGetLocal, OpSetLocal, OpGetFree, OpSetFree
//   - Calls: OpCall, OpCallName, OpGetBuiltin, OpReturn
//   - Operations: OpBinaryOp, OpUnary, OpEqual, OpNotEqual
//   - Control Flow: OpJump, OpJumpFalsy, OpAndJump, OpOrJump
//   - Data Structures: OpMap, OpArray, OpSliceIndex, OpGetIndex, OpSetIndex
//   - Closures: OpClosure, OpGetLocalPtr, OpGetFreePtr
//   - Iteration: OpIterInit, OpIterNext, OpIterKey, OpIterValue
//   - Modules: OpLoadModule, OpStoreModule
//   - Exceptions: OpSetupTry, OpSetupCatch, OpSetupFinally, OpThrow, OpFinalizer
//   - Stack: OpPop, OpDefineLocal
package charlang

// Opcode represents a single byte operation code for the VM.
type Opcode = byte

// List of opcodes. Each opcode is a single byte value.
// See OpcodeOperands for the operand specification of each opcode.
const (
	// OpNoOp is a no-operation instruction.
	OpNoOp Opcode = iota

	// OpConstant pushes a constant from the constant pool onto the stack.
	// Operands: [2] - constant index (0-65535)
	OpConstant

	// OpCall calls a function with the specified number of arguments.
	// Operands: [1, 1] - argument count, flags
	OpCall

	// OpGetGlobal retrieves a global variable by name and pushes it onto the stack.
	// Operands: [2] - constant index (variable name)
	OpGetGlobal

	// OpSetGlobal sets a global variable by name to the value on top of stack.
	// Operands: [2] - constant index (variable name)
	OpSetGlobal

	// OpGetLocal retrieves a local variable by index and pushes it onto the stack.
	// Operands: [1] - local variable index
	OpGetLocal

	// OpSetLocal sets a local variable by index to the value on top of stack.
	// Operands: [1] - local variable index
	OpSetLocal

	// OpGetBuiltin retrieves a built-in function by index and pushes it onto the stack.
	// Operands: [2] - builtin function index
	OpGetBuiltin

	// OpBinaryOp performs a binary operation on the top two stack values.
	// Operands: [1] - operator token
	OpBinaryOp

	// OpUnary performs a unary operation on the top stack value.
	// Operands: [1] - operator token
	OpUnary

	// OpEqual checks equality of the top two stack values and pushes the result.
	OpEqual

	// OpNotEqual checks inequality of the top two stack values and pushes the result.
	OpNotEqual

	// OpJump unconditionally jumps to the specified instruction position.
	// Operands: [2] - instruction position
	OpJump

	// OpJumpFalsy jumps to the specified position if the top of stack is falsy.
	// Operands: [2] - instruction position
	OpJumpFalsy

	// OpAndJump implements short-circuit AND: jumps if top of stack is falsy.
	// Operands: [2] - instruction position
	OpAndJump

	// OpOrJump implements short-circuit OR: jumps if top of stack is truthy.
	// Operands: [2] - instruction position
	OpOrJump

	// OpMap creates a new map with the specified number of key-value pairs.
	// Operands: [2] - number of pairs
	OpMap

	// OpArray creates a new array with the specified number of elements.
	// Operands: [2] - number of elements
	OpArray

	// OpSliceIndex performs a slice operation [start:end] on the top value.
	OpSliceIndex

	// OpGetIndex retrieves a value by index from the collection on the stack.
	// Operands: [1] - number of selectors (for chained access)
	OpGetIndex

	// OpSetIndex sets a value by index in the collection on the stack.
	OpSetIndex

	// OpNull pushes Undefined onto the stack.
	OpNull

	// OpPop removes and discards the top value from the stack.
	OpPop

	// OpGetFree retrieves a free variable (from enclosing scope) by index.
	// Operands: [1] - free variable index
	OpGetFree

	// OpSetFree sets a free variable by index to the value on top of stack.
	// Operands: [1] - free variable index
	OpSetFree

	// OpGetLocalPtr gets a pointer to a local variable for reference semantics.
	// Operands: [1] - local variable index
	OpGetLocalPtr

	// OpGetFreePtr gets a pointer to a free variable for reference semantics.
	// Operands: [1] - free variable index
	OpGetFreePtr

	// OpClosure creates a closure from a compiled function with captured variables.
	// Operands: [2, 1] - constant index (function), number of free variables
	OpClosure

	// OpIterInit initializes an iterator for the collection on top of stack.
	OpIterInit

	// OpIterNext advances the iterator and pushes the next value (or null if done).
	OpIterNext

	// OpIterKey pushes the current key from the iterator onto the stack.
	OpIterKey

	// OpIterValue pushes the current value from the iterator onto the stack.
	OpIterValue

	// OpLoadModule loads a module by name and pushes it onto the stack.
	// Operands: [2, 2] - constant index (module name), module index
	OpLoadModule

	// OpStoreModule stores the top of stack as a module with the given name.
	// Operands: [2] - constant index (module name)
	OpStoreModule

	// OpSetupTry sets up a try block for exception handling.
	// Operands: [2, 2] - catch position, finally position
	OpSetupTry

	// OpSetupCatch sets up a catch block to handle exceptions.
	OpSetupCatch

	// OpSetupFinally sets up a finally block for cleanup code.
	OpSetupFinally

	// OpThrow throws an exception (re-throws if operand is 0).
	// Operands: [1] - 0 for re-throw, 1 for throw expression
	OpThrow

	// OpFinalizer handles cleanup after exception handling.
	// Operands: [1] - error handler index
	OpFinalizer

	// OpReturn returns from the current function with the top of stack.
	// Operands: [1] - number of return values (0 or 1)
	OpReturn

	// OpDefineLocal defines a new local variable at the specified index.
	// Operands: [1] - local variable index
	OpDefineLocal

	// OpTrue pushes the boolean value true onto the stack.
	OpTrue

	// OpFalse pushes the boolean value false onto the stack.
	OpFalse

	// OpCallName calls a method by name on the object on top of stack.
	// Operands: [1, 1] - argument count, flags
	OpCallName
)

// OpcodeNames maps each opcode to its human-readable string representation.
// Used for debugging and disassembly output.
var OpcodeNames = [...]string{
	OpNoOp:         "NOOP",
	OpConstant:     "CONSTANT",
	OpCall:         "CALL",
	OpGetGlobal:    "GETGLOBAL",
	OpSetGlobal:    "SETGLOBAL",
	OpGetLocal:     "GETLOCAL",
	OpSetLocal:     "SETLOCAL",
	OpGetBuiltin:   "GETBUILTIN",
	OpBinaryOp:     "BINARYOP",
	OpUnary:        "UNARY",
	OpEqual:        "EQUAL",
	OpNotEqual:     "NOTEQUAL",
	OpJump:         "JUMP",
	OpJumpFalsy:    "JUMPFALSY",
	OpAndJump:      "ANDJUMP",
	OpOrJump:       "ORJUMP",
	OpMap:          "MAP",
	OpArray:        "ARRAY",
	OpSliceIndex:   "SLICEINDEX",
	OpGetIndex:     "GETINDEX",
	OpSetIndex:     "SETINDEX",
	OpNull:         "NULL",
	OpPop:          "POP",
	OpGetFree:      "GETFREE",
	OpSetFree:      "SETFREE",
	OpGetLocalPtr:  "GETLOCALPTR",
	OpGetFreePtr:   "GETFREEPTR",
	OpClosure:      "CLOSURE",
	OpIterInit:     "ITERINIT",
	OpIterNext:     "ITERNEXT",
	OpIterKey:      "ITERKEY",
	OpIterValue:    "ITERVALUE",
	OpLoadModule:   "LOADMODULE",
	OpStoreModule:  "STOREMODULE",
	OpReturn:       "RETURN",
	OpSetupTry:     "SETUPTRY",
	OpSetupCatch:   "SETUPCATCH",
	OpSetupFinally: "SETUPFINALLY",
	OpThrow:        "THROW",
	OpFinalizer:    "FINALIZER",
	OpDefineLocal:  "DEFINELOCAL",
	OpTrue:         "TRUE",
	OpFalse:        "FALSE",
	OpCallName:     "CALLNAME",
}

// OpcodeOperands defines the operand specification for each opcode.
// Each entry is a slice of operand widths in bytes:
//   - {} means no operands
//   - {1} means one 1-byte operand (0-255)
//   - {2} means one 2-byte operand (0-65535, big-endian)
//   - {1, 1} means two 1-byte operands
//   - {2, 1} means one 2-byte and one 1-byte operand
var OpcodeOperands = [...][]int{
	OpNoOp:         {},
	OpConstant:     {2},    // constant index
	OpCall:         {1, 1}, // number of arguments, flags
	OpGetGlobal:    {2},    // constant index
	OpSetGlobal:    {2},    // constant index
	OpGetLocal:     {1},    // local variable index
	OpSetLocal:     {1},    // local variable index
	OpGetBuiltin:   {2},    // builtin index
	OpBinaryOp:     {1},    // operator
	OpUnary:        {1},    // operator
	OpEqual:        {},
	OpNotEqual:     {},
	OpJump:         {2}, // position
	OpJumpFalsy:    {2}, // position
	OpAndJump:      {2}, // position
	OpOrJump:       {2}, // position
	OpMap:          {2}, // number of keys and values
	OpArray:        {2}, // number of items
	OpSliceIndex:   {},
	OpGetIndex:     {1}, // number of selectors
	OpSetIndex:     {},
	OpNull:         {},
	OpPop:          {},
	OpGetFree:      {1},    // index
	OpSetFree:      {1},    // index
	OpGetLocalPtr:  {1},    // index
	OpGetFreePtr:   {1},    // index
	OpClosure:      {2, 1}, // constant index, item count
	OpIterInit:     {},
	OpIterNext:     {},
	OpIterKey:      {},
	OpIterValue:    {},
	OpLoadModule:   {2, 2}, // constant index, module index
	OpStoreModule:  {2},    // module index
	OpReturn:       {1},    // number of items (0 or 1)
	OpSetupTry:     {2, 2},
	OpSetupCatch:   {},
	OpSetupFinally: {},
	OpThrow:        {1}, // 0:re-throw (system), 1:throw <expression>
	OpFinalizer:    {1}, // up to error handler index
	OpDefineLocal:  {1},
	OpTrue:         {},
	OpFalse:        {},
	OpCallName:     {1, 1}, // number of arguments, flags
}

// ReadOperands reads operands from bytecode instructions.
// It parses the operand bytes according to the specified widths and returns
// the operand values and the total bytes consumed.
//
// Parameters:
//   - numOperands: slice of operand widths (1 or 2 bytes each)
//   - ins: bytecode instruction slice starting after the opcode
//   - operands: pre-allocated slice to fill (reused to reduce allocations)
//
// Returns:
//   - operands: slice of parsed operand values
//   - int: total number of bytes consumed
//
// Example:
//
//	// For OpConstant with operands {2}:
//	operands, offset := ReadOperands([]int{2}, ins[1:], make([]int, 0, 1))
//	// operands[0] contains the constant index
func ReadOperands(numOperands []int, ins []byte, operands []int) ([]int, int) {
	operands = operands[:0]
	var offset int
	for _, width := range numOperands {
		switch width {
		case 1:
			operands = append(operands, int(ins[offset]))
		case 2:
			operands = append(operands, int(ins[offset+1])|int(ins[offset])<<8)
		}
		offset += width
	}
	return operands, offset
}
