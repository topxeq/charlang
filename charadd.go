package charlang

import (
	"fmt"
	"net/http"
	"os"
	"time"

	"github.com/topxeq/charlang/token"

	"github.com/topxeq/tk"

	_ "github.com/denisenkom/go-mssqldb"
	// _ "github.com/godror/godror"
	_ "github.com/sijms/go-ora/v2"

	// full version related end

	_ "github.com/go-sql-driver/mysql"
	_ "github.com/mattn/go-sqlite3"
	// ugofmt "github.com/topxeq/charlang/stdlib/fmt"
)

// global vars
var VersionG = "0.5a"

var CodeTextG = ""

var VerboseG = false

var ScriptPathG string

var ServerModeG = false

var TkFunction = &Function{
	Name: "tk",
	Value: func(args ...Object) (Object, error) {

		if len(args) < 1 {
			return Undefined, NewCommonError("not enough paramters")
		}

		if args[0].TypeName() != "string" {
			return Undefined, NewCommonError("invalid type for command")
		}

		cmdT := args[0].String()

		switch cmdT {
		case "test":
			fmt.Printf("args: %v\n", args[1:])
			return ConvertToObject("Response!"), nil

		case "getNowTime":
			return ConvertToObject(time.Now()), nil

		default:
			return Undefined, NewCommonError("unknown comman")
		}

		return Undefined, nil
	},
}

// objects

// Any represents container object and implements the Object interfaces.
type Any struct {
	ObjectImpl
	Value        interface{}
	OriginalType string
	OriginalCode int
}

var (
	_ Object = (*Any)(nil)
	_ Copier = (*Any)(nil)
)

func (o *Any) TypeCode() int {
	return 999
}

// TypeName implements Object interface.
func (*Any) TypeName() string {
	return "any"
}

// String implements Object interface.
func (o *Any) String() string {
	return tk.ToStr(o.Value)
}

// Copy implements Copier interface.
func (o *Any) Copy() Object {
	return &Any{
		Value:        o.Value,
		OriginalType: o.OriginalType,
		OriginalCode: o.OriginalCode,
	}
}

// Equal implements Object interface.
func (o *Any) Equal(right Object) bool {
	if v, ok := right.(*Any); ok {
		return v.Value == o.Value
	}
	return false
}

// IsFalsy implements Object interface.
func (o *Any) IsFalsy() bool { return false }

// IndexGet implements Object interface.
func (o *Any) IndexGet(index Object) (Object, error) {
	s := index.String()
	if s == "type" {
		return String(o.OriginalType), nil
	}

	if s == "code" {
		return Int(o.OriginalCode), nil
	}

	// if s == "New" {
	// 	return &Function{
	// 		Name: "New",
	// 		Value: func(args ...Object) (Object, error) {
	// 			switch len(args) {
	// 			case 1:
	// 				return o.NewError(args[0].String()), nil
	// 			case 0:
	// 				return o.NewError(o.Message), nil
	// 			default:
	// 				msgs := make([]string, len(args))
	// 				for i := range args {
	// 					msgs[i] = args[0].String()
	// 				}
	// 				return o.NewError(msgs...), nil
	// 			}
	// 		},
	// 	}, nil
	// }
	return Undefined, nil
}

// // NewError creates a new Error and sets original Error as its cause which can be unwrapped.
// func (o *Error) NewError(messages ...string) *Error {
// 	cp := o.Copy().(*Error)
// 	cp.Message = strings.Join(messages, " ")
// 	cp.Cause = o
// 	return cp
// }

// IndexSet implements Object interface.
func (*Any) IndexSet(index, value Object) error {
	return ErrNotIndexAssignable
}

// BinaryOp implements Object interface.
func (o *Any) BinaryOp(tok token.Token, right Object) (Object, error) {
	return nil, ErrInvalidOperator
}

// CanCall implements Object interface.
func (*Any) CanCall() bool { return false }

// Call implements Object interface.
func (*Any) Call(_ ...Object) (Object, error) {
	return nil, ErrNotCallable
}

// CanIterate implements Object interface.
func (*Any) CanIterate() bool { return false }

// Iterate implements Object interface.
func (*Any) Iterate() Iterator { return nil }

func NewAny(vA interface{}, argsA ...string) *Any {
	originalT := ""

	if len(argsA) > 0 {
		originalT = argsA[0]
	}

	originalCodeT := -1

	nv, ok := vA.(Object)

	if ok {
		originalCodeT = nv.TypeCode()
	}

	return &Any{
		Value:        vA,
		OriginalType: originalT,
		OriginalCode: originalCodeT,
	}
}

// common funcs

func QuickCompile(codeA string, compilerOptionsA ...*CompilerOptions) interface{} {
	var compilerOptionsT *CompilerOptions
	if len(compilerOptionsA) > 0 {
		compilerOptionsT = compilerOptionsA[0]
	} else {
		compilerOptionsT = &DefaultCompilerOptions
	}

	bytecodeT, errT := Compile([]byte(codeA), *compilerOptionsT)
	if errT != nil {
		return errT
	}

	return bytecodeT
}

func NewBaseEnv(varsA map[string]interface{}, additionsA ...Object) *Map {
	envT := Map{}

	envT["tk"] = TkFunction
	envT["argsG"] = Array{}
	envT["versionG"] = ToStringObject(VersionG)

	for k, v := range varsA {
		envT[k] = ConvertToObject(v)
	}

	for i, v := range additionsA {
		envT[tk.IntToStr(i+1)] = v
	}

	return &envT
}

func QuickRun(codeA interface{}, globalsA map[string]interface{}, additionsA ...Object) interface{} {
	var errT error
	nv, ok := codeA.(*Bytecode)

	if !ok {
		codeT, ok := codeA.(string)
		if !ok {
			return fmt.Errorf("invalid parameter")
		}

		nv, errT = Compile([]byte(codeT), DefaultCompilerOptions)
		if errT != nil {
			return errT
		}
	}

	envT := NewBaseEnv(globalsA) // Map{}

	// envT["tk"] = TkFunction
	// envT["argsG"] = Array{}
	// envT["versionG"] = ToString(VersionG)

	// for k, v := range globalsA {
	// 	envT[k] = ConvertToObject(v)
	// }

	// for i, v := range additionsA {
	// 	envT[tk.IntToStr(i+1)] = v
	// }

	retT, errT := NewVM(nv).Run(envT, additionsA...)

	if errT != nil {
		return errT
	}

	return ConvertFromObject(retT)
}

func NewEvalQuick(globalsA map[string]interface{}, optsA *CompilerOptions, localsA ...Object) *Eval {
	// moduleMap := NewModuleMap()
	// // moduleMap.AddBuiltinModule("time", ugotime.Module).
	// // 	AddBuiltinModule("strings", ugostrings.Module).
	// moduleMap.AddBuiltinModule("fmt", ugofmt.Module)

	var optsT CompilerOptions

	if optsA != nil {
		optsT = *optsA
	} else {
		optsT = CompilerOptions{
			ModulePath:        "",
			ModuleMap:         nil,
			SymbolTable:       NewSymbolTable(),
			OptimizerMaxCycle: TraceCompilerOptions.OptimizerMaxCycle,
			// TraceParser:       false,
			// TraceOptimizer:    false,
			// TraceCompiler:     false,
			// OptimizeConst:     !noOptimizer,
			// OptimizeExpr:      !noOptimizer,
		}
	}

	envT := NewBaseEnv(globalsA) // Map{}

	// if globals == nil {
	// 	globals = Map{}
	// }

	if optsT.SymbolTable == nil {
		optsT.SymbolTable = NewSymbolTable()
	}

	// if optsT.ModuleIndexes == nil {
	// 	optsT.ModuleIndexes = NewModuleIndexes()
	// }

	return &Eval{
		Locals:  localsA,
		Globals: *envT,
		Opts:    optsT,
		VM:      NewVM(nil).SetRecover(true),
	}
}

func RunScriptOnHttp(codeA string, compilerOptionsA *CompilerOptions, res http.ResponseWriter, req *http.Request, inputA string, argsA []string, parametersA map[string]string, optionsA ...string) (string, error) {
	if tk.IfSwitchExists(optionsA, "-verbose") {
		tk.Pl("Starting...")
	}

	if tk.StartsWith(codeA, "//TXDEF#") {
		tmps := tk.DecryptStringByTXDEF(codeA, "topxeq")

		if !tk.IsErrStr(tmps) {
			codeA = tmps
		}
	}

	if res != nil {
		res.Header().Set("Access-Control-Allow-Origin", "*")
		res.Header().Set("Access-Control-Allow-Headers", "*")
		res.Header().Set("Content-Type", "text/html; charset=utf-8")
	}

	if req != nil {
		req.ParseForm()
	}

	reqT := tk.GetFormValueWithDefaultValue(req, "charms", "")
	// if req.RequestURI != "/charms/ed01/addAccessLog" {
	// 	tk.Pl("RequestURI: %v", req.RequestURI)
	// }

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

	// tk.Pl("charms: %v", reqT)

	var paraMapT map[string]string
	var errT error

	retT := ""

	vo := tk.GetFormValueWithDefaultValue(req, "vo", "")

	if vo == "" {
		paraMapT = tk.FormToMap(req.Form)
	} else {
		paraMapT, errT = tk.MSSFromJSON(vo)

		if errT != nil {
			res.Write([]byte(tk.ErrStrf("%v", "invalid vo format")))
			// res.Write([]byte(genFailCompact("操作失败", "invalid vo format", "-compact")))
			return retT, nil
		}
	}

	// if !tk.ContainsIn(req.RequestURI, "/charms/ed01/addAccessLog", "/charms/ed01/getRuleColor.js", "/charms/ed01/getLangList.js", "/charms/ed01/getKnowListByBoard") {
	// 	tk.Pl("[%v] REQ: %#v (%#v)", tk.GetNowTimeStringFormal(), reqT, paraMapT)
	// }

	toWriteT := ""

	fileNameT := reqT

	if !tk.EndsWith(fileNameT, ".char") {
		fileNameT += ".char"
	}

	fcT := codeA

	var compilerOptionsT CompilerOptions

	if compilerOptionsA == nil {
		compilerOptionsT = DefaultCompilerOptions
	} else {
		compilerOptionsT = *compilerOptionsA
	}

	bytecodeT, errT := Compile([]byte(fcT), compilerOptionsT)
	if errT != nil {
		res.Write([]byte(tk.ErrStrf("%v", errT.Error())))
		// res.Write([]byte(genFailCompact("操作失败", errT.Error(), "-compact")))
		return retT, nil
	}

	inParasT := make(Map, len(paraMapT))
	for k, v := range paraMapT {
		inParasT[k] = ToStringObject(v)
	}

	envT := NewBaseEnv(nil) // Map{}

	// envT["tk"] = TkFunction
	(*envT)["argsG"] = ConvertToObject(argsA)

	(*envT)["requestG"] = ConvertToObject(req)
	(*envT)["responseG"] = ConvertToObject(res)
	(*envT)["reqNameG"] = ConvertToObject(reqT)
	(*envT)["inputG"] = ConvertToObject(inputA)
	(*envT)["basePathG"] = ConvertToObject(tk.GetSwitch(optionsA, "-base=", ""))
	(*envT)["paraMapG"] = ConvertToObject(parametersA)

	retObjectT, errT := NewVM(bytecodeT).Run(
		envT,
		inParasT,
	)

	if errT != nil {
		res.Write([]byte(tk.ErrStrf("%v", errT.Error())))
		return retObjectT.String(), nil
	}

	toWriteT = retObjectT.String()

	if toWriteT == "TX_END_RESPONSE_XT" {
		return retObjectT.String(), nil
	}

	res.Header().Set("Content-Type", "text/html; charset=utf-8")

	res.Write([]byte(toWriteT))

	return retObjectT.String(), nil
}

func ConvertToObject(vA interface{}) Object {
	if vA == nil {
		return Undefined
	}

	switch nv := vA.(type) {
	case error:
		return WrapError(nv)
	case string:
		return ToStringObject(nv)
	case bool:
		return Bool(nv)
	case int:
		return Int(nv)
	case int16:
		return Int(nv)
	case rune:
		return Char(nv)
	case int64:
		return Int(nv)
	case byte:
		return Byte(nv)
	case uint16:
		return Uint(nv)
	case uint32:
		return Uint(nv)
	case uint64:
		return Uint(nv)
	case float32:
		return Float(nv)
	case float64:
		return Float(nv)
	case []byte:
		return Bytes(nv)
	case []uint32:
		if nv == nil {
			return Undefined
		}

		rsT := make(Array, 0, len(nv))

		for _, v := range nv {
			rsT = append(rsT, Char(v))
		}

		return rsT
	case []int:
		if nv == nil {
			return Undefined
		}

		rsT := make(Array, 0, len(nv))

		for _, v := range nv {
			rsT = append(rsT, Int(v))
		}

		return rsT
	case []int64:
		if nv == nil {
			return Undefined
		}

		rsT := make(Array, 0, len(nv))

		for _, v := range nv {
			rsT = append(rsT, Int(v))
		}

		return rsT
	case []string:
		if nv == nil {
			return Undefined
		}

		rsT := make(Array, 0, len(nv))

		for _, v := range nv {
			rsT = append(rsT, ToStringObject(v))
		}

		return rsT
	case [][]string:
		if nv == nil {
			return Undefined
		}

		rsT := make(Array, 0, len(nv))

		for _, v := range nv {
			lineListT := make(Array, 0, len(v))
			for _, jv := range v {
				lineListT = append(lineListT, ToStringObject(jv))
			}

			rsT = append(rsT, lineListT)
		}

		return rsT
	case [][]int:
		if nv == nil {
			return Undefined
		}

		rsT := make(Array, 0, len(nv))

		for _, v := range nv {
			lineListT := make(Array, 0, len(v))
			for _, jv := range v {
				lineListT = append(lineListT, ToIntObject(jv))
			}

			rsT = append(rsT, lineListT)
		}

		return rsT
	case []Object:
		if nv == nil {
			return Undefined
		}

		rsT := make(Array, 0, len(nv))

		for _, v := range nv {
			rsT = append(rsT, v)
		}

		return rsT
	case []interface{}:
		if nv == nil {
			return Undefined
		}

		rsT := make(Array, 0, len(nv))

		for _, v := range nv {
			rsT = append(rsT, ConvertToObject(v))
		}

		return rsT
	case map[string]string:
		if nv == nil {
			return Undefined
		}

		rsT := make(Map, len(nv))

		for k, v := range nv {
			rsT[k] = ToStringObject(v)
		}

		return rsT
	case map[string]interface{}:
		if nv == nil {
			return Undefined
		}

		rsT := make(Map, len(nv))

		for k, v := range nv {
			rsT[k] = ConvertToObject(v)
		}

		return rsT
	case map[string]map[string]string:
		if nv == nil {
			return Undefined
		}

		rsT := make(Map, len(nv))

		for k, v := range nv {
			mapT := make(Map, len(nv))
			for jk, jv := range v {
				mapT[jk] = ToStringObject(jv)
			}

			rsT[k] = mapT
		}

		return rsT
	case []map[string]string:
		if nv == nil {
			return Undefined
		}

		rsT := make(Array, 0, len(nv))

		for _, v := range nv {
			rsT = append(rsT, ConvertToObject(v))
		}

		return rsT
	case []map[string]interface{}:
		if nv == nil {
			return Undefined
		}

		rsT := make(Array, 0, len(nv))

		for _, v := range nv {
			rsT = append(rsT, ConvertToObject(v))
		}

		return rsT
	// case time.Time:
	// 	return Any{Value: nv, OriginalType: "time.Time"}
	case Function:
		return &nv
	case String:
		return nv

	default:
		originalCodeT := -1
		nv1, ok := vA.(Object)

		if ok {
			originalCodeT = nv1.TypeCode()
		}

		return &Any{Value: nv, OriginalType: fmt.Sprintf("%T", nv), OriginalCode: originalCodeT}
		// tk.Pl("Unknown type: %T, %#v, %v", vA, vA, vA)
		// return Undefined
	}
}

func ConvertFromObject(vA Object) interface{} {
	// if vA.TypeName() == "int" {
	// 	return int(vA)
	// }
	switch nv := vA.(type) {
	case Bool:
		return bool(nv)
	case Byte:
		return byte(nv)
	case Char:
		return rune(nv)
	case Int:
		return int(nv)
	case Uint:
		return uint64(nv)
	case Float:
		return float64(nv)
	case String:
		return nv.Value
	case *String:
		return nv.Value
	case Bytes:
		return []byte(nv)
	case *Error:
		return nv.Unwrap()
	case *Any:
		return nv.Value
	case Map:
		if nv == nil {
			return nil
		}

		rsT := make(map[string]interface{}, len(nv))

		for k, v := range nv {
			rsT[k] = ConvertFromObject(v)
		}

		return rsT
	case Array:
		if nv == nil {
			return nil
		}

		rsT := make([]interface{}, 0, len(nv))

		for _, v := range nv {
			rsT = append(rsT, ConvertFromObject(v))
		}

		return rsT
	}

	if vA.TypeCode() == 0 {
		return nil
	}

	return vA
}

func ObjectsToI(aryA []Object) []interface{} {
	if aryA == nil {
		return nil
	}

	rs := make([]interface{}, 0, len(aryA))

	for _, v := range aryA {
		rs = append(rs, ConvertFromObject(v))
	}

	return rs
}

func ObjectsToN(aryA []Object) []int {
	if aryA == nil {
		return nil
	}

	rs := make([]int, 0, len(aryA))

	for _, v := range aryA {
		vT := ConvertFromObject(v)
		if nv, ok := vT.(int); ok {
			rs = append(rs, nv)
		}
	}

	return rs
}

func ObjectsToS(aryA []Object) []string {
	if aryA == nil {
		return nil
	}

	rs := make([]string, 0, len(aryA))

	for _, v := range aryA {
		rs = append(rs, v.String())
	}

	return rs
}

func ObjectsToBytes(aryA []Object) []byte {
	if aryA == nil {
		return nil
	}

	rs := make([]byte, 0, len(aryA))

	for _, v := range aryA {
		rs = append(rs, byte(v.(Byte)))
	}

	return rs
}

func DownloadStringFromSSH(sshA string, filePathA string) string {
	aryT := tk.Split(sshA, ":")

	basePathT, errT := tk.EnsureBasePath("char")

	if errT != nil {
		return tk.GenerateErrorStringF("failed to find base path: %v", errT)
	}

	if len(aryT) != 5 {
		aryT = tk.Split(tk.LoadStringFromFile(tk.JoinPath(basePathT, "ssh.cfg"))+filePathA, ":")

		if len(aryT) != 5 {
			return tk.ErrStrF("invalid ssh config: %v", "")
		}

	}

	clientT, errT := tk.NewSSHClient(aryT[0], tk.StrToIntWithDefaultValue(aryT[1], 22), aryT[2], aryT[3])

	if errT != nil {
		return tk.ErrToStrF("failed to create SSH client:", errT)
	}

	tmpPathT := tk.JoinPath(basePathT, "tmp")

	errT = tk.EnsureMakeDirsE(tmpPathT)

	if errT != nil {
		return tk.ErrToStrF("failed to create tmp dir:", errT)
	}

	tmpFileT, errT := tk.CreateTempFile(tmpPathT, "")

	if errT != nil {
		return tk.ErrToStrF("failed to create tmp dir:", errT)
	}

	defer os.Remove(tmpFileT)

	errT = clientT.Download(aryT[4], tmpFileT)

	if errT != nil {
		return tk.ErrToStrF("failed to download file:", errT)
	}

	fcT := tk.LoadStringFromFile(tmpFileT)

	return fcT
}

func GetCfgString(fileNameA string) string {
	basePathT, errT := tk.EnsureBasePath("char")

	if errT == nil {
		cfgPathT := tk.JoinPath(basePathT, fileNameA)

		cfgStrT := tk.Trim(tk.LoadStringFromFile(cfgPathT))

		if !tk.IsErrorString(cfgStrT) {
			return cfgStrT
		}

		return tk.ErrStrF("failed to get config string: %v", tk.GetErrorString(cfgStrT))

	}

	return tk.ErrStrF("failed to get config string: %v", errT)
}

func SetCfgString(fileNameA string, strA string) string {
	basePathT, errT := tk.EnsureBasePath("char")

	if errT == nil {
		cfgPathT := tk.JoinPath(basePathT, fileNameA)

		rsT := tk.SaveStringToFile(strA, cfgPathT)

		if tk.IsErrorString(rsT) {
			return tk.ErrStrF("failed to save config string: %v", tk.GetErrorString(rsT))
		}

		return ""

	}

	return tk.ErrStrF("failed to save config string: %v", errT)
}

func NewChar(codeA string) (interface{}, error) {
	bytecodeT, errT := Compile([]byte(codeA), DefaultCompilerOptions)
	// if errT != nil {
	// 	return nil, errT
	// }

	return bytecodeT, errT
}

func NewCommonError(formatA string, argsA ...interface{}) *Error {
	return ErrCommon.NewError(
		fmt.Sprintf(formatA, argsA...))
}

func NewError(nameA string, formatA string, argsA ...interface{}) *Error {
	errT := ErrCommon.NewError(fmt.Sprintf(formatA, argsA...))

	errT.Name = nameA

	return errT
}

func NewFromError(errA error) *Error {
	if errA == nil {
		return nil
	}

	errT := ErrCommon.NewError(errA.Error())

	errT.Name = "error"

	return errT
}

func WrapError(errA error) *Error {
	return &Error{Name: "Error", Message: errA.Error(), Cause: errA}
}
