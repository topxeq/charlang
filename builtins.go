package charlang

import (
	"bytes"
	"compress/flate"
	"database/sql"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"sort"
	"strconv"
	"strings"
	"time"
	"unicode/utf8"

	"github.com/topxeq/charlang/token"
	"github.com/topxeq/sqltk"
	"github.com/topxeq/tk"

	"github.com/mholt/archiver/v3"
)

var (
	// PrintWriter is the default writer for printf and println builtins.
	PrintWriter io.Writer = os.Stdout
)

// BuiltinType represents a builtin type
type BuiltinType byte

// Builtins
const (
	BuiltinAppend BuiltinType = iota

	// BuiltinSortByFunc
	BuiltintStrFindDiffPos
	BuiltinRemoveItems
	BuiltinAppendList
	BuiltinGetRandomInt
	BuiltinGetRandomFloat
	BuiltinGetRandomStr
	BuiltinFormatSQLValue
	BuiltinDbClose
	BuiltinDbQuery
	BuiltinDbQueryRecs
	BuiltinDbQueryMap
	BuiltinDbQueryMapArray
	BuiltinDbQueryCount
	BuiltinDbQueryFloat
	BuiltinDbQueryString
	BuiltinDbExec
	BuiltinRemoveFile
	BuiltinFileExists
	BuiltinGenJSONResp
	BuiltinUrlExists
	BuiltinErrStrf
	BuiltinCharCode
	BuiltinGel
	BuiltinGetReqBody
	BuiltinGetReqHeader
	BuiltinSetRespHeader
	BuiltinParseReqForm
	BuiltinParseReqFormEx
	BuiltinWriteResp
	BuiltinMux
	BuiltinMutex
	BuiltinFatalf
	BuiltinSeq
	BuiltinIsNil
	BuiltinGetValue
	BuiltinSetValue
	BuiltinGetMember
	BuiltinSetMember
	BuiltinCallMethod
	BuiltinDumpVar
	BuiltinDebugInfo
	BuiltinMake
	BuiltinDatabase
	BuiltinStatusResult
	BuiltinUnref
	BuiltinSetValueByRef
	BuiltinGetWeb
	BuiltintRegFindFirstGroups
	BuiltintWriteStr
	BuiltintStrSplitLines
	BuiltinNew
	BuiltinStringBuilder
	BuiltinStrReplace
	BuiltinGetErrStrX
	BuiltinSshUpload
	BuiltinArchiveFilesToZip
	BuiltinGetOSName
	BuiltinGetOSArch
	BuiltinGetAppDir
	BuiltinGetCurDir
	BuiltinGetHomeDir
	BuiltinGetClipText
	BuiltinSetClipText
	BuiltinRegQuote
	BuiltinRegReplace
	BuiltinAny
	BuiltinTrim
	BuiltinStrTrim
	BuiltinStrTrimStart
	BuiltinStrTrimEnd
	BuiltinStrTrimLeft
	BuiltinStrTrimRight
	BuiltinRegFindFirst
	BuiltinRegFindAll
	BuiltinCheckErrX
	BuiltinLoadText
	BuiltinSaveText
	BuiltinJoinPath
	BuiltinGetEnv
	BuiltinSetEnv
	BuiltinTypeOfAny
	BuiltinToStr
	BuiltinCallNamedFunc
	BuiltinCallInternalFunc
	BuiltinGetNamedValue
	BuiltinNewEx
	BuiltinCallMethodEx
	BuiltinToTime
	// BuiltinNewAny
	BuiltinTime
	BuiltinSleep
	BuiltinExit
	BuiltinSystemCmd
	BuiltinIsErrX
	BuiltinToJSON
	BuiltinFromJSON
	BuiltinPlo
	BuiltinPlt
	BuiltinGetParam
	BuiltinGetSwitch
	BuiltinIfSwitchExists
	BuiltinTypeCode
	BuiltinTypeName
	BuiltinPl
	BuiltinPrf
	BuiltinPln
	BuiltinPlv
	BuiltinTestByText
	BuiltinTestByStartsWith
	BuiltinTestByEndsWith
	BuiltinTestByContains
	BuiltinTestByRegContains
	BuiltinTestByReg
	BuiltinGetSeq
	BuiltinPass

	BuiltinDelete
	BuiltinCopy
	BuiltinRepeat
	BuiltinContains
	BuiltinLen
	BuiltinSort
	BuiltinSortReverse
	BuiltinError
	BuiltinBool
	BuiltinInt
	BuiltinUint
	BuiltinFloat
	BuiltinChar
	BuiltinByte
	BuiltinString
	BuiltinMutableString
	BuiltinBytes
	BuiltinChars
	BuiltinPrintf
	BuiltinPrintln
	BuiltinSprintf
	BuiltinGlobals

	BuiltinIsError
	BuiltinIsInt
	BuiltinIsUint
	BuiltinIsFloat
	BuiltinIsChar
	BuiltinIsBool
	BuiltinIsString
	BuiltinIsBytes
	BuiltinIsMap
	BuiltinIsSyncMap
	BuiltinIsArray
	BuiltinIsUndefined
	BuiltinIsFunction
	BuiltinIsCallable
	BuiltinIsIterable

	BuiltinWrongNumArgumentsError
	BuiltinInvalidOperatorError
	BuiltinIndexOutOfBoundsError
	BuiltinNotIterableError
	BuiltinNotIndexableError
	BuiltinNotIndexAssignableError
	BuiltinNotCallableError
	BuiltinNotImplementedError
	BuiltinZeroDivisionError
	BuiltinTypeError

	BuiltinMakeArray
	BuiltinCap
)

// BuiltinsMap is list of builtin types, exported for REPL.
var BuiltinsMap = map[string]BuiltinType{
	// funcs start

	// internal & debug related
	"testByText":        BuiltinTestByText,
	"testByStartsWith":  BuiltinTestByStartsWith,
	"testByEndsWith":    BuiltinTestByEndsWith,
	"testByContains":    BuiltinTestByContains,
	"testByReg":         BuiltinTestByReg,
	"testByRegContains": BuiltinTestByRegContains,

	"dumpVar":   BuiltinDumpVar,
	"debugInfo": BuiltinDebugInfo,

	// data type related
	"typeCode":  BuiltinTypeCode,
	"typeName":  BuiltinTypeName,
	"typeOf":    BuiltinTypeName,
	"typeOfAny": BuiltinTypeOfAny,

	"time":          BuiltinTime,
	"stringBuilder": BuiltinStringBuilder,
	"statusResult":  BuiltinStatusResult,
	"seq":           BuiltinSeq,
	"mutex":         BuiltinMutex,
	"charCode":      BuiltinCharCode,
	"gel":           BuiltinGel,
	"any":           BuiltinAny,

	"toTime": BuiltinToTime,

	"isNil": BuiltinIsNil,

	// new related
	"make": BuiltinMake,
	"new":  BuiltinNew,

	"newEx": BuiltinNewEx,

	// array/map related
	"appendList":  BuiltinAppendList,
	"appendArray": BuiltinAppendList,
	"appendSlice": BuiltinAppendList,

	"removeItems": BuiltinRemoveItems, // inclusive

	// ref/pointer related
	"setValueByRef": BuiltinSetValueByRef,
	"unref":         BuiltinUnref,

	// convert related
	"toStr": BuiltinToStr,

	// string related
	"trim":          BuiltinTrim,
	"strTrim":       BuiltinStrTrim,
	"strTrimStart":  BuiltinStrTrimStart,
	"strTrimEnd":    BuiltinStrTrimEnd,
	"strTrimLeft":   BuiltinStrTrimLeft,
	"strTrimRight":  BuiltinStrTrimRight,
	"strReplace":    BuiltinStrReplace,
	"strSplitLines": BuiltintStrSplitLines,

	"strFindDiffPos": BuiltintStrFindDiffPos, // return -1 if 2 strings are identical

	// regex related
	"regFindFirst":       BuiltinRegFindFirst,
	"regFindFirstGroups": BuiltintRegFindFirstGroups, // obtain the first match of a regular expression and return a list of all matching groups, where the first item is the complete matching result and the second item is the first matching group..., usage example: result := regFindFirstGroups(str1, regex1)
	"regFindAll":         BuiltinRegFindAll,
	"regQuote":           BuiltinRegQuote,
	"regReplace":         BuiltinRegReplace,

	// time related

	// control related
	"exit": BuiltinExit,

	// print related
	"pl":     BuiltinPl,
	"pln":    BuiltinPln,
	"plv":    BuiltinPlv,
	"plo":    BuiltinPlo,
	"plt":    BuiltinPlt,
	"prf":    BuiltinPrf,
	"fatalf": BuiltinFatalf,

	// control related

	// error related
	"isErrX":     BuiltinIsErrX,
	"isErr":      BuiltinIsErrX,
	"getErrStrX": BuiltinGetErrStrX,
	"getErrStr":  BuiltinGetErrStrX,

	"checkErrX": BuiltinCheckErrX,
	"checkErr":  BuiltinCheckErrX,

	"errStrf": BuiltinErrStrf,

	// random related
	"getRandomInt":   BuiltinGetRandomInt,
	"getRandomFloat": BuiltinGetRandomFloat,
	"getRandomStr":   BuiltinGetRandomStr,
	"genRandomStr":   BuiltinGetRandomStr,

	// member/method related
	"getValue":         BuiltinGetValue,
	"setValue":         BuiltinSetValue,
	"getMember":        BuiltinGetMember,
	"mb":               BuiltinGetMember,
	"setMember":        BuiltinSetMember,
	"callMethod":       BuiltinCallMethod,
	"mt":               BuiltinCallMethod,
	"callMethodEx":     BuiltinCallMethodEx,
	"mtEx":             BuiltinCallMethodEx,
	"callNamedFunc":    BuiltinCallNamedFunc,
	"callInternalFunc": BuiltinCallInternalFunc,
	"getNamedValue":    BuiltinGetNamedValue,

	// read/write related
	"writeStr": BuiltintWriteStr,

	// encode/decode related
	"toJSON":   BuiltinToJSON,
	"toJson":   BuiltinToJSON,
	"fromJSON": BuiltinFromJSON,
	"fromJson": BuiltinFromJSON,

	// command-line related
	"ifSwitchExists": BuiltinIfSwitchExists,
	"getSwitch":      BuiltinGetSwitch,
	"getParam":       BuiltinGetParam,

	// clipboard related
	"getClipText": BuiltinGetClipText,
	"setClipText": BuiltinSetClipText,

	// thread related
	"sleep": BuiltinSleep,

	// os/system related
	"systemCmd":  BuiltinSystemCmd,
	"getEnv":     BuiltinGetEnv,
	"setEnv":     BuiltinSetEnv,
	"getOsName":  BuiltinGetOSName,
	"getOSName":  BuiltinGetOSName,
	"getOSArch":  BuiltinGetOSArch,
	"getOsArch":  BuiltinGetOSArch,
	"getAppDir":  BuiltinGetAppDir,
	"getCurDir":  BuiltinGetCurDir,
	"getHomeDir": BuiltinGetHomeDir,

	// path related
	"joinPath": BuiltinJoinPath, // join multiple file paths into one, equivalent to path/filepath.Join in the Go language standard library

	// file related
	"fileExists":   BuiltinFileExists,
	"ifFileExists": BuiltinFileExists,

	"removeFile": BuiltinRemoveFile,

	"loadText": BuiltinLoadText,
	"saveText": BuiltinSaveText,

	// compress/zip related
	"archiveFilesToZip": BuiltinArchiveFilesToZip, // Add multiple files to a newly created zip file. The first parameter is the zip file name, with a suffix of '.zip'. Optional parameters include '-overwrite' (whether to overwrite existing files) and '-makeDirs' (whether to create a new directory as needed). Other parameters are treated as files or directories to be added, and the directory will be recursively added to the zip file. If the parameter is a list, it will be treated as a list of file names, and all files in it will be added

	// network/web related
	"getWeb":    BuiltinGetWeb,
	"urlExists": BuiltinUrlExists,

	// server/service related
	"mux":            BuiltinMux,
	"getReqHeader":   BuiltinGetReqHeader,
	"getReqBody":     BuiltinGetReqBody,
	"parseReqForm":   BuiltinParseReqForm,
	"parseReqFormEx": BuiltinParseReqFormEx,
	"setRespHeader":  BuiltinSetRespHeader,
	"writeResp":      BuiltinWriteResp,
	"genJSONResp":    BuiltinGenJSONResp,
	"genJsonResp":    BuiltinGenJSONResp,

	// ssh related
	"sshUpload": BuiltinSshUpload,

	// database related
	"database":  BuiltinDatabase,
	"dbConnect": BuiltinDatabase,
	"dbClose":   BuiltinDbClose,

	"dbQuery":         BuiltinDbQuery,
	"dbQueryRecs":     BuiltinDbQueryRecs,
	"dbQueryMap":      BuiltinDbQueryMap,
	"dbQueryMapArray": BuiltinDbQueryMapArray,
	"dbQueryCount":    BuiltinDbQueryCount,
	"dbQueryFloat":    BuiltinDbQueryFloat,
	"dbQueryString":   BuiltinDbQueryString,

	"dbExec": BuiltinDbExec,

	"formatSQLValue": BuiltinFormatSQLValue,

	// misc related
	"getSeq": BuiltinGetSeq,
	"pass":   BuiltinPass,

	// "sortByFunc": BuiltinSortByFunc,

	// original internal related
	"append":        BuiltinAppend,
	"delete":        BuiltinDelete,
	"copy":          BuiltinCopy,
	"repeat":        BuiltinRepeat,
	"contains":      BuiltinContains,
	"len":           BuiltinLen,
	"sort":          BuiltinSort,
	"sortReverse":   BuiltinSortReverse,
	"error":         BuiltinError,
	"bool":          BuiltinBool,
	"int":           BuiltinInt,
	"uint":          BuiltinUint,
	"float":         BuiltinFloat,
	"char":          BuiltinChar,
	"byte":          BuiltinByte,
	"string":        BuiltinString,
	"mutableString": BuiltinMutableString,
	"bytes":         BuiltinBytes,
	"chars":         BuiltinChars,
	"printf":        BuiltinPrintf,
	"println":       BuiltinPrintln,
	"sprintf":       BuiltinSprintf,
	"globals":       BuiltinGlobals,

	"isError":     BuiltinIsError,
	"isInt":       BuiltinIsInt,
	"isUint":      BuiltinIsUint,
	"isFloat":     BuiltinIsFloat,
	"isChar":      BuiltinIsChar,
	"isBool":      BuiltinIsBool,
	"isString":    BuiltinIsString,
	"isBytes":     BuiltinIsBytes,
	"isMap":       BuiltinIsMap,
	"isSyncMap":   BuiltinIsSyncMap,
	"isArray":     BuiltinIsArray,
	"isUndefined": BuiltinIsUndefined,
	"isFunction":  BuiltinIsFunction,
	"isCallable":  BuiltinIsCallable,
	"isIterable":  BuiltinIsIterable,

	"WrongNumArgumentsError":  BuiltinWrongNumArgumentsError,
	"InvalidOperatorError":    BuiltinInvalidOperatorError,
	"IndexOutOfBoundsError":   BuiltinIndexOutOfBoundsError,
	"NotIterableError":        BuiltinNotIterableError,
	"NotIndexableError":       BuiltinNotIndexableError,
	"NotIndexAssignableError": BuiltinNotIndexAssignableError,
	"NotCallableError":        BuiltinNotCallableError,
	"NotImplementedError":     BuiltinNotImplementedError,
	"ZeroDivisionError":       BuiltinZeroDivisionError,
	"TypeError":               BuiltinTypeError,

	":makeArray": BuiltinMakeArray,
	"cap":        BuiltinCap,

	// funcs end

}

// BuiltinObjects is list of builtins, exported for REPL.
var BuiltinObjects = [...]Object{
	// :makeArray is a private builtin function to help destructuring array assignments
	BuiltinMakeArray: &BuiltinFunction{
		Name:    ":makeArray",
		Value:   funcPiOROe(builtinMakeArrayFunc),
		ValueEx: funcPiOROeEx(builtinMakeArrayFunc),
	},
	// char add start
	BuiltinGetRandomInt: &BuiltinFunction{
		Name:    "getRandomInt",
		Value:   CallExAdapter(builtinGetRandomIntFunc),
		ValueEx: builtinGetRandomIntFunc,
	},
	BuiltinGetRandomFloat: &BuiltinFunction{
		Name:    "getRandomFloat",
		Value:   CallExAdapter(builtinGetRandomFloatFunc),
		ValueEx: builtinGetRandomFloatFunc,
	},
	BuiltinGetRandomStr: &BuiltinFunction{
		Name:    "getRandomStr",
		Value:   CallExAdapter(builtinGetRandomStrFunc),
		ValueEx: builtinGetRandomStrFunc,
	},
	BuiltinDbClose: &BuiltinFunction{
		Name:    "dbClose",
		Value:   CallExAdapter(BuiltinDbCloseFunc),
		ValueEx: BuiltinDbCloseFunc,
	},
	BuiltintStrFindDiffPos: &BuiltinFunction{
		Name:    "strFindDiffPos",
		Value:   fnASSRI(tk.FindFirstDiffIndex),
		ValueEx: fnASSRIex(tk.FindFirstDiffIndex),
	},
	BuiltinDbQuery: &BuiltinFunction{
		Name:    "dbQuery",
		Value:   fnADSVaRA(sqltk.QueryDBX),
		ValueEx: fnADSVaRAex(sqltk.QueryDBX),
	},
	BuiltinDbQueryCount: &BuiltinFunction{
		Name:    "dbQueryCount",
		Value:   fnADSVaRA(sqltk.QueryCountX),
		ValueEx: fnADSVaRAex(sqltk.QueryCountX),
	},
	BuiltinDbQueryFloat: &BuiltinFunction{
		Name:    "dbQueryFloat",
		Value:   fnADSVaRA(sqltk.QueryFloatX),
		ValueEx: fnADSVaRAex(sqltk.QueryFloatX),
	},
	BuiltinDbQueryString: &BuiltinFunction{
		Name:    "dbQueryString",
		Value:   fnADSVaRA(sqltk.QueryStringX),
		ValueEx: fnADSVaRAex(sqltk.QueryStringX),
	},
	BuiltinDbQueryRecs: &BuiltinFunction{
		Name:    "dbQueryRecs",
		Value:   fnADSVaRA(sqltk.QueryDBRecsX),
		ValueEx: fnADSVaRAex(sqltk.QueryDBRecsX),
	},
	BuiltinDbQueryMap: &BuiltinFunction{
		Name:    "dbQueryMap",
		Value:   fnADSSVaRA(sqltk.QueryDBMapX),
		ValueEx: fnADSSVaRAex(sqltk.QueryDBMapX),
	},
	BuiltinDbQueryMapArray: &BuiltinFunction{
		Name:    "dbQueryMapArray",
		Value:   fnADSSVaRA(sqltk.QueryDBMapArrayX),
		ValueEx: fnADSSVaRAex(sqltk.QueryDBMapArrayX),
	},
	BuiltinDbExec: &BuiltinFunction{
		Name:    "dbExec",
		Value:   fnADSVaRA(sqltk.ExecDBX),
		ValueEx: fnADSVaRAex(sqltk.ExecDBX),
	},
	BuiltinFormatSQLValue: &BuiltinFunction{
		Name:    "formatSQLValue",
		Value:   fnASRS(sqltk.FormatSQLValue),
		ValueEx: fnASRSex(sqltk.FormatSQLValue),
	},
	BuiltinRemoveFile: &BuiltinFunction{
		Name:    "removeFile",
		Value:   fnASRE(tk.RemoveFile),
		ValueEx: fnASREex(tk.RemoveFile),
	},
	BuiltinGenJSONResp: &BuiltinFunction{
		Name:    "genJsonResp",
		Value:   CallExAdapter(builtinGenJSONRespFunc),
		ValueEx: builtinGenJSONRespFunc,
	},
	BuiltinGetReqBody: &BuiltinFunction{
		Name:    "getReqBody",
		Value:   CallExAdapter(builtinGetReqBodyFunc),
		ValueEx: builtinGetReqBodyFunc,
	},
	BuiltinGetReqHeader: &BuiltinFunction{
		Name:    "getReqHeader",
		Value:   CallExAdapter(builtinGetReqHeaderFunc),
		ValueEx: builtinGetReqHeaderFunc,
	},
	BuiltinSetRespHeader: &BuiltinFunction{
		Name:    "setRespHeader",
		Value:   CallExAdapter(builtinSetRespHeaderFunc),
		ValueEx: builtinSetRespHeaderFunc,
	},
	BuiltinParseReqForm: &BuiltinFunction{
		Name:    "parseReqForm",
		Value:   CallExAdapter(builtinParseReqFormFunc),
		ValueEx: builtinParseReqFormFunc,
	},
	BuiltinParseReqFormEx: &BuiltinFunction{
		Name:    "parseReqFormEx",
		Value:   CallExAdapter(builtinParseReqFormExFunc),
		ValueEx: builtinParseReqFormExFunc,
	},
	BuiltinWriteResp: &BuiltinFunction{
		Name:    "writeResp",
		Value:   CallExAdapter(builtinWriteRespFunc),
		ValueEx: builtinWriteRespFunc,
	},
	BuiltinMux: &BuiltinFunction{
		Name:    "mux",
		Value:   CallExAdapter(builtinMuxFunc),
		ValueEx: builtinMuxFunc,
	},
	BuiltinMutex: &BuiltinFunction{
		Name:    "mutex",
		Value:   CallExAdapter(builtinMutexFunc),
		ValueEx: builtinMutexFunc,
	},
	BuiltinErrStrf: &BuiltinFunction{
		Name:    "errStrf",
		Value:   fnASVaRS(tk.ErrStrf),
		ValueEx: fnASVaRSex(tk.ErrStrf),
	},
	BuiltinCharCode: &BuiltinFunction{
		Name:    "charCode",
		Value:   CallExAdapter(builtinCharCodeFunc),
		ValueEx: builtinCharCodeFunc,
	},
	BuiltinGel: &BuiltinFunction{
		Name:    "gel",
		Value:   CallExAdapter(builtinGelFunc),
		ValueEx: builtinGelFunc,
	},
	BuiltinFatalf: &BuiltinFunction{
		Name:    "fatalf",
		Value:   CallExAdapter(builtinFatalfFunc),
		ValueEx: builtinFatalfFunc,
	},
	BuiltinDumpVar: &BuiltinFunction{
		Name:    "dumpVar",
		Value:   CallExAdapter(builtinDumpVarFunc),
		ValueEx: builtinDumpVarFunc,
	},
	BuiltinDebugInfo: &BuiltinFunction{
		Name:    "debugInfo",
		Value:   CallExAdapter(builtinDebugInfoFunc),
		ValueEx: builtinDebugInfoFunc,
	},
	BuiltinMake: &BuiltinFunction{
		Name:    "make",
		Value:   CallExAdapter(builtinMakeFunc),
		ValueEx: builtinMakeFunc,
	},
	BuiltinDatabase: &BuiltinFunction{
		Name:    "database",
		Value:   CallExAdapter(builtinDatabaseFunc),
		ValueEx: builtinDatabaseFunc,
	},
	BuiltinSeq: &BuiltinFunction{
		Name:    "seq",
		Value:   CallExAdapter(builtinSeqFunc),
		ValueEx: builtinSeqFunc,
	},
	BuiltinStatusResult: &BuiltinFunction{
		Name:    "statusResult",
		Value:   CallExAdapter(builtinStatusResultFunc),
		ValueEx: builtinStatusResultFunc,
	},
	BuiltinUnref: &BuiltinFunction{
		Name:    "unref",
		Value:   CallExAdapter(builtinUnrefFunc),
		ValueEx: builtinUnrefFunc,
	},
	BuiltinSetValueByRef: &BuiltinFunction{
		Name:    "setValueByRef",
		Value:   CallExAdapter(builtinSetValueByRefFunc),
		ValueEx: builtinSetValueByRefFunc,
	},
	BuiltinGetWeb: &BuiltinFunction{
		Name:    "getWeb",
		Value:   fnASVaRA(tk.GetWeb),
		ValueEx: fnASVaRAex(tk.GetWeb),
	},
	BuiltinUrlExists: &BuiltinFunction{
		Name:    "urlExists",
		Value:   fnASVaRA(tk.UrlExists),
		ValueEx: fnASVaRAex(tk.UrlExists),
	},
	BuiltintRegFindFirstGroups: &BuiltinFunction{
		Name:    "regFindFirstGroups",
		Value:   fnASSRLs(tk.RegFindFirstGroupsX),
		ValueEx: fnASSRLsex(tk.RegFindFirstGroupsX),
	},
	BuiltintWriteStr: &BuiltinFunction{
		Name:    "writeStr",
		Value:   CallExAdapter(builtinWriteStrFunc),
		ValueEx: builtinWriteStrFunc,
	},
	BuiltinNew: &BuiltinFunction{
		Name:    "new",
		Value:   CallExAdapter(builtinNewFunc),
		ValueEx: builtinNewFunc,
	},
	BuiltinStringBuilder: &BuiltinFunction{
		Name:    "stringBuilder",
		Value:   CallExAdapter(builtinStringBuilderFunc),
		ValueEx: builtinStringBuilderFunc,
	},
	BuiltintStrSplitLines: &BuiltinFunction{
		Name:    "strSplitLines",
		Value:   fnASRLs(tk.SplitLines),
		ValueEx: fnASRLsex(tk.SplitLines),
	},
	BuiltinStrReplace: &BuiltinFunction{
		Name:    "strReplace",
		Value:   fnASVsRS(tk.StringReplace),
		ValueEx: fnASVsRSex(tk.StringReplace),
	},
	BuiltinRegReplace: &BuiltinFunction{
		Name:    "regReplace",
		Value:   fnASSSRS(tk.RegReplaceX),
		ValueEx: fnASSSRSex(tk.RegReplaceX),
	},
	BuiltinGetErrStrX: &BuiltinFunction{
		Name:    "getErrStrX",
		Value:   fnAARS(tk.GetErrStrX),
		ValueEx: fnAARSex(tk.GetErrStrX),
	},
	BuiltinSshUpload: &BuiltinFunction{
		Name:    "sshUpload",
		Value:   CallExAdapter(builtinSshUploadFunc),
		ValueEx: builtinSshUploadFunc,
	},
	BuiltinArchiveFilesToZip: &BuiltinFunction{
		Name:    "archiveFilesToZip",
		Value:   CallExAdapter(builtinArchiveFilesToZipFunc),
		ValueEx: builtinArchiveFilesToZipFunc,
	},
	BuiltinGetOSName: &BuiltinFunction{
		Name:    "getOSName",
		Value:   fnARS(tk.GetOSName),
		ValueEx: fnARSex(tk.GetOSName),
	},
	BuiltinGetOSArch: &BuiltinFunction{
		Name:    "getOSArch",
		Value:   fnARS(tk.GetOSArch),
		ValueEx: fnARSex(tk.GetOSArch),
	},
	BuiltinGetHomeDir: &BuiltinFunction{
		Name:    "getHomeDir",
		Value:   fnARS(tk.GetHomeDir),
		ValueEx: fnARSex(tk.GetHomeDir),
	},
	BuiltinGetAppDir: &BuiltinFunction{
		Name:    "getAppDir",
		Value:   fnARS(tk.GetApplicationPath),
		ValueEx: fnARSex(tk.GetApplicationPath),
	},
	BuiltinGetCurDir: &BuiltinFunction{
		Name:    "getCurDir",
		Value:   fnARS(tk.GetCurrentDir),
		ValueEx: fnARSex(tk.GetCurrentDir),
	},
	BuiltinTrim: &BuiltinFunction{
		Name:    "trim",
		Value:   CallExAdapter(builtinTrimFunc),
		ValueEx: builtinTrimFunc,
	},
	BuiltinStrTrim: &BuiltinFunction{
		Name:    "strTrim",
		Value:   CallExAdapter(builtinStrTrimFunc),
		ValueEx: builtinStrTrimFunc,
	},
	BuiltinStrTrimStart: &BuiltinFunction{
		Name:    "strTrimStart",
		Value:   CallExAdapter(builtinStrTrimStartFunc),
		ValueEx: builtinStrTrimStartFunc,
	},
	BuiltinStrTrimEnd: &BuiltinFunction{
		Name:    "strTrimEnd",
		Value:   CallExAdapter(builtinStrTrimEndFunc),
		ValueEx: builtinStrTrimEndFunc,
	},
	BuiltinStrTrimLeft: &BuiltinFunction{
		Name:    "strTrimLeft",
		Value:   CallExAdapter(builtinStrTrimLeftFunc),
		ValueEx: builtinStrTrimLeftFunc,
	},
	BuiltinStrTrimRight: &BuiltinFunction{
		Name:    "strTrimRight",
		Value:   CallExAdapter(builtinStrTrimRightFunc),
		ValueEx: builtinStrTrimRightFunc,
	},
	BuiltinRegFindFirst: &BuiltinFunction{
		Name:    "regFindFirst",
		Value:   fnASSIRS(tk.RegFindFirstX),
		ValueEx: fnASSIRSex(tk.RegFindFirstX),
	},
	BuiltinRegFindAll: &BuiltinFunction{
		Name:    "regFindAll",
		Value:   fnASSIRLs(tk.RegFindAllX),
		ValueEx: fnASSIRLsex(tk.RegFindAllX),
	},
	BuiltinCheckErrX: &BuiltinFunction{
		Name:    "checkErrX",
		Value:   CallExAdapter(builtinCheckErrXFunc),
		ValueEx: builtinCheckErrXFunc,
	},
	BuiltinFileExists: &BuiltinFunction{
		Name:    "fileExists",
		Value:   fnASRB(tk.IfFileExists),
		ValueEx: fnASRBex(tk.IfFileExists),
	},
	BuiltinLoadText: &BuiltinFunction{
		Name:    "loadText",
		Value:   fnASRS(tk.LoadStringFromFile),
		ValueEx: fnASRSex(tk.LoadStringFromFile),
	},
	BuiltinSaveText: &BuiltinFunction{
		Name:    "saveText",
		Value:   fnASSRS(tk.SaveStringToFile),
		ValueEx: fnASSRSex(tk.SaveStringToFile),
	},
	BuiltinJoinPath: &BuiltinFunction{
		Name:    "joinPath",
		Value:   fnAVsRS(filepath.Join),
		ValueEx: fnAVsRSex(filepath.Join),
	},
	BuiltinGetEnv: &BuiltinFunction{
		Name:    "getEnv",
		Value:   CallExAdapter(builtinGetEnvFunc),
		ValueEx: builtinGetEnvFunc,
	},
	BuiltinSetEnv: &BuiltinFunction{
		Name:    "setEnv",
		Value:   fnASSRE(os.Setenv),
		ValueEx: fnASSREex(os.Setenv),
	},
	BuiltinTypeOfAny: &BuiltinFunction{
		Name:    "typeOfAny",
		Value:   CallExAdapter(builtinTypeOfAnyFunc),
		ValueEx: builtinTypeOfAnyFunc,
	},
	BuiltinToStr: &BuiltinFunction{
		Name:    "toStr", // usage: toStr(any)
		Value:   CallExAdapter(builtinToStrFunc),
		ValueEx: builtinToStrFunc,
	},
	BuiltinCallNamedFunc: &BuiltinFunction{
		Name:    "callNamedFunc",
		Value:   CallExAdapter(builtinCallNamedFuncFunc),
		ValueEx: builtinCallNamedFuncFunc,
	},
	BuiltinCallInternalFunc: &BuiltinFunction{
		Name:    "callInternalFunc",
		Value:   CallExAdapter(builtinCallInternalFuncFunc),
		ValueEx: builtinCallInternalFuncFunc,
	},
	BuiltinGetNamedValue: &BuiltinFunction{
		Name:    "getNamedValue",
		Value:   CallExAdapter(builtinGetNamedValueFunc),
		ValueEx: builtinGetNamedValueFunc,
	},
	BuiltinNewEx: &BuiltinFunction{
		Name:    "newEx",
		Value:   CallExAdapter(builtinNewExFunc),
		ValueEx: builtinNewExFunc,
	},
	BuiltinCallMethodEx: &BuiltinFunction{
		Name:    "callMethodEx",
		Value:   CallExAdapter(builtinCallMethodExFunc),
		ValueEx: builtinCallMethodExFunc,
	},
	BuiltinGetValue: &BuiltinFunction{
		Name:    "getValue",
		Value:   CallExAdapter(builtinGetValueFunc),
		ValueEx: builtinGetValueFunc,
	},
	BuiltinGetMember: &BuiltinFunction{
		Name:    "getMember",
		Value:   CallExAdapter(builtinGetMemberFunc),
		ValueEx: builtinGetMemberFunc,
	},
	BuiltinSetMember: &BuiltinFunction{
		Name:    "setMember",
		Value:   CallExAdapter(builtinSetMemberFunc),
		ValueEx: builtinSetMemberFunc,
	},
	BuiltinSetValue: &BuiltinFunction{
		Name:    "setValue",
		Value:   CallExAdapter(builtinSetValueFunc),
		ValueEx: builtinSetValueFunc,
	},
	BuiltinCallMethod: &BuiltinFunction{
		Name:    "callMethod",
		Value:   CallExAdapter(builtinCallMethodFunc),
		ValueEx: builtinCallMethodFunc,
	},
	// BuiltinNewAny: &BuiltinFunction{
	// 	Name:  "newAny",
	// 	Value: builtinNewAnyFunc,
	// },
	BuiltinGetClipText: &BuiltinFunction{
		Name:    "getClipText",
		Value:   fnARS(tk.GetClipText),
		ValueEx: fnARSex(tk.GetClipText),
	},
	BuiltinSetClipText: &BuiltinFunction{
		Name:    "setClipText",
		Value:   fnASRE(tk.SetClipText),
		ValueEx: fnASREex(tk.SetClipText),
	},
	BuiltinRegQuote: &BuiltinFunction{
		Name:    "regQuote",
		Value:   CallExAdapter(builtinRegQuoteFunc),
		ValueEx: builtinRegQuoteFunc,
	},
	BuiltinAny: &BuiltinFunction{
		Name:    "any",
		Value:   CallExAdapter(builtinAnyFunc),
		ValueEx: builtinAnyFunc,
	},
	BuiltinTime: &BuiltinFunction{
		Name:    "time", // new a Time object
		Value:   CallExAdapter(builtinTimeFunc),
		ValueEx: builtinTimeFunc,
	},
	BuiltinToTime: &BuiltinFunction{
		Name:    "toTime", // new a Time object
		Value:   CallExAdapter(builtinTimeFunc),
		ValueEx: builtinTimeFunc,
	},
	BuiltinExit: &BuiltinFunction{
		Name:  "exit", // usage: exit() or exit(1)
		Value: builtinExitFunc,
	},
	BuiltinSleep: &BuiltinFunction{
		Name:    "sleep", // usage: sleep(1.2) sleep for 1.2 seconds
		ValueEx: builtinSleepFunc,
	},
	BuiltinIsErrX: &BuiltinFunction{
		Name:    "isErrX", // usage: isErrX(err1), check if err1 is error or error string(which starts with TXERROR:)
		Value:   CallExAdapter(builtinIsErrXFunc),
		ValueEx: builtinIsErrXFunc,
	},
	BuiltinIsNil: &BuiltinFunction{
		Name:    "isNil", // usage: isNil(err1), check if the argument is nil
		Value:   CallExAdapter(builtinIsNilFunc),
		ValueEx: builtinIsNilFunc,
	},
	BuiltinToJSON: &BuiltinFunction{
		Name:    "toJSON",
		Value:   CallExAdapter(builtinToJSONFunc),
		ValueEx: builtinToJSONFunc,
	},
	BuiltinFromJSON: &BuiltinFunction{
		Name:    "fromJSON",
		Value:   CallExAdapter(builtinFromJSONFunc),
		ValueEx: builtinFromJSONFunc,
	},
	BuiltinPlo: &BuiltinFunction{
		Name: "plo",
		Value: func(args ...Object) (Object, error) {
			return fnAVaRex(tk.Plo)(NewCall(nil, args))
		},
		ValueEx: fnAVaRex(tk.Plo),
	},
	BuiltinPlt: &BuiltinFunction{
		Name:    "plt",
		Value:   CallExAdapter(builtinPltFunc),
		ValueEx: builtinPltFunc,
	},
	BuiltinIfSwitchExists: &BuiltinFunction{
		Name:    "ifSwitchExists", // usage: if ifSwitchExists(argsG, "-verbose") {...}
		Value:   CallExAdapter(builtinIfSwitchExistsFunc),
		ValueEx: builtinIfSwitchExistsFunc,
	},
	BuiltinGetSwitch: &BuiltinFunction{
		Name:    "getSwitch",
		Value:   CallExAdapter(builtinGetSwitchFunc),
		ValueEx: builtinGetSwitchFunc,
	},
	BuiltinGetParam: &BuiltinFunction{
		Name:    "getParam", // usage: getParam(argsG, 1, "default")
		Value:   CallExAdapter(builtinGetParamFunc),
		ValueEx: builtinGetParamFunc,
	},
	BuiltinPln: &BuiltinFunction{
		Name:    "pln",
		Value:   fnAVaR(tk.Pln),
		ValueEx: fnAVaRex(tk.Pln),
	},
	BuiltinPlv: &BuiltinFunction{
		Name:    "plv",
		Value:   fnAVaR(tk.Plv),
		ValueEx: fnAVaRex(tk.Plv),
	},
	BuiltinTypeCode: &BuiltinFunction{
		Name:    "typeCode",
		Value:   CallExAdapter(builtinTypeCodeFunc),
		ValueEx: builtinTypeCodeFunc,
	},
	BuiltinPl: &BuiltinFunction{
		Name:    "pl", // usage: the same as printf, but with a line-end(\n) at the end
		Value:   CallExAdapter(builtinPlFunc),
		ValueEx: builtinPlFunc,
	},
	BuiltinPrf: &BuiltinFunction{
		Name:    "prf", // usage: the same as printf
		Value:   fnASVaR(tk.Printf),
		ValueEx: fnASVaRex(tk.Printf),
	},
	BuiltinPass: &BuiltinFunction{
		Name:    "pass",
		Value:   CallExAdapter(builtinPassFunc),
		ValueEx: builtinPassFunc,
	},
	BuiltinGetSeq: &BuiltinFunction{
		Name: "getSeq",
		Value: func(args ...Object) (Object, error) {
			return fnARIex(tk.GetSeq)(NewCall(nil, args))
		},
		ValueEx: fnARIex(tk.GetSeq),
	},
	BuiltinTestByText: &BuiltinFunction{
		Name:    "testByText",
		Value:   CallExAdapter(builtinTestByTextFunc),
		ValueEx: builtinTestByTextFunc,
	},
	BuiltinTestByStartsWith: &BuiltinFunction{
		Name:    "testByStartsWith",
		Value:   CallExAdapter(builtinTestByStartsWithFunc),
		ValueEx: builtinTestByStartsWithFunc,
	},
	BuiltinTestByEndsWith: &BuiltinFunction{
		Name:    "testByEndsWith",
		Value:   CallExAdapter(builtinTestByEndsWithFunc),
		ValueEx: builtinTestByEndsWithFunc,
	},
	BuiltinTestByContains: &BuiltinFunction{
		Name:    "testByContains",
		Value:   CallExAdapter(builtinTestByContainsFunc),
		ValueEx: builtinTestByContainsFunc,
	},
	BuiltinTestByRegContains: &BuiltinFunction{
		Name:    "testByRegContains",
		Value:   CallExAdapter(builtinTestByRegContainsFunc),
		ValueEx: builtinTestByRegContainsFunc,
	},
	BuiltinTestByReg: &BuiltinFunction{
		Name:    "testByReg",
		Value:   CallExAdapter(builtinTestByRegFunc),
		ValueEx: builtinTestByRegFunc,
	},
	BuiltinSystemCmd: &BuiltinFunction{
		Name:    "systemCmd",
		Value:   fnASVsRS(tk.SystemCmd),
		ValueEx: fnASVsRSex(tk.SystemCmd),
	},
	BuiltinAppendList: &BuiltinFunction{
		Name:    "appendList",
		Value:   CallExAdapter(builtinAppendListFunc),
		ValueEx: builtinAppendListFunc,
	},
	BuiltinRemoveItems: &BuiltinFunction{
		Name:    "removeItems",
		Value:   CallExAdapter(builtinRemoveItemsFunc),
		ValueEx: builtinRemoveItemsFunc,
	},
	// BuiltinSortByFunc: &BuiltinFunction{
	// 	Name:    "sortByFunc",
	// 	Value:   CallExAdapter(builtinSortByFuncFunc),
	// 	ValueEx: builtinSortByFuncFunc,
	// },
	// char add end
	BuiltinAppend: &BuiltinFunction{
		Name:    "append",
		Value:   CallExAdapter(builtinAppendFunc),
		ValueEx: builtinAppendFunc,
	},
	BuiltinDelete: &BuiltinFunction{
		Name:    "delete",
		Value:   funcPOsRe(builtinDeleteFunc),
		ValueEx: funcPOsReEx(builtinDeleteFunc),
	},
	BuiltinCopy: &BuiltinFunction{
		Name:    "copy",
		Value:   funcPORO(builtinCopyFunc),
		ValueEx: funcPOROEx(builtinCopyFunc),
	},
	BuiltinRepeat: &BuiltinFunction{
		Name:    "repeat",
		Value:   funcPOiROe(builtinRepeatFunc),
		ValueEx: funcPOiROeEx(builtinRepeatFunc),
	},
	BuiltinContains: &BuiltinFunction{
		Name:    "contains",
		Value:   funcPOOROe(builtinContainsFunc),
		ValueEx: funcPOOROeEx(builtinContainsFunc),
	},
	BuiltinLen: &BuiltinFunction{
		Name:    "len",
		Value:   funcPORO(builtinLenFunc),
		ValueEx: funcPOROEx(builtinLenFunc),
	},
	BuiltinCap: &BuiltinFunction{
		Name:    "cap",
		Value:   funcPORO(builtinCapFunc),
		ValueEx: funcPOROEx(builtinCapFunc),
	},
	BuiltinSort: &BuiltinFunction{
		Name:    "sort",
		Value:   funcPOROe(builtinSortFunc),
		ValueEx: funcPOROeEx(builtinSortFunc),
	},
	BuiltinSortReverse: &BuiltinFunction{
		Name:    "sortReverse",
		Value:   funcPOROe(builtinSortReverseFunc),
		ValueEx: funcPOROeEx(builtinSortReverseFunc),
	},
	BuiltinError: &BuiltinFunction{
		Name:    "error",
		Value:   funcPORO(builtinErrorFunc),
		ValueEx: funcPOROEx(builtinErrorFunc),
	},
	BuiltinTypeName: &BuiltinFunction{
		Name:    "typeName",
		Value:   funcPORO(builtinTypeNameFunc),
		ValueEx: funcPOROEx(builtinTypeNameFunc),
	},
	BuiltinBool: &BuiltinFunction{
		Name:    "bool",
		Value:   funcPORO(builtinBoolFunc),
		ValueEx: funcPOROEx(builtinBoolFunc),
	},
	BuiltinInt: &BuiltinFunction{
		Name:    "int",
		Value:   funcPi64RO(builtinIntFunc),
		ValueEx: funcPi64ROEx(builtinIntFunc),
	},
	BuiltinUint: &BuiltinFunction{
		Name:    "uint",
		Value:   funcPu64RO(builtinUintFunc),
		ValueEx: funcPu64ROEx(builtinUintFunc),
	},
	BuiltinFloat: &BuiltinFunction{
		Name:    "float",
		Value:   funcPf64RO(builtinFloatFunc),
		ValueEx: funcPf64ROEx(builtinFloatFunc),
	},
	BuiltinChar: &BuiltinFunction{
		Name:    "char",
		Value:   funcPOROe(builtinCharFunc),
		ValueEx: funcPOROeEx(builtinCharFunc),
	},
	BuiltinByte: &BuiltinFunction{
		Name:    "byte",
		Value:   CallExAdapter(builtinByteFunc),
		ValueEx: builtinByteFunc,
	},
	BuiltinString: &BuiltinFunction{
		Name:    "string",
		Value:   CallExAdapter(builtinStringFunc),
		ValueEx: builtinStringFunc,
	},
	BuiltinMutableString: &BuiltinFunction{
		Name:    "mutableString",
		Value:   CallExAdapter(builtinMutableStringFunc),
		ValueEx: builtinMutableStringFunc,
	},
	BuiltinBytes: &BuiltinFunction{
		Name:    "bytes",
		Value:   CallExAdapter(builtinBytesFunc),
		ValueEx: builtinBytesFunc,
	},
	BuiltinChars: &BuiltinFunction{
		Name:    "chars",
		Value:   funcPOROe(builtinCharsFunc),
		ValueEx: funcPOROeEx(builtinCharsFunc),
	},
	BuiltinPrintf: &BuiltinFunction{
		Name:    "printf",
		Value:   CallExAdapter(builtinPrintfFunc),
		ValueEx: builtinPrintfFunc,
	},
	BuiltinPrintln: &BuiltinFunction{
		Name:    "println",
		Value:   CallExAdapter(builtinPrintlnFunc),
		ValueEx: builtinPrintlnFunc,
	},
	BuiltinSprintf: &BuiltinFunction{
		Name:    "sprintf",
		Value:   CallExAdapter(builtinSprintfFunc),
		ValueEx: builtinSprintfFunc,
	},
	BuiltinGlobals: &BuiltinFunction{
		Name:    "globals",
		Value:   CallExAdapter(builtinGlobalsFunc),
		ValueEx: builtinGlobalsFunc,
	},
	BuiltinIsError: &BuiltinFunction{
		Name:    "isError",
		Value:   CallExAdapter(builtinIsErrorFunc),
		ValueEx: builtinIsErrorFunc,
	},
	BuiltinIsInt: &BuiltinFunction{
		Name:    "isInt",
		Value:   funcPORO(builtinIsIntFunc),
		ValueEx: funcPOROEx(builtinIsIntFunc),
	},
	BuiltinIsUint: &BuiltinFunction{
		Name:    "isUint",
		Value:   funcPORO(builtinIsUintFunc),
		ValueEx: funcPOROEx(builtinIsUintFunc),
	},
	BuiltinIsFloat: &BuiltinFunction{
		Name:    "isFloat",
		Value:   funcPORO(builtinIsFloatFunc),
		ValueEx: funcPOROEx(builtinIsFloatFunc),
	},
	BuiltinIsChar: &BuiltinFunction{
		Name:    "isChar",
		Value:   funcPORO(builtinIsCharFunc),
		ValueEx: funcPOROEx(builtinIsCharFunc),
	},
	BuiltinIsBool: &BuiltinFunction{
		Name:    "isBool",
		Value:   funcPORO(builtinIsBoolFunc),
		ValueEx: funcPOROEx(builtinIsBoolFunc),
	},
	BuiltinIsString: &BuiltinFunction{
		Name:    "isString",
		Value:   funcPORO(builtinIsStringFunc),
		ValueEx: funcPOROEx(builtinIsStringFunc),
	},
	BuiltinIsBytes: &BuiltinFunction{
		Name:    "isBytes",
		Value:   funcPORO(builtinIsBytesFunc),
		ValueEx: funcPOROEx(builtinIsBytesFunc),
	},
	BuiltinIsMap: &BuiltinFunction{
		Name:    "isMap",
		Value:   funcPORO(builtinIsMapFunc),
		ValueEx: funcPOROEx(builtinIsMapFunc),
	},
	BuiltinIsSyncMap: &BuiltinFunction{
		Name:    "isSyncMap",
		Value:   funcPORO(builtinIsSyncMapFunc),
		ValueEx: funcPOROEx(builtinIsSyncMapFunc),
	},
	BuiltinIsArray: &BuiltinFunction{
		Name:    "isArray",
		Value:   funcPORO(builtinIsArrayFunc),
		ValueEx: funcPOROEx(builtinIsArrayFunc),
	},
	BuiltinIsUndefined: &BuiltinFunction{
		Name:    "isUndefined",
		Value:   funcPORO(builtinIsUndefinedFunc),
		ValueEx: funcPOROEx(builtinIsUndefinedFunc),
	},
	BuiltinIsFunction: &BuiltinFunction{
		Name:    "isFunction",
		Value:   funcPORO(builtinIsFunctionFunc),
		ValueEx: funcPOROEx(builtinIsFunctionFunc),
	},
	BuiltinIsCallable: &BuiltinFunction{
		Name:  "isCallable",
		Value: funcPORO(builtinIsCallableFunc),
		//ValueEx: funcPOROEx(builtinIsCallableFunc),
	},
	BuiltinIsIterable: &BuiltinFunction{
		Name:    "isIterable",
		Value:   funcPORO(builtinIsIterableFunc),
		ValueEx: funcPOROEx(builtinIsIterableFunc),
	},

	BuiltinWrongNumArgumentsError:  ErrWrongNumArguments,
	BuiltinInvalidOperatorError:    ErrInvalidOperator,
	BuiltinIndexOutOfBoundsError:   ErrIndexOutOfBounds,
	BuiltinNotIterableError:        ErrNotIterable,
	BuiltinNotIndexableError:       ErrNotIndexable,
	BuiltinNotIndexAssignableError: ErrNotIndexAssignable,
	BuiltinNotCallableError:        ErrNotCallable,
	BuiltinNotImplementedError:     ErrNotImplemented,
	BuiltinZeroDivisionError:       ErrZeroDivision,
	BuiltinTypeError:               ErrType,
}

func builtinMakeArrayFunc(n int, arg Object) (Object, error) {
	if n <= 0 {
		return arg, nil
	}

	arr, ok := arg.(Array)
	if !ok {
		ret := make(Array, n)
		for i := 1; i < n; i++ {
			ret[i] = Undefined
		}
		ret[0] = arg
		return ret, nil
	}

	length := len(arr)
	if n <= length {
		return arr[:n], nil
	}

	ret := make(Array, n)
	x := copy(ret, arr)
	for i := x; i < n; i++ {
		ret[i] = Undefined
	}
	return ret, nil
}

func builtinAppendFunc(c Call) (Object, error) {
	target, ok := c.shift()
	if !ok {
		return Undefined, ErrWrongNumArguments.NewError("want>=1 got=0")
	}

	switch obj := target.(type) {
	case Array:
		obj = append(obj, c.args...)
		obj = append(obj, c.vargs...)
		return obj, nil
	case Bytes:
		n := 0
		for _, args := range [][]Object{c.args, c.vargs} {
			for _, v := range args {
				n++
				switch vv := v.(type) {
				case Byte:
					obj = append(obj, byte(vv))
				case Int:
					obj = append(obj, byte(vv))
				case Uint:
					obj = append(obj, byte(vv))
				case Char:
					obj = append(obj, byte(vv))
				default:
					return Undefined, NewArgumentTypeError(
						strconv.Itoa(n),
						"int|uint|char",
						vv.TypeName(),
					)
				}
			}
		}
		return obj, nil
	case Chars:
		n := 0
		for _, args := range [][]Object{c.args, c.vargs} {
			for _, v := range args {
				n++
				switch vv := v.(type) {
				case Byte:
					obj = append(obj, rune(vv))
				case Int:
					obj = append(obj, rune(vv))
				case Uint:
					obj = append(obj, rune(vv))
				case Char:
					obj = append(obj, rune(vv))
				default:
					return Undefined, NewArgumentTypeError(
						strconv.Itoa(n),
						"int|uint|char",
						vv.TypeName(),
					)
				}
			}
		}
		return obj, nil
	case *UndefinedType:
		ret := make(Array, 0, c.Len())
		ret = append(ret, c.args...)
		ret = append(ret, c.vargs...)
		return ret, nil
	default:
		return Undefined, NewArgumentTypeError(
			"1st",
			"array",
			obj.TypeName(),
		)
	}
}

func builtinRemoveItemsFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 3 {
		return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
	}

	target := args[0]

	start, ok := args[1].(Int)

	if !ok {
		return Undefined, NewCommonErrorWithPos(c, "invalid type for arg 2: (%T)%v", args[1], args[1])
	}

	end, ok := args[2].(Int)

	if !ok {
		return Undefined, NewCommonErrorWithPos(c, "invalid type for arg 3: (%T)%v", args[1], args[1])
	}

	switch obj := target.(type) {
	case Array:
		startT := int(start)
		endT := int(end)
		lenT := len(obj)
		if startT < 0 || startT >= lenT {
			return Undefined, NewCommonErrorWithPos(c, "start index out of range: %v -> %v", startT, lenT)
		}

		if endT < 0 || endT >= lenT {
			return Undefined, NewCommonErrorWithPos(c, "end index out of range: %v -> %v", endT, lenT)
		}

		rs := make(Array, 0, lenT-(endT+1-startT))

		rs = append(rs, obj[:startT]...)
		rs = append(rs, obj[endT+1:]...)

		return rs, nil
	}

	return Undefined, NewCommonErrorWithPos(c, "unsupported type: (%T)%v", target, target)
}

func builtinAppendListFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
	}

	target := args[0]
	src := args[1]

	switch obj := target.(type) {
	case Array:
		nv1, ok := src.(Array)

		if ok {
			obj = append(obj, nv1...)
		}

		return obj, nil
	case Bytes:
		nv1, ok := src.(Bytes)

		if ok {
			obj = append(obj, nv1...)
		}

		return obj, nil
	case Chars:
		nv1, ok := src.(Chars)

		if ok {
			obj = append(obj, nv1...)
		}

		return obj, nil
	case *UndefinedType:
		nv1, ok := src.(Array)

		if ok {
			ret := make(Array, 0, len(nv1))

			ret = append(ret, nv1...)

			return ret, nil
		}

		return Undefined, NewCommonErrorWithPos(c, "unsupported type: (%T)%v", src, src)
	default:
		return Undefined, NewArgumentTypeError(
			"1st",
			"array",
			obj.TypeName(),
		)
	}
}

func builtinDeleteFunc(arg Object, key string) (err error) {
	if v, ok := arg.(IndexDeleter); ok {
		err = v.IndexDelete(ToStringObject(key))
	} else {
		err = NewArgumentTypeError(
			"1st",
			"map|syncMap|IndexDeleter",
			arg.TypeName(),
		)
	}
	return
}

func builtinCopyFunc(arg Object) Object {
	if v, ok := arg.(Copier); ok {
		return v.Copy()
	}
	return arg
}

func builtinRepeatFunc(arg Object, count int) (ret Object, err error) {
	if count < 0 {
		return nil, NewArgumentTypeError(
			"2nd",
			"non-negative integer",
			"negative integer",
		)
	}

	switch v := arg.(type) {
	case Array:
		out := make(Array, 0, len(v)*count)
		for i := 0; i < count; i++ {
			out = append(out, v...)
		}
		ret = out
	case String:
		ret = ToStringObject(strings.Repeat(v.String(), count))
	case *MutableString:
		ret = ToMutableStringObject(strings.Repeat(v.String(), count))
	case Bytes:
		ret = Bytes(bytes.Repeat(v, count))
	default:
		err = NewArgumentTypeError(
			"1st",
			"array|string|bytes",
			arg.TypeName(),
		)
	}
	return
}

func builtinContainsFunc(arg0, arg1 Object) (Object, error) {
	var ok bool
	switch obj := arg0.(type) {
	case Map:
		_, ok = obj[arg1.String()]
	case *SyncMap:
		_, ok = obj.Get(arg1.String())
	case Array:
		for _, item := range obj {
			if item.Equal(arg1) {
				ok = true
				break
			}
		}
	case String:
		ok = strings.Contains(obj.String(), arg1.String())
	case *MutableString:
		ok = strings.Contains(obj.String(), arg1.String())
	case Bytes:
		switch v := arg1.(type) {
		case Int:
			ok = bytes.Contains(obj, []byte{byte(v)})
		case Uint:
			ok = bytes.Contains(obj, []byte{byte(v)})
		case Char:
			ok = bytes.Contains(obj, []byte{byte(v)})
		case String:
			ok = bytes.Contains(obj, []byte(v.String()))
		case *MutableString:
			ok = bytes.Contains(obj, []byte(v.String()))
		case Bytes:
			ok = bytes.Contains(obj, v)
		default:
			return Undefined, NewArgumentTypeError(
				"2nd",
				"int|uint|string|char|bytes",
				arg1.TypeName(),
			)
		}
	case *UndefinedType:
	default:
		return Undefined, NewArgumentTypeError(
			"1st",
			"map|array|string|bytes",
			arg0.TypeName(),
		)
	}
	return Bool(ok), nil
}

func builtinLenFunc(arg Object) Object {
	var n int
	if v, ok := arg.(LengthGetter); ok {
		n = v.Len()
	}
	return Int(n)
}

func builtinCapFunc(arg Object) Object {
	var n int
	switch v := arg.(type) {
	case Array:
		n = cap(v)
	case Bytes:
		n = cap(v)
	}
	return Int(n)
}

func builtinSortFunc(arg Object) (ret Object, err error) {
	switch obj := arg.(type) {
	case Array:
		sort.Slice(obj, func(i, j int) bool {
			v, e := obj[i].BinaryOp(token.Less, obj[j])
			if e != nil && err == nil {
				err = e
				return false
			}

			if v != nil {
				return !v.IsFalsy()
			}
			return false
		})
		ret = arg
	case String:
		s := []rune(obj.String())
		sort.Slice(s, func(i, j int) bool {
			return s[i] < s[j]
		})
		ret = ToStringObject(s)
	case *MutableString:
		s := []rune(obj.String())
		sort.Slice(s, func(i, j int) bool {
			return s[i] < s[j]
		})
		ret = ToMutableStringObject(s)
	case Bytes:
		sort.Slice(obj, func(i, j int) bool {
			return obj[i] < obj[j]
		})
		ret = arg
	case *UndefinedType:
		ret = Undefined
	default:
		ret = Undefined
		err = NewArgumentTypeError(
			"1st",
			"array|string|bytes",
			arg.TypeName(),
		)
	}
	return
}

func builtinSortReverseFunc(arg Object) (Object, error) {
	switch obj := arg.(type) {
	case Array:
		var err error
		sort.Slice(obj, func(i, j int) bool {
			v, e := obj[j].BinaryOp(token.Less, obj[i])
			if e != nil && err == nil {
				err = e
				return false
			}
			if v != nil {
				return !v.IsFalsy()
			}
			return false
		})

		if err != nil {
			return nil, err
		}
		return obj, nil
	case String:
		s := []rune(obj.String())
		sort.Slice(s, func(i, j int) bool {
			return s[j] < s[i]
		})
		return ToStringObject(s), nil
	case *MutableString:
		s := []rune(obj.String())
		sort.Slice(s, func(i, j int) bool {
			return s[j] < s[i]
		})
		return ToMutableStringObject(s), nil
	case Bytes:
		sort.Slice(obj, func(i, j int) bool {
			return obj[j] < obj[i]
		})
		return obj, nil
	case *UndefinedType:
		return Undefined, nil
	}

	return Undefined, NewArgumentTypeError(
		"1st",
		"array|string|bytes",
		arg.TypeName(),
	)
}

// func builtinSortByFuncFunc(c Call) (ret Object, err error) {
// 	args := c.GetArgs()

// 	arg0 := args[0]

// 	arg1, ok := args[0].(*CompiledFunction)

// 	if !ok {
// 		return NewCommonErrorWithPos(c, "invalid type: (%T)%v", args[0], args[0]), nil
// 	}

// 	switch obj := arg0.(type) {
// 	case Array:
// 		sort.Slice(obj, func(i, j int) bool {
// 			retT, errT := NewInvoker(c.VM(), arg1).Invoke(obj[i], obj[j])

// 			if errT != nil {
// 				return false
// 			}

// 			nv1, ok := retT.(Bool)

// 			if !ok {
// 				return false
// 			}

// 			return bool(nv1)
// 		})

// 		ret = arg0
// 	case String:
// 		s := []rune(obj.String())
// 		sort.Slice(s, func(i, j int) bool {
// 			retT, errT := NewInvoker(c.VM(), arg1).Invoke(ToStringObject(obj.Value[i]), ToStringObject(obj.Value[j]))

// 			if errT != nil {
// 				return false
// 			}

// 			nv1, ok := retT.(Bool)

// 			if !ok {
// 				return false
// 			}

// 			return bool(nv1)
// 		})

// 		ret = ToStringObject(s)
// 	case *MutableString:
// 		s := []rune(obj.String())
// 		sort.Slice(s, func(i, j int) bool {
// 			retT, errT := NewInvoker(c.VM(), arg1).Invoke(ToStringObject(obj.Value[i]), ToStringObject(obj.Value[j]))

// 			if errT != nil {
// 				return false
// 			}

// 			nv1, ok := retT.(Bool)

// 			if !ok {
// 				return false
// 			}

// 			return bool(nv1)
// 		})

// 		ret = ToMutableStringObject(s)
// 	case Bytes:
// 		sort.Slice(obj, func(i, j int) bool {
// 			retT, errT := NewInvoker(c.VM(), arg1).Invoke(ToStringObject(obj[i]), ToStringObject(obj[j]))

// 			if errT != nil {
// 				return false
// 			}

// 			nv1, ok := retT.(Bool)

// 			if !ok {
// 				return false
// 			}

// 			return bool(nv1)
// 		})

// 		ret = arg0
// 	case *UndefinedType:
// 		ret = Undefined
// 	default:
// 		ret = Undefined
// 		err = NewArgumentTypeError(
// 			"1st",
// 			"array|string|bytes",
// 			arg0.TypeName(),
// 		)
// 	}

// 	return
// }

func builtinErrorFunc(arg Object) Object {
	return &Error{Name: "error", Message: arg.String()}
}

func builtinTypeNameFunc(arg Object) Object { return ToStringObject(arg.TypeName()) }

func builtinBoolFunc(arg Object) Object { return Bool(!arg.IsFalsy()) }

func builtinIntFunc(v int64) Object { return Int(v) }

func builtinUintFunc(v uint64) Object { return Uint(v) }

func builtinFloatFunc(v float64) Object { return Float(v) }

func builtinByteFunc(c Call) (Object, error) {
	if c.Len() < 1 {
		return Byte(0), nil
	}

	arg1 := c.Get(0)

	switch nv := arg1.(type) {
	case Byte:
		return nv, nil
	case Int:
		return Byte(nv), nil
	case Uint:
		return Byte(nv), nil
	case Float:
		return Byte(nv), nil
	case String:
		return Byte(tk.ToInt(nv)), nil
	case *MutableString:
		return Byte(tk.ToInt(nv)), nil
	default:
		return NewCommonErrorWithPos(c, "unsupported type: %T", arg1), nil
	}
}

func builtinCharFunc(arg Object) (Object, error) {
	v, ok := ToChar(arg)
	if ok && v != utf8.RuneError {
		return v, nil
	}
	if v == utf8.RuneError || arg == Undefined {
		return Undefined, nil
	}
	return Undefined, NewArgumentTypeError(
		"1st",
		"numeric|string|bool",
		arg.TypeName(),
	)
}

func builtinStringFunc(c Call) (Object, error) {
	args := c.GetArgs()

	lenT := len(args)
	if lenT < 1 {
		return String{Value: ""}, nil
	}

	if lenT < 2 {
		return String{Value: args[0].String()}, nil
	}

	addLenT := (lenT - 1) / 2

	mbT := make(map[string]Object)

	for i := 0; i < addLenT; i++ {
		mbT[args[1+i*2].String()] = args[1+i*2+1]
	}

	if (lenT - 1) > (addLenT * 2) {
		mbT[args[1+addLenT*2].String()] = ToStringObject("")
	}

	s1 := String{Value: args[0].String(), Members: mbT}

	return s1, nil
}

func builtinMutableStringFunc(c Call) (Object, error) {
	args := c.GetArgs()

	lenT := len(args)
	if lenT < 1 {
		return &MutableString{Value: ""}, nil
	}

	if lenT < 2 {
		return &MutableString{Value: args[0].String()}, nil
	}

	addLenT := (lenT - 1) / 2

	mbT := make(map[string]Object)

	for i := 0; i < addLenT; i++ {
		mbT[args[1+i*2].String()] = args[1+i*2+1]
	}

	if (lenT - 1) > (addLenT * 2) {
		mbT[args[1+addLenT*2].String()] = ToStringObject("")
	}

	s1 := &MutableString{Value: args[0].String(), Members: mbT}

	return s1, nil
}

// func builtinMutableStringFunc(arg Object) Object { return ToMutableStringObject(arg.String()) }

func builtinBytesFunc(c Call) (Object, error) {
	size := c.Len()

	switch size {
	case 0:
		return Bytes{}, nil
	case 1:
		if v, ok := ToBytes(c.Get(0)); ok {
			return v, nil
		}
	}

	out := make(Bytes, 0, size)
	for _, args := range [][]Object{c.args, c.vargs} {
		for i, obj := range args {
			switch v := obj.(type) {
			case Int:
				out = append(out, byte(v))
			case Uint:
				out = append(out, byte(v))
			case Char:
				out = append(out, byte(v))
			default:
				return Undefined, NewArgumentTypeError(
					strconv.Itoa(i+1),
					"int|uint|char",
					args[i].TypeName(),
				)
			}
		}
	}
	return out, nil
}

func builtinCharsFunc(arg Object) (ret Object, err error) {
	switch obj := arg.(type) {
	case String:
		s := obj.Value
		ret = make(Array, 0, utf8.RuneCountInString(s))
		sz := len(obj.Value)
		i := 0

		for i < sz {
			r, w := utf8.DecodeRuneInString(s[i:])
			if r == utf8.RuneError {
				return Undefined, nil
			}
			ret = append(ret.(Array), Char(r))
			i += w
		}
	case *MutableString:
		s := obj.Value
		ret = make(Array, 0, utf8.RuneCountInString(s))
		sz := len(obj.Value)
		i := 0

		for i < sz {
			r, w := utf8.DecodeRuneInString(s[i:])
			if r == utf8.RuneError {
				return Undefined, nil
			}
			ret = append(ret.(Array), Char(r))
			i += w
		}
	case Bytes:
		ret = make(Array, 0, utf8.RuneCount(obj))
		sz := len(obj)
		i := 0

		for i < sz {
			r, w := utf8.DecodeRune(obj[i:])
			if r == utf8.RuneError {
				return Undefined, nil
			}
			ret = append(ret.(Array), Char(r))
			i += w
		}
	default:
		ret = Undefined
		err = NewArgumentTypeError(
			"1st",
			"string|bytes",
			arg.TypeName(),
		)
	}
	return
}

func builtinPrintfFunc(c Call) (ret Object, err error) {
	ret = Undefined
	switch size := c.Len(); size {
	case 0:
		err = ErrWrongNumArguments.NewError("want>=1 got=0")
	case 1:
		_, err = fmt.Fprint(PrintWriter, c.Get(0).String())
	default:
		format, _ := c.shift()
		vargs := make([]interface{}, 0, size-1)
		for i := 0; i < size-1; i++ {
			vargs = append(vargs, c.Get(i))
		}
		_, err = fmt.Fprintf(PrintWriter, format.String(), vargs...)
	}
	return
}

// char add start
func builtinSystemCmdFunc(c Call) (ret Object, err error) {
	ret = Undefined
	switch size := c.Len(); size {
	case 0:
		_, err = fmt.Fprintln(PrintWriter)
	case 1:
		_, err = fmt.Fprintln(PrintWriter, c.Get(0))
	default:
		vargs := make([]interface{}, 0, size)
		for i := 0; i < size; i++ {
			vargs = append(vargs, c.Get(i))
		}
		_, err = fmt.Fprintln(PrintWriter, vargs...)
	}
	return
}

// char add end

func builtinPrintlnFunc(c Call) (ret Object, err error) {
	ret = Undefined
	switch size := c.Len(); size {
	case 0:
		_, err = fmt.Fprintln(PrintWriter)
	case 1:
		_, err = fmt.Fprintln(PrintWriter, c.Get(0))
	default:
		vargs := make([]interface{}, 0, size)
		for i := 0; i < size; i++ {
			vargs = append(vargs, c.Get(i))
		}
		_, err = fmt.Fprintln(PrintWriter, vargs...)
	}
	return
}

func builtinSprintfFunc(c Call) (ret Object, err error) {
	ret = Undefined
	switch size := c.Len(); size {
	case 0:
		err = ErrWrongNumArguments.NewError("want>=1 got=0")
	case 1:
		ret = ToStringObject(c.Get(0).String())
	default:
		format, _ := c.shift()
		vargs := make([]interface{}, 0, size-1)
		for i := 0; i < size-1; i++ {
			vargs = append(vargs, c.Get(i))
		}
		ret = ToStringObject(fmt.Sprintf(format.String(), vargs...))
	}
	return
}

func builtinGlobalsFunc(c Call) (Object, error) {
	return c.VM().GetGlobals(), nil
}

func builtinIsErrorFunc(c Call) (ret Object, err error) {
	ret = False
	switch c.Len() {
	case 1:
		// We have Error, BuiltinError and also user defined error types.
		if _, ok := c.Get(0).(error); ok {
			ret = True
		}
	case 2:
		if err, ok := c.Get(0).(error); ok {
			if target, ok := c.Get(1).(error); ok {
				ret = Bool(errors.Is(err, target))
			}
		}
	default:
		err = ErrWrongNumArguments.NewError(
			"want=1..2 got=", strconv.Itoa(c.Len()))
	}
	return
}

func builtinIsIntFunc(arg Object) Object {
	_, ok := arg.(Int)
	return Bool(ok)
}

func builtinIsUintFunc(arg Object) Object {
	_, ok := arg.(Uint)
	return Bool(ok)
}

func builtinIsFloatFunc(arg Object) Object {
	_, ok := arg.(Float)
	return Bool(ok)
}

func builtinIsCharFunc(arg Object) Object {
	_, ok := arg.(Char)
	return Bool(ok)
}

func builtinIsBoolFunc(arg Object) Object {
	_, ok := arg.(Bool)
	return Bool(ok)
}

func builtinIsStringFunc(arg Object) Object {
	_, ok := arg.(String)
	return Bool(ok)
}

func builtinIsBytesFunc(arg Object) Object {
	_, ok := arg.(Bytes)
	return Bool(ok)
}

func builtinIsMapFunc(arg Object) Object {
	_, ok := arg.(Map)
	return Bool(ok)
}

func builtinIsSyncMapFunc(arg Object) Object {
	_, ok := arg.(*SyncMap)
	return Bool(ok)
}

func builtinIsArrayFunc(arg Object) Object {
	_, ok := arg.(Array)
	return Bool(ok)
}

func builtinIsUndefinedFunc(arg Object) Object {
	_, ok := arg.(*UndefinedType)
	return Bool(ok)
}

func builtinIsFunctionFunc(arg Object) Object {
	_, ok := arg.(*CompiledFunction)
	if ok {
		return True
	}

	_, ok = arg.(*BuiltinFunction)
	if ok {
		return True
	}

	_, ok = arg.(*Function)
	return Bool(ok)
}

func builtinIsCallableFunc(arg Object) Object { return Bool(arg.CanCall()) }

func builtinIsIterableFunc(arg Object) Object { return Bool(arg.CanIterate()) }

func CallExAdapter(fn CallableExFunc) CallableFunc {
	// tk.Pl("CallExAdapter: %v", fn)
	return func(args ...Object) (Object, error) {
		return fn(Call{args: args})
	}
}

// char add start
func toArgsA(offset int, c Call) []interface{} {
	size := c.Len()
	vargs := make([]interface{}, 0, size-offset)
	for i := offset; i < size; i++ {
		vargs = append(vargs, c.Get(i))
	}
	return vargs
}

func toArgsS(offset int, c Call) []string {
	size := c.Len()
	vargs := make([]string, 0, size-offset)
	for i := offset; i < size; i++ {
		vargs = append(vargs, c.Get(i).String())
	}
	return vargs
}

// func converters

// like tk.GetOSName
func fnARS(fn func() string) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		rs := fn()
		return ToStringObject(rs), nil
	}
}

func fnARSex(fn func() string) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		rs := fn()
		return ToStringObject(rs), nil
	}
}

// like tk.GetErrStrX
func fnAARS(fn func(interface{}) string) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(ConvertFromObject(args[0]))
		return ToStringObject(rs), nil
	}
}

func fnAARSex(fn func(interface{}) string) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		if c.Len() < 1 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(ConvertFromObject(c.Get(0)))
		return ToStringObject(rs), nil
	}
}

// / like os.GetEnv
func fnASRS(fn func(string) string) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(args[0].String())
		return ToStringObject(rs), nil
	}
}

func fnASRSex(fn func(string) string) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		if c.Len() < 1 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(c.Get(0).String())
		return ToStringObject(rs), nil
	}
}

// / like tk.IfFileExists
func fnASRB(fn func(string) bool) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(args[0].String())
		return Bool(rs), nil
	}
}

func fnASRBex(fn func(string) bool) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		if c.Len() < 1 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(c.Get(0).String())
		return Bool(rs), nil
	}
}

// like tk.SetClipText
func fnASRE(fn func(string) error) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(args[0].String())
		return ConvertToObject(rs), nil
	}
}

func fnASREex(fn func(string) error) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		if c.Len() < 1 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(c.Get(0).String())
		return ConvertToObject(rs), nil
	}
}

// like tk.RegFindFirstX
func fnASSIRS(fn func(string, string, int) string) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 3 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(args[0].String(), args[1].String(), tk.ToInt(ConvertFromObject(args[2])))
		return ToStringObject(rs), nil
	}
}

func fnASSIRSex(fn func(string, string, int) string) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		if c.Len() < 3 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(c.Get(0).String(), c.Get(1).String(), tk.ToInt(ConvertFromObject(c.Get(2))))
		return ToStringObject(rs), nil
	}
}

// like tk.SplitLines
func fnASRLs(fn func(string) []string) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(args[0].String())
		return ConvertToObject(rs), nil
	}
}

func fnASRLsex(fn func(string) []string) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		if c.Len() < 1 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(c.Get(0).String())
		return ConvertToObject(rs), nil
	}
}

// like tk.RegFindFirstGroupsX
func fnASSRLs(fn func(string, string) []string) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 2 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(args[0].String(), args[1].String())
		return ConvertToObject(rs), nil
	}
}

func fnASSRLsex(fn func(string, string) []string) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		if c.Len() < 2 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(c.Get(0).String(), c.Get(1).String())
		return ConvertToObject(rs), nil
	}
}

// like tk.RegFindAllX
func fnASSIRLs(fn func(string, string, int) []string) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 3 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(args[0].String(), args[1].String(), tk.ToInt(ConvertFromObject(args[2])))
		return ConvertToObject(rs), nil
	}
}

func fnASSIRLsex(fn func(string, string, int) []string) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		if c.Len() < 3 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(c.Get(0).String(), c.Get(1).String(), tk.ToInt(ConvertFromObject(c.Get(2))))
		return ConvertToObject(rs), nil
	}
}

// // like tk.LoadStringFromFile
// func fnASRSe(fn func(string) string) CallableFunc {
// 	return func(args ...Object) (ret Object, err error) {
// 		if len(args) < 1 {
// 			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
// 		}

// 		rs := fn(args[0].String())
// 		return ToStringObject(rs), nil
// 	}
// }

// func fnASRSeex(fn func(string) string) CallableExFunc {
// 	return func(c Call) (ret Object, err error) {
// 		if c.Len() < 1 {
// 			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
// 		}

// 		rs := fn(c.Get(0).String())
// 		return ToStringObject(rs), nil
// 	}
// }

// like tk.SaveStringToFile
func fnASSRS(fn func(string, string) string) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 2 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(args[0].String(), args[1].String())
		return ToStringObject(rs), nil
	}
}

func fnASSRSex(fn func(string, string) string) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		if c.Len() < 2 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(c.Get(0).String(), c.Get(1).String())
		return ToStringObject(rs), nil
	}
}

// like tk.RegReplaceX
func fnASSSRS(fn func(string, string, string) string) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 3 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(args[0].String(), args[1].String(), args[2].String())
		return ToStringObject(rs), nil
	}
}

func fnASSSRSex(fn func(string, string, string) string) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		if c.Len() < 3 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(c.Get(0).String(), c.Get(1).String(), c.Get(2).String())
		return ToStringObject(rs), nil
	}
}

// like tk.FindFirstDiffPosInStrs
func fnASSRI(fn func(string, string) int) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 2 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(args[0].String(), args[1].String())
		return ToIntObject(rs), nil
	}
}

func fnASSRIex(fn func(string, string) int) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		if c.Len() < 2 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(c.Get(0).String(), c.Get(1).String())
		return ToIntObject(rs), nil
	}
}

// like os.SetEnv
func fnASSRE(fn func(string, string) error) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 2 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(args[0].String(), args[1].String())
		return ConvertToObject(rs), nil
	}
}

func fnASSREex(fn func(string, string) error) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()

		if len(args) < 2 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(args[0].String(), args[1].String())
		return ConvertToObject(rs), nil
	}
}

// like filepath.Join
func fnAVsRS(fn func(...string) string) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		rs := fn(ObjectsToS(args)...)
		return ToStringObject(rs), nil
	}
}

func fnAVsRSex(fn func(...string) string) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()

		rs := fn(ObjectsToS(args)...)
		return ToStringObject(rs), nil
	}
}

// like tk.GetSeq
func fnARIex(fn func() int) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		rs := fn()
		return ToIntObject(rs), nil
	}
}

// like tk.SystemCmd
func fnASVsRS(fn func(string, ...string) string) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return NewCommonError("not enough parameters"), nil
			// return Undefined, ErrWrongNumArguments.NewError(
			// 	"want>=1 got=" + strconv.Itoa(len(args)))
		}

		vargs := ObjectsToS(args[1:])
		rs := fn(args[0].String(), vargs...)
		return ToStringObject(rs), nil
	}
}

func fnASVsRSex(fn func(string, ...string) string) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		if c.Len() < 1 {
			return NewCommonErrorWithPos(c, "not enough parameters"), nil
			// return Undefined, ErrWrongNumArguments.NewError(
			// 	"want>=1 got=" + strconv.Itoa(c.Len()))
		}
		vargs := toArgsS(1, c)
		rs := fn(c.Get(0).String(), vargs...)
		return ToStringObject(rs), nil
	}
}

// like tk.ErrStrf
func fnASVaRS(fn func(string, ...interface{}) string) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return NewCommonError("not enough parameters"), nil
			// return Undefined, ErrWrongNumArguments.NewError(
			// 	"want>=1 got=" + strconv.Itoa(len(args)))
		}

		vargs := ObjectsToI(args[1:])
		rs := fn(args[0].String(), vargs...)
		return ToStringObject(rs), nil
	}
}

func fnASVaRSex(fn func(string, ...interface{}) string) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		if c.Len() < 1 {
			return NewCommonErrorWithPos(c, "not enough parameters"), nil
			// return Undefined, ErrWrongNumArguments.NewError(
			// 	"want>=1 got=" + strconv.Itoa(c.Len()))
		}
		vargs := toArgsA(1, c)
		rs := fn(c.Get(0).String(), vargs...)
		return ToStringObject(rs), nil
	}
}

// like sqltk.ExecDBX
func fnADSVaRA(fn func(*sql.DB, string, ...interface{}) interface{}) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 2 {
			return NewCommonError("not enough parameters"), nil
			// return Undefined, ErrWrongNumArguments.NewError(
			// 	"want>=1 got=" + strconv.Itoa(len(args)))
		}

		nv1, ok := args[0].(*Database)

		if !ok {
			return NewCommonError("invalid parameter type: (%T)%v", args[0], args[0]), nil
		}

		vargs := ObjectsToI(args[2:])
		rs := fn(nv1.Value, args[1].String(), vargs...)
		return ConvertToObject(rs), nil
	}
}

func fnADSVaRAex(fn func(*sql.DB, string, ...interface{}) interface{}) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()

		if len(args) < 2 {
			return NewCommonErrorWithPos(c, "not enough parameters"), nil
			// return Undefined, ErrWrongNumArguments.NewError(
			// 	"want>=1 got=" + strconv.Itoa(c.Len()))
		}

		nv1, ok := args[0].(*Database)

		if !ok {
			return NewCommonError("invalid parameter type: (%T)%v", args[0], args[0]), nil
		}

		vargs := ObjectsToI(args[2:])
		rs := fn(nv1.Value, args[1].String(), vargs...)
		return ConvertToObject(rs), nil
	}
}

// like sqltk.QueryDBMapX
func fnADSSVaRA(fn func(*sql.DB, string, string, ...interface{}) interface{}) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 3 {
			return NewCommonError("not enough parameters"), nil
			// return Undefined, ErrWrongNumArguments.NewError(
			// 	"want>=1 got=" + strconv.Itoa(len(args)))
		}

		nv1, ok := args[0].(*Database)

		if !ok {
			return NewCommonError("invalid parameter type: (%T)%v", args[0], args[0]), nil
		}

		vargs := ObjectsToI(args[3:])
		rs := fn(nv1.Value, args[1].String(), args[2].String(), vargs...)
		return ConvertToObject(rs), nil
	}
}

func fnADSSVaRAex(fn func(*sql.DB, string, string, ...interface{}) interface{}) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()

		if len(args) < 3 {
			return NewCommonErrorWithPos(c, "not enough parameters"), nil
			// return Undefined, ErrWrongNumArguments.NewError(
			// 	"want>=1 got=" + strconv.Itoa(c.Len()))
		}

		nv1, ok := args[0].(*Database)

		if !ok {
			return NewCommonError("invalid parameter type: (%T)%v", args[0], args[0]), nil
		}

		vargs := ObjectsToI(args[3:])
		rs := fn(nv1.Value, args[1].String(), args[2].String(), vargs...)
		return ConvertToObject(rs), nil
	}
}

// like tk.GetWeb
func fnASVaRA(fn func(string, ...interface{}) interface{}) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return NewCommonError("not enough parameters"), nil
		}
		vargs := ObjectsToI(args[1:])
		rs := fn(args[0].String(), vargs...)
		return ConvertToObject(rs), nil
	}
}

func fnASVaRAex(fn func(string, ...interface{}) interface{}) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		if c.Len() < 1 {
			return NewCommonErrorWithPos(c, "not enough parameters"), nil
		}
		vargs := toArgsA(1, c)
		rs := fn(c.Get(0).String(), vargs...)
		return ConvertToObject(rs), nil
	}
}

// like tk.Pln
func fnAVaR(fn func(...interface{})) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		vargs := ObjectsToI(args)
		fn(vargs...)
		return nil, nil
	}
}

func fnAVaRex(fn func(...interface{})) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		vargs := toArgsA(0, c)
		fn(vargs...)
		return nil, nil
	}
}

// like tk.Pln
func fnASVaR(fn func(string, ...interface{})) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return NewCommonError("not enough parameters"), nil
		}

		vargs := ObjectsToI(args[1:])

		fn(args[0].String(), vargs...)

		return nil, nil
	}
}

func fnASVaRex(fn func(string, ...interface{})) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()

		if len(args) < 1 {
			return NewCommonErrorWithPos(c, "not enough parameters"), nil
		}

		vargs := toArgsA(1, c)

		fn(args[0].String(), vargs...)

		return nil, nil
	}
}

// like fmt.printf
func fnASVaRIEex(fn func(string, ...interface{}) (int, error)) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		if c.Len() < 1 {
			return Undefined, ErrWrongNumArguments.NewError(
				"want>=1 got=" + strconv.Itoa(c.Len()))
		}
		vargs := toArgsA(1, c)
		n, err := fn(c.Get(0).String(), vargs...)
		return Int(n), err
	}
}

// builtin funcs
func builtinTestByTextFunc(c Call) (ret Object, err error) {
	argsA := c.GetArgs()

	lenT := len(argsA)

	if lenT < 2 {
		return nil, fmt.Errorf("not enough parameters")
	}

	v1 := argsA[0]
	v2 := argsA[1]

	var v3 string
	var v4 string

	if lenT > 3 {
		v3 = tk.ToStr(argsA[2])
		v4 = "(" + tk.ToStr(argsA[3]) + ")"
	} else if lenT > 2 {
		v3 = tk.ToStr(argsA[2])
	} else {
		v3 = tk.ToStr(tk.GetSeq())
	}

	nv1, ok := v1.(String)

	if !ok {
		return nil, fmt.Errorf("test %v%v failed(invalid type v1): %#v <-> %#v\n-----\n%v\n-----\n%v", v3, v4, v1, v2, v1, v2)
	}

	nv2, ok := v2.(String)

	if !ok {
		return nil, fmt.Errorf("test %v%v failed(invalid type v2): %#v <-> %#v\n-----\n%v\n-----\n%v", v3, v4, v1, v2, v1, v2)
	}

	if nv1.Value == nv2.Value {
		tk.Pl("test %v%v passed", v3, v4)
	} else {
		return nil, fmt.Errorf("test %v%v failed: (pos: %v) %#v <-> %#v\n-----\n%v\n-----\n%v", v3, v4, tk.FindFirstDiffIndex(nv1.Value, nv2.Value), v1, v2, v1, v2)
	}

	return nil, nil
}

func builtinTestByStartsWithFunc(c Call) (Object, error) {
	argsA := c.GetArgs()
	lenT := len(argsA)

	if lenT < 2 {
		return nil, fmt.Errorf("not enough parameters")
	}

	v1 := argsA[0]
	v2 := argsA[1]

	var v3 string
	var v4 string

	if lenT > 3 {
		v3 = tk.ToStr(argsA[2])
		v4 = "(" + tk.ToStr(argsA[3]) + ")"
	} else if lenT > 2 {
		v3 = tk.ToStr(argsA[2])
	} else {
		v3 = tk.ToStr(tk.GetSeq())
	}

	nv1, ok := v1.(String)

	if !ok {
		return nil, fmt.Errorf("test %v%v failed(invalid type v1): %#v <-> %#v\n-----\n%v\n-----\n%v", v3, v4, v1, v2, v1, v2)
	}

	nv2, ok := v2.(String)

	if !ok {
		return nil, fmt.Errorf("test %v%v failed(invalid type v2): %#v <-> %#v\n-----\n%v\n-----\n%v", v3, v4, v1, v2, v1, v2)
	}

	if strings.HasPrefix(nv1.Value, nv2.Value) {
		tk.Pl("test %v%v passed", v3, v4)
	} else {
		return nil, fmt.Errorf("test %v%v failed: %#v <-> %#v\n-----\n%v\n-----\n%v", v3, v4, v1, v2, v1, v2)
	}

	return nil, nil
}

func builtinTestByEndsWithFunc(c Call) (Object, error) {
	argsA := c.GetArgs()
	lenT := len(argsA)

	if lenT < 2 {
		return nil, fmt.Errorf("not enough parameters")
	}

	v1 := argsA[0]
	v2 := argsA[1]

	var v3 string
	var v4 string

	if lenT > 3 {
		v3 = tk.ToStr(argsA[2])
		v4 = "(" + tk.ToStr(argsA[3]) + ")"
	} else if lenT > 2 {
		v3 = tk.ToStr(argsA[2])
	} else {
		v3 = tk.ToStr(tk.GetSeq())
	}

	nv1, ok := v1.(String)

	if !ok {
		return nil, fmt.Errorf("test %v%v failed(invalid type v1): %#v <-> %#v\n-----\n%v\n-----\n%v", v3, v4, v1, v2, v1, v2)
	}

	nv2, ok := v2.(String)

	if !ok {
		return nil, fmt.Errorf("test %v%v failed(invalid type v2): %#v <-> %#v\n-----\n%v\n-----\n%v", v3, v4, v1, v2, v1, v2)
	}

	if strings.HasSuffix(nv1.Value, nv2.Value) {
		tk.Pl("test %v%v passed", v3, v4)
	} else {
		return nil, fmt.Errorf("test %v%v failed: %#v <-> %#v\n-----\n%v\n-----\n%v", v3, v4, v1, v2, v1, v2)
	}

	return nil, nil
}

func builtinTestByContainsFunc(c Call) (Object, error) {
	argsA := c.GetArgs()
	lenT := len(argsA)

	if lenT < 2 {
		return nil, fmt.Errorf("not enough parameters")
	}

	v1 := argsA[0]
	v2 := argsA[1]

	var v3 string
	var v4 string

	if lenT > 3 {
		v3 = tk.ToStr(argsA[2])
		v4 = "(" + tk.ToStr(argsA[3]) + ")"
	} else if lenT > 2 {
		v3 = tk.ToStr(argsA[2])
	} else {
		v3 = tk.ToStr(tk.GetSeq())
	}

	nv1, ok := v1.(String)

	if !ok {
		return nil, fmt.Errorf("test %v%v failed(invalid type v1): %#v <-> %#v\n-----\n%v\n-----\n%v", v3, v4, v1, v2, v1, v2)
	}

	nv2, ok := v2.(String)

	if !ok {
		return nil, fmt.Errorf("test %v%v failed(invalid type v2): %#v <-> %#v\n-----\n%v\n-----\n%v", v3, v4, v1, v2, v1, v2)
	}

	if strings.Contains(nv1.Value, nv2.Value) {
		tk.Pl("test %v%v passed", v3, v4)
	} else {
		return nil, fmt.Errorf("test %v%v failed: %#v <-> %#v\n-----\n%v\n-----\n%v", v3, v4, v1, v2, v1, v2)
	}

	return nil, nil
}

func builtinTestByRegContainsFunc(c Call) (Object, error) {
	argsA := c.GetArgs()
	lenT := len(argsA)

	if lenT < 2 {
		return nil, fmt.Errorf("not enough parameters")
	}

	v1 := argsA[0]
	v2 := argsA[1]

	var v3 string
	var v4 string

	if lenT > 3 {
		v3 = tk.ToStr(argsA[2])
		v4 = "(" + tk.ToStr(argsA[3]) + ")"
	} else if lenT > 2 {
		v3 = tk.ToStr(argsA[2])
	} else {
		v3 = tk.ToStr(tk.GetSeq())
	}

	nv1, ok := v1.(String)

	if !ok {
		return nil, fmt.Errorf("test %v%v failed(invalid type v1): %#v <-> %#v\n-----\n%v\n-----\n%v", v3, v4, v1, v2, v1, v2)
	}

	nv2, ok := v2.(String)

	if !ok {
		return nil, fmt.Errorf("test %v%v failed(invalid type v2): %#v <-> %#v\n-----\n%v\n-----\n%v", v3, v4, v1, v2, v1, v2)
	}

	if tk.RegContainsX(nv1.Value, nv2.Value) {
		tk.Pl("test %v%v passed", v3, v4)
	} else {
		return nil, fmt.Errorf("test %v%v failed: %#v <-> %#v\n-----\n%v\n-----\n%v", v3, v4, v1, v2, v1, v2)
	}

	return nil, nil
}

func builtinTestByRegFunc(c Call) (Object, error) {
	argsA := c.GetArgs()
	lenT := len(argsA)

	if lenT < 2 {
		return nil, fmt.Errorf("not enough parameters")
	}

	v1 := argsA[0]
	v2 := argsA[1]

	var v3 string
	var v4 string

	if lenT > 3 {
		v3 = tk.ToStr(argsA[2])
		v4 = "(" + tk.ToStr(argsA[3]) + ")"
	} else if lenT > 2 {
		v3 = tk.ToStr(argsA[2])
	} else {
		v3 = tk.ToStr(tk.GetSeq())
	}

	nv1, ok := v1.(String)

	if !ok {
		return nil, fmt.Errorf("test %v%v failed(invalid type v1): %#v <-> %#v\n-----\n%v\n-----\n%v", v3, v4, v1, v2, v1, v2)
	}

	nv2, ok := v2.(String)

	if !ok {
		return nil, fmt.Errorf("test %v%v failed(invalid type v2): %#v <-> %#v\n-----\n%v\n-----\n%v", v3, v4, v1, v2, v1, v2)
	}

	if tk.RegMatchX(nv1.Value, nv2.Value) {
		tk.Pl("test %v%v passed", v3, v4)
	} else {
		return nil, fmt.Errorf("test %v%v failed: %#v <-> %#v\n-----\n%v\n-----\n%v", v3, v4, v1, v2, v1, v2)
	}

	return nil, nil
}

func builtinPassFunc(c Call) (Object, error) {
	return Undefined, nil
}

func builtinPlFunc(c Call) (Object, error) {
	args := c.GetArgs()
	if len(args) < 1 {
		tk.Pln()
		return Undefined, nil
	}

	v, ok := args[0].(String)
	if !ok {
		return Undefined, NewCommonErrorWithPos(c, "required format string")
	}

	tk.Pl(v.String(), ObjectsToI(args[1:])...)

	return Undefined, nil
}

func builtinTypeCodeFunc(c Call) (Object, error) {
	args := c.GetArgs()
	return ToIntObject(args[0].TypeCode()), nil
}

// func builtinTypeNameFunc(c Call) (Object, error) {
// 	args := c.GetArgs()
// 	return ToString(args[0].TypeName()), nil
// }

func builtinGetParamFunc(c Call) (Object, error) {
	argsA := c.GetArgs()

	defaultT := ToStringObject("")
	idxT := 1

	if argsA == nil {
		return defaultT, nil
	}

	if len(argsA) < 1 {
		return defaultT, nil
	}

	if len(argsA) > 1 {
		idxT = tk.StrToInt(argsA[1].String(), 1)
	}

	if len(argsA) > 2 {
		defaultT = ToStringObject(argsA[2].String())
	}

	listT, ok := argsA[0].(Array)

	if !ok {
		return defaultT, nil
	}

	var cnt int
	for _, v := range listT {
		argT := v.String()
		if tk.StartsWith(argT, "-") {
			continue
		}

		if cnt == idxT {
			if _, ok := v.(String); ok {
				if tk.StartsWith(argT, "\"") && tk.EndsWith(argT, "\"") {
					return ToStringObject(argT[1 : len(argT)-1]), nil
				}
			}

			return v, nil
		}

		cnt++

	}

	return defaultT, nil
}

func builtinGetSwitchFunc(c Call) (Object, error) {
	argsA := c.GetArgs()
	defaultT := ToStringObject("")

	if argsA == nil {
		return defaultT, nil
	}

	if len(argsA) < 2 {
		return defaultT, nil
	}

	if len(argsA) > 2 {
		defaultT = ToStringObject(argsA[2].String())
	}

	switchStrT := argsA[1].String()

	tmpStrT := ""

	listT, ok := argsA[0].(Array)

	if !ok {
		return defaultT, nil
	}

	for _, v := range listT {
		argOT, ok := v.(String)
		if !ok {
			continue
		}

		argT := FromStringObject(argOT)
		if tk.StartsWith(argT, switchStrT) {
			tmpStrT = argT[len(switchStrT):]
			if tk.StartsWith(tmpStrT, "\"") && tk.EndsWith(tmpStrT, "\"") {
				return ToStringObject(tmpStrT[1 : len(tmpStrT)-1]), nil
			}

			return ToStringObject(tmpStrT), nil
		}

	}

	return defaultT, nil
}

func builtinIfSwitchExistsFunc(c Call) (Object, error) {
	argsA := c.GetArgs()
	if len(argsA) < 2 {
		return Bool(false), nil
	}

	argListT, ok := argsA[0].(Array)

	if !ok {
		return Bool(false), nil
	}

	listT := ObjectsToS(argListT)

	if listT == nil {
		return Bool(false), nil
	}

	return Bool(tk.IfSwitchExistsWhole(listT, argsA[1].String())), nil

}

func builtinToJSONFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
	}

	cObjT := ConvertFromObject(args[0])

	rsT := tk.ToJSONX(cObjT, ObjectsToS(args[1:])...)

	return ToStringObject(rsT), nil
}

func builtinFromJSONFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
	}

	jsonTextT := args[0].String()

	jObjT := tk.FromJSONWithDefault(jsonTextT, nil)

	cObjT := ConvertToObject(jObjT)

	return cObjT, nil
}

func builtinIsErrXFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
	}

	switch nv := args[0].(type) {
	case *Error, *RuntimeError:
		return True, nil
	case String:
		if strings.HasPrefix(nv.Value, "TXERROR:") {
			return True, nil
		}
	case *MutableString:
		if strings.HasPrefix(nv.Value, "TXERROR:") {
			return True, nil
		}
	case *Any:
		_, ok := nv.Value.(error)

		if ok {
			return True, nil
		}

		s1, ok := nv.Value.(string)

		if ok {
			if strings.HasPrefix(s1, "TXERROR:") {
				return True, nil
			}

			return True, nil
		}

		return False, nil
	}

	return False, nil
}

func builtinIsNilFunc(c Call) (Object, error) {
	if c.Len() < 1 {
		return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
	}

	args1 := c.Get(0)

	if args1 == nil {
		return Bool(true), nil
	}

	nv := ConvertFromObject(args1)

	return Bool(tk.IsNil(nv)), nil
}

func builtinSleepFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
	}

	v := args[0].String()

	f := tk.StrToFloat64WithDefaultValue(v, 0)

	if f <= 0 {
		return Undefined, NewCommonErrorWithPos(c, "invalid time")
	}

	tk.Sleep(f)

	return Undefined, nil
}

func builtinExitFunc(args ...Object) (Object, error) {
	resultT := 0

	if len(args) > 0 {
		resultT = tk.StrToIntWithDefaultValue(args[0].String(), 0)
	}

	os.Exit(resultT)

	return Undefined, nil
}

func builtinTimeFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return &Time{Value: time.Now()}, nil
	}

	switch obj := args[0].(type) {
	case Int:
		return &Time{Value: tk.GetTimeFromUnixTimeStampMid(obj.String())}, nil
	// case Float:
	// 	return DateTime{Value: float64(obj)}, nil
	case *Time:
		return &Time{Value: obj.Value}, nil
	case String:
		rsT := tk.ToTime(obj.Value, ObjectsToI(args[1:])...)

		if tk.IsError(rsT) {
			return Undefined, NewCommonErrorWithPos(c, "failed to convert time: %v", rsT)
		}
		return &Time{Value: rsT.(time.Time)}, nil
	case *MutableString:
		rsT := tk.ToTime(obj.Value, ObjectsToI(args[1:])...)

		if tk.IsError(rsT) {
			return Undefined, NewCommonErrorWithPos(c, "failed to convert time: %v", rsT)
		}
		return &Time{Value: rsT.(time.Time)}, nil
	default:
		return Undefined, NewCommonErrorWithPos(c, "failed to convert time")
	}
}

func builtinCallNamedFuncFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
	}

	str1 := args[0].String()

	fn1, ok := namedFuncMapG[str1]

	if !ok {
		return Undefined, NewCommonErrorWithPos(c, "named func not found")
	}

	rsT := tk.ReflectCallFuncQuick(fn1, ObjectsToI(args[1:]))

	return ConvertToObject(rsT), nil
}

func builtinCallInternalFuncFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
	}

	rsT := tk.ReflectCallFuncQuick(ConvertFromObject(args[0]), ObjectsToI(args[1:]))

	return ConvertToObject(rsT), nil
}

func builtinGetNamedValueFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
	}

	str1 := args[0].String()

	v1, ok := namedValueMapG[str1]

	if !ok {
		return Undefined, NewCommonErrorWithPos(c, "named value not found")
	}

	return ConvertToObject(v1), nil
}

func builtinNewExFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
	}

	rsT := tk.NewObject(ObjectsToI(args)...)

	return ConvertToObject(rsT), nil
}

func builtinCallMethodExFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
	}

	var name1 string = ""

	var objT interface{}

	objT = ConvertFromObject(args[0])

	name1 = args[1].String()

	// map1, ok := methodMapG[str1]

	// if !ok {
	// 	return Undefined, NewCommonErrorWithPos(c, "method not found 1")
	// }

	// map2, ok := map1[name1]

	// if !ok {
	// 	return Undefined, NewCommonErrorWithPos(c, "method not found 2")
	// }

	paramsT := ObjectsToI(args[2:])

	rsT := tk.ReflectCallMethodQuick(objT, name1, paramsT...)

	return ConvertToObject(rsT), nil
}

func builtinGetValueFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
	}

	return args[0].GetValue(), nil
}

func builtinGetMemberFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
	}

	return args[0].GetMember(args[1].String()), nil
}

func builtinSetMemberFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 3 {
		return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
	}

	return WrapError(args[0].SetMember(args[1].String(), args[2])), nil
}

func builtinSetValueFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
	}

	args0, ok := args[0].(ValueSetter)

	if !ok {
		return NewCommonErrorWithPos(c, "unsupported action(set value)"), nil
	}

	return WrapError(args0.SetValue(args[1])), nil
}

func builtinCallMethodFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
	}

	return args[0].CallMethod(args[1].String(), args[2:]...)
}

func builtinRegQuoteFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
	}

	return ToStringObject(tk.RegQuote(args[0].String())), nil
}

func builtinGetClipTextFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
	}

	return ToStringObject(tk.GetClipText()), nil
}

func builtinAnyFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return &Any{Value: nil, OriginalType: fmt.Sprintf("%T", nil), OriginalCode: Undefined.TypeCode()}, nil
	}

	switch obj := args[0].(type) {
	case Bool:
		return &Any{Value: bool(obj), OriginalType: fmt.Sprintf("%T", obj), OriginalCode: obj.TypeCode()}, nil
	case Byte:
		return &Any{Value: byte(obj), OriginalType: fmt.Sprintf("%T", obj), OriginalCode: obj.TypeCode()}, nil
	case Int:
		return &Any{Value: int(obj), OriginalType: fmt.Sprintf("%T", obj), OriginalCode: obj.TypeCode()}, nil
	case Uint:
		return &Any{Value: uint64(obj), OriginalType: fmt.Sprintf("%T", obj), OriginalCode: obj.TypeCode()}, nil
	case Char:
		return &Any{Value: rune(obj), OriginalType: fmt.Sprintf("%T", obj), OriginalCode: obj.TypeCode()}, nil
	case Float:
		return &Any{Value: float64(obj), OriginalType: fmt.Sprintf("%T", obj), OriginalCode: obj.TypeCode()}, nil
	case Bytes:
		return &Any{Value: []byte(obj), OriginalType: fmt.Sprintf("%T", obj), OriginalCode: obj.TypeCode()}, nil
	case Chars:
		return &Any{Value: []rune(obj), OriginalType: fmt.Sprintf("%T", obj), OriginalCode: obj.TypeCode()}, nil
	case String:
		return &Any{Value: obj.Value, OriginalType: fmt.Sprintf("%T", obj.Value), OriginalCode: obj.TypeCode()}, nil
	case *MutableString:
		return &Any{Value: obj.Value, OriginalType: fmt.Sprintf("%T", obj.Value), OriginalCode: obj.TypeCode()}, nil
	case Array:
		return &Any{Value: []Object(obj), OriginalType: fmt.Sprintf("%T", obj), OriginalCode: obj.TypeCode()}, nil
	case Map:
		return &Any{Value: map[string]Object(obj), OriginalType: fmt.Sprintf("%T", obj), OriginalCode: obj.TypeCode()}, nil
	case *StringBuilder:
		return &Any{Value: (*strings.Builder)(obj.Value), OriginalType: fmt.Sprintf("%T", obj.Value), OriginalCode: obj.TypeCode()}, nil
	case *BytesBuffer:
		return &Any{Value: (*bytes.Buffer)(obj.Value), OriginalType: fmt.Sprintf("%T", obj.Value), OriginalCode: obj.TypeCode()}, nil
	case *Seq:
		return &Any{Value: obj.Value, OriginalType: fmt.Sprintf("%T", obj.Value), OriginalCode: obj.TypeCode()}, nil
	case *Mutex:
		return &Any{Value: obj.Value, OriginalType: fmt.Sprintf("%T", obj.Value), OriginalCode: obj.TypeCode()}, nil
	case *Mux:
		return &Any{Value: obj.Value, OriginalType: fmt.Sprintf("%T", obj.Value), OriginalCode: obj.TypeCode()}, nil
	case *Reader:
		return &Any{Value: obj.Value, OriginalType: fmt.Sprintf("%T", obj.Value), OriginalCode: obj.TypeCode()}, nil
	case *HttpReq:
		return &Any{Value: obj.Value, OriginalType: fmt.Sprintf("%T", obj.Value), OriginalCode: obj.TypeCode()}, nil
	case *HttpResp:
		return &Any{Value: obj.Value, OriginalType: fmt.Sprintf("%T", obj.Value), OriginalCode: obj.TypeCode()}, nil
	case *CharCode:
		return &Any{Value: obj.Value, OriginalType: fmt.Sprintf("%T", obj.Value), OriginalCode: obj.TypeCode()}, nil
	case *Gel:
		return &Any{Value: obj.Value, OriginalType: fmt.Sprintf("%T", obj.Value), OriginalCode: obj.TypeCode()}, nil
	case *Any:
		return &Any{Value: obj.Value, OriginalType: fmt.Sprintf("%T", obj.Value), OriginalCode: obj.TypeCode()}, nil
	default:
		return &Any{Value: obj, OriginalType: obj.TypeName(), OriginalCode: obj.TypeCode()}, nil
	}
}

// func builtinNewAnyFunc(args ...Object) (Object, error) {
// 	if len(args) < 1 {
// 		return &Any{Value: nil}, nil
// 	}

// 	var s1s string

// 	s1, ok := args[0].(String)

// 	if !ok {
// 		s1s = args[0].String()
// 	} else {
// 		s1s = s1.Value
// 	}

// 	switch s1s {
// 	case "strings.Builder", "*strings.Builder", "stringBuilder":
// 		return builtinMakeStringBuilderFunc(args[1:]...)
// 		// case "mux":
// 		// 	return Any{
// 		// 		Value:        http.NewServeMux(),
// 		// 		OriginalType: "*http.ServeMux",
// 		// 	}, nil
// 	}

// 	return &Any{Value: nil}, nil
// }

func builtinSetValueByRefFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	// tk.Pl("builtinSetValueByRefFunc: %#v %#v", args[0], args[1])

	switch obj := args[0].(type) {
	case *ObjectRef:
		// tk.Pl("builtinSetValueByRefFunc *ObjectRef: %#v %#v", args[0], args[1])
		obj.Value = &args[1]

		return nil, nil
	case *ObjectPtr:
		// tk.Pl("builtinSetValueByRefFunc *ObjectPtr: %#v %#v", args[0], args[1])
		obj.Value = &args[1]

		return nil, nil
	case *Bool:
		nv1, ok := args[1].(Bool)

		if ok {
			(*obj) = nv1
			return nil, nil
		}

		(*obj) = Bool(tk.ToBool(ConvertFromObject(args[1])))

		return nil, nil
	case Byte:
		return &Any{Value: byte(obj), OriginalType: fmt.Sprintf("%T", obj), OriginalCode: obj.TypeCode()}, nil
	case Int:
		return &Any{Value: int(obj), OriginalType: fmt.Sprintf("%T", obj), OriginalCode: obj.TypeCode()}, nil
	case Uint:
		return &Any{Value: uint64(obj), OriginalType: fmt.Sprintf("%T", obj), OriginalCode: obj.TypeCode()}, nil
	case Char:
		return &Any{Value: rune(obj), OriginalType: fmt.Sprintf("%T", obj), OriginalCode: obj.TypeCode()}, nil
	case Float:
		return &Any{Value: float64(obj), OriginalType: fmt.Sprintf("%T", obj), OriginalCode: obj.TypeCode()}, nil
	case Bytes:
		return &Any{Value: []byte(obj), OriginalType: fmt.Sprintf("%T", obj), OriginalCode: obj.TypeCode()}, nil
	case Chars:
		return &Any{Value: []rune(obj), OriginalType: fmt.Sprintf("%T", obj), OriginalCode: obj.TypeCode()}, nil
	case String:
		return &Any{Value: obj.Value, OriginalType: fmt.Sprintf("%T", obj.Value), OriginalCode: obj.TypeCode()}, nil
	case *MutableString:
		return &Any{Value: obj.Value, OriginalType: fmt.Sprintf("%T", obj.Value), OriginalCode: obj.TypeCode()}, nil
	case Array:
		return &Any{Value: []Object(obj), OriginalType: fmt.Sprintf("%T", obj), OriginalCode: obj.TypeCode()}, nil
	case Map:
		return &Any{Value: map[string]Object(obj), OriginalType: fmt.Sprintf("%T", obj), OriginalCode: obj.TypeCode()}, nil
	case *StringBuilder:
		return &Any{Value: (*strings.Builder)(obj.Value), OriginalType: fmt.Sprintf("%T", obj.Value), OriginalCode: obj.TypeCode()}, nil
	case *Any:
		return &Any{Value: obj.Value, OriginalType: fmt.Sprintf("%T", obj.Value), OriginalCode: obj.TypeCode()}, nil
	default:
		return &Any{Value: obj, OriginalType: obj.TypeName(), OriginalCode: obj.TypeCode()}, nil
	}
}

func builtinUnrefFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return nil, fmt.Errorf("not enough parameters")
	}

	switch obj := args[0].(type) {
	case *ObjectRef:
		// tk.Pl("builtinSetValueByRefFunc *ObjectRef: %#v %#v", args[0], args[1])

		return *(obj.Value), nil
	default:
		return nil, fmt.Errorf("unsupported type: (%T) %v", args[0], args[0])
	}
}

func builtinDumpVarFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return nil, fmt.Errorf("not enough parameters")
	}

	tk.Dump(args[0])

	return nil, nil
}

func builtinDebugInfoFunc(c Call) (Object, error) {
	// args := c.GetArgs()

	// if len(args) < 1 {
	// 	return nil, fmt.Errorf("not enough parameters")
	// }

	bcInfoT := c.VM().GetBytecodeInfo()

	var bufT strings.Builder

	bufT.WriteString("\n ----- \n")
	bufT.WriteString(bcInfoT)
	bufT.WriteString("\n ----- \n")
	bufT.WriteString("Globals \n")
	bufT.WriteString(fmt.Sprintf("%v", c.VM().GetGlobals()))
	bufT.WriteString("\n ----- \n")
	bufT.WriteString("CurFunc \n")
	bufT.WriteString(fmt.Sprintf("%#v", c.VM().GetCurFunc()))
	bufT.WriteString("\n ----- \n")
	bufT.WriteString("CurInstr \n")
	bufT.WriteString(fmt.Sprintf("%#v", c.VM().GetCurInstr()))
	bufT.WriteString("\n ----- \n")
	bufT.WriteString("Locals \n")
	bufT.WriteString(fmt.Sprintf("%v", c.VM().GetLocalsQuick()))
	bufT.WriteString("\n ----- \n")

	return ToStringObject(bufT), nil
}

// func builtinMakeStringBuilderFunc(args ...Object) (Object, error) {
// 	return &Any{
// 		Value:        new(strings.Builder),
// 		OriginalType: "StringBuilder",
// 	}, nil
// }

func builtinToStrFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return ToStringObject(""), nil
	}

	rsT := tk.ToStr(ConvertFromObject(args[0]))

	return ToStringObject(rsT), nil
}

func builtinTypeOfAnyFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return nil, fmt.Errorf("not enough parameters")
	}

	any1, ok := args[0].(*Any)

	if !ok {
		return nil, fmt.Errorf("not any type")
	}

	rsT := fmt.Sprintf("%T", any1.Value)

	return ToStringObject(rsT), nil
}

func builtinPltFunc(c Call) (Object, error) {
	args := c.GetArgs()

	lenT := len(args)

	if lenT < 1 {
		n, e := fmt.Println()
		return Int(n), e
	}

	if lenT == 1 {
		n, e := fmt.Printf("(%v)%v\n", args[0].TypeName(), args[0])
		return Int(n), e
	}

	countT := 0
	for i, v := range args {
		n, e := fmt.Printf("[%v] (%v)%v\n", i, v.TypeCode(), v)

		countT += n

		if e != nil {
			return Int(countT), e
		}

	}

	return Int(countT), nil
}

// func builtinToTimeFunc(args ...Object) (Object, error) {
// 	if len(args) < 1 {
// 		return &Time{Value: time.Now()}, nil
// 	}

// 	switch obj := args[0].(type) {
// 	case Int:
// 		return &Time{Value: tk.GetTimeFromUnixTimeStampMid(obj.String())}, nil
// 	// case Float:
// 	// 	return DateTime{Value: float64(obj)}, nil
// 	case Time:
// 		return &Time{Value: obj.Value}, nil
// 	case String:
// 		rsT := tk.ToTime(obj)

// 		if tk.IsError(rsT) {
// 			return Undefined, NewCommonErrorWithPos(c, "failed to convert time")
// 		}
// 		return &Time{Value: rsT.(time.Time)}, nil
// 	default:
// 		return Undefined, NewCommonErrorWithPos(c, "failed to convert time")
// 	}
// }

func builtinGetEnvFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return nil, fmt.Errorf("not enough parameters")
	}

	rsT := os.Getenv(args[0].String())

	return ToStringObject(rsT), nil
}

func builtinCheckErrXFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return nil, fmt.Errorf("not enough parameters")
	}

	objT := ConvertFromObject(args[0])

	if tk.IsErrX(objT) {
		fmt.Printf("Error: %v\n", tk.GetErrStrX(objT))
		os.Exit(0)
	}

	return args[0], nil
}

func builtinTrimFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return nil, fmt.Errorf("not enough parameters")
	}

	arg0 := args[0]

	_, ok := arg0.(*UndefinedType)
	if ok {
		return ToStringObject(""), nil
	}

	return ToStringObject(tk.Trim(arg0.String(), ObjectsToS(args[1:])...)), nil
}

func builtinStrTrimFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return nil, fmt.Errorf("not enough parameters")
	}

	arg0 := args[0]

	_, ok := arg0.(String)
	if !ok {
		return nil, fmt.Errorf("invalid type: %v", arg0.TypeName())
	}

	return ToStringObject(tk.Trim(arg0.String(), ObjectsToS(args[1:])...)), nil
}

func builtinStrTrimStartFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return nil, fmt.Errorf("not enough parameters")
	}

	arg0 := args[0]

	s1, ok := arg0.(String)
	if !ok {
		return nil, fmt.Errorf("invalid type: %v", arg0.TypeName())
	}

	arg1 := args[1]

	s2, ok := arg1.(String)
	if !ok {
		return nil, fmt.Errorf("invalid type: %v", arg0.TypeName())
	}

	return ToStringObject(strings.TrimPrefix(s1.Value, s2.Value)), nil
}

func builtinStrTrimEndFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return nil, fmt.Errorf("not enough parameters")
	}

	s1, ok := args[0].(String)
	if !ok {
		return nil, fmt.Errorf("invalid type of arg 1: %v", args[0].TypeName())
	}

	s2, ok := args[1].(String)
	if !ok {
		return nil, fmt.Errorf("invalid type of arg 2: %v", args[1].TypeName())
	}

	return ToStringObject(strings.TrimSuffix(s1.Value, s2.Value)), nil
}

func builtinStrTrimLeftFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return nil, fmt.Errorf("not enough parameters")
	}

	arg0 := args[0]

	s1, ok := arg0.(String)
	if !ok {
		return nil, fmt.Errorf("invalid type: %v", arg0.TypeName())
	}

	arg1 := args[1]

	s2, ok := arg1.(String)
	if !ok {
		return nil, fmt.Errorf("invalid type: %v", arg0.TypeName())
	}

	return ToStringObject(strings.TrimLeft(s1.Value, s2.Value)), nil
}

func builtinStrTrimRightFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return nil, fmt.Errorf("not enough parameters")
	}

	arg0 := args[0]

	s1, ok := arg0.(String)
	if !ok {
		return nil, fmt.Errorf("invalid type: %v", arg0.TypeName())
	}

	arg1 := args[1]

	s2, ok := arg1.(String)
	if !ok {
		return nil, fmt.Errorf("invalid type: %v", arg0.TypeName())
	}

	return ToStringObject(strings.TrimRight(s1.Value, s2.Value)), nil
}

func builtinArchiveFilesToZipFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return nil, fmt.Errorf("not enough parameters")
	}

	arg0 := args[0]

	nv1, ok := arg0.(String)
	if !ok {
		return nil, fmt.Errorf("invalid type: %v", arg0.TypeName())
	}

	vs := ObjectsToI(args[1:])

	fileNamesT := make([]string, 0, len(vs))
	args1T := make([]string, 0, len(vs))

	for _, vi1 := range vs {
		nvs, ok := vi1.(string)

		if ok {
			if !strings.HasPrefix(nvs, "-") {
				fileNamesT = append(fileNamesT, nvs)
			} else {
				args1T = append(args1T, nvs)
			}

			continue
		}

		nvsa, ok := vi1.([]string)
		if ok {
			for _, vj1 := range nvsa {
				fileNamesT = append(fileNamesT, vj1)
			}

			continue
		}

		nvsi, ok := vi1.([]interface{})
		if ok {
			for _, vj1 := range nvsi {
				fileNamesT = append(fileNamesT, tk.ToStr(vj1))
			}

			continue
		}

	}

	z := &archiver.Zip{
		CompressionLevel:  flate.DefaultCompression,
		OverwriteExisting: tk.IfSwitchExistsWhole(args1T, "-overwrite"),
		MkdirAll:          tk.IfSwitchExistsWhole(args1T, "-makeDirs"),
		// SelectiveCompression:   true,
		// ImplicitTopLevelFolder: false,
		// ContinueOnError:        false,
		FileMethod: archiver.Deflate,
	}

	errT := z.Archive(fileNamesT, nv1.String())

	return ConvertToObject(errT), nil
}

func builtinSshUploadFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return nil, fmt.Errorf("not enough parameters")
	}

	pa := ObjectsToS(args)

	var v1, v2, v3, v4, v5, v6 string

	v1 = strings.TrimSpace(tk.GetSwitch(pa, "-host=", v1))
	v2 = strings.TrimSpace(tk.GetSwitch(pa, "-port=", v2))
	v3 = strings.TrimSpace(tk.GetSwitch(pa, "-user=", v3))
	v4 = strings.TrimSpace(tk.GetSwitch(pa, "-password=", v4))
	if strings.HasPrefix(v4, "740404") {
		v4 = strings.TrimSpace(tk.DecryptStringByTXDEF(v4))
	}
	v5 = strings.TrimSpace(tk.GetSwitch(pa, "-path=", v5))
	v6 = strings.TrimSpace(tk.GetSwitch(pa, "-remotePath=", v6))

	withProgressT := tk.IfSwitchExistsWhole(pa, "-progress")

	if v1 == "" {
		return ConvertToObject(fmt.Errorf("emtpy host")), nil
	}

	if v2 == "" {
		return ConvertToObject(fmt.Errorf("emtpy port")), nil
	}

	if v3 == "" {
		return ConvertToObject(fmt.Errorf("emtpy user")), nil
	}

	if v4 == "" {
		return ConvertToObject(fmt.Errorf("emtpy password")), nil
	}

	if v5 == "" {
		return ConvertToObject(fmt.Errorf("emtpy path")), nil
	}

	if v6 == "" {
		return ConvertToObject(fmt.Errorf("emtpy remotePath")), nil
	}

	sshT, errT := tk.NewSSHClient(v1, v2, v3, v4)

	if errT != nil {
		return ConvertToObject(errT), nil
	}

	defer sshT.Close()

	if withProgressT {
		fmt.Println()
		errT = sshT.UploadWithProgressFunc(v5, v6, func(i interface{}) interface{} {
			fmt.Printf("\rprogress: %v                ", tk.IntToKMGT(i))
			return ""
		}, pa...)
	} else {
		errT = sshT.Upload(v5, v6, pa...)
	}

	fmt.Println()

	if errT != nil {
		return ConvertToObject(errT), nil
	}

	return ConvertToObject(errT), nil
}

func builtinStringBuilderFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		rs := &StringBuilder{Value: new(strings.Builder)}
		rs.Value.WriteString("")
		return rs, nil
	}

	s := args[0].String()

	rs := &StringBuilder{Value: new(strings.Builder)}

	rs.Value.WriteString(s)

	return rs, nil
}

func builtinStatusResultFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return &StatusResult{Status: "success", Value: ""}, nil
	}

	if len(args) < 2 {
		return &StatusResult{Status: args[0].String(), Value: ""}, nil
	}

	if len(args) < 3 {
		return &StatusResult{Status: args[0].String(), Value: args[1].String()}, nil
	}

	return &StatusResult{Status: args[0].String(), Value: args[1].String(), Objects: tk.ToJSONX(ObjectsToS(args[2:]))}, nil
}

func builtinSeqFunc(c Call) (Object, error) {
	args := c.GetArgs()

	return NewSeq(args...), nil
}

func builtinMutexFunc(c Call) (Object, error) {
	args := c.GetArgs()

	return NewMutex(args...), nil
}

func builtinCharCodeFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
	}

	var compilerOptionsT *CompilerOptions

	vmT := c.VM()

	if vmT != nil {
		compilerOptionsT = vmT.bytecode.CompilerOptionsM
	} else {
		compilerOptionsT = &DefaultCompilerOptions
	}

	return NewCharCode(args[0].String(), compilerOptionsT), nil
}

func builtinGelFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
	}

	return NewGel(args[0])
}

func builtinMuxFunc(c Call) (Object, error) {
	args := c.GetArgs()

	return NewMux(args...), nil
}

func builtinWriteRespFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	v, ok := args[0].(*HttpResp)
	if !ok {
		return NewCommonErrorWithPos(c, "invalid object type: (%T)%v", args[0], args[0]), nil
	}

	var contentT Bytes = nil

	v1, ok := args[1].(String)

	if ok {
		contentT = Bytes(v1.Value)
	}

	if contentT == nil {
		v2, ok := args[1].(Bytes)
		if !ok {
			return NewCommonErrorWithPos(c, "invalid content type: (%T)%v", args[1], args[1]), nil
		}

		contentT = v2
	}

	r, errT := v.Value.Write(contentT)

	if errT != nil {
		return NewCommonErrorWithPos(c, "failed to write response: %v", errT), nil
	}

	return Int(r), errT
}

func builtinParseReqFormFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	nv, ok := args[0].(*HttpReq)
	if !ok {
		return NewCommonErrorWithPos(c, "invalid object type: (%T)%v", args[0], args[0]), nil
	}

	reqT := nv.Value

	reqT.ParseForm()

	paraMapT := tk.FormToMap(reqT.Form)

	return ConvertToObject(paraMapT), nil
}

func builtinParseReqFormExFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	nv, ok := args[0].(*HttpReq)
	if !ok {
		return NewCommonErrorWithPos(c, "invalid object type: (%T)%v", args[0], args[0]), nil
	}

	reqT := nv.Value

	reqT.ParseForm()

	paraMapT := tk.FormToMap(reqT.Form)

	return ConvertToObject(paraMapT), nil
}

func builtinSetRespHeaderFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 3 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	nv1, ok := args[0].(*HttpResp)
	if !ok {
		return NewCommonErrorWithPos(c, "invalid object type: (%T)%v", args[0], args[0]), nil
	}

	reqT := nv1.Value

	reqT.Header().Set(args[1].String(), args[2].String())

	return Undefined, nil
}

func builtinGetReqHeaderFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		rs := NewCommonErrorWithPos(c, "not enough parameters")
		return rs, nil
	}

	nv1, ok := args[0].(*HttpReq)
	if !ok {
		return NewCommonErrorWithPos(c, "invalid object type: (%T)%v", args[0], args[0]), nil
	}

	reqT := nv1.Value

	return ToStringObject(reqT.Header.Get(args[1].String())), nil
}

func builtinGetReqBodyFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		rs := NewCommonErrorWithPos(c, "not enough parameters")
		return rs, nil
	}

	nv1, ok := args[0].(*HttpReq)
	if !ok {
		return NewCommonErrorWithPos(c, "invalid object type: (%T)%v", args[0], args[0]), nil
	}

	rsT, errT := io.ReadAll(nv1.Value.Body)

	if errT != nil {
		return NewCommonErrorWithPos(c, "failed to read body content: %v", errT), nil
	}

	return Bytes(rsT), nil
}

func builtinNewFunc(c Call) (Object, error) {
	// args := c.GetArgs()

	// if len(args) < 1 {
	// 	return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
	// }

	// s1 := args[0].String()

	// tk.Pln("builtinNewFunc:", s1, args)

	var tmpv Object

	tmpv, errT := builtinMakeFunc(c)

	if errT != nil {
		return Undefined, errT
	}

	return &ObjectRef{Value: &tmpv}, nil

	// switch s1 {
	// case "bool":
	// 	// tk.Pln("builtinNewFunc bool:", s1, args)
	// 	if len(args) > 1 {
	// 		tmpv = Bool(!args[1].IsFalsy())
	// 	} else {
	// 		tmpv = Bool(false)
	// 	}

	// 	return &ObjectRef{Value: &tmpv}, nil
	// case "byte":
	// 	if len(args) > 1 {
	// 		tmpv = Byte(tk.ToInt(args[1].String(), 0))
	// 	} else {
	// 		tmpv = Byte(0)
	// 	}

	// 	return &ObjectRef{Value: &tmpv}, nil
	// case "int":
	// 	if len(args) > 1 {
	// 		return Int(tk.ToInt(args[1].String(), 0)), nil
	// 	} else {
	// 		return Int(0), nil
	// 	}
	// 	return &ObjectRef{Value: &tmpv}, nil
	// case "uint":
	// 	if len(args) > 1 {
	// 		return Uint(tk.ToInt(args[1].String(), 0)), nil
	// 	} else {
	// 		return Uint(0), nil
	// 	}
	// case "char":
	// 	if len(args) > 1 {
	// 		return Char(tk.ToInt(args[1].String(), 0)), nil
	// 	} else {
	// 		return Char(0), nil
	// 	}
	// case "float":
	// 	if len(args) > 1 {
	// 		return Char(tk.ToFloat(args[1].String(), 0.0)), nil
	// 	} else {
	// 		return Char(0), nil
	// 	}
	// case "str", "string":
	// 	if len(args) > 1 {
	// 		return String{Value: args[1].String()}, nil
	// 	} else {
	// 		return String{Value: ""}, nil
	// 	}
	// case "array", "list":
	// 	if len(args) > 1 {
	// 		return make(Array, 0, tk.ToInt(args[1].String(), 0)), nil
	// 	} else {
	// 		return make(Array, 0, 0), nil
	// 	}
	// case "map":
	// 	if len(args) > 1 {
	// 		return make(Map, tk.ToInt(args[1].String(), 0)), nil
	// 	} else {
	// 		return make(Map, 0), nil
	// 	}
	// case "stringBuilder":
	// 	return builtinStringBuilderFunc(Call{args: args[1:]})
	// }

	// return Undefined, NewCommonErrorWithPos(c, "invalid data type: %v", s1)
}

func builtinMakeFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
	}

	s1 := args[0].String()

	switch s1 {
	case "bool":
		// tk.Pln("builtinMakeFunc bool:", s1, args)
		if len(args) > 1 {
			return Bool(!args[1].IsFalsy()), nil
		} else {
			return Bool(false), nil
		}
	case "byte":
		if len(args) > 1 {
			return Byte(tk.ToInt(args[1].String(), 0)), nil
		} else {
			return Byte(0), nil
		}
	case "int":
		if len(args) > 1 {
			return Int(tk.ToInt(args[1].String(), 0)), nil
		} else {
			return Int(0), nil
		}
	case "uint":
		if len(args) > 1 {
			return Uint(tk.ToInt(args[1].String(), 0)), nil
		} else {
			return Uint(0), nil
		}
	case "char":
		if len(args) > 1 {
			return Char(tk.ToInt(args[1].String(), 0)), nil
		} else {
			return Char(0), nil
		}
	case "float":
		if len(args) > 1 {
			return Float(tk.ToFloat(args[1].String(), 0.0)), nil
		} else {
			return Float(0.0), nil
		}
	case "str", "string":
		if len(args) > 1 {
			return String{Value: args[1].String()}, nil
		} else {
			return String{Value: ""}, nil
		}
	case "mutableStr", "mutableString":
		if len(args) > 1 {
			return &MutableString{Value: args[1].String()}, nil
		} else {
			return &MutableString{Value: ""}, nil
		}
	case "array", "list":
		if len(args) > 2 {
			return make(Array, tk.ToInt(args[1].String(), 0), tk.ToInt(args[2].String(), 0)), nil
		} else if len(args) > 1 {
			return make(Array, 0, tk.ToInt(args[1].String(), 0)), nil
		} else {
			return make(Array, 0, 0), nil
		}
	case "map":
		if len(args) > 1 {
			return make(Map, tk.ToInt(args[1].String(), 0)), nil
		} else {
			return make(Map, 0), nil
		}
	case "bytes":
		if len(args) > 1 {
			var bufT Bytes = Bytes(args[1].String())
			buf1 := make(Bytes, len(bufT))

			copy(buf1, bufT)
			return buf1, nil

		} else {
			return make(Bytes, 0), nil
		}
	case "chars":
		if len(args) > 1 {
			var bufT Chars = Chars(args[1].String())
			buf1 := make(Chars, len(bufT))

			copy(buf1, bufT)
			return buf1, nil
		} else {
			return make(Chars, 0), nil
		}
	case "error":
		if len(args) > 1 {
			return NewCommonErrorWithPos(c, "%v", args[1].String()), nil
		} else {
			return NewCommonErrorWithPos(c, ""), nil
		}
	case "syncMap":
		return &SyncMap{Value: make(Map)}, nil
	case "time":
		return builtinTimeFunc(Call{args: args[1:]})
	case "stringBuilder":
		return builtinStringBuilderFunc(Call{args: args[1:]})
	case "any":
		return builtinAnyFunc(Call{args: args[1:]})
	case "ref", "objectRef":
		return &ObjectRef{Value: nil}, nil
	case "statusResult":
		return builtinStatusResultFunc(Call{args: args[1:]})
	case "database":
		return builtinStatusResultFunc(Call{args: args[1:]})
	case "seq":
		return builtinSeqFunc(Call{args: args[1:]})
	case "mutex":
		return builtinMutexFunc(Call{args: args[1:]})
	case "mux":
		return builtinMuxFunc(Call{args: args[1:]})
	case "charCode":
		return builtinCharCodeFunc(Call{args: args[1:]})
	case "gel":
		return builtinGelFunc(Call{args: args[1:]})
	case "undefined":
		return Undefined, nil
	}

	return Undefined, NewCommonErrorWithPos(c, "invalid data type: %v", s1)
}

func builtinWriteStrFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	s1, ok := args[1].(String)

	if !ok {
		s1 = ToStringObject(args[1].String())
	}

	if args[0].TypeName() == "any" {
		vT := args[0].(*Any)
		switch nv := vT.Value.(type) {
		case *strings.Builder:
			n, errT := nv.WriteString(s1.Value)

			if errT != nil {
				return NewCommonErrorWithPos(c, "%v", errT), nil
			}

			return Int(n), nil

		case string:
			s1.Value = nv + s1.Value
			return Int(len(s1.Value)), nil
		case io.StringWriter:
			n, errT := nv.WriteString(s1.Value)

			if errT != nil {
				return Int(n), nil
			}

			return NewCommonErrorWithPos(c, "%v", errT), nil
		default:
			return NewCommonErrorWithPos(c, "invalid type: %T", vT.Value), nil

		}
	} else if args[0].TypeName() == "stringBuilder" {
		vT := args[0].(*StringBuilder)
		n, errT := vT.Value.WriteString(s1.Value)

		if errT != nil {
			return NewCommonErrorWithPos(c, "%v", errT), nil
		}

		return Int(n), nil
	}

	return NewCommonErrorWithPos(c, "%v", "invalid data"), nil
}

func builtinDatabaseFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return NewCommonErrorWithPos(c, "not enough paramters"), nil
	}

	nv0, ok := args[0].(String)

	if !ok {
		return NewCommonErrorWithPos(c, "invalid paramter 1"), nil
	}

	nv1, ok := args[1].(String)

	if !ok {
		return NewCommonErrorWithPos(c, "invalid paramter 2"), nil
	}

	rsT := sqltk.ConnectDBX(nv0.Value, nv1.Value)
	if tk.IsError(rsT) {
		return NewFromError(rsT.(error)), nil
	}

	return &Database{DBType: nv0.Value, DBConnectString: nv1.String(), Value: rsT.(*sql.DB)}, nil
}

// func builtinGetErrStrXFunc(c Call) (Object, error) {
// 	args := c.GetArgs()

// 	if len(args) < 1 {
// 		return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
// 	}

// 	v1, errT := builtinIsErrorFunc(c)

// 	if errT != nil {
// 		return Undefined, errT
// 	}

// 	if v1.(Bool) {
// 		return ToStringObject(v1.String()), nil
// 	}

// 	objT := args[0]

// 	s1, ok := objT.(String)

// 	if ok {
// 		return ToStringObject(tk.GetErrStrX(s1)), nil
// 	}

// 	return tk.GetErrStrX(args[0])

// }

func builtinFatalfFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return Undefined, nil
	}

	v, ok := args[0].(String)
	if !ok {
		return Undefined, NewCommonErrorWithPos(c, "required format string")
	}

	tk.Pl(v.String(), ObjectsToI(args[1:])...)

	tk.Exit(1)

	return Undefined, nil
}

func builtinGenJSONRespFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 3 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	v0, ok := args[0].(*HttpReq)
	if !ok {
		return NewCommonErrorWithPos(c, "invalid param type: %T(%v)", args[0], args[0]), nil
	}

	rsT := tk.GenerateJSONPResponseWithMore(args[1].String(), args[2].String(), v0.Value, ObjectsToS(args[3:])...)

	return ToStringObject(rsT), nil
}

func BuiltinDbCloseFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonError("not enough parameters"), nil
	}

	nv, ok := args[0].(*Database)

	if !ok {
		return NewCommonError("invalid parameter type: (%T)%v", args[0], args[0]), nil
	}

	errT := nv.Value.Close()

	if errT != nil {
		return NewCommonError("failed to close DB: %v", errT), nil
	}

	return Undefined, nil
}

func builtinGetRandomIntFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return Int(tk.MAX_INT), nil
	}

	// ToIntObject()

	// v, ok := args[0].(Int)
	// if !ok {
	// 	return NewCommonErrorWithPos(c, "invalid parameter type: (%T)%v", args[0], args[0]), nil
	// }

	return Int(tk.GetRandomIntLessThan(int(ToIntObject(args[0])))), nil
}

func builtinGetRandomFloatFunc(c Call) (Object, error) {
	if RandomGeneratorG == nil {
		RandomGeneratorG = tk.NewRandomGenerator()

		RandomGeneratorG.Randomize()
	}

	return Float(RandomGeneratorG.Float64()), nil
}

func builtinGetRandomStrFunc(c Call) (Object, error) {
	args := c.GetArgs()

	rs := tk.GenerateRandomStringX(ObjectsToS(args)...)

	return &String{Value: rs}, nil
}

// char add end
