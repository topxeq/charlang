// Copyright (c) 2020 Ozan Hacıbekiroğlu.
// Use of this source code is governed by a MIT License
// that can be found in the LICENSE file.

package charlang

import (
	"bytes"
	"database/sql"
	"errors"
	"fmt"
	"io"
	"net/http"
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
)

// modi by TopXeQ

var (
	// PrintWriter is the default writer for printf and println builtins.
	PrintWriter io.Writer = os.Stdout
)

// BuiltinType represents a builtin type
type BuiltinType int

// Builtins
const (
	BuiltinAppend BuiltinType = iota
	BuiltinDelete
	BuiltinCopy
	BuiltinRepeat
	BuiltinContains
	BuiltinLen
	BuiltinSort
	BuiltinSortReverse
	BuiltinError
	BuiltinTypeName

	BuiltinAny
	BuiltinStatusResult
	BuiltinDateTime
	BuiltinDatabase
	BuiltinStringBuilder
	BuiltinBool
	BuiltinInt
	BuiltinUint
	BuiltinByte
	BuiltinFloat
	BuiltinChar
	BuiltinString
	BuiltinBytes
	BuiltinChars

	BuiltinPrintf
	BuiltinPrintln
	BuiltinSprintf
	BuiltinGlobals

	BuiltinIsError
	BuiltinIsAny
	BuiltinIsInt
	BuiltinIsUint
	BuiltinIsByte
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

	// by char

	BuiltinNewAny
	BuiltinNewValue

	BuiltinMakeStringBuilder
	BuiltinWriteString

	// BuiltinGo
	BuiltinNilToEmpty

	BuiltinCheckError

	BuiltinGetRandom
	BuiltinGetRandomInt

	BuiltinPr
	BuiltinPl
	BuiltinPln
	BuiltinPlv
	BuiltinSpr

	BuiltinGetInputf

	BuiltinErrStrf
	BuiltinIsErrStr
	BuiltinGetErrStr

	BuiltinStrTrim
	BuiltinStrJoin
	BuiltinStrSplit
	BuiltinStrStartsWith
	BuiltinStrEndsWith
	BuiltinStrIn
	BuiltinStrContains
	BuiltinStrContainsIn
	BuiltinStrReplace
	BuiltinStrFindAllSub

	BuiltinRegMatch
	BuiltinRegContains
	BuiltinRegContainsIn
	BuiltinRegReplace
	BuiltinRegFind
	BuiltinRegFindAll
	BuiltinRegFindAllIndex

	BuiltinEncryptStr
	BuiltinDecryptStr

	BuiltinStrToInt
	BuiltinStrToTime
	BuiltinToStr

	BuiltinGetNowStr

	BuiltinGetParam
	BuiltinGetSwitch
	BuiltinIfSwitchExists

	BuiltinJoinPath
	BuiltinGetBaseFileName

	BuiltinIfFileExists
	BuiltinLoadText
	BuiltinSaveText
	BuiltinAppendText
	BuiltinLoadBytes
	BuiltinSaveBytes
	BuiltinGetFileList

	BuiltinCopyFile
	BuiltinMoveFile
	BuiltinRemoveFile
	BuiltinGetFileInfo

	BuiltinGetWebPage

	BuiltinSetRespHeader
	BuiltinWriteRespHeader
	BuiltinWriteResp

	BuiltinGenJSONResp

	BuiltinToJSON
	BuiltinFromJSON

	BuiltinSimpleEncode
	BuiltinSimpleDecode

	BuiltinReplaceHtmlByMap
	BuiltinCleanHtmlPlaceholders

	BuiltinSleep

	BuiltinGetOSName
	BuiltinSystemCmd
	BuiltinSystemOpenFile

	BuiltinFormatSQLValue

	BuiltinDbConnect
	BuiltinDbQuery
	BuiltinDbQueryRecs
	BuiltinDbQueryMap
	BuiltinDbQueryMapArray
	BuiltinDbQueryCount
	BuiltinDbQueryFloat
	BuiltinDbQueryString
	BuiltinDbExec
	BuiltinDbClose

	BuiltinExit
)

// BuiltinsMap is list of builtin types, exported for REPL.
var BuiltinsMap = map[string]BuiltinType{
	"append":        BuiltinAppend,
	"delete":        BuiltinDelete,
	"copy":          BuiltinCopy,
	"repeat":        BuiltinRepeat,
	"contains":      BuiltinContains,
	"len":           BuiltinLen,
	"sort":          BuiltinSort,
	"sortReverse":   BuiltinSortReverse,
	"error":         BuiltinError,
	"typeName":      BuiltinTypeName,
	"any":           BuiltinAny,
	"dateTime":      BuiltinDateTime,
	"database":      BuiltinDatabase,
	"statusResult":  BuiltinStatusResult,
	"stringBuilder": BuiltinStringBuilder,
	"bool":          BuiltinBool,
	"int":           BuiltinInt,
	"uint":          BuiltinUint,
	"byte":          BuiltinByte,
	"float":         BuiltinFloat,
	"char":          BuiltinChar,
	"string":        BuiltinString,
	"bytes":         BuiltinBytes,
	"chars":         BuiltinChars,
	"printf":        BuiltinPrintf,
	"println":       BuiltinPrintln,
	"globals":       BuiltinGlobals,
	"sprintf":       BuiltinSprintf,

	"isError":     BuiltinIsError,
	"isAny":       BuiltinIsAny,
	"isInt":       BuiltinIsInt,
	"isUint":      BuiltinIsUint,
	"isByte":      BuiltinIsByte,
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

	// by char
	// "go":    BuiltinGo,

	"newAny":   BuiltinNewAny,
	"newValue": BuiltinNewValue,

	"makeStringBuilder": BuiltinMakeStringBuilder,
	"writeString":       BuiltinWriteString,

	"nilToEmpty": BuiltinNilToEmpty,

	"checkError": BuiltinCheckError,

	"sleep":          BuiltinSleep,
	"systemCmd":      BuiltinSystemCmd,
	"systemOpenFile": BuiltinSystemOpenFile,
	"getOSName":      BuiltinGetOSName,
	"exit":           BuiltinExit,

	"formatSQLValue":  BuiltinFormatSQLValue,
	"dbConnect":       BuiltinDbConnect,
	"dbQuery":         BuiltinDbQuery,
	"dbQueryRecs":     BuiltinDbQueryRecs,
	"dbQueryMap":      BuiltinDbQueryMap,
	"dbQueryMapArray": BuiltinDbQueryMapArray,
	"dbQueryCount":    BuiltinDbQueryCount,
	"dbQueryFloat":    BuiltinDbQueryFloat,
	"dbQueryString":   BuiltinDbQueryString,
	"dbExec":          BuiltinDbExec,
	"dbClose":         BuiltinDbClose,

	"pr":  BuiltinPr,
	"pl":  BuiltinPl,
	"pln": BuiltinPln,
	"plv": BuiltinPlv,
	"spr": BuiltinSpr,

	"strTrim":       BuiltinStrTrim,
	"strSplit":      BuiltinStrSplit,
	"strJoin":       BuiltinStrJoin,
	"strStartsWith": BuiltinStrStartsWith,
	"strEndsWith":   BuiltinStrEndsWith,
	"strContains":   BuiltinStrContains,
	"strContainsIn": BuiltinStrContainsIn,
	"strReplace":    BuiltinStrReplace,
	"strFindAllSub": BuiltinStrFindAllSub,

	"regMatch":        BuiltinRegMatch,
	"regContains":     BuiltinRegContains,
	"regContainsIn":   BuiltinRegContainsIn,
	"regReplace":      BuiltinRegReplace,
	"regFind":         BuiltinRegFind,
	"regFindAll":      BuiltinRegFindAll,
	"regFindAllIndex": BuiltinRegFindAllIndex,

	"strIn": BuiltinStrIn,

	"encryptStr": BuiltinEncryptStr,
	"decryptStr": BuiltinDecryptStr,

	"joinPath":        BuiltinJoinPath,
	"getBaseFileName": BuiltinGetBaseFileName,

	"strToInt":  BuiltinStrToInt,
	"strToTime": BuiltinStrToTime,
	"toStr":     BuiltinToStr,

	"getNowStr": BuiltinGetNowStr,

	"ifSwitchExists": BuiltinIfSwitchExists,
	"getSwitch":      BuiltinGetSwitch,
	"getParam":       BuiltinGetParam,

	"getInputf": BuiltinGetInputf,

	"errStrf":   BuiltinErrStrf,
	"isErrStr":  BuiltinIsErrStr,
	"getErrStr": BuiltinGetErrStr,

	"getRandom":    BuiltinGetRandom,
	"getRandomInt": BuiltinGetRandomInt,

	"ifFileExists": BuiltinIfFileExists,
	"copyFile":     BuiltinCopyFile,
	"moveFile":     BuiltinMoveFile,
	"removeFile":   BuiltinRemoveFile,
	"getFileInfo":  BuiltinGetFileInfo,

	"loadText":   BuiltinLoadText,
	"saveText":   BuiltinSaveText,
	"appendText": BuiltinAppendText,

	"loadBytes": BuiltinLoadBytes,
	"saveBytes": BuiltinSaveBytes,

	"getFileList": BuiltinGetFileList,

	"getWebPage": BuiltinGetWebPage,

	"setRespHeader":   BuiltinSetRespHeader,
	"writeRespHeader": BuiltinWriteRespHeader,
	"writeResp":       BuiltinWriteResp,
	"genJSONResp":     BuiltinGenJSONResp,

	"toJSON":   BuiltinToJSON,
	"fromJSON": BuiltinFromJSON,

	"simpleEncode": BuiltinSimpleEncode,
	"simpleDecode": BuiltinSimpleDecode,

	"replaceHtmlByMap":      BuiltinReplaceHtmlByMap,
	"cleanHtmlPlaceholders": BuiltinCleanHtmlPlaceholders,
}

// BuiltinObjects is list of builtins, exported for REPL.
var BuiltinObjects = [...]Object{
	// by char start
	BuiltinReplaceHtmlByMap: &BuiltinFunction{
		Name:   "replaceHtmlByMap",
		Value:  BuiltinReplaceHtmlByMapFunc,
		Remark: ", usage: replaceHtmlByMap(v, mapA)",
	},
	BuiltinCleanHtmlPlaceholders: &BuiltinFunction{
		Name:   "cleanHtmlPlaceholders",
		Value:  fnASRS(tk.CleanHtmlPlaceholders),
		Remark: ", usage: cleanHtmlPlaceholders(v)",
	},
	BuiltinCheckError: &BuiltinFunction{
		Name:   "checkError",
		Value:  builtinCheckErrorFunc,
		Remark: ", usage: checkError(v), if v is Error, output it and exit",
	},
	BuiltinNilToEmpty: &BuiltinFunction{
		Name:   "nilToEmpty",
		Value:  builtinNilToEmptyFunc,
		Remark: ", usage: nilToEmpty(v), if v is Undefined or other errors occur, output empty string",
	},
	BuiltinExit: &BuiltinFunction{
		Name:   "exit",
		Value:  builtinExitFunc,
		Remark: ", usage: exit() or exit(1)",
	},
	// BuiltinGo: &BuiltinFunction{
	// 	Name:  "go",
	// 	Value: builtinGoFunc,
	// },
	BuiltinSleep: &BuiltinFunction{
		Name:   "sleep",
		Value:  builtinSleepFunc,
		Remark: ", usage: sleep(1.2) sleep for 1.2 seconds",
	},
	BuiltinGetFileList: &BuiltinFunction{
		Name:  "getFileList",
		Value: fnASSVRMSSR(tk.GetFileList),
	},
	BuiltinGetWebPage: &BuiltinFunction{
		Name:  "getWebPage",
		Value: builtinGetWebPageFunc,
	},
	BuiltinIfSwitchExists: &BuiltinFunction{
		Name:   "ifSwitchExists",
		Value:  builtinIfSwitchExistsFunc,
		Remark: `, usage: if ifSwitchExists(argsG, "-verbose") {...}`,
	},
	BuiltinGetSwitch: &BuiltinFunction{
		Name:  "getSwitch",
		Value: builtinGetSwitchFunc,
	},
	BuiltinGetParam: &BuiltinFunction{
		Name:   "getParam",
		Value:  builtinGetParamFunc,
		Remark: `, usage: getParam(argsG, 1, "default")`,
	},
	BuiltinSpr: &BuiltinFunction{
		Name:  "spr",
		Value: builtinSprFunc,
	},
	BuiltinPl: &BuiltinFunction{
		Name:   "pl",
		Value:  builtinPlFunc,
		Remark: `, usage: the same as printf, but with a line-end(\n) at the end`,
	},
	BuiltinPr: &BuiltinFunction{
		Name:   "pr",
		Value:  builtinPrFunc,
		Remark: `, usage: the same as print`,
	},
	BuiltinPln: &BuiltinFunction{
		Name:  "pln",
		Value: fnAIV(tk.Pln),
	},
	BuiltinPlv: &BuiltinFunction{
		Name:  "plv",
		Value: fnAIV(tk.Plvsr),
	},
	BuiltinIfFileExists: &BuiltinFunction{
		Name:  "ifFileExists",
		Value: fnASRB(tk.IfFileExists),
	},
	BuiltinCopyFile: &BuiltinFunction{
		Name:  "copyFile",
		Value: builtinCopyFileFunc,
	},
	BuiltinMoveFile: &BuiltinFunction{
		Name:  "moveFile",
		Value: fnASSSVRE(tk.RenameFile),
	},
	BuiltinRemoveFile: &BuiltinFunction{
		Name:  "removeFile",
		Value: fnASRE(tk.RemoveFile),
	},
	BuiltinGetFileInfo: &BuiltinFunction{
		Name:  "getFileInfo",
		Value: builtinGetFileInfoFunc,
	},
	BuiltinLoadText: &BuiltinFunction{
		Name:   "loadText",
		Value:  fnASRS(tk.LoadStringFromFile),
		Remark: `, usage: loadText("file.txt"), return TXERROR: string if failed`,
	},
	BuiltinSaveText: &BuiltinFunction{
		Name:   "saveText",
		Value:  fnASSRS(tk.SaveStringToFile),
		Remark: `, usage: saveText(textT, "file.txt"), return TXERROR: string if failed, empty string if succeed`,
	},
	BuiltinLoadBytes: &BuiltinFunction{
		Name:   "loadBytes",
		Value:  fnASNVRI(tk.LoadBytesFromFile),
		Remark: `, usage: loadBytes("file.bin"), return error or Bytes([]byte)`,
	},
	BuiltinSaveBytes: &BuiltinFunction{
		Name:   "saveBytes",
		Value:  fnAYSRS(tk.SaveBytesToFile),
		Remark: `, usage: saveBytes(bytesT, "file.bin"), return TXERROR: string if failed`,
	},
	BuiltinAppendText: &BuiltinFunction{
		Name:   "appendText",
		Value:  fnASSRS(tk.AppendStringToFile),
		Remark: `, usage: appendText(textT, "file.txt"), return TXERROR: string if failed, empty string if succeed`,
	},
	BuiltinErrStrf: &BuiltinFunction{
		Name:  "errStrf",
		Value: fnASIVRS(tk.ErrStrf),
	},
	BuiltinGetInputf: &BuiltinFunction{
		Name:  "getInputf",
		Value: fnASIVRS(tk.GetInputf),
	},
	BuiltinSystemCmd: &BuiltinFunction{
		Name:  "systemCmd",
		Value: fnASSVRS(tk.SystemCmd),
	},
	BuiltinSystemOpenFile: &BuiltinFunction{
		Name:  "systemOpenFile",
		Value: fnASRS(tk.RunWinFileWithSystemDefault),
	},
	BuiltinDbConnect: &BuiltinFunction{
		Name:  "dbConnect",
		Value: fnASSRI(sqltk.ConnectDBX),
	},
	BuiltinDbQuery: &BuiltinFunction{
		Name:  "dbQuery",
		Value: fnADSIVRI(sqltk.QueryDBX),
	},
	BuiltinDbQueryCount: &BuiltinFunction{
		Name:  "dbQueryCount",
		Value: fnADSIVRI(sqltk.QueryCountX),
	},
	BuiltinDbQueryFloat: &BuiltinFunction{
		Name:  "dbQueryFloat",
		Value: fnADSIVRI(sqltk.QueryFloatX),
	},
	BuiltinDbQueryString: &BuiltinFunction{
		Name:  "dbQueryString",
		Value: fnADSIVRI(sqltk.QueryStringX),
	},
	BuiltinDbQueryRecs: &BuiltinFunction{
		Name:  "dbQueryRecs",
		Value: fnADSIVRI(sqltk.QueryDBRecsX),
	},
	BuiltinDbQueryMap: &BuiltinFunction{
		Name:  "dbQueryMap",
		Value: fnADSSIVRI(sqltk.QueryDBMapX),
	},
	BuiltinDbQueryMapArray: &BuiltinFunction{
		Name:  "dbQueryMapArray",
		Value: fnADSSIVRI(sqltk.QueryDBMapArrayX),
	},
	BuiltinDbExec: &BuiltinFunction{
		Name:  "dbExec",
		Value: fnADSIVRI(sqltk.ExecDBX),
	},
	BuiltinDbClose: &BuiltinFunction{
		Name:  "dbClose",
		Value: BuiltinDbCloseFunc,
	},
	BuiltinGetOSName: &BuiltinFunction{
		Name:  "getOSName",
		Value: fnRS(tk.GetOSName),
	},
	BuiltinIsErrStr: &BuiltinFunction{
		Name:  "isErrStr",
		Value: fnASRB(tk.IsErrStr),
	},
	BuiltinGetErrStr: &BuiltinFunction{
		Name:  "getErrStr",
		Value: fnASRS(tk.GetErrStr),
	},
	BuiltinStrJoin: &BuiltinFunction{
		Name:  "strJoin",
		Value: builtinStrJoinFunc,
	},
	BuiltinStrSplit: &BuiltinFunction{
		Name:  "strSplit",
		Value: fnASSRA(strings.Split),
	},
	BuiltinEncryptStr: &BuiltinFunction{
		Name:  "encryptStr",
		Value: fnASSVRS(tk.EncryptStringByTXDEF),
	},
	BuiltinDecryptStr: &BuiltinFunction{
		Name:  "decryptStr",
		Value: fnASSVRS(tk.DecryptStringByTXDEF),
	},
	BuiltinStrContains: &BuiltinFunction{
		Name:  "strContains",
		Value: fnASSRB(strings.Contains),
	},
	BuiltinStrContainsIn: &BuiltinFunction{
		Name:  "strContainsIn",
		Value: fnASSVRB(tk.ContainsIn),
	},
	BuiltinRegContainsIn: &BuiltinFunction{
		Name:  "regContainsIn",
		Value: fnASSVRB(tk.RegContainsIn),
	},
	BuiltinStrReplace: &BuiltinFunction{
		Name:  "strReplace",
		Value: fnASSVRS(tk.StringReplace),
	},
	BuiltinFormatSQLValue: &BuiltinFunction{
		Name:  "formatSQLValue",
		Value: fnASRS(sqltk.FormatSQLValue),
	},
	BuiltinRegReplace: &BuiltinFunction{
		Name:  "regReplace",
		Value: fnASSSRS(tk.RegReplaceX),
	},
	BuiltinRegFind: &BuiltinFunction{
		Name:  "regFind",
		Value: fnASSNRS(tk.RegFindFirstX),
	},
	BuiltinRegFindAll: &BuiltinFunction{
		Name:  "regFindAll",
		Value: fnASSNRAS(tk.RegFindAllX),
	},
	BuiltinRegFindAllIndex: &BuiltinFunction{
		Name:  "regFindAllIndex",
		Value: fnASSRA2N(tk.RegFindAllIndexX),
	},
	BuiltinStrFindAllSub: &BuiltinFunction{
		Name:  "strFindAllSub",
		Value: fnASSRA2N(tk.FindSubStringAll),
	},
	BuiltinRegContains: &BuiltinFunction{
		Name:  "regContains",
		Value: fnASSRB(tk.RegContainsX),
	},
	BuiltinRegMatch: &BuiltinFunction{
		Name:  "regMatch",
		Value: fnASSRB(tk.RegMatchX),
	},
	BuiltinJoinPath: &BuiltinFunction{
		Name:  "joinPath",
		Value: fnASVRS(filepath.Join),
	},
	BuiltinGetBaseFileName: &BuiltinFunction{
		Name:  "getBaseFileName",
		Value: fnASRS(filepath.Base),
	},
	BuiltinStrIn: &BuiltinFunction{
		Name:  "strIn",
		Value: fnASSVRB(tk.InStrings),
	},
	BuiltinStrStartsWith: &BuiltinFunction{
		Name:  "strStartsWith",
		Value: fnASSRB(strings.HasPrefix),
	},
	BuiltinStrTrim: &BuiltinFunction{
		Name:  "strTrim",
		Value: fnASSVRS_safely(tk.Trim),
	},
	BuiltinStrEndsWith: &BuiltinFunction{
		Name:  "strEndsWith",
		Value: fnASSRB(strings.HasSuffix),
	},
	BuiltinStrToInt: &BuiltinFunction{
		Name:  "strToInt",
		Value: builtinStrToIntFunc,
	},
	BuiltinStrToTime: &BuiltinFunction{
		Name:  "strToTime",
		Value: builtinStrToTimeFunc,
	},
	BuiltinToStr: &BuiltinFunction{
		Name:   "toStr",
		Value:  builtinToStrFunc,
		Remark: ", usage: toStr(any)",
	},
	BuiltinGetNowStr: &BuiltinFunction{
		Name:  "getNowStr",
		Value: builtinGetNowStrFunc,
	},
	BuiltinGetRandom: &BuiltinFunction{
		Name:  "getRandom",
		Value: builtinGetRandomFunc,
	},
	BuiltinGetRandomInt: &BuiltinFunction{
		Name:  "getRandomInt",
		Value: builtinGetRandomIntFunc,
	},
	BuiltinWriteResp: &BuiltinFunction{
		Name:  "writeResp",
		Value: builtinWriteRespFunc,
	},
	BuiltinGenJSONResp: &BuiltinFunction{
		Name:  "genJSONResp",
		Value: builtinGenJSONRespFunc,
	},
	BuiltinToJSON: &BuiltinFunction{
		Name:  "toJSON",
		Value: builtinToJSONFunc,
	},
	BuiltinFromJSON: &BuiltinFunction{
		Name:  "fromJSON",
		Value: builtinFromJSONFunc,
	},
	BuiltinSimpleEncode: &BuiltinFunction{
		Name:  "simpleEncode",
		Value: BuiltinSimpleEncodeFunc,
	},
	BuiltinSimpleDecode: &BuiltinFunction{
		Name:  "simpleDecode",
		Value: BuiltinSimpleDecodeFunc,
	},
	BuiltinSetRespHeader: &BuiltinFunction{
		Name:  "setRespHeader",
		Value: builtinSetRespHeaderFunc,
	},
	BuiltinWriteRespHeader: &BuiltinFunction{
		Name:  "writeRespHeader",
		Value: builtinWriteRespHeaderFunc,
	},
	// by char end

	// :makeArray is a private builtin function to help destructuring array assignments
	BuiltinMakeArray: &BuiltinFunction{
		Name:  ":makeArray",
		Value: builtinWant2(builtinMakeArrayFunc),
	},
	BuiltinMakeStringBuilder: &BuiltinFunction{
		Name:  "makeStringBuilder",
		Value: builtinMakeStringBuilderFunc,
	},
	BuiltinWriteString: &BuiltinFunction{
		Name:  "writeString",
		Value: builtinWriteStringFunc,
	},
	BuiltinNewAny: &BuiltinFunction{
		Name:  "newAny",
		Value: builtinNewAnyFunc,
	},
	BuiltinNewValue: &BuiltinFunction{
		Name:  "newValue",
		Value: builtinNewValueFunc,
	},
	BuiltinAppend: &BuiltinFunction{
		Name:  "append",
		Value: builtinAppendFunc,
	},
	BuiltinDelete: &BuiltinFunction{
		Name:  "delete",
		Value: builtinWant2(builtinDeleteFunc),
	},
	BuiltinCopy: &BuiltinFunction{
		Name:  "copy",
		Value: builtinWant1(builtinCopyFunc),
	},
	BuiltinRepeat: &BuiltinFunction{
		Name:  "repeat",
		Value: builtinWant2(builtinRepeatFunc),
	},
	BuiltinContains: &BuiltinFunction{
		Name:  "contains",
		Value: builtinWant2(builtinContainsFunc),
	},
	BuiltinLen: &BuiltinFunction{
		Name:  "len",
		Value: builtinWant1(builtinLenFunc),
	},
	BuiltinSort: &BuiltinFunction{
		Name:  "sort",
		Value: builtinWant1(builtinSortFunc),
	},
	BuiltinSortReverse: &BuiltinFunction{
		Name:  "sortReverse",
		Value: builtinWant1(builtinSortReverseFunc),
	},
	BuiltinError: &BuiltinFunction{
		Name:  "error",
		Value: builtinWant1(builtinErrorFunc),
	},
	BuiltinTypeName: &BuiltinFunction{
		Name:  "typeName",
		Value: builtinWant1(builtinTypeNameFunc),
	},
	BuiltinAny: &BuiltinFunction{
		Name:  "any",
		Value: builtinWant1(builtinAnyFunc),
	},
	BuiltinDateTime: &BuiltinFunction{
		Name:  "dateTime",
		Value: builtinDateTimeFunc,
	},
	BuiltinDatabase: &BuiltinFunction{
		Name:  "database",
		Value: builtinDatabaseFunc,
	},
	BuiltinStatusResult: &BuiltinFunction{
		Name:  "statusResult",
		Value: builtinStatusResultFunc,
	},
	BuiltinStringBuilder: &BuiltinFunction{
		Name:  "stringBuilder",
		Value: builtinStringBuilderFunc,
	},
	BuiltinBool: &BuiltinFunction{
		Name:  "bool",
		Value: builtinWant1(builtinBoolFunc),
	},
	BuiltinInt: &BuiltinFunction{
		Name:  "int",
		Value: builtinWant1(builtinIntFunc),
	},
	BuiltinUint: &BuiltinFunction{
		Name:  "uint",
		Value: builtinWant1(builtinUintFunc),
	},
	BuiltinByte: &BuiltinFunction{
		Name:  "byte",
		Value: builtinWant1(builtinByteFunc),
	},
	BuiltinFloat: &BuiltinFunction{
		Name:  "float",
		Value: builtinWant1(builtinFloatFunc),
	},
	BuiltinChar: &BuiltinFunction{
		Name:  "char",
		Value: builtinWant1(builtinCharFunc),
	},
	BuiltinString: &BuiltinFunction{
		Name:  "string",
		Value: builtinWant1(builtinStringFunc),
	},
	BuiltinBytes: &BuiltinFunction{
		Name:  "bytes",
		Value: builtinBytesFunc,
	},
	BuiltinChars: &BuiltinFunction{
		Name:  "chars",
		Value: builtinWant1(builtinCharsFunc),
	},
	BuiltinPrintf: &BuiltinFunction{
		Name:  "printf",
		Value: builtinPrintfFunc,
	},
	BuiltinPrintln: &BuiltinFunction{
		Name:  "println",
		Value: builtinPrintlnFunc,
	},
	BuiltinSprintf: &BuiltinFunction{
		Name:  "sprintf",
		Value: builtinSprintfFunc,
	},
	BuiltinGlobals: &BuiltinFunction{
		Name:  "globals",
		Value: noopFunc, // Value is set at runtime
	},

	BuiltinIsError: &BuiltinFunction{
		Name:  "isError",
		Value: builtinIsErrorFunc,
	},
	BuiltinIsAny: &BuiltinFunction{
		Name:  "isAny",
		Value: builtinWant1(builtinIsAnyFunc),
	},
	BuiltinIsInt: &BuiltinFunction{
		Name:  "isInt",
		Value: builtinWant1(builtinIsIntFunc),
	},
	BuiltinIsByte: &BuiltinFunction{
		Name:  "isByte",
		Value: builtinWant1(builtinIsByteFunc),
	},
	BuiltinIsUint: &BuiltinFunction{
		Name:  "isUint",
		Value: builtinWant1(builtinIsUintFunc),
	},
	BuiltinIsFloat: &BuiltinFunction{
		Name:  "isFloat",
		Value: builtinWant1(builtinIsFloatFunc),
	},
	BuiltinIsChar: &BuiltinFunction{
		Name:  "isChar",
		Value: builtinWant1(builtinIsCharFunc),
	},
	BuiltinIsBool: &BuiltinFunction{
		Name:  "isBool",
		Value: builtinWant1(builtinIsBoolFunc),
	},
	BuiltinIsString: &BuiltinFunction{
		Name:  "isString",
		Value: builtinWant1(builtinIsStringFunc),
	},
	BuiltinIsBytes: &BuiltinFunction{
		Name:  "isBytes",
		Value: builtinWant1(builtinIsBytesFunc),
	},
	BuiltinIsMap: &BuiltinFunction{
		Name:  "isMap",
		Value: builtinWant1(builtinIsMapFunc),
	},
	BuiltinIsSyncMap: &BuiltinFunction{
		Name:  "isSyncMap",
		Value: builtinWant1(builtinIsSyncMapFunc),
	},
	BuiltinIsArray: &BuiltinFunction{
		Name:  "isArray",
		Value: builtinWant1(builtinIsArrayFunc),
	},
	BuiltinIsUndefined: &BuiltinFunction{
		Name:  "isUndefined",
		Value: builtinWant1(builtinIsUndefinedFunc),
	},
	BuiltinIsFunction: &BuiltinFunction{
		Name:  "isFunction",
		Value: builtinWant1(builtinIsFunctionFunc),
	},
	BuiltinIsCallable: &BuiltinFunction{
		Name:  "isCallable",
		Value: builtinWant1(builtinIsCallableFunc),
	},
	BuiltinIsIterable: &BuiltinFunction{
		Name:  "isIterable",
		Value: builtinWant1(builtinIsIterableFunc),
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

func noopFunc(args ...Object) (Object, error) {
	return Undefined, nil
}

func builtinWant1(fn CallableFunc) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) != 1 {
			return nil, ErrWrongNumArguments.NewError(wantEqXGotY(1, len(args)))
		}
		return fn(args...)
	}
}

func builtinWant2(fn CallableFunc) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) != 2 {
			return nil, ErrWrongNumArguments.NewError(wantEqXGotY(2, len(args)))
		}
		return fn(args...)
	}
}

func builtinMakeArrayFunc(args ...Object) (Object, error) {
	n, ok := args[0].(Int)
	if !ok {
		return nil, NewArgumentTypeError(
			"first",
			"int",
			args[0].TypeName(),
		)
	}

	nn := int(n)
	if nn <= 0 {
		return args[1], nil
	}

	arr, ok := args[1].(Array)
	if !ok {
		ret := make(Array, nn)
		for i := 1; i < nn; i++ {
			ret[i] = Undefined
		}
		ret[0] = args[1]
		return ret, nil
	}

	length := len(arr)
	if nn <= length {
		return arr[:nn], nil
	}

	ret := make(Array, nn)
	x := copy(ret, arr)
	for i := x; i < nn; i++ {
		ret[i] = Undefined
	}
	return ret, nil
}

func builtinMakeStringBuilderFunc(args ...Object) (Object, error) {
	return Any{
		Value:        new(strings.Builder),
		OriginalType: "StringBuilder",
	}, nil
}

func builtinWriteStringFunc(args ...Object) (Object, error) {
	if len(args) < 2 {
		return NewCommonError("not enough parameters"), nil
	}

	s1, ok := args[1].(String)

	if !ok {
		s1 = ToString(args[1].String())
	}

	if args[0].TypeName() == "any" {
		vT := args[0].(Any)
		switch nv := vT.Value.(type) {
		case *strings.Builder:
			n, errT := nv.WriteString(s1.Value)

			if errT != nil {
				return NewCommonError("%v", errT), nil
			}

			return Int(n), nil
		}
	} else if args[0].TypeName() == "stringBuilder" {
		vT := args[0].(StringBuilder)
		n, errT := vT.Value.WriteString(s1.Value)

		if errT != nil {
			return NewCommonError("%v", errT), nil
		}

		return Int(n), nil
	}

	return NewCommonError("%v", "invalid data"), nil
}

func builtinNewAnyFunc(args ...Object) (Object, error) {
	if len(args) < 1 {
		return Any{Value: nil}, nil
	}

	var s1s string

	s1, ok := args[0].(String)

	if !ok {
		s1s = args[0].String()
	} else {
		s1s = s1.Value
	}

	switch s1s {
	case "strings.Builder", "*strings.Builder", "stringBuilder":
		return builtinMakeStringBuilderFunc(args[1:]...)
	}

	return Any{Value: nil}, nil
}

func builtinNewValueFunc(args ...Object) (Object, error) {
	if len(args) < 1 {
		return Any{Value: nil}, nil
	}

	var s0s string

	s0, ok := args[0].(String)

	if !ok {
		s0s = args[0].String()
	} else {
		s0s = s0.Value
	}

	switch s0s {
	case "":
		if len(args) > 1 {
			return args[1], nil
		}

		return Undefined, nil
	case "strings.Builder", "*strings.Builder", "stringBuilder":
		return builtinMakeStringBuilderFunc(args[1:]...)
	}

	return Undefined, nil
}

func builtinAppendFunc(args ...Object) (Object, error) {
	if len(args) < 1 {
		return nil, ErrWrongNumArguments.NewError("want>=1 got=0")
	}

	switch obj := args[0].(type) {
	case Array:
		if len(args) > 1 {
			return append(obj, args[1:]...), nil
		}
		return obj, nil
	case Bytes:
		if len(args) > 1 {
			var rest []byte
			for i, v := range args[1:] {
				switch vv := v.(type) {
				case Int:
					rest = append(rest, byte(vv))
				case Uint:
					rest = append(rest, byte(vv))
				case Byte:
					rest = append(rest, byte(vv))
				case Char:
					rest = append(rest, byte(vv))
				default:
					return nil, NewArgumentTypeError(
						strconv.Itoa(i+1),
						"int|uint|char",
						args[i+1].TypeName(),
					)
				}
			}
			return append(obj, rest...), nil
		}
		return obj, nil
	case undefined:
		if len(args) > 1 {
			return append(Array{}, args[1:]...), nil
		}
		return Array{}, nil
	default:
		return nil, NewArgumentTypeError(
			"first",
			"array",
			args[0].TypeName(),
		)
	}
}

func builtinDeleteFunc(args ...Object) (Object, error) {
	switch arg := args[0].(type) {
	case Map:
		if key, ok := args[1].(String); ok {
			delete(arg, FromString(key))
			return Undefined, nil
		}
		return nil, NewArgumentTypeError(
			"second",
			"string",
			args[1].TypeName(),
		)
	case *SyncMap:
		if key, ok := args[1].(String); ok {
			if arg.Map == nil {
				return Undefined, nil
			}
			delete(arg.Map, FromString(key))
			return Undefined, nil
		}
		return nil, NewArgumentTypeError(
			"second",
			"string",
			args[1].TypeName(),
		)
	default:
		return nil, NewArgumentTypeError(
			"first",
			"map|sync-map",
			args[0].TypeName(),
		)
	}
}

func builtinCopyFunc(args ...Object) (Object, error) {
	if v, ok := args[0].(Copier); ok {
		return v.Copy(), nil
	}
	return args[0], nil
}

func builtinRepeatFunc(args ...Object) (Object, error) {
	var count int
	switch v := args[1].(type) {
	case Int:
		count = int(v)
	case Uint:
		count = int(v)
	case Byte:
		count = int(v)
	default:
		return nil, NewArgumentTypeError(
			"second",
			"int|uint",
			args[1].TypeName(),
		)
	}

	if count < 0 {
		return nil, NewArgumentTypeError(
			"second",
			"non-negative integer",
			args[1].TypeName(),
		)
	}

	switch v := args[0].(type) {
	case Array:
		out := make(Array, 0, len(v)*count)
		for i := 0; i < count; i++ {
			out = append(out, v...)
		}
		return out, nil
	case String:
		return ToString(strings.Repeat(FromString(v), count)), nil
	case Bytes:
		return Bytes(bytes.Repeat(v, count)), nil
	}

	return nil, NewArgumentTypeError(
		"first",
		"array|string|bytes",
		args[0].TypeName(),
	)
}

func builtinContainsFunc(args ...Object) (Object, error) {
	switch obj := args[0].(type) {
	case Map:
		_, ok := obj[args[1].String()]
		return Bool(ok), nil
	case *SyncMap:
		_, ok := obj.Get(args[1].String())
		return Bool(ok), nil
	case Array:
		search := args[1]
		for _, item := range obj {
			if item.Equal(search) {
				return True, nil
			}
		}
		return False, nil
	case String:
		return Bool(strings.Contains(FromString(obj), args[1].String())), nil
	case Bytes:
		switch v := args[1].(type) {
		case Int:
			return Bool(bytes.Contains(obj, []byte{byte(v)})), nil
		case Uint:
			return Bool(bytes.Contains(obj, []byte{byte(v)})), nil
		case Byte:
			return Bool(bytes.Contains(obj, []byte{byte(v)})), nil
		case Char:
			return Bool(bytes.Contains(obj, []byte{byte(v)})), nil
		case String:
			return Bool(bytes.Contains(obj, []byte(v.Value))), nil
		case Bytes:
			return Bool(bytes.Contains(obj, v)), nil
		default:
			return nil, NewArgumentTypeError(
				"second",
				"int|uint|string|char|bytes",
				args[1].TypeName(),
			)
		}
	case undefined:
		return False, nil
	default:
		return nil, NewArgumentTypeError(
			"first",
			"map|array|string|bytes",
			args[0].TypeName(),
		)
	}
}

func builtinLenFunc(args ...Object) (Object, error) {
	switch v := args[0].(type) {
	case String:
		return Int(len(v.Value)), nil
	case Array:
		return Int(len(v)), nil
	case Map:
		return Int(len(v)), nil
	case *SyncMap:
		return Int(len(v.Map)), nil
	case Bytes:
		return Int(len(v)), nil
	}
	return Int(0), nil
}

func builtinSortFunc(args ...Object) (Object, error) {
	switch obj := args[0].(type) {
	case Array:
		var err error
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

		if err != nil {
			return nil, err
		}
		return obj, nil
	case String:
		s := []rune(obj.Value)
		sort.Slice(s, func(i, j int) bool {
			return s[i] < s[j]
		})
		return ToString(s), nil
	case Bytes:
		sort.Slice(obj, func(i, j int) bool {
			return obj[i] < obj[j]
		})
		return obj, nil
	case undefined:
		return Undefined, nil
	}

	return nil, NewArgumentTypeError(
		"first",
		"array|string|bytes",
		args[0].TypeName(),
	)
}

func builtinSortReverseFunc(args ...Object) (Object, error) {
	switch obj := args[0].(type) {
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
		s := []rune(obj.Value)
		sort.Slice(s, func(i, j int) bool {
			return s[j] < s[i]
		})
		return ToString(s), nil
	case Bytes:
		sort.Slice(obj, func(i, j int) bool {
			return obj[j] < obj[i]
		})
		return obj, nil
	case undefined:
		return Undefined, nil
	}

	return nil, NewArgumentTypeError(
		"first",
		"array|string|bytes",
		args[0].TypeName(),
	)
}

func builtinErrorFunc(args ...Object) (Object, error) {
	return &Error{Name: "error", Message: args[0].String()}, nil
}

func builtinTypeNameFunc(args ...Object) (Object, error) {
	return ToString(args[0].TypeName()), nil
}

func builtinBoolFunc(args ...Object) (Object, error) {
	return Bool(!args[0].IsFalsy()), nil
}

func builtinIntFunc(args ...Object) (Object, error) {
	switch obj := args[0].(type) {
	case Uint:
		return Int(obj), nil
	case Byte:
		return Int(obj), nil
	case Float:
		return Int(obj), nil
	case Int:
		return obj, nil
	case Char:
		return Int(obj), nil
	case String:
		v, err := strconv.ParseInt(obj.Value, 0, 64)
		if err != nil {
			return nil, err
		}
		return Int(v), nil
	case Bool:
		if obj {
			return Int(1), nil
		}
		return Int(0), nil
	default:
		return nil, NewArgumentTypeError(
			"first",
			"numeric|string|bool",
			args[0].TypeName(),
		)
	}
}

func builtinUintFunc(args ...Object) (Object, error) {
	switch obj := args[0].(type) {
	case Int:
		return Uint(obj), nil
	case Byte:
		return Uint(obj), nil
	case Float:
		return Uint(obj), nil
	case Char:
		return Uint(obj), nil
	case Uint:
		return obj, nil
	case String:
		v, err := strconv.ParseUint(obj.Value, 0, 64)
		if err != nil {
			return nil, err
		}
		return Uint(v), nil
	case Bool:
		if obj {
			return Uint(1), nil
		}
		return Uint(0), nil
	default:
		return nil, NewArgumentTypeError(
			"first",
			"numeric|string|bool",
			args[0].TypeName(),
		)
	}
}

func builtinByteFunc(args ...Object) (Object, error) {
	switch obj := args[0].(type) {
	case Int:
		return Byte(obj), nil
	case Float:
		return Byte(obj), nil
	case Char:
		return Byte(obj), nil
	case Uint:
		return Byte(obj), nil
	case Byte:
		return obj, nil
	case String:
		v, err := strconv.ParseUint(obj.Value, 0, 64)
		if err != nil {
			return nil, err
		}
		return Byte(v), nil
	case Bool:
		if obj {
			return Byte(1), nil
		}
		return Byte(0), nil
	default:
		return nil, NewArgumentTypeError(
			"first",
			"numeric|string|bool",
			args[0].TypeName(),
		)
	}
}

func builtinAnyFunc(args ...Object) (Object, error) {
	switch obj := args[0].(type) {
	case Int:
		return Any{Value: int(obj)}, nil
	case Float:
		return Any{Value: float64(obj)}, nil
	case Char:
		return Any{Value: rune(obj)}, nil
	case Uint:
		return Any{Value: uint64(obj)}, nil
	case Byte:
		return Any{Value: byte(obj)}, nil
	case String:
		return Any{Value: obj.Value}, nil
	case Bool:
		if obj {
			return Any{Value: true}, nil
		}
		return Any{Value: false}, nil
	default:
		return Any{Value: obj}, nil
	}
}

func builtinDateTimeFunc(args ...Object) (Object, error) {
	if len(args) < 1 {
		return DateTime{Value: time.Now()}, nil
	}

	switch obj := args[0].(type) {
	case Int:
		return DateTime{Value: tk.GetTimeFromUnixTimeStampMid(obj.String())}, nil
	// case Float:
	// 	return DateTime{Value: float64(obj)}, nil
	case DateTime:
		return DateTime{Value: obj.Value}, nil
	case String:
		return DateTime{Value: tk.ToTime(obj)}, nil
	default:
		return Undefined, NewCommonError("failed to convert time")
	}
}

func builtinDatabaseFunc(args ...Object) (Object, error) {
	if len(args) < 2 {
		return Database{Value: nil}, nil
	}

	nv0, ok := args[0].(String)

	if !ok {
		return NewCommonError("invalid paramter 1"), nil
	}

	nv1, ok := args[1].(String)

	if !ok {
		return NewCommonError("invalid paramter 2"), nil
	}

	rsT := sqltk.ConnectDBX(nv0.Value, nv1.Value)
	if tk.IsError(rsT) {
		return NewFromError(rsT.(error)), nil
	}

	return Database{DBType: nv0.Value, DBConnectString: nv1.String(), Value: rsT.(*sql.DB)}, nil
}

func builtinStatusResultFunc(args ...Object) (Object, error) {
	return GenStatusResult(args...)
}

func builtinStringBuilderFunc(args ...Object) (Object, error) {
	if len(args) < 1 {
		rs := StringBuilder{Value: new(strings.Builder)}
		rs.Value.WriteString("")
		return rs, nil
	}

	s := args[0].String()

	rs := StringBuilder{Value: new(strings.Builder)}

	rs.Value.WriteString(s)

	return rs, nil
}

func builtinCharFunc(args ...Object) (Object, error) {
	switch obj := args[0].(type) {
	case Int:
		return Char(obj), nil
	case Uint:
		return Char(obj), nil
	case Byte:
		return Char(obj), nil
	case Float:
		return Char(obj), nil
	case Char:
		return obj, nil
	case String:
		r, _ := utf8.DecodeRuneInString(obj.Value)
		if r == utf8.RuneError {
			return Undefined, nil
		}
		return Char(r), nil
	case Bool:
		if obj {
			return Char(1), nil
		}
		return Char(0), nil
	default:
		return nil, NewArgumentTypeError(
			"first",
			"numeric|string|bool",
			args[0].TypeName(),
		)
	}
}

func builtinFloatFunc(args ...Object) (Object, error) {
	switch obj := args[0].(type) {
	case Int:
		return Float(obj), nil
	case Uint:
		return Float(obj), nil
	case Byte:
		return Float(obj), nil
	case Char:
		return Float(obj), nil
	case Float:
		return obj, nil
	case String:
		v, err := strconv.ParseFloat(obj.Value, 64)
		if err != nil {
			return nil, err
		}
		return Float(v), nil
	case Bool:
		if obj {
			return Float(1), nil
		}
		return Float(0), nil
	default:
		return nil, NewArgumentTypeError(
			"first",
			"numeric|string|bool",
			args[0].TypeName(),
		)
	}
}

func builtinStringFunc(args ...Object) (Object, error) {
	if len(args) < 1 {
		return String{Value: ""}, nil
	}
	return String{Value: args[0].String()}, nil
}

func builtinBytesFunc(args ...Object) (Object, error) {
	if len(args) == 0 {
		return Bytes{}, nil
	}

	switch obj := args[0].(type) {
	case String:
		return Bytes([]byte(obj.Value)), nil
	case Bytes:
		return obj, nil
	default:
		var out Bytes
		for i, obj := range args {
			switch v := obj.(type) {
			case Int:
				out = append(out, byte(v))
			case Uint:
				out = append(out, byte(v))
			case Byte:
				out = append(out, byte(v))
			case Char:
				out = append(out, byte(v))
			default:
				return nil, NewArgumentTypeError(
					strconv.Itoa(i+1),
					"int|uint|char",
					args[i].TypeName(),
				)
			}
		}
		return out, nil
	}
}

func builtinCharsFunc(args ...Object) (Object, error) {
	switch obj := args[0].(type) {
	case String:
		s := obj.Value
		var out = make(Array, 0, utf8.RuneCountInString(s))
		sz := len(obj.Value)
		i := 0

		for i < sz {
			r, w := utf8.DecodeRuneInString(s[i:])
			if r == utf8.RuneError {
				return Undefined, nil
			}
			out = append(out, Char(r))
			i += w
		}
		return out, nil
	case Bytes:
		var out = make(Array, 0, utf8.RuneCount(obj))
		sz := len(obj)
		i := 0

		for i < sz {
			r, w := utf8.DecodeRune(obj[i:])
			if r == utf8.RuneError {
				return Undefined, nil
			}
			out = append(out, Char(r))
			i += w
		}
		return out, nil
	}
	return nil, NewArgumentTypeError(
		"first",
		"string|bytes",
		args[0].TypeName(),
	)
}

func builtinPrintfFunc(args ...Object) (Object, error) {
	if len(args) < 1 {
		return nil, ErrWrongNumArguments.NewError("want>=1 got=0")
	}

	switch len(args) {
	case 1:
		if _, err := fmt.Fprint(PrintWriter, args[0].String()); err != nil {
			return nil, err
		}
	default:
		vargs := make([]interface{}, len(args)-1)
		for i := range args[1:] {
			vargs[i] = args[i+1]
		}
		_, err := fmt.Fprintf(PrintWriter, args[0].String(), vargs...)
		if err != nil {
			return nil, err
		}
	}
	return Undefined, nil
}

func builtinPrintlnFunc(args ...Object) (Object, error) {
	switch len(args) {
	case 0:
		if _, err := fmt.Fprintln(PrintWriter); err != nil {
			return nil, err
		}
	case 1:
		if _, err := fmt.Fprintln(PrintWriter, args[0]); err != nil {
			return nil, err
		}
	default:
		vargs := make([]interface{}, len(args))
		for i := range args {
			vargs[i] = args[i]
		}
		if _, err := fmt.Fprintln(PrintWriter, vargs...); err != nil {
			return nil, err
		}
	}
	return Undefined, nil
}

func builtinSprintfFunc(args ...Object) (Object, error) {
	if len(args) < 1 {
		return nil, ErrWrongNumArguments.NewError("want>=1 got=0")
	}

	vargs := make([]interface{}, len(args)-1)
	for i := range args[1:] {
		vargs[i] = args[i+1]
	}

	return ToString(fmt.Sprintf(args[0].String(), vargs...)), nil
}

func builtinIsErrorFunc(args ...Object) (Object, error) {
	switch len(args) {
	case 1:
		switch args[0].(type) {
		case *Error, *RuntimeError:
			return True, nil
			// case Any:
			// 	_, ok := nv.Value.(error)

			// 	if ok {
			// 		return True, nil
			// 	}
			// 	return False, nil
		}
	case 2:
		if err, ok := args[0].(error); ok {
			if target, ok := args[1].(error); ok {
				return Bool(errors.Is(err, target)), nil
			}
		}
	default:
		return nil, ErrWrongNumArguments.NewError(
			fmt.Sprint("want=1..2 got=", len(args)))
	}
	return False, nil
}

func builtinCheckErrorFunc(args ...Object) (Object, error) {
	rsT, errT := builtinIsErrorFunc(args...)

	if errT != nil && rsT == True {
		fmt.Printf("Error: %v", args[0])
		os.Exit(0)
	}

	return Undefined, nil
}

func builtinIsIntFunc(args ...Object) (Object, error) {
	_, ok := args[0].(Int)
	return Bool(ok), nil
}

func builtinIsByteFunc(args ...Object) (Object, error) {
	_, ok := args[0].(Byte)
	return Bool(ok), nil
}

func builtinIsAnyFunc(args ...Object) (Object, error) {
	_, ok := args[0].(Any)
	return Bool(ok), nil
}

// Undefined to empty string
func builtinNilToEmptyFunc(argsA ...Object) (Object, error) {
	if len(argsA) < 1 {
		return ToString(""), nil
	}

	vA := argsA[0]

	if vA.TypeName() == "undefined" {
		return ToString(""), nil
	}

	if vA == nil {
		return ToString(""), nil
	}

	if tk.IsNil(vA) {
		return ToString(""), nil
	}

	rsT := vA.String()

	if len(argsA) > 1 {
		argsT := ObjectsToS(argsA[1:])
		if tk.IfSwitchExists(argsT, "-trim") {
			rsT = tk.Trim(rsT)
		}
	}

	return ToString(rsT), nil
}

func builtinStrJoinFunc(args ...Object) (Object, error) {
	if len(args) != 2 {
		return nil, ErrWrongNumArguments.NewError(wantEqXGotY(2, len(args)))
	}
	arr, ok := args[0].(Array)
	if !ok {
		return nil, NewArgumentTypeError("first", "array",
			args[0].TypeName())
	}
	sep, ok := args[1].(String)
	if !ok {
		return nil, NewArgumentTypeError("second", "string",
			args[1].TypeName())
	}
	elems := make([]string, len(arr))
	for i := range arr {
		elems[i] = arr[i].String()
	}
	return ToString(strings.Join(elems, sep.Value)), nil
}

func builtinStrToIntFunc(args ...Object) (Object, error) {
	defaultT := -1

	if len(args) > 1 {
		defaultA, ok := args[1].(Int)
		if ok {
			defaultT = int(defaultA)
		}
	}

	if len(args) < 1 {
		return Int(defaultT), nil
	}

	strT, ok := args[0].(String)
	if !ok {
		return Int(defaultT), nil
	}

	rsT := tk.StrToIntWithDefaultValue(strT.String(), defaultT)

	return Int(rsT), nil
}

func builtinStrToTimeFunc(args ...Object) (Object, error) {
	// defaultT := NewDateTimeValue("")

	if len(args) < 1 {
		return NewCommonError("not enough parameters"), nil
	}

	formatT := ""

	if len(args) > 1 {
		defaultA, ok := args[1].(String)
		if ok {
			formatT = defaultA.String()
		}
	}

	strT, ok := args[0].(String)
	if !ok {
		return NewCommonError("invalid paramter"), nil
	}

	rsT, errT := tk.StrToTimeByFormat(strT.String(), formatT)

	if errT != nil {
		return NewCommonError("time parse failed: %v", errT), nil
	}

	return NewDateTimeValue(rsT), nil
}

func builtinToStrFunc(args ...Object) (Object, error) {
	if len(args) < 1 {
		return ToString(""), nil
	}

	rsT := tk.ToStr(ConvertFromObject(args[0]))

	return ToString(rsT), nil
}

func builtinPlFunc(args ...Object) (Object, error) {
	if len(args) < 1 {
		return Undefined, nil
	}

	v, ok := args[0].(String)
	if !ok {
		return Undefined, NewCommonError("required format string")
	}

	tk.Pl(v.String(), ObjectsToI(args[1:])...)

	return Undefined, nil
}

func builtinPrFunc(args ...Object) (Object, error) {
	if len(args) < 1 {
		return Undefined, nil
	}

	fmt.Print(ObjectsToI(args)...)

	return Undefined, nil
}

func BuiltinDbCloseFunc(args ...Object) (Object, error) {
	if len(args) < 1 {
		return NewCommonError("not enough parameters"), nil
	}

	nv, ok := args[0].(Any)

	if !ok {
		return NewCommonError("type error"), nil
	}

	dbT, ok := nv.Value.(*sql.DB)

	if !ok {
		return NewCommonError("type error for *sql.DB"), nil
	}

	errT := dbT.Close()

	if errT != nil {
		return NewCommonError("type error for *sql.DB"), nil
	}

	return Undefined, nil
}

func builtinSprFunc(args ...Object) (Object, error) {
	if len(args) < 1 {
		return Undefined, nil
	}

	v, ok := args[0].(String)
	if !ok {
		return Undefined, NewCommonError("required format string")
	}

	return ToString(fmt.Sprintf(v.String(), ObjectsToI(args[1:])...)), nil
}

func builtinSleepFunc(args ...Object) (Object, error) {
	if len(args) < 1 {
		return Undefined, NewCommonError("not enough parameters")
	}

	v := args[0].String()

	f := tk.StrToFloat64WithDefaultValue(v, 0)

	if f <= 0 {
		return Undefined, NewCommonError("invalid time")
	}

	tk.Sleep(f)

	return Undefined, nil
}

func builtinGetWebPageFunc(args ...Object) (Object, error) {
	if len(args) < 3 {
		return ToString(tk.ErrStrf("not enough parameters")), nil
	}

	v0, ok := args[0].(String)

	if !ok {
		return ToString(tk.ErrStrf("type error for arg 0")), nil
	}

	v1, ok := args[1].(Map)

	if !ok {
		return ToString(tk.ErrStrf("type error for arg 1")), nil
	}

	v2, ok := args[2].(Map)

	if !ok {
		return ToString(tk.ErrStrf("type error for arg 2")), nil
	}

	rsT := tk.DownloadWebPage(v0.String(), tk.MSI2MSS(ConvertFromObject(v1).(map[string]interface{})),
		tk.MSI2MSS(ConvertFromObject(v2).(map[string]interface{})), ObjectsToS(args[3:])...)

	return ToString(rsT), nil
}

func builtinExitFunc(args ...Object) (Object, error) {
	resultT := 0

	if len(args) > 0 {
		resultT = tk.StrToIntWithDefaultValue(args[0].String(), 0)
	}

	os.Exit(resultT)

	return Undefined, nil
}

// not completed...
// func builtinGoFunc(args ...Object) (Object, error) {
// 	if len(args) < 1 {
// 		return Undefined, NewCommonError("not enough parameters")
// 	}

// 	v, ok := args[0].(*CompiledFunction)
// 	if !ok {
// 		return Undefined, NewCommonError("required function type, got: %T %#v", args[0], args[0])
// 	}

// 	// go v.Call(args[1:]...)
// 	v.Call(args[1:]...)

// 	return Undefined, nil
// }

func builtinGetNowStrFunc(args ...Object) (Object, error) {
	return ToString(tk.GetNowTimeStringFormat(ObjectsToS(args)...)), nil
}

func builtinGetRandomIntFunc(args ...Object) (Object, error) {
	if len(args) < 1 {
		return Undefined, nil
	}

	v, ok := args[0].(Int)
	if !ok {
		return Undefined, NewCommonError("required int")
	}

	return Int(tk.GetRandomIntLessThan(int(v))), nil
}

func builtinGetRandomFunc(args ...Object) (Object, error) {
	g := tk.NewRandomGenerator()

	g.Randomize()

	f := g.Float64()

	return Float(f), nil
}

func builtinWriteRespFunc(args ...Object) (Object, error) {
	if len(args) < 2 {
		return Undefined, NewCommonError("not enough parameters")
	}

	v, ok := args[0].(Any)
	if !ok {
		return Undefined, NewArgumentTypeError(
			"1",
			"any",
			args[0].TypeName(),
		)
	}

	var contentT Bytes = nil

	v1, ok := args[1].(String)

	if ok {
		contentT = Bytes(v1.Value)
	}

	if contentT == nil {
		v2, ok := args[1].(Bytes)
		if !ok {
			return Undefined, NewArgumentTypeError(
				"2",
				"any",
				args[1].TypeName(),
			)
		}
		contentT = v2
	}

	vv, ok := v.Value.(http.ResponseWriter)

	if !ok {
		return Undefined, NewCommonError("invalid type in Any object(expect http.ResponseWriter)")
	}

	r, errT := vv.Write(contentT)

	return Int(r), errT
}

func builtinGenJSONRespFunc(args ...Object) (Object, error) {
	if len(args) < 3 {
		return Undefined, NewCommonError("not enough parameters")
	}

	v0, ok := args[0].(Any)
	if !ok {
		return Undefined, NewArgumentTypeError(
			"1",
			"any",
			args[0].TypeName(),
		)
	}

	v0v, ok := v0.Value.(*http.Request)
	if !ok {
		return Undefined, NewArgumentTypeError(
			"1",
			"any(*http.Request)",
			args[0].TypeName(),
		)
	}

	rsT := tk.GenerateJSONPResponseWithMore(args[1].String(), args[2].String(), v0v, ObjectsToS(args[3:])...)

	return ToString(rsT), nil
}

func builtinToJSONFunc(args ...Object) (Object, error) {
	if len(args) < 1 {
		return Undefined, NewCommonError("not enough parameters")
	}

	cObjT := ConvertFromObject(args[0])

	rsT := tk.ToJSONX(cObjT, ObjectsToS(args[1:])...)

	return ToString(rsT), nil
}

func builtinFromJSONFunc(args ...Object) (Object, error) {
	if len(args) < 1 {
		return Undefined, NewCommonError("not enough parameters")
	}

	jsonTextT := args[0].String()

	jObjT := tk.FromJSONWithDefault(jsonTextT, nil)

	cObjT := ConvertToObject(jObjT)

	return cObjT, nil
}

func BuiltinSimpleEncodeFunc(args ...Object) (Object, error) {
	if len(args) < 1 {
		return Undefined, NewCommonError("not enough parameters")
	}

	return ToString(tk.EncodeStringCustomEx(args[0].String())), nil
}

func BuiltinSimpleDecodeFunc(args ...Object) (Object, error) {
	if len(args) < 1 {
		return Undefined, NewCommonError("not enough parameters")
	}

	return ToString(tk.DecodeStringCustom(args[0].String())), nil
}

func builtinGetSwitchFunc(argsA ...Object) (Object, error) {
	defaultT := ToString("")

	if argsA == nil {
		return defaultT, nil
	}

	if len(argsA) < 2 {
		return defaultT, nil
	}

	if len(argsA) > 2 {
		defaultT = ToString(argsA[2].String())
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

		argT := FromString(argOT)
		if tk.StartsWith(argT, switchStrT) {
			tmpStrT = argT[len(switchStrT):]
			if tk.StartsWith(tmpStrT, "\"") && tk.EndsWith(tmpStrT, "\"") {
				return ToString(tmpStrT[1 : len(tmpStrT)-1]), nil
			}

			return ToString(tmpStrT), nil
		}

	}

	return defaultT, nil
}

func builtinIfSwitchExistsFunc(argsA ...Object) (Object, error) {
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

func builtinGetFileInfoFunc(argsA ...Object) (Object, error) {
	if len(argsA) < 1 {
		return Undefined, NewCommonError("not enough parameters")
	}

	fileNameObjT, ok := argsA[0].(String)
	if !ok {
		return Undefined, NewCommonError("invalid parameter")
	}

	filePathT := FromString(fileNameObjT)

	fi, errT := os.Stat(filePathT)
	if errT != nil && !os.IsExist(errT) {
		return Undefined, NewFromError(errT)
	}

	absPathT, errT := filepath.Abs(filePathT)
	if errT != nil {
		return Undefined, NewFromError(errT)
	}

	mapT := Map{"Path": ToString(filePathT), "Abs": ToString(absPathT), "Name": ToString(filepath.Base(filePathT)), "Ext": ToString(filepath.Ext(filePathT)), "Size": ToString(tk.Int64ToStr(fi.Size())), "IsDir": ToString(tk.BoolToStr(fi.IsDir())), "Time": ToString(tk.FormatTime(fi.ModTime(), tk.TimeFormatCompact)), "Mode": ToString(fmt.Sprintf("%v", fi.Mode()))}

	return mapT, nil

}

func builtinGetParamFunc(argsA ...Object) (Object, error) {
	defaultT := ToString("")
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
		defaultT = ToString(argsA[2].String())
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
					return ToString(argT[1 : len(argT)-1]), nil
				}
			}

			return v, nil
		}

		cnt++

	}

	return defaultT, nil
}

func builtinWriteRespHeaderFunc(args ...Object) (Object, error) {
	if len(args) < 2 {
		return Undefined, NewCommonError("not enough parameters")
	}

	v, ok := args[0].(Any)
	if !ok {
		return Undefined, NewArgumentTypeError(
			"1",
			"any",
			args[0].TypeName(),
		)
	}

	vv, ok := v.Value.(http.ResponseWriter)

	if !ok {
		return Undefined, NewCommonError("invalid type in Any object(expect http.ResponseWriter)")
	}

	v2, ok := args[1].(Int)
	if !ok {
		return Undefined, NewArgumentTypeError(
			"2",
			"int",
			args[1].TypeName(),
		)
	}

	vv.WriteHeader(int(v2))

	return nil, nil
}

func builtinSetRespHeaderFunc(args ...Object) (Object, error) {
	if len(args) < 3 {
		return Undefined, NewCommonError("not enough parameters")
	}

	v, ok := args[0].(Any)
	if !ok {
		return Undefined, NewArgumentTypeError(
			"1",
			"any",
			args[0].TypeName(),
		)
	}

	v2, ok := args[1].(String)
	if !ok {
		return Undefined, NewArgumentTypeError(
			"2",
			"any",
			args[1].TypeName(),
		)
	}

	v3, ok := args[2].(String)
	if !ok {
		return Undefined, NewArgumentTypeError(
			"3",
			"any",
			args[3].TypeName(),
		)
	}

	vv, ok := v.Value.(http.ResponseWriter)

	if !ok {
		return Undefined, NewCommonError("invalid type in Any object(expect http.ResponseWriter)")
	}

	vv.Header().Set(v2.Value, v3.Value)

	return Undefined, nil
}

func builtinIsUintFunc(args ...Object) (Object, error) {
	_, ok := args[0].(Uint)
	return Bool(ok), nil
}

func builtinIsFloatFunc(args ...Object) (Object, error) {
	_, ok := args[0].(Float)
	return Bool(ok), nil
}

func builtinIsCharFunc(args ...Object) (Object, error) {
	_, ok := args[0].(Char)
	return Bool(ok), nil
}

func builtinIsBoolFunc(args ...Object) (Object, error) {
	_, ok := args[0].(Bool)
	return Bool(ok), nil
}

func builtinIsStringFunc(args ...Object) (Object, error) {
	_, ok := args[0].(String)
	return Bool(ok), nil
}

func builtinIsBytesFunc(args ...Object) (Object, error) {
	_, ok := args[0].(Bytes)
	return Bool(ok), nil
}

func builtinIsMapFunc(args ...Object) (Object, error) {
	_, ok := args[0].(Map)
	return Bool(ok), nil
}

func builtinIsSyncMapFunc(args ...Object) (Object, error) {
	_, ok := args[0].(*SyncMap)
	return Bool(ok), nil
}

func builtinIsArrayFunc(args ...Object) (Object, error) {
	_, ok := args[0].(Array)
	return Bool(ok), nil
}

func builtinIsUndefinedFunc(args ...Object) (Object, error) {
	return Bool(args[0].TypeName() == "undefined"), nil
	// return Bool(args[0] == Undefined), nil
}

func builtinIsFunctionFunc(args ...Object) (Object, error) {
	_, ok := args[0].(*CompiledFunction)
	if ok {
		return True, nil
	}

	_, ok = args[0].(*BuiltinFunction)
	if ok {
		return True, nil
	}

	_, ok = args[0].(*Function)
	return Bool(ok), nil
}

func builtinIsCallableFunc(args ...Object) (Object, error) {
	return Bool(args[0].CanCall()), nil
}

func builtinIsIterableFunc(args ...Object) (Object, error) {
	return Bool(args[0].CanIterate()), nil
}

func wantEqXGotY(x, y int) string {
	buf := make([]byte, 0, 20)
	buf = append(buf, "want="...)
	buf = strconv.AppendInt(buf, int64(x), 10)
	buf = append(buf, " got="...)
	buf = strconv.AppendInt(buf, int64(y), 10)
	return string(buf)
}

func wantGEqXGotY(x, y int) string {
	buf := make([]byte, 0, 20)
	buf = append(buf, "want>="...)
	buf = strconv.AppendInt(buf, int64(x), 10)
	buf = append(buf, " got="...)
	buf = strconv.AppendInt(buf, int64(y), 10)
	return string(buf)
}

func fnASRS(fn func(string) string) CallableFunc {
	return func(args ...Object) (Object, error) {
		if len(args) != 1 {
			return ErrWrongNumArguments.NewError(wantEqXGotY(1, len(args))), nil
		}

		s, ok := args[0].(String)
		if !ok {
			return NewArgumentTypeError("first", "string", args[0].TypeName()), nil
		}

		return ToString(fn(s.Value)), nil
	}
}

func fnASMSSRS(fn func(string, map[string]string) string) CallableFunc {
	return func(args ...Object) (Object, error) {
		if len(args) < 2 {
			return ErrWrongNumArguments.NewError(wantEqXGotY(2, len(args))), nil
		}

		s, ok := args[0].(String)
		if !ok {
			return NewArgumentTypeError("first", "string", args[0].TypeName()), nil
		}

		m, ok := args[1].(Map)
		if !ok {
			return NewArgumentTypeError("second", "map", args[1].TypeName()), nil
		}

		return ToString(fn(s.Value, tk.MSI2MSS(ConvertFromObject(m).(map[string]interface{})))), nil
	}
}

func BuiltinReplaceHtmlByMapFunc(args ...Object) (Object, error) {
	if len(args) < 2 {
		return ErrWrongNumArguments.NewError(wantEqXGotY(2, len(args))), nil
	}

	s, ok := args[0].(String)
	if !ok {
		return NewArgumentTypeError("first", "string", args[0].TypeName()), nil
	}

	m, ok := args[1].(Map)
	if !ok {
		return NewArgumentTypeError("second", "map", args[1].TypeName()), nil
	}

	if m == nil {
		return args[0], nil
	}

	st := s.Value

	for k, v := range m {
		st = tk.Replace(st, "TX_"+k+"_XT", v.String())
	}

	return ToString(st), nil
}

func fnASSRS(fn func(string, string) string) CallableFunc {
	return func(args ...Object) (Object, error) {
		if len(args) != 2 {
			return ErrWrongNumArguments.NewError(wantEqXGotY(2, len(args))), nil
		}
		s1, ok := args[0].(String)
		if !ok {
			return NewArgumentTypeError("first", "string", args[0].TypeName()), nil
		}
		s2, ok := args[1].(String)
		if !ok {
			return NewArgumentTypeError("second", "string", args[1].TypeName()), nil
		}
		return ToString(fn(s1.Value, s2.Value)), nil
	}
}

func fnAYSRS(fn func([]byte, string) string) CallableFunc {
	return func(args ...Object) (Object, error) {
		if len(args) != 2 {
			return ErrWrongNumArguments.NewError(wantEqXGotY(2, len(args))), nil
		}
		y0, ok := args[0].(Bytes)
		if !ok {
			return NewArgumentTypeError("first", "string", args[0].TypeName()), nil
		}
		s1, ok := args[1].(String)
		if !ok {
			return NewArgumentTypeError("second", "string", args[1].TypeName()), nil
		}
		return ToString(fn([]byte(y0), s1.Value)), nil
	}
}

func fnASSSRS(fn func(string, string, string) string) CallableFunc {
	return func(args ...Object) (Object, error) {
		if len(args) < 3 {
			return ErrWrongNumArguments.NewError(wantEqXGotY(3, len(args))), nil
		}
		s1, ok := args[0].(String)
		if !ok {
			return NewArgumentTypeError("first", "string", args[0].TypeName()), nil
		}
		s2, ok := args[1].(String)
		if !ok {
			return NewArgumentTypeError("second", "string", args[1].TypeName()), nil
		}
		s3, ok := args[2].(String)
		if !ok {
			return NewArgumentTypeError("third", "string", args[2].TypeName()), nil
		}
		return ToString(fn(s1.Value, s2.Value, s3.Value)), nil
	}
}

func fnASSNRS(fn func(string, string, int) string) CallableFunc {
	return func(args ...Object) (Object, error) {
		if len(args) < 3 {
			return ErrWrongNumArguments.NewError(wantEqXGotY(3, len(args))), nil
		}
		s1, ok := args[0].(String)
		if !ok {
			return NewArgumentTypeError("first", "string", args[0].TypeName()), nil
		}
		s2, ok := args[1].(String)
		if !ok {
			return NewArgumentTypeError("second", "string", args[1].TypeName()), nil
		}
		n3, ok := args[2].(Int)
		if !ok {
			return NewArgumentTypeError("third", "int", args[2].TypeName()), nil
		}
		return ToString(fn(s1.Value, s2.Value, int(n3))), nil
	}
}

func fnASSNRAS(fn func(string, string, int) []string) CallableFunc {
	return func(args ...Object) (Object, error) {
		if len(args) < 3 {
			return ErrWrongNumArguments.NewError(wantEqXGotY(3, len(args))), nil
		}
		s1, ok := args[0].(String)
		if !ok {
			return NewArgumentTypeError("first", "string", args[0].TypeName()), nil
		}
		s2, ok := args[1].(String)
		if !ok {
			return NewArgumentTypeError("second", "string", args[1].TypeName()), nil
		}
		n3, ok := args[2].(Int)
		if !ok {
			return NewArgumentTypeError("third", "int", args[2].TypeName()), nil
		}
		return ConvertToObject(fn(s1.Value, s2.Value, int(n3))), nil
	}
}

func fnASSRA2N(fn func(string, string) [][]int) CallableFunc {
	return func(args ...Object) (Object, error) {
		if len(args) < 2 {
			return ErrWrongNumArguments.NewError(wantEqXGotY(2, len(args))), nil
		}
		s1, ok := args[0].(String)
		if !ok {
			return NewArgumentTypeError("first", "string", args[0].TypeName()), nil
		}
		s2, ok := args[1].(String)
		if !ok {
			return NewArgumentTypeError("second", "string", args[1].TypeName()), nil
		}
		return ConvertToObject(fn(s1.Value, s2.Value)), nil
	}
}

func fnASSRA(fn func(string, string) []string) CallableFunc {
	return func(args ...Object) (Object, error) {
		if len(args) < 2 {
			return ErrWrongNumArguments.NewError(wantEqXGotY(2, len(args))), nil
		}
		s1, ok := args[0].(String)
		if !ok {
			return NewArgumentTypeError("first", "string", args[0].TypeName()), nil
		}
		s2, ok := args[1].(String)
		if !ok {
			return NewArgumentTypeError("second", "string", args[1].TypeName()), nil
		}
		return ConvertToObject(fn(s1.Value, s2.Value)), nil
	}
}

func builtinCopyFileFunc(args ...Object) (Object, error) {
	if len(args) < 2 {
		return ToString(tk.ErrStrf("%v", ErrWrongNumArguments.NewError(wantEqXGotY(2, len(args))))), nil
	}

	s1, ok := args[0].(String)
	if !ok {
		return ToString(tk.ErrStrf("%v", NewArgumentTypeError("first", "string", args[0].TypeName()))), nil
	}

	s2, ok := args[1].(String)
	if !ok {
		return ToString(tk.ErrStrf("%v", NewArgumentTypeError("second", "string", args[1].TypeName()))), nil
	}

	errT := tk.CopyFile(s1.Value, s2.Value, ObjectsToS(args[2:])...)

	if errT != nil {
		return ToString(tk.ErrStrf("%v", errT)), nil
	}

	return ToString(""), nil
}

func fnASSBNRE(fn func(string, string, bool, int) error) CallableFunc {
	return func(args ...Object) (Object, error) {
		if len(args) < 4 {
			return ToString(tk.ErrStrf("%v", ErrWrongNumArguments.NewError(wantEqXGotY(4, len(args))))), nil
		}

		s1, ok := args[0].(String)
		if !ok {
			return ToString(tk.ErrStrf("%v", NewArgumentTypeError("first", "string", args[0].TypeName()))), nil
		}

		s2, ok := args[1].(String)
		if !ok {
			return ToString(tk.ErrStrf("%v", NewArgumentTypeError("second", "string", args[1].TypeName()))), nil
		}

		b3, ok := args[2].(Bool)
		if !ok {
			return ToString(tk.ErrStrf("%v", NewArgumentTypeError("third", "bool", args[2].TypeName()))), nil
		}

		n4, ok := args[3].(Int)
		if !ok {
			return ToString(tk.ErrStrf("%v", NewArgumentTypeError("4th", "int", args[3].TypeName()))), nil
		}

		errT := fn(FromString(s1), FromString(s2), bool(b3), int(n4))

		if errT != nil {
			return ToString(tk.ErrStrf("%v", errT)), nil
		}

		return ToString(""), nil
	}
}

func fnASRE(fn func(string) error) CallableFunc {
	return func(args ...Object) (Object, error) {
		if len(args) < 1 {
			return ToString(tk.ErrStrf("%v", ErrWrongNumArguments.NewError(wantEqXGotY(1, len(args))))), nil
		}

		s1, ok := args[0].(String)
		if !ok {
			return ToString(tk.ErrStrf("%v", NewArgumentTypeError("first", "string", args[0].TypeName()))), nil
		}

		errT := fn(FromString(s1))

		if errT != nil {
			return ToString(tk.ErrStrf("%v", errT)), nil
		}

		return ToString(""), nil
	}
}

func fnASSRI(fn func(string, string) interface{}) CallableFunc {
	return func(args ...Object) (Object, error) {
		if len(args) != 2 {
			return ErrWrongNumArguments.NewError(wantEqXGotY(2, len(args))), nil
		}

		s1, ok := args[0].(String)
		if !ok {
			return NewArgumentTypeError("first", "string", args[0].TypeName()), nil
		}

		s2, ok := args[1].(String)
		if !ok {
			return NewArgumentTypeError("second", "string", args[1].TypeName()), nil
		}

		return NewAnyValue(fn(FromString(s1), FromString(s2))), nil
	}
}

func fnASRB(fn func(string) bool) CallableFunc {
	return func(args ...Object) (Object, error) {
		if len(args) != 1 {
			return ErrWrongNumArguments.NewError(wantEqXGotY(1, len(args))), nil
		}

		s1, ok := args[0].(String)
		if !ok {
			return NewArgumentTypeError("first", "string", args[0].TypeName()), nil
		}

		return Bool(fn(FromString(s1))), nil
	}
}

func fnASIVRS(fn func(string, ...interface{}) string) CallableFunc {
	return func(args ...Object) (Object, error) {
		if len(args) < 1 {
			return ErrWrongNumArguments.NewError(wantEqXGotY(1, len(args))), nil
		}

		s1, ok := args[0].(String)
		if !ok {
			return NewArgumentTypeError("first", "string", args[0].TypeName()), nil
		}

		objsT := ObjectsToI(args[1:])

		return ToString(fn(FromString(s1), objsT...)), nil
	}
}

func fnADSIVRI(fn func(*sql.DB, string, ...interface{}) interface{}) CallableFunc {
	return func(args ...Object) (Object, error) {
		if len(args) < 2 {
			return ErrWrongNumArguments.NewError(wantEqXGotY(2, len(args))), nil
		}

		nv, ok := args[0].(Any)

		if !ok {
			return NewCommonError("type error: %T", args[0]), nil
		}

		dbT, ok := nv.Value.(*sql.DB)

		if !ok {
			return NewCommonError("type error: %T", nv.Value), nil
		}

		s1, ok := args[1].(String)
		if !ok {
			return NewArgumentTypeError("second", "string", args[1].TypeName()), nil
		}

		objsT := ObjectsToI(args[2:])

		return ConvertToObject(fn(dbT, s1.Value, objsT...)), nil
	}
}

func fnASNVRI(fn func(string, ...int) interface{}) CallableFunc {
	return func(args ...Object) (Object, error) {
		if len(args) < 1 {
			return ErrWrongNumArguments.NewError(wantEqXGotY(1, len(args))), nil
		}

		s0, ok := args[0].(String)
		if !ok {
			return NewArgumentTypeError("first", "string", args[0].TypeName()), nil
		}

		objsT := ObjectsToN(args[1:])

		return ConvertToObject(fn(s0.Value, objsT...)), nil
	}
}

func fnADSSIVRI(fn func(*sql.DB, string, string, ...interface{}) interface{}) CallableFunc {
	return func(args ...Object) (Object, error) {
		if len(args) < 2 {
			return ErrWrongNumArguments.NewError(wantEqXGotY(2, len(args))), nil
		}

		nv, ok := args[0].(Any)

		if !ok {
			return NewCommonError("type error"), nil
		}

		dbT, ok := nv.Value.(*sql.DB)

		if !ok {
			return NewCommonError("type error for *sql.DB"), nil
		}

		s1, ok := args[1].(String)
		if !ok {
			return NewArgumentTypeError("second", "string",
				args[1].TypeName()), nil
		}

		s2, ok := args[2].(String)
		if !ok {
			return NewArgumentTypeError("third", "string",
				args[2].TypeName()), nil
		}

		objsT := ObjectsToI(args[3:])

		return ConvertToObject(fn(dbT, FromString(s1), FromString(s2), objsT...)), nil
	}
}

func fnASSVRS(fn func(string, ...string) string) CallableFunc {
	return func(args ...Object) (Object, error) {
		if len(args) < 1 {
			return ErrWrongNumArguments.NewError(
				wantEqXGotY(1, len(args))), nil
		}

		s1, ok := args[0].(String)
		if !ok {
			return NewArgumentTypeError("first", "string",
				args[0].TypeName()), nil
		}

		objsT := ObjectsToS(args[1:])

		return ToString(fn(FromString(s1), objsT...)), nil
	}
}

func fnASYVRS(fn func(string, ...byte) string) CallableFunc {
	return func(args ...Object) (Object, error) {
		if len(args) < 1 {
			return ErrWrongNumArguments.NewError(
				wantEqXGotY(1, len(args))), nil
		}

		s1, ok := args[0].(String)
		if !ok {
			return NewArgumentTypeError("first", "string",
				args[0].TypeName()), nil
		}

		objsT := ObjectsToBytes(args[1:])

		return ToString(fn(s1.Value, objsT...)), nil
	}
}

func fnASSVRS_safely(fn func(string, ...string) string) CallableFunc {
	return func(args ...Object) (Object, error) {
		if len(args) < 1 {
			return ToString(""), nil
		}

		s1, ok := args[0].(String)
		if !ok {
			if args[0].TypeName() == "undefined" {
				s1 = ToString("")
			} else {
				s1 = ToString(args[0].String())
			}
			// return NewArgumentTypeError("first", "string",
			// 	args[0].TypeName()), nil
		}

		objsT := ObjectsToS(args[1:])

		return ToString(fn(FromString(s1), objsT...)), nil
	}
}

func fnASVRS(fn func(...string) string) CallableFunc {
	return func(args ...Object) (Object, error) {
		objsT := ObjectsToS(args)

		return ToString(fn(objsT...)), nil
	}
}

func fnASSVRB(fn func(string, ...string) bool) CallableFunc {
	return func(args ...Object) (Object, error) {
		if len(args) < 1 {
			return ErrWrongNumArguments.NewError(
				wantEqXGotY(1, len(args))), nil
		}

		s1, ok := args[0].(String)
		if !ok {
			return NewArgumentTypeError("first", "string",
				args[0].TypeName()), nil
		}

		objsT := ObjectsToS(args[1:])

		return Bool(fn(FromString(s1), objsT...)), nil
	}
}

func fnASSSVRE(fn func(string, string, ...string) error) CallableFunc {
	return func(args ...Object) (Object, error) {
		if len(args) < 2 {
			return ToString(tk.ErrStrf("%v", ErrWrongNumArguments.NewError(wantEqXGotY(2, len(args))))), nil
		}

		s1, ok := args[0].(String)
		if !ok {
			return ToString(tk.ErrStrf("%v", NewArgumentTypeError("first", "string", args[0].TypeName()))), nil
		}

		s2, ok := args[1].(String)
		if !ok {
			return ToString(tk.ErrStrf("%v", NewArgumentTypeError("second", "string", args[1].TypeName()))), nil
		}

		objsT := ObjectsToS(args[2:])

		errT := fn(FromString(s1), FromString(s2), objsT...)

		if errT != nil {
			return ToString(tk.ErrStrf("%v", errT)), nil
		}

		return ToString(""), nil
	}
}

func fnASSVRMSSR(fn func(string, ...string) []map[string]string) CallableFunc {
	return func(args ...Object) (Object, error) {
		if len(args) < 1 {
			return ErrWrongNumArguments.NewError(wantEqXGotY(1, len(args))), nil
		}

		s1, ok := args[0].(String)
		if !ok {
			return NewArgumentTypeError("first", "string", args[0].TypeName()), nil
		}

		objsT := ObjectsToS(args[1:])

		return ConvertToObject(fn(FromString(s1), objsT...)), nil
	}
}

func fnAIV(fn func(...interface{})) CallableFunc {
	return func(args ...Object) (Object, error) {
		objsT := ObjectsToI(args)

		fn(objsT...)

		return Undefined, nil
	}
}

func fnRS(fn func() string) CallableFunc {
	return func(args ...Object) (Object, error) {
		return ToString(fn()), nil
	}
}

func fnASSRB(fn func(string, string) bool) CallableFunc {
	return func(args ...Object) (Object, error) {
		if len(args) != 2 {
			return ErrWrongNumArguments.NewError(wantEqXGotY(2, len(args))), nil
		}
		s1, ok := args[0].(String)
		if !ok {
			return NewArgumentTypeError("first", "string", args[0].TypeName()), nil
		}
		s2, ok := args[1].(String)
		if !ok {
			return NewArgumentTypeError("second", "string", args[1].TypeName()), nil
		}
		return Bool(fn(FromString(s1), FromString(s2))), nil
	}
}
