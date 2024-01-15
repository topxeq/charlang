package charlang

import (
	"bytes"
	"compress/flate"
	"database/sql"
	"encoding/base64"
	"encoding/csv"
	"errors"
	"fmt"
	"io"
	"math"
	"math/big"
	"net/http"
	"net/url"
	"os"
	"path/filepath"
	"reflect"
	"sort"
	"strconv"
	"strings"
	"time"
	"unicode/utf8"

	"github.com/topxeq/awsapi"
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
type BuiltinType int

// Builtins
const (
	BuiltinAppend BuiltinType = iota

	BuiltinClose
	BuiltinRegSplit
	BuiltinReadCsv
	BuiltinWriteCsv
	BuiltinRemovePath
	BuiltinRemoveDir
	BuiltinGetInput
	BuiltinRegCount
	BuiltinStrRepeat
	BuiltinRegMatch
	BuiltinRegContains
	BuiltinLePrint
	BuiltinLeFindLines
	BuiltinLeFind
	BuiltinLeFindAll
	BuiltinLeReplace
	BuiltinLeLoadFromSsh
	BuiltinLeSaveToSsh
	BuiltinLeRemoveLine
	BuiltinLeRemoveLines
	BuiltinLeInsertLine
	BuiltinLeAppendLine
	BuiltinLeGetLine
	BuiltinLeSetLine
	BuiltinLeSetLines
	BuiltinLeConvertToUtf8
	BuiltinLeSort
	BuiltinLeViewAll
	BuiltinLeViewLine
	BuiltinLeViewLines
	BuiltinLeLoadFromUrl
	BuiltinLeLoadFromClip
	BuiltinLeSaveToClip
	BuiltinLeAppendFromFile
	BuiltinLeAppendToFile
	BuiltinLeLoadFromFile
	BuiltinLeSaveToFile
	BuiltinLeLoadFromStr
	BuiltinLeSaveToStr
	BuiltinLeGetList
	BuiltinLeAppendFromStr
	BuiltinLeClear
	BuiltinAwsSign
	BuiltinNow
	BuiltinTimeToTick
	BuiltinGetNowTimeStamp
	BuiltinBase64Encode
	BuiltinBase64Decode
	BuiltinXmlGetNodeStr
	BuiltinStrXmlEncode
	BuiltinMd5
	BuiltinPostRequest
	BuiltinHttpRedirect
	BuiltinReader
	BuiltinWriter
	BuiltinFile
	BuiltinImage
	BuiltinLoadImageFromBytes
	BuiltinThumbImage
	BuiltinBytesStartsWith
	BuiltinBytesEndsWith
	BuiltinEncryptData
	BuiltinDecryptData
	BuiltinSimpleEncode
	BuiltinSimpleDecode
	BuiltinToPinyin
	BuiltinIsHttps
	BuiltinRenameFile
	BuiltinStrJoin
	BuiltinStrCount
	BuiltinStrPad
	BuiltinStrIn
	BuiltinEnsureMakeDirs
	BuiltinExtractFileDir
	BuiltinCheckToken
	BuiltinEncryptText
	BuiltinDecryptText
	BuiltinHtmlEncode
	BuiltinHtmlDecode
	BuiltinServeFile
	BuiltinGetFileAbs
	BuiltinGetFileRel
	BuiltinGetFileExt
	BuiltinGetMimeType
	BuiltinRenderMarkdown
	BuiltinIsDir
	BuiltinStrStartsWith
	BuiltinStrEndsWith
	BuiltinStrSplit
	BuiltinGenToken
	BuiltinStrContains
	BuiltinGetNowStr
	BuiltinGetNowStrCompact
	BuiltinLock
	BuiltinUnlock
	BuiltinTryLock
	BuiltinRLock
	BuiltinRUnlock
	BuiltinTryRLock
	BuiltinToKMG
	BuiltinGetFileList
	BuiltinMathSqrt
	BuiltinAdjustFloat
	BuiltinBigInt
	BuiltinBigFloat
	BuiltinToOrderedMap
	BuiltinUnhex
	BuiltinBitNot
	BuiltinToUpper
	BuiltinToLower
	BuiltinSscanf
	BuiltinStrQuote
	BuiltinStrUnquote
	BuiltinToInt
	BuiltinToFloat
	BuiltinToHex
	BuiltinCompareBytes
	BuiltinLoadBytes
	BuiltinSaveBytes
	BuiltinUrlEncode
	BuiltinUrlDecode
	BuiltinOrderedMap
	BuiltinArrayContains
	// BuiltinSortByFunc
	BuiltintLimitStr
	BuiltintStrFindDiffPos
	BuiltinRemoveItems
	BuiltinAppendList
	BuiltinGetRandomInt
	BuiltinGetRandomFloat
	BuiltinGetRandomStr
	BuiltinFormatSQLValue
	BuiltinDbClose
	BuiltinDbQuery
	BuiltinDbQueryOrdered
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
	BuiltinDelegate
	BuiltinGetReqBody
	BuiltinGetReqHeader
	BuiltinWriteRespHeader
	BuiltinSetRespHeader
	BuiltinParseReqForm
	BuiltinParseReqFormEx
	BuiltinWriteResp
	BuiltinMux
	BuiltinMutex
	BuiltinHttpHandler
	BuiltinFatalf
	BuiltinSeq
	BuiltinIsNil
	BuiltinIsNilOrEmpty
	BuiltinIsNilOrErr
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
	BuiltintReadAllStr
	BuiltintReadAllBytes
	BuiltintWriteStr
	BuiltintWriteBytes
	BuiltintStrSplitLines
	BuiltinNew
	BuiltinStringBuilder
	BuiltinStrReplace
	BuiltinGetErrStrX
	BuiltinSshUpload
	BuiltinArchiveFilesToZip
	BuiltinGetOSName
	BuiltinGetOSArch
	BuiltinGetOSArgs
	BuiltinGetAppDir
	BuiltinGetCurDir
	BuiltinGetHomeDir
	BuiltinGetTempDir
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
	BuiltinRegFindAllGroups
	BuiltinCheckErrX
	BuiltinLoadText
	BuiltinSaveText
	BuiltinAppendText
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
	BuiltinPlErr
	BuiltinGetParam
	BuiltinGetParams
	BuiltinGetSwitch
	BuiltinGetSwitches
	BuiltinGetIntSwitch
	BuiltinIfSwitchExists
	BuiltinTypeCode
	BuiltinTypeName
	BuiltinPl
	BuiltinPrf
	BuiltinPln
	BuiltinPlv
	BuiltinSpr
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
	BuiltinIsByte
	BuiltinIsBool
	BuiltinIsString
	BuiltinIsBytes
	BuiltinIsChars
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

	// the "testBy*" functions are builtin test functions for internal use only, see testAll.char for usage examples

	"testByText":        BuiltinTestByText, // usage: testByText(strToTest, strToCompare, indexInteger, scriptFileName)
	"testByStartsWith":  BuiltinTestByStartsWith,
	"testByEndsWith":    BuiltinTestByEndsWith,
	"testByContains":    BuiltinTestByContains,
	"testByReg":         BuiltinTestByReg,
	"testByRegContains": BuiltinTestByRegContains,

	"dumpVar":   BuiltinDumpVar,
	"debugInfo": BuiltinDebugInfo,

	// infrastructure related
	"globals": BuiltinGlobals,

	"len": BuiltinLen,

	":makeArray": BuiltinMakeArray,

	// data type related
	"typeCode":  BuiltinTypeCode,
	"typeName":  BuiltinTypeName,
	"typeOf":    BuiltinTypeName,
	"typeOfAny": BuiltinTypeOfAny,

	"any": BuiltinAny,

	"bool":  BuiltinBool,
	"byte":  BuiltinByte,
	"char":  BuiltinChar,
	"int":   BuiltinInt,
	"uint":  BuiltinUint,
	"float": BuiltinFloat,

	"bigInt":   BuiltinBigInt,
	"bigFloat": BuiltinBigFloat,

	"string":        BuiltinString,
	"mutableString": BuiltinMutableString,
	"bytes":         BuiltinBytes,
	"chars":         BuiltinChars,

	"error": BuiltinError,

	"time":          BuiltinTime,
	"stringBuilder": BuiltinStringBuilder,
	"orderedMap":    BuiltinOrderedMap,

	"statusResult": BuiltinStatusResult,
	"seq":          BuiltinSeq,
	"mutex":        BuiltinMutex,

	"mux":         BuiltinMux,
	"httpHandler": BuiltinHttpHandler,

	"reader": BuiltinReader,
	"writer": BuiltinWriter,

	"file": BuiltinFile,

	"image": BuiltinImage,

	"charCode": BuiltinCharCode,
	"gel":      BuiltinGel,
	"delegate": BuiltinDelegate,

	"database": BuiltinDatabase,

	// new related
	"make": BuiltinMake,
	"new":  BuiltinNew,

	"newEx":  BuiltinNewEx,
	"newObj": BuiltinNewEx,

	// bitwise related
	"bitNot": BuiltinBitNot,
	"not":    BuiltinBitNot,

	// array/map related
	"append":      BuiltinAppend,
	"appendList":  BuiltinAppendList,
	"appendArray": BuiltinAppendList,
	"appendSlice": BuiltinAppendList,
	"delete":      BuiltinDelete,

	"removeItems": BuiltinRemoveItems, // inclusive

	"arrayContains": BuiltinArrayContains,

	"toOrderedMap": BuiltinToOrderedMap,

	// ref/pointer related
	"unref":         BuiltinUnref,
	"setValueByRef": BuiltinSetValueByRef,

	// convert related
	"toStr":   BuiltinToStr,
	"toInt":   BuiltinToInt,
	"toFloat": BuiltinToFloat,
	"toTime":  BuiltinToTime,
	"toHex":   BuiltinToHex,
	"unhex":   BuiltinUnhex,
	"toKMG":   BuiltinToKMG,

	// string related
	"trim":          BuiltinTrim,
	"strTrim":       BuiltinStrTrim,
	"strTrimStart":  BuiltinStrTrimStart,
	"strTrimEnd":    BuiltinStrTrimEnd,
	"strTrimLeft":   BuiltinStrTrimLeft,
	"strTrimRight":  BuiltinStrTrimRight,
	"toUpper":       BuiltinToUpper,
	"strToUpper":    BuiltinToUpper,
	"toLower":       BuiltinToLower,
	"strToLower":    BuiltinToLower,
	"strContains":   BuiltinStrContains,
	"strStartsWith": BuiltinStrStartsWith,
	"strEndsWith":   BuiltinStrEndsWith,
	"strReplace":    BuiltinStrReplace,
	"strSplit":      BuiltinStrSplit,
	"strSplitLines": BuiltintStrSplitLines,
	"strJoin":       BuiltinStrJoin,
	"strRepeat":     BuiltinStrRepeat,
	"strCount":      BuiltinStrCount,
	"strPad":        BuiltinStrPad,

	"strIn": BuiltinStrIn,

	"strFindDiffPos": BuiltintStrFindDiffPos, // return -1 if 2 strings are identical

	"limitStr": BuiltintLimitStr,

	"strQuote":   BuiltinStrQuote,
	"strUnquote": BuiltinStrUnquote,

	// regex related
	"regMatch":    BuiltinRegMatch,
	"regContains": BuiltinRegContains,

	"regFindFirst":       BuiltinRegFindFirst,
	"regFindFirstGroups": BuiltintRegFindFirstGroups, // obtain the first match of a regular expression and return a list of all matching groups, where the first item is the complete matching result and the second item is the first matching group..., usage example: result := regFindFirstGroups(str1, regex1)
	"regFindAll":         BuiltinRegFindAll,
	"regFindAllGroups":   BuiltinRegFindAllGroups,
	"regQuote":           BuiltinRegQuote,
	"regReplace":         BuiltinRegReplace,
	"regCount":           BuiltinRegCount,
	"regSplit":           BuiltinRegSplit,

	// math related
	"adjustFloat": BuiltinAdjustFloat,

	"mathSqrt": BuiltinMathSqrt,

	// random related
	"getRandomInt":   BuiltinGetRandomInt,
	"getRandomFloat": BuiltinGetRandomFloat,
	"getRandomStr":   BuiltinGetRandomStr,
	"genRandomStr":   BuiltinGetRandomStr,

	// time related
	"now":              BuiltinNow,
	"getNowStr":        BuiltinGetNowStr,
	"getNowStrCompact": BuiltinGetNowStrCompact,
	"getNowTimeStamp":  BuiltinGetNowTimeStamp,
	"timeToTick":       BuiltinTimeToTick,

	// binary/bytes related
	"bytesStartsWith": BuiltinBytesStartsWith,
	"bytesEndsWith":   BuiltinBytesEndsWith,

	// compare related
	"compareBytes": BuiltinCompareBytes,

	// control related
	"isNil":        BuiltinIsNil,
	"isNilOrEmpty": BuiltinIsNilOrEmpty,
	"isNilOrErr":   BuiltinIsNilOrErr,
	"isUndefined":  BuiltinIsUndefined,
	"isUndef":      BuiltinIsUndefined,
	"isBool":       BuiltinIsBool,
	"isByte":       BuiltinIsByte,
	"isChar":       BuiltinIsChar,
	"isInt":        BuiltinIsInt,
	"isUint":       BuiltinIsUint,
	"isFloat":      BuiltinIsFloat,
	"isString":     BuiltinIsString,
	"isBytes":      BuiltinIsBytes,
	"isChars":      BuiltinIsChars,
	"isArray":      BuiltinIsArray,
	"isMap":        BuiltinIsMap,
	"isSyncMap":    BuiltinIsSyncMap,
	"isError":      BuiltinIsError,
	"isFunction":   BuiltinIsFunction,
	"isCallable":   BuiltinIsCallable,
	"isIterable":   BuiltinIsIterable,

	"exit": BuiltinExit,

	"pass": BuiltinPass,

	// error related
	"isErrX":     BuiltinIsErrX,
	"isErr":      BuiltinIsErrX,
	"getErrStrX": BuiltinGetErrStrX,
	"getErrStr":  BuiltinGetErrStrX,

	"checkErrX": BuiltinCheckErrX,
	"checkErr":  BuiltinCheckErrX,

	"errStrf": BuiltinErrStrf,

	// output/print related
	"prf":    BuiltinPrf,
	"pl":     BuiltinPl,
	"pln":    BuiltinPln,
	"plv":    BuiltinPlv,
	"plt":    BuiltinPlt,
	"plo":    BuiltinPlo,
	"plErr":  BuiltinPlErr,
	"fatalf": BuiltinFatalf,
	"spr":    BuiltinSpr,

	// scan related
	"sscanf": BuiltinSscanf,

	// resource related
	"getNamedValue": BuiltinGetNamedValue,
	"getConst":      BuiltinGetNamedValue,

	"callNamedFunc":    BuiltinCallNamedFunc,
	"callInternalFunc": BuiltinCallInternalFunc,

	// member/method related
	"getValue": BuiltinGetValue,
	"setValue": BuiltinSetValue,

	"getMember": BuiltinGetMember,
	"mb":        BuiltinGetMember,
	"setMember": BuiltinSetMember,

	"callMethod":   BuiltinCallMethod,
	"mt":           BuiltinCallMethod,
	"callMethodEx": BuiltinCallMethodEx,
	"mtEx":         BuiltinCallMethodEx,

	// open/close related
	"close": BuiltinClose,

	// read/write related
	"readAllStr":   BuiltintReadAllStr,
	"readAllBytes": BuiltintReadAllBytes,
	"writeStr":     BuiltintWriteStr,
	"writeBytes":   BuiltintWriteBytes,

	// encode/decode related
	"md5": BuiltinMd5,

	"urlEncode":    BuiltinUrlEncode,
	"urlDecode":    BuiltinUrlDecode,
	"htmlEncode":   BuiltinHtmlEncode,
	"htmlDecode":   BuiltinHtmlDecode,
	"simpleEncode": BuiltinSimpleEncode,
	"simpleDecode": BuiltinSimpleDecode,

	"base64Encode": BuiltinBase64Encode,
	"base64Decode": BuiltinBase64Decode,

	"toJSON":   BuiltinToJSON,
	"toJson":   BuiltinToJSON,
	"fromJSON": BuiltinFromJSON,
	"fromJson": BuiltinFromJSON,

	// XML related
	"xmlEncodeStr":  BuiltinStrXmlEncode,
	"xmlGetNodeStr": BuiltinXmlGetNodeStr,

	// command-line related
	"ifSwitchExists": BuiltinIfSwitchExists,
	"getSwitch":      BuiltinGetSwitch,
	"getIntSwitch":   BuiltinGetIntSwitch,
	"getSwitches":    BuiltinGetSwitches,
	"getParam":       BuiltinGetParam,
	"getParams":      BuiltinGetParams,

	// clipboard related
	"getClipText": BuiltinGetClipText,
	"setClipText": BuiltinSetClipText,

	// thread related
	"sleep": BuiltinSleep,

	"lock":     BuiltinLock,
	"unlock":   BuiltinUnlock,
	"rLock":    BuiltinRLock,
	"rUnlock":  BuiltinRUnlock,
	"tryLock":  BuiltinTryLock,
	"tryRLock": BuiltinTryRLock,

	// os/system related
	"systemCmd": BuiltinSystemCmd,

	"getEnv": BuiltinGetEnv,
	"setEnv": BuiltinSetEnv,

	"getOSName": BuiltinGetOSName,
	"getOsName": BuiltinGetOSName,
	"getOSArch": BuiltinGetOSArch,
	"getOsArch": BuiltinGetOSArch,

	"getOSArgs": BuiltinGetOSArgs,
	"getOsArgs": BuiltinGetOSArgs,

	"getAppDir":  BuiltinGetAppDir,
	"getCurDir":  BuiltinGetCurDir,
	"getHomeDir": BuiltinGetHomeDir,
	"getTempDir": BuiltinGetTempDir,

	"getInput": BuiltinGetInput,

	// dir/path related
	"joinPath": BuiltinJoinPath, // join multiple file paths into one, equivalent to path/filepath.Join in the Go language standard library

	"isDir": BuiltinIsDir,

	"ensureMakeDirs": BuiltinEnsureMakeDirs,

	"getFileList": BuiltinGetFileList,
	"genFileList": BuiltinGetFileList,

	// file related
	"fileExists":   BuiltinFileExists,
	"ifFileExists": BuiltinFileExists,

	"getFileAbs": BuiltinGetFileAbs,
	"getFileExt": BuiltinGetFileExt,
	"getFileRel": BuiltinGetFileRel,

	"extractFileDir": BuiltinExtractFileDir,

	"renameFile": BuiltinRenameFile,
	"removeFile": BuiltinRemoveFile,
	"removeDir":  BuiltinRemoveDir,
	"removePath": BuiltinRemovePath,

	"loadText":   BuiltinLoadText,
	"saveText":   BuiltinSaveText,
	"appendText": BuiltinAppendText,

	"loadBytes": BuiltinLoadBytes,
	"saveBytes": BuiltinSaveBytes,

	// compress/zip related
	"archiveFilesToZip": BuiltinArchiveFilesToZip, // Add multiple files to a newly created zip file. The first parameter is the zip file name, with a suffix of '.zip'. Optional parameters include '-overwrite' (whether to overwrite existing files) and '-makeDirs' (whether to create a new directory as needed). Other parameters are treated as files or directories to be added, and the directory will be recursively added to the zip file. If the parameter is a list, it will be treated as a list of file names, and all files in it will be added

	// network/web related
	"getWeb": BuiltinGetWeb,

	"postRequest": BuiltinPostRequest,

	"urlExists": BuiltinUrlExists,

	"isHttps": BuiltinIsHttps,

	"httpRedirect": BuiltinHttpRedirect,

	// server/service related
	"getReqHeader":    BuiltinGetReqHeader,
	"getReqBody":      BuiltinGetReqBody,
	"parseReqForm":    BuiltinParseReqForm,
	"parseReqFormEx":  BuiltinParseReqFormEx,
	"writeRespHeader": BuiltinWriteRespHeader,
	"setRespHeader":   BuiltinSetRespHeader,
	"genJSONResp":     BuiltinGenJSONResp,
	"genJsonResp":     BuiltinGenJSONResp,
	"genResp":         BuiltinGenJSONResp,
	"writeResp":       BuiltinWriteResp,

	"serveFile": BuiltinServeFile,

	"getMimeType": BuiltinGetMimeType,

	// security related
	"genToken":   BuiltinGenToken,
	"checkToken": BuiltinCheckToken,

	"encryptText": BuiltinEncryptText,
	"decryptText": BuiltinDecryptText,

	"encryptData": BuiltinEncryptData,
	"decryptData": BuiltinDecryptData,

	// ssh related
	"sshUpload": BuiltinSshUpload,

	// eTable related
	"readCsv":  BuiltinReadCsv,
	"writeCsv": BuiltinWriteCsv,

	// database related
	"formatSQLValue": BuiltinFormatSQLValue,

	"dbConnect": BuiltinDatabase,
	"dbClose":   BuiltinDbClose,

	"dbQuery":         BuiltinDbQuery,
	"dbQueryOrdered":  BuiltinDbQueryOrdered,
	"dbQueryRecs":     BuiltinDbQueryRecs,
	"dbQueryMap":      BuiltinDbQueryMap,
	"dbQueryMapArray": BuiltinDbQueryMapArray,
	"dbQueryCount":    BuiltinDbQueryCount,
	"dbQueryFloat":    BuiltinDbQueryFloat,
	"dbQueryString":   BuiltinDbQueryString,
	"dbQueryStr":      BuiltinDbQueryString,

	"dbExec": BuiltinDbExec,

	// unicode related
	"toPinyin": BuiltinToPinyin,

	// line editor related
	"leClear":          BuiltinLeClear,
	"leLoadFromStr":    BuiltinLeLoadFromStr,
	"leAppendFromStr":  BuiltinLeAppendFromStr,
	"leSaveToStr":      BuiltinLeSaveToStr,
	"leToStr":          BuiltinLeSaveToStr,
	"leLoadFromFile":   BuiltinLeLoadFromFile,
	"leAppendFromFile": BuiltinLeAppendFromFile,
	"leSaveToFile":     BuiltinLeSaveToFile,
	"leAppendToFile":   BuiltinLeAppendToFile,
	"leLoadFromClip":   BuiltinLeLoadFromClip,
	"leSaveToClip":     BuiltinLeSaveToClip,
	"leLoadFromUrl":    BuiltinLeLoadFromUrl,
	"leLoadFromSsh":    BuiltinLeLoadFromSsh,
	"leSaveToSsh":      BuiltinLeSaveToSsh,
	"leViewAll":        BuiltinLeViewAll,
	"leViewLine":       BuiltinLeViewLine,
	"leViewLines":      BuiltinLeViewLines,
	"leSort":           BuiltinLeSort,
	"leConvertToUtf8":  BuiltinLeConvertToUtf8,
	"leGetLine":        BuiltinLeGetLine,
	"leSetLine":        BuiltinLeSetLine,
	"leSetLines":       BuiltinLeSetLines,
	"leInsertLine":     BuiltinLeInsertLine,
	"leAppendLine":     BuiltinLeAppendLine,
	"leRemoveLine":     BuiltinLeRemoveLine,
	"leRemoveLines":    BuiltinLeRemoveLines,
	"leFindLines":      BuiltinLeFindLines,
	"leFind":           BuiltinLeFind,
	"leFindAll":        BuiltinLeFindAll,
	"leReplace":        BuiltinLeReplace,
	"lePrint":          BuiltinLePrint,
	"leGetList":        BuiltinLeGetList,

	// 3rd party related
	"awsSign": BuiltinAwsSign,

	// misc related
	"getSeq": BuiltinGetSeq,

	"renderMarkdown": BuiltinRenderMarkdown,

	// "sortByFunc": BuiltinSortByFunc,

	// original internal related
	"copy":        BuiltinCopy,
	"repeat":      BuiltinRepeat,
	"contains":    BuiltinContains,
	"sort":        BuiltinSort,
	"sortReverse": BuiltinSortReverse,
	"cap":         BuiltinCap,

	"printf":  BuiltinPrintf,
	"println": BuiltinPrintln,
	"sprintf": BuiltinSprintf,

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

	// funcs end

}

// BuiltinObjects is list of builtins, exported for REPL.
var BuiltinObjects = [...]Object{
	// funcs detail start

	// internal & debug related
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
	BuiltinTestByReg: &BuiltinFunction{
		Name:    "testByReg",
		Value:   CallExAdapter(builtinTestByRegFunc),
		ValueEx: builtinTestByRegFunc,
	},
	BuiltinTestByRegContains: &BuiltinFunction{
		Name:    "testByRegContains",
		Value:   CallExAdapter(builtinTestByRegContainsFunc),
		ValueEx: builtinTestByRegContainsFunc,
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

	// infrastructure related
	BuiltinGlobals: &BuiltinFunction{
		Name:    "globals",
		Value:   CallExAdapter(builtinGlobalsFunc),
		ValueEx: builtinGlobalsFunc,
	},
	BuiltinLen: &BuiltinFunction{
		Name:    "len",
		Value:   funcPORO(builtinLenFunc),
		ValueEx: funcPOROEx(builtinLenFunc),
	},

	// :makeArray is a private builtin function to help destructuring array assignments
	BuiltinMakeArray: &BuiltinFunction{
		Name:    ":makeArray",
		Value:   funcPiOROe(builtinMakeArrayFunc),
		ValueEx: funcPiOROeEx(builtinMakeArrayFunc),
		Remark:  `this function is only for internal use`,
	},

	// data type related
	BuiltinTypeCode: &BuiltinFunction{
		Name:    "typeCode",
		Value:   CallExAdapter(builtinTypeCodeFunc),
		ValueEx: builtinTypeCodeFunc,
	},
	BuiltinTypeName: &BuiltinFunction{
		Name:    "typeName",
		Value:   funcPORO(builtinTypeNameFunc),
		ValueEx: funcPOROEx(builtinTypeNameFunc),
	},
	BuiltinTypeOfAny: &BuiltinFunction{
		Name:    "typeOfAny",
		Value:   CallExAdapter(builtinTypeOfAnyFunc),
		ValueEx: builtinTypeOfAnyFunc,
	},

	BuiltinAny: &BuiltinFunction{
		Name:    "any",
		Value:   CallExAdapter(builtinAnyFunc),
		ValueEx: builtinAnyFunc,
	},

	BuiltinBool: &BuiltinFunction{
		Name:    "bool",
		Value:   funcPORO(builtinBoolFunc),
		ValueEx: funcPOROEx(builtinBoolFunc),
	},
	BuiltinByte: &BuiltinFunction{
		Name:    "byte",
		Value:   CallExAdapter(builtinByteFunc),
		ValueEx: builtinByteFunc,
	},
	BuiltinChar: &BuiltinFunction{
		Name:    "char",
		Value:   funcPOROe(builtinCharFunc),
		ValueEx: funcPOROeEx(builtinCharFunc),
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

	BuiltinBigInt: &BuiltinFunction{
		Name:    "bigInt",
		Value:   CallExAdapter(builtinBigIntFunc),
		ValueEx: builtinBigIntFunc,
	},
	BuiltinBigFloat: &BuiltinFunction{
		Name:    "bigFloat",
		Value:   CallExAdapter(builtinBigFloatFunc),
		ValueEx: builtinBigFloatFunc,
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

	BuiltinError: &BuiltinFunction{
		Name:    "error",
		Value:   funcPORO(builtinErrorFunc),
		ValueEx: funcPOROEx(builtinErrorFunc),
	},

	BuiltinTime: &BuiltinFunction{
		Name:    "time", // new a Time object
		Value:   CallExAdapter(builtinTimeFunc),
		ValueEx: builtinTimeFunc,
	},

	BuiltinStringBuilder: &BuiltinFunction{
		Name:    "stringBuilder",
		Value:   CallExAdapter(builtinStringBuilderFunc),
		ValueEx: builtinStringBuilderFunc,
	},

	BuiltinOrderedMap: &BuiltinFunction{
		Name:    "orderedMap",
		Value:   NewOrderedMap,
		ValueEx: builtinOrderedMapFunc,
	},

	BuiltinStatusResult: &BuiltinFunction{
		Name:    "statusResult",
		Value:   CallExAdapter(builtinStatusResultFunc),
		ValueEx: builtinStatusResultFunc,
	},

	BuiltinSeq: &BuiltinFunction{
		Name:    "seq",
		Value:   CallExAdapter(builtinSeqFunc),
		ValueEx: builtinSeqFunc,
	},

	BuiltinMutex: &BuiltinFunction{
		Name:    "mutex",
		Value:   CallExAdapter(builtinMutexFunc),
		ValueEx: builtinMutexFunc,
	},
	BuiltinMux: &BuiltinFunction{
		Name:    "mux",
		Value:   CallExAdapter(builtinMuxFunc),
		ValueEx: builtinMuxFunc,
	},
	BuiltinHttpHandler: &BuiltinFunction{
		Name:    "httpHandler",
		Value:   CallExAdapter(builtinHttpHandlerFunc),
		ValueEx: builtinHttpHandlerFunc,
	},

	BuiltinImage: &BuiltinFunction{
		Name:    "image",
		Value:   CallExAdapter(builtinImageFunc),
		ValueEx: builtinImageFunc,
	},

	BuiltinReader: &BuiltinFunction{
		Name:    "reader",
		Value:   CallExAdapter(builtinReaderFunc),
		ValueEx: builtinReaderFunc,
	},

	BuiltinWriter: &BuiltinFunction{
		Name:    "writer",
		Value:   CallExAdapter(builtinWriterFunc),
		ValueEx: builtinWriterFunc,
	},

	BuiltinFile: &BuiltinFunction{
		Name:    "file",
		Value:   CallExAdapter(builtinFileFunc),
		ValueEx: builtinFileFunc,
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
	BuiltinDelegate: &BuiltinFunction{
		Name:    "delegate",
		Value:   CallExAdapter(BuiltinDelegateFunc),
		ValueEx: BuiltinDelegateFunc,
	},

	BuiltinDatabase: &BuiltinFunction{
		Name:    "database",
		Value:   CallExAdapter(builtinDatabaseFunc),
		ValueEx: builtinDatabaseFunc,
	},

	BuiltinMake: &BuiltinFunction{
		Name:    "make",
		Value:   CallExAdapter(builtinMakeFunc),
		ValueEx: builtinMakeFunc,
	},
	BuiltinNew: &BuiltinFunction{
		Name:    "new",
		Value:   CallExAdapter(builtinNewFunc),
		ValueEx: builtinNewFunc,
	},
	BuiltinNewEx: &BuiltinFunction{
		Name:    "newEx",
		Value:   CallExAdapter(builtinNewExFunc),
		ValueEx: builtinNewExFunc,
	},

	// bitwise related
	BuiltinBitNot: &BuiltinFunction{
		Name:    "bitNot",
		Value:   CallExAdapter(builtinBitNotFunc),
		ValueEx: builtinBitNotFunc,
	},

	// array/map related
	BuiltinAppend: &BuiltinFunction{
		Name:    "append",
		Value:   CallExAdapter(builtinAppendFunc),
		ValueEx: builtinAppendFunc,
	},
	BuiltinAppendList: &BuiltinFunction{
		Name:    "appendList",
		Value:   CallExAdapter(builtinAppendListFunc),
		ValueEx: builtinAppendListFunc,
	},
	BuiltinDelete: &BuiltinFunction{
		Name:    "delete",
		Value:   funcPOsRe(builtinDeleteFunc),
		ValueEx: funcPOsReEx(builtinDeleteFunc),
	},
	BuiltinRemoveItems: &BuiltinFunction{
		Name:    "removeItems",
		Value:   CallExAdapter(builtinRemoveItemsFunc),
		ValueEx: builtinRemoveItemsFunc,
	},
	BuiltinArrayContains: &BuiltinFunction{
		Name:    "arrayContains",
		Value:   CallExAdapter(builtinArrayContainsFunc),
		ValueEx: builtinArrayContainsFunc,
	},
	BuiltinToOrderedMap: &BuiltinFunction{
		Name:    "toOrderedMap",
		Value:   CallExAdapter(builtinToOrderedMapFunc),
		ValueEx: builtinToOrderedMapFunc,
	},

	// ref/pointer related
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

	// convert related
	BuiltinToStr: &BuiltinFunction{
		Name:    "toStr", // usage: toStr(any)
		Value:   CallExAdapter(builtinToStrFunc),
		ValueEx: builtinToStrFunc,
	},
	BuiltinToInt: &BuiltinFunction{
		Name:    "toInt",
		Value:   CallExAdapter(builtinToIntFunc),
		ValueEx: builtinToIntFunc,
	},
	BuiltinToFloat: &BuiltinFunction{
		Name:    "toFloat",
		Value:   CallExAdapter(builtinToFloatFunc),
		ValueEx: builtinToFloatFunc,
	},
	BuiltinToTime: &BuiltinFunction{
		Name:    "toTime", // new a Time object
		Value:   CallExAdapter(builtinTimeFunc),
		ValueEx: builtinTimeFunc,
	},
	BuiltinToHex: &BuiltinFunction{
		Name:    "toHex",
		Value:   CallExAdapter(builtinToHexFunc),
		ValueEx: builtinToHexFunc,
	},
	BuiltinUnhex: &BuiltinFunction{
		Name:    "unhex",
		Value:   FnASRLby(tk.HexToBytes),
		ValueEx: FnASRLbyex(tk.HexToBytes),
	},
	BuiltinToKMG: &BuiltinFunction{
		Name:    "toKMG",
		Value:   FnAAVaRS(tk.IntToKMGT),
		ValueEx: FnAAVaRSex(tk.IntToKMGT),
	},

	// string related
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
	BuiltinToUpper: &BuiltinFunction{
		Name:    "toUpper",
		Value:   FnASRS(strings.ToUpper),
		ValueEx: FnASRSex(strings.ToUpper),
		Remark:  `usage: upperStr := toUpper("abD")`,
	},
	BuiltinToLower: &BuiltinFunction{
		Name:    "toLower",
		Value:   FnASRS(strings.ToLower),
		ValueEx: FnASRSex(strings.ToLower),
		Remark:  `usage: lowerStr := toLower("abD")`,
	},
	BuiltinStrContains: &BuiltinFunction{
		Name:    "strContains",
		Value:   FnASSRB(strings.Contains),
		ValueEx: FnASSRBex(strings.Contains),
		Remark:  `usage: if strContains("abD", "bD") {...}`,
	},
	BuiltinStrStartsWith: &BuiltinFunction{
		Name:    "strStartsWith",
		Value:   FnASSRB(strings.HasPrefix),
		ValueEx: FnASSRBex(strings.HasPrefix),
		Remark:  `usage: if strStartsWith("abD", "bD") {...}`,
	},
	BuiltinStrEndsWith: &BuiltinFunction{
		Name:    "strEndsWith",
		Value:   FnASSRB(strings.HasSuffix),
		ValueEx: FnASSRBex(strings.HasSuffix),
		Remark:  `usage: if strEndsWith("abD", "bD") {...}`,
	},
	BuiltinStrReplace: &BuiltinFunction{
		Name:    "strReplace",
		Value:   FnASVsRS(tk.StringReplace),
		ValueEx: FnASVsRSex(tk.StringReplace),
	},
	BuiltinStrSplit: &BuiltinFunction{
		Name:    "strSplit",
		Value:   CallExAdapter(builtinStrSplitFunc),
		ValueEx: builtinStrSplitFunc,
	},
	BuiltintStrSplitLines: &BuiltinFunction{
		Name:    "strSplitLines",
		Value:   FnASRLs(tk.SplitLines),
		ValueEx: FnASRLsex(tk.SplitLines),
	},
	BuiltinStrJoin: &BuiltinFunction{
		Name:    "strJoin",
		Value:   CallExAdapter(builtinStrJoinFunc),
		ValueEx: builtinStrJoinFunc,
	},
	BuiltinStrRepeat: &BuiltinFunction{
		Name:    "strRepeat",
		Value:   FnASIRS(strings.Repeat),
		ValueEx: FnASIRSex(strings.Repeat),
	},
	BuiltinStrCount: &BuiltinFunction{
		Name:    "strCount",
		Value:   FnASSRI(strings.Count),
		ValueEx: FnASSRIex(strings.Count),
	},
	BuiltinStrPad: &BuiltinFunction{
		Name:    "strPad",
		Value:   FnASIVsRS(tk.PadString),
		ValueEx: FnASIVsRSex(tk.PadString),
	},
	BuiltinStrIn: &BuiltinFunction{
		Name:    "strIn",
		Value:   FnASVsRB(tk.InStrings),
		ValueEx: FnASVsRBex(tk.InStrings),
	},
	BuiltintStrFindDiffPos: &BuiltinFunction{
		Name:    "strFindDiffPos",
		Value:   FnASSRI(tk.FindFirstDiffIndex),
		ValueEx: FnASSRIex(tk.FindFirstDiffIndex),
	},
	BuiltintLimitStr: &BuiltinFunction{
		Name:    "limitStr",
		Value:   FnASIVsRS(tk.LimitString),
		ValueEx: FnASIVsRSex(tk.LimitString),
	},
	BuiltinStrQuote: &BuiltinFunction{
		Name:    "strQuote",
		Value:   FnASRS(strconv.Quote),
		ValueEx: FnASRSex(strconv.Quote),
	},
	BuiltinStrUnquote: &BuiltinFunction{
		Name:    "strUnquote",
		Value:   CallExAdapter(builtintStrUnquoteFunc),
		ValueEx: builtintStrUnquoteFunc,
	},

	// regex related
	BuiltinRegMatch: &BuiltinFunction{
		Name:    "regMatch",
		Value:   FnASSRB(tk.RegMatchX),
		ValueEx: FnASSRBex(tk.RegMatchX),
	},
	BuiltinRegContains: &BuiltinFunction{
		Name:    "regContains",
		Value:   FnASSRB(tk.RegContainsX),
		ValueEx: FnASSRBex(tk.RegContainsX),
	},
	BuiltinRegFindFirst: &BuiltinFunction{
		Name:    "regFindFirst",
		Value:   FnASSIRS(tk.RegFindFirstX),
		ValueEx: FnASSIRSex(tk.RegFindFirstX),
	},
	BuiltintRegFindFirstGroups: &BuiltinFunction{
		Name:    "regFindFirstGroups",
		Value:   FnASSRLs(tk.RegFindFirstGroupsX),
		ValueEx: FnASSRLsex(tk.RegFindFirstGroupsX),
	},
	BuiltinRegFindAll: &BuiltinFunction{
		Name:    "regFindAll",
		Value:   FnASSIRLs(tk.RegFindAllX),
		ValueEx: FnASSIRLsex(tk.RegFindAllX),
	},
	BuiltinRegFindAllGroups: &BuiltinFunction{
		Name:    "regFindAllGroups",
		Value:   FnASSRLls(tk.RegFindAllGroupsX),
		ValueEx: FnASSRLlsex(tk.RegFindAllGroupsX),
	},
	BuiltinRegQuote: &BuiltinFunction{
		Name:    "regQuote",
		Value:   CallExAdapter(builtinRegQuoteFunc),
		ValueEx: builtinRegQuoteFunc,
	},
	BuiltinRegReplace: &BuiltinFunction{
		Name:    "regReplace",
		Value:   FnASSSRS(tk.RegReplaceX),
		ValueEx: FnASSSRSex(tk.RegReplaceX),
	},
	BuiltinRegCount: &BuiltinFunction{
		Name:    "regCount",
		Value:   FnASSRI(tk.RegCount),
		ValueEx: FnASSRIex(tk.RegCount),
	},
	BuiltinRegSplit: &BuiltinFunction{
		Name:    "regSplit",
		Value:   FnASSViRLs(tk.RegSplitX),
		ValueEx: FnASSViRLsex(tk.RegSplitX),
	},

	// math related
	BuiltinAdjustFloat: &BuiltinFunction{
		Name:    "adjustFloat",
		Value:   CallExAdapter(builtinAdjustFloatFunc),
		ValueEx: builtinAdjustFloatFunc,
	},
	BuiltinMathSqrt: &BuiltinFunction{
		Name:    "mathSqrt",
		Value:   CallExAdapter(builtinMathSqrtFunc),
		ValueEx: builtinMathSqrtFunc,
	},

	// random related
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

	// time related
	BuiltinNow: &BuiltinFunction{
		Name:    "now",
		Value:   FnART(time.Now),
		ValueEx: FnARTex(time.Now),
	},
	BuiltinGetNowStr: &BuiltinFunction{
		Name:    "getNowStr",
		Value:   FnARS(tk.GetNowTimeStringFormal),
		ValueEx: FnARSex(tk.GetNowTimeStringFormal),
	},
	BuiltinGetNowStrCompact: &BuiltinFunction{
		Name:    "getNowStrCompact",
		Value:   FnARS(tk.GetNowTimeString),
		ValueEx: FnARSex(tk.GetNowTimeString),
	},
	BuiltinTimeToTick: &BuiltinFunction{
		Name:    "timeToTick",
		Value:   FnATRS(tk.GetTimeStampMid),
		ValueEx: FnATRSex(tk.GetTimeStampMid),
	},
	BuiltinGetNowTimeStamp: &BuiltinFunction{
		Name:    "getNowTimeStamp",
		Value:   CallExAdapter(builtinGetNowTimeStampFunc),
		ValueEx: builtinGetNowTimeStampFunc,
	},

	// binary/bytes related
	BuiltinBytesStartsWith: &BuiltinFunction{
		Name:    "bytesStartsWith",
		Value:   FnALyARB(tk.BytesStartsWith),
		ValueEx: FnALyARBex(tk.BytesStartsWith),
	},
	BuiltinBytesEndsWith: &BuiltinFunction{
		Name:    "bytesEndsWith",
		Value:   FnALyARB(tk.BytesEndsWith),
		ValueEx: FnALyARBex(tk.BytesEndsWith),
	},

	// compare related
	BuiltinCompareBytes: &BuiltinFunction{
		Name:    "compareBytes",
		Value:   FnALbyLbyViRLLi(tk.CompareBytes),
		ValueEx: FnALbyLbyViRLLiex(tk.CompareBytes),
		Remark:  `compare two bytes object to find differences, usage: compareBytes(bytes1, bytes2[, limit]), return an Array, each item is an array with the difference index, byte in bytes1, byte in bytes2`,
	},

	// control related
	BuiltinIsNil: &BuiltinFunction{
		Name:    "isNil", // usage: isNil(err1), check if the argument is nil
		Value:   CallExAdapter(builtinIsNilFunc),
		ValueEx: builtinIsNilFunc,
	},
	BuiltinIsNilOrEmpty: &BuiltinFunction{
		Name:    "isNilOrEmpty", // usage: isNilOrEmpty(err1), check if the argument is nil or empty string
		Value:   CallExAdapter(builtinIsNilOrEmptyFunc),
		ValueEx: builtinIsNilOrEmptyFunc,
	},
	BuiltinIsNilOrErr: &BuiltinFunction{
		Name:    "isNilOrErr", // usage: isNilOrErr(err1), check if the argument is nil, error object, or TXERROR string
		Value:   CallExAdapter(builtinIsNilOrErrFunc),
		ValueEx: builtinIsNilOrErrFunc,
	},
	BuiltinIsUndefined: &BuiltinFunction{
		Name:    "isUndefined",
		Value:   funcPORO(builtinIsUndefinedFunc),
		ValueEx: funcPOROEx(builtinIsUndefinedFunc),
	},
	BuiltinIsBool: &BuiltinFunction{
		Name:    "isBool",
		Value:   funcPORO(builtinIsBoolFunc),
		ValueEx: funcPOROEx(builtinIsBoolFunc),
	},
	BuiltinIsByte: &BuiltinFunction{
		Name:    "isByte",
		Value:   funcPORO(builtinIsByteFunc),
		ValueEx: funcPOROEx(builtinIsByteFunc),
	},
	BuiltinIsChar: &BuiltinFunction{
		Name:    "isChar",
		Value:   funcPORO(builtinIsCharFunc),
		ValueEx: funcPOROEx(builtinIsCharFunc),
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
	BuiltinIsChars: &BuiltinFunction{
		Name:    "isChars",
		Value:   funcPORO(builtinIsCharsFunc),
		ValueEx: funcPOROEx(builtinIsCharsFunc),
	},
	BuiltinIsArray: &BuiltinFunction{
		Name:    "isArray",
		Value:   funcPORO(builtinIsArrayFunc),
		ValueEx: funcPOROEx(builtinIsArrayFunc),
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
	BuiltinIsError: &BuiltinFunction{
		Name:    "isError",
		Value:   CallExAdapter(builtinIsErrorFunc),
		ValueEx: builtinIsErrorFunc,
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

	BuiltinExit: &BuiltinFunction{
		Name:  "exit", // usage: exit() or exit(1)
		Value: builtinExitFunc,
	},
	BuiltinPass: &BuiltinFunction{
		Name:    "pass",
		Value:   CallExAdapter(builtinPassFunc),
		ValueEx: builtinPassFunc,
	},

	// error related
	BuiltinIsErrX: &BuiltinFunction{
		Name:    "isErrX", // usage: isErrX(err1), check if err1 is error or error string(which starts with TXERROR:)
		Value:   CallExAdapter(builtinIsErrXFunc),
		ValueEx: builtinIsErrXFunc,
	},
	BuiltinGetErrStrX: &BuiltinFunction{
		Name:    "getErrStrX",
		Value:   FnAARS(tk.GetErrStrX),
		ValueEx: FnAARSex(tk.GetErrStrX),
	},
	BuiltinCheckErrX: &BuiltinFunction{
		Name:    "checkErrX",
		Value:   CallExAdapter(builtinCheckErrXFunc),
		ValueEx: builtinCheckErrXFunc,
	},
	BuiltinErrStrf: &BuiltinFunction{
		Name:    "errStrf",
		Value:   FnASVaRS(tk.ErrStrf),
		ValueEx: FnASVaRSex(tk.ErrStrf),
	},

	// output/print related
	BuiltinPrf: &BuiltinFunction{
		Name:    "prf", // usage: the same as printf
		Value:   FnASVaR(tk.Printf),
		ValueEx: FnASVaRex(tk.Printf),
	},
	BuiltinPl: &BuiltinFunction{
		Name:    "pl", // usage: the same as printf, but with a line-end(\n) at the end
		Value:   CallExAdapter(builtinPlFunc),
		ValueEx: builtinPlFunc,
	},
	BuiltinPln: &BuiltinFunction{
		Name:    "pln",
		Value:   FnAVaR(tk.Pln),
		ValueEx: FnAVaRex(tk.Pln),
	},
	BuiltinPlv: &BuiltinFunction{
		Name:    "plv",
		Value:   FnAVaR(tk.Plv),
		ValueEx: FnAVaRex(tk.Plv),
	},
	BuiltinPlt: &BuiltinFunction{
		Name:    "plt",
		Value:   CallExAdapter(builtinPltFunc),
		ValueEx: builtinPltFunc,
	},
	BuiltinPlo: &BuiltinFunction{
		Name: "plo",
		Value: func(args ...Object) (Object, error) {
			return FnAVaRex(tk.Plo)(NewCall(nil, args))
		},
		ValueEx: FnAVaRex(tk.Plo),
	},
	BuiltinPlErr: &BuiltinFunction{
		Name:    "plErr",
		Value:   FnAAR(tk.PlErrX),
		ValueEx: FnAARex(tk.PlErrX),
	},
	BuiltinFatalf: &BuiltinFunction{
		Name:    "fatalf",
		Value:   CallExAdapter(builtinFatalfFunc),
		ValueEx: builtinFatalfFunc,
	},
	BuiltinSpr: &BuiltinFunction{
		Name:    "spr", // usage: the same as sprintf
		Value:   FnASVaRS(fmt.Sprintf),
		ValueEx: FnASVaRSex(fmt.Sprintf),
	},

	// scan related
	BuiltinSscanf: &BuiltinFunction{
		Name:    "sscanf",
		Value:   CallExAdapter(builtinSscanfFunc),
		ValueEx: builtinSscanfFunc,
	},

	// resource related
	BuiltinGetNamedValue: &BuiltinFunction{
		Name:    "getNamedValue",
		Value:   CallExAdapter(builtinGetNamedValueFunc),
		ValueEx: builtinGetNamedValueFunc,
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

	// member/method related
	BuiltinGetValue: &BuiltinFunction{
		Name:    "getValue",
		Value:   CallExAdapter(builtinGetValueFunc),
		ValueEx: builtinGetValueFunc,
	},
	BuiltinSetValue: &BuiltinFunction{
		Name:    "setValue",
		Value:   CallExAdapter(builtinSetValueFunc),
		ValueEx: builtinSetValueFunc,
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
	BuiltinCallMethod: &BuiltinFunction{
		Name:    "callMethod",
		Value:   CallExAdapter(builtinCallMethodFunc),
		ValueEx: builtinCallMethodFunc,
	},
	BuiltinCallMethodEx: &BuiltinFunction{
		Name:    "callMethodEx",
		Value:   CallExAdapter(builtinCallMethodExFunc),
		ValueEx: builtinCallMethodExFunc,
	},

	// open/close related
	BuiltinClose: &BuiltinFunction{
		Name:    "close",
		Value:   CallExAdapter(builtinCloseFunc),
		ValueEx: builtinCloseFunc,
	},

	// read/write related
	BuiltintReadAllStr: &BuiltinFunction{
		Name:    "readAllStr",
		Value:   CallExAdapter(builtinReadAllStrFunc),
		ValueEx: builtinReadAllStrFunc,
	},
	BuiltintReadAllBytes: &BuiltinFunction{
		Name:    "readAllBytes",
		Value:   CallExAdapter(builtinReadAllBytesFunc),
		ValueEx: builtinReadAllBytesFunc,
	},
	BuiltintWriteStr: &BuiltinFunction{
		Name:    "writeStr",
		Value:   CallExAdapter(builtinWriteStrFunc),
		ValueEx: builtinWriteStrFunc,
	},
	BuiltintWriteBytes: &BuiltinFunction{
		Name:    "writeBytes",
		Value:   CallExAdapter(builtinWriteBytesFunc),
		ValueEx: builtinWriteBytesFunc,
	},

	// encode/decode related
	BuiltinMd5: &BuiltinFunction{
		Name:    "md5",
		Value:   FnASRS(tk.MD5Encrypt),
		ValueEx: FnASRSex(tk.MD5Encrypt),
	},
	BuiltinUrlEncode: &BuiltinFunction{
		Name:    "urlEncode",
		Value:   FnASRS(tk.UrlEncode),
		ValueEx: FnASRSex(tk.UrlEncode),
	},
	BuiltinUrlDecode: &BuiltinFunction{
		Name:    "urlDecode",
		Value:   FnASRS(tk.UrlDecode),
		ValueEx: FnASRSex(tk.UrlDecode),
	},
	BuiltinHtmlEncode: &BuiltinFunction{
		Name:    "htmlEncode",
		Value:   FnASRS(tk.EncodeHTML),
		ValueEx: FnASRSex(tk.EncodeHTML),
	},
	BuiltinHtmlDecode: &BuiltinFunction{
		Name:    "htmlDecode",
		Value:   FnASRS(tk.DecodeHTML),
		ValueEx: FnASRSex(tk.DecodeHTML),
	},
	BuiltinSimpleEncode: &BuiltinFunction{
		Name:    "simpleEncode",
		Value:   CallExAdapter(builtinSimpleEncodeFunc),
		ValueEx: builtinSimpleEncodeFunc,
	},
	BuiltinSimpleDecode: &BuiltinFunction{
		Name:    "simpleDecode",
		Value:   CallExAdapter(builtinSimpleDecodeFunc),
		ValueEx: builtinSimpleDecodeFunc,
	},
	BuiltinBase64Encode: &BuiltinFunction{
		Name:    "base64Encode",
		Value:   CallExAdapter(builtinBase64EncodeFunc),
		ValueEx: builtinBase64EncodeFunc,
	},
	BuiltinBase64Decode: &BuiltinFunction{
		Name:    "base64Decode",
		Value:   FnASRA(tk.FromBase64),
		ValueEx: FnASRAex(tk.FromBase64),
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

	// XML related
	BuiltinStrXmlEncode: &BuiltinFunction{
		Name:    "xmlEncodeStr",
		Value:   FnASRS(tk.EncodeToXMLString),
		ValueEx: FnASRSex(tk.EncodeToXMLString),
	},
	BuiltinXmlGetNodeStr: &BuiltinFunction{
		Name:    "xmlGetNodeStr",
		Value:   FnASSRSE(tk.GetNodeStringFromXML),
		ValueEx: FnASSRSEex(tk.GetNodeStringFromXML),
	},

	// command-line related
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
	BuiltinGetIntSwitch: &BuiltinFunction{
		Name:    "getIntSwitch",
		Value:   FnALsSViRI(tk.GetSwitchWithDefaultIntValue),
		ValueEx: FnALsSViRIex(tk.GetSwitchWithDefaultIntValue),
	},
	BuiltinGetSwitches: &BuiltinFunction{
		Name:    "getSwitches", // usage: getSwitches(argsG)
		Value:   FnALsRLs(tk.GetAllSwitches),
		ValueEx: FnALsRLsex(tk.GetAllSwitches),
	},
	BuiltinGetParam: &BuiltinFunction{
		Name:    "getParam", // usage: getParam(argsG, 1, "default")
		Value:   CallExAdapter(builtinGetParamFunc),
		ValueEx: builtinGetParamFunc,
	},
	BuiltinGetParams: &BuiltinFunction{
		Name:    "getParams", // usage: getParams(argsG)
		Value:   FnALsRLs(tk.GetAllParameters),
		ValueEx: FnALsRLsex(tk.GetAllParameters),
	},

	// clipboard related
	BuiltinGetClipText: &BuiltinFunction{
		Name:    "getClipText",
		Value:   FnARS(tk.GetClipText),
		ValueEx: FnARSex(tk.GetClipText),
	},
	BuiltinSetClipText: &BuiltinFunction{
		Name:    "setClipText",
		Value:   FnASRE(tk.SetClipText),
		ValueEx: FnASREex(tk.SetClipText),
	},

	// thread related
	BuiltinSleep: &BuiltinFunction{
		Name:    "sleep", // usage: sleep(1.2) sleep for 1.2 seconds
		ValueEx: builtinSleepFunc,
	},

	BuiltinLock: &BuiltinFunction{
		Name:    "lock", // usage: lock(mutext1)
		ValueEx: builtinLockFunc,
	},
	BuiltinUnlock: &BuiltinFunction{
		Name:    "unlock", // usage: unlock(mutext1)
		ValueEx: builtinUnlockFunc,
	},
	BuiltinRLock: &BuiltinFunction{
		Name:    "rLock", // usage: rLock(mutext1)
		ValueEx: builtinRLockFunc,
	},
	BuiltinRUnlock: &BuiltinFunction{
		Name:    "rUnlock", // usage: rUnlock(mutext1)
		ValueEx: builtinRUnlockFunc,
	},
	BuiltinTryLock: &BuiltinFunction{
		Name:    "tryLock", // usage: tryLock(mutext1)
		ValueEx: builtinTryLockFunc,
	},
	BuiltinTryRLock: &BuiltinFunction{
		Name:    "tryRLock", // usage: tryRLock(mutext1)
		ValueEx: builtinTryRLockFunc,
	},

	// os/system related
	BuiltinSystemCmd: &BuiltinFunction{
		Name:    "systemCmd",
		Value:   FnASVsRS(tk.SystemCmd),
		ValueEx: FnASVsRSex(tk.SystemCmd),
	},

	BuiltinGetEnv: &BuiltinFunction{
		Name:    "getEnv",
		Value:   CallExAdapter(builtinGetEnvFunc),
		ValueEx: builtinGetEnvFunc,
	},
	BuiltinSetEnv: &BuiltinFunction{
		Name:    "setEnv",
		Value:   FnASSRE(os.Setenv),
		ValueEx: FnASSREex(os.Setenv),
	},

	BuiltinGetOSName: &BuiltinFunction{
		Name:    "getOSName",
		Value:   FnARS(tk.GetOSName),
		ValueEx: FnARSex(tk.GetOSName),
	},
	BuiltinGetOSArch: &BuiltinFunction{
		Name:    "getOSArch",
		Value:   FnARS(tk.GetOSArch),
		ValueEx: FnARSex(tk.GetOSArch),
	},
	BuiltinGetOSArgs: &BuiltinFunction{
		Name:    "getOSArgs",
		Value:   FnARLs(tk.GetOSArgs),
		ValueEx: FnARLsex(tk.GetOSArgs),
	},

	BuiltinGetAppDir: &BuiltinFunction{
		Name:    "getAppDir",
		Value:   FnARS(tk.GetApplicationPath),
		ValueEx: FnARSex(tk.GetApplicationPath),
	},
	BuiltinGetCurDir: &BuiltinFunction{
		Name:    "getCurDir",
		Value:   FnARS(tk.GetCurrentDir),
		ValueEx: FnARSex(tk.GetCurrentDir),
	},
	BuiltinGetHomeDir: &BuiltinFunction{
		Name:    "getHomeDir",
		Value:   FnARS(tk.GetHomeDir),
		ValueEx: FnARSex(tk.GetHomeDir),
	},
	BuiltinGetTempDir: &BuiltinFunction{
		Name:    "getTempDir",
		Value:   FnARS(os.TempDir),
		ValueEx: FnARSex(os.TempDir),
	},
	BuiltinGetInput: &BuiltinFunction{
		Name:    "getInput",
		Value:   FnASVaRS(tk.GetInputf),
		ValueEx: FnASVaRSex(tk.GetInputf),
	},

	// dir/path related
	BuiltinJoinPath: &BuiltinFunction{
		Name:    "joinPath",
		Value:   FnAVsRS(filepath.Join),
		ValueEx: FnAVsRSex(filepath.Join),
	},
	BuiltinIsDir: &BuiltinFunction{
		Name:    "isDir",
		Value:   FnASRB(tk.IsDirectory),
		ValueEx: FnASRBex(tk.IsDirectory),
	},
	BuiltinEnsureMakeDirs: &BuiltinFunction{
		Name:    "ensureMakeDirs",
		Value:   FnASRS(tk.EnsureMakeDirs),
		ValueEx: FnASRSex(tk.EnsureMakeDirs),
	},
	BuiltinGetFileList: &BuiltinFunction{
		Name:    "getFileList",
		Value:   FnASVsRLmss(tk.GetFileList),
		ValueEx: FnASVsRLmssex(tk.GetFileList),
	},

	// file related
	BuiltinFileExists: &BuiltinFunction{
		Name:    "fileExists",
		Value:   FnASRB(tk.IfFileExists),
		ValueEx: FnASRBex(tk.IfFileExists),
	},
	BuiltinGetFileAbs: &BuiltinFunction{
		Name:    "getFileAbs",
		Value:   FnASRS(tk.GetFileAbs),
		ValueEx: FnASRSex(tk.GetFileAbs),
	},
	BuiltinGetFileExt: &BuiltinFunction{
		Name:    "getFileExt",
		Value:   FnASRS(filepath.Ext),
		ValueEx: FnASRSex(filepath.Ext),
	},
	BuiltinGetFileRel: &BuiltinFunction{
		Name:    "getFileRel",
		Value:   FnASSRSE(filepath.Rel),
		ValueEx: FnASSRSEex(filepath.Rel),
	},
	BuiltinExtractFileDir: &BuiltinFunction{
		Name:    "extractFileDir",
		Value:   FnASRS(filepath.Dir),
		ValueEx: FnASRSex(filepath.Dir),
	},
	BuiltinRenameFile: &BuiltinFunction{
		Name:    "renameFile",
		Value:   FnASSVsRE(tk.RenameFile),
		ValueEx: FnASSVsREex(tk.RenameFile),
	},
	BuiltinRemoveFile: &BuiltinFunction{
		Name:    "removeFile",
		Value:   FnASRE(tk.RemoveFile),
		ValueEx: FnASREex(tk.RemoveFile),
	},
	BuiltinRemoveDir: &BuiltinFunction{
		Name:    "removeDir",
		Value:   CallExAdapter(builtinRemoveDirFunc),
		ValueEx: builtinRemoveDirFunc,
	},
	BuiltinRemovePath: &BuiltinFunction{
		Name:    "removePath",
		Value:   CallExAdapter(builtinRemovePathFunc),
		ValueEx: builtinRemovePathFunc,
	},

	BuiltinLoadText: &BuiltinFunction{
		Name:    "loadText",
		Value:   FnASRS(tk.LoadStringFromFile),
		ValueEx: FnASRSex(tk.LoadStringFromFile),
	},
	BuiltinSaveText: &BuiltinFunction{
		Name:    "saveText",
		Value:   FnASSRS(tk.SaveStringToFile),
		ValueEx: FnASSRSex(tk.SaveStringToFile),
	},
	BuiltinAppendText: &BuiltinFunction{
		Name:    "appendText",
		Value:   FnASSRS(tk.AppendStringToFile),
		ValueEx: FnASSRSex(tk.AppendStringToFile),
	},
	BuiltinLoadBytes: &BuiltinFunction{
		Name:    "loadBytes",
		Value:   FnASVIRA(tk.LoadBytesFromFile),
		ValueEx: FnASVIRAex(tk.LoadBytesFromFile),
		Remark:  `load bytes from file, usage: loadBytes("file.bin"), return error or Bytes([]byte)`,
	},
	BuiltinSaveBytes: &BuiltinFunction{
		Name:    "saveBytes",
		Value:   FnALbySRE(tk.SaveBytesToFileE),
		ValueEx: FnALbySREex(tk.SaveBytesToFileE),
		Remark:  `save bytes to file, usage: saveBytes(bytesT, "file.bin"), return error if failed`,
	},

	// compress/zip related
	BuiltinArchiveFilesToZip: &BuiltinFunction{
		Name:    "archiveFilesToZip",
		Value:   CallExAdapter(builtinArchiveFilesToZipFunc),
		ValueEx: builtinArchiveFilesToZipFunc,
	},

	// network/web related
	BuiltinGetWeb: &BuiltinFunction{
		Name:    "getWeb",
		Value:   FnASVaRA(tk.GetWeb),
		ValueEx: FnASVaRAex(tk.GetWeb),
	},
	BuiltinPostRequest: &BuiltinFunction{
		Name:    "postRequest",
		Value:   CallExAdapter(builtinPostRequestFunc),
		ValueEx: builtinPostRequestFunc,
	},
	BuiltinUrlExists: &BuiltinFunction{
		Name:    "urlExists",
		Value:   FnASVaRA(tk.UrlExists),
		ValueEx: FnASVaRAex(tk.UrlExists),
	},
	BuiltinIsHttps: &BuiltinFunction{
		Name:    "isHttps",
		Value:   CallExAdapter(builtinIsHttpsFunc),
		ValueEx: builtinIsHttpsFunc,
	},
	BuiltinHttpRedirect: &BuiltinFunction{
		Name:    "httpRedirect",
		Value:   CallExAdapter(builtinHttpRedirectFunc),
		ValueEx: builtinHttpRedirectFunc,
	},

	// server/service related
	BuiltinGetReqHeader: &BuiltinFunction{
		Name:    "getReqHeader",
		Value:   CallExAdapter(builtinGetReqHeaderFunc),
		ValueEx: builtinGetReqHeaderFunc,
	},
	BuiltinGetReqBody: &BuiltinFunction{
		Name:    "getReqBody",
		Value:   CallExAdapter(builtinGetReqBodyFunc),
		ValueEx: builtinGetReqBodyFunc,
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
	BuiltinWriteRespHeader: &BuiltinFunction{
		Name:    "writeRespHeader",
		Value:   CallExAdapter(builtinWriteRespHeaderFunc),
		ValueEx: builtinWriteRespHeaderFunc,
	},
	BuiltinSetRespHeader: &BuiltinFunction{
		Name:    "setRespHeader",
		Value:   CallExAdapter(builtinSetRespHeaderFunc),
		ValueEx: builtinSetRespHeaderFunc,
	},
	BuiltinGenJSONResp: &BuiltinFunction{
		Name:    "genJSONResp",
		Value:   CallExAdapter(builtinGenJSONRespFunc),
		ValueEx: builtinGenJSONRespFunc,
	},
	BuiltinWriteResp: &BuiltinFunction{
		Name:    "writeResp",
		Value:   CallExAdapter(builtinWriteRespFunc),
		ValueEx: builtinWriteRespFunc,
	},
	BuiltinServeFile: &BuiltinFunction{
		Name:    "serveFile",
		Value:   CallExAdapter(builtinServeFileFunc),
		ValueEx: builtinServeFileFunc,
	},
	BuiltinGetMimeType: &BuiltinFunction{
		Name:    "getMimeType",
		Value:   FnASRS(tk.GetMimeTypeByExt),
		ValueEx: FnASRSex(tk.GetMimeTypeByExt),
	},

	// security related
	BuiltinGenToken: &BuiltinFunction{
		Name:    "genToken",
		Value:   FnASSSVsRS(tk.GenerateToken),
		ValueEx: FnASSSVsRSex(tk.GenerateToken),
	},
	BuiltinCheckToken: &BuiltinFunction{
		Name:    "checkToken",
		Value:   FnASVsRS(tk.CheckToken),
		ValueEx: FnASVsRSex(tk.CheckToken),
	},
	BuiltinEncryptText: &BuiltinFunction{
		Name:    "encryptText",
		Value:   FnASVsRS(tk.EncryptStringByTXDEF),
		ValueEx: FnASVsRSex(tk.EncryptStringByTXDEF),
	},
	BuiltinDecryptText: &BuiltinFunction{
		Name:    "decryptText",
		Value:   FnASVsRS(tk.DecryptStringByTXDEF),
		ValueEx: FnASVsRSex(tk.DecryptStringByTXDEF),
	},
	BuiltinEncryptData: &BuiltinFunction{
		Name:    "encryptData",
		Value:   FnALyVsRLy(tk.EncryptDataByTXDEF),
		ValueEx: FnALyVsRLyex(tk.EncryptDataByTXDEF),
	},
	BuiltinDecryptData: &BuiltinFunction{
		Name:    "decryptData",
		Value:   FnALyVsRLy(tk.DecryptDataByTXDEF),
		ValueEx: FnALyVsRLyex(tk.DecryptDataByTXDEF),
	},

	// ssh related
	BuiltinSshUpload: &BuiltinFunction{
		Name:    "sshUpload",
		Value:   CallExAdapter(builtinSshUploadFunc),
		ValueEx: builtinSshUploadFunc,
	},

	// eTable related
	BuiltinReadCsv: &BuiltinFunction{
		Name:    "readCsv",
		Value:   CallExAdapter(builtinReadCsvFunc),
		ValueEx: builtinReadCsvFunc,
	},
	BuiltinWriteCsv: &BuiltinFunction{
		Name:    "WriteCsv",
		Value:   CallExAdapter(builtinWriteCsvFunc),
		ValueEx: builtinWriteCsvFunc,
	},

	// database related
	BuiltinFormatSQLValue: &BuiltinFunction{
		Name:    "formatSQLValue",
		Value:   FnASRS(sqltk.FormatSQLValue),
		ValueEx: FnASRSex(sqltk.FormatSQLValue),
	},
	BuiltinDbClose: &BuiltinFunction{
		Name:    "dbClose",
		Value:   CallExAdapter(BuiltinDbCloseFunc),
		ValueEx: BuiltinDbCloseFunc,
	},
	BuiltinDbQuery: &BuiltinFunction{
		Name:    "dbQuery",
		Value:   FnADSVaRA(sqltk.QueryDBX),
		ValueEx: FnADSVaRAex(sqltk.QueryDBX),
	},
	BuiltinDbQueryOrdered: &BuiltinFunction{
		Name:    "dbQueryOrdered",
		Value:   FnADSVaRA(sqltk.QueryDBOrderedX),
		ValueEx: FnADSVaRAex(sqltk.QueryDBOrderedX),
	},
	BuiltinDbQueryRecs: &BuiltinFunction{
		Name:    "dbQueryRecs",
		Value:   FnADSVaRA(sqltk.QueryDBRecsX),
		ValueEx: FnADSVaRAex(sqltk.QueryDBRecsX),
	},
	BuiltinDbQueryMap: &BuiltinFunction{
		Name:    "dbQueryMap",
		Value:   FnADSSVaRA(sqltk.QueryDBMapX),
		ValueEx: FnADSSVaRAex(sqltk.QueryDBMapX),
	},
	BuiltinDbQueryMapArray: &BuiltinFunction{
		Name:    "dbQueryMapArray",
		Value:   FnADSSVaRA(sqltk.QueryDBMapArrayX),
		ValueEx: FnADSSVaRAex(sqltk.QueryDBMapArrayX),
	},
	BuiltinDbQueryCount: &BuiltinFunction{
		Name:    "dbQueryCount",
		Value:   FnADSVaRA(sqltk.QueryCountX),
		ValueEx: FnADSVaRAex(sqltk.QueryCountX),
	},
	BuiltinDbQueryFloat: &BuiltinFunction{
		Name:    "dbQueryFloat",
		Value:   FnADSVaRA(sqltk.QueryFloatX),
		ValueEx: FnADSVaRAex(sqltk.QueryFloatX),
	},
	BuiltinDbQueryString: &BuiltinFunction{
		Name:    "dbQueryString",
		Value:   FnADSVaRA(sqltk.QueryStringX),
		ValueEx: FnADSVaRAex(sqltk.QueryStringX),
	},
	BuiltinDbExec: &BuiltinFunction{
		Name:    "dbExec",
		Value:   FnADSVaRA(sqltk.ExecDBX),
		ValueEx: FnADSVaRAex(sqltk.ExecDBX),
	},

	// unicode related
	BuiltinToPinyin: &BuiltinFunction{
		Name:    "toPinyin",
		Value:   FnASVsRA(tk.ToPinYin),
		ValueEx: FnASVsRAex(tk.ToPinYin),
	},

	// line editor related
	BuiltinLeClear: &BuiltinFunction{
		Name:    "leClear",
		Value:   CallExAdapter(builtinLeClearFunc),
		ValueEx: builtinLeClearFunc,
	},
	BuiltinLeLoadFromStr: &BuiltinFunction{
		Name:    "leLoadFromStr",
		Value:   CallExAdapter(builtinLeLoadFromStrFunc),
		ValueEx: builtinLeLoadFromStrFunc,
	},
	BuiltinLeAppendFromStr: &BuiltinFunction{
		Name:    "leAppendFromStr",
		Value:   CallExAdapter(builtinLeAppendFromStrFunc),
		ValueEx: builtinLeAppendFromStrFunc,
	},
	BuiltinLeSaveToStr: &BuiltinFunction{
		Name:    "leSaveToStr",
		Value:   CallExAdapter(builtinLeSaveToStrFunc),
		ValueEx: builtinLeSaveToStrFunc,
	},
	BuiltinLeLoadFromFile: &BuiltinFunction{
		Name:    "leLoadFromFile",
		Value:   CallExAdapter(builtinLeLoadFromFileFunc),
		ValueEx: builtinLeLoadFromFileFunc,
	},
	BuiltinLeAppendFromFile: &BuiltinFunction{
		Name:    "leAppendFromFile",
		Value:   CallExAdapter(builtinLeAppendFromFileFunc),
		ValueEx: builtinLeAppendFromFileFunc,
	},
	BuiltinLeSaveToFile: &BuiltinFunction{
		Name:    "leSaveToFile",
		Value:   CallExAdapter(builtinLeSaveToFileFunc),
		ValueEx: builtinLeSaveToFileFunc,
	},
	BuiltinLeAppendToFile: &BuiltinFunction{
		Name:    "leAppendToFile",
		Value:   CallExAdapter(builtinLeAppendToFileFunc),
		ValueEx: builtinLeAppendToFileFunc,
	},
	BuiltinLeLoadFromClip: &BuiltinFunction{
		Name:    "leLoadFromClip",
		Value:   CallExAdapter(builtinLeLoadFromClipFunc),
		ValueEx: builtinLeLoadFromClipFunc,
	},
	BuiltinLeSaveToClip: &BuiltinFunction{
		Name:    "leSaveToClip",
		Value:   CallExAdapter(builtinLeSaveToClipFunc),
		ValueEx: builtinLeSaveToClipFunc,
	},
	BuiltinLeLoadFromUrl: &BuiltinFunction{
		Name:    "leLoadFromUrl",
		Value:   CallExAdapter(builtinLeLoadFromUrlFunc),
		ValueEx: builtinLeLoadFromUrlFunc,
	},
	BuiltinLeLoadFromSsh: &BuiltinFunction{
		Name:    "leLoadFromSsh",
		Value:   CallExAdapter(builtinLeLoadFromSshFunc),
		ValueEx: builtinLeLoadFromSshFunc,
	},
	BuiltinLeSaveToSsh: &BuiltinFunction{
		Name:    "leSaveToSsh",
		Value:   CallExAdapter(builtinLeSaveToSshFunc),
		ValueEx: builtinLeSaveToSshFunc,
	},
	BuiltinLeViewAll: &BuiltinFunction{
		Name:    "leViewAll",
		Value:   CallExAdapter(builtinLeViewAllFunc),
		ValueEx: builtinLeViewAllFunc,
	},
	BuiltinLeViewLine: &BuiltinFunction{
		Name:    "leViewLine",
		Value:   CallExAdapter(builtinLeViewLineFunc),
		ValueEx: builtinLeViewLineFunc,
	},
	BuiltinLeViewLines: &BuiltinFunction{
		Name:    "leViewLines",
		Value:   CallExAdapter(builtinLeViewLinesFunc),
		ValueEx: builtinLeViewLinesFunc,
	},
	BuiltinLeSort: &BuiltinFunction{
		Name:    "leSort",
		Value:   CallExAdapter(builtinLeSortFunc),
		ValueEx: builtinLeSortFunc,
	},
	BuiltinLeConvertToUtf8: &BuiltinFunction{
		Name:    "leConvertToUtf8",
		Value:   CallExAdapter(builtinLeConvertToUtf8Func),
		ValueEx: builtinLeConvertToUtf8Func,
	},
	BuiltinLeGetLine: &BuiltinFunction{
		Name:    "leGetLine",
		Value:   CallExAdapter(builtinLeGetLineFunc),
		ValueEx: builtinLeGetLineFunc,
	},
	BuiltinLeSetLine: &BuiltinFunction{
		Name:    "leSetLine",
		Value:   CallExAdapter(builtinLeSetLineFunc),
		ValueEx: builtinLeSetLineFunc,
	},
	BuiltinLeSetLines: &BuiltinFunction{
		Name:    "leSetLines",
		Value:   CallExAdapter(builtinLeSetLinesFunc),
		ValueEx: builtinLeSetLinesFunc,
	},
	BuiltinLeInsertLine: &BuiltinFunction{
		Name:    "leInsertLine",
		Value:   CallExAdapter(builtinLeInsertLineFunc),
		ValueEx: builtinLeInsertLineFunc,
	},
	BuiltinLeAppendLine: &BuiltinFunction{
		Name:    "leAppendLine",
		Value:   CallExAdapter(builtinLeAppendLineFunc),
		ValueEx: builtinLeAppendLineFunc,
	},
	BuiltinLeRemoveLine: &BuiltinFunction{
		Name:    "leRemoveLine",
		Value:   CallExAdapter(builtinLeRemoveLineFunc),
		ValueEx: builtinLeRemoveLineFunc,
	},
	BuiltinLeRemoveLines: &BuiltinFunction{
		Name:    "leRemoveLines",
		Value:   CallExAdapter(builtinLeRemoveLinesFunc),
		ValueEx: builtinLeRemoveLinesFunc,
	},
	BuiltinLeFindLines: &BuiltinFunction{
		Name:    "leFindLines",
		Value:   CallExAdapter(builtinLeFindLinesFunc),
		ValueEx: builtinLeFindLinesFunc,
	},
	BuiltinLeFind: &BuiltinFunction{
		Name:    "leFind",
		Value:   CallExAdapter(builtinLeFindFunc),
		ValueEx: builtinLeFindFunc,
	},
	BuiltinLeFindAll: &BuiltinFunction{
		Name:    "leFindAll",
		Value:   CallExAdapter(builtinLeFindAllFunc),
		ValueEx: builtinLeFindAllFunc,
	},
	BuiltinLeReplace: &BuiltinFunction{
		Name:    "leReplace",
		Value:   CallExAdapter(builtinLeReplaceFunc),
		ValueEx: builtinLeReplaceFunc,
	},
	BuiltinLePrint: &BuiltinFunction{
		Name:    "lePrint",
		Value:   CallExAdapter(builtinLePrintFunc),
		ValueEx: builtinLePrintFunc,
	},
	BuiltinLeGetList: &BuiltinFunction{
		Name:    "leGetList",
		Value:   CallExAdapter(builtinLeGetListFunc),
		ValueEx: builtinLeGetListFunc,
	},

	// 3rd party related

	BuiltinAwsSign: &BuiltinFunction{
		Name:    "awsSign",
		Value:   CallExAdapter(builtinAwsSignFunc),
		ValueEx: builtinAwsSignFunc,
	},

	// misc related
	BuiltinGetSeq: &BuiltinFunction{
		Name: "getSeq",
		Value: func(args ...Object) (Object, error) {
			return FnARIex(tk.GetSeq)(NewCall(nil, args))
		},
		ValueEx: FnARIex(tk.GetSeq),
	},
	BuiltinRenderMarkdown: &BuiltinFunction{
		Name:    "renderMarkdown",
		Value:   FnASRS(tk.RenderMarkdown),
		ValueEx: FnASRSex(tk.RenderMarkdown),
	},

	// original internal related
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
	BuiltinCap: &BuiltinFunction{
		Name:    "cap",
		Value:   funcPORO(builtinCapFunc),
		ValueEx: funcPOROEx(builtinCapFunc),
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

	// char add start @
	// BuiltinNewAny: &BuiltinFunction{
	// 	Name:  "newAny",
	// 	Value: builtinNewAnyFunc,
	// },
	// BuiltinSortByFunc: &BuiltinFunction{
	// 	Name:    "sortByFunc",
	// 	Value:   CallExAdapter(builtinSortByFuncFunc),
	// 	ValueEx: builtinSortByFuncFunc,
	// },
	// char add end

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

	// funcs detail end

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
		obj = append(obj, c.Args...)
		obj = append(obj, c.Vargs...)
		return obj, nil
	case Bytes:
		n := 0
		for _, args := range [][]Object{c.Args, c.Vargs} {
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
		for _, args := range [][]Object{c.Args, c.Vargs} {
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
		ret = append(ret, c.Args...)
		ret = append(ret, c.Vargs...)
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

	nv, ok := arg.(*Any)

	if ok {
		return Int(tk.Len(nv.Value))
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
	case Bool:
		if nv {
			return Byte(1), nil
		}

		return Byte(0), nil
	case Byte:
		return nv, nil
	case Char:
		return Byte(nv), nil
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
	for _, args := range [][]Object{c.Args, c.Vargs} {
		for i, obj := range args {
			switch v := obj.(type) {
			case Byte:
				out = append(out, byte(v))
			case Char:
				out = append(out, byte(v))
			case Int:
				out = append(out, byte(v))
			case Uint:
				out = append(out, byte(v))
			default:
				return Undefined, NewArgumentTypeError(
					strconv.Itoa(i+1),
					"byte|int|uint|char",
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

func builtinIsByteFunc(arg Object) Object {
	_, ok := arg.(Byte)
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

func builtinIsCharsFunc(arg Object) Object {
	_, ok := arg.(Chars)
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
		// tk.Pl("func in CallExAdapter: %v", fn)
		return fn(Call{Args: args})
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

func toArgsN(offset int, c Call) []int {
	size := c.Len()
	vargs := make([]int, 0, size-offset)
	for i := offset; i < size; i++ {
		vargs = append(vargs, ToIntQuick(c.Get(i)))
	}
	return vargs
}

// func converters

// like tk.GetOSName
func FnARS(fn func() string) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		rs := fn()
		return ToStringObject(rs), nil
	}
}

func FnARSex(fn func() string) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		rs := fn()
		return ToStringObject(rs), nil
	}
}

// like time.Now
func FnART(fn func() time.Time) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		rs := fn()
		return &Time{Value: rs}, nil
	}
}

func FnARTex(fn func() time.Time) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		rs := fn()
		return &Time{Value: rs}, nil
	}
}

// like tk.GetTimeStampMid
func FnATRS(fn func(time.Time) string) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return NewCommonError("not enough parameters"), nil
		}

		nv, ok := args[0].(*Time)

		if !ok {
			return NewCommonError("unsupported type: %T", args[0]), nil
		}

		rs := fn(nv.Value)
		return String{Value: rs}, nil
	}
}

func FnATRSex(fn func(time.Time) string) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()

		if len(args) < 1 {
			return NewCommonError("not enough parameters"), nil
		}

		nv, ok := args[0].(*Time)

		if !ok {
			return NewCommonError("unsupported type: %T", args[0]), nil
		}

		rs := fn(nv.Value)
		return String{Value: rs}, nil
	}
}

// like tk.GetErrStrX
func FnAARS(fn func(interface{}) string) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(ConvertFromObject(args[0]))
		return ToStringObject(rs), nil
	}
}

func FnAARSex(fn func(interface{}) string) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		if c.Len() < 1 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(ConvertFromObject(c.Get(0)))
		return ToStringObject(rs), nil
	}
}

// like os.GetEnv
func FnASRS(fn func(string) string) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(args[0].String())
		return ToStringObject(rs), nil
	}
}

func FnASRSex(fn func(string) string) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		if c.Len() < 1 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(c.Get(0).String())
		return ToStringObject(rs), nil
	}
}

// like tk.FromBase64
func FnASRA(fn func(string) interface{}) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return NewCommonError("not enough parameters"), nil
		}

		rs := fn(args[0].String())
		return ConvertToObject(rs), nil
	}
}

func FnASRAex(fn func(string) interface{}) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()

		if len(args) < 1 {
			return NewCommonErrorWithPos(c, "not enough parameters"), nil
		}

		rs := fn(args[0].String())
		return ConvertToObject(rs), nil
	}
}

// like filepath.Rel
func FnASSRSE(fn func(string, string) (string, error)) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 2 {
			return NewCommonError("not enough parameters"), nil
		}

		rs, errT := fn(args[0].String(), args[1].String())

		if errT != nil {
			return NewCommonError(errT.Error()), nil
		}

		return ToStringObject(rs), nil
	}
}

func FnASSRSEex(fn func(string, string) (string, error)) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()

		if len(args) < 2 {
			return NewCommonError("not enough parameters"), nil
		}

		rs, errT := fn(args[0].String(), args[1].String())

		if errT != nil {
			return NewCommonError(errT.Error()), nil
		}

		return ToStringObject(rs), nil
	}
}

// like strings.Contains
func FnASSRB(fn func(string, string) bool) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 2 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(args[0].String(), args[1].String())
		return ConvertToObject(rs), nil
	}
}

func FnASSRBex(fn func(string, string) bool) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()

		if len(args) < 2 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(args[0].String(), args[1].String())
		return ConvertToObject(rs), nil
	}
}

// like math.Sqrt
func FnAFRF(fn func(float64) float64) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(ToFloatQuick(args[0]))

		return Float(rs), nil
	}
}

func FnAFRFex(fn func(float64) float64) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		if c.Len() < 1 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(ToFloatQuick(c.Get(0)))

		return Float(rs), nil
	}
}

// like tk.HexToBytes
func FnASRLby(fn func(string) []byte) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(args[0].String())

		return ConvertToObject(rs), nil
	}
}

func FnASRLbyex(fn func(string) []byte) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		if c.Len() < 1 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(c.Get(0).String())

		return ConvertToObject(rs), nil
	}
}

// like tk.IfFileExists
func FnASRB(fn func(string) bool) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(args[0].String())
		return Bool(rs), nil
	}
}

func FnASRBex(fn func(string) bool) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		if c.Len() < 1 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(c.Get(0).String())
		return Bool(rs), nil
	}
}

// like tk.SetClipText
func FnASRE(fn func(string) error) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(args[0].String())
		return ConvertToObject(rs), nil
	}
}

func FnASREex(fn func(string) error) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		if c.Len() < 1 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(c.Get(0).String())
		return ConvertToObject(rs), nil
	}
}

// like tk.RegFindFirstX
func FnASSIRS(fn func(string, string, int) string) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 3 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(args[0].String(), args[1].String(), tk.ToInt(ConvertFromObject(args[2])))
		return ToStringObject(rs), nil
	}
}

func FnASSIRSex(fn func(string, string, int) string) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		if c.Len() < 3 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(c.Get(0).String(), c.Get(1).String(), tk.ToInt(ConvertFromObject(c.Get(2))))
		return ToStringObject(rs), nil
	}
}

// like tk.SplitLines
func FnASRLs(fn func(string) []string) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(args[0].String())
		return ConvertToObject(rs), nil
	}
}

func FnASRLsex(fn func(string) []string) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		if c.Len() < 1 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(c.Get(0).String())
		return ConvertToObject(rs), nil
	}
}

// like tk.GetOSArgs
func FnARLs(fn func() []string) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		rs := fn()
		return ConvertToObject(rs), nil
	}
}

func FnARLsex(fn func() []string) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		rs := fn()
		return ConvertToObject(rs), nil
	}
}

// like tk.RegFindFirstGroupsX
func FnASSRLs(fn func(string, string) []string) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 2 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(args[0].String(), args[1].String())
		return ConvertToObject(rs), nil
	}
}

func FnASSRLsex(fn func(string, string) []string) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		if c.Len() < 2 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(c.Get(0).String(), c.Get(1).String())
		return ConvertToObject(rs), nil
	}
}

// like tk.RegFindAllX
func FnASSIRLs(fn func(string, string, int) []string) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 3 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(args[0].String(), args[1].String(), tk.ToInt(ConvertFromObject(args[2])))
		return ConvertToObject(rs), nil
	}
}

func FnASSIRLsex(fn func(string, string, int) []string) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		if c.Len() < 3 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(c.Get(0).String(), c.Get(1).String(), tk.ToInt(ConvertFromObject(c.Get(2))))
		return ConvertToObject(rs), nil
	}
}

// like tk.RegSplitX
func FnASSViRLs(fn func(string, string, ...int) []string) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 2 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		lenT := len(args) - 2

		intsT := make([]int, 0)

		for i := 0; i < lenT; i++ {
			intsT = append(intsT, ToGoIntQuick(args[i]))
		}

		rs := fn(args[0].String(), args[1].String(), intsT...)
		return ConvertToObject(rs), nil
	}
}

func FnASSViRLsex(fn func(string, string, ...int) []string) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()
		if len(args) < 2 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		lenT := len(args) - 2

		intsT := make([]int, 0)

		for i := 0; i < lenT; i++ {
			intsT = append(intsT, ToGoIntQuick(args[i]))
		}

		rs := fn(args[0].String(), args[1].String(), intsT...)
		return ConvertToObject(rs), nil
	}
}

// like tk.RegFindAllGroupsX
func FnASSRLls(fn func(string, string) [][]string) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 2 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(args[0].String(), args[1].String())
		return ConvertToObject(rs), nil
	}
}

func FnASSRLlsex(fn func(string, string) [][]string) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()
		if len(args) < 2 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(args[0].String(), args[1].String())
		return ConvertToObject(rs), nil
	}
}

// // like tk.LoadStringFromFile
// func FnASRSe(fn func(string) string) CallableFunc {
// 	return func(args ...Object) (ret Object, err error) {
// 		if len(args) < 1 {
// 			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
// 		}

// 		rs := fn(args[0].String())
// 		return ToStringObject(rs), nil
// 	}
// }

// func FnASRSeex(fn func(string) string) CallableExFunc {
// 	return func(c Call) (ret Object, err error) {
// 		if c.Len() < 1 {
// 			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
// 		}

// 		rs := fn(c.Get(0).String())
// 		return ToStringObject(rs), nil
// 	}
// }

// like tk.SaveStringToFile
func FnASSRS(fn func(string, string) string) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 2 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(args[0].String(), args[1].String())
		return ToStringObject(rs), nil
	}
}

func FnASSRSex(fn func(string, string) string) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		if c.Len() < 2 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(c.Get(0).String(), c.Get(1).String())
		return ToStringObject(rs), nil
	}
}

// like strings.Repeat
func FnASIRS(fn func(string, int) string) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 2 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(args[0].String(), ToGoIntQuick(args[1]))
		return ToStringObject(rs), nil
	}
}

func FnASIRSex(fn func(string, int) string) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()

		if len(args) < 2 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(args[0].String(), ToGoIntQuick(args[1]))
		return ToStringObject(rs), nil
	}
}

// like tk.RegReplaceX
func FnASSSRS(fn func(string, string, string) string) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 3 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(args[0].String(), args[1].String(), args[2].String())
		return ToStringObject(rs), nil
	}
}

func FnASSSRSex(fn func(string, string, string) string) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		if c.Len() < 3 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(c.Get(0).String(), c.Get(1).String(), c.Get(2).String())
		return ToStringObject(rs), nil
	}
}

// like tk.FindFirstDiffPosInStrs
func FnASSRI(fn func(string, string) int) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 2 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(args[0].String(), args[1].String())
		return ToIntObject(rs), nil
	}
}

func FnASSRIex(fn func(string, string) int) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		if c.Len() < 2 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(c.Get(0).String(), c.Get(1).String())
		return ToIntObject(rs), nil
	}
}

// like os.SetEnv
func FnASSRE(fn func(string, string) error) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 2 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(args[0].String(), args[1].String())
		return ConvertToObject(rs), nil
	}
}

func FnASSREex(fn func(string, string) error) CallableExFunc {
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
func FnAVsRS(fn func(...string) string) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		rs := fn(ObjectsToS(args)...)
		return ToStringObject(rs), nil
	}
}

func FnAVsRSex(fn func(...string) string) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()

		rs := fn(ObjectsToS(args)...)
		return ToStringObject(rs), nil
	}
}

// like tk.GetSeq
func FnARIex(fn func() int) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		rs := fn()
		return ToIntObject(rs), nil
	}
}

// like tk.SystemCmd
func FnASVsRS(fn func(string, ...string) string) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return NewCommonError("not enough parameters"), nil
		}

		vargs := ObjectsToS(args[1:])

		rs := fn(args[0].String(), vargs...)

		return ToStringObject(rs), nil
	}
}

func FnASVsRSex(fn func(string, ...string) string) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()
		if len(args) < 1 {
			return NewCommonError("not enough parameters"), nil
		}

		vargs := ObjectsToS(args[1:])

		tmps := args[0].String()

		rs := fn(tmps, vargs...)

		return ToStringObject(rs), nil
	}
}

// like tk.EncryptDataByTXDEF
func FnALyVsRLy(fn func([]byte, ...string) []byte) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return NewCommonError("not enough parameters"), nil
		}

		nv1, ok := args[0].(Bytes)

		if !ok {
			return NewCommonError("unsupported type: %T", args[0]), nil
		}

		vargs := ObjectsToS(args[1:])

		rs := fn([]byte(nv1), vargs...)

		return Bytes(rs), nil
	}
}

func FnALyVsRLyex(fn func([]byte, ...string) []byte) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()
		if len(args) < 1 {
			return NewCommonError("not enough parameters"), nil
		}

		nv1, ok := args[0].(Bytes)

		if !ok {
			return NewCommonError("unsupported type: %T", args[0]), nil
		}

		vargs := ObjectsToS(args[1:])

		rs := fn([]byte(nv1), vargs...)

		return Bytes(rs), nil
	}
}

// like tk.BytesStartsWith
func FnALyARB(fn func([]byte, interface{}) bool) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 2 {
			return NewCommonError("not enough parameters"), nil
		}

		nv1, ok := args[0].(Bytes)

		if !ok {
			return NewCommonError("unsupported type: %T", args[0]), nil
		}

		rs := fn([]byte(nv1), ConvertFromObject(args[1]))

		return Bool(rs), nil
	}
}

func FnALyARBex(fn func([]byte, interface{}) bool) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()
		if len(args) < 2 {
			return NewCommonError("not enough parameters"), nil
		}

		nv1, ok := args[0].(Bytes)

		if !ok {
			return NewCommonError("unsupported type: %T", args[0]), nil
		}

		rs := fn([]byte(nv1), ConvertFromObject(args[1]))

		return Bool(rs), nil
	}
}

// like tk.ToPinyin
func FnASVsRA(fn func(string, ...string) interface{}) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return NewCommonError("not enough parameters"), nil
		}

		vargs := ObjectsToS(args[1:])

		rs := fn(args[0].String(), vargs...)

		return ConvertToObject(rs), nil
	}
}

func FnASVsRAex(fn func(string, ...string) interface{}) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()
		if len(args) < 1 {
			return NewCommonError("not enough parameters"), nil
		}

		vargs := ObjectsToS(args[1:])

		tmps := args[0].String()

		rs := fn(tmps, vargs...)

		return ConvertToObject(rs), nil
	}
}

// like tk.RenameFile
func FnASSVsRE(fn func(string, string, ...string) error) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 2 {
			return NewCommonError("not enough parameters"), nil
		}

		vargs := ObjectsToS(args[2:])

		rs := fn(args[0].String(), args[1].String(), vargs...)

		return ConvertToObject(rs), nil
	}
}

func FnASSVsREex(fn func(string, string, ...string) error) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()

		if len(args) < 2 {
			return NewCommonError("not enough parameters"), nil
		}

		vargs := ObjectsToS(args[2:])

		rs := fn(args[0].String(), args[1].String(), vargs...)

		return ConvertToObject(rs), nil
	}
}

// like tk.InStrings
func FnASVsRB(fn func(string, ...string) bool) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return NewCommonError("not enough parameters"), nil
		}

		vargs := ObjectsToS(args[1:])
		rs := fn(args[0].String(), vargs...)
		return Bool(rs), nil
	}
}

func FnASVsRBex(fn func(string, ...string) bool) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()
		if len(args) < 1 {
			return NewCommonError("not enough parameters"), nil
		}

		vargs := ObjectsToS(args[1:])
		rs := fn(args[0].String(), vargs...)
		return Bool(rs), nil
	}
}

// like tk.GenerateToken
func FnASSSVsRS(fn func(string, string, string, ...string) string) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 3 {
			return NewCommonError("not enough parameters"), nil
		}

		vargs := ObjectsToS(args[1:])

		rs := fn(args[0].String(), args[1].String(), args[2].String(), vargs...)

		return ToStringObject(rs), nil
	}
}

func FnASSSVsRSex(fn func(string, string, string, ...string) string) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()

		if len(args) < 3 {
			return NewCommonError("not enough parameters"), nil
		}

		vargs := ObjectsToS(args[1:])

		rs := fn(args[0].String(), args[1].String(), args[2].String(), vargs...)

		return ToStringObject(rs), nil
	}
}

// like tk.GetFileList
func FnASVsRLmss(fn func(string, ...string) []map[string]string) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return NewCommonError("not enough parameters"), nil
		}

		vargs := ObjectsToS(args[1:])
		rs := fn(args[0].String(), vargs...)
		return ConvertToObject(rs), nil
	}
}

func FnASVsRLmssex(fn func(string, ...string) []map[string]string) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()

		if len(args) < 1 {
			return NewCommonError("not enough parameters"), nil
		}

		vargs := ObjectsToS(args[1:])
		rs := fn(args[0].String(), vargs...)
		return ConvertToObject(rs), nil
	}
}

// like tk.ErrStrf
func FnASVaRS(fn func(string, ...interface{}) string) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return NewCommonError("not enough parameters"), nil
		}

		vargs := ObjectsToI(args[1:])

		rs := fn(args[0].String(), vargs...)
		return ToStringObject(rs), nil
	}
}

func FnASVaRSex(fn func(string, ...interface{}) string) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()

		if len(args) < 1 {
			return NewCommonError("not enough parameters"), nil
		}

		vargs := ObjectsToI(args[1:])

		rs := fn(args[0].String(), vargs...)
		return ToStringObject(rs), nil
	}
}

// like tk.ErrStrf
func FnAAVaRS(fn func(interface{}, ...interface{}) string) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return NewCommonError("not enough parameters"), nil
		}

		vargs := ObjectsToI(args[1:])

		rs := fn(ConvertFromObject(args[0]), vargs...)
		return ToStringObject(rs), nil
	}
}

func FnAAVaRSex(fn func(interface{}, ...interface{}) string) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()

		if len(args) < 1 {
			return NewCommonError("not enough parameters"), nil
		}

		vargs := ObjectsToI(args[1:])

		rs := fn(ConvertFromObject(args[0]), vargs...)
		return ToStringObject(rs), nil
	}
}

// like tk.LoadBytesFromFile
func FnASVIRA(fn func(string, ...int) interface{}) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return NewCommonError("not enough parameters"), nil
		}

		vargs := ObjectsToN(args[1:])
		rs := fn(args[0].String(), vargs...)
		return ConvertToObject(rs), nil
	}
}

func FnASVIRAex(fn func(string, ...int) interface{}) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		if c.Len() < 1 {
			return NewCommonErrorWithPos(c, "not enough parameters"), nil
		}
		vargs := toArgsN(1, c)
		rs := fn(c.Get(0).String(), vargs...)
		return ConvertToObject(rs), nil
	}
}

// like tk.SaveBytesToFileE(func(bytesA []byte, fileA string) error)
func FnALbySRE(fn func([]byte, string) error) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 2 {
			return NewCommonError("not enough parameters"), nil
		}

		nv, ok := args[0].(Bytes)
		if !ok {
			return NewCommonError("invalid parameter 1 type: (%T)%v", args[0], args[0]), nil
		}

		rs := fn([]byte(nv), args[1].String())
		return ConvertToObject(rs), nil
	}
}

func FnALbySREex(fn func([]byte, string) error) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		if c.Len() < 2 {
			return NewCommonErrorWithPos(c, "not enough parameters"), nil
		}

		nv, ok := c.Get(0).(Bytes)
		if !ok {
			return NewCommonError("invalid parameter 1 type: (%T)%v", c.Get(0), c.Get(0)), nil
		}

		rs := fn(nv, c.Get(1).String())
		return ConvertToObject(rs), nil
	}
}

// like tk.CompareBytes(func([]byte, []byte, ...int) [][]int)
func FnALbyLbyViRLLi(fn func([]byte, []byte, ...int) [][]int) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 2 {
			return NewCommonError("not enough parameters"), nil
		}

		nv1, ok := args[0].(Bytes)
		if !ok {
			return NewCommonError("invalid parameter 1 type: (%T)%v", args[0], args[0]), nil
		}

		nv2, ok := args[1].(Bytes)
		if !ok {
			return NewCommonError("invalid parameter 2 type: (%T)%v", args[1], args[1]), nil
		}

		rs := fn([]byte(nv1), []byte(nv2), ObjectsToN(args[2:])...)

		return ConvertToObject(rs), nil
	}
}

func FnALbyLbyViRLLiex(fn func([]byte, []byte, ...int) [][]int) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()
		if c.Len() < 2 {
			return NewCommonErrorWithPos(c, "not enough parameters"), nil
		}

		nv1, ok := args[0].(Bytes)
		if !ok {
			return NewCommonError("invalid parameter 1 type: (%T)%v", args[0], args[0]), nil
		}

		nv2, ok := args[1].(Bytes)
		if !ok {
			return NewCommonError("invalid parameter 2 type: (%T)%v", args[1], args[1]), nil
		}

		rs := fn([]byte(nv1), []byte(nv2), ObjectsToN(args[2:])...)

		return ConvertToObject(rs), nil
	}
}

// like tk.GetSwitchWithDefaultIntValue
func FnALsSViRI(fn func([]string, string, ...int) int) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 2 {
			return NewCommonError("not enough parameters"), nil
		}

		nv, ok := args[0].(Array)
		if !ok {
			return NewCommonError("invalid parameter 1 type: (%T)%v", args[0], args[0]), nil
		}

		rs := fn(ObjectsToS(nv), args[1].String(), ObjectsToN(args[2:])...)
		return ToIntObject(rs), nil
	}
}

func FnALsSViRIex(fn func([]string, string, ...int) int) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()
		if len(args) < 2 {
			return NewCommonErrorWithPos(c, "not enough parameters"), nil
		}

		nv, ok := args[0].(Array)
		if !ok {
			return NewCommonError("invalid parameter 1 type: (%T)%v", args[0], args[0]), nil
		}

		rs := fn(ObjectsToS(nv), args[1].String(), ObjectsToN(args[2:])...)
		return ToIntObject(rs), nil
	}
}

// like tk.GetAllParameters
func FnALsRLs(fn func([]string) []string) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return NewCommonError("not enough parameters"), nil
		}

		nv, ok := args[0].(Array)
		if !ok {
			return NewCommonError("invalid parameter 1 type: (%T)%v", args[0], args[0]), nil
		}

		rs := fn(ObjectsToS(nv))

		return ConvertToObject(rs), nil
	}
}

func FnALsRLsex(fn func([]string) []string) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()
		if len(args) < 1 {
			return NewCommonErrorWithPos(c, "not enough parameters"), nil
		}

		nv, ok := args[0].(Array)
		if !ok {
			return NewCommonError("invalid parameter 1 type: (%T)%v", args[0], args[0]), nil
		}

		rs := fn(ObjectsToS(nv))

		return ConvertToObject(rs), nil
	}
}

// like tk.LimitString
func FnASIVsRS(fn func(string, int, ...string) string) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 2 {
			return NewCommonError("not enough parameters"), nil
		}

		v2, ok := ToGoInt(args[1])

		if !ok {
			return NewCommonError("invalid type for arg 2: (%T)%v"), nil
		}

		vargs := ObjectsToS(args[2:])
		rs := fn(args[0].String(), v2, vargs...)
		return ToStringObject(rs), nil
	}
}

func FnASIVsRSex(fn func(string, int, ...string) string) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		if c.Len() < 2 {
			return NewCommonErrorWithPos(c, "not enough parameters"), nil
		}

		v2, ok := ToGoInt(c.Get(1))

		if !ok {
			return NewCommonError("invalid type for arg 2: (%T)%v"), nil
		}

		vargs := toArgsS(2, c)
		rs := fn(c.Get(0).String(), v2, vargs...)
		return ToStringObject(rs), nil
	}
}

// like sqltk.ExecDBX
func FnADSVaRA(fn func(*sql.DB, string, ...interface{}) interface{}) CallableFunc {
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

func FnADSVaRAex(fn func(*sql.DB, string, ...interface{}) interface{}) CallableExFunc {
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
func FnADSSVaRA(fn func(*sql.DB, string, string, ...interface{}) interface{}) CallableFunc {
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

func FnADSSVaRAex(fn func(*sql.DB, string, string, ...interface{}) interface{}) CallableExFunc {
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
func FnASVaRA(fn func(string, ...interface{}) interface{}) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return NewCommonError("not enough parameters"), nil
		}

		vargs := ObjectsToI(args[1:])

		rs := fn(args[0].String(), vargs...)

		return ConvertToObject(rs), nil
	}
}

func FnASVaRAex(fn func(string, ...interface{}) interface{}) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()

		if len(args) < 1 {
			return NewCommonError("not enough parameters"), nil
		}

		vargs := ObjectsToI(args[1:])

		rs := fn(args[0].String(), vargs...)

		return ConvertToObject(rs), nil
	}
}

// like tk.Pln
func FnAVaR(fn func(...interface{})) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		vargs := ObjectsToI(args)
		fn(vargs...)
		return nil, nil
	}
}

func FnAVaRex(fn func(...interface{})) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		vargs := toArgsA(0, c)
		fn(vargs...)
		return nil, nil
	}
}

// like tk.PlErrX
func FnAAR(fn func(interface{})) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return NewCommonError("not enough parameters"), nil
		}

		fn(ConvertFromObject(args[0]))

		return nil, nil
	}
}

func FnAARex(fn func(interface{})) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		if c.Len() < 1 {
			return NewCommonError("not enough parameters"), nil
		}

		fn(ConvertFromObject(c.Get(0)))

		return nil, nil
	}
}

// like tk.Pln
func FnASVaR(fn func(string, ...interface{})) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return NewCommonError("not enough parameters"), nil
		}

		vargs := ObjectsToI(args[1:])

		fn(args[0].String(), vargs...)

		return nil, nil
	}
}

func FnASVaRex(fn func(string, ...interface{})) CallableExFunc {
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
func FnASVaRIEex(fn func(string, ...interface{}) (int, error)) CallableExFunc {
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

	tk.Pl(v.String(), ObjectsToO(args[1:])...)

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

func GetSwitchFromObjects(argsA []Object, switchA string, defaultA string) string {
	if argsA == nil {
		return defaultA
	}

	if len(argsA) < 1 {
		return defaultA
	}

	tmpStrT := ""

	for _, v := range argsA {
		argOT := v.String()

		if tk.StartsWith(argOT, switchA) {
			tmpStrT = argOT[len(switchA):]
			if tk.StartsWith(tmpStrT, "\"") && tk.EndsWith(tmpStrT, "\"") {
				return tmpStrT[1 : len(tmpStrT)-1]
			}

			return tmpStrT
		}
	}

	return defaultA
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

func IfSwitchExistsInObjects(argsA []Object, switchA string) bool {
	if len(argsA) < 1 {
		return false
	}

	listT := ObjectsToS(argsA)

	if listT == nil {
		return false
	}

	return tk.IfSwitchExistsWhole(listT, switchA)

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

func builtinSimpleEncodeFunc(c Call) (Object, error) {
	args := c.GetArgs()

	lenT := len(args)

	if lenT < 1 {
		return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
	}

	var rsT string

	if lenT > 1 {
		nv1, ok := args[1].(Byte)

		if ok {
			rsT = tk.EncodeStringCustomEx(args[0].String(), byte(nv1))
		} else {
			rsT = tk.EncodeStringCustomEx(args[0].String(), tk.ToByte(ConvertFromObject(args[1])))
		}

	} else {
		rsT = tk.EncodeStringCustomEx(args[0].String())
	}

	return ToStringObject(rsT), nil
}

func builtinSimpleDecodeFunc(c Call) (Object, error) {
	args := c.GetArgs()

	lenT := len(args)

	if lenT < 1 {
		return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
	}

	var rsT string

	if lenT > 1 {
		nv1, ok := args[1].(Byte)

		if ok {
			rsT = tk.DecodeStringCustom(args[0].String(), byte(nv1))
		} else {
			rsT = tk.DecodeStringCustom(args[0].String(), tk.ToByte(ConvertFromObject(args[1])))
		}

	} else {
		rsT = tk.DecodeStringCustom(args[0].String())
	}

	return ToStringObject(rsT), nil
}

func builtinBase64EncodeFunc(c Call) (Object, error) {
	args := c.GetArgs()

	lenT := len(args)

	if lenT < 1 {
		return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
	}

	switch nv := args[0].(type) {
	case String:
		return String{Value: base64.StdEncoding.EncodeToString([]byte(nv.Value))}, nil
	case Bytes:
		return String{Value: base64.StdEncoding.EncodeToString([]byte(nv))}, nil
	case *StringBuilder:
		return String{Value: base64.StdEncoding.EncodeToString([]byte(nv.String()))}, nil
	case *BytesBuffer:
		return String{Value: base64.StdEncoding.EncodeToString([]byte(nv.Value.Bytes()))}, nil
	default:
		return String{Value: base64.StdEncoding.EncodeToString([]byte(nv.String()))}, nil
	}
}

func builtinFromJSONFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
	}

	var jsonTextT string

	objT := args[0]

	switch nv := objT.(type) {
	case String:
		jsonTextT = nv.Value
	case Bytes:
		jsonTextT = string(nv)
	case Chars:
		jsonTextT = string(nv)
	default:
		jsonTextT = tk.ToStr(ConvertFromObject(args[0]))

	}

	jObjT, errT := tk.FromJSON(jsonTextT)

	if errT != nil {
		return NewCommonError("%v", errT), nil
	}

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

func builtinGetNowTimeStampFunc(c Call) (Object, error) {
	return String{Value: tk.GetTimeStampMid(time.Now())}, nil
}

func builtinIsNilFunc(c Call) (Object, error) {
	if c.Len() < 1 {
		return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
	}

	arg0 := c.Get(0)

	if arg0 == nil {
		return Bool(true), nil
	}

	_, ok := arg0.(*UndefinedType)
	if ok {
		return Bool(true), nil
	}

	nv := ConvertFromObject(arg0)

	return Bool(tk.IsNil(nv)), nil
}

func builtinIsNilOrEmptyFunc(c Call) (Object, error) {
	if c.Len() < 1 {
		return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
	}

	arg0 := c.Get(0)

	if arg0 == nil {
		return Bool(true), nil
	}

	_, ok := arg0.(*UndefinedType)
	if ok {
		return Bool(true), nil
	}

	nv := ConvertFromObject(arg0)

	return Bool(tk.IsNilOrEmpty(nv)), nil
}

func builtinIsNilOrErrFunc(c Call) (Object, error) {
	if c.Len() < 1 {
		return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
	}

	arg0 := c.Get(0)

	switch nv := arg0.(type) {
	case nil:
		return Bool(true), nil
	case *UndefinedType:
		return Bool(true), nil
	case *Error:
		return Bool(true), nil
	case *RuntimeError:
		return Bool(true), nil
	case error:
		if nv != nil {
			return Bool(true), nil
		}
	case String:
		if strings.HasPrefix(nv.Value, "TXERROR:") {
			return Bool(true), nil
		}

		return Bool(false), nil
	case *MutableString:
		if strings.HasPrefix(nv.Value, "TXERROR:") {
			return Bool(true), nil
		}

		return Bool(false), nil
	}

	nv1 := ConvertFromObject(arg0)

	return Bool(tk.IsNilOrErrX(nv1)), nil
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

func builtinLockFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
	}

	nv, ok := args[0].(*Mutex)

	if !ok {
		return Undefined, NewCommonErrorWithPos(c, "invalid parameter type: %T", args[0])
	}

	if nv == nil {
		return Undefined, NewCommonErrorWithPos(c, "parameter is nil: %v", args[0])
	}

	nv.Value.Lock()

	return Undefined, nil
}

func builtinUnlockFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
	}

	nv, ok := args[0].(*Mutex)

	if !ok {
		return Undefined, NewCommonErrorWithPos(c, "invalid parameter type: %T", args[0])
	}

	if nv == nil {
		return Undefined, NewCommonErrorWithPos(c, "parameter is nil: %v", args[0])
	}

	nv.Value.Unlock()

	return Undefined, nil
}

func builtinRLockFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
	}

	nv, ok := args[0].(*Mutex)

	if !ok {
		return Undefined, NewCommonErrorWithPos(c, "invalid parameter type: %T", args[0])
	}

	if nv == nil {
		return Undefined, NewCommonErrorWithPos(c, "parameter is nil: %v", args[0])
	}

	nv.Value.RLock()

	return Undefined, nil
}

func builtinRUnlockFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
	}

	nv, ok := args[0].(*Mutex)

	if !ok {
		return Undefined, NewCommonErrorWithPos(c, "invalid parameter type: %T", args[0])
	}

	if nv == nil {
		return Undefined, NewCommonErrorWithPos(c, "parameter is nil: %v", args[0])
	}

	nv.Value.RUnlock()

	return Undefined, nil
}

func builtinTryLockFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
	}

	nv, ok := args[0].(*Mutex)

	if !ok {
		return Undefined, NewCommonErrorWithPos(c, "invalid parameter type: %T", args[0])
	}

	if nv == nil {
		return Undefined, NewCommonErrorWithPos(c, "parameter is nil: %v", args[0])
	}

	b1 := nv.Value.TryLock()

	return Bool(b1), nil
}

func builtinTryRLockFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
	}

	nv, ok := args[0].(*Mutex)

	if !ok {
		return Undefined, NewCommonErrorWithPos(c, "invalid parameter type: %T", args[0])
	}

	if nv == nil {
		return Undefined, NewCommonErrorWithPos(c, "parameter is nil: %v", args[0])
	}

	b1 := nv.Value.TryRLock()

	return Bool(b1), nil
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
	case *Writer:
		return &Any{Value: obj.Value, OriginalType: fmt.Sprintf("%T", obj.Value), OriginalCode: obj.TypeCode()}, nil
	case *File:
		return &Any{Value: obj.Value, OriginalType: fmt.Sprintf("%T", obj.Value), OriginalCode: obj.TypeCode()}, nil
	case *HttpReq:
		return &Any{Value: obj.Value, OriginalType: fmt.Sprintf("%T", obj.Value), OriginalCode: obj.TypeCode()}, nil
	case *HttpResp:
		return &Any{Value: obj.Value, OriginalType: fmt.Sprintf("%T", obj.Value), OriginalCode: obj.TypeCode()}, nil
	case *HttpHandler:
		return &Any{Value: obj.Value, OriginalType: fmt.Sprintf("%T", obj.Value), OriginalCode: obj.TypeCode()}, nil
	case *CharCode:
		return &Any{Value: obj.Value, OriginalType: fmt.Sprintf("%T", obj.Value), OriginalCode: obj.TypeCode()}, nil
	case *Gel:
		return &Any{Value: obj.Value, OriginalType: fmt.Sprintf("%T", obj.Value), OriginalCode: obj.TypeCode()}, nil
	case *OrderedMap:
		return &Any{Value: obj.Value, OriginalType: fmt.Sprintf("%T", obj.Value), OriginalCode: obj.TypeCode()}, nil
	case *BigInt:
		return &Any{Value: obj.Value, OriginalType: fmt.Sprintf("%T", obj.Value), OriginalCode: obj.TypeCode()}, nil
	case *BigFloat:
		return &Any{Value: obj.Value, OriginalType: fmt.Sprintf("%T", obj.Value), OriginalCode: obj.TypeCode()}, nil
	case *Image:
		return &Any{Value: obj.Value, OriginalType: fmt.Sprintf("%T", obj.Value), OriginalCode: obj.TypeCode()}, nil
	case *Delegate:
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

		return Undefined, nil
	case *ObjectPtr:
		// tk.Pl("builtinSetValueByRefFunc *ObjectPtr: %#v %#v", args[0], args[1])
		obj.Value = &args[1]

		return Undefined, nil
	case *Bool:
		nv1, ok := args[1].(Bool)

		if ok {
			(*obj) = nv1
			return Undefined, nil
		}

		(*obj) = Bool(tk.ToBool(ConvertFromObject(args[1])))

		return Undefined, nil
	// case Byte:
	// 	return &Any{Value: byte(obj), OriginalType: fmt.Sprintf("%T", obj), OriginalCode: obj.TypeCode()}, nil
	// case Int:
	// 	return &Any{Value: int(obj), OriginalType: fmt.Sprintf("%T", obj), OriginalCode: obj.TypeCode()}, nil
	// case Uint:
	// 	return &Any{Value: uint64(obj), OriginalType: fmt.Sprintf("%T", obj), OriginalCode: obj.TypeCode()}, nil
	// case Char:
	// 	return &Any{Value: rune(obj), OriginalType: fmt.Sprintf("%T", obj), OriginalCode: obj.TypeCode()}, nil
	// case Float:
	// 	return &Any{Value: float64(obj), OriginalType: fmt.Sprintf("%T", obj), OriginalCode: obj.TypeCode()}, nil
	// case Bytes:
	// 	return &Any{Value: []byte(obj), OriginalType: fmt.Sprintf("%T", obj), OriginalCode: obj.TypeCode()}, nil
	// case Chars:
	// 	return &Any{Value: []rune(obj), OriginalType: fmt.Sprintf("%T", obj), OriginalCode: obj.TypeCode()}, nil
	// case String:
	// 	return &Any{Value: obj.Value, OriginalType: fmt.Sprintf("%T", obj.Value), OriginalCode: obj.TypeCode()}, nil
	case *MutableString:
		(*obj).Value = args[1].String()

		return Undefined, nil
		// return &Any{Value: obj.Value, OriginalType: fmt.Sprintf("%T", obj.Value), OriginalCode: obj.TypeCode()}, nil
	// case Array:
	// 	return &Any{Value: []Object(obj), OriginalType: fmt.Sprintf("%T", obj), OriginalCode: obj.TypeCode()}, nil
	// case Map:
	// 	return &Any{Value: map[string]Object(obj), OriginalType: fmt.Sprintf("%T", obj), OriginalCode: obj.TypeCode()}, nil
	case *StringBuilder:
		return &Any{Value: (*strings.Builder)(obj.Value), OriginalType: fmt.Sprintf("%T", obj.Value), OriginalCode: obj.TypeCode()}, nil
	case *Any:
		nv2 := args[1]
		switch nv1 := obj.Value.(type) {
		case *tk.UndefinedStruct:
			*nv1 = tk.Undefined
			obj.OriginalCode = nv2.TypeCode()
			obj.OriginalType = fmt.Sprintf("%T", nv2.TypeName())
			return Undefined, nil
		case *bool:
			*nv1 = bool(nv2.(Bool))
			obj.OriginalCode = nv2.TypeCode()
			obj.OriginalType = fmt.Sprintf("%T", nv2.TypeName())
			return Undefined, nil
		case *byte:
			*nv1 = byte(ToIntQuick(nv2))
			obj.OriginalCode = nv2.TypeCode()
			obj.OriginalType = fmt.Sprintf("%T", nv2.TypeName())
			return Undefined, nil
		case *int:
			*nv1 = int(ToIntQuick(nv2))
			obj.OriginalCode = nv2.TypeCode()
			obj.OriginalType = fmt.Sprintf("%T", nv2.TypeName())
			return Undefined, nil
		case *int64:
			*nv1 = int64(ToIntQuick(nv2))
			obj.OriginalCode = nv2.TypeCode()
			obj.OriginalType = fmt.Sprintf("%T", nv2.TypeName())
			return Undefined, nil
		case *rune:
			*nv1 = rune(ToIntQuick(nv2))
			obj.OriginalCode = nv2.TypeCode()
			obj.OriginalType = fmt.Sprintf("%T", nv2.TypeName())
			return Undefined, nil
		case *uint:
			*nv1 = uint(ToIntQuick(nv2))
			obj.OriginalCode = nv2.TypeCode()
			obj.OriginalType = fmt.Sprintf("%T", nv2.TypeName())
			return Undefined, nil
		case *uint32:
			*nv1 = uint32(ToIntQuick(nv2))
			obj.OriginalCode = nv2.TypeCode()
			obj.OriginalType = fmt.Sprintf("%T", nv2.TypeName())
			return Undefined, nil
		case *uint64:
			*nv1 = uint64(ToIntQuick(nv2))
			obj.OriginalCode = nv2.TypeCode()
			obj.OriginalType = fmt.Sprintf("%T", nv2.TypeName())
			return Undefined, nil
		case *float32:
			*nv1 = float32(ToFloatQuick(nv2))
			obj.OriginalCode = nv2.TypeCode()
			obj.OriginalType = fmt.Sprintf("%T", nv2.TypeName())
			return Undefined, nil
		case *float64:
			*nv1 = float64(ToFloatQuick(nv2))
			obj.OriginalCode = nv2.TypeCode()
			obj.OriginalType = fmt.Sprintf("%T", nv2.TypeName())
			return Undefined, nil
		case *string:
			*nv1 = nv2.String()
			obj.OriginalCode = nv2.TypeCode()
			obj.OriginalType = fmt.Sprintf("%T", nv2.TypeName())
			return Undefined, nil
		case *interface{}:
			*nv1 = ConvertToObject(nv2)
			obj.OriginalCode = nv2.TypeCode()
			obj.OriginalType = fmt.Sprintf("%T", nv2.TypeName())
			return Undefined, nil
			// return &Any{Value: *nv1, OriginalType: fmt.Sprintf("%T", *nv1), OriginalCode: -1}, nil
		}

		valueT := reflect.ValueOf(obj.Value)

		kindT := valueT.Kind()

		if kindT != reflect.Pointer {
			return NewCommonErrorWithPos(c, "not pointer type of param 1: %T", obj.Value), nil
		}

		elemT := valueT.Elem()

		if !elemT.CanSet() {
			return NewCommonErrorWithPos(c, "value cannot be set: %#v", obj.Value), nil
		}

		elemT.Set(reflect.ValueOf(ConvertToObject(nv2)))

		return Undefined, nil

		// return ConvertToObject(elemT.Interface()), nil
		// return &Any{Value: obj.Value, OriginalType: fmt.Sprintf("%T", obj.Value), OriginalCode: obj.TypeCode()}, nil
	default:
		return NewCommonErrorWithPos(c, "invalid type of param 1: %v", args[0]), nil
	}
}

func builtinUnrefFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return nil, fmt.Errorf("not enough parameters")
	}

	switch nv := args[0].(type) {
	case *ObjectRef:
		// tk.Pl("builtinSetValueByRefFunc *ObjectRef: %#v %#v", args[0], args[1])

		return *(nv.Value), nil
	case *Any:
		if nv == nil {
			return Undefined, nil
		}

		switch nv1 := nv.Value.(type) {
		case tk.UndefinedStruct:
			return Undefined, nil
		case *tk.UndefinedStruct:
			return Undefined, nil
		case *bool:
			return Bool(*nv1), nil
		case *byte:
			return Byte(*nv1), nil
		case *int:
			return Int(*nv1), nil
		case *int16:
			return Int(*nv1), nil
		case *int64:
			return Int(*nv1), nil
		case *rune:
			return Char(*nv1), nil
		case *uint:
			return Uint(*nv1), nil
		case *uint16:
			return Uint(*nv1), nil
		case *uint32:
			return Uint(*nv1), nil
		case *uint64:
			return Uint(*nv1), nil
		case *float32:
			return Float(*nv1), nil
		case *float64:
			return Float(*nv1), nil
		case *string:
			return String{Value: *nv1}, nil
		case *interface{}:
			return &Any{Value: *nv1, OriginalType: fmt.Sprintf("%T", *nv1), OriginalCode: -1}, nil
		}

		valueT := reflect.ValueOf(nv.Value)

		kindT := valueT.Kind()

		if kindT != reflect.Pointer {
			return Undefined, fmt.Errorf("not pointer type")
		}

		elemT := valueT.Elem()

		if !elemT.CanInterface() {
			return Undefined, fmt.Errorf("value cannot convert to interface")
		}

		return ConvertToObject(elemT.Interface()), nil

	default:
		return Undefined, fmt.Errorf("unsupported type: (%T) %v", args[0], args[0])
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

func builtinToHexFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return ToStringObject(""), nil
	}

	rsT := tk.ToHex(ConvertFromObject(args[0]))

	return ToStringObject(rsT), nil
}

func builtinToIntFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return Int(0), nil
	}

	if len(args) > 1 {
		rsT := tk.ToInt(ConvertFromObject(args[0]), ToGoIntQuick(args[1]))

		return Int(rsT), nil
	}

	rsT := tk.ToInt(ConvertFromObject(args[0]), 0)

	return Int(rsT), nil
}

func builtinToFloatFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return Float(0.0), nil
	}

	rsT := tk.ToFloat(ConvertFromObject(args[0]), 0.0)

	return Float(rsT), nil
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
		n, e := fmt.Printf("[%v] (%v)%v\n", i, v.TypeName(), v)

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

func builtinRemoveDirFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	filePathT := args[0].String()

	if !tk.IfFileExists(filePathT) {
		return NewCommonErrorWithPos(c, "dir not exists"), nil
	}

	if !tk.IsDirectory(filePathT) {
		return NewCommonErrorWithPos(c, "not a directory"), nil
	}

	optsA := args[1:]

	ifRecursivelyT := IfSwitchExistsInObjects(optsA, "-recursive")

	if ifRecursivelyT {
		errT := os.RemoveAll(filePathT)

		if errT != nil {
			return NewCommonErrorWithPos(c, "%v", errT), nil
		}

		return Undefined, nil
	}

	errT := os.Remove(filePathT)

	if errT != nil {
		return NewCommonErrorWithPos(c, "%v", errT), nil
	}

	return Undefined, nil
}

func builtinRemovePathFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	filePathT := args[0].String()

	if !tk.IfFileExists(filePathT) {
		return NewCommonErrorWithPos(c, "dir not exists"), nil
	}

	optsA := args[1:]

	ifRecursivelyT := IfSwitchExistsInObjects(optsA, "-recursive")

	if ifRecursivelyT {
		errT := os.RemoveAll(filePathT)

		if errT != nil {
			return NewCommonErrorWithPos(c, "%v", errT), nil
		}

		return Undefined, nil
	}

	errT := os.Remove(filePathT)

	if errT != nil {
		return NewCommonErrorWithPos(c, "%v", errT), nil
	}

	return Undefined, nil
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

func builtinReadCsvFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	r1, ok := args[0].(*Reader)

	var readerT *csv.Reader

	if !ok {
		filePathT := args[0].String()

		f, err := os.Open(filePathT)

		if err != nil {
			return NewCommonErrorWithPos(c, "failed to open file: %v", err), nil
		}

		defer f.Close()

		readerT = csv.NewReader(f)

	} else {
		readerT = csv.NewReader(r1.Value)
	}

	readerT.LazyQuotes = true

	rowsT, err := readerT.ReadAll()
	if err != nil {
		return NewCommonErrorWithPos(c, "failed to read file content: %v", err), nil
	}

	return ConvertToObject(rowsT), nil

}

// writeCsv(writerA/filePathA, dataA, ...optsA)
func builtinWriteCsvFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	vs := ObjectsToS(args[2:])

	r1, ok := args[0].(*Writer)

	var writerT *csv.Writer

	if !ok {
		filePathT := args[0].String()

		rsT := tk.OpenFile(filePathT, vs...)

		if tk.IsError(rsT) {
			return NewCommonErrorWithPos(c, "failed to open file: %v", rsT), nil
		}

		f := rsT.(*os.File)

		defer f.Close()

		writerT = csv.NewWriter(f)

	} else {
		writerT = csv.NewWriter(r1.Value)
	}

	if tk.IfSwitchExists(vs, "-useCRLF") {
		writerT.UseCRLF = true
	} else {
		writerT.UseCRLF = false
	}

	switch nv := args[1].(type) {
	case Array:
		for i, v := range nv {
			switch nvi := v.(type) {
			case Array:
				strAryT := make([]string, len(nvi))

				for j, jv := range nvi {
					strAryT[j] = jv.String()
				}

				errT := writerT.Write(strAryT)

				if errT != nil {
					return NewCommonErrorWithPos(c, "failed to write record of line(%v): %v", i, errT), nil
				}

				writerT.Flush()

				return Undefined, nil
			}
		}
	case *Any:
		switch nvi := nv.Value.(type) {
		case [][]string:
			errT := writerT.WriteAll(nvi)

			if errT != nil {
				return NewCommonErrorWithPos(c, "failed to write records: %v", errT), nil
			}

			writerT.Flush()

			return Undefined, nil
		}
	}

	return NewCommonErrorWithPos(c, "unsupported type for write csv content: %T", args[1]), nil
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
		if MainCompilerOptions != nil {
			compilerOptionsT = MainCompilerOptions
		} else {
			compilerOptionsT = &DefaultCompilerOptions
		}
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

func builtinOrderedMapFunc(c Call) (Object, error) {
	args := c.GetArgs()

	return NewOrderedMap(args...)
}

func builtinBigIntFunc(c Call) (Object, error) {
	return NewBigInt(c)
}

func builtinBigFloatFunc(c Call) (Object, error) {
	return NewBigFloat(c)
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

func builtinServeFileFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 3 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	v1, ok := args[0].(*HttpResp)
	if !ok {
		return NewCommonErrorWithPos(c, "invalid object type: (%T)%v", args[0], args[0]), nil
	}

	v2, ok := args[1].(*HttpReq)
	if !ok {
		return NewCommonErrorWithPos(c, "invalid object type: (%T)%v", args[1], args[1]), nil
	}

	v3 := args[2].String()

	http.ServeFile(v1.Value, v2.Value, v3)

	return Undefined, nil
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

func builtinIsHttpsFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	nv1, ok := args[0].(*HttpReq)
	if !ok {
		return NewCommonErrorWithPos(c, "invalid object type: (%T)%v", args[0], args[0]), nil
	}

	rs := tk.IsHttps(nv1.Value)

	return Bool(rs), nil
}

func builtinPostRequestFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 4 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	// nv1, ok := args[0].(*HttpReq)
	// if !ok {
	// 	return NewCommonErrorWithPos(c, "invalid object type: (%T)%v", args[0], args[0]), nil
	// }

	rs, errT := tk.PostRequestX(args[0].String(), args[1].String(), args[2].String(), time.Duration(ToGoIntWithDefault(args[3], 30)), ObjectsToS(args[4:])...)

	if errT != nil {
		return NewCommonErrorWithPos(c, errT.Error()), nil
	}

	return ToStringObject(rs), nil
}

func builtinHttpRedirectFunc(c Call) (Object, error) {
	args := c.GetArgs()

	lenT := len(args)

	if lenT < 3 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	nv1, ok := args[0].(*HttpResp)
	if !ok {
		return NewCommonErrorWithPos(c, "invalid object type: (%T)%v", args[0], args[0]), nil
	}

	nv2, ok := args[1].(*HttpReq)
	if !ok {
		return NewCommonErrorWithPos(c, "invalid object type: (%T)%v", args[1], args[1]), nil
	}

	nv3 := args[2].String()

	statusCodeT := http.StatusFound

	if lenT > 3 {
		statusCodeT = ToGoIntWithDefault(args[3], 302)
	}

	http.Redirect(nv1.Value, nv2.Value, nv3, statusCodeT)

	return Undefined, nil
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
	// 	return builtinStringBuilderFunc(Call{Args: args[1:]})
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
		return builtinTimeFunc(Call{Args: args[1:]})
	case "stringBuilder":
		return builtinStringBuilderFunc(Call{Args: args[1:]})
	case "any":
		return builtinAnyFunc(Call{Args: args[1:]})
	case "ref", "objectRef":
		return &ObjectRef{Value: nil}, nil
	case "statusResult":
		return builtinStatusResultFunc(Call{Args: args[1:]})
	case "database":
		return builtinStatusResultFunc(Call{Args: args[1:]})
	case "seq":
		return builtinSeqFunc(Call{Args: args[1:]})
	case "mutex":
		return builtinMutexFunc(Call{Args: args[1:]})
	case "mux":
		return builtinMuxFunc(Call{Args: args[1:]})
	case "charCode":
		return builtinCharCodeFunc(Call{Args: args[1:]})
	case "gel":
		return builtinGelFunc(Call{Args: args[1:]})
	case "orderedMap":
		return NewOrderedMap(args[1:]...)
	case "bigInt":
		return NewBigInt(Call{Args: args[1:]})
	case "bigFloat":
		return NewBigFloat(Call{Args: args[1:]})
	case "httpHandler":
		return NewHttpHandler(Call{Args: args[1:]})
	case "image":
		return NewImage(Call{Args: args[1:]})
	case "delegate":
		return NewDelegate(Call{Args: args[1:]})
	case "etable":
		return NewEtable(Call{Args: args[1:]})
	case "reader":
		return NewReader(Call{Args: args[1:]})
	case "writer":
		return NewWriter(Call{Args: args[1:]})
	case "file":
		return NewFile(Call{Args: args[1:]})
	case "undefined":
		return Undefined, nil
	}

	return Undefined, NewCommonErrorWithPos(c, "invalid data type: %v", s1)
}

func builtinReadAllStrFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	nv1, ok := args[0].(*Reader)

	if ok {
		bufT, errT := io.ReadAll(nv1.Value)
		if errT != nil {
			return NewCommonErrorWithPos(c, "failed to read all string: %v", errT), nil
		}

		return String{Value: string(bufT)}, nil
	}

	nv1a, ok := args[0].(*File)

	if ok {
		bufT, errT := io.ReadAll(nv1a.Value)
		if errT != nil {
			return NewCommonErrorWithPos(c, "failed to read all string: %v", errT), nil
		}

		return String{Value: string(bufT)}, nil
	}

	nv2, ok := args[0].(io.Reader)

	if ok {
		bufT, errT := io.ReadAll(nv2)
		if errT != nil {
			return NewCommonErrorWithPos(c, "failed to read all string: %v", errT), nil
		}

		return String{Value: string(bufT)}, nil
	}

	nv3, ok := args[0].(*Any)

	switch nv := nv3.Value.(type) {
	case string:
		return String{Value: nv}, nil
	default:
		return NewCommonErrorWithPos(c, "invalid type in any: %T", nv3.Value), nil
	}

	return NewCommonErrorWithPos(c, "unsupported type for read all: %T", args[0]), nil
}

func builtinReadAllBytesFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	nv1, ok := args[0].(*Reader)

	if ok {
		bufT, errT := io.ReadAll(nv1.Value)
		if errT != nil {
			return NewCommonErrorWithPos(c, "failed to read all string: %v", errT), nil
		}

		return Bytes(bufT), nil
	}

	nv1a, ok := args[0].(*File)

	if ok {
		bufT, errT := io.ReadAll(nv1a.Value)
		if errT != nil {
			return NewCommonErrorWithPos(c, "failed to read all string: %v", errT), nil
		}

		return Bytes(bufT), nil
	}

	nv2, ok := args[0].(io.Reader)

	if ok {
		bufT, errT := io.ReadAll(nv2)
		if errT != nil {
			return NewCommonErrorWithPos(c, "failed to read all string: %v", errT), nil
		}

		return Bytes(bufT), nil
	}

	nv3, ok := args[0].(*Any)

	switch nv := nv3.Value.(type) {
	case string:
		return Bytes(nv), nil
	default:
		return NewCommonErrorWithPos(c, "invalid type in any: %T", nv3.Value), nil
	}

	return NewCommonErrorWithPos(c, "unsupported type for read all: %T", args[0]), nil
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

	nv1, ok := args[0].(*Writer)

	if ok {
		n, errT := nv1.Value.Write([]byte(s1.Value))

		if errT != nil {
			return NewCommonErrorWithPos(c, "failed to write string: %v", errT), nil
		}

		return Int(n), nil
	}

	nv2, ok := args[0].(*File)

	if ok {
		n, errT := nv2.Value.Write([]byte(s1.Value))

		if errT != nil {
			return NewCommonErrorWithPos(c, "failed to write string: %v", errT), nil
		}

		return Int(n), nil
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

func builtinWriteBytesFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	var bufT []byte
	var errT error
	var n int

	switch nv := args[1].(type) {
	case Bytes:
		bufT = []byte(nv)
	case String:
		bufT = []byte(nv.Value)
	case *MutableString:
		bufT = []byte(nv.Value)
	default:
		return NewCommonErrorWithPos(c, "unsupport content type to write: %T", args[1]), nil
	}

	nv1, ok := args[0].(*Writer)

	if ok {
		n, errT := nv1.Value.Write(bufT)

		if errT != nil {
			return NewCommonErrorWithPos(c, "failed to write string: %v", errT), nil
		}

		return Int(n), nil
	}

	nv3, ok := args[0].(*File)

	if ok {
		n, errT := nv3.Value.Write(bufT)

		if errT != nil {
			return NewCommonErrorWithPos(c, "failed to write string: %v", errT), nil
		}

		return Int(n), nil
	}

	switch nv2 := args[0].(type) {
	case *StringBuilder:
		n, errT = nv2.Value.Write(bufT)

		if errT != nil {
			return NewCommonErrorWithPos(c, "%v", errT), nil
		}

		return Int(n), nil
	case *BytesBuffer:
		n, errT = nv2.Value.Write(bufT)

		if errT != nil {
			return NewCommonErrorWithPos(c, "%v", errT), nil
		}

		return Int(n), nil
	case *MutableString:
		nv2.Value += string(bufT)

		n = len(bufT)

		return Int(n), nil
	case *Any:
		nvi1, ok := nv2.Value.(io.Writer)

		if !ok {
			switch nvi2 := nv2.Value.(type) {
			case *strings.Builder:
				n, errT := nvi2.Write(bufT)

				if errT != nil {
					return NewCommonErrorWithPos(c, "%v", errT), nil
				}

				return Int(n), nil
			case *bytes.Buffer:
				n, errT := nvi2.Write(bufT)

				if errT != nil {
					return NewCommonErrorWithPos(c, "%v", errT), nil
				}

				return Int(n), nil
			case string:
				nv2.Value = nvi2 + string(bufT)
				return Int(len(bufT)), nil
			case []byte:
				nv2.Value = append(nvi2, bufT...)
				return Int(len(bufT)), nil
			case io.StringWriter:
				n, errT := nvi2.WriteString(string(bufT))

				if errT != nil {
					return Int(n), nil
				}

				return NewCommonErrorWithPos(c, "%v", errT), nil
				// default:
				// 	return NewCommonErrorWithPos(c, "invalid type in any: %T", nv2.Value), nil
			}

			return NewCommonErrorWithPos(c, "unsupport object type to write: %T", args[0]), nil
		}

		n, errT = nvi1.Write(bufT)

		if errT != nil {
			return NewCommonErrorWithPos(c, "%v", errT), nil
		}

		return Int(n), nil
	default:
		return NewCommonErrorWithPos(c, "unsupport object type to write: %T", args[0]), nil
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

	v0, ok := args[2].(*HttpReq)
	if !ok {
		return NewCommonErrorWithPos(c, "invalid param type: %T(%v)", args[0], args[0]), nil
	}

	rsT := tk.GenerateJSONPResponseWithMore(args[0].String(), args[1].String(), v0.Value, ObjectsToS(args[3:])...)

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

func builtinLeClearFunc(c Call) (Object, error) {
	vmT := c.VM()

	if vmT == nil {
		return NewCommonErrorWithPos(c, "no VM specified"), nil
	}

	vmT.LeBuf = make([]string, 0, 100)

	return Undefined, nil
}

func builtinLeLoadFromStrFunc(c Call) (Object, error) {
	vmT := c.VM()

	if vmT == nil {
		return NewCommonErrorWithPos(c, "no VM specified"), nil
	}

	// if vmT.LeBuf == nil {
	// 	builtinLeClearFunc(c)
	// }

	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonError("not enough parameters"), nil
	}

	vmT.LeBuf = tk.SplitLines(args[0].String())

	return Undefined, nil
}

func builtinLeAppendFromStrFunc(c Call) (Object, error) {
	vmT := c.VM()

	if vmT == nil {
		return NewCommonErrorWithPos(c, "no VM specified"), nil
	}

	if vmT.LeBuf == nil {
		builtinLeClearFunc(c)
	}

	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonError("not enough parameters"), nil
	}

	vmT.LeBuf = append(vmT.LeBuf, tk.SplitLines(args[0].String())...)

	return Undefined, nil
}

func builtinLeSaveToStrFunc(c Call) (Object, error) {
	vmT := c.VM()

	if vmT == nil {
		return NewCommonErrorWithPos(c, "no VM specified"), nil
	}

	if vmT.LeBuf == nil {
		builtinLeClearFunc(c)
	}

	return String{Value: tk.JoinLines(vmT.LeBuf, "\n")}, nil
}

func builtinLeLoadFromFileFunc(c Call) (Object, error) {
	vmT := c.VM()

	if vmT == nil {
		return NewCommonErrorWithPos(c, "no VM specified"), nil
	}

	// if vmT.LeBuf == nil {
	// 	builtinLeClearFunc(c)
	// }

	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonError("not enough parameters"), nil
	}

	strT, errT := tk.LoadStringFromFileE(args[0].String())

	if errT != nil {
		return NewCommonErrorWithPos(c, "%v", errT), nil
	}

	vmT.LeBuf = tk.SplitLines(strT)

	return Undefined, nil
}

func builtinLeLoadFromUrlFunc(c Call) (Object, error) {
	vmT := c.VM()

	if vmT == nil {
		return NewCommonErrorWithPos(c, "no VM specified"), nil
	}

	// if vmT.LeBuf == nil {
	// 	builtinLeClearFunc(c)
	// }

	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonError("not enough parameters"), nil
	}

	strT := tk.GetWeb(args[0].String(), ObjectsToI(args[1:])...)

	if tk.IsErrX(strT) {
		return NewCommonErrorWithPos(c, "%v", tk.GetErrStrX(strT)), nil
	}

	vmT.LeBuf = tk.SplitLines(strT.(string))

	return Undefined, nil
}

func builtinLeLoadFromSshFunc(c Call) (Object, error) {
	vmT := c.VM()

	if vmT == nil {
		return NewCommonErrorWithPos(c, "no VM specified"), nil
	}

	if vmT.LeSshInfo == nil {
		vmT.LeSshInfo = make(map[string]string)
	}

	// if vmT.LeBuf == nil {
	// 	builtinLeClearFunc(c)
	// }

	args := c.GetArgs()

	if len(args) < 5 {
		return NewCommonError("not enough parameters"), nil
	}

	pa := ObjectsToS(args)

	var v1, v2, v3, v4, v5 string

	v1 = tk.SafelyGetStringForKeyWithDefault(vmT.LeSshInfo, "Host")
	v2 = tk.SafelyGetStringForKeyWithDefault(vmT.LeSshInfo, "Port")
	v3 = tk.SafelyGetStringForKeyWithDefault(vmT.LeSshInfo, "User")
	v4 = tk.SafelyGetStringForKeyWithDefault(vmT.LeSshInfo, "Password")
	v5 = tk.SafelyGetStringForKeyWithDefault(vmT.LeSshInfo, "Path")

	v1 = tk.GetSwitch(pa, "-host=", v1)
	v2 = tk.GetSwitch(pa, "-port=", v2)
	v3 = tk.GetSwitch(pa, "-user=", v3)
	v4 = tk.GetSwitch(pa, "-password=", v4)
	if strings.HasPrefix(v4, "740404") {
		v4 = tk.DecryptStringByTXDEF(v4)
	}
	v5 = tk.GetSwitch(pa, "-path=", v5)

	sshT, errT := tk.NewSSHClient(v1, v2, v3, v4)

	if errT != nil {
		return NewCommonErrorWithPos(c, "failed to connect to server: %v", errT), nil
	}

	defer sshT.Close()

	bufT, errT := sshT.GetFileContent(v5)

	if errT != nil {
		return NewCommonErrorWithPos(c, "failed to get file content from server: %v", errT), nil
	}

	vmT.LeSshInfo["Host"] = v1
	vmT.LeSshInfo["Port"] = v2
	vmT.LeSshInfo["User"] = v3
	vmT.LeSshInfo["Password"] = v4
	vmT.LeSshInfo["Path"] = v5

	_, errT = builtinLeLoadFromStrFunc(Call{Args: []Object{String{Value: string(bufT)}}})
	if errT != nil {
		return NewCommonErrorWithPos(c, "failed to load file content into le buffer: %v", errT), nil
	}

	return Undefined, nil
}

func builtinLeSaveToSshFunc(c Call) (Object, error) {
	vmT := c.VM()

	if vmT == nil {
		return NewCommonErrorWithPos(c, "no VM specified"), nil
	}

	if vmT.LeSshInfo == nil {
		vmT.LeSshInfo = make(map[string]string)
	}

	// if vmT.LeBuf == nil {
	// 	builtinLeClearFunc(c)
	// }

	args := c.GetArgs()

	if len(args) < 5 {
		return NewCommonError("not enough parameters"), nil
	}

	pa := ObjectsToS(args)

	var v1, v2, v3, v4, v5 string

	v1 = tk.SafelyGetStringForKeyWithDefault(vmT.LeSshInfo, "Host")
	v2 = tk.SafelyGetStringForKeyWithDefault(vmT.LeSshInfo, "Port")
	v3 = tk.SafelyGetStringForKeyWithDefault(vmT.LeSshInfo, "User")
	v4 = tk.SafelyGetStringForKeyWithDefault(vmT.LeSshInfo, "Password")
	v5 = tk.SafelyGetStringForKeyWithDefault(vmT.LeSshInfo, "Path")

	v1 = tk.GetSwitch(pa, "-host=", v1)
	v2 = tk.GetSwitch(pa, "-port=", v2)
	v3 = tk.GetSwitch(pa, "-user=", v3)
	v4 = tk.GetSwitch(pa, "-password=", v4)
	if strings.HasPrefix(v4, "740404") {
		v4 = tk.DecryptStringByTXDEF(v4)
	}
	v5 = tk.GetSwitch(pa, "-path=", v5)

	sshT, errT := tk.NewSSHClient(v1, v2, v3, v4)

	if errT != nil {
		return NewCommonErrorWithPos(c, "failed to connect to server: %v", errT), nil
	}

	defer sshT.Close()

	errT = sshT.UploadFileContent([]byte(tk.JoinLines(vmT.LeBuf, "\n")), v5)

	if errT != nil {
		return NewCommonErrorWithPos(c, "failed to get file content from server: %v", errT), nil
	}

	vmT.LeSshInfo["Host"] = v1
	vmT.LeSshInfo["Port"] = v2
	vmT.LeSshInfo["User"] = v3
	vmT.LeSshInfo["Password"] = v4
	vmT.LeSshInfo["Path"] = v5

	return Undefined, nil
}

func builtinLeAppendFromFileFunc(c Call) (Object, error) {
	vmT := c.VM()

	if vmT == nil {
		return NewCommonErrorWithPos(c, "no VM specified"), nil
	}

	if vmT.LeBuf == nil {
		builtinLeClearFunc(c)
	}

	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonError("not enough parameters"), nil
	}

	strT, errT := tk.LoadStringFromFileE(args[0].String())

	if errT != nil {
		return NewCommonErrorWithPos(c, "%v", errT), nil
	}

	vmT.LeBuf = append(vmT.LeBuf, tk.SplitLines(strT)...)

	return Undefined, nil
}

func builtinLeSaveToFileFunc(c Call) (Object, error) {
	vmT := c.VM()

	if vmT == nil {
		return NewCommonErrorWithPos(c, "no VM specified"), nil
	}

	if vmT.LeBuf == nil {
		builtinLeClearFunc(c)
	}

	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	var errT error

	textT := tk.JoinLines(vmT.LeBuf, "\n")

	if tk.IsErrStr(textT) {
		return NewCommonErrorWithPos(c, "%v", tk.GetErrStr(textT)), nil
	}

	errT = tk.SaveStringToFileE(textT, args[0].String())

	if errT != nil {
		return NewCommonErrorWithPos(c, "%v", errT), nil
	}

	return Undefined, nil
}

func builtinLeAppendToFileFunc(c Call) (Object, error) {
	vmT := c.VM()

	if vmT == nil {
		return NewCommonErrorWithPos(c, "no VM specified"), nil
	}

	if vmT.LeBuf == nil {
		builtinLeClearFunc(c)
	}

	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	textT := tk.JoinLines(vmT.LeBuf, "\n")

	if tk.IsErrStr(textT) {
		return NewCommonErrorWithPos(c, "%v", tk.GetErrStr(textT)), nil
	}

	errStrT := tk.AppendStringToFile(textT, args[0].String())

	if tk.IsErrStr(errStrT) {
		return NewCommonErrorWithPos(c, "%v", tk.GetErrStr(errStrT)), nil
	}

	return Undefined, nil
}

func builtinLeLoadFromClipFunc(c Call) (Object, error) {
	vmT := c.VM()

	if vmT == nil {
		return NewCommonErrorWithPos(c, "no VM specified"), nil
	}

	// if vmT.LeBuf == nil {
	// 	builtinLeClearFunc(c)
	// }

	// args := c.GetArgs()

	// if len(args) < 1 {
	// 	return NewCommonError("not enough parameters"), nil
	// }

	textT := tk.GetClipText()

	if tk.IsErrStr(textT) {
		return NewCommonErrorWithPos(c, "%v", tk.GetErrStr(textT)), nil
	}

	vmT.LeBuf = tk.SplitLines(textT)

	return Undefined, nil
}

func builtinLeSaveToClipFunc(c Call) (Object, error) {
	vmT := c.VM()

	if vmT == nil {
		return NewCommonErrorWithPos(c, "no VM specified"), nil
	}

	if vmT.LeBuf == nil {
		builtinLeClearFunc(c)
	}

	// args := c.GetArgs()

	// if len(args) < 1 {
	// 	return NewCommonErrorWithPos(c, "not enough parameters"), nil
	// }

	textT := tk.JoinLines(vmT.LeBuf, "\n")

	if tk.IsErrStr(textT) {
		return NewCommonErrorWithPos(c, "%v", tk.GetErrStr(textT)), nil
	}

	errT := tk.SetClipText(textT)

	if errT != nil {
		return NewCommonErrorWithPos(c, "%v", errT), nil
	}

	return Undefined, nil
}

func builtinLeViewAllFunc(c Call) (Object, error) {
	vmT := c.VM()

	if vmT == nil {
		return NewCommonErrorWithPos(c, "no VM specified"), nil
	}

	if vmT.LeBuf == nil {
		builtinLeClearFunc(c)
	}

	args := c.GetArgs()

	// if len(args) < 1 {
	// 	return NewCommonError("not enough parameters"), nil
	// }

	noLineNumberT := tk.IfSwitchExistsWhole(ObjectsToS(args), "-nl")

	for i, v := range vmT.LeBuf {
		if noLineNumberT {
			tk.Pl("%v", v)
		} else {
			tk.Pl("%v: %v", i, v)
		}
	}

	return Undefined, nil
}

func builtinLeViewLinesFunc(c Call) (Object, error) {
	vmT := c.VM()

	if vmT == nil {
		return NewCommonErrorWithPos(c, "no VM specified"), nil
	}

	if vmT.LeBuf == nil {
		builtinLeClearFunc(c)
	}

	args := c.GetArgs()

	if len(args) < 2 {
		return NewCommonError("not enough parameters"), nil
	}

	startA := tk.ToInt(ConvertFromObject(args[0]), 0)

	endA := tk.ToInt(ConvertFromObject(args[1]), -1)

	if startA >= len(vmT.LeBuf) {
		return NewCommonError("start index out of range: %v/%v", startA, len(vmT.LeBuf)), nil
	}

	if startA < 0 {
		return NewCommonError("start index out of range: %v/%v", startA, len(vmT.LeBuf)), nil
	}

	if endA < 0 {
		endA = len(vmT.LeBuf) - 1
	}

	if endA >= len(vmT.LeBuf) {
		return NewCommonError("end index out of range: %v/%v", endA, len(vmT.LeBuf)), nil
	}

	noLineNumberT := tk.IfSwitchExistsWhole(ObjectsToS(args), "-nl")

	for i := startA; i <= endA; i++ {
		if noLineNumberT {
			tk.Pl("%v", vmT.LeBuf[i])
		} else {
			tk.Pl("%v: %v", i, vmT.LeBuf[i])
		}
	}

	return Undefined, nil
}

func builtinLeGetListFunc(c Call) (Object, error) {
	vmT := c.VM()

	if vmT == nil {
		return NewCommonErrorWithPos(c, "no VM specified"), nil
	}

	if vmT.LeBuf == nil {
		builtinLeClearFunc(c)
	}

	args := c.GetArgs()

	// if len(args) < 1 {
	// 	return NewCommonError("not enough parameters"), nil
	// }

	startT := 0

	if len(args) > 0 {
		startT = ToGoIntWithDefault(args[0], 0)
	}

	lenT := len(vmT.LeBuf)

	if startT < 0 {
		return NewCommonError("start index out of range: %v/%v", startT, lenT), nil
	}

	endT := lenT - 1

	if len(args) > 1 {
		endT = ToGoIntWithDefault(args[1], -1)
	}

	if endT < 0 {
		endT = lenT - 1
	}

	if endT > lenT-1 {
		return NewCommonError("start index out of range: %v/%v", endT, lenT), nil
	}

	rs := make(Array, 0, lenT)

	for i := startT; i <= endT; i++ {
		rs = append(rs, String{Value: vmT.LeBuf[i]})
	}

	return rs, nil
}

func builtinLeViewLineFunc(c Call) (Object, error) {
	vmT := c.VM()

	if vmT == nil {
		return NewCommonErrorWithPos(c, "no VM specified"), nil
	}

	if vmT.LeBuf == nil {
		builtinLeClearFunc(c)
	}

	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonError("not enough parameters"), nil
	}

	idxA := tk.ToInt(ConvertFromObject(args[0]), 0)

	if idxA < 0 || idxA >= len(vmT.LeBuf) {
		return NewCommonErrorWithPos(c, "line index out of range"), nil
	}

	tk.Pln(vmT.LeBuf[idxA])

	return Undefined, nil
}

func builtinLeSortFunc(c Call) (Object, error) {
	vmT := c.VM()

	if vmT == nil {
		return NewCommonErrorWithPos(c, "no VM specified"), nil
	}

	if vmT.LeBuf == nil {
		builtinLeClearFunc(c)
	}

	args := c.GetArgs()

	// if len(args) < 1 {
	// 	return NewCommonError("not enough parameters"), nil
	// }

	argListT := ObjectsToS(args)

	descentT := false

	if tk.GetSwitch(argListT, "-order=", "") == "desc" {
		descentT = true
	}

	if descentT {
		sort.Sort(sort.Reverse(sort.StringSlice(vmT.LeBuf)))
	} else {
		sort.Sort(sort.StringSlice(vmT.LeBuf))
	}

	return Undefined, nil
}

func builtinLeConvertToUtf8Func(c Call) (Object, error) {
	vmT := c.VM()

	if vmT == nil {
		return NewCommonErrorWithPos(c, "no VM specified"), nil
	}

	if vmT.LeBuf == nil {
		builtinLeClearFunc(c)
	}

	args := c.GetArgs()

	// if len(args) < 1 {
	// 	return NewCommonError("not enough parameters"), nil
	// }

	encT := GetSwitchFromObjects(args, "-encoding=", "")

	vmT.LeBuf = tk.SplitLines(tk.ConvertStringToUTF8(tk.JoinLines(vmT.LeBuf, "\n"), encT))

	return Undefined, nil
}

func builtinLeGetLineFunc(c Call) (Object, error) {
	vmT := c.VM()

	if vmT == nil {
		return NewCommonErrorWithPos(c, "no VM specified"), nil
	}

	if vmT.LeBuf == nil {
		builtinLeClearFunc(c)
	}

	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonError("not enough parameters"), nil
	}

	idxA := tk.ToInt(ConvertFromObject(args[0]), 0)

	if idxA < 0 || idxA >= len(vmT.LeBuf) {
		return NewCommonErrorWithPos(c, "%v", "line index out of range"), nil
	}

	return String{Value: vmT.LeBuf[idxA]}, nil
}

func builtinLeSetLineFunc(c Call) (Object, error) {
	vmT := c.VM()

	if vmT == nil {
		return NewCommonErrorWithPos(c, "no VM specified"), nil
	}

	if vmT.LeBuf == nil {
		builtinLeClearFunc(c)
	}

	args := c.GetArgs()

	if len(args) < 2 {
		return NewCommonError("not enough parameters"), nil
	}

	idxA := tk.ToInt(ConvertFromObject(args[0]), 0)

	if idxA < 0 || idxA >= len(vmT.LeBuf) {
		return NewCommonErrorWithPos(c, "%v", "line index out of range"), nil
	}

	vmT.LeBuf[idxA] = args[1].String()

	return Undefined, nil
}

func builtinLeSetLinesFunc(c Call) (Object, error) {
	vmT := c.VM()

	if vmT == nil {
		return NewCommonErrorWithPos(c, "no VM specified"), nil
	}

	if vmT.LeBuf == nil {
		builtinLeClearFunc(c)
	}

	args := c.GetArgs()

	if len(args) < 3 {
		return NewCommonError("not enough parameters"), nil
	}

	startA := tk.ToInt(ConvertFromObject(args[0]), 0)

	endA := tk.ToInt(ConvertFromObject(args[1]), -1)

	if startA > endA {
		return NewCommonErrorWithPos(c, "%v", "start index greater than end index"), nil
	}

	listT := tk.SplitLines(args[2].String())

	if endA < 0 {
		rs := make([]string, 0, len(vmT.LeBuf)+len(listT))

		rs = append(rs, listT...)
		rs = append(rs, vmT.LeBuf...)

		vmT.LeBuf = rs

		return Undefined, nil
	}

	if startA >= len(vmT.LeBuf) {
		vmT.LeBuf = append(vmT.LeBuf, listT...)

		return Undefined, nil
	}

	if startA < 0 {
		startA = 0
	}

	if endA >= len(vmT.LeBuf) {
		endA = len(vmT.LeBuf) - 1
	}

	rs := make([]string, 0, len(vmT.LeBuf)+len(listT)-1)

	rs = append(rs, vmT.LeBuf[:startA]...)
	rs = append(rs, listT...)
	rs = append(rs, vmT.LeBuf[endA+1:]...)

	vmT.LeBuf = rs

	return Undefined, nil
}

func builtinLeInsertLineFunc(c Call) (Object, error) {
	vmT := c.VM()

	if vmT == nil {
		return NewCommonErrorWithPos(c, "no VM specified"), nil
	}

	if vmT.LeBuf == nil {
		builtinLeClearFunc(c)
	}

	args := c.GetArgs()

	if len(args) < 2 {
		return NewCommonError("not enough parameters"), nil
	}

	idxA := tk.ToInt(ConvertFromObject(args[0]), 0)

	if idxA < 0 {
		idxA = 0
	}

	listT := tk.SplitLines(args[1].String())

	if idxA >= len(vmT.LeBuf) {
		vmT.LeBuf = append(vmT.LeBuf, listT...)
	} else {
		rs := make([]string, 0, len(vmT.LeBuf)+1)

		rs = append(rs, vmT.LeBuf[:idxA]...)
		rs = append(rs, listT...)
		rs = append(rs, vmT.LeBuf[idxA:]...)

		vmT.LeBuf = rs

	}

	return Undefined, nil
}

func builtinLeAppendLineFunc(c Call) (Object, error) {
	vmT := c.VM()

	if vmT == nil {
		return NewCommonErrorWithPos(c, "no VM specified"), nil
	}

	if vmT.LeBuf == nil {
		builtinLeClearFunc(c)
	}

	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonError("not enough parameters"), nil
	}

	listT := tk.SplitLines(args[0].String())

	vmT.LeBuf = append(vmT.LeBuf, listT...)

	return Undefined, nil
}

func builtinLeRemoveLineFunc(c Call) (Object, error) {
	vmT := c.VM()

	if vmT == nil {
		return NewCommonErrorWithPos(c, "no VM specified"), nil
	}

	if vmT.LeBuf == nil {
		builtinLeClearFunc(c)
	}

	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonError("not enough parameters"), nil
	}

	idxA := tk.ToInt(ConvertFromObject(args[0]), 0)

	if idxA < 0 || idxA >= len(vmT.LeBuf) {
		return NewCommonErrorWithPos(c, "%v", "line index out of range"), nil
	}

	rs := make([]string, 0, len(vmT.LeBuf)+1)

	rs = append(rs, vmT.LeBuf[:idxA]...)
	rs = append(rs, vmT.LeBuf[idxA+1:]...)

	vmT.LeBuf = rs

	return Undefined, nil
}

func builtinLeRemoveLinesFunc(c Call) (Object, error) {
	vmT := c.VM()

	if vmT == nil {
		return NewCommonErrorWithPos(c, "no VM specified"), nil
	}

	if vmT.LeBuf == nil {
		builtinLeClearFunc(c)
	}

	args := c.GetArgs()

	if len(args) < 2 {
		return NewCommonError("not enough parameters"), nil
	}

	startA := tk.ToInt(ConvertFromObject(args[0]), 0)

	endA := tk.ToInt(ConvertFromObject(args[1]), -1)

	if endA < 0 {
		endA = len(vmT.LeBuf) - 1
	}

	if startA < 0 || startA >= len(vmT.LeBuf) {
		return NewCommonErrorWithPos(c, "%v", "start line index out of range"), nil
	}

	if endA < 0 || endA >= len(vmT.LeBuf) {
		return NewCommonErrorWithPos(c, "%v", "end line index out of range"), nil
	}

	if startA > endA {
		return NewCommonErrorWithPos(c, "%v", "start line index greater than end line index"), nil
	}

	rs := make([]string, 0, len(vmT.LeBuf)+1)

	rs = append(rs, vmT.LeBuf[:startA]...)
	rs = append(rs, vmT.LeBuf[endA+1:]...)

	vmT.LeBuf = rs

	return Undefined, nil
}

func builtinLeFindLinesFunc(c Call) (Object, error) {
	vmT := c.VM()

	if vmT == nil {
		return NewCommonErrorWithPos(c, "no VM specified"), nil
	}

	if vmT.LeBuf == nil {
		builtinLeClearFunc(c)
	}

	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonError("not enough parameters"), nil
	}

	regA := args[0].String()

	ifPrintT := IfSwitchExistsInObjects(args[1:], "-print")

	aryT := make(Array, 0, 10)

	for i, v := range vmT.LeBuf {
		if tk.RegContains(v, regA) {
			aryT = append(aryT, Map{"Index": Int(i), "Value": String{Value: v}})

			if ifPrintT {
				tk.Pl("%v: %v", i, v)
			}
		}
	}

	return aryT, nil
}

func builtinLeFindFunc(c Call) (Object, error) {
	vmT := c.VM()

	if vmT == nil {
		return NewCommonErrorWithPos(c, "no VM specified"), nil
	}

	if vmT.LeBuf == nil {
		builtinLeClearFunc(c)
	}

	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonError("not enough parameters"), nil
	}

	groupT := 0

	if len(args) > 1 {
		groupT = ToIntQuick(args[1])
	}

	regA := args[0].String()

	for i, v := range vmT.LeBuf {
		findT := tk.RegFindFirstGroupIndexX(v, regA, groupT)

		if findT != nil {
			return Map{"Line": Int(i), "Group": Int(groupT), "Count": Int(0), "Start": Int(findT[0]), "End": Int(findT[1]), "Text": String{Value: v[findT[0]:findT[1]]}}, nil
		}
	}

	return Undefined, nil
}

func builtinLeFindAllFunc(c Call) (Object, error) {
	vmT := c.VM()

	if vmT == nil {
		return NewCommonErrorWithPos(c, "no VM specified"), nil
	}

	if vmT.LeBuf == nil {
		builtinLeClearFunc(c)
	}

	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonError("not enough parameters"), nil
	}

	groupT := 0

	if len(args) > 1 {
		groupT = ToIntQuick(args[1])
	}

	regA := args[0].String()

	totalFindsT := Array{}

	for i, v := range vmT.LeBuf {
		findsT := tk.RegFindAllByGroupIndexX(v, regA, groupT)

		if findsT != nil {
			for j, jv := range findsT {
				totalFindsT = append(totalFindsT, Map{"Line": Int(i), "Group": Int(groupT), "Count": Int(j), "Start": Int(jv[0]), "End": Int(jv[1]), "Text": String{Value: v[jv[0]:jv[1]]}})
			}
		}
	}

	return totalFindsT, nil
}

func builtinLeReplaceFunc(c Call) (Object, error) {
	vmT := c.VM()

	if vmT == nil {
		return NewCommonErrorWithPos(c, "no VM specified"), nil
	}

	if vmT.LeBuf == nil {
		builtinLeClearFunc(c)
	}

	args := c.GetArgs()

	if len(args) < 2 {
		return NewCommonError("not enough parameters"), nil
	}

	regA := args[0].String()
	replA := args[1].String()

	aryT := make(Array, 0, 10)

	for i, v := range vmT.LeBuf {
		if tk.RegContains(v, regA) {
			vmT.LeBuf[i] = tk.RegReplace(v, regA, replA)
			aryT = append(aryT, String{Value: fmt.Sprintf("%v: %v -> %v", i, v, vmT.LeBuf[i])})
		}
	}

	return aryT, nil
}

func builtinLePrintFunc(c Call) (Object, error) {
	vmT := c.VM()

	if vmT == nil {
		return NewCommonErrorWithPos(c, "no VM specified"), nil
	}

	if vmT.LeBuf == nil {
		builtinLeClearFunc(c)
	}

	args := c.GetArgs()

	ifLineNumberT := IfSwitchExistsInObjects(args, "-lineNumber")
	withLenT := IfSwitchExistsInObjects(args, "-withLen")

	for i, v := range vmT.LeBuf {
		withLenStrT := ""
		if withLenT {
			withLenStrT = "(" + tk.IntToStr(len(v)) + ")"
		}

		if ifLineNumberT {
			tk.Pl("%v%v: %v", i, withLenStrT, v)
		} else {
			tk.Pl("%v%v", withLenStrT, v)
		}
	}

	return Undefined, nil
}

func builtinAwsSignFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return NewCommonError("not enough parameters"), nil
	}

	nv1, ok := args[0].(Map)

	if !ok {
		return NewCommonError("invalid parameter type: (%T)%v", args[0], args[0]), nil
	}

	nv2 := args[1].String()

	postDataT := url.Values{}

	for k, v := range nv1 {
		postDataT.Set(k, v.String())
	}

	rsT := awsapi.Sign(postDataT, nv2)

	return String{Value: rsT}, nil
}

func builtinArrayContainsFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return NewCommonError("not enough parameters"), nil
	}

	nv1, ok := args[0].(Array)

	rightT := args[1]

	if ok {
		for _, v := range nv1 {
			if v.Equal(rightT) {
				return Bool(true), nil
			}
		}

		return Bool(false), nil
	}

	nv2, ok := args[0].(*Any)

	if ok {
		return Bool(tk.ArrayContains(nv2.Value, ConvertFromObject(rightT))), nil
	}

	return NewCommonErrorWithPos(c, "unsupported type: (%T)%v", args[0], args[0]), nil
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

func builtintStrUnquoteFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonError("not enough parameters"), nil
	}

	rs, errT := strconv.Unquote(args[0].String())

	if errT != nil {
		return NewCommonError("%v", errT), nil
	}

	return &String{Value: rs}, nil
}

func builtinSscanfFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return NewCommonError("not enough paramters"), nil
	}

	rsT, errT := fmt.Sscanf(args[0].String(), args[1].String(), AnysToOriginal(args[2:])...)

	if errT != nil {
		return NewCommonErrorWithPos(c, "%v", errT), nil
	}

	return ToIntObject(rsT), nil
}

func builtinBitNotFunc(c Call) (Object, error) {
	// ^x    bitwise complement    is m ^ x  with m = "all bits set to 1" for unsigned x
	// and  m = -1 for signed x
	// so ^x is bitwise not of x
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonError("not enough paramters"), nil
	}

	switch nv1 := args[0].(type) {
	case Bool:
		return Bool(!nv1), nil
	case Byte:
		return Byte(^nv1), nil
	case Int:
		return Int(^nv1), nil
	case Uint:
		return Uint(^nv1), nil
	case Char:
		return Char(^nv1), nil
	case String:
		return ConvertToObject(tk.GetBitNotResult(nv1.Value)), nil
	case *MutableString:
		return ConvertToObject(tk.GetBitNotResult(nv1.Value)), nil
	case Bytes:
		return ConvertToObject(tk.GetBitNotResult([]byte(nv1))), nil
	}

	return Undefined, NewCommonErrorWithPos(c, "unsupported type: (%T)%v", args[0], args[0])
}

func builtinToOrderedMapFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonError("not enough parameters"), nil
	}

	nv, ok := args[0].(Map)

	if !ok {
		return NewCommonErrorWithPos(c, "unsupported type: %T", args[0]), nil
	}

	rs, errT := builtinOrderedMapFunc(Call{Args: []Object{nv}})

	if errT != nil {
		return NewCommonError("%v", errT), nil
	}

	return rs, nil
}

func builtinAdjustFloatFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonError("not enough parameters"), nil
	}

	limitT := 10

	nv1, ok := args[0].(Float)

	if ok {
		if len(args) > 1 {
			limitT = ToIntQuick(args[1])
		}

		return Float(tk.AdjustFloat(float64(nv1), limitT)), nil
	}

	nv2, ok := args[0].(*BigFloat)

	if ok {
		if len(args) > 1 {
			limitT = ToIntQuick(args[1])
		}

		return &BigFloat{Value: tk.AdjustBigFloat(nv2.Value, limitT)}, nil
	}

	return NewCommonErrorWithPos(c, "unsupported type: %T", args[0]), nil
}

func builtinStrSplitFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return NewCommonError("not enough parameters"), nil
	}

	nv1 := args[0].String()

	nv2 := args[1].String()

	limitT := -1

	if len(args) > 2 {
		limitT = ToGoIntWithDefault(args[2], -1)
	}

	return ConvertToObject(strings.SplitN(nv1, nv2, limitT)), nil
}

func builtinStrJoinFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return NewCommonError("not enough parameters"), nil
	}

	nv2 := args[1].String()

	switch nv1 := args[0].(type) {
	case Array:
		var bufT strings.Builder
		for j, jv := range nv1 {
			if j > 0 {
				bufT.WriteString(nv2)
			}

			bufT.WriteString(jv.String())
		}

		return ConvertToObject(bufT.String()), nil
	case *Any:
		switch nv1a := nv1.Value.(type) {
		case []string:
			return ConvertToObject(strings.Join(nv1a, nv2)), nil
		case []interface{}:
			var bufT strings.Builder
			for j, jv := range nv1a {
				if j > 0 {
					bufT.WriteString(nv2)
				}

				bufT.WriteString(fmt.Sprintf("%v", jv))
			}

			return ConvertToObject(bufT.String()), nil
		}
	}

	return NewCommonErrorWithPos(c, "unsupported type: %T", args[0]), nil
}

func builtinMathSqrtFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonError("not enough parameters"), nil
	}

	switch nv := args[0].(type) {
	case Byte:
		return Float(math.Sqrt(float64(nv))), nil
	case Char:
		return Float(math.Sqrt(float64(nv))), nil
	case Int:
		return Float(math.Sqrt(float64(nv))), nil
	case Uint:
		return Float(math.Sqrt(float64(nv))), nil
	case Float:
		return Float(math.Sqrt(float64(nv))), nil
	case *BigInt:
		return &BigFloat{Value: big.NewFloat(0).Sqrt(big.NewFloat(0).SetInt(nv.Value))}, nil
	case *BigFloat:
		return &BigFloat{Value: big.NewFloat(0).Sqrt(nv.Value)}, nil
	}

	return NewCommonErrorWithPos(c, "unsupported type: %T", args[0]), nil
}

func builtinHttpHandlerFunc(c Call) (Object, error) {
	return NewHttpHandler(c)
}

func builtinImageFunc(c Call) (Object, error) {
	return NewImage(c)
}

func builtinReaderFunc(c Call) (Object, error) {
	return NewReader(c)
}

func builtinWriterFunc(c Call) (Object, error) {
	return NewWriter(c)
}

func builtinFileFunc(c Call) (Object, error) {
	return NewFile(c)
}

func BuiltinDelegateFunc(c Call) (Object, error) {
	return NewDelegate(c)
}

func builtinWriteRespHeaderFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return NewCommonError("not enough parameters"), nil
	}

	v, ok := args[0].(*HttpResp)
	if !ok {
		return NewCommonError("invalid parameter 1 type: (%T)%v", args[0], args[0]), nil
	}

	var statusT int = 200

	switch nv := args[1].(type) {
	case Int:
		statusT = int(nv)
	case String:

		v1, ok := namedValueMapG[nv.Value]

		if !ok {
			return NewCommonError("status code not found: (%T)%v", nv.Value, nv.Value), nil
		}

		statusT = tk.ToInt(v1)
	default:
		return NewCommonError("invalid parameter 2 type: (%T)%v", args[1], args[1]), nil
	}

	v.Value.WriteHeader(statusT)

	return Undefined, nil
}

func builtinCloseFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	// r1, ok := args[0].(*Reader)

	// if ok {
	// 	return r1.CallMethod("close")
	// 	// rs := r1.GetMember(strT)

	// 	// if !IsUndefInternal(rs) {
	// 	// 	f1 := rs.(*Function)
	// 	// 	return (*f1).CallEx(Call{Args: append([]Object{o}, argsA[1:]...)}), nil
	// 	// }

	// 	// CallObjectMethodFunc(r1, "close")
	// }

	nv, ok := args[0].(io.Closer)

	if ok {
		errT := nv.Close()

		if errT != nil {
			return NewCommonErrorWithPos(c, "failed to close: %v", errT), nil
		}

		return Undefined, nil
	}

	typeNameT := args[0].TypeName()

	if typeNameT == "reader" {
		r1 := args[0].(*Reader)

		rs := r1.GetMember("close")

		if !IsUndefInternal(rs) {
			f1 := rs.(*Function)
			return (*f1).CallEx(Call{Args: []Object{}})
		}

		return NewCommonErrorWithPos(c, "unsupport method: close"), nil
	} else if typeNameT == "any" {
		vT := args[0].(*Any)
		switch nv := vT.Value.(type) {
		case io.Closer:
			errT := nv.Close()

			if errT != nil {
				return NewCommonErrorWithPos(c, "failed to close: %v", errT), nil
			}

			return Undefined, nil

		default:
			rsT := tk.ReflectCallMethodQuick(vT.Value, "Close")

			if len(rsT) > 0 {
				if tk.IsErrX(rsT[0]) {
					return NewCommonErrorWithPos(c, "failed to close: %v", tk.GetErrStrX(rsT[0])), nil
				}

				return Undefined, nil
			}

			return NewCommonErrorWithPos(c, "unsupported any type for close method: %T", vT.Value), nil

		}
	}

	return NewCommonErrorWithPos(c, "unsupported type: %v", typeNameT), nil
}

// char add end
