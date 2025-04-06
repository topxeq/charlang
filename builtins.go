package charlang

import (
	"bytes"
	"compress/flate"
	"context"
	"database/sql"
	"encoding/base64"
	"encoding/csv"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"image"
	"image/gif"
	"image/jpeg"
	"image/png"
	"io"
	"math"
	"math/big"
	"mime/multipart"
	"net"
	"net/http"
	"net/url"
	"os"
	"os/exec"
	"path/filepath"
	"reflect"
	"sort"
	"strconv"
	"strings"
	"time"
	"unicode/utf8"

	"github.com/domodwyer/mailyak"
	"github.com/fogleman/gg"
	"github.com/golang/freetype/truetype"
	"github.com/guptarohit/asciigraph"
	"github.com/minio/minio-go/v7"
	"github.com/minio/minio-go/v7/pkg/credentials"
	"github.com/topxeq/awsapi"
	"github.com/topxeq/charlang/token"
	"github.com/topxeq/sqltk"
	tk "github.com/topxeq/tkc"
	"github.com/wcharczuk/go-chart/v2/drawing"
	"github.com/xuri/excelize/v2"
	"golang.org/x/image/bmp"

	"github.com/mholt/archiver/v3"

	charts "github.com/vicanso/go-charts/v2"

	// "github.com/gojp/kana"
	"github.com/jtclarkjr/kanjikana"
	
	"github.com/jlaffaye/ftp"
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

	BuiltinReset
	BuiltinStatusToStr
	BuiltinStatusToMap
	BuiltinDocxToStrs
	BuiltinDocxReplacePattern
	BuiltinDocxGetPlaceholders
	BuiltinShowTable
	BuiltinGuiServerCommand
	BuiltinGetMapItem
	BuiltinSetMapItem
	BuiltinSendMail
	BuiltinGetTextSimilarity
	BuiltinLeSshInfo
	BuiltinStack
	BuiltinQueue
	BuiltinMapArray
	BuiltinPlotClearConsole
	BuiltinPlotDataToStr
	BuiltinPlotDataToImage
	BuiltinPlotLoadFont
	BuiltinResizeImage
	BuiltinImageToAscii
	BuiltinLoadImageFromFile
	BuiltinSaveImageToFile
	BuiltinGetImageInfo
	BuiltinStrToRgba
	BuiltinEncodeImage
	BuiltinEncodeBytesInImage
	BuiltinDecodeBytesFromImage
	BuiltinDrawImageOnImage
	BuiltinDrawTextWrappedOnImage
	BuiltinGenQr
	BuiltinSaveImageToBytes
	BuiltinClose
	BuiltinRegSplit
	BuiltinReadCsv
	BuiltinExcelNew
	BuiltinExcelOpen
	BuiltinExcelOpenFile
	BuiltinExcelSaveAs
	BuiltinExcelWriteTo
	BuiltinExcelClose
	BuiltinExcelNewSheet
	BuiltinExcelReadAll
	BuiltinExcelGetSheetCount
	BuiltinExcelGetSheetList
	BuiltinExcelGetSheetName
	BuiltinExcelReadSheet
	BuiltinExcelReadCell
	BuiltinExcelWriteSheet
	BuiltinExcelWriteCell
	BuiltinExcelGetColumnIndexByName
	BuiltinExcelGetColumnNameByIndex
	BuiltinWriteCsv
	BuiltinRemovePath
	BuiltinRemoveDir
	BuiltinGetInput
	BuiltinGetInputf
	BuiltinGetInputPasswordf
	BuiltinGetChar
	BuiltinGetMultiLineInput
	BuiltinSetStdin
	BuiltinSetStdout
	BuiltinSetStderr
	BuiltinGetPipe
	BuiltinRegCount
	BuiltinStrRepeat
	BuiltinRegMatch
	BuiltinRegContains
	BuiltinRegContainsIn
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
	BuiltinLeInfo
	BuiltinS3GetObjectBytes
	BuiltinS3GetObjectText
	BuiltinS3PutObject
	BuiltinS3GetObjectToFile
	BuiltinS3GetObjectReader
	BuiltinS3GetObjectUrl
	BuiltinS3GetObjectTags
	BuiltinS3GetObjectStat
	BuiltinS3StatObject
	BuiltinS3CopyObject
	BuiltinS3MoveObject
	BuiltinS3RemoveObject
	BuiltinS3ListObjects
	BuiltinAwsSign
	BuiltinNow
	BuiltinTimeToTick
	BuiltinFormatTime
	BuiltinTimeAddSecs
	BuiltinTimeAddDate
	BuiltinTimeBefore
	BuiltinGetNowTimeStamp
	BuiltinBase64Encode
	BuiltinBase64Decode
	BuiltinBase64EncodeByRawUrl
	BuiltinBase64DecodeByRawUrl
	BuiltinXmlGetNodeStr
	BuiltinStrXmlEncode
	BuiltinFromXml
	BuiltinFormatXml
	BuiltinMd5
	BuiltinPostRequest
	BuiltinPrepareMultiPartFieldFromBytes
	BuiltinPrepareMultiPartFileFromBytes
	BuiltinHttpRedirect
	BuiltinReader
	BuiltinWriter
	BuiltinFile
	BuiltinBytesBuffer
	BuiltinImage
	BuiltinLoadImageFromBytes
	BuiltinThumbImage
	BuiltinBytesStartsWith
	BuiltinBytesEndsWith
	BuiltinBytesContains
	BuiltinBytesIndex
	BuiltinIsEncrypted
	BuiltinEncryptData
	BuiltinDecryptData
	BuiltinEncryptStream
	BuiltinDecryptStream
	BuiltinSimpleEncode
	BuiltinSimpleDecode
	BuiltinToPinYin
	BuiltinKanjiToKana
	BuiltinKanaToRomaji
	BuiltinKanjiToRomaji
	BuiltinIsHttps
	BuiltinCopyFile
	BuiltinRenameFile
	BuiltinStrJoin
	BuiltinStrCount
	BuiltinStrPad
	BuiltinStrRuneLen
	BuiltinStrIn
	BuiltinStrGetLastComponent
	BuiltinEnsureMakeDirs
	BuiltinExtractFileDir
	BuiltinExtractFileName
	BuiltinCheckToken
	BuiltinGetOtpCode
	BuiltinCheckOtpCode
	BuiltinEncryptText
	BuiltinDecryptText
	BuiltinEncryptTextByTXTE
	BuiltinDecryptTextByTXTE
	BuiltinEncryptDataByTXDEE
	BuiltinDecryptDataByTXDEE
	BuiltinEncryptTextByTXDEE
	BuiltinDecryptTextByTXDEE
	BuiltinAesEncrypt
	BuiltinAesDecrypt
	BuiltinHtmlEncode
	BuiltinHtmlDecode
	BuiltinServeFile
	BuiltinGetFileInfo
	BuiltinGetFileAbs
	BuiltinGetFileRel
	BuiltinGetFileExt
	BuiltinGetMimeType
	BuiltinStartSocksServer
	BuiltinStartSocksClient
	BuiltinStartTransparentProxy
	BuiltinStartTransparentProxyEx
	BuiltinRenderMarkdown
	BuiltinReplaceHtmlByMap
	BuiltinProcessHtmlTemplate
	BuiltinIsDir
	BuiltinStrStartsWith
	BuiltinStrEndsWith
	BuiltinStrSplit
	BuiltinStrSplitN
	BuiltinGenToken
	BuiltinGenJwtToken
	BuiltinParseJwtToken
	BuiltinStrContains
	BuiltinStrContainsAny
	BuiltinStrContainsIn
	BuiltinStrIndex
	BuiltinGetNowStr
	BuiltinGetNowStrCompact
	BuiltinLock
	BuiltinUnlock
	BuiltinTryLock
	BuiltinRLock
	BuiltinRUnlock
	BuiltinTryRLock
	BuiltinToKMG
	BuiltinFloatToStr
	BuiltinStrToUtf8
	BuiltinStrUtf8ToGb
	BuiltinIsUtf8
	BuiltinSimpleStrToMap
	BuiltinSimpleStrToMapReverse
	BuiltinReverseMap
	BuiltinGetFileList
	BuiltinMathAbs
	BuiltinMathSqrt
	BuiltinMathPow
	BuiltinMathExp
	BuiltinMathLog
	BuiltinMathLog10
	BuiltinMathMin
	BuiltinMathMax
	BuiltinMathCeil
	BuiltinMathFloor
	BuiltinMathRound
	BuiltinFlexEval
	BuiltinAdjustFloat
	BuiltinBigInt
	BuiltinBigFloat
	BuiltinToOrderedMap
	BuiltinUnhex
	BuiltinHexToStr
	BuiltinBitNot
	BuiltinToUpper
	BuiltinToLower
	BuiltinSscanf
	BuiltinStrQuote
	BuiltinStrUnquote
	BuiltinStrToInt
	BuiltinStrToTime
	BuiltinDealStr
	BuiltinToInt
	BuiltinToBool
	BuiltinToBoolWithDefault
	BuiltinToFloat
	BuiltinToHex
	BuiltinCompareBytes
	BuiltinLoadBytes
	BuiltinLoadBytesFromFile
	BuiltinSaveBytes
	BuiltinOpenFile
	BuiltinCloseFile
	BuiltinCompressData
	BuiltinCompressStr
	BuiltinUncompressData
	BuiltinUncompressStr
	BuiltinUrlEncode
	BuiltinUrlEncode1
	BuiltinUrlDecode
	BuiltinOrderedMap
	BuiltinExcel
	BuiltinArrayContains
	BuiltinSortArray
	// BuiltinSortByFunc
	BuiltinLimitStr
	BuiltinStrFindDiffPos
	BuiltinStrDiff
	BuiltinStrFindAllSub
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
	BuiltinParseUrl
	BuiltinParseQuery
	BuiltinErrStrf
	BuiltinErrf
	BuiltinErrToEmpty
	BuiltinCharCode
	BuiltinEvalMachine
	BuiltinJsVm
	BuiltinGel
	BuiltinDelegate
	BuiltinGetReqBody
	BuiltinGetReqHeader
	BuiltinGetReqHeaders
	BuiltinWriteRespHeader
	BuiltinSetRespHeader
	BuiltinParseReqForm
	BuiltinParseReqFormEx
	BuiltinWriteResp
	BuiltinMux
	BuiltinMutex
	BuiltinHttpHandler
	BuiltinHttpReq
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
	BuiltinDownloadFile
	BuiltinGetWebBytes
	BuiltinGetWebBytesWithHeaders
	BuiltinGetWebRespBody
	BuiltinRegFindFirstGroups
	BuiltinReadAllStr
	BuiltinReadAllBytes
	BuiltinReadBytes
	BuiltinWriteStr
	BuiltinWriteBytes
	BuiltinWriteBytesAt
	BuiltinIoCopy
	BuiltinStrSplitLines
	BuiltinNew
	BuiltinStringBuilder
	BuiltinStrReplace
	BuiltinGetErrStrX
	BuiltinFtpList
	BuiltinFtpCreateDir
	BuiltinFtpSize
	BuiltinFtpUpload
	BuiltinFtpUploadFromReader
	BuiltinFtpDownloadBytes
	BuiltinSshUpload
	BuiltinSshUploadBytes
	BuiltinSshDownload
	BuiltinSshDownloadBytes
	BuiltinSshRun
	BuiltinArchiveFilesToZip
	BuiltinGetFileListInArchive
	BuiltinGetFileListInArchiveBytes
	BuiltinGetFileListInZip
	BuiltinLoadBytesInArchive
	BuiltinLoadBytesInArchiveBytes
	BuiltinExtractFileInArchive
	BuiltinExtractArchive
	BuiltinIsFileNameUtf8InZipBytes
	BuiltinGetOSName
	BuiltinGetOSArch
	BuiltinGetOSArgs
	BuiltinGetAppDir
	BuiltinGetCurDir
	BuiltinGetHomeDir
	BuiltinGetTempDir
	BuiltinCreateTempDir
	BuiltinCreateTempFile
	BuiltinChangeDir
	BuiltinLookPath
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
	BuiltinRegFindAllIndex
	BuiltinRegFindAllGroups
	BuiltinCheckErrX
	BuiltinCheckEmpty
	BuiltinLoadText
	BuiltinSaveText
	BuiltinAppendText
	BuiltinJoinPath
	BuiltinGetSysInfo
	BuiltinGetEnv
	BuiltinSetEnv
	BuiltinTypeOfAny
	BuiltinToStr
	BuiltinCallNamedFunc
	BuiltinCallInternalFunc
	BuiltinGetProcessVar
	BuiltinSetProcessVar
	BuiltinDeleteProcessVar
	BuiltinGetNamedValue
	BuiltinNewEx
	BuiltinCallMethodEx
	BuiltinToTime
	// BuiltinNewAny
	BuiltinTime
	BuiltinSleep
	BuiltinExit
	BuiltinSystemCmd
	BuiltinSystemCmdDetached
	BuiltinSystemStart
	BuiltinIsErrX
	BuiltinIsErrStr
	BuiltinToJSON
	BuiltinFromJSON
	BuiltinFormatJson
	BuiltinCompactJson
	BuiltinGetJsonNodeStr
	BuiltinGetJsonNodeStrs
	BuiltinStrsToJson
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
	BuiltinPr
	BuiltinPrf
	BuiltinFprf
	BuiltinPlNow
	BuiltinPln
	BuiltinPlv
	BuiltinSpr
	BuiltinSpln
	BuiltinTestByText
	BuiltinTestByStartsWith
	BuiltinTestByEndsWith
	BuiltinTestByContains
	BuiltinTestByRegContains
	BuiltinTestByReg
	BuiltinGetSeq
	BuiltinMagic
	BuiltinGetUuid
	BuiltinPass

	BuiltinDelete
	BuiltinGetArrayItem
	BuiltinCopy
	BuiltinCopyBytes
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
	BuiltinBytesWithSize
	BuiltinBytesWithCap
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

	"testByText":        BuiltinTestByText,        // definition: testByText(strToTest string, strToCompare string, indexInteger int, scriptFileName string)
	"testByStartsWith":  BuiltinTestByStartsWith,  // definition: testByStartsWith(strToTest string, strToCompare string, indexInteger int, scriptFileName string)
	"testByEndsWith":    BuiltinTestByEndsWith,    // definition: testByEndsWith(strToTest string, strToCompare string, indexInteger int, scriptFileName string)
	"testByContains":    BuiltinTestByContains,    // definition: testByContains(strToTest string, strToCompare string, indexInteger int, scriptFileName string)
	"testByReg":         BuiltinTestByReg,         // definition: testByReg(strToTest string, strToCompare string, indexInteger int, scriptFileName string)
	"testByRegContains": BuiltinTestByRegContains, // definition: testByRegContains(strToTest string, strToCompare string, indexInteger int, scriptFileName string)

	"dumpVar":   BuiltinDumpVar,   // for internal debug
	"debugInfo": BuiltinDebugInfo, // for internal debug

	// infrastructure related
	"globals": BuiltinGlobals, // show global variables, usage: pln(globals())

	"len": BuiltinLen, // get length/size of the object, usage: a := len(array1)

	":makeArray": BuiltinMakeArray, // for internal use

	// data type related
	"typeCode":  BuiltinTypeCode,  // get type code of an object
	"typeName":  BuiltinTypeName,  // get type name of an object
	"typeOf":    BuiltinTypeName,  // get type name of an object, the same as typeName
	"typeOfAny": BuiltinTypeOfAny, // get type name of an object with type 'any'

	"any": BuiltinAny, // create a value with type 'any', usage: a1 := any(),  a2 := any(object1)

	"bool":  BuiltinBool,  // create a boolean value(with type 'bool'), usage: b1 := bool(),  b2 := bool(true)
	"byte":  BuiltinByte,  // create a byte value(with type 'byte'), usage: by1 := byte(),  by2 := byte(33)
	"char":  BuiltinChar,  // create a char/rune value(with type 'char'), usage: c1 := char(),  c2 := char(33), c3 := char('&')
	"rune":  BuiltinChar,  // same as char
	"int":   BuiltinInt,   // create an integer/int value(with type 'int'), usage: n1 := int(),  n2 := int(-112)
	"uint":  BuiltinUint,  // create an unsigned integer/int value(with type 'uint'), usage: n1 := uint(),  n2 := uint(68)
	"float": BuiltinFloat, // create a float value(with type 'float'), usage: f1 := float(),  f2 := float(-57.23)

	"bigInt":   BuiltinBigInt,   // create a big integer/int value(with type 'bigInt'), usage: n1 := bigInt(),  n2 := bigInt(-112)
	"bigFloat": BuiltinBigFloat, // create a big float value(with type 'bigFloat'), usage: f1 := bigFloat(),  f2 := bigFloat(-57.23)

	"string":        BuiltinString,        // create a string value(with type 'string'), usage: s1 := string(),  s2 := string("abc")
	"mutableString": BuiltinMutableString, // create a mutable string value(with type 'mutableString'), mutableString could change value at run time, usage: s1 := mutableString(),  s2 := mutableString("abc")
	"bytes":         BuiltinBytes,         // create a bytes value(with type 'bytes'), usage: b1 := bytes([0xef, 0xbc, 0x81]), b2 := bytes("abc123")
	"bytesWithSize":         BuiltinBytesWithSize,         // create a bytes value(with type 'bytes') with specified size, usage: b1 := bytesWithSize(5)
	"bytesWithCap":         BuiltinBytesWithCap,         // create a bytes value(with type 'bytes') with specified capacity(init with zero size), usage: b1 := bytesWithCap(5)
	"chars":         BuiltinChars,         // create a chars/runes value(with type 'chars'), usage: c1 := chars([0xefab, 0xbc01, 0x81cf]) , c2 := ("abc123")

	"bytesBuffer": BuiltinBytesBuffer, // create a bytes buffer, usage: buf1 := bytesBuffer() , buf2 := bytesBuffer(bytes("abc123"))

	"stringBuilder": BuiltinStringBuilder, // create a string builder, usage: sb1 := stringBuilder() , sb2 := stringBuilder("abc123")
	"stringBuffer":  BuiltinStringBuilder, // same as stringBuilder

	"orderedMap": BuiltinOrderedMap, // create an ordered-map, usage: map1 := orderedMap() , map2 := orderedMap({1, 2.5, true, "abc123"})

	"stack": BuiltinStack, // create a stack object(first-in-last-out), usage: st1 := stack() , st2 := stack(1, 2.5, true, "abc123"), the objects passed as parameters for builtin stack function will be pushed in sequence after the creation of the stack
	"queue": BuiltinQueue, // create a queue object(first-in-first-out), usage: que1 := queue() , que2 := queue(10), the integer value passed as parameters for builtin queue function will set the capacity(default infinite) of the queue, the first item will be discarded while a new item is pushing into the queue and the queue is full
	
	"mapArray": BuiltinMapArray, // create an MapArray/SimpleFlexObject which is an array with some items have keys

	"error": BuiltinError, // manually create an error object, usage: err1 := error("failed to do something")

	"time":     BuiltinTime, // create a time object, usage: time1 := time(), get a new time object with the value of current time; time2 := time(123432545), create a time object with the value set to the tick/timestamp of the integer value; time3 := time("2023-01-02 00:18:23")
	"dateTime": BuiltinTime, // same as 'time'

	"excel": BuiltinExcel, // create an Excel object

	"statusResult": BuiltinStatusResult, // create a statusResult object(i.e. {"Status": "success", "Value": "some value"}, or {"Status": "fail", "Value": "failed reason/description"})
	"seq":          BuiltinSeq, // create a sequence object
	"mutex":        BuiltinMutex, // create a mutex object for threads

	"mux":         BuiltinMux, // create a mux object for http routing
	"httpHandler": BuiltinHttpHandler, // create a httpHandler object for handling http routes

	"httpReq": BuiltinHttpReq, // create a http request object

	"reader": BuiltinReader, // create a reader object from bytes, string, file or other objects
	"writer": BuiltinWriter, // create a writer object from bytes, string, file or other objects

	"file": BuiltinFile, // usage: file("c:\\tmp\abc.txt"), file(`/home/user1/a.json`), file("stdin"), file("stdout"), file("stderr"),  another example: fileT := file(`c:\test\a.json`, "-create"), options include: -flag=0, -readOnly, -append, -truncate, -perm=0777 (only octec format is supported)

	"image": BuiltinImage, // new an image object, usage: imageT := image("-width=480", "-height=640", "-color=#FF0000")

	"charCode": BuiltinCharCode,
	"evalMachine": BuiltinEvalMachine,
	"gel":      BuiltinGel,
	"delegate": BuiltinDelegate,

	"jsVm": BuiltinJsVm, // new a JavaScript VM

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

	"reset":  BuiltinReset,

	"getArrayItem": BuiltinGetArrayItem,

	"removeItems": BuiltinRemoveItems, // inclusive

	"arrayContains": BuiltinArrayContains,

	"sortArray": BuiltinSortArray, // usage: sortArray(totalFindsT, "runeStart", "desc")

	"getMapItem": BuiltinGetMapItem,
	"setMapItem": BuiltinSetMapItem,

	"toOrderedMap": BuiltinToOrderedMap,

	// ref/pointer related
	"unref":         BuiltinUnref,
	"setValueByRef": BuiltinSetValueByRef,

	// convert related
	"toStr":             BuiltinToStr,
	"toBool":            BuiltinToBool,
	"toBoolWithDefault": BuiltinToBoolWithDefault,
	"toInt":             BuiltinToInt,
	"toFloat":           BuiltinToFloat,
	"toTime":            BuiltinToTime,
	"toHex":             BuiltinToHex,
	"strToHex":             BuiltinToHex,
	"hexEncode":         BuiltinToHex,
	"unhex":             BuiltinUnhex,
	"hexDecode":         BuiltinUnhex,
	"hexToStr":          BuiltinHexToStr,
	"toKMG":             BuiltinToKMG,

	"floatToStr": BuiltinFloatToStr,
	
	"strToUtf8": BuiltinStrToUtf8,
	"strUtf8ToGb": BuiltinStrUtf8ToGb,
	"isUtf8": BuiltinIsUtf8,
	
	"simpleStrToMap": BuiltinSimpleStrToMap,
	"simpleStrToMapReverse": BuiltinSimpleStrToMapReverse,

	"reverseMap": BuiltinReverseMap,

	// string related
	"trim":          BuiltinTrim,         // trim spaces of the string, also convert undefined value to empty string
	"nilToEmpty":    BuiltinTrim,         // convert undefined value to empty string
	"strTrim":       BuiltinTrim,         // same as trim
	"strTrimStart":  BuiltinStrTrimStart, // usage: strTrimStart(s, prefix), returns string without the provided leading prefix sub-string. If the string doesn't start with prefix, s is returned unchanged.
	"strTrimEnd":    BuiltinStrTrimEnd,   // usage: strTrimEnd(s, suffix), returns string without the provided trailing suffix sub-string. If the string doesn't end with suffix, s is returned unchanged.
	"strTrimLeft":   BuiltinStrTrimLeft,  // usage: strTrimLeft(s, cutset string), strTrimLeft returns a slice of the string s with all leading Unicode code points contained in cutset removed.
	"strTrimRight":  BuiltinStrTrimRight, // usage: strTrimRight(s, cutset string), strTrimRight returns a slice of the string s, with all trailing Unicode code points contained in cutset removed.
	"toUpper":       BuiltinToUpper,
	"strToUpper":    BuiltinToUpper,
	"toLower":       BuiltinToLower,
	"strToLower":    BuiltinToLower,
	"strContains":   BuiltinStrContains,
	"strContainsAny":   BuiltinStrContainsAny, // as strings.ContainsAny, reports whether any Unicode code points in chars are within s. 
	"strContainsIn": BuiltinStrContainsIn,
	"strIndex":      BuiltinStrIndex,
	"strStartsWith": BuiltinStrStartsWith,
	"strEndsWith":   BuiltinStrEndsWith,
	"strReplace":    BuiltinStrReplace,
	"strSplit":      BuiltinStrSplit,
	"strSplitN":      BuiltinStrSplitN,
	"strSplitLines": BuiltinStrSplitLines,
	"strJoin":       BuiltinStrJoin,
	"strRepeat":     BuiltinStrRepeat,
	"strCount":      BuiltinStrCount,
	"strPad":        BuiltinStrPad, // string padding operations such as zero padding, for example, result := strPad(strT, 5, "-fill=0", "-right=true"), where the first parameter is the string to be padded, and the second parameter is the number of characters to be padded. The default padding string is fill as string 0, and right (indicating whether to fill on the right side) is false (which can also be written directly as -right). Therefore, the above example is equivalent to result := strPad(strT, 5). If the fill string contains more than one character, the final number of padding will not exceed the value specified by the second parameter, but it may be less

	"strRuneLen": BuiltinStrRuneLen, // get the length of string by rune(how many rune characters in the string)

	"strIn": BuiltinStrIn,

	"strGetLastComponent": BuiltinStrGetLastComponent, // strGetLastComponent("/root/abc", "/"), default separator is \ in Windows or / in Linux/MacOS

	"strFindDiffPos": BuiltinStrFindDiffPos, // return -1 if 2 strings are identical
	
	"strDiff": BuiltinStrDiff,

	"strFindAllSub": BuiltinStrFindAllSub,

	"limitStr": BuiltinLimitStr,

	"strQuote":   BuiltinStrQuote,
	"strUnquote": BuiltinStrUnquote,

	"strToInt":  BuiltinStrToInt,  // convert string to int, return error if failed
	"strToTime": BuiltinStrToTime, // convert string to time by format, usage: strToTime(strA, "20060102150405"), default "2006-01-02 15:04:05"

	"dealStr": BuiltinDealStr, // deal with hex-encoded, encrypted or other special-treated string

	"getTextSimilarity": BuiltinGetTextSimilarity, // calculate the cosine similarity of two strings

	// regex related
	"regMatch":      BuiltinRegMatch,      // determine whether a string fully conforms to a regular expression, usage example: result := regMatch("abcab", `a.*b`)
	"regContains":   BuiltinRegContains,   // determine whether the string contains substrings that conform to the regular expression
	"regContainsIn": BuiltinRegContainsIn, // determine whether the string contains substrings that conform to any regular expression

	"regFindFirst":       BuiltinRegFindFirst,       // get the first match of a regular expression, usage example: result := regFindFirst(str1, regex1, group)
	"regFindFirstGroups": BuiltinRegFindFirstGroups, // obtain the first match of a regular expression and return a list of all matching groups, where the first item is the complete matching result and the second item is the first matching group..., usage example: result := regFindFirstGroups(str1, regex1)
	"regFindAll":         BuiltinRegFindAll,         // get all matches of a regular expression, and the default matching group number is 0, which means a complete match. Usage example: result := regFindAll(str1, regex1, group)
	"regFindAllIndex":    BuiltinRegFindAllIndex,    // get all matches' indexes of a regular expression, and the default matching group number is 0, which means a complete match. Usage example: result := regFindAll(str1, regex1, group)
	"regFindAllGroups":   BuiltinRegFindAllGroups,   // get all matches of a regular expression, the result is a two-dimensional string array containing various groups, where the 0th group is a complete match, and the 1st group starts with the matching groups in parentheses. usage example: result := regFindAllGroups(str1, regex1)
	"regQuote":           BuiltinRegQuote,           // escaping and replacing special characters related to regular expressions in a regular string for use in regular expressions
	"regReplace":         BuiltinRegReplace,         // replace in a string based on regular expressions, function definition: regReplace(strA, patternA, replaceA string) string, example: regReplace("abcdefgabcdfg", "(b. *) f (ga. *?) g", "$ {1}_ ${2} "), the result is abcd_gabcdf
	"regCount":           BuiltinRegCount,           // determine if a certain string contains several substrings that match the regular expression, usage: result := regCount(str1, regex1)
	"regSplit":           BuiltinRegSplit,           // split strings using regular expressions, usage: listT := regSplit(str1, regex1)

	// math related
	"adjustFloat": BuiltinAdjustFloat, // remove the number of digits from floating-point calculation error values such as 32.0000000004. The result parameter cannot be omitted. Usage: adjustFloat(0.65-0.6, 10). The second parameter is the number of decimal places to which it is organized, which can be omitted. The default is 10.

	"mathAbs":   BuiltinMathAbs,
	"abs":       BuiltinMathAbs,
	"mathSqrt":  BuiltinMathSqrt,
	"sqrt":      BuiltinMathSqrt,
	"mathPow":   BuiltinMathPow,
	"pow":       BuiltinMathPow,
	"mathExp":   BuiltinMathExp,
	"exp":       BuiltinMathExp,
	"mathLog":   BuiltinMathLog,
	"log":       BuiltinMathLog,
	"mathLog10": BuiltinMathLog10,
	"log10":     BuiltinMathLog10,

	"min": BuiltinMathMin, // returns the largest of several values(integer of float).
	"max": BuiltinMathMax, // returns the smallest of several values(integer of float).

	"ceil":  BuiltinMathCeil,  // returns the least integer value greater than or equal to x.
	"floor": BuiltinMathFloor, // returns the greatest integer value less than or equal to x.
	"round": BuiltinMathRound, // returns the nearest integer, rounding half away from zero.

	"flexEval": BuiltinFlexEval,

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

	"formatTime": BuiltinFormatTime,

	"timeAddSecs": BuiltinTimeAddSecs, // add seconds(could be float) to time and return the result, usage: time2 := timeAddSecs(time1, 1.2)
	"timeAddDate": BuiltinTimeAddDate, // add years, months, days to time and return the result, usage: time2 := timeAddDate(time1, 0, -1, 0), will add -1 month to time1
	
	"timeBefore": BuiltinTimeBefore, // usage: b1 := timeBefore(time1, time2)

	// binary/bytes related
	"bytesStartsWith": BuiltinBytesStartsWith,
	"bytesEndsWith":   BuiltinBytesEndsWith,
	"bytesContains":   BuiltinBytesContains,
	"bytesIndex":   BuiltinBytesIndex,

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
	"isErrStr":      BuiltinIsErrStr,
	"getErrStrX": BuiltinGetErrStrX,
	"getErrStr":  BuiltinGetErrStrX,

	"checkErrX": BuiltinCheckErrX, // check if the object is error or error string, if is, exit the program with output, usage: checkErrX(result, "-format=Failed to process: %v\n"), the default format is "Error: %v\n"
	"checkErr":  BuiltinCheckErrX, // the same as checkErrX, check if the object is error or error string, if is, exit the program with output, usage: checkErr(result, "-format=Failed to process: %v\n"), the default format is "Error: %v\n"

	"checkEmpty": BuiltinCheckEmpty, // similar to checkErr, check if the object is undefined or empty string(for other objects, will use its string representation), if is, exit the program with output, usage: checkEmpty(result, "-format=Failed to process: %v\n"), the default format is "Empty: %v\n"

	"errStrf": BuiltinErrStrf,
	"errf":    BuiltinErrf,

	"errToEmpty":    BuiltinErrToEmpty,
	
	// output/print related
	"pr":     BuiltinPr,
	"prf":    BuiltinPrf,
	"fprf":   BuiltinFprf,
	"pl":     BuiltinPl,
	"plNow":  BuiltinPlNow, // the same as pl, with additional current time before the output
	"pln":    BuiltinPln,   // the same as 'println' in other languages. pln formats using the default formats for its arguments and writes to standard output. Usage: pln("the name is", str1)
	"plv":    BuiltinPlv,
	"plt":    BuiltinPlt,
	"plo":    BuiltinPlo,
	"plErr":  BuiltinPlErr,
	"fatalf": BuiltinFatalf,
	"spr":    BuiltinSpr,
	"spln":    BuiltinSpln,

	// scan related
	"sscanf": BuiltinSscanf,

	// process related
	"getProcessVar":    BuiltinGetProcessVar,    // set a process wide global variable value, usage: v1 := getProcessVar("key1", "defaultValue")
	"setProcessVar":    BuiltinSetProcessVar,    // get a process wide global variable value
	"deleteProcessVar": BuiltinDeleteProcessVar, // delete a process wide global variable value

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
	"readAllStr":   BuiltinReadAllStr,
	"readAllBytes": BuiltinReadAllBytes,
	"readBytes": BuiltinReadBytes,
	"writeStr":     BuiltinWriteStr,
	"writeBytes":   BuiltinWriteBytes,
	"writeBytesAt":   BuiltinWriteBytesAt, // write bytes at the specified index in Bytes, return the result buffer(maybe the same), if not enough size, enlarge the buffer and reture the new buffer(i.e. the Bytes object), usage: buf1 = writeBytesAt(bytes([1, 2, 3]), 1, bytes([4, 5, 6, 7]))
	"copyBytes":   BuiltinCopyBytes,

	"ioCopy": BuiltinIoCopy,

	// encode/decode related
	"md5": BuiltinMd5,

	"urlEncode":    BuiltinUrlEncode,
	"urlEncode1":   BuiltinUrlEncode1,
	"urlDecode":    BuiltinUrlDecode,
	"htmlEncode":   BuiltinHtmlEncode,
	"htmlDecode":   BuiltinHtmlDecode,
	"simpleEncode": BuiltinSimpleEncode,
	"simpleDecode": BuiltinSimpleDecode,

	"base64Encode": BuiltinBase64Encode,
	"base64Decode": BuiltinBase64Decode,

	"base64EncodeByRawUrl": BuiltinBase64EncodeByRawUrl,
	"base64DecodeByRawUrl": BuiltinBase64DecodeByRawUrl,

	"toJSON":   BuiltinToJSON,
	"toJson":   BuiltinToJSON,
	"fromJSON": BuiltinFromJSON,
	"fromJson": BuiltinFromJSON,
	
	"formatJson": BuiltinFormatJson,
	"compactJson": BuiltinCompactJson,
	
	"getJsonNodeStr": BuiltinGetJsonNodeStr, // getJsonNodeStr(jsonA, pathA), pathA refer to github.com/tidwall/gjson
	"getJsonNodeStrs": BuiltinGetJsonNodeStrs, // getJsonNodeStrs(jsonA, pathA), pathA refer to github.com/tidwall/gjson

	"strsToJson": BuiltinStrsToJson,

	// XML related
	"xmlEncodeStr":  BuiltinStrXmlEncode,
	"xmlGetNodeStr": BuiltinXmlGetNodeStr,
	"fromXml": BuiltinFromXml,
	"formatXml": BuiltinFormatXml,

	// command-line related
	"ifSwitchExists": BuiltinIfSwitchExists,
	"switchExists":   BuiltinIfSwitchExists,
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
	"systemCmdDetached": BuiltinSystemCmdDetached,
	"systemStart": BuiltinSystemStart,

	"getSysInfo": BuiltinGetSysInfo, // getSysInfo("-disk=/", "-cpuTime=0.5"), by default, disk info will not be retrieved

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
	"getUserDir": BuiltinGetHomeDir,
	"getTempDir": BuiltinGetTempDir,

	"createTempDir": BuiltinCreateTempDir, // usage: tempDirNameT := createTempDir(dirA, patternA)  , createTempDir creates a new temporary directory in the directory dirA and returns the pathname of the new directory. The new directory's name is generated by adding a random string to the end of patternA. If patternA includes a "*", the random string replaces the last "*" instead. The directory is created with mode 0o700 (before umask). If dirA is the empty string, createTempDir uses the default directory for temporary files. It is the caller's responsibility to remove the directory when it is no longer needed. 
	"createTempFile": BuiltinCreateTempFile, // usage: tempFileNameT := createTempFile(dirA, patternA)  , createTempFile creates a new temporary file in the directory dir, and returns the resulting file name. The filename is generated by taking patternA and adding a random string to the end. If patternA includes a "*", the random string replaces the last "*". The file is created with mode 0o600 (before umask). If dirA is the empty string, createTempFile uses the default directory for temporary files. It is the caller's responsibility to remove the file when it is no longer needed. 

	"changeDir": BuiltinChangeDir,
	"chdir":      BuiltinChangeDir,

	"lookPath": BuiltinLookPath,

	"getInput":  BuiltinGetInput,
	"getInputf": BuiltinGetInputf,
	"getInputPasswordf": BuiltinGetInputPasswordf,
	"getChar":   BuiltinGetChar,
	"getMultiLineInput":   BuiltinGetMultiLineInput,

	"setStdin":  BuiltinSetStdin,
	"setStdout": BuiltinSetStdout,
	"setStderr": BuiltinSetStderr,

	"getPipe": BuiltinGetPipe,

	// dir/path related
	"joinPath": BuiltinJoinPath, // join multiple file paths into one, equivalent to path/filepath.Join in the Go language standard library

	"isDir": BuiltinIsDir,

	"ensureMakeDirs": BuiltinEnsureMakeDirs,

	"getFileList": BuiltinGetFileList,
	"genFileList": BuiltinGetFileList,

	// file related
	"fileExists":   BuiltinFileExists,
	"ifFileExists": BuiltinFileExists,

	"getFileInfo": BuiltinGetFileInfo,
	"getFileAbs":  BuiltinGetFileAbs,
	"getFileExt":  BuiltinGetFileExt,
	"getFileRel":  BuiltinGetFileRel,

	"extractFileDir":  BuiltinExtractFileDir,
	"extractFileName": BuiltinExtractFileName,
	"getBaseFileName": BuiltinExtractFileName,
	"getFileBase":     BuiltinExtractFileName,

	"copyFile":   BuiltinCopyFile,
	"renameFile": BuiltinRenameFile,
	"moveFile":   BuiltinRenameFile,
	"removeFile": BuiltinRemoveFile, // remove file only
	"removeDir":  BuiltinRemoveDir, // remove directory only
	"removePath": BuiltinRemovePath, // remove file or directory, use "-recursive" to remove with files and sub-directories

	"loadText":   BuiltinLoadText,
	"saveText":   BuiltinSaveText,
	"appendText": BuiltinAppendText,

	"loadBytes":         BuiltinLoadBytesFromFile, // load bytes from file, usage: loadBytes("file.bin"), return error or Bytes([]byte), loadBytes("a.txt", 5) to read only 5 bytes, can accept a File object
	"loadBytesFromFile": BuiltinLoadBytesFromFile, // the same as loadBytes
	"saveBytes":         BuiltinSaveBytes,

	"openFile":  BuiltinOpenFile, // usage: fileT := openFile(`c:\test\a.json`, "-create"), options include: -flag=0, -readOnly, -append, -truncate, -perm=0777 (only octec format is supported)
	"closeFile": BuiltinCloseFile,

	// compress/zip related
	"compressData":   BuiltinCompressData,
	"compressStr":    BuiltinCompressStr,
	"uncompressData": BuiltinUncompressData,
	"uncompressStr":  BuiltinUncompressStr,

	"archiveFilesToZip": BuiltinArchiveFilesToZip, // Add multiple files to a newly created zip file. The first parameter is the zip file name, with a suffix of '.zip'. Optional parameters include '-overwrite' (whether to overwrite existing files) and '-makeDirs' (whether to create a new directory as needed). Other parameters are treated as files or directories to be added, and the directory will be recursively added to the zip file. If the parameter is a list, it will be treated as a list of file names, and all files in it will be added
	"getFileListInArchive": BuiltinGetFileListInArchive,
	"getFileListInArchiveBytes": BuiltinGetFileListInArchiveBytes,
	"getFileListInZip": BuiltinGetFileListInZip,

	"loadBytesInArchive": BuiltinLoadBytesInArchive, // loadBytesInArchive("example.zip", "subdir1/a.txt", "-limit=3")
	"loadBytesInArchiveBytes": BuiltinLoadBytesInArchiveBytes, // loadBytesInArchiveBytes(bytesT)
	"extractFileInArchive": BuiltinExtractFileInArchive, // extractFileInArchive("example.zip", "subdir1/a.txt", "toDir/a.txt")
	"extractArchive": BuiltinExtractArchive, // extractArchive("example.zip", "toDir", "-noFileDir", "-force")
	"isFileNameUtf8InZipBytes": BuiltinIsFileNameUtf8InZipBytes, // return boolean value or error

	// network/web related
	"getWeb":                 BuiltinGetWeb,
	"downloadFile":                 BuiltinDownloadFile,
	"getWebBytes":            BuiltinGetWebBytes,
	"getWebBytesWithHeaders": BuiltinGetWebBytesWithHeaders,
	"getWebRespBody":         BuiltinGetWebRespBody, // rs := getWebRespBody(urlT, "-withLen"); if isErr(rs) {...}; readerT := rs[0]; lenT := rs[1]; rs = s3PutObject(readerT, "tmpbucket", keyT, "-endPoint=xxxxx", "-accessKey=xxxxx", "-secretAccessKey=xxxxx", "-ssl", "-force", "-size="+toStr(lenT), "-contentType=application/octet-stream", "-timeout=600");  close(readerT)

	"postRequest": BuiltinPostRequest,
	"prepareMultiPartFieldFromBytes": BuiltinPrepareMultiPartFieldFromBytes, // prepareMultiPartFieldFromBytes("file", bytes[0x65, 0x66, 0x67]), return ["for content-type", bytes generated]
	"prepareMultiPartFileFromBytes": BuiltinPrepareMultiPartFileFromBytes, // prepareMultiPartFileFromBytes("file", "a.txt", bytes[0x65, 0x66, 0x67]), return ["for content-type", bytes generated], then rs := getWeb(spr("https://example.com/ms/mgmt/uploadAttach?uid=%v&valueonly=true&witherror=true&billNo=%v", uidT, newBillNoT), formObjT[1], `-headers={"Content-Type":"`+formObjT[0]+`"}`, "-timeout=30")

	"urlExists": BuiltinUrlExists,

	"parseUrl":   BuiltinParseUrl,   // parse URL and return a map
	"parseQuery": BuiltinParseQuery, // parse URL query string(such as 'x=1&y=2&y=3') and return a map({"x": "1", "y": ["2", "3"]})

	"isHttps": BuiltinIsHttps,

	"httpRedirect": BuiltinHttpRedirect,

	// server/service related
	"getReqHeader":    BuiltinGetReqHeader,
	"getReqHeaders":    BuiltinGetReqHeaders,
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

	"startSocksServer":        BuiltinStartSocksServer,
	"startSocksClient":        BuiltinStartSocksClient,
	"startTransparentProxy":   BuiltinStartTransparentProxy,
	"startTransparentProxyEx": BuiltinStartTransparentProxyEx,

	// security related
	"genToken":   BuiltinGenToken,
	"checkToken": BuiltinCheckToken,

	"genJwtToken":   BuiltinGenJwtToken, // genJwtToken({"sub":"user1","exp":1742450426,"userId":116}, "my_secret", "-noType", "-base64Secret"), genJwtToken({"sub":"user1","exp":1742450426,"userId":116}, base64DecodeByRawUrl("my_secret"), "-noType")
	"parseJwtToken":   BuiltinParseJwtToken, 

	"getOtpCode":   BuiltinGetOtpCode,
	"genOtpCode":   BuiltinGetOtpCode,
	"checkOtpCode": BuiltinCheckOtpCode,

	"isEncrypted": BuiltinIsEncrypted,

	"encryptText": BuiltinEncryptText,
	"encryptStr":  BuiltinEncryptText,
	"decryptText": BuiltinDecryptText,
	"decryptStr":  BuiltinDecryptText,

	"encryptTextByTXTE": BuiltinEncryptTextByTXTE,
	"decryptTextByTXTE": BuiltinDecryptTextByTXTE,

	"encryptDataByTXDEE": BuiltinEncryptDataByTXDEE,
	"decryptDataByTXDEE": BuiltinDecryptDataByTXDEE,

	"encryptTextByTXDEE": BuiltinEncryptTextByTXDEE,
	"decryptTextByTXDEE": BuiltinDecryptTextByTXDEE,

	"encryptData":  BuiltinEncryptData,
	"encryptBytes": BuiltinEncryptData,
	"decryptData":  BuiltinDecryptData,
	"decryptBytes": BuiltinDecryptData,

	"encryptStream": BuiltinEncryptStream,
	"decryptStream": BuiltinDecryptStream,

	"aesEncrypt": BuiltinAesEncrypt, // AES encrypt string or bytes, "-cbc" to use cbc
	"aesDecrypt": BuiltinAesDecrypt, // AES decrypt string or bytes, "-cbc" to use cbc

	// image related
	"loadImageFromBytes": BuiltinLoadImageFromBytes, // usage: imageT := loadImageFromBytes(bytesT, "-type=png")
	"saveImageToBytes":   BuiltinSaveImageToBytes,   // usage: bytesT := saveImageToBytes(imageT) or bytesT := saveImageToBytes(imageT, ".png") to save image with specified format, .jpg, .png, .gif, .bmp is supported
	"imageToBytes":   BuiltinSaveImageToBytes,

	"loadImageFromFile": BuiltinLoadImageFromFile, // usage: imageT := loadImageFromFile(`c:\test\abc.png`) or image2T := loadImageFromFile(`c:\test\abc.jpg`, "-type=jpg")
	"saveImageToFile":   BuiltinSaveImageToFile,   // usage: errT := saveImageToFile(imageT, `c:\test\newabc.png`) or errT := saveImageToFile(imageT, `c:\test\newabc.png`, ".png") to save image with specified format, .jpg, .png, .gif, .bmp is supported

	"getImageInfo": BuiltinGetImageInfo,

	"strToRgba": BuiltinStrToRgba,

	"encodeImage":          BuiltinEncodeImage,
	"encodeBytesInImage":   BuiltinEncodeBytesInImage,
	"decodeBytesFromImage": BuiltinDecodeBytesFromImage,

	"drawImageOnImage":       BuiltinDrawImageOnImage,
	"drawTextWrappedOnImage": BuiltinDrawTextWrappedOnImage,

	"genQr": BuiltinGenQr,

	"imageToAscii": BuiltinImageToAscii, // convert an image object to colorful ASCII graph(array of string), usage: asciiT := imageToAscii(imageT, "-width=60", "-height=80"), set one of width or height will keep aspect ratio

	"resizeImage": BuiltinResizeImage, // get a new image by resizing an image object, usage: newImageT := resizeImage(imageT, "-width=60", "-height=80"), set one of width or height will keep aspect ratio

	// plot related
	"plotClearConsole": BuiltinPlotClearConsole, // clear console for plot
	"plotDataToStr":    BuiltinPlotDataToStr,    // this function is based on github.com/guptarohit/asciigraph(thanks), for usage, see example script file asciiPlot.char
	"plotDataToImage":  BuiltinPlotDataToImage,  // this function is based on github.com/vicanso/go-charts(thanks), for usage, see example script file pngPlot.char and svgPlot.char

	"plotLoadFont": BuiltinPlotLoadFont, // load a font file in ttf format for plot, usage: plotLoadFont("c:\windows\tahoma.ttf", "tahoma", true), the secode parameter gives the font name(default is the file name), pass true for the third parameter to set the font as default font used in plot(default is false)

	// ssh/ftp related
	"ftpList":      BuiltinFtpList,
	"ftpCreateDir":      BuiltinFtpCreateDir,
	"ftpSize":      BuiltinFtpSize, // could used to determine if file exists, by check the result if is error and contains certain text
	"ftpUpload":      BuiltinFtpUpload,
	"ftpUploadFromReader":      BuiltinFtpUploadFromReader,
	"ftpDownloadBytes":      BuiltinFtpDownloadBytes,

	"sshUpload":      BuiltinSshUpload,
	"sshUploadBytes": BuiltinSshUploadBytes,
	"sshDownload":    BuiltinSshDownload,
	"sshDownloadBytes":    BuiltinSshDownloadBytes,
	"sshRun":         BuiltinSshRun,

	// eTable related
	"readCsv":  BuiltinReadCsv,
	"csvRead":  BuiltinReadCsv,
	"writeCsv": BuiltinWriteCsv,
	"csvWrite": BuiltinWriteCsv,

	"excelNew":           BuiltinExcelNew,
	"excelOpen":          BuiltinExcelOpen,
	"excelOpenFile":      BuiltinExcelOpenFile,
	"excelSaveAs":        BuiltinExcelSaveAs,
	"excelWriteTo":       BuiltinExcelWriteTo,
	"excelClose":         BuiltinExcelClose,
	"excelNewSheet":      BuiltinExcelNewSheet,
	"excelReadAll":     BuiltinExcelReadAll,
	"excelGetSheetCount": BuiltinExcelGetSheetCount,
	"excelGetSheetList":  BuiltinExcelGetSheetList,
	"excelGetSheetName":  BuiltinExcelGetSheetName,
	"excelReadSheet":     BuiltinExcelReadSheet,
	"excelWriteSheet":    BuiltinExcelWriteSheet,
	"excelReadCell":      BuiltinExcelReadCell,
	"excelGetCellValue":      BuiltinExcelReadCell,
	"excelWriteCell":     BuiltinExcelWriteCell,
	"excelSetCellValue":     BuiltinExcelWriteCell,
	"excelGetColumnIndexByName":  BuiltinExcelGetColumnIndexByName,
	"excelGetColumnNameByIndex":  BuiltinExcelGetColumnNameByIndex,

	// database related
	"formatSQLValue": BuiltinFormatSQLValue,
	"formatSqlValue": BuiltinFormatSQLValue,

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
	"toPinYin": BuiltinToPinYin,

	"kanjiToKana":   BuiltinKanjiToKana,
	"kanaToRomaji":  BuiltinKanaToRomaji,
	"kanjiToRomaji": BuiltinKanjiToRomaji,

	// line editor related
	"leClear":          BuiltinLeClear,
	"leInfo":          BuiltinLeInfo, // show lines count, chars count, ...
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
	"leLoadFromSsh":    BuiltinLeLoadFromSsh, // usage: rs := leLoadFromSsh("-host=a.b.com", "-port=22", "-user=user1", "-password=pass1", "-path=/home/user1/abc.txt")
	"leSaveToSsh":      BuiltinLeSaveToSsh,   // usage: rs := leSaveToSsh("-host=a.b.com", "-port=22", "-user=user1", "-password=pass1", "-path="+pathT)

	"leSshInfo":       BuiltinLeSshInfo,
	"leViewAll":       BuiltinLeViewAll,
	"leViewLine":      BuiltinLeViewLine,
	"leViewLines":     BuiltinLeViewLines,
	"leSort":          BuiltinLeSort,
	"leConvertToUtf8": BuiltinLeConvertToUtf8,
	"leGetLine":       BuiltinLeGetLine,
	"leSetLine":       BuiltinLeSetLine,
	"leSetLines":      BuiltinLeSetLines,
	"leInsertLine":    BuiltinLeInsertLine,
	"leAppendLine":    BuiltinLeAppendLine,
	"leRemoveLine":    BuiltinLeRemoveLine,
	"leRemoveLines":   BuiltinLeRemoveLines,
	"leFindLines":     BuiltinLeFindLines,
	"leFind":          BuiltinLeFind,
	"leFindAll":       BuiltinLeFindAll,
	"leReplace":       BuiltinLeReplace,
	"lePrint":         BuiltinLePrint,
	"leGetList":       BuiltinLeGetList,

	// mail related
	"sendMail": BuiltinSendMail,

	// GUI server related
	"guiServerCommand": BuiltinGuiServerCommand, // send command to GUI server, usage: rs := cgsCmd("pln", "-url="+trim(guiServerUrlG), "value", "18")
	"cgsCmd": BuiltinGuiServerCommand,

	// s3 related
	"s3GetObjectBytes":  BuiltinS3GetObjectBytes,
	"s3GetObjectText":   BuiltinS3GetObjectText,
	"s3PutObject":       BuiltinS3PutObject,
	"s3GetObjectToFile":       BuiltinS3GetObjectToFile,
	"s3GetObjectReader": BuiltinS3GetObjectReader,
	"s3GetObjectUrl":    BuiltinS3GetObjectUrl,
	"s3GetObjectTags":   BuiltinS3GetObjectTags,
	"s3GetObjectStat":   BuiltinS3GetObjectStat,
	"s3StatObject":      BuiltinS3StatObject,
	"s3CopyObject":      BuiltinS3CopyObject,
	"s3MoveObject":      BuiltinS3MoveObject,
	"s3RemoveObject":    BuiltinS3RemoveObject,
	"s3ListObjects":     BuiltinS3ListObjects,

	// 3rd party related
	"awsSign": BuiltinAwsSign,

	// misc related
	"magic":  BuiltinMagic,

	"getSeq":  BuiltinGetSeq,
	"getUuid": BuiltinGetUuid,
	"genUuid": BuiltinGetUuid,

	"renderMarkdown": BuiltinRenderMarkdown,

	"replaceHtmlByMap":    BuiltinReplaceHtmlByMap,
	"processHtmlTemplate": BuiltinProcessHtmlTemplate,
	
	"statusToStr": BuiltinStatusToStr,
	"statusToMap": BuiltinStatusToMap,

	"docxToStrs": BuiltinDocxToStrs,

	"docxReplacePattern": BuiltinDocxReplacePattern,
	"docxGetPlaceholders": BuiltinDocxGetPlaceholders,

	"showTable": BuiltinShowTable,

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
	BuiltinBytesWithSize: &BuiltinFunction{
		Name:    "bytesWithSize",
		Value:   CallExAdapter(builtinBytesWithSizeFunc),
		ValueEx: builtinBytesWithSizeFunc,
	},
	BuiltinBytesWithCap: &BuiltinFunction{
		Name:    "bytesWithCap",
		Value:   CallExAdapter(builtinBytesWithCapFunc),
		ValueEx: builtinBytesWithCapFunc,
	},
	BuiltinChars: &BuiltinFunction{
		Name:    "chars",
		Value:   funcPOROe(builtinCharsFunc),
		ValueEx: funcPOROeEx(builtinCharsFunc),
	},
	BuiltinBytesBuffer: &BuiltinFunction{
		Name:    "bytesBuffer",
		Value:   CallExAdapter(NewBytesBuffer),
		ValueEx: NewBytesBuffer,
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

	BuiltinStack: &BuiltinFunction{
		Name:    "stack",
		Value:   CallExAdapter(NewStack),
		ValueEx: NewStack,
	},

	BuiltinQueue: &BuiltinFunction{
		Name:    "queue",
		Value:   CallExAdapter(NewQueue),
		ValueEx: NewQueue,
	},

	BuiltinMapArray: &BuiltinFunction{
		Name:    "mapArray",
		Value:   CallExAdapter(NewMapArray),
		ValueEx: NewMapArray,
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

	BuiltinExcel: &BuiltinFunction{
		Name:    "excel",
		Value:   CallExAdapter(NewExcel),
		ValueEx: builtinExcelFunc,
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
	BuiltinHttpReq: &BuiltinFunction{
		Name:    "httpReq",
		Value:   CallExAdapter(builtinHttpReqFunc),
		ValueEx: builtinHttpReqFunc,
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
	BuiltinEvalMachine: &BuiltinFunction{
		Name:    "evalMachine",
		Value:   CallExAdapter(NewEvalMachine),
		ValueEx: NewEvalMachine,
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
	BuiltinJsVm: &BuiltinFunction{
		Name:    "jsVm",
		Value:   CallExAdapter(NewJsVm),
		ValueEx: NewJsVm,
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
	BuiltinReset: &BuiltinFunction{
		Name:    "reset",
		Value:   CallExAdapter(builtinResetFunc),
		ValueEx: builtinResetFunc,
	},
	BuiltinGetArrayItem: &BuiltinFunction{
		Name:    "getArrayItem",
		Value:   CallExAdapter(builtinGetArrayItemFunc),
		ValueEx: builtinGetArrayItemFunc,
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
	BuiltinSortArray: &BuiltinFunction{
		Name:    "sortArray",
		Value:   CallExAdapter(builtinSortArrayFunc),
		ValueEx: builtinSortArrayFunc,
	},
	BuiltinGetMapItem: &BuiltinFunction{
		Name:    "getMapItem",
		Value:   CallExAdapter(builtinGetMapItemFunc),
		ValueEx: builtinGetMapItemFunc,
	},
	BuiltinSetMapItem: &BuiltinFunction{
		Name:    "setMapItem",
		Value:   CallExAdapter(builtinSetMapItemFunc),
		ValueEx: builtinSetMapItemFunc,
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
	BuiltinToBool: &BuiltinFunction{
		Name:    "toBool",
		Value:   CallExAdapter(builtinToBoolFunc),
		ValueEx: builtinToBoolFunc,
	},
	BuiltinToBoolWithDefault: &BuiltinFunction{
		Name:    "toBoolWithDefault",
		Value:   CallExAdapter(builtinToBoolWithDefaultFunc),
		ValueEx: builtinToBoolWithDefaultFunc,
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
		Value:   CallExAdapter(builtinToTimeFunc),
		ValueEx: builtinToTimeFunc,
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
	BuiltinHexToStr: &BuiltinFunction{
		Name:    "hexToStr",
		Value:   FnASRS(tk.HexToStr),
		ValueEx: FnASRSex(tk.HexToStr),
	},
	BuiltinToKMG: &BuiltinFunction{
		Name:    "toKMG",
		Value:   FnAAVaRS(tk.IntToKMGT),
		ValueEx: FnAAVaRSex(tk.IntToKMGT),
	},
	BuiltinFloatToStr: &BuiltinFunction{
		Name:    "floatToStr",
		Value:   FnAFRS(tk.Float64ToStr),
		ValueEx: FnAFRSex(tk.Float64ToStr),
	},
	BuiltinStrToUtf8: &BuiltinFunction{
		Name:    "strToUtf8",
		Value:   CallExAdapter(builtinStrToUtf8Func),
		ValueEx: builtinStrToUtf8Func,
	},
	BuiltinStrUtf8ToGb: &BuiltinFunction{
		Name:    "strUtf8ToGb",
		Value:   FnASRS(tk.ConvertToGB18030),
		ValueEx: FnASRSex(tk.ConvertToGB18030),
	},
	BuiltinIsUtf8: &BuiltinFunction{
		Name:    "isUtf8",
		Value:   CallExAdapter(builtinIsUtf8Func),
		ValueEx: builtinIsUtf8Func,
	},
	BuiltinSimpleStrToMap: &BuiltinFunction{
		Name:    "simpleStrToMap",
		Value:   CallExAdapter(builtinSimpleStrToMapFunc),
		ValueEx: builtinSimpleStrToMapFunc,
	},
	BuiltinSimpleStrToMapReverse: &BuiltinFunction{
		Name:    "simpleStrToMapReverse",
		Value:   CallExAdapter(builtinSimpleStrToMapReverseFunc),
		ValueEx: builtinSimpleStrToMapReverseFunc,
	},
	BuiltinReverseMap: &BuiltinFunction{
		Name:    "reverseMap",
		Value:   CallExAdapter(builtinReverseMapFunc),
		ValueEx: builtinReverseMapFunc,
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
	BuiltinStrContainsAny: &BuiltinFunction{
		Name:    "strContainsAny",
		Value:   FnASSRB(strings.ContainsAny),
		ValueEx: FnASSRBex(strings.ContainsAny),
	},
	BuiltinStrContainsIn: &BuiltinFunction{
		Name:    "strContainsIn",
		Value:   FnASVsRB(tk.ContainsIn),
		ValueEx: FnASVsRBex(tk.ContainsIn),
	},
	BuiltinStrIndex: &BuiltinFunction{
		Name:    "strIndex",
		Value:   FnASSRI(strings.Index),
		ValueEx: FnASSRIex(strings.Index),
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
	BuiltinStrSplitN: &BuiltinFunction{
		Name:    "strSplitN",
		Value:   CallExAdapter(builtinStrSplitNFunc),
		ValueEx: builtinStrSplitNFunc,
	},
	BuiltinStrSplitLines: &BuiltinFunction{
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
	BuiltinStrRuneLen: &BuiltinFunction{
		Name:    "strRuneLen",
		Value:   FnASRI(tk.RuneLen),
		ValueEx: FnASRIex(tk.RuneLen),
	},
	BuiltinStrIn: &BuiltinFunction{
		Name:    "strIn",
		Value:   FnASVsRB(tk.InStrings),
		ValueEx: FnASVsRBex(tk.InStrings),
	},
	BuiltinStrGetLastComponent: &BuiltinFunction{
		Name:    "strGetLastComponent",
		Value:   FnASVsRS(tk.GetLastComponentBySeparator),
		ValueEx: FnASVsRSex(tk.GetLastComponentBySeparator),
	},
	BuiltinStrFindDiffPos: &BuiltinFunction{
		Name:    "strFindDiffPos",
		Value:   FnASSRI(tk.FindFirstDiffIndex),
		ValueEx: FnASSRIex(tk.FindFirstDiffIndex),
	},
	BuiltinStrDiff: &BuiltinFunction{
		Name:    "strDiff",
		Value:   FnASSVsRA(tk.StrDiff),
		ValueEx: FnASSVsRAex(tk.StrDiff),
	},
	BuiltinStrFindAllSub: &BuiltinFunction{
		Name:    "strFindAllSub",
		Value:   FnASSRA2I(tk.FindSubStringAll),
		ValueEx: FnASSRA2Iex(tk.FindSubStringAll),
	},
	BuiltinLimitStr: &BuiltinFunction{
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
	BuiltinStrToInt: &BuiltinFunction{
		Name:    "strToInt",
		Value:   FnASViRI(tk.StrToIntWithDefaultValue),
		ValueEx: FnASViRIex(tk.StrToIntWithDefaultValue),
	},
	BuiltinStrToTime: &BuiltinFunction{
		Name:    "strToTime",
		Value:   CallExAdapter(builtinStrToTimeFunc),
		ValueEx: builtinStrToTimeFunc,
	},
	BuiltinDealStr: &BuiltinFunction{
		Name:    "dealStr",
		Value:   CallExAdapter(BuiltinDealStrFunc),
		ValueEx: BuiltinDealStrFunc,
	},
	BuiltinGetTextSimilarity: &BuiltinFunction{
		Name:    "getTextSimilarity",
		Value:   FnASSVIRF(tk.GetTextSimilarity),
		ValueEx: FnASSVIRFex(tk.GetTextSimilarity),
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
	BuiltinRegContainsIn: &BuiltinFunction{
		Name:    "regContainsIn",
		Value:   FnASVsRB(tk.RegContainsIn),
		ValueEx: FnASVsRBex(tk.RegContainsIn),
	},
	BuiltinRegFindFirst: &BuiltinFunction{
		Name:    "regFindFirst",
		Value:   FnASSIRS(tk.RegFindFirstX),
		ValueEx: FnASSIRSex(tk.RegFindFirstX),
	},
	BuiltinRegFindFirstGroups: &BuiltinFunction{
		Name:    "regFindFirstGroups",
		Value:   FnASSRLs(tk.RegFindFirstGroupsX),
		ValueEx: FnASSRLsex(tk.RegFindFirstGroupsX),
	},
	BuiltinRegFindAll: &BuiltinFunction{
		Name:    "regFindAll",
		Value:   FnASSIRLs(tk.RegFindAllX),
		ValueEx: FnASSIRLsex(tk.RegFindAllX),
	},
	BuiltinRegFindAllIndex: &BuiltinFunction{
		Name:    "regFindAllIndex",
		Value:   FnASSRA2N(tk.RegFindAllIndexX),
		ValueEx: FnASSRA2Nex(tk.RegFindAllIndexX),
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
	BuiltinMathAbs: &BuiltinFunction{
		Name:    "mathAbs",
		Value:   CallExAdapter(builtinMathAbsFunc),
		ValueEx: builtinMathAbsFunc,
	},
	BuiltinMathSqrt: &BuiltinFunction{
		Name:    "mathSqrt",
		Value:   CallExAdapter(builtinMathSqrtFunc),
		ValueEx: builtinMathSqrtFunc,
	},
	BuiltinMathPow: &BuiltinFunction{
		Name:    "mathPow",
		Value:   CallExAdapter(builtinMathPowFunc),
		ValueEx: builtinMathPowFunc,
	},
	BuiltinMathExp: &BuiltinFunction{
		Name:    "mathExp",
		Value:   CallExAdapter(builtinMathExpFunc),
		ValueEx: builtinMathExpFunc,
	},
	BuiltinMathLog: &BuiltinFunction{
		Name:    "mathLog",
		Value:   CallExAdapter(builtinMathLogFunc),
		ValueEx: builtinMathLogFunc,
	},
	BuiltinMathLog10: &BuiltinFunction{
		Name:    "mathLog10",
		Value:   CallExAdapter(builtinMathLog10Func),
		ValueEx: builtinMathLog10Func,
	},
	BuiltinMathMin: &BuiltinFunction{
		Name:    "min",
		Value:   FnAVaRA(tk.Min),
		ValueEx: FnAVaRAex(tk.Min),
	},
	BuiltinMathMax: &BuiltinFunction{
		Name:    "max",
		Value:   FnAVaRA(tk.Max),
		ValueEx: FnAVaRAex(tk.Max),
	},
	BuiltinMathCeil: &BuiltinFunction{
		Name:    "ceil",
		Value:   FnAARA(tk.Ceil),
		ValueEx: FnAARAex(tk.Ceil),
	},
	BuiltinMathFloor: &BuiltinFunction{
		Name:    "floor",
		Value:   FnAARA(tk.Floor),
		ValueEx: FnAARAex(tk.Floor),
	},
	BuiltinMathRound: &BuiltinFunction{
		Name:    "round",
		Value:   FnAARA(tk.Round),
		ValueEx: FnAARAex(tk.Round),
	},
	BuiltinFlexEval: &BuiltinFunction{
		Name:    "flexEval",
		Value:   FnASVaRA(tk.FlexEval),
		ValueEx: FnASVaRAex(tk.FlexEval),
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
		Value:   FnAVsRS(tk.GetNowTimeStringFormat),
		ValueEx: FnAVsRSex(tk.GetNowTimeStringFormat),
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
	BuiltinFormatTime: &BuiltinFunction{
		Name:    "formatTime",
		Value:   FnATVsRS(tk.FormatTime),
		ValueEx: FnATVsRSex(tk.FormatTime),
	},
	BuiltinTimeAddSecs: &BuiltinFunction{
		Name:    "timeAddSecs",
		Value:   FnATFRT(tk.TimeAddSecs),
		ValueEx: FnATFRTex(tk.TimeAddSecs),
	},
	BuiltinTimeAddDate: &BuiltinFunction{
		Name:    "timeAddDate",
		Value:   FnATIIIRT(tk.TimeAddDate),
		ValueEx: FnATIIIRTex(tk.TimeAddDate),
	},
	BuiltinTimeBefore: &BuiltinFunction{
		Name:    "timeBefore",
		Value:   CallExAdapter(builtinTimeBeforeFunc),
		ValueEx: builtinTimeBeforeFunc,
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
	BuiltinBytesContains: &BuiltinFunction{
		Name:    "bytesContains",
		Value:   FnALyARB(tk.BytesContains),
		ValueEx: FnALyARBex(tk.BytesContains),
	},
	BuiltinBytesIndex: &BuiltinFunction{
		Name:    "bytesIndex",
		Value:   FnALyARI(tk.BytesIndex),
		ValueEx: FnALyARIex(tk.BytesIndex),
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
	BuiltinIsErrStr: &BuiltinFunction{
		Name:    "isErrStr", // usage: isErrStr(errStr1), check if errStr1 is error string(which starts with TXERROR:)
		Value:   FnASRB(tk.IsErrStr),
		ValueEx: FnASRBex(tk.IsErrStr),
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
	BuiltinCheckEmpty: &BuiltinFunction{
		Name:    "checkEmpty",
		Value:   CallExAdapter(builtinCheckEmptyFunc),
		ValueEx: builtinCheckEmptyFunc,
	},
	BuiltinErrStrf: &BuiltinFunction{
		Name:    "errStrf",
		Value:   FnASVaRS(tk.ErrStrf),
		ValueEx: FnASVaRSex(tk.ErrStrf),
	},
	BuiltinErrf: &BuiltinFunction{
		Name:    "errf",
		Value:   FnASVaRE(tk.Errf),
		ValueEx: FnASVaREex(tk.Errf),
	},
	BuiltinErrToEmpty: &BuiltinFunction{
		Name:    "errToEmpty",
		Value:   CallExAdapter(builtinErrToEmptyFunc),
		ValueEx: builtinErrToEmptyFunc,
	},

	// output/print related
	BuiltinPr: &BuiltinFunction{
		Name:    "pr", // usage: the same as print
		Value:   FnAVaRIE(fmt.Print),
		ValueEx: FnAVaRIEex(fmt.Print),
	},
	BuiltinPrf: &BuiltinFunction{
		Name:    "prf", // usage: the same as printf
		Value:   FnASVaR(tk.Printf),
		ValueEx: FnASVaRex(tk.Printf),
	},
	BuiltinFprf: &BuiltinFunction{
		Name:    "fprf", // usage: the same as fprintf
		Value:   FnAWSVaRIE(fmt.Fprintf),
		ValueEx: FnAWSVaRIEex(fmt.Fprintf),
	},
	BuiltinPl: &BuiltinFunction{
		Name:    "pl", // usage: the same as printf, but with a line-end(\n) at the end
		Value:   CallExAdapter(builtinPlFunc),
		ValueEx: builtinPlFunc,
	},
	BuiltinPlNow: &BuiltinFunction{
		Name:    "plNow",
		Value:   FnASVaR(tk.PlNow),
		ValueEx: FnASVaRex(tk.PlNow),
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
	BuiltinSpln: &BuiltinFunction{
		Name:    "spln", // usage: the same as sprintln
		Value:   FnAVaRS(fmt.Sprintln),
		ValueEx: FnAVaRSex(fmt.Sprintln),
	},

	// scan related
	BuiltinSscanf: &BuiltinFunction{
		Name:    "sscanf",
		Value:   CallExAdapter(builtinSscanfFunc),
		ValueEx: builtinSscanfFunc,
	},

	// process related
	BuiltinGetProcessVar: &BuiltinFunction{
		Name:    "getProcessVar",
		Value:   FnASVaRA(tk.GetVarEx),
		ValueEx: FnASVaRAex(tk.GetVarEx),
	},
	BuiltinSetProcessVar: &BuiltinFunction{
		Name:    "setProcessVar",
		Value:   FnASAR(tk.SetVar),
		ValueEx: FnASARex(tk.SetVar),
	},
	BuiltinDeleteProcessVar: &BuiltinFunction{
		Name:    "deleteProcessVar",
		Value:   FnASR(tk.DeleteVar),
		ValueEx: FnASRex(tk.DeleteVar),
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
	BuiltinReadAllStr: &BuiltinFunction{
		Name:    "readAllStr",
		Value:   CallExAdapter(builtinReadAllStrFunc),
		ValueEx: builtinReadAllStrFunc,
	},
	BuiltinReadAllBytes: &BuiltinFunction{
		Name:    "readAllBytes",
		Value:   CallExAdapter(builtinReadAllBytesFunc),
		ValueEx: builtinReadAllBytesFunc,
	},
	BuiltinReadBytes: &BuiltinFunction{
		Name:    "readBytes",
		Value:   CallExAdapter(builtinReadBytesFunc),
		ValueEx: builtinReadBytesFunc,
	},
	BuiltinWriteStr: &BuiltinFunction{
		Name:    "writeStr",
		Value:   CallExAdapter(builtinWriteStrFunc),
		ValueEx: builtinWriteStrFunc,
	},
	BuiltinWriteBytes: &BuiltinFunction{
		Name:    "writeBytes",
		Value:   CallExAdapter(builtinWriteBytesFunc),
		ValueEx: builtinWriteBytesFunc,
	},
	BuiltinWriteBytesAt: &BuiltinFunction{
		Name:    "writeBytesAt",
		Value:   CallExAdapter(builtinWriteBytesAtFunc),
		ValueEx: builtinWriteBytesAtFunc,
	},
	BuiltinCopyBytes: &BuiltinFunction{
		Name:    "copyBytes",
		Value:   CallExAdapter(builtinCopyBytesFunc),
		ValueEx: builtinCopyBytesFunc,
	},
	BuiltinIoCopy: &BuiltinFunction{
		Name:    "ioCopy",
		Value:   FnAWtRdRI64E(io.Copy),
		ValueEx: FnAWtRdRI64Eex(io.Copy),
		// Value:   CallExAdapter(builtinIoCopyFunc),
		// ValueEx: builtinIoCopyFunc,
	},

	// encode/decode related
	BuiltinMd5: &BuiltinFunction{
		Name:    "md5",
		Value:   FnASRS(tk.MD5Encrypt),
		ValueEx: FnASRSex(tk.MD5Encrypt),
	},
	BuiltinUrlEncode: &BuiltinFunction{
		Name:    "urlEncode",
		Value:   FnASRS(tk.UrlEncode2),
		ValueEx: FnASRSex(tk.UrlEncode2),
	},
	BuiltinUrlEncode1: &BuiltinFunction{
		Name:    "urlEncode1",
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
	BuiltinBase64EncodeByRawUrl: &BuiltinFunction{
		Name:    "base64EncodeByRawUrl",
		Value:   CallExAdapter(builtinBase64EncodeByRawUrlFunc),
		ValueEx: builtinBase64EncodeByRawUrlFunc,
	},
	BuiltinBase64DecodeByRawUrl: &BuiltinFunction{
		Name:    "base64DecodeByRawUrl",
		Value:   CallExAdapter(builtinBase64DecodeByRawUrlFunc),
		ValueEx: builtinBase64DecodeByRawUrlFunc,
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
	BuiltinFormatJson: &BuiltinFunction{
		Name:    "formatJson",
		Value:   FnASVsRS(tk.FormatJson),
		ValueEx: FnASVsRSex(tk.FormatJson),
	},
	BuiltinCompactJson: &BuiltinFunction{
		Name:    "compactJson",
		Value:   FnASVsRS(tk.CompactJson),
		ValueEx: FnASVsRSex(tk.CompactJson),
	},
	BuiltinGetJsonNodeStr: &BuiltinFunction{
		Name:    "getJsonNodeStr",
		Value:   FnASSRS(tk.GetJSONNodeString),
		ValueEx: FnASSRSex(tk.GetJSONNodeString),
	},
	BuiltinGetJsonNodeStrs: &BuiltinFunction{
		Name:    "getJsonNodeStrs",
		Value:   FnASSRA(tk.GetJSONNodeStrings),
		ValueEx: FnASSRAex(tk.GetJSONNodeStrings),
	},
	BuiltinStrsToJson: &BuiltinFunction{
		Name:    "strsToJson",
		Value:   FnAVsRS(tk.StringsToJson),
		ValueEx: FnAVsRSex(tk.StringsToJson),
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
	BuiltinFromXml: &BuiltinFunction{
		Name:    "fromXml",
		Value:   CallExAdapter(builtinFromXmlFunc),
		ValueEx: builtinFromXmlFunc,
	},

	BuiltinFormatXml: &BuiltinFunction{
		Name:    "formatXml",
		Value:   FnASRS(tk.ReshapeXML),
		ValueEx: FnASRSex(tk.ReshapeXML),
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

	BuiltinSystemCmdDetached: &BuiltinFunction{
		Name:    "systemCmdDetached",
		Value:   FnASVsRS(tk.SystemCmdDetached),
		ValueEx: FnASVsRSex(tk.SystemCmdDetached),
	},

	BuiltinSystemStart: &BuiltinFunction{
		Name:    "systemStart",
		Value:   FnASRS(tk.RunWinFileWithSystemDefault),
		ValueEx: FnASRSex(tk.RunWinFileWithSystemDefault),
	},

	BuiltinGetSysInfo: &BuiltinFunction{
		Name:    "getSysInfo",
		Value:   FnAVsRA(tk.GetSystemInfo),
		ValueEx: FnAVsRAex(tk.GetSystemInfo),
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
	BuiltinCreateTempDir: &BuiltinFunction{
		Name:    "createTempDir",
		Value:   FnASSVsRSE(tk.CreateTempDir),
		ValueEx: FnASSVsRSEex(tk.CreateTempDir),
	},
	BuiltinCreateTempFile: &BuiltinFunction{
		Name:    "createTempFile",
		Value:   FnASSVsRSE(tk.CreateTempFile),
		ValueEx: FnASSVsRSEex(tk.CreateTempFile),
	},
	BuiltinChangeDir: &BuiltinFunction{
		Name:    "changeDir",
		Value:   FnASRE(os.Chdir),
		ValueEx: FnASREex(os.Chdir),
	},
	BuiltinLookPath: &BuiltinFunction{
		Name:    "lookPath",
		Value:   FnASRSE(exec.LookPath),
		ValueEx: FnASRSEex(exec.LookPath),
	},
	BuiltinGetInput: &BuiltinFunction{
		Name:    "getInput",
		Value:   FnAVsRS(tk.GetInput),
		ValueEx: FnAVsRSex(tk.GetInput),
	},
	BuiltinGetInputf: &BuiltinFunction{
		Name:    "getInputf",
		Value:   FnASVaRS(tk.GetInputf),
		ValueEx: FnASVaRSex(tk.GetInputf),
	},
	BuiltinGetInputPasswordf: &BuiltinFunction{
		Name:    "getInputPasswordf",
		Value:   FnASVaRS(tk.GetInputPasswordf),
		ValueEx: FnASVaRSex(tk.GetInputPasswordf),
	},
	BuiltinGetChar: &BuiltinFunction{
		Name:    "getChar",
		Value:   FnARA(tk.GetChar),
		ValueEx: FnARAex(tk.GetChar),
	},
	BuiltinGetMultiLineInput: &BuiltinFunction{
		Name:    "getMultiLineInput",
		Value:   CallExAdapter(builtinGetMultiLineInputFunc),
		ValueEx: builtinGetMultiLineInputFunc,
	},
	BuiltinSetStdin: &BuiltinFunction{
		Name:    "setStdin",
		Value:   CallExAdapter(builtinSetStdinFunc),
		ValueEx: builtinSetStdinFunc,
	},
	BuiltinSetStdout: &BuiltinFunction{
		Name:    "setStdout",
		Value:   CallExAdapter(builtinSetStdoutFunc),
		ValueEx: builtinSetStdoutFunc,
	},
	BuiltinSetStderr: &BuiltinFunction{
		Name:    "setStderr",
		Value:   CallExAdapter(builtinSetStderrFunc),
		ValueEx: builtinSetStderrFunc,
	},
	BuiltinGetPipe: &BuiltinFunction{
		Name:    "getPipe",
		Value:   CallExAdapter(builtinGetPipeFunc),
		ValueEx: builtinGetPipeFunc,
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
	BuiltinGetFileInfo: &BuiltinFunction{
		Name:    "getFileInfo",
		Value:   CallExAdapter(builtinGetFileInfoFunc),
		ValueEx: builtinGetFileInfoFunc,
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
	BuiltinExtractFileName: &BuiltinFunction{
		Name:    "extractFileName",
		Value:   FnASRS(filepath.Base),
		ValueEx: FnASRSex(filepath.Base),
	},
	BuiltinCopyFile: &BuiltinFunction{
		Name:    "copyFile",
		Value:   FnASSVsRE(tk.CopyFile),
		ValueEx: FnASSVsREex(tk.CopyFile),
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
		Value:   FnASViRA(tk.LoadBytesFromFile),
		ValueEx: FnASViRAex(tk.LoadBytesFromFile),
		Remark:  `load bytes from file, usage: loadBytes("file.bin"), return error or Bytes([]byte)`,
	},
	BuiltinLoadBytesFromFile: &BuiltinFunction{
		Name:    "loadBytesFromFile",
		Value:   CallExAdapter(builtinLoadBytesFromFileFunc),
		ValueEx: builtinLoadBytesFromFileFunc,
	},
	BuiltinSaveBytes: &BuiltinFunction{
		Name:    "saveBytes",
		Value:   FnALbySRE(tk.SaveBytesToFileE),
		ValueEx: FnALbySREex(tk.SaveBytesToFileE),
		Remark:  `save bytes to file, usage: saveBytes(bytesT, "file.bin"), return error if failed`,
	},
	BuiltinOpenFile: &BuiltinFunction{
		Name:    "openFile",
		Value:   CallExAdapter(builtinOpenFileFunc),
		ValueEx: builtinOpenFileFunc,
	},
	BuiltinCloseFile: &BuiltinFunction{
		Name:    "closeFile",
		Value:   CallExAdapter(builtinCloseFileFunc),
		ValueEx: builtinCloseFileFunc,
	},

	// compress/zip related
	BuiltinCompressData: &BuiltinFunction{
		Name:    "compressData",
		Value:   FnAAVaRA(tk.Compress),
		ValueEx: FnAAVaRAex(tk.Compress),
	},
	BuiltinCompressStr: &BuiltinFunction{
		Name:    "compressStr",
		Value:   FnASRS(tk.CompressText),
		ValueEx: FnASRSex(tk.CompressText),
	},
	BuiltinUncompressData: &BuiltinFunction{
		Name:    "uncompressData",
		Value:   FnAAVaRA(tk.Uncompress),
		ValueEx: FnAAVaRAex(tk.Uncompress),
	},
	BuiltinUncompressStr: &BuiltinFunction{
		Name:    "uncompressStr",
		Value:   FnASRS(tk.UncompressText),
		ValueEx: FnASRSex(tk.UncompressText),
	},
	BuiltinArchiveFilesToZip: &BuiltinFunction{
		Name:    "archiveFilesToZip",
		Value:   CallExAdapter(builtinArchiveFilesToZipFunc),
		ValueEx: builtinArchiveFilesToZipFunc,
	},
	BuiltinGetFileListInArchive: &BuiltinFunction{
		Name:    "getFileListInArchive",
		Value:   FnASVsRA(tk.GetFileListInArchive),
		ValueEx: FnASVsRAex(tk.GetFileListInArchive),
	},
	BuiltinGetFileListInArchiveBytes: &BuiltinFunction{
		Name:    "getFileListInArchiveBytes",
		Value:   FnALyVsRA(tk.GetFileListInArchiveBytes),
		ValueEx: FnALyVsRAex(tk.GetFileListInArchiveBytes),
	},
	BuiltinGetFileListInZip: &BuiltinFunction{
		Name:    "getFileListInZip",
		Value:   FnASVsRA(tk.GetFileListInZip),
		ValueEx: FnASVsRAex(tk.GetFileListInZip),
	},
	BuiltinLoadBytesInArchive: &BuiltinFunction{
		Name:    "loadBytesInArchive",
		Value:   FnASSVsRA(tk.LoadBytesInArchive),
		ValueEx: FnASSVsRAex(tk.LoadBytesInArchive),
	},
	BuiltinLoadBytesInArchiveBytes: &BuiltinFunction{
		Name:    "loadBytesInArchiveBytes",
		Value:   FnALySVsRA(tk.GetFileContentInArchiveBytes),
		ValueEx: FnALySVsRAex(tk.GetFileContentInArchiveBytes),
	},
	BuiltinExtractFileInArchive: &BuiltinFunction{
		Name:    "extractFileInArchive",
		Value:   FnASSSVsRA(tk.ExtractFileInArchive),
		ValueEx: FnASSSVsRAex(tk.ExtractFileInArchive),
	},
	BuiltinExtractArchive: &BuiltinFunction{
		Name:    "extractArchive",
		Value:   FnASSVsRA(tk.ExtractArchive),
		ValueEx: FnASSVsRAex(tk.ExtractArchive),
	},
	BuiltinIsFileNameUtf8InZipBytes: &BuiltinFunction{
		Name:    "isFileNameUtf8InZipBytes",
		Value:   FnALyVsRA(tk.IsFileNameUtf8InZipBytes),
		ValueEx: FnALyVsRAex(tk.IsFileNameUtf8InZipBytes),
	},

	// network/web related
	BuiltinGetWeb: &BuiltinFunction{
		Name:    "getWeb",
		Value:   FnASVaRA(tk.GetWeb),
		ValueEx: FnASVaRAex(tk.GetWeb),
	},
	BuiltinDownloadFile: &BuiltinFunction{
		Name:    "downloadFile",
		Value:   FnASSSVsRS(tk.DownloadFile),
		ValueEx: FnASSSVsRSex(tk.DownloadFile),
	},
	BuiltinGetWebBytes: &BuiltinFunction{
		Name:    "getWebBytes",
		Value:   CallExAdapter(builtinGetWebBytesFunc),
		ValueEx: builtinGetWebBytesFunc,
	},
	BuiltinGetWebBytesWithHeaders: &BuiltinFunction{
		Name:    "getWebBytesWithHeaders",
		Value:   CallExAdapter(builtinGetWebBytesWithHeadersFunc),
		ValueEx: builtinGetWebBytesWithHeadersFunc,
	},
	BuiltinGetWebRespBody: &BuiltinFunction{
		Name:    "getWebRespBody",
		Value:   CallExAdapter(builtinGetWebRespBodyFunc),
		ValueEx: builtinGetWebRespBodyFunc,
	},
	BuiltinPostRequest: &BuiltinFunction{
		Name:    "postRequest",
		Value:   CallExAdapter(builtinPostRequestFunc),
		ValueEx: builtinPostRequestFunc,
	},
	BuiltinPrepareMultiPartFieldFromBytes: &BuiltinFunction{
		Name:    "prepareMultiPartFieldFromBytes",
		Value:   CallExAdapter(builtinPrepareMultiPartFieldFromBytesFunc),
		ValueEx: builtinPrepareMultiPartFieldFromBytesFunc,
	},
	BuiltinPrepareMultiPartFileFromBytes: &BuiltinFunction{
		Name:    "prepareMultiPartFileFromBytes",
		Value:   CallExAdapter(builtinPrepareMultiPartFileFromBytesFunc),
		ValueEx: builtinPrepareMultiPartFileFromBytesFunc,
	},
	BuiltinUrlExists: &BuiltinFunction{
		Name:    "urlExists",
		Value:   FnASVaRA(tk.UrlExists),
		ValueEx: FnASVaRAex(tk.UrlExists),
	},
	BuiltinParseUrl: &BuiltinFunction{
		Name:    "parseUrl",
		Value:   CallExAdapter(builtinParseUrlFunc),
		ValueEx: builtinParseUrlFunc,
	},
	BuiltinParseQuery: &BuiltinFunction{
		Name:    "parseQuery",
		Value:   CallExAdapter(builtinParseQueryFunc),
		ValueEx: builtinParseQueryFunc,
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
	BuiltinGetReqHeaders: &BuiltinFunction{
		Name:    "getReqHeaders",
		Value:   CallExAdapter(builtinGetReqHeadersFunc),
		ValueEx: builtinGetReqHeadersFunc,
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
	BuiltinStartSocksServer: &BuiltinFunction{
		Name:    "startSocksServer",
		Value:   FnAVsRE(tk.StartSocksServer),
		ValueEx: FnAVsREex(tk.StartSocksServer),
	},
	BuiltinStartSocksClient: &BuiltinFunction{
		Name:    "startSocksClient",
		Value:   FnAVsRE(tk.StartSocksClient),
		ValueEx: FnAVsREex(tk.StartSocksClient),
	},
	BuiltinStartTransparentProxy: &BuiltinFunction{
		Name:    "startTransparentProxy",
		Value:   FnASSVsRE(tk.StartTransparentProxy),
		ValueEx: FnASSVsREex(tk.StartTransparentProxy),
	},
	BuiltinStartTransparentProxyEx: &BuiltinFunction{
		Name:    "startTransparentProxyEx",
		Value:   FnASSVsRE(tk.StartTransparentProxy2),
		ValueEx: FnASSVsREex(tk.StartTransparentProxy2),
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
	BuiltinGenJwtToken: &BuiltinFunction{
		Name:    "genJwtToken",
		Value:   CallExAdapter(builtinGenJwtTokenFunc),
		ValueEx: builtinGenJwtTokenFunc,
	},
	BuiltinParseJwtToken: &BuiltinFunction{
		Name:    "parseJwtToken",
		Value:   CallExAdapter(builtinParseJwtTokenFunc),
		ValueEx: builtinParseJwtTokenFunc,
	},
	BuiltinGetOtpCode: &BuiltinFunction{
		Name:    "getOtpCode",
		Value:   FnASVsRS(tk.GenerateOtpCode),
		ValueEx: FnASVsRSex(tk.GenerateOtpCode),
	},
	BuiltinCheckOtpCode: &BuiltinFunction{
		Name:    "checkOtpCode",
		Value:   FnASSVsRB(tk.ValidateOtpCode),
		ValueEx: FnASSVsRBex(tk.ValidateOtpCode),
	},
	BuiltinIsEncrypted: &BuiltinFunction{
		Name:    "isEncrypted",
		Value:   CallExAdapter(builtinIsEncryptedFunc),
		ValueEx: builtinIsEncryptedFunc,
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
	BuiltinEncryptTextByTXTE: &BuiltinFunction{
		Name:    "encryptTextByTXTE",
		Value:   FnASSRS(tk.EncryptStringByTXTE),
		ValueEx: FnASSRSex(tk.EncryptStringByTXTE),
	},
	BuiltinDecryptTextByTXTE: &BuiltinFunction{
		Name:    "decryptTextByTXTE",
		Value:   FnASSRS(tk.DecryptStringByTXTE),
		ValueEx: FnASSRSex(tk.DecryptStringByTXTE),
	},
	BuiltinEncryptDataByTXDEE: &BuiltinFunction{
		Name:    "encryptDataByTXDEE",
		Value:   FnALySRLy(tk.EncryptDataByTXDEE),
		ValueEx: FnALySRLyex(tk.EncryptDataByTXDEE),
	},
	BuiltinDecryptDataByTXDEE: &BuiltinFunction{
		Name:    "decryptDataByTXDEE",
		Value:   FnALySRLy(tk.DecryptDataByTXDEE),
		ValueEx: FnALySRLyex(tk.DecryptDataByTXDEE),
	},
	BuiltinEncryptTextByTXDEE: &BuiltinFunction{
		Name:    "encryptTextByTXDEE",
		Value:   FnASSRS(tk.EncryptStringByTXDEE),
		ValueEx: FnASSRSex(tk.EncryptStringByTXDEE),
	},
	BuiltinDecryptTextByTXDEE: &BuiltinFunction{
		Name:    "decryptTextByTXDEE",
		Value:   FnASSRS(tk.DecryptStringByTXDEE),
		ValueEx: FnASSRSex(tk.DecryptStringByTXDEE),
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
	BuiltinEncryptStream: &BuiltinFunction{
		Name:    "encryptStream",
		Value:   FnARSWRE(tk.EncryptStreamByTXDEF),
		ValueEx: FnARSWREex(tk.EncryptStreamByTXDEF),
	},
	BuiltinDecryptStream: &BuiltinFunction{
		Name:    "decryptStream",
		Value:   FnARSWRE(tk.DecryptStreamByTXDEF),
		ValueEx: FnARSWREex(tk.DecryptStreamByTXDEF),
	},
	BuiltinAesEncrypt: &BuiltinFunction{
		Name:    "aesEncrypt",
		Value:   CallExAdapter(builtinAesEncryptFunc),
		ValueEx: builtinAesEncryptFunc,
	},
	BuiltinAesDecrypt: &BuiltinFunction{
		Name:    "aesDecrypt",
		Value:   CallExAdapter(builtinAesDecryptFunc),
		ValueEx: builtinAesDecryptFunc,
	},

	// image related
	BuiltinLoadImageFromBytes: &BuiltinFunction{
		Name:    "loadImageFromBytes",
		Value:   FnALyVsRA(tk.LoadImageFromBytes),
		ValueEx: FnALyVsRAex(tk.LoadImageFromBytes),
	},
	BuiltinSaveImageToBytes: &BuiltinFunction{
		Name:    "saveImageToBytes",
		Value:   CallExAdapter(builtinSaveImageToBytesFunc),
		ValueEx: builtinSaveImageToBytesFunc,
	},
	BuiltinLoadImageFromFile: &BuiltinFunction{
		Name:    "loadImageFromFile",
		Value:   FnASVsRA(tk.LoadImageFromFile),
		ValueEx: FnASVsRAex(tk.LoadImageFromFile),
	},
	BuiltinSaveImageToFile: &BuiltinFunction{
		Name:    "saveImageToFile",
		Value:   CallExAdapter(builtinSaveImageToFileFunc),
		ValueEx: builtinSaveImageToFileFunc,
	},
	BuiltinGetImageInfo: &BuiltinFunction{
		Name:    "getImageInfo",
		Value:   CallExAdapter(builtinGetImageInfoFunc),
		ValueEx: builtinGetImageInfoFunc,
	},
	BuiltinStrToRgba: &BuiltinFunction{
		Name:    "strToRgba",
		Value:   CallExAdapter(builtinStrToRgbaFunc),
		ValueEx: builtinStrToRgbaFunc,
	},
	BuiltinEncodeImage: &BuiltinFunction{
		Name:    "encodeImage",
		Value:   CallExAdapter(builtinEncodeImageFunc),
		ValueEx: builtinEncodeImageFunc,
	},
	BuiltinEncodeBytesInImage: &BuiltinFunction{
		Name:    "encodeBytesInImage",
		Value:   CallExAdapter(builtinEncodeBytesInImageFunc),
		ValueEx: builtinEncodeBytesInImageFunc,
	},
	BuiltinDecodeBytesFromImage: &BuiltinFunction{
		Name:    "decodeBytesFromImage",
		Value:   CallExAdapter(builtinDecodeBytesFromImageFunc),
		ValueEx: builtinDecodeBytesFromImageFunc,
	},
	BuiltinDrawImageOnImage: &BuiltinFunction{
		Name:    "drawImageOnImage",
		Value:   CallExAdapter(builtinDrawImageOnImageFunc),
		ValueEx: builtinDrawImageOnImageFunc,
	},
	BuiltinDrawTextWrappedOnImage: &BuiltinFunction{
		Name:    "drawTextWrappedOnImage",
		Value:   CallExAdapter(builtinDrawTextWrappedOnImageFunc),
		ValueEx: builtinDrawTextWrappedOnImageFunc,
	},
	BuiltinGenQr: &BuiltinFunction{
		Name:    "genQr",
		Value:   CallExAdapter(builtinGenQrFunc),
		ValueEx: builtinGenQrFunc,
	},
	BuiltinImageToAscii: &BuiltinFunction{
		Name:    "imageToAscii",
		Value:   CallExAdapter(builtinImageToAsciiFunc),
		ValueEx: builtinImageToAsciiFunc,
	},
	BuiltinResizeImage: &BuiltinFunction{
		Name:    "resizeImage",
		Value:   CallExAdapter(builtinResizeImageFunc),
		ValueEx: builtinResizeImageFunc,
	},

	// plot related
	BuiltinPlotClearConsole: &BuiltinFunction{
		Name:    "plotClearConsole",
		Value:   FnAR(asciigraph.Clear),
		ValueEx: FnARex(asciigraph.Clear),
	},

	BuiltinPlotDataToStr: &BuiltinFunction{
		Name:    "plotDataToStr",
		Value:   CallExAdapter(builtinPlotDataToStrFunc),
		ValueEx: builtinPlotDataToStrFunc,
	},

	BuiltinPlotDataToImage: &BuiltinFunction{
		Name:    "plotDataToImage",
		Value:   CallExAdapter(builtinPlotDataToImageFunc),
		ValueEx: builtinPlotDataToImageFunc,
	},

	BuiltinPlotLoadFont: &BuiltinFunction{
		Name:    "plotLoadFont",
		Value:   CallExAdapter(builtinPlotLoadFontFunc),
		ValueEx: builtinPlotLoadFontFunc,
	},

	// ssh/ftp related
	BuiltinFtpList: &BuiltinFunction{
		Name:    "ftpList",
		Value:   CallExAdapter(builtinFtpListFunc),
		ValueEx: builtinFtpListFunc,
	},

	BuiltinFtpCreateDir: &BuiltinFunction{
		Name:    "ftpCreateDir",
		Value:   CallExAdapter(builtinFtpCreateDirFunc),
		ValueEx: builtinFtpCreateDirFunc,
	},

	BuiltinFtpSize: &BuiltinFunction{
		Name:    "ftpSize",
		Value:   CallExAdapter(builtinFtpSizeFunc),
		ValueEx: builtinFtpSizeFunc,
	},

	BuiltinFtpUpload: &BuiltinFunction{
		Name:    "ftpUpload",
		Value:   CallExAdapter(builtinFtpUploadFunc),
		ValueEx: builtinFtpUploadFunc,
	},

	BuiltinFtpUploadFromReader: &BuiltinFunction{
		Name:    "ftpUploadFromReader",
		Value:   CallExAdapter(builtinFtpUploadFromReaderFunc),
		ValueEx: builtinFtpUploadFromReaderFunc,
	},

	BuiltinFtpDownloadBytes: &BuiltinFunction{
		Name:    "ftpDownloadBytes",
		Value:   CallExAdapter(builtinFtpDownloadBytesFunc),
		ValueEx: builtinFtpDownloadBytesFunc,
	},

	BuiltinSshUpload: &BuiltinFunction{
		Name:    "sshUpload",
		Value:   CallExAdapter(builtinSshUploadFunc),
		ValueEx: builtinSshUploadFunc,
	},
	BuiltinSshUploadBytes: &BuiltinFunction{
		Name:    "sshUploadBytes",
		Value:   CallExAdapter(builtinSshUploadBytesFunc),
		ValueEx: builtinSshUploadBytesFunc,
	},
	BuiltinSshDownload: &BuiltinFunction{
		Name:    "sshDownload",
		Value:   CallExAdapter(builtinSshDownloadFunc),
		ValueEx: builtinSshDownloadFunc,
	},
	BuiltinSshDownloadBytes: &BuiltinFunction{
		Name:    "sshDownloadBytes",
		Value:   CallExAdapter(builtinSshDownloadBytesFunc),
		ValueEx: builtinSshDownloadBytesFunc,
	},
	BuiltinSshRun: &BuiltinFunction{
		Name:    "sshRun",
		Value:   CallExAdapter(builtinSshRunFunc),
		ValueEx: builtinSshRunFunc,
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
	BuiltinExcelNew: &BuiltinFunction{
		Name:    "excelNew",
		Value:   CallExAdapter(NewExcel),
		ValueEx: NewExcel,
	},
	BuiltinExcelOpen: &BuiltinFunction{
		Name:    "excelOpen",
		Value:   CallExAdapter(builtinExcelOpenFunc),
		ValueEx: builtinExcelOpenFunc,
	},
	BuiltinExcelOpenFile: &BuiltinFunction{
		Name:    "excelOpenFile",
		Value:   CallExAdapter(builtinExcelOpenFileFunc),
		ValueEx: builtinExcelOpenFileFunc,
	},
	BuiltinExcelSaveAs: &BuiltinFunction{
		Name:    "excelSaveAs",
		Value:   CallExAdapter(builtinExcelSaveAsFunc),
		ValueEx: builtinExcelSaveAsFunc,
	},
	BuiltinExcelWriteTo: &BuiltinFunction{
		Name:    "excelWriteTo",
		Value:   CallExAdapter(builtinExcelWriteToFunc),
		ValueEx: builtinExcelWriteToFunc,
	},
	BuiltinExcelClose: &BuiltinFunction{
		Name:    "excelClose",
		Value:   CallExAdapter(builtinExcelCloseFunc),
		ValueEx: builtinExcelCloseFunc,
	},
	BuiltinExcelNewSheet: &BuiltinFunction{
		Name:    "excelNewSheet",
		Value:   CallExAdapter(builtinExcelNewSheetFunc),
		ValueEx: builtinExcelNewSheetFunc,
	},
	BuiltinExcelReadAll: &BuiltinFunction{
		Name:    "excelReadAll",
		Value:   CallExAdapter(builtinExcelReadAllFunc),
		ValueEx: builtinExcelReadAllFunc,
	},
	BuiltinExcelGetSheetCount: &BuiltinFunction{
		Name:    "excelGetSheetCount",
		Value:   CallExAdapter(builtinExcelGetSheetCountFunc),
		ValueEx: builtinExcelGetSheetCountFunc,
	},
	BuiltinExcelGetSheetList: &BuiltinFunction{
		Name:    "excelGetSheetList",
		Value:   CallExAdapter(builtinExcelGetSheetListFunc),
		ValueEx: builtinExcelGetSheetListFunc,
	},
	BuiltinExcelGetSheetName: &BuiltinFunction{
		Name:    "excelGetSheetName",
		Value:   CallExAdapter(builtinExcelGetSheetNameFunc),
		ValueEx: builtinExcelGetSheetNameFunc,
	},
	BuiltinExcelReadSheet: &BuiltinFunction{
		Name:    "excelReadSheet",
		Value:   CallExAdapter(builtinExcelReadSheetFunc),
		ValueEx: builtinExcelReadSheetFunc,
	},
	BuiltinExcelWriteSheet: &BuiltinFunction{
		Name:    "excelWriteSheet",
		Value:   CallExAdapter(builtinExcelWriteSheetFunc),
		ValueEx: builtinExcelWriteSheetFunc,
	},
	BuiltinExcelReadCell: &BuiltinFunction{
		Name:    "excelReadCell",
		Value:   CallExAdapter(builtinExcelReadCellFunc),
		ValueEx: builtinExcelReadCellFunc,
	},
	BuiltinExcelWriteCell: &BuiltinFunction{
		Name:    "excelWriteCell",
		Value:   CallExAdapter(builtinExcelWriteCellFunc),
		ValueEx: builtinExcelWriteCellFunc,
	},
	BuiltinExcelGetColumnIndexByName: &BuiltinFunction{
		Name:    "excelGetColumnIndexByName",
		Value:   CallExAdapter(builtinExcelGetColumnIndexByNameFunc),
		ValueEx: builtinExcelGetColumnIndexByNameFunc,
	},
	BuiltinExcelGetColumnNameByIndex: &BuiltinFunction{
		Name:    "excelGetColumnNameByIndex",
		Value:   CallExAdapter(builtinExcelGetColumnNameByIndexFunc),
		ValueEx: builtinExcelGetColumnNameByIndexFunc,
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
	BuiltinToPinYin: &BuiltinFunction{
		Name:    "toPinYin",
		Value:   FnASVsRA(tk.ToPinYin),
		ValueEx: FnASVsRAex(tk.ToPinYin),
	},
	BuiltinKanjiToKana: &BuiltinFunction{
		Name:    "kanjiToKana",
		Value:   FnASRSE(kanjikana.ConvertKanjiToKana),
		ValueEx: FnASRSEex(kanjikana.ConvertKanjiToKana),
	},
	BuiltinKanaToRomaji: &BuiltinFunction{
		Name:    "kanaToRomaji",
		Value:   FnASRS(kanjikana.ConvertKanaToRomaji),
		ValueEx: FnASRSex(kanjikana.ConvertKanaToRomaji),
	},
	BuiltinKanjiToRomaji: &BuiltinFunction{
		Name:    "kanjiToRomaji",
		Value:   FnASRSE(kanjikana.ConvertKanjiToRomaji),
		ValueEx: FnASRSEex(kanjikana.ConvertKanjiToRomaji),
	},

	// line editor related
	BuiltinLeClear: &BuiltinFunction{
		Name:    "leClear",
		Value:   CallExAdapter(builtinLeClearFunc),
		ValueEx: builtinLeClearFunc,
	},
	BuiltinLeInfo: &BuiltinFunction{
		Name:    "leInfo",
		Value:   CallExAdapter(builtinLeInfoFunc),
		ValueEx: builtinLeInfoFunc,
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
	BuiltinLeSshInfo: &BuiltinFunction{
		Name:    "leSshInfo",
		Value:   CallExAdapter(builtinLeSshInfoFunc),
		ValueEx: builtinLeSshInfoFunc,
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

	// mail related

	BuiltinSendMail: &BuiltinFunction{
		Name:    "sendMail",
		Value:   CallExAdapter(builtinSendMailFunc),
		ValueEx: builtinSendMailFunc,
	},

	// GUI server related

	BuiltinGuiServerCommand: &BuiltinFunction{
		Name:    "guiServerCommand",
		Value:   CallExAdapter(builtinGuiServerCommandFunc),
		ValueEx: builtinGuiServerCommandFunc,
	},

	// s3 related

	BuiltinS3GetObjectBytes: &BuiltinFunction{
		Name:    "s3GetObjectBytes",
		Value:   CallExAdapter(builtinS3GetObjectBytesFunc),
		ValueEx: builtinS3GetObjectBytesFunc,
	},

	BuiltinS3GetObjectText: &BuiltinFunction{
		Name:    "s3GetObjectText",
		Value:   CallExAdapter(builtinS3GetObjectTextFunc),
		ValueEx: builtinS3GetObjectTextFunc,
	},

	BuiltinS3PutObject: &BuiltinFunction{
		Name:    "s3PutObject",
		Value:   CallExAdapter(builtinS3PutObjectFunc),
		ValueEx: builtinS3PutObjectFunc,
	},

	BuiltinS3GetObjectToFile: &BuiltinFunction{
		Name:    "s3GetObjectToFile",
		Value:   CallExAdapter(builtinS3GetObjectToFileFunc),
		ValueEx: builtinS3GetObjectToFileFunc,
	},

	BuiltinS3GetObjectReader: &BuiltinFunction{
		Name:    "s3GetObjectReader",
		Value:   CallExAdapter(builtinS3GetObjectReaderFunc),
		ValueEx: builtinS3GetObjectReaderFunc,
	},

	BuiltinS3GetObjectUrl: &BuiltinFunction{
		Name:    "s3GetObjectUrl",
		Value:   CallExAdapter(builtinS3GetObjectUrlFunc),
		ValueEx: builtinS3GetObjectUrlFunc,
	},

	BuiltinS3GetObjectTags: &BuiltinFunction{
		Name:    "s3GetObjectTags",
		Value:   CallExAdapter(builtinS3GetObjectTagsFunc),
		ValueEx: builtinS3GetObjectTagsFunc,
	},

	BuiltinS3GetObjectStat: &BuiltinFunction{
		Name:    "s3GetObjectStat",
		Value:   CallExAdapter(builtinS3GetObjectStatFunc),
		ValueEx: builtinS3GetObjectStatFunc,
	},

	BuiltinS3StatObject: &BuiltinFunction{
		Name:    "s3StatObject",
		Value:   CallExAdapter(builtinS3StatObjectFunc),
		ValueEx: builtinS3StatObjectFunc,
	},

	BuiltinS3CopyObject: &BuiltinFunction{
		Name:    "s3CopyObject",
		Value:   CallExAdapter(builtinS3CopyObjectFunc),
		ValueEx: builtinS3CopyObjectFunc,
	},

	BuiltinS3MoveObject: &BuiltinFunction{
		Name:    "s3MoveObject",
		Value:   CallExAdapter(builtinS3MoveObjectFunc),
		ValueEx: builtinS3MoveObjectFunc,
	},

	BuiltinS3RemoveObject: &BuiltinFunction{
		Name:    "s3RemoveObject",
		Value:   CallExAdapter(builtinS3RemoveObjectFunc),
		ValueEx: builtinS3RemoveObjectFunc,
	},

	BuiltinS3ListObjects: &BuiltinFunction{
		Name:    "s3ListObjects",
		Value:   CallExAdapter(builtinS3ListObjectsFunc),
		ValueEx: builtinS3ListObjectsFunc,
	},

	// 3rd party related

	BuiltinAwsSign: &BuiltinFunction{
		Name:    "awsSign",
		Value:   CallExAdapter(builtinAwsSignFunc),
		ValueEx: builtinAwsSignFunc,
	},

	// misc related
	BuiltinMagic: &BuiltinFunction{
		Name: "magic",
		Value:   CallExAdapter(builtinMagicFunc),
		ValueEx: builtinMagicFunc,
	},
	BuiltinGetSeq: &BuiltinFunction{
		Name: "getSeq",
		Value: func(args ...Object) (Object, error) {
			return FnARIex(tk.GetSeq)(NewCall(nil, args))
		},
		ValueEx: FnARIex(tk.GetSeq),
	},
	BuiltinGetUuid: &BuiltinFunction{
		Name:    "getUuid",
		Value:   FnARS(tk.GetUUID),
		ValueEx: FnARSex(tk.GetUUID),
	},
	BuiltinRenderMarkdown: &BuiltinFunction{
		Name:    "renderMarkdown",
		Value:   FnASRS(tk.RenderMarkdown),
		ValueEx: FnASRSex(tk.RenderMarkdown),
	},
	BuiltinReplaceHtmlByMap: &BuiltinFunction{
		Name:    "replaceHtmlByMap",
		Value:   CallExAdapter(builtinReplaceHtmlByMapFunc),
		ValueEx: builtinReplaceHtmlByMapFunc,
	},
	BuiltinProcessHtmlTemplate: &BuiltinFunction{
		Name: "processHtmlTemplate",
		// Value:   CallExAdapter(builtinProcessHtmlTemplateFunc),
		// ValueEx: builtinProcessHtmlTemplateFunc,
		Value:   FnASSAVaRA(tk.ProcessHtmlTemplate),
		ValueEx: FnASSAVaRAex(tk.ProcessHtmlTemplate),
	},
	BuiltinStatusToStr: &BuiltinFunction{
		Name: "statusToStr",
		Value:   FnASRS(tk.StatusToString),
		ValueEx: FnASRSex(tk.StatusToString),
	},
	BuiltinStatusToMap: &BuiltinFunction{
		Name: "statusToMap",
		Value:   FnASRA(tk.StatusToMap),
		ValueEx: FnASRAex(tk.StatusToMap),
	},
	BuiltinDocxToStrs: &BuiltinFunction{
		Name: "docxToStrs",
		Value:   CallExAdapter(builtinDocxToStrsFunc),
		ValueEx: builtinDocxToStrsFunc,
	},
	BuiltinDocxReplacePattern: &BuiltinFunction{
		Name: "docxReplacePattern",
		Value:   CallExAdapter(builtinDocxReplacePatternFunc),
		ValueEx: builtinDocxReplacePatternFunc,
	},
	BuiltinDocxGetPlaceholders: &BuiltinFunction{
		Name: "docxGetPlaceholders",
		Value:   CallExAdapter(builtinDocxGetPlaceholdersFunc),
		ValueEx: builtinDocxGetPlaceholdersFunc,
	},
	BuiltinShowTable: &BuiltinFunction{
		Name: "showTable",
		Value:   CallExAdapter(builtinShowTableFunc),
		ValueEx: builtinShowTableFunc,
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
		
//		fmt.Printf("obj: %#v\n", obj)

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

func builtinResetFunc(c Call) (Object, error) {
	args := c.GetArgs()
	
	lenT := len(args)

	if lenT < 1 {
		return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
	}

	target := args[0]

	switch obj := target.(type) {
	case Array:
		if lenT > 2 {
			obj = make(Array, ToIntQuick(args[1]), ToIntQuick(args[2]))
		} else if lenT > 1 {
			obj = make(Array, ToIntQuick(args[1]))
		} else {
			obj = make(Array, 0)
		}

		return obj, nil
	case Bytes:
		if lenT > 2 {
			obj = make(Bytes, ToIntQuick(args[1]), ToIntQuick(args[2]))
		} else if lenT > 1 {
			obj = make(Bytes, ToIntQuick(args[1]))
		} else {
			obj = make(Bytes, 0)
		}

		return obj, nil
	case Chars:
		if lenT > 2 {
			obj = make(Chars, ToIntQuick(args[1]), ToIntQuick(args[2]))
		} else if lenT > 1 {
			obj = make(Chars, ToIntQuick(args[1]))
		} else {
			obj = make(Chars, 0)
		}

		return obj, nil
	case String:
		obj.Value = ""

		return obj, nil
	case Map:
		if lenT > 1 {
			obj = make(Map, ToIntQuick(args[1]))
		} else {
			obj = make(Map, 0)
		}

		return obj, nil
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

func builtinGetArrayItemFunc(c Call) (Object, error) {
	args := c.GetArgs()

	defaultT := Undefined

	if len(args) < 2 {
		return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
	}

	if len(args) > 2 {
		defaultT = args[2]
	}

	lenT := 0

	target, ok := args[0].(LengthGetter)

	if ok {
		lenT = target.Len()
	} else {
		return defaultT, NewCommonErrorWithPos(c, "unable to get length")
	}

	idxT := ToIntQuick(args[1])

	if idxT < 0 || idxT >= lenT {
		return defaultT, nil
	}

	return args[0].IndexGet(Int(idxT))
}

func builtinGetMapItemFunc(c Call) (Object, error) {
	args := c.GetArgs()

	defaultT := Undefined

	if len(args) < 2 {
		return defaultT, NewCommonErrorWithPos(c, "not enough parameters")
	}

	if len(args) > 2 {
		defaultT = args[2]
	}

	// tk.Pl("defaultT: %#v", defaultT)

	valueT, errT := args[0].IndexGet(args[1])

	if errT != nil {
		return defaultT, nil
	}

	b := isErrX(valueT)

	if b {
		return defaultT, nil
	}

	if IsUndefInternal(valueT) {
		return defaultT, nil
	}

	return valueT, nil
}

func builtinSetMapItemFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 3 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	nv1, ok := args[0].(Map)

	if !ok {
		return NewCommonErrorWithPos(c, "unsupported type: %T", args[0]), nil
	}

	keyT := args[1].String()

	nv1[keyT] = args[2]

	return Undefined, nil
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
	case Map:
		rs := tk.NewOrderedMap()

		for k, v := range obj {
			rs.Set(k, v)
		}

		errT := rs.SortStringKeys()

		if errT != nil {
			return NewCommonError("failed to sort: %v", errT), nil
		}

		return &OrderedMap{Value: rs}, nil
	case *OrderedMap:
		errT := obj.Value.SortStringKeys()

		if errT != nil {
			return NewCommonError("failed to sort: %v", errT), nil
		}

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
	case Map:
		rs := tk.NewOrderedMap()

		for k, v := range obj {
			rs.Set(k, v)
		}

		errT := rs.SortStringKeys("-desc")

		if errT != nil {
			return NewCommonError("failed to sort: %v", errT), nil
		}

		return &OrderedMap{Value: rs}, nil
	case *OrderedMap:
		errT := obj.Value.SortStringKeys("-desc")

		if errT != nil {
			return NewCommonError("failed to sort: %v", errT), nil
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

// 	if len(args) < 2 {
// 		return NewCommonErrorWithPos(c, "not enough parameters"), nil
// 	}

// 	arg0 := args[0]

// 	nv1, ok := args[1].(*CharCode)

// 	if !ok {
// 		nv2 := args[1].String()

// 		nv3, errT := builtinCharCodeFunc(Call{Vm: c.VM(), Args: []Object{String{Value: nv2}}})

// 		if errT != nil {
// 			return NewCommonErrorWithPos(c, "failed to compile function: (%T)%v", args[1], errT), nil
// 		}

// 		if tk.IsError(nv3) {
// 			return NewCommonErrorWithPos(c, "failed to compile function: (%T)%v", args[1], nv3), nil
// 		}

// 		nv1 = nv3.(*CharCode)
// 	}

// 	switch obj := arg0.(type) {
// 	case Array:
// 		sort.Slice(obj, func(i, j int) bool {
// 			retT := nv1.RunAsFunc(obj[i], obj[j])

// 			if tk.IsError(retT) {
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
// 			retT := nv1.RunAsFunc(ToStringObject(obj.Value[i]), ToStringObject(obj.Value[j]))

// 			if tk.IsError(retT) {
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
// 			retT := nv1.RunAsFunc(ToStringObject(obj.Value[i]), ToStringObject(obj.Value[j]))

// 			if tk.IsError(retT) {
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
// 			retT := nv1.RunAsFunc(ToStringObject(obj.Value[i]), ToStringObject(obj.Value[j]))

// 			if tk.IsError(retT) {
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

func builtinBytesWithSizeFunc(c Call) (Object, error) {
	sizeT := 0
	if c.Len() > 0 {
		sizeT = ToIntQuick(c.Get(0))
	}

	return Bytes(make([]byte, sizeT)), nil
}

func builtinBytesWithCapFunc(c Call) (Object, error) {
	capT := 0
	if c.Len() > 0 {
		capT = ToIntQuick(c.Get(0))
	}

	return Bytes(make([]byte, 0, capT)), nil
}

func builtinCharsFunc(arg Object) (ret Object, err error) {
	switch obj := arg.(type) {
	case Byte:
		aryT := make(Chars, 1)
		aryT[0] = rune(obj)

		ret = aryT
		return
	case Char:
		aryT := make(Chars, 1)
		aryT[0] = rune(obj)

		ret = aryT
		return
	case Int:
		aryT := make(Chars, 1)
		aryT[0] = rune(obj)

		ret = aryT
		return
	case String:
		s := obj.Value
		ret = make(Chars, 0, utf8.RuneCountInString(s))
		sz := len(obj.Value)
		i := 0

		for i < sz {
			r, w := utf8.DecodeRuneInString(s[i:])
			if r == utf8.RuneError {
				return Undefined, nil
			}
			ret = append(ret.(Chars), r)
			i += w
		}
	case *MutableString:
		s := obj.Value
		ret = make(Chars, 0, utf8.RuneCountInString(s))
		sz := len(obj.Value)
		i := 0

		for i < sz {
			r, w := utf8.DecodeRuneInString(s[i:])
			if r == utf8.RuneError {
				return Undefined, nil
			}
			ret = append(ret.(Chars), r)
			i += w
		}
	case Bytes:
		ret = make(Chars, 0, utf8.RuneCount(obj))
		sz := len(obj)
		i := 0

		for i < sz {
			r, w := utf8.DecodeRune(obj[i:])
			if r == utf8.RuneError {
				return Undefined, nil
			}
			ret = append(ret.(Chars), r)
			i += w
		}
	case Chars:
		sz := len(obj)
		ret = make(Chars, 0, sz)
		for i := 0; i < sz; i++ {
			ret = append(ret.(Chars), obj[i])
		}
	case Array:
		sz := len(obj)
		ret = make(Chars, 0, sz)
		for i := 0; i < sz; i++ {
			ret = append(ret.(Chars), rune(ToIntQuick(obj[i])))
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

// like tk.Pass
func FnAR(fn func()) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		fn()
		return Undefined, nil
	}
}

func FnARex(fn func()) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		fn()
		return Undefined, nil
	}
}

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
			return Undefined, NewCommonError("not enough parameters")
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
			return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
		}

		nv, ok := args[0].(*Time)

		if !ok {
			return NewCommonError("unsupported type: %T", args[0]), nil
		}

		rs := fn(nv.Value)
		return String{Value: rs}, nil
	}
}

// like tk.FormatTime
func FnATVsRS(fn func(time.Time, ...string) string) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return Undefined, NewCommonError("not enough parameters")
		}

		nv, ok := args[0].(*Time)

		if !ok {
			return NewCommonError("unsupported type: %T", args[0]), nil
		}

		vs := ObjectsToS(args[1:])

		rs := fn(nv.Value, vs...)
		return String{Value: rs}, nil
	}
}

func FnATVsRSex(fn func(time.Time, ...string) string) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()

		if len(args) < 1 {
			return Undefined, NewCommonError("not enough parameters")
		}

		nv, ok := args[0].(*Time)

		if !ok {
			return NewCommonError("unsupported type: %T", args[0]), nil
		}

		vs := ObjectsToS(args[1:])

		rs := fn(nv.Value, vs...)
		return String{Value: rs}, nil
	}
}

// like tk.TimeAddSecs
func FnATFRT(fn func(time.Time, float64) time.Time) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 2 {
			return Undefined, NewCommonError("not enough parameters")
		}

		nv, ok := args[0].(*Time)

		if !ok {
			return NewCommonError("unsupported type: %T", args[0]), nil
		}

		v2 := ToFloatQuick(args[1])

		rs := fn(nv.Value, v2)
		return &Time{Value: rs}, nil
	}
}

func FnATFRTex(fn func(time.Time, float64) time.Time) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()

		if len(args) < 2 {
			return Undefined, NewCommonError("not enough parameters")
		}

		nv, ok := args[0].(*Time)

		if !ok {
			return NewCommonError("unsupported type: %T", args[0]), nil
		}

		v2 := ToFloatQuick(args[1])

		rs := fn(nv.Value, v2)
		return &Time{Value: rs}, nil
	}
}

// like tk.TimeAddDate
func FnATIIIRT(fn func(time.Time, int, int, int) time.Time) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 4 {
			return Undefined, NewCommonError("not enough parameters")
		}

		nv, ok := args[0].(*Time)

		if !ok {
			return NewCommonError("unsupported type: %T", args[0]), nil
		}

		v2 := ToIntQuick(args[1])
		v3 := ToIntQuick(args[2])
		v4 := ToIntQuick(args[3])

		rs := fn(nv.Value, v2, v3, v4)
		return &Time{Value: rs}, nil
	}
}

func FnATIIIRTex(fn func(time.Time, int, int, int) time.Time) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()

		if len(args) < 4 {
			return Undefined, NewCommonError("not enough parameters")
		}

		nv, ok := args[0].(*Time)

		if !ok {
			return NewCommonError("unsupported type: %T", args[0]), nil
		}

		v2 := ToIntQuick(args[1])
		v3 := ToIntQuick(args[2])
		v4 := ToIntQuick(args[3])

		rs := fn(nv.Value, v2, v3, v4)
		return &Time{Value: rs}, nil
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

// like io.Copy
func FnAWtRdRI64E(fn func(io.Writer, io.Reader) (int64, error)) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 2 {
			return Undefined, NewCommonError("not enough parameters")
		}

		nv1, ok := args[0].(io.Writer)

		if !ok {
			return NewCommonError("unsupported parameter 1 type: %T", args[0]), nil
		}

		nv2, ok := args[1].(io.Reader)

		if !ok {
			return NewCommonError("unsupported parameter 2 type: %T", args[1]), nil
		}

		rs, errT := fn(nv1, nv2)

		if errT != nil {
			return NewCommonError("%v", errT), nil
		}

		return ToIntObject(rs), nil
	}
}

func FnAWtRdRI64Eex(fn func(io.Writer, io.Reader) (int64, error)) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()

		if len(args) < 2 {
			return Undefined, NewCommonError("not enough parameters")
		}

		nv1, ok := args[0].(io.Writer)

		if !ok {
			return NewCommonError("unsupported parameter 1 type: %T", args[0]), nil
		}

		nv2, ok := args[1].(io.Reader)

		if !ok {
			return NewCommonError("unsupported parameter 2 type: %T", args[1]), nil
		}

		rs, errT := fn(nv1, nv2)

		if errT != nil {
			return NewCommonError("%v", errT), nil
		}

		return ToIntObject(rs), nil
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

//// like tkc.StatusToMap
//func FnASRM(fn func(string) interface) CallableFunc {
//	return func(args ...Object) (ret Object, err error) {
//		if len(args) < 1 {
//			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
//		}
//
//		rs := fn(args[0].String())
//		return ConvertToObject(rs), nil
//	}
//}
//
//func FnASRMex(fn func(string) map[string]string) CallableExFunc {
//	return func(c Call) (ret Object, err error) {
//		args := c.GetArgs()
//
//		if len(args) < 1 {
//			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
//		}
//
//		rs := fn(args[0].String())
//		return ConvertToObject(rs), nil
//	}
//}

// like tk.ProcessHtmlTemplate
func FnASSAVaRA(fn func(string, string, interface{}, ...string) interface{}) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 3 {
			return Undefined, NewCommonError("not enough parameters")
		}

		vs := ObjectsToS(args[3:])

		rs := fn(args[0].String(), args[1].String(), ConvertFromObject(args[2]), vs...)

		return ConvertToObject(rs), nil
	}
}

func FnASSAVaRAex(fn func(string, string, interface{}, ...string) interface{}) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()

		if len(args) < 3 {
			return NewCommonErrorWithPos(c, "not enough parameters"), nil
		}

		vs := ObjectsToS(args[3:])

		rs := fn(args[0].String(), args[1].String(), ConvertFromObject(args[2]), vs...)

		return ConvertToObject(rs), nil
	}
}

// like tk.RuneLen
func FnASRI(fn func(string) int) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(args[0].String())
		return ToIntObject(rs), nil
	}
}

func FnASRIex(fn func(string) int) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()

		if len(args) < 1 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(args[0].String())
		return ToIntObject(rs), nil
	}
}

// like tk.FromBase64
func FnASRA(fn func(string) interface{}) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return Undefined, NewCommonError("not enough parameters")
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
			return Undefined, NewCommonError("not enough parameters")
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
			return Undefined, NewCommonError("not enough parameters")
		}

		rs, errT := fn(args[0].String(), args[1].String())

		if errT != nil {
			return NewCommonError(errT.Error()), nil
		}

		return ToStringObject(rs), nil
	}
}

// like tk.CreateTempDir
func FnASSVsRSE(fn func(string, string, ...string) (string, error)) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 2 {
			return Undefined, NewCommonError("not enough parameters")
		}

		vs := ObjectsToS(args[2:])

		rs, errT := fn(args[0].String(), args[1].String(), vs...)

		if errT != nil {
			return NewCommonError("%v", errT.Error()), nil
		}

		return String{Value: rs}, nil
	}
}

func FnASSVsRSEex(fn func(string, string, ...string) (string, error)) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()

		if len(args) < 2 {
			return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
		}

		vs := ObjectsToS(args[2:])

		rs, errT := fn(args[0].String(), args[1].String(), vs...)

		if errT != nil {
			return NewCommonErrorWithPos(c, "%v", errT.Error()), nil
		}

		return String{Value: rs}, nil
	}
}

// like kanjikana.ConvertKanjiToRomaji
func FnASRSE(fn func(string) (string, error)) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return Undefined, NewCommonError("not enough parameters")
		}

		rs, errT := fn(args[0].String())

		if errT != nil {
			return NewCommonError(errT.Error()), nil
		}

		return ToStringObject(rs), nil
	}
}

func FnASRSEex(fn func(string) (string, error)) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()

		if len(args) < 1 {
			return Undefined, NewCommonError("not enough parameters")
		}

		rs, errT := fn(args[0].String())

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

// like tk.SetVar
func FnASAR(fn func(string, interface{})) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 2 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		fn(args[0].String(), ConvertFromObject(args[1]))
		return Undefined, nil
	}
}

func FnASARex(fn func(string, interface{})) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()

		if len(args) < 2 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		fn(args[0].String(), ConvertFromObject(args[1]))
		return Undefined, nil
	}
}

// like tk.DeleteVar
func FnASR(fn func(string)) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		fn(args[0].String())
		return Undefined, nil
	}
}

func FnASRex(fn func(string)) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()

		if len(args) < 1 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		fn(args[0].String())
		return Undefined, nil
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

// like tk.Float64ToStr
func FnAFRS(fn func(float64) string) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return Undefined, NewCommonError("not enough parameters")
		}

		rs := fn(ToFloatQuick(args[0]))

		return ToStringObject(rs), nil
	}
}

func FnAFRSex(fn func(float64) string) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()

		if len(args) < 1 {
			return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
		}

		rs := fn(ToFloatQuick(args[0]))

		return ToStringObject(rs), nil
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

// like tk.RegFindAllIndexX
func FnASSRA2N(fn func(string, string) [][]int) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 2 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(args[0].String(), args[1].String())
		return ConvertToObject(rs), nil
	}
}

func FnASSRA2Nex(fn func(string, string) [][]int) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()

		if len(args) < 2 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(args[0].String(), args[1].String())
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
			intsT = append(intsT, ToIntQuick(args[i]))
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
			intsT = append(intsT, ToIntQuick(args[i]))
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

// like tk.GetJSONNodeStrings
func FnASSRA(fn func(string, string) interface{}) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 2 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(args[0].String(), args[1].String())
		return ConvertToObject(rs), nil
	}
}

func FnASSRAex(fn func(string, string) interface{}) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()
		
		if len(args) < 2 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(args[0].String(), args[1].String())
		return ConvertToObject(rs), nil
	}
}

// like tk.EncryptStreamByTXDEF
func FnARSWRE(fn func(io.Reader, string, io.Writer) error) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 3 {
			return Undefined, NewCommonError("not enough parameters")
		}

		nv1, ok := args[0].(*Reader)

		if !ok {
			// bufT, errT := io.ReadAll(nv1.Value)
			return NewCommonError("invalid type of parameter 1: %v", nv1.TypeName()), nil
		}

		nv2 := args[1].String()

		nv3, ok := args[0].(*Writer)

		if !ok {
			// bufT, errT := io.ReadAll(nv1.Value)
			return NewCommonError("invalid type of parameter 3: %v", nv1.TypeName()), nil
		}

		rs := fn(nv1.Value, nv2, nv3.Value)

		return ConvertToObject(rs), nil
	}
}

func FnARSWREex(fn func(io.Reader, string, io.Writer) error) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()

		if len(args) < 3 {
			return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
		}

		nv1, ok := args[0].(*Reader)

		if !ok {
			// bufT, errT := io.ReadAll(nv1.Value)
			return NewCommonErrorWithPos(c, "invalid type of parameter 1: %v", nv1.TypeName()), nil
		}

		nv2 := args[1].String()

		nv3, ok := args[0].(*Writer)

		if !ok {
			// bufT, errT := io.ReadAll(nv1.Value)
			return NewCommonErrorWithPos(c, "invalid type of parameter 3: %v", nv1.TypeName()), nil
		}

		rs := fn(nv1.Value, nv2, nv3.Value)

		return ConvertToObject(rs), nil
	}
}

// like tk.CalTextSimilarity
func FnASSRF(fn func(string, string) float64) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 2 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(args[0].String(), args[1].String())
		return Float(rs), nil
	}
}

func FnASSRFex(fn func(string, string) float64) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()

		if len(args) < 2 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(args[0].String(), args[1].String())
		return Float(rs), nil
	}
}

// like tk.GetTextSimilarity
func FnASSVIRF(fn func(string, string, ...int) float64) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 2 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		vargs := ObjectsToN(args[2:])

		rs := fn(args[0].String(), args[1].String(), vargs...)
		return Float(rs), nil
	}
}

func FnASSVIRFex(fn func(string, string, ...int) float64) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()

		if len(args) < 2 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		vargs := ObjectsToN(args[2:])

		rs := fn(args[0].String(), args[1].String(), vargs...)
		return Float(rs), nil
	}
}

// like strings.Repeat
func FnASIRS(fn func(string, int) string) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 2 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(args[0].String(), ToIntQuick(args[1]))
		return ToStringObject(rs), nil
	}
}

func FnASIRSex(fn func(string, int) string) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()

		if len(args) < 2 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(args[0].String(), ToIntQuick(args[1]))
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

// like tk.FindSubStringAll
func FnASSRA2I(fn func(string, string) [][]int) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 2 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(args[0].String(), args[1].String())
		return ConvertToObject(rs), nil
	}
}

func FnASSRA2Iex(fn func(string, string) [][]int) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()

		if len(args) < 2 {
			return Undefined, ErrWrongNumArguments.NewError("not enough parameters")
		}

		rs := fn(args[0].String(), args[1].String())
		return ConvertToObject(rs), nil
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

// like tk.GetSystemInfo
func FnAVsRA(fn func(...string) interface{}) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		rs := fn(ObjectsToS(args)...)
		return ConvertToObject(rs), nil
	}
}

func FnAVsRAex(fn func(...string) interface{}) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()

		rs := fn(ObjectsToS(args)...)
		return ConvertToObject(rs), nil
	}
}

// like tk.StartSocksServer
func FnAVsRE(fn func(...string) error) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		rs := fn(ObjectsToS(args)...)
		return ConvertToObject(rs), nil
	}
}

func FnAVsREex(fn func(...string) error) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()

		rs := fn(ObjectsToS(args)...)
		return ConvertToObject(rs), nil
	}
}

// like tk.GetSeq
func FnARI(fn func() int) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		rs := fn()
		return ToIntObject(rs), nil
	}
}

func FnARIex(fn func() int) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		rs := fn()
		return ToIntObject(rs), nil
	}
}

// like tk.GetChar
func FnARA(fn func() interface{}) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		rs := fn()
		return ToIntObject(rs), nil
	}
}

func FnARAex(fn func() interface{}) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		rs := fn()
		return ConvertToObject(rs), nil
	}
}

// like tk.SystemCmd
func FnASVsRS(fn func(string, ...string) string) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return Undefined, NewCommonError("not enough parameters")
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
			return Undefined, NewCommonError("not enough parameters")
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
			return Undefined, NewCommonError("unsupported type: %T", args[0])
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
			return Undefined, NewCommonError("not enough parameters")
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

// like tk.EncryptDataByTXDEE
func FnALySRLy(fn func([]byte, string) []byte) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 2 {
			return NewCommonError("not enough parameters"), nil
		}

		nv1, ok := args[0].(Bytes)

		if !ok {
			return Undefined, NewCommonError("unsupported type: %T", args[0])
		}

		rs := fn([]byte(nv1), args[1].String())

		return Bytes(rs), nil
	}
}

func FnALySRLyex(fn func([]byte, string) []byte) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()
		
		if len(args) < 2 {
			return NewCommonError("not enough parameters"), nil
		}

		nv1, ok := args[0].(Bytes)

		if !ok {
			return Undefined, NewCommonError("unsupported type: %T", args[0])
		}

		rs := fn([]byte(nv1), args[1].String())

		return Bytes(rs), nil
	}
}

// like tk.LoadImageFromBytes
func FnALyVsRA(fn func([]byte, ...string) interface{}) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return Undefined, NewCommonError("not enough parameters")
		}

		nv1, ok := args[0].(Bytes)

		if !ok {
			return Undefined, NewCommonError("unsupported type: %T", args[0])
		}

		vargs := ObjectsToS(args[1:])

		rs := fn([]byte(nv1), vargs...)

		return ConvertToObject(rs), nil
	}
}

func FnALyVsRAex(fn func([]byte, ...string) interface{}) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()
		if len(args) < 1 {
			return Undefined, NewCommonError("not enough parameters")
		}

		nv1, ok := args[0].(Bytes)

		if !ok {
			return Undefined, NewCommonError("unsupported type: %T", args[0])
		}

		vargs := ObjectsToS(args[1:])

		rs := fn([]byte(nv1), vargs...)

		return ConvertToObject(rs), nil
	}
}

// like tk.GetFileContentInArchiveBytes
func FnALySVsRA(fn func([]byte, string, ...string) interface{}) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 2 {
			return Undefined, NewCommonError("not enough parameters")
		}

		nv1, ok := args[0].(Bytes)

		if !ok {
			return Undefined, NewCommonError("unsupported type: %T", args[0])
		}

		vargs := ObjectsToS(args[2:])

		rs := fn([]byte(nv1), args[1].String(), vargs...)

		return ConvertToObject(rs), nil
	}
}

func FnALySVsRAex(fn func([]byte, string, ...string) interface{}) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()
		
		if len(args) < 2 {
			return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
		}

		nv1, ok := args[0].(Bytes)

		if !ok {
			return Undefined, NewCommonErrorWithPos(c, "unsupported type: %T", args[0])
		}

		vargs := ObjectsToS(args[2:])

		rs := fn([]byte(nv1), args[1].String(), vargs...)

		return ConvertToObject(rs), nil
	}
}

// like tk.BytesStartsWith
func FnALyARB(fn func([]byte, interface{}) bool) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 2 {
			return Undefined, NewCommonError("not enough parameters")
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
			return Undefined, NewCommonError("not enough parameters")
		}

		nv1, ok := args[0].(Bytes)

		if !ok {
			return NewCommonError("unsupported type: %T", args[0]), nil
		}

		rs := fn([]byte(nv1), ConvertFromObject(args[1]))

		return Bool(rs), nil
	}
}

// like tk.BytesIndex
func FnALyARI(fn func([]byte, interface{}) int) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 2 {
			return Undefined, NewCommonError("not enough parameters")
		}

		nv1, ok := args[0].(Bytes)

		if !ok {
			return NewCommonError("unsupported type: %T", args[0]), nil
		}

		rs := fn([]byte(nv1), ConvertFromObject(args[1]))

		return Int(rs), nil
	}
}

func FnALyARIex(fn func([]byte, interface{}) int) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()
		if len(args) < 2 {
			return Undefined, NewCommonError("not enough parameters")
		}

		nv1, ok := args[0].(Bytes)

		if !ok {
			return NewCommonError("unsupported type: %T", args[0]), nil
		}

		rs := fn([]byte(nv1), ConvertFromObject(args[1]))

		return Int(rs), nil
	}
}

// like tk.IsDataEncryptedByTXDEF
func FnALyRB(fn func([]byte) bool) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return Undefined, NewCommonError("not enough parameters")
		}

		nv1, ok := args[0].(Bytes)

		if !ok {
			return NewCommonError("unsupported type: %T", args[0]), nil
		}

		rs := fn([]byte(nv1))

		return Bool(rs), nil
	}
}

func FnALyRBex(fn func([]byte) bool) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()
		if len(args) < 1 {
			return Undefined, NewCommonError("not enough parameters")
		}

		nv1, ok := args[0].(Bytes)

		if !ok {
			return NewCommonError("unsupported type: %T", args[0]), nil
		}

		rs := fn([]byte(nv1))

		return Bool(rs), nil
	}
}

// like tk.ToPinyin
func FnASVsRA(fn func(string, ...string) interface{}) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return Undefined, NewCommonError("not enough parameters")
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

// like tk.LoadBytesInArchive
func FnASSVsRA(fn func(string, string, ...string) interface{}) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 2 {
			return Undefined, NewCommonError("not enough parameters")
		}

		vargs := ObjectsToS(args[2:])

		rs := fn(args[0].String(), args[1].String(), vargs...)

		return ConvertToObject(rs), nil
	}
}

func FnASSVsRAex(fn func(string, string, ...string) interface{}) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()
		
		if len(args) < 2 {
			return Undefined, NewCommonError("not enough parameters")
		}

		vargs := ObjectsToS(args[2:])

		rs := fn(args[0].String(), args[1].String(), vargs...)

		return ConvertToObject(rs), nil
	}
}

// like tk.ExtractFileInArchive
func FnASSSVsRA(fn func(string, string, string, ...string) interface{}) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 3 {
			return Undefined, NewCommonError("not enough parameters")
		}

		vargs := ObjectsToS(args[3:])

		rs := fn(args[0].String(), args[1].String(), args[2].String(), vargs...)

		return ConvertToObject(rs), nil
	}
}

func FnASSSVsRAex(fn func(string, string, string, ...string) interface{}) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()
		
		if len(args) < 3 {
			return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
		}

		vargs := ObjectsToS(args[3:])

		rs := fn(args[0].String(), args[1].String(), args[2].String(), vargs...)

		return ConvertToObject(rs), nil
	}
}

// like tk.RenameFile
func FnASSVsRE(fn func(string, string, ...string) error) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 2 {
			return Undefined, NewCommonError("not enough parameters")
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
			return Undefined, NewCommonError("not enough parameters")
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
			return Undefined, NewCommonError("not enough parameters")
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
			return Undefined, NewCommonError("not enough parameters")
		}

		vargs := ObjectsToS(args[1:])
		rs := fn(args[0].String(), vargs...)
		return Bool(rs), nil
	}
}

// like tk.ValidateOtpCode
func FnASSVsRB(fn func(string, string, ...string) bool) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 2 {
			return Undefined, NewCommonError("not enough parameters")
		}

		vargs := ObjectsToS(args[2:])
		rs := fn(args[0].String(), args[1].String(), vargs...)
		return Bool(rs), nil
	}
}

func FnASSVsRBex(fn func(string, string, ...string) bool) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()
		if len(args) < 2 {
			return Undefined, NewCommonError("not enough parameters")
		}

		vargs := ObjectsToS(args[2:])
		rs := fn(args[0].String(), args[1].String(), vargs...)
		return Bool(rs), nil
	}
}

// like tk.GenerateToken
func FnASSSVsRS(fn func(string, string, string, ...string) string) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 3 {
			return Undefined, NewCommonError("not enough parameters")
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
			return Undefined, NewCommonError("not enough parameters")
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
			return Undefined, NewCommonError("not enough parameters")
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
			return Undefined, NewCommonError("not enough parameters")
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
			return Undefined, NewCommonError("not enough parameters")
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
			return Undefined, NewCommonError("not enough parameters")
		}

		vargs := ObjectsToI(args[1:])

		rs := fn(args[0].String(), vargs...)
		return ToStringObject(rs), nil
	}
}

// like fmt.Sprintln
func FnAVaRS(fn func(...interface{}) string) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		vargs := ObjectsToI(args[0:])

		rs := fn(vargs...)
		return ToStringObject(rs), nil
	}
}

func FnAVaRSex(fn func(...interface{}) string) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()

		vargs := ObjectsToI(args[0:])

		rs := fn(vargs...)
		return ToStringObject(rs), nil
	}
}

// like tk.Errf
func FnASVaRE(fn func(string, ...interface{}) error) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return Undefined, NewCommonError("not enough parameters")
		}

		vargs := ObjectsToI(args[1:])

		rs := fn(args[0].String(), vargs...)
		return &Error{Name: "error", Message: rs.Error()}, nil
	}
}

func FnASVaREex(fn func(string, ...interface{}) error) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()

		if len(args) < 1 {
			return Undefined, NewCommonError("not enough parameters")
		}

		vargs := ObjectsToI(args[1:])

		rs := fn(args[0].String(), vargs...)
		return &Error{Name: "error", Message: rs.Error()}, nil
	}
}

// like tk.ErrStrf
func FnAAVaRS(fn func(interface{}, ...interface{}) string) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return Undefined, NewCommonError("not enough parameters")
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
			return Undefined, NewCommonError("not enough parameters")
		}

		vargs := ObjectsToI(args[1:])

		rs := fn(ConvertFromObject(args[0]), vargs...)
		return ToStringObject(rs), nil
	}
}

// like tk.Compress
func FnAAVaRA(fn func(interface{}, ...interface{}) interface{}) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return Undefined, NewCommonError("not enough parameters")
		}

		vargs := ObjectsToI(args[1:])

		rs := fn(ConvertFromObject(args[0]), vargs...)
		return ConvertToObject(rs), nil
	}
}

func FnAAVaRAex(fn func(interface{}, ...interface{}) interface{}) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()

		if len(args) < 1 {
			return Undefined, NewCommonError("not enough parameters")
		}

		vargs := ObjectsToI(args[1:])

		rs := fn(ConvertFromObject(args[0]), vargs...)
		return ConvertToObject(rs), nil
	}
}

// like tk.LoadBytesFromFile
func FnASViRA(fn func(string, ...int) interface{}) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return Undefined, NewCommonError("not enough parameters")
		}

		vargs := ObjectsToN(args[1:])
		rs := fn(args[0].String(), vargs...)
		return ConvertToObject(rs), nil
	}
}

func FnASViRAex(fn func(string, ...int) interface{}) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		if c.Len() < 1 {
			return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
		}
		vargs := toArgsN(1, c)
		rs := fn(c.Get(0).String(), vargs...)
		return ConvertToObject(rs), nil
	}
}

// like tk.StrToIntWithDefaultValue
func FnASViRI(fn func(string, ...int) int) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return Undefined, NewCommonError("not enough parameters")
		}

		vargs := ObjectsToN(args[1:])
		rs := fn(args[0].String(), vargs...)
		return ToIntObject(rs), nil
	}
}

func FnASViRIex(fn func(string, ...int) int) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		if c.Len() < 1 {
			return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
		}
		vargs := toArgsN(1, c)
		rs := fn(c.Get(0).String(), vargs...)
		return ToIntObject(rs), nil
	}
}

// like tk.SaveBytesToFileE(func(bytesA []byte, fileA string) error)
func FnALbySRE(fn func([]byte, string) error) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 2 {
			return Undefined, NewCommonError("not enough parameters")
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
			return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
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
			return Undefined, NewCommonError("not enough parameters")
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
			return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
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
			return Undefined, NewCommonError("not enough parameters")
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
			return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
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
			return Undefined, NewCommonError("not enough parameters")
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
			return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
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
			return Undefined, NewCommonError("not enough parameters")
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
			return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
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
			return Undefined, NewCommonError("not enough parameters")
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
			return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
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
			return Undefined, NewCommonError("not enough parameters")
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
			return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
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
			return Undefined, NewCommonError("not enough parameters")
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
			return Undefined, NewCommonError("not enough parameters")
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

// like tk.Min
func FnAVaRA(fn func(...interface{}) interface{}) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		vargs := ObjectsToI(args)
		rs := fn(vargs...)
		return ConvertToObject(rs), nil
	}
}

func FnAVaRAex(fn func(...interface{}) interface{}) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()

		vargs := ObjectsToI(args)
		rs := fn(vargs...)
		return ConvertToObject(rs), nil
	}
}

// like tk.Ceil
func FnAARA(fn func(interface{}) interface{}) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return Undefined, NewCommonError("not enough parameters")
		}

		rs := fn(ConvertFromObject(args[0]))

		return ConvertToObject(rs), nil
	}
}

func FnAARAex(fn func(interface{}) interface{}) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()

		if len(args) < 1 {
			return Undefined, NewCommonError("not enough parameters")
		}

		rs := fn(ConvertFromObject(args[0]))

		return ConvertToObject(rs), nil
	}
}

// like fmt.Print
func FnAVaRIE(fn func(...interface{}) (int, error)) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		vargs := ObjectsToI(args)
		n, errT := fn(vargs...)

		if errT != nil {
			return NewCommonError("%v", errT), nil
		}

		return Int(n), nil
	}
}

func FnAVaRIEex(fn func(...interface{}) (int, error)) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()

		vargs := ObjectsToI(args)

		n, errT := fn(vargs...)

		if errT != nil {
			return NewCommonError("%v", errT), nil
		}

		return Int(n), nil
	}
}

// like tk.PlErrX
func FnAAR(fn func(interface{})) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return Undefined, NewCommonError("not enough parameters")
		}

		fn(ConvertFromObject(args[0]))

		return nil, nil
	}
}

func FnAARex(fn func(interface{})) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		if c.Len() < 1 {
			return Undefined, NewCommonError("not enough parameters")
		}

		fn(ConvertFromObject(c.Get(0)))

		return nil, nil
	}
}

// like tk.Pln
func FnASVaR(fn func(string, ...interface{})) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 1 {
			return Undefined, NewCommonError("not enough parameters")
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
			return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
		}

		vargs := toArgsA(1, c)

		fn(args[0].String(), vargs...)

		return nil, nil
	}
}

// like fmt.Printf
func FnAWSVaRIE(fn func(io.Writer, string, ...interface{}) (int, error)) CallableFunc {
	return func(args ...Object) (ret Object, err error) {
		if len(args) < 2 {
			return Undefined, NewCommonError("not enough parameters")
		}
		// fmt.Fprintf()

		nv1, ok := args[0].(*Writer)

		if !ok {
			return NewCommonError("unsupported type: %v", args[0].TypeName()), nil
		}

		vargs := ObjectsToI(args[2:])

		n, errT := fn(nv1.Value, args[1].String(), vargs...)

		if errT != nil {
			return NewCommonError("failed to fprintf: %v", errT), nil
		}

		return Int(n), nil
	}
}

func FnAWSVaRIEex(fn func(io.Writer, string, ...interface{}) (int, error)) CallableExFunc {
	return func(c Call) (ret Object, err error) {
		args := c.GetArgs()

		if len(args) < 2 {
			return Undefined, NewCommonError("not enough parameters")
		}
		// fmt.Fprintf()

		nv1, ok := args[0].(*Writer)

		if !ok {
			return NewCommonError("unsupported type: %v", args[0].TypeName()), nil
		}

		vargs := ObjectsToI(args[2:])

		n, errT := fn(nv1.Value, args[1].String(), vargs...)

		if errT != nil {
			return NewCommonError("failed to fprintf: %v", errT), nil
		}

		return Int(n), nil
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
		return Undefined, fmt.Errorf("not enough parameters")
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
		return Undefined, fmt.Errorf("not enough parameters")
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
		return Undefined, fmt.Errorf("not enough parameters")
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
		return Undefined, fmt.Errorf("not enough parameters")
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
		return Undefined, fmt.Errorf("not enough parameters")
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
		return Undefined, fmt.Errorf("not enough parameters")
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

	vs := ObjectsToS(args[1:])

	switch nv := args[0].(type) {
	case String:
		return String{Value: base64.StdEncoding.EncodeToString([]byte(nv.Value))}, nil
	case Bytes:
		return String{Value: base64.StdEncoding.EncodeToString([]byte(nv))}, nil
	case *StringBuilder:
		return String{Value: base64.StdEncoding.EncodeToString([]byte(nv.String()))}, nil
	case *BytesBuffer:
		return String{Value: base64.StdEncoding.EncodeToString([]byte(nv.Value.Bytes()))}, nil
	case *Image:
		formatT := strings.TrimSpace(tk.GetSwitch(vs, "-format=", ".png"))
		bytesT := tk.SaveImageToBytes(nv.Value, formatT)

		tmps := base64.StdEncoding.EncodeToString(bytesT)

		if tk.IfSwitchExists(vs, "-addHead") {
			tmps = "data:image/" + strings.Trim(formatT, ".") + ";base64," + tmps
		}

		return String{Value: tmps}, nil
	default:
		return String{Value: base64.StdEncoding.EncodeToString([]byte(nv.String()))}, nil
	}
}

func builtinBase64EncodeByRawUrlFunc(c Call) (Object, error) {
	args := c.GetArgs()

	lenT := len(args)

	if lenT < 1 {
		return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
	}

	vs := ObjectsToS(args[1:])

	switch nv := args[0].(type) {
	case String:
		return String{Value: base64.RawURLEncoding.EncodeToString([]byte(nv.Value))}, nil
	case Bytes:
		return String{Value: base64.RawURLEncoding.EncodeToString([]byte(nv))}, nil
	case *StringBuilder:
		return String{Value: base64.RawURLEncoding.EncodeToString([]byte(nv.String()))}, nil
	case *BytesBuffer:
		return String{Value: base64.RawURLEncoding.EncodeToString([]byte(nv.Value.Bytes()))}, nil
	case *Image:
		formatT := strings.TrimSpace(tk.GetSwitch(vs, "-format=", ".png"))
		bytesT := tk.SaveImageToBytes(nv.Value, formatT)

		tmps := base64.RawURLEncoding.EncodeToString(bytesT)

		if tk.IfSwitchExists(vs, "-addHead") {
			tmps = "data:image/" + strings.Trim(formatT, ".") + ";base64," + tmps
		}

		return String{Value: tmps}, nil
	default:
		return String{Value: base64.RawURLEncoding.EncodeToString([]byte(nv.String()))}, nil
	}
}

func builtinBase64DecodeByRawUrlFunc(c Call) (Object, error) {
	args := c.GetArgs()

	lenT := len(args)

	if lenT < 1 {
		return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
	}

	bufT, errT := base64.RawURLEncoding.DecodeString(args[0].String())

	if errT != nil {
		return NewCommonErrorWithPos(c, "failed to decode base64-raw-url: %v", errT), nil
	}

	return Bytes(bufT), nil
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

func builtinFromXmlFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
	}

	var xmlTextT string

	objT := args[0]

	switch nv := objT.(type) {
	case String:
		xmlTextT = nv.Value
	case Bytes:
		xmlTextT = string(nv)
	case Chars:
		xmlTextT = string(nv)
	default:
		xmlTextT = tk.ToStr(ConvertFromObject(args[0]))

	}

	jObjT := tk.FromXMLX(xmlTextT, ObjectsToI(args[1:])...)

	if tk.IsError(jObjT) {
		return NewCommonError("%v", jObjT), nil
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
		}

		return False, nil
	}

	return False, nil
}

func isErrX(objA Object) bool {
	switch objA.TypeCode() {
	case 153: // *Error
		nv1 := objA.(*Error)
		if nv1 != nil {
			return true
		}
	case 155: // *RuntimeError
		nv1 := objA.(*RuntimeError)

		if nv1 != nil {
			return true
		}

	case 105: //String
		if strings.HasPrefix(objA.String(), "TXERROR:") {
			return true
		}
	case 106: // *MutableString
		if strings.HasPrefix(objA.String(), "TXERROR:") {
			return true
		}
	case 999: // *Any
		nv := objA.(*Any)

		nv1, ok := nv.Value.(error)

		if ok {
			if nv1 != nil {
				return true
			}
		}

		s1, ok := nv.Value.(string)

		if ok {
			if strings.HasPrefix(s1, "TXERROR:") {
				return true
			}
		}

		return false
	}

	return false
}

func builtinGetNowTimeStampFunc(c Call) (Object, error) {
	return String{Value: tk.GetTimeStampMid(time.Now())}, nil
}

func builtinTimeBeforeFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
	}

	v1, ok := args[0].(*Time)
	
	if !ok {
		return Undefined, NewCommonErrorWithPos(c, "unsupported type of param1: %T", args[0])
	}

	v2, ok := args[1].(*Time)
	
	if !ok {
		return Undefined, NewCommonErrorWithPos(c, "unsupported type of param2: %T", args[1])
	}

	return Bool(v1.Value.Before(v2.Value)), nil
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

	nv1, ok := args[0].(Int)

	if ok {
		tk.LockN(int(nv1))
		return Undefined, nil
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

	nv1, ok := args[0].(Int)

	if ok {
		tk.UnlockN(int(nv1))
		return Undefined, nil
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

func builtinToTimeFunc(c Call) (Object, error) {
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
			return NewCommonErrorWithPos(c, "failed to convert time: %v", rsT), nil
		}
		return &Time{Value: rsT.(time.Time)}, nil
	case *MutableString:
		rsT := tk.ToTime(obj.Value, ObjectsToI(args[1:])...)

		if tk.IsError(rsT) {
			return NewCommonErrorWithPos(c, "failed to convert time: %v", rsT), nil
		}
		return &Time{Value: rsT.(time.Time)}, nil
	default:
		return NewCommonErrorWithPos(c, "failed to convert time"), nil
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

	rsT := tk.ReflectCallFuncQuick(fn1, ObjectsToI(args[1:])...)

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

	// tk.Plv(objT, name1, paramsT)

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
	case *Stack:
		return &Any{Value: obj.Value, OriginalType: fmt.Sprintf("%T", obj.Value), OriginalCode: obj.TypeCode()}, nil
	case *Queue:
		return &Any{Value: obj.Value, OriginalType: fmt.Sprintf("%T", obj.Value), OriginalCode: obj.TypeCode()}, nil
	case *MapArray:
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
	// bufT.WriteString(fmt.Sprintf("codeG: %v", tk.LimitString(codeG, 50)))
	// bufT.WriteString("\n ----- \n")

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
		rsT := tk.ToInt(ConvertFromObject(args[0]), ToIntQuick(args[1]))

		return Int(rsT), nil
	}

	rsT := tk.ToInt(ConvertFromObject(args[0]), 0)

	return Int(rsT), nil
}

func builtinToBoolFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return Bool(false), nil
	}

	rsT := tk.ToBool(ConvertFromObject(args[0]))

	return Bool(rsT), nil
}

func builtinToBoolWithDefaultFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return Bool(false), nil
	}

	nv2, ok := args[1].(Bool)

	if !ok {
		nv2 = Bool(false)
	}

	if len(args) > 1 {
		rsT := tk.ToBoolWithDefaultValue(ConvertFromObject(args[0]), bool(nv2))

		return Bool(rsT), nil
	}

	rsT := tk.ToBoolWithDefaultValue(ConvertFromObject(args[0]))

	return Bool(rsT), nil
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
		vs := ObjectsToS(args[1:])

		formatT := tk.GetSwitch(vs, "-format=", "Error: %v\n")

		fmt.Printf(formatT, tk.GetErrStrX(objT))
		os.Exit(0)
	}

	return args[0], nil
}

func builtinCheckEmptyFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return nil, fmt.Errorf("not enough parameters")
	}

	if IsUndefInternal(args[0]) {
		vs := ObjectsToS(args[1:])

		formatT := tk.GetSwitch(vs, "-format=", "Empty\n")

		fmt.Printf(formatT)
		os.Exit(0)
	}

	strT := args[0].String()

	if strT == "" {
		vs := ObjectsToS(args[1:])

		formatT := tk.GetSwitch(vs, "-format=", "Empty\n\n")

		fmt.Printf(formatT)
		os.Exit(0)
	}

	return args[0], nil
}

func builtinErrToEmptyFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
	}

	switch nv := args[0].(type) {
	case *Error, *RuntimeError:
		return String{Value: ""}, nil
	case String:
		if strings.HasPrefix(nv.Value, "TXERROR:") {
			return String{Value: ""}, nil
		}
	case *MutableString:
		if strings.HasPrefix(nv.Value, "TXERROR:") {
			return String{Value: ""}, nil
		}
	case *Any:
		_, ok := nv.Value.(error)

		if ok {
			return String{Value: ""}, nil
		}

		s1, ok := nv.Value.(string)

		if ok {
			if strings.HasPrefix(s1, "TXERROR:") {
				return String{Value: ""}, nil
			}
		}

		return args[0], nil
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

// won't convert undefined value to empty string
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

func builtinGetFileInfoFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	fileT, ok := args[0].(*File)

	filePathT := ""

	if ok {
		filePathMemberT := fileT.GetMember("Path")
		filePathT = strings.TrimSpace(filePathMemberT.String())

		if IsUndefInternal(filePathMemberT) || filePathT == "" {
			fi, errT := fileT.Value.Stat()
			if errT != nil && !os.IsExist(errT) {
				return NewCommonErrorWithPos(c, "%v", errT), nil
			}

			fileNameT := fi.Name()
			absPathT := ""

			mapT := Map{"Path": ToStringObject(filePathT), "Abs": ToStringObject(absPathT), "Name": ToStringObject(fileNameT), "Ext": ToStringObject(filepath.Ext(fileNameT)), "Size": ToStringObject(tk.Int64ToStr(fi.Size())), "IsDir": ToStringObject(tk.BoolToStr(fi.IsDir())), "Time": ToStringObject(tk.FormatTime(fi.ModTime(), tk.TimeFormatCompact)), "Mode": ToStringObject(fmt.Sprintf("%v", fi.Mode()))}

			return mapT, nil
		}
	} else {
		filePathT = args[0].String()
	}

	fi, errT := os.Stat(filePathT)
	if errT != nil && !os.IsExist(errT) {
		return NewCommonErrorWithPos(c, "%v", errT), nil
	}

	absPathT, errT := filepath.Abs(filePathT)
	if errT != nil {
		return NewCommonErrorWithPos(c, "%v", errT), nil
	}

	fileNameT := filepath.Base(filePathT)

	mapT := Map{"Path": ToStringObject(filePathT), "Abs": ToStringObject(absPathT), "Name": ToStringObject(fileNameT), "Ext": ToStringObject(filepath.Ext(fileNameT)), "Size": ToStringObject(tk.Int64ToStr(fi.Size())), "IsDir": ToStringObject(tk.BoolToStr(fi.IsDir())), "Time": ToStringObject(tk.FormatTime(fi.ModTime(), tk.TimeFormatCompact)), "Mode": ToStringObject(fmt.Sprintf("%v", fi.Mode()))}

	return mapT, nil
}

func builtinRemovePathFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	filePathT := args[0].String()

	if !tk.IfFileExists(filePathT) {
		return NewCommonErrorWithPos(c, "dir/file not exists"), nil
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

func builtinSetStdinFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	readerT, ok := args[0].(*File)

	if !ok {
		return NewCommonErrorWithPos(c, "unsupported parameter type: %v", args[0].TypeName()), nil
	}

	os.Stdin = readerT.Value

	return Undefined, nil
}

func builtinGetMultiLineInputFunc(c Call) (Object, error) {
	args := c.GetArgs()

	var funcT func(...interface{}) interface{} = nil
	
	vs := []string{}
	
	var ok bool = false
	
	var nv1 *Delegate
	
	for _, v := range args {
		if !ok {
			nv1, ok = v.(*Delegate)
			
			if ok {
//				fmt.Printf("nv1 type: %T, %#v\n", nv1.Value, nv1.Value)
				funcT = nv1.Value
			} else {
				vs = append(vs, v.String())
			}
		} else {
			vs = append(vs, v.String())
		}
	}

//	if len(args) < 1 {
//	} else {
//		nv1, ok := args[0].(*Delegate)
//		
//		if !ok {
//			return NewCommonErrorWithPos(c, "unsupported parameter type: %v", args[0].TypeName()), nil
//		}
//		
//		funcT = nv1.Value
//		
//		vs = ObjectsToS(args[1:])
//	}
//	fmt.Printf("funcT: %#v\n", funcT)
	rs := tk.GetMultiLineInput(funcT, vs...)

	return String{Value: rs}, nil
}

func builtinSetStdoutFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	writerT, ok := args[0].(*File)

	if !ok {
		return NewCommonErrorWithPos(c, "unsupported parameter type: %v", args[0].TypeName()), nil
	}

	os.Stdout = writerT.Value

	return Undefined, nil
}

func builtinSetStderrFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	writerT, ok := args[0].(*File)

	if !ok {
		return NewCommonErrorWithPos(c, "unsupported parameter type: %v", args[0].TypeName()), nil
	}

	os.Stderr = writerT.Value

	return Undefined, nil
}

func builtinGetPipeFunc(c Call) (Object, error) {

	r, w, err := os.Pipe()

	if err != nil {
		return NewCommonErrorWithPos(c, "%v", err), nil
	}

	return Array{&File{Value: r}, &File{Value: w}}, nil

	return Undefined, nil
}

func builtinGetWebBytesFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	urlT := args[0].String()

	vs := ObjectsToI(args[1:])

	vs = append(vs, "-bytes")

	rsT := tk.GetWeb(urlT, vs...)

	if tk.IsErrX(rsT) {
		return NewCommonErrorWithPos(c, "%v", rsT), nil
	}

	nv, ok := rsT.([]byte)

	if !ok {
		return NewCommonErrorWithPos(c, "unsupported return type"), nil
	}

	return Bytes(nv), nil
}

func builtinGetWebRespBodyFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	urlT := args[0].String()

	vs := ObjectsToI(args[1:])

	rsT := tk.GetWebResponseBody(urlT, vs...)

	if tk.IsErrX(rsT) {
		return NewCommonErrorWithPos(c, "%v", rsT), nil
	}

	nv, ok := rsT.(io.ReadCloser)

	if ok {
		return &Reader{Value: nv}, nil
	}

	nv2, ok := rsT.([]interface{})

	if ok {
		aryT := make(Array, 0)
		for _, v := range nv2 {
			aryT = append(aryT, ConvertToObject(v))
		}

		return aryT, nil
	}

	return NewCommonErrorWithPos(c, "unsupported return type"), nil
}

func builtinGetWebBytesWithHeadersFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return ToStringObject(tk.ErrStrf("not enough parameters")), nil
	}

	v0, ok := args[0].(String)

	if !ok {
		return ToStringObject(tk.ErrStrf("type error for arg 0")), nil
	}

	var v1 Map

	if len(args) < 2 {
		v1 = Map{}
	} else {
		v1, ok = args[1].(Map)

		if !ok {
			return ToStringObject(tk.ErrStrf("type error for arg 1")), nil
		}
	}

	var v2 Map

	if len(args) < 3 {
		v2 = Map{}
	} else {
		v2, ok = args[2].(Map)

		if !ok {
			return ToStringObject(tk.ErrStrf("type error for arg 2")), nil
		}
	}

	var optsT []Object

	if len(args) < 4 {
		optsT = []Object{}
	} else {
		optsT = args[3:]
	}

	b, m, e := tk.DownloadWebBytes(v0.String(), tk.MSI2MSS(ConvertFromObject(v1).(map[string]interface{})), tk.MSI2MSS(ConvertFromObject(v2).(map[string]interface{})), ObjectsToS(optsT)...)
	// b, m, e := tk.DownloadWebBytes(v0.String(), nil, nil)

	if e != nil {
		return ToStringObject(tk.ErrStrf(e.Error())), nil
	}

	rsT := Array{}
	rsT = append(rsT, Bytes(b))
	rsT = append(rsT, ConvertToObject(m))

	return rsT, nil
}

func builtinParseUrlFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	strT := args[0].String()

	rs, errT := url.Parse(strT)

	if errT != nil {
		return NewCommonErrorWithPos(c, "%v", errT), nil
	}

	return ConvertToObject(tk.FromJSONX(tk.ToJSONX(rs))), nil
}

func builtinParseQueryFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	strT := args[0].String()

	vs := ObjectsToS(args[1:])

	rs, errT := url.ParseQuery(strT)

	if errT != nil {
		return NewCommonErrorWithPos(c, "%v", errT), nil
	}

	if tk.IfSwitchExists(vs, "-compact") {
		mapT := make(Map)

		for k, v := range rs {
			mapT[k] = ToStringObject(v[0])
		}

		return mapT, nil
	}

	return ConvertToObject(tk.FromJSONX(tk.ToJSONX(rs))), nil
}

func builtinOpenFileFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	pathT := args[0].String()

	vs := ObjectsToS(args[1:])

	rsT := tk.OpenFile(pathT, vs...)

	if tk.IsErrX(rsT) {
		return NewCommonErrorWithPos(c, "%v", rsT), nil
	}

	objT := &File{Value: rsT.(*os.File)}

	objT.SetMember("Path", ToStringObject(pathT))

	return objT, nil
}

func builtinLoadBytesFromFileFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	fileT, ok := args[0].(*File)

	numT := 0

	if len(args) > 1 {
		numT = ToGoIntWithDefault(args[1], 0)
	}

	if ok {
		if numT < 1 {
			fileContentT, err := io.ReadAll(fileT)
			if err != nil {
				return NewCommonErrorWithPos(c, "faild to read file content: %v", err), nil
			}

			return Bytes(fileContentT), nil
		}

		bufT := make([]byte, numT)
		nn, err := fileT.Read(bufT)
		if err != nil {
			return NewCommonErrorWithPos(c, "faild to read file content: %v", err), nil
		}

		if nn != len(bufT) {
			return NewCommonErrorWithPos(c, "faild to read file content, length not match: %v/%v", nn, len(bufT)), nil
		}

		return Bytes(bufT), nil

	}

	pathT := args[0].String()

	rsT := tk.LoadBytesFromFile(pathT, numT)

	if tk.IsErrX(rsT) {
		return NewCommonErrorWithPos(c, "%v", rsT), nil
	}

	return Bytes(rsT.([]byte)), nil
}

func builtinCloseFileFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	r1 := args[0].(*File)

	errT := r1.Value.Close()

	if errT != nil {
		return NewCommonErrorWithPos(c, "failed to close file: %v", errT), nil
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

func builtinExcelOpenFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	var f *excelize.File
	var err error

	r1, ok := args[0].(*Reader)

	if ok {
		f, err = excelize.OpenReader(r1.Value)

		if err != nil {
			return NewCommonErrorWithPos(c, "failed to open file: %v", err), nil
		}

	} else {
		f, err = excelize.OpenFile(args[0].String())
		if err != nil {
			return NewCommonErrorWithPos(c, "failed to open excel file: %v", err), nil
		}
	}

	return &Excel{Value: f}, nil
}

func builtinExcelOpenFileFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	f, err := excelize.OpenFile(args[0].String())
	if err != nil {
		return NewCommonErrorWithPos(c, "failed to open excel file: %v", err), nil
	}

	return &Excel{Value: f}, nil
}

func builtinExcelSaveAsFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	var f *excelize.File

	r0, ok := args[0].(*Excel)

	if !ok {
		return NewCommonErrorWithPos(c, "invalid type: %T", args[0]), nil
	} else {
		f = r0.Value
	}

	var nameT = args[1].String()

	errT := f.SaveAs(nameT)

	if errT != nil {
		return NewCommonErrorWithPos(c, "failed to save excel file: %v", errT), nil
	}

	return Undefined, nil

}

func builtinExcelWriteToFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	var f *excelize.File

	r0, ok := args[0].(*Excel)

	if !ok {
		return NewCommonErrorWithPos(c, "invalid type: %T", args[0]), nil
	} else {
		f = r0.Value
	}

	respT, ok := args[1].(*HttpResp)

	if ok {
		cntT, errT := f.WriteTo(respT.Value)

		if errT != nil {
			return NewCommonErrorWithPos(c, "failed to write excel data: %v", errT), nil
		}

		return Int(cntT), nil
	}

	writerT, ok := args[1].(*Writer)

	if !ok {
		return NewCommonErrorWithPos(c, "not a writer type: %T", args[1]), nil
	}

	cntT, errT := f.WriteTo(writerT.Value)

	if errT != nil {
		return NewCommonErrorWithPos(c, "failed to write excel data: %v", errT), nil
	}

	return Int(cntT), nil

}

func builtinExcelCloseFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	var f *excelize.File

	r0, ok := args[0].(*Excel)

	if !ok {
		return NewCommonErrorWithPos(c, "invalid type: %T", args[0]), nil
	}

	f = r0.Value

	errT := f.Close()

	if errT != nil {
		return NewCommonErrorWithPos(c, "failed to close excel file: %v", errT), nil
	}

	return Undefined, nil

}

func builtinExcelReadSheetFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	var f *excelize.File
	var err error

	r0, ok := args[0].(*Excel)

	if !ok {
		r1, ok := args[0].(*Reader)

		if !ok {
			filePathT := args[0].String()

			f, err = excelize.OpenFile(filePathT)

			if err != nil {
				return NewCommonErrorWithPos(c, "failed to open file: %v", err), nil
			}

			defer f.Close()

		} else {
			f, err = excelize.OpenReader(r1.Value)

			if err != nil {
				return NewCommonErrorWithPos(c, "failed to open file: %v", err), nil
			}

			defer f.Close()
		}

	} else {
		f = r0.Value
	}

	var indexT Object = Int(0)

	if len(args) > 1 {
		indexT = args[1]
	}

	var nameT string

	nv1, ok := indexT.(Int)

	if ok {
		nameT = f.GetSheetName(int(nv1))
	} else {
		nameT = indexT.String()
	}

	// tk.Pl("nameT: %#v", nameT)

	rowsT, errT := f.GetRows(nameT)

	if errT != nil {
		return NewCommonErrorWithPos(c, "failed to get rows: %v", errT), nil
	}

	return ConvertToObject(rowsT), nil

}

func builtinExcelReadAllFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	var f *excelize.File
	var err error

	r0, ok := args[0].(*Excel)

	if !ok {
		r1, ok := args[0].(*Reader)

		if !ok {
			filePathT := args[0].String()

			f, err = excelize.OpenFile(filePathT)

			if err != nil {
				return NewCommonErrorWithPos(c, "failed to open file: %v", err), nil
			}

			defer f.Close()

		} else {
			f, err = excelize.OpenReader(r1.Value)

			if err != nil {
				return NewCommonErrorWithPos(c, "failed to open file: %v", err), nil
			}

			defer f.Close()
		}

	} else {
		f = r0.Value
	}
	
	sheetListT := f.GetSheetList()
	
	sheetLenT := len(sheetListT)

	aryT := make(Array, 0, sheetLenT)
	
	for _, iv := range sheetListT {
		rowsT, errT := f.GetRows(iv)

		if errT != nil {
			return NewCommonErrorWithPos(c, "failed to get rows: %v", errT), nil
		}
		
		rsT := make(Array, 0, len(rowsT))

		for _, v := range rowsT {
			lineListT := make(Array, 0, len(v))
			for _, jv := range v {
				lineListT = append(lineListT, ToStringObject(jv))
			}

			rsT = append(rsT, lineListT)
		}
		
		aryT = append(aryT, rsT)

	}

	// tk.Pl("nameT: %#v", nameT)


	return aryT, nil

}

func builtinExcelGetSheetNameFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	var f *excelize.File
	var err error

	r0, ok := args[0].(*Excel)

	if !ok {
		r1, ok := args[0].(*Reader)

		if !ok {
			filePathT := args[0].String()

			f, err = excelize.OpenFile(filePathT)

			if err != nil {
				return NewCommonErrorWithPos(c, "failed to open file: %v", err), nil
			}

			defer f.Close()

		} else {
			f, err = excelize.OpenReader(r1.Value)

			if err != nil {
				return NewCommonErrorWithPos(c, "failed to open file: %v", err), nil
			}

			defer f.Close()
		}

	} else {
		f = r0.Value
	}

	var indexT Object = Int(0)

	if len(args) > 1 {
		indexT = args[1]
	}

	return ConvertToObject(f.GetSheetName(ToGoIntWithDefault(indexT, 0))), nil

}

func builtinExcelGetSheetCountFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	var f *excelize.File
	var err error

	r0, ok := args[0].(*Excel)

	if !ok {
		r1, ok := args[0].(*Reader)

		if !ok {
			filePathT := args[0].String()

			f, err = excelize.OpenFile(filePathT)

			if err != nil {
				return NewCommonErrorWithPos(c, "failed to open file: %v", err), nil
			}

			defer f.Close()

		} else {
			f, err = excelize.OpenReader(r1.Value)

			if err != nil {
				return NewCommonErrorWithPos(c, "failed to open file: %v", err), nil
			}

			defer f.Close()
		}

	} else {
		f = r0.Value
	}

	listT := f.GetSheetList()

	return Int(len(listT)), nil

}

func builtinExcelGetSheetListFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	var f *excelize.File
	var err error

	r0, ok := args[0].(*Excel)

	if !ok {
		r1, ok := args[0].(*Reader)

		if !ok {
			filePathT := args[0].String()

			f, err = excelize.OpenFile(filePathT)

			if err != nil {
				return NewCommonErrorWithPos(c, "failed to open file: %v", err), nil
			}

			defer f.Close()

		} else {
			f, err = excelize.OpenReader(r1.Value)

			if err != nil {
				return NewCommonErrorWithPos(c, "failed to open file: %v", err), nil
			}

			defer f.Close()
		}

	} else {
		f = r0.Value
	}

	listT := f.GetSheetList()

	return ConvertToObject(listT), nil
}

func builtinExcelReadCellFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 3 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	var f *excelize.File
	var err error

	r0, ok := args[0].(*Excel)

	if !ok {
		r1, ok := args[0].(*Reader)

		if !ok {
			filePathT := args[0].String()

			f, err = excelize.OpenFile(filePathT)

			if err != nil {
				return NewCommonErrorWithPos(c, "failed to open file: %v", err), nil
			}

			defer f.Close()

		} else {
			f, err = excelize.OpenReader(r1.Value)

			if err != nil {
				return NewCommonErrorWithPos(c, "failed to open file: %v", err), nil
			}

			defer f.Close()
		}

	} else {
		f = r0.Value
	}

	var indexT Object = args[1]

	var nameT string

	nv1, ok := indexT.(Int)

	if ok {
		nameT = f.GetSheetName(int(nv1))
	} else {
		nameT = indexT.String()
	}

	posT := args[2].String()

	valueT, errT := f.GetCellValue(nameT, posT)

	if errT != nil {
		return NewCommonErrorWithPos(c, "failed to get cell value: %v", errT), nil
	}

	return ConvertToObject(valueT), nil

}

func builtinExcelWriteSheetFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 3 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	vs := ObjectsToS(args[3:])

	var f *excelize.File

	r0, ok := args[0].(*Excel)

	if !ok {
		// r1, ok := args[0].(*Reader)

		// if !ok {
		// 	filePathT := args[0].String()

		// 	f, err = excelize.OpenFile(filePathT)

		// 	if err != nil {
		// 		return NewCommonErrorWithPos(c, "failed to open file: %v", err), nil
		// 	}

		// 	defer f.Close()

		// } else {
		// 	f, err = excelize.OpenReader(r1.Value)

		// 	if err != nil {
		// 		return NewCommonErrorWithPos(c, "failed to open file: %v", err), nil
		// 	}

		// 	defer f.Close()
		// }
		return NewCommonErrorWithPos(c, "invalid type: %T", args[0]), nil

	} else {
		f = r0.Value
	}

	indexT := args[1]

	var nameT string

	nv1, ok := indexT.(Int)

	if ok {
		nameT = f.GetSheetName(int(nv1))
	} else {
		nameT = indexT.String()
	}

	// tk.Pl("nameT: %#v", nameT)

	rowsT, ok := args[2].(Array)

	if !ok {
		return NewCommonErrorWithPos(c, "invalid data type: %T", args[2]), nil
	}

	byRowT := tk.IfSwitchExists(vs, "-row")

	var rowT []interface{}

	for i, v := range rowsT {
		if !byRowT {
			rowAryT, ok := v.(Array)

			if !ok {
				rowT = []interface{}{ConvertFromObject(v)}
			} else {
				rowT = make([]interface{}, len(rowAryT))
				for j, jv := range rowAryT {
					rowT[j] = ConvertFromObject(jv)
				}
			}

		} else {
			rowT = ConvertFromObject(v).([]interface{})
		}

		// tk.Plv(nameT, fmt.Sprintf("A%v", i+1), rowT)

		// tk.Pl("%T %T", rowT, &rowT)

		errT := f.SetSheetRow(nameT, fmt.Sprintf("A%v", i+1), &rowT)

		if errT != nil {
			return NewCommonErrorWithPos(c, "failed to write cell(%v, %v): %v", nameT, fmt.Sprintf("A%v", i+1), errT), nil
		}

	}

	return Undefined, nil

}

func builtinExcelWriteCellFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 4 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	// vs := ObjectsToS(args[4:])

	var f *excelize.File

	r0, ok := args[0].(*Excel)

	if !ok {
		return NewCommonErrorWithPos(c, "invalid type: %T", args[0]), nil
	} else {
		f = r0.Value
	}

	indexT := args[1]

	var nameT string

	nv1, ok := indexT.(Int)

	if ok {
		nameT = f.GetSheetName(int(nv1))
	} else {
		nameT = indexT.String()
	}

	posT := args[2].String()

	errT := f.SetCellValue(nameT, posT, ConvertFromObject(args[3]))
	if errT != nil {
		return NewCommonErrorWithPos(c, "failed to write cell value: %v", errT), nil
	}

	return Undefined, nil

}

func builtinExcelGetColumnIndexByNameFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	rs, errT := excelize.ColumnNameToNumber(args[0].String())
	if errT != nil {
		return NewCommonErrorWithPos(c, "failed to convert: %v", errT), nil
	}

	return Int(rs), nil

}

func builtinExcelGetColumnNameByIndexFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	rs, errT := excelize.ColumnNumberToName(ToIntQuick(args[0]))
	if errT != nil {
		return NewCommonErrorWithPos(c, "failed to convert: %v", errT), nil
	}

	return String{Value: rs}, nil

}

func builtinExcelNewSheetFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	var f *excelize.File

	r0, ok := args[0].(*Excel)

	if !ok {
		return NewCommonErrorWithPos(c, "invalid type: %T", args[0]), nil
	}

	f = r0.Value

	nameT := args[1].String()

	indexT, errT := f.NewSheet(nameT)
	if errT != nil {
		return NewCommonErrorWithPos(c, "failed to new sheet: %v", errT), nil
	}

	return Int(indexT), nil

}

// writeCsv(writerA/filePathA, dataA, ...optsA)
func builtinWriteCsvFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	vs := ObjectsToS(args[2:])

	r1, ok := args[1].(*Writer)

	var writerT *csv.Writer

	if !ok {
		filePathT := args[1].String()

		rsT := tk.OpenFile(filePathT, append([]string{"-create", "-truncate"}, vs...)...)

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

	switch nv := args[0].(type) {
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

			default:
				tk.Pl("unsupported line type: %T", args[1])
				return Undefined, nil
			}
		}

		writerT.Flush()

		return Undefined, nil
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

func builtinFtpCreateDirFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return nil, fmt.Errorf("not enough parameters")
	}

	pa := ObjectsToS(args)

	var v1, v2, v3, v4, v6 string

	v1 = strings.TrimSpace(tk.GetSwitch(pa, "-host=", v1))
	v2 = strings.TrimSpace(tk.GetSwitch(pa, "-port=", v2))
	v3 = strings.TrimSpace(tk.GetSwitch(pa, "-user=", v3))
	v4 = strings.TrimSpace(tk.GetSwitch(pa, "-password=", v4))
	if strings.HasPrefix(v4, "740404") {
		v4 = strings.TrimSpace(tk.DecryptStringByTXDEF(v4))
	}
	if strings.HasPrefix(v4, "//TXDEF#") {
		v4 = strings.TrimSpace(tk.DecryptStringByTXDEF(v4))
	}

	v6 = strings.TrimSpace(tk.GetSwitch(pa, "-remotePath=", v6))

	v7 := tk.ToInt(strings.TrimSpace(tk.GetSwitch(pa, "-timeout=", "15")), 0)

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

	if v6 == "" {
		return ConvertToObject(fmt.Errorf("emtpy remotePath")), nil
	}

	clientT, err := ftp.Dial(v1+":"+v2, ftp.DialWithTimeout(time.Duration(v7)*time.Second))
	if err != nil {
		return ConvertToObject(fmt.Errorf("failed to connect ftp server: %v", err)), nil
	}

	err = clientT.Login(v3, v4)
	if err != nil {
		return ConvertToObject(fmt.Errorf("failed to login ftp server: %v", err)), nil
	}

	err = clientT.MakeDir(v6)
	if err != nil {
		return ConvertToObject(fmt.Errorf("failed to create dir: %v", err)), nil
	}
	
	err = clientT.Quit()

	if err != nil {
		return ConvertToObject(fmt.Errorf("failed to close ftp connection: %v", err)), nil
	}

	return Undefined, nil
}

func builtinFtpListFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return nil, fmt.Errorf("not enough parameters")
	}

	pa := ObjectsToS(args)

	var v1, v2, v3, v4, v6 string

	v1 = strings.TrimSpace(tk.GetSwitch(pa, "-host=", v1))
	v2 = strings.TrimSpace(tk.GetSwitch(pa, "-port=", v2))
	v3 = strings.TrimSpace(tk.GetSwitch(pa, "-user=", v3))
	v4 = strings.TrimSpace(tk.GetSwitch(pa, "-password=", v4))
	if strings.HasPrefix(v4, "740404") {
		v4 = strings.TrimSpace(tk.DecryptStringByTXDEF(v4))
	}
	if strings.HasPrefix(v4, "//TXDEF#") {
		v4 = strings.TrimSpace(tk.DecryptStringByTXDEF(v4))
	}

	v6 = strings.TrimSpace(tk.GetSwitch(pa, "-remotePath=", v6))

	v7 := tk.ToInt(strings.TrimSpace(tk.GetSwitch(pa, "-timeout=", "15")), 0)

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

	if v6 == "" {
		return ConvertToObject(fmt.Errorf("emtpy remotePath")), nil
	}

	clientT, err := ftp.Dial(v1+":"+v2, ftp.DialWithTimeout(time.Duration(v7)*time.Second))
	if err != nil {
		return ConvertToObject(fmt.Errorf("failed to connect ftp server: %v", err)), nil
	}

	err = clientT.Login(v3, v4)
	if err != nil {
		return ConvertToObject(fmt.Errorf("failed to login ftp server: %v", err)), nil
	}

	err = clientT.MakeDir(v6)
	if err != nil {
		return ConvertToObject(fmt.Errorf("failed to create dir: %v", err)), nil
	}
	
	err = clientT.Quit()

	if err != nil {
		return ConvertToObject(fmt.Errorf("failed to close ftp connection: %v", err)), nil
	}

	return Undefined, nil
}

func builtinFtpSizeFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return nil, fmt.Errorf("not enough parameters")
	}

	pa := ObjectsToS(args)

	var v1, v2, v3, v4, v6 string

	v1 = strings.TrimSpace(tk.GetSwitch(pa, "-host=", v1))
	v2 = strings.TrimSpace(tk.GetSwitch(pa, "-port=", v2))
	v3 = strings.TrimSpace(tk.GetSwitch(pa, "-user=", v3))
	v4 = strings.TrimSpace(tk.GetSwitch(pa, "-password=", v4))
	if strings.HasPrefix(v4, "740404") {
		v4 = strings.TrimSpace(tk.DecryptStringByTXDEF(v4))
	}
	if strings.HasPrefix(v4, "//TXDEF#") {
		v4 = strings.TrimSpace(tk.DecryptStringByTXDEF(v4))
	}

	v6 = strings.TrimSpace(tk.GetSwitch(pa, "-remotePath=", v6))

	v7 := tk.ToInt(strings.TrimSpace(tk.GetSwitch(pa, "-timeout=", "15")), 0)

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

	if v6 == "" {
		return ConvertToObject(fmt.Errorf("emtpy remotePath")), nil
	}

	clientT, err := ftp.Dial(v1+":"+v2, ftp.DialWithTimeout(time.Duration(v7)*time.Second))
	if err != nil {
		return ConvertToObject(fmt.Errorf("failed to connect ftp server: %v", err)), nil
	}

	err = clientT.Login(v3, v4)
	if err != nil {
		return ConvertToObject(fmt.Errorf("failed to login ftp server: %v", err)), nil
	}

	sizeT, err := clientT.FileSize(v6)
	if err != nil {
		return ConvertToObject(fmt.Errorf("failed to get file size: %v", err)), nil
	}
	
	err = clientT.Quit()

	if err != nil {
		return ConvertToObject(fmt.Errorf("failed to close ftp connection: %v", err)), nil
	}

	return Int(sizeT), nil
}

func builtinFtpUploadFunc(c Call) (Object, error) {
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
	if strings.HasPrefix(v4, "//TXDEF#") {
		v4 = strings.TrimSpace(tk.DecryptStringByTXDEF(v4))
	}
	v5 = strings.TrimSpace(tk.GetSwitch(pa, "-path=", v5))
	v6 = strings.TrimSpace(tk.GetSwitch(pa, "-remotePath=", v6))
	
	v7 := tk.ToInt(strings.TrimSpace(tk.GetSwitch(pa, "-timeout=", "15")), 0)

	forceT := tk.IfSwitchExists(pa, "-force")
	
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

	clientT, err := ftp.Dial(v1+":"+v2, ftp.DialWithTimeout(time.Duration(v7)*time.Second))
	if err != nil {
		return ConvertToObject(fmt.Errorf("failed to connect ftp server: %v", err)), nil
	}

	err = clientT.Login(v3, v4)
	if err != nil {
		return ConvertToObject(fmt.Errorf("failed to login ftp server: %v", err)), nil
	}

	if !forceT {
		_, err := clientT.FileSize(v6)
		if err == nil {
			return NewCommonErrorWithPos(c, "remote file already exists"), nil
		}
	}

	fileT, err := os.Open(v5)
	if err != nil {
		return NewCommonErrorWithPos(c, "failed to open local file: %v", err), nil
	}
	defer fileT.Close()

	//	data := bytes.NewBufferString("Hello World")
	err = clientT.Stor(v6, fileT)
	if err != nil {
		return NewCommonErrorWithPos(c, "failed to upload file: %v", err), nil
	}
	
	err = clientT.Quit()

	if err != nil {
		return NewCommonErrorWithPos(c, "failed to close ftp connection: %v", err), nil
	}

//	sshT, errT := tk.NewSSHClient(v1, v2, v3, v4)
//
//	if errT != nil {
//		return ConvertToObject(errT), nil
//	}
//
//	defer sshT.Close()
//
//	if withProgressT {
//		fmt.Println()
//		errT = sshT.UploadWithProgressFunc(v5, v6, func(i interface{}) interface{} {
//			fmt.Printf("\rprogress: %v                ", i)
//			return ""
//		}, pa...)
//		fmt.Println()
//
//	} else {
//		errT = sshT.Upload(v5, v6, pa...)
//	}
//
//	if errT != nil {
//		return ConvertToObject(errT), nil
//	}
//
	return Undefined, nil
}

func builtinFtpDownloadBytesFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return nil, fmt.Errorf("not enough parameters")
	}

	pa := ObjectsToS(args)

	var v1, v2, v3, v4, v6 string

	v1 = strings.TrimSpace(tk.GetSwitch(pa, "-host=", v1))
	v2 = strings.TrimSpace(tk.GetSwitch(pa, "-port=", v2))
	v3 = strings.TrimSpace(tk.GetSwitch(pa, "-user=", v3))
	v4 = strings.TrimSpace(tk.GetSwitch(pa, "-password=", v4))
	if strings.HasPrefix(v4, "740404") {
		v4 = strings.TrimSpace(tk.DecryptStringByTXDEF(v4))
	}
	if strings.HasPrefix(v4, "//TXDEF#") {
		v4 = strings.TrimSpace(tk.DecryptStringByTXDEF(v4))
	}

	v6 = strings.TrimSpace(tk.GetSwitch(pa, "-remotePath=", v6))
	
	v7 := tk.ToInt(strings.TrimSpace(tk.GetSwitch(pa, "-timeout=", "15")), 0)

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

//	if v5 == "" {
//		return ConvertToObject(fmt.Errorf("emtpy path")), nil
//	}

	if v6 == "" {
		return ConvertToObject(fmt.Errorf("emtpy remotePath")), nil
	}

	clientT, err := ftp.Dial(v1+":"+v2, ftp.DialWithTimeout(time.Duration(v7)*time.Second))
	if err != nil {
		return ConvertToObject(fmt.Errorf("failed to connect ftp server: %v", err)), nil
	}

	err = clientT.Login(v3, v4)
	if err != nil {
		return ConvertToObject(fmt.Errorf("failed to login ftp server: %v", err)), nil
	}

//	_, err := clientT.FileSize(v6)
//	if err != nil {
//		return NewCommonErrorWithPos(c, "remote file not exists"), nil
//	}

	r, err := clientT.Retr(v6)
	if err != nil {
		return NewCommonErrorWithPos(c, "failed to download file: %v", err), nil
	}
	
	defer r.Close()
	
	bufT, err := io.ReadAll(r)
	if err != nil {
		return NewCommonErrorWithPos(c, "failed to read contents: %v", err), nil
	}
	
	err = clientT.Quit()

	if err != nil {
		return NewCommonErrorWithPos(c, "failed to close ftp connection: %v", err), nil
	}

	return Bytes(bufT), nil
}

func builtinFtpUploadFromReaderFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return nil, fmt.Errorf("not enough parameters")
	}

	pa := ObjectsToS(args[1:])

	var v1, v2, v3, v4, v6 string

	v1 = strings.TrimSpace(tk.GetSwitch(pa, "-host=", v1))
	v2 = strings.TrimSpace(tk.GetSwitch(pa, "-port=", v2))
	v3 = strings.TrimSpace(tk.GetSwitch(pa, "-user=", v3))
	v4 = strings.TrimSpace(tk.GetSwitch(pa, "-password=", v4))
	if strings.HasPrefix(v4, "740404") {
		v4 = strings.TrimSpace(tk.DecryptStringByTXDEF(v4))
	}
	if strings.HasPrefix(v4, "//TXDEF#") {
		v4 = strings.TrimSpace(tk.DecryptStringByTXDEF(v4))
	}
//	v5 = strings.TrimSpace(tk.GetSwitch(pa, "-path=", v5))
	v6 = strings.TrimSpace(tk.GetSwitch(pa, "-remotePath=", v6))
	
	v7 := tk.ToInt(strings.TrimSpace(tk.GetSwitch(pa, "-timeout=", "15")), 0)

	forceT := tk.IfSwitchExists(pa, "-force")
	
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

//	if v5 == "" {
//		return ConvertToObject(fmt.Errorf("emtpy path")), nil
//	}

	if v6 == "" {
		return ConvertToObject(fmt.Errorf("emtpy remotePath")), nil
	}

	clientT, err := ftp.Dial(v1+":"+v2, ftp.DialWithTimeout(time.Duration(v7)*time.Second))
	if err != nil {
		return ConvertToObject(fmt.Errorf("failed to connect ftp server: %v", err)), nil
	}

	err = clientT.Login(v3, v4)
	if err != nil {
		return ConvertToObject(fmt.Errorf("failed to login ftp server: %v", err)), nil
	}

	if !forceT {
		_, err := clientT.FileSize(v6)
		if err == nil {
			return NewCommonErrorWithPos(c, "remote file already exists"), nil
		}
	}

	//	data := bytes.NewBufferString("Hello World")
	err = clientT.Stor(v6, ConvertFromObject(args[0]).(io.Reader))
	if err != nil {
		return ConvertToObject(fmt.Errorf("failed to upload file: %v", err)), nil
	}
	
	err = clientT.Quit()

	if err != nil {
		return ConvertToObject(fmt.Errorf("failed to close ftp connection: %v", err)), nil
	}

	return Undefined, nil
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
	if strings.HasPrefix(v4, "//TXDEF#") {
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
			fmt.Printf("\rprogress: %v                ", i)
			return ""
		}, pa...)
		fmt.Println()

	} else {
		errT = sshT.Upload(v5, v6, pa...)
	}

	if errT != nil {
		return ConvertToObject(errT), nil
	}

	return Undefined, nil
}

func builtinSshUploadBytesFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return nil, fmt.Errorf("not enough parameters")
	}

	pa := ObjectsToS(args[1:])

	var v1, v2, v3, v4, v6 string

	v1 = strings.TrimSpace(tk.GetSwitch(pa, "-host=", v1))
	v2 = strings.TrimSpace(tk.GetSwitch(pa, "-port=", v2))
	v3 = strings.TrimSpace(tk.GetSwitch(pa, "-user=", v3))
	v4 = strings.TrimSpace(tk.GetSwitch(pa, "-password=", v4))
	if strings.HasPrefix(v4, "740404") {
		v4 = strings.TrimSpace(tk.DecryptStringByTXDEF(v4))
	}
	if strings.HasPrefix(v4, "//TXDEF#") {
		v4 = strings.TrimSpace(tk.DecryptStringByTXDEF(v4))
	}

	v5, ok := args[0].(Bytes)

	if !ok {
		v5 = Bytes([]byte(args[0].String()))
	}

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

	// if v5 == "" {
	// 	return ConvertToObject(fmt.Errorf("emtpy path")), nil
	// }

	if v6 == "" {
		return ConvertToObject(fmt.Errorf("emtpy remotePath")), nil
	}

	sshT, errT := tk.NewSSHClient(v1, v2, v3, v4)

	if errT != nil {
		return ConvertToObject(errT), nil
	}

	defer sshT.Close()

	if withProgressT {
		errT = sshT.UploadFileContent(v5, v6, pa...)
	} else {
		errT = sshT.UploadFileContent(v5, v6, pa...)
	}

	if errT != nil {
		return ConvertToObject(errT), nil
	}

	return Undefined, nil
}

func builtinSshDownloadFunc(c Call) (Object, error) {
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
	if strings.HasPrefix(v4, "//TXDEF#") {
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
		// fmt.Println()
		errT = sshT.Download(v5, v6, pa...)
	} else {
		errT = sshT.Download(v5, v6, pa...)
	}

	// fmt.Println()

	if errT != nil {
		return ConvertToObject(errT), nil
	}

	return Undefined, nil
}

func builtinSshDownloadBytesFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return nil, fmt.Errorf("not enough parameters")
	}

	pa := ObjectsToS(args)

	var v1, v2, v3, v4, v6 string

	v1 = strings.TrimSpace(tk.GetSwitch(pa, "-host=", v1))
	v2 = strings.TrimSpace(tk.GetSwitch(pa, "-port=", v2))
	v3 = strings.TrimSpace(tk.GetSwitch(pa, "-user=", v3))
	v4 = strings.TrimSpace(tk.GetSwitch(pa, "-password=", v4))
	if strings.HasPrefix(v4, "740404") {
		v4 = strings.TrimSpace(tk.DecryptStringByTXDEF(v4))
	}
	if strings.HasPrefix(v4, "//TXDEF#") {
		v4 = strings.TrimSpace(tk.DecryptStringByTXDEF(v4))
	}
	v6 = strings.TrimSpace(tk.GetSwitch(pa, "-remotePath=", v6))

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

	if v6 == "" {
		return ConvertToObject(fmt.Errorf("emtpy remotePath")), nil
	}

	sshT, errT := tk.NewSSHClient(v1, v2, v3, v4)

	if errT != nil {
		return ConvertToObject(errT), nil
	}

	defer sshT.Close()

	bytesT, errT := sshT.GetFileContent(v6, pa...)

	// fmt.Println()

	if errT != nil {
		return ConvertToObject(errT), nil
	}

	return Bytes(bytesT), nil
}

func builtinSshRunFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return nil, fmt.Errorf("not enough parameters")
	}

	pa := ObjectsToS(args)

	var v1, v2, v3, v4, v5 string

	v1 = strings.TrimSpace(tk.GetSwitch(pa, "-host=", v1))
	v2 = strings.TrimSpace(tk.GetSwitch(pa, "-port=", v2))
	v3 = strings.TrimSpace(tk.GetSwitch(pa, "-user=", v3))
	v4 = strings.TrimSpace(tk.GetSwitch(pa, "-password=", v4))
	if strings.HasPrefix(v4, "740404") {
		v4 = strings.TrimSpace(tk.DecryptStringByTXDEF(v4))
	}
	if strings.HasPrefix(v4, "//TXDEF#") {
		v4 = strings.TrimSpace(tk.DecryptStringByTXDEF(v4))
	}
	v5 = strings.TrimSpace(tk.GetSwitch(pa, "-cmd=", v5))

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
		return ConvertToObject(fmt.Errorf("emtpy command")), nil
	}

	sshT, errT := tk.NewSSHClient(v1, v2, v3, v4)

	if errT != nil {
		return ConvertToObject(errT), nil
	}

	defer sshT.Close()

	outT, errT := sshT.Run(v5)

	if errT != nil {
		return ConvertToObject(errT), nil
	}

	return ToStringObject(outT), nil
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
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
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
	vs := ObjectsToS(args[1:])

	if !tk.IfSwitchExists(vs, "-inherit") {
		compilerOptionsT = nil
	}

	if compilerOptionsT == nil {
		compilerOptionsT = &DefaultCompilerOptions
	}

	return NewCharCode(args[0].String(), compilerOptionsT), nil
}

var BuiltinCharCodeFunc = builtinCharCodeFunc

func builtinGelFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return Undefined, NewCommonErrorWithPos(c, "not enough parameters")
	}

	return NewGel(args...)
}

func builtinOrderedMapFunc(c Call) (Object, error) {
	args := c.GetArgs()

	return NewOrderedMap(args...)
}

func builtinExcelFunc(c Call) (Object, error) {
	return NewExcel(c)
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

	var contentT Bytes = nil

	v1, ok := args[1].(String)

	if ok {
		contentT = Bytes(v1.Value)
	}

	if contentT == nil {
		v2, ok := args[1].(Bytes)

		if ok {
			contentT = v2
		}
	}

	if contentT == nil {
		return NewCommonErrorWithPos(c, "invalid content type: (%T)%v", args[1], args[1]), nil
	}

	v, ok := args[0].(*HttpResp)
	if !ok {
		var writerT io.Writer = nil

		nv2, ok := args[0].(*Writer)

		if ok {
			writerT = nv2.Value
		} else {
			nv3, ok := args[0].(*File)

			if ok {
				writerT = nv3.Value
			}
		}

		if writerT == nil {
			return NewCommonErrorWithPos(c, "invalid object type: (%T)%v", args[0], args[0]), nil
		}

		// v2, ok := args[0].(*Writer)

		// if !ok {
		// 	return NewCommonErrorWithPos(c, "invalid object type: (%T)%v", args[0], args[0]), nil
		// }

		r, errT := writerT.Write(contentT)

		if errT != nil {
			return NewCommonErrorWithPos(c, "failed to write to writer: %v", errT), nil
		}

		return Int(r), errT
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

func builtinIsEncryptedFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	v1, ok := args[0].(Bytes)
	if ok {
		return Bool(tk.IsDataEncryptedByTXDEF([]byte(v1))), nil
	}

	v2, ok := args[0].(String)
	if ok {
		return Bool(tk.IsStringEncryptedByTXDEF(v2.Value)), nil
	}

	v3, ok := args[0].(*MutableString)
	if ok {
		return Bool(tk.IsStringEncryptedByTXDEF(v3.Value)), nil
	}

	return NewCommonErrorWithPos(c, "unsupport object type: %T", args[0]), nil
}

func builtinGenJwtTokenFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}
	
//	var v1Str string
//
//	v1, ok := args[0].(Map)
//	if ok {
//		v1Str = tk.ToJSONX(v1, "-sort")
//	} else {
//		v1Str = args[0].String()
//	}

	vs := ObjectsToS(args[2:])

	var v2Bytes []byte
	var errT error
	
	v2, ok := args[1].(Bytes)
	if ok {
		v2Bytes = []byte(v2)
	} else {
		if tk.IfSwitchExists(vs, "-base64Secret") {
			v2Bytes, errT = base64.RawURLEncoding.DecodeString(args[1].String())

			if errT != nil {
				return NewCommonErrorWithPos(c, "failed to decode base64-raw-url: %v", errT), nil
			}

		} else {
			v2Bytes = []byte(args[1].String())
		}
		
	}
	
	rs := tk.GenerateJwtToken(args[0], v2Bytes, vs...)

	return String{Value: rs}, nil
}

func builtinParseJwtTokenFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}
	
//	var v1Str string
//
//	v1, ok := args[0].(Map)
//	if ok {
//		v1Str = tk.ToJSONX(v1, "-sort")
//	} else {
//		v1Str = args[0].String()
//	}

	vs := ObjectsToS(args[2:])

	var v2Bytes []byte
	var errT error
	
	v2, ok := args[1].(Bytes)
	if ok {
		v2Bytes = []byte(v2)
	} else {
		if tk.IfSwitchExists(vs, "-base64Secret") {
			v2Bytes, errT = base64.RawURLEncoding.DecodeString(args[1].String())

			if errT != nil {
				return NewCommonErrorWithPos(c, "failed to decode base64-raw-url: %v", errT), nil
			}

		} else {
			v2Bytes = []byte(args[1].String())
		}
		
	}
	
	rs := tk.ParseJwtToken(args[0].String(), v2Bytes, vs...)

	return ConvertToObject(rs), nil
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
		var writerT io.Writer = nil

		nv2, ok := args[0].(*Writer)

		if ok {
			writerT = nv2.Value
		} else {
			nv3, ok := args[0].(*File)

			if ok {
				writerT = nv3.Value
			}
		}

		if writerT == nil {
			return NewCommonErrorWithPos(c, "invalid object type: (%T)%v", args[0], args[0]), nil
		}

		writerT.Write([]byte(fmt.Sprintf("setRespHeader: %v -> %v\n", args[1].String(), args[2].String())))

		return Undefined, nil
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

func builtinGetReqHeadersFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		rs := NewCommonErrorWithPos(c, "not enough parameters")
		return rs, nil
	}

	nv1, ok := args[0].(*HttpReq)
	if !ok {
		return NewCommonErrorWithPos(c, "invalid object type: (%T)%v", args[0], args[0]), nil
	}

	reqT := nv1.Value
	
	mapT := make(Map)
	
	for k, v := range reqT.Header {
		mapT[k] = String{Value: v[0]}
	}

	return mapT, nil
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

func builtinPrepareMultiPartFieldFromBytesFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	bytesT, ok := args[1].(Bytes)
	
	if !ok {
		return NewCommonErrorWithPos(c, "invalid object type: (%T)%v", args[1], args[1]), nil
	}
	
	fieldNameT := args[0].String()
	
	var b bytes.Buffer
    w := multipart.NewWriter(&b)
	
	r := bytes.NewReader([]byte(bytesT))
	
	var fw io.Writer
	
	var err error

	if fw, err = w.CreateFormField(fieldNameT); err != nil {
		return NewCommonErrorWithPos(c, "failed to create field: (%v) %v", args[0], err), nil
	}
	
	if _, err = io.Copy(fw, r); err != nil {
		return NewCommonErrorWithPos(c, "failed to set field: (%v) %v", args[0], err), nil
	}
	
    w.Close()
	
	return Array{ToStringObject(w.FormDataContentType()), Bytes(b.Bytes())}, nil
}

func builtinPrepareMultiPartFileFromBytesFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 3 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	bytesT, ok := args[2].(Bytes)
	
	if !ok {
		return NewCommonErrorWithPos(c, "invalid object type: (%T)%v", args[2], args[2]), nil
	}
	
	fieldNameT := args[0].String()
	fileNameT := args[1].String()
	
	var b bytes.Buffer
    w := multipart.NewWriter(&b)
	
	r := bytes.NewReader([]byte(bytesT))
	
	var fw io.Writer
	
	var err error

	if fw, err = w.CreateFormFile(fieldNameT, fileNameT); err != nil {
		return NewCommonErrorWithPos(c, "failed to create field: (%v) %v", args[0], err), nil
	}
	
	if _, err = io.Copy(fw, r); err != nil {
		return NewCommonErrorWithPos(c, "failed to set field: (%v) %v", args[0], err), nil
	}
	
    w.Close()
	
	return Array{ToStringObject(w.FormDataContentType()), Bytes(b.Bytes())}, nil
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
	
	bytesT, ok := args[1].(Bytes)
	
	if ok {
		rs, errT := tk.PostRequestBytesX(args[0].String(), []byte(bytesT), args[2].String(), time.Duration(ToGoIntWithDefault(args[3], 30)))

		if errT != nil {
			return NewCommonErrorWithPos(c, errT.Error()), nil
		}

		return ToStringObject(rs), nil
	}

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
	case "[]", "array", "list":
		if len(args) > 2 {
			return make(Array, tk.ToInt(args[1].String(), 0), tk.ToInt(args[2].String(), 0)), nil
		} else if len(args) > 1 {
			return make(Array, tk.ToInt(args[1].String(), 0)), nil
		} else {
			return make(Array, 0, 0), nil
		}
	case "{}", "map":
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
	case "stringBuilder", "stringBuffer":
		return builtinStringBuilderFunc(Call{Args: args[1:]})
	case "any":
		return builtinAnyFunc(Call{Args: args[1:]})
	case "ref", "objectRef":
		return &ObjectRef{Value: nil}, nil
	case "statusResult":
		return builtinStatusResultFunc(Call{Args: args[1:]})
	case "database":
		return builtinDatabaseFunc(Call{Args: args[1:]})
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
	case "stack":
		return NewStack(Call{Args: args[1:]})
	case "queue":
		return NewQueue(Call{Args: args[1:]})
	case "mapArray":
		return NewMapArray(Call{Args: args[1:]})
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
	case "excel":
		return NewExcel(Call{Args: args[1:]})
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

func builtinReadBytesFunc(c Call) (Object, error) {
	args := c.GetArgs()
	
	lenT := len(args)

	if lenT < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	nv1, ok := args[0].(*Reader)
	
	bufLenT := 100
	
	if lenT > 1 {
		bufLenT = ToIntQuick(args[1])
	}
	
	bufT := make([]byte, bufLenT)
	
//	resultBufT := make([]byte, 0, bufLenT)

	if ok {
		n, errT := nv1.Value.Read(bufT)
		if errT != nil {
			if errT == io.EOF {
				if n <= 0 {
					return NewCommonError("EOF"), nil
				} else {
					return Bytes(bufT[:n]), nil
				}
			} else {
				return NewCommonErrorWithPos(c, "failed to read bytes: %v", errT), nil
			}
		}

		if n <= 0 {
			return Bytes([]byte{}), nil
		}
		
//		resultBufT = append(resultBufT, bufT[:n]...)

		return Bytes(bufT[:n]), nil
	}

	nv1a, ok := args[0].(*File)

	if ok {
		n, errT := nv1a.Value.Read(bufT)
		if errT != nil {
			if errT == io.EOF {
				return NewCommonError("EOF"), nil
			}
			
			return NewCommonErrorWithPos(c, "failed to read bytes: %v", errT), nil
		}
		
		if n <= 0 {
			return Bytes([]byte{}), nil
		}
		
//		resultBufT = append(resultBufT, bufT[:n]...)

		return Bytes(bufT[:n]), nil
	}

	nv2, ok := args[0].(io.Reader)

	if ok {
		n, errT := nv2.Read(bufT)
		if errT != nil {
			if errT == io.EOF {
				return NewCommonError("EOF"), nil
			}
			
			return NewCommonErrorWithPos(c, "failed to read bytes: %v", errT), nil
		}
		
		if n <= 0 {
			return Bytes([]byte{}), nil
		}
		
//		resultBufT = append(resultBufT, bufT[:n]...)

		return Bytes(bufT[:n]), nil
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

	nv0, ok := args[0].(Bytes)

	if ok {
		n := copy(nv0, bufT)

		return Int(n), nil
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

func builtinWriteBytesAtFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 3 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	var oldBufT []byte
	var bufT []byte
//	var errT error
//	var n int

	nv0, ok := args[0].(Bytes)
	if !ok {
		return NewCommonErrorWithPos(c, "unsupport type of arg 1: %T", args[0]), nil
	}
	
	oldBufT = []byte(nv0)
	
	switch nv := args[2].(type) {
	case Bytes:
		bufT = []byte(nv)
	case String:
		bufT = []byte(nv.Value)
	case *MutableString:
		bufT = []byte(nv.Value)
	default:
		return NewCommonErrorWithPos(c, "unsupport type of arg 3: %T", args[2]), nil
	}
	
	idxT := ToIntQuick(args[1])
	
//	oldLenT := len(oldBufT)
//	writeLenT := len(bufT)
	
	n := copy(oldBufT[idxT:], bufT)
	
//	if oldLenT < 1 {
//		return Bytes(bufT), nil
//	}
//
//	if idxT < 0 {
//		idxT = oldLenT - idxT
//		
//		if idxT < 0 {
//			return NewCommonErrorWithPos(c, "invalid index: %v", args[1]), nil
//		}
//	} else if idxT == oldLenT {
//	} else if idxT > oldLenT {
//		return NewCommonErrorWithPos(c, "invalid index: %v", args[1]), nil
//	}
//	
//	

	return Int(n), nil
}

func builtinCopyBytesFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

//	var oldBufT []byte
//	var bufT []byte
	
//	var errT error
	var n int

	nv0, ok := args[0].(Bytes)
	if !ok {
		return NewCommonErrorWithPos(c, "unsupport type of arg 1: %T", args[0]), nil
	}
	
	nv1, ok := args[1].(Bytes)
	if !ok {
		return NewCommonErrorWithPos(c, "unsupport type of arg 2: %T", args[1]), nil
	}
	
	n = copy(nv0, nv1)

	return Int(n), nil
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

	v1 := nv0.Value
	v2 := nv1.Value

	if v1 == "godror" {
		matchesT := tk.RegFindFirstGroupsX(v2, `^(.*?)/(.*?)@(.*?)$`)

		if matchesT != nil {
			v1 = "oracle"
			v2 = "oracle://" + matchesT[1] + ":" + matchesT[2] + "@" + matchesT[3]
		}
	} else if v1 == "sqlite3" {
		v1 = "sqlite"
	}

	rsT := sqltk.ConnectDBX(v1, v2)
	if tk.IsError(rsT) {
		return NewFromError(rsT.(error)), nil
	}

	return &Database{DBType: v1, DBConnectString: v2, Value: rsT.(*sql.DB)}, nil
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

	var s1, s2 string

	if IsUndefInternal(args[2]) {
		args[2] = &HttpReq{Value: nil}
	}

	v0, ok := args[2].(*HttpReq)

	if ok {
		s1 = args[0].String()
		s2 = args[1].String()
	} else {
		v0, ok = args[0].(*HttpReq)

		if !ok {
			return NewCommonErrorWithPos(c, "invalid param type: %T(%v)", args[0], args[0]), nil
		}

		s1 = args[1].String()
		s2 = args[2].String()
	}

	rsT := tk.GenerateJSONPResponseWithMore(s1, s2, v0.Value, ObjectsToS(args[3:])...)

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

func builtinLeSshInfoFunc(c Call) (Object, error) {
	vmT := c.VM()

	if vmT == nil {
		return NewCommonErrorWithPos(c, "no VM specified"), nil
	}

	return ToStringObject(fmt.Sprintf("%v", vmT.LeSshInfo)), nil
}

func builtinLeInfoFunc(c Call) (Object, error) {
	vmT := c.VM()

	if vmT == nil {
		return NewCommonErrorWithPos(c, "no VM specified"), nil
	}

	return ToStringObject(fmt.Sprintf("line count: %v, char count: %v, rune count: %v", len(vmT.LeBuf), len(tk.JoinLines(vmT.LeBuf, "\n")), len([]rune(tk.JoinLines(vmT.LeBuf, "\n"))))), nil
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

	rsT, errT := builtinLeLoadFromStrFunc(Call{Vm: vmT, Args: []Object{String{Value: string(bufT)}}})

	if errT != nil {
		return NewCommonErrorWithPos(c, "failed to load file content into le buffer: %v", errT), nil
	}

	if tk.IsError(rsT) {
		return NewCommonErrorWithPos(c, "failed to load file content into le buffer: %v", rsT), nil
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

	// if len(args) < 0 {
	// 	return NewCommonError("not enough parameters"), nil
	// }

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

	errT = sshT.UploadFileContent([]byte(tk.JoinLines(vmT.LeBuf, "\n")), v5, pa...)

	if errT != nil {
		return NewCommonErrorWithPos(c, "failed to upload file content from server: %v", errT), nil
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

	if startA < 0 {
		startA = len(vmT.LeBuf) + startA
	}

	if startA >= len(vmT.LeBuf) {
		return NewCommonError("start index out of range: %v/%v", startA, len(vmT.LeBuf)), nil
	}

	if startA < 0 {
		return NewCommonError("start index out of range: %v/%v", startA, len(vmT.LeBuf)), nil
	}

	if endA < 0 {
		endA = len(vmT.LeBuf) + endA
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

func builtinStrToUtf8Func(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	vs := ObjectsToS(args[1:])

	encT := tk.GetSwitch(vs, "-encoding=", "")

	return String{Value: tk.ConvertStringToUTF8(args[0].String(), encT)}, nil
}

func builtinIsUtf8Func(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}
	
	nv1, ok := args[0].(Bytes)
	
	if ok {
		return Bool(utf8.Valid([]byte(nv1))), nil
	}

	nv2, ok := args[0].(String)
	
	if ok {
		return Bool(utf8.ValidString(nv2.Value)), nil
	}

	nv3, ok := args[0].(*MutableString)
	
	if ok {
		return Bool(utf8.ValidString(nv3.Value)), nil
	}

	return NewCommonErrorWithPos(c, "invalid parameter type: (%T)%v", args[0], args[0]), nil
//	return Bool(utf8.ValidString(args[0].String())), nil
}

func builtinSimpleStrToMapFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}
	
	nv1 := args[0].String()

	var mapT map[string]string = make(map[string]string)

	listT := strings.Split(nv1, "|")
	
	for _, v := range listT {
		list1T := strings.SplitN(v, ":", 2)
		
		if len(list1T) < 2 {
			continue
		}
		
		mapT[list1T[0]] = list1T[1]
	}

	return ConvertToObject(mapT), nil
}

func builtinSimpleStrToMapReverseFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}
	
	nv1 := args[0].String()

	var mapT map[string]string = make(map[string]string)

	listT := strings.Split(nv1, "|")
	
	for _, v := range listT {
		list1T := strings.SplitN(v, ":", 2)
		
		if len(list1T) < 2 {
			continue
		}
		
		mapT[list1T[1]] = list1T[0]
	}

	return ConvertToObject(mapT), nil
}

func builtinReverseMapFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}
	
	nv1, ok := args[0].(Map)
	
	if !ok {
		return NewCommonError("unsupported parameter type: %v", args[0].TypeName()), nil
	}

	mapT := make(Map, len(nv1))

	for k, v := range nv1 {
//		fmt.Printf("k: %#v, v: %#v\n", k, v)
		mapT[v.String()] = String{Value: k}
	}

	return mapT, nil
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

func builtinMagicFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	nv1 := ToIntQuick(args[0])

//	vs := ObjectsToS(args[1:])

	fcT := GetMagic(nv1)
	
	if tk.IsErrStr(fcT) {
		return NewCommonErrorWithPos(c, "failed to get magic script: %v", tk.GetErrStr(fcT)), nil
	}

	return &Gel{Value: NewCharCode(fcT, nil)}, nil
}

func builtinS3PutObjectFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 3 {
		return NewCommonError("not enough parameters"), nil
	}

	var readerT io.Reader = nil

	nv1, ok := args[0].(*Reader)

	if !ok {
		nv1a, ok := args[0].(*File)

		if !ok {
			return NewCommonError("invalid parameter 1 type: (%T)", args[0]), nil
		}

		readerT = nv1a.Value
	} else {
		readerT = nv1.Value
	}

	nv2 := args[1].String()

	nv3 := args[2].String()

	vs := ObjectsToS(args[3:])

	endPointT := tk.GetSwitch(vs, "-endPoint=", "")
	accessKeyT := tk.GetSwitch(vs, "-accessKey=", "")
	secretAccessKeyT := tk.GetSwitch(vs, "-secretAccessKey=", "")
	useSslT := tk.IfSwitchExists(vs, "-ssl")

	bucketNameT := tk.GetSwitch(vs, "-bucket=", nv2)
	pathT := tk.GetSwitch(vs, "-path=", nv3)

	contentTypeT := tk.GetSwitch(vs, "-contentType=", "application/octet-stream")

	sizeT := tk.ToInt(tk.GetSwitch(vs, "-size=", "-1"), -1)

	timeoutT := time.Duration(tk.ToFloat(tk.GetSwitch(vs, "-timeout=", "600"), 600.0) * float64(time.Second))

	getUrlT := tk.IfSwitchExists(vs, "-getUrl")

	urlTimeoutT := time.Duration(tk.ToFloat(tk.GetSwitch(vs, "-urlTimeout=", "86400"), 86400) * float64(time.Second))

	tagsStrT := tk.GetSwitch(vs, "-tags=", "")

	metaStrT := tk.GetSwitch(vs, "-meta=", "")

	optionsT := &minio.Options{
		Creds:  credentials.NewStaticV4(accessKeyT, secretAccessKeyT, ""),
		Secure: useSslT,
		Transport: &http.Transport{
			Proxy: http.ProxyFromEnvironment,
			DialContext: (&net.Dialer{
				Timeout:   30 * time.Second,
				KeepAlive: 30 * time.Second,
			}).DialContext,
			IdleConnTimeout:       90 * time.Second,
			TLSHandshakeTimeout:   10 * time.Second,
			ExpectContinueTimeout: 1 * time.Second,
			ResponseHeaderTimeout: timeoutT,
			// TLSClientConfig:       tlsConfig,
		},
	}

	minioClient, errT := minio.New(endPointT, optionsT)

	// minioClient.SetCustomTransport(transport)

	if errT != nil {
		return NewCommonError("failed to initialize s3 client: %v", errT), nil
	}

	putObjectOptionsT := minio.PutObjectOptions{
		ContentType:  contentTypeT,
		AutoChecksum: minio.ChecksumSHA256,
	}

	var tagsT map[string]string

	if tagsStrT != "" {

		errT = json.Unmarshal([]byte(tagsStrT), &tagsT)

		if errT != nil {
			return NewCommonError("failed to parse user tags: %v", errT), nil
		}

		putObjectOptionsT.UserTags = tagsT
	}

	var metaT map[string]string

	if metaStrT != "" {

		errT = json.Unmarshal([]byte(metaStrT), &metaT)

		if errT != nil {
			return NewCommonError("failed to parse meta data: %v", errT), nil
		}

		putObjectOptionsT.UserMetadata = metaT
	}

	infoT, errT := minioClient.PutObject(context.Background(), bucketNameT, pathT, readerT, int64(sizeT), putObjectOptionsT)
	if errT != nil {
		return NewCommonError("failed to put object: %v", errT), nil
	}

	mapT := make(Map, 0)

	objT := tk.FromJSONX(tk.ToJSONX(infoT)).(map[string]interface{})

	for k, v := range objT {
		mapT[k] = ToStringObject(tk.ToStr(v))
	}

	if getUrlT {
		reqParams := make(url.Values)
		reqParams.Set("response-content-disposition", `attachment; filename="`+tk.GetLastComponentOfUrl(pathT)+`"`)

		presignedURL, errT := minioClient.PresignedGetObject(context.Background(), bucketNameT, pathT, urlTimeoutT, reqParams)
		if errT != nil {
			mapT["PresignedUrl"] = ToStringObject(fmt.Sprintf("Error: %v", errT))
			// return NewCommonError("failed to get url : %v", errT), nil
		} else {
			mapT["PresignedUrl"] = ToStringObject(presignedURL.String())
		}

	}

	return mapT, nil
}

func builtinS3GetObjectUrlFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return NewCommonError("not enough parameters"), nil
	}

	bucketNameT := args[0].String()

	pathT := args[1].String()

	vs := ObjectsToS(args[2:])

	endPointT := tk.GetSwitch(vs, "-endPoint=", "")
	accessKeyT := tk.GetSwitch(vs, "-accessKey=", "")
	secretAccessKeyT := tk.GetSwitch(vs, "-secretAccessKey=", "")
	useSslT := tk.IfSwitchExists(vs, "-ssl")

	bucketNameT = tk.GetSwitch(vs, "-bucket=", bucketNameT)
	pathT = tk.GetSwitch(vs, "-path=", pathT)
	fileNameT := strings.TrimSpace(tk.GetSwitch(vs, "-fileName=", tk.GetLastComponentOfUrl(pathT)))

	if fileNameT == "" {
		fileNameT = tk.GetLastComponentOfUrl(pathT)
	}

	timeoutT := strings.TrimSpace(tk.GetSwitch(vs, "-timeout=", "86400"))

	if timeoutT == "" {
		timeoutT = "86400"
	}

	urlTimeoutT := time.Duration(tk.ToFloat(timeoutT, 86400) * float64(time.Second))

	optionsT := &minio.Options{
		Creds:  credentials.NewStaticV4(accessKeyT, secretAccessKeyT, ""),
		Secure: useSslT,
		Transport: &http.Transport{
			Proxy: http.ProxyFromEnvironment,
			DialContext: (&net.Dialer{
				Timeout:   30 * time.Second,
				KeepAlive: 30 * time.Second,
			}).DialContext,
			IdleConnTimeout:       90 * time.Second,
			TLSHandshakeTimeout:   10 * time.Second,
			ExpectContinueTimeout: 1 * time.Second,
			ResponseHeaderTimeout: 30 * time.Second,
			// TLSClientConfig:       tlsConfig,
		},
	}

	minioClient, errT := minio.New(endPointT, optionsT)

	// minioClient.SetCustomTransport(transport)

	if errT != nil {
		return NewCommonError("failed to initialize s3 client: %v", errT), nil
	}

	reqParams := make(url.Values)
	reqParams.Set("response-content-disposition", `attachment; filename="`+fileNameT+`"`)

	presignedURL, errT := minioClient.PresignedGetObject(context.Background(), bucketNameT, pathT, urlTimeoutT, reqParams)

	if errT != nil {
		return NewCommonErrorWithPos(c, "%v", errT), nil
	}

	return ToStringObject(presignedURL.String()), nil
}

func builtinS3GetObjectTagsFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return NewCommonError("not enough parameters"), nil
	}

	bucketNameT := args[0].String()

	pathT := args[1].String()

	vs := ObjectsToS(args[2:])

	endPointT := tk.GetSwitch(vs, "-endPoint=", "")
	accessKeyT := tk.GetSwitch(vs, "-accessKey=", "")
	secretAccessKeyT := tk.GetSwitch(vs, "-secretAccessKey=", "")
	useSslT := tk.IfSwitchExists(vs, "-ssl")

	bucketNameT = tk.GetSwitch(vs, "-bucket=", bucketNameT)
	pathT = tk.GetSwitch(vs, "-path=", pathT)

	optionsT := &minio.Options{
		Creds:  credentials.NewStaticV4(accessKeyT, secretAccessKeyT, ""),
		Secure: useSslT,
		Transport: &http.Transport{
			Proxy: http.ProxyFromEnvironment,
			DialContext: (&net.Dialer{
				Timeout:   30 * time.Second,
				KeepAlive: 30 * time.Second,
			}).DialContext,
			IdleConnTimeout:       90 * time.Second,
			TLSHandshakeTimeout:   10 * time.Second,
			ExpectContinueTimeout: 1 * time.Second,
			ResponseHeaderTimeout: 30 * time.Second,
			// TLSClientConfig:       tlsConfig,
		},
	}

	minioClient, errT := minio.New(endPointT, optionsT)

	// minioClient.SetCustomTransport(transport)

	if errT != nil {
		return NewCommonError("failed to initialize s3 client: %v", errT), nil
	}

	tagsT, errT := minioClient.GetObjectTagging(context.Background(), bucketNameT, pathT, minio.GetObjectTaggingOptions{})

	if errT != nil {
		return NewCommonErrorWithPos(c, "failed to get object tags: %v", errT), nil
	}

	return ConvertToObject(tagsT), nil
}

func builtinS3GetObjectStatFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return NewCommonError("not enough parameters"), nil
	}

	bucketNameT := args[0].String()

	pathT := args[1].String()

	vs := ObjectsToS(args[2:])

	endPointT := tk.GetSwitch(vs, "-endPoint=", "")
	accessKeyT := tk.GetSwitch(vs, "-accessKey=", "")
	secretAccessKeyT := tk.GetSwitch(vs, "-secretAccessKey=", "")
	useSslT := tk.IfSwitchExists(vs, "-ssl")

	bucketNameT = tk.GetSwitch(vs, "-bucket=", bucketNameT)
	pathT = tk.GetSwitch(vs, "-path=", pathT)

	optionsT := &minio.Options{
		Creds:  credentials.NewStaticV4(accessKeyT, secretAccessKeyT, ""),
		Secure: useSslT,
		Transport: &http.Transport{
			Proxy: http.ProxyFromEnvironment,
			DialContext: (&net.Dialer{
				Timeout:   30 * time.Second,
				KeepAlive: 30 * time.Second,
			}).DialContext,
			IdleConnTimeout:       90 * time.Second,
			TLSHandshakeTimeout:   10 * time.Second,
			ExpectContinueTimeout: 1 * time.Second,
			ResponseHeaderTimeout: 30 * time.Second,
			// TLSClientConfig:       tlsConfig,
		},
	}

	minioClient, errT := minio.New(endPointT, optionsT)

	// minioClient.SetCustomTransport(transport)

	if errT != nil {
		return NewCommonError("failed to initialize s3 client: %v", errT), nil
	}

	objT, errT := minioClient.GetObject(context.Background(), bucketNameT, pathT, minio.GetObjectOptions{})

	if errT != nil {
		return NewCommonErrorWithPos(c, "failed to get object: %v", errT), nil
	}

	statT, errT := objT.Stat()

	if errT != nil {
		return NewCommonErrorWithPos(c, "failed to get object stat: %v", errT), nil
	}

	return ConvertToObject(tk.FromJSONX(tk.ToJSONX(statT))), nil
}

func builtinS3GetObjectBytesFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return NewCommonError("not enough parameters"), nil
	}

	bucketNameT := args[0].String()

	pathT := args[1].String()

	vs := ObjectsToS(args[2:])

	endPointT := tk.GetSwitch(vs, "-endPoint=", "")
	accessKeyT := tk.GetSwitch(vs, "-accessKey=", "")
	secretAccessKeyT := tk.GetSwitch(vs, "-secretAccessKey=", "")
	useSslT := tk.IfSwitchExists(vs, "-ssl")

	bucketNameT = tk.GetSwitch(vs, "-bucket=", bucketNameT)
	pathT = tk.GetSwitch(vs, "-path=", pathT)

	optionsT := &minio.Options{
		Creds:  credentials.NewStaticV4(accessKeyT, secretAccessKeyT, ""),
		Secure: useSslT,
		Transport: &http.Transport{
			Proxy: http.ProxyFromEnvironment,
			DialContext: (&net.Dialer{
				Timeout:   30 * time.Second,
				KeepAlive: 30 * time.Second,
			}).DialContext,
			IdleConnTimeout:       90 * time.Second,
			TLSHandshakeTimeout:   10 * time.Second,
			ExpectContinueTimeout: 1 * time.Second,
			ResponseHeaderTimeout: 30 * time.Second,
			// TLSClientConfig:       tlsConfig,
		},
	}

	minioClient, errT := minio.New(endPointT, optionsT)

	// minioClient.SetCustomTransport(transport)

	if errT != nil {
		return NewCommonError("failed to initialize s3 client: %v", errT), nil
	}

	objT, errT := minioClient.GetObject(context.Background(), bucketNameT, pathT, minio.GetObjectOptions{})

	if errT != nil {
		return NewCommonErrorWithPos(c, "failed to get object: %v", errT), nil
	}

	bytesT, errT := io.ReadAll(objT)

	if errT != nil {
		return NewCommonErrorWithPos(c, "failed to read object bytes: %v", errT), nil
	}

	objT.Close()

	return Bytes(bytesT), nil
}

func builtinS3GetObjectTextFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return NewCommonError("not enough parameters"), nil
	}

	bucketNameT := args[0].String()

	pathT := args[1].String()

	vs := ObjectsToS(args[2:])

	endPointT := tk.GetSwitch(vs, "-endPoint=", "")
	accessKeyT := tk.GetSwitch(vs, "-accessKey=", "")
	secretAccessKeyT := tk.GetSwitch(vs, "-secretAccessKey=", "")
	useSslT := tk.IfSwitchExists(vs, "-ssl")

	bucketNameT = tk.GetSwitch(vs, "-bucket=", bucketNameT)
	pathT = tk.GetSwitch(vs, "-path=", pathT)

	optionsT := &minio.Options{
		Creds:  credentials.NewStaticV4(accessKeyT, secretAccessKeyT, ""),
		Secure: useSslT,
		Transport: &http.Transport{
			Proxy: http.ProxyFromEnvironment,
			DialContext: (&net.Dialer{
				Timeout:   30 * time.Second,
				KeepAlive: 30 * time.Second,
			}).DialContext,
			IdleConnTimeout:       90 * time.Second,
			TLSHandshakeTimeout:   10 * time.Second,
			ExpectContinueTimeout: 1 * time.Second,
			ResponseHeaderTimeout: 30 * time.Second,
			// TLSClientConfig:       tlsConfig,
		},
	}

	minioClient, errT := minio.New(endPointT, optionsT)

	// minioClient.SetCustomTransport(transport)

	if errT != nil {
		return NewCommonError("failed to initialize s3 client: %v", errT), nil
	}

	objT, errT := minioClient.GetObject(context.Background(), bucketNameT, pathT, minio.GetObjectOptions{})

	if errT != nil {
		return NewCommonErrorWithPos(c, "failed to get object: %v", errT), nil
	}

	bytesT, errT := io.ReadAll(objT)

	if errT != nil {
		return NewCommonErrorWithPos(c, "failed to read object text: %v", errT), nil
	}

	objT.Close()

	return &String{Value: string(bytesT)}, nil
}

func builtinS3GetObjectReaderFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return NewCommonError("not enough parameters"), nil
	}

	bucketNameT := args[0].String()

	pathT := args[1].String()

	vs := ObjectsToS(args[2:])

	endPointT := tk.GetSwitch(vs, "-endPoint=", "")
	accessKeyT := tk.GetSwitch(vs, "-accessKey=", "")
	secretAccessKeyT := tk.GetSwitch(vs, "-secretAccessKey=", "")
	useSslT := tk.IfSwitchExists(vs, "-ssl")

	bucketNameT = tk.GetSwitch(vs, "-bucket=", bucketNameT)
	pathT = tk.GetSwitch(vs, "-path=", pathT)

	optionsT := &minio.Options{
		Creds:  credentials.NewStaticV4(accessKeyT, secretAccessKeyT, ""),
		Secure: useSslT,
		Transport: &http.Transport{
			Proxy: http.ProxyFromEnvironment,
			DialContext: (&net.Dialer{
				Timeout:   30 * time.Second,
				KeepAlive: 30 * time.Second,
			}).DialContext,
			IdleConnTimeout:       90 * time.Second,
			TLSHandshakeTimeout:   10 * time.Second,
			ExpectContinueTimeout: 1 * time.Second,
			ResponseHeaderTimeout: 30 * time.Second,
			// TLSClientConfig:       tlsConfig,
		},
	}

	minioClient, errT := minio.New(endPointT, optionsT)

	// minioClient.SetCustomTransport(transport)

	if errT != nil {
		return NewCommonError("failed to initialize s3 client: %v", errT), nil
	}

	objT, errT := minioClient.GetObject(context.Background(), bucketNameT, pathT, minio.GetObjectOptions{})

	if errT != nil {
		return NewCommonErrorWithPos(c, "failed to get object: %v", errT), nil
	}

	readerT := &Reader{Value: objT}

	return readerT, nil
}

func builtinS3GetObjectToFileFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 3 {
		return NewCommonError("not enough parameters"), nil
	}

	bucketNameT := args[0].String()

	pathT := args[1].String()

	localPathT := args[2].String()

	vs := ObjectsToS(args[3:])

	endPointT := tk.GetSwitch(vs, "-endPoint=", "")
	accessKeyT := tk.GetSwitch(vs, "-accessKey=", "")
	secretAccessKeyT := tk.GetSwitch(vs, "-secretAccessKey=", "")
	useSslT := tk.IfSwitchExists(vs, "-ssl")

	bucketNameT = tk.GetSwitch(vs, "-bucket=", bucketNameT)
	pathT = tk.GetSwitch(vs, "-path=", pathT)
	localPathT = tk.GetSwitch(vs, "-localPath=", localPathT)

	forceT := tk.IfSwitchExists(vs, "-force")
	
	if !forceT && tk.IfFileExists(localPathT) {
		return NewCommonError("local file already exists"), nil
	}

	optionsT := &minio.Options{
		Creds:  credentials.NewStaticV4(accessKeyT, secretAccessKeyT, ""),
		Secure: useSslT,
		Transport: &http.Transport{
			Proxy: http.ProxyFromEnvironment,
			DialContext: (&net.Dialer{
				Timeout:   30 * time.Second,
				KeepAlive: 30 * time.Second,
			}).DialContext,
			IdleConnTimeout:       90 * time.Second,
			TLSHandshakeTimeout:   10 * time.Second,
			ExpectContinueTimeout: 1 * time.Second,
			ResponseHeaderTimeout: 30 * time.Second,
			// TLSClientConfig:       tlsConfig,
		},
	}

	minioClient, errT := minio.New(endPointT, optionsT)

	// minioClient.SetCustomTransport(transport)

	if errT != nil {
		return NewCommonError("failed to initialize s3 client: %v", errT), nil
	}

	errT = minioClient.FGetObject(context.Background(), bucketNameT, pathT, localPathT, minio.GetObjectOptions{})

	if errT != nil {
		return NewCommonErrorWithPos(c, "failed to get object to file: %v", errT), nil
	}

	return Undefined, nil
}

func builtinS3StatObjectFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return NewCommonError("not enough parameters"), nil
	}

	bucketNameT := args[0].String()

	pathT := args[1].String()

	vs := ObjectsToS(args[2:])

	endPointT := tk.GetSwitch(vs, "-endPoint=", "")
	accessKeyT := tk.GetSwitch(vs, "-accessKey=", "")
	secretAccessKeyT := tk.GetSwitch(vs, "-secretAccessKey=", "")
	useSslT := tk.IfSwitchExists(vs, "-ssl")
	compactT := tk.IfSwitchExists(vs, "-compact")

	bucketNameT = tk.GetSwitch(vs, "-bucket=", bucketNameT)
	pathT = tk.GetSwitch(vs, "-path=", pathT)

	optionsT := &minio.Options{
		Creds:  credentials.NewStaticV4(accessKeyT, secretAccessKeyT, ""),
		Secure: useSslT,
		Transport: &http.Transport{
			Proxy: http.ProxyFromEnvironment,
			DialContext: (&net.Dialer{
				Timeout:   30 * time.Second,
				KeepAlive: 30 * time.Second,
			}).DialContext,
			IdleConnTimeout:       90 * time.Second,
			TLSHandshakeTimeout:   10 * time.Second,
			ExpectContinueTimeout: 1 * time.Second,
			ResponseHeaderTimeout: 30 * time.Second,
			// TLSClientConfig:       tlsConfig,
		},
	}

	minioClient, errT := minio.New(endPointT, optionsT)

	// minioClient.SetCustomTransport(transport)

	if errT != nil {
		return NewCommonError("failed to initialize s3 client: %v", errT), nil
	}

	statT, errT := minioClient.StatObject(context.Background(), bucketNameT, pathT, minio.StatObjectOptions{})

	if errT != nil {
		return NewCommonErrorWithPos(c, "failed to stat object: %v", errT), nil
	}

	if compactT {
		mapT := Map{
			"Key":          ToStringObject(statT.Key),
			"LastModified": ToStringObject(tk.FormatTime(statT.LastModified.Local())),
			"Size":         ToIntObject(statT.Size),
		}

		mapT["ContentType"] = ToStringObject(statT.Metadata.Get("Content-Type"))
		originalNameT := statT.Metadata.Get("X-Amz-Meta-Originname")

		if strings.HasPrefix(originalNameT, "HEX_") {
			originalNameT = tk.HexToStr(originalNameT)
		}

		uploaderNameT := statT.Metadata.Get("X-Amz-Meta-Uploadername")

		if strings.HasPrefix(uploaderNameT, "HEX_") {
			uploaderNameT = tk.HexToStr(uploaderNameT)
		}

		sourceT := statT.Metadata.Get("X-Amz-Meta-Source")
		if strings.HasPrefix(sourceT, "HEX_") {
			sourceT = tk.HexToStr(sourceT)
		}

		mapT["VersionID"] = ToStringObject(statT.VersionID)
		mapT["OriginName"] = ToStringObject(originalNameT)
		mapT["Uploader"] = ToStringObject(statT.Metadata.Get("X-Amz-Meta-Uploader"))
		mapT["UploaderName"] = ToStringObject(uploaderNameT)
		mapT["Source"] = ToStringObject(sourceT)

		return mapT, nil
	}

	map1T := tk.FromJSONX(tk.ToJSONX(statT))
	// tk.Plv(map1T)

	return ConvertToObject(map1T), nil
}

func builtinS3RemoveObjectFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return NewCommonError("not enough parameters"), nil
	}

	bucketNameT := args[0].String()

	pathT := args[1].String()

	vs := ObjectsToS(args[2:])

	endPointT := tk.GetSwitch(vs, "-endPoint=", "")
	accessKeyT := tk.GetSwitch(vs, "-accessKey=", "")
	secretAccessKeyT := tk.GetSwitch(vs, "-secretAccessKey=", "")
	useSslT := tk.IfSwitchExists(vs, "-ssl")

	bucketNameT = tk.GetSwitch(vs, "-bucket=", bucketNameT)
	pathT = tk.GetSwitch(vs, "-path=", pathT)

	forceT := tk.IfSwitchExists(vs, "-force")

	versionIdT := strings.TrimSpace(tk.GetSwitch(vs, "-versionId=", ""))

	optionsT := &minio.Options{
		Creds:  credentials.NewStaticV4(accessKeyT, secretAccessKeyT, ""),
		Secure: useSslT,
		Transport: &http.Transport{
			Proxy: http.ProxyFromEnvironment,
			DialContext: (&net.Dialer{
				Timeout:   30 * time.Second,
				KeepAlive: 30 * time.Second,
			}).DialContext,
			IdleConnTimeout:       90 * time.Second,
			TLSHandshakeTimeout:   10 * time.Second,
			ExpectContinueTimeout: 1 * time.Second,
			ResponseHeaderTimeout: 30 * time.Second,
			// TLSClientConfig:       tlsConfig,
		},
	}

	minioClient, errT := minio.New(endPointT, optionsT)

	// minioClient.SetCustomTransport(transport)

	if errT != nil {
		return NewCommonError("failed to initialize s3 client: %v", errT), nil
	}

	removeOptionsT := minio.RemoveObjectOptions{
		ForceDelete: forceT,
	}

	if versionIdT != "" {
		removeOptionsT.VersionID = versionIdT
	}

	errT = minioClient.RemoveObject(context.Background(), bucketNameT, pathT, removeOptionsT)

	if errT != nil {
		return NewCommonErrorWithPos(c, "failed to remove object: %v", errT), nil
	}

	return Undefined, nil
}

func builtinS3CopyObjectFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 3 {
		return NewCommonError("not enough parameters"), nil
	}

	srcBucketNameT := args[0].String()
	srcPathT := args[1].String()

	destBucketNameT := args[2].String()
	destPathT := args[3].String()

	vs := ObjectsToS(args[2:])

	endPointT := tk.GetSwitch(vs, "-endPoint=", "")
	accessKeyT := tk.GetSwitch(vs, "-accessKey=", "")
	secretAccessKeyT := tk.GetSwitch(vs, "-secretAccessKey=", "")
	useSslT := tk.IfSwitchExists(vs, "-ssl")

	srcBucketNameT = tk.GetSwitch(vs, "-srcBucket=", srcBucketNameT)
	destBucketNameT = tk.GetSwitch(vs, "-destBucket=", destBucketNameT)
	srcPathT = tk.GetSwitch(vs, "-srcPath=", srcPathT)
	destPathT = tk.GetSwitch(vs, "-destPath=", destPathT)

//	forceT := tk.IfSwitchExists(vs, "-force")

	versionIdT := strings.TrimSpace(tk.GetSwitch(vs, "-versionId=", ""))

	srcOpts := minio.CopySrcOptions{
		Bucket: srcBucketNameT,
		Object: srcPathT,
	}
	
	if versionIdT != "" {
		srcOpts.VersionID = versionIdT
	}

	dstOpts := minio.CopyDestOptions{
		Bucket: destBucketNameT,
		Object: destPathT,
	}

	optionsT := &minio.Options{
		Creds:  credentials.NewStaticV4(accessKeyT, secretAccessKeyT, ""),
		Secure: useSslT,
		Transport: &http.Transport{
			Proxy: http.ProxyFromEnvironment,
			DialContext: (&net.Dialer{
				Timeout:   30 * time.Second,
				KeepAlive: 30 * time.Second,
			}).DialContext,
			IdleConnTimeout:       90 * time.Second,
			TLSHandshakeTimeout:   10 * time.Second,
			ExpectContinueTimeout: 1 * time.Second,
			ResponseHeaderTimeout: 30 * time.Second,
			// TLSClientConfig:       tlsConfig,
		},
	}

	minioClient, errT := minio.New(endPointT, optionsT)

	// minioClient.SetCustomTransport(transport)

	if errT != nil {
		return NewCommonError("failed to initialize s3 client: %v", errT), nil
	}

	uploadInfo, errT := minioClient.CopyObject(context.Background(), dstOpts, srcOpts)
	if errT != nil {
		return NewCommonErrorWithPos(c, "failed to copy object: %v", errT), nil
	}

	return ToStringObject(uploadInfo.VersionID), nil
}

func builtinS3MoveObjectFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 3 {
		return NewCommonError("not enough parameters"), nil
	}

	srcBucketNameT := args[0].String()
	srcPathT := args[1].String()

	destBucketNameT := args[2].String()
	destPathT := args[3].String()

	vs := ObjectsToS(args[2:])

	endPointT := tk.GetSwitch(vs, "-endPoint=", "")
	accessKeyT := tk.GetSwitch(vs, "-accessKey=", "")
	secretAccessKeyT := tk.GetSwitch(vs, "-secretAccessKey=", "")
	useSslT := tk.IfSwitchExists(vs, "-ssl")

	srcBucketNameT = tk.GetSwitch(vs, "-srcBucket=", srcBucketNameT)
	destBucketNameT = tk.GetSwitch(vs, "-destBucket=", destBucketNameT)
	srcPathT = tk.GetSwitch(vs, "-srcPath=", srcPathT)
	destPathT = tk.GetSwitch(vs, "-destPath=", destPathT)

	forceT := tk.IfSwitchExists(vs, "-force")

	versionIdT := strings.TrimSpace(tk.GetSwitch(vs, "-versionId=", ""))

	srcOpts := minio.CopySrcOptions{
		Bucket: srcBucketNameT,
		Object: srcPathT,
	}
	
	if versionIdT != "" {
		srcOpts.VersionID = versionIdT
	}

	dstOpts := minio.CopyDestOptions{
		Bucket: destBucketNameT,
		Object: destPathT,
	}

	optionsT := &minio.Options{
		Creds:  credentials.NewStaticV4(accessKeyT, secretAccessKeyT, ""),
		Secure: useSslT,
		Transport: &http.Transport{
			Proxy: http.ProxyFromEnvironment,
			DialContext: (&net.Dialer{
				Timeout:   30 * time.Second,
				KeepAlive: 30 * time.Second,
			}).DialContext,
			IdleConnTimeout:       90 * time.Second,
			TLSHandshakeTimeout:   10 * time.Second,
			ExpectContinueTimeout: 1 * time.Second,
			ResponseHeaderTimeout: 30 * time.Second,
			// TLSClientConfig:       tlsConfig,
		},
	}

	minioClient, errT := minio.New(endPointT, optionsT)

	// minioClient.SetCustomTransport(transport)

	if errT != nil {
		return NewCommonError("failed to initialize s3 client: %v", errT), nil
	}

	uploadInfo, errT := minioClient.CopyObject(context.Background(), dstOpts, srcOpts)
	if errT != nil {
		return NewCommonErrorWithPos(c, "failed to copy object: %v", errT), nil
	}

	removeOptionsT := minio.RemoveObjectOptions{
		ForceDelete: forceT,
	}

	if versionIdT != "" {
		removeOptionsT.VersionID = versionIdT
	}

	errT = minioClient.RemoveObject(context.Background(), srcBucketNameT, srcPathT, removeOptionsT)

	if errT != nil {
		return NewCommonErrorWithPos(c, "failed to remove object: %v", errT), nil
	}

	return ToStringObject(uploadInfo.VersionID), nil
}

func builtinS3ListObjectsFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonError("not enough parameters"), nil
	}

	bucketNameT := args[0].String()

	vs := ObjectsToS(args[1:])

	endPointT := tk.GetSwitch(vs, "-endPoint=", "")
	accessKeyT := tk.GetSwitch(vs, "-accessKey=", "")
	secretAccessKeyT := tk.GetSwitch(vs, "-secretAccessKey=", "")
	useSslT := tk.IfSwitchExists(vs, "-ssl")

	useV1T := tk.IfSwitchExists(vs, "-useV1")

	bucketNameT = tk.GetSwitch(vs, "-bucket=", bucketNameT)

	prefixT := tk.GetSwitch(vs, "-prefix=", "")
	withVersionT := tk.IfSwitchExists(vs, "-withVersions")
	withMetadataT := tk.IfSwitchExists(vs, "-withMetadata") || tk.IfSwitchExists(vs, "-withMeta")
	recursiveT := tk.IfSwitchExists(vs, "-recursive")
	detailT := tk.IfSwitchExists(vs, "-detail")
	compactT := tk.IfSwitchExists(vs, "-compact")

	optionsT := &minio.Options{
		Creds:  credentials.NewStaticV4(accessKeyT, secretAccessKeyT, ""),
		Secure: useSslT,
		Transport: &http.Transport{
			Proxy: http.ProxyFromEnvironment,
			DialContext: (&net.Dialer{
				Timeout:   30 * time.Second,
				KeepAlive: 30 * time.Second,
			}).DialContext,
			IdleConnTimeout:       90 * time.Second,
			TLSHandshakeTimeout:   10 * time.Second,
			ExpectContinueTimeout: 1 * time.Second,
			ResponseHeaderTimeout: 30 * time.Second,
			// TLSClientConfig:       tlsConfig,
		},
	}

	minioClient, errT := minio.New(endPointT, optionsT)

	// minioClient.SetCustomTransport(transport)

	if errT != nil {
		return NewCommonErrorWithPos(c, "failed to initialize s3 client: %v", errT), nil
	}

	listOptionsT := minio.ListObjectsOptions{
		WithVersions: withVersionT,
		WithMetadata: withMetadataT,
		Recursive:    recursiveT,
		UseV1:        useV1T,
	}

	if prefixT != "" {
		listOptionsT.Prefix = prefixT
	}

	// tk.Pl("listOptionsT: %#v", listOptionsT)

	aryT := make(Array, 0)

	for object := range minioClient.ListObjects(context.Background(), bucketNameT, listOptionsT) {
		// aryT = append(aryT, ToStringObject(tk.ToJSONX(object)))
		if compactT {
			mapT := Map{
				"Key":          ToStringObject(object.Key),
				"LastModified": ToStringObject(tk.FormatTime(object.LastModified.Local())),
				"Size":         ToIntObject(object.Size),
			}

			if prefixT != "" {
				mapT["Rel"] = ToStringObject(strings.TrimPrefix(object.Key, prefixT))
			} else {
				mapT["Rel"] = ToStringObject(object.Key)
			}

			if detailT {
				statT, errT := minioClient.StatObject(context.Background(), bucketNameT, object.Key, minio.StatObjectOptions{})

				if errT == nil {
					// return NewCommonErrorWithPos(c, "failed to stat object: %v", errT), nil
					mapT["ContentType"] = ToStringObject(statT.Metadata.Get("Content-Type"))
					originalNameT := statT.Metadata.Get("X-Amz-Meta-Originname")

					if strings.HasPrefix(originalNameT, "HEX_") {
						originalNameT = tk.HexToStr(originalNameT)
					}

					uploaderNameT := statT.Metadata.Get("X-Amz-Meta-Uploadername")
					if strings.HasPrefix(uploaderNameT, "HEX_") {
						uploaderNameT = tk.HexToStr(uploaderNameT)
					}

					sourceT := statT.Metadata.Get("X-Amz-Meta-Source")
					if strings.HasPrefix(sourceT, "HEX_") {
						sourceT = tk.HexToStr(sourceT)
					}

					mapT["VersionID"] = ToStringObject(statT.VersionID)
					mapT["OriginName"] = ToStringObject(originalNameT)
					mapT["Uploader"] = ToStringObject(statT.Metadata.Get("X-Amz-Meta-Uploader"))
					mapT["UploaderName"] = ToStringObject(uploaderNameT)
					mapT["Source"] = ToStringObject(sourceT)
				}

			}

			aryT = append(aryT, mapT)
		} else {
			if detailT {
				statT, errT := minioClient.StatObject(context.Background(), bucketNameT, object.Key, minio.StatObjectOptions{})

				if errT == nil {
					// return NewCommonErrorWithPos(c, "failed to stat object: %v", errT), nil
					object.ContentType = statT.Metadata.Get("Content-Type")
					originalNameT := statT.Metadata.Get("X-Amz-Meta-Originname")

					if strings.HasPrefix(originalNameT, "HEX_") {
						originalNameT = tk.HexToStr(originalNameT)
					}

					object.UserMetadata = minio.StringMap{"VersionID": statT.VersionID, "OriginName": originalNameT, "Uploader": statT.Metadata.Get("X-Amz-Meta-Uploader")}
				}

			}

			aryT = append(aryT, ConvertToObject(object))
		}
	}

	return aryT, nil
}

func builtinReplaceHtmlByMapFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return NewCommonError("not enough parameters"), nil
	}

	s := args[0].String()

	m, ok := args[1].(Map)
	if !ok {
		return NewCommonError("unsupported parameter type: %v", args[1].TypeName()), nil
	}

	if m == nil {
		return args[0], nil
	}

	for k, v := range m {
		s = tk.Replace(s, "TX_"+k+"_XT", v.String())
	}

	return ToStringObject(s), nil
}

func builtinDocxReplacePatternFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	s, ok := args[0].(Bytes)
	if !ok {
		return NewCommonErrorWithPos(c, "unsupported parameter 1 type(expect bytes): %v", args[0].TypeName()), nil
	}

	m, ok := args[1].(Array)
	if !ok {
		return NewCommonErrorWithPos(c, "unsupported parameter 2 type(expect array): %v", args[1].TypeName()), nil
	}

	if m == nil {
		return args[0], nil
	}
	
	lenT := len(m)
	
	aryT := make([]string, lenT)

	for i := 0; i < lenT; i ++ {
		aryT[i] = m[i].String()
	}
	
	rs := tk.ReplacePatternsInDocxBytes([]byte(s), aryT)
	
	errT, ok := rs.(error)
	if ok && errT != nil {
		return NewCommonErrorWithPos(c, "%v", errT), nil
	}

	return Bytes(rs.([]byte)), nil
}

func builtinDocxGetPlaceholdersFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	s, ok := args[0].(Bytes)
	if !ok {
		return NewCommonErrorWithPos(c, "unsupported parameter 1 type(expect bytes): %v", args[0].TypeName()), nil
	}

	rs := tk.GetPlaceholderInDocxBytes([]byte(s))
	
	errT, ok := rs.(error)
	if ok && errT != nil {
		return NewCommonErrorWithPos(c, "%v", errT), nil
	}

	return ConvertToObject(rs), nil
}

func builtinShowTableFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	var ok bool = false
	
	nv1, ok1 := args[0].(Array)
	if !ok1 {
		return NewCommonErrorWithPos(c, "unsupported parameter 1 type(expect array): %v", args[0].TypeName()), nil
	}
	
	len1 := len(nv1)
	
	var funcT func(...interface{}) interface{} = nil
	
	vs := []string{}
	
	var nv2 *Delegate
	
	for i, v := range args {
		if i < 1 {
			continue
		}
		
		if !ok {
			nv2, ok = v.(*Delegate)
			
			if ok {
//				fmt.Printf("nv2 type: %T, %#v\n", nv2.Value, nv2.Value)
				funcT = nv2.Value
			} else {
				vs = append(vs, v.String())
			}
		} else {
			vs = append(vs, v.String())
		}
	}

	aryT := make([][]string, 0, len1)
	
	for i := 0; i < len1; i ++ {
		nv2, ok := nv1[i].(Array)
		
		if !ok {
			return NewCommonErrorWithPos(c, "unsupported row %v type(expect array): %v", i, nv1[i].TypeName()), nil
		}
		
		aryT = append(aryT, ObjectsToS(nv2))
	}

	rs := tk.ShowTableCompact(aryT, funcT, vs...)
	
	return String{Value: rs}, nil
}

func builtinDocxToStrsFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	vs := ObjectsToS(args[1:])

	nv1, ok := args[0].(Bytes)
	if ok {
		rs := tk.DocxBytesToText([]byte(nv1), vs...)
		
	//	if tk.IsError(rs) {
	//		return NewCommonErrorWithPos(c, "failed convert docx bytes to text: %v", rs), nil
	//	}
		
		return ConvertToObject(rs), nil
	}
	
	rs := tk.DocxToText(args[0].String(), vs...)

	return ConvertToObject(rs), nil
}

func builtinSendMailFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 6 {
		return NewCommonError("not enough parameters"), nil
	}

	vs := ObjectsToS(args)

	hostT := strings.TrimSpace(tk.GetSwitch(vs, "-host=", ""))

	if hostT == "" {
		return NewCommonError("host empty"), nil
	}

	portT := tk.ToInt(strings.TrimSpace(tk.GetSwitch(vs, "-port=", "25")), -1)

	if portT < 1 {
		return NewCommonError("invalid or empty port number"), nil
	}

	userT := strings.TrimSpace(tk.GetSwitch(vs, "-user=", ""))

	if userT == "" {
		return NewCommonError("user empty"), nil
	}

	passwordT := strings.TrimSpace(tk.GetSwitch(vs, "-password=", ""))

	if passwordT == "" {
		return NewCommonError("password empty"), nil
	}

	if strings.HasPrefix(passwordT, "740404") {
		passwordT = strings.TrimSpace(tk.DecryptStringByTXDEF(passwordT))
	}

	subjectT := strings.TrimSpace(tk.GetSwitch(vs, "-subject=", ""))

	if subjectT == "" {
		return NewCommonError("subject empty"), nil
	}

	bodyT := strings.TrimSpace(tk.GetSwitch(vs, "-body=", ""))

	if bodyT == "" {
		return NewCommonError("body empty"), nil
	}

	toT := strings.TrimSpace(tk.GetSwitch(vs, "-to=", ""))

	ccT := strings.TrimSpace(tk.GetSwitch(vs, "-cc=", ""))

	bccT := strings.TrimSpace(tk.GetSwitch(vs, "-bcc=", ""))

	if toT+ccT+bccT == "" {
		return NewCommonError("no receiver"), nil
	}

	fromT := strings.TrimSpace(tk.GetSwitch(vs, "-from=", ""))

	if fromT == "" {
		if tk.RegMatchX(userT, `.+@.+`) {
			fromT = userT
		}
	}

	fromNameT := strings.TrimSpace(tk.GetSwitch(vs, "-fromName=", ""))

	attachFilesT := strings.TrimSpace(tk.GetSwitch(vs, "-attachFiles=", ""))

	mail := mailyak.New(fmt.Sprintf("%v:%v", hostT, portT), tk.GetLoginAuth(userT, passwordT))

	if toT != "" {
		mail.To(strings.Split(strings.ReplaceAll(toT, ";", ","), ",")...)
	}

	if ccT != "" {
		mail.Cc(strings.Split(strings.ReplaceAll(ccT, ";", ","), ",")...)
	}

	if bccT != "" {
		mail.Bcc(strings.Split(strings.ReplaceAll(bccT, ";", ","), ",")...)
	}

	mail.From(fromT)
	mail.FromName(fromNameT)

	mail.Subject(subjectT)

	mail.HTML().Set(bodyT)

	attachListT := strings.Split(strings.ReplaceAll(attachFilesT, ";", ","), ",")

	// tk.Plv(attachListT)

	for i, v := range attachListT {
		v = strings.TrimSpace(v)

		if v == "" {
			continue
		}

		lineListT := strings.SplitN(v, ":", 2)

		if len(lineListT) < 2 {
			return NewCommonError("invalid attach file format(should be in form NAME:FILE_PATH): %v", v), nil
		}

		frsT := tk.LoadBytesFromFile(strings.TrimSpace(lineListT[1]))

		if tk.IsErrX(frsT) {
			return NewCommonError("failed to attach file #%v: %v", i, frsT), nil
		}

		bufT := bytes.NewBuffer(frsT.([]byte))

		mail.Attach(lineListT[0], bufT)
	}

	// _, errT = io.WriteString(mail.HTML(), mailBodyHtml)

	// if errT != nil {
	// 	wResp("fail", errStrf("邮件格式解析错误：%v", errT))
	// 	return
	// }

	mail.Plain().Set(bodyT)

	errT := mail.Send()
	if errT != nil {
		return NewCommonError("failed to send mail: %v", errT), nil
	}

	return Undefined, nil
}

func builtinGuiServerCommandFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}
	
	cmdT := args[0].String()

	vs := ObjectsToS(args[1:])

	urlT := strings.TrimSpace(tk.GetSwitch(vs, "-url=", "http://127.0.0.1:7458"))

	if urlT == "" {
		urlT = "http://127.0.0.1:7458"
//		return NewCommonErrorWithPos(c, "url empty"), nil
	}

	mapT := map[string]string{"cmd": cmdT}
	
	parasT := make([]string, 0, len(vs))

	optsT := make([]interface{}, 0, len(vs))

	for _, argT := range vs {
		if strings.HasPrefix(argT, "-") {
			optsT = append(optsT, argT)
			continue
		}

		parasT = append(parasT, argT)
	}

	lenT := len(parasT)
	cntT := 0
	
	for {
		if cntT >= lenT {
			break
		}
		
		if cntT + 1 >= lenT {
			break
		}
		
		mapT[parasT[cntT]] = parasT[cntT + 1]
		
		cntT += 2
	}
	
	optsT = append(optsT, mapT)

	rs := tk.GetWeb(urlT, optsT...)
	if tk.IsErrX(rs) {
		return NewCommonErrorWithPos(c, "failed to send command to GUI server: %v", tk.GetErrStrX(rs)), nil
	}

	return ConvertToObject(rs), nil
}

func builtinArrayContainsFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
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

func builtinStrToTimeFunc(c Call) (Object, error) {
	args := c.GetArgs()

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

	strT := args[0].String()

	rsT, errT := tk.StrToTimeByFormat(strT, formatT)

	if errT != nil {
		return NewCommonError("time parse failed: %v", errT), nil
	}

	return &Time{Value: rsT}, nil
}

func BuiltinDealStrFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonError("not enough parameters"), nil
	}

	strT := args[0].String()

	if strings.HasPrefix(strT, "HEX_") {
		strT = strT[4:]

		buf, errT := hex.DecodeString(strT)
		if errT != nil {
			return NewCommonError("failed decode hex: %v", errT), nil
		}

		return &String{Value: string(buf)}, nil
	} else if strings.HasPrefix(strT, "//TXHEX#") {
		strT = strT[8:]

		buf, errT := hex.DecodeString(strT)
		if errT != nil {
			return NewCommonError("failed decode hex: %v", errT), nil
		}

		return &String{Value: string(buf)}, nil
	} else if strings.HasPrefix(strT, "//TXTE#") {
		codeT := ""

		if len(args) > 1 {
			codeT = args[1].String()
		}

		return &String{Value: tk.DecryptStringByTXTE(strT[7:], codeT)}, nil
	} else if strings.HasPrefix(strT, "//TXDEF#") || strings.HasPrefix(strT, "740404") {
		codeT := ""

		if len(args) > 1 {
			codeT = args[1].String()
		}

		return &String{Value: tk.DecryptStringByTXDEF(strT, codeT)}, nil
	} else if strings.HasPrefix(strT, "//TXRR#") {
		strT = strT[7:]

		codeT := "char"

		if len(args) > 1 {
			codeT = args[1].String()
		}

		if strings.HasPrefix(strT, "//TXDEF#") {
			strT = tk.DecryptStringByTXDEF(strings.TrimSpace(strT[8:]), codeT)
		}

		strT = strings.ReplaceAll(strT, "TX_secureCode_XT", tk.EncryptStringByTXDEF(codeT, "char"))

		if strings.HasPrefix(strT, "http") {
			strT = tk.ToStr(tk.GetWeb(strings.TrimSpace(strT)))
		}

		return &String{Value: strT}, nil
	}

	return &String{Value: strT}, nil
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

func builtinStrSplitNFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 3 {
		return NewCommonError("not enough parameters"), nil
	}

	nv1 := args[0].String()

	nv2 := args[1].String()

	limitT := ToGoIntWithDefault(args[2], -1)

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

func builtinMathExpFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonError("not enough parameters"), nil
	}

	switch nv := args[0].(type) {
	case Byte:
		return Float(math.Exp(float64(nv))), nil
	case Char:
		return Float(math.Exp(float64(nv))), nil
	case Int:
		return Float(math.Exp(float64(nv))), nil
	case Uint:
		return Float(math.Exp(float64(nv))), nil
	case Float:
		return Float(math.Exp(float64(nv))), nil
	case *BigInt:
		return &BigFloat{Value: tk.BigFloatExp(big.NewFloat(0).SetInt(nv.Value))}, nil
	case *BigFloat:
		return &BigFloat{Value: tk.BigFloatExp(big.NewFloat(0).Set(nv.Value))}, nil
	}

	return NewCommonErrorWithPos(c, "unsupported type: %T", args[0]), nil
}

func builtinMathLogFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonError("not enough parameters"), nil
	}

	switch nv := args[0].(type) {
	case Byte:
		return Float(math.Log(float64(nv))), nil
	case Char:
		return Float(math.Log(float64(nv))), nil
	case Int:
		return Float(math.Log(float64(nv))), nil
	case Uint:
		return Float(math.Log(float64(nv))), nil
	case Float:
		return Float(math.Log(float64(nv))), nil
	case *BigInt:
		return &BigFloat{Value: tk.BigFloatLog(big.NewFloat(0).SetInt(nv.Value))}, nil
	case *BigFloat:
		return &BigFloat{Value: tk.BigFloatLog(big.NewFloat(0).Set(nv.Value))}, nil
	}

	return NewCommonErrorWithPos(c, "unsupported type: %T", args[0]), nil
}

func builtinMathLog10Func(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonError("not enough parameters"), nil
	}

	switch nv := args[0].(type) {
	case Byte:
		return Float(math.Log10(float64(nv))), nil
	case Char:
		return Float(math.Log10(float64(nv))), nil
	case Int:
		return Float(math.Log10(float64(nv))), nil
	case Uint:
		return Float(math.Log10(float64(nv))), nil
	case Float:
		return Float(math.Log10(float64(nv))), nil
	}

	return NewCommonErrorWithPos(c, "unsupported type: %T", args[0]), nil
}

func builtinMathPowFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	switch nv := args[0].(type) {
	case Byte:
		return Float(math.Pow(float64(nv), tk.ToFloat(ConvertFromObject(args[1]), 0.0))), nil
	case Char:
		return Float(math.Pow(float64(nv), tk.ToFloat(ConvertFromObject(args[1]), 0.0))), nil
	case Int:
		return Float(math.Pow(float64(nv), tk.ToFloat(ConvertFromObject(args[1]), 0.0))), nil
	case Uint:
		return Float(math.Pow(float64(nv), tk.ToFloat(ConvertFromObject(args[1]), 0.0))), nil
	case Float:
		return Float(math.Pow(float64(nv), tk.ToFloat(ConvertFromObject(args[1]), 0.0))), nil
	case *BigInt:
		return &BigFloat{Value: tk.BigFloatPower(big.NewFloat(0).SetInt(nv.Value), big.NewFloat(0).SetFloat64(tk.ToFloat(ConvertFromObject(args[1]), 0.0)))}, nil
	case *BigFloat:
		return &BigFloat{Value: tk.BigFloatPower(big.NewFloat(0).Set(nv.Value), big.NewFloat(0).SetFloat64(tk.ToFloat(ConvertFromObject(args[1]), 0.0)))}, nil
	}

	return NewCommonErrorWithPos(c, "unsupported type: %T", args[0]), nil
}

func builtinMathAbsFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonError("not enough parameters"), nil
	}

	switch nv := args[0].(type) {
	case Byte:
		if nv < 0 {
			return -nv, nil
		}

		return nv, nil
	case Char:
		if nv < 0 {
			return -nv, nil
		}

		return nv, nil
	case Int:
		if nv < 0 {
			return -nv, nil
		}

		return nv, nil
	case Uint:
		if nv < 0 {
			return -nv, nil
		}

		return nv, nil
	case Float:
		if nv < 0 {
			return -nv, nil
		}

		return nv, nil
	case *BigInt:
		if nv.Value.Cmp(big.NewInt(int64(0))) < 0 {
			return &BigInt{Value: big.NewInt(int64(0)).Sub(big.NewInt(int64(0)), nv.Value)}, nil
		}

		return nv, nil
	case *BigFloat:
		if nv.Value.Cmp(big.NewFloat(0.0)) < 0 {
			return &BigFloat{Value: big.NewFloat(0.0).Sub(big.NewFloat(0.0), nv.Value)}, nil
		}

		return nv, nil
	}

	return NewCommonErrorWithPos(c, "unsupported type: %T", args[0]), nil
}

func builtinHttpHandlerFunc(c Call) (Object, error) {
	return NewHttpHandler(c)
}

func builtinHttpReqFunc(c Call) (Object, error) {
	return NewHttpReq(c)
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
		var writerT io.Writer = nil

		nv2, ok := args[0].(*Writer)

		if ok {
			writerT = nv2.Value
		} else {
			nv3, ok := args[0].(*File)

			if ok {
				writerT = nv3.Value
			}
		}

		if writerT == nil {
			return NewCommonErrorWithPos(c, "invalid object type: (%T)%v", args[0], args[0]), nil
		}

		writerT.Write([]byte(fmt.Sprintf("writeRespHeader: %v\n", args[1].String())))

		return Undefined, nil

		// return NewCommonError("invalid parameter 1 type: (%T)%v", args[0], args[0]), nil
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

func builtinImageToAsciiFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonError("not enough parameters"), nil
	}

	v, ok := args[0].(*Image)
	if !ok {
		return NewCommonError("invalid parameter 1 type: (%T)%v", args[0], args[0]), nil
	}

	vargsT := args[1:]

	widthT := tk.StrToInt(GetSwitchFromObjects(vargsT, "-width=", "0"), 0)
	heightT := tk.StrToInt(GetSwitchFromObjects(vargsT, "-height=", "0"), 0)

	imageT := tk.ImageToAscii(v.Value, widthT, heightT)

	return ConvertToObject(imageT.([]string)), nil
}

func builtinGetImageInfoFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonError("not enough parameters"), nil
	}

	v, ok := args[0].(*Image)
	if !ok {
		return NewCommonError("invalid parameter 1 type: (%T)%v", args[0], args[0]), nil
	}

	// vs := args[1:]

	rsT := make(Map)

	rsT["width"] = ToIntObject(v.Value.Bounds().Max.X - v.Value.Bounds().Min.X)
	rsT["height"] = ToIntObject(v.Value.Bounds().Max.Y - v.Value.Bounds().Min.Y)

	return rsT, nil
}

func builtinStrToRgbaFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonError("not enough parameters"), nil
	}

	v1 := args[0].String()

	r, g, b, a := tk.ParseHexColor(v1)

	rsT := make(Map)

	rsT["r"] = ToIntObject(r)
	rsT["g"] = ToIntObject(g)
	rsT["b"] = ToIntObject(b)
	rsT["a"] = ToIntObject(a)

	return rsT, nil
}

func builtinEncodeImageFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonError("not enough parameters"), nil
	}

	v1, ok := args[0].(*Image)
	if !ok {
		return NewCommonError("invalid parameter 1 type: (%T)%v", args[0], args[0]), nil
	}

	var writerT io.Writer

	switch v2 := args[1].(type) {
	case *Writer:
		writerT = v2.Value
	case *File:
		writerT = v2.Value
	case *StringBuilder:
		writerT = v2.Value
	case *BytesBuffer:
		writerT = v2.Value
	default:
		return NewCommonError("invalid parameter 2 type: (%T)%v", args[1], args[1]), nil
	}

	vs := ObjectsToS(args[2:])

	var formatT string = strings.TrimSpace(tk.GetSwitch(vs, "-type=", ".png"))

	if len(formatT) < 1 {
		formatT = ".png"
	} else {
		formatT = strings.ToLower(formatT)
	}

	var errT error

	formatT = strings.Trim(formatT, ".")

	switch formatT {
	case "", "png":
		errT = png.Encode(writerT, v1.Value)
	case "jpg", "jpeg":
		errT = jpeg.Encode(writerT, v1.Value, nil)
	case "gif":
		errT = gif.Encode(writerT, v1.Value, nil)
	case "bmp":
		errT = bmp.Encode(writerT, v1.Value)
	default:
		errT = png.Encode(writerT, v1.Value)
	}

	if errT != nil {
		return NewCommonError("invalid parameter 2 type: (%T)%v", args[1], args[1]), nil
	}

	return Undefined, nil
}

func builtinEncodeBytesInImageFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return NewCommonError("not enough parameters"), nil
	}

	v1, ok := args[0].(Bytes)
	if !ok {
		return NewCommonError("invalid parameter 1 type: (%T)%v", args[0], args[0]), nil
	}

	v2, ok := args[1].(*Image)
	if !ok {
		return NewCommonError("invalid parameter 2 type: (%T)%v", args[1], args[1]), nil
	}

	vs := ObjectsToS(args[2:])

	rs := tk.EncodeBytesInImage([]byte(v1), v2.Value, vs...)

	nv1, ok := rs.(error)

	if ok {
		return NewCommonErrorWithPos(c, "%v", nv1), nil
	}

	return Bytes(rs.([]byte)), nil
}

func builtinDecodeBytesFromImageFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonError("not enough parameters"), nil
	}

	v1, ok := args[0].(*Image)
	if !ok {
		return NewCommonError("invalid parameter 1 type: (%T)%v", args[0], args[0]), nil
	}

	vs := ObjectsToS(args[1:])

	rs := tk.DecodeBytesFromImage(v1.Value, vs...)

	return Bytes(rs), nil
}

func builtinDrawImageOnImageFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return NewCommonError("not enough parameters"), nil
	}

	v1, ok := args[0].(*Image)
	if !ok {
		return NewCommonError("invalid parameter 1 type: (%T)%v", args[0], args[0]), nil
	}

	v2, ok := args[1].(*Image)
	if !ok {
		return NewCommonError("invalid parameter 2 type: (%T)%v", args[1], args[1]), nil
	}

	widthT := v2.Value.Bounds().Max.X - v2.Value.Bounds().Min.X
	height := v2.Value.Bounds().Max.Y - v2.Value.Bounds().Min.Y

	dc := gg.NewContext(widthT, height)

	// if err := dc.LoadFontFace(`C:\Windows\Fonts\simsun.ttc`, 48); err != nil {
	// 	panic(err)
	// }

	dc.DrawImage(v2.Value, 0, 0)

	vs := ObjectsToS(args[2:])

	x := tk.ToInt(tk.GetSwitch(vs, "-x=", "0"), 0)
	y := tk.ToInt(tk.GetSwitch(vs, "-y=", "0"), 0)

	dc.DrawImage(v1.Value, x, y)

	// if errT != nil {
	// 	return NewCommonError("invalid parameter 2 type: (%T)%v", args[1], args[1]), nil
	// }

	return &Image{Value: dc.Image()}, nil
}

func builtinDrawTextWrappedOnImageFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return NewCommonError("not enough parameters"), nil
	}

	v1 := args[0].String()

	v2, ok := args[1].(*Image)
	if !ok {
		return NewCommonError("invalid parameter 2 type: (%T)%v", args[1], args[1]), nil
	}

	vs := ObjectsToS(args[2:])

	widthT := v2.Value.Bounds().Max.X - v2.Value.Bounds().Min.X
	height := v2.Value.Bounds().Max.Y - v2.Value.Bounds().Min.Y

	dc := gg.NewContext(widthT, height)

	// if err := dc.LoadFontFace(`C:\Windows\Fonts\simsun.ttc`, 48); err != nil {
	// 	panic(err)
	// }

	dc.DrawImage(v2.Value, 0, 0)

	x := tk.ToFloat(tk.GetSwitch(vs, "-x=", "0"), 0.0)
	y := tk.ToFloat(tk.GetSwitch(vs, "-y=", "0"), 0.0)
	ax := tk.ToFloat(tk.GetSwitch(vs, "-ax=", "0.5"), 0.0)
	ay := tk.ToFloat(tk.GetSwitch(vs, "-ay=", "0.5"), 0.0)

	textWidthT := tk.ToFloat(tk.GetSwitch(vs, "-width=", tk.ToStr(widthT)), 0.0)

	lineSpacingT := tk.ToFloat(tk.GetSwitch(vs, "-lineSpacing=", "0.0"), 0.0)

	alignStrT := strings.TrimSpace(tk.GetSwitch(vs, "-align=", "center"))

	colorT := strings.TrimSpace(tk.GetSwitch(vs, "-color=", `#000000`))

	r, g, b, a := tk.ParseHexColor(colorT)

	dc.SetRGBA255(r, g, b, a)

	fontPathT := tk.GetSwitch(vs, "-font=", `C:\Windows\Fonts\simsun.ttc`)

	fontSizeT := tk.ToFloat(tk.GetSwitch(vs, "-fontSize=", `16`), 0.0)

	errT := dc.LoadFontFace(fontPathT, fontSizeT)

	if errT != nil {
		return NewCommonError("failed to load font: %v", errT), nil
	}

	var alignT = gg.AlignCenter

	if alignStrT == "left" {
		alignT = gg.AlignLeft
	} else if alignStrT == "right" {
		alignT = gg.AlignRight
	}

	// tk.Pln(v1, x, y, ax, ay, textWidthT, lineSpacingT, 	alignT)

	dc.DrawStringWrapped(v1, x, y, ax, ay, textWidthT, lineSpacingT, alignT)

	// if errT != nil {
	// 	return NewCommonError("invalid parameter 2 type: (%T)%v", args[1], args[1]), nil
	// }

	return &Image{Value: dc.Image()}, nil
}

func builtinGenQrFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonError("not enough parameters"), nil
	}

	v1 := args[0].String()

	vs := ObjectsToS(args[1:])

	qrCodeT, errT := tk.GenerateQR(v1, vs...)

	if errT != nil {
		return NewCommonError("failed to generate QR code: %v", errT), nil
	}

	return &Image{Value: qrCodeT}, nil
}

func builtinAesEncryptFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return NewCommonError("not enough parameters"), nil
	}
	
	var vp []byte

	v, ok := args[0].(Bytes)
	if !ok {
		vp = []byte(args[0].String())
//		return NewCommonError("invalid parameter 1 type: (%T)%v", args[0], args[0]), nil
	} else {
		vp = []byte(v)
	}
	
	keyT := args[1].String()
	
	vs := ObjectsToS(args[2:])
	
	modeT := tk.GetSwitch(vs, "-mode=", "")

	var rs []byte
	var errT error
	
	if modeT == "cbc" {
		rs, errT = tk.AESEncryptCBC(vp, []byte(keyT))
	} else {
		rs, errT = tk.AESEncrypt(vp, []byte(keyT))
	}

	if errT != nil {
		return NewCommonErrorWithPos(c, "failed to encrypt with AES: %v", errT), nil
	}

	return String{Value: string(rs)}, nil
}


func builtinAesDecryptFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return NewCommonError("not enough parameters"), nil
	}
	
	var vp []byte

	v, ok := args[0].(Bytes)
	if !ok {
		vp = []byte(args[0].String())
//		return NewCommonError("invalid parameter 1 type: (%T)%v", args[0], args[0]), nil
	} else {
		vp = []byte(v)
	}

	keyT := args[1].String()
	
	vs := ObjectsToS(args[2:])
	
	modeT := tk.GetSwitch(vs, "-mode=", "")

	var rs []byte
	var errT error
	
	if modeT == "cbc" {
		rs, errT = tk.AESDecryptCBC(vp, []byte(keyT))
	} else {
		rs, errT = tk.AESDecrypt(vp, []byte(keyT))
	}

	if errT != nil {
		return NewCommonErrorWithPos(c, "failed to decrypt with AES: %v", errT), nil
	}

	return String{Value: string(rs)}, nil
}

func builtinSaveImageToFileFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 2 {
		return NewCommonError("not enough parameters"), nil
	}

	v, ok := args[0].(*Image)
	if !ok {
		return NewCommonError("invalid parameter 1 type: (%T)%v", args[0], args[0]), nil
	}

	filePathT := args[1].String()

	vargsT := ObjectsToS(args[2:])

	errT := tk.SaveImageToFile(v.Value, filePathT, vargsT...)

	return ConvertToObject(errT), nil
}

func builtinSaveImageToBytesFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonError("not enough parameters"), nil
	}

	v, ok := args[0].(*Image)
	if !ok {
		return NewCommonError("invalid parameter 1 type: (%T)%v", args[0], args[0]), nil
	}

	vargsT := ObjectsToS(args[1:])

	rsT := tk.SaveImageToBytes(v.Value, vargsT...)

	return ConvertToObject(rsT), nil
}

func builtinResizeImageFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonError("not enough parameters"), nil
	}

	v, ok := args[0].(*Image)
	if !ok {
		return NewCommonError("invalid parameter 1 type: (%T)%v", args[0], args[0]), nil
	}

	vargsT := args[1:]

	widthT := tk.StrToInt(GetSwitchFromObjects(vargsT, "-width=", "0"), 0)
	heightT := tk.StrToInt(GetSwitchFromObjects(vargsT, "-height=", "0"), 0)

	imageT := tk.ResizeImageQuick(v.Value, widthT, heightT)

	return ConvertToObject(imageT), nil
}

func builtinPlotDataToStrFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonError("not enough parameters"), nil
	}

	nv1, ok := args[0].(Array)
	if !ok {
		return NewCommonError("invalid parameter 1 type: (%T)%v", args[0], args[0]), nil
	}

	vargsT := ObjectsToS(args[1:])

	var optionsT []asciigraph.Option = make([]asciigraph.Option, 0)

	captionT := tk.GetSwitch(vargsT, "-caption=", "TXERROR:nil")

	if captionT != "TXERROR:nil" {
		optionsT = append(optionsT, asciigraph.Caption(captionT))
	}

	widthT := tk.StrToInt(tk.GetSwitch(vargsT, "-width=", "0"), 0)

	if widthT > 0 {
		optionsT = append(optionsT, asciigraph.Width(widthT))
	}

	heightT := tk.StrToInt(tk.GetSwitch(vargsT, "-height=", "0"), 0)

	if heightT > 0 {
		optionsT = append(optionsT, asciigraph.Height(heightT))
	}

	axisColorT := tk.StrToInt(tk.GetSwitch(vargsT, "-axisColor=", "0"), 0)

	if axisColorT > 0 {
		optionsT = append(optionsT, asciigraph.AxisColor(asciigraph.AnsiColor(axisColorT)))
	}

	captionColorT := tk.StrToInt(tk.GetSwitch(vargsT, "-captionColor=", "0"), 0)

	if captionColorT > 0 {
		optionsT = append(optionsT, asciigraph.CaptionColor(asciigraph.AnsiColor(captionColorT)))
	}

	labelColorT := tk.StrToInt(tk.GetSwitch(vargsT, "-labelColor=", "0"), 0)

	if labelColorT > 0 {
		optionsT = append(optionsT, asciigraph.LabelColor(asciigraph.AnsiColor(labelColorT)))
	}

	seriesColorStrT := strings.TrimSpace(tk.GetSwitch(vargsT, "-seriesColor=", ""))

	if seriesColorStrT != "" {
		listT := strings.Split(seriesColorStrT, ",")

		var seriesColorAnsiT = make([]asciigraph.AnsiColor, 0, len(listT))

		for _, v := range listT {
			v = strings.TrimSpace(v)

			if v != "" {
				seriesColorAnsiT = append(seriesColorAnsiT, asciigraph.AnsiColor(tk.StrToInt(v, 0)))
			}
		}

		if len(seriesColorAnsiT) > 0 {
			optionsT = append(optionsT, asciigraph.SeriesColors(seriesColorAnsiT...))
		}
	}

	minStrT := tk.GetSwitch(vargsT, "-min=", "TXERROR:nil")

	if minStrT != "TXERROR:nil" {
		optionsT = append(optionsT, asciigraph.LowerBound(tk.StrToFloat64(minStrT, 0)))
	}

	maxStrT := tk.GetSwitch(vargsT, "-max=", "TXERROR:nil")

	if maxStrT != "TXERROR:nil" {
		optionsT = append(optionsT, asciigraph.UpperBound(tk.StrToFloat64(maxStrT, 0)))
	}

	precisionT := tk.StrToInt(tk.GetSwitch(vargsT, "-precision=", "0"), 0)

	if precisionT > 0 {
		optionsT = append(optionsT, asciigraph.Precision(uint(precisionT)))
	}

	OffsetStrT := tk.GetSwitch(vargsT, "-offset=", "TXERROR:nil")

	if OffsetStrT != "TXERROR:nil" {
		optionsT = append(optionsT, asciigraph.Offset(tk.StrToInt(OffsetStrT, 0)))
	}

	var floatsT [][]float64 = make([][]float64, 0, len(nv1))

	for _, v := range nv1 {
		nv1i, ok := v.(Array)

		if !ok {
			continue
		}

		floatsiT := make([]float64, 0, len(nv1i))

		for _, jv := range nv1i {
			floatsiT = append(floatsiT, ToFloatQuick(jv))
		}

		floatsT = append(floatsT, floatsiT)
	}

	rs := asciigraph.PlotMany(floatsT, optionsT...)

	return ConvertToObject(rs), nil
}

func builtinPlotLoadFontFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonError("not enough parameters"), nil
	}

	nv1 := args[0].String()

	var fontFamilyT string

	if len(args) > 1 {
		fontFamilyT = args[1].String()
	} else {
		fontFamilyT = nv1
	}

	var setAsDefaultT bool = false

	if len(args) > 2 {
		nv2, ok := args[2].(Bool)

		if ok {
			setAsDefaultT = bool(nv2)
		}
	}

	fontBufT := tk.LoadBytes(nv1)

	if fontBufT != nil {
		errT := charts.InstallFont(fontFamilyT, fontBufT)

		if errT != nil {
			return NewCommonError("failed to load font: %v", errT), nil
		}

		if setAsDefaultT {
			font, _ := charts.GetFont(fontFamilyT)
			charts.SetDefaultFont(font)
		}
	}

	return Undefined, nil
}

func builtinPlotDataToImageFunc(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonError("not enough parameters"), nil
	}

	nv1, ok := args[0].(Array)
	if !ok {
		return NewCommonError("invalid parameter 1 type: (%T)%v", args[0], args[0]), nil
	}

	vargsT := ObjectsToS(args[1:])

	outputT := tk.GetSwitch(vargsT, "-output=", "") // empty or bytes, file(expect fileName arg), image

	var optionsT []charts.OptionFunc = make([]charts.OptionFunc, 0)

	imageTypeT := tk.GetSwitch(vargsT, "-imageType=", "png")

	if imageTypeT == "svg" {
		optionsT = append(optionsT, charts.SVGTypeOption())
	} else {
		optionsT = append(optionsT, charts.PNGTypeOption())
	}

	// chart option
	showSymbolFlagT := charts.FalseFlag()

	if strings.ToLower(strings.TrimSpace(tk.GetSwitch(vargsT, "-showSymbol=", "true"))) == "true" {
		showSymbolFlagT = charts.TrueFlag()
	}

	lineStrokeWidthT := tk.StrToFloat64(tk.GetSwitch(vargsT, "-lineStrokeWidth=", "1"), 1.0)

	valueFormatterT := tk.GetSwitch(vargsT, "-valueFormatter=", "%.0f")

	optionCollectionT := func(opt *charts.ChartOption) {
		// opt.Legend.Padding = charts.Box{
		// 	Top:    5,
		// 	Bottom: 10,
		// }
		opt.SymbolShow = showSymbolFlagT
		opt.LineStrokeWidth = lineStrokeWidthT
		opt.ValueFormatter = func(f float64) string {
			return fmt.Sprintf(valueFormatterT, f)
		}
	}

	optionsT = append(optionsT, optionCollectionT)

	// for convenient font setting, usually one chart only
	fontStrT := tk.GetSwitch(vargsT, "-fontFile=", "TXERROR:nil")

	if fontStrT != "TXERROR:nil" {
		fontBufT := tk.LoadBytes(fontStrT)
		if fontBufT != nil {
			errT := charts.InstallFont(fontStrT, fontBufT)
			if errT == nil {
				font, _ := charts.GetFont(fontStrT)
				charts.SetDefaultFont(font)
			}
		}
	}

	paddingStrT := strings.TrimSpace(tk.GetSwitch(vargsT, "-padding=", ""))

	if paddingStrT != "" {
		listT := strings.Split(paddingStrT, ",")

		var paddingDataT = make([]int, 4)

		for i, v := range listT {
			// v = strings.TrimSpace(v)

			paddingDataT[i] = tk.StrToInt(v, 0)
		}

		optionsT = append(optionsT, charts.PaddingOptionFunc(charts.Box{Top: paddingDataT[0], Right: paddingDataT[1], Bottom: paddingDataT[2], Left: paddingDataT[3]}))
	}

	backgroundColorStrT := strings.TrimSpace(tk.GetSwitch(vargsT, "-backgroundColor=", ""))

	if backgroundColorStrT != "" {
		backgroundColorStrT = strings.TrimPrefix(backgroundColorStrT, "#")

		r1, g1, b1, a1 := tk.ParseHexColor(backgroundColorStrT)

		optionsT = append(optionsT, charts.BackgroundColorOptionFunc(drawing.Color{R: uint8(r1), G: uint8(g1), B: uint8(b1), A: uint8(a1)}))
	}

	captionT := tk.GetSwitch(vargsT, "-caption=", "")

	subCaptionT := tk.GetSwitch(vargsT, "-subCaption=", "")

	var captionFontT *truetype.Font = nil

	captionFontStrT := tk.GetSwitch(vargsT, "-captionFont=", "TXERROR:nil")

	if captionFontStrT != "TXERROR:nil" {
		fontT, errT := charts.GetFont(captionFontStrT)

		if errT != nil {
			fontT, errT = charts.GetDefaultFont()

			if errT != nil {
				fontT = nil
			}
		}

		captionFontT = fontT
	}

	captionFontSizeT := tk.StrToFloat64(tk.GetSwitch(vargsT, "-captionFontSize=", "0"), 0)

	subCaptionFontSizeT := tk.StrToFloat64(tk.GetSwitch(vargsT, "-subCaptionFontSize=", "0"), 0)

	captionColorStrT := strings.TrimPrefix(strings.TrimSpace(tk.GetSwitch(vargsT, "-captionColor=", "")), "#")

	var captionColorT drawing.Color = drawing.Color{}

	if captionColorStrT != "" {
		r1, g1, b1, a1 := tk.ParseHexColor(captionColorStrT)

		captionColorT = drawing.Color{R: uint8(r1), G: uint8(g1), B: uint8(b1), A: uint8(a1)}
	}

	subCaptionColorStrT := strings.TrimPrefix(strings.TrimSpace(tk.GetSwitch(vargsT, "-subCaptionColor=", "")), "#")

	var subCaptionColorT drawing.Color = drawing.Color{}

	if subCaptionColorStrT != "" {
		r1, g1, b1, a1 := tk.ParseHexColor(subCaptionColorStrT)

		subCaptionColorT = drawing.Color{R: uint8(r1), G: uint8(g1), B: uint8(b1), A: uint8(a1)}
	}

	captionLeftT := tk.GetSwitch(vargsT, "-captionLeft=", "center")
	captionTopT := tk.GetSwitch(vargsT, "-captionTop=", "")

	titleOptionT := charts.TitleOption{
		Text:             captionT,
		Subtext:          subCaptionT,
		Left:             captionLeftT,
		Top:              captionTopT,
		Font:             captionFontT,
		FontSize:         captionFontSizeT,
		FontColor:        captionColorT,
		SubtextFontSize:  subCaptionFontSizeT,
		SubtextFontColor: subCaptionColorT,
		// Theme: themeT,
	}

	optionsT = append(optionsT, charts.TitleOptionFunc(titleOptionT))

	widthT := tk.StrToInt(tk.GetSwitch(vargsT, "-width=", "0"), 0)

	if widthT > 0 {
		optionsT = append(optionsT, charts.WidthOptionFunc(widthT))
	}

	heightT := tk.StrToInt(tk.GetSwitch(vargsT, "-height=", "0"), 0)

	if heightT > 0 {
		optionsT = append(optionsT, charts.HeightOptionFunc(heightT))
	}

	showYAxisFlagT := charts.FalseFlag()

	if strings.ToLower(strings.TrimSpace(tk.GetSwitch(vargsT, "-showYAxis=", "true"))) == "true" {
		showYAxisFlagT = charts.TrueFlag()
	}

	yAxisPosT := strings.ToLower(strings.TrimSpace(tk.GetSwitch(vargsT, "-yAxisPos=", "left")))

	showYAxisSplitLineFlagT := charts.FalseFlag()

	if strings.ToLower(strings.TrimSpace(tk.GetSwitch(vargsT, "-showYAxisSplitLine=", "false"))) == "true" {
		showYAxisSplitLineFlagT = charts.TrueFlag()
	}

	yAxisFontSizeT := tk.StrToFloat64(tk.GetSwitch(vargsT, "-yAxisFontSize=", "0"), 0)

	yAxisColorStrT := strings.TrimPrefix(strings.TrimSpace(tk.GetSwitch(vargsT, "-yAxisColor=", "")), "#")

	var yAxisColorT drawing.Color = drawing.Color{}

	if yAxisColorStrT != "" {
		r1, g1, b1, a1 := tk.ParseHexColor(yAxisColorStrT)

		yAxisColorT = drawing.Color{R: uint8(r1), G: uint8(g1), B: uint8(b1), A: uint8(a1)}
	}

	yAxisFontColorStrT := strings.TrimPrefix(strings.TrimSpace(tk.GetSwitch(vargsT, "-yAxisFontColor=", "")), "#")

	var yAxisFontColorT drawing.Color = drawing.Color{}

	if yAxisFontColorStrT != "" {
		r1, g1, b1, a1 := tk.ParseHexColor(yAxisFontColorStrT)

		yAxisFontColorT = drawing.Color{R: uint8(r1), G: uint8(g1), B: uint8(b1), A: uint8(a1)}
	}

	minStrT := tk.GetSwitch(vargsT, "-min=", "TXERROR:nil")

	var minValueT *float64 = nil

	if minStrT != "TXERROR:nil" {
		minValueHolderT := tk.StrToFloat64(minStrT, 0)
		minValueT = &minValueHolderT
	}

	maxStrT := tk.GetSwitch(vargsT, "-max=", "TXERROR:nil")

	var maxValueT *float64 = nil

	if maxStrT != "TXERROR:nil" {
		maxValueHolderT := tk.StrToFloat64(maxStrT, 0)
		maxValueT = &maxValueHolderT
	}

	// themeT := charts.NewTheme("yAxis01")

	// themeT.SetAxisStrokeColor(yAxisColorT)
	// themeT.SetBackgroundColor(drawing.Color{R: uint8(5), G: uint8(200), B: uint8(20), A: uint8(80)})
	// themeT.SetTextColor(drawing.Color{R: uint8(50), G: uint8(20), B: uint8(180), A: uint8(20)})

	var yAxisFontT *truetype.Font = nil

	yAxisFontStrT := tk.GetSwitch(vargsT, "-yAxisFont=", "TXERROR:nil")

	if yAxisFontStrT != "TXERROR:nil" {
		fontT, errT := charts.GetFont(yAxisFontStrT)

		if errT != nil {
			fontT, errT = charts.GetDefaultFont()

			if errT != nil {
				fontT = nil
			}
		}

		yAxisFontT = fontT
	}

	yAxisOptionT := charts.YAxisOption{
		Position:      yAxisPosT,
		SplitLineShow: showYAxisSplitLineFlagT,
		Show:          showYAxisFlagT,
		Min:           minValueT,
		Max:           maxValueT,
		Color:         yAxisColorT,
		// Theme: themeT,
		Font:      yAxisFontT,
		FontSize:  yAxisFontSizeT,
		FontColor: yAxisFontColorT,
	}

	optionsT = append(optionsT, charts.YAxisOptionFunc(yAxisOptionT))

	// boxStrT := strings.TrimSpace(tk.GetSwitch(vargsT, "-box=", ""))

	// if boxStrT != "" {
	// 	listT := strings.Split(boxStrT, ",")

	// 	var boxDataT = make([]int, 4)

	// 	for i, v := range listT {
	// 		// v = strings.TrimSpace(v)

	// 		boxDataT[i] = tk.StrToInt(v, 0)
	// 	}

	// 	optionsT = append(optionsT, charts.BoxOptionFunc(charts.Box{Top: boxDataT[0], Right: boxDataT[1], Bottom: boxDataT[2], Left: boxDataT[3]}))
	// }

	var floatsT [][]float64 = make([][]float64, 0, len(nv1))

	for _, v := range nv1 {
		nv1i, ok := v.(Array)

		if !ok {
			continue
		}

		floatsiT := make([]float64, 0, len(nv1i))

		for _, jv := range nv1i {
			if IsUndefInternal(jv) {
				floatsiT = append(floatsiT, charts.GetNullValue())
			} else {
				floatsiT = append(floatsiT, ToFloatQuick(jv))
			}
		}

		floatsT = append(floatsT, floatsiT)
	}

	// tk.Plv(floatsT)
	// tk.Plv(optionsT)

	var legendDataT []string

	legendStrT := strings.TrimSpace(tk.GetSwitch(vargsT, "-legendData=", ""))

	if legendStrT != "" {
		// posStrT := ""
		// firstListT := strings.Split(legendStrT, "|")

		// if len(firstListT) > 1 {
		// 	posStrT = firstListT[1]
		// }

		listT := strings.Split(legendStrT, ",")

		legendDataT = make([]string, 0, len(listT))

		if legendStrT == "auto" {
			for i := 0; i < len(floatsT); i++ {
				legendDataT = append(legendDataT, fmt.Sprintf("series %v", i+1))
			}
		} else {
			for _, v := range listT {
				// v = strings.TrimSpace(v)

				legendDataT = append(legendDataT, v)
			}
		}

		// if len(legendDataT) > 0 {
		// 	optionsT = append(optionsT, charts.LegendLabelsOptionFunc(legendDataT, posStrT))
		// }
	}

	showLegendFlagT := charts.FalseFlag()

	if strings.ToLower(strings.TrimSpace(tk.GetSwitch(vargsT, "-showLegend=", "true"))) == "true" {
		showLegendFlagT = charts.TrueFlag()
	}

	legendFontSizeT := tk.StrToFloat64(tk.GetSwitch(vargsT, "-legendFontSize=", "0"), 0)

	legendFontColorStrT := strings.TrimPrefix(strings.TrimSpace(tk.GetSwitch(vargsT, "-legendFontColor=", "")), "#")

	var legendFontColorT drawing.Color = drawing.Color{}

	if legendFontColorStrT != "" {
		r1, g1, b1, a1 := tk.ParseHexColor(legendFontColorStrT)

		legendFontColorT = drawing.Color{R: uint8(r1), G: uint8(g1), B: uint8(b1), A: uint8(a1)}
	}

	legendLeftT := tk.GetSwitch(vargsT, "-legendLeft=", "center")
	legendTopT := tk.GetSwitch(vargsT, "-legendTop=", "")

	legendAlignT := tk.GetSwitch(vargsT, "-legendAlign=", "left")
	legendOrientT := tk.GetSwitch(vargsT, "-legendOrient=", "horizontal")

	legendIconT := tk.GetSwitch(vargsT, "-legendIcon=", "")

	legendPaddingStrT := strings.TrimSpace(tk.GetSwitch(vargsT, "-legendPadding=", ""))

	legendPaddingBoxT := charts.Box{}

	if legendPaddingStrT != "" {
		listT := strings.Split(legendPaddingStrT, ",")

		var boxDataT = make([]int, 4)

		for i, v := range listT {
			// v = strings.TrimSpace(v)

			boxDataT[i] = tk.StrToInt(v, 0)
		}

		legendPaddingBoxT = charts.Box{Top: boxDataT[0], Right: boxDataT[1], Bottom: boxDataT[2], Left: boxDataT[3]}
	}

	legendOptionT := charts.LegendOption{
		Show:      showLegendFlagT,
		Left:      legendLeftT,
		Top:       legendTopT,
		Align:     legendAlignT,
		Orient:    legendOrientT,
		Icon:      legendIconT,
		FontSize:  legendFontSizeT,
		FontColor: legendFontColorT,
		// Theme: themeT,
		Padding: legendPaddingBoxT,
		Data:    legendDataT,
	}

	optionsT = append(optionsT, charts.LegendOptionFunc(legendOptionT))

	showXAxisFlagT := charts.FalseFlag()

	if strings.ToLower(strings.TrimSpace(tk.GetSwitch(vargsT, "-showXAxis=", "true"))) == "true" {
		showXAxisFlagT = charts.TrueFlag()
	}

	var xAxisDataT []string

	xAxisDataStrT := strings.TrimSpace(tk.GetSwitch(vargsT, "-xAxisData=", ""))

	if xAxisDataStrT != "" {
		listT := strings.Split(xAxisDataStrT, ",")

		xAxisDataT = make([]string, 0, len(listT))

		for _, v := range listT {
			v = strings.TrimSpace(v)

			xAxisDataT = append(xAxisDataT, v)
		}

		// if len(xAxisDataT) > 0 {
		// 	optionsT = append(optionsT, charts.XAxisDataOptionFunc(xAxisDataT, &gapFlagT))
		// }
	} else if len(floatsT) > 0 {
		maxLenT := 0

		for _, v := range floatsT {
			if len(v) > maxLenT {
				maxLenT = len(v)
			}
		}

		xAxisDataT = make([]string, 0, maxLenT)

		for i := 0; i < maxLenT; i++ {
			xAxisDataT = append(xAxisDataT, fmt.Sprintf("%v", i))
		}

		// if len(xAxisDataT) > 0 {
		// 	optionsT = append(optionsT, charts.XAxisDataOptionFunc(xAxisDataT, &gapFlagT))
		// }

	}

	boundaryGapT := charts.FalseFlag()

	if strings.ToLower(strings.TrimSpace(tk.GetSwitch(vargsT, "-boundaryGap=", "false"))) == "true" {
		boundaryGapT = charts.TrueFlag()
	}

	var xAxisFontT *truetype.Font = nil

	xAxisFontStrT := tk.GetSwitch(vargsT, "-xAxisFont=", "TXERROR:nil")

	if xAxisFontStrT != "TXERROR:nil" {
		fontT, errT := charts.GetFont(xAxisFontStrT)

		if errT != nil {
			fontT, errT = charts.GetDefaultFont()

			if errT != nil {
				fontT = nil
			}
		}

		xAxisFontT = fontT
	}

	xAxisPosT := strings.ToLower(strings.TrimSpace(tk.GetSwitch(vargsT, "-xAxisPos=", "bottom")))

	xAxisColorStrT := strings.TrimPrefix(strings.TrimSpace(tk.GetSwitch(vargsT, "-xAxisColor=", "")), "#")

	var xAxisColorT drawing.Color = drawing.Color{}

	if xAxisColorStrT != "" {
		r1, g1, b1, a1 := tk.ParseHexColor(xAxisColorStrT)

		xAxisColorT = drawing.Color{R: uint8(r1), G: uint8(g1), B: uint8(b1), A: uint8(a1)}
	}

	xAxisFontSizeT := tk.StrToFloat64(tk.GetSwitch(vargsT, "-xAxisFontSize=", "0"), 0)

	xAxisFontColorStrT := strings.TrimPrefix(strings.TrimSpace(tk.GetSwitch(vargsT, "-xAxisFontColor=", "")), "#")

	var xAxisFontColorT drawing.Color = drawing.Color{}

	if xAxisFontColorStrT != "" {
		r1, g1, b1, a1 := tk.ParseHexColor(xAxisFontColorStrT)

		xAxisFontColorT = drawing.Color{R: uint8(r1), G: uint8(g1), B: uint8(b1), A: uint8(a1)}
	}

	xAxisLabelOffsetStrT := strings.TrimSpace(tk.GetSwitch(vargsT, "-xAxisLabelOffset=", ""))

	xAxisLabelOffsetBoxT := charts.Box{}

	if xAxisLabelOffsetStrT != "" {
		listT := strings.Split(xAxisLabelOffsetStrT, ",")

		var boxDataT = make([]int, 4)

		for i, v := range listT {
			// v = strings.TrimSpace(v)

			boxDataT[i] = tk.StrToInt(v, 0)
		}

		xAxisLabelOffsetBoxT = charts.Box{Top: boxDataT[0], Right: boxDataT[1], Bottom: boxDataT[2], Left: boxDataT[3]}
	}

	xAxisOptionT := charts.XAxisOption{
		Show: showXAxisFlagT,

		BoundaryGap: boundaryGapT,

		Position: xAxisPosT,

		Font: xAxisFontT,

		FontSize: xAxisFontSizeT,

		FontColor: xAxisFontColorT,

		StrokeColor: xAxisColorT,

		Data: xAxisDataT,

		LabelOffset: xAxisLabelOffsetBoxT,
	}

	optionsT = append(optionsT, charts.XAxisOptionFunc(xAxisOptionT))

	// render the chart
	p, errT := charts.LineRender(floatsT, optionsT...)

	if errT != nil {
		return ConvertToObject(errT), nil
	}

	bytesT, errT := p.Bytes()

	if errT != nil {
		return NewCommonErrorWithPos(c, "failed to generate image content: %v", errT), nil
	}

	if outputT == "" || outputT == "bytes" {
		return Bytes(bytesT), nil
	} else if outputT == "file" {
		filePathT := strings.TrimSpace(tk.GetSwitch(vargsT, "-file=", ""))

		if filePathT == "" {
			return NewCommonErrorWithPos(c, "file path not specified"), nil
		}

		errT := tk.SaveBytesToFileE(bytesT, filePathT)

		if errT != nil {
			return NewCommonErrorWithPos(c, "failed to save file: %v", errT), nil
		}

		return Undefined, nil
	} else if outputT == "image" {
		if imageTypeT != "png" {
			return NewCommonErrorWithPos(c, "image format not supported: %v", imageTypeT), nil
		}

		imageT := tk.LoadImageFromBytes(bytesT, "-type=png")

		errT, ok := imageT.(error)

		if ok {
			return NewCommonErrorWithPos(c, "failed to output image: %v", errT), nil
		}

		return &Image{Value: imageT.(image.Image)}, nil
	}

	return Bytes(bytesT), nil
}

func builtinCloseFunc(c Call) (result Object, err error) {
	defer func() {
		r := recover()

		if r != nil {
			result = NewCommonErrorWithPos(c, "failed: %v", r)
			err = nil
			return
		}
	}()

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
	} else if typeNameT == "file" {
		r1 := args[0].(*File)

		errT := r1.Value.Close()

		if errT != nil {
			return NewCommonErrorWithPos(c, "failed to close file: %v", errT), nil
		}

		return Undefined, nil
	} else if typeNameT == "excel" {
		r1 := args[0].(*Excel)

		rs := r1.GetMember("close")

		if !IsUndefInternal(rs) {
			f1 := rs.(*Function)
			return (*f1).CallEx(Call{Args: []Object{}})
		}

		errT := r1.Value.Close()

		if errT != nil {
			return NewCommonErrorWithPos(c, "failed to close: %v", errT), nil
		}

		return Undefined, nil
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

func builtinSortArrayFunc(c Call) (Object, error) {
	args := c.GetArgs()

	switch obj := args[0].(type) {
	case Array:
		var err error

		keyT := ""
		if len(args) > 1 {
			keyT = args[1].String()
		}

		descT := false
		if len(args) > 2 {
			descT = (args[2].String() == "desc") || (!(args[2].IsFalsy()))
		}

		if keyT != "" {
			sort.Slice(obj, func(i, j int) bool {
				// v1 := Undefined
				// v2 := Undefined

				m1, ok := obj[i].(Map)

				if ok {
					m2, ok := obj[j].(Map)
					if ok {
						v1, ok := m1[keyT]
						if ok {
							v2, ok := m2[keyT]

							if ok {
								v, e := v1.BinaryOp(tk.IfThenElse(descT, token.Greater, token.Less).(token.Token), v2)
								if e != nil && err == nil {
									err = e
									return false
								}
								if v != nil {
									return !v.IsFalsy()
								}
								return false
							}
						}
					}
				}

				return false
			})
		} else {
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
		}

		if err != nil {
			return nil, err
		}
		return obj, nil
	case String:
		s := []rune(obj.Value)
		sort.Slice(s, func(i, j int) bool {
			return s[i] < s[j]
		})
		return ToStringObject(s), nil
	case Bytes:
		sort.Slice(obj, func(i, j int) bool {
			return obj[i] < obj[j]
		})
		return obj, nil
	case *UndefinedType:
		return Undefined, nil
	}

	return nil, NewArgumentTypeError(
		"first",
		"array|string|bytes",
		args[0].TypeName(),
	)
}

// char add end
