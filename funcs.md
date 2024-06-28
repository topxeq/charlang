<!-- |title: The Builtin Functions of Charlang| -->

## Charlang Builtin Function Reference

### --- internal & debug related ---

// the "testBy*" functions are builtin test functions for internal use only, see testAll.char for usage examples

**testByText**: definition: testByText(strToTest string, strToCompare string, indexInteger int, scriptFileName string)

**testByStartsWith**: definition: testByStartsWith(strToTest string, strToCompare string, indexInteger int, scriptFileName string)

**testByEndsWith**: definition: testByEndsWith(strToTest string, strToCompare string, indexInteger int, scriptFileName string)

**testByContains**: definition: testByContains(strToTest string, strToCompare string, indexInteger int, scriptFileName string)

**testByReg**: definition: testByReg(strToTest string, strToCompare string, indexInteger int, scriptFileName string)

**testByRegContains**: definition: testByRegContains(strToTest string, strToCompare string, indexInteger int, scriptFileName string)

**dumpVar**: for internal debug

**debugInfo**: for internal debug

### --- infrastructure related ---

**globals**: show global variables, usage: pln(globals())

**len**: get length/size of the object, usage: a := len(array1)

**:makeArray**: for internal use

### --- data type related ---

**typeCode**: get type code of an object

**typeName**: get type name of an object

**typeOf**: get type name of an object, the same as typeName

**typeOfAny**: get type name of an object with type 'any'

**any**: create a value with type 'any', usage: a1 := any(),  a2 := any(object1)

**bool**: create a boolean value(with type 'bool'), usage: b1 := bool(),  b2 := bool(true)

**byte**

**char**

**int**

**uint**

**float**

**bigInt**

**bigFloat**

**string**

**mutableString**

**bytes**

**chars**

**error**

**time**

**stringBuilder**

**stringBuffer**

**orderedMap**

**stack**

**queue**

**excel**

**statusResult**

**seq**

**mutex**

**mux**

**httpHandler**

**reader**

**writer**

**file**

**image**: new an image object, usage: imageT := image("-width=480", "-height=640", "-color=#FF0000")

**charCode**

**gel**

**delegate**

**database**

### --- new related ---

**make**

**new**

**newEx**

**newObj**

### --- bitwise related ---

**bitNot**

**not**

### --- array/map related ---

**append**

**appendList**

**appendArray**

**appendSlice**

**delete**

**getArrayItem**

**removeItems**: inclusive

**arrayContains**

**toOrderedMap**

### --- ref/pointer related ---

**unref**

**setValueByRef**

### --- convert related ---

**toStr**

**toInt**

**toFloat**

**toTime**

**toHex**

**unhex**

**hexDecode**

**toKMG**

### --- string related ---

**trim**

**strTrim**

**strTrimStart**

**strTrimEnd**

**strTrimLeft**

**strTrimRight**

**toUpper**

**strToUpper**

**toLower**

**strToLower**

**strContains**

**strStartsWith**

**strEndsWith**

**strReplace**

**strSplit**

**strSplitLines**

**strJoin**

**strRepeat**

**strCount**

**strPad**

**strIn**

**strFindDiffPos**: return -1 if 2 strings are identical

**limitStr**

**strQuote**

**strUnquote**

**getTextSimilarity**

### --- regex related ---

**regMatch**

**regContains**

**regFindFirst**

**regFindFirstGroups**: obtain the first match of a regular expression and return a list of all matching groups, where the first item is the complete matching result and the second item is the first matching group..., usage example: result := regFindFirstGroups(str1, regex1)

**regFindAll**

**regFindAllGroups**

**regQuote**

**regReplace**

**regCount**

**regSplit**

### --- math related ---

**adjustFloat**

**mathAbs**

**mathSqrt**

### --- random related ---

**getRandomInt**

**getRandomFloat**

**getRandomStr**

**genRandomStr**

### --- time related ---

**now**

**getNowStr**

**getNowStrCompact**

**getNowTimeStamp**

**timeToTick**

**formatTime**

### --- binary/bytes related ---

**bytesStartsWith**

**bytesEndsWith**

### --- compare related ---

**compareBytes**

### --- control related ---

**isNil**

**isNilOrEmpty**

**isNilOrErr**

**isUndefined**

**isUndef**

**isBool**

**isByte**

**isChar**

**isInt**

**isUint**

**isFloat**

**isString**

**isBytes**

**isChars**

**isArray**

**isMap**

**isSyncMap**

**isError**

**isFunction**

**isCallable**

**isIterable**

**exit**

**pass**

### --- error related ---

**isErrX**

**isErr**

**getErrStrX**

**getErrStr**

**checkErrX**

**checkErr**

**errStrf**

**errf**

### --- output/print related ---

**pr**

**prf**

**pl**

**pln**

**plv**

**plt**

**plo**

**plErr**

**fatalf**

**spr**

### --- scan related ---

**sscanf**

### --- resource related ---

**getNamedValue**

**getConst**

**callNamedFunc**

**callInternalFunc**

### --- member/method related ---

**getValue**

**setValue**

**getMember**

**mb**

**setMember**

**callMethod**

**mt**

**callMethodEx**

**mtEx**

### --- open/close related ---

**close**

### --- read/write related ---

**readAllStr**

**readAllBytes**

**writeStr**

**writeBytes**

### --- encode/decode related ---

**md5**

**urlEncode**

**urlDecode**

**htmlEncode**

**htmlDecode**

**simpleEncode**

**simpleDecode**

**base64Encode**

**base64Decode**

**toJSON**

**toJson**

**fromJSON**

**fromJson**

### --- XML related ---

**xmlEncodeStr**

**xmlGetNodeStr**

### --- command-line related ---

**ifSwitchExists**

**getSwitch**

**getIntSwitch**

**getSwitches**

**getParam**

**getParams**

### --- clipboard related ---

**getClipText**

**setClipText**

### --- thread related ---

**sleep**

**lock**

**unlock**

**rLock**

**rUnlock**

**tryLock**

**tryRLock**

### --- os/system related ---

**systemCmd**

**getEnv**

**setEnv**

**getOSName**

**getOsName**

**getOSArch**

**getOsArch**

**getOSArgs**

**getOsArgs**

**getAppDir**

**getCurDir**

**getHomeDir**

**getTempDir**

**getInput**

**getChar**

### --- dir/path related ---

**joinPath**: join multiple file paths into one, equivalent to path/filepath.Join in the Go language standard library

**isDir**

**ensureMakeDirs**

**getFileList**

**genFileList**

### --- file related ---

**fileExists**

**ifFileExists**

**getFileAbs**

**getFileExt**

**getFileRel**

**extractFileDir**

**renameFile**

**removeFile**

**removeDir**

**removePath**

**loadText**

**saveText**

**appendText**

**loadBytes**

**saveBytes**

### --- compress/zip related ---

**archiveFilesToZip**: Add multiple files to a newly created zip file. The first parameter is the zip file name, with a suffix of '.zip'. Optional parameters include '-overwrite' (whether to overwrite existing files) and '-makeDirs' (whether to create a new directory as needed). Other parameters are treated as files or directories to be added, and the directory will be recursively added to the zip file. If the parameter is a list, it will be treated as a list of file names, and all files in it will be added

### --- network/web related ---

**getWeb**

**getWebBytes**

**postRequest**

**urlExists**

**isHttps**

**httpRedirect**

### --- server/service related ---

**getReqHeader**

**getReqBody**

**parseReqForm**

**parseReqFormEx**

**writeRespHeader**

**setRespHeader**

**genJSONResp**

**genJsonResp**

**genResp**

**writeResp**

**serveFile**

**getMimeType**

### --- security related ---

**genToken**

**checkToken**

**isEncrypted**

**encryptText**

**decryptText**

**encryptData**

**encryptBytes**

**decryptData**

**decryptBytes**

### --- image related ---

**loadImageFromBytes**: usage: imageT := loadImageFromBytes(bytesT, "-type=png")

**saveImageToBytes**: usage: bytesT := saveImageToBytes(imageT) or bytesT := saveImageToBytes(imageT, ".png") to save image with specified format, .jpg, .png, .gif, .bmp is supported

**loadImageFromFile**: usage: imageT := loadImageFromFile(`c:\test\abc.png`) or image2T := loadImageFromFile(`c:\test\abc.jpg`, "-type=jpg")

**saveImageToFile**: usage: errT := saveImageToFile(imageT, `c:\test\newabc.png`) or errT := saveImageToFile(imageT, `c:\test\newabc.png`, ".png") to save image with specified format, .jpg, .png, .gif, .bmp is supported

**imageToAscii**: convert an image object to colorful ASCII graph(array of string), usage: asciiT := imageToAscii(imageT, "-width=60", "-height=80"), set one of width or height will keep aspect ratio

**resizeImage**: get a new image by resizing an image object, usage: newImageT := resizeImage(imageT, "-width=60", "-height=80"), set one of width or height will keep aspect ratio

### --- plot related ---

**plotClearConsole**: clear console for plot

**plotDataToStr**: this function is based on github.com/guptarohit/asciigraph(thanks), for usage, see example script file asciiPlot.char

**plotDataToImage**: this function is based on github.com/vicanso/go-charts(thanks), for usage, see example script file pngPlot.char and svgPlot.char

**plotLoadFont**: load a font file in ttf format for plot, usage: plotLoadFont("c:\windows\tahoma.ttf", "tahoma", true), the secode parameter gives the font name(default is the file name), pass true for the third parameter to set the font as default font used in plot(default is false)

### --- ssh related ---

**sshUpload**

### --- eTable related ---

**readCsv**

**writeCsv**

**excelNew**

**excelOpen**

**excelSaveAs**

**excelWriteTo**

**excelClose**

**excelNewSheet**

**excelReadSheet**

**excelWriteSheet**

**excelReadCell**

**excelWriteCell**

### --- database related ---

**formatSQLValue**

**dbConnect**

**dbClose**

**dbQuery**

**dbQueryOrdered**

**dbQueryRecs**

**dbQueryMap**

**dbQueryMapArray**

**dbQueryCount**

**dbQueryFloat**

**dbQueryString**

**dbQueryStr**

**dbExec**

### --- unicode related ---

**toPinyin**

### --- line editor related ---

**leClear**

**leLoadFromStr**

**leAppendFromStr**

**leSaveToStr**

**leToStr**

**leLoadFromFile**

**leAppendFromFile**

**leSaveToFile**

**leAppendToFile**

**leLoadFromClip**

**leSaveToClip**

**leLoadFromUrl**

**leLoadFromSsh**

**leSaveToSsh**

**leSshInfo**

**leViewAll**

**leViewLine**

**leViewLines**

**leSort**

**leConvertToUtf8**

**leGetLine**

**leSetLine**

**leSetLines**

**leInsertLine**

**leAppendLine**

**leRemoveLine**

**leRemoveLines**

**leFindLines**

**leFind**

**leFindAll**

**leReplace**

**lePrint**

**leGetList**

### --- 3rd party related ---

**awsSign**

### --- misc related ---

**getSeq**

**renderMarkdown**

// "sortByFunc": BuiltinSortByFunc,

### --- original internal related ---

**copy**

**repeat**

**contains**

**sort**

**sortReverse**

**cap**

**printf**

**println**

**sprintf**

**WrongNumArgumentsError**

**InvalidOperatorError**

**IndexOutOfBoundsError**

**NotIterableError**

**NotIndexableError**

**NotIndexAssignableError**

**NotCallableError**

**NotImplementedError**

**ZeroDivisionError**

**TypeError**

## Functions in module 'ex'

### --- compile/run/thread related ---

**compile**: compile a piece of code

**runCompiled**: run compiled code

**threadRunCompiled**: run compiled code in a new thread

**loadGel**: compile a piece of code and turn it to Gel

**sortByFunc**: sort by compiled function

**sortByFuncQuick**: sort by compiled function

**newFunc**: new a go function

**close**: close objects that can close

