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

**byte**: create a byte value(with type 'byte'), usage: by1 := byte(),  by2 := byte(33)

**char**: create a char/rune value(with type 'char'), usage: c1 := char(),  c2 := char(33), c3 := char('&')

**rune**: same as char

**int**: create an integer/int value(with type 'int'), usage: n1 := int(),  n2 := int(-112)

**uint**: create an unsigned integer/int value(with type 'uint'), usage: n1 := uint(),  n2 := uint(68)

**float**: create a float value(with type 'float'), usage: f1 := float(),  f2 := float(-57.23)

**bigInt**: create a big integer/int value(with type 'bigInt'), usage: n1 := bigInt(),  n2 := bigInt(-112)

**bigFloat**: create a big float value(with type 'bigFloat'), usage: f1 := bigFloat(),  f2 := bigFloat(-57.23)

**string**: create a string value(with type 'string'), usage: s1 := string(),  s2 := string("abc")

**mutableString**: create a mutable string value(with type 'mutableString'), mutableString could change value at run time, usage: s1 := mutableString(),  s2 := mutableString("abc")

**bytes**: create a bytes value(with type 'bytes'), usage: b1 := bytes([0xef, 0xbc, 0x81]), b2 := bytes("abc123")

**chars**: create a chars/runes value(with type 'chars'), usage: c1 := chars([0xefab, 0xbc01, 0x81cf]) , c2 := ("abc123")

**bytesBuffer**

**stringBuilder**

**stringBuffer**

**orderedMap**

**stack**

**queue**

**error**

**time**

**dateTime**

**excel**

**statusResult**

**seq**

**mutex**

**mux**

**httpHandler**

**httpReq**

**reader**

**writer**

**file**: usage: file("c:\\tmp\abc.txt"), file(`/home/user1/a.json`), file("stdin"), file("stdout"), file("stderr"),  another example: fileT := file(`c:\test\a.json`, "-create"), options include: -flag=0, -readOnly, -append, -truncate, -perm=0777 (only octec format is supported)

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

**sortArray**: usage: sortArray(totalFindsT, "runeStart", "desc")

**getMapItem**

**setMapItem**

**toOrderedMap**

### --- ref/pointer related ---

**unref**

**setValueByRef**

### --- convert related ---

**toStr**

**toBool**

**toBoolWithDefault**

**toInt**

**toFloat**

**toTime**

**toHex**

**hexEncode**

**unhex**

**hexDecode**

**hexToStr**

**toKMG**

**floatToStr**

### --- string related ---

**trim**

**nilToEmpty**

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

**strContainsIn**

**strIndex**

**strStartsWith**

**strEndsWith**

**strReplace**

**strSplit**

**strSplitLines**

**strJoin**

**strRepeat**

**strCount**

**strPad**

**strRuneLen**

**strIn**

**strGetLastComponent**

**strFindDiffPos**: return -1 if 2 strings are identical

**strFindAllSub**

**limitStr**

**strQuote**

**strUnquote**

**strToInt**

**strToTime**

**dealStr**

**getTextSimilarity**

### --- regex related ---

**regMatch**

**regContains**

**regFindFirst**

**regFindFirstGroups**: obtain the first match of a regular expression and return a list of all matching groups, where the first item is the complete matching result and the second item is the first matching group..., usage example: result := regFindFirstGroups(str1, regex1)

**regFindAll**

**regFindAllIndex**

**regFindAllGroups**

**regQuote**

**regReplace**

**regCount**

**regSplit**

### --- math related ---

**adjustFloat**

**mathAbs**

**abs**

**mathSqrt**

**mathPow**

**mathExp**

**mathLog**

**min**

**max**

**flexEval**

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

**bytesContains**

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

**checkErrX**: check if the object is error or error string, if is, exit the program with output, usage: checkErrX(result, "-format=Failed to process: %v\n"), the default format is "Error: %v\n"

**checkErr**: the same as checkErrX, check if the object is error or error string, if is, exit the program with output, usage: checkErr(result, "-format=Failed to process: %v\n"), the default format is "Error: %v\n"

**errStrf**

**errf**

### --- output/print related ---

**pr**

**prf**

**fprf**

**pl**

**plNow**: the same as pl, with additional current time before the output

**pln**: the same as 'println' in other languages. pln formats using the default formats for its arguments and writes to standard output. Usage: pln("the name is", str1)

**plv**

**plt**

**plo**

**plErr**

**fatalf**

**spr**

### --- scan related ---

**sscanf**

### --- process related ---

**getProcessVar**: set a process wide global variable value, usage: v1 := getProcessVar("key1", "defaultValue")

**setProcessVar**: get a process wide global variable value

**deleteProcessVar**: delete a process wide global variable value

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

**ioCopy**

### --- encode/decode related ---

**md5**

**urlEncode**

**urlEncode1**

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

**switchExists**

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

**getUserDir**

**getTempDir**

**changeDirj**

**chdir**

**lookPath**

**getInput**

**getInputf**

**getChar**

**setStdin**

**setStdout**

**setStderr**

**getPipe**

### --- dir/path related ---

**joinPath**: join multiple file paths into one, equivalent to path/filepath.Join in the Go language standard library

**isDir**

**ensureMakeDirs**

**getFileList**

**genFileList**

### --- file related ---

**fileExists**

**ifFileExists**

**getFileInfo**

**getFileAbs**

**getFileExt**

**getFileRel**

**extractFileDir**

**extractFileName**

**getBaseFileName**

**getFileBase**

**copyFile**

**renameFile**

**moveFile**

**removeFile**

**removeDir**

**removePath**

**loadText**

**saveText**

**appendText**

**loadBytes**: load bytes from file, usage: loadBytes("file.bin"), return error or Bytes([]byte), loadBytes("a.txt", 5) to read only 5 bytes, can accept a File object

**loadBytesFromFile**: the same as loadBytes

**saveBytes**

**openFile**: usage: fileT := openFile(`c:\test\a.json`, "-create"), options include: -flag=0, -readOnly, -append, -truncate, -perm=0777 (only octec format is supported)

**closeFile**

### --- compress/zip related ---

**compressData**

**compressStr**

**uncompressData**

**uncompressStr**

**archiveFilesToZip**: Add multiple files to a newly created zip file. The first parameter is the zip file name, with a suffix of '.zip'. Optional parameters include '-overwrite' (whether to overwrite existing files) and '-makeDirs' (whether to create a new directory as needed). Other parameters are treated as files or directories to be added, and the directory will be recursively added to the zip file. If the parameter is a list, it will be treated as a list of file names, and all files in it will be added

### --- network/web related ---

**getWeb**

**getWebBytes**

**getWebBytesWithHeaders**

**getWebRespBody**: rs := getWebRespBody(urlT, "-withLen"); if isErr(rs) {...}; readerT := rs[0]; lenT := rs[1]; rs = s3PutObject(readerT, "tmpbucket", keyT, "-endPoint=xxxxx", "-accessKey=xxxxx", "-secretAccessKey=xxxxx", "-ssl", "-force", "-size="+toStr(lenT), "-contentType=application/octet-stream", "-timeout=600");  close(readerT)

**postRequest**

**urlExists**

**parseUrl**: parse URL and return a map

**parseQuery**: parse URL query string(such as 'x=1&y=2&y=3') and return a map({"x": "1", "y": ["2", "3"]})

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

**startSocksServer**

**startSocksClient**

**startTransparentProxy**

**startTransparentProxyEx**

### --- security related ---

**genToken**

**checkToken**

**getOtpCode**

**genOtpCode**

**checkOtpCode**

**isEncrypted**

**encryptText**

**encryptStr**

**decryptText**

**decryptStr**

**encryptTextByTXTE**

**decryptTextByTXTE**

**encryptData**

**encryptBytes**

**decryptData**

**decryptBytes**

**encryptStream**

**decryptStream**

### --- image related ---

**loadImageFromBytes**: usage: imageT := loadImageFromBytes(bytesT, "-type=png")

**saveImageToBytes**: usage: bytesT := saveImageToBytes(imageT) or bytesT := saveImageToBytes(imageT, ".png") to save image with specified format, .jpg, .png, .gif, .bmp is supported

**loadImageFromFile**: usage: imageT := loadImageFromFile(`c:\test\abc.png`) or image2T := loadImageFromFile(`c:\test\abc.jpg`, "-type=jpg")

**saveImageToFile**: usage: errT := saveImageToFile(imageT, `c:\test\newabc.png`) or errT := saveImageToFile(imageT, `c:\test\newabc.png`, ".png") to save image with specified format, .jpg, .png, .gif, .bmp is supported

**getImageInfo**

**strToRgba**

**encodeImage**

**encodeBytesInImage**

**decodeBytesFromImage**

**drawImageOnImage**

**drawTextWrappedOnImage**

**genQr**

**imageToAscii**: convert an image object to colorful ASCII graph(array of string), usage: asciiT := imageToAscii(imageT, "-width=60", "-height=80"), set one of width or height will keep aspect ratio

**resizeImage**: get a new image by resizing an image object, usage: newImageT := resizeImage(imageT, "-width=60", "-height=80"), set one of width or height will keep aspect ratio

### --- plot related ---

**plotClearConsole**: clear console for plot

**plotDataToStr**: this function is based on github.com/guptarohit/asciigraph(thanks), for usage, see example script file asciiPlot.char

**plotDataToImage**: this function is based on github.com/vicanso/go-charts(thanks), for usage, see example script file pngPlot.char and svgPlot.char

**plotLoadFont**: load a font file in ttf format for plot, usage: plotLoadFont("c:\windows\tahoma.ttf", "tahoma", true), the secode parameter gives the font name(default is the file name), pass true for the third parameter to set the font as default font used in plot(default is false)

### --- ssh related ---

**sshUpload**

**sshUploadBytes**

**sshDownload**

**sshRun**

### --- eTable related ---

**readCsv**

**writeCsv**

**excelNew**

**excelOpen**

**excelOpenFile**

**excelSaveAs**

**excelWriteTo**

**excelClose**

**excelNewSheet**

**excelGetSheetCount**

**excelGetSheetList**

**excelGetSheetName**

**excelReadSheet**

**excelWriteSheet**

**excelReadCell**

**excelWriteCell**

### --- database related ---

**formatSQLValue**

**formatSqlValue**

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

**toPinYin**

**kanjiToKana**

**kanaToRomaji**

**kanjiToRomaji**

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

**leLoadFromSsh**: usage: rs := leLoadFromSsh("-host=a.b.com", "-port=22", "-user=user1", "-password=pass1", "-path=/home/user1/abc.txt")

**leSaveToSsh**: usage: rs := leSaveToSsh("-host=a.b.com", "-port=22", "-user=user1", "-password=pass1", "-path="+pathT)

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

### --- mail related ---

**sendMail**

### --- s3 related ---

**s3GetObjectBytes**

**s3GetObjectText**

**s3PutObject**

**s3GetObjectReader**

**s3GetObjectUrl**

**s3GetObjectTags**

**s3GetObjectStat**

**s3StatObject**

**s3RemoveObject**

**s3ListObjects**

### --- 3rd party related ---

**awsSign**

### --- misc related ---

**getSeq**

**getUuid**

**genUuid**

**renderMarkdown**

**replaceHtmlByMap**

**processHtmlTemplate**

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

**threadRunFunc**: run compiled function in a new thread

**loadGel**: compile a piece of code and turn it to Gel

**sortByFunc**: sort by compiled function

**sortByFuncQuick**: sort by compiled function

**newFunc**: new a go function

**close**: close objects that can close

