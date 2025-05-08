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

**bytesWithSize**: create a bytes value(with type 'bytes') with specified size, usage: b1 := bytesWithSize(5)

**bytesWithCap**: create a bytes value(with type 'bytes') with specified capacity(init with zero size), usage: b1 := bytesWithCap(5)

**chars**: create a chars/runes value(with type 'chars'), usage: c1 := chars([0xefab, 0xbc01, 0x81cf]) , c2 := ("abc123")

**bytesBuffer**: create a bytes buffer, usage: buf1 := bytesBuffer() , buf2 := bytesBuffer(bytes("abc123"))

**stringBuilder**: create a string builder, usage: sb1 := stringBuilder() , sb2 := stringBuilder("abc123")

**stringBuffer**: same as stringBuilder

**orderedMap**: create an ordered-map, usage: map1 := orderedMap() , map2 := orderedMap({1, 2.5, true, "abc123"})

**stack**: create a stack object(first-in-last-out), usage: st1 := stack() , st2 := stack(1, 2.5, true, "abc123"), the objects passed as parameters for builtin stack function will be pushed in sequence after the creation of the stack

**queue**: create a queue object(first-in-first-out), usage: que1 := queue() , que2 := queue(10), the integer value passed as parameters for builtin queue function will set the capacity(default infinite) of the queue, the first item will be discarded while a new item is pushing into the queue and the queue is full

**mapArray**: create an MapArray/SimpleFlexObject which is an array with some items have keys

**error**: manually create an error object, usage: err1 := error("failed to do something")

**time**: create a time object, usage: time1 := time(), get a new time object with the value of current time; time2 := time(123432545), create a time object with the value set to the tick/timestamp of the integer value; time3 := time("2023-01-02 00:18:23")

**dateTime**: same as 'time'

**excel**: create an Excel object

**statusResult**: create a statusResult object(i.e. {"Status": "success", "Value": "some value"}, or {"Status": "fail", "Value": "failed reason/description"})

**seq**: create a sequence object

**mutex**: create a mutex object for threads

**mux**: create a mux object for http routing

**httpHandler**: create a httpHandler object for handling http routes

**httpReq**: create a http request object

**reader**: create a reader object from bytes, string, file or other objects

**writer**: create a writer object from bytes, string, file or other objects

**file**: usage: file("c:\\tmp\abc.txt"), file(`/home/user1/a.json`), file("stdin"), file("stdout"), file("stderr"),  another example: fileT := file(`c:\test\a.json`, "-create"), options include: -flag=0, -readOnly, -append, -truncate, -perm=0777 (only octec format is supported)

**image**: new an image object, usage: imageT := image("-width=480", "-height=640", "-color=#FF0000")

**charCode**

**evalMachine**

**gel**

**delegate**

**jsVm**: new a JavaScript VM

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

**reset**

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

**strToHex**

**hexEncode**

**unhex**

**hexDecode**

**hexToStr**

**toKMG**

**floatToStr**

**strToUtf8**

**strUtf8ToGb**

**isUtf8**

**simpleStrToMap**

**simpleStrToMapReverse**

**reverseMap**

### --- string related ---

**trim**: trim spaces of the string, also convert undefined value to empty string

**nilToEmpty**: convert undefined value to empty string

**strTrim**: same as trim

**strTrimStart**: usage: strTrimStart(s, prefix), returns string without the provided leading prefix sub-string. If the string doesn't start with prefix, s is returned unchanged.

**strTrimEnd**: usage: strTrimEnd(s, suffix), returns string without the provided trailing suffix sub-string. If the string doesn't end with suffix, s is returned unchanged.

**strTrimLeft**: usage: strTrimLeft(s, cutset string), strTrimLeft returns a slice of the string s with all leading Unicode code points contained in cutset removed.

**strTrimRight**: usage: strTrimRight(s, cutset string), strTrimRight returns a slice of the string s, with all trailing Unicode code points contained in cutset removed.

**toUpper**

**strToUpper**

**toLower**

**strToLower**

**strContains**

**strContainsAny**: as strings.ContainsAny, reports whether any Unicode code points in chars are within s.

**strContainsIn**

**strIndex**

**strStartsWith**

**strEndsWith**

**strReplace**

**strSplit**

**strSplitN**

**strSplitLines**

**strJoin**

**strRepeat**

**strCount**

**strPad**: string padding operations such as zero padding, for example, result := strPad(strT, 5, "-fill=0", "-right=true"), where the first parameter is the string to be padded, and the second parameter is the number of characters to be padded. The default padding string is fill as string 0, and right (indicating whether to fill on the right side) is false (which can also be written directly as -right). Therefore, the above example is equivalent to result := strPad(strT, 5). If the fill string contains more than one character, the final number of padding will not exceed the value specified by the second parameter, but it may be less

**strRuneLen**: get the length of string by rune(how many rune characters in the string)

**strIn**

**strGetLastComponent**: strGetLastComponent("/root/abc", "/"), default separator is \ in Windows or / in Linux/MacOS

**strFindDiffPos**: return -1 if 2 strings are identical

**strDiff**

**strFindAllSub**

**limitStr**

**strQuote**

**strUnquote**

**strToInt**: convert string to int, return error if failed

**strToTime**: convert string to time by format, usage: strToTime(strA, "20060102150405"), default "2006-01-02 15:04:05"

**dealStr**: deal with hex-encoded, encrypted or other special-treated string

**getTextSimilarity**: calculate the cosine similarity of two strings

### --- regex related ---

**regMatch**: determine whether a string fully conforms to a regular expression, usage example: result := regMatch("abcab", `a.*b`)

**regContains**: determine whether the string contains substrings that conform to the regular expression

**regContainsIn**: determine whether the string contains substrings that conform to any regular expression

**regFindFirst**: get the first match of a regular expression, usage example: result := regFindFirst(str1, regex1, group)

**regFindFirstGroups**: obtain the first match of a regular expression and return a list of all matching groups, where the first item is the complete matching result and the second item is the first matching group..., usage example: result := regFindFirstGroups(str1, regex1)

**regFindAll**: get all matches of a regular expression, and the default matching group number is 0, which means a complete match. Usage example: result := regFindAll(str1, regex1, group)

**regFindAllIndex**: get all matches' indexes of a regular expression, and the default matching group number is 0, which means a complete match. Usage example: result := regFindAll(str1, regex1, group)

**regFindAllGroups**: get all matches of a regular expression, the result is a two-dimensional string array containing various groups, where the 0th group is a complete match, and the 1st group starts with the matching groups in parentheses. usage example: result := regFindAllGroups(str1, regex1)

### --- escaping and replacing special characters related ---

**regReplace**: replace in a string based on regular expressions, function definition: regReplace(strA, patternA, replaceA string) string, example: regReplace("abcdefgabcdfg", "(b. *) f (ga. *?) g", "$ {1}_ ${2} "), the result is abcd_gabcdf

**regCount**: determine if a certain string contains several substrings that match the regular expression, usage: result := regCount(str1, regex1)

**regSplit**: split strings using regular expressions, usage: listT := regSplit(str1, regex1)

### --- math related ---

**adjustFloat**: remove the number of digits from floating-point calculation error values such as 32.0000000004. The result parameter cannot be omitted. Usage: adjustFloat(0.65-0.6, 10). The second parameter is the number of decimal places to which it is organized, which can be omitted. The default is 10.

**mathAbs**

**abs**

**mathSqrt**

**sqrt**

**mathPow**

**pow**

**mathExp**

**exp**

**mathLog**

**log**

**mathLog10**

**log10**

**min**: returns the largest of several values(integer of float).

**max**: returns the smallest of several values(integer of float).

**ceil**: returns the least integer value greater than or equal to x.

**floor**: returns the greatest integer value less than or equal to x.

**round**: returns the nearest integer, rounding half away from zero.

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

**timeAddSecs**: add seconds(could be float) to time and return the result, usage: time2 := timeAddSecs(time1, 1.2)

**timeAddDate**: add years, months, days to time and return the result, usage: time2 := timeAddDate(time1, 0, -1, 0), will add -1 month to time1

**timeBefore**: usage: b1 := timeBefore(time1, time2)

### --- binary/bytes related ---

**bytesStartsWith**

**bytesEndsWith**

**bytesContains**

**bytesIndex**

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

**isErrStr**

**getErrStrX**

**getErrStr**

**checkErrX**: check if the object is error or error string, if is, exit the program with output, usage: checkErrX(result, "-format=Failed to process: %v\n"), the default format is "Error: %v\n"

**checkErr**: the same as checkErrX, check if the object is error or error string, if is, exit the program with output, usage: checkErr(result, "-format=Failed to process: %v\n"), the default format is "Error: %v\n"

**checkEmpty**: similar to checkErr, check if the object is undefined or empty string(for other objects, will use its string representation), if is, exit the program with output, usage: checkEmpty(result, "-format=Failed to process: %v\n"), the default format is "Empty: %v\n"

**errStrf**

**errf**

**errToEmpty**

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

**spln**

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

**readBytes**

**writeStr**

**writeBytes**

**writeBytesAt**: write bytes at the specified index in Bytes, return the result buffer(maybe the same), if not enough size, enlarge the buffer and reture the new buffer(i.e. the Bytes object), usage: buf1 = writeBytesAt(bytes([1, 2, 3]), 1, bytes([4, 5, 6, 7]))

**copyBytes**

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

**base64EncodeByRawUrl**

**base64DecodeByRawUrl**

**toJSON**

**toJson**

**fromJSON**

**fromJson**

**formatJson**

**compactJson**

**getJsonNodeStr**: getJsonNodeStr(jsonA, pathA), pathA refer to github.com/tidwall/gjson

**getJsonNodeStrs**: getJsonNodeStrs(jsonA, pathA), pathA refer to github.com/tidwall/gjson

**strsToJson**

### --- XML related ---

**xmlEncodeStr**

**xmlGetNodeStr**

**fromXml**

**formatXml**

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

**systemCmdDetached**

**systemStart**

**getSysInfo**: getSysInfo("-disk=/", "-cpuTime=0.5"), by default, disk info will not be retrieved

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

**createTempDir**: usage: tempDirNameT := createTempDir(dirA, patternA)  , createTempDir creates a new temporary directory in the directory dirA and returns the pathname of the new directory. The new directory's name is generated by adding a random string to the end of patternA. If patternA includes a "*", the random string replaces the last "*" instead. The directory is created with mode 0o700 (before umask). If dirA is the empty string, createTempDir uses the default directory for temporary files. It is the caller's responsibility to remove the directory when it is no longer needed.

**createTempFile**: usage: tempFileNameT := createTempFile(dirA, patternA)  , createTempFile creates a new temporary file in the directory dir, and returns the resulting file name. The filename is generated by taking patternA and adding a random string to the end. If patternA includes a "*", the random string replaces the last "*". The file is created with mode 0o600 (before umask). If dirA is the empty string, createTempFile uses the default directory for temporary files. It is the caller's responsibility to remove the file when it is no longer needed.

**changeDir**

**chdir**

**lookPath**

**getInput**

**getInputf**

**getInputPasswordf**

**getChar**

**getMultiLineInput**

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

**removeFile**: remove file only

**removeDir**: remove directory only

**removePath**: remove file or directory, use "-recursive" to remove with files and sub-directories

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

**getFileListInArchive**

**getFileListInArchiveBytes**

**getFileListInZip**

**loadBytesInArchive**: loadBytesInArchive("example.zip", "subdir1/a.txt", "-limit=3")

**loadBytesInArchiveBytes**: loadBytesInArchiveBytes(bytesT)

**extractFileInArchive**: extractFileInArchive("example.zip", "subdir1/a.txt", "toDir/a.txt")

**extractArchive**: extractArchive("example.zip", "toDir", "-noFileDir", "-force")

**isFileNameUtf8InZipBytes**: return boolean value or error

### --- network/web related ---

**joinUrlPath**: join multiple url paths into one, equivalent to path.Join in the Go language standard library

**getWeb**

**downloadFile**

**getWebBytes**

**getWebBytesWithHeaders**

**getWebRespBody**: rs := getWebRespBody(urlT, "-withLen"); if isErr(rs) {...}; readerT := rs[0]; lenT := rs[1]; rs = s3PutObject(readerT, "tmpbucket", keyT, "-endPoint=xxxxx", "-accessKey=xxxxx", "-secretAccessKey=xxxxx", "-ssl", "-force", "-size="+toStr(lenT), "-contentType=application/octet-stream", "-timeout=600");  close(readerT)

**postRequest**

**prepareMultiPartFieldFromBytes**: prepareMultiPartFieldFromBytes("file", bytes[0x65, 0x66, 0x67]), return ["for content-type", bytes generated]

**prepareMultiPartFileFromBytes**: prepareMultiPartFileFromBytes("file", "a.txt", bytes[0x65, 0x66, 0x67]), return ["for content-type", bytes generated], then rs := getWeb(spr("https://example.com/ms/mgmt/uploadAttach?uid=%v&valueonly=true&witherror=true&billNo=%v", uidT, newBillNoT), formObjT[1], `-headers={"Content-Type":"`+formObjT[0]+`"}`, "-timeout=30")

**urlExists**

**parseUrl**: parse URL and return a map

**parseQuery**: parse URL query string(such as 'x=1&y=2&y=3') and return a map({"x": "1", "y": ["2", "3"]})

**isHttps**

**httpRedirect**

### --- server/service related ---

**getReqHeader**

**getReqHeaders**

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

**genJwtToken**: genJwtToken({"sub":"user1","exp":1742450426,"userId":116}, "my_secret", "-noType", "-base64Secret"), genJwtToken({"sub":"user1","exp":1742450426,"userId":116}, base64DecodeByRawUrl("my_secret"), "-noType")

**parseJwtToken**

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

**encryptDataByTXDEE**

**decryptDataByTXDEE**

**encryptTextByTXDEE**

**decryptTextByTXDEE**

**encryptData**

**encryptBytes**

**decryptData**

**decryptBytes**

**encryptStream**

**decryptStream**

**aesEncrypt**: AES encrypt string or bytes, "-cbc" to use cbc

**aesDecrypt**: AES decrypt string or bytes, "-cbc" to use cbc

### --- image related ---

**loadImageFromBytes**: usage: imageT := loadImageFromBytes(bytesT, "-type=png")

**saveImageToBytes**: usage: bytesT := saveImageToBytes(imageT) or bytesT := saveImageToBytes(imageT, ".png") to save image with specified format, .jpg, .png, .gif, .bmp is supported

**imageToBytes**

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

### --- ssh/ftp related ---

**ftpList**

**ftpCreateDir**

**ftpSize**: could used to determine if file exists, by check the result if is error and contains certain text

**ftpUpload**

**ftpUploadFromReader**

**ftpDownloadBytes**

**ftpRemoveFile**

**sshUpload**

**sshUploadBytes**

**sshDownload**

**sshDownloadBytes**

**sshList**: sshList("-host=xx.xx.xx.xx", "-port=22", "-user=root", "-password=password123", "-remotePath=/root", "-recursive", "-sep=/", "-pattern=*", "-withDir", "-dirOnly")

**sshIfFileExists**

**sshGetFileInfo**

**sshRun**

**sshRename**

**sshJoinPath**

**sshRemoveFile**

**sshRemoveDir**

**sshEnsureMakeDirs**

### --- eTable related ---

**readCsv**

**csvRead**

**writeCsv**

**csvWrite**

**excelNew**

**excelOpen**

**excelOpenFile**

**excelSaveAs**

**excelWriteTo**

**excelClose**

**excelNewSheet**

**excelReadAll**

**excelGetSheetCount**

**excelGetSheetList**

**excelGetSheetName**

**excelReadSheet**

**excelWriteSheet**

**excelReadCell**

**excelGetCellValue**

**excelWriteCell**

**excelSetCellValue**

**excelGetColumnIndexByName**

**excelGetColumnNameByIndex**

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

**leInfo**: show lines count, chars count, ...

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

### --- GUI server related ---

**guiServerCommand**: send command to GUI server, usage: rs := cgsCmd("pln", "-url="+trim(guiServerUrlG), "value", "18")

**cgsCmd**

### --- s3 related ---

**s3GetObjectBytes**

**s3GetObjectText**

**s3PutObject**

**s3GetObjectToFile**

**s3GetObjectReader**

**s3GetObjectUrl**

**s3GetObjectTags**

**s3GetObjectStat**

**s3StatObject**

**s3CopyObject**

**s3MoveObject**

**s3RemoveObject**

**s3ListObjects**

### --- 3rd party related ---

**awsSign**

### --- misc related ---

**magic**

**getSeq**

**getUuid**

**genUuid**

**renderMarkdown**

**replaceHtmlByMap**

**processHtmlTemplate**

**statusToStr**

**statusToMap**

**docxToStrs**

**docxReplacePattern**

**docxGetPlaceholders**

**showTable**

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

