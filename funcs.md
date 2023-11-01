## Charlang Builtin Function Reference

### --- internal & debug related ---

// the "testBy*" functions are builtin test functions for internal use only, see testAll.char for usage examples

**testByText**: usage: testByText(strToTest, strToCompare, indexInteger, scriptFileName)

**testByStartsWith**

**testByEndsWith**

**testByContains**

**testByReg**

**testByRegContains**

**dumpVar**

**debugInfo**

### --- infrastructure related ---

**globals**

**len**

**:makeArray**

### --- data type related ---

**typeCode**

**typeName**

**typeOf**

**typeOfAny**

**any**

**bool**

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

**orderedMap**

**statusResult**

**seq**

**mutex**

**mux**

**httpHandler**

**image**

**charCode**

**gel**

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

**toKMG**

### --- string related ---

**trim**

**strTrim**

**strTrimStart**

**strTrimEnd**

**strTrimLeft**

**strTrimRight**

**toUpper**

**toLower**

**strContains**

**strStartsWith**

**strEndsWith**

**strReplace**

**strSplit**

**strSplitLines**

**strJoin**

**strIn**

**strFindDiffPos**: return -1 if 2 strings are identical

**limitStr**

**strQuote**

**strUnquote**

### --- regex related ---

**regFindFirst**

**regFindFirstGroups**: obtain the first match of a regular expression and return a list of all matching groups, where the first item is the complete matching result and the second item is the first matching group..., usage example: result := regFindFirstGroups(str1, regex1)

**regFindAll**

**regFindAllGroups**

**regQuote**

**regReplace**

### --- math related ---

**adjustFloat**

**mathSqrt**

### --- random related ---

**getRandomInt**

**getRandomFloat**

**getRandomStr**

**genRandomStr**

### --- time related ---

**getNowStr**

**getNowStrCompact**

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

### --- output/print related ---

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

### --- read/write related ---

**writeStr**

### --- encode/decode related ---

**md5**

**urlEncode**

**urlDecode**

**htmlEncode**

**htmlDecode**

**simpleEncode**

**simpleDecode**

**toJSON**

**toJson**

**fromJSON**

**fromJson**

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

**loadText**

**saveText**

**loadBytes**

**saveBytes**

### --- compress/zip related ---

**archiveFilesToZip**: Add multiple files to a newly created zip file. The first parameter is the zip file name, with a suffix of '.zip'. Optional parameters include '-overwrite' (whether to overwrite existing files) and '-makeDirs' (whether to create a new directory as needed). Other parameters are treated as files or directories to be added, and the directory will be recursively added to the zip file. If the parameter is a list, it will be treated as a list of file names, and all files in it will be added

### --- network/web related ---

**getWeb**

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

**encryptText**

**decryptText**

**encryptData**

**decryptData**

### --- ssh related ---

**sshUpload**

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

**dbExec**

### --- unicode related ---

**toPinyin**

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

