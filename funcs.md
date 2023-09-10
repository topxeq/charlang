## 谢语言指令参考（Xielang Instruction Reference）

### --- internal & debug related ---

**testByText**

**testByStartsWith**

**testByReg**

### --- data type related ---

**typeCode**

**typeName**

**typeOfAny**

**new**

**time**

**stringBuilder**

**any**

**toTime**

### --- new related ---

**newObj**

### --- convert related ---

**toStr**

### --- string related ---

**trim**

**strTrim**

**strReplace**

**strSplitLines**

### --- regex related ---

**regFindFirst**

**regFindFirstGroups**: obtain the first match of a regular expression and return a list of all matching groups, where the first item is the complete matching result and the second item is the first matching group..., usage example: result := regFindFirstGroups(str1, regex1)

**regFindAll**

**regQuote**

### --- time related ---

### --- control related ---

**exit**

### --- print related ---

**pl**

**pln**

**plo**

**plt**

### --- error related ---

**isErrX**

**getErrStrX**

**checkErrX**

### --- member/method related ---

**method**

**callNamedFunc**

**callInternalFunc**

**getNamedValue**

### --- read/write related ---

**writeStr**

### --- encode/decode related ---

**toJSON**

**toJson**

**fromJSON**

**fromJson**

### --- command-line related ---

**ifSwitchExists**

**getSwitch**

**getParam**

### --- clipboard related ---

**getClipText**

**setClipText**

### --- thread related ---

**sleep**

### --- os/system related ---

**systemCmd**

**getEnv**

**setEnv**

**getOsName**

**getOSName**

### --- path related ---

**joinPath**: join multiple file paths into one, equivalent to path/filepath.Join in the Go language standard library

### --- file related ---

**loadText**

**saveText**

### --- compress/zip related ---

**archiveFilesToZip**: Add multiple files to a newly created zip file. The first parameter is the zip file name, with a suffix of '.zip'. Optional parameters include '-overwrite' (whether to overwrite existing files) and '-makeDirs' (whether to create a new directory as needed). Other parameters are treated as files or directories to be added, and the directory will be recursively added to the zip file. If the parameter is a list, it will be treated as a list of file names, and all files in it will be added

### --- ssh related ---

**sshUpload**

### --- misc related ---

**getSeq**

**pass**

**append**

**delete**

**copy**

**repeat**

**contains**

**len**

**sort**

**sortReverse**

**error**

**bool**

**int**

**uint**

**float**

**char**

**string**

**bytes**

**chars**

**printf**

**println**

**sprintf**

**globals**

**isError**

**isInt**

**isUint**

**isFloat**

**isChar**

**isBool**

**isString**

**isBytes**

**isMap**

**isSyncMap**

**isArray**

**isUndefined**

**isFunction**

**isCallable**

**isIterable**

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

**:makeArray**

**cap**

