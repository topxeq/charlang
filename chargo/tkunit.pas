unit tkunit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Forms, Dialogs, SysUtils, StrUtils, httpprotocol, fphttpclient,
  openssl, opensslsockets, Variants, TypInfo, fpjsonrtti, fpjson,
  Generics.Collections, lazutf8;

type

  tkStrMap = specialize TDictionary<string, string>;

  tk = class
  public
    // is related
    class function isHex(strA: string): boolean;

    // string related
    class function isStringNullOrEmpty(strA: string): boolean;
    class function strSlice(strA: string; fromA: integer; toA: integer = -1): string;
    class function strLen(strA: string): integer;

    class function trim(strA: string): string;
    class function trim(strA: WideString): WideString;
    class function toStr(const AValue; ATypeInfo: PTypeInfo): string;

    class function strStartsWith(strA: string; subStrA: string): boolean;

    class function strSplitLines(strA: string): TStringArray;

    // error related
    class function errStr(strA: string): string;
    class function isErrStr(strA: string): boolean;
    class function getErrStr(strA: string): string;

    // array/list related
    class function getArrayItem(listA: TStringList; idxA: integer;
      defaultA: string = ''): string;

    // map/dictionary related
    class function newStrMap(pairsA: array of string): tkStrMap;

    // encode/decode related
    class function urlEncode(strA: string): string;
    class function urlDecode(strA: string): string;

    // json related
    class function toJson(valueA: TObject): string;

    // encrypt/decrypt related
    class function encryptStringByTXTE(originStr: string; code: string): string;
    class function decryptStringByTXTE(originStr: string; code: string): string;

    // command-line related
    class function getParams(): TStringList;
    class function getSwitch(listA: TStringList; keyA: string;
      defaultA: string): string;
    class function getCliSwitch(keyA: string; defaultA: string): string;
    class function ifSwitchExists(listA: TStringList; keyA: string): boolean;
    class function ifCliSwitchExists(keyA: string): boolean;
    class function parseCommandLine(originStr: string;
      var argList: TStringList): string;

    // dir/path related
    class function getAppDir(): string;
    class function getTempDir(): string;
    class function getUserDir(): string;
    class function joinPath(argsA: array of string): string;

    // file related
    class function saveStringToFile(strA, filePathA: string): string;
    class function appendStringToFile(strA: string; filePathA: string): string;
    class function loadStringFromFile(fileNameA: string): string;

    // conversion related
    class function boolToStr(boolA: boolean): string;
    class function HexToByte(Hexa: string): byte;

    // network related

    // web related
    class function downloadFile(urlA, filePathA: string): string;
    class function getWeb(urlA: string; const optsA: array of string): string;

    // gui related
    class function showInfo(titleA, msgA: string): TModalResult;
    class function showError(titleA, msgA: string): TModalResult;

    // misc related
    class procedure pass;

  end;

  txObject = class(TObject)
  public
    constructor Create;
  public
    typeCode: integer;
    ptr: pointer;
    tag: integer;
    attach: pointer;


    function toString(): string; override;
  end;

  //var
  //tk: tkClass;


implementation

constructor txObject.Create;
begin
  self.typeCode := -1;
end;

function txObject.toString(): string;
begin
  Result := '(' + self.ClassName + ')' + IntToStr(tag);
end;

class function tk.appendStringToFile(strA: string; filePathA: string): string;
var
  fileT: TextFile;
begin
  AssignFile(fileT, filePathA);

  try
    try
      if FileExists(filePathA) then
        Append(fileT)
      else
        Rewrite(fileT);

      Write(fileT, strA);

      CloseFile(fileT);
    except
      on E: Exception do
      begin
        Result := 'TXERROR:' + 'failed to append stirng to file: ' +
          E.ClassName + '/' + E.Message;
        exit;
      end;
    end;
  finally
  end;

  Result := '';
end;

class function tk.urlEncode(strA: string): string;
begin
  Result := HTTPEncode(strA);
end;

class function tk.urlDecode(strA: string): string;
begin
  Result := HTTPDecode(strA);
end;

class function tk.getSwitch(listA: TStringList; keyA: string; defaultA: string): string;
var
  i: integer;
  tmps: string;
begin
  for i := 0 to listA.Count - 1 do
  begin
    tmps := listA[i];

    if (StartsStr(keyA, tmps)) then
    begin
      exit(rightstr(tmps, length(tmps) - length(keyA)));
    end;
  end;
  Result := defaultA;
end;

class function tk.getCliSwitch(keyA: string; defaultA: string): string;
var
  i: integer;
  tmps: string;
begin
  for i := 0 to paramCount do
  begin
    tmps := ParamStr(i);

    if (StartsStr(keyA, tmps)) then
    begin
      exit(rightstr(tmps, length(tmps) - length(keyA)));
    end;
  end;
  Result := defaultA;
end;

class function tk.ifSwitchExists(listA: TStringList; keyA: string): boolean;
var
  i: integer;
  tmps: string;
begin
  for i := 0 to listA.Count - 1 do
  begin
    tmps := listA[i];

    if (SameStr(keyA, tmps)) then
    begin
      exit(True);
    end;
  end;
  Result := False;
end;

class function tk.ifCliSwitchExists(keyA: string): boolean;
var
  i: integer;
  tmps: string;
begin
  //writeln('paramCount: ', paramCount);
  for i := 0 to paramCount do
  begin
    tmps := ParamStr(i);
    //writeln('i', ' ', i, ' ', tmps);

    if (SameStr(keyA, tmps)) then
    begin
      //writeln('same', keyA, tmps);
      exit(True);
    end;
  end;
  Result := False;
end;

class function tk.getParams(): TStringList;
var
  i: longint;
  paramsT: TStringList;
begin
  paramsT := TStringList.Create;

  for i := 0 to ParamCount do
  begin
    paramsT.Add(ParamStr(i));
  end;

  Result := paramsT;
  //FreeAndNil(paramsT);
end;

class function tk.trim(strA: string): string;
begin
  Result := SysUtils.Trim(strA);
end;

class function tk.trim(strA: WideString): WideString;
begin
  Result := SysUtils.Trim(strA);
end;

class function tk.getAppDir(): string;
begin
  Result := ExtractFileDir(Application.ExeName);
end;

class function tk.getTempDir(): string;
begin
  Result := GetTempDir();
end;

class function tk.getUserDir(): string;
begin
  Result := GetUserDir();
end;

class function tk.saveStringToFile(strA, filePathA: string): string;
var
  fileT: TextFile;
begin
  AssignFile(fileT, filePathA);

  try
    try
      Rewrite(fileT);

      Write(fileT, strA);

      CloseFile(fileT);
    except
      on E: Exception do
      begin
        Result := 'TXERROR:' + 'failed to append stirng to file: ' +
          E.ClassName + '/' + E.Message;
        exit;
      end;
    end;
  finally
  end;

  Result := '';
end;

class function tk.loadStringFromFile(fileNameA: string): string;
var
  ms: TMemoryStream;
  s, hs: string;
  b: boolean;
begin
  b := False;
  s := '';
  hs := '';

  Result := '';
  if not FileExists(fileNameA) then
  begin
    exit;
  end;
  ms := TMemoryStream.Create;
  ms.LoadFromFile(fileNameA);
  if b then
  begin
    SetLength(hs, 3);
    ms.Read(hs[1], 3);
    if hs <> #$EF#$BB#$BF then
    begin
      // ms.Free;
      // exit;
    end
    else
    begin
      SetLength(s, ms.Size - 3);
      ms.Read(s[1], ms.Size - 3);
    end;
  end
  else
  begin
    SetLength(s, ms.Size);
    ms.Read(s[1], ms.Size);
  end;

  Result := s;
  ms.Free;
  // Result := '';
end;

class function tk.errStr(strA: string): string;
begin
  Result := 'TXERROR:' + strA;
end;

class function tk.downloadFile(urlA, filePathA: string): string;
var
  clientT: TFPHttpClient;
  FS: TStream;
  //SL: TStringList;
begin
  { SSL initialization has to be done by hand here }
  InitSSLInterface;

  clientT := TFPHttpClient.Create(nil);
  FS := TFileStream.Create(filePathA, fmCreate or fmOpenWrite);

  try
    try
      clientT.AddHeader('User-Agent', 'Mozilla/5.0 (compatible; fpweb)');
      { Allow redirections }
      clientT.AllowRedirect := True;
      clientT.Get(urlA, FS);

      Result := '';
    except
      on E: Exception do
      begin
        Result := errStr(E.Message);
        //exit;
      end;
    end;
  finally
    FS.Free;
    clientT.Free;
  end;

  //{ Test our file }
  //if FileExists(Filename) then
  //try
  //  SL := TStringList.Create;
  //  SL.LoadFromFile(Filename);
  //  writeln(SL.Text);
  //finally
  //  SL.Free;
  //end;
end;

class function tk.getWeb(urlA: string; const optsA: array of string): string;
var
  clientT: TFPHttpClient;
  tmps: rawbytestring;
begin
  InitSSLInterface;

  clientT := TFPHttpClient.Create(nil);

  try
    try
      clientT.AddHeader('User-Agent', 'Mozilla/5.0 (compatible; fpweb)');
      clientT.AllowRedirect := True;

      tmps := clientT.simpleGet(urlA);

      Result := tmps;
    except
      on E: Exception do
      begin
        Result := errStr(E.Message);
        //exit;
      end;
    end;
  finally
    clientT.Free;
  end;
end;

class function tk.joinPath(argsA: array of string): string;
begin
  Result := concatPaths(argsA);
end;

class function tk.toStr(const AValue; ATypeInfo: PTypeInfo): string;
type
  TArray = array of byte;
var
  I: longint;
  FormatSettings: TFormatSettings;
  FirstField, Field: PManagedField;
  ElementSize: SizeInt;
begin
  FormatSettings := DefaultFormatSettings;
  case ATypeInfo^.Kind of
    tkChar: Result := QuotedStr(char(AValue));
    tkWChar: Result := QuotedStr(ansistring(WChar(AValue)));
    tkBool: Result := SysUtils.BoolToStr(boolean(AValue), True);
    tkInt64: Result := IntToStr(int64(AValue));
    tkQWord: Result := IntToStr(QWord(AValue));
    tkSString: Result := QuotedStr(shortstring(AValue));
    tkAString: Result := QuotedStr(ansistring(AValue));
    tkWString: Result := QuotedStr(ansistring(WideString(AValue)));
    tkUString: Result := QuotedStr(ansistring(unicodestring(AValue)));
    tkClass: Result := TObject(AValue).ToString;
    tkEnumeration: Result := GetEnumName(ATypeInfo, integer(AValue));
    tkSet: Result := SetToString(ATypeInfo, integer(AValue), True).Replace(',', ', ');
    tkVariant: Result := VarToStr(variant(AValue));
    tkInteger:
    begin
      case GetTypeData(ATypeInfo)^.OrdType of
        otSByte: Result := IntToStr(shortint(AValue));
        otUByte: Result := IntToStr(byte(AValue));
        otSWord: Result := IntToStr(smallint(AValue));
        otUWord: Result := IntToStr(word(AValue));
        otSLong: Result := IntToStr(longint(AValue));
        otULong: Result := IntToStr(longword(AValue));
      end;
    end;
    tkFloat:
    begin
      FillByte(FormatSettings, SizeOf(TFormatSettings), 0);
      FormatSettings.DecimalSeparator := '.';
      case GetTypeData(ATypeInfo)^.FloatType of
        ftSingle: Result := FormatFloat('0.######', single(AValue), FormatSettings);
        ftDouble: Result := FormatFloat('0.######', double(AValue), FormatSettings);
        ftExtended: Result := FormatFloat('0.######', extended(AValue), FormatSettings);
        ftComp: Result := FormatFloat('0.######', comp(AValue), FormatSettings);
        ftCurr: Result := FormatFloat('0.######', currency(AValue), FormatSettings);
      end;
    end;
    tkRecord:
    begin
      Result := '(';
      with GetTypeData(ATypeInfo)^ do
      begin
        {$IFNDEF VER3_0}//ifdef needed because of a field rename in trunk (ManagedFldCount to TotalFieldCount)
        FirstField := PManagedField(pbyte(@TotalFieldCount) + SizeOf(TotalFieldCount));
        for I := 0 to TotalFieldCount - 1 do
          {$ELSE}
          FirstField := PManagedField(pbyte(@ManagedFldCount) + SizeOf(ManagedFldCount));
        for I := 0 to ManagedFldCount - 1 do
          {$ENDIF}
        begin
          if I > 0 then Result += ', ';
          Field := PManagedField(pbyte(FirstField) + (I * SizeOf(TManagedField)));
          Result += ToStr((pbyte(@AValue) + Field^.FldOffset)^, Field^.TypeRef);
        end;
      end;
      Result += ')';
    end;
    tkArray:
    begin
      Result := '[';
      with GetTypeData(ATypeInfo)^ do
      begin
        ElementSize := ArrayData.Size div ArrayData.ElCount;
        for I := 0 to ArrayData.ElCount - 1 do
        begin
          if I > 0 then Result += ', ';
          Result += ToStr((pbyte(@AValue) + (I * ElementSize))^, ArrayData.ElType);
        end;
      end;
      Result += ']';
    end;
    tkDynArray:
    begin
      Result := '[';
      with GetTypeData(ATypeInfo)^ do
      begin
        for I := 0 to Length(TArray(AValue)) - 1 do
        begin
          if I > 0 then Result += ', ';
          Result += ToStr((pbyte(@TArray(AValue)[0]) + (I * ElSize))^, ElType2);
        end;
      end;
      Result += ']';
    end;
    else
      Result := Format('%s@%p', [ATypeInfo^.Name, @AValue]);
  end;
end;

class function tk.isStringNullOrEmpty(strA: string): boolean;
begin
  if Length(strA) < 1 then
  begin
    Result := True;
  end
  else
  begin
    Result := False;
  end;
end;

class procedure tk.pass;
begin

end;

class function tk.isErrStr(strA: string): boolean;
begin
  if leftStr(strA, 8) = 'TXERROR:' then
    Result := True
  else
    Result := False;
end;

class function tk.getErrStr(strA: string): string;
begin
  if leftStr(strA, 8) = 'TXERROR:' then
    Result := midStr(strA, 9, length(strA) - 8)
  else
    Result := strA;
end;

// mrNone, mrOK, mrCancel, mrAbort, mrRetry, mrIgnore, mrYes, mrNo, mrAll, mrNoToAll, mrYesToAll, mrClose, mrLast
class function tk.showError(titleA, msgA: string): TModalResult;
begin
  Result := messageDlg(titleA, msgA, mtError, [mbClose], 0);
end;

class function tk.showInfo(titleA, msgA: string): TModalResult;
begin
  Result := messageDlg(titleA, msgA, mtInformation, [mbClose], 0);
end;

class function tk.boolToStr(boolA: boolean): string;
begin
  if boolA then exit('true')
  else
    exit('false');
end;

class function tk.isHex(strA: string): boolean;
var
  c: char;
begin
  for c in strA do
  begin
    if not ((('0' <= c) and (c <= '9'))  // C is in range 0..9
      or (('a' <= c) and (c <= 'z'))  // C is in range a..z
      or (('A' <= c) and (c <= 'Z'))  // C is in range A..Z
      ) then exit(False);
  end;

  exit(True);
end;

class function tk.encryptStringByTXTE(originStr: string; code: string): string;
var
  tmpStr: string;
  I: integer;
  tmpB: byte;
  codeI: string;
  codeLenT: integer;
begin
  tmpStr := '';
  if code = '' then
  begin
    codeI := 'topxeq';
  end
  else
  begin
    codeI := code;
  end;

  codeLenT := Length(codeI);

  for I := 1 to Length(originStr) do
  begin
    tmpB := byte(originStr[I]) + byte(codeI[((I - 1) mod codeLenT) + 1]) + byte(I);
    tmpStr := tmpStr + IntToHex(tmpB, 2);
  end;

  Result := tmpStr;
end;

class function tk.decryptStringByTXTE(originStr: string; code: string): string;
var
  tmpStr: string;
var
  I: integer;
var
  tmpB: byte;
var
  codeI: string;
  codeLenT: integer;
  tmps: string;
begin
  originStr := trim(originStr);

  tmpStr := '';
  if code = '' then
  begin
    codeI := 'topxeq';
  end
  else
  begin
    codeI := code;
  end;

  codeLenT := Length(codeI);

  for I := 1 to Length(originStr) div 2 do
  begin
    //tmps := originStr[(I - 1) * 2 + 1] +
    //  originStr[(I - 1) * 2 + 2];
    //tmpB := byte(hex2Dec(tmps));
    //tmpB := tk.HexToByte(originStr[(I - 1) * 2 + 1] +
    //  originStr[(I - 1) * 2 + 2]);
    tmpB := byte(hex2Dec(originStr[(I - 1) * 2 + 1] + originStr[(I - 1) * 2 + 2])) -
      byte(codeI[((I - 1) mod codeLenT) + 1]) - byte(I);
    tmpStr := tmpStr + Chr(tmpB);
  end;

  Result := tmpStr;
end;

class function tk.hexToByte(hexA: string): byte;
  //const
  //  ValoresHexa: array ['A' .. 'F'] of integer = (10, 11, 12, 13, 14, 15);
  //var
  //  nDecimal: byte;
  //  nIndex: byte;
  //begin
  //  nDecimal := 0;
  //  hexA := Uppercase(hexA);
  //  for nIndex := Length(hexA) downto 1 do
  //    if hexA[nIndex] in ['0' .. '9'] then
  //      nDecimal := nDecimal + StrToInt(hexA[nIndex]) *
  //        Trunc(Exp((Length(hexA) - nIndex) * ln(16)))
  //    else
  //      nDecimal := nDecimal + ValoresHexa[hexA[nIndex]] *
  //        Trunc(Exp((Length(hexA) - nIndex) * ln(16)));

  //  Result := nDecimal;
  //end;
begin
  Result := byte(hex2Dec(hexA));
end;

class function tk.parseCommandLine(originStr: string; var argList: TStringList): string;
var
  tmpS: string;
  p: integer;
begin
  if argList <> nil then
  begin
    if Length(originStr) < 1 then
    begin
      argList.Clear;
      exit('');
    end;

    argList.Clear;

    tmpS := trim(originStr);
    if tmpS = '' then
    begin
      exit('');
    end;

    repeat
      if (tmpS[1] = '"') then
      begin
        tmpS := trim(RightBStr(tmpS, Length(tmpS) - 1));
        p := Pos('"', tmpS);
        if (p > 0) then
        begin
          argList.Add(trim(LeftBStr(tmpS, p - 1)));
          tmpS := trim(RightBStr(tmpS, Length(tmpS) - p + 1));
          if (tmpS = '"') then
          begin
            tmpS := '';
            break;
          end;

          if tmpS[1] = '"' then
            tmpS := trim(RightBStr(tmpS, Length(tmpS) - 1));
        end;
      end
      else
      begin
        p := Pos(' ', tmpS);
        if (p > 0) then
        begin
          argList.Add(trim(LeftBStr(tmpS, p - 1)));
          tmpS := trim(RightBStr(tmpS, Length(tmpS) - p + 1));
        end;
      end;
    until p < 1;

    if (tmpS <> '') then
      argList.Add(tmpS);

    exit('');
  end;

  exit('TXERROR:output list is nil');
end;

class function tk.toJson(valueA: TObject): string;
var
  jsoSerialize: TJSONStreamer;
  classNameT: string;
  tmps: string;
  strAryT: array of string;
  jsonAryT: TJSONArray;
  jsonObjectT: TJSONObject;
begin
  Result := '';

  classNameT := valueA.ClassName;

  case classNameT of
    'TStringList': begin
      jsonAryT := TJSONArray.Create;

      for tmps in TStringList(valueA) do
      begin
        jsonAryT.Add(tmps);
      end;

      tmps := jsonAryT.AsJson;

      FreeAndNil(jsonAryT);

      exit(tmps);
    end;
    'TDictionary<System.AnsiString,System.AnsiString>': begin
      jsonObjectT := TJSONObject.Create;

      strAryT := tkStrMap(valueA).Keys.ToArray;

      for tmps in strAryT do
      begin
        jsonObjectT.Add(tmps, tkStrMap(valueA).Items[tmps]);
      end;

      tmps := jsonObjectT.AsJson;

      FreeAndNil(jsonObjectT);

      exit(tmps);
    end;
  end;

  jsoSerialize := TJSONStreamer.Create(nil);

  try
    Result := jsoSerialize.ObjectToJSONString(valueA);
  finally
    jsoSerialize.Free;
  end;

end;

class function tk.getArrayItem(listA: TStringList; idxA: integer;
  defaultA: string): string;
begin
  if listA = nil then
  begin
    exit(defaultA);
  end;

  if (idxA < 0) or (idxA >= listA.Count) then
  begin
    exit(defaultA);
  end;

  exit(listA[idxA]);
end;

class function tk.newStrMap(pairsA: array of string): tkStrMap;
var
  rListT: tkStrMap;
  tmps: string;
  cntT: integer;
  lenT: integer;
begin
  rListT := tkStrMap.Create;

  lenT := length(pairsA);

  cntT := 0;
  while cntT < lenT do
  begin
    if cntT + 1 >= lenT then
      break;

    rListT.addOrSetValue(pairsA[cntT], pairsA[cntT + 1]);

    cntT := cntT + 2;
  end;

  exit(rListT);

end;

class function tk.strSlice(strA: string; fromA: integer; toA: integer = -1): string;
var
  lenT, toT: integer;
begin
  lenT := UTF8Length(strA);

  if (fromA < 1) or (fromA > lenT) then
  begin
    exit(tk.errStr('start index over range'));
  end;

  toT := toA;

  if (toT < 1) then toT := lenT;

  if (toT < 1) or (toT > lenT) then
  begin
    exit(tk.errStr('end index over range'));
  end;

  exit(UTF8Copy(strA, fromA, toT - fromA));
end;

class function tk.strLen(strA: string): integer;
begin
  exit(UTF8Length(strA));
end;

class function tk.strSplitLines(strA: string): TStringArray;
var
  tmps: string;
begin
  tmps := strA.Replace(#13, '', [rfReplaceAll]);
  exit(tmps.Split(#10));
end;

class function tk.strStartsWith(strA: string; subStrA: string): boolean;
begin
     exit(startsStr(subStrA, strA));
end;

end.
