unit tkunit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, Forms, Dialogs, Controls, StdCtrls, ExtCtrls, Buttons,
  SysUtils, StrUtils, Graphics, httpprotocol, fphttpclient,
  openssl, opensslsockets, Variants, TypInfo, fpjsonrtti, fpjson,
  Generics.Collections, lazutf8, Math;

type

  txStrMap = specialize TDictionary<string, string>;

  txStrIntMap = specialize TDictionary<string, integer>;

  txIntStrMap = specialize TDictionary<integer, string>;

  SimpleFlexObject = class
  public
    isValid: boolean;
    items: TStringArray;
    itemsMap: txStrIntMap;
    keysMap: txIntStrMap;
  public
    constructor Create;
    constructor Create(strA: string; mustA: boolean = True);

    destructor Destroy; override;

    procedure decode(strA: string; mustA: boolean = True);
    function toStr(): string;

    function setMapItem(keyA: string; valueA: string): string;
    function getMapItem(strA: string; defaultA: string = ''): string;
    function encode(defaultA: string = ''): string;
  end;

  tk = class
  public
    // is related
    class function isHex(strA: string): boolean;
    class function isCharHex(ch: char): boolean;
    class function isCharWord(ch: char): boolean;

    // string related
    class function isStringNullOrEmpty(strA: string): boolean;
    class function strSlice(strA: string; fromA: integer; toA: integer = -1): string;
    class function strLen(strA: string): integer;

    class function trim(strA: string): string;
    class function trim(strA: WideString): WideString;
    class function toStr(const AValue; ATypeInfo: PTypeInfo): string;

    class function strStartsWith(strA: string; subStrA: string): boolean;

    class function strSplitLines(strA: string): TStringArray;
    class function strSplit(strA: string; sepA: string): TStringArray;

    // error related
    class function errStr(strA: string): string;
    class function isErrStr(strA: string): boolean;
    class function getErrStr(strA: string): string;

    // array/list related
    class function getArrayItem(listA: TStringList; idxA: integer;
      defaultA: string = ''): string;

    // map/dictionary related
    class function newStrMap(pairsA: array of string): txStrMap;
    class function getMapItem(mapA: txStrMap; keyA: string;
      defaultA: string = ''): string;
    class function getMapItem(mapA: SimpleFlexObject; keyA: string;
      defaultA: string = ''): string;

    // random related
    class function realRandom(range: integer): integer;
    class function realRandomByRange(range1: integer; range2: integer): integer;
    class function getRandomByte(): byte;

    // time related
    class function getNowStr(): string;
    class function getNowStrCompact(): string;

    // encode/decode related
    class function urlEncode(strA: string): string;
    class function urlDecode(strA: string): string;

    // json related
    class function toJson(valueA: TObject): string;
    class function getJsonPathString(jsonDataA: TJSONData; pathA: string;
      defaultA: string = ''): string;
    class function jsonToStrMap(jsonA: string): txStrMap;
    class function jsonDicToSimpleFlexObject(jsonA: string): SimpleFlexObject;
    class function arrayMapToJson(aryA: array of const): string;

    // encrypt/decrypt related
    class function encryptStringByTXTE(originStr: string; code: string = ''): string;
    class function decryptStringByTXTE(originStr: string; code: string = ''): string;

    class function encryptStringByTXDEF(strA: string; codeA: string = ''): string;
    class function decryptStringByTXDEF(strA: string; codeA: string = ''): string;

    // command-line related
    class function getCliParams(): TStringList;
    class function getCliParam(idxA: integer): string;
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
    class function byteToHex(byteA: byte): string;

    // network related

    // web related
    class function downloadFile(urlA, filePathA: string): string;
    class function getWeb(urlA: string; const optsA: array of string): string;

    // graphic related

    class function hexToColor(strA: string; defaultA: TColor = clWhite): TColor;
    class function colorToHex(colorA: TColor): string;

    // gui related
    class function showInfo(titleA, msgA: string; optsA: string = ''): string;
    class function showError(titleA, msgA: string; optsA: string = ''): string;

    class function ShowMessage(titleA, promptA: string; optsA: string = ''): string;

    class function getPassword(captionA, promptA: string; optsA: string = ''): string;
    class function selectItem(captionA: string; promptA: string;
      itemsA: string; optsA: string = ''): string;
    class function login(captionA, promptA: string; optsA: string): string;

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

class function tk.getCliParams(): TStringList;
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

class function tk.getCliParam(idxA: integer): string;
var
  i: integer;
  cntT: integer;
begin
  result := '';

  cntT := 0;

  for i := 0 to ParamCount do
  begin
    if ParamStr(i).StartsWith('-') then begin
      continue;
    end;

    if cntT = idxA then begin
      exit(ParamStr(i));
    end;

    cntT += 1;
  end;

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

// uses TypInfo
//addMessage('FormKeyUp: ' + IntToStr(Key) + ' ' + tk.toStr(Shift, typeInfo(Shift)));
class function tk.toStr(const AValue; ATypeInfo: PTypeInfo): string;
type
  TArray = array of byte;
var
  I: longint;
  FormatSettings: TFormatSettings;
  FirstField, Field: PManagedField;
  ElementSize: SizeInt;
  typeNameT: string;
  map1T: txStrMap;
  tmps: string;
  sbufT: TStringBuilder;
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
    tkClass: begin
      typeNameT := TObject(AValue).ToString;

      if typeNameT = 'TDictionary<System.AnsiString,System.AnsiString>' then
        //'TDictionary<System.AnsiString,System.AnsiString>':
      begin
        //Result := '{';
        //with GetTypeData(ATypeInfo)^ do
        //begin
        //  {$IFNDEF VER3_0}//ifdef needed because of a field rename in trunk (ManagedFldCount to TotalFieldCount)
        //  FirstField := PManagedField(pbyte(@TotalFieldCount) + SizeOf(TotalFieldCount));
        //  for I := 0 to TotalFieldCount - 1 do
        //    {$ELSE}
        //    FirstField := PManagedField(pbyte(@ManagedFldCount) + SizeOf(ManagedFldCount));
        //  for I := 0 to ManagedFldCount - 1 do
        //    {$ENDIF}
        //  begin
        //    if I > 0 then Result += ', ';
        //    Field := PManagedField(pbyte(FirstField) + (I * SizeOf(TManagedField)));
        //    Result += ToStr((pbyte(@AValue) + Field^.FldOffset)^, Field^.TypeRef);
        //  end;
        //end;
        //Result += '}';

        map1T := txStrMap(AValue);

        sbufT := TStringBuilder.Create;

        sbufT.append('{');

        i := 0;
        for tmps in map1T.Keys do
        begin
          if i > 0 then
          begin
            sbufT.append(', ');
          end;

          sbufT.append(tmps.QuotedString('"'));
          sbufT.append(': ');
          sbufT.append(map1T[tmps].QuotedString('"'));

          i += 1;
        end;


        sbufT.append('}');

        Result := sbufT.ToString();
        FreeAndNil(sbufT);

      end
      else
      begin
        Result := TObject(AValue).ToString;
        //Result := Format('%s@%p', [typeNameT, @AValue]);
      end;

    end;
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
      Result := Format('%s@%p', [typeNameT, @AValue]);

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
class function tk.showError(titleA, msgA: string; optsA: string = ''): string;
  //begin
  //  Result := messageDlg(titleA, msgA, mtError, [mbClose], 0);
var
  cliListT: TStringList;
  closeLabelT, titleT: string;
  dlgTypeT: TMsgDlgType = mtInformation;
  res: TModalResult;
begin

  cliListT := TStringList.Create;

  tk.parseCommandLine(optsA, cliListT);

  titleT := tk.getSwitch(cliListT, '-title=', titleA);

  closeLabelT := tk.getSwitch(cliListT, '-closeLabel=', 'Close');

  FreeAndNil(cliListT);

  res := QuestionDlg(titleT, msgA, mtError, [mrClose, closeLabelT,
    'IsDefault', 'IsCancel'], 0);

  Result := IntToStr(res);
end;

//class function tk.showInfo(titleA, msgA: string): TModalResult;
//begin
//  Result := messageDlg(titleA, msgA, mtInformation, [mbClose], 0);
//end;
class function tk.showInfo(titleA, msgA: string; optsA: string = ''): string;
  //begin
  //  Result := messageDlg(titleA, msgA, mtError, [mbClose], 0);
var
  cliListT: TStringList;
  closeLabelT, titleT: string;
  res: TModalResult;
begin

  cliListT := TStringList.Create;

  tk.parseCommandLine(optsA, cliListT);

  titleT := tk.getSwitch(cliListT, '-title=', titleA);

  closeLabelT := tk.getSwitch(cliListT, '-closeLabel=', 'Close');

  FreeAndNil(cliListT);

  res := QuestionDlg(titleT, msgA, mtInformation,
    [mrClose, closeLabelT, 'IsDefault', 'IsCancel'], 0);

  Result := IntToStr(res);
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

      strAryT := txStrMap(valueA).Keys.ToArray;

      for tmps in strAryT do
      begin
        jsonObjectT.Add(tmps, txStrMap(valueA).Items[tmps]);
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

class function tk.newStrMap(pairsA: array of string): txStrMap;
var
  rListT: txStrMap;
  tmps: string;
  cntT: integer;
  lenT: integer;
begin
  rListT := txStrMap.Create;

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

class function tk.strSplit(strA: string; sepA: string): TStringArray;
begin
  exit(strA.Split(sepA));
end;

class function tk.strStartsWith(strA: string; subStrA: string): boolean;
begin
  exit(startsStr(subStrA, strA));
end;

class function tk.getJsonPathString(jsonDataA: TJSONData; pathA: string;
  defaultA: string): string;
var
  tmps: string;
begin
  if jsonDataA = nil then exit(defaultA);

  try
    tmps := jsonDataA.GetPath(pathA).AsString;
  except
    on E: Exception do
    begin
      exit(defaultA);
    end;
  end;

  exit(tmps);
end;

class function tk.selectItem(captionA: string; promptA: string;
  itemsA: string; optsA: string): string;
var
  aForm: TForm;
  labelA: TLabel;
  listBoxT: TListBox;
  Cancel: TBitBtn;
  Ok: TBitBtn;
  Top, Left, Width: integer;
  cliListT: TStringList;
  rs, tmps: string;
  okLabelT, closeLabelT: string;
  itemListT: TStringArray;
begin

  cliListT := TStringList.Create;

  tk.parseCommandLine(optsA, cliListT);

  okLabelT := tk.getSwitch(cliListT, '-okLabel=', 'OK');
  closeLabelT := tk.getSwitch(cliListT, '-closeLabel=', 'Close');

  FreeAndNil(cliListT);

  Top := 0;
  Left := 0;
  Width := 300;

  aForm := TForm.Create(nil);
  aForm.Position := poDesktopCenter;
  aForm.Top := Top;
  aForm.Left := Left;
  aForm.Width := Width;
  aForm.Height := 300;
  aForm.Constraints.MinWidth := 300;
  aForm.Caption := captionA;

  labelA := TLabel.Create(aForm);
  labelA.Parent := aForm;
  labelA.Top := 10;
  labelA.Left := 10;
  labelA.Caption := promptA;
  labelA.AutoSize := True;

  Cancel := TBitBtn.Create(aForm);
  Cancel.Parent := aForm;
  Cancel.Top := aForm.ClientHeight - Cancel.Height - 10;
  Cancel.Left := Width div 2 + 10;
  Cancel.Kind := bkCancel;
  Cancel.Caption := closeLabelT;

  ok := TBitBtn.Create(aForm);
  ok.Parent := aForm;
  ok.Top := aForm.ClientHeight - Cancel.Height - 10;
  ok.Left := Width div 2 - cancel.Width - 10;
  Ok.Kind := bkOK;
  Ok.Caption := okLabelT;

  listBoxT := TListBox.Create(aForm);
  listBoxT.Parent := aForm;
  listBoxT.Top := labelA.Top + labelA.Height + 10;
  listBoxT.Left := 10;
  listBoxT.Width := Width - 20;
  listBoxT.Height :=
    aForm.ClientHeight - (labelA.Top + labelA.Height + 10) - (Cancel.Height + 10 + 10);

  itemListT := tk.strSplitLines(itemsA);

  for tmps in itemListT do
  begin
    listBoxT.Items.Add(trim(tmps));
  end;

  //freeAndNil(itemListT);

  Result := 'TXERROR:not selected';
  if ((not (aForm.ShowModal = mrCancel)) and (listBoxT.ItemIndex >= 0)) then
  begin
    Result := listBoxT.items[listBoxT.ItemIndex];
  end;

  aForm.Free;

end;

class function tk.getPassword(captionA, promptA: string; optsA: string): string;
var
  aForm: TForm;
  aLabel: TLabel;
  Edit: TEdit;
  Cancel: TBitBtn;
  Ok: TBitBtn;
  Top, Left, Width: integer;
  cliListT: TStringList;
  okLabelT, closeLabelT: string;
  rs, tmps: string;
begin

  cliListT := TStringList.Create;

  tk.parseCommandLine(optsA, cliListT);

  okLabelT := tk.getSwitch(cliListT, '-okLabel=', 'OK');
  closeLabelT := tk.getSwitch(cliListT, '-closeLabel=', 'Close');

  FreeAndNil(cliListT);

  Top := 0;
  Left := 0;
  Width := 300;


  aForm := TForm.Create(nil);
  aForm.Top := Top;
  aForm.Left := Left;
  aForm.Width := Width;
  aForm.Height := 110;
  aForm.Constraints.MinWidth := 200;
  aForm.Caption := captionA;
  aForm.Position := poDesktopCenter;

  aLabel := TLabel.Create(aForm);
  aLabel.Parent := aForm;
  aLabel.Top := 10;
  aLabel.Left := 20;
  aLabel.Caption := promptA;
  aLabel.AutoSize := True;

  Edit := TEdit.Create(aForm);
  Edit.Parent := aForm;
  Edit.Top := 30;
  Edit.Left := 20;
  Edit.Width := Width - 40;
  Edit.PasswordChar := '*';
  Edit.Text := '';

  Cancel := TBitBtn.Create(aForm);
  Cancel.Parent := aForm;
  Cancel.Top := 65;
  Cancel.Left := Width - 95;
  Cancel.Kind := bkCancel;
  Cancel.Caption := closeLabelT;

  ok := TBitBtn.Create(aForm);
  ok.Parent := aForm;
  ok.Top := 65;
  ok.Left := Width - 180;
  Ok.Kind := bkOK;
  Ok.Caption := okLabelT;

  Result := 'TXERROR:canceled';
  if not (aForm.ShowModal = mrCancel) then
    Result := Edit.Text;

  aForm.Free;

end;

class function tk.login(captionA, promptA: string; optsA: string): string;
var
  aForm: TForm;
  aLabel: TLabel;
  editUserT, editPasswordT: TEdit;
  Cancel: TBitBtn;
  Ok: TBitBtn;
  Top, Left, Width: integer;
  cliListT: TStringList;
  okLabelT, closeLabelT: string;
  userLabelT, passwordLabelT: string;
  defaultUserT, defaultPasswordT: string;
  rs, tmps: string;
begin

  cliListT := TStringList.Create;

  tk.parseCommandLine(optsA, cliListT);

  okLabelT := tk.getSwitch(cliListT, '-okLabel=', 'OK');
  closeLabelT := tk.getSwitch(cliListT, '-closeLabel=', 'Close');

  userLabelT := tk.getSwitch(cliListT, '-userLabel=', 'User: ');
  passwordLabelT := tk.getSwitch(cliListT, '-passwordLabel=', 'Password: ');

  defaultUserT := tk.getSwitch(cliListT, '-defaultUser=', '');
  defaultPasswordT := tk.getSwitch(cliListT, '-defaultPassword=', '');

  FreeAndNil(cliListT);

  Top := 0;
  Left := 0;
  Width := 300;


  aForm := TForm.Create(nil);
  aForm.Top := Top;
  aForm.Left := Left;
  aForm.Width := Width;
  aForm.Height := 150;
  aForm.Constraints.MinWidth := 200;
  aForm.Caption := captionA;
  aForm.Position := poDesktopCenter;

  aLabel := TLabel.Create(aForm);
  aLabel.Parent := aForm;
  aLabel.Top := 10;
  aLabel.Left := 20;
  aLabel.Caption := promptA;
  aLabel.AutoSize := True;

  editUserT := TEdit.Create(aForm);
  editUserT.Parent := aForm;
  editUserT.Top := 30;
  editUserT.Left := 20;
  editUserT.Width := Width - 40;
  //editUserT.PasswordChar := '#0';
  editUserT.Text := defaultUserT;
  editUserT.TextHint := userLabelT;

  editPasswordT := TEdit.Create(aForm);
  editPasswordT.Parent := aForm;
  editPasswordT.Top := 60;
  editPasswordT.Left := 20;
  editPasswordT.Width := Width - 40;
  editPasswordT.PasswordChar := '*';
  editPasswordT.Text := defaultPasswordT;
  editPasswordT.TextHint := passwordLabelT;

  Cancel := TBitBtn.Create(aForm);
  Cancel.Parent := aForm;
  Cancel.Top := 105;
  Cancel.Left := Width - 95;
  Cancel.Kind := bkCancel;
  Cancel.Caption := closeLabelT;

  ok := TBitBtn.Create(aForm);
  ok.Parent := aForm;
  ok.Top := 105;
  ok.Left := Width - 180;
  Ok.Kind := bkOK;
  Ok.Caption := okLabelT;

  Result := 'TXERROR:canceled';
  if not (aForm.ShowModal = mrCancel) then
    Result := tk.arrayMapToJson(['user', editUserT.Text, "password", editPasswordT.Text]);

  aForm.Free;

end;

class function tk.arrayMapToJson(aryA: array of const): string;
var
  tmps: string;
  objT: TJSONObject;
begin
  objT := TJSONObject.Create(aryA);

  result := objT.AsJSON;

  freeAndNil(objT);
  //for tmps in strsA do begin
  //
  //end;
end;

class function tk.ShowMessage(titleA, promptA: string; optsA: string = ''): string;
var
  cliListT: TStringList;
  closeLabelT, okLabelT: string;
  typeT: string;
  dlgTypeT: TMsgDlgType = mtInformation;
  res: TModalResult;
begin

  cliListT := TStringList.Create;

  tk.parseCommandLine(optsA, cliListT);

  typeT := tk.getSwitch(cliListT, '-type=', 'info');

  okLabelT := tk.getSwitch(cliListT, '-okLabel=', 'Confirm');

  closeLabelT := tk.getSwitch(cliListT, '-closeLabel=', 'Close');

  FreeAndNil(cliListT);

  case typeT of
    '', 'info': dlgTypeT := mtInformation;
    'error': dlgTypeT := mtError;
    'warning': dlgTypeT := mtWarning;
    'confirm': begin
      dlgTypeT := mtConfirmation;

      res := QuestionDlg(titleA, promptA, dlgTypeT,
        [mrYes, okLabelT, mrClose, closeLabelT, 'IsDefault', 'IsCancel'], 0);

      Result := IntToStr(res);
      exit;
    end;
  end;

  res := QuestionDlg(titleA, promptA, dlgTypeT, [mrClose, closeLabelT,
    'IsDefault', 'IsCancel'], 0);

  Result := IntToStr(res);
end;

//class function tk.hexToColor(strA: string): TColor;
//var
//  tmps: string;
//begin
//  exit(StringToColor('$'+strA.Replace('#', '', [rfReplaceAll]).Replace('$', '', [rfReplaceAll])));
//end;

class function tk.isCharWord(ch: char): boolean;
begin
  Result := ch in ['a'..'z', 'A'..'Z', '_', '0'..'9'];
end;

class function tk.isCharHex(ch: char): boolean;
begin
  Result := ch in ['0'..'9', 'a'..'f', 'A'..'F'];
end;

class function tk.colorToHex(colorA: TColor): string;
var
  N: longint;
begin
  if colorA = clNone then
  begin
    Result := '';
    exit;
  end;
  N := ColorToRGB(colorA);
  Result := '#' + IntToHex(Red(N), 2) + IntToHex(Green(N), 2) + IntToHex(Blue(N), 2);
end;

class function tk.hexToColor(strA: string; defaultA: TColor): TColor;
var
  N1, N2, N3: integer;
  i: integer;
  len: integer;
begin
  Result := defaultA;
  Len := 0;
  if (strA <> '') and (strA[1] = '#') then Delete(strA, 1, 1);
  if (strA = '') then exit;

  //delete after first nonword char
  i := 1;
  while (i <= Length(strA)) and tk.isCharWord(strA[i]) do Inc(i);
  Delete(strA, i, Maxint);

  //allow only #rgb, #rrggbb
  Len := Length(strA);
  if (Len <> 3) and (Len <> 6) then exit;

  for i := 1 to Len do
    if not tk.isCharHex(strA[i]) then exit;

  if Len = 6 then
  begin
    N1 := StrToInt('$' + Copy(strA, 1, 2));
    N2 := StrToInt('$' + Copy(strA, 3, 2));
    N3 := StrToInt('$' + Copy(strA, 5, 2));
  end
  else
  begin
    N1 := StrToInt('$' + strA[1] + strA[1]);
    N2 := StrToInt('$' + strA[2] + strA[2]);
    N3 := StrToInt('$' + strA[3] + strA[3]);
  end;

  Result := RGBToColor(N1, N2, N3);
end;

class function tk.realRandom(range: integer): integer;
var
  tmpi: integer;
begin
  tmpi := random(range);
  Result := tmpi;
end;

class function tk.realRandomByRange(range1: integer; range2: integer): integer;
var
  tmpi: integer;
begin
  tmpi := RandomRange(range1, range2);
  Result := tmpi;
end;

class function tk.getRandomByte(): byte;
var
  tmpi: integer;
begin
  tmpi := random(256);
  Result := tmpi;
end;

class function tk.byteToHex(byteA: byte): string;
begin
  exit(hexStr(byteA, 2));
end;

class function tk.encryptStringByTXDEF(strA: string; codeA: string): string;
var
  //tmpStr: string;
  i: integer;
  //tmpB: byte;
  codeI: string;
  dataLenT, codeLenT, bufLenT: integer;
  bufT: TBytes;
  resBufT: TStringBuilder;
  sumT: byte;
  addLenT: integer;
  encIndexT: integer;
begin
  codeA := trim(codeA);

  //tmpStr := '';
  if codeA = '' then
  begin
    codeI := 'topxeq';
  end
  else
  begin
    codeI := codeA;
  end;

  codeLenT := Length(codeI);

  sumT := 0;

  for i := 1 to codeLenT do
  begin
    sumT := sumT + byte(codeI[i]);
  end;

  addLenT := (sumT mod 5) + 2;

  encIndexT := sumT mod addLenT;

  dataLenT := length(strA);

  bufLenT := dataLenT + addLenT;

  bufT := default(TBytes);
  setLength(bufT, bufLenT);

  for i := 0 to addLenT - 1 do
  begin
    bufT[i] := tk.getRandomByte();
  end;

  for i := 0 to dataLenT - 1 do
  begin
    bufT[i + addLenT] := byte(strA[i + 1]) + byte(codeI[(i mod codeLenT) + 1]) +
      byte(i + 1) + byte(bufT[encIndexT]);
  end;

  resBufT := TStringBuilder.Create;

  for i := 0 to bufLenT - 1 do
  begin
    resBufT.append(hexStr(bufT[i], 2));
  end;

  Result := resBufT.ToString;
  FreeAndNil(resBufT);

end;

class function tk.decryptStringByTXDEF(strA: string; codeA: string): string;
var
  //tmpStr: string;
  i: integer;
  //tmpB: byte;
  codeI: string;
  dataLenT, codeLenT, bufLenT: integer;
  bufT, buf0T: TBytes;
  resBufT: TStringBuilder;
  sumT: byte;
  addLenT: integer;
  encIndexT: integer;
begin
  strA := trim(strA);

  if strA.StartsWith('//TXDEF#') then
  begin
    strA := strA.Remove(8);
  end;

  codeA := trim(codeA);

  //tmpStr := '';
  if codeA = '' then
  begin
    codeI := 'topxeq';
  end
  else
  begin
    codeI := codeA;
  end;

  codeLenT := Length(codeI);

  sumT := 0;

  for i := 1 to codeLenT do
  begin
    sumT := sumT + byte(codeI[i]);
  end;

  addLenT := (sumT mod 5) + 2;

  encIndexT := sumT mod addLenT;

  bufLenT := length(strA) div 2;

  if bufLenT < addLenT then
    exit('TXERROR:invalid data');

  dataLenT := bufLenT - addLenT;

  buf0T := default(TBytes);
  setLength(buf0T, bufLenT);

  for i := 0 to bufLenT - 1 do
  begin
    buf0T[i] := hex2Dec(midStr(strA, i * 2 + 1, 2));
  end;

  bufT := default(TBytes);
  setLength(bufT, dataLenT);

  for i := 0 to dataLenT - 1 do
  begin
    bufT[i] := byte(buf0T[addLenT + i]) - byte(codeI[(i mod codeLenT) + 1]) -
      byte(i + 1) - byte(buf0T[encIndexT]);
  end;

  resBufT := TStringBuilder.Create;

  for i := 0 to dataLenT - 1 do
  begin
    resBufT.append(char(bufT[i]));
  end;

  Result := resBufT.ToString;
  FreeAndNil(resBufT);

end;

constructor SimpleFlexObject.Create;
begin
  inherited Create;

  self.items := default(TStringArray);
  setLength(self.items, 0);

  self.itemsMap := txStrIntMap.Create;

  self.keysMap := txIntStrMap.Create;

  self.isValid := True;
end;

constructor SimpleFlexObject.Create(strA: string; mustA: boolean);
begin
  inherited Create;

  self.items := default(TStringArray);
  setLength(self.items, 0);

  self.itemsMap := txStrIntMap.Create;

  self.keysMap := txIntStrMap.Create;

  self.isValid := True;

  self.decode(strA, mustA);
end;

destructor SimpleFlexObject.Destroy;
begin
  self.isValid := False;

  FreeAndNil(self.itemsMap);
  FreeAndNil(self.keysMap);

  setLength(self.items, 0);

  inherited;
end;

procedure SimpleFlexObject.decode(strA: string; mustA: boolean);
var
  i: integer;
  baseLenT, len0T, len1T: integer;
  tmps0: string;
  list0T, list1T, list2T: TStringArray;
begin
  list0T := strA.Split(['|||']);

  len0T := length(list0T);

  if len0T > 2 then
  begin
    tmps0 := list0T[1];
  end
  else
  begin
    if not mustA then
    begin
      self.isValid := False;
      exit;
    end;

    tmps0 := list0T[0];
  end;

  list1T := tmps0.Split(['$$']);

  len1T := length(list1T);

  baseLenT := length(self.items);

  setLength(self.items, baseLenT + len1T);

  for i := 0 to len1T - 1 do
  begin
    list2T := list1T[i].Split(['^^']);

    if length(list2T) > 1 then
    begin
      self.items[i + baseLenT] := list2T[1];
      self.itemsMap.AddOrSetValue(list2T[0], i + baseLenT);
      self.keysMap.AddOrSetValue(i + baseLenT, list2T[0]);
    end
    else
    begin
      self.items[i + baseLenT] := list2T[0];
    end;

    setLength(list2T, 0);
  end;

  setLength(list1T, 0);
  setLength(list0T, 0);

  //self.isValid := true;
end;

function SimpleFlexObject.toStr(): string;
var
  i: integer;
  tmpi: integer;
  len0T, len1T: integer;
  tmps: string;
  bufT: TStringBuilder;
begin
  bufT := TStringBuilder.Create;

  len0T := self.itemsMap.Count;
  len1T := length(self.items);

  bufT.append('{');

  i := 0;
  for tmps in self.itemsMap.Keys do
  begin
    if i <> 0 then
      bufT.append(', ');

    bufT.append(tmps);
    bufT.append(': ');
    bufT.append(IntToStr(self.itemsMap[tmps]));

    i := i + 1;
  end;
  bufT.append('}');

  bufT.append('{');

  i := 0;
  for tmpi in self.keysMap.Keys do
  begin
    if i <> 0 then
      bufT.append(', ');

    bufT.append(IntToStr(tmpi));
    bufT.append(': ');
    bufT.append(self.keysMap[tmpi]);

    i := i + 1;
  end;
  bufT.append('}');

  bufT.append('[');
  for i := 0 to len1T - 1 do
  begin
    if i <> 0 then
      bufT.append(', ');

    bufT.append(self.items[i].QuotedString('"'));

  end;
  bufT.append(']');

  Result := bufT.ToString;
  FreeAndNil(bufT);
end;

function SimpleFlexObject.getMapItem(strA: string; defaultA: string = ''): string;
var
  tmpi: integer;
  b: boolean;
begin
  if not self.isValid then exit(defaultA);

  b := self.itemsMap.TryGetValue(strA, tmpi);

  if not b then exit(defaultA);

  if (tmpi < 0) or (tmpi >= length(self.items)) then exit(defaultA);

  Result := self.items[tmpi];
end;

function SimpleFlexObject.setMapItem(keyA: string; valueA: string): string;
var
  tmpi: integer;
  b: boolean;
  baseLenT: integer;
begin
  if not self.isValid then exit(('TXERROR:object is invalid'));

  b := self.itemsMap.TryGetValue(keyA, tmpi);

  if b then
  begin
    self.items[tmpi] := valueA;
    exit('');
  end;

  baseLenT := length(self.items);

  setLength(self.items, baseLenT + 1);

  self.items[baseLenT] := valueA;
  self.itemsMap.AddOrSetValue(keyA, baseLenT);
  self.keysMap.AddOrSetValue(baseLenT, keyA);

  exit('');
end;

function SimpleFlexObject.encode(defaultA: string): string;
var
  len0T: integer;
  i, tmpi: integer;
  b: boolean;
  tmps0, tmps1: string;
  sbufT: TStringBuilder;
begin
  if not self.isValid then exit(defaultA);

  len0T := length(self.items);

  sbufT := TStringBuilder.Create;

  sbufT.append('|||');

  for i := 0 to len0T - 1 do
  begin
    if i <> 0 then sbufT.append('$$');

    b := self.keysMap.TryGetValue(i, tmps1);

    if b then
    begin
      sbufT.append(tmps1);
      sbufT.append('^^');
    end;

    sbufT.append(self.items[i]);

  end;

  sbufT.append('|||');

  Result := sbufT.ToString();

  FreeAndNil(sbufT);
end;

class function tk.getNowStr(): string;
var
  tmps: string;
begin
  DateTimeToString(tmps, 'yyyy-mm-dd hh:nn:ss', Now);

  Result := tmps;
end;

class function tk.getNowStrCompact(): string;
var
  tmps: string;
begin
  DateTimeToString(tmps, 'yyyymmddhhnnss', Now);

  Result := tmps;
end;

// remember to free the return value after use
class function tk.jsonToStrMap(jsonA: string): txStrMap;
var
  i: integer;
  tmps: string;
  //data1T: TJSONData;
  obj1T: TJSONObject;
  map1T: txStrMap;
  itemT: TJSONObject;
begin
  map1T := txStrMap.Create;

  try
    obj1T := GetJSON(jsonA) as TJSONObject;

    for i := 0 to obj1T.Count - 1 do
    begin
      tmps := obj1T.Names[i];
      map1T.AddOrSetValue(tmps, obj1T.FindPath(tmps).AsString);
    end;

  except
    on E: Exception do
    begin
      FreeAndNil(map1T);
      exit(nil);
    end;
  end;

  FreeAndNil(map1T);

  exit(map1T);
end;

// remember to free the return value after use
class function tk.jsonDicToSimpleFlexObject(jsonA: string): SimpleFlexObject;
var
  i: integer;
  tmps: string;
  //data1T: TJSONData;
  obj1T: TJSONObject;
  map1T: SimpleFlexObject;
  itemT: TJSONObject;
begin
  map1T := SimpleFlexObject.Create;

  try
    obj1T := GetJSON(jsonA) as TJSONObject;

    for i := 0 to obj1T.Count - 1 do
    begin
      tmps := obj1T.Names[i];
      map1T.setMapItem(tmps, obj1T.FindPath(tmps).AsString);
    end;

  except
    on E: Exception do
    begin
      if obj1T <> nil then  FreeAndNil(obj1T);
      FreeAndNil(map1T);
      exit(nil);
    end;
  end;

  if obj1T <> nil then  FreeAndNil(obj1T);


  exit(map1T);
end;

class function tk.getMapItem(mapA: txStrMap; keyA: string;
  defaultA: string = ''): string;
var
  tmps: string;
  b: boolean;
begin
  b := mapA.TryGetValue(keyA, tmps);

  if not b then exit(defaultA);

  exit(tmps);
end;

class function tk.getMapItem(mapA: SimpleFlexObject; keyA: string;
  defaultA: string = ''): string;
begin
  exit(mapA.getMapItem(keyA, defaultA));
end;

end.
