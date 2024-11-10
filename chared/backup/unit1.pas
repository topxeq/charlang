unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  StdCtrls, ExtCtrls, PairSplitter, Buttons, SynEdit,
  SynHighlighterAny, SynHighlighterCpp, SynHighlighterSQL,
  SynHighlighterJScript, SynHighlighterPas, tkunit, fphttpserver, fileutil,
  RegExpr, fpjson, Process, LazUTF8, SynEditTypes, Generics.Collections;

  // , ATStringProc, ATSynEdit_LineParts

const
  VersionG = '0.9.1';
  WebPortG = 7458;

type
  strHashMap = specialize TDictionary<string, string>;

  TfuncCharlangBackG = function(codeA, paramA, secureCodeA, injectA: PChar): PChar;
    stdcall;

  THTTPServerThread = class(TThread)
  private
    _Error: string;
  public
    Server: TFPHTTPServer;
    constructor Create(APort: word);      //  const OnRequest: THTTPServerRequestHandler
    destructor Destroy; override;
    procedure Execute; override;
    property Error: string read _Error;

  public
    msgTextM: string;

    guiCmdM: string;
    guiValue1M: string;
    guiValue2M: string;
    guiValue3M: string;

    guiOut1M: string;
    guiOut2M: string;

    procedure AddMessage;
    procedure doGuiCmd;
    procedure onRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
  end;

  TRunCharThread = class(TThread)
  private
    procedure AddMessage;
  protected
    procedure Execute; override;
  public
    msgTextM: string;

    codeTextM: string;

    secureCodeM: string;

    //threadModeM: string;

    constructor Create(CreateSuspended: boolean);
    destructor Destroy; override;
    //procedure onTerminateProc(Sender: TObject);
  end;

  TRunCharExtThread = class(TThread)
  private
    procedure AddMessage;
  protected
    procedure Execute; override;
  public
    msgTextM: string;

    codeTextM: string;

    secureCodeM: string;

    threadModeM: string;

    constructor Create(CreateSuspended: boolean);
    destructor Destroy; override;
  end;

  TInit1Thread = class(TThread)
  private
    procedure AddMessage;
    procedure ClearCombo;
    procedure AddCombo;
  protected
    procedure Execute; override;
  public
    msgTextM: string;

    addComboStrM: string;

    constructor Create(CreateSuspended: boolean);
    destructor Destroy; override;
  end;

  TDownload1Thread = class(TThread)
  private
    procedure AddMessage;
    procedure DoGuiTask;
  protected
    procedure Execute; override;
  public
    msgTextM: string;

    urlM: string;
    nrsM: string;
    taskModeM: string;

    constructor Create(CreateSuspended: boolean; urlA: string; taskModeA: string);
    destructor Destroy; override;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    ComboBox1: TComboBox;
    ImageList1: TImageList;
    LabeledEdit1: TLabeledEdit;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    OpenDialog1: TOpenDialog;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    SaveDialog1: TSaveDialog;
    Separator1: TMenuItem;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    StaticText1: TStaticText;
    injectHolder1: TStaticText;
    StatusBar1: TStatusBar;
    SynEdit1: TSynEdit;
    SynJScriptSyn1: TSynJScriptSyn;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    //procedure ATSynEdit1CalcHilite(Sender: TObject; var AParts: TATLineParts;
    //ALineIndex, ACharIndex, ALineLen: integer; var AColorAfterEol: TColor);
    //procedure ATSynEdit1ChangeModified(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure ComboBox1Select(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure MenuItem4Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SynEdit1Change(Sender: TObject);
    procedure SynEdit1StatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure ToolButton10Click(Sender: TObject);
    procedure ToolButton11Click(Sender: TObject);
    procedure ToolButton13Click(Sender: TObject);
    procedure ToolButton1Click(Sender: TObject);
    //procedure ToolButton10Click(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure ToolButton5Click(Sender: TObject);
    procedure ToolButton6Click(Sender: TObject);
    procedure ToolButton7Click(Sender: TObject);
    procedure ToolButton8Click(Sender: TObject);
    procedure ToolButton9Click(Sender: TObject);
  private

  public
    procedure AddMessage(msgA: string);
    procedure updateFileNameGui;
    procedure filterCombo1;

    function checkForSave(): boolean;

  end;

  // global variables
var
  libHandleG: THandle;

  httpServerThreadG: THTTPServerThread;
  funcCharlangBackG: TfuncCharlangBackG;

  runCharThreadG: TRunCharThread;
  runCharThreadSignalG: integer;

  runCharExtThreadG: TRunCharExtThread;
  runCharExtThreadSignalG: integer;

  filePathG: string;

  dllPathG: string;
  cliExePathG: string;

  examplesMapG: strHashMap;
  examplesListG: TStringList;

  injectG: string = 'pl := func(formatA, ...valuesA) { ' + #10 +
    '        rs1 := getWeb("http://127.0.0.1:7458", {"cmd": "pln", "value": spr(formatA, ...valuesA)})  '
    + #10 + '          return toInt(rs1, 0)      ' + #10 + '}' + #10 +
    #10 + 'pln := func(...valuesA) { ' + #10 +
    '        rs1 := getWeb("http://127.0.0.1:7458", {"cmd": "pln", "value": spln(...valuesA)})  '
    + #10 + '          return toInt(rs1, 0)      ' + #10 + '}' + #10;

  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  rs: string;
begin
  //DebugLn('start...');
  //tk.appendStringToFile('start...' + #10, 'd:\tmpx\abclog.txt');

  //FPHttpServer1.Active := true;

  // init global variables
  filePathG := '';
  runCharThreadSignalG := 0;
  runCharExtThreadSignalG := 0;
  examplesListG := TStringList.Create;

  injectG := stringReplace(injectHolder1.Caption, ':7458', ':' +
    IntToStr(webPortG), [rfReplaceAll]);

  // start http thread
  httpServerThreadG := THTTPServerThread.Create(WebPortG);

  // load Charlang dll
  //tk.appendStringToFile('['+DateTimeToStr(Now)+'] start...' + #10, 'd:\tmpx\abclog.txt');

  dllPathG := FindDefaultExecutablePath('char.dll');

  if tk.isStringNullOrEmpty(dllPathG) then
  begin
    if MessageDlg('Please confirm',
      'Required DLL not found, download now?(please wait patiently after downloading started)',
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      rs := tk.downloadFile('https://topget.org/pub/char.dll',
        tk.joinPath([tk.getAppDir(), 'char.dll']));
      if tk.isErrStr(rs) then
      begin
        ShowMessage(format('Failed to download required DLL file: %s',
          [tk.getErrStr(rs)]));
        application.Terminate;
      end;

      dllPathG := FindDefaultExecutablePath('char.dll');
    end;
  end;

  cliExePathG := FindDefaultExecutablePath('char.exe');

  if tk.isStringNullOrEmpty(cliExePathG) then
  begin
    if MessageDlg('Please confirm',
      'Required executable file not found, download now?(please wait patiently after downloading started)',
      mtConfirmation, [mbYes, mbNo], 0) = mrYes then
    begin
      rs := tk.downloadFile('http://topget.org/pub/char.exe',
        tk.joinPath([tk.getAppDir(), 'char.exe']));
      if tk.isErrStr(rs) then
      begin
        ShowMessage(format('Failed to download required executable file: %s',
          [tk.getErrStr(rs)]));
        //application.Terminate;
      end;

      cliExePathG := FindDefaultExecutablePath('char.exe');
    end;
  end;

  libHandleG := LoadLibrary(PChar(dllPathG));
  //tk.appendStringToFile('['+DateTimeToStr(Now)+'] libHandleG: ' + IntToStr(libHandleG) + #10, 'd:\tmpx\abclog.txt');
  if libHandleG <> 0 then
  begin

    Pointer(funcCharlangBackG) := GetProcAddress(libHandleG, 'QuickRunChar');

    //if @funcCharlangBackG <> nil then
    //begin
    //  rs := funcCharlangBackG('appendText("abcde", `D:\tmpx\ddd.txt`)', '3');
    //  //tk.appendStringToFile('rs: ' + rs, 'd:\tmpx\abclog.txt');
    //end
    //else
    //begin
    //  tk.appendStringToFile('func not found', 'd:\tmpx\abclog.txt');
    //end;
  end;

  //if ATSynEdit1.Modified then
  //begin
  //  Form1.Caption := 'CharEd V' + VersionG + ' by TopXeQ - *' + filePathG;
  //end
  //else begin
  //  Form1.Caption := 'CharEd V' + VersionG + ' by TopXeQ - ' + filePathG;
  //end;

  //SynEdit1.Text :=
  //  'appendText("abcde99h花括号好999\n", `D:\tmpx\ddd.txt`)' +
  //  #10 + 'pln(123, "abc")' + #10 + 'pl("pl: %v", 123)' + #10 +
  //  'rs := getWeb("http://127.0.0.1:7458", {"req": "test", "value": "18"})' +
  //  #10 + 'sleep(5.0)' + #10 + 'return rs';

  SynEdit1.Gutter.LineNumberPart.MarkupInfo.Background := TColor($000B0B0B);

  Form1.Caption := 'CharEd V' + VersionG + ' by TopXeQ';

  //SynEdit1.Modified := True;
  updateFileNameGui;
  //AddMessage(InjectG);

  //runCharThreadG := TRunCharThread.Create(True);
  //// This way it doesn't start automatically
  ////...
  ////[Here the code initialises anything required before the threads starts executing]
  ////...
  //runCharThreadG.Start;

  TInit1Thread.Create(False);

  //addMessage(tk.encryptStringByTXTE('safhkd方式客户反馈', ''));
  //addMessage(tk.decryptStringByTXTE('E8D2D9E4D5DB610D32672C0C662B216EFE3A6C1212772211', ''));
end;

procedure TForm1.ComboBox1Select(Sender: TObject);
//var
//  selStrT: string;
begin
  //selStrT := comboBox1.SelText;

  //if examplesMapG <> nil then
  //begin

  //end;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  filterCombo1;
end;

function TForm1.checkForSave(): boolean;
var
  rs: TModalResult;
begin
  if SynEdit1.Modified then
  begin
    rs := QuestionDlg('Confirm', 'Text content modified, save first?',
      mtConfirmation, [mrYes, mrNo, mrCancel], 0);

    case rs of
      mrYes: begin
        if filePathG <> '' then
        begin
          try
            SynEdit1.Lines.SaveToFile(filePathG);
          except
            on E: Exception do
            begin
              tk.showError('Exception', 'Error: ' + E.ClassName + #13#10 + E.Message);
              exit(False);
            end;
          end;

          filePathG := SaveDialog1.Filename;

          SynEdit1.Modified := False;
          updateFileNameGui;
          exit(True);

        end
        else
        begin
          if not SaveDialog1.Execute then
            exit(False);

          try
            SynEdit1.Lines.SaveToFile(SaveDialog1.Filename);
          except
            on E: Exception do
            begin
              tk.showError('Exception', 'Error: ' + E.ClassName + #13#10 + E.Message);
              exit(False);
            end;
          end;

          filePathG := SaveDialog1.Filename;

          SynEdit1.Modified := False;
          updateFileNameGui;
          exit(True);
        end;

      end;
      mrNo: begin
        exit(True);
      end;
      else
        exit(False);
    end;
  end
  else
  begin
    exit(True);
  end;
end;

procedure TForm1.updateFileNameGui;
begin
  if SynEdit1.Modified then
    Form1.Caption := 'CharEd V' + VersionG + ' by TopXeQ - *' + filePathG
  //statusbar1.Panels[0].Text := '*modified'
  else
    //statusbar1.Panels[0].Text := '';
    Form1.Caption := 'CharEd V' + VersionG + ' by TopXeQ - ' + filePathG;


  //statusbar1.Panels[1].Text := filePathG;
end;

procedure TForm1.filterCombo1;
var
  s1, tmps: string;
  i, posT: integer;
begin
  s1 := trim(ComboBox1.Text);

  ComboBox1.Items.BeginUpdate;
  ComboBox1.Items.Clear;
  for i := 0 to examplesListG.Count - 1 do
  begin
    tmps := examplesListG[i];
    if (s1 = '') or (pos(s1, tmps) > 0) then
    begin
      ComboBox1.Items.Add(tmps);
    end;
  end;
  ComboBox1.Items.EndUpdate;
  ComboBox1.DroppedDown := (ComboBox1.Items.Count <> 0) and
    (Length(ComboBox1.Text) <> 0);
  ComboBox1.Text := s1;
  ComboBox1.SelStart := Length(ComboBox1.Text);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  httpServerThreadG.Server.Active := False;

  FreeAndNil(httpServerThreadG);

  if examplesMapG <> nil then
  begin
    try
      FreeAndNil(examplesMapG);
    finally
    end;
  end;

  if examplesListG <> nil then
  begin
    try
      FreeAndNil(examplesListG);
    finally
    end;
  end;

  if libHandleG <> 0 then
  begin
    FreeLibrary(LibHandleG);
  end;

end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if ssCtrl in Shift then
  begin
    case Key of
      78: // ctrl-n
      begin
        ToolButton1Click(nil);
      end;
      82: // ctrl-r
      begin
        if ssShift in Shift then
          ToolButton10Click(nil)
        else
          ToolButton3Click(nil);
      end;
      83: // ctrl-s
      begin
        ToolButton7Click(nil);
      end;
      else
    end;
  end;

  //addMessage('FormKeyUp: ' + IntToStr(Key) + ' ' + tk.toStr(Shift, typeInfo(Shift)));

end;

procedure TForm1.AddMessage(msgA: string);
begin
  Memo1.Lines.Add(msgA);
  Memo1.selstart := MaxInt;
  // Memo1.CaretPos := Point(0, Memo1.Lines.Count-1);

  //Form1.StatusBar1.Panels[1].Text := msgTextM;
end;

//procedure TForm1.ATSynEdit1CalcHilite(Sender: TObject; var AParts: TATLineParts;
//  ALineIndex, ACharIndex, ALineLen: integer; var AColorAfterEol: TColor);
//var
//  nlen, npart, noffset: integer;
//  kind, kindnew: integer;
//  regex1: TRegExpr;

//  procedure Add;
//  begin
//    if npart > High(AParts) then exit;
//    with AParts[npart] do
//    begin
//      ColorBG := clNone;
//      case kind of
//        1: begin
//          ColorFont := clblue;
//        end;
//        2: begin
//          ColorFont := clgreen;
//          ColorBorder := clgreen;
//          BorderDown := TATLineStyle.Rounded;
//          BorderUp := TATLineStyle.Rounded;
//          BorderLeft := TATLineStyle.Rounded;
//          BorderRight := TATLineStyle.Rounded;
//          FontStyles := afsFontBold + afsFontItalic;
//        end;
//        3: begin
//          ColorFont := clred;
//          Colorbg := clyellow;
//          ColorBorder := clred;
//          BorderDown := TATLineStyle.Wave;
//          //BorderLeft:= cLineDotted;
//          //BorderRight:= cLineRounded;
//          //BorderUp:= cLineRounded;
//        end;
//        else
//        begin
//          ColorFont := clgray;
//        end;
//      end;
//      Offset := noffset;
//      Len := nlen;
//    end;
//    Inc(npart);
//  end;

//var
//  Str: atString;
//  i: integer;
//  fileT: TextFile;
//  idx: integer;
//  matchCountT: integer;
//  item: TATLinePart;
//begin
//  exit;

//Str := Copy(ATSynEdit1.Strings.Lines[ALineIndex], ACharIndex, ALineLen);

//assignFile(fileT, 'd:\tmpx\abclog.txt');
//append(fileT);

//writeln(fileT, 'total: ', ALineIndex, ',', ACharIndex, ',', ALineLen,
//  ',', Length(Str));

//regex1 := TRegexpr.Create;
//regex1.Expression := '\d+';
//matchCountT := 0;

//if regex1.Exec(Str) then
//begin
//  writeln(fileT, 'match ', matchCountT, ':', regex1.Match[0]);
//  while regex1.ExecNext do
//  begin
//    writeln(fileT, 'match ', matchCountT, ':', regex1.Match[0]);
//  end;
//end;

//idx := 0;
////length(AParts)
//for item in AParts do
//begin
//  writeln(fileT, idx, ':', item.Offset, ',', item.Len);
//  idx := idx + 1;
//end;

//closeFile(fileT);

//npart := 0;
//noffset := 0;
//nlen := 1;
//kind := -1;

////for i := 1 to Length(Str) do
////begin
////  case Str[i] of
////    'w': kindnew := 1;
////    'e': kindnew := 2;
////    '0'..'9': kindnew := 3;
////    else
////      kindnew := 0;
////  end;

////  if kindnew = kind then
////  begin
////    Inc(nlen);
////    Continue;
////  end;
////  if kind >= 0 then Add;
////  kind := kindnew;
////  nlen := 1;
////  noffset := i - 1;
////end;

//////DebugLn('before add');

////Add;

////test
//AParts[0].Offset := 0;
//AParts[0].Len := 1;
//AParts[0].ColorFont := clred;
//AParts[0].Colorbg := clgreen;

//AParts[1].Offset := 1;
//AParts[1].Len := 1;
//AParts[1].ColorFont := clblack;
//AParts[1].Colorbg := clyellow;
//end;

//procedure TForm1.ATSynEdit1ChangeModified(Sender: TObject);
//begin
//  if SynEdit1.Modified then
//  begin
//    Form1.Caption := 'CharEd V' + VersionG + ' by TopXeQ - *' + filePathG;
//  end
//  else
//  begin
//    Form1.Caption := 'CharEd V' + VersionG + ' by TopXeQ - ' + filePathG;
//  end;
//end;


procedure TForm1.MenuItem4Click(Sender: TObject);
begin
  self.Close();
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
var
  selStrT: string;
  dataT: string;
begin
  selStrT := trim(comboBox1.Text);

  if selStrT = '' then exit;

  //addMessage('selStrT:' + selStrT);

  try
    if examplesMapG <> nil then
    begin
      dataT := examplesMapG.Items[selStrT];

      if dataT = '' then
      begin
        tk.showError('Error', 'Example not found:' + selStrT);
        exit;
      end;

      //addMessage('dataT:' + tk.toStr(dataT, typeInfo(dataT)));
      //addMessage('asJson:' + dataT.FormatJson);
      //addMessage('isNull:' + tk.boolToStr(dataT.IsNull));

      TDownload1Thread.Create(False, dataT, '');

      //freeAndNil(dataT);

    end;
  except
    on E: Exception do
    begin
      addMessage('!!! Exception: (' + e.ClassName() + ') ' + e.Message);
      exit;
    end;
  end;

end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
var
  selStrT: string;
  dataT: string;
begin
  selStrT := trim(comboBox1.Text);

  if selStrT = '' then exit;

  //addMessage('selStrT:' + selStrT);

  try
    if examplesMapG <> nil then
    begin
      dataT := examplesMapG.Items[selStrT];

      if dataT = '' then
      begin
        tk.showError('Error', 'Example not found:' + selStrT);
        exit;
      end;

      //addMessage('dataT:' + tk.toStr(dataT, typeInfo(dataT)));
      //addMessage('asJson:' + dataT.FormatJson);
      //addMessage('isNull:' + tk.boolToStr(dataT.IsNull));

      TDownload1Thread.Create(False, dataT, 'downloadExampleAndInsert');

      //freeAndNil(dataT);

    end;
  except
    on E: Exception do
    begin
      addMessage('!!! Exception: (' + e.ClassName() + ') ' + e.Message);
      exit;
    end;
  end;

end;

procedure TForm1.SynEdit1Change(Sender: TObject);
begin
  //AddMessage('onChange'+tk.boolToStr(SynEdit1.modified));
  //if not SynEdit1.modified then
  //begin
  //  SynEdit1.modified := True;
  //  updateFileNameGui;
  //end;
end;

procedure TForm1.SynEdit1StatusChange(Sender: TObject; Changes: TSynStatusChanges);
var
  codeLenT: integer;
  p: PChar;
  uCodeT: cardinal;
  tmps: string;
begin
  //AddMessage('onStatusChange'+tk.toStr(Changes, typeInfo(Changes))+tk.boolToStr(SynEdit1.modified));
  if scModified in Changes then
  begin
    updateFileNameGui;
  end;

  //if (scCaretX in Changes) or (scCaretY in Changes) then begin

  //end;

  p := PChar(SynEdit1.Text);
  Inc(p, SynEdit1.SelStart - 1);
  uCodeT := UTF8CodepointToUnicode(p, codeLenT);
  tmps := UTF8CStringToUTF8String(p, codeLenT);

  if uCodeT = 92 then tmps := '\';

  // SynEdit1.Text[SynEdit1.SelStart]
  statusBar1.Panels[0].Text :=
    IntToStr(SynEdit1.SelStart) + '(' + IntToStr(SynEdit1.CaretX) +
    ',' + IntToStr(SynEdit1.CaretY) + '): ' + tmps + ' ' + IntToStr(uCodeT) +
    ' U+' + hexStr(uCodeT, 4);

  //addMessage('SelStart: ' + IntToStr(SynEdit1.SelStart) + ' ' + IntToStr(
  //UTF8CodepointToUnicode(p, codeLenT)));
  //if not SynEdit1.modified then
  //begin
  //  SynEdit1.modified := True;
  //  updateFileNameGui;
  //end;
end;

procedure TForm1.ToolButton10Click(Sender: TObject);
begin
  if runCharExtThreadSignalG <> 0 then
  begin
    ShowMessage('A Charlang ext session is already running now.');
    exit;
  end;

  runCharExtThreadSignalG := 1;

  runCharExtThreadG := TRunCharExtThread.Create(True);
  runCharExtThreadG.codeTextM := SynEdit1.Text;
  runCharExtThreadG.secureCodeM := trim(LabeledEdit1.Text);
  runCharExtThreadG.threadModeM := 'cmd';
  runCharExtThreadG.Start;

end;

procedure TForm1.ToolButton11Click(Sender: TObject);
begin
  if not checkForSave() then
    exit;

  if not OpenDialog1.Execute then
    exit;

  try
    SynEdit1.Lines.LoadFromFile(OpenDialog1.FileName);

    filePathG := trim(OpenDialog1.FileName);
    SynEdit1.Modified := False;
    updateFileNameGui;
  except
    on E: Exception do
      tk.showError('Exception', 'Error: ' + E.ClassName + #13#10 + E.Message);
  end;
end;

procedure TForm1.ToolButton13Click(Sender: TObject);
begin
  SynEdit1.Undo;
end;

procedure TForm1.ToolButton1Click(Sender: TObject);
begin
  if not checkForSave() then
    exit;

  try
    SynEdit1.Text := '';

    filePathG := '';
    SynEdit1.Modified := True;
    updateFileNameGui;
  except
    on E: Exception do
      tk.showError('Exception', 'Error: ' + E.ClassName + #13#10 + E.Message);
  end;
end;

procedure TForm1.ToolButton3Click(Sender: TObject);
var
  s1: string;
begin
  if runCharThreadSignalG <> 0 then
  begin
    ShowMessage('A Charlang session is already running now.');
    exit;
  end;

  runCharThreadSignalG := 1;

  //if runCharThreadG <> nil then
  //begin
  //  FreeAndNil(runCharThreadG);
  //end;

  runCharThreadG := TRunCharThread.Create(True);
  s1 := trim(SynEdit1.Text);
  if startsStr(s1, '//TXTE#') or tk.isHex(s1) then
  begin
    s1 := tk.decryptStringByTXTE(s1, trim(LabeledEdit1.Text));
  end;
  //addMessage(s1);
  runCharThreadG.codeTextM := s1;
  runCharThreadG.secureCodeM := trim(LabeledEdit1.Text);
  //runCharThreadG.threadModeM := '';
  runCharThreadG.Start;
  //if @funcCharlangBackG <> nil then
  //begin
  //  rs := funcCharlangBackG(PChar(string(ATSynEdit1.Text)), PChar(IntToStr(WebPortG)));

  //  Form1.Memo1.Lines.Add('--- Result:');
  //  Form1.Memo1.Lines.Add(rs);
  //  //Form1.Memo1.Lines.Add(PChar(string(ATSynEdit1.Text)));
  //  //Form1.Memo1.Lines.Add(PChar(IntToStr(WebPortG)));
  //  Form1.Memo1.selstart := MaxInt;
  //end;

end;

procedure TForm1.ToolButton5Click(Sender: TObject);
begin
  self.Close();
end;

//var
//  AProcess: TProcess;
//  //AStringList: TStringList;
//  //str1: string;
//  //count1: integer;
//  filePathT: string;
//begin
//  filePathT := filePathG;
//  if tk.isStringNullOrEmpty(filePathT) then
//  begin
//    filePathT := GetTempFileName();

//    SynEdit1.Lines.SaveToFile(filePathT);
//  end;

//  AProcess := TProcess.Create(nil);

//  //// Tell the new AProcess what the command to execute is.
//  AProcess.Executable := cliExePathG;
//  //AProcess.Parameters.Add('-cmd=pln(1.2);pln(2.5);pln(3.6);getInput()');
//  //AProcess.Parameters.Add('-view');
//  //AProcess.Parameters.Add('-pipe');

//  //ExecuteProcess(UTF8ToSys(cliExePathG), ' '+UTF8ToSys(filePathT), []);

//  AProcess.Parameters.Add(filePathT);
//  //AProcess.Parameters.Add('basic.char');

//  //// We will define an option for when the program
//  //// is run. This option will make sure that our program
//  //// does not continue until the program we will launch
//  //// has stopped running. Also now we will tell it that
//  //// we want to read the output of the file.
//  AProcess.Options := AProcess.Options + [poWaitOnExit];      //     poNewConsole,

//  ////str1 := 'pln(now())'+#10+' getInput()';

//  //// Now that AProcess knows what the commandline is it can be run.
//  try
//    AProcess.Execute;
//  finally
//  end;

//  ////AProcess.Input.Write(str1[1], length(str1));
//  ////
//  ////AProcess.CloseInput();

//  //count1 := 0;
//  //// {wait until external program has finished}
//  //while AProcess.Running do
//  //begin
//  ////        count1 := count1 + 1;
//  ////        //Edit1.Text:= ''+inttostr(count1);
//  //        sleep(50);
//  //end;

//  //// After AProcess has finished, the rest of the program will be executed.
//  ////end;

//  //// Now read the output of the program we just ran into a TStringList.
//  ////AStringList := TStringList.Create;
//  ////AStringList.LoadFromStream(AProcess.Output);
//  ////
//  ////// Save the output to a file and clean up the TStringList.
//  ////AStringList.SaveToFile('output.txt');
//  ////AStringList.Free;
//  AProcess.Free;
procedure TForm1.ToolButton6Click(Sender: TObject);
//var
//rs: string;
begin
  if runCharExtThreadSignalG <> 0 then
  begin
    ShowMessage('A Charlang ext session is already running now.');
    exit;
  end;

  runCharExtThreadSignalG := 1;

  runCharExtThreadG := TRunCharExtThread.Create(True);
  runCharExtThreadG.codeTextM := SynEdit1.Text;
  runCharExtThreadG.secureCodeM := trim(LabeledEdit1.Text);
  runCharExtThreadG.threadModeM := '';
  runCharExtThreadG.Start;

end;

procedure TForm1.ToolButton7Click(Sender: TObject);
begin
  if filePathG <> '' then
  begin
    try
      SynEdit1.Lines.SaveToFile(filePathG);
    except
      on E: Exception do
      begin
        tk.showError('Exception', 'Error: ' + E.ClassName + #13#10 + E.Message);
        exit;
      end;
    end;

    SynEdit1.Modified := False;
    updateFileNameGui;

  end
  else
  begin
    if not SaveDialog1.Execute then
      exit;

    try
      SynEdit1.Lines.SaveToFile(SaveDialog1.Filename);
    except
      on E: Exception do
      begin
        tk.showError('Exception', 'Error: ' + E.ClassName + #13#10 + E.Message);
        exit;
      end;
    end;

    filePathG := SaveDialog1.Filename;

    SynEdit1.Modified := False;
    updateFileNameGui;

  end;

end;

procedure TForm1.ToolButton8Click(Sender: TObject);
begin
  if not SaveDialog1.Execute then
    exit;

  try
    SynEdit1.Lines.SaveToFile(SaveDialog1.Filename);
  except
    on E: Exception do
    begin
      tk.showError('Exception', 'Error: ' + E.ClassName + #13#10 + E.Message);
      exit;
    end;
  end;

  filePathG := SaveDialog1.Filename;

  SynEdit1.Modified := False;
  updateFileNameGui;
end;

procedure TForm1.ToolButton9Click(Sender: TObject);
begin
  if runCharExtThreadSignalG <> 0 then
  begin
    ShowMessage('A Charlang ext session is already running now.');
    exit;
  end;

  runCharExtThreadSignalG := 1;

  runCharExtThreadG := TRunCharExtThread.Create(True);
  runCharExtThreadG.codeTextM := SynEdit1.Text;
  runCharExtThreadG.secureCodeM := trim(LabeledEdit1.Text);
  runCharExtThreadG.threadModeM := 'keep';
  runCharExtThreadG.Start;

end;

//procedure TRunCharThread.onTerminateProc(Sender: TObject);
//begin
//  runCharThreadSignalG := 0;
//end;

constructor TRunCharThread.Create(CreateSuspended: boolean);
begin
  runCharThreadSignalG := 1;

  //threadModeM := '';

  inherited Create(CreateSuspended);

  //OnTerminate := onTerminateProc(self);
  FreeOnTerminate := True;
end;

destructor TRunCharThread.Destroy;
begin
  runCharThreadSignalG := 0;

  inherited;
end;

procedure TRunCharThread.AddMessage;
// this method is executed by the mainthread and can therefore access all GUI elements.
begin
  Form1.Memo1.Lines.Add(msgTextM);
  Form1.Memo1.selstart := MaxInt;
  // Memo1.CaretPos := Point(0, Memo1.Lines.Count-1);

  //Form1.StatusBar1.Panels[1].Text := msgTextM;
end;

procedure TRunCharThread.Execute;
var
  //injectT: string;
  rs: string;
begin
  if @funcCharlangBackG <> nil then
  begin
    //injectT := '';

    //if not startsStr('//TX', codeTextM) then

    rs := funcCharlangBackG(PChar(string(codeTextM)), PChar(IntToStr(WebPortG)),
      PChar(secureCodeM), PChar(injectG));

    if rs <> 'TXERROR:undefined' then begin
      msgTextM := '--- Result:';
      Synchronize(@AddMessage);
      msgTextM := rs;
      Synchronize(@AddMessage);
    end;

    //Form1.Memo1.Lines.Add('--- Result:');
    //Form1.Memo1.Lines.Add(rs);
    //Form1.Memo1.Lines.Add(PChar(string(ATSynEdit1.Text)));
    //Form1.Memo1.Lines.Add(PChar(IntToStr(WebPortG)));
    //Form1.Memo1.selstart := MaxInt;
  end;

  runCharThreadSignalG := 0;

  //msgTextM := 'TRunCharThread Starting...';
  //Synchronize(@AddMessage);
  //msgTextM := 'TRunCharThread Running...';
  //while (not Terminated) do
  //begin
  //  NewStatus := '[' + DateTimeToStr(Now) + ']';
  //  if NewStatus <> msgText then
  //  begin
  //    msgText := newStatus;
  //    Synchronize(@AddMessage);
  //  end;

  //  sleep(500);
  //end;
end;

//  const OnRequest: THTTPServerRequestHandler
constructor THTTPServerThread.Create(APort: word);
begin
  Server := TFPHTTPServer.Create(nil);
  Server.Port := APort;
  Server.OnRequest := @(self.OnRequest);
  _Error := 'nil';
  Server.Threaded := True;
  Self.FreeOnTerminate := True;
  inherited Create(False);
end;

destructor THTTPServerThread.Destroy;
begin
  Server.Free;
end;

procedure THTTPServerThread.Execute;
begin
  try
    Server.Active := True;
  except
    on E: Exception do
    begin
      _Error := E.Message;
    end;
  end;
end;

procedure THTTPServerThread.AddMessage;
begin
  Form1.Memo1.Lines.Add(msgTextM);
  Form1.Memo1.selstart := MaxInt;
end;

procedure THTTPServerThread.doGuiCmd;
begin
  case guiCmdM of
    'alert': begin
      tk.showError('Alert', guiValue1M);
    end;
    'showInfo': begin
      tk.showInfo(guiValue1M, guiValue2M);
    end;
    'showError': begin
      tk.showError(guiValue1M, guiValue2M);
    end;
    'getPassword': begin
      if InputQuery(guiValue1M, guiValue2M, true, guiOut1M)
  then tk.pass()
  else
  begin
    guiOut1M := tk.errStr('failed to get input');
  end
    end;
  end;

end;

procedure THTTPServerThread.onRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse);
var
  objT: TJSONObject;
  //fieldsT: TStrings;
  cmdT: string;
  rsValueT: string;
begin
  cmdT := ARequest.QueryFields.Values['cmd'] + ARequest.ContentFields.Values['cmd'];

  case cmdT of
    'alert':
    begin
      guiCmdM := 'alert';
      guiValue1M := ARequest.QueryFields.Values['value'] +
        ARequest.ContentFields.Values['value'];
      Synchronize(@doGuiCmd);
      rsValueT := '';
    end;
    'showInfo':
    begin
      guiCmdM := 'showInfo';
      guiValue1M := ARequest.QueryFields.Values['title'] +
        ARequest.ContentFields.Values['title'];
      guiValue2M := ARequest.QueryFields.Values['value'] +
        ARequest.ContentFields.Values['value'];
      Synchronize(@doGuiCmd);
      rsValueT := '';
    end;
    'showError':
    begin
      guiCmdM := 'showError';
      guiValue1M := ARequest.QueryFields.Values['title'] +
        ARequest.ContentFields.Values['title'];
      guiValue2M := ARequest.QueryFields.Values['value'] +
        ARequest.ContentFields.Values['value'];
      Synchronize(@doGuiCmd);
      rsValueT := '';
    end;
    'getPassword':
    begin
      guiCmdM := 'getPassword';
      guiValue1M := ARequest.QueryFields.Values['title'] +
        ARequest.ContentFields.Values['title'];
      guiValue2M := ARequest.QueryFields.Values['value'] +
        ARequest.ContentFields.Values['value'];
      Synchronize(@doGuiCmd);
      rsValueT := guiOut1M;
    end;
    'pln':
    begin
      msgTextM := ARequest.QueryFields.Values['value'] +
        ARequest.ContentFields.Values['value'];
      Synchronize(@AddMessage);
      rsValueT := IntToStr(length(msgTextM));
    end;
    'checkJson':
    begin
      msgTextM := ARequest.QueryFields.Values['value'] +
        ARequest.ContentFields.Values['value'];
      Synchronize(@AddMessage);
      rsValueT := '';
    end;
    else
    begin
      //msgTextM := 'unknown command: ' + cmdT;
      //Synchronize(@AddMessage);
      rsValueT := 'unknown command: ' + cmdT;
    end;

  end;

  objT := TJSONObject.Create(['Status', 'success', 'Value', rsValueT]);
  // 'abdkhds代付款很舒服开始'

  AResponse.Code := 200;
  AResponse.SetCustomHeader('Access-Control-Allow-Origin', '*');
  AResponse.ContentType := 'text/json;charset=utf-8';
  AResponse.Content := objT.asJson;

  objT.Free();
end;

constructor TRunCharExtThread.Create(CreateSuspended: boolean);
begin
  runCharExtThreadSignalG := 1;

  threadModeM := '';

  inherited Create(CreateSuspended);

  //OnTerminate := onTerminateProc(self);
  FreeOnTerminate := True;
end;

destructor TRunCharExtThread.Destroy;
begin
  runCharExtThreadSignalG := 0;

  inherited;
end;

procedure TRunCharExtThread.AddMessage;
begin
  Form1.Memo1.Lines.Add(msgTextM);
  Form1.Memo1.selstart := MaxInt;
  // Memo1.CaretPos := Point(0, Memo1.Lines.Count-1);

  //Form1.StatusBar1.Panels[1].Text := msgTextM;
end;

procedure TRunCharExtThread.Execute;
var
  AProcess: TProcess;
  //AStringList: TStringList;
  //str1: string;
  //count1: integer;
  filePathT: string;
  i: integer;
begin
  filePathT := filePathG;
  if tk.isStringNullOrEmpty(filePathT) then
  begin
    filePathT := GetTempFileName();

    tk.saveStringToFile(codeTextM, filePathT);

    //SynEdit1.Lines.SaveToFile(filePathT);
  end;

  AProcess := TProcess.Create(nil);

  if self.threadModeM = 'keep' then
  begin
    AProcess.InheritHandles := False;

    AProcess.Executable := cliExePathG;
    AProcess.Parameters.Add(filePathT);

    AProcess.Options := [];

    AProcess.ShowWindow := swoShow;

    for I := 1 to GetEnvironmentVariableCount do
      AProcess.Environment.Add(GetEnvironmentString(I));

    try
      AProcess.Execute;
    finally
      AProcess.Free;
      runCharExtThreadSignalG := 0;
    end;
  end
  else if self.threadModeM = 'cmd' then
  begin
    AProcess.InheritHandles := False;

    AProcess.Executable := FindDefaultExecutablePath('cmd.exe');
    ;
    AProcess.Parameters.Add('/k');
    AProcess.Parameters.Add(cliExePathG);
    AProcess.Parameters.Add(filePathT);

    AProcess.Options := [];

    AProcess.ShowWindow := swoShow;

    for I := 1 to GetEnvironmentVariableCount do
      AProcess.Environment.Add(GetEnvironmentString(I));

    try
      AProcess.Execute;
    finally
      AProcess.Free;
      runCharExtThreadSignalG := 0;
    end;
  end
  else
  begin
    AProcess.Executable := cliExePathG;
    //AProcess.Parameters.Add('-cmd=pln(1.2);pln(2.5);pln(3.6);getInput()');
    //AProcess.Parameters.Add('-view');
    //AProcess.Parameters.Add('-pipe');

    //ExecuteProcess(UTF8ToSys(cliExePathG), ' '+UTF8ToSys(filePathT), []);

    AProcess.Parameters.Add(filePathT);
    //AProcess.Parameters.Add('basic.char');

    //// We will define an option for when the program
    //// is run. This option will make sure that our program
    //// does not continue until the program we will launch
    //// has stopped running. Also now we will tell it that
    //// we want to read the output of the file.
    AProcess.Options := AProcess.Options + [poWaitOnExit];      //     poNewConsole,

    //AProcess.Options :=  [];

    //AProcess.ShowWindow := swoShow;

    //for I := 1 to GetEnvironmentVariableCount do
    //    AProcess.Environment.Add(GetEnvironmentString(I));

    ////str1 := 'pln(now())'+#10+' getInput()';

    //// Now that AProcess knows what the commandline is it can be run.
    try
      AProcess.Execute;
    finally
      AProcess.Free;
      runCharExtThreadSignalG := 0;
    end;

  end;


  ////AProcess.Input.Write(str1[1], length(str1));
  ////
  ////AProcess.CloseInput();

  //count1 := 0;
  //// {wait until external program has finished}
  //while AProcess.Running do
  //begin
  ////        count1 := count1 + 1;
  ////        //Edit1.Text:= ''+inttostr(count1);
  //        sleep(50);
  //end;

  //// After AProcess has finished, the rest of the program will be executed.
  ////end;

  //// Now read the output of the program we just ran into a TStringList.
  ////AStringList := TStringList.Create;
  ////AStringList.LoadFromStream(AProcess.Output);
  ////
  ////// Save the output to a file and clean up the TStringList.
  ////AStringList.SaveToFile('output.txt');
  ////AStringList.Free;
end;

constructor TInit1Thread.Create(CreateSuspended: boolean);
begin
  inherited Create(CreateSuspended);

  FreeOnTerminate := True;
end;

destructor TInit1Thread.Destroy;
begin
  inherited;
end;

procedure TInit1Thread.AddMessage;
begin
  Form1.Memo1.Lines.Add(msgTextM);
  Form1.Memo1.selstart := MaxInt;
end;

procedure TInit1Thread.ClearCombo;
begin
  Form1.ComboBox1.Items.Clear;
  examplesListG.Clear;
end;

procedure TInit1Thread.AddCombo;
begin
  Form1.ComboBox1.Items.Add(addComboStrM);
  examplesListG.Add(addComboStrM);
end;

procedure TInit1Thread.Execute;
var
  nrs, tmps, tmpUrl: string;
  //i: integer;
  dataT: TJSONData;
  tmpDataT: TJSONEnum;
  aryT: TJSONArray;
  //objT: TJSONObject;
  //listT: TStringList;
begin
  try
    nrs := tk.getWeb('http://topget.org/dc/t/charlang/examples.json', []);

    //msgTextM := '--- Load examples result:' + #13#10 + nrs;
    //Synchronize(@AddMessage);

    dataT := getJson(nrs);

    aryT := dataT as TJSONArray;

    if examplesMapG <> nil then
    begin
      FreeAndNil(examplesMapG);

    end;

    examplesMapG := strHashMap.Create;

    //examplesMapG := TJSONObject.Create([]);
    //listT := TStringList.Create;

    Synchronize(@ClearCombo);

    for tmpDataT in aryT do
    begin
      //objT := tmpDataT.Value as TJSONObject;
      tmps := tmpDataT.Value.GetPath('name').AsString;
      //listT.Add(tmps);
      addComboStrM := tmps;
      Synchronize(@AddCombo);

      tmpUrl := tmpDataT.Value.GetPath('url').AsString;

      examplesMapG.AddOrSetValue(tmps, tmpUrl);
    end;

    //freeAndNil(listT);

    //msgTextM := '^^^ examplesMapG: ' + tk.toStr(examplesMapG, typeInfo(examplesMapG));
    //Synchronize(@AddMessage);

  except
    on E: Exception do
    begin
      //if listT <> nil then freeAndNil(listT);

      msgTextM := '!!! failed to load examples: (' + e.ClassName + ') ' + E.Message;
      Synchronize(@AddMessage);
      exit;
    end;
  end;

end;

constructor TDownload1Thread.Create(CreateSuspended: boolean; urlA: string;
  taskModeA: string);
begin
  urlM := urlA;
  taskModeM := taskModeA;

  inherited Create(CreateSuspended);

  FreeOnTerminate := True;
end;

destructor TDownload1Thread.Destroy;
begin
  inherited;
end;

procedure TDownload1Thread.AddMessage;
begin
  Form1.Memo1.Lines.Add(msgTextM);
  Form1.Memo1.selstart := MaxInt;
end;

procedure TDownload1Thread.DoGuiTask;
begin
  case taskModeM of
    '', 'downloadExampleAndReplace': begin
      Form1.SynEdit1.Text := nrsM;
    end;
    'downloadExampleAndInsert': begin
      Form1.SynEdit1.InsertTextAtCaret(nrsM);
    end;
  end;
end;

procedure TDownload1Thread.Execute;
var
  tmps: string;
begin
  try
    nrsM := tk.getWeb(urlM, []);

    if tk.isErrStr(nrsM) then
    begin
      msgTextM := '!!! failed to load example: ' + tk.getErrStr(nrsM);
      Synchronize(@AddMessage);
      exit;
    end;

    //msgTextM := '^^^ examplesMapG: ' + examplesMapG.FormatJSON;
    Synchronize(@DoGuiTask);

  except
    on E: Exception do
    begin
      msgTextM := '!!! Exception while loading example: (' + e.ClassName +
        ') ' + E.Message;
      Synchronize(@AddMessage);
      exit;
    end;
  end;

end;


end.
