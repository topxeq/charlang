unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Types, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  StdCtrls, ExtCtrls, PairSplitter, Buttons, uCmdBox, SynEdit,
  SynHighlighterAny, SynHighlighterCpp, SynHighlighterSQL,
  SynHighlighterJScript, SynHighlighterPas, tkunit, fphttpserver, fileutil,
  RegExpr, fpjson, Process, LazUTF8, SynEditTypes, Generics.Collections,
  unitCommandPalette, UnitSelectList;

  // , ATStringProc, ATSynEdit_LineParts

const
  VersionG = '0.9.3';
  WebPortG = 7458;

type
  strHashMap = specialize TDictionary<string, string>;

  TfuncCharlangBackG = function(codeA, paramA, secureCodeA, injectA, globalsA, comBufA: PChar):
    PChar; stdcall;

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

  TMonitorCharThread = class(TThread)
  private
    procedure AddMessage;
    procedure AddMessagePr;
    procedure AddMessagePrColor;
    procedure ChangeConsoleColor;
  protected
    procedure Execute; override;
  public
    msgTextM: string;
    colorTextM: string;
    colorM: TColor;

    guiCmdM: string;
    guiValue1M: string;
    guiValue2M: string;
    guiValue3M: string;
    guiValue4M: string;
    guiValue5M: string;

    guiOut1M: string;
    guiOut2M: string;

    procedure doGuiCmd;

    constructor Create(CreateSuspended: boolean);
    destructor Destroy; override;
  end;

  TRunCharThread = class(TThread)
  private
    procedure AddMessage;
    procedure ShowError;
    procedure QuitApp;
  protected
    procedure Execute; override;
  public
    msgTitleM: string;
    msgTextM: string;

    codeTextM: string;

    secureCodeM: string;

    terminateWhileFailM: boolean;

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
    CmdBox1: TCmdBox;
    ComboBox1: TComboBox;
    ImageList1: TImageList;
    LabeledEdit1: TLabeledEdit;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    OpenDialog2: TOpenDialog;
    PopupMenu2: TPopupMenu;
    Separator6: TMenuItem;
    Separator5: TMenuItem;
    PopupMenu1: TPopupMenu;
    Separator4: TMenuItem;
    Separator3: TMenuItem;
    Separator2: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
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
    Timer1: TTimer;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure MenuItem10Click(Sender: TObject);
    procedure MenuItem11Click(Sender: TObject);
    procedure MenuItem13Click(Sender: TObject);
    procedure MenuItem14Click(Sender: TObject);
    procedure MenuItem15Click(Sender: TObject);
    procedure MenuItem16Click(Sender: TObject);
    procedure MenuItem17Click(Sender: TObject);
    procedure MenuItem18Click(Sender: TObject);
    procedure MenuItem19Click(Sender: TObject);
    procedure MenuItem20Click(Sender: TObject);
    procedure MenuItem27Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SynEdit1Change(Sender: TObject);
    procedure SynEdit1StatusChange(Sender: TObject; Changes: TSynStatusChanges);
    procedure Timer1Timer(Sender: TObject);
    procedure ToolButton10Click(Sender: TObject);
    procedure ToolButton11Click(Sender: TObject);
    procedure ToolButton13Click(Sender: TObject);
    procedure ToolButton15Click(Sender: TObject);
    procedure ToolButton16Click(Sender: TObject);
    procedure ToolButton17Click(Sender: TObject);
    procedure ToolButton18Click(Sender: TObject);
    procedure ToolButton19Click(Sender: TObject);
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

  monitorCharThreadG: TMonitorCharThread;

  runCharThreadG: TRunCharThread;
  runCharThreadSignalG: integer;

  runCharExtThreadG: TRunCharExtThread;
  runCharExtThreadSignalG: integer;

  filePathG: string;

  dllPathG: string;
  cliExePathG: string;

  examplesMapG: strHashMap;
  examplesListG: TStringList;

  terminateFlagG: boolean = False;

  comBufG: array [0..16777216] of Byte;

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

  //if MessageDlg('Please confirm', 'Required executable file not found, download now?(please wait patiently after downloading started)',
  //    mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  //begin
  //  application.Terminate;
  //end;

  //Form1.hide();
  //Form2.showModal();
  //Form1.show();

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

  monitorCharThreadG := TMonitorCharThread.Create(false);

  // madarinStart
  addMessage(tk.getErrStr('TXERROR:safhkd方式客户反馈'));
  //addMessage(tk.strSlice('safhkd方式客户反馈', 0, 3));
  //addMessage(tk.strSlice('safhkd方式客户反馈', 1, 3));
  //addMessage(tk.strSlice('safhkd方式客户反馈', 1, 8));
  //addMessage(tk.strSlice('safhkd方式客户反馈', 3, 10));

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

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FormDestroy(nil);
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
  //monitorCharThreadG.Terminate;

  if httpServerThreadG <> nil then
  begin
    httpServerThreadG.Server.Active := False;
    FreeAndNil(httpServerThreadG);
  end;


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

  Application.Terminate;
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
      80: // ctrl-p
      begin
        ToolButton19Click(nil);
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

procedure TForm1.MenuItem10Click(Sender: TObject);
begin
  ToolButton9Click(Sender);
end;

procedure TForm1.MenuItem11Click(Sender: TObject);
begin
  ToolButton10Click(Sender);
end;

procedure TForm1.MenuItem13Click(Sender: TObject);
begin
  SynEdit1.CopyToClipboard;
end;

procedure TForm1.MenuItem14Click(Sender: TObject);
begin
  SynEdit1.CutToClipboard;
end;

procedure TForm1.MenuItem15Click(Sender: TObject);
begin
  SynEdit1.PasteFromClipboard(False);
end;

procedure TForm1.MenuItem16Click(Sender: TObject);
var
  mapT: txStrMap;
begin
  SynEdit1.Clear;

  mapT := tk.newStrMap(['inputG', SynEdit1.Text, 'secureCodeG', 'abc123']);

  addMessage(tk.toJson(mapT));

  FreeAndNil(mapT);

end;

procedure TForm1.MenuItem17Click(Sender: TObject);
begin
  SynEdit1.SelectAll;
end;

procedure TForm1.MenuItem18Click(Sender: TObject);
begin
  SynEdit1.Undo;
end;

procedure TForm1.MenuItem19Click(Sender: TObject);
begin
  SynEdit1.Redo;
end;

procedure TForm1.MenuItem20Click(Sender: TObject);
begin
  ToolButton19Click(Sender);
end;

procedure TForm1.MenuItem27Click(Sender: TObject);
begin
  if saveDialog1.Execute then
  begin
    CmdBox1.SaveToFile(saveDialog1.FileName);
  end;
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
begin
  ToolButton1Click(Sender);
end;

procedure TForm1.MenuItem3Click(Sender: TObject);
begin
  ToolButton11Click(Sender);
end;

procedure TForm1.AddMessage(msgA: string);
begin
  Form1.CmdBox1.TextColors(clWhite, clBlack);
  Form1.CmdBox1.Writeln(msgA);
  //Memo1.Lines.Add(msgA);
  //Memo1.selstart := MaxInt;
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
  application.terminate;
end;

procedure TForm1.MenuItem8Click(Sender: TObject);
begin
  ToolButton3Click(Sender);
end;

procedure TForm1.MenuItem9Click(Sender: TObject);
begin
  ToolButton6Click(Sender);
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

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if terminateFlagG then
  begin
    self.Close();

    application.Terminate;
  end;
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

procedure TForm1.ToolButton15Click(Sender: TObject);
begin
  SynEdit1.CopyToClipboard;
end;

procedure TForm1.ToolButton16Click(Sender: TObject);
begin
  SynEdit1.CutToClipboard;
end;

procedure TForm1.ToolButton17Click(Sender: TObject);
begin
  SynEdit1.PasteFromClipboard(False);
end;

procedure TForm1.ToolButton18Click(Sender: TObject);
begin
  SynEdit1.Redo;
end;

procedure TForm1.ToolButton19Click(Sender: TObject);
begin
  Form2.Memo1.Text := '';
  Form2.showModal;
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
  if startsStr('//TXTE#', s1) or tk.isHex(s1) then
  begin
    s1 := tk.decryptStringByTXTE(s1.remove(0, 7), trim(LabeledEdit1.Text));
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
  Application.Terminate;
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

  terminateWhileFailM := False;
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
  Form1.CmdBox1.TextColors(clWhite, clBlack);
  Form1.CmdBox1.Writeln(msgTextM);
end;

procedure TRunCharThread.ShowError;
begin
  tk.showError(msgTitleM, msgTextM);
end;

procedure TRunCharThread.QuitApp;
begin
  terminateFlagG := True;
end;

procedure TRunCharThread.Execute;
var
  //injectT: string;
  rs: string;
begin
  if @funcCharlangBackG <> nil then
  begin
    //injectT := '';

    rs := funcCharlangBackG(PChar(string(codeTextM)), PChar(IntToStr(WebPortG)),
      PChar(secureCodeM), PChar(''),
      PChar('{"guiServerUrlG":"http://127.0.0.1:' + IntToStr(WebPortG) + '"}'), PChar(@comBufG));

    if startsStr('TXERROR:', rs) then
    begin
      if rs = 'TXERROR:undefined' then
      begin

      end
      else
      begin
        msgTitleM := 'Error';
        msgTextM := tk.getErrStr(rs);           // + '|' + codeTextM
        Synchronize(@ShowError);

        if terminateWhileFailM then
        begin
          Synchronize(@QuitApp);
        end;

      end;
    end
    else
    begin
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
  Form1.CmdBox1.TextColors(clWhite, clBlack);
  Form1.CmdBox1.Writeln(msgTextM);
end;

procedure THTTPServerThread.doGuiCmd;
var
  i: integer;
  tmps: string;
  jAryT: TJSONArray;
  tmpDataT: TJSONEnum;
begin
  case guiCmdM of
    'quit': begin
      terminateFlagG := True;
      //application.Terminate;
    end;
    'alert': begin
      tk.showError('Alert', guiValue1M);
    end;
    'showInfo': begin
      tk.showInfo(guiValue1M, guiValue2M);
    end;
    'showError': begin
      tk.showError(guiValue1M, guiValue2M);
    end;
    'getInput': begin
      if InputQuery(guiValue1M, guiValue2M, False, guiOut1M) then tk.pass()
      else
      begin
        guiOut1M := tk.errStr('failed to get input');
      end;
    end;
    'getPassword': begin
      if InputQuery(guiValue1M, guiValue2M, True, guiOut1M) then tk.pass()
      else
      begin
        guiOut1M := tk.errStr('failed to get input');
      end;
    end;
    'selectItem': begin
      Form4.Caption := guiValue1M;
      jAryT := getJson(guiValue2M) as TJSONArray;

      Form4.ListBox1.Clear;

      for tmpDataT in jAryT do
      begin
        tmps := tmpDataT.Value.AsString;
        Form4.ListBox1.Items.Add(tmps);
      end;

      FreeAndNil(jAryT);

      if Form4.ShowModal = mrOk then
      begin
        if Form4.ListBox1.SelCount < 1 then
        begin
          guiOut1M := tk.errStr('no item selected');
          exit;
        end;

        for i := 0 to Form4.ListBox1.Count - 1 do
        begin
          if Form4.ListBox1.Selected[i] then
          begin
            guiOut1M := Form4.ListBox1.Items[i];
            exit;
          end;
        end;
        guiOut1M := tk.errStr('failed to get selected item');
      end
      else
      begin
        guiOut1M := tk.errStr('canceled');
      end;
    end;
    'selectFile': begin
      Form1.OpenDialog2.Title := guiValue1M;

      if not Form1.OpenDialog2.Execute then
      begin
        guiOut1M := tk.errStr('canceled');
      end
      else
      begin
        guiOut1M := Form1.OpenDialog2.FileName;
      end;
    end;
  end;

end;

procedure THTTPServerThread.onRequest(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse);
var
  objT: TJSONObject;
  //fieldsT: TStrings;
  cmdT: string;
  statusValueT, rsValueT: string;
begin
  cmdT := ARequest.QueryFields.Values['cmd'] + ARequest.ContentFields.Values['cmd'];

  statusValueT := 'success';

  case cmdT of
    'selectFile':
    begin
      guiCmdM := 'selectFile';
      guiValue1M := ARequest.QueryFields.Values['title'] +
        ARequest.ContentFields.Values['title'];
      guiValue2M := ARequest.QueryFields.Values['opts'] +
        ARequest.ContentFields.Values['opts'];
      Synchronize(@doGuiCmd);
      rsValueT := guiOut1M;
    end;
    'quit':
    begin
      guiCmdM := 'quit';
      Synchronize(@doGuiCmd);
      rsValueT := '';
    end;
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
    'getInput':
    begin
      guiCmdM := 'getInput';
      guiValue1M := ARequest.QueryFields.Values['title'] +
        ARequest.ContentFields.Values['title'];
      guiValue2M := ARequest.QueryFields.Values['value'] +
        ARequest.ContentFields.Values['value'];
      Synchronize(@doGuiCmd);
      rsValueT := guiOut1M;
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
    'selectItem':
    begin
      guiCmdM := 'selectItem';
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
      statusValueT := 'fail';
      rsValueT := 'unknown command: ' + cmdT;
    end;

  end;

  objT := TJSONObject.Create(['Status', statusValueT, 'Value', rsValueT]);
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
  Form1.CmdBox1.TextColors(clWhite, clBlack);
  Form1.CmdBox1.Writeln(msgTextM);
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
  Form1.CmdBox1.TextColors(clWhite, clBlack);
  Form1.CmdBox1.Writeln(msgTextM);
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
  Form1.CmdBox1.TextColors(clWhite, clBlack);
  Form1.CmdBox1.Writeln(msgTextM);
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

constructor TMonitorCharThread.Create(CreateSuspended: boolean);
begin
  inherited Create(CreateSuspended);

  FreeOnTerminate := True;
end;

destructor TMonitorCharThread.Destroy;
begin
  inherited;
end;

procedure TMonitorCharThread.AddMessage;
begin
  Form1.CmdBox1.TextColors(clWhite, clBlack);
  Form1.CmdBox1.Writeln(msgTextM);
  //Form1.CmdBox1.AdjustScrollBarsOut(true);
end;

procedure TMonitorCharThread.AddMessagePr;
begin
  Form1.CmdBox1.TextColors(clWhite, clBlack);
  Form1.CmdBox1.Write(msgTextM);
end;

procedure TMonitorCharThread.AddMessagePrColor;
begin
  Form1.CmdBox1.TextColors(colorM, clBlack);
  Form1.CmdBox1.Write(msgTextM);
end;

procedure TMonitorCharThread.ChangeConsoleColor;
var
  colorT: TColor;
begin
  case colorTextM of
    'silver': colorT := clSilver;
    'black': colorT := clBlack;
    'white': colorT := clWhite;
    else
      colorT := tk.hexToColor(colorTextM);
  end;

  Form1.CmdBox1.TextColors(colorT, clBlack);
end;

procedure TMonitorCharThread.Execute;
var
  signalT: byte;
  lenT: int32;
  len2T: int32;
  comStrT: string;
  comObjT: SimpleFlexObject;// txStrMap; // TJSONData;
  objT: TJSONObject;
  cmdT: string;
  statusValueT: string;
  rs: string;
  rsValueT: string;
  pieces1: TStringDynArray;
  pieces2: TStringDynArray;
  tmps, tmps0, tmps1: string;
begin
  comBufG[1] := 1;
  comBufG[2] := 0;
  comBufG[3] := 0;
  comBufG[4] := 0;

  comBufG[0] := 0;

  while (application <> nil) and (not application.Terminated) do
  begin
    signalT := comBufG[0];

    if signalT = 1 then
    begin
      //msgTextM := '--- change to 1';
      //Synchronize(@AddMessage);

      lenT := comBufG[1] * 65536 * 256 + comBufG[2] * 65536 +
        comBufG[3] * 256 + comBufG[4];
      //msgTextM := '--- size: ' + IntToStr(lenT);
      //Synchronize(@AddMessage);

      comStrT := string(PChar(@(comBufG[5])));
      //msgTextM := '--- str: ' + comStrT;
      //Synchronize(@AddMessage);

      //msgTextM := 'got: '+comStrT;
      //Synchronize(@AddMessage);

      if comStrT.StartsWith('|||') then begin
        comObjT := SimpleFlexObject.Create(comStrT);
      end else begin
        comObjT := tk.jsonDicToSimpleFlexObject(comStrT); // getJson(comStrT);
      end;


      cmdT := tk.getMapItem(comObjT, 'cmd'); // tk.getJsonPathString(comObjT, 'cmd');

      statusValueT := 'success';

      case cmdT of
        'selectFile':
        begin
          guiCmdM := 'selectFile';
          //guiValue1M := tk.getJsonPathString(comObjT, 'title');
          guiValue1M := tk.getMapItem(comObjT, 'title');
          guiValue2M := tk.getMapItem(comObjT, 'opts');
          Synchronize(@doGuiCmd);
          rsValueT := guiOut1M;
        end;
        'fatal', 'confirmToQuit':
        begin
          guiCmdM := 'confirmToQuit';
          guiValue1M := tk.getMapItem(comObjT, 'title');
          guiValue2M := tk.getMapItem(comObjT, 'value');
          guiValue3M := tk.getMapItem(comObjT, 'opts');
          Synchronize(@doGuiCmd);
          rsValueT := 'TX_nr_XT';
        end;
        'quit':
        begin
          guiCmdM := 'quit';
          Synchronize(@doGuiCmd);
          rsValueT := '';
        end;
        'alert':
        begin
          guiCmdM := 'alert';
          guiValue1M := tk.getMapItem(comObjT, 'value');
          guiValue2M := tk.getMapItem(comObjT, 'opts');
          Synchronize(@doGuiCmd);
          rsValueT := 'TX_nr_XT';
        end;
        'showInfo':
        begin
          guiCmdM := 'showInfo';
          guiValue1M := tk.getMapItem(comObjT, 'title');
          guiValue2M := tk.getMapItem(comObjT, 'value');
          guiValue3M := tk.getMapItem(comObjT, 'opts');
          Synchronize(@doGuiCmd);
          rsValueT := '';
        end;
        'showError':
        begin
          guiCmdM := 'showError';
          guiValue1M := tk.getMapItem(comObjT, 'title');
          guiValue2M := tk.getMapItem(comObjT, 'value');
          guiValue3M := tk.getMapItem(comObjT, 'opts');
          Synchronize(@doGuiCmd);
          rsValueT := '';
        end;
        'getInput':
        begin
          guiCmdM := 'getInput';
          guiValue1M := tk.getMapItem(comObjT, 'title');
          guiValue2M := tk.getMapItem(comObjT, 'value');
          Synchronize(@doGuiCmd);
          rsValueT := guiOut1M;
        end;
        'getPassword':
        begin
          guiCmdM := 'getPassword';
          guiValue1M := tk.getMapItem(comObjT, 'title');
          guiValue2M := tk.getMapItem(comObjT, 'value');
          guiValue3M := tk.getMapItem(comObjT, 'opts');
          Synchronize(@doGuiCmd);
          rsValueT := guiOut1M;
        end;
        'selectItem':
        begin
          guiCmdM := 'selectItem';
          guiValue1M := tk.getMapItem(comObjT, 'title');
          guiValue2M := tk.getMapItem(comObjT, 'value');
          guiValue3M := tk.getMapItem(comObjT, 'items');
          guiValue4M := tk.getMapItem(comObjT, 'opts');
          Synchronize(@doGuiCmd);
          rsValueT := guiOut1M;
        end;
        'setAppTitle':
        begin
          guiCmdM := 'setAppTitle';
          guiValue1M := tk.getMapItem(comObjT, 'title');
          Synchronize(@doGuiCmd);
          rsValueT := 'TX_nr_XT';
        end;
        'pln':
        begin
          msgTextM := tk.getMapItem(comObjT, 'value');
          Synchronize(@AddMessage);
          //rsValueT := IntToStr(length(msgTextM));
          rsValueT := 'TX_nr_XT';
        end;
        'plnColor':
        begin
          msgTextM := tk.getMapItem(comObjT, 'value');
          pieces1 := SplitString(msgTextM, '@@@');

          for tmps in pieces1 do
          begin
            pieces2 := SplitString(tmps, '+++');

            if length(pieces2) < 2 then
            begin
              msgTextM := pieces2[0];
              Synchronize(@AddMessagePr);
            end
            else
            begin
              msgTextM := pieces2[0];
              tmps1 := pieces2[1];
              case tmps1 of
                'silver': colorM := clSilver;
                'black': colorM := clBlack;
                'white': colorM := clWhite;
                else
                  colorM := tk.hexToColor(tmps1);
              end;

              Synchronize(@AddMessagePrColor);
            end;
          end;

          msgTextM := '';
          Synchronize(@AddMessage);

          //Synchronize(@AddMessage);
          //rsValueT := IntToStr(length(msgTextM));
          rsValueT := 'TX_nr_XT';
        end;
        'pr':
        begin
          msgTextM := tk.getMapItem(comObjT, 'value');
          Synchronize(@AddMessagePr);
          //rsValueT := IntToStr(length(msgTextM));
          rsValueT := 'TX_nr_XT';
        end;
        'prColor':
        begin
          msgTextM := tk.getMapItem(comObjT, 'value');
          colorTextM := tk.getMapItem(comObjT, 'color');
          Synchronize(@AddMessagePrColor);
          //rsValueT := IntToStr(length(msgTextM));
          rsValueT := 'TX_nr_XT';
        end;
        'changeConsoleColor':
        begin
          colorTextM := tk.getMapItem(comObjT, 'value');
          Synchronize(@ChangeConsoleColor);
          //rsValueT := IntToStr(length(msgTextM));
          rsValueT := 'TX_nr_XT';
        end;
        'checkJson':
        begin
          msgTextM := tk.getMapItem(comObjT, 'value');
          Synchronize(@AddMessage);
          rsValueT := '';
        end;
        else
        begin
          //msgTextM := 'unknown command: ' + cmdT;
          //Synchronize(@AddMessage);
          statusValueT := 'fail';
          rsValueT := 'unknown command: ' + cmdT;
        end;

      end;

      FreeAndNil(comObjT);

      if rsValueT = 'TX_nr_XT' then
      begin
        comBufG[1] := 1;
        comBufG[2] := 0;
        comBufG[3] := 0;
        comBufG[4] := 0;

        comBufG[0] := 0;
        continue;
      end;

      objT := TJSONObject.Create(['Status', statusValueT, 'Value', rsValueT]);

      rs := string(objT.asJson);

      objT.Free();

      len2T := length(rs);

      StrCopy(PChar(@(comBufG[5])), PChar(rs));

      comBufG[1] := len2T div (65536 * 256);
      comBufG[2] := (len2T mod (65536 * 256)) div 65536;
      comBufG[3] := (len2T mod (65536)) div 256;
      comBufG[4] := len2T mod (256);

      comBufG[0] := 2;
    end;

    sleep(10);
  end;

  comBufG[0] := 99;
end;

procedure TMonitorCharThread.doGuiCmd;
var
  i: integer;
  tmps: string;
  jAryT: TJSONArray;
  tmpDataT: TJSONEnum;
begin
  case guiCmdM of
    'quit': begin
      terminateFlagG := True;
      //application.Terminate;
    end;
    'alert': begin
      tk.showError('', guiValue1M, guiValue2M);
    end;
    'showInfo': begin
      tk.showInfo(guiValue1M, guiValue2M, guiValue3M);
    end;
    'showError': begin
      tk.showError(guiValue1M, guiValue2M, guiValue3M);
    end;
    'setAppTitle': begin
      Form1.Caption := guiValue1M;
    end;
    'confirmToQuit': begin
      tk.ShowMessage(guiValue1M, guiValue2M, guiValue3M);
      terminateFlagG := True;
    end;
    'getInput': begin
      if InputQuery(guiValue1M, guiValue2M, False, guiOut1M) then tk.pass()
      else
      begin
        guiOut1M := tk.errStr('failed to get input');
      end;
    end;
    'getPassword': begin
      guiOut1M := tk.getPassword(guiValue1M, guiValue2M, guiValue3M);
      //:= tk.errStr('failed to get input');
    end;
    'selectItem': begin
      tmps := tk.selectItem(guiValue1M, guiValue2M, guiValue3M, guiValue4M);

      guiOut1M := tmps;
    end;
    //'selectItem': begin
    //  Form4.Caption := guiValue1M;
    //  jAryT := getJson(guiValue2M) as TJSONArray;

    //  Form4.ListBox1.Clear;

    //  for tmpDataT in jAryT do
    //  begin
    //    tmps := tmpDataT.Value.AsString;
    //    Form4.ListBox1.Items.Add(tmps);
    //  end;

    //  FreeAndNil(jAryT);

    //  if Form4.ShowModal = mrOk then
    //  begin
    //    if Form4.ListBox1.SelCount < 1 then
    //    begin
    //      guiOut1M := tk.errStr('no item selected');
    //      exit;
    //    end;

    //    for i := 0 to Form4.ListBox1.Count - 1 do
    //    begin
    //      if Form4.ListBox1.Selected[i] then
    //      begin
    //        guiOut1M := Form4.ListBox1.Items[i];
    //        exit;
    //      end;
    //    end;
    //    guiOut1M := tk.errStr('failed to get selected item');
    //  end
    //  else
    //  begin
    //    guiOut1M := tk.errStr('canceled');
    //  end;
    //end;
    'selectFile': begin
      Form1.OpenDialog2.Title := guiValue1M;

      if not Form1.OpenDialog2.Execute then
      begin
        guiOut1M := tk.errStr('canceled');
      end
      else
      begin
        guiOut1M := Form1.OpenDialog2.FileName;
      end;
    end;
  end;

end;

end.
