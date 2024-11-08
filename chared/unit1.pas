unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  StdCtrls, ExtCtrls, PairSplitter, ATSynEdit, SynEdit, SynHighlighterAny,
  SynHighlighterCpp, SynHighlighterSQL, SynHighlighterJScript,
  SynHighlighterPas, ATStrings, tkunit, fphttpserver, fileutil,
  RegExpr, fpjson, Process, LazUTF8 ;

  // , ATStringProc, ATSynEdit_LineParts

const
  VersionG = '0.9.1';
  WebPortG = 7458;
  InjectG = 'pl := func(formatA, ...valuesA) { ' + #10 +
    '        rs1 := getWeb("http://127.0.0.1:7458", {"cmd": "pln", "value": spr(formatA, ...valuesA)})  '
    + #10 + '          return toInt(rs1, 0)      ' + #10 + '}' + #10 +
    #10 + 'pln := func(...valuesA) { ' + #10 +
    '        rs1 := getWeb("http://127.0.0.1:7458", {"cmd": "pln", "value": spln(...valuesA)})  '
    + #10 + '          return toInt(rs1, 0)      ' + #10 + '}' + #10;

type

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

    procedure AddMessage;
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

    constructor Create(CreateSuspended: boolean);
    destructor Destroy; override;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    Panel1: TPanel;
    Panel2: TPanel;
    Separator1: TMenuItem;
    StatusBar1: TStatusBar;
    SynEdit1: TSynEdit;
    SynJScriptSyn1: TSynJScriptSyn;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    //procedure ATSynEdit1CalcHilite(Sender: TObject; var AParts: TATLineParts;
    //ALineIndex, ACharIndex, ALineLen: integer; var AColorAfterEol: TColor);
    //procedure ATSynEdit1ChangeModified(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure SynEdit1Change(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure ToolButton5Click(Sender: TObject);
    procedure ToolButton6Click(Sender: TObject);
  private

  public
    procedure AddMessage(msgA: string);

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

  SynEdit1.Modified := True;
  Form1.Caption := 'CharEd V' + VersionG + ' by TopXeQ - *' + filePathG;

  //AddMessage(InjectG);

  //runCharThreadG := TRunCharThread.Create(True);
  //// This way it doesn't start automatically
  ////...
  ////[Here the code initialises anything required before the threads starts executing]
  ////...
  //runCharThreadG.Start;

  //ATSynEdit1.Gutter.LineNumberPart.MarkupInfo.Foreground := TColor($00999999);
  SynEdit1.Gutter.LineNumberPart.MarkupInfo.Background := TColor($000B0B0B);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  httpServerThreadG.Server.Active := False;

  freeAndNil(httpServerThreadG);

  if libHandleG <> 0 then
  begin
    FreeLibrary(LibHandleG);
  end;

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

procedure TForm1.SynEdit1Change(Sender: TObject);
begin

end;

procedure TForm1.ToolButton3Click(Sender: TObject);
//var
//rs: string;
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
  runCharThreadG.codeTextM := SynEdit1.Text;
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
//
//    SynEdit1.Lines.SaveToFile(filePathT);
//  end;
//
//  AProcess := TProcess.Create(nil);
//
//  //// Tell the new AProcess what the command to execute is.
//  AProcess.Executable := cliExePathG;
//  //AProcess.Parameters.Add('-cmd=pln(1.2);pln(2.5);pln(3.6);getInput()');
//  //AProcess.Parameters.Add('-view');
//  //AProcess.Parameters.Add('-pipe');
//
//  //ExecuteProcess(UTF8ToSys(cliExePathG), ' '+UTF8ToSys(filePathT), []);
//
//  AProcess.Parameters.Add(filePathT);
//  //AProcess.Parameters.Add('basic.char');
//
//  //// We will define an option for when the program
//  //// is run. This option will make sure that our program
//  //// does not continue until the program we will launch
//  //// has stopped running. Also now we will tell it that
//  //// we want to read the output of the file.
//  AProcess.Options := AProcess.Options + [poWaitOnExit];      //     poNewConsole,
//
//  ////str1 := 'pln(now())'+#10+' getInput()';
//
//  //// Now that AProcess knows what the commandline is it can be run.
//  try
//    AProcess.Execute;
//  finally
//  end;
//
//  ////AProcess.Input.Write(str1[1], length(str1));
//  ////
//  ////AProcess.CloseInput();
//
//  //count1 := 0;
//  //// {wait until external program has finished}
//  //while AProcess.Running do
//  //begin
//  ////        count1 := count1 + 1;
//  ////        //Edit1.Text:= ''+inttostr(count1);
//  //        sleep(50);
//  //end;
//
//  //// After AProcess has finished, the rest of the program will be executed.
//  ////end;
//
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
  runCharExtThreadG.Start;

end;

//procedure TRunCharThread.onTerminateProc(Sender: TObject);
//begin
//  runCharThreadSignalG := 0;
//end;

constructor TRunCharThread.Create(CreateSuspended: boolean);
begin
  runCharThreadSignalG := 1;
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
  //newStatus: string;
  rs: string;
begin
  if @funcCharlangBackG <> nil then
  begin
    rs := funcCharlangBackG(PChar(string(codeTextM)), PChar(IntToStr(WebPortG)),
      PChar(''), PChar(InjectG));

    msgTextM := '--- Result:';
    Synchronize(@AddMessage);
    msgTextM := rs;
    Synchronize(@AddMessage);
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
    'showInfo':
    begin
      msgTextM := ARequest.QueryFields.Values['value'] +
        ARequest.ContentFields.Values['value'];
      Synchronize(@AddMessage);
      rsValueT := '';
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
begin
  filePathT := filePathG;
  if tk.isStringNullOrEmpty(filePathT) then
  begin
    filePathT := GetTempFileName();

    tk.saveStringToFile(codeTextM, filePathT);

    //SynEdit1.Lines.SaveToFile(filePathT);
  end;

  AProcess := TProcess.Create(nil);

  //// Tell the new AProcess what the command to execute is.
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

  ////str1 := 'pln(now())'+#10+' getInput()';

  //// Now that AProcess knows what the commandline is it can be run.
  try
    AProcess.Execute;
  finally
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
  AProcess.Free;

  runCharExtThreadSignalG := 0;
end;


end.
