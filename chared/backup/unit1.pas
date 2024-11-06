unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  StdCtrls, ATSynEdit, SynEdit, SynHighlighterAny, SynHighlighterCpp,
  SynHighlighterSQL, SynHighlighterJScript, SynHighlighterPas,
  ATSynEdit_LineParts, ATStrings, ATStringProc, tkunit, fphttpserver, RegExpr;

const
  VersionG = '0.9.1';
  WebPortG = 7458;

type

  TfuncCharlangBack = function(codeA, paramA: PChar): PChar; stdcall;

  THTTPServerThread = class(TThread)
  private
    _Error: string;
  public
    Server: TFPHTTPServer;
    constructor Create(APort: word; const OnRequest: THTTPServerRequestHandler);
    destructor Destroy; override;
    procedure Execute; override;
    property Error: string read _Error;
  end;

  TRunCharThread = class(TThread)
  private
    procedure AddMessage;
  protected
    procedure Execute; override;
  public
    msgText: string;

    codeText: string;

    constructor Create(CreateSuspended: boolean);
  end;

  { TForm1 }

  TForm1 = class(TForm)
    ATSynEdit1: TATSynEdit;
    FPHttpServer1: TFPHttpServer;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    Separator1: TMenuItem;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    procedure ATSynEdit1CalcHilite(Sender: TObject; var AParts: TATLineParts;
      ALineIndex, ACharIndex, ALineLen: integer; var AColorAfterEol: TColor);
    procedure ATSynEdit1ChangeModified(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FPHttpServer1Request(Sender: TObject;
      var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
    procedure MenuItem4Click(Sender: TObject);
    procedure SynEdit1Change(Sender: TObject);
    procedure ToolButton3Click(Sender: TObject);
    procedure ToolButton5Click(Sender: TObject);
  private

  public
    funcCharlangBack: TfuncCharlangBack;
    libHandle: THandle;
    runCharThread: TRunCharThread;
    filePathM: string;

    procedure onRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest;
      var AResponse: TFPHTTPConnectionResponse);
  end;

var
  httpServerThreadM: THTTPServerThread;
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

  filePathM := '';

  httpServerThreadM := THTTPServerThread.Create(WebPortG, @onRequest);

  //tk.appendStringToFile('['+DateTimeToStr(Now)+'] start...' + #10, 'd:\tmpx\abclog.txt');
  LibHandle := LoadLibrary(PChar('char.dll'));
  //tk.appendStringToFile('['+DateTimeToStr(Now)+'] LibHandle: ' + IntToStr(LibHandle) + #10, 'd:\tmpx\abclog.txt');
  if LibHandle <> 0 then
  begin

    Pointer(funcCharlangBack) := GetProcAddress(LibHandle, 'QuickRunChar');

    if @funcCharlangBack <> nil then
    begin
      rs := funcCharlangBack('appendText("abcde", `D:\tmpx\ddd.txt`)', '3');
      //tk.appendStringToFile('rs: ' + rs, 'd:\tmpx\abclog.txt');
    end
    else
    begin
      tk.appendStringToFile('func not found', 'd:\tmpx\abclog.txt');
    end;
  end;

  //if ATSynEdit1.Modified then
  //begin
  //  Form1.Caption := 'CharEd V' + VersionG + ' by TopXeQ - *' + filePathM;
  //end
  //else begin
  //  Form1.Caption := 'CharEd V' + VersionG + ' by TopXeQ - ' + filePathM;
  //end;

  ATSynEdit1.Text := 'appendText("abcde99999\n", `D:\tmpx\ddd.txt`)' +
    #10 + #10 + 'pln(123)' +
    'rs := getWeb("http://127.0.0.1:7458", {"req": "test", "value": "18"})' +
    #10 + 'return rs';

  ATSynEdit1.Modified := True;
  Form1.Caption := 'CharEd V' + VersionG + ' by TopXeQ - *' + filePathM;

  runCharThread := TRunCharThread.Create(True);
  // This way it doesn't start automatically
  //...
  //[Here the code initialises anything required before the threads starts executing]
  //...
  runCharThread.Start;

  //ATSynEdit1.Gutter.LineNumberPart.MarkupInfo.Foreground := TColor($00999999);
  //SynEdit1.Gutter.LineNumberPart.MarkupInfo.Background := TColor($00111111);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  httpServerThreadM.Server.Active := False;
end;

procedure TForm1.FPHttpServer1Request(Sender: TObject;
  var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse);
begin
  AResponse.Code := 200;
  AResponse.SetCustomHeader('Access-Control-Allow-Origin', '*');
  AResponse.ContentType := 'text/html;charset=utf-8';
  AResponse.Content := 'abdkhds代付款很舒服开始';
end;

procedure TForm1.ATSynEdit1CalcHilite(Sender: TObject; var AParts: TATLineParts;
  ALineIndex, ACharIndex, ALineLen: integer; var AColorAfterEol: TColor);
var
  nlen, npart, noffset: integer;
  kind, kindnew: integer;
  regex1: TRegExpr;

  procedure Add;
  begin
    if npart > High(AParts) then exit;
    with AParts[npart] do
    begin
      ColorBG := clNone;
      case kind of
        1: begin
          ColorFont := clblue;
        end;
        2: begin
          ColorFont := clgreen;
          ColorBorder := clgreen;
          BorderDown := TATLineStyle.Rounded;
          BorderUp := TATLineStyle.Rounded;
          BorderLeft := TATLineStyle.Rounded;
          BorderRight := TATLineStyle.Rounded;
          FontStyles := afsFontBold + afsFontItalic;
        end;
        3: begin
          ColorFont := clred;
          Colorbg := clyellow;
          ColorBorder := clred;
          BorderDown := TATLineStyle.Wave;
          //BorderLeft:= cLineDotted;
          //BorderRight:= cLineRounded;
          //BorderUp:= cLineRounded;
        end;
        else
        begin
          ColorFont := clgray;
        end;
      end;
      Offset := noffset;
      Len := nlen;
    end;
    Inc(npart);
  end;

var
  Str: atString;
  i: integer;
  fileT: TextFile;
  idx: integer;
  matchCountT: integer;
  item: TATLinePart;
begin
  exit;

  Str := Copy(ATSynEdit1.Strings.Lines[ALineIndex], ACharIndex, ALineLen);

  assignFile(fileT, 'd:\tmpx\abclog.txt');
  append(fileT);

  writeln(fileT, 'total: ', ALineIndex, ',', ACharIndex, ',', ALineLen,
    ',', Length(Str));

  regex1 := TRegexpr.Create;
  regex1.Expression := '\d+';
  matchCountT := 0;

  if regex1.Exec(Str) then
  begin
    writeln(fileT, 'match ', matchCountT, ':', regex1.Match[0]);
    while regex1.ExecNext do
    begin
      writeln(fileT, 'match ', matchCountT, ':', regex1.Match[0]);
    end;
  end;

  idx := 0;
  //length(AParts)
  for item in AParts do
  begin
    writeln(fileT, idx, ':', item.Offset, ',', item.Len);
    idx := idx + 1;
  end;

  closeFile(fileT);

  npart := 0;
  noffset := 0;
  nlen := 1;
  kind := -1;

  //for i := 1 to Length(Str) do
  //begin
  //  case Str[i] of
  //    'w': kindnew := 1;
  //    'e': kindnew := 2;
  //    '0'..'9': kindnew := 3;
  //    else
  //      kindnew := 0;
  //  end;

  //  if kindnew = kind then
  //  begin
  //    Inc(nlen);
  //    Continue;
  //  end;
  //  if kind >= 0 then Add;
  //  kind := kindnew;
  //  nlen := 1;
  //  noffset := i - 1;
  //end;

  ////DebugLn('before add');

  //Add;

  //test
  AParts[0].Offset := 0;
  AParts[0].Len := 1;
  AParts[0].ColorFont := clred;
  AParts[0].Colorbg := clgreen;

  AParts[1].Offset := 1;
  AParts[1].Len := 1;
  AParts[1].ColorFont := clblack;
  AParts[1].Colorbg := clyellow;
end;

procedure TForm1.ATSynEdit1ChangeModified(Sender: TObject);
begin
  if ATSynEdit1.Modified then
  begin
    Form1.Caption := 'CharEd V' + VersionG + ' by TopXeQ - *' + filePathM;
  end
  else
  begin
    Form1.Caption := 'CharEd V' + VersionG + ' by TopXeQ - ' + filePathM;
  end;
end;


procedure TForm1.MenuItem4Click(Sender: TObject);
begin
  self.Close();
end;

procedure TForm1.SynEdit1Change(Sender: TObject);
begin

end;

procedure TForm1.ToolButton3Click(Sender: TObject);
var
  rs: string;
begin
  if @funcCharlangBack <> nil then
  begin
    rs := funcCharlangBack(PChar(string(ATSynEdit1.Text)), PChar(IntToStr(WebPortG)));

    Form1.Memo1.Lines.Add('--- Result:');
    Form1.Memo1.Lines.Add(rs);
    //Form1.Memo1.Lines.Add(PChar(string(ATSynEdit1.Text)));
    //Form1.Memo1.Lines.Add(PChar(IntToStr(WebPortG)));
    Form1.Memo1.selstart := MaxInt;
  end;

end;

procedure TForm1.ToolButton5Click(Sender: TObject);
begin
  self.Close();
end;

procedure TForm1.onRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest;
  var AResponse: TFPHTTPConnectionResponse);
begin
  Form1.Memo1.Lines.Add('HTTP request');
  Form1.Memo1.selstart := MaxInt;

  AResponse.Code := 200;
  AResponse.SetCustomHeader('Access-Control-Allow-Origin', '*');
  AResponse.ContentType := 'text/html;charset=utf-8';
  AResponse.Content := 'abdkhds代付款很舒服开始';
end;

constructor TRunCharThread.Create(CreateSuspended: boolean);
begin
  inherited Create(CreateSuspended);
  FreeOnTerminate := True;
end;

procedure TRunCharThread.AddMessage;
// this method is executed by the mainthread and can therefore access all GUI elements.
begin
  //Form1.Memo1.Lines.Add(msgText);

  //Form1.Memo1.selstart := MaxInt;
  //// Memo1.CaretPos := Point(0, Memo1.Lines.Count-1);

  Form1.StatusBar1.Panels[1].Text := msgText;
end;

procedure TRunCharThread.Execute;
var
  newStatus: string;
begin
  msgText := 'TRunCharThread Starting...';
  Synchronize(@AddMessage);
  msgText := 'TRunCharThread Running...';
  while (not Terminated) do
  begin
    NewStatus := '[' + DateTimeToStr(Now) + ']';
    if NewStatus <> msgText then
    begin
      msgText := newStatus;
      Synchronize(@AddMessage);
    end;

    sleep(500);
  end;
end;

constructor THTTPServerThread.Create(APort: word;
  const OnRequest: THTTPServerRequestHandler);
begin
  Server := TFPHTTPServer.Create(nil);
  Server.Port := APort;
  Server.OnRequest := OnRequest;
  _Error := 'nil';
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

end.
