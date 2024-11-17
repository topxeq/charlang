unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, fpjson, fileutil, tkunit, Forms, Controls,
  Graphics, Dialogs, ExtCtrls,
  uCmdBox;

type

  TfuncCharlangBackG = function(codeA, paramA, secureCodeA, injectA,
    globalsA, comBufA: PChar): PChar; stdcall;

  TMonitorCharThread = class(TThread)
  private
    procedure AddMessage;
    procedure AddMessagePr;
  protected
    procedure Execute; override;
  public
    msgTextM: string;

    guiCmdM: string;
    guiValue1M: string;
    guiValue2M: string;
    guiValue3M: string;

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

    constructor Create(CreateSuspended: boolean);
    destructor Destroy; override;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    CmdBox1: TCmdBox;
    OpenDialog2: TOpenDialog;
    Timer1: TTimer;
    procedure CmdBox1Input(ACmdBox: TCmdBox; Input: string);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private

  public
    TextPosition: integer;
    Rdpw: boolean;


  end;

var
  Form1: TForm1;

  // globals
var
  versionG: string = '0.9.1';
  WebPortG: integer = 7458;

  libHandleG: THandle;

  funcCharlangBackG: TfuncCharlangBackG;

  monitorCharThreadG: TMonitorCharThread;

  runCharThreadG: TRunCharThread;
  runCharThreadSignalG: integer;

  terminateFlagG: boolean = False;

  filePathG: string;

  dllPathG: string;
  cliExePathG: string;

  comBufG: array [0..16777216] of byte;


implementation


{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  rs: string;
  filePathT: string;
  s1: string;
begin
  Form1.Caption := 'Chargo V' + versionG + ' by TopXeQ';

  filePathG := '';

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

  libHandleG := LoadLibrary(PChar(dllPathG));
  //tk.appendStringToFile('['+DateTimeToStr(Now)+'] libHandleG: ' + IntToStr(libHandleG) + #10, 'd:\tmpx\abclog.txt');
  if libHandleG <> 0 then
  begin

    Pointer(funcCharlangBackG) := GetProcAddress(libHandleG, 'QuickRunChar');

  end;

  runCharThreadSignalG := 0;

  monitorCharThreadG := TMonitorCharThread.Create(False);

  //CmdBox1.StartRead(clSilver, clBlack, '>', clYellow, clBlack);
  //CmdBox1.TextColors(clWhite, clBlack);
  //CmdBox1.Writeln(#27#218#27#10#191);
  //CmdBox1.Writeln(#27#179'Type "help" to see a short list of available commands.'#27#10#179);
  //CmdBox1.Writeln(#27#217#27#10#217);

  filePathT := tk.joinPath([tk.getAppDir(), 'auto.char']);

  if fileExists(filePathT) then
  begin
    if runCharThreadSignalG <> 0 then
    begin
      ShowMessage('A Charlang session is already running now.');
      exit;
    end;

    runCharThreadSignalG := 1;

    runCharThreadG := TRunCharThread.Create(True);
    s1 := trim(tk.loadStringFromFile(filePathT));
    if startsStr('//TXTE#', s1) or tk.isHex(s1) then
    begin
      s1 := tk.decryptStringByTXTE(s1.remove(0, 7), '');
    end;

    runCharThreadG.codeTextM := s1;
    runCharThreadG.secureCodeM := '';

    runCharThreadG.Start;
  end else begin
    CmdBox1.StartRead(clSilver, clBlack, '>', clYellow, clBlack);
  end;

  CmdBox1.TextColors(clWhite, clBlack);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if libHandleG <> 0 then
  begin
    FreeLibrary(LibHandleG);
  end;

  Application.Terminate;

end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if terminateFlagG then
  begin
    self.Close();

    application.Terminate;
  end;
end;

procedure TForm1.CmdBox1Input(ACmdBox: TCmdBox; Input: string);
var
  i: integer;
begin
  if rdpw then
  begin
    CmdBox1.TextColors(clLime, clBlue);
    CmdBox1.Writeln('Your Secret Password : ' + Input);
    CmdBox1.TextColors(clSilver, clNavy);
    rdpw := False;
  end
  else
  begin
    rdpw := False;
    Input := LowerCase(Input);
    if Input = 'help' then
    begin
      CmdBox1.TextColors(clLime, clNavy);
      CmdBox1.Writeln(#27#218#27#197#128#0#27#194#27#10#191);
      CmdBox1.Writeln(#27#179' Command'#27#33#128#0#27#179' Explanation'#27#10#179);
      CmdBox1.Writeln(#27#195#27#197#128#0#27#198#27#10#180);
      CmdBox1.Writeln(#27#179' help'#27#33#128#0#27#179' Gives this list of Commands'#27#10#179);
      CmdBox1.Writeln(#27#179' clear'#27#33#128#0#27#179' Clears the Content of CmdBox'#27#10#179);
      CmdBox1.Writeln(#27#179' start'#27#33#128#0#27#179' Outputs the Content of Demotext.txt from the beginning'#27#10#179);
      CmdBox1.Writeln(#27#179' stop'#27#33#128#0#27#179' Stops output and resets to Start'#27#10#179);
      CmdBox1.Writeln(#27#179' pause'#27#33#128#0#27#179' Interrupts output'#27#10#179);
      CmdBox1.Writeln(#27#179' resume'#27#33#128#0#27#179' Resumes output from the last position'#27#10#179);
      CmdBox1.Writeln(#27#179' clearhistory'#27#33#128#0#27#179' Clears all history entries'#27#10#179);
      //CmdBox1.Writeln(#27#179' readpwd'#27#33#128#0#27#179' Read a Password (just as a test)'#27#10#179);
      CmdBox1.Writeln(#27#179' exit'#27#33#128#0#27#179' Exit program'#27#10#179);
      CmdBox1.Writeln(#27#217#27#197#128#0#27#193#27#10#217);

      CmdBox1.TextColors(clWhite, clBlack);
      CmdBox1.Write('这是');
      CmdBox1.TextColors(clGreen, clBlack);
      CmdBox1.Write('1');
      CmdBox1.TextColors(clWhite, clBlack);
      CmdBox1.Write('个故事');
      CmdBox1.Writeln('');


      CmdBox1.TextColor(clSilver);
    end
    else
    if Input = 'readpwd' then
    begin
      rdpw := True;
    end
    else
    if Input = 'clearhistory' then
    begin
      CmdBox1.TextColor(clYellow);
      CmdBox1.Writeln('Clear History...');
      CmdBox1.TextColor(clSilver);
      CmdBox1.ClearHistory;
    end
    else
    if Input = 'start' then
    begin
      TextPosition := 0;
      //ReaderTimer.Enabled:=true;
      CmdBox1.TextColors(clLime, clBlue);
      CmdBox1.Writeln('Start...');
    end
    else if Input = 'stop' then
    begin
      TextPosition := 0;
      //ReaderTimer.Enabled:=false;
      CmdBox1.TextColors(clRed, clBlue);
      CmdBox1.Writeln('Stop...');
    end
    else if Input = 'pause' then
    begin
      //ReaderTimer.Enabled:=false;
      CmdBox1.TextColors(clPurple, clBlue);
      CmdBox1.Writeln('Pause...');
    end
    else if Input = 'resume' then
    begin
      //ReaderTimer.Enabled:=true;
      CmdBox1.TextColors(clGreen, clBlue);
      CmdBox1.Writeln('Continue...');
    end
    else if Input = 'clear' then
    begin
      CmdBox1.Clear;
    end
    else if Input = 'exit' then Close
    else
    begin
      CmdBox1.TextColors(clYellow, ClRed);
      CmdBox1.Writeln('Invalid Command!');
    end;
  end;
  if rdpw then CmdBox1.StartReadPassWord(clYellow, clNavy, 'Pwd:', clLime, clNavy)
  else
    CmdBox1.StartRead(clSilver, clBlack, '>', clYellow, clBlack);
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
end;

procedure TMonitorCharThread.AddMessagePr;
begin
  Form1.CmdBox1.TextColors(clWhite, clBlack);
  Form1.CmdBox1.Write(msgTextM);
end;

procedure TMonitorCharThread.Execute;
var
  signalT: byte;
  lenT: int32;
  len2T: int32;
  comStrT: string;
  comObjT: TJSONData;
  objT: TJSONObject;
  cmdT: string;
  statusValueT: string;
  rs: string;
  rsValueT: string;
begin
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

      comObjT := getJson(comStrT);

      cmdT := comObjT.GetPath('cmd').AsString;

      statusValueT := 'success';

      case cmdT of
        'selectFile':
        begin
          guiCmdM := 'selectFile';
          guiValue1M := comObjT.GetPath('title').AsString;
          guiValue2M := comObjT.GetPath('opts').AsString;
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
          guiValue1M := comObjT.GetPath('value').AsString;
          Synchronize(@doGuiCmd);
          rsValueT := '';
        end;
        'showInfo':
        begin
          guiCmdM := 'showInfo';
          guiValue1M := comObjT.GetPath('title').AsString;
          guiValue2M := comObjT.GetPath('value').AsString;
          Synchronize(@doGuiCmd);
          rsValueT := '';
        end;
        'showError':
        begin
          guiCmdM := 'showError';
          guiValue1M := comObjT.GetPath('title').AsString;
          guiValue2M := comObjT.GetPath('value').AsString;
          Synchronize(@doGuiCmd);
          rsValueT := '';
        end;
        'getInput':
        begin
          guiCmdM := 'getInput';
          guiValue1M := comObjT.GetPath('title').AsString;
          guiValue2M := comObjT.GetPath('value').AsString;
          Synchronize(@doGuiCmd);
          rsValueT := guiOut1M;
        end;
        'getPassword':
        begin
          guiCmdM := 'getPassword';
          guiValue1M := comObjT.GetPath('title').AsString;
          guiValue2M := comObjT.GetPath('value').AsString;
          Synchronize(@doGuiCmd);
          rsValueT := guiOut1M;
        end;
        'selectItem':
        begin
          guiCmdM := 'selectItem';
          guiValue1M := comObjT.GetPath('title').AsString;
          guiValue2M := comObjT.GetPath('value').AsString;
          Synchronize(@doGuiCmd);
          rsValueT := guiOut1M;
        end;
        'pln':
        begin
          msgTextM := comObjT.GetPath('value').AsString;
          Synchronize(@AddMessage);
          rsValueT := IntToStr(length(msgTextM));
        end;
        'pr':
        begin
          msgTextM := comObjT.GetPath('value').AsString;
          Synchronize(@AddMessagePr);
          rsValueT := IntToStr(length(msgTextM));
        end;
        'checkJson':
        begin
          msgTextM := comObjT.GetPath('value').AsString;
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

      objT := TJSONObject.Create(['Status', statusValueT, 'Value', rsValueT]);
      // 'abdkhds代付款很舒服开始'

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
begin
  Form1.CmdBox1.TextColors(clWhite, clBlack);
  Form1.CmdBox1.Writeln(msgTextM);
  //Form1.Memo1.Lines.Add(msgTextM);
  //Form1.Memo1.selstart := MaxInt;
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
      PChar('{"guiServerUrlG":"http://127.0.0.1:' + IntToStr(WebPortG) + '"}'),
      PChar(@comBufG));

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


end.
