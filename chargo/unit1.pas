unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, fpjson, fileutil, tkunit, Forms, Controls,
  Graphics, Dialogs, ExtCtrls, Menus, Types,
  uCmdBox, StrHolder, TypInfo, UnitCommandPalette;

const
  maxComBufLenG = 16777216;
  versionG = '0.9.2';
  webPortG = 7458;

type

  TfuncCharlangBackG = function(codeA, paramA, secureCodeA, injectA,
    globalsA, comBufA: PChar): PChar; stdcall;

  TStartCommandPaletteThread = class(TThread)
    //private
    //  procedure AddMessage;
  protected
    procedure Execute; override;
  public
    msgTextM: string;

    procedure doGuiCmd;

    constructor Create(CreateSuspended: boolean);
    destructor Destroy; override;
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

    constructor Create(CreateSuspended: boolean);
    destructor Destroy; override;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    CmdBox1: TCmdBox;
    MenuItem1: TMenuItem;
    OpenDialog2: TOpenDialog;
    PopupMenu1: TPopupMenu;
    SaveDialog1: TSaveDialog;
    StrHolder1: TStrHolder;
    Timer1: TTimer;
    Timer2: TTimer;
    procedure CmdBox1Input(ACmdBox: TCmdBox; Input: string);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
  private

  public
    TextPosition: integer;
    Rdpw: boolean;


  end;

var
  Form1: TForm1;

  // globals
var

  libHandleG: THandle;

  funcCharlangBackG: TfuncCharlangBackG;

  monitorCharThreadG: TMonitorCharThread;

  runCharThreadG: TRunCharThread;
  runCharThreadSignalG: integer;

  startCommandPaletteThreadG: TStartCommandPaletteThread;
  startCommandPaletteThreadSignalG: integer;

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
  //s1: string;
  listT: TStringArray;
  titleT, promptT: string;
  optsT: string;
  sfoT: SimpleFlexObject;
  tmps: string;
  innerCodeT: string;
  scriptPathT: string;
begin
  Form1.Caption := 'Chargo V' + versionG + ' by TopXeQ';

  Application.CreateForm(TForm2, Form2);

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

  startCommandPaletteThreadSignalG := 0;

  monitorCharThreadG := TMonitorCharThread.Create(False);

  CmdBox1.TextColors(clWhite, clBlack);

  //CmdBox1.StartRead(clSilver, clBlack, '>', clYellow, clBlack);
  //CmdBox1.TextColors(clWhite, clBlack);
  //CmdBox1.Writeln(#27#218#27#10#191);
  //CmdBox1.Writeln(#27#179'Type "help" to see a short list of available commands.'#27#10#179);
  //CmdBox1.Writeln(#27#217#27#10#217);

  scriptPathT := tk.getCliParam(1);

  filePathT := tk.joinPath([tk.getAppDir(), 'auto.char']);

  innerCodeT := '';

  if (scriptPathT <> '') then
  begin
    innerCodeT := trim(tk.loadStringFromFile(scriptPathT));

    if tk.isErrStr(innerCodeT) then
    begin
      tk.showError('error', 'failed to load script: ' + tk.getErrStr(innerCodeT));
      application.Terminate;
      exit;
    end;
  end
  else if (StrHolder1.Strings.Text.Trim <> '') then
  begin
    innerCodeT := StrHolder1.Strings.Text;
  end
  else if fileExists(filePathT) then
  begin
    innerCodeT := trim(tk.loadStringFromFile(filePathT));

    if tk.isErrStr(innerCodeT) then
    begin
      tk.showError('error', 'failed to load script: ' + tk.getErrStr(innerCodeT));
      application.Terminate;
      exit;
    end;
  end;

  if innerCodeT <> '' then
  begin
    if runCharThreadSignalG <> 0 then
    begin
      ShowMessage('A Charlang session is already running now.');
      exit;
    end;

    runCharThreadSignalG := 1;

    runCharThreadG := TRunCharThread.Create(True);

    if startsStr('//TXTE#', innerCodeT) or tk.isHex(innerCodeT) then
    begin
      innerCodeT := tk.decryptStringByTXTE(innerCodeT.remove(0, 7), '');
    end;

    if startsStr('//TXSR#', innerCodeT) then
    begin
      innerCodeT := innerCodeT.remove(0, 7);

      listT := innerCodeT.Split(['|||']);

      optsT := '';

      if length(listT) < 3 then
      begin
        titleT := 'Please enter';
        promptT := 'Secure code: ';
      end
      else
      begin
        sfoT := SimpleFlexObject.Create(listT[1]);

        titleT := sfoT.getMapItem('title');
        promptT := sfoT.getMapItem('prompt');
        optsT := sfoT.getMapItem('opts');

        FreeAndNil(sfoT);

        innerCodeT := listT[2];

      end;

      tmps := tk.getPassword(titleT, promptT, optsT);

      if tk.isErrStr(tmps) then
      begin
        runCharThreadG.Terminate;
        //freeAndNil( runCharThreadG);

        application.Terminate;
        exit;
      end;

      runCharThreadG.secureCodeM := tmps;
    end;

    runCharThreadG.codeTextM := innerCodeT;
    //runCharThreadG.secureCodeM := '';

    runCharThreadG.Start;
  end
  else
  begin
    //CmdBox1.StartRead(clSilver, clBlack, '>', clYellow, clBlack);
    //Form2.Memo1.Text := '';
    //Form2.showModal;
    if startCommandPaletteThreadSignalG <> 0 then
    begin
      ShowMessage('A command palette session is already running now.');
      exit;
    end else begin
      startCommandPaletteThreadSignalG := 1;

      startCommandPaletteThreadG := TStartCommandPaletteThread.Create(false);
    end;


  end;

  // madarinStart
  //Form1.CmdBox1.WriteLn(scriptPathT);
  //Form1.CmdBox1.WriteLn(tk.toStr(tk.jsonToStrMap('{"cmd": "plnColor", "value": "value1", "opts": "abc"}'), typeInfo(txStrMap)));
  //timer2.Enabled:=true;
  //Form1.CmdBox1.WriteLn(tk.encryptStringByTXDEF(
  //  'a74khfdsk  答复客户福克斯hdksfhs@!*^#'));
  //Form1.CmdBox1.WriteLn(tk.decryptStringByTXDEF(
  //  '8C97B78D5F5E9E899496A19B59471BE5C81BE3BA1FECDC22CDEA27EACF27D0C42CE0F5B0B5AABFB6B4C197667CB475'));
  ////Form1.CmdBox1.StartRead(clSilver, clBlack, '>', clYellow, clBlack);
  //Form1.CmdBox1.WriteLn('|||title^^abc电缆敷设$$  468264832 dfdf$$msg^^ldsjfld记录到时间了$$jlsajflads|||');
  //sfoT := SimpleFlexObject.Create(
  //  '|||title^^abc电缆敷设$$  468264832 dfdf$$msg^^ldsjfld记录到时间了$$jlsajflads|||');
  //Form1.CmdBox1.WriteLn(sfoT.toStr());
  //Form1.CmdBox1.WriteLn(sfoT.encode());
  //Form1.CmdBox1.WriteLn(
  //  'a74khfabcdefg[hi]jk{lmno}pqrstuvwxyzdsk  答复客户福克斯hdksfhs@!*^#');
  //Form1.CmdBox1.WriteLn(tk.encryptStringByTXDEF(
  //  'a74kgtzbcdefhfdsk  答复客户福克斯hdksfhs@!*^#'));
  ////Form1.CmdBox1.UpdateLineHeights   ;

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  if libHandleG <> 0 then
  begin
    FreeLibrary(LibHandleG);
  end;

  Application.Terminate;

end;

procedure TForm1.MenuItem1Click(Sender: TObject);
begin
  if saveDialog1.Execute then
  begin
    CmdBox1.SaveToFile(saveDialog1.FileName);
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if terminateFlagG then
  begin
    self.Close();

    application.Terminate;
  end;
end;

procedure TForm1.Timer2Timer(Sender: TObject);
begin
  //CmdBox1.Write(#8#8+tk.getNowStr());
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

constructor TStartCommandPaletteThread.Create(CreateSuspended: boolean);
begin
  inherited Create(CreateSuspended);

  FreeOnTerminate := True;
end;

destructor TStartCommandPaletteThread.Destroy;
begin
  inherited;
end;

procedure TStartCommandPaletteThread.doGuiCmd;
begin
  Form2.Memo1.Text := '';
  Form2.showModal;
end;

procedure TStartCommandPaletteThread.Execute;
begin
  Synchronize(@doGuiCmd);
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

      if comStrT.StartsWith('|||') then
      begin
        comObjT := SimpleFlexObject.Create(comStrT);
      end
      else
      begin
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
          rsValueT := 'TX_nr_XT';
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
          rsValueT := 'TX_nr_XT';
        end;
        'showError':
        begin
          guiCmdM := 'showError';
          guiValue1M := tk.getMapItem(comObjT, 'title');
          guiValue2M := tk.getMapItem(comObjT, 'value');
          guiValue3M := tk.getMapItem(comObjT, 'opts');
          Synchronize(@doGuiCmd);
          rsValueT := 'TX_nr_XT';
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
        'login':
        begin
          guiCmdM := 'login';
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
    'login': begin
      guiOut1M := tk.login(guiValue1M, guiValue2M, guiValue3M);
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

    rs := funcCharlangBackG(PChar(string(codeTextM)), PChar(IntToStr(webPortG)),
      PChar(secureCodeM), PChar(''),
      PChar('{"guiServerUrlG":"http://127.0.0.1:' + IntToStr(webPortG) + '"}'),
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
