unit unitProcess;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, Buttons,
  StdCtrls, tkunit;

type

  { TForm3 }

  TForm3 = class(TForm)
    ButtonClose: TButton;
    ButtonOk: TButton;
    Edit1: TEdit;
    ProgressBar1: TProgressBar;
    SpeedButton1: TSpeedButton;
    StaticText1: TStaticText;
    procedure ButtonCloseClick(Sender: TObject);
    procedure ButtonOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private

  public
    runModeM: string;
    codeTextM: string;
    secureCodeM: string;
    terminateWhileFailM: boolean;

  end;

var
  Form3: TForm3;

implementation

{$R *.lfm}

uses unit1;

  { TForm3 }

  //var
  //runCharThreadG: TRunCharThread;
  //runCharThreadSignalG: integer;


procedure TForm3.FormCreate(Sender: TObject);
begin
  terminateWhileFailM := False;
end;

procedure TForm3.ButtonCloseClick(Sender: TObject);
begin
  application.Terminate;
end;

procedure TForm3.ButtonOkClick(Sender: TObject);
begin
  case runModeM of
    'password': begin
      secureCodeM := trim(Edit1.Text);
      Form3.Hide;

      SpeedButton1Click(nil);
    end;

  end;
end;

procedure TForm3.FormShow(Sender: TObject);
var
  tmps: string;
  fcT: string;
  cmdT: string;
  listT: TStringArray;
begin
  tmps := tk.joinPath([tk.getAppDir(), 'auto.cxg']);

  if fileExists(tmps) then
  begin
    fcT := tk.loadStringFromFile(tmps);
    listT := tk.strSplitLines(fcT);

    if length(listT) > 0 then
    begin
      runModeM := trim(listT[0]);
      case runModeM of
        'password': begin
          if length(listT) >= 8 then
          begin
            Form3.Caption := trim(listT[1]);
            Form3.StaticText1.Caption := trim(listT[2]);

            if trim(listT[3]) = 'true' then
            begin
              Edit1.Text := '';
              Edit1.PasswordChar := '*';
              Edit1.Visible := True;
            end
            else
              Edit1.Visible := False;

            if trim(listT[4]) = 'true' then
            begin
              ProgressBar1.Visible := True;
            end
            else
              ProgressBar1.Visible := False;

            if trim(listT[5]) = '' then
            begin
              ButtonOk.Visible := False;
            end
            else
            begin
              ButtonOk.Visible := True;
              ButtonOk.Caption := trim(listT[5]);
            end;

            if trim(listT[6]) = '' then
            begin
              ButtonClose.Visible := False;
            end
            else
            begin
              ButtonClose.Visible := True;
              ButtonClose.Caption := trim(listT[6]);
            end;

            if trim(listT[7]) = 'true' then
            begin
              terminateWhileFailM := True;
            end
            else
            begin
              terminateWhileFailM := False;
            end;

            codeTextM := string.Join(#10, listT, 8, length(listT) - 7);
            //showMessage(codeTextM);

            Edit1.SetFocus;
            exit;
          end
          else
          begin
            tk.showError('Error', 'not enough paramters');
          end;
        end;
        else
          tk.showError('Error', 'failed to parse auto start file');
      end;

      application.Terminate;
      exit;
    end;

    tk.showError('Error', 'failed to parse auto start file');

    //freeAndNil(listT);
    application.Terminate;
    exit;

  end;

  Form3.hide();
  Form1.ShowModal;

end;

procedure TForm3.SpeedButton1Click(Sender: TObject);
var
  s1: string;
begin
  if runCharThreadSignalG <> 0 then
  begin
    ShowMessage('A Charlang session is already running now.');
    exit;
  end;

  runCharThreadSignalG := 1;

  runCharThreadG := TRunCharThread.Create(True);
  s1 := trim(codeTextM);
  if startsStr('//TXTE#', s1) or tk.isHex(s1) then
  begin
    s1 := tk.decryptStringByTXTE(s1.Remove(0, 7), trim('char'));
  end;

  runCharThreadG.codeTextM := trim(s1);
  runCharThreadG.secureCodeM := trim(secureCodeM);
  runCharThreadG.terminateWhileFailM := terminateWhileFailM;
  runCharThreadG.Start;

  //application.Terminate;
end;

end.
