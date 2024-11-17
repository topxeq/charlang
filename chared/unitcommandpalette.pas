unit unitCommandPalette;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, StrUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Buttons, tkunit;

type

  { TForm2 }

  TForm2 = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    procedure BitBtn1Click(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
  private

  public

  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

uses unit1, unitProcess;

  { TForm2 }

procedure TForm2.FormCreate(Sender: TObject);
begin

end;

procedure TForm2.FormKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if Shift = [] then
  begin
    case Key of
      27: // esc
      begin
        self.Close();
        exit;
      end;
    end;
  end;

  if Shift = [ssCtrl] then
  begin
    case Key of
      71: // g
      begin
        BitBtn1Click(nil);
        exit;
      end;
    end;
  end;

  //Form1.addMessage('FormKeyUp: ' + IntToStr(Key) + ' ' +
  //tk.toStr(Shift, typeInfo(Shift)));

  //  if ssCtrl in Shift then
  //begin
  //  case Key of
  //    78: // ctrl-n
  //    begin
  //      ToolButton1Click(nil);
  //    end;
  //    80: // ctrl-p
  //    begin
  //      ToolButton19Click(nil);
  //    end;
  //    82: // ctrl-r
  //    begin
  //      if ssShift in Shift then
  //        ToolButton10Click(nil)
  //      else
  //        ToolButton3Click(nil);
  //    end;
  //    83: // ctrl-s
  //    begin
  //      ToolButton7Click(nil);
  //    end;
  //    else
  //  end;
  //end;
  //end;

end;

procedure TForm2.BitBtn2Click(Sender: TObject);
begin
  self.Close;
end;

procedure TForm2.BitBtn1Click(Sender: TObject);
var
  tmps: string;
  listT: TStringList;
  rs: string;
  cmdT: string;
  secodeT: string;
  mapT: tkStrMap;
begin
  tmps := trim(memo1.Text);

  if tmps = '' then exit;

  listT := TStringList.Create;

  rs := tk.parseCommandLine(tmps, listT);

  if rs <> '' then
  begin
    tk.showError('Error', 'failed to parse command: ' + tk.getErrStr(rs));
    exit;
  end;

  if listT.Count < 1 then
  begin
    tk.showError('Error', 'failed to parse command: ' + 'empty command');
    exit;
  end;

  //Form1.addMessage(tk.toJson(listT));

  cmdT := listT[0];

  //Form1.addMessage('cmdT: ' + cmdT);

  if startsStr('mc', cmdT) then
  begin
    if @funcCharlangBackG <> nil then
    begin
      mapT := tk.newStrMap(['inputG', trim(cmdT)]);

      rs := funcCharlangBackG(PChar('global inputG' + #10 +
        'return getWeb("https://topget.org/ms/magic?auth=topxeq&valueonly=true&witherror=true&code="+inputG)'),
        PChar(''), PChar(''), PChar(''), PChar(tk.toJson(mapT)), PChar(@comBufG));

      FreeAndNil(mapT);

      //Form1.addMessage('rs: ' + rs);
      if rs <> 'TXERROR:undefined' then
      begin
        if startsStr('//TXTE#', rs) then
        begin
          rs := tk.decryptStringByTXTE(midStr(rs, 8, length(rs) - 7), 'char');
        end;
        //Form1.addMessage('rs: ' + rs);

        if runCharThreadSignalG <> 0 then
        begin
          ShowMessage('A Charlang session is already running now.');
          exit;
        end;

        runCharThreadSignalG := 1;

        runCharThreadG := TRunCharThread.Create(True);

        runCharThreadG.codeTextM := rs;
        //runCharThreadG.secureCodeM := trim(Form1.LabeledEdit1.Text);

        runCharThreadG.Start;

        exit;
        //rs := funcCharlangBackG(PChar(rs),
        //  PChar(''), PChar(''), PChar(''), PChar('{"guiServerUrlG":"http://127.0.0.1:' + IntToStr(WebPortG) + '"}'));

        //if rs <> 'TXERROR:undefined' then
        //begin
        //  Form1.addMessage('--- result: '+#10 + rs);
        //end;

        //Form1.SynEdit1.Text := rs;
      end;
    end;

    exit;
  end;

  case cmdT of
    'test': begin
        Form3.showModal();


    end;
    'encryptText': begin
      secodeT := tk.getArrayItem(listT, 1);

      if @funcCharlangBackG <> nil then
      begin
        mapT := tk.newStrMap(['inputG', Form1.SynEdit1.Text,
          'secureCodeG', secodeT]);

        rs := funcCharlangBackG(PChar(
          'global inputG'#10'global secureCodeG'#10'return encryptText(inputG, secureCodeG)'), PChar(''), PChar(''),
          PChar(''), PChar(tk.toJson(mapT)), PChar(@comBufG));

        FreeAndNil(mapT);

        if rs <> 'TXERROR:undefined' then
        begin
          Form1.SynEdit1.Text := rs;
          //Form1.AddMessage('--- Result:');
          //Form1.AddMessage(rs);
        end;
      end;

    end;
    'decryptText': begin
      secodeT := tk.getArrayItem(listT, 1);

      if @funcCharlangBackG <> nil then
      begin
        mapT := tk.newStrMap(['inputG', trim(Form1.SynEdit1.Text),
          'secureCodeG', secodeT]);

        rs := funcCharlangBackG(PChar(
          'global inputG'#10'global secureCodeG'#10'return decryptText(inputG, secureCodeG)'), PChar(''), PChar(''),
          PChar(''), PChar(tk.toJson(mapT)), PChar(@comBufG));

        FreeAndNil(mapT);

        if rs <> 'TXERROR:undefined' then
        begin
          Form1.SynEdit1.Text := rs;
        end;
      end;

    end;
  end;

  FreeAndNil(listT);
end;

end.
