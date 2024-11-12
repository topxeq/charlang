unit unitCommandPalette;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
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

uses unit1;

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
  listT: TStringList;
  rs: string;
  cmdT: string;
  secodeT: string;
  mapT: tkStrMap;
begin
  listT := TStringList.Create;

  rs := tk.parseCommandLine(memo1.Text, listT);

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
  case cmdT of
    'encryptText': begin
      secodeT := tk.getArrayItem(listT, 1);

      if @funcCharlangBackG <> nil then
      begin
        mapT := tk.newStrMap(['inputG', Form1.SynEdit1.Text,
          'secureCodeG', secodeT]);

        rs := funcCharlangBackG(PChar(
          'global inputG'#10'global secureCodeG'#10'return encryptText(inputG, secureCodeG)'), PChar(''),
          PChar(''), PChar(''), PChar(tk.toJson(mapT)));

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
        mapT := tk.newStrMap(['inputG', Form1.SynEdit1.Text,
          'secureCodeG', secodeT]);

        rs := funcCharlangBackG(PChar(
          'global inputG'#10'global secureCodeG'#10'return decryptText(inputG, secureCodeG)'), PChar(''),
          PChar(''), PChar(''), PChar(tk.toJson(mapT)));

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
