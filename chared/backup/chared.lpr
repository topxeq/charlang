program chared;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, unit1, tkunit, unitCommandPalette, unitProcess   ,SysUtils
  { you can add units after this };

{$R *.res}
//var
//  SplashForm: TForm3;
begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;

  //SplashForm := TForm3.Create(nil);
  //SplashForm.Show;
  //Application.ProcessMessages;
  //Sleep(1000);

  Application.CreateForm(TForm3, Form3);
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);

  //SplashForm.Close;
  //SplashForm.Free;

  //Application.ShowMainForm := False;
  Application.Run;
end.

