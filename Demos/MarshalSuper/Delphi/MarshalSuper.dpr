program MarshalSuper;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  uMarshalSuper in '..\uMarshalSuper.pas',
  supertypes in '..\..\..\supertypes.pas',
  supertimezone in '..\..\..\supertimezone.pas',
  superdate in '..\..\..\superdate.pas',
  //superdbg in '..\..\..\superdbg.pas',
  superobject in '..\..\..\superobject.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
