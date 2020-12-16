unit Unit1;

//{$mode objfpc}{$H+}
{$mode delphi}{$H+}

interface

uses
  // windows
  {$IFDEF MSWINDOWS}
  Windows, {%H-}Messages,
  {$ENDIF}
  // system
  {%H-}Types, SysUtils, Variants, Classes,
  DateUtils,
  // superobject all:
  //supertypes, supertimezone, superdate, superobject, uMarshalSuper,
  // superobject demo:
  superobject, superdate, uMarshalSuper,
  // LCL:
  Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    BMarshal: TButton;
    BUnMarshal: TButton;
    Memo1: TMemo;
    procedure BMarshalClick(Sender: TObject);
    procedure BUnMarshalClick(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.BMarshalClick(Sender: TObject);
var
  {%H-}D: TDateTime;
  S: string;
begin
  D := Now();

  S := JsonSerializer.Marshal<TDateTime>(D);

  Memo1.Text := S;
end;

procedure TForm1.BUnMarshalClick(Sender: TObject);
var
  D: TDateTime;
  S: string;
begin
  S := Trim(Memo1.Text);

  JsonSerializer.Unmarshal<TDateTime>(S, D);

  S := FormatDateTime('yyyy-mm-dd hh:nn:ss', D);
  S := string(DelphiDateToISO8601(D));

  ShowMessage('date: "'+S+'"');
end;

end.
