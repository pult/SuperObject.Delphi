unit Unit1;

interface

uses
  // windows
  Winapi.Windows, Winapi.Messages,
  // system
  Types, SysUtils, Variants, System.Classes,
  DateUtils,
  // superobjec
  superobject, superdate, uMarshalSuper,
  // VCL
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

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

{$R *.dfm}

{ TForm1 }

procedure TForm1.BMarshalClick(Sender: TObject);
var
  D: TDateTime;
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
  S := DelphiDateToISO8601(D);

  ShowMessage('date: "'+S+'"');
end;

end.
