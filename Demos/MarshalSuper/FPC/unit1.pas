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
    CType: TComboBox;
    MMarshal: TMemo;
    MUnMarshal: TMemo;
    procedure BMarshalClick(Sender: TObject);
    procedure BUnMarshalClick(Sender: TObject);
  private
    procedure marshal_DateTime();
    procedure un_marshal_DateTime();

    procedure marshal_CustomRecord();
    procedure un_marshal_CustomRecord();

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  supertypes, {%H-}Rtti, TypInfo;

type
  TCustomDataTime = type TDateTime;
  //TCustomDataTime2 = type TDateTime;
  //TCustomBoolean = type Boolean; // marshaled into texts CustomBooleanValues

  (*TCustomRecord = record
    [SOName('DATE_1')]
    FieldDT1: TCustomDataTime;

    [SOName('DATE_2')]
    //--[SOTypeMap<TDateTime>] // DX ERROR: Failed call attribute constructor
    [SOType(TypeInfo(TDateTime))]
    FieldDT2: TCustomDataTime2;

    [SOName('BOOL')]
    FieldBool: TCustomBoolean;

    [SOName('SET')]
    FieldSet: TComponentState;

    [SOName('ENUM')]
    FieldEnum: TSeekOrigin;

    [SOIgnore]
    UnusedField: string;
  end;//*)

  TCustomRecord = record
    FieldDT1: TCustomDataTime;
  end;

{ TForm1 }

procedure TForm1.BMarshalClick(Sender: TObject);
begin
  MMarshal.Text := '';

  case CType.ItemIndex of
    0: // TCustomDateTime
      marshal_DateTime(); // Succsessfull!

    1: // TCustomRecord
      marshal_CustomRecord(); // TODO: ERROR: ... ?GPF in TDictionary

  end; // case
end;

procedure TForm1.BUnMarshalClick(Sender: TObject);
begin
  MUnMarshal.Text := '';

  case CType.ItemIndex of
    0: // TCustomDateTime
      un_marshal_DateTime(); // Succsessfull!

    1: // TCustomRecord
      un_marshal_CustomRecord(); // TODO: ERROR: ...

  end; // case
end;

procedure TForm1.marshal_DateTime();
var
  {%H-}D: TDateTime;
  S: string;
begin
  D := Now();
  S := JsonSerializer.Marshal<TDateTime>(D);
  MMarshal.Text := S;
end;

procedure TForm1.un_marshal_DateTime();
var
  D: TDateTime;
  S: string;
begin
  S := Trim(MMarshal.Text);

  D := 0;
  JsonSerializer.Unmarshal<TDateTime>(S, D);

  S := string(DelphiDateToISO8601(D));

  S := 'date: "'+S+'"';

  MUnMarshal.Text := S;
end;

procedure TForm1.marshal_CustomRecord();
var
  D: TCustomRecord;
  S: string;
begin
  // Fill record
  D.FieldDT1 := Now();

  S := JsonSerializer.Marshal<TCustomRecord>(D);

  MMarshal.Text := S;
end;

procedure TForm1.un_marshal_CustomRecord();
var
  D: TCustomRecord;
  S: string;
begin
  S := Trim(MMarshal.Text);

  D.FieldDT1 := 0;
  JsonSerializer.Unmarshal<TCustomRecord>(S, D);

  S := '{'#13#10
//    +'  "SET"     :"' + SuperSetToString(PTypeInfo(TypeInfo(TComponentState)), D.FieldSet, true) + '",'#13#10
//    +'  "DATE_2"  :"' + DelphiDateToISO8601(D.FieldDT2) + '",'#13#10
    +'  "DATE_1"  :"' + string(DelphiDateToISO8601(D.FieldDT1)) + '",'#13#10
//    +'  "ENUM"    :"' + {TypInfo.pas}GetEnumName(PTypeInfo(TypeInfo(TSeekOrigin)), Integer(D.FieldEnum)) + '",'#13#10
//    +'  "BOOL"    :"' + CustomBooleanValues[D.FieldBool] + '"'#13#10
  +'}';//}

  MUnMarshal.Text := S;
end;


initialization
  {$warnings off}
  if False then
  begin
    //uMarshalSuper.TSerialContext.InitializeGlobal; // optional

    // Sample: RWSynchronize - multithreaded protected access to rtti object when marshaling
    //SuperRttiContextDefault.RWSynchronize := True;

    // Map TCustomDataTime2 into TDateTime
    {$if declared(TCustomDataTime2)}
    SuperRttiContextDefault.ForceTypeMap := True; // Allow use attributes +[SOType(TypeInfo(T))] or -[SOTypeMap<T>]
    {$ifend}

    // Map TCustomDataTime to TDateTime
    {$if declared(TCustomDataTime)}
    SuperRegisterCustomTypeInfo(TypeInfo(TCustomDataTime), TypeInfo(TDateTime));
    {$ifend}

    // Map TCustomBoolean into Boolean
    {$if declared(TCustomBoolean)}
    if False then
    begin
      SuperRegisterCustomTypeInfo(TypeInfo(TCustomBoolean), TypeInfo(Boolean));

      // Sample: Customize marshaling TCustomBoolean
      SuperRttiContextDefault.SerialFromJson.AddOrSetValue(TypeInfo(TCustomBoolean), UnMarshalCustomBool);
      SuperRttiContextDefault.SerialToJson.AddOrSetValue(TypeInfo(TCustomBoolean), MarshalCustomBool);
    end;
    {$ifend}
  end;
end.
