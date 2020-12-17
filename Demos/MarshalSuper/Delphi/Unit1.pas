unit Unit1;

interface

uses
  // windows
  Winapi.Windows, Winapi.Messages,
  // system
  Types, SysUtils, Variants, System.Classes,
  DateUtils,
  // superobject
  superobject, superdate, uMarshalSuper,
  // VCL
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    BMarshal: TButton;
    BUnMarshal: TButton;
    MMarshal: TMemo;
    MUnMarshal: TMemo;
    procedure BMarshalClick(Sender: TObject);
    procedure BUnMarshalClick(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  supertypes, Rtti, TypInfo;

type
  TCustomDataTime = type TDateTime;
  TCustomDataTime2 = type TDateTime;
  TCustomBoolean = type Boolean; // marshaled into texts CustomBooleanValues

  TCustomRecord = record
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
  end;

const
  CustomBooleanValues: array[TCustomBoolean] of SOString = ('-', '+');

function MarshalCustomBool({%H-}ctx: TSuperRttiContext;
  var Value: TValue; const {%H-}Index: ISuperObject): ISuperObject;
var
  LValue: Boolean;
begin
  LValue := TValueData(value).FAsSLong <> 0;
  if LValue or (ctx = nil) or ctx.ForceDefault then
    Result := TSuperObject.Create( CustomBooleanValues[LValue] )
  else
    Result := nil;
end;

function UnMarshalCustomBool({%H-}ctx: TSuperRttiContext;
  const obj: ISuperObject; var Value: TValue): Boolean;
var
  o: ISuperObject;
  S: string;
begin
  if obj = nil then
  begin
    TValueData(Value).FAsSLong := Ord(False);
    Result := True;
    Exit;
  end;
  case ObjectGetType(obj) of
  stBoolean:
    begin
      TValueData(Value).FAsSLong := obj.AsInteger;
      Result := True;
    end;
  stInt:
    begin
      TValueData(Value).FAsSLong := ord(obj.AsInteger <> 0);
      Result := True;
    end;
  stString:
    begin
      Result := False;
      o := SO(obj.AsString);
      if not ObjectIsType(o, stString) then
        Result := UnMarshalCustomBool(ctx, SO(obj.AsString), Value) else
      begin
        S := string(obj.AsString);
        if (S <> '') then begin
          if //(S = '1') or (S = '-1') or SameText(S, 'true') or
            SameText(S, '+') then
          begin
            TValueData(Value).FAsSLong := Ord(True);
            Result := True;
          end
          else if //(S = '0') or SameText(S, 'false') or
            SameText(S, '-') then
          begin
            TValueData(Value).FAsSLong := Ord(False);
            Result := True;
          end;
        end;
      end;
    end;
  else
    Result := False;
  end; // case
end; // function UnMarshalCustomBool

{ TForm1 }

procedure TForm1.BMarshalClick(Sender: TObject);
var
  S: string;
  //D: TDateTime;
  D: TCustomRecord;
begin
  //D := Now();
  //S := JsonSerializer.Marshal<TDateTime>(D);

  // Fill TCustomRecord
  D.FieldDT1 := Now();
  D.FieldDT2 := D.FieldDT1-1;
  D.FieldBool := True;
  D.FieldSet := [csDesigning, csInline];
  D.FieldEnum := soEnd;
  D.UnusedField := 'Unused';

  S := JsonSerializer.Marshal<TCustomRecord>(D);

  MMarshal.Text := S;
end;

procedure TForm1.BUnMarshalClick(Sender: TObject);
var
  S: string;
  //D: TDateTime;
  D: TCustomRecord;
begin
  S := Trim(MMarshal.Text);

  //JsonSerializer.Unmarshal<TDateTime>(S, D);

  D.FieldDT1 := 0;
  D.FieldDT2 := 0;
  D.FieldBool := False;
  D.FieldSet := [];
  D.FieldEnum := soCurrent;
  D.UnusedField := 'Unused';
  JsonSerializer.Unmarshal<TCustomRecord>(S, D);

  //S := DelphiDateToISO8601(D);
  //S := 'date: "'+S+'"';

  S := '{'#13#10
    +'  "SET"     :"' + SuperSetToString(PTypeInfo(TypeInfo(TComponentState)), D.FieldSet, true) + '",'#13#10
    +'  "DATE_2"  :"' + DelphiDateToISO8601(D.FieldDT2) + '",'#13#10
    +'  "DATE_1"  :"' + DelphiDateToISO8601(D.FieldDT1) + '",'#13#10
    +'  "ENUM"    :"' + {TypInfo.pas}GetEnumName(PTypeInfo(TypeInfo(TSeekOrigin)), Integer(D.FieldEnum)) + '",'#13#10
    +'  "BOOL"    :"' + CustomBooleanValues[D.FieldBool] + '"'#13#10
  +'}';//}

  //ShowMessage(S);
  MUnMarshal.Text := S;
end;

initialization
  // Sample: RWSynchronize - multithreaded protected access to rtti object when marshaling
  //SuperRttiContextDefault.RWSynchronize := True;

  // Map TCustomDataTime2 into TDateTime
  SuperRttiContextDefault.ForceTypeMap := True; // Allow use attributes +[SOType(TypeInfo(T))] or -[SOTypeMap<T>]

  // Map TCustomDataTime to TDateTime
  SuperRegisterCustomTypeInfo(TypeInfo(TCustomDataTime), TypeInfo(TDateTime));

  // Map TCustomBoolean into Boolean
  SuperRegisterCustomTypeInfo(TypeInfo(TCustomBoolean), TypeInfo(Boolean));

  // Sample: Customize marshaling TCustomBoolean
  SuperRttiContextDefault.SerialFromJson.AddOrSetValue(TypeInfo(TCustomBoolean), UnMarshalCustomBool);
  SuperRttiContextDefault.SerialToJson.AddOrSetValue(TypeInfo(TCustomBoolean), MarshalCustomBool);
end.
