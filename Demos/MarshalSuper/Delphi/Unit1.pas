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

uses
  supertypes, Rtti;

type
  TCustomDataTime = type TDateTime;
  TCustomDataTime2 = type TDateTime;
  TCustomBoolean = type Boolean; // marshaled into texts CustomBooleanValues

  TCustomRecord = record
    [SOName('DATE_1')]
    DataTimeField1: TCustomDataTime;

    [SOName('DATE_2')]
    //--[SOTypeMap<TDateTime>] // DX ERROR: Failed call attribute constructor
    [SOType(TypeInfo(TDateTime))]
    DataTimeField2: TCustomDataTime2;

    [SOName('BOOL')]
    BooleanField: TCustomBoolean;

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
  D.DataTimeField1 := Now();
  D.DataTimeField2 := D.DataTimeField1-1;
  D.BooleanField := True;
  D.UnusedField := 'Unused';

  S := JsonSerializer.Marshal<TCustomRecord>(D);

  Memo1.Text := S;
end;

procedure TForm1.BUnMarshalClick(Sender: TObject);
var
  S: string;
  //D: TDateTime;
  D: TCustomRecord;
begin
  S := Trim(Memo1.Text);

  //JsonSerializer.Unmarshal<TDateTime>(S, D);

  D.DataTimeField1 := 0;
  D.DataTimeField2 := 0;
  D.BooleanField := False;
  D.UnusedField := 'Unused';
  JsonSerializer.Unmarshal<TCustomRecord>(S, D);

  //S := DelphiDateToISO8601(D);
  //S := 'date: "'+S+'"';

  S := '{'#13#10
    +'  "DataTimeField2": "' + DelphiDateToISO8601(D.DataTimeField2) + '",'#13#10
    +'  "DataTimeField1": "' + DelphiDateToISO8601(D.DataTimeField1) + '",'#13#10
    +'  "BooleanField":   "' + CustomBooleanValues[D.BooleanField] + '"'#13#10
  +'}';//}

  ShowMessage(S);
end;

initialization
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
