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
  TCustomBoolean = type Boolean; // marshaled into texts CustomBooleanValues

  TCustomRecord = record
    [SOName('D')]
    DataTimeField : TCustomDataTime;

    [SOName('B')]
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

  D.DataTimeField := Now();
  D.BooleanField := True;
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

  D.DataTimeField := 0;
  D.BooleanField := False;
  D.UnusedField := 'Unused';
  JsonSerializer.Unmarshal<TCustomRecord>(S, D);

  //-S := FormatDateTime('yyyy-mm-dd hh:nn:ss', D);
  //-S := FormatDateTime('yyyy-mm-dd hh:nn:ss', D.DataTimeField);

  //S := DelphiDateToISO8601(D);
  S := DelphiDateToISO8601(D.DataTimeField);

  //S := 'date: "'+S+'"';
  S := '{'#13#10
    +'  "DataTimeField": "' + S + '",'#13#10
    +'  "BooleanField":  "' + CustomBooleanValues[D.BooleanField] + '"'#13#10
  +'}';//}
  ShowMessage(S);
end;

initialization
  // Map TCustomDataTime into TDateTime
  SuperRegisterCustomTypeInfo(TypeInfo(TCustomDataTime), TypeInfo(TDateTime));
  // Map TCustomBoolean into Boolean
  SuperRegisterCustomTypeInfo(TypeInfo(TCustomBoolean), TypeInfo(Boolean));
  // Sample: Customize marshaling TCustomBoolean
  SuperRttiContextDefault.SerialFromJson.AddOrSetValue(TypeInfo(TCustomBoolean), UnMarshalCustomBool);
  SuperRttiContextDefault.SerialToJson.AddOrSetValue(TypeInfo(TCustomBoolean), MarshalCustomBool);
end.
