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
    BMarshalUnMarshal: TButton;
    procedure BMarshalClick(Sender: TObject);
    procedure BUnMarshalClick(Sender: TObject);
    procedure BMarshalUnMarshalClick(Sender: TObject);
  private
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

uses
  supertypes, Rtti, TypInfo{, superdbg};

type
  TCustomDataTime = type TDateTime;
  TCustomDataTime2 = type TDateTime;
  TCustomBoolean = type Boolean; // marshaled into texts CustomBooleanValues

  TRecordObject = class;

{$IF CompilerVersion >= 34.00} // Delphi 2020 Up (RAD XE10.4) Sydney
  // http://docwiki.embarcadero.com/RADStudio/Sydney/en/Custom_Managed_Records
  {$DEFINE _Managed_Records_}
{$IFEND} // {$IF CompilerVersion >= 34.00}

{$IFDEF _Managed_Records_}
  {$DEFINE _TCustomRecord_Manage} { optional }
{$ENDIF _Managed_Records_}

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

    RO: TRecordObject;

    {$IFDEF _TCustomRecord_Manage}
    class operator Initialize(out Dest: TCustomRecord);
    class operator Finalize(var Dest: TCustomRecord);
    class operator Assign(var Dest: TCustomRecord;	const [ref] Src: TCustomRecord);
    {$ENDIF _TCustomRecord_Manage}
  end;

 TRecordObject = class(TObject)
 private
   FP1 : Integer;
 public
   property P1 : Integer read FP1 write FP1;

 public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
 end;

{$IFDEF _TCustomRecord_Manage}
class operator TCustomRecord.Initialize(out Dest: TCustomRecord);
begin
  dbg('TCustomRecord.Initialize');
  FillChar(Dest, SizeOf(Dest), 0);
  //--D.RO := TRecordObject.Create();
end;
class operator TCustomRecord.Finalize(var Dest: TCustomRecord);
begin
  dbg('TCustomRecord.Finalize');
  FreeAndNil(Dest.RO);
end;
class operator TCustomRecord.Assign(var Dest: TCustomRecord;	const [ref] Src: TCustomRecord);
begin
  Dest.FieldDT1 := Src.FieldDT1;
  Dest.FieldDT2 := Src.FieldDT2;
  Dest.FieldBool := Src.FieldBool;
  Dest.FieldSet := Src.FieldSet;
  Dest.FieldEnum := Src.FieldEnum;
  Dest.UnusedField := Src.UnusedField;

  //
  // Manage RO object field: copy sub fields (not reference)
  //
  if Assigned(Src.RO) then begin
    if Dest.RO = nil then
      Dest.RO := TRecordObject.Create;
    Dest.RO.FP1 := Src.RO.FP1;
  end
  else
    FreeAndNil(Dest.RO);
end;
{$ENDIF _TCustomRecord_Manage}

var
  iTRecordObject_Count: Integer; // simple reference counter for detecting/testing leaks ...
procedure TRecordObject.AfterConstruction;
begin
  inc(iTRecordObject_Count);
  dbg('TRecordObject.AfterConstruction(C:'+inttostr(iTRecordObject_Count)+')');
  inherited;
end;
procedure TRecordObject.BeforeDestruction;
begin
  dec(iTRecordObject_Count);
  dbg('TRecordObject.BeforeDestruction(C:'+inttostr(iTRecordObject_Count)+')');
  inherited;
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


function SampleMarshalCustomRecord(out D: TCustomRecord): string;
begin
  // Fill TCustomRecord
  D.FieldDT1 := Now();
  D.FieldDT2 := D.FieldDT1-1;
  D.FieldBool := True;
  D.FieldSet := [csDesigning, csInline];
  D.FieldEnum := soEnd;
  D.UnusedField := 'Unused';

  {$IFDEF _TCustomRecord_Manage}
  if D.RO = nil then
  {$ENDIF _TCustomRecord_Manage}
  D.RO := TRecordObject.Create();

  D.RO.P1 := 17;

  Result := JsonSerializer.Marshal<TCustomRecord>(D);
end;

function SampleUnMarshalCustomRecord(const S: string; var D: TCustomRecord): string;
begin
  {$IFNDEF _TCustomRecord_Manage}
  FillChar(D, SizeOf(D), 0);
  {$ENDIF _TCustomRecord_Manage}

  // fill fields to other values
  D.FieldDT1 := 0;
  D.FieldDT2 := 0;
  D.FieldBool := False;
  D.FieldSet := [];
  D.FieldEnum := soCurrent;
  D.UnusedField := 'Unused';
  if Assigned(D.RO) then
    D.RO.P1 := 17;
  JsonSerializer.Unmarshal<TCustomRecord>(Trim(S), D);

  Result := '{'#13#10
    +'  "SET"     :"' + SuperSetToString(PTypeInfo(TypeInfo(TComponentState)), D.FieldSet, true) + '",'#13#10
    +'  "DATE_2"  :"' + DelphiDateToISO8601(D.FieldDT2) + '",'#13#10
    +'  "DATE_1"  :"' + DelphiDateToISO8601(D.FieldDT1) + '",'#13#10
    +'  "ENUM"    :"' + {TypInfo.pas}GetEnumName(PTypeInfo(TypeInfo(TSeekOrigin)), Integer(D.FieldEnum)) + '",'#13#10
    +'  "BOOL"    :"' + CustomBooleanValues[D.FieldBool] + '"'#13#10
  +'}';//}

end;

{ TForm1 }

procedure TForm1.BMarshalClick(Sender: TObject);
var
  S: string;
  //D: TDateTime;
  D: TCustomRecord;
begin
  //D := Now();
  //S := JsonSerializer.Marshal<TDateTime>(D);
  //MMarshal.Text := S;

  S := SampleMarshalCustomRecord(D);
  MMarshal.Text := S;
  {$IFNDEF _TCustomRecord_Manage}
  FreeAndNil(D.RO);
  {$ENDIF _TCustomRecord_Manage}
end;

procedure TForm1.BMarshalUnMarshalClick(Sender: TObject);
var
  S: string;
  D: TCustomRecord;
begin
  S := SampleMarshalCustomRecord(D);
  MMarshal.Text := S;
  {$IFNDEF _TCustomRecord_Manage}
  FreeAndNil(D.RO); // !!! NB: exclude leak for non managed records
  {$ENDIF _TCustomRecord_Manage}

  Finalize(D);

  S := SampleUnMarshalCustomRecord(S, D);
  MUnMarshal.Text := S;

  {$IFNDEF _TCustomRecord_Manage}
  FreeAndNil(D.RO);
  {$ENDIF _TCustomRecord_Manage}
end;

procedure TForm1.BUnMarshalClick(Sender: TObject);
var
  S: string;
  //D: TDateTime;
  D: TCustomRecord;
begin
  MUnMarshal.Text := '';
  S := Trim(MMarshal.Text);
  //JsonSerializer.Unmarshal<TDateTime>(S, D);
  //S := DelphiDateToISO8601(D);
  //S := 'date: "'+S+'"';

  {$IFNDEF _TCustomRecord_Manage}
  FillChar(D, SizeOf(D), 0);
  {$ENDIF _TCustomRecord_Manage}

  S := SampleUnMarshalCustomRecord(S, D);

  {$IFNDEF _TCustomRecord_Manage}
  FreeAndNil(D.RO);
  {$ENDIF _TCustomRecord_Manage}

  //ShowMessage(S);
  MUnMarshal.Text := S;
end;

initialization
  {$warnings off}
  //if False then
  begin
    //uMarshalSuper.TSerialContext.InitializeGlobal; // optional

    // Sample: RWSynchronize - multithreaded protected access to rtti object when marshaling
    //SuperRttiContextDefault.RWSynchronize := True;

    // Map TCustomDataTime2 into TDateTime
    SuperRttiContextDefault.ForceTypeMap := True; // Allow use attributes +[SOType(TypeInfo(T))] or -[SOTypeMap<T>]

    // Map TCustomDataTime to TDateTime
    SuperRegisterCustomTypeInfo(TypeInfo(TCustomDataTime), TypeInfo(TDateTime));

    //if False then
    begin
      // Map TCustomBoolean into Boolean
      SuperRegisterCustomTypeInfo(TypeInfo(TCustomBoolean), TypeInfo(Boolean));

      // Sample: Customize marshaling TCustomBoolean
      SuperRttiContextDefault.SerialFromJson.AddOrSetValue(TypeInfo(TCustomBoolean), UnMarshalCustomBool);
      SuperRttiContextDefault.SerialToJson.AddOrSetValue(TypeInfo(TCustomBoolean), MarshalCustomBool);
    end;
  end;
finalization
  dbg('TRecordObject leak count: '+inttostr(iTRecordObject_Count));
end.
