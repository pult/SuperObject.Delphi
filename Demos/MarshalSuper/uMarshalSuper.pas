{ uMarshalSuper.pas } // version: 2021.0116.1923
unit uMarshalSuper;

{$IFDEF FPC}
  {$mode delphi}
{$ENDIF}

interface

uses
  {%H-}Types, SysUtils, Classes, {%H-}Rtti,
  supertypes, superobject;

type
  EJsonSerializer = class(ESuperObject);
  JsonSerializer = record
  public
    class function Marshal<T>(const Data: T): string; overload; static;
    class function Unmarshal<T>(Text: string; out Data:T): Boolean; overload; static;
  end;

  { TSerialContext }

  TSerialContext = class(TSuperRttiContext)
  public
    class procedure InitializeGlobal;
    constructor Create; override;
  end;

implementation

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  TypInfo, DateUtils, superdate;

const
  NullDateTime = -328716;

function SerialToDateTime({%H-}ctx: TSuperRttiContext;
  var Value: TValue; const {%H-}Index: ISuperObject): ISuperObject;
var
  LDateTime: TDateTime;
  sDateTime: string;
begin
  Result := nil;
  if Value.IsEmpty then
    Exit;
  LDateTime := Value.AsExtended;
  sDateTime := FloatToStr(LDateTime);
  if (sDateTime <> FloatToStr(0))
    and (sDateTime <> FloatToStr(NullDateTime)) then
  begin
    Result := TSuperObject.Create( DelphiDateToISO8601(LDateTime) );
  end;
end;

function SerialFromDateTime({%H-}ctx: TSuperRttiContext;
  const obj: ISuperObject; var Value: TValue): Boolean;
var
  S: string;
  D: TDateTime;
begin
  if Assigned(obj) then
    S := Trim(string(obj.AsString));
  Result := S = '';
  if Result then begin
    Value := TValue.From<TDateTime>(NullDateTime);
  end else begin
    Result := TryISO8601ToDelphiDate(SOString(S), D);
    if Result then
      Value := TValue.From<TDateTime>(D);
  end;
end;

{ JsonSerializer }

class function JsonSerializer.Marshal<T>(const Data: T): string;
var
  ctxowned : Boolean;
  ctx      : TSuperRttiContext; //TSerialContext;
  obj      : ISuperObject;
  sError   : string;
begin
  Result := ''; sError := '';
  if @Data <> nil then begin
    ctxowned := False;
    ctx := SuperRttiContextDefault;
    if ctx = nil then begin
      ctx := TSerialContext.Create;
      ctxowned := True;
    end;
    try
      obj := ctx.AsJson<T>(Data);
      if Assigned(obj) then begin
        Result := string(obj.AsJSon( {indent:}True, {escape:}False ));
        //Result := string( ISuperObject( ctx.AsJson<T>(Data) ).AsJSon);
      end else begin
        sError := 'Failed json serialization (exists unhandled types)';
        {$IFDEF MSWINDOWS}
        OutputDebugString(PChar('ERROR: '+sError));
        {$ENDIF}
        //sError := '';
      end;
    except
      on e: Exception do
      begin
        sError := e.Message;
        {$IFDEF MSWINDOWS}
        OutputDebugString(PChar('EXCEPTION: '+e.ClassName+': '+sError));
        {$ENDIF}
        sError := #13#10+sError;
        //Result := ''; { optional }
      end;
    end;
    if ctxowned then
      ctx.Free;
    if (Result = '') then begin
      if (sError <> '') then
        raise EJsonSerializer.Create('JsonSerializer.Marshal: '+sError)
      else
        raise EJsonSerializer.Create('JsonSerializer.Marshal: Empty');
    end;
  end;
end;

class function JsonSerializer.Unmarshal<T>(Text: string; out Data: T): Boolean;
var
  ctxowned : Boolean;
  ctx      : TSuperRttiContext; //TSerialContext;
  sError   : string;
begin
  ctxowned := False;
  ctx := SuperRttiContextDefault;
  if ctx = nil then begin
    ctx := TSerialContext.Create;
    ctxowned := True;
  end;
  try
    Data   := ctx.AsType<T>(SO(SOString(Text)));
    Result := True;
  except
    on e: Exception do
    begin
      sError := e.Message;
      {$IFDEF MSWINDOWS}
      OutputDebugString(PChar('EXCEPTION: '+e.ClassName+': '+sError));
      {$ENDIF}
      sError := #13#10+sError;
      Result := False;
      Data   := Default(T);
    end;
  end;
  if ctxowned then
    ctx.Free;
  if (not Result) then
    raise EJsonSerializer.Create('JsonSerializer.Unmarshal:'+#13#10+Text{+sError});
end;

{ TSerialContext }

var
  sc_Initialized: Boolean;
class procedure TSerialContext.InitializeGlobal;
begin
  if sc_Initialized then
    Exit;
  sc_Initialized := True;

  {$if declared(SuperRttiContextClassDefault)} // optional: default json marshaling class
  SuperRttiContextClassDefault := TSerialContext;
  {$ifend}

  {$if declared(SuperRttiContextDefault)} // optional: default json global marshaling object
  SuperRttiContextDefault.Free;
  SuperRttiContextDefault := TSerialContext.Create;
  {$ifend}
end;

constructor TSerialContext.Create;
begin
  inherited;

  SuperDateTimeZoneHandling := sdzUTC;
  SuperDateFormatHandling := sdfISO; // OLD: sdfJava

  SerialFromJson.AddOrSetValue(TypeInfo(TDateTime), SerialFromDateTime);
  SerialToJson.AddOrSetValue(TypeInfo(TDateTime), SerialToDateTime);
end;

initialization
  //TSerialContext.InitializeGlobal; // optional
end.
