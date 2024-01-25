program so_issue_6; //# 2024.0125: https://github.com/pult/SuperObject.Delphi/issues/6
{$APPTYPE CONSOLE}
{-$R *.res}
{$hints off}
uses System.SysUtils, superobject;

const json_str: String =
  '{'#13#10
//+ '    "request_id": "47f314cc-4279-4ee7-bf47-df4256be124c",'#13#10
+ '    "result": {'#13#10
//+ '        "class_id": "ef9a343f-1c8d-4c9f-864b-4bce79ca4075",'#13#10
+ '        "code": 2036,'#13#10
+ '        "human_readable_message": "!human_readable_message",'#13#10
+ '        "message": "Error",'#13#10
+ '        "res": "TRANSACTION_IMPOSSIBLE_TYPE_ID_FOR_RETURN"'#13#10
+ '    }'#13#10
+ '    ,"data": { "ids": ["1",2] }'#13#10
//+ '    ,"rids": ["r3",4]'#13#10
+ '    ,"sids": ["s5",6]'#13#10
+ '}';
//*)
(*
'{' +
' "data": { ids: ["1", 2] }'+
'}';
//*)


Type
  {$M+} // save published rtti
  TCustomResponseModel = Class Abstract
  End;
  {$M-}

  {$M+} // save published rtti
  TResponseResult = Class
  Public
    res: String;
    code: Int64;
    message: String;

  //{
  Public
    [SOName('human_readable_message')] [SODefault('#Default')]
    humanReadableMessage: String;
    //#
    //# or published property humanReadableMessage :
    //#
  //}
  {
  Private
    f_humanReadableMessage: String;
  Published
    [SOName('human_readable_message')] [SODefault('#Default')]
    property humanReadableMessage: String read f_humanReadableMessage write f_humanReadableMessage;
  //}
  End;
  {$M-}

  TArray11i = array[0..1] of integer;
  TCardLabResponse = Class(TCustomResponseModel)
  Public
    result: TResponseResult;

    //#TESTS:
    [SODefault('[r7,7,7]')]
    rids: TArray<String>; //# test array

    //sids: array[0..1] of string;     //# +test array
    //--sids: array[0..1] of integer;  //# -test array: ERROR: D26 failed rtti info for integer type
    sids: TArray11i;                   //# +test array: Fix for up
    //#TESTS.
  End;

  TTransactionItem = Record
  Private
    mode: String;
    on_device_id: String;
    create_ts: Int64;
    state: String;
  Public
    id: TGUID;
    ext_id: Int64;
    ids: TArray<String>;
    coupons_for_print: TArray<String>;
  End;

  TCustomTransactionRs = Class(TCardLabResponse)
  Public
    data: TTransactionItem;
  End;

var
  ACTX: TSuperRttiContext;
  json: ISuperObject;
  obj: TCustomTransactionRs;
  data: TTransactionItem;
begin
  try
    ACTX := SuperRttiContextDefault;
    //# or
    //ACTX := TSuperRttiContext.Create;
    try
      //obj:= ACTX.AsTypeFromJson<TCustomTransactionRs>(json_str);
      json := SO(json_str);
      obj := ACTX.AsType<TCustomTransactionRs>(json);

      data := obj.data; //@dbg: (Evaluate/Modify window): obj.result,r  #  obj.data.ids
    finally
      if ACTX <> SuperRttiContextDefault then
        ACTX.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
