unit ValidateHelper;

interface

uses
  System.JSON,System.Variants,System.RegularExpressions, System.SysUtils, System.Classes,
  Web.HTTPApp;

type
  CValidateHelper = Class

  private

  public
    constructor Create;
    function IfNullInt( Value : OleVariant ) : Integer;
    function IfNullString( Value : OleVariant ) : String;
    function IfNullDouble( Value : OleVariant ) : Double;
    function IfNullBoolean( Value : OleVariant ;DefaultValue : Boolean) : Boolean;
    function IfNullDateTime( Value : OleVariant ) : TDateTime;
    function FilterScript(Value: String) : String;
    function EmailPattern(Value: String) : Boolean;
    function StrengthPassword(Value: String) : Boolean;
    function FormatSlug(Value: String): String;
    function CheckBlank(FieldName,Value: String): TJSONObject;
    function CheckMinMax(FieldName: String;Value,MinValue,MaxValue:Integer): TJSONObject;
  End;

implementation

constructor CValidateHelper.Create();
begin

end;

function CValidateHelper.IfNullBoolean( Value : OleVariant ;DefaultValue : Boolean) : Boolean;
begin
  if Value = NULL then
    Result := DefaultValue
  else
    Result := Value;
end;

function CValidateHelper.IfNullString( Value : OleVariant ) : String;
begin
  if Value = NULL then
    Result := ''
  else
    Result := Value;
end;

function CValidateHelper.IfNullInt( Value : OleVariant ) : Integer;
begin
  if Value = NULL then
    Result := 0
  else
    Result := Value;
end;

function CValidateHelper.IfNullDouble( Value : OleVariant ) : Double;
begin
  if Value = NULL then
    Result := 0
  else
    Result := Value;
end;

function CValidateHelper.IfNullDateTime( Value : OleVariant ) : TDateTime;
begin
  if Value = NULL then
    Result := 0
  else
    Result := Value;
end;

function CValidateHelper.StrengthPassword(Value: string): Boolean;
var
  NewStr: String;
  Regex: TRegEx;
begin
  Regex := TRegEx.Create('(?=.*\d)(?=.*\w)(?=.*[!@#$&*])(?!.*[\s]).{8,}',[roIgnoreCase]);
  Result := Regex.IsMatch(Value)
end;

function CValidateHelper.EmailPattern(Value: string): Boolean;
var
  NewStr: String;
  Regex: TRegEx;
begin
  Regex := TRegEx.Create('[A-Z0-9._%+-]+\@[A-Z0-9.-]+\.[A-Z]{2,4}',[roIgnoreCase]);
  Result := Regex.IsMatch(Value)
end;

function CValidateHelper.FilterScript(Value: string): String;
var
  NewStr: String;
  Regex: TRegEx;
begin
  Regex := TRegEx.Create('<script[\s\S]*?>[\s\S]*?<\/script>',[roIgnoreCase]);
  NewStr := Regex.Replace(Value,'');
  Regex := TRegEx.Create('javascript[\s\S]',[roIgnoreCase]);
  NewStr := Regex.Replace(NewStr,'');
  NewStr := HTMLDecode(NewStr);
  Result := HTMLEncode(NewStr);
end;

function CValidateHelper.FormatSlug(Value: String): String;
var
  newName: String;
  Regex: TRegEx;
begin
  Regex := TRegEx.Create('[^a-zA-Z0-9_-]');
  newName := Regex.Replace(StringReplace(Value, '#', '',[rfReplaceAll, rfIgnoreCase]), '-');
  Result := LowerCase(newName);
end;

function CValidateHelper.CheckBlank(FieldName,Value: string): TJSONObject;
begin
  var ResultObj := TJSONObject.Create;
  if Trim(FilterScript(Value)).Length = 0 then
  begin
    ResultObj.AddPair('result',TJSONBool.Create(False));
    ResultObj.AddPair('message',Concat(FieldName,' is required'));
  end
  else
  begin
    ResultObj.AddPair('result',TJSONBool.Create(True));
  end;
  Result:=ResultObj;
end;

function CValidateHelper.CheckMinMax(FieldName: string; Value:Integer; MinValue: Integer; MaxValue: Integer): TJSONObject;
begin
  var ResultObj := TJSONObject.Create;
  //check min and max
  if (MinValue > 0) And (MaxValue > 0) then
  begin
    if (Value < MinValue) OR (Value > MaxValue) then
    begin
      ResultObj.AddPair('result',TJSONBool.Create(False));
      ResultObj.AddPair('message',Concat(FieldName,' value between ',MinValue.ToString,' and ',MaxValue.ToString));
    end
    else
    begin
      ResultObj.AddPair('result',TJSONBool.Create(True));
    end;
  end;
  //check min
  if (MinValue > 0) And (MaxValue = 0) then
  begin
    if (Value < MinValue) then
    begin
      ResultObj.AddPair('result',TJSONBool.Create(False));
      ResultObj.AddPair('message',Concat(FieldName,' value min ',MinValue.ToString));
    end
    else
    begin
      ResultObj.AddPair('result',TJSONBool.Create(True));
    end;
  end;
  //check max
  if (MinValue = 0) And (MaxValue > 0) then
  begin
    if (Value > MaxValue) then
    begin
      ResultObj.AddPair('result',TJSONBool.Create(False));
      ResultObj.AddPair('message',Concat(FieldName,' value max ',MaxValue.ToString));
    end
    else
    begin
      ResultObj.AddPair('result',TJSONBool.Create(True));
    end;
  end;
  Result:=ResultObj;
end;
end.
