unit ModelHelper;

interface

uses
  System.JSON,System.SysUtils;

type
  TModelHelper = Class

  private

  public
    constructor Create;
    function BuildLanguageString(Lang,Value:String):TJSONObject;
    function BuildLanguageBoolean(Lang:String;Value:Boolean):TJSONObject;
    function BuildLanguageInt(Lang:String;Value:Integer):TJSONObject;
    function BuildLanguageObject(Lang:String;Value:TJSONObject):TJSONObject;
    function BuildLanguageJsonArray(Lang:String;Value:TJSONArray):TJSONObject;
    function BuildLanguageJsonObject(Lang:String;Value:TJSONObject):TJSONObject;
    function BuildLink(LinkType,LinkId:String):TJSONObject;
    function BuildRichText(Value:String):TJSONObject;
    function BuildRichTextContent(Value:String):TJSONArray;
  End;

implementation

constructor TModelHelper.Create();
begin

end;

function TModelHelper.BuildLanguageString(Lang: string;Value: string): TJSONObject;
begin
  var ResultObject := TJSONObject.Create;
  if Lang = 'en' then ResultObject.AddPair('en-US',Value);
  Result:=ResultObject;
end;

function TModelHelper.BuildLanguageObject(Lang:String;Value:TJSONObject): TJSONObject;
begin
  var ResultObject := TJSONObject.Create;
  if Lang = 'en' then ResultObject.AddPair('en-US',Value);
  Result:=ResultObject;
end;

function TModelHelper.BuildLanguageBoolean(Lang: String;
  Value: Boolean): TJSONObject;
begin
  var ResultObject := TJSONObject.Create;
  if Lang = 'en' then ResultObject.AddPair('en-US',TJSONBool.Create(Value));
  Result:=ResultObject;
end;

function TModelHelper.BuildLanguageInt(Lang: String;
  Value: Integer): TJSONObject;
begin
  var ResultObject := TJSONObject.Create;
  if Lang = 'en' then ResultObject.AddPair('en-US',TJSONNumber.Create(Value));
  Result:=ResultObject;
end;

function TModelHelper.BuildLanguageJsonArray(Lang:String;Value:TJSONArray): TJSONObject;
begin
  var ResultObject := TJSONObject.Create;
  if Lang = 'en' then ResultObject.AddPair('en-US',Value);
  Result:=ResultObject;
end;

function TModelHelper.BuildLanguageJsonObject(Lang:String;Value:TJSONObject): TJSONObject;
begin
  var ResultObject := TJSONObject.Create;
  if Lang = 'en' then ResultObject.AddPair('en-US',Value);
  Result:=ResultObject;
end;

function TModelHelper.BuildLink(LinkType: string; LinkId: string): TJSONObject;
begin
  var SysObject := TJSONObject.Create;
  SysObject.AddPair('type','Link');
  SysObject.AddPair('linkType',LinkType);
  SysObject.AddPair('id',LinkId);
  var ResultObject := TJSONObject.Create;
  ResultObject.AddPair('sys',SysObject);
  Result:=ResultObject;
end;

function TModelHelper.BuildRichText(Value:String):TJSONObject;
begin
  var ResultObject := TJSONObject.Create;
  ResultObject.AddPair('data',TJSONObject.Create);
  ResultObject.AddPair('content',BuildRichTextContent(Value));
  ResultObject.AddPair('nodeType','document');
  Result:=ResultObject;
end;

function TModelHelper.BuildRichTextContent(Value:String):TJSONArray;
begin
  var ResultArray := TJSONArray.Create;
  var SubContentObject := TJSONObject.Create;
  SubContentObject.AddPair('nodeType','text');
  SubContentObject.AddPair('value',Value);
  SubContentObject.AddPair('marks',TJSONArray.Create);
  SubContentObject.AddPair('data',TJSONObject.Create);
  var SubContentArray := TJSONArray.Create;
  SubContentArray.Add(SubContentObject);
  var ContentObject := TJSONObject.Create;
  ContentObject.AddPair('data',TJSONObject.Create);
  ContentObject.AddPair('content',SubContentArray);
  ContentObject.AddPair('nodeType','paragraph');
  ResultArray.Add(ContentObject);
  Result := ResultArray;
end;

end.
