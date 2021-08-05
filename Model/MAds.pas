unit MAds;

interface

uses
  System.JSON,System.SysUtils,REST.Client,REST.Types,
  ModelHelper,ValidateHelper,System.Classes,System.Math;

type
  TMAds = Class

  private
    FId : String;
    FTitle : WideString;
    FSlug : String;
    FPosition : String;
    FLink : WideString;
    FType : String;
    FAdsstart : String;
    FAdsend : String;
    FAdsmedia : WideString;
    FAdsmediaStatus : WideString;
    FVersion,FAdsmediaVersion : Integer;
    FAdsmediaId : WideString;
    ContentfulUrl : String;
    CmaToken: String;
    SpaceId: String;
    Environments : String;
    ModelHelperObject : TModelHelper;
    FAcLog : WideString;
    FClick,FView,FRatio : Integer;
  public
    constructor Create(pCmaToken,pSpaceId,pEnvironments:String);
    property Id: String read FId write FId;
    property Version: Integer read FVersion write FVersion;
    property AdsmediaVersion: Integer read FAdsmediaVersion write FAdsmediaVersion;
    property Title: WideString read FTitle write FTitle;
    property Slug: String read FSlug write FSlug;
    property Adsstart: String read FAdsstart write FAdsstart;
    property Adsend: String read FAdsend write FAdsend;
    property Position: String read FPosition write FPosition;
    property Link: WideString read FLink write FLink;
    property AType: String read FType write FType;
    property Adsmedia: WideString read FAdsmedia write FAdsmedia;
    property AdsmediaId : WideString read FAdsmediaId write FAdsmediaId;
    property AdsmediaStatus : WideString read FAdsmediaStatus write FAdsmediaStatus;
    property AcLog : WideString read FAcLog write FAcLog;
    property AClick : Integer read FClick write FClick;
    property AView : Integer read FView write FView;
    property Ratio : Integer read FRatio write FRatio;
    function GetFieldsJson:TJSONObject;
    function GetAll(Search:String;page,limit:Integer;SortValue,uId,uTitle,cStatus:String):TJSONObject;
    function GetAllDraft(page,limit:Integer;Search,SortValue:String):TJSONObject;
    function Save:TJSONObject;
    function Update:TJSONObject;
    function GetById(ContentId: string): TJSONObject;
    function CheckSlug(Value:String):TJSONObject;
    function Validate:TJSONObject;
    function Delete: TJSONObject;
    function Publish: TJSONObject;
    function Unpublish: TJSONObject;
    function Archive: TJSONObject;
    function Unarchive: TJSONObject;
    procedure UpdateSchedule(pContentId:String;pVersion:Integer;value:Boolean);
  End;

implementation

constructor TMAds.Create(pCmaToken,pSpaceId,pEnvironments:String);
begin
  ModelHelperObject := TModelHelper.Create;
  CmaToken := pCmaToken;
  SpaceId := pSpaceId;
  Environments := pEnvironments;
  ContentfulUrl := Concat('https://api.contentful.com/spaces/',SpaceId,'/environments/',Environments);
end;

function TMAds.Archive: TJSONObject;
begin

end;

function TMAds.CheckSlug(Value: String): TJSONObject;
begin
  var ResultRest := TJSONObject.Create;
  var CAClient := TRESTClient.Create(nil);
  var CARequest := TRESTRequest.Create(nil);
  CARequest.AddParameter('Authorization','Bearer '+CmaToken,pkHTTPHEADER,[poDoNotEncode]);
  CARequest.AddParameter('content_type','ads',pkGETorPOST,[poDoNotEncode]);
  CARequest.AddParameter('fields.slug',Value,pkGETorPOST,[poDoNotEncode]);
  // กำหนด url ของ api
  CAClient.BaseURL := Concat(ContentfulUrl,'/entries');
  // assige client ให้ request
  CARequest.Client := CAClient;
  // set parameter rmGET,rmPOST,rmPUT,rmDELETE
  CARequest.Method := rmGET;
  // เรียก api
  CARequest.Execute;
  if (CARequest.Response.StatusCode = 200) then
  begin
    var CheckResult := CARequest.Response.JSONValue as TJSONObject;
    if StrToInt(CheckResult.FindValue('total').Value) > 0 then
    begin
      ResultRest.AddPair('result',TJSONBool.Create(False));
      ResultRest.AddPair('message','Slug has been used.');
    end
    else
    begin
      ResultRest.AddPair('result',TJSONBool.Create(True));
    end;
  end
  else
  begin
    ResultRest.AddPair('result',TJSONBool.Create(False));
    ResultRest.AddPair('message',CARequest.Response.JSONText);
  end;
//  CAClient.Free;
//  CARequest.Free;
  Result := ResultRest;
end;

function TMAds.Delete: TJSONObject;
begin

end;

function TMAds.GetAll(Search: String; page, limit: Integer; SortValue, uId,
  uTitle, cStatus: String): TJSONObject;
begin
  var ResultRest := TJSONObject.Create;
  var CAClient := TRESTClient.Create(nil);
  var CARequest := TRESTRequest.Create(nil);
  var skip := (page - 1) * limit;
  CARequest.AddParameter('Authorization','Bearer '+CmaToken,pkHTTPHEADER,[poDoNotEncode]);
  CARequest.AddParameter('content_type','ads',pkGETorPOST,[poDoNotEncode]);
  CARequest.AddParameter('skip',skip.ToString,pkGETorPOST,[poDoNotEncode]);
  CARequest.AddParameter('limit',limit.ToString,pkGETorPOST,[poDoNotEncode]);
  if cStatus = 'Archive' then
  begin
    CARequest.AddParameter('sys.archivedVersion[exists]','true',pkGETorPOST,[poDoNotEncode]);
  end
  else
  begin
    if cStatus = 'All' then
    begin
      CARequest.AddParameter('sys.archivedVersion[exists]','false',pkGETorPOST,[poDoNotEncode]);
    end
    else
    begin
      if cStatus = 'Publish' then
      begin
        CARequest.AddParameter('sys.publishedVersion[exists]','true',pkGETorPOST,[poDoNotEncode]);
      end
      else
      begin
        //Draft
        CARequest.AddParameter('sys.publishedVersion[exists]','false',pkGETorPOST,[poDoNotEncode]);
        CARequest.AddParameter('sys.archivedVersion[exists]','false',pkGETorPOST,[poDoNotEncode]);
      end;
    end;
  end;
  CARequest.AddParameter('query',Trim(Search),pkGETorPOST,[poDoNotEncode]);
  CARequest.AddParameter('order',SortValue,pkGETorPOST,[poDoNotEncode]);
  if uTitle = 'reporter' then
  begin
    CARequest.AddParameter('sys.createdBy.sys.id',uId,pkGETorPOST,[poDoNotEncode]);
  end;
  // กำหนด url ของ api
  CAClient.BaseURL := Concat(ContentfulUrl,'/entries');
  // assige client ให้ request
  CARequest.Client := CAClient;
  // set parameter rmGET,rmPOST,rmPUT,rmDELETE
  CARequest.Method := rmGET;
//    Memo1.Lines.Add(BodyJson.ToJSON);
  // เรียก api
  CARequest.Execute;
  if (CARequest.Response.StatusCode = 200) then
  begin
    var resultObj := CARequest.Response.JSONValue as TJSONObject;
    var totalrecord := resultObj.FindValue('total').Value.ToInteger;
    var totalpage := System.Math.Ceil(totalrecord/limit);
    ResultRest.AddPair('status',TJSONBool.Create(True));
    ResultRest.AddPair('result',resultObj);
    ResultRest.AddPair('totalpage',totalpage.ToString);
  end
  else
  begin
    ResultRest.AddPair('status',TJSONBool.Create(False));
    ResultRest.AddPair('result',CARequest.Response.JSONText);
  end;
//  CAClient.Free;
//  CARequest.Free;
  Result := ResultRest;
end;

function TMAds.GetAllDraft(page, limit: Integer; Search,
  SortValue: String): TJSONObject;
begin

end;

function TMAds.GetById(ContentId: string): TJSONObject;
begin
  var ResultRest := TJSONObject.Create;
  var CAClient := TRESTClient.Create(nil);
  var CARequest := TRESTRequest.Create(nil);
  CARequest.AddParameter('Authorization','Bearer '+CmaToken,pkHTTPHEADER,[poDoNotEncode]);
//  CARequest.AddBody(ContentObj.GetFieldsJson.ToString, ctAPPLICATION_JSON);
  // New Item
  // กำหนด url ของ api
  CAClient.BaseURL := Concat(ContentfulUrl,'/entries/',ContentId,'/references?include=1');
  // assige client ให้ request
  CARequest.Client := CAClient;
  // set parameter rmGET,rmPOST,rmPUT,rmDELETE
  CARequest.Method := rmGET;
//    Memo1.Lines.Add(BodyJson.ToJSON);
  // เรียก api
  CARequest.Execute;
  if (CARequest.Response.StatusCode = 200) then
  begin
    var ResultJson :=  CARequest.Response.JSONValue as TJSONObject;
    var ItemObj := ResultJson.FindValue('items[0]') as TJSONObject;
    Version := StrToInt(ItemObj.FindValue('sys.version').Value);
    Id := ItemObj.FindValue('sys.id').Value;
    Title := ItemObj.FindValue('fields.title.en-US').Value;
    Slug := ItemObj.FindValue('fields.slug.en-US').Value;
    AType := ItemObj.FindValue('fields.type.en-US').Value;
    Link := ItemObj.FindValue('fields.link.en-US').Value;
    Position := ItemObj.FindValue('fields.position.en-US').Value;
    Adsstart := ItemObj.FindValue('fields.adsstart.en-US').Value;
    Adsend := ItemObj.FindValue('fields.adsend.en-US').Value;
//  view
    AView := 0;
    if ItemObj.FindValue('fields.view.en-US') <> nil then
    begin
      AView := ItemObj.FindValue('fields.view.en-US').Value.ToInteger;
    end;
//  click
    AClick := 0;
    if ItemObj.FindValue('fields.click.en-US') <> nil then
    begin
      AClick := ItemObj.FindValue('fields.click.en-US').Value.ToInteger;
    end;
//  ratio
    Ratio := 0;
    if ItemObj.FindValue('fields.ratio.en-US') <> nil then
    begin
      Ratio := ItemObj.FindValue('fields.ratio.en-US').Value.ToInteger;
    end;
    //Log
    var i := 0;
    if  ItemObj.FindValue('fields.log.en-US') <> nil then
    begin
      var LogArray := ItemObj.FindValue('fields.log.en-US.content') as TJSONArray;
      for i := 0 to LogArray.Count - 1 do
      begin
        var LogObject:=LogArray.Get(i) as TJSONObject;
        AcLog := Concat(AcLog,LogObject.FindValue('content[0].value').Value);
      end;
    end;
    var AssetArray := ResultJson.FindValue('includes.Asset') as TJSONArray;
    //Thumbnail
    if  ItemObj.FindValue('fields.adsmedia.en-US.sys.id') <> nil then
    begin
      AdsmediaId := ItemObj.FindValue('fields.adsmedia.en-US.sys.id').Value;
      AdsmediaStatus := 'Draft';
      for i := 0 to AssetArray.Count - 1 do
      begin
        var ImgObject:=AssetArray.Get(i) as TJSONObject;
        if ImgObject.FindValue('sys.id').Value = AdsmediaId then
        begin
          AdsmediaVersion := ImgObject.FindValue('sys.version').Value.ToInteger;
          Adsmedia := Concat('https:',ImgObject.FindValue('fields.file.en-US.url').Value);
          if ImgObject.FindValue('sys.publishedVersion') <> nil then AdsmediaStatus := 'Publish';
          break
        end;
      end;
    end;
    ResultRest.AddPair('status',TJSONBool.Create(True));
    ResultRest.AddPair('result',ItemObj as TJSONObject);
  end
  else
  begin
    ResultRest.AddPair('status',TJSONBool.Create(False));
    ResultRest.AddPair('result',CARequest.Response.JSONText);
  end;
  Result := ResultRest;
end;

function TMAds.GetFieldsJson: TJSONObject;
begin
  var ResultObject := TJSONObject.Create;
  var TagsLists := TJSONArray.Create;

  var FieldObject := TJSONObject.Create;
  FieldObject.AddPair('title',ModelHelperObject.BuildLanguageString('en',Title));
  FieldObject.AddPair('slug',ModelHelperObject.BuildLanguageString('en',Slug));
  FieldObject.AddPair('position',ModelHelperObject.BuildLanguageString('en',Position));
  FieldObject.AddPair('type',ModelHelperObject.BuildLanguageString('en',AType));
  FieldObject.AddPair('link',ModelHelperObject.BuildLanguageString('en',Link));
  FieldObject.AddPair('adsmedia',ModelHelperObject.BuildLanguageObject('en',ModelHelperObject.BuildLink('Asset',AdsmediaId)));
  FieldObject.AddPair('click',ModelHelperObject.BuildLanguageInt('en',AClick));
  FieldObject.AddPair('view',ModelHelperObject.BuildLanguageInt('en',AView));
  FieldObject.AddPair('ratio',ModelHelperObject.BuildLanguageInt('en',Ratio));
  FieldObject.AddPair('adsstart',ModelHelperObject.BuildLanguageString('en',Adsstart));
  FieldObject.AddPair('adsend',ModelHelperObject.BuildLanguageString('en',Adsend));
  FieldObject.AddPair('log',ModelHelperObject.BuildLanguageObject('en',ModelHelperObject.BuildRichText(AcLog)));
  ResultObject.AddPair('fields',FieldObject);
  Result:=ResultObject;
end;

function TMAds.Publish: TJSONObject;
begin
    var resultRest := TJSONObject.Create;
    var CPAClient := TRESTClient.Create(nil);
    var CPARequest := TRESTRequest.Create(nil);
    CPARequest.AddParameter('Authorization','Bearer '+CmaToken,pkHTTPHEADER,[poDoNotEncode]);
    CPARequest.AddParameter('X-Contentful-Version',Version.ToString,pkHTTPHEADER,[poDoNotEncode]);
    // กำหนด url ของ api
    CPAClient.BaseURL := Concat(ContentfulUrl,'/entries/',Id,'/published');
    // assige client ให้ request
    CPARequest.Client := CPAClient;
    // set parameter rmGET,rmPOST,rmPUT,rmDELETE
    CPARequest.Method := rmPUT;
    // เรียก api
    CPARequest.Execute;
    if CPARequest.Response.StatusCode = 200 then
    begin
      resultRest.AddPair('status',TJSONBool.Create(True));
      resultRest.AddPair('result',CPARequest.Response.JSONValue as TJSONObject);
    end
    else
    begin
      resultRest.AddPair('status',TJSONBool.Create(False));
      resultRest.AddPair('result',CPARequest.Response.JSONText);
    end;
    Result := resultRest;
end;

function TMAds.Save: TJSONObject;
begin
  AcLog := Concat('create,',FormatDateTime('dd/MM/yyyy HH:mm:ss',Now));
  var ResultRest := TJSONObject.Create;
  var CAClient := TRESTClient.Create(nil);
  var CARequest := TRESTRequest.Create(nil);
  CARequest.AddParameter('Authorization','Bearer '+CmaToken,pkHTTPHEADER,[poDoNotEncode]);
  CARequest.AddParameter('Content-Type','application/vnd.contentful.management.v1+json',pkHTTPHEADER,[poDoNotEncode]);
  CARequest.AddParameter('X-Contentful-Content-Type','ads',pkHTTPHEADER,[poDoNotEncode]);
  CARequest.AddBody(GetFieldsJson.ToString, ctAPPLICATION_JSON);
  // New Item
  // กำหนด url ของ api
  CAClient.BaseURL := Concat(ContentfulUrl,'/entries');
  // assige client ให้ request
  CARequest.Client := CAClient;
  // set parameter rmGET,rmPOST,rmPUT,rmDELETE
  CARequest.Method := rmPOST;
  // เรียก api
  CARequest.Execute;
  if (CARequest.Response.StatusCode = 201) then
  begin
    ResultRest.AddPair('status',TJSONBool.Create(True));
    ResultRest.AddPair('message','Create ads complete.');
    ResultRest.AddPair('result',CARequest.Response.JSONValue as TJSONObject);
  end
  else
  begin
    ResultRest.AddPair('status',TJSONBool.Create(False));
    ResultRest.AddPair('result',CARequest.Response.JSONText);
  end;
  Result:=ResultRest;
end;

function TMAds.Unarchive: TJSONObject;
begin

end;

function TMAds.Unpublish: TJSONObject;
begin
    var CPAClient := TRESTClient.Create(nil);
    var CPARequest := TRESTRequest.Create(nil);
    CPARequest.AddParameter('Authorization','Bearer '+CmaToken,pkHTTPHEADER,[poDoNotEncode]);
    CPARequest.AddParameter('X-Contentful-Version',Version.ToString,pkHTTPHEADER,[poDoNotEncode]);
    // กำหนด url ของ api
    CPAClient.BaseURL := Concat(ContentfulUrl,'/entries/',Id,'/published');
    // assige client ให้ request
    CPARequest.Client := CPAClient;
    // set parameter rmGET,rmPOST,rmPUT,rmDELETE
    CPARequest.Method := rmDELETE;
    // เรียก api
    CPARequest.Execute;
    var resultRest := TJSONObject.Create;
    if CPARequest.Response.StatusCode = 200 then
    begin
      resultRest.AddPair('status',TJSONBool.Create(True));
      resultRest.AddPair('result',CPARequest.Response.JSONValue as TJSONObject);
    end
    else
    begin
      resultRest.AddPair('status',TJSONBool.Create(False));
      resultRest.AddPair('result',CPARequest.Response.JSONText);
    end;
    Result := resultRest;
end;

function TMAds.Update: TJSONObject;
begin
  const
      sLineBreak = {$IFDEF MACOS64} AnsiChar(#10) {$ENDIF} {$IFDEF LINUX} AnsiChar(#10) {$ENDIF} {$IFDEF MSWINDOWS} AnsiString(#13#10) {$ENDIF};
  AcLog := Concat(AcLog,sLineBreak,'update,',FormatDateTime('dd/MM/yyyy HH:mm:ss',Now));
  var ResultRest := TJSONObject.Create;
  var CAClient := TRESTClient.Create(nil);
  var CARequest := TRESTRequest.Create(nil);
  CARequest.AddParameter('Authorization','Bearer '+CmaToken,pkHTTPHEADER,[poDoNotEncode]);
  CARequest.AddParameter('Content-Type','application/vnd.contentful.management.v1+json',pkHTTPHEADER,[poDoNotEncode]);
  CARequest.AddParameter('X-Contentful-Version',Version.ToString,pkHTTPHEADER,[poDoNotEncode]);
  CARequest.AddBody(GetFieldsJson.ToString, ctAPPLICATION_JSON);
  // New Item
  // กำหนด url ของ api
  CAClient.BaseURL := Concat(ContentfulUrl,'/entries/',id);
  // assige client ให้ request
  CARequest.Client := CAClient;
  // set parameter rmGET,rmPOST,rmPUT,rmDELETE
  CARequest.Method := rmPUT;
//    Memo1.Lines.Add(BodyJson.ToJSON);
  // เรียก api
  CARequest.Execute;
  if (CARequest.Response.StatusCode = 200) then
  begin
    ResultRest.AddPair('status',TJSONBool.Create(True));
    ResultRest.AddPair('message','Update content complete.');
    ResultRest.AddPair('result',CARequest.Response.JSONValue as TJSONObject);
//    ResultRest := CARequest.Response.JSONValue as TJSONObject;
  end
  else
  begin
    ResultRest.AddPair('status',TJSONBool.Create(False));
    ResultRest.AddPair('result',CARequest.Response.JSONText);
  end;
  Result:=ResultRest;
end;

procedure TMAds.UpdateSchedule(pContentId: String; pVersion: Integer;
  value: Boolean);
begin
    var bodyObj := TJSONObject.Create;
    bodyObj.AddPair('op','add');
    bodyObj.AddPair('path','/fields/schedule/en-US');
    bodyObj.AddPair('value',TJSONBool.Create(value));
    var bodyArray := TJSONArray.Create;
    bodyArray.Add(bodyObj);
    var CPAClient := TRESTClient.Create(nil);
    var CPARequest := TRESTRequest.Create(nil);
    CPARequest.AddParameter('Authorization','Bearer '+CmaToken,pkHTTPHEADER,[poDoNotEncode]);
    CPARequest.AddParameter('X-Contentful-Version',pVersion.ToString,pkHTTPHEADER,[poDoNotEncode]);
    CPARequest.AddParameter('Content-Type','application/json-patch+json',pkHTTPHEADER,[poDoNotEncode]);
    CPARequest.AddBody(bodyArray.ToString, ctAPPLICATION_JSON);

    // กำหนด url ของ api
    CPAClient.BaseURL := Concat(ContentfulUrl,'/entries/',pContentId);
    // assige client ให้ request
    CPARequest.Client := CPAClient;
    // set parameter rmGET,rmPOST,rmPUT,rmDELETE
    CPARequest.Method := rmPATCH;
    // เรียก api
    CPARequest.Execute;
    if (CPARequest.Response.StatusCode = 200) then
    begin
      var result := CPARequest.Response.JSONValue as TJSONObject;
      Version := result.FindValue('sys.version').Value.ToInteger;
    end;
end;

function TMAds.Validate: TJSONObject;
begin
var CheckResult := TJSONObject.Create;
var ErrorArray := TJSONArray.Create;
var CheckObject := CValidateHelper.Create;
Title:=Trim(Title);
Slug:=Trim(Slug);
var ResultTitle := CheckObject.CheckBlank('Title',Title);
if ResultTitle.FindValue('result').Value.ToBoolean = False then
begin
ErrorArray.Add(ResultTitle);
end;
var ResultSlug := CheckObject.CheckBlank('Slug',Slug);
if ResultSlug.FindValue('result').Value.ToBoolean = False then
begin
ErrorArray.Add(ResultSlug);
end;
//Thumbnail
var ResultAdsmedia := CheckObject.CheckBlank('Adsmedia',AdsmediaId);
if ResultAdsmedia.FindValue('result').Value.ToBoolean = False then
begin
ErrorArray.Add(ResultAdsmedia);
end;
if ErrorArray.Count = 0 then
begin
CheckResult.AddPair('result',TJSONBool.Create(True));
end
else
begin
CheckResult.AddPair('result',TJSONBool.Create(False));
CheckResult.AddPair('error',ErrorArray);
end;
Result:=CheckResult;
end;

end.
