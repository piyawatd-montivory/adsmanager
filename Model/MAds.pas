unit MAds;

interface

uses
  System.JSON, System.SysUtils, REST.Client, REST.Types,
  ModelHelper, ValidateHelper, System.Classes, System.Math;

type
  TMAds = Class

  private
    FId: String;
    FTitle: WideString;
    FSlug: String;
    FPosition: String;
    FLink: WideString;
    FType: String;
    FAdsstart: String;
    FAdsend: String;
    FAdsmedia: WideString;
    FAdsmediaStatus: WideString;
    FVersion, FAdsmediaVersion: Integer;
    FAdsmediaId: WideString;
    ContentfulUrl: String;
    CmaToken: String;
    SpaceId: String;
    Environments: String;
    ModelHelperObject: TModelHelper;
    FAcLog: WideString;
    FClick, FView, FRatio: Integer;
  public
    constructor Create(pCmaToken, pSpaceId, pEnvironments: String);
    property Id: String read FId write FId;
    property Version: Integer read FVersion write FVersion;
    property AdsmediaVersion: Integer read FAdsmediaVersion
      write FAdsmediaVersion;
    property Title: WideString read FTitle write FTitle;
    property Slug: String read FSlug write FSlug;
    property Adsstart: String read FAdsstart write FAdsstart;
    property Adsend: String read FAdsend write FAdsend;
    property Position: String read FPosition write FPosition;
    property Link: WideString read FLink write FLink;
    property AType: String read FType write FType;
    property Adsmedia: WideString read FAdsmedia write FAdsmedia;
    property AdsmediaId: WideString read FAdsmediaId write FAdsmediaId;
    property AdsmediaStatus: WideString read FAdsmediaStatus
      write FAdsmediaStatus;
    property AcLog: WideString read FAcLog write FAcLog;
    property AClick: Integer read FClick write FClick;
    property AView: Integer read FView write FView;
    property Ratio: Integer read FRatio write FRatio;
    function GetFieldsJson: TJSONObject;
    function GetAll(pSearch,pPosition, pSortValue, pStatus: String;
      pPage, pLimit: Integer): TJSONObject;
    function GetList(pPosition,pId:String): TJSONObject;
    function Save: TJSONObject;
    function Update: TJSONObject;
    function GetById(pId: string): TJSONObject;
    function CheckSlug(pSlug,pId: String): TJSONObject;
    function Validate: TJSONObject;
    function Delete: Boolean;
    function Publish: Boolean;
    function Unpublish: Boolean;
    function Archive: Boolean;
    function Unarchive: Boolean;
    procedure DeleteSchedule;
    procedure UpdateSchedule(pContentId: String; pVersion: Integer;
      pValue: Boolean);
  End;

implementation

uses ContentfulSchedule;

constructor TMAds.Create(pCmaToken, pSpaceId, pEnvironments: String);
begin
  ModelHelperObject := TModelHelper.Create;
  CmaToken := pCmaToken;
  SpaceId := pSpaceId;
  Environments := pEnvironments;
  ContentfulUrl := Concat('https://api.contentful.com/spaces/', SpaceId,
    '/environments/', Environments);
end;

function TMAds.Archive: Boolean;
begin
  var pResult := False;
  var
  CPAClient := TRESTClient.Create(nil);
  var
  CPARequest := TRESTRequest.Create(nil);
  CPARequest.AddParameter('Authorization', 'Bearer ' + CmaToken, pkHTTPHEADER,
    [poDoNotEncode]);
  CPARequest.AddParameter('X-Contentful-Version', Version.ToString,
    pkHTTPHEADER, [poDoNotEncode]);
  // กำหนด url ของ api
  CPAClient.BaseURL := Concat(ContentfulUrl, '/entries/', Id, '/archived');
  // assige client ให้ request
  CPARequest.Client := CPAClient;
  // set parameter rmGET,rmPOST,rmPUT,rmDELETE
  CPARequest.Method := rmPUT;
  // เรียก api
  try
    CPARequest.Execute;
    if CPARequest.Response.StatusCode = 200 then
    begin
      pResult := True;
    end;
  finally
    CPAClient.Free;
  end;
Result := pResult;
end;

function TMAds.CheckSlug(pSlug,pId: String): TJSONObject;
begin
  var
  ResultRest := TJSONObject.Create;
  var
  CAClient := TRESTClient.Create(nil);
  var
  CARequest := TRESTRequest.Create(nil);
  CARequest.AddParameter('Authorization', 'Bearer ' + CmaToken, pkHTTPHEADER,
    [poDoNotEncode]);
  CARequest.AddParameter('content_type', 'ads', pkGETorPOST, [poDoNotEncode]);
  CARequest.AddParameter('fields.slug', pSlug, pkGETorPOST, [poDoNotEncode]);
  if pId <> '' then CARequest.AddParameter('sys.id[ne]', pId, pkGETorPOST, [poDoNotEncode]);
  // กำหนด url ของ api
  CAClient.BaseURL := Concat(ContentfulUrl, '/entries');
  // assige client ให้ request
  CARequest.Client := CAClient;
  // set parameter rmGET,rmPOST,rmPUT,rmDELETE
  CARequest.Method := rmGET;
  // เรียก api
  CARequest.Execute;
  if (CARequest.Response.StatusCode = 200) then
  begin
    var
    CheckResult := CARequest.Response.JSONValue as TJSONObject;
    if StrToInt(CheckResult.FindValue('total').value) > 0 then
    begin
      ResultRest.AddPair('result', TJSONBool.Create(False));
      ResultRest.AddPair('message', 'Slug has been used.');
    end
    else
    begin
      ResultRest.AddPair('result', TJSONBool.Create(True));
    end;
  end
  else
  begin
    ResultRest.AddPair('result', TJSONBool.Create(False));
    ResultRest.AddPair('message', CARequest.Response.JSONText);
  end;
  // CAClient.Free;
  // CARequest.Free;
  Result := ResultRest;
end;

function TMAds.Delete: Boolean;
begin

end;

procedure TMAds.DeleteSchedule;
begin
  // schedule ads
  // create contentful schedule object
  var
  ContentfulSchedule := TContentfulSchedule.Create(CmaToken, SpaceId,
    Environments);
  // clean all schedule
  var
  ScheduleList := ContentfulSchedule.GetAll(Id);
  var
  i := 0;
  for i := 0 to ScheduleList.Count - 1 do
  begin
    var
    ItemObject := TJSONObject(ScheduleList.Get(i));
    var
    delResult := ContentfulSchedule.ScheduleCancel(Id,
      ItemObject.FindValue('sys.id').Value);
  end;
  ContentfulSchedule.Free;
end;

function TMAds.GetList(pPosition,pId:String): TJSONObject;
begin
  var
  ResultRest := TJSONObject.Create;
  var
  CAClient := TRESTClient.Create(nil);
  var
  CARequest := TRESTRequest.Create(nil);
  CARequest.AddParameter('Authorization', 'Bearer ' + CmaToken, pkHTTPHEADER,
    [poDoNotEncode]);
  CARequest.AddParameter('content_type', 'ads', pkGETorPOST, [poDoNotEncode]);
  CARequest.AddParameter('limit', '200', pkGETorPOST,
    [poDoNotEncode]);
  CARequest.AddParameter('sys.archivedVersion[exists]', 'false', pkGETorPOST,
      [poDoNotEncode]);
  CARequest.AddParameter('order', '-fields.adsstart', pkGETorPOST, [poDoNotEncode]);
  if pId <> '' then CARequest.AddParameter('sys.id[ne]', pId, pkGETorPOST, [poDoNotEncode]);
  CARequest.AddParameter('fields.position.en-US', pPosition, pkGETorPOST, [poDoNotEncode]);
  // กำหนด url ของ api
  CAClient.BaseURL := Concat(ContentfulUrl, '/entries');
  // assige client ให้ request
  CARequest.Client := CAClient;
  // set parameter rmGET,rmPOST,rmPUT,rmDELETE
  CARequest.Method := rmGET;
  // Memo1.Lines.Add(BodyJson.ToJSON);
  // เรียก api
  try
    CARequest.Execute;
    if (CARequest.Response.StatusCode = 200) then
    begin
      var
      resultObj := CARequest.Response.JSONValue as TJSONObject;
      var
      totalrecord := resultObj.FindValue('total').value.ToInteger;
      ResultRest.AddPair('status', TJSONBool.Create(True));
      ResultRest.AddPair('result', resultObj);
    end
    else
    begin
      ResultRest.AddPair('status', TJSONBool.Create(False));
      ResultRest.AddPair('result', CARequest.Response.JSONText);
    end;
  finally
    CAClient.Free;
  end;
  Result := ResultRest;
end;

function TMAds.GetAll(pSearch, pPosition, pSortValue, pStatus: String;
  pPage, pLimit: Integer): TJSONObject;
begin
  var
  ResultRest := TJSONObject.Create;
  var
  CAClient := TRESTClient.Create(nil);
  var
  CARequest := TRESTRequest.Create(nil);
  var
  skip := (pPage - 1) * pLimit;
  CARequest.AddParameter('Authorization', 'Bearer ' + CmaToken, pkHTTPHEADER,
    [poDoNotEncode]);
  CARequest.AddParameter('content_type', 'ads', pkGETorPOST, [poDoNotEncode]);
  if pPosition <> 'all' then
  begin
    CARequest.AddParameter('fields.position.en-US', pPosition, pkGETorPOST, [poDoNotEncode]);
  end;
  CARequest.AddParameter('skip', skip.ToString, pkGETorPOST, [poDoNotEncode]);
  CARequest.AddParameter('limit', pLimit.ToString, pkGETorPOST,
    [poDoNotEncode]);
  if pStatus = 'Archive' then
  begin
    CARequest.AddParameter('sys.archivedVersion[exists]', 'true', pkGETorPOST,
      [poDoNotEncode]);
  end;
  if pStatus = 'Publish' then
  begin
    CARequest.AddParameter('sys.publishedVersion[exists]', 'true', pkGETorPOST,
      [poDoNotEncode]);
  end;
  if pStatus = 'Draft' then
  begin
    // Draft
    CARequest.AddParameter('sys.publishedVersion[exists]', 'false', pkGETorPOST,
      [poDoNotEncode]);
    CARequest.AddParameter('sys.archivedVersion[exists]', 'false', pkGETorPOST,
      [poDoNotEncode]);
  end;
  CARequest.AddParameter('query', Trim(pSearch), pkGETorPOST, [poDoNotEncode]);
  CARequest.AddParameter('order', pSortValue, pkGETorPOST, [poDoNotEncode]);
  // กำหนด url ของ api
  CAClient.BaseURL := Concat(ContentfulUrl, '/entries');
  // assige client ให้ request
  CARequest.Client := CAClient;
  // set parameter rmGET,rmPOST,rmPUT,rmDELETE
  CARequest.Method := rmGET;
  // Memo1.Lines.Add(BodyJson.ToJSON);
  // เรียก api
  try
    CARequest.Execute;
    if (CARequest.Response.StatusCode = 200) then
    begin
      var
      resultObj := CARequest.Response.JSONValue as TJSONObject;
      var
      totalrecord := resultObj.FindValue('total').value.ToInteger;
      var
      totalpage := System.Math.Ceil(totalrecord / pLimit);
      ResultRest.AddPair('status', TJSONBool.Create(True));
      ResultRest.AddPair('result', resultObj);
      ResultRest.AddPair('totalpage', totalpage.ToString);
    end
    else
    begin
      ResultRest.AddPair('status', TJSONBool.Create(False));
      ResultRest.AddPair('result', CARequest.Response.JSONText);
    end;
  finally
    CAClient.Free;
  end;
  Result := ResultRest;
end;

function TMAds.GetById(pId: string): TJSONObject;
begin
  var
  pResult := TJSONObject.Create;
  var
  CAClient := TRESTClient.Create(nil);
  var
  CARequest := TRESTRequest.Create(nil);
  CARequest.AddParameter('Authorization', 'Bearer ' + CmaToken, pkHTTPHEADER,
    [poDoNotEncode]);
  // New Item
  // กำหนด url ของ api
  CAClient.BaseURL := Concat(ContentfulUrl, '/entries/', pId,
    '/references?include=1');
  // assige client ให้ request
  CARequest.Client := CAClient;
  // set parameter rmGET,rmPOST,rmPUT,rmDELETE
  CARequest.Method := rmGET;
  // เรียก api
  try
    CARequest.Execute;
    if (CARequest.Response.StatusCode = 200) then
    begin
      var
      ResultJson := CARequest.Response.JSONValue as TJSONObject;
      var
      ItemObj := ResultJson.FindValue('items[0]') as TJSONObject;
      Version := StrToInt(ItemObj.FindValue('sys.version').value);
      Id := ItemObj.FindValue('sys.id').value;
      Title := ItemObj.FindValue('fields.title.en-US').value;
      Slug := ItemObj.FindValue('fields.slug.en-US').value;
      AType := ItemObj.FindValue('fields.type.en-US').value;
      Link := ItemObj.FindValue('fields.link.en-US').value;
      Position := ItemObj.FindValue('fields.position.en-US').value;
      Adsstart := ItemObj.FindValue('fields.adsstart.en-US').value;
      Adsend := ItemObj.FindValue('fields.adsend.en-US').value;
      // view
      AView := 0;
      if ItemObj.FindValue('fields.view.en-US') <> nil then
      begin
        AView := ItemObj.FindValue('fields.view.en-US').value.ToInteger;
      end;
      // click
      AClick := 0;
      if ItemObj.FindValue('fields.click.en-US') <> nil then
      begin
        AClick := ItemObj.FindValue('fields.click.en-US').value.ToInteger;
      end;
      // ratio
      Ratio := 0;
      if ItemObj.FindValue('fields.ratio.en-US') <> nil then
      begin
        Ratio := ItemObj.FindValue('fields.ratio.en-US').value.ToInteger;
      end;
      // Log
      var
      i := 0;
      if ItemObj.FindValue('fields.log.en-US') <> nil then
      begin
        var
        LogArray := ItemObj.FindValue('fields.log.en-US.content') as TJSONArray;
        for i := 0 to LogArray.Count - 1 do
        begin
          var
          LogObject := LogArray.Get(i) as TJSONObject;
          AcLog := Concat(AcLog, LogObject.FindValue('content[0].value').value);
        end;
      end;
      var
      AssetArray := ResultJson.FindValue('includes.Asset') as TJSONArray;
      // Thumbnail
      if ItemObj.FindValue('fields.adsmedia.en-US.sys.id') <> nil then
      begin
        AdsmediaId := ItemObj.FindValue('fields.adsmedia.en-US.sys.id').value;
        AdsmediaStatus := 'Draft';
        for i := 0 to AssetArray.Count - 1 do
        begin
          var
          ImgObject := AssetArray.Get(i) as TJSONObject;
          if ImgObject.FindValue('sys.id').value = AdsmediaId then
          begin
            AdsmediaVersion := ImgObject.FindValue('sys.version')
              .value.ToInteger;
            Adsmedia := Concat('https:',
              ImgObject.FindValue('fields.file.en-US.url').value);
            if ImgObject.FindValue('sys.publishedVersion') <> nil then
              AdsmediaStatus := 'Publish';
            break
          end;
        end;
      end;
      pResult.AddPair('status', TJSONBool.Create(True));
      pResult.AddPair('result', ItemObj as TJSONObject);
    end
    else
    begin
      pResult.AddPair('status', TJSONBool.Create(False));
      pResult.AddPair('result', CARequest.Response.JSONText);
    end;
  finally
    CAClient.Free;
  end;
  Result := pResult;
end;

function TMAds.GetFieldsJson: TJSONObject;
begin
  var
  ResultObject := TJSONObject.Create;
  var
  TagsLists := TJSONArray.Create;

  var
  FieldObject := TJSONObject.Create;
  FieldObject.AddPair('title', ModelHelperObject.BuildLanguageString
    ('en', Title));
  FieldObject.AddPair('slug', ModelHelperObject.BuildLanguageString
    ('en', Slug));
  FieldObject.AddPair('position', ModelHelperObject.BuildLanguageString('en',
    Position));
  FieldObject.AddPair('type', ModelHelperObject.BuildLanguageString
    ('en', AType));
  FieldObject.AddPair('link', ModelHelperObject.BuildLanguageString
    ('en', Link));
  FieldObject.AddPair('adsmedia', ModelHelperObject.BuildLanguageObject('en',
    ModelHelperObject.BuildLink('Asset', AdsmediaId)));
  FieldObject.AddPair('click', ModelHelperObject.BuildLanguageInt('en',
    AClick));
  FieldObject.AddPair('view', ModelHelperObject.BuildLanguageInt('en', AView));
  FieldObject.AddPair('ratio', ModelHelperObject.BuildLanguageInt('en', Ratio));
  FieldObject.AddPair('adsstart', ModelHelperObject.BuildLanguageString('en',
    Adsstart));
  FieldObject.AddPair('adsend', ModelHelperObject.BuildLanguageString
    ('en', Adsend));
  FieldObject.AddPair('log', ModelHelperObject.BuildLanguageObject('en',
    ModelHelperObject.BuildRichText(AcLog)));
  ResultObject.AddPair('fields', FieldObject);
  Result := ResultObject;
end;

function TMAds.Publish: Boolean;
begin
  var
  pResult := False;
  var
  CPAClient := TRESTClient.Create(nil);
  var
  CPARequest := TRESTRequest.Create(nil);
  CPARequest.AddParameter('Authorization', 'Bearer ' + CmaToken, pkHTTPHEADER,
    [poDoNotEncode]);
  CPARequest.AddParameter('X-Contentful-Version', Version.ToString,
    pkHTTPHEADER, [poDoNotEncode]);
  // กำหนด url ของ api
  CPAClient.BaseURL := Concat(ContentfulUrl, '/entries/', Id, '/published');
  // assige client ให้ request
  CPARequest.Client := CPAClient;
  // set parameter rmGET,rmPOST,rmPUT,rmDELETE
  CPARequest.Method := rmPUT;
  // เรียก api
  try
  CPARequest.Execute;
  if CPARequest.Response.StatusCode = 200 then
  begin
    pResult := True;
    var pResultObj := CPARequest.Response.JSONValue as TJSONObject;
    Version := pResultObj.FindValue('sys.version').Value.ToInteger;
  end;
  finally
    CPAClient.Free;
  end;
  Result := pResult;
end;

function TMAds.Save: TJSONObject;
begin
  AcLog := Concat('create,', FormatDateTime('dd/MM/yyyy HH:mm:ss', Now));
  var
  pResult := TJSONObject.Create;
  var
  CAClient := TRESTClient.Create(nil);
  var
  CARequest := TRESTRequest.Create(nil);
  CARequest.AddParameter('Authorization', 'Bearer ' + CmaToken, pkHTTPHEADER,
    [poDoNotEncode]);
  CARequest.AddParameter('Content-Type',
    'application/vnd.contentful.management.v1+json', pkHTTPHEADER,
    [poDoNotEncode]);
  CARequest.AddParameter('X-Contentful-Content-Type', 'ads', pkHTTPHEADER,
    [poDoNotEncode]);
  CARequest.AddBody(GetFieldsJson.ToString, ctAPPLICATION_JSON);
  // New Item
  // กำหนด url ของ api
  CAClient.BaseURL := Concat(ContentfulUrl, '/entries');
  // assige client ให้ request
  CARequest.Client := CAClient;
  // set parameter rmGET,rmPOST,rmPUT,rmDELETE
  CARequest.Method := rmPOST;
  // เรียก api
  try
  CARequest.Execute;
  if (CARequest.Response.StatusCode = 201) then
  begin
    var pResultObj := CARequest.Response.JSONValue as TJSONObject;
    Id := pResultObj.FindValue('sys.id').Value;
    Version := pResultObj.FindValue('sys.version').Value.ToInteger;
    pResult.AddPair('status', TJSONBool.Create(True));
    pResult.AddPair('message', 'Create ads complete.');
    pResult.AddPair('result', pResultObj);
  end
  else
  begin
    pResult.AddPair('status', TJSONBool.Create(False));
    pResult.AddPair('result', CARequest.Response.JSONText);
  end;
  finally
    CAClient.Free;
  end;
  Result := pResult;
end;

function TMAds.Unarchive: Boolean;
begin

end;

function TMAds.Unpublish: Boolean;
begin
  var pResult := False;
  var
  CPAClient := TRESTClient.Create(nil);
  var
  CPARequest := TRESTRequest.Create(nil);
  CPARequest.AddParameter('Authorization', 'Bearer ' + CmaToken, pkHTTPHEADER,
    [poDoNotEncode]);
  CPARequest.AddParameter('X-Contentful-Version', Version.ToString,
    pkHTTPHEADER, [poDoNotEncode]);
  // กำหนด url ของ api
  CPAClient.BaseURL := Concat(ContentfulUrl, '/entries/', Id, '/published');
  // assige client ให้ request
  CPARequest.Client := CPAClient;
  // set parameter rmGET,rmPOST,rmPUT,rmDELETE
  CPARequest.Method := rmDELETE;
  // เรียก api
  try
  CPARequest.Execute;
  if CPARequest.Response.StatusCode = 200 then
  begin
    var pResultObj := CPARequest.Response.JSONValue as TJSONObject;
    Version := pResultObj.FindValue('sys.version').Value.ToInteger;
    pResult := True;
  end;
  finally
    CPAClient.Free;
  end;
  Result := pResult;
end;

function TMAds.Update: TJSONObject;
begin
  const
    sLineBreak = {$IFDEF MACOS64} AnsiChar(#10) {$ENDIF} {$IFDEF LINUX} AnsiChar(#10) {$ENDIF} {$IFDEF MSWINDOWS} AnsiString(#13#10) {$ENDIF};
  AcLog := Concat(AcLog, sLineBreak, 'update,',
    FormatDateTime('dd/MM/yyyy HH:mm:ss', Now));
  var
  pResult := TJSONObject.Create;
  var
  CAClient := TRESTClient.Create(nil);
  var
  CARequest := TRESTRequest.Create(nil);
  CARequest.AddParameter('Authorization', 'Bearer ' + CmaToken, pkHTTPHEADER,
    [poDoNotEncode]);
  CARequest.AddParameter('Content-Type',
    'application/vnd.contentful.management.v1+json', pkHTTPHEADER,
    [poDoNotEncode]);
  CARequest.AddParameter('X-Contentful-Version', Version.ToString, pkHTTPHEADER,
    [poDoNotEncode]);
  CARequest.AddBody(GetFieldsJson.ToString, ctAPPLICATION_JSON);
  // New Item
  // กำหนด url ของ api
  CAClient.BaseURL := Concat(ContentfulUrl, '/entries/', Id);
  // assige client ให้ request
  CARequest.Client := CAClient;
  // set parameter rmGET,rmPOST,rmPUT,rmDELETE
  CARequest.Method := rmPUT;
  // Memo1.Lines.Add(BodyJson.ToJSON);
  // เรียก api
  CARequest.Execute;
  if (CARequest.Response.StatusCode = 200) then
  begin
    var pResultObj := CARequest.Response.JSONValue as TJSONObject;
    Version := pResultObj.FindValue('sys.version').Value.ToInteger;
    pResult.AddPair('status', TJSONBool.Create(True));
    pResult.AddPair('message', 'Update content complete.');
    pResult.AddPair('result', pResultObj);
  end
  else
  begin
    pResult.AddPair('status', TJSONBool.Create(False));
    pResult.AddPair('result', CARequest.Response.JSONText);
  end;
  Result := pResult;
end;

procedure TMAds.UpdateSchedule(pContentId: String; pVersion: Integer;
  pValue: Boolean);
begin
  var
  bodyObj := TJSONObject.Create;
  bodyObj.AddPair('op', 'add');
  bodyObj.AddPair('path', '/fields/schedule/en-US');
  bodyObj.AddPair('value', TJSONBool.Create(pValue));
  var
  bodyArray := TJSONArray.Create;
  bodyArray.Add(bodyObj);
  var
  CPAClient := TRESTClient.Create(nil);
  var
  CPARequest := TRESTRequest.Create(nil);
  CPARequest.AddParameter('Authorization', 'Bearer ' + CmaToken, pkHTTPHEADER,
    [poDoNotEncode]);
  CPARequest.AddParameter('X-Contentful-Version', pVersion.ToString,
    pkHTTPHEADER, [poDoNotEncode]);
  CPARequest.AddParameter('Content-Type', 'application/json-patch+json',
    pkHTTPHEADER, [poDoNotEncode]);
  CPARequest.AddBody(bodyArray.ToString, ctAPPLICATION_JSON);

  // กำหนด url ของ api
  CPAClient.BaseURL := Concat(ContentfulUrl, '/entries/', pContentId);
  // assige client ให้ request
  CPARequest.Client := CPAClient;
  // set parameter rmGET,rmPOST,rmPUT,rmDELETE
  CPARequest.Method := rmPATCH;
  // เรียก api
  CPARequest.Execute;
  if (CPARequest.Response.StatusCode = 200) then
  begin
    var
    Result := CPARequest.Response.JSONValue as TJSONObject;
    Version := Result.FindValue('sys.version').value.ToInteger;
  end;
end;

function TMAds.Validate: TJSONObject;
begin
  var
  CheckResult := TJSONObject.Create;
  var
  ErrorArray := TJSONArray.Create;
  var
  CheckObject := CValidateHelper.Create;
  Title := Trim(Title);
  Slug := Trim(Slug);
  var
  ResultTitle := CheckObject.CheckBlank('Title', Title);
  if ResultTitle.FindValue('result').value.ToBoolean = False then
  begin
    ErrorArray.Add(ResultTitle);
  end;
  var
  ResultSlug := CheckObject.CheckBlank('Slug', Slug);
  if ResultSlug.FindValue('result').value.ToBoolean = False then
  begin
    ErrorArray.Add(ResultSlug);
  end;
  // Thumbnail
  var
  ResultAdsmedia := CheckObject.CheckBlank('Adsmedia', AdsmediaId);
  if ResultAdsmedia.FindValue('result').value.ToBoolean = False then
  begin
    ErrorArray.Add(ResultAdsmedia);
  end;
  if ErrorArray.Count = 0 then
  begin
    CheckResult.AddPair('result', TJSONBool.Create(True));
  end
  else
  begin
    CheckResult.AddPair('result', TJSONBool.Create(False));
    CheckResult.AddPair('error', ErrorArray);
  end;
  Result := CheckResult;
end;

end.
