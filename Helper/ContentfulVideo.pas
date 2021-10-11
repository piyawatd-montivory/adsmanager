unit ContentfulVideo;

interface

uses System.JSON,REST.Types,REST.Client,System.Classes,System.SysUtils,
System.Hash,System.Net.HttpClient,System.Net.HttpClientComponent,FMX.Objects,
System.Types,System.UITypes,FMX.Graphics,FMX.Types,Log;

type
  MContentfulVideo = Class

  private
    CmaToken,SpaceId,Environment:String;
  public
    ContentfulUrl : String;
    Id,SourceId,FileExt : String;
    Version : Integer;
    LogObj : MLog;
    constructor Create(pCmaToken,pSpaceId,pEnvironment:String);
    function Upload(pFilePath:WideString):Boolean;
    function CreateAssets(pTitle,pFilename:WideString):TJSONObject;
    function UpdateAssets(UploadId,ImageId,Title,Filename,Filetype:String;Version:Integer):TJSONObject;
    function ProcessAssets:Boolean;
    function GetVersion:Boolean;
    function Publish:Boolean;
    function UnPublish:Boolean;
    procedure DeleteVdo;

  End;

implementation

constructor MContentfulVideo.Create(pCmaToken,pSpaceId,pEnvironment:String);
begin
CmaToken:=pCmaToken;
SpaceId:=pSpaceId;
Environment:=pEnvironment;
ContentfulUrl := Concat('https://api.contentful.com/spaces/',SpaceId,'/environments/',pEnvironment);
LogObj := MLog.Create;
end;

function MContentfulVideo.UpdateAssets(UploadId,ImageId,Title,
  Filename, Filetype: String; Version: Integer): TJSONObject;
begin
var ResultRest:TJSONObject;
begin
    var today := Now;
    Filetype := 'video/mp4';
    ResultRest := TJSONObject.Create;
    var newFilename := Concat(Filename,FormatDateTime('dd',today),
    FormatDateTime('mm',today),FormatDateTime('yyyy',today),
    FormatDateTime('hh',today),FormatDateTime('nn',today),System.Hash.THash.GetRandomString(5));
    var SysJson := TJSONObject.Create;
    SysJson.AddPair('type','Link');
    SysJson.AddPair('linkType','Upload');
    SysJson.AddPair('id',UploadId);
    var UploadFormJson := TJSONObject.Create;
    UploadFormJson.AddPair('sys',SysJson);
    var LangFileJson := TJSONObject.Create;
    LangFileJson.AddPair('contentType',Filetype);
    LangFileJson.AddPair('fileName',newFilename);
    LangFileJson.AddPair('uploadFrom',UploadFormJson);
    var LangTitleJson := TJSONObject.Create;
    LangTitleJson.AddPair('en-US',Title);
    var FileJson := TJSONObject.Create;
    FileJson.AddPair('en-US',LangFileJson);
    var FieldsJson := TJSONObject.Create;
    FieldsJson.AddPair('title',LangTitleJson);
    FieldsJson.AddPair('file',FileJson);
    var BodyJson := TJSONObject.Create;
    BodyJson.AddPair('fields',FieldsJson);

    var CAClient := TRESTClient.Create(nil);
    var CARequest := TRESTRequest.Create(nil);
    try
    CARequest.AddParameter('Authorization','Bearer '+CmaToken,pkHTTPHEADER,[poDoNotEncode]);
    CARequest.AddParameter('X-Contentful-Version',Version.ToString,pkHTTPHEADER,[poDoNotEncode]);
    CARequest.AddParameter('Content-Type','application/vnd.contentful.management.v1+json',pkHTTPHEADER,[poDoNotEncode]);
    CARequest.AddBody(BodyJson.ToString, ctAPPLICATION_JSON);
    // New Item
    // กำหนด url ของ api
    CAClient.BaseURL := Concat(ContentfulUrl,'/assets/',ImageId);
    // assige client ให้ request
    CARequest.Client := CAClient;
    // set parameter rmGET,rmPOST,rmPUT,rmDELETE
    CARequest.Method := rmPUT;
    // เรียก api
    CARequest.Execute;
    if (CARequest.Response.StatusCode = 201) then
    begin
      ResultRest := CARequest.Response.JSONValue as TJSONObject;
    end;
    finally
//      CAClient.Free;
//      CARequest.Free;
    end;
    Result := ResultRest;
end;
end;

function MContentfulVideo.Upload(pFilePath:WideString): Boolean;
begin
  var vdoFile := TFileStream.Create(pFilePath, fmOpenRead);
  var LStream := TMemoryStream.Create;
  LStream := TMemoryStream.Create;
  LStream.CopyFrom(vdoFile);
  LStream.Position:= 0;
  var CClient := TRESTClient.Create(nil);
  var CRequest := TRESTRequest.Create(nil);
  LStream.Position := 0;
  CRequest.AddParameter('Authorization','Bearer '+CmaToken,pkHTTPHEADER,[poDoNotEncode]);
  CRequest.AddParameter('Content-Type','application/octet-stream',pkHTTPHEADER,[poDoNotEncode]);
  CRequest.AddBody(LStream,TRESTContentType.ctVIDEO_MP4);
  // New Item
  // กำหนด url ของ api
  CClient.BaseURL := 'https://upload.contentful.com/spaces/'+SpaceId+'/uploads';
  // assige client ให้ request
  CRequest.Client := CClient;
  // set parameter rmGET,rmPOST,rmPUT,rmDELETE
  CRequest.Method := rmPOST;
  CRequest.ConnectTimeout := 300000;
  CRequest.Timeout := 300000;
  // เรียก api
  var pResult := False;
  try
    CRequest.Execute;
    if (CRequest.Response.StatusCode = 201) then
    begin
      var RestResult := CRequest.Response.JSONValue as TJSONObject;
      SourceId := RestResult.FindValue('sys.id').Value;
      pResult := True;
    end;
  finally
    CClient.Free;
    LStream.Free;
    vdoFile.Free;
  end;
  Result := pResult;
end;

function MContentfulVideo.CreateAssets(pTitle,pFilename:WideString):TJSONObject;
begin
    var today := Now;
    var pResult := TJSONObject.Create;
    var newFilename := Concat(pFilename,FormatDateTime('dd',today),
    FormatDateTime('mm',today),FormatDateTime('yyyy',today),
    FormatDateTime('hh',today),FormatDateTime('nn',today),System.Hash.THash.GetRandomString(5),'.mp4');
    var SysJson := TJSONObject.Create;
    SysJson.AddPair('type','Link');
    SysJson.AddPair('linkType','Upload');
    SysJson.AddPair('id',SourceId);
    var UploadFormJson := TJSONObject.Create;
    UploadFormJson.AddPair('sys',SysJson);
    var LangFileJson := TJSONObject.Create;
    LangFileJson.AddPair('contentType','video/mp4');
    LangFileJson.AddPair('fileName',newFilename);
    LangFileJson.AddPair('uploadFrom',UploadFormJson);
    var LangTitleJson := TJSONObject.Create;
    LangTitleJson.AddPair('en-US',pTitle);
    var FileJson := TJSONObject.Create;
    FileJson.AddPair('en-US',LangFileJson);
    var FieldsJson := TJSONObject.Create;
    FieldsJson.AddPair('title',LangTitleJson);
    FieldsJson.AddPair('file',FileJson);
    var BodyJson := TJSONObject.Create;
    BodyJson.AddPair('fields',FieldsJson);
    //metadata
    var sysValue := TJSONObject.Create;
    sysValue.AddPair('type','Link');
    sysValue.AddPair('linkType','Tag');
    sysValue.AddPair('id','_ads');
    var tagItem := TJSONObject.Create;
    tagItem.AddPair('sys',sysValue);
    var tagArray := TJSONArray.Create;
    tagArray.Add(tagItem);
    var metaTagsObj := TJSONObject.Create;
    metaTagsObj.AddPair('tags',tagArray);
    BodyJson.AddPair('metadata',metaTagsObj);

    var CAClient := TRESTClient.Create(nil);
    var CARequest := TRESTRequest.Create(nil);
    try
    CARequest.AddParameter('Authorization','Bearer '+CmaToken,pkHTTPHEADER,[poDoNotEncode]);
    CARequest.AddParameter('Content-Type','application/vnd.contentful.management.v1+json',pkHTTPHEADER,[poDoNotEncode]);
    CARequest.AddBody(BodyJson.ToString, ctAPPLICATION_JSON);
    // New Item
    // กำหนด url ของ api
    CAClient.BaseURL := Concat(ContentfulUrl,'/assets');
    // assige client ให้ request
    CARequest.Client := CAClient;
    // set parameter rmGET,rmPOST,rmPUT,rmDELETE
    CARequest.Method := rmPOST;
    // เรียก api
    CARequest.Execute;
    if (CARequest.Response.StatusCode = 201) then
    begin
      pResult := CARequest.Response.JSONValue as TJSONObject;
      pResult.AddPair('status', TJSONBool.Create(True));
      Id := pResult.FindValue('sys.id').Value;
      Version := pResult.FindValue('sys.version').Value.ToInteger;
      LogObj.AddLog(Concat(Id,' create assets version ',pResult.FindValue('sys.version').Value));
    end
    else
    begin
      pResult.AddPair('result', CARequest.Response.JSONText);
      pResult.AddPair('body', BodyJson.ToJSON());
      pResult.AddPair('status', TJSONBool.Create(False));
    end;
    finally
      CAClient.Free;
//      CARequest.Free;
    end;
    Result := pResult;
end;

procedure MContentfulVideo.DeleteVdo;
begin
    var DClient := TRESTClient.Create(nil);
    var DRequest := TRESTRequest.Create(nil);
    DRequest.AddParameter('Authorization','Bearer '+CmaToken,pkHTTPHEADER,[poDoNotEncode]);
    DRequest.AddParameter('X-Contentful-Version',Version.ToString,pkHTTPHEADER,[poDoNotEncode]);
    // New Item
    // กำหนด url ของ api
    DClient.BaseURL := Concat(ContentfulUrl,'/assets/',Id);
    // assige client ให้ request
    DRequest.Client := DClient;
    // set parameter rmGET,rmPOST,rmPUT,rmDELETE
    DRequest.Method := rmDELETE;
      // เรียก api
      try
    DRequest.Execute;
      finally
       DClient.Free;
      end;
end;

function MContentfulVideo.GetVersion:Boolean;
begin
    var CPAClient := TRESTClient.Create(nil);
    var CPARequest := TRESTRequest.Create(nil);
    CPARequest.AddParameter('Authorization','Bearer '+CmaToken,pkHTTPHEADER,[poDoNotEncode]);
    // New Item
    // กำหนด url ของ api
    CPAClient.BaseURL := Concat(ContentfulUrl,'/assets/',Id);
    // assige client ให้ request
    CPARequest.Client := CPAClient;
    // set parameter rmGET,rmPOST,rmPUT,rmDELETE
    CPARequest.Method := rmGET;
    var MS := TMemoryStream.Create;
      // เรียก api
      var pResult := False;
      try
    CPARequest.Execute;
    if CPARequest.Response.StatusCode = 200 then
    begin
      var vdoResult := CPARequest.Response.JSONValue as TJSONObject;
      Version := vdoResult.FindValue('sys.version').Value.ToInteger;
      LogObj.AddLog(Concat(Id,' get version version ',vdoResult.FindValue('sys.version').Value));
      pResult:= True;
    end;
      finally
       CPAClient.Free;
      end;
    Result :=  pResult;
end;

function MContentfulVideo.ProcessAssets:Boolean;
begin
var
  pResult := False;
  var
  CPAClient := TRESTClient.Create(nil);
  var
  CPARequest := TRESTRequest.Create(nil);
  CPARequest.AddParameter('Authorization', 'Bearer ' + CmaToken, pkHTTPHEADER,
    [poDoNotEncode]);
  CPARequest.AddParameter('X-Contentful-Version', Version.ToString, pkHTTPHEADER,
    [poDoNotEncode]);
  // New Item
  // กำหนด url ของ api
  CPAClient.BaseURL := Concat(ContentfulUrl, '/assets/', Id,
    '/files/en-US/process');
  // assige client ให้ request
  CPARequest.Client := CPAClient;
  // set parameter rmGET,rmPOST,rmPUT,rmDELETE
  CPARequest.Method := rmPUT;
  CPARequest.ConnectTimeout := 300000;
  CPARequest.Timeout := 300000;
  try
    // เรียก api
    CPARequest.Execute;
    if (CPARequest.Response.StatusCode = 204) then
    begin
      pResult := True;
      LogObj.AddLog(Concat(Id,' process complete.'));
    end;
  finally
    CPAClient.Free;
  end;
  sleep(1000);
  Result := pResult;
end;

function MContentfulVideo.Publish:Boolean;
begin
  LogObj.AddLog(Concat(Id,' publish version ', Version.ToString));
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
  CPAClient.BaseURL := Concat(ContentfulUrl, '/assets/', Id, '/published');
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
      var pRestResult := CPARequest.Response.JSONValue as TJSONObject;
      Version := pRestResult.FindValue('sys.version').Value.ToInteger;
    end;
  finally
    CPAClient.Free;
  end;
  Result := pResult;
end;

function MContentfulVideo.UnPublish:Boolean;
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
  CPAClient.BaseURL := Concat(ContentfulUrl, '/assets/', Id, '/published');
  // assige client ให้ request
  CPARequest.Client := CPAClient;
  // set parameter rmGET,rmPOST,rmPUT,rmDELETE
  CPARequest.Method := rmDELETE;
  // เรียก api
  try
    CPARequest.Execute;
    if CPARequest.Response.StatusCode = 200 then
    begin
      pResult := True;
      var pRestResult := CPARequest.Response.JSONValue as TJSONObject;
      Version := pRestResult.FindValue('sys.version').Value.ToInteger;
    end;
  finally
    CPAClient.Free;
  end;
  Result := pResult;
end;
end.
