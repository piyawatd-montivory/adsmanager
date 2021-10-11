unit ContentfulImage;

interface

uses System.JSON,REST.Types,REST.Client,System.Classes,System.SysUtils,
System.Hash,System.Net.HttpClient,System.Net.HttpClientComponent,FMX.Objects,
System.Types,System.UITypes,FMX.Graphics,FMX.Types,Log;

type
  MContentfulImage = Class

  private
    CmaToken,SpaceId,Environment:String;
  public
    PreviewImage : TBitmap;
    AssetVersion : Integer;
    ContentfulUrl : String;
    Id,SourceId,FileExt : String;
    Version : Integer;
    LogObj : MLog;
    constructor Create(pCmaToken,pSpaceId,pEnvironment:String);
    function Upload(pFilePath,pFileExt:String):Boolean;
    function CreateAsset(pTitle,pDescription,pFilename:WideString):TJSONObject;
    function UpdateAsset(pId,pTitle,pDescription,pFilename:WideString;pVersion:Integer):TJSONObject;
    function ProcessAssets:Boolean;
    function LoadImage(url:String):TMemoryStream;
    function GetById(pId:String):TMemoryStream;
    function GetVersion:Boolean;
    function Publish:Boolean;
    function UnPublish:Boolean;
    procedure DeleteImage;
  End;

implementation

constructor MContentfulImage.Create(pCmaToken,pSpaceId,pEnvironment:String);
begin
CmaToken:=pCmaToken;
SpaceId:=pSpaceId;
Environment:=pEnvironment;
ContentfulUrl := Concat('https://api.contentful.com/spaces/',SpaceId,'/environments/',pEnvironment);
LogObj := MLog.Create;
end;

function MContentfulImage.UpdateAsset(pId,pTitle,pDescription,pFilename:WideString;pVersion:Integer): TJSONObject;
begin
begin
    var pResult := TJSONObject.Create;
    var today := Now;
    var newFilename := Concat(pFilename,FormatDateTime('dd',today),
    FormatDateTime('mm',today),FormatDateTime('yyyy',today),
    FormatDateTime('hh',today),FormatDateTime('nn',today),System.Hash.THash.GetRandomString(5));
    var SysJson := TJSONObject.Create;
    SysJson.AddPair('type','Link');
    SysJson.AddPair('linkType','Upload');
    SysJson.AddPair('id',SourceId);
    var UploadFormJson := TJSONObject.Create;
    UploadFormJson.AddPair('sys',SysJson);
    var LangFileJson := TJSONObject.Create;
    LangFileJson.AddPair('contentType',FileExt);
    LangFileJson.AddPair('fileName',newFilename);
    LangFileJson.AddPair('uploadFrom',UploadFormJson);
    //title
    var LangTitleJson := TJSONObject.Create;
    LangTitleJson.AddPair('en-US',pTitle);
    //caption
    var LangDesJson := TJSONObject.Create;
    LangDesJson.AddPair('en-US',pDescription);
    var FileJson := TJSONObject.Create;
    FileJson.AddPair('en-US',LangFileJson);
    var FieldsJson := TJSONObject.Create;
    FieldsJson.AddPair('title',LangTitleJson);
    FieldsJson.AddPair('description',LangDesJson);
    FieldsJson.AddPair('file',FileJson);
    var BodyJson := TJSONObject.Create;
    BodyJson.AddPair('fields',FieldsJson);

    var CAClient := TRESTClient.Create(nil);
    var CARequest := TRESTRequest.Create(nil);
    CARequest.AddParameter('Authorization','Bearer '+CmaToken,pkHTTPHEADER,[poDoNotEncode]);
    CARequest.AddParameter('X-Contentful-Version',pVersion.ToString,pkHTTPHEADER,[poDoNotEncode]);
    CARequest.AddParameter('Content-Type','application/vnd.contentful.management.v1+json',pkHTTPHEADER,[poDoNotEncode]);
    CARequest.AddBody(BodyJson.ToString, ctAPPLICATION_JSON);
    // New Item
    // กำหนด url ของ api
    CAClient.BaseURL := Concat(ContentfulUrl,'/assets/',pId);
    // assige client ให้ request
    CARequest.Client := CAClient;
    // set parameter rmGET,rmPOST,rmPUT,rmDELETE
    CARequest.Method := rmPUT;
    // เรียก api
    try
    CARequest.Execute;
    if (CARequest.Response.StatusCode = 200) then
    begin
      pResult := CARequest.Response.JSONValue as TJSONObject;
      pResult.AddPair('status', TJSONBool.Create(True));
      Id := pResult.FindValue('sys.id').Value;
      Version := pResult.FindValue('sys.version').Value.ToInteger;
    end
    else
    begin
      pResult.AddPair('result', CARequest.Response.JSONText);
      pResult.AddPair('body', BodyJson.ToJSON());
      pResult.AddPair('status', TJSONBool.Create(False));
    end;
    finally
      CAClient.Free;
    end;
    Result := pResult;
end;
end;

function MContentfulImage.Upload(pFilePath,pFileExt:String): Boolean;
begin
  var pResult := False;
  var OriginImg:= TImage.Create(nil);
  OriginImg.Bitmap.LoadFromFile(pFilePath);
//  var MyRect := TRectF.Create( 0, 0, OriginImg.Bitmap.Width-10, OriginImg.Bitmap.Height-5);
//  with OriginImg.Bitmap.Canvas do
//  begin
//        BeginScene;
//        Fill.Color:=TAlphaColors.White;
//        Font.Size:=Int(OriginImg.Bitmap.Height*2)/100;
//        {$IFDEF MACOS}
//        FillText(MyRect, 'Copyright © spacebar', false, 50,
//        [TFillTextFlag.RightToLeft], TTextAlign.Trailing, TTextAlign.Trailing);
//        {$ENDIF}
//        {$IFDEF MSWINDOWS}
//          FillText(MyRect, 'Copyright © spacebar', false, 50,
//        [TFillTextFlag.RightToLeft], TTextAlign.Leading, TTextAlign.Trailing);
//        {$ENDIF}
//        EndScene
//  end;
  var LStream := TMemoryStream.Create;
  OriginImg.Bitmap.SaveToStream(LStream);
  var CClient := TRESTClient.Create(nil);
  var CRequest := TRESTRequest.Create(nil);
    LStream.Position := 0;
    CRequest.AddParameter('Authorization','Bearer '+CmaToken,pkHTTPHEADER,[poDoNotEncode]);
    CRequest.AddParameter('Content-Type','application/octet-stream',pkHTTPHEADER,[poDoNotEncode]);
    if (LowerCase(pFileExt) = '.jpg') OR (LowerCase(pFileExt) = '.jpeg') then
    begin
      CRequest.AddBody(LStream,TRESTContentType.ctIMAGE_JPEG);
      FileExt := 'image/jpeg';
    end;
    if LowerCase(pFileExt) = '.png' then
    begin
      CRequest.AddBody(LStream,TRESTContentType.ctIMAGE_PNG);
      FileExt := 'image/png';
    end;
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
  end;
  Result := pResult;
end;

function MContentfulImage.CreateAsset(pTitle,pDescription,pFilename:WideString):TJSONObject;
begin
    var today := Now;
    var pResult := TJSONObject.Create;
    var newFilename := Concat(pFilename,FormatDateTime('dd',today),
    FormatDateTime('mm',today),FormatDateTime('yyyy',today),
    FormatDateTime('hh',today),FormatDateTime('nn',today),System.Hash.THash.GetRandomString(5));
    var SysJson := TJSONObject.Create;
    SysJson.AddPair('type','Link');
    SysJson.AddPair('linkType','Upload');
    SysJson.AddPair('id',SourceId);
    var UploadFormJson := TJSONObject.Create;
    UploadFormJson.AddPair('sys',SysJson);
    var LangFileJson := TJSONObject.Create;
    LangFileJson.AddPair('contentType',FileExt);
    LangFileJson.AddPair('fileName',newFilename);
    LangFileJson.AddPair('uploadFrom',UploadFormJson);
    //Title
    var LangTitleJson := TJSONObject.Create;
    LangTitleJson.AddPair('en-US',pTitle);
    //Description
    var LangDesJson := TJSONObject.Create;
    LangDesJson.AddPair('en-US',pDescription);
    //File
    var FileJson := TJSONObject.Create;
    FileJson.AddPair('en-US',LangFileJson);
    var FieldsJson := TJSONObject.Create;
    FieldsJson.AddPair('title',LangTitleJson);
    FieldsJson.AddPair('description',LangDesJson);
    FieldsJson.AddPair('file',FileJson);
    var BodyJson := TJSONObject.Create;
    BodyJson.AddPair('fields',FieldsJson);
    //metadata
    var sysValue := TJSONObject.Create;
    sysValue.AddPair('type','Link');
    sysValue.AddPair('linkType','Tag');
    sysValue.AddPair('id','_imgcontent');
    var tagItem := TJSONObject.Create;
    tagItem.AddPair('sys',sysValue);
    var tagArray := TJSONArray.Create;
    tagArray.Add(tagItem);
    var metaTagsObj := TJSONObject.Create;
    metaTagsObj.AddPair('tags',tagArray);
    BodyJson.AddPair('metadata',metaTagsObj);

    var CAClient := TRESTClient.Create(nil);
    var CARequest := TRESTRequest.Create(nil);
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
    CARequest.ConnectTimeout := 300000;
    CARequest.Timeout := 300000;
    // เรียก api
    try
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
    end;
    Result := pResult;
end;

procedure MContentfulImage.DeleteImage;
begin
    var CPAClient := TRESTClient.Create(nil);
    var CPARequest := TRESTRequest.Create(nil);
    CPARequest.AddParameter('Authorization','Bearer '+CmaToken,pkHTTPHEADER,[poDoNotEncode]);
    CPARequest.AddParameter('X-Contentful-Version',Version.ToString,pkHTTPHEADER,[poDoNotEncode]);
    // กำหนด url ของ api
    CPAClient.BaseURL := Concat(ContentfulUrl,'/assets/',Id);
    // assige client ให้ request
    CPARequest.Client := CPAClient;
    // set parameter rmGET,rmPOST,rmPUT,rmDELETE
    CPARequest.Method := rmDELETE;
      // เรียก api
      try
    CPARequest.Execute;
      finally
      CPAClient.Free;
      end;
end;

function MContentfulImage.GetById(pId: String):TMemoryStream;
begin
    var CPAClient := TRESTClient.Create(nil);
    var CPARequest := TRESTRequest.Create(nil);
    CPARequest.AddParameter('Authorization','Bearer '+CmaToken,pkHTTPHEADER,[poDoNotEncode]);
    // New Item
    // กำหนด url ของ api
    CPAClient.BaseURL := Concat(ContentfulUrl,'/assets/',pId);
    // assige client ให้ request
    CPARequest.Client := CPAClient;
    // set parameter rmGET,rmPOST,rmPUT,rmDELETE
    CPARequest.Method := rmGET;
    var MS := TMemoryStream.Create;
      // เรียก api
      try
    CPARequest.Execute;
    var imgResult := CPARequest.Response.JSONValue as TJSONObject;
    AssetVersion := imgResult.FindValue('sys.version').Value.ToInteger;
    var NetClient := TNetHTTPClient.Create(Nil);
    var NetHTTP := TNetHTTPRequest.Create(Nil);
    NetHTTP.Client := NetClient;
    NetHTTP.ResponseTimeout := 300000;
    NetHTTP.ConnectionTimeout := 300000;
    NetHTTP.Get(Concat('https:',imgResult.FindValue('fields.file.en-US.url').Value,'?w=150'), MS);
    MS.Seek(0, soFromBeginning);
      finally
     CPAClient.Free;
      end;
    Result := MS;
end;

function MContentfulImage.GetVersion:Boolean;
begin
  var pResult := False;
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
    CPARequest.ConnectTimeout := 300000;
    CPARequest.Timeout := 300000;
//    var MS := TMemoryStream.Create;
      // เรียก api
      try
    CPARequest.Execute;
    if CPARequest.Response.StatusCode = 200 then
    begin
      var imgResult := CPARequest.Response.JSONValue as TJSONObject;
      Version := imgResult.FindValue('sys.version').Value.ToInteger;
      LogObj.AddLog(Concat(Id,' get version version ',imgResult.FindValue('sys.version').Value));
      pResult:= True;
    end;
      finally
       CPAClient.Free;
      end;
    Result :=  pResult;
end;

function MContentfulImage.ProcessAssets:Boolean;
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

function MContentfulImage.Publish:Boolean;
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

function MContentfulImage.UnPublish:Boolean;
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

function MContentfulImage.LoadImage(url: string): TMemoryStream;
begin
    var MS := TMemoryStream.Create;
    var NetClient := TNetHTTPClient.Create(Nil);
    var NetHTTP := TNetHTTPRequest.Create(Nil);
    NetHTTP.Client := NetClient;
    try
    NetHTTP.Get(url, MS);
    MS.Seek(0, soFromBeginning);
    finally
      NetHTTP.Free;
    end;
    Result := MS;
end;

end.
