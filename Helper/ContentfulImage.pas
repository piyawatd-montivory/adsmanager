unit ContentfulImage;

interface

uses System.JSON,REST.Types,REST.Client,System.Classes,System.SysUtils,
System.Hash,System.Net.HttpClient,System.Net.HttpClientComponent,FMX.Objects,
System.Types,System.UITypes,FMX.Graphics,FMX.Types;

type
  MContentfulImage = Class

  private
    CmaToken,SpaceId,Environment:String;
  public
    PreviewImage : TBitmap;
    AssetVersion : Integer;
    ContentfulUrl : String;
    constructor Create(pCmaToken,pSpaceId,pEnvironment:String);
    function Upload(FilePath,Filename,FileExt:String):String;
    function CreateAssets(UploadId,Title,Filename,Filetype:String):TJSONObject;
    function UpdateAssets(UploadId,ImageId,Title,Filename,Filetype:String;Version:Integer):TJSONObject;
    function ProcessAssets(AssetId:String):Boolean;
    function LoadImage(url:String):TMemoryStream;
    function GetById(assetid:String):TMemoryStream;
    function GetVersionById(assetid:String):Integer;
    function PublishAssets(assetId:String;version:Integer):Integer;
    function UnPublishAssets(assetId:String;version:Integer):Integer;
    procedure DeleteImage(assetId:String;version:Integer);

  End;

implementation

constructor MContentfulImage.Create(pCmaToken,pSpaceId,pEnvironment:String);
begin
CmaToken:=pCmaToken;
SpaceId:=pSpaceId;
Environment:=pEnvironment;
ContentfulUrl := Concat('https://api.contentful.com/spaces/',SpaceId,'/environments/',pEnvironment);
end;

function MContentfulImage.UpdateAssets(UploadId,ImageId,Title,
  Filename, Filetype: String; Version: Integer): TJSONObject;
begin
var ResultRest:TJSONObject;
begin
    var today := Now;
    if (Filetype = '.jpg') OR (Filetype = '.jpeg') then
    begin
      Filetype := 'image/jpeg';
    end;
    if Filetype = '.png' then
    begin
      Filetype := 'image/png';
    end;
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

function MContentfulImage.Upload(FilePath,Filename,FileExt:String): String;
var UploadId : String;
begin

  var OriginImg:= TImage.Create(nil);
  OriginImg.Bitmap.LoadFromFile(FilePath);
//  var MyRect := TRectF.Create( 0, 0, OriginImg.Bitmap.Width-10, OriginImg.Bitmap.Height-10);
//  with OriginImg.Bitmap.Canvas do
//  begin
//        BeginScene;
//        Fill.Color:=TAlphaColors.White;
//        Font.Size:=Int(OriginImg.Bitmap.Height*5)/100;
//        {$IFDEF MACOS}
//        FillText(MyRect, 'Copyright © spacebar', false, 100,
//        [TFillTextFlag.RightToLeft], TTextAlign.Trailing, TTextAlign.Trailing);
//        {$ENDIF}
//        {$IFDEF MSWINDOWS}
//          FillText(MyRect, 'Copyright © spacebar', false, 100,
//        [TFillTextFlag.RightToLeft], TTextAlign.Leading, TTextAlign.Trailing);
//        {$ENDIF}
//        EndScene
//  end;
  var LStream := TMemoryStream.Create;
  OriginImg.Bitmap.SaveToStream(LStream);
  PreviewImage := OriginImg.Bitmap;
  var CClient := TRESTClient.Create(nil);
  var CRequest := TRESTRequest.Create(nil);
  try
    LStream.Position := 0;
    CRequest.AddParameter('Authorization','Bearer '+CmaToken,pkHTTPHEADER,[poDoNotEncode]);
    CRequest.AddParameter('Content-Type','application/octet-stream',pkHTTPHEADER,[poDoNotEncode]);
    if (LowerCase(FileExt) = '.jpg') OR (LowerCase(FileExt) = '.jpeg') then
    begin
      CRequest.AddBody(LStream,TRESTContentType.ctIMAGE_JPEG);
      FileExt := 'image/jpeg';
    end;
    if LowerCase(FileExt) = '.png' then
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
    CRequest.Execute;
    if (CRequest.Response.StatusCode = 201) then
    begin
      var RestResult := CRequest.Response.JSONValue as TJSONObject;
      UploadId := RestResult.FindValue('sys.id').Value;
    end;
  finally
    CClient.Free;
    CRequest.Free;
    LStream.Free;
  end;
  Result := UploadId;
end;

function MContentfulImage.CreateAssets(UploadId,Title,Filename,Filetype:String):TJSONObject;
var ResultRest:TJSONObject;
begin
    var today := Now;
    if (Filetype = '.jpg') OR (Filetype = '.jpeg') then
    begin
      Filetype := 'image/jpeg';
    end;
    if Filetype = '.png' then
    begin
      Filetype := 'image/png';
    end;
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
      ResultRest := CARequest.Response.JSONValue as TJSONObject;
    end
    else
    begin
      ResultRest := TJSONObject.Create;
      ResultRest.AddPair('result',CARequest.Response.JSONText);
      ResultRest.AddPair('body',BodyJson.ToJSON());
    end;
    finally
//      CAClient.Free;
//      CARequest.Free;
    end;
    Result := ResultRest;
end;

procedure MContentfulImage.DeleteImage(assetId: String;version : Integer);
begin
    var CPAClient := TRESTClient.Create(nil);
    var CPARequest := TRESTRequest.Create(nil);
    CPARequest.AddParameter('Authorization','Bearer '+CmaToken,pkHTTPHEADER,[poDoNotEncode]);
    CPARequest.AddParameter('X-Contentful-Version',version.ToString,pkHTTPHEADER,[poDoNotEncode]);
    // New Item
    // กำหนด url ของ api
    CPAClient.BaseURL := Concat(ContentfulUrl,'/assets/',assetId,'/published');
    // assige client ให้ request
    CPARequest.Client := CPAClient;
    // set parameter rmGET,rmPOST,rmPUT,rmDELETE
    CPARequest.Method := rmDELETE;
      // เรียก api
    CPARequest.Execute;
    if CPARequest.Response.StatusCode = 200 then
    begin
      version := version + 1;
    end;
    var DClient := TRESTClient.Create(nil);
    var DRequest := TRESTRequest.Create(nil);
    DRequest.AddParameter('Authorization','Bearer '+CmaToken,pkHTTPHEADER,[poDoNotEncode]);
    DRequest.AddParameter('X-Contentful-Version',version.ToString,pkHTTPHEADER,[poDoNotEncode]);
    // New Item
    // กำหนด url ของ api
    DClient.BaseURL := Concat(ContentfulUrl,'/assets/',assetId);
    // assige client ให้ request
    DRequest.Client := DClient;
    // set parameter rmGET,rmPOST,rmPUT,rmDELETE
    DRequest.Method := rmDELETE;
      // เรียก api
    DRequest.Execute;
end;

function MContentfulImage.GetById(assetid: String):TMemoryStream;
begin
    var CPAClient := TRESTClient.Create(nil);
    var CPARequest := TRESTRequest.Create(nil);
    CPARequest.AddParameter('Authorization','Bearer '+CmaToken,pkHTTPHEADER,[poDoNotEncode]);
    // New Item
    // กำหนด url ของ api
    CPAClient.BaseURL := Concat(ContentfulUrl,'/assets/',AssetId);
    // assige client ให้ request
    CPARequest.Client := CPAClient;
    // set parameter rmGET,rmPOST,rmPUT,rmDELETE
    CPARequest.Method := rmGET;
    var MS := TMemoryStream.Create;
      // เรียก api
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
    Result := MS;
end;

function MContentfulImage.GetVersionById(assetid: String): Integer;
begin
    var CPAClient := TRESTClient.Create(nil);
    var CPARequest := TRESTRequest.Create(nil);
    CPARequest.AddParameter('Authorization','Bearer '+CmaToken,pkHTTPHEADER,[poDoNotEncode]);
    // New Item
    // กำหนด url ของ api
    CPAClient.BaseURL := Concat(ContentfulUrl,'/assets/',AssetId);
    // assige client ให้ request
    CPARequest.Client := CPAClient;
    // set parameter rmGET,rmPOST,rmPUT,rmDELETE
    CPARequest.Method := rmGET;
    var MS := TMemoryStream.Create;
      // เรียก api
    CPARequest.Execute;
    var imgResult := CPARequest.Response.JSONValue as TJSONObject;
    Result := imgResult.FindValue('sys.version').Value.ToInteger;
end;

function MContentfulImage.ProcessAssets(AssetId:String):Boolean;
var ResultRest:Boolean;
begin
    var CPAClient := TRESTClient.Create(nil);
    var CPARequest := TRESTRequest.Create(nil);
    CPARequest.AddParameter('Authorization','Bearer '+CmaToken,pkHTTPHEADER,[poDoNotEncode]);
    CPARequest.AddParameter('X-Contentful-Version','1',pkHTTPHEADER,[poDoNotEncode]);
    // New Item
    // กำหนด url ของ api
    CPAClient.BaseURL := Concat(ContentfulUrl,'/assets/',AssetId,'/files/en-US/process');
    // assige client ให้ request
    CPARequest.Client := CPAClient;
    // set parameter rmGET,rmPOST,rmPUT,rmDELETE
    CPARequest.Method := rmPUT;
    try
      // เรียก api
      CPARequest.Execute;
      if (CPARequest.Response.StatusCode = 204) then
      begin
        ResultRest:=True;
      end
      else
      begin
        ResultRest:=False;
      end;
    finally
    end;
    Result := ResultRest;
end;

function MContentfulImage.PublishAssets(assetId: String; version: Integer):Integer;
begin
    var CPAClient := TRESTClient.Create(nil);
    var CPARequest := TRESTRequest.Create(nil);
    CPARequest.AddParameter('Authorization','Bearer '+CmaToken,pkHTTPHEADER,[poDoNotEncode]);
    CPARequest.AddParameter('X-Contentful-Version',version.ToString,pkHTTPHEADER,[poDoNotEncode]);
    // กำหนด url ของ api
    CPAClient.BaseURL := Concat(ContentfulUrl,'/assets/',AssetId,'/published');
    // assige client ให้ request
    CPARequest.Client := CPAClient;
    // set parameter rmGET,rmPOST,rmPUT,rmDELETE
    CPARequest.Method := rmPUT;
    // เรียก api
    CPARequest.Execute;
    if CPARequest.Response.StatusCode = 200 then
    begin
      Result :=  version+1;
    end
    else
    begin
      Result := version;
    end;
end;

function MContentfulImage.UnPublishAssets(assetId: String; version: Integer):Integer;
begin
    var CPAClient := TRESTClient.Create(nil);
    var CPARequest := TRESTRequest.Create(nil);
    CPARequest.AddParameter('Authorization','Bearer '+CmaToken,pkHTTPHEADER,[poDoNotEncode]);
    CPARequest.AddParameter('X-Contentful-Version',version.ToString,pkHTTPHEADER,[poDoNotEncode]);
    // กำหนด url ของ api
    CPAClient.BaseURL := Concat(ContentfulUrl,'/assets/',AssetId,'/published');
    // assige client ให้ request
    CPARequest.Client := CPAClient;
    // set parameter rmGET,rmPOST,rmPUT,rmDELETE
    CPARequest.Method := rmDELETE;
    // เรียก api
    CPARequest.Execute;
    if CPARequest.Response.StatusCode = 200 then
    begin
      Result :=  version+1;
    end
    else
    begin
      Result := version;
    end;
end;

function MContentfulImage.LoadImage(url: string): TMemoryStream;
begin
    var MS := TMemoryStream.Create;
    var NetClient := TNetHTTPClient.Create(Nil);
    var NetHTTP := TNetHTTPRequest.Create(Nil);
    NetHTTP.Client := NetClient;
    NetHTTP.Get(url, MS);
    MS.Seek(0, soFromBeginning);
    Result := MS;
end;

end.
