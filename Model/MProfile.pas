unit MProfile;

interface

uses
  System.JSON, System.SysUtils, MarkdownProcessor, REST.Client, REST.Types,
  ModelHelper, ValidateHelper, System.Math, FMX.Graphics;

type
  TMProfile = Class

  private
    FId: String;
    FUId: String;
    FFirstname, FLastname, FIntro,FTitle: WideString;
    FProfileImage, FAssetId, ContentfulUrl, CmaToken, SpaceId,
      Environments: String;
    FProfilePicture: TBitmap;
    FVersion, FAssetVersion: Integer;
    ModelHelperObject: TModelHelper;
  public
    constructor Create(pCmaToken, pSpaceId, pEnvironments: String);
    property Id: String read FId write FId;
    property UId: String read FUId write FUId;
    property Version: Integer read FVersion write FVersion;
    property AssetVersion: Integer read FAssetVersion write FAssetVersion;
    property Firstname: WideString read FFirstname write FFirstname;
    property Lastname: WideString read FLastname write FLastname;
    property Title: WideString read FTitle write FTitle;
    property Intro: WideString read FIntro write FIntro;
    property ProfileImage: String read FProfileImage write FProfileImage;
    property AssetId: String read FAssetId write FAssetId;
    property ProfilePicture: TBitmap read FProfilePicture write FProfilePicture;
    function GetFieldsJson: TJSONObject;
    function Save: TJSONObject;
    function Update: TJSONObject;
    function GetByToken: TJSONObject;
    function GetById: TJSONObject;
    function Validate: TJSONObject;
  End;

implementation

constructor TMProfile.Create(pCmaToken, pSpaceId, pEnvironments: String);
begin
  ModelHelperObject := TModelHelper.Create;
  CmaToken := pCmaToken;
  SpaceId := pSpaceId;
  Environments := pEnvironments;
  ContentfulUrl := Concat('https://api.contentful.com/spaces/', SpaceId,
    '/environments/', Environments);
end;

function TMProfile.GetById: TJSONObject;
begin
  var
  ResultRest := TJSONObject.Create;
  var
  CAClient := TRESTClient.Create(nil);
  var
  CARequest := TRESTRequest.Create(nil);
  CARequest.AddParameter('Authorization', 'Bearer ' + CmaToken, pkHTTPHEADER,
    [poDoNotEncode]);
  CARequest.AddParameter('content_type', 'spacebarprofile', pkGETorPOST,
    [poDoNotEncode]);
  CARequest.AddParameter('limit', '1', pkGETorPOST, [poDoNotEncode]);
  CARequest.AddParameter('fields.uid', UId, pkGETorPOST, [poDoNotEncode]);
  // กำหนด url ของ api
  CAClient.BaseURL := Concat(ContentfulUrl, '/entries');
  // assige client ให้ request
  CARequest.Client := CAClient;
  // set parameter rmGET,rmPOST,rmPUT,rmDELETE
  CARequest.Method := rmGET;
  // Memo1.Lines.Add(BodyJson.ToJSON);
  // เรียก api
  CARequest.Execute;
  if (CARequest.Response.StatusCode = 200) then
  begin
    var
    ResultJson := CARequest.Response.JSONValue as TJSONObject;
    var
    ItemArray := ResultJson.FindValue('items') as TJSONArray;
    if ItemArray.Count > 0 then
    begin
      var
      profileResult := ResultJson.FindValue('items[0]') as TJSONObject;
      Id := profileResult.FindValue('sys.id').Value;
      Version := StrToInt(profileResult.FindValue('sys.version').Value);
      Firstname := profileResult.FindValue('fields.firstname.en-US').Value;
      Lastname := profileResult.FindValue('fields.lastname.en-US').Value;
      Title := profileResult.FindValue('fields.title.en-US').Value;
      // Log
      if profileResult.FindValue('fields.intro.en-US') <> nil then
      begin
        var
        IntroArray := profileResult.FindValue('fields.intro.en-US.content')
          as TJSONArray;
        var
        i := 0;
        for i := 0 to IntroArray.Count - 1 do
        begin
          var
          IntroObject := IntroArray.Get(i) as TJSONObject;
          Intro := Concat(Intro,
            IntroObject.FindValue('content[0].value').Value);
        end;
      end;
      if profileResult.FindValue('fields.profileimage.en-US.sys.id') <> nil then
      begin
        AssetId := profileResult.FindValue
          ('fields.profileimage.en-US.sys.id').Value;
      end;
      ResultRest.AddPair('status', TJSONBool.Create(True));
      ResultRest.AddPair('result', profileResult as TJSONObject);
    end
    else
    begin
      ResultRest.AddPair('status', TJSONBool.Create(False));
      ResultRest.AddPair('result', 'No profile config');
    end;
  end
  else
  begin
    ResultRest.AddPair('status', TJSONBool.Create(False));
    ResultRest.AddPair('result', CARequest.Response.JSONText);
  end;
  Result := ResultRest;
end;

function TMProfile.GetByToken: TJSONObject;
begin
  var
  ResultRest := TJSONObject.Create;
  var
  CAClient := TRESTClient.Create(nil);
  var
  CARequest := TRESTRequest.Create(nil);
  CARequest.AddParameter('Authorization', 'Bearer ' + CmaToken, pkHTTPHEADER,
    [poDoNotEncode]);
  // New Item
  // กำหนด url ของ api
  CAClient.BaseURL := 'https://api.contentful.com/users/me';
  // assige client ให้ request
  CARequest.Client := CAClient;
  CARequest.Method := rmGET;
  // เรียก api
  CARequest.Execute;
  if (CARequest.Response.StatusCode = 200) then
  begin
    var
    ResultJson := CARequest.Response.JSONValue as TJSONObject;
    Firstname := ResultJson.FindValue('firstName').Value;
    Lastname := ResultJson.FindValue('lastName').Value;
    UId := ResultJson.FindValue('sys.id').Value;
    ResultRest.AddPair('status', TJSONBool.Create(True));
    ResultRest.AddPair('result', ResultJson);
  end
  else
  begin
    ResultRest.AddPair('status', TJSONBool.Create(False));
    ResultRest.AddPair('result', CARequest.Response.JSONText);
  end;
  Result := ResultRest;
end;

function TMProfile.GetFieldsJson: TJSONObject;
begin
  var
  md := TMarkdownProcessor.createDialect(mdDaringFireball);
  md.UnSafe := True;
  var
  FieldObject := TJSONObject.Create;
  FieldObject.AddPair('uid', ModelHelperObject.BuildLanguageString('en', UId));
  FieldObject.AddPair('firstname', ModelHelperObject.BuildLanguageString('en',
    Firstname));
  FieldObject.AddPair('lastname', ModelHelperObject.BuildLanguageString('en',
    Lastname));
  FieldObject.AddPair('title', ModelHelperObject.BuildLanguageString('en',
    Title));
  if Intro <> '' then
    FieldObject.AddPair('intro', ModelHelperObject.BuildLanguageObject('en',
      ModelHelperObject.BuildRichText(Intro)));
  if AssetId <> '' then
    FieldObject.AddPair('profileimage',
      ModelHelperObject.BuildLanguageObject('en',
      ModelHelperObject.BuildLink('Asset', AssetId)));
  var
  ResultObject := TJSONObject.Create;
  ResultObject.AddPair('fields', FieldObject);
  Result := ResultObject;
end;

function TMProfile.Validate: TJSONObject;
begin
  var
  CheckResult := TJSONObject.Create;
  var
  ErrorArray := TJSONArray.Create;
  var
  CheckObject := CValidateHelper.Create;
  Firstname := Trim(Firstname);
  Lastname := Trim(Lastname);
  UId := Trim(UId);
  var
  ResultUId := CheckObject.CheckBlank('UId', UId);
  if ResultUId.FindValue('result').Value.ToBoolean = False then
  begin
    ErrorArray.Add(ResultUId);
  end;
  var
  ResultTitle := CheckObject.CheckBlank('Firstname', Firstname);
  if ResultTitle.FindValue('result').Value.ToBoolean = False then
  begin
    ErrorArray.Add(ResultTitle);
  end;
  var
  ResultSlug := CheckObject.CheckBlank('Lastname', Lastname);
  if ResultSlug.FindValue('result').Value.ToBoolean = False then
  begin
    ErrorArray.Add(ResultSlug);
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

function TMProfile.Save: TJSONObject;
begin
  var
  ResultRest := TJSONObject.Create;
  var
  CAClient := TRESTClient.Create(nil);
  var
  CARequest := TRESTRequest.Create(nil);
  CARequest.AddParameter('Authorization', 'Bearer ' + CmaToken, pkHTTPHEADER,
    [poDoNotEncode]);
  CARequest.AddParameter('Content-Type',
    'application/vnd.contentful.management.v1+json', pkHTTPHEADER,
    [poDoNotEncode]);
  CARequest.AddParameter('X-Contentful-Content-Type', 'spacebarprofile',
    pkHTTPHEADER, [poDoNotEncode]);
  CARequest.AddBody(GetFieldsJson.ToString, ctAPPLICATION_JSON);
  // กำหนด url ของ api
  CAClient.BaseURL := Concat(ContentfulUrl, '/entries');
  // assige client ให้ request
  CARequest.Client := CAClient;
  // set parameter rmGET,rmPOST,rmPUT,rmDELETE
  CARequest.Method := rmPOST;
  // Memo1.Lines.Add(BodyJson.ToJSON);
  // เรียก api
  CARequest.Execute;
  if (CARequest.Response.StatusCode = 201) then
  begin
    var
    cResult := CARequest.Response.JSONValue as TJSONObject;
    Id := cResult.FindValue('sys.id').Value;
    Version := cResult.FindValue('sys.version').Value.ToInteger();
    ResultRest.AddPair('status', TJSONBool.Create(True));
    ResultRest.AddPair('message', 'Update profile complete.');
    ResultRest.AddPair('result', cResult);
  end
  else
  begin
    ResultRest.AddPair('status', TJSONBool.Create(False));
    ResultRest.AddPair('result', CARequest.Response.JSONText);
  end;
  Result := ResultRest;
end;

function TMProfile.Update: TJSONObject;
begin
  var
  ResultRest := TJSONObject.Create;
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
    var
    cResult := CARequest.Response.JSONValue as TJSONObject;
    Version := cResult.FindValue('sys.version').Value.ToInteger();
    ResultRest.AddPair('status', TJSONBool.Create(True));
    ResultRest.AddPair('message', 'Update profile complete.');
    ResultRest.AddPair('result', cResult);
  end
  else
  begin
    ResultRest.AddPair('status', TJSONBool.Create(False));
    ResultRest.AddPair('result', CARequest.Response.JSONText);
  end;
  Result := ResultRest;
//  Result := GetFieldsJson;
end;

end.
