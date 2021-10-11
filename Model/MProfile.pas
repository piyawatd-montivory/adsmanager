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
    FProfileImage, FAssetId, ContentfulUrl, FCmaToken, SpaceId,
      Environments: String;
    FProfileName:WideString;
    FProfilePicture: TBitmap;
    FVersion, FAssetVersion: Integer;
    FNewsdesk: TJSONArray;
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
    property Newsdesk: TJSONArray read FNewsdesk write FNewsdesk;
    property ProfileName: WideString read FProfileName write FProfileName;
    property CmaToken: String read FCmaToken write FCmaToken;
    function GetFieldsJson: TJSONObject;
    function Save: TJSONObject;
    function Update: TJSONObject;
    function GetByToken: TJSONObject;
    function GetById: TJSONObject;
    function Validate: TJSONObject;
    function Publish: TJSONObject;
  End;

implementation

constructor TMProfile.Create(pCmaToken, pSpaceId, pEnvironments: String);
begin
  ModelHelperObject := TModelHelper.Create;
  FCmaToken := pCmaToken;
  SpaceId := pSpaceId;
  Environments := pEnvironments;
  ContentfulUrl := Concat('https://api.contentful.com/spaces/', SpaceId,
    '/environments/', Environments);
end;

function TMProfile.GetById: TJSONObject;
begin
  var
  pResult := TJSONObject.Create;
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
  // เรียก api
  try
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
      ProfileName := Concat(Firstname,' ',Lastname);
      Title := profileResult.FindValue('fields.title.en-US').Value;
      // Intro
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
      // Newsdesk
      FNewsdesk := TJSONArray.Create;
      if profileResult.FindValue('fields.newsdesk.en-US') <> nil then
      begin
        var
        NewsdeskArray := profileResult.FindValue('fields.newsdesk.en-US')
          as TJSONArray;
        var
        i := 0;
        for i := 0 to NewsdeskArray.Count - 1 do
        begin
          var
          DeskObject := NewsdeskArray.Get(i) as TJSONObject;
          var nObj := TJSONObject.Create;
          nObj.AddPair('id',DeskObject.FindValue('sys.id'));
          FNewsdesk.Add(nObj);
        end;
      end;
      if profileResult.FindValue('fields.profileimage.en-US.sys.id') <> nil then
      begin
        AssetId := profileResult.FindValue
          ('fields.profileimage.en-US.sys.id').Value;
      end;
      pResult.AddPair('status', TJSONBool.Create(True));
      pResult.AddPair('result', profileResult as TJSONObject);
    end
    else
    begin
      pResult.AddPair('status', TJSONBool.Create(False));
      pResult.AddPair('result', 'No profile config');
    end;
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

function TMProfile.GetByToken: TJSONObject;
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
  CAClient.BaseURL := 'https://api.contentful.com/users/me';
  // assige client ให้ request
  CARequest.Client := CAClient;
  CARequest.Method := rmGET;
  // เรียก api
  try
  CARequest.Execute;
  if (CARequest.Response.StatusCode = 200) then
  begin
    var
    ResultJson := CARequest.Response.JSONValue as TJSONObject;
    Firstname := ResultJson.FindValue('firstName').Value;
    Lastname := ResultJson.FindValue('lastName').Value;
    UId := ResultJson.FindValue('sys.id').Value;
    pResult.AddPair('status', TJSONBool.Create(True));
    pResult.AddPair('result', ResultJson);
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
  //News Desk
  if Newsdesk.Count > 0 then
  begin
    var newdeskArray := TJSONArray.Create;
    var i := 0;
    for i := 0 to Newsdesk.Count - 1 do
    begin
      var itemObj := Newsdesk.Get(i) as TJSONObject;
      var deskObj := TJSONObject.Create;
      deskObj.AddPair('type','Link');
      deskObj.AddPair('linkType','Entry');
      deskObj.AddPair('id',itemObj.FindValue('id').Value);
      var sysObj := TJSONObject.Create;
      sysObj.AddPair('sys',deskObj);
      newdeskArray.Add(sysObj);
    end;
    FieldObject.AddPair('newsdesk',ModelHelperObject.BuildLanguageJsonArray('en',newdeskArray));
  end;
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

function TMProfile.Publish: TJSONObject;
begin
    var pResult := TJSONObject.Create;
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
    try
    CPARequest.Execute;
    if CPARequest.Response.StatusCode = 200 then
    begin
      var
      cResult := CPARequest.Response.JSONValue as TJSONObject;
      Version := cResult.FindValue('sys.version').Value.ToInteger();
      pResult.AddPair('status',TJSONBool.Create(True));
      pResult.AddPair('result',cResult);
    end
    else
    begin
      pResult.AddPair('status',TJSONBool.Create(False));
      pResult.AddPair('result',CPARequest.Response.JSONText);
    end;
    finally
      CPAClient.Free;
    end;
    Result := pResult;
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
  try
  CARequest.Execute;
  if (CARequest.Response.StatusCode = 201) then
  begin
    var
    cResult := CARequest.Response.JSONValue as TJSONObject;
    Id := cResult.FindValue('sys.id').Value;
    Version := cResult.FindValue('sys.version').Value.ToInteger();
    pResult.AddPair('status', TJSONBool.Create(True));
    pResult.AddPair('message', 'Update profile complete.');
    pResult.AddPair('result', cResult);
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

function TMProfile.Update: TJSONObject;
begin
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
  // เรียก api
  try
  CARequest.Execute;
  if (CARequest.Response.StatusCode = 200) then
  begin
    var
    cResult := CARequest.Response.JSONValue as TJSONObject;
    Version := cResult.FindValue('sys.version').Value.ToInteger();
    pResult.AddPair('status', TJSONBool.Create(True));
    pResult.AddPair('message', 'Update profile complete.');
    pResult.AddPair('result', cResult);
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

end.
