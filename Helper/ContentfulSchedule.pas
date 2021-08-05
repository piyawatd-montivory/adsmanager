unit ContentfulSchedule;

interface

uses
  System.JSON,System.SysUtils,REST.Client,REST.Types,
  ModelHelper,ValidateHelper,System.Math;

type
  TContentfulSchedule = Class

  private
    ContentfulUrl : String;
    CmaToken: String;
    SpaceId: String;
    Environments : String;
  public
    constructor Create(pCmaToken,pSpaceId,pEnvironments:String);
    function GetAll(ContentId:String):TJSONArray;
    function Save(ContentId,ScheduleType:String;scheduledDatetime:TDateTime):TJSONObject;
    function ScheduleCancel(ContentId, scheduledId: String): TJSONObject;
  End;

implementation

constructor TContentfulSchedule.Create(pCmaToken,pSpaceId,pEnvironments:String);
begin
  CmaToken := pCmaToken;
  SpaceId := pSpaceId;
  Environments := pEnvironments;
  ContentfulUrl := Concat('https://api.contentful.com/spaces/',SpaceId,'/scheduled_actions');
end;

function TContentfulSchedule.GetAll(ContentId:String): TJSONArray;
begin
  var ResultRest := TJSONArray.Create;
  var CAClient := TRESTClient.Create(nil);
  var CARequest := TRESTRequest.Create(nil);
  CARequest.AddParameter('Authorization','Bearer '+CmaToken,pkHTTPHEADER,[poDoNotEncode]);
  CARequest.AddParameter('sys.status','scheduled',pkGETorPOST,[poDoNotEncode]);
  CARequest.AddParameter('order','scheduledFor.datetime',pkGETorPOST,[poDoNotEncode]);
  CARequest.AddParameter('entity.sys.id',ContentId,pkGETorPOST,[poDoNotEncode]);
  CARequest.AddParameter('environment.sys.id',Environments,pkGETorPOST,[poDoNotEncode]);
  // กำหนด url ของ api
  CAClient.BaseURL := ContentfulUrl;
  // assige client ให้ request
  CARequest.Client := CAClient;
  CARequest.Method := rmGET;
  // เรียก api
  CARequest.Execute;
  if (CARequest.Response.StatusCode = 200) then
  begin
    var resultObj := CARequest.Response.JSONValue as TJSONObject;
    ResultRest := resultObj.FindValue('items') as TJSONArray;
  end;
  Result := ResultRest;
end;

function TContentfulSchedule.Save(ContentId,ScheduleType: String;
  scheduledDatetime: TDateTime): TJSONObject;
begin
  var ResultRest := TJSONObject.Create;
  var CAClient := TRESTClient.Create(nil);
  var CARequest := TRESTRequest.Create(nil);
  CARequest.AddParameter('Authorization','Bearer '+CmaToken,pkHTTPHEADER,[poDoNotEncode]);
  CARequest.AddParameter('Content-Type','application/vnd.contentful.management.v1+json',pkHTTPHEADER,[poDoNotEncode]);

  var entrysysvalue := TJSONObject.Create;
  entrysysvalue.AddPair('type','Link');
  entrysysvalue.AddPair('linkType','Entry');
  entrysysvalue.AddPair('id',ContentId);
  var entrysys := TJSONObject.Create;
  entrysys.AddPair('sys',entrysysvalue);
  var envsysvalue := TJSONObject.Create;
  envsysvalue.AddPair('type','Link');
  envsysvalue.AddPair('linkType','Environment');
  envsysvalue.AddPair('id',Environments);
  var envsys := TJSONObject.Create;
  envsys.AddPair('sys',envsysvalue);
  var scheduledforvalue := TJSONObject.Create;
  scheduledforvalue.AddPair('datetime',Concat(FormatDateTime('yyyy-mm-dd', scheduledDatetime),'T',FormatDateTime('hh:nn', scheduledDatetime),':00.000+07:00'));
  scheduledforvalue.AddPair('timezone','Asia/Bangkok');
  var bodyJson := TJSONObject.Create;
  bodyJson.AddPair('entity',entrysys);
  bodyJson.AddPair('environment',envsys);
  bodyJson.AddPair('scheduledFor',scheduledforvalue);
  bodyJson.AddPair('action',ScheduleType);

  CARequest.AddBody(bodyJson.ToString, ctAPPLICATION_JSON);
  // กำหนด url ของ api
  CAClient.BaseURL := Concat(ContentfulUrl,'?entity.sys.id=',ContentId,'&environment.sys.id=',Environments);
  // assige client ให้ request
  CARequest.Client := CAClient;
  // set parameter rmGET,rmPOST,rmPUT,rmDELETE
  CARequest.Method := rmPOST;
  // เรียก api
  CARequest.Execute;
  if (CARequest.Response.StatusCode = 201) then
  begin
    var resultJson := CARequest.Response.JSONValue as TJSONObject;
    ResultRest.AddPair('status',TJSONBool.Create(True));
    ResultRest.AddPair('message','Create category complete.');
    ResultRest.AddPair('id',resultJson.FindValue('sys.id').Value);
  end
  else
  begin
    ResultRest.AddPair('status',TJSONBool.Create(False));
    ResultRest.AddPair('result',CARequest.Response.JSONText);
  end;
  Result:=ResultRest;
end;

function TContentfulSchedule.ScheduleCancel(ContentId,
  scheduledId: String): TJSONObject;
begin
var ResultRest := TJSONObject.Create;
  var CAClient := TRESTClient.Create(nil);
  var CARequest := TRESTRequest.Create(nil);
  CARequest.AddParameter('Authorization','Bearer '+CmaToken,pkHTTPHEADER,[poDoNotEncode]);
  CARequest.AddParameter('Content-Type','application/vnd.contentful.management.v1+json',pkHTTPHEADER,[poDoNotEncode]);
  CARequest.AddParameter('entity.sys.id',ContentId,pkGETorPOST,[poDoNotEncode]);
  CARequest.AddParameter('environment.sys.id',Environments,pkGETorPOST,[poDoNotEncode]);
  // กำหนด url ของ api
  CAClient.BaseURL := Concat(ContentfulUrl,'/',scheduledId);
  // assige client ให้ request
  CARequest.Client := CAClient;
  // set parameter rmGET,rmPOST,rmPUT,rmDELETE
  CARequest.Method := rmDELETE;
  // เรียก api
  CARequest.Execute;
  if (CARequest.Response.StatusCode = 200) then
  begin
    var resultJson := CARequest.Response.JSONValue as TJSONObject;
    ResultRest.AddPair('status',TJSONBool.Create(True));
    ResultRest.AddPair('message','Cancel complete.');
  end
  else
  begin
    ResultRest.AddPair('status',TJSONBool.Create(False));
    ResultRest.AddPair('result',CARequest.Response.JSONText);
  end;
  Result:=ResultRest;
end;

end.
