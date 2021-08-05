unit DesktopLogin;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.WebBrowser;

type
  TFDesktopLogin = class(TForm)
    WebLogin: TWebBrowser;
    procedure FormActivate(Sender: TObject);
    procedure WebLoginDidFinishLoad(ASender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    AccessToken:String;
  end;

var
  FDesktopLogin: TFDesktopLogin;

implementation

{$R *.fmx}

procedure TFDesktopLogin.FormActivate(Sender: TObject);
begin
var LURL := 'https://be.contentful.com/oauth/authorize?response_type=token&client_id=aXf4kYlyn7cQlrb-cBG6UuiF2vapwfJ8Mx6soxEWJb4&redirect_uri=https://demo.spacebar.co.th&scope=content_management_manage';
WebLogin.Navigate(LURL);
end;

procedure TFDesktopLogin.WebLoginDidFinishLoad(ASender: TObject);
begin
var webB := ASender as TWebBrowser;
var checkUrl := webB.URL;
var LatPos := Pos('access_token=',checkUrl);
if LatPos > 0 then
begin
  var StPos := Pos('access_token=',checkUrl)+13;
  var EnPos := Pos('&token_type',checkUrl);
  AccessToken:=Copy(checkUrl,StPos,EnPos-StPos);
  ModalResult:=mrOK;
end;
end;

end.
