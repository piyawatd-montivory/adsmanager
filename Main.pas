unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, SubjectStand,
  FrameStand, FMX.StdCtrls, FMX.Objects, FMX.MultiView, FMX.Ani, FMX.Layouts,
  FMX.Controls.Presentation,
  {$IFDEF MSWINDOWS}
  System.Win.Registry,
  {$ENDIF}
  AdsLists,System.JSON,REST.Types,REST.Client, FMX.Memo.Types, FMX.ScrollBox,
  FMX.Memo, FMX.DateTimeCtrls,System.DateUtils,MProfile, FMX.WebBrowser,
  FMX.ListBox;

type
  TMainApplication = class(TForm)
    ToolBar1: TToolBar;
    Rectangle1: TRectangle;
    MenuBtn: TSpeedButton;
    Image1: TImage;
    Button1: TButton;
    LogBtn: TButton;
    MainLayout: TLayout;
    DefaultImg: TImage;
    BGProcess: TRectangle;
    ProcessText: TText;
    ProcessLabelAnimate: TColorAnimation;
    MultiViewMenu: TMultiView;
    Rectangle2: TRectangle;
    ProfileLayout: TLayout;
    ProfileImageLayout: TLayout;
    SProfileImage: TCircle;
    ProfileName: TLabel;
    TitleLabel: TLabel;
    AdsMenuBtn: TButton;
    Image7: TImage;
    Label6: TLabel;
    AppConfigBtn: TButton;
    Image9: TImage;
    Label8: TLabel;
    LoginMenuBtn: TButton;
    Image5: TImage;
    Label4: TLabel;
    TabConfigBg: TRectangle;
    Label11: TLabel;
    Rectangle3: TRectangle;
    Label9: TLabel;
    ProfileBtn: TButton;
    RegisterBtn: TButton;
    FrameStand1: TFrameStand;
    Button2: TButton;
    Button3: TButton;
    Label1: TLabel;
    PositionCbo: TComboBox;
    AllPosition: TListBoxItem;
    IndexPosition1: TListBoxItem;
    IndexPosition2: TListBoxItem;
    IndexPosition3: TListBoxItem;
    IndexPosition4: TListBoxItem;
    IndexPosition5: TListBoxItem;
    IndexPosition6: TListBoxItem;
    CategoryPosition1: TListBoxItem;
    CategoryPosition2: TListBoxItem;
    ArticlePosition1: TListBoxItem;
    ArticlePosition2: TListBoxItem;
    Button4: TButton;
    procedure AdsMenuBtnClick(Sender: TObject);
    procedure AppConfigBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure LoginMenuBtnClick(Sender: TObject);
    procedure SProfileImageClick(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
    FFrameAdsLists: TFrameInfo<TfAdsLists>;
    {$IFDEF MACOS}
    procedure _LoadFrame(Value: String);
    procedure _OpenLogin;
    procedure _OpenConfig;
    procedure _GetProfile;
    {$ENDIF}
    {$IFDEF MSWINDOWS}
    procedure LoadFrame(Value: String);
    procedure OpenLogin;
    procedure OpenConfig;
    procedure SetPermissions;
    procedure GetProfile;
    {$ENDIF}
  public
    { Public declarations }
    SpaceId, CmaToken, Environment, DefaultTagToken, TagToken: String;
    ProfileObj:TMProfile;
    ImageWidthLimit: Integer;
  end;

var
  MainApplication: TMainApplication;

implementation

uses FApplicationConfig,AdsForm,ContentfulVideo,DesktopLogin,ContentfulImage,SpacebarProfile;

{$R *.fmx}

procedure TMainApplication.AdsMenuBtnClick(Sender: TObject);
begin
{$IFDEF MACOS}
_LoadFrame('Ads');
{$ENDIF}
{$IFDEF MSWINDOWS}
LoadFrame('Ads');
{$ENDIF}
end;


procedure TMainApplication.AppConfigBtnClick(Sender: TObject);
begin
{$IFDEF MACOS}
        _OpenConfig;
{$ENDIF}
{$IFDEF MSWINDOWS}
        OpenConfig;
{$ENDIF}
end;

{$IFDEF MACOS}
  procedure TMainApplication._OpenConfig;
{$ENDIF}
{$IFDEF MSWINDOWS}
    procedure TMainApplication.OpenConfig;
{$ENDIF}
    begin
      var
      AppConfig := TApplicationConfig.Create(nil);
      if AppConfig.ShowModal = mrOK then
      begin
          SpaceId := AppConfig.SpaceTxt.Text;
          Environment := AppConfig.EnvCbo.Selected.Text;
          ImageWidthLimit := AppConfig.ImageWidth.Value.ToString.ToInteger();
      end;
    end;

procedure TMainApplication.Button2Click(Sender: TObject);
begin
  var
  FormAds := TfAdsForm.Create(nil);
  FormAds.CmaToken := CmaToken;
  FormAds.SpaceId := SpaceId;
  FormAds.Environment := Environment;
  FormAds.ImageWidthLimit := 1920;
  FormAds.PublishStatus := False;
//  FormAds.AdsId := '5FzPpXvHDKXl1upBswGWHy';
  FormAds.PublishStatus := False;
  if FormAds.ShowModal = mrOk then
end;

procedure TMainApplication.Button3Click(Sender: TObject);
begin
var
  FormAds := TfAdsForm.Create(nil);
  FormAds.CmaToken := CmaToken;
  FormAds.SpaceId := SpaceId;
  FormAds.Environment := Environment;
  FormAds.ImageWidthLimit := ImageWidthLimit;
  FormAds.PublishStatus := True;
  FormAds.AdsId:='52ZX9gePyIlxMaOqdJorsP';
  if FormAds.ShowModal = mrOk then
end;

procedure TMainApplication.Button4Click(Sender: TObject);
begin
ShowMessage(PositionCbo.Selected.Index.ToString);
end;

procedure TMainApplication.FormCreate(Sender: TObject);
      var
        i: Integer;
        ConfigFile: String;
      begin
{$IFDEF MACOS}
        ConfigFile := Concat(GetHomePath, '/spacebar/configads.ini');
{$ELSE}
        var
        ProjectPath := GetCurrentDir;
        ProjectPath := StringReplace(ProjectPath, '\Win64\Debug', '',
          [rfReplaceAll, rfIgnoreCase]);
        ConfigFile := Concat(ProjectPath, '\configads.ini');
{$ENDIF}
        ImageWidthLimit := 1920;
        if fileexists(ConfigFile) then
        begin
          var
          ConfigList := TStringList.Create;
          ConfigList.LoadFromFile(ConfigFile);
          for i := 0 to ConfigList.Count - 1 do
          begin
            var
            ConfigData := TStringList.Create;
            ConfigData.Delimiter := '=';
            ConfigData.DelimitedText := ConfigList[i];
            if ConfigData[0] = 'spaceid' then
              SpaceId := ConfigData[1];
            if ConfigData[0] = 'environment' then
              Environment := ConfigData[1];
            if ConfigData[0] = 'imagewidth' then
              ImageWidthLimit := ConfigData[1].ToInteger();
          end;
        end
        else
        begin
{$IFDEF MACOS}
          _OpenConfig;
{$ENDIF}
{$IFDEF MSWINDOWS}
          OpenConfig;
{$ENDIF}
        end;
        // SpaceId := 'e7ee8kzf5re6';
        // Environment := 'master';
        CmaToken := 'CFPAT-DTNsQqLMS0flsOmUKPsVrZIkwPfC_2jCkpXA9cV9cfw';
        DefaultTagToken := 'CFPAT-DTNsQqLMS0flsOmUKPsVrZIkwPfC_2jCkpXA9cV9cfw';
        // CmaToken := 'jbR9iaBYqp1UnuuHr9zDc6kspqIRzfvHCdm_cOFRMP4';
{$IFDEF MSWINDOWS}
        SetPermissions;
{$ENDIF}
end;

{$IFDEF MSWINDOWS}
      procedure TMainApplication.SetPermissions;

      const
        cHomePath = 'SOFTWARE';
        cFeatureBrowserEmulation =
          'Microsoft\Internet Explorer\Main\FeatureControl\FEATURE_BROWSER_EMULATION\';
        cIE11 = 11001;

      var
        Reg: TRegIniFile;
        sKey: string;
      begin

        sKey := ExtractFileName(ParamStr(0));
        Reg := TRegIniFile.Create(cHomePath);
        try
          if Reg.OpenKey(cFeatureBrowserEmulation, true) and
            not(TRegistry(Reg).KeyExists(sKey) and
            (TRegistry(Reg).ReadInteger(sKey) = cIE11)) then
            TRegistry(Reg).WriteInteger(sKey, cIE11);
        finally
          Reg.Free;
        end;

      end;
{$ENDIF}

procedure TMainApplication.SProfileImageClick(Sender: TObject);
begin
        ProfileObj.ProfilePicture := SProfileImage.Fill.Bitmap.Bitmap;
        var
        ProfileUpdateForm := TfProfile.Create(nil);
        ProfileUpdateForm.CmaToken := CmaToken;
        ProfileUpdateForm.SpaceId := SpaceId;
        ProfileUpdateForm.Environment := Environment;
        ProfileUpdateForm.ProfileObj := ProfileObj;
        if ProfileObj.AssetId <> '' then
        begin
          ProfileUpdateForm.DelImageBtn.Enabled := true;
        end;
        if ProfileUpdateForm.ShowModal = mrOK then
        begin
          ProfileObj := ProfileUpdateForm.ProfileObj;
          if ProfileObj.AssetId <> '' then
          begin
            SProfileImage.Fill.Bitmap.Bitmap := ProfileObj.ProfilePicture;
          end
          else
          begin
            SProfileImage.Fill.Bitmap.Bitmap := DefaultImg.Bitmap;
          end;
          ProfileName.Text := Concat(ProfileObj.Firstname, ' ',
            ProfileObj.Lastname);
        end;
end;


{$IFDEF MACOS}
procedure TMainApplication._LoadFrame(Value: String);
{$ENDIF}
{$IFDEF MSWINDOWS}
procedure TMainApplication.LoadFrame(Value: String);
{$ENDIF}
  begin
    FrameStand1.HideAndCloseAll;
    if Value = 'test' then
    begin
//      FFrameTestFrame := FrameStand1.New<TTestFrame>(MainLayout);
//      FFrameTestFrame.Frame.CmaToken := CmaToken;
//      FFrameTestFrame.Frame.SpaceId := SpaceId;
//      FFrameTestFrame.Frame.Environment := Environment;
//      FFrameTestFrame.Frame.LoginName := 'test';
//      FFrameTestFrame.Frame.UserId := 'uid';
//      FFrameTestFrame.Show();

//          FFrameEditorImage := FrameStand1.New<TEditorImage>(MainLayout);
//          FFrameEditorImage.Frame.CmaToken := CmaToken;
//          FFrameEditorImage.Frame.SpaceId := SpaceId;
//          FFrameEditorImage.Frame.Environment := Environment;
//          FFrameEditorImage.Frame.Page := 1;
//          FFrameEditorImage.Frame.Limit := 20;
//          FFrameEditorImage.Frame.GetList;
//          FFrameEditorImage.Frame.LoginName := ProfileName.Text;
//          FFrameEditorImage.Frame.UserId := ProfileObj.UId;
//          FFrameEditorImage.Show();
//          FFrameEditorImage.Frame.SetSize;
    end
    else
    begin
      if ProfileObj <> nil then
      begin
        FrameStand1.HideAndCloseAll;
        if Value = 'Ads' then
        begin
          FFrameAdsLists := FrameStand1.New<TfAdsLists>(MainLayout);
          FFrameAdsLists.Frame.CmaToken := CmaToken;
          FFrameAdsLists.Frame.SpaceId := SpaceId;
          FFrameAdsLists.Frame.Environment := Environment;
          FFrameAdsLists.Frame.Page := 1;
          FFrameAdsLists.Frame.Limit := 20;
          FFrameAdsLists.Frame.ImageWidthLimit := ImageWidthLimit;
          FFrameAdsLists.Frame.GetLists;
          FFrameAdsLists.Show();
          FFrameAdsLists.Frame.SetSize;
        end;
//        if Value = 'EditorContent' then
//        begin
//          FFrameEditorContent := FrameStand1.New<TEditorContent>(MainLayout);
//          FFrameEditorContent.Frame.CmaToken := CmaToken;
//          FFrameEditorContent.Frame.SpaceId := SpaceId;
//          FFrameEditorContent.Frame.Environment := Environment;
//          FFrameEditorContent.Frame.Page := 1;
//          FFrameEditorContent.Frame.Limit := 20;
//          FFrameEditorContent.Frame.LoginName := ProfileName.Text;
//          FFrameEditorContent.Frame.UserId := ProfileObj.UId;
//          FFrameEditorContent.Frame.ImageWidthLimit := ImageWidthLimit;
//          FFrameEditorContent.Frame.GetList;
//          FFrameEditorContent.Show();
//          FFrameEditorContent.Frame.SetSize;
//        end;
//        if Value = 'EditorImage' then
//        begin
//          FFrameEditorImage := FrameStand1.New<TEditorImage>(MainLayout);
//          FFrameEditorImage.Frame.CmaToken := CmaToken;
//          FFrameEditorImage.Frame.SpaceId := SpaceId;
//          FFrameEditorImage.Frame.Environment := Environment;
//          FFrameEditorImage.Frame.Page := 1;
//          FFrameEditorImage.Frame.Limit := 20;
//          FFrameEditorImage.Frame.GetList;
//          FFrameEditorImage.Frame.LoginName := ProfileName.Text;
//          FFrameEditorImage.Frame.UserId := ProfileObj.UId;
//          FFrameEditorImage.Show();
//          FFrameEditorImage.Frame.SetSize;
//        end;
//        if Value = 'Tags' then
//        begin
//          FFrameTagsList := FrameStand1.New<TTagsList>(MainLayout);
//          FFrameTagsList.Frame.CmaToken := CmaToken;
//          FFrameTagsList.Frame.SpaceId := SpaceId;
//          FFrameTagsList.Frame.Environment := Environment;
//          FFrameTagsList.Frame.Page := 1;
//          FFrameTagsList.Frame.Limit := 20;
//          FFrameTagsList.Frame.GetTags;
//          FFrameTagsList.Show();
//          FFrameTagsList.Frame.SetSize;
//        end;
//        if Value = 'Category' then
//        begin
//          FFrameCategoryList := FrameStand1.New<TCategoryList>(MainLayout);
//          FFrameCategoryList.Frame.CmaToken := CmaToken;
//          FFrameCategoryList.Frame.SpaceId := SpaceId;
//          FFrameCategoryList.Frame.Environment := Environment;
//          FFrameCategoryList.Frame.Page := 1;
//          FFrameCategoryList.Frame.Limit := 20;
//{$IFDEF MACOS}
//          FFrameCategoryList.Frame._GetCategory;
//{$ENDIF}
//{$IFDEF MSWINDOWS}
//          FFrameCategoryList.Frame.GetCategory;
//{$ENDIF}
//          FFrameCategoryList.Show();
//          FFrameCategoryList.Frame.SetSize;
//        end;
//        if Value = 'Images' then
//        begin
//          FFrameImageList := FrameStand1.New<TImgList>(MainLayout);
//          FFrameImageList.Frame.CmaToken := CmaToken;
//          FFrameImageList.Frame.SpaceId := SpaceId;
//          FFrameImageList.Frame.Environment := Environment;
//          FFrameImageList.Frame.UserId := ProfileObj.UId;
//          FFrameImageList.Frame.UserTitle := ProfileObj.Title;
//          FFrameImageList.Frame.Page := 1;
//          FFrameImageList.Frame.Limit := 9;
//          FFrameImageList.Frame.ImageWidthLimit := ImageWidthLimit;
//          FFrameImageList.Frame.GetImage;
//          FFrameImageList.Show();
//          FFrameImageList.Frame.SetSize;
//        end;
//        MultiViewMenu.HideMaster;
      end
      else
      begin
{$IFDEF MACOS}
        _OpenLogin;
{$ENDIF}
{$IFDEF MSWINDOWS}
        OpenLogin;
{$ENDIF}
      end;
    end;
  end;

procedure TMainApplication.LoginMenuBtnClick(Sender: TObject);
begin
{$IFDEF MACOS}
    _OpenLogin;
{$ENDIF}
{$IFDEF MSWINDOWS}
    OpenLogin;
{$ENDIF}
end;

{$IFDEF MACOS}
    procedure TMainApplication._OpenLogin;
{$ENDIF}
{$IFDEF MSWINDOWS}
      procedure TMainApplication.OpenLogin;
{$ENDIF}
      begin
        var
        FormLogin := TFDesktopLogin.Create(nil);
        if FormLogin.ShowModal = mrOK then
        begin
          CmaToken := FormLogin.AccessToken;
          TagToken := CmaToken;
          // TokenShow.Text := FormLogin.AccessToken;
          {$IFDEF MACOS}
          _GetProfile;
          {$ENDIF}
          {$IFDEF MSWINDOWS}
          GetProfile;
          {$ENDIF}
        end;
      end;

{$IFDEF MACOS}
procedure TMainApplication._GetProfile;
{$ENDIF}
{$IFDEF MSWINDOWS}
procedure TMainApplication.GetProfile;
{$ENDIF}
begin
// Get Profile
        ProfileObj := TMProfile.Create(CmaToken, SpaceId, Environment);
        var
        ProfileResult := ProfileObj.GetByToken;
        ProfileResult := ProfileObj.GetById;
        if ProfileResult.FindValue('status').Value.ToBoolean = False then
        begin
          ProfileObj.Title := 'reporter';
          var
          saveResult := ProfileObj.Save;
        end
        else
        begin
          if ProfileObj.AssetId <> '' then
          begin
            var
            vObj := MContentfulImage.Create(CmaToken, SpaceId, Environment);
            var
            MS := vObj.GetById(ProfileObj.AssetId);
            ProfileObj.AssetVersion := vObj.AssetVersion;
            SProfileImage.Fill.Bitmap.Bitmap.LoadFromStream(MS);
          end;
        end;
        ProfileName.Text := Concat(ProfileObj.Firstname, ' ',
          ProfileObj.Lastname);
        TitleLabel.Text := ProfileObj.Title;
        //Menu Control
        ProfileLayout.Visible := true;
        LoginMenuBtn.Height := 0;
end;

end.
