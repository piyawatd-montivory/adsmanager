unit SpacebarProfile;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Ani,
  FMX.Objects, FMX.StdCtrls, FMX.DateTimeCtrls, FMX.ListBox, FMX.Edit,
  FMX.TabControl, FMX.Controls.Presentation, FMX.Layouts, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo,System.JSON,MProfile,ValidateHelper;

type
  TfProfile = class(TForm)
    AccountLabelLayout: TLayout;
    AccountTitleLabel: TLabel;
    Line1: TLine;
    FormLayout: TLayout;
    TitlelLayout: TLayout;
    TitleLabel: TLabel;
    FirstnameTxt: TEdit;
    BrowseLayout: TLayout;
    BrowseLabel: TLabel;
    UploadPhoto: TButton;
    Layout23: TLayout;
    Image12: TImage;
    Label18: TLabel;
    TabConfigBg: TRectangle;
    PreviewImage: TImage;
    OpenDialog1: TOpenDialog;
    BottomLayout: TLayout;
    SaveBtn: TButton;
    Layout15: TLayout;
    Image3: TImage;
    Label10: TLabel;
    CloseBtn: TButton;
    Layout4: TLayout;
    Image1: TImage;
    Label2: TLabel;
    OpenDialogAds: TOpenDialog;
    BGProcess: TRectangle;
    ProcessText: TText;
    ProcessLabelAnimate: TColorAnimation;
    Layout2: TLayout;
    Label6: TLabel;
    Layout3: TLayout;
    Label7: TLabel;
    LastnameTxt: TEdit;
    IntroTxt: TMemo;
    DelImageBtn: TButton;
    Layout1: TLayout;
    Image2: TImage;
    Label1: TLabel;
    procedure SaveBtnClick(Sender: TObject);
    procedure UploadPhotoClick(Sender: TObject);
    procedure DelImageBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    aFile : String;
  public
    { Public declarations }
    ProfileObj : TMProfile;
    AssetId,DeleteAssetId, Id, UId: String;
    Version: Integer;
    ValidateObject: CValidateHelper;
    CmaToken, SpaceId, Environment: String;
    ResultForm: String;
  end;

var
  fProfile: TfProfile;

implementation

uses ContentfulImage;

{$R *.fmx}

procedure TfProfile.DelImageBtnClick(Sender: TObject);
begin
DeleteAssetId := AssetId;
AssetId := '';
DelImageBtn.Enabled:=False;
PreviewImage.Visible:=False;
end;

procedure TfProfile.FormShow(Sender: TObject);
begin
UId := ProfileObj.UId;
Id := ProfileObj.Id;
FirstnameTxt.Text := ProfileObj.Firstname;
LastnameTxt.Text := ProfileObj.Lastname;
IntroTxt.Lines.Text := ProfileObj.Intro;
AssetId := ProfileObj.AssetId;
Version := ProfileObj.Version;
PreviewImage.Bitmap := ProfileObj.ProfilePicture;
end;

procedure TfProfile.SaveBtnClick(Sender: TObject);
var
  Result: TJSONObject;
begin
  Result := TJSONObject.Create;
  ProfileObj.Firstname := FirstnameTxt.Text;
  ProfileObj.Lastname := LastnameTxt.Text;
  ProfileObj.Intro := IntroTxt.Text;
  var
  ValidateResult := ProfileObj.Validate;
  // verify pass
  if ValidateResult.FindValue('result').Value.ToBoolean then
  begin
    //Upload Image if new image upload
    var
      ImgObj := MContentfulImage.Create(CmaToken,SpaceId,Environment);
    if DeleteAssetId <> '' then
    begin
      ImgObj.DeleteImage(DeleteAssetId,ProfileObj.AssetVersion);
      ProfileObj.AssetId := '';
    end;
    if aFile <> '' then
    begin
      var
      FileName := extractfilename(aFile);
      var
      extFile := ExtractFileExt(aFile);
      FileName := StringReplace(FileName, extFile, '', [rfReplaceAll]);
      var
      uploadId := ImgObj.Upload(aFile, FileName, extFile);
      var
      casset := ImgObj.CreateAssets(uploadId, FileName,
        FileName, extFile);
      ImgObj.DeleteImage(AssetId,2);
      var newAssetId := casset.FindValue('sys.id').Value;
      var
      proresult := ImgObj.ProcessAssets(newAssetId);
      ProfileObj.AssetId := newAssetId;
      ProfileObj.AssetVersion := 2;
    end;
    // update
    Result := ProfileObj.Update;
    if Result.FindValue('status').Value.ToBoolean then
    begin
      ShowMessage(Result.FindValue('message').Value);
      ProfileObj.ProfilePicture := PreviewImage.Bitmap;
      ModalResult := mrOk;
    end
    else
    begin
      ShowMessage(Result.FindValue('result').Value);
    end;
  end
  else
  begin
    const
      sLineBreak = {$IFDEF MACOS64} AnsiChar(#10) {$ENDIF} {$IFDEF LINUX} AnsiChar(#10) {$ENDIF} {$IFDEF MSWINDOWS} AnsiString(#13#10) {$ENDIF};
    var
    ListError := '';
    var
    ErrorArray := ValidateResult.FindValue('error') as TJSONArray;
    var
      i: Integer;
    for i := 0 to ErrorArray.Count - 1 do
    begin
      var
      ErrorItem := ErrorArray.Get(i) as TJSONObject;
      ListError := Concat(ListError, ErrorItem.FindValue('message').Value,
        sLineBreak);
    end;
    ShowMessage(ListError);
  end;
end;

procedure TfProfile.UploadPhotoClick(Sender: TObject);
begin
OpenDialog1.Filter := FMX.Graphics.TBitmapCodecManager.GetFilterString;
  if OpenDialog1.Execute then
    if FileExists(OpenDialog1.FileName) then
    begin
      aFile := OpenDialog1.FileName;
      PreviewImage.Bitmap.LoadFromFile(aFile);
    end;
end;

end.
