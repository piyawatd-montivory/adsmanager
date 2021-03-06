unit AdsForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation, FMX.Layouts,
  FMX.TabControl,
  FMX.Memo.Types, FMX.ListBox, FMX.ScrollBox, FMX.Memo, FMX.Media, System.JSON,
  FMX.DateTimeCtrls, FMX.Ani, System.DateUtils, System.Threading, MAds;

type
  TfAdsForm = class(TForm)
    AccountLabelLayout: TLayout;
    AccountTitleLabel: TLabel;
    Line1: TLine;
    FormLayout: TLayout;
    TitlelLayout: TLayout;
    TitleLabel: TLabel;
    TitleTxt: TEdit;
    BrowseLayout: TLayout;
    BrowseLabel: TLabel;
    UploadPhoto: TButton;
    PhotoMediaLayout: TLayout;
    TabConfigBg: TRectangle;
    PreviewImage: TImage;
    PhotoDetailLabel: TLabel;
    BottomLayout: TLayout;
    SaveBtn: TButton;
    Image3: TImage;
    Label10: TLabel;
    CloseBtn: TButton;
    Image1: TImage;
    Label2: TLabel;
    OpenDialogAdsVideo: TOpenDialog;
    Layout23: TLayout;
    Image12: TImage;
    Label18: TLabel;
    LinkLayout: TLayout;
    LinkLabel: TLabel;
    LinkTxt: TEdit;
    PositionLayout: TLayout;
    Label4: TLabel;
    SlugLayout: TLayout;
    SlugLabel: TLabel;
    SlugTxt: TEdit;
    TypeLayout: TLayout;
    TypeLabel: TLabel;
    TabControl1: TTabControl;
    DetailTab: TTabItem;
    MediaTab: TTabItem;
    VdoLabel: TLabel;
    TypeCbo: TComboBox;
    PositionCbo: TComboBox;
    VdoTxt: TEdit;
    ButtonVdo: TButton;
    Layout1: TLayout;
    Image2: TImage;
    Label1: TLabel;
    OpenDialogAds: TOpenDialog;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    IndexPosition1: TListBoxItem;
    IndexPosition2: TListBoxItem;
    IndexPosition3: TListBoxItem;
    StartLayout: TLayout;
    Label5: TLabel;
    AdsstartdateTxt: TDateEdit;
    AdsstarttimeTxt: TTimeEdit;
    EndLayout: TLayout;
    Label3: TLabel;
    AdsenddateTxt: TDateEdit;
    AdsendtimeTxt: TTimeEdit;
    BGProcess: TRectangle;
    ProcessText: TText;
    ProcessLabelAnimate: TColorAnimation;
    IndexPosition4: TListBoxItem;
    IndexPosition5: TListBoxItem;
    IndexPosition6: TListBoxItem;
    CategoryPosition1: TListBoxItem;
    CategoryPosition2: TListBoxItem;
    ArticlePosition1: TListBoxItem;
    ArticlePosition2: TListBoxItem;
    AdsSizeLabel: TLabel;
    procedure UploadPhotoClick(Sender: TObject);
    procedure ButtonVdoClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PositionCboChange(Sender: TObject);
    procedure SlugTxtExit(Sender: TObject);
  private
    { Private declarations }
    aAdsImage, aAdsVdo, AcLog: WideString;
    delAdsImageId, delAdsVdoId, AdsImageId, AdsVdoId, CurrentMediaType: String;
    AdsImageVersion, AdsVdoVersion, AView, AClick, Ratio: Integer;
    FormOpen: Boolean;
    ContentObj: TMAds;
    procedure ProcessContentful;
  public
    { Public declarations }
    CmaToken, SpaceId, Environment, AdsId, ResultForm: String;
    UploadType: WideString;
    PublishStatus: Boolean;
    ImageWidthLimit, Version: Integer;
    procedure ChangeAdsLabel;
    function CreateDateTime(pDateinput: String; pTimeinput: TDateTime)
      : TDateTime;
  end;

var
  fAdsForm: TfAdsForm;

implementation

uses ContentfulVideo, ContentfulImage, ContentfulSchedule, DDateTime, ValidateHelper;

{$R *.fmx}

function TfAdsForm.CreateDateTime(pDateinput: String; pTimeinput: TDateTime)
  : TDateTime;
begin
  var
  sYear := StrToInt(Copy(pDateinput, 7, 4));
  var
  sMonth := StrToInt(Copy(pDateinput, 4, 2));
  var
  sDay := StrToInt(Copy(pDateinput, 1, 2));
  var
  sHour := FormatDateTime('hh', pTimeinput).ToInteger;
  var
  sMinute := FormatDateTime('nn', pTimeinput).ToInteger;
  // var sSecond := 00;
  // var sMillisecond := Round(0000);
  Result := EncodeDateTime(sYear, sMonth, sDay, sHour, sMinute, 00,
    Round(0000));
end;

procedure TfAdsForm.ChangeAdsLabel;
begin
  var
  labelList := TStringList.Create;
  labelList.Add('Ads ขนาด 970 * 250 pixel');
  labelList.Add('Ads ขนาด 336 * 280 pixel');
  labelList.Add('Ads ขนาด 336 * 280 pixel');
  labelList.Add('Ads ขนาด 336 * 280 pixel');
  labelList.Add('Ads ขนาด 970 * 250 pixel');
  labelList.Add('Ads ขนาด 300 * 600 pixel');
  labelList.Add('Ads ขนาด 970 * 250 pixel');
  labelList.Add('Ads ขนาด 336 * 280 pixel');
  labelList.Add('Ads ขนาด 728 * 90 pixel');
  labelList.Add('Ads ขนาด 336 * 280 pixel');
  AdsSizeLabel.Text := labelList[PositionCbo.Selected.Index];
end;

procedure TfAdsForm.FormCreate(Sender: TObject);
begin
  FormOpen := False;
  AView := 0;
  AClick := 0;
  Ratio := 0;
  AdsstartdateTxt.DateTime := now;
  AdsstarttimeTxt.DateTime := now;
  AdsenddateTxt.DateTime := now;
  AdsendtimeTxt.DateTime := now;
end;

procedure TfAdsForm.FormShow(Sender: TObject);
begin
  if FormOpen = False then
  begin
    if AdsId <> '' then
    begin
      var
      ConObj := TMAds.Create(CmaToken, SpaceId, Environment);
      var
      Result := ConObj.GetById(AdsId);
      AcLog := ConObj.AcLog;
      Version := ConObj.Version;
      AView := ConObj.AView;
      AClick := ConObj.AClick;
      Ratio := ConObj.Ratio;
      CurrentMediaType := ConObj.AType;
      if ConObj.AType = 'Photo' then
      begin
        TypeCbo.Selected.Index := 0;
        AdsImageId := ConObj.AdsmediaId;
        AdsImageVersion := ConObj.AdsmediaVersion;
        var
        vObj := MContentfulImage.Create(CmaToken, SpaceId, Environment);
        var
        MS := vObj.LoadImage(ConObj.Adsmedia);
        PreviewImage.Bitmap.LoadFromStream(MS);
        vObj.Free;
      end
      else
      begin
        TypeCbo.Selected.Index := 1;
        AdsVdoId := ConObj.AdsmediaId;
        AdsVdoVersion := ConObj.AdsmediaVersion;
        VdoTxt.Text := ConObj.Adsmedia;
      end;
      TitleTxt.Text := ConObj.Title;
      SlugTxt.Text := ConObj.Slug;
      SlugTxt.ReadOnly := True;
      LinkTxt.Text := ConObj.Link;
      var
      dateObj := CDateTime.Create;
      var
      adsstarto := dateObj.ContentfulStrToDatetime(ConObj.Adsstart);
      var
      adsendo := dateObj.ContentfulStrToDatetime(ConObj.Adsend);
      AdsstartdateTxt.DateTime := adsstarto;
      AdsstarttimeTxt.DateTime := adsstarto;
      AdsenddateTxt.DateTime := adsendo;
      AdsendtimeTxt.DateTime := adsendo;
      // Position
      if ConObj.Position = 'Index-Position1' then
        PositionCbo.ItemIndex := 0;
      if ConObj.Position = 'Index-Position2' then
        PositionCbo.ItemIndex := 1;
      if ConObj.Position = 'Index-Position3' then
        PositionCbo.ItemIndex := 2;
      if ConObj.Position = 'Index-Position4' then
        PositionCbo.ItemIndex := 3;
      if ConObj.Position = 'Index-Position5' then
        PositionCbo.ItemIndex := 4;
      if ConObj.Position = 'Index-Position6' then
        PositionCbo.ItemIndex := 5;
      if ConObj.Position = 'Category-Position1' then
        PositionCbo.ItemIndex := 6;
      if ConObj.Position = 'Category-Position2' then
        PositionCbo.ItemIndex := 7;
      if ConObj.Position = 'Article-Position1' then
        PositionCbo.ItemIndex := 8;
      if ConObj.Position = 'Article-Position2' then
        PositionCbo.ItemIndex := 9;
      ChangeAdsLabel;
    end;
  end;
end;

procedure TfAdsForm.UploadPhotoClick(Sender: TObject);
begin
  UploadType := 'photo';
  if OpenDialogAds.Execute then
    if FileExists(OpenDialogAds.FileName) then
    begin
      var
      checkSizeImg := TBitmap.Create;
      checkSizeImg.LoadFromFile(OpenDialogAds.FileName);
      if checkSizeImg.Width > ImageWidthLimit then
      begin
        ShowMessage('Image Max Width ' + ImageWidthLimit.ToString + ' px.');
        exit
      end;
      aAdsImage := OpenDialogAds.FileName;
      if CurrentMediaType = 'Photo' then
      begin
        if delAdsImageId = '' then
        begin
          delAdsImageId := AdsImageId;
          AdsImageId := '';
        end;
        PreviewImage.Visible := True;
      end
      else
      begin
        if delAdsVdoId = '' then
        begin
          delAdsVdoId := AdsVdoId;
          AdsVdoId := '';
        end;
      end;
      PreviewImage.Bitmap.LoadFromFile(aAdsImage);
    end;
end;

procedure TfAdsForm.ButtonVdoClick(Sender: TObject);
begin
  UploadType := 'video';
  if OpenDialogAdsVideo.Execute then
    if FileExists(OpenDialogAdsVideo.FileName) then
    begin
      var
      aAdsVdo := OpenDialogAdsVideo.FileName;
      if CurrentMediaType = 'Photo' then
      begin
        if delAdsImageId = '' then
        begin
          delAdsImageId := AdsImageId;
          AdsImageId := '';
        end;
        PreviewImage.Visible := False;
      end
      else
      begin
        if delAdsVdoId = '' then
        begin
          delAdsVdoId := AdsVdoId;
          AdsVdoId := '';
        end;
      end;
      VdoTxt.Text := OpenDialogAdsVideo.FileName;
    end;
end;

procedure TfAdsForm.PositionCboChange(Sender: TObject);
begin
  ChangeAdsLabel;
end;

procedure TfAdsForm.SaveBtnClick(Sender: TObject);
var
  aTask: ITask;
begin
  var
  timepass := True;
  // Create Ads Start Date
  var
  CAdsStart := CreateDateTime(AdsstartdateTxt.Text, AdsstarttimeTxt.DateTime);
  // Create Ads End Date
  var
  CAdsEnd := CreateDateTime(AdsenddateTxt.Text, AdsendtimeTxt.DateTime);
  if CompareDateTime(CAdsStart, CAdsEnd) >= 0 then
  begin
    ShowMessage('Ads Start must before Ads End');
    exit;
  end;
  // check date time old ads
  var
  positionlist := TStringList.Create;
  positionlist.Add('Index-Position1');
  positionlist.Add('Index-Position2');
  positionlist.Add('Index-Position3');
  positionlist.Add('Index-Position4');
  positionlist.Add('Index-Position5');
  positionlist.Add('Index-Position6');
  positionlist.Add('Category-Position1');
  positionlist.Add('Category-Position2');
  positionlist.Add('Article-Position1');
  positionlist.Add('Article-Position2');
  var
  CheckAdsObj := TMAds.Create(CmaToken, SpaceId, Environment);
  var
  AdsListResult := CheckAdsObj.GetList(positionlist[PositionCbo.Selected.Index],AdsId);
  if AdsListResult.FindValue('status').Value.ToBoolean then
  begin
    var
    timeObj := CDateTime.Create;
    var
    resultArray := AdsListResult.FindValue('result.items') as TJSONArray;
    var
    i := 0;
    for i := 0 to resultArray.Count - 1 do
    begin
      var
      ItemObject := TJSONObject(resultArray.Get(i));
      if ItemObject.FindValue('sys.archivedVersion') = nil then
      begin
        var
        adsstarto := timeObj.ContentfulStrToDatetime
          (ItemObject.FindValue('fields.adsstart.en-US').Value);
        var
        adsendo := timeObj.ContentfulStrToDatetime
          (ItemObject.FindValue('fields.adsend.en-US').Value);
        if DateTimeInRange(CAdsStart, adsstarto, adsendo) then
        begin
          timepass := False;
          break;
        end;
        if DateTimeInRange(CAdsEnd, adsstarto, adsendo) then
        begin
          timepass := False;
          break;
        end;
        if DateTimeInRange(adsstarto, CAdsStart, CAdsEnd) then
        begin
          timepass := False;
          break;
        end;
        if DateTimeInRange(adsendo, CAdsStart, CAdsEnd) then
        begin
          timepass := False;
          break;
        end;
      end;
    end;
    timeObj.Free;
    resultArray.Free;
  end;
  CheckAdsObj.Free;
  if timepass = False then
  begin
    ShowMessage('Ads time is overlap.');
    exit;
  end;
  ContentObj := TMAds.Create(CmaToken, SpaceId, Environment);
  ContentObj.Title := TitleTxt.Text;
  ContentObj.Slug := SlugTxt.Text;
  ContentObj.Link := LinkTxt.Text;
  ContentObj.AType := TypeCbo.Selected.Text;
  ContentObj.Position := PositionCbo.Selected.Text;
  ContentObj.AView := AView;
  ContentObj.AClick := AClick;
  ContentObj.Ratio := Ratio;
  ContentObj.AcLog := AcLog;
  ContentObj.Adsstart := Concat(FormatDateTime('yyyy-mm-dd',
    AdsstartdateTxt.DateTime), 'T', FormatDateTime('hh:nn',
    AdsstarttimeTxt.DateTime));
  ContentObj.Adsend := Concat(FormatDateTime('yyyy-mm-dd',
    AdsenddateTxt.DateTime), 'T', FormatDateTime('hh:nn',
    AdsendtimeTxt.DateTime));
  if TypeCbo.Selected.Text = 'Photo' then
  begin
    if AdsImageId <> '' then
      ContentObj.AdsmediaId := AdsImageId;
    if aAdsImage <> '' then
      ContentObj.AdsmediaId := 'addid';
  end
  else
  begin
    if AdsVdoId <> '' then
      ContentObj.AdsmediaId := AdsVdoId;
    if aAdsVdo <> '' then
      ContentObj.AdsmediaId := 'addid';
  end;
  var
  ValidateResult := ContentObj.Validate;
  // // verify pass
  if ValidateResult.FindValue('result').Value.ToBoolean = False then
  begin
    const
      sLineBreak = {$IFDEF MACOS64} AnsiChar(#10) {$ENDIF} {$IFDEF LINUX} AnsiChar(#10) {$ENDIF} {$IFDEF MSWINDOWS} AnsiString(#13#10) {$ENDIF};
    var
    ListError := '';
    var
    ErrorArray := ValidateResult.FindValue('error') as TJSONArray;
    var
    i := 0;
    for i := 0 to ErrorArray.Count - 1 do
    begin
      var
      ErrorItem := ErrorArray.Get(i) as TJSONObject;
      ListError := Concat(ListError, ErrorItem.FindValue('message').Value,
        sLineBreak);
    end;
    ShowMessage(ListError);
    ContentObj.Free;
    exit;
  end;
  aTask := TTask.Create(
    procedure
    begin
      sleep(2000); // 2 seconds
      TThread.Synchronize(TThread.Current,
        procedure
        begin
          ProcessContentful;
        end);
    end);
  aTask.Start;
  BGProcess.Visible := True;
  ProcessLabelAnimate.Enabled := True;
end;

procedure TfAdsForm.SlugTxtExit(Sender: TObject);
begin
  var ValidateObject := CValidateHelper.Create;
  var
  checkBlankResult := ValidateObject.CheckBlank('Slug', SlugTxt.Text);
  if checkBlankResult.FindValue('result').Value.ToBoolean then
  begin
    var
    Slug := ValidateObject.FormatSlug(SlugTxt.Text);
    var
    CheckObj := TMAds.Create(CmaToken, SpaceId, Environment);
    var
    Result := CheckObj.CheckSlug(SlugTxt.Text,AdsId);
    if Result.FindValue('result').Value.ToBoolean = False then
    begin
      SlugTxt.Text := '';
      ShowMessage(Result.FindValue('message').Value);
    end;
    ValidateObject.Free;
    CheckObj.Free;
  end
  else
  begin
    ShowMessage(checkBlankResult.FindValue('message').Value);
    SlugTxt.SetFocus;
  end;
end;

procedure TfAdsForm.ProcessContentful;
begin
  // Create Ads Start Date
  var
  Adsstart := CreateDateTime(AdsstartdateTxt.Text, AdsstarttimeTxt.DateTime);
  // Create Ads End Date
  var
  Adsend := CreateDateTime(AdsenddateTxt.Text, AdsendtimeTxt.DateTime);
  try
    var
    Result := TJSONObject.Create;
    // new file delete old file
    var
    ImgObj := MContentfulImage.Create(CmaToken, SpaceId, Environment);
    var
    VideoObj := MContentfulVideo.Create(CmaToken, SpaceId, Environment);
    if delAdsImageId <> '' then
    begin
      ImgObj.Id := delAdsImageId;
      ImgObj.Version := AdsImageVersion;
      if PublishStatus then
      begin
        ImgObj.UnPublish;
      end;
      ImgObj.DeleteImage;
    end;
    if delAdsVdoId <> '' then
    begin
      VideoObj.Id := delAdsVdoId;
      VideoObj.Version := AdsVdoVersion;
      if PublishStatus then
      begin
        VideoObj.UnPublish;
      end;
      VideoObj.DeleteVdo;
    end;
    var
    uploadresult := False;
    if TypeCbo.Selected.Text = 'Photo' then
    begin
      if AdsImageId <> '' then
      begin
        ContentObj.AdsmediaId := AdsImageId;
        uploadresult := True;
      end;
      if aAdsImage <> '' then
      begin
        var
        FileName := extractfilename(aAdsImage);
        var
        extFile := ExtractFileExt(aAdsImage);
        FileName := StringReplace(FileName, extFile, '', [rfReplaceAll]);
        if ImgObj.Upload(aAdsImage, extFile) then
        begin
          var
          casset := ImgObj.CreateAsset(FileName, FileName, FileName);
          if casset.FindValue('status').Value.ToBoolean then
          begin
            if ImgObj.ProcessAssets then
            begin
              var
              assetVersion := ImgObj.GetVersion;
              if ImgObj.Publish then
              begin
                ContentObj.AdsmediaId := ImgObj.Id;
                uploadresult := True;
              end
              else
              begin
                ShowMessage('Can not publish photo.');
              end;
            end
            else
            begin
              ShowMessage('Can not process photo.');
            end;
          end
          else
          begin
            ShowMessage('Can not create photo.');
          end;
        end
        else
        begin
          ShowMessage('Can not upload photo.');
        end;
      end;
    end
    else
    begin
      if AdsVdoId <> '' then
      begin
        ContentObj.AdsmediaId := AdsVdoId;
        uploadresult := True;
      end;
      if aAdsVdo <> '' then
      begin
        var
        FileName := extractfilename(aAdsVdo);
        var
        extFile := ExtractFileExt(aAdsVdo);
        FileName := StringReplace(FileName, extFile, '', [rfReplaceAll]);
        if VideoObj.Upload(aAdsVdo) then
        begin
          var
          casset := VideoObj.CreateAssets(FileName, FileName);
          if casset.FindValue('status').Value.ToBoolean then
          begin
            if VideoObj.ProcessAssets then
            begin
              if VideoObj.GetVersion then
              begin
                if VideoObj.Publish then
                begin
                  ContentObj.AdsmediaId := VideoObj.Id;
                  uploadresult := True;
                end
                else
                begin
                  ShowMessage('Can not publish video.');
                end;
              end
              else
              begin
                ShowMessage('Can not get version video.');
              end;
            end
            else
            begin
              ShowMessage('Can not process video.');
            end;
          end
          else
          begin
            ShowMessage('Can not create video.');
          end;
        end
        else
        begin
          ShowMessage('Can not upload video.');
        end;
      end;
    end;
    ImgObj.Free;
    VideoObj.Free;
    if uploadresult <> True then
    begin
      BGProcess.Visible := False;
      ProcessLabelAnimate.Enabled := False;
      exit;
    end;
    if AdsId = '' then
    begin
      // save
      Result := ContentObj.Save;
    end
    else
    begin
      // update
      ContentObj.Id := AdsId;
      ContentObj.Version := Version;
      Result := ContentObj.Update;
      if PublishStatus then
        ContentObj.UnPublish;
    end;
    if Result.FindValue('status').Value.ToBoolean then
    begin
      // schedule ads
      // create contentful schedule object
      var
      ContentfulSchedule := TContentfulSchedule.Create(CmaToken, SpaceId,
        Environment);
      if AdsId <> '' then
      begin
        // clean all schedule
        var
        ScheduleList := ContentfulSchedule.GetAll(AdsId);
        var
        i := 0;
        for i := 0 to ScheduleList.Count - 1 do
        begin
          var
          ItemObject := TJSONObject(ScheduleList.Get(i));
          var
          delResult := ContentfulSchedule.ScheduleCancel(AdsId,
            ItemObject.FindValue('sys.id').Value);
        end;
      end;
      // Ads Start
      // if start time more than now
      if CompareDateTime(Adsstart, now) > 0 then
      begin
        // set start schedule
        var
        resultstart := ContentfulSchedule.Save(ContentObj.Id, 'publish',
          Adsstart);
      end
      else
      begin
        // start time less than or equal now auto publish
        var
        presult := ContentObj.Publish;
      end;
      // Ads End
      if CompareDateTime(Adsend, now) > 0 then
      begin
        var
        resultend := ContentfulSchedule.Save(ContentObj.Id,
          'unpublish', Adsend);
      end;
      ContentfulSchedule.Free;
      // End schedule
      ShowMessage(Result.FindValue('message').Value);
      ModalResult := mrOk;
    end
    else
    begin
      BGProcess.Visible := False;
      ProcessLabelAnimate.Enabled := False;
      ShowMessage(Result.FindValue('result').Value);
    end;
  except
    on E: Exception do
    begin
      BGProcess.Visible := False;
      ProcessLabelAnimate.Enabled := False;
      ShowMessage(E.ClassName + ' error raised, with message : ' + E.Message);
    end;
  end;
end;

end.
