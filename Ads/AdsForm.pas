﻿unit AdsForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.StdCtrls, FMX.Edit, FMX.Controls.Presentation, FMX.Layouts, FMX.TabControl,
  FMX.Memo.Types, FMX.ListBox, FMX.ScrollBox, FMX.Memo, FMX.Media,System.JSON,
  FMX.DateTimeCtrls, FMX.Ani,System.DateUtils,System.Threading;

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
    Layout15: TLayout;
    Image3: TImage;
    Label10: TLabel;
    CloseBtn: TButton;
    Layout4: TLayout;
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
    Position1: TListBoxItem;
    Position2: TListBoxItem;
    Position3: TListBoxItem;
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
    Position4: TListBoxItem;
    Position5: TListBoxItem;
    Position6: TListBoxItem;
    Position7: TListBoxItem;
    Position8: TListBoxItem;
    Position9: TListBoxItem;
    procedure UploadPhotoClick(Sender: TObject);
    procedure ButtonVdoClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    aAdsImage,aAdsVdo,AcLog:WideString;
    delAdsImageId,delAdsVdoId,AdsImageId,AdsVdoId,CurrentMediaType:String;
    AdsImageVersion,AdsVdoVersion,AView,AClick,Ratio : Integer;
    FormOpen:Boolean;
    procedure ProcessContentful;
  public
    { Public declarations }
    CmaToken, SpaceId, Environment,AdsId, ResultForm: String;
    UploadType: WideString;
    PublishStatus: Boolean;
    ImageWidthLimit,Version : Integer;
  end;

var
  fAdsForm: TfAdsForm;

implementation

uses ContentfulVideo,ContentfulImage,ContentfulSchedule,MAds,DDateTime;

{$R *.fmx}

procedure TfAdsForm.FormCreate(Sender: TObject);
begin
FormOpen:=False;
AView:=0;
AClick:=0;
Ratio:=0;
AdsstartdateTxt.DateTime:=now;
AdsstarttimeTxt.DateTime:=now;
AdsenddateTxt.DateTime:=now;
AdsendtimeTxt.DateTime:=now;
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
      TypeCbo.Selected.Index:=0;
      AdsImageId:=ConObj.AdsmediaId;
      AdsImageVersion:=ConObj.AdsmediaVersion;
      var
      vObj := MContentfulImage.Create(CmaToken, SpaceId, Environment);
      var
      MS := vObj.LoadImage(ConObj.Adsmedia);
      PreviewImage.Bitmap.LoadFromStream(MS);
      vObj.Free;
    end
    else
    begin
      TypeCbo.Selected.Index:=1;
      AdsVdoId:=ConObj.AdsmediaId;
      AdsVdoVersion:=ConObj.AdsmediaVersion;
      VdoTxt.Text := ConObj.Adsmedia;
    end;
    TitleTxt.Text := ConObj.Title;
    SlugTxt.Text := ConObj.Slug;
    SlugTxt.ReadOnly := True;
    LinkTxt.Text := ConObj.Link;
    var dateObj := CDateTime.Create;
    var adsstarto := dateObj.ContentfulStrToDatetime(ConObj.Adsstart);
    var adsendo := dateObj.ContentfulStrToDatetime(ConObj.Adsend);
    AdsstartdateTxt.DateTime := adsstarto;
    AdsstarttimeTxt.DateTime := adsstarto;
    AdsenddateTxt.DateTime := adsendo;
    AdsendtimeTxt.DateTime := adsendo;
    //Position
    if ConObj.Position = 'Index-Position1' then PositionCbo.ItemIndex := 0;
    if ConObj.Position = 'Index-Position2' then PositionCbo.ItemIndex := 1;
    if ConObj.Position = 'Index-Position3' then PositionCbo.ItemIndex := 2;
    if ConObj.Position = 'Index-Position4' then PositionCbo.ItemIndex := 3;
    if ConObj.Position = 'Index-Position5' then PositionCbo.ItemIndex := 4;
    if ConObj.Position = 'Index-Position6' then PositionCbo.ItemIndex := 5;
    if ConObj.Position = 'Category-Position1' then PositionCbo.ItemIndex := 6;
    if ConObj.Position = 'Category-Position2' then PositionCbo.ItemIndex := 7;
    if ConObj.Position = 'Article' then PositionCbo.ItemIndex:=8;
  end;
end;
end;

procedure TfAdsForm.SaveBtnClick(Sender: TObject);
var
  aTask: ITask;
begin
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
//ProcessContentful;
end;

procedure TfAdsForm.UploadPhotoClick(Sender: TObject);
begin
UploadType:='photo';
  if OpenDialogAds.Execute then
    if FileExists(OpenDialogAds.FileName) then
    begin
      var
      checkSizeImg := TBitmap.Create;
      checkSizeImg.LoadFromFile(OpenDialogAds.FileName);
      if checkSizeImg.Width > ImageWidthLimit then
      begin
        ShowMessage('Image Max Width '+ImageWidthLimit.ToString+' px.');
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
        PreviewImage.Visible:=True;
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
  UploadType:='video';
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
        PreviewImage.Visible:=False;
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

procedure TfAdsForm.ProcessContentful;
var
  Result: TJSONObject;
  sYear, sMonth, sDay, sHour, sMinute, sSecond, sMillisecond: Word;
  eYear, eMonth, eDay, eHour, eMinute, eSecond, eMillisecond: Word;
begin
  //Create Ads Start Date
  sYear := StrToInt(Copy(AdsstartdateTxt.Text, 7, 4));
  sMonth := StrToInt(Copy(AdsstartdateTxt.Text, 4, 2));
  sDay := StrToInt(Copy(AdsstartdateTxt.Text, 1, 2));
  sHour := FormatDateTime('hh',AdsstarttimeTxt.DateTime).ToInteger;
  sMinute := FormatDateTime('nn',AdsstarttimeTxt.DateTime).ToInteger;
  sSecond := 00;
  sMillisecond := Round(0000);
  var
  ScheduleDateTime := EncodeDateTime(sYear, sMonth, sDay, sHour, sMinute,
    sSecond, sMillisecond);
  //Create Ads End Date
  eYear := StrToInt(Copy(AdsenddateTxt.Text, 7, 4));
  eMonth := StrToInt(Copy(AdsenddateTxt.Text, 4, 2));
  eDay := StrToInt(Copy(AdsenddateTxt.Text, 1, 2));
  eHour := FormatDateTime('hh',AdsstarttimeTxt.DateTime).ToInteger;
  eMinute := FormatDateTime('nn',AdsstarttimeTxt.DateTime).ToInteger;
  eSecond := 00;
  eMillisecond := Round(0000);
  var
  ScheduleDateEndTime := EncodeDateTime(eYear, eMonth, eDay, eHour, eMinute,
    eSecond, eMillisecond);
  if ScheduleDateEndTime <= ScheduleDateTime then
  begin
    BGProcess.Visible := False;
    ProcessLabelAnimate.Enabled := False;
    ShowMessage('End Time must more than Start Time');
    exit
  end;

  try
    Result := TJSONObject.Create;
    var
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
    ContentObj.Adsstart := Concat(FormatDateTime('yyyy-mm-dd',AdsstartdateTxt.DateTime),'T',FormatDateTime('hh:nn',AdsstarttimeTxt.DateTime));
    ContentObj.Adsend := Concat(FormatDateTime('yyyy-mm-dd',AdsenddateTxt.DateTime),'T',FormatDateTime('hh:nn',AdsendtimeTxt.DateTime));
    if TypeCbo.Selected.Text = 'Photo' then
    begin
      if AdsImageId <> '' then ContentObj.AdsmediaId := AdsImageId;
      if aAdsImage <> '' then ContentObj.AdsmediaId := 'addid';
    end
    else
    begin
      if AdsVdoId <> '' then ContentObj.AdsmediaId := AdsVdoId;
      if aAdsVdo <> '' then ContentObj.AdsmediaId := 'addid';
    end;
    var
    ValidateResult := ContentObj.Validate;
//    // verify pass
    if ValidateResult.FindValue('result').Value.ToBoolean then
    begin
      //new file delete old file
      var ImgObj := MContentfulImage.Create(CmaToken, SpaceId, Environment);
      var VideoObj := MContentfulVideo.Create(CmaToken, SpaceId, Environment);
      if delAdsImageId <> '' then
      begin
        if PublishStatus then
        begin
          AdsImageVersion:=ImgObj.UnPublishAssets(delAdsImageId, AdsImageVersion);
        end;
        ImgObj.DeleteImage(delAdsImageId, AdsImageVersion);
      end;
      if delAdsVdoId <> '' then
      begin
        if PublishStatus then
        begin
          AdsVdoVersion:=VideoObj.UnPublishAssets(delAdsVdoId, AdsVdoVersion);
        end;
        VideoObj.DeleteVdo(delAdsVdoId, AdsVdoVersion);
      end;
      if TypeCbo.Selected.Text = 'Photo' then
      begin
        if AdsImageId <> '' then ContentObj.AdsmediaId := AdsImageId;
        if aAdsImage <> '' then
        begin
          var
          FileName := extractfilename(aAdsImage);
          var
          extFile := ExtractFileExt(aAdsImage);
          FileName := StringReplace(FileName, extFile, '', [rfReplaceAll]);
          var
          uploadId := ImgObj.Upload(aAdsImage, FileName, extFile);
          var
          casset := ImgObj.CreateAssets(uploadId, FileName, FileName, extFile);
          var
          newAssetId := casset.FindValue('sys.id').Value;
          var
          proresult := ImgObj.ProcessAssets(newAssetId);
          var assetVersion := ImgObj.GetVersionById(newAssetId);
          var ContentImageVersion := ImgObj.PublishAssets(newAssetId,
              assetVersion);
          ContentObj.AdsmediaId := newAssetId;
        end;
      end
      else
      begin
        if AdsVdoId <> '' then ContentObj.AdsmediaId := AdsVdoId;
        if aAdsVdo <> '' then
        begin
          var
          FileName := extractfilename(aAdsVdo);
          var
          extFile := ExtractFileExt(aAdsVdo);
          FileName := StringReplace(FileName, extFile, '', [rfReplaceAll]);
          var
          uploadId := VideoObj.Upload(aAdsVdo, FileName);
          var
          casset := VideoObj.CreateAssets(uploadId, FileName, FileName);
          var
          newAssetId := casset.FindValue('sys.id').Value;
          var assetVersion := VideoObj.GetVersionById(newAssetId);
          var
          proresult := VideoObj.ProcessAssets(newAssetId);
          var ContentImageVersion := VideoObj.PublishAssets(newAssetId,
              assetVersion);
          ContentObj.AdsmediaId := newAssetId;
        end;
      end;
      ImgObj.Free;
      VideoObj.Free;
      if AdsId = '' then
      begin
        // save
        Result := ContentObj.Save;
      end
      else
      begin
        // update
        ContentObj.id := AdsId;
        ContentObj.Version := Version;
        Result := ContentObj.Update;
      end;
      if Result.FindValue('status').Value.ToBoolean then
      begin
        //schedule ads
        //get version
        ContentObj.Version := Result.FindValue('result.sys.version').Value.ToInteger;
        //get content id
        ContentObj.Id := Result.FindValue('result.sys.id').Value;
        //create contentful schedule object
        var
        ContentfulSchedule := TContentfulSchedule.Create(CmaToken, SpaceId,
          Environment);
        if AdsId = '' then
        begin
          //Ads Start
          //if start time more than now
          if ScheduleDateTime > now then
          begin
            //set start schedule
            var
            resultstart := ContentfulSchedule.Save(ContentObj.Id, 'publish',
            ScheduleDateTime);
          end
          else
          begin
            //start time less than or equal now auto publish
            var presult := ContentObj.Publish;
            ContentObj.Version := presult.FindValue('result.sys.version').Value.ToInteger;
          end;
          //Ads End
          if ScheduleDateEndTime > now then
          begin
            var resultend := ContentfulSchedule.Save(ContentObj.Id, 'unpublish',
            ScheduleDateEndTime);
          end;
        end
        else
        begin
          //clean all schedule
          var ScheduleList := ContentfulSchedule.GetAll(AdsId);
          var i := 0;
          for i := 0 to ScheduleList.Count - 1 do
          begin
            var
              ItemObject := TJSONObject(ScheduleList.Get(i));
              var delResult := ContentfulSchedule.ScheduleCancel(AdsId,ItemObject.FindValue('sys.id').Value);
          end;
          //if ads is publish
          if PublishStatus then
          begin
            //set only unpublish
            //end time more than now
            if ScheduleDateEndTime > now then
            begin
              //set unpublish schedule
              var resultend := ContentfulSchedule.Save(ContentObj.Id, 'unpublish',
              ScheduleDateEndTime);
            end
            else
            begin
              //if end time less than and equal now set unpublish
              var unResult := ContentObj.Unpublish;
            end;
          end
          else
          begin
            //ads not publish
            //Ads Start
            //if start time more than now
            if ScheduleDateTime > now then
            begin
              //set start schedule
              var
              resultstart := ContentfulSchedule.Save(ContentObj.Id, 'publish',
              ScheduleDateTime);
            end
            else
            begin
              //start time less than or equal now auto publish
              var presult := ContentObj.Publish;
              ContentObj.Version := presult.FindValue('result.sys.version').Value.ToInteger;
            end;
            //Ads End
            if ScheduleDateEndTime > now then
            begin
              var resultend := ContentfulSchedule.Save(ContentObj.Id, 'unpublish',
              ScheduleDateEndTime);
            end;
          end;
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
    end
    else
    begin
      BGProcess.Visible := False;
      ProcessLabelAnimate.Enabled := False;
      const
        sLineBreak = {$IFDEF MACOS64} AnsiChar(#10) {$ENDIF} {$IFDEF LINUX} AnsiChar(#10) {$ENDIF} {$IFDEF MSWINDOWS} AnsiString(#13#10) {$ENDIF};
      var
      ListError := '';
      var
      ErrorArray := ValidateResult.FindValue('error') as TJSONArray;
      var i := 0;
      for i := 0 to ErrorArray.Count - 1 do
      begin
        var
        ErrorItem := ErrorArray.Get(i) as TJSONObject;
        ListError := Concat(ListError, ErrorItem.FindValue('message').Value,
          sLineBreak);
      end;
      ShowMessage(ListError);
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