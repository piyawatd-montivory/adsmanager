unit AdsLists;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  System.Rtti, FMX.Grid.Style, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf,
  FireDAC.DApt.Intf, Data.Bind.EngExt, FMX.Bind.DBEngExt, FMX.Bind.Grid,
  System.Bindings.Outputs, FMX.Bind.Editors, System.ImageList, FMX.ImgList,
  Data.Bind.Components, Data.Bind.Grid, Data.Bind.DBScope, Data.DB,
  FireDAC.Comp.DataSet, FireDAC.Comp.Client, FMX.EditBox, FMX.NumberBox,
  FMX.ScrollBox, FMX.Grid, FMX.ListBox, FMX.Edit, FMX.Objects,
  FMX.Controls.Presentation, FMX.Layouts, System.JSON;

type
  TfAdsLists = class(TFrame)
    MainLayout: TLayout;
    TopLayout: TLayout;
    Label1: TLabel;
    Line1: TLine;
    ToolbarLayout: TLayout;
    NewBtn: TButton;
    Layout9: TLayout;
    Image10: TImage;
    Label9: TLabel;
    ArchiveBtn: TButton;
    Layout1: TLayout;
    Image1: TImage;
    ArchiveLabel: TLabel;
    SearchLabel: TLabel;
    SearchTxt: TEdit;
    SearchIconLayout: TLayout;
    SearchIcon: TImage;
    SortCombo: TComboBox;
    titleasc: TListBoxItem;
    titledesc: TListBoxItem;
    createasc: TListBoxItem;
    createdesc: TListBoxItem;
    updateasc: TListBoxItem;
    updatedesc: TListBoxItem;
    BodyLayout: TLayout;
    ContentGrid: TStringGrid;
    BottomLayout: TLayout;
    PageLayout: TLayout;
    PageTxt: TNumberBox;
    OfPageLabel: TLabel;
    TotalPageLabel: TLabel;
    PageFirstBtn: TCornerButton;
    PageFirstImg: TImage;
    PageBackBtn: TCornerButton;
    PageBackImg: TImage;
    PageLastBtn: TCornerButton;
    PageLastImg: TImage;
    PageNextBtn: TCornerButton;
    PageNextImg: TImage;
    SPStyleBook: TStyleBook;
    FDMAds: TFDMemTable;
    FDMAdsid: TWideStringField;
    FDMAdstitle: TWideStringField;
    FDMAdsslug: TWideStringField;
    FDMAdscategory: TWideStringField;
    FDMAdscreatedat: TWideStringField;
    FDMAdsupdatedat: TWideStringField;
    FDMAdspublishstatus: TBooleanField;
    FDMAdsversion: TWideStringField;
    FDMAdsstatus: TWideStringField;
    FDMAdscreateuser: TWideStringField;
    FDMAdsupdateuser: TWideStringField;
    FDMAdsselcontent: TBooleanField;
    FDMAdsschedule: TBooleanField;
    BindSourceContent: TBindSourceDB;
    BindingsList1: TBindingsList;
    LinkGridContent: TLinkGridToDataSource;
    sortlist: TImageList;
    StatusCbo: TComboBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    ListBoxItem3: TListBoxItem;
    ListBoxItem4: TListBoxItem;
    adsstartasc: TListBoxItem;
    adsstartdesc: TListBoxItem;
    adsendasc: TListBoxItem;
    adsenddesc: TListBoxItem;
    FDMAdsadsstart: TWideStringField;
    FDMAdsadsend: TWideStringField;
    procedure NewBtnClick(Sender: TObject);
    procedure StatusCboChange(Sender: TObject);
    procedure SortComboChange(Sender: TObject);
    procedure ContentGridCellDblClick(const Column: TColumn;
      const Row: Integer);
    procedure ContentGridCellClick(const Column: TColumn; const Row: Integer);
    procedure ArchiveBtnClick(Sender: TObject);
  private
    { Private declarations }
    procedure ChangePage(ChangeType: String);
  public
    { Public declarations }
    CmaToken, SpaceId, Environment, UserId, UserTitle, TagToken: String;
    Page, Limit, CurrentPage, ImageWidthLimit: Integer;
    LoginName: WideString;
    procedure GetLists;
    procedure SetSize;
  end;

implementation

uses AdsForm, MAds, DDateTime, Log;

{$R *.fmx}

procedure TfAdsLists.ArchiveBtnClick(Sender: TObject);
begin
  if (FDMAds.RecordCount > 0) AND (StatusCbo.Selected.Text <> 'archive') then
  begin
    case MessageDlg(Concat('Confirm Archive selected content ?'),
      System.UITypes.TMsgDlgType.mtConfirmation,
      [System.UITypes.TMsgDlgBtn.mbOK,
      System.UITypes.TMsgDlgBtn.mbCancel], 0) of
      mrOk:
        begin
          var
          ContentObj := TMAds.Create(CmaToken, SpaceId, Environment);
          FDMAds.First;
          while not FDMAds.Eof do
          begin
            if FDMAds.FieldValues['status'] = 'Draft' then
            begin
              if FDMAds.FieldValues['schedule'] = False then
              begin
                ContentObj.Id := FDMAds.FieldValues['id'];
                ContentObj.Version := FDMAds.FieldValues['version'];
                var
                delResult := ContentObj.Archive;
              end;
            end;
            FDMAds.Next;
          end;
          ShowMessage('Archive success.');
          ContentObj.Free;
          Page := 1;
          PageTxt.Value := 1;
          GetLists;
        end;
    end;
  end;
end;

procedure TfAdsLists.ChangePage(ChangeType: String);
begin
  var
  changepagevalue := PageTxt.Value.ToString.ToInteger;
  if ChangeType = 'change' then
  begin
    if PageTxt.Value > TotalPageLabel.Text.ToInteger then
    begin
      PageTxt.Value := TotalPageLabel.Text.ToInteger;
    end;
  end;
  if ChangeType = 'back' then
  begin
    if changepagevalue <= 1 then
    begin
      PageTxt.Value := 1;
      changepagevalue := 1;
    end
    else
    begin
      changepagevalue := changepagevalue - 1;
      PageTxt.Value := changepagevalue;
    end;
  end;
  if ChangeType = 'first' then
  begin
    PageTxt.Value := 1;
    changepagevalue := 1;
  end;
  if ChangeType = 'last' then
  begin
    PageTxt.Value := TotalPageLabel.Text.ToInteger;
    changepagevalue := TotalPageLabel.Text.ToInteger;
  end;
  if ChangeType = 'next' then
  begin
    if PageTxt.Value >= TotalPageLabel.Text.ToInteger then
    begin
      PageTxt.Value := TotalPageLabel.Text.ToInteger;
      changepagevalue := TotalPageLabel.Text.ToInteger;
    end
    else
    begin
      changepagevalue := changepagevalue + 1;
      PageTxt.Value := changepagevalue;
    end;
  end;
  if changepagevalue <> CurrentPage then
  begin
    Page := changepagevalue;
    GetLists;
  end;
end;

procedure TfAdsLists.ContentGridCellClick(const Column: TColumn;
  const Row: Integer);
begin
  if Column.Index = 0 then
  begin
    if FDMAds.RecordCount > 0 then
    begin
      FDMAds.Edit;
      if FDMAds.FieldValues['selcontent'] then
      begin
        FDMAds.FieldByName('selcontent').AsBoolean := False;
      end
      else
      begin
        FDMAds.FieldByName('selcontent').AsBoolean := True;
      end;
      FDMAds.Post;
    end;
  end;
end;

procedure TfAdsLists.ContentGridCellDblClick(const Column: TColumn;
  const Row: Integer);
begin
  if Column.Index <> 0 then
  begin
    var
    FormContent := TfAdsForm.Create(nil);
    FormContent.CmaToken := CmaToken;
    FormContent.SpaceId := SpaceId;
    FormContent.Environment := Environment;
    FormContent.ImageWidthLimit := ImageWidthLimit;
    FormContent.AdsId := FDMAds.FieldValues['id'];
    if FDMAds.FieldValues['status'] = 'Published' then
    begin
      FormContent.PublishStatus := True;
    end
    else
    begin
      FormContent.PublishStatus := False;
    end;
    if FormContent.ShowModal = mrOk then
      GetLists;
  end;
end;

procedure TfAdsLists.GetLists;
begin
  try
    var
    sortlist := TStringList.Create;
    sortlist.Add('fields.title');
    sortlist.Add('-fields.title');
    sortlist.Add('sys.createdAt');
    sortlist.Add('-sys.createdAt');
    sortlist.Add('sys.updatedAt');
    sortlist.Add('-sys.updatedAt');
    sortlist.Add('fields.adsstart');
    sortlist.Add('-fields.adsstart');
    sortlist.Add('fields.adsend');
    sortlist.Add('-fields.adsend');
    var
    ContentObj := TMAds.Create(CmaToken, SpaceId, Environment);
    var
    ContentResult := ContentObj.GetAll(SearchTxt.Text, Page, Limit,
      sortlist[SortCombo.Selected.Index], UserId, UserTitle,
      StatusCbo.Selected.Text);
    if ContentResult.FindValue('status').Value.ToBoolean then
    begin
      CurrentPage := Page;
      TotalPageLabel.Text := ContentResult.FindValue('totalpage').Value;
      var
      resultArray := ContentResult.FindValue('result.items') as TJSONArray;
      var
        i: Integer;
      FDMAds.Close;
      FDMAds.Open;
      var
      timeObj := CDateTime.Create;
      for i := 0 to resultArray.Count - 1 do
      begin
        var
        ItemObject := TJSONObject(resultArray.Get(i));
        FDMAds.Append;
        FDMAds.FieldByName('selcontent').AsBoolean := False;
        FDMAds.FieldByName('id').AsString :=
          ItemObject.FindValue('sys.id').Value;
        FDMAds.FieldByName('version').AsString :=
          ItemObject.FindValue('sys.version').Value;
        FDMAds.FieldByName('title').AsString :=
          ItemObject.FindValue('fields.title.en-US').Value;
        if ItemObject.FindValue('sys.archivedVersion') <> nil then
        begin
          FDMAds.FieldByName('status').AsString := 'Archive';
        end
        else
        begin
          if ItemObject.FindValue('sys.publishedVersion') <> nil then
          begin
            FDMAds.FieldByName('status').AsString := 'Published';
          end
          else
          begin
            FDMAds.FieldByName('status').AsString := 'Draft';
          end;
        end;
        var
        adsstarto := timeObj.ContentfulStrToDatetime
          (ItemObject.FindValue('fields.adsstart.en-US').Value);
        var
        adsendo := timeObj.ContentfulStrToDatetime
          (ItemObject.FindValue('fields.adsend.en-US').Value);
        FDMAds.FieldByName('adsstart').AsString :=
          FormatDateTime('dd/mm/yyyy hh:mm', adsstarto);
        FDMAds.FieldByName('adsend').AsString :=
          FormatDateTime('dd/mm/yyyy hh:mm', adsendo);
        var
        createAt := timeObj.UTCtoDatetime
          (ItemObject.FindValue('sys.createdAt').Value);
        createAt := timeObj.CalculateTime(createAt, 'plushour', 7);
        FDMAds.FieldByName('createdat').AsString :=
          FormatDateTime('dd/mm/yyyy hh:mm', createAt);
        var
        updatedAt := timeObj.UTCtoDatetime
          (ItemObject.FindValue('sys.updatedAt').Value);
        updatedAt := timeObj.CalculateTime(updatedAt, 'plushour', 7);
        FDMAds.FieldByName('updatedat').AsString :=
          FormatDateTime('dd/mm/yyyy hh:mm', updatedAt);
        FDMAds.Post;
      end;
      LinkGridContent.Active := False;
      LinkGridContent.Active := True;
      ContentObj.Free;
      FDMAds.First;
      timeObj.Free;
      SetSize;
    end;
  Except
    on E: Exception do
    begin
      var
      lObj := MLog.Create;
      lObj.AddErrorLog('ads list : ' + E.ClassName +
        ' error raised, with message : ' + E.Message);
      lObj.Free;
    end;
  end;
end;

procedure TfAdsLists.NewBtnClick(Sender: TObject);
begin
  var
  FormAds := TfAdsForm.Create(nil);
  FormAds.CmaToken := CmaToken;
  FormAds.SpaceId := SpaceId;
  FormAds.Environment := Environment;
  FormAds.ImageWidthLimit := ImageWidthLimit;
  FormAds.PublishStatus := False;
  if FormAds.ShowModal = mrOk then
    GetLists;
end;

procedure TfAdsLists.SetSize;
begin
  var
  gridWidth := ContentGrid.Width.ToString.ToInteger - 15;
  ContentGrid.Columns[0].Width := (gridWidth * 5) / 100;
  ContentGrid.Columns[1].Width := (gridWidth * 25) / 100;
  ContentGrid.Columns[2].Width := (gridWidth * 15) / 100;
  ContentGrid.Columns[3].Width := (gridWidth * 15) / 100;
  ContentGrid.Columns[4].Width := (gridWidth * 10) / 100;
  ContentGrid.Columns[5].Width := (gridWidth * 15) / 100;
  ContentGrid.Columns[6].Width := (gridWidth * 15) / 100;
end;

procedure TfAdsLists.SortComboChange(Sender: TObject);
begin
  Page := 1;
  PageTxt.Value := 1;
  GetLists;
end;

procedure TfAdsLists.StatusCboChange(Sender: TObject);
begin
  Page := 1;
  PageTxt.Value := 1;
  GetLists;
end;

end.
