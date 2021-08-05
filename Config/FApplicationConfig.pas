unit FApplicationConfig;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.TabControl,
  System.Actions, FMX.ActnList, FMX.Edit, FMX.StdCtrls, FMX.Objects,
  FMX.Controls.Presentation, FMX.Layouts, FMX.ListBox,FMX.Styles, FMX.EditBox,
  FMX.NumberBox;

type
  TApplicationConfig = class(TForm)
    AccountLabelLayout: TLayout;
    AccountTitleLabel: TLabel;
    Line1: TLine;
    SpaceLayout: TLayout;
    SpaceLabel: TLabel;
    SpaceTxt: TEdit;
    EnvironmentLayout: TLayout;
    EnvironmentLabel: TLabel;
    EnvironmentTxt: TEdit;
    SaveBtn: TButton;
    CancelBtn: TButton;
    Layout2: TLayout;
    ImageWidthLabel: TLabel;
    ImageWidth: TNumberBox;
    Layout15: TLayout;
    Image3: TImage;
    Label10: TLabel;
    Layout3: TLayout;
    Image1: TImage;
    Label1: TLabel;
    procedure SaveBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  ApplicationConfig: TApplicationConfig;

implementation

{$R *.fmx}

procedure TApplicationConfig.FormCreate(Sender: TObject);
var i:Integer;ConfigFile:String;
begin
  {$IFDEF MACOS}
    ConfigFile := Concat(GetHomePath, '/spacebar/configads.ini');
  {$ELSE}
    var ProjectPath := GetCurrentDir;
    ProjectPath := StringReplace(ProjectPath, '\Win64\Debug', '',
    [rfReplaceAll, rfIgnoreCase]);
    ConfigFile := Concat(ProjectPath, '\configads.ini');
  {$ENDIF}
  if fileexists(ConfigFile) then
  begin
    var ConfigList := TStringList.Create;
    ConfigList.LoadFromFile(ConfigFile);
    for i := 0 to ConfigList.Count - 1 do
    begin
//      // read file sync
      var ConfigData := TStringList.Create;
      ConfigData.Delimiter := '=';
      ConfigData.DelimitedText := ConfigList[i];
      if ConfigData[0] = 'spaceid' then
        SpaceTxt.Text := ConfigData[1];
      if ConfigData[0] = 'environment' then
        EnvironmentTxt.Text := ConfigData[1];
    end;
  end
  else
  begin
    SpaceTxt.Text := 'e7ee8kzf5re6';
    EnvironmentTxt.Text := 'master';
  end;
end;

procedure TApplicationConfig.SaveBtnClick(Sender: TObject);
var NewConfig : TextFile;
ConfigFile:String;
begin
  if (Trim(SpaceTxt.Text).Length > 0) AND (Trim(EnvironmentTxt.Text).Length > 0) then
  begin
    // create file with word
    var themename := 'Default';
    {$IFDEF MACOS}
      var ConfigFolder := Concat(GetHomePath, '/spacebar');
      if not DirectoryExists(ConfigFolder) then CreateDir(ConfigFolder);
      ConfigFile := Concat(ConfigFolder, '/configads.ini');
    {$ELSE}
      var ProjectPath := GetCurrentDir;
      ProjectPath := StringReplace(ProjectPath, '\Win64\Debug', '',
      [rfReplaceAll, rfIgnoreCase]);
      ConfigFile := Concat(ProjectPath, '\configads.ini');
    {$ENDIF}
    AssignFile(NewConfig, ConfigFile);
    ReWrite(NewConfig);
    Writeln(NewConfig, Concat('spaceid=',SpaceTxt.Text));
    Writeln(NewConfig, Concat('environment=',EnvironmentTxt.Text));
    Writeln(NewConfig, Concat('imagewidth=',ImageWidth.Value.ToString));
    // close file
    CloseFile(NewConfig);
    ModalResult:=mrOK;
  end
  else
  begin
    ShowMessage('Please config SpaceId and Environment');
  end;
end;

end.
