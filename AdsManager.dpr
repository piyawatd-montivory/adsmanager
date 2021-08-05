program AdsManager;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {MainApplication},
  AdsLists in 'Ads\AdsLists.pas' {fAdsLists: TFrame},
  AdsForm in 'Ads\AdsForm.pas' {fAdsForm},
  MAds in 'Model\MAds.pas',
  ContentfulImage in 'Helper\ContentfulImage.pas',
  ContentfulSchedule in 'Helper\ContentfulSchedule.pas',
  DDateTime in 'Helper\DDateTime.pas',
  ModelHelper in 'Helper\ModelHelper.pas',
  ValidateHelper in 'Helper\ValidateHelper.pas',
  Log in 'Helper\Log.pas',
  FApplicationConfig in 'Config\FApplicationConfig.pas' {ApplicationConfig},
  ContentfulVideo in 'Helper\ContentfulVideo.pas',
  DesktopLogin in 'DesktopLogin.pas' {FDesktopLogin},
  MProfile in 'Model\MProfile.pas',
  MarkdownCommonMark in 'Helper\MarkdownCommonMark.pas',
  MarkdownDaringFireball in 'Helper\MarkdownDaringFireball.pas',
  MarkdownHTMLEntities in 'Helper\MarkdownHTMLEntities.pas',
  MarkdownProcessor in 'Helper\MarkdownProcessor.pas',
  MarkdownUnicodeUtils in 'Helper\MarkdownUnicodeUtils.pas',
  SpacebarProfile in 'Profile\SpacebarProfile.pas' {fProfile};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainApplication, MainApplication);
  Application.CreateForm(TfAdsForm, fAdsForm);
  Application.CreateForm(TApplicationConfig, ApplicationConfig);
  Application.CreateForm(TFDesktopLogin, FDesktopLogin);
  Application.CreateForm(TfProfile, fProfile);
  Application.Run;
end.
