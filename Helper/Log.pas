unit Log;

interface

uses
  System.SysUtils;

type
  MLog = Class

  private
    LogFileName,ErrorLogFileName:String;
    LogDirectory:WideString;
    LogFile,ErrorLogFile: TextFile;
  public
    constructor Create;
    procedure AddLog(LogMessage:WideString);
    procedure AddErrorLog(LogMessage:WideString);
  End;

implementation

constructor MLog.Create;
var LogPath:String;
begin
  {$IFDEF MACOS}
    LogPath := Concat(GetHomePath, '/spacebar');
    if not DirectoryExists(logPath) then CreateDir(logPath);
    LogPath := Concat(GetHomePath, '/log');
    if not DirectoryExists(logPath) then CreateDir(logPath);
    LogFileName:=Concat(LogPath,'/log_', FormatDateTime('ddMMyyyy',
    Now),'.log');
    ErrorLogFileName:=Concat(LogPath,'/error_log_', FormatDateTime('ddMMyyyy',
    Now),'.log');
  {$ELSE}
    var ProjectPath := GetCurrentDir;
    ProjectPath := StringReplace(ProjectPath, '\Win64\Debug', '',
    [rfReplaceAll, rfIgnoreCase]);
    LogPath := Concat(ProjectPath, '\log');
    if not DirectoryExists(logPath) then CreateDir(logPath);
    LogFileName:=Concat(LogPath,'\log_', FormatDateTime('ddMMyyyy',
    Now),'.log');
    ErrorLogFileName:=Concat(LogPath,'\error_log_', FormatDateTime('ddMMyyyy',
    Now),'.log');
  {$ENDIF}
end;

procedure MLog.AddErrorLog(LogMessage: WideString);
var
  TimeString:String;
begin
  // create file with word
  AssignFile(ErrorLogFile, ErrorLogFileName);
  if not FileExists(ErrorLogFileName) then
  begin
    // Rewrite creates a new external file with the name assigned
    Rewrite(ErrorLogFile);
  end
  else
  begin
    // Append File
    Append(ErrorLogFile);
  end;
  // write text in file
  TimeString:=FormatDateTime('dd/MM/yyyy HH:mm:ss',Now);
  Writeln(ErrorLogFile, Concat('"',TimeString,'","',LogMessage,'"'));
  // close file
  CloseFile(ErrorLogFile);
end;

procedure MLog.AddLog(LogMessage: WideString);
var
  TimeString:String;
begin
  // create file with word
  AssignFile(LogFile, LogFileName);
  if not FileExists(LogFileName) then
  begin
    // Rewrite creates a new external file with the name assigned
    Rewrite(LogFile);
  end
  else
  begin
    // Append File
    Append(LogFile);
  end;
  // write text in file
  TimeString:=FormatDateTime('dd/MM/yyyy HH:mm:ss',Now);
  Writeln(LogFile, Concat('"',TimeString,'","',LogMessage,'"'));
  // close file
  CloseFile(LogFile);
end;

end.

