unit RefMgr;

interface

uses
  Windows, Messages, SysUtils, Forms, Dialogs, ShlObj, FileCtrl;


type
  TRefSource = (rsIotaRef, rsLotwUsers, rsFccCb, rsRacCb, rsCanSpec{, rsRdaCB});

  
function GetAfreetDataFolder: TFileName;
function GetRefFolder: TFileName;
function GetUserFolder: TFileName;
function GetHotFolder: TFileName;
function GetIniName: TFileName;
function GetIniFolder: TFileName;

function DownloadRefDataset(Src: TRefSource): boolean;



implementation


function GetAfreetDataFolder: TFileName;
begin
  SetLength(Result, MAX_PATH);
  SHGetSpecialFolderPath(Application.Handle, @Result[1], CSIDL_APPDATA, true);
  Result := PChar(Result) + '\Afreet\';
end;


function GetRefFolder: TFileName;
begin
  Result := GetAfreetDataFolder + 'Reference\';
  try ForceDirectories(Result); except end;
end;


function GetUserFolder: TFileName;
begin
  Result := GetAfreetDataFolder + 'UserData\';
  try ForceDirectories(Result); except end;
end;

function GetHotFolder: TFileName;
begin
  Result := GetAfreetDataFolder + 'HotInfo\';
  try ForceDirectories(Result); except end;
end;


function GetIniName: TFileName;
var
  AppName: TFileName;
begin
  if (GetVersion and $FF) < 6 //6.0 = Vista
    then
      Result := ChangeFileExt(ParamStr(0), '.ini')
    else
      begin
      Result := GetAfreetDataFolder + 'Products\';
      AppName := ChangeFileExt(ExtractFileName(ParamStr(0)), '');
      Result := Result + AppName + '\';
      try ForceDirectories(Result); except end;
      Result := Result + AppName + '.ini';
      end;
end;


//in setup sctipt:  {userappdata}\Afreet\Products\BandMaster\

function GetIniFolder: TFileName;
begin
  Result := ExtractFilePath(GetIniName
  );
end;










//------------------------------------------------------------------------------
//                          execute downloader
//------------------------------------------------------------------------------
function ExecAndWait(Path: TFileName; Hide: boolean): DWord;
var
  Si: TStartupInfo;
  Pi: TProcessInformation;
  Msg: TMsg;
begin
  ZeroMemory(@Si, SizeOf(TStartupInfo));
  Si.cb := SizeOf(TStartupInfo);
  Si.dwFlags := STARTF_USESHOWWINDOW;
  if Hide then Si.wShowWindow := SW_HIDE else Si.wShowWindow := SW_SHOWNORMAL;

  CreateProcess(nil, PChar(Path), nil, nil, false, NORMAL_PRIORITY_CLASS, nil, nil, Si, Pi);

  repeat
    while PeekMessage(Msg, 0, 0, 0, PM_REMOVE) do
      begin
      if Msg.Message = WM_QUIT then Halt(Msg.wParam);
      TranslateMessage(Msg);
      DispatchMessage(Msg);
      end;
    GetExitCodeProcess(Pi.hProcess, Result);
  until Result <> STILL_ACTIVE;

  CloseHandle(Pi.hProcess);
  CloseHandle(Pi.hThread);
end;


function DownloadRefDataset(Src: TRefSource): boolean;
const
  CSIDL_PROGRAM_FILES = $26;
  REFMGR_EXE = '\Afreet\RefManager\RefManager.exe';
var
  FilePath: TFileName;
begin
  SetLength(FilePath, MAX_PATH);
  SHGetSpecialFolderPath(Application.Handle, @FilePath[1], CSIDL_PROGRAM_FILES, true);
  FilePath := PChar(FilePath) + REFMGR_EXE + ' ' + IntToStr(Ord(Src));
  Result := ExecAndWait(FilePath, false) = 0;
end;


end.

