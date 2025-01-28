unit Help;

interface

uses
  Windows, SysUtils, Classes, Forms, Dialogs, ShellApi, Controls, Math,
  {$IFNDEF VER130} Variants, ShLwApi, {$ENDIF}
  ComObj;

type
  SilentException = class(Exception);


procedure OpenWebPage(Url: string);
procedure OpenEmailClient(Address, Subject: string);

procedure LoadHHelp;
procedure UnLoadHHelp;
function IsHHelpLoaded: boolean;
procedure OpenHHelpFile(FileName: TFileName = '');
procedure ShowHHelpPopup(Id: DWORD; FileName: TFileName); overload;
procedure ShowHHelpPopup(Id: DWORD); overload;

function GetFileVersion(FileName: TFileName): Integer; overload;
function GetFileVersion: Integer; overload;

procedure ForceForegroundWindow(H: THandle);
procedure ChangeCursor(AControl: TControl; ACursor: TCursor);
procedure AppendToLog(AFileName: TFileName; AText: AnsiString);

function GetVersion(AMajor: boolean = true): integer;
function GetVersionString: string;
function GetBuildString: string;

function WrapHint(S: string; MaxLen: integer = 70): string;

procedure OpenWebPagePost(Url, Data: string);

function MemToHex(const Mem; ByteCnt: integer): string;
procedure HexToMem(HexStr: string; var Mem);


function AppDir: TFileName;
function AppExe: TFileName;
{$IFNDEF VER130}function RelToAbsPath(BaseDir, RelPath: TFileName): TFileName;{$ENDIF}
procedure ShellExecWait(Operation, FileName, Parameters: string; ShowCmd: Integer);
function GetTempDir: String;

procedure StringToFileW(S: WideString; AFileName: TFileName);
function FileToStringA(AFileName: TFileName): AnsiString;
procedure AppendStringToFileA(S: AnsiString; AFileName: TFileName);
{$IFNDEF VER130}function GetCpuCount: Integer;{$ENDIF}


implementation


procedure OpenWebPage(Url: string);
begin
  ShellExecute(GetDesktopWindow, 'open', PChar(Url), '', '', SW_SHOWNORMAL);
end;


procedure OpenWebPagePost(Url, Data: string);
const
  HdrStr = 'Content-Type: application/x-www-form-urlencoded'#13#10;
var
  Flags, Target, Headers, DataArr: OleVariant;
  i: integer;
  Ie: Variant;
begin
  DataArr := VarArrayCreate([0, Length(Data)-1], varByte);
  for i:=1 to Length(Data) do DataArr[i-1] := Ord(Data[i]);

  Headers := HdrStr;
  Flags := 0;
  Target := '';

  Ie := CreateOleObject('InternetExplorer.Application');
  Ie.Navigate(Url, Flags, Target, DataArr, Headers);
  Ie.Visible := true;
end;


procedure OpenEmailClient(Address, Subject: string);
var
  Param: string;
begin
  Param := 'mailto:' + Address;
  if Subject <> '' then Param := Param + '?subject=' + Subject;
  ShellExecute(Application.Handle, nil, PChar(Param), '', '', SW_SHOWNORMAL);
end;


//------------------------------------------------------------------------------
//                               HHelp
//------------------------------------------------------------------------------
type
  THtmlHelpFun = function (hwndCaller: HWND; pszFile: PChar; uCommand: UINT;
    dwData: DWORD): HWND; stdcall;

  THhPopup = record
    cbStruct: Integer;
    hinst: Longint;
    idString: UINT;
    pszText: PAnsiChar;
    pt: TPOINT;
    clrForeground: TCOLORREF;
    clrBackground: TCOLORREF;
    rcMargins: TRECT;
    pszFont: PAnsiChar;
  end;


const
  HH_DISPLAY_TOC = $0001;
  HH_DISPLAY_TEXT_POPUP = $000E;
  HH_CLOSE_ALL = $0012;

var
  Lib: THandle = 0;
  HtmlHelpFun: THtmlHelpFun;
  DefaultHelpFile: TFileName;
  HhData: THhPopup;

procedure LoadHHelp;
const
  DlMessage = 'Please download and install Microsoft Html Help Viewer, HHUPD.EXE. Download now?';
  DlPageUrl = 'http://www.microsoft.com/downloads/release.asp?releaseid=33071';
begin
  //load library
  try
    if Lib = 0 then Lib := LoadLibrary('hhctrl.ocx');
    if Lib = 0 then Abort;
    HtmlHelpFun := GetProcAddress(Lib, 'HtmlHelpA');
    if not Assigned(HtmlHelpFun) then begin UnloadHHelp; Abort; end;
  except
    if MessageDlg(DlMessage, mtWarning, mbOKCancel, 0) = mrOk
      then OpenWebPage(DlPageUrl);
  end;

  //get help file name
  SetLength(DefaultHelpFile, MAX_PATH);
  SetLength(DefaultHelpFile, GetModuleFileName(HInstance, PChar(DefaultHelpFile), MAX_PATH));
  DefaultHelpFile := ChangeFileExt(DefaultHelpFile, '.chm');

  //fill popup help structure
  with HhData do
    begin
    cbStruct := SizeOf(THhPopup);
    hinst := 0;
    idString  := 0; //put topic Id here
    pszText := nil;
    pt := POINT(0,0); //put cursor coordinates here
    clrForeground := $FFFFFFFF;
    clrBackground := $FFFFFFFF;
    rcMargins := RECT(-1,-1,-1,-1);
    pszFont := nil;
    end;
end;


procedure UnLoadHHelp;
begin
  if Lib <> 0 then
    begin
    HtmlHelpFun(0, nil, HH_CLOSE_ALL, 0);
    Sleep(500); //HtmlHelpFun returns asynchronously
    FreeLibrary(Lib);
    end;
  Lib := 0;
end;


function IsHHelpLoaded: boolean;
begin
  Result := Lib <> 0;
end;


procedure OpenHHelpFile(FileName: TFileName);
begin
  if not IsHHelpLoaded then LoadHHelp;
  if not IsHHelpLoaded then Exit;
  if FileName = '' then FileName := DefaultHelpFile;

  HtmlHelpFun(Application.Handle, PChar(FileName), HH_DISPLAY_TOC, 0);
end;


procedure ShowHHelpPopup(Id: DWORD);
begin
  if not IsHHelpLoaded then LoadHHelp;
  if not IsHHelpLoaded then Exit;
  ShowHHelpPopup(Id, DefaultHelpFile);
end;


procedure ShowHHelpPopup(Id: DWORD; FileName: TFileName); overload;
var
  H: THandle;
  P:TPoint;
  DRect, WRect: TRect;
begin
  if not IsHHelpLoaded then LoadHHelp;
  if not IsHHelpLoaded then Exit;

  HhData.idString := Id;

  //store mouse pos
  P := Mouse.CursorPos;
  Inc(P.y, 16);
  //get hint's rect
  HhData.pt := POINT(P.x, -2000);
  H := HtmlHelpFun(Application.Handle, PChar(FileName), HH_DISPLAY_TEXT_POPUP, DWORD(@HhData));
  GetWindowRect(H, WRect);
  DestroyWindow(H);
  SystemParametersInfo(SPI_GETWORKAREA, 0, @DRect, 0);
  //adjust position
  Inc(P.x, Min(0, DRect.Right - WRect.Right));
  Inc(P.y, Min(0, DRect.Bottom - (P.y + WRect.Bottom - WRect.Top)));
  //create hint
  HhData.pt := P;
  H := HtmlHelpFun(Application.Handle, PChar(FileName), HH_DISPLAY_TEXT_POPUP, DWORD(@HhData));
  SetWindowPos(H, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE);
end;



function GetFileVersion: Integer; overload;
var
  Path: string;
begin
  SetLength(Path, MAX_PATH);
  SetLength(Path, GetModuleFileName(HInstance, PChar(Path), MAX_PATH));

  Result := GetFileVersion(Path);
end;


function GetFileVersion(FileName: TFileName): Integer; overload;
var
  Dummy: DWord;
  Buf: array of Byte;
  Info: PVSFixedFileInfo;
  Len: UINT;
begin
  Result := 0;
  SetLength(Buf, GetFileVersionInfoSize(PChar(FileName), Dummy));
  if Length(Buf) = 0 then Exit;
  if not GetFileVersionInfo(PChar(FileName), 0, Length(Buf), @Buf[0]) then Exit;
  if not VerQueryValue(@Buf[0], '\', Pointer(Info), Len) then Exit;
  if Len < SizeOf(TVSFixedFileInfo) then Exit;
  Result := Info.dwFileVersionMS;
end;


//to bring our own window to foreground,
//we attach temporarily to the active thread
procedure ForceForegroundWindow(H: THandle);
var
  idAttach, idAttachTo: THandle;
begin
  idAttach := GetCurrentThreadId;
  idAttachTo := GetWindowThreadProcessId(GetForegroundWindow, nil);
  AttachThreadInput(idAttach, idAttachTo, true);
  try SetForegroundWindow(H);
  finally AttachThreadInput(idAttach, idAttachTo, false); end;
end;


procedure ChangeCursor(AControl: TControl; ACursor: TCursor);
begin
  AControl.Cursor := ACursor;
  Screen.Cursor := crCross;
  Screen.Cursor := crDefault;
end;


procedure AppendToLog(AFileName: TFileName; AText: AnsiString);
var
  Fs: TFileStream;
begin
  if AText = '' then Exit;

  if FileExists(AFileName)
    then Fs := TFileStream.Create(AFileName, fmOpenReadWrite)
    else Fs := TFileStream.Create(AFileName, fmCreate);

  try
    Fs.Seek(0, soFromEnd);
    Fs.WriteBuffer(AText[1], Length(AText));
  finally Fs.Free; end;
end;


function GetVersion(AMajor: boolean = true): integer;
var
  Dummy: DWord;
  Buf: array of Byte;
  Info: PVSFixedFileInfo;
  Len: UINT;
begin
  Result := 0;
  SetLength(Buf, GetFileVersionInfoSize(PChar(ParamStr(0)), Dummy));
  if Length(Buf) = 0 then Exit;
  if not GetFileVersionInfo(PChar(ParamStr(0)), 0, Length(Buf), @Buf[0]) then Exit;
  if not VerQueryValue(@Buf[0], '\', Pointer(Info), Len) then Exit;
  if Len < SizeOf(TVSFixedFileInfo) then Exit;
  if AMajor
    then Result := Info.dwFileVersionMS
    else Result := Info.dwFileVersionLS;
end;



function WrapHint(S: string; MaxLen: integer = 70): string;
var
  p: integer;
begin
  for p:= Length(S)-1 downto 2 do
    if (AnsiChar(S[p]) in [',', ';']) and (S[p+1] <> ' ')
      then Insert(' ', S, p+1);

  Result := S;

  with TStringList.Create do
    try
      while Length(S) > MaxLen do
        begin
        p := MaxLen+1;
        while p > 1 do
          if S[p] = ' '
            then begin System.Delete(S, p, 1); Break; end
            else Dec(p);

        if p = 1 then Break;
        
        Add(Copy(S, 1, p-1));
        System.Delete(S, 1, p-1);
        end;
      if S <> '' then Add(S);
      Result := Trim(Text);
    finally
      Free;
    end;
end;


function GetVersionString: string;
begin
  Result := Format('%d.%d', [HiWord(GetVersion), LoWord(GetVersion)]);
end;


function GetBuildString: string;
begin
  Result := Format('%d.%d.%d.%d',
    [HiWord(GetVersion), LoWord(GetVersion), HiWord(GetVersion(false)), LoWord(GetVersion(false))]);
end;



function AppDir: TFileName;
begin
  Result := ExtractFilePath(ParamStr(0));
end;


function AppExe: TFileName;
begin
  Result := ParamStr(0);
end;


//http://stackoverflow.com/questions/5329472/conversion-between-absolute-and-relative-paths-in-delphi
{$IFNDEF VER130}
function RelToAbsPath(BaseDir, RelPath: TFileName): TFileName;
var
  Buffer: array [0..MAX_PATH-1] of Char;
begin
  if not PathIsRelative(PChar(RelPath)) then Exit(RelPath);
  Result := IncludeTrailingPathDelimiter(BaseDir) + RelPath;
  if PathCanonicalize(@Buffer, PChar(Result)) then Result := Buffer;
end;
{$ENDIF}


function GetProcessHandleFromHwnd(H: THandle): THandle; stdcall; external 'Oleacc.dll';

procedure ShellExecWait(Operation, FileName, Parameters: string; ShowCmd: Integer);
var
  Info: TShellExecuteInfoW;
  CurrHwnd, NewHwnd: THandle;
  i: integer;
begin
  CurrHwnd := GetForegroundWindow;

  //http://www.codeproject.com/Articles/1842/A-newbie-s-elementary-guide-to-spawning-processes
  ZeroMemory(@Info, SizeOf(Info));
  Info.cbSize := sizeof(SHELLEXECUTEINFO);
  Info.fMask := SEE_MASK_NOCLOSEPROCESS;
  Info.Wnd := GetDesktopWindow;
  Info.lpVerb := PWideChar(Operation);
  Info.lpFile := PWideChar(FileName);
  Info.lpParameters := PWideChar(Parameters);
  Info.lpDirectory := nil;
  Info.nShow := ShowCmd;
  Info.hInstApp := 0;
  ShellExecuteEx(@Info);

  //http://www.delphipages.com/forum/showthread.php?t=134510
  for i:=1 to 20 do
    begin
    Sleep(100);
    NewHwnd := GetForegroundWindow;
    if (NewHwnd = 0) or (NewHwnd = CurrHwnd) then Continue;
    SetWindowPos(NewHwnd, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE);
    Break;
    end;

  WaitForSingleObject(Info.hProcess, INFINITE);
end;


function FileToStringA(AFileName: TFileName): AnsiString;
begin
  with TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone) do
    try
      SetLength(Result, Size);
      if Size > 0 then ReadBuffer(Result[1], Size);
    finally Free; end;
end;


procedure StringToFileA(S: AnsiString; AFileName: TFileName);
begin
  with TFileStream.Create(AFileName, fmCreate) do
    try
      if S <> '' then WriteBuffer(S[1], Length(S));
    finally
      Free;
    end;
end;


procedure StringToFileW(S: WideString; AFileName: TFileName);
begin
  with TFileStream.Create(AFileName, fmCreate) do
    try
      if S <> '' then WriteBuffer(S[1], Length(S) * SizeOf(Char));
    finally
      Free;
    end;
end;

procedure AppendStringToFileA(S: AnsiString; AFileName: TFileName);
begin
  if not FileExists(AFileName) then
    begin StringToFileA(S, AFileName); Exit; end;

  if S = '' then Exit;

  with TFileStream.Create(AFileName, fmOpenReadWrite) do
    try
      Position := Size;
      WriteBuffer(S[1], Length(S));
    finally
      Free;
    end;
end;



function GetTempDir: String;
begin
  SetLength(Result, MAX_PATH);
  SetLength(Result, GetTempPath(MAX_PATH, @Result[1]));
end;


{$IFNDEF VER130}
function GetCpuCount: Integer;
var
SysInfo: TSystemInfo;
HyperThread : Integer;
begin
  GetSystemInfo(SysInfo);
  HyperThread := 0;
  //detect hperthreading EDX bit 28 = 1 after calling CPUID
  if SysInfo.dwNumberOfProcessors > 1 then
    asm
    CPUID
    test edx, $800000 //bit 28 set?
    jz @@No_HT_Support
    mov HyperThread , 1 //set to true
    @@No_HT_Support:
    end;

  Result := SysInfo.dwNumberOfProcessors;
  if Boolean(HyperThread) then result := result div 2;
end;
{$ENDIF}



function MemToHex(const Mem; ByteCnt: integer): string;
var
  i: integer;
  PMem: PByteArray;
begin
  PMem := @Mem;
  Result := '';
  for i:=0 to ByteCnt-1 do Result := Result + IntToHex(Ord(PMem[i]), 2);
end;


procedure HexToMem(HexStr: string; var Mem);
var
  i: integer;
  PMem: PByteArray;
begin
  PMem := @Mem;
  for i:=0 to (Length(HexStr) div 2) - 1 do
    PMem[i] := SysUtils.StrToInt('$' + Copy(HexStr, 1+2*i, 2));
end;


initialization

finalization
  UnLoadHHelp;


end.



