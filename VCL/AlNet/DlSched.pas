unit DlSched;

interface

uses
  SysUtils, Classes, DLoader, IniFiles, Utc;

type
  TFilePurpose = (fpIndex, fpIonosonde, fpText);

  PScheduleEntry = ^TScheduleEntry;
  TScheduleEntry = record
    Title: string;
    LatestUrl: string;
    PreviousUrl: string;
    HistoryUrl: string;
    First: TDateTime;
    Step: string;
    PrevStep: string;
    HistoryStep: string;
    HistoryDepth: string;
    LatestInPrev: boolean;
{
    LatestStartPos: integer;
    PreviousStartPos: integer;
    HistoryStartPos: integer;
}
    NextStart: TDateTime;
    Purpose: TFilePurpose;
    end;


  TFileEvent = procedure (ASender: TObject; const AFile: TDownloadedFile) of object;


  TDownloadScheduler = class
  private
    FSchedule: array of TScheduleEntry;
    FDownloaders: array[0..5] of TDownloader;
    FUrls: TStringList;
    FileTimes: TStringList;
    FStartingNext: boolean;

    FOnGetFile: TFileEvent;
    FOnError: TFileEvent;
    FOnFinish: TNotifyEvent;
    FOnStart: TNotifyEvent;
    FVersionInfo: string;

    procedure StartNext;
    function ExpandUrl(AUrl: string; T: TDateTime): string;
    function AddUrl(AUrl: string; AEntry: PScheduleEntry): integer;
    function AddStep(ATime: TDateTime; Sign: Char; AStep: string): TDateTime;
    procedure DlSuccess(Sender: TObject);
    procedure DlError(Sender: TObject);
    procedure UpdateStartTime(AEntry: PScheduleEntry);
    function GetScheduleCount: integer;
  public
    IncludeReports: boolean;

    constructor Create;
    destructor Destroy; override;
    procedure LoadSchedule(FilePath: string);
    procedure DownloadLatest;
    procedure DownloadInitial;
    procedure DownloadHistory;
    procedure DownloadReports;
    procedure Tick;
    function MinutesToStart: integer;

    procedure Abort;
    function IsBusy: boolean;

    property VersionInfo: string read FVersionInfo;
    property ScheduleCount: integer read GetScheduleCount;

    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    property OnGetFile: TFileEvent read FOnGetFile write FOnGetFile;
    property OnError: TFileEvent read FOnError write FOnError;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
  end;



implementation


function DecodeHH_NN(Str: string): TDateTime;
var
  h, m: WORD;
begin
  if Length(Str) <> 5 then begin Result := 0; Exit; end;

  h := StrToInt(Copy(Str, 1, 2));
  m := StrToInt(Copy(Str, 4, 2));
  Result := EncodeTime(h, m, 0, 0);
end;


{
function IncYear(ATime: TDateTime; dY: integer): TDateTime;
var
  y, m, d: WORD;
begin
  //leap year problem
  DecodeDate(ATime, y, m, d);
  Result := EncodeDate(y + dY, m, d);
  ReplaceTime(Result, ATime);
end;
}

{ TDownloadScheduler }

constructor TDownloadScheduler.Create;
var
  i: integer;
begin
  FUrls := TStringList.Create;
  FileTimes := TStringList.Create;

  for i:=0 to High(FDownloaders) do
    begin
    FDownloaders[i] := TDownloader.Create(nil);
    FDownloaders[i].OnSuccess := DlSuccess;
    FDownloaders[i].OnError := DlError;
    end;
end;


destructor TDownloadScheduler.Destroy;
var
  i: integer;
begin
  FileTimes.Free;
  FUrls.Free;
  for i:=0 to High(FDownloaders) do FDownloaders[i].Free;
  inherited;
end;


procedure TDownloadScheduler.Abort;
var
  i: integer;
begin
  FUrls.Clear;
  for i:=0 to High(FDownloaders) do FDownloaders[i].AbortDownload;
end;


procedure TDownloadScheduler.DlError(Sender: TObject);
var
  DL: TDownloader;
begin
  Dl := Sender as TDownloader;

  if Assigned(FOnError) then FOnError(Self, Dl.DownloadedFile);

  StartNext;
end;


procedure TDownloadScheduler.DlSuccess(Sender: TObject);
var
  DL: TDownloader;
begin
  Dl := Sender as TDownloader;

  //add to file time list
  with Dl.DownloadedFile do
    FileTimes.Values[FileName] := IsoDateTimeToStr(FileTime);

  //fire event
  if Assigned(FOnGetFile) then FOnGetFile(Self, Dl.DownloadedFile);
  StartNext;
end;


procedure TDownloadScheduler.DownloadLatest;
var
  i: integer;
begin
  Abort;
  //add latest
  for i:=0 to High(FSchedule) do
    if FSchedule[i].Purpose <> fpText then
      if FSchedule[i].LatestUrl <> '' then
        AddUrl(ExpandUrl(FSchedule[i].LatestUrl, UtcNow), @FSchedule[i]);
  //start
  StartNext;
end;


procedure TDownloadScheduler.DownloadInitial;
var
  i: integer;
  T: TDateTime;
begin
  Abort;

  //add previous
  for i:=0 to High(FSchedule) do
    if FSchedule[i].PreviousUrl <> '' then
      with FSchedule[i] do
        begin
        //start at prev day if file not updated yet
        T := UtcNow;
        if (Step='D1') and (Frac(T) < First) then T := T - 1;

        if PrevStep <> '' then
          AddUrl(ExpandUrl(PreviousUrl, AddStep(T, '-', PrevStep)), nil);

        if LatestInPrev or (PrevStep = '') then //DL Prev for today
          AddUrl(ExpandUrl(PreviousUrl, T), @FSchedule[i]);
        end;

  //add latest
  for i:=0 to High(FSchedule) do
    if not FSchedule[i].LatestInPrev //no Prev for today, DL latest
      then if (FSchedule[i].Purpose <> fpText) or IncludeReports
        then AddUrl(ExpandUrl(FSchedule[i].LatestUrl, UtcNow), @FSchedule[i]);

  //start
  StartNext;
end;


procedure TDownloadScheduler.DownloadHistory;
var
  i: integer;
  T: TDateTime;
begin
  for i:=0 to High(FSchedule) do
    with FSchedule[i] do
      if HistoryUrl <> '' then
        begin
        T := AddStep(UtcNow, '-', HistoryDepth);
        while T < (UtcNow + 1/24/60) do
          begin
          AddUrl(ExpandUrl(HistoryUrl, T), nil);
          T := AddStep(T, '+', HistoryStep);
          end;
        end;

  //do not stop current DL, but start new if channels available
  StartNext;
end;


procedure TDownloadScheduler.DownloadReports;
var
  i: integer;
begin
  Abort;
  //add latest
  for i:=0 to High(FSchedule) do
    if FSchedule[i].Purpose = fpText then
      AddUrl(ExpandUrl(FSchedule[i].LatestUrl, UtcNow), @FSchedule[i]);
  //start
  StartNext;
end;


procedure TDownloadScheduler.LoadSchedule(FilePath: string);
var
  L: TStringList;
  i: integer;
begin
  with TIniFile.Create(FilePath) do
    try
      //list entries
      L := TStringList.Create;
      try
        ReadSections(L);

        //next version info
        FVersionInfo := ReadString('Version', 'Latest', '');
        i := L.IndexOf('Version');
        if (i >= 0) and (i < L.Count) then L.Delete(i);

        for i:=L.Count-1 downto 0 do
          if ReadString(L[i], 'Latest', '') = ''
            then L.Delete(i);

        SetLength(FSchedule, L.Count);
        for i:=0 to High(FSchedule) do FSchedule[i].Title := L[i];
      finally L.Free; end;

      //read each entry
      for i:=0 to High(FSchedule) do with FSchedule[i] do
        begin
        LatestUrl := ReadString(Title, 'Latest', '');
        PreviousUrl := ReadString(Title, 'Previous', '');
        HistoryUrl := ReadString(Title, 'History', '');
        First := DecodeHH_NN(ReadString(Title, 'First', ''));
        Step := ReadString(Title, 'Step', '');
        PrevStep := ReadString(Title, 'PrevStep', '');
        HistoryStep := ReadString(Title, 'HistoryStep', '');
        HistoryDepth := ReadString(Title, 'HistoryDepth', '');
        LatestInPrev := ReadBool(Title, 'LatestInPrev', false);

        try Purpose := TFilePurpose(ReadInteger(Title, 'Purpose', 0));
        except Purpose := fpIndex; end;
{
        LatestStartPos := ReadInteger(Title, 'LatestStartPos', 0);
        PreviousStartPos := ReadInteger(Title, 'PreviousStartPos', 0);
        HistoryStartPos := ReadInteger(Title, 'HistoryStartPos', 0);
}
        end;
    finally
      Free;
    end;
end;


procedure TDownloadScheduler.StartNext;
var
  i: integer;
  S: string;
  OldFileTime: TDateTime;
begin
  if FStartingNext then Exit;
  FStartingNext := true;

  try
    for i:=0 to High(FDownloaders) do
      begin
      //if cannot start
      if (FUrls.Count = 0) then Break
      else if FDownloaders[i].IsBusy then Continue;

      //if-modified-since
      S := FileTimes.Values[FileNameFromUrl(FUrls[0])];
      if S <> '' then OldFileTime := IsoStrToDateTime(S) else OldFileTime := 0;

  {
      //start pos
      T := UtcNow;
      StartPos := 0;
      Se := PScheduleEntry(FUrls.Objects[0]);
      if (Se <> nil) then
        if FUrls[i] = ExpandUrl(Se.LatestUrl, T) then StartPos := Se.LatestStartPos
        else if FUrls[i] = ExpandUrl(Se.PreviousUrl, T) then StartPos := Se.PreviousStartPos
        else if FUrls[i] = ExpandUrl(Se.HistoryUrl, T) then StartPos := Se.HistoryStartPos;
  }

      //start

      //IE7 does not support "Range: bytes"
      //FDownloaders[i].Download(FUrls[0], OldFileTime, -200000);
      FDownloaders[i].Download(FUrls[0], OldFileTime, 0);

      if FUrls.Objects[0] <> nil then
        FDownloaders[i].DownloadedFile.Purpose :=
          Ord(PScheduleEntry(FUrls.Objects[0]).Purpose);
      FDownloaders[i].Client := FUrls.Objects[0];
      FUrls.Delete(0);
      end;

  finally
    FStartingNext := false;
  end;

  if (not IsBusy) and Assigned(FOnFinish) then FOnFinish(Self);
end;


function TDownloadScheduler.ExpandUrl(AUrl: string; T: TDateTime): string;
const
  MonthName: array[1..12] of String =
    ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun',
    'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
  F = [rfReplaceAll, rfIgnoreCase];
var
  y, m, d, q: WORD;
begin
  DecodeDate(T, y, m, d);
  q := (m+2) div 3;

  Result := AUrl;
  Result := StringReplace(Result, '{D}',    Format('%d', [d]),   F);
  Result := StringReplace(Result, '{DD}',   Format('%.2d', [d]), F);
  Result := StringReplace(Result, '{M}',    Format('%d', [m]),   F);
  Result := StringReplace(Result, '{MM}',   Format('%.2d', [m]), F);
  Result := StringReplace(Result, '{Mon}',  MonthName[m],        F);
  Result := StringReplace(Result, '{Q}',    Format('%d', [q]),   F);
  Result := StringReplace(Result, '{YYYY}', Format('%.4d', [y]), F);
end;

function TDownloadScheduler.IsBusy: boolean;
var
  i: integer;
begin
  Result := FUrls.Count > 0;
  if not Result then
    for i:=0 to High(FDownloaders) do
      Result := Result or FDownloaders[i].IsBusy;
end;


function TDownloadScheduler.AddUrl(AUrl: string; AEntry: PScheduleEntry): integer;
begin
  if FUrls.IndexOf(AUrl) < 0 then Result := 1 else Result := 0;
  if Result = 1 then FUrls.AddObject(AUrl, TObject(AEntry));

  UpdateStartTime(AEntry);
end;


function TDownloadScheduler.AddStep(ATime: TDateTime; Sign: Char;
  AStep: string): TDateTime;
var
  N: integer;
begin
  N := StrToIntDef(Copy(AStep, 2, MAXINT), 1);
  if Sign = '-' then N := -N;

  case AStep[1] of
    'N': Result := ATime + N * (1/24/60);
    'H': Result := ATime + N * (1/24);
    'M': Result := IncMonth(ATime, N);
    'Q': Result := IncMonth(ATime, N*3);
    'Y': Result := IncMonth(ATime, N*12);
    else {'D':} Result := ATime + N;
    end;
end;



procedure TDownloadScheduler.UpdateStartTime(AEntry: PScheduleEntry);
begin
  if AEntry = nil then Exit;
  with AEntry^ do
    begin
    NextStart := Trunc(UtcNow) + First;
    while NextStart < UtcNow do NextStart := AddStep(NextStart, '+', Step);
    end;
end;

procedure TDownloadScheduler.Tick;
var
  i: integer;
  AddCnt: integer;
begin
  AddCnt := 0;

  //add latest
  for i:=0 to High(FSchedule) do
    if FSchedule[i].NextStart <= UtcNow then
      if (FSchedule[i].Purpose <> fpText) or IncludeReports then
        Inc(AddCnt, AddUrl(ExpandUrl(FSchedule[i].LatestUrl, UtcNow), @FSchedule[i]));

  if AddCnt > 0 then
    begin
    if Assigned(FOnStart) then FOnStart(Self);
    StartNext;
    end;
end;



function TDownloadScheduler.MinutesToStart: integer;
var
  i: integer;
  T: TDateTime;
begin
  T := MAXINT;
  for i:=0 to High(FSchedule) do
    if FSchedule[i].NextStart < T then T := FSchedule[i].NextStart;

  if T = MAXINT then Result := MAXINT else Result := Round((T-UtcNow) * 24*60);
end;


function TDownloadScheduler.GetScheduleCount: integer;
begin
  Result := Length(FSchedule);
end;



end.

