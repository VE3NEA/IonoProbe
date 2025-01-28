unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Menus, TrayIcon, Ini, Tabs, StdCtrls, SetDlg, AboutDlg,
  Math, Help, ImgList, AppEvnts, LiveData, DLoader, Utc, SolArrs,
  QuickPnl, mmsystem, SolArray, FileCtrl, Fram, AlStrLst,
  IniFiles, ComServ, AutoApp, SafeShDown, RefMgr;

type
  TDlStatus = (dsNone, dsRunning, dsWarning, dsStopped, dsWaiting, dsOk, dsError);

  TMainForm = class(TForm)
    TrayIcon1: TTrayIcon;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Download1: TMenuItem;
    Help1: TMenuItem;
    HideMNU: TMenuItem;
    N1: TMenuItem;
    Exit1MNU: TMenuItem;
    HelpMNU: TMenuItem;
    N2: TMenuItem;
    AboutMNU: TMenuItem;
    LatestData1MNU: TMenuItem;
    RecentDataMNU: TMenuItem;
    OldDataMNU: TMenuItem;
    N3: TMenuItem;
    AutoDownload1MNU: TMenuItem;
    PopupMenu1: TPopupMenu;
    ShowMNU: TMenuItem;
    N4: TMenuItem;
    Exit2MNU: TMenuItem;
    Bevel1: TBevel;
    SettingsMNU: TMenuItem;
    LatestData2MNU: TMenuItem;
    AutoDownload2MNU: TMenuItem;
    N5: TMenuItem;
    N6: TMenuItem;
    WebSiteMNU: TMenuItem;
    EmailMNU: TMenuItem;
    MenuImageList: TImageList;
    IconPanel: TQuickPanel;
    Image1: TImage;
    AlertIcon: TImage;
    QuietIcon: TImage;
    ApplicationEvents1: TApplicationEvents;
    N7: TMenuItem;
    AbortMNU: TMenuItem;
    Timer1: TTimer;
    Downloader1: TDownloader;
    ImportMNU: TMenuItem;
    OpenDialog1: TOpenDialog;
    RegisterMNU: TMenuItem;
    LivePlotFrame1: TLivePlotFrame;
    ReportsMNU: TMenuItem;
    LiveData1: TLiveData;
    SafeShutdown1: TSafeShutdown;
    N8: TMenuItem;
    DataFolder1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ShowMNUClick(Sender: TObject);
    procedure HideMNUClick(Sender: TObject);
    procedure ExitMNUClick(Sender: TObject);
    procedure SettingsMNUClick(Sender: TObject);
    procedure AutoDownloadMNUClick(Sender: TObject);
    procedure AboutMNUClick(Sender: TObject);
    procedure LatestDataMNUClick(Sender: TObject);
    procedure EmailMNUClick(Sender: TObject);
    procedure WebSiteMNUClick(Sender: TObject);
    procedure HelpMNUClick(Sender: TObject);
    procedure ApplicationEvents1Message(var Msg: tagMSG;
      var Handled: Boolean);
    procedure RecentDataMNUClick(Sender: TObject);
    procedure OldDataMNUClick(Sender: TObject);
    procedure LiveData1DlSuccess(ASender: TObject;
      const AFile: TDownloadedFile);
    procedure LiveData1DlStart(Sender: TObject);
    procedure LiveData1DlError(ASender: TObject;
      const AFile: TDownloadedFile);
    procedure LiveData1DlFinish(Sender: TObject);
    procedure AbortMNUClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure Image1DblClick(Sender: TObject);
    procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure Downloader1Error(Sender: TObject);
    procedure Downloader1Success(Sender: TObject);
    procedure ImportMNUClick(Sender: TObject);
    procedure LivePlotFrame1ComboBox1Change(Sender: TObject);
    procedure ReportsMNUClick(Sender: TObject);
    procedure LivePlotFrame1QuickPanel1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LivePlotFrame1Timer1Timer(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure DataFolder1Click(Sender: TObject);
  private
    FCloseAllowed: boolean;
    DLOkCount: integer;
    DLErrCount: integer;
    FIconChangeTime: TDateTime;
    FLastTick: TDateTime;
    FStatus: TDlStatus;

    FStormG, FStormS, FStormR: boolean;

    procedure WmSizing(var Msg: TMessage); message WM_SIZING;
    procedure Log(Txt: string);
    procedure LogDL(const AFile: TDownloadedFile);
    procedure SetStatusHint;
    procedure SaveLog;
    procedure CheckStorm;
    procedure InitStorm;
    procedure DownloadSchedule;
    procedure SaveRawFile(const AFile: TDownloadedFile);
    procedure SaveAll;
    procedure UpdateFileCombo;
    procedure Loadtext(ATextFile: TFileName);
    procedure CheckVersion;
    procedure WMQueryEndSession(var Msg: TMessage); message WM_QUERYENDSESSION;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    FLog: TStringList;
    FPlaySound: boolean;
    FSoundFile: TFileName;
    FNeedSchedule: boolean;
    FSaveRaw: boolean;

    TextDir: string;
    TextFile: TFileName;
    TextTime: TDateTime;

    procedure SetAutoDL(Value: boolean);
    function GetAutoDL: boolean;
    procedure ShowDlStatus(AStatus: TDlStatus);
    procedure PlayWavFile(AFileName: TFileName);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.DFM}


//------------------------------------------------------------------------------
//                               Init
//------------------------------------------------------------------------------

procedure TMainForm.FormCreate(Sender: TObject);
begin
  CreateMutex(nil, False, 'IonoProbe');

  DoubleBuffered := true;
  LivePlotFrame1.SetArrays(LiveData1);
  IconPanel.Left := ClientWidth - IconPanel.Width - 2;
  IconPanel.Top := ClientHeight - IconPanel.Height - 2;
  IconPanel.BringToFront;

  Ini.Load;
  if ComServer.StartMode = smAutomation then Application.ShowMainForm := false;

  LiveData1.LoadFromFiles(GetIniFolder + 'Data\');
  LiveData1.Update;
  LivePlotFrame1.DataChanged;


  TextDir := GetIniFolder + 'Text\';
  ForceDirectories(TextDir);

  InitStorm;
  CheckStorm;

  Log('<Program start>');
  if GetAutoDL then RecentDataMNU.Click;

  UpdateFileCombo;

  //see if new version info is encoded in the schedule
  CheckVersion;
end;


procedure TMainForm.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params.WindowClass do style := style or CS_HREDRAW;
end;


procedure TMainForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if FCloseAllowed or SafeShutdown1.ShuttingDown
    then ComExitRequest;     

  CanClose := SafeShutdown1.ShuttingDown or (FCloseAllowed and not IsAutomated);
  if not CanClose then Hide;
end;


procedure TMainForm.FormDestroy(Sender: TObject);
begin
  SaveAll;
end;


procedure TMainForm.SaveAll;
begin
  LiveData1.SaveToFiles(GetIniFolder + 'Data\');
  Ini.Save;

  SaveLog;
  FLog.Free;
end;







//------------------------------------------------------------------------------
//                               System stuff
//------------------------------------------------------------------------------

procedure TMainForm.WmSizing(var Msg: TMessage);
var
  NcHeight: integer;
  NewHeight, NewWidth: integer;
begin
  with PRect(Msg.LParam)^ do
    begin
    NcHeight := Height - ClientHeight;
    NewHeight := NcHeight + LivePlotFrame1.GetNearestHeight(Bottom - Top - NcHeight);
    NewWidth := Max(Right - Left, 160);

    //height
    case Msg.WParam of
      WMSZ_TOP, WMSZ_TOPLEFT, WMSZ_TOPRIGHT:
        Top := Bottom - NewHeight;

      WMSZ_BOTTOM, WMSZ_BOTTOMLEFT, WMSZ_BOTTOMRIGHT:
        Bottom := Top + NewHeight;
      end;

    //width
    case Msg.WParam of
      WMSZ_LEFT, WMSZ_TOPLEFT, WMSZ_BOTTOMLEFT:
        Left := Right - NewWidth;

      WMSZ_RIGHT, WMSZ_TOPRIGHT, WMSZ_BOTTOMRIGHT:
        Right := Left + NewWidth;
      end;
    end;

  Msg.Result := 1;
end;


procedure TMainForm.ApplicationEvents1Message(var Msg: tagMSG;
  var Handled: Boolean);
begin
  //another instance is asking us to show window
  Handled := (Msg.Message = WM_USER) and (Msg.WParam = 73) and (Msg.LParam = 88);

  if Handled then begin Show; ForceForegroundWindow(Application.Handle); end;
end;


procedure TMainForm.PlayWavFile (AFileName: TFileName);
begin
  sndPlaySound(PChar(aFileName),
    SND_ASYNC or SND_NOWAIT or SND_FILENAME or SND_NODEFAULT);
end;





//------------------------------------------------------------------------------
//                            menu commands
//------------------------------------------------------------------------------
procedure TMainForm.ShowMNUClick(Sender: TObject);
begin
  if Visible
    then Hide
    else begin Show; ForceForegroundWindow(Application.Handle); end;
end;


procedure TMainForm.HideMNUClick(Sender: TObject);
begin
  Hide;
end;


procedure TMainForm.ExitMNUClick(Sender: TObject);
begin
  FCloseAllowed := true;
  Close;
end;


procedure TMainForm.SettingsMNUClick(Sender: TObject);
begin
  TSettingsDialog.Create(nil).ShowModal;
end;


procedure TMainForm.AboutMNUClick(Sender: TObject);
begin
  TAboutDialog.Create(nil).ShowModal;
end;


procedure TMainForm.EmailMNUClick(Sender: TObject);
begin
  OpenEmailClient('ionoprobe@dxatlas.com', 'IonoProbe');
end;


procedure TMainForm.WebSiteMNUClick(Sender: TObject);
begin
  OpenWebPage('http://www.dxatlas.com/IonoProbe');
end;


procedure TMainForm.HelpMNUClick(Sender: TObject);
begin
  OpenHHelpFile;
end;

procedure TMainForm.LatestDataMNUClick(Sender: TObject);
begin
  LiveData1DlStart(nil);
  LiveData1.Downloaders.DownloadLatest;
  Log('<Download latest>')
end;

procedure TMainForm.RecentDataMNUClick(Sender: TObject);
begin
  LiveData1DlStart(nil);
  LiveData1.Downloaders.DownloadInitial;
  Log('<Download recent>')
end;

procedure TMainForm.OldDataMNUClick(Sender: TObject);
begin
  LiveData1DlStart(nil);
  LiveData1.Downloaders.DownloadHistory;
  Log('<Download old>')
end;


procedure TMainForm.ReportsMNUClick(Sender: TObject);
begin
  LiveData1DlStart(nil);
  LiveData1.Downloaders.DownloadReports;
  Log('<Download reports>')
end;


procedure TMainForm.AutoDownloadMNUClick(Sender: TObject);
begin
  SetAutoDL(not GetAutoDL);
  if GetAutoDL then Log('<Autodownload = ON>') else Log('<Autodownload = OFF>');
end;


procedure TMainForm.AbortMNUClick(Sender: TObject);
begin
  LiveData1.Downloaders.Abort;
  LiveData1DlFinish(nil);
end;







//------------------------------------------------------------------------------
//                               Options
//------------------------------------------------------------------------------

function TMainForm.GetAutoDL: boolean;
begin
  Result := AutoDownload1MNU.Checked;
end;

procedure TMainForm.SetAutoDL(Value: boolean);
begin
  AutoDownload1MNU.Checked := Value;
  AutoDownload2MNU.Checked := Value;

  if FStatus <> dsRunning then
    if Value
      then ShowDlStatus(dsWaiting)
      else ShowDlStatus(dsStopped);

  //next tick will start DL
  if Value and (LiveData1.Downloaders.MinutesToStart < 0) then FLastTick := 0;
end;





//------------------------------------------------------------------------------
//                           Downloader events
//------------------------------------------------------------------------------
procedure TMainForm.LiveData1DlStart(Sender: TObject);
begin
  DLOkCount := 0;
  DLErrCount := 0;
  ShowDlStatus(dsRunning);
  AbortMNU.Enabled := true;

  DownloadSchedule;
end;


procedure TMainForm.LiveData1DlSuccess(ASender: TObject;
  const AFile: TDownloadedFile);
begin
  Inc(DLOkCount);
  LogDL(AFile);

  if AFile.Purpose = 2 then //fpText
    with TStringList.Create do
      try try
        Text := AFile.Data;
        SaveToFile(TextDir + AFile.FileName);
        //ChangeFileDate(TextDir + AFile.FileName, AFile.FileTime);
        UpdateFileCombo;
      finally Free; end; except end

  else if FSaveRaw then SaveRawFile(AFile);

  //update graphs
  LivePlotFrame1.DataChanged;
end;


procedure TMainForm.LiveData1DlError(ASender: TObject;
  const AFile: TDownloadedFile);
begin
  //ErrorCode = 0  == Not Modified
  if AFile.ErrorCode <> 0 then Inc(DLErrCount);
  LogDL(AFile);
end;

procedure TMainForm.LiveData1DlFinish(Sender: TObject);
begin
  if DlErrCount = 0 then ShowDlStatus(dsOk)
  else if DlOkCount > 0 then ShowDlStatus(dsWarning)
  else ShowDlStatus(dsError);
  AbortMNU.Enabled := false;
  Log('<FINISHED>');
  CheckStorm;

  if DlOkCount > 0 then ComDataArrived;
end;


procedure TMainForm.ShowDlStatus(AStatus: TDlStatus);
begin
  FStatus := AStatus;
  Image1.Left := -16 * Integer(AStatus);
  Image1.Update;

  //SetStatusHint;

  if AStatus in [dsOk, dsWarning, dsError]
    then FIconChangeTime := Now + 1/24/60/2{30 s};
end;


procedure TMainForm.SetStatusHint;
begin
  case FStatus of
    dsNone:    Image1.Hint := '';
    dsRunning: Image1.Hint := 'Downloading data...';
    dsWarning: Image1.Hint := Format('%d download(s) of %d failed',
                 [DlErrCount, DlOkCount + DlErrCount]);
    dsStopped: Image1.Hint := 'Auto download disabled';
    dsWaiting: Image1.Hint := Format('Next download in %d min.',
                 [Max(1, LiveData1.Downloaders.MinutesToStart)]);
    dsOk:      Image1.Hint := Format('%d download(s), no errors', [DlOkCount]);
    dsError:   Image1.Hint := 'All downloads failed';
  end;
end;


procedure TMainForm.Timer1Timer(Sender: TObject);
var
  T: TDateTime;
begin
  T := Now;
  //LiveData tick
  if GetAutoDL and (Trunc(T * 24*60) <> Trunc(FLastTick * 24*60))
     then LiveData1.Tick;
  FLastTick := T;
  //DL icon change
  if (T > FIconChangeTime) and (FStatus <> dsRunning) then
    begin
    if GetAutoDL then ShowDlStatus(dsWaiting) else ShowDlStatus(dsStopped);
    FIconChangeTime := MAXINT;
    end;
end;


procedure TMainForm.Image1DblClick(Sender: TObject);
begin
  AutoDownload1MNU.Click;
end;

procedure TMainForm.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  SetStatusHint;
end;


//------------------------------------------------------------------------------
//                             Storm alerts
//------------------------------------------------------------------------------
procedure TMainForm.InitStorm;
begin
  FStormG := LiveData1[dkKp3].IsStorm(false);
  FStormS := LiveData1[dkProtons].IsStorm(false);
  FStormR := LiveData1[dkXrays].IsStorm(false);
end;


procedure TMainForm.CheckStorm;
var
  NewStormG, NewStormS, NewStormR: boolean;
begin
  //check current conditions
  NewStormG := LiveData1[dkKp3].IsStorm(FStormG);
  NewStormS := LiveData1[dkProtons].IsStorm(FStormS);
  NewStormR := LiveData1[dkXrays].IsStorm(FStormR);

  //systray icon
  if NewStormG or NewStormS or NewStormR
    then TrayIcon1.Icon := AlertIcon.Picture.Icon
    else TrayIcon1.Icon := QuietIcon.Picture.Icon;

  //systray hint
  TrayIcon1.ToolTip := Application.Title;
  if NewStormG then TrayIcon1.ToolTip := TrayIcon1.ToolTip + #13#10'Geomagnetic storm';
  if NewStormS then TrayIcon1.ToolTip := TrayIcon1.ToolTip + #13#10'Solar Radiation storm';
  if NewStormR then TrayIcon1.ToolTip := TrayIcon1.ToolTip + #13#10'Radio blackout';
  if not (NewStormG or NewStormS or NewStormR)
    then TrayIcon1.ToolTip := TrayIcon1.ToolTip + #13#10'No storms';
  //sound
  if (NewStormG and not FStormG) or
     (NewStormS and not FStormS) or
     (NewStormR and not FStormR) then
    begin
    Log('<Storm alert>');
    if FPlaySound then PlayWavFile(FSoundFile);
    end;

  //save new
  FStormG := NewStormG;
  FStormS := NewStormS;
  FStormR := NewStormR;
end;



//------------------------------------------------------------------------------
//                             Schedule update
//------------------------------------------------------------------------------
procedure TMainForm.DownloadSchedule;
begin
  if FNeedSchedule and not Downloader1.IsBusy then
    Downloader1.Download('http://www.dxatlas.com/tech/sked131.ini');
end;


procedure TMainForm.Downloader1Error(Sender: TObject);
begin
  //do not download sked in this session
  FNeedSchedule := false;

  LogDL(Downloader1.DownloadedFile);
end;

procedure TMainForm.Downloader1Success(Sender: TObject);
var
  Path: TFileName;
  NoSked: boolean;
begin
  NoSked := LiveData1.Downloaders.ScheduleCount = 0;
  LogDL(Downloader1.DownloadedFile);

  //save sked to file
  Path := GetIniFolder + 'Schedule.ini';
  with TStringList.Create do
    try try
      Text := Downloader1.DownloadedFile.Data;
      SaveToFile(Path);
    finally Free; end; except end;

  //do not download sked for the next 5 days
  FNeedSchedule := false;
  Ini.SaveSkedDate(Trunc(UtcNow) + 5);

  //load into scheduler
  LiveData1.Downloaders.LoadSchedule(Path);

  //see if new version info is encoded in the schedule
  CheckVersion;

  //start if there was no schedule befor
  if NoSked then LiveData1.Downloaders.DownloadInitial;
end;



procedure TMainForm.CheckVersion;
begin
  if LiveData1.Downloaders.VersionInfo <> '' then
    with LivePlotFrame1.VersionPanel do
      begin
      Caption := LiveData1.Downloaders.VersionInfo;
      Hint := Caption + '. Click here for details';
      Color := clLime;
      Cursor := crHandPoint;
      BorderWidth := 0;
      end;
end;


//------------------------------------------------------------------------------
//                                Debug
//------------------------------------------------------------------------------
procedure TMainForm.Log(Txt: string);
begin
  if FLog <> nil then FLog.Add(FormatDateTime('yyyy-mm-dd hh:nn:ss  ', UtcNow) + Txt);
end;


procedure TMainForm.LogDL(const AFile: TDownloadedFile);
begin
  Log('[' + FormatDateTime('yyyy-mm-dd hh:nn:ss', AFile.FileTime) + ']  '
    + Format('%3d', [Round(AFile.ByteCount / 1024)]) + '  '
    + AFile.FileName + '    ' + AFile.ErrorText);
end;

procedure TMainForm.SaveLog;
var
  FileName: TFileName;
  L: TStringList;
  i: integer;
begin
  if FLog = nil then Exit;
  Log('<Exit>');

  try
    FileName := ChangeFileExt(GetIniName, '.log');
    if FileExists(FileName) then
      begin
      L:= TStringList.Create;
      try
        L.LoadFromFile(FileName);
        L.AddStrings(FLog);
        FLog.Assign(L);
      finally L.Free; end;
      end;

    for i:= FLog.Count-301 downto 0 do FLog.Delete(i);
    FLog.SaveToFile(FileName);
  except end;
end;


procedure TMainForm.SaveRawFile(const AFile: TDownloadedFile);
var
  Dir: string;
  Ss: TStringStream;
begin
  if Length(AFile.Data) < 1 then Exit;
  Dir := GetIniFolder + 'RawFiles\';
  ForceDirectories(Dir);

  Ss := TStringStream.Create(AFile.Data);
  try
    with TFileStream.Create(Dir + AFile.FileName, fmCreate) do
      try CopyFrom(Ss, 0); finally Free; end;
  finally
    Ss.Free;
  end;
end;


procedure TMainForm.ImportMNUClick(Sender: TObject);
begin
  if not OpenDialog1.Execute then Exit;
  LiveData1.ImportFile(OpenDialog1.FileName);
  LivePlotFrame1.DataChanged;
end;

procedure TMainForm.UpdateFileCombo;
var
  Lst: TAlStringList;
  Idx: integer;
begin
  //load file list
  Lst := TAlStringList.Create;
  try
    Lst.LoadFileList(TextDir + '*.txt');
    Lst.Sort;
    LivePlotFrame1.ComboBox1.Items.Assign(Lst);
  finally
    Lst.Free;
  end;

  if TextFile = ''
    then Idx := -1
    else Idx := LivePlotFrame1.ComboBox1.Items.IndexOf(TextFile);
  //file was open
  if Idx >= 0 then
    begin
    LivePlotFrame1.ComboBox1.ItemIndex := Idx;
    //reload only if date changed
    if FileAge(TextDir + TextFile) <> TextTime then LoadText(TextFile);
    end
  //file was not open, but now some files are available
  else if LivePlotFrame1.ComboBox1.Items.Count > 0 then
    begin
    LivePlotFrame1.ComboBox1.ItemIndex := 0;
    //load 1-st file
    LoadText(LivePlotFrame1.ComboBox1.Text);
    end
  //no files available
  else
    LoadText('');
end;


procedure TMainForm.LivePlotFrame1ComboBox1Change(Sender: TObject);
begin
  LoadText(LivePlotFrame1.ComboBox1.Text);
end;


procedure TMainForm.Loadtext(ATextFile: TFileName);
begin
  TextFile := ATextFile;

  if TextFile <> ''
    then //load file in box
      begin
      LivePlotFrame1.RichEdit1.Lines.LoadFromFile(TextDir + TextFile);
      TextTime := FileAge(TextDir + TextFile);
      end
    else //clear box
      begin
      LivePlotFrame1.RichEdit1.Lines.Text := '';
      TextTime := 0;
      end;

  //scroll to top
  LivePlotFrame1.RichEdit1.SelStart := 0;
  LivePlotFrame1.RichEdit1.Perform(EM_SCROLLCARET, 0, 0);
end;



procedure TMainForm.LivePlotFrame1QuickPanel1Click(Sender: TObject);
begin
  if LivePlotFrame1.VersionPanel.Cursor = crHandPoint
    then WebSiteMNU.Click;
end;



procedure TMainForm.LivePlotFrame1Timer1Timer(Sender: TObject);
begin
  LivePlotFrame1.Timer1Timer(Sender);

end;


procedure TMainForm.WMQueryEndSession(var Msg: TMessage);
begin
  SafeShutdown1.ShuttingDown := true;
  if ComServer <> nil then ComServer.UIInteractive := false;
  inherited;
end;


procedure TMainForm.FormShow(Sender: TObject);
begin
  Caption := Application.Title;
end;


procedure TMainForm.DataFolder1Click(Sender: TObject);
begin
  OpenWebPage(GetIniFolder);
end;


end.

