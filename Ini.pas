unit Ini;

interface

uses
  Windows, SysUtils, Classes, Forms, IniFiles, Math, Utc, Graphics, Controls,
  RefMgr;

function IniFileName: TFileName;
procedure Load;
procedure Save;
procedure LoadPos(Dialog: TCustomForm);
procedure SavePos(Dialog: TCustomForm);
procedure DlgInWorkArea(Dialog: TCustomForm);
procedure SaveSkedDate(NewDate: integer);

var
  InTray: boolean;

implementation

uses
  Main;


const
  SEC_WIN = 'Window';
  SEC_DLG = 'Dialogs';
  SEC_OPT = 'Options';
  SEC_DBG = 'Debugging';

var
  WinPlacement: TWindowPlacement;

const
  R: PRect = @WinPlacement.rcNormalPosition;


function IniFileName: TFileName;
begin
  Result := GetIniName;
end;


procedure Save;
begin
  with TIniFile.Create(IniFileName) do
    try
      //MAIN WINDOW

      //get window rect
      WinPlacement.length := SizeOf(TWindowPlacement);
      if GetWindowPlacement(MainForm.Handle, @WinPlacement) then
        //write window rect
        begin
        WriteInteger(SEC_WIN, 'Left', R.Left);
        WriteInteger(SEC_WIN, 'Top', R.Top);
        WriteInteger(SEC_WIN, 'Right', R.Right);
        WriteInteger(SEC_WIN, 'Bottom', R.Bottom);
        end;

      WriteInteger(SEC_WIN, 'Tab', MainForm.LivePlotFrame1.TabIndex);

      WriteString(SEC_OPT, 'TextFile', MainForm.TextFile);

      //options
      WriteBool(SEC_OPT, 'OnTop', MainForm.FormStyle = fsStayOnTop);
      WriteBool(SEC_OPT, 'InTray', InTray);
      WriteBool(SEC_OPT, 'AutoDL', MainForm.GetAutoDL);
      WriteBool(SEC_OPT, 'Sound', MainForm.FPlaySound);
      WriteString(SEC_OPT, 'SoundFile', MainForm.FSoundFile);
      WriteBool(SEC_OPT, 'DlReports', MainForm.LiveData1.Downloaders.IncludeReports);
    finally
      Free;
    end;

  MainForm.LivePlotFrame1.SavePanelOrder(IniFileName);
end;


procedure Load;
begin
  with TIniFile.Create(IniFileName) do
    try
      //MAIN WINDOW

      //read current window settings
      WinPlacement.length := SizeOf(TWindowPlacement);
      if GetWindowPlacement(MainForm.Handle, @WinPlacement) then
        begin
        WinPlacement.showCmd := SW_HIDE;
        //set default window rect
        SystemParametersInfo(SPI_GETWORKAREA, 0, R, 0);
        R.Left := R.Right - MainForm.Width;
        R.Top := R.Bottom - MainForm.Height;
        //read saved window rect
        R.Left := ReadInteger(SEC_WIN, 'Left', R.Left);
        R.Top := ReadInteger(SEC_WIN, 'Top', R.Top);
        R.Right := ReadInteger(SEC_WIN, 'Right', R.Right);
        R.Bottom := ReadInteger(SEC_WIN, 'Bottom', R.Bottom);
        //update window settings
        SetWindowPlacement(MainForm.Handle, @WinPlacement);
        end;

      MainForm.LivePlotFrame1.TabIndex := ReadInteger(SEC_WIN, 'Tab', 0);

      MainForm.TextFile := ReadString(SEC_OPT, 'TextFile', '');

      //options
      if ReadBool(SEC_OPT, 'OnTop', false)
        then MainForm.FormStyle := fsStayOnTop
        else MainForm.FormStyle := fsNormal;

      InTray := ReadBool(SEC_OPT, 'InTray', false);
      Application.ShowMainForm := not InTray;

      MainForm.FPlaySound := ReadBool(SEC_OPT, 'Sound', false);
      MainForm.FSoundFile := ReadString(SEC_OPT, 'SoundFile',
        ExtractFilePath(ParamStr(0)) + 'Alarm.wav');

      //schedule update
      MainForm.FNeedSchedule := false;//Trunc(UtcNow) >= ReadInteger(SEC_OPT, 'Sked', 0);

      //auto DL
      MainForm.SetAutoDL(ReadBool(SEC_OPT, 'AutoDL', true));
      MainForm.LiveData1.Downloaders.IncludeReports := ReadBool(SEC_OPT, 'DlReports', true);



      //DEBUG:

{
      //enable log permanently
      //[Debugging]
      //AlwaysLog=1
      //LogEnabled := ReadBool(SEC_DBG, 'AlwaysLog', false);
      //enable log for 3 days
      //[Debugging]
      //Log=ON
      S := UpperCase(ReadString(SEC_DBG, 'Log', ''));
      if S = 'ON' then
        begin
        LogEnabled := true;
        WriteString(SEC_DBG, 'Log', IsoDateTimeToStr(Now));
        end;
      LogEnabled := LogEnabled or (Trunc(Now) - Trunc(IsoStrToDateTime(S)) < 3);
      if LogEnabled then MainForm.FLog := TStringList.Create;
}
      //always log
      MainForm.FLog := TStringList.Create;


      //save raw files

      //[Debugging]
      //SaveRawFiles=1

      MainForm.FSaveRaw := ReadBool(SEC_DBG, 'SaveRawFiles', false);


      //enable Import in the menu

      //[Debugging]
      //ImportMenu=1

      MainForm.ImportMNU.Visible := ReadBool(SEC_DBG, 'ImportMenu', false);
    finally
      Free;
    end;

  MainForm.LivePlotFrame1.LoadPanelOrder(IniFileName);

end;



procedure LoadPos(Dialog: TCustomForm);
begin
  with TIniFile.Create(IniFileName) do
    try
      Dialog.Left := ReadInteger(SEC_DLG, Dialog.Name + 'Left', Dialog.Left);
      Dialog.Top := ReadInteger(SEC_DLG, Dialog.Name + 'Top', Dialog.Top);
      if Dialog.BorderStyle in [bsSizeable, bsSizeToolWin] then
        begin
        Dialog.Width := ReadInteger(SEC_DLG, Dialog.Name + 'Width', Dialog.Width);
        Dialog.Height := ReadInteger(SEC_DLG, Dialog.Name + 'Height', Dialog.Height);
        end;
    finally
      Free;
    end;
end;


procedure SavePos(Dialog: TCustomForm);
begin
  with TIniFile.Create(IniFileName) do
    try
      WriteInteger(SEC_DLG, Dialog.Name + 'Left', Dialog.Left);
      WriteInteger(SEC_DLG, Dialog.Name + 'Top', Dialog.Top);
      if Dialog.BorderStyle in [bsSizeable, bsSizeToolWin] then
        begin
        WriteInteger(SEC_DLG, Dialog.Name + 'Width', Dialog.Width);
        WriteInteger(SEC_DLG, Dialog.Name + 'Height', Dialog.Height);
        end;
    finally
      Free;
    end;
end;


procedure DlgInWorkArea(Dialog: TCustomForm);
var
  R: TRect;
begin
  SystemParametersInfo(SPI_GETWORKAREA, 0, @R, 0);
  with Dialog do
    begin
    Left := Min(Left, R.Right - Width);
    Left := Max(Left, R.Left);
    Top := Min(Top, R.Bottom - Height);
    Top := Max(Top, R.Top);
    end;
end;


procedure SaveSkedDate(NewDate: integer);
begin
  with TIniFile.Create(IniFileName) do
    try WriteInteger(SEC_OPT, 'Sked', NewDate);
    finally Free; end;
end;


end.
