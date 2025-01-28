program IonoProbe;

uses
  Forms,
  Main in 'Main.pas' {MainForm},
  Ini in 'Ini.pas',
  SetDlg in 'SetDlg.pas' {SettingsDialog},
  AboutDlg in 'AboutDlg.pas' {AboutDialog},
  IonoProbe_TLB in 'IonoProbe_TLB.pas',
  AutoApp in 'AutoApp.pas' {IonoProbeApp: CoClass},
  Fram in 'Fram.pas' {LivePlotFrame: TFrame},
  SolArray in 'VCL\AlProp\SolArray.pas',
  GeomFun in 'VCL\AlProp\GeomFun.pas',
  SolArrs in 'VCL\AlProp\SolArrs.pas',
  SsneArr in 'VCL\AlProp\SsneArr.pas',
  SpherHrm in 'VCL\AlProp\IriClass\SpherHrm.pas',
  Interpol in 'VCL\AlProp\IriClass\Interpol.pas',
  IriData in 'VCL\AlProp\IriClass\IriData.pas',
  MagFld in 'VCL\AlProp\IriClass\MagFld.pas',
  Solar in 'VCL\AlProp\IriClass\Solar.pas',
  LiveData in 'VCL\AlProp\LiveData.pas',
  DLoader in 'VCL\AlNet\Dloader.pas',
  DynUrlMn in 'VCL\AlNet\DynUrlMn.pas',
  DlSched in 'VCL\AlNet\DlSched.pas',
  TrayIcon in 'Vendor\TrayIcon\TrayIcon.pas',
  Utc in 'VCL\AlCommon\Utc.pas',
  AutoStrt in 'VCL\AlCommon\AutoStrt.pas',
  Help in 'VCL\AlCommon\Help.pas',
  QuickPnl in 'VCL\AlCommon\QuickPnl.pas',
  PermHint in 'VCL\AlCommon\PermHint.pas',
  AlStrLst in 'VCL\AlCommon\AlStrLst.pas',
  SafeShDown in 'VCL\AlCommon\SafeShDown.pas',
  RefMgr in 'VCL\Ham\RefMgr.pas',
  ExecWait in 'VCL\AlCommon\ExecWait.pas',
  ComServ in 'VCL\VclFix\comserv.pas',
  TgzInMem_D5 in 'VCL\AlProp\TgzInMem_D5.pas',
  ZLibExGZ in 'Vendor\ZLib_D5\ZLibExGZ.pas',
  ZLibEx in 'Vendor\ZLib_D5\ZLibEx.pas',
  MyMath in 'VCL\Math\MyMath.pas';

{$R *.TLB}

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Title := 'IonoProbe 1.42';
  Application.Run;
end.

