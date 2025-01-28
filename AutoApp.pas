unit AutoApp;

interface

uses
  SysUtils, ComObj, ActiveX, AxCtrls, Classes, IonoProbe_TLB, StdVcl,
  Help, SolArray;

type
  TIonoProbeApp = class(TAutoObject, IConnectionPointContainer, IIonoProbeApp)
  private
    { Private declarations }
    FConnectionPoints: TConnectionPoints;
    FConnectionPoint: TConnectionPoint;
    FSinkList: TList;
    FEvents: IIonoProbeAppEvents;
  public
    procedure Initialize; override;
    destructor Destroy; override;
  protected
    { Protected declarations }
    property ConnectionPoints: TConnectionPoints read FConnectionPoints
      implements IConnectionPointContainer;
    procedure EventSinkChanged(const EventSink: IUnknown); override;
    function Version: Integer; safecall;
    function GetValue(Kind: Integer; Utc: TDateTime): Single; safecall;
    function Get_Visible: WordBool; safecall;
    procedure Set_Visible(Value: WordBool); safecall;
    function Get_AutoDL: WordBool; safecall;
    procedure DownloadLatest; safecall;
    procedure DownloadOld; safecall;
    procedure DownloadRecent; safecall;
    procedure Set_AutoDL(Value: WordBool); safecall;
  end;


procedure ComExitRequest;
procedure ComDataArrived;
function IsAutomated: boolean;


implementation

uses ComServ, Main;

var
  ObjList: TList;


function IsAutomated: boolean;
begin
  Result := ObjList.Count > 0;
end;


procedure ComExitRequest;
var
  i: integer;
begin
  for i:=ObjList.Count-1 downto 0 do
    with TIonoProbeApp(ObjList[i]) do
      if FEvents <> nil
        then FEvents.ExitRequest;
end;


procedure ComDataArrived;
var
  i: integer;
begin
  for i:=ObjList.Count-1 downto 0 do
    with TIonoProbeApp(ObjList[i]) do
      if FEvents <> nil
        then FEvents.DataArrived;
end;







procedure TIonoProbeApp.Initialize;
begin
  //auto-generated code
  inherited Initialize;
  FConnectionPoints := TConnectionPoints.Create(Self);
  if AutoFactory.EventTypeInfo <> nil then
    FConnectionPoint := FConnectionPoints.CreateConnectionPoint(
      AutoFactory.EventIID, ckSingle, EventConnect)
  else FConnectionPoint := nil;
  //my code
  ObjList.Add(Self);
end;


destructor TIonoProbeApp.Destroy;
begin
  ObjList.Remove(Self);
  FreeAndNil(FConnectionPoints);
  inherited;
end;


procedure TIonoProbeApp.EventSinkChanged(const EventSink: IUnknown);
begin
  FEvents := EventSink as IIonoProbeAppEvents;
  if FConnectionPoint <> nil then
     FSinkList := FConnectionPoint.SinkList;
end;


function TIonoProbeApp.Version: Integer;
begin
  //in Help.pas
  Result := GetFileVersion;
end;


function TIonoProbeApp.GetValue(Kind: Integer; Utc: TDateTime): Single;
var
  Idx: integer;
  Arr: TSolarArray;
begin
  //indices, 0..99
  if (Kind >= 0) and (Kind <= Ord(High(TDataKind))) then
    begin
    Arr := MainForm.LiveData1.Arrays[TDataKind(Kind)];
    Idx := Arr.TimeToIdx(Utc);
    if (Idx < 0) or (Idx > High(Arr.Data)) or (Arr.Kind[Idx] = vkNone)
      then Result := -999
      else Result := Arr.Data[Idx];
    Exit;
    end;

  //alerts, 100..199
  Dec(Kind, 100);
  if (Kind >= 0) and (Kind <= Ord(High(TAlertKind))) then
    begin
    Result := MainForm.LiveData1.Alerts.Value[TAlertKind(Kind)];
    if Result < 0 then Result := -999;
    Exit;
    end;

  //inferred, 200..299
  Dec(Kind, 100);
  case Kind of
    0: //90-day SSN
      Result := MainForm.LivePlotFrame1.Average90d(MainForm.LiveData1.Arrays[dkSsn], Utc);
    1: //90-day SFI
      Result := MainForm.LivePlotFrame1.Average90d(MainForm.LiveData1.Arrays[dkSfi], Utc);
    else
      Result := -999;
    end;
end;


function TIonoProbeApp.Get_Visible: WordBool;
begin
  Result := MainForm.Visible;
end;


procedure TIonoProbeApp.Set_Visible(Value: WordBool);
begin
  MainForm.Visible := Value;
end;       


procedure TIonoProbeApp.DownloadLatest;
begin
  MainForm.LatestData1MNU.Click;
end;

procedure TIonoProbeApp.DownloadRecent;
begin
  MainForm.RecentDataMNU.Click;
end;

procedure TIonoProbeApp.DownloadOld;
begin
  MainForm.OldDataMNU.Click;
end;

function TIonoProbeApp.Get_AutoDL: WordBool;
begin
  Result := MainForm.GetAutoDL;
end;

procedure TIonoProbeApp.Set_AutoDL(Value: WordBool);
begin
  MainForm.SetAutoDL(Value);
end;



initialization
  TAutoObjectFactory.Create(ComServer, TIonoProbeApp, Class_IonoProbeApp,
    ciMultiInstance, tmApartment);

  ObjList := TList.Create;

finalization
  ObjList.Free;



end.

