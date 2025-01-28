unit IonoProbe_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// PASTLWTR : $Revision:   1.88.1.0.1.0  $
// File generated on 2003-02-27 PM 1:51:20 from Type Library described below.

// *************************************************************************//
// NOTE:                                                                      
// Items guarded by $IFDEF_LIVE_SERVER_AT_DESIGN_TIME are used by properties  
// which return objects that may need to be explicitly created via a function 
// call prior to any access via the property. These items have been disabled  
// in order to prevent accidental use from within the object inspector. You   
// may enable them by defining LIVE_SERVER_AT_DESIGN_TIME or by selectively   
// removing them from the $IFDEF blocks. However, such items must still be    
// programmatically created via a method of the appropriate CoClass before    
// they can be used.                                                          
// ************************************************************************ //
// Type Lib: D:\IonoProbe\IonoProbe.exe (1)
// IID\LCID: {D78B6B38-B90D-4E47-8A95-CD1E53A6440D}\0
// Helpfile: 
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINNT\System32\Stdole2.tlb)
//   (2) v4.0 StdVCL, (C:\WINNT\System32\STDVCL40.DLL)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
interface

uses Windows, ActiveX, Classes, Graphics, OleServer, OleCtrls, StdVCL;

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  IonoProbeMajorVersion = 1;
  IonoProbeMinorVersion = 0;

  LIBID_IonoProbe: TGUID = '{D78B6B38-B90D-4E47-8A95-CD1E53A6440D}';

  IID_IIonoProbeApp: TGUID = '{B891679C-E9CC-4C8F-8911-D9D6B80262C9}';
  DIID_IIonoProbeAppEvents: TGUID = '{F0E69A36-8CEE-4EC9-8389-3274626BE261}';
  CLASS_IonoProbeApp: TGUID = '{E08045D3-A708-4AF6-B6EB-4246D9E5CF46}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IIonoProbeApp = interface;
  IIonoProbeAppDisp = dispinterface;
  IIonoProbeAppEvents = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  IonoProbeApp = IIonoProbeApp;


// *********************************************************************//
// Interface: IIonoProbeApp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B891679C-E9CC-4C8F-8911-D9D6B80262C9}
// *********************************************************************//
  IIonoProbeApp = interface(IDispatch)
    ['{B891679C-E9CC-4C8F-8911-D9D6B80262C9}']
    function  Version: Integer; safecall;
    function  GetValue(Kind: Integer; Utc: TDateTime): Single; safecall;
    function  Get_Visible: WordBool; safecall;
    procedure Set_Visible(Value: WordBool); safecall;
    function  Get_AutoDL: WordBool; safecall;
    procedure Set_AutoDL(Value: WordBool); safecall;
    procedure DownloadLatest; safecall;
    procedure DownloadRecent; safecall;
    procedure DownloadOld; safecall;
    property Visible: WordBool read Get_Visible write Set_Visible;
    property AutoDL: WordBool read Get_AutoDL write Set_AutoDL;
  end;

// *********************************************************************//
// DispIntf:  IIonoProbeAppDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {B891679C-E9CC-4C8F-8911-D9D6B80262C9}
// *********************************************************************//
  IIonoProbeAppDisp = dispinterface
    ['{B891679C-E9CC-4C8F-8911-D9D6B80262C9}']
    function  Version: Integer; dispid 1;
    function  GetValue(Kind: Integer; Utc: TDateTime): Single; dispid 2;
    property Visible: WordBool dispid 3;
    property AutoDL: WordBool dispid 4;
    procedure DownloadLatest; dispid 5;
    procedure DownloadRecent; dispid 6;
    procedure DownloadOld; dispid 7;
  end;

// *********************************************************************//
// DispIntf:  IIonoProbeAppEvents
// Flags:     (4096) Dispatchable
// GUID:      {F0E69A36-8CEE-4EC9-8389-3274626BE261}
// *********************************************************************//
  IIonoProbeAppEvents = dispinterface
    ['{F0E69A36-8CEE-4EC9-8389-3274626BE261}']
    procedure ExitRequest; dispid 1;
    procedure DataArrived; dispid 2;
  end;

// *********************************************************************//
// The Class CoIonoProbeApp provides a Create and CreateRemote method to          
// create instances of the default interface IIonoProbeApp exposed by              
// the CoClass IonoProbeApp. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoIonoProbeApp = class
    class function Create: IIonoProbeApp;
    class function CreateRemote(const MachineName: string): IIonoProbeApp;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TIonoProbeApp
// Help String      : IonoProbeApp Object
// Default Interface: IIonoProbeApp
// Def. Intf. DISP? : No
// Event   Interface: IIonoProbeAppEvents
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TIonoProbeAppProperties= class;
{$ENDIF}
  TIonoProbeApp = class(TOleServer)
  private
    FOnExitRequest: TNotifyEvent;
    FOnDataArrived: TNotifyEvent;
    FIntf:        IIonoProbeApp;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps:       TIonoProbeAppProperties;
    function      GetServerProperties: TIonoProbeAppProperties;
{$ENDIF}
    function      GetDefaultInterface: IIonoProbeApp;
  protected
    procedure InitServerData; override;
    procedure InvokeEvent(DispID: TDispID; var Params: TVariantArray); override;
    function  Get_Visible: WordBool;
    procedure Set_Visible(Value: WordBool);
    function  Get_AutoDL: WordBool;
    procedure Set_AutoDL(Value: WordBool);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IIonoProbeApp);
    procedure Disconnect; override;
    function  Version: Integer;
    function  GetValue(Kind: Integer; Utc: TDateTime): Single;
    procedure DownloadLatest;
    procedure DownloadRecent;
    procedure DownloadOld;
    property  DefaultInterface: IIonoProbeApp read GetDefaultInterface;
    property Visible: WordBool read Get_Visible write Set_Visible;
    property AutoDL: WordBool read Get_AutoDL write Set_AutoDL;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TIonoProbeAppProperties read GetServerProperties;
{$ENDIF}
    property OnExitRequest: TNotifyEvent read FOnExitRequest write FOnExitRequest;
    property OnDataArrived: TNotifyEvent read FOnDataArrived write FOnDataArrived;
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TIonoProbeApp
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TIonoProbeAppProperties = class(TPersistent)
  private
    FServer:    TIonoProbeApp;
    function    GetDefaultInterface: IIonoProbeApp;
    constructor Create(AServer: TIonoProbeApp);
  protected
    function  Get_Visible: WordBool;
    procedure Set_Visible(Value: WordBool);
    function  Get_AutoDL: WordBool;
    procedure Set_AutoDL(Value: WordBool);
  public
    property DefaultInterface: IIonoProbeApp read GetDefaultInterface;
  published
    property Visible: WordBool read Get_Visible write Set_Visible;
    property AutoDL: WordBool read Get_AutoDL write Set_AutoDL;
  end;
{$ENDIF}


procedure Register;

implementation

uses ComObj;

class function CoIonoProbeApp.Create: IIonoProbeApp;
begin
  Result := CreateComObject(CLASS_IonoProbeApp) as IIonoProbeApp;
end;

class function CoIonoProbeApp.CreateRemote(const MachineName: string): IIonoProbeApp;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_IonoProbeApp) as IIonoProbeApp;
end;

procedure TIonoProbeApp.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{E08045D3-A708-4AF6-B6EB-4246D9E5CF46}';
    IntfIID:   '{B891679C-E9CC-4C8F-8911-D9D6B80262C9}';
    EventIID:  '{F0E69A36-8CEE-4EC9-8389-3274626BE261}';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TIonoProbeApp.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    ConnectEvents(punk);
    Fintf:= punk as IIonoProbeApp;
  end;
end;

procedure TIonoProbeApp.ConnectTo(svrIntf: IIonoProbeApp);
begin
  Disconnect;
  FIntf := svrIntf;
  ConnectEvents(FIntf);
end;

procedure TIonoProbeApp.DisConnect;
begin
  if Fintf <> nil then
  begin
    DisconnectEvents(FIntf);
    FIntf := nil;
  end;
end;

function TIonoProbeApp.GetDefaultInterface: IIonoProbeApp;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call ''Connect'' or ''ConnectTo'' before this operation');
  Result := FIntf;
end;

constructor TIonoProbeApp.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TIonoProbeAppProperties.Create(Self);
{$ENDIF}
end;

destructor TIonoProbeApp.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TIonoProbeApp.GetServerProperties: TIonoProbeAppProperties;
begin
  Result := FProps;
end;
{$ENDIF}

procedure TIonoProbeApp.InvokeEvent(DispID: TDispID; var Params: TVariantArray);
begin
  case DispID of
    -1: Exit;  // DISPID_UNKNOWN
   1: if Assigned(FOnExitRequest) then
            FOnExitRequest(Self);
   2: if Assigned(FOnDataArrived) then
            FOnDataArrived(Self);
  end; {case DispID}
end;

function  TIonoProbeApp.Get_Visible: WordBool;
begin
  Result := DefaultInterface.Get_Visible;
end;

procedure TIonoProbeApp.Set_Visible(Value: WordBool);
begin
  DefaultInterface.Set_Visible(Value);
end;

function  TIonoProbeApp.Get_AutoDL: WordBool;
begin
  Result := DefaultInterface.Get_AutoDL;
end;

procedure TIonoProbeApp.Set_AutoDL(Value: WordBool);
begin
  DefaultInterface.Set_AutoDL(Value);
end;

function  TIonoProbeApp.Version: Integer;
begin
  Result := DefaultInterface.Version;
end;

function  TIonoProbeApp.GetValue(Kind: Integer; Utc: TDateTime): Single;
begin
  Result := DefaultInterface.GetValue(Kind, Utc);
end;

procedure TIonoProbeApp.DownloadLatest;
begin
  DefaultInterface.DownloadLatest;
end;

procedure TIonoProbeApp.DownloadRecent;
begin
  DefaultInterface.DownloadRecent;
end;

procedure TIonoProbeApp.DownloadOld;
begin
  DefaultInterface.DownloadOld;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TIonoProbeAppProperties.Create(AServer: TIonoProbeApp);
begin
  inherited Create;
  FServer := AServer;
end;

function TIonoProbeAppProperties.GetDefaultInterface: IIonoProbeApp;
begin
  Result := FServer.DefaultInterface;
end;

function  TIonoProbeAppProperties.Get_Visible: WordBool;
begin
  Result := DefaultInterface.Get_Visible;
end;

procedure TIonoProbeAppProperties.Set_Visible(Value: WordBool);
begin
  DefaultInterface.Set_Visible(Value);
end;

function  TIonoProbeAppProperties.Get_AutoDL: WordBool;
begin
  Result := DefaultInterface.Get_AutoDL;
end;

procedure TIonoProbeAppProperties.Set_AutoDL(Value: WordBool);
begin
  DefaultInterface.Set_AutoDL(Value);
end;

{$ENDIF}

procedure Register;
begin
  RegisterComponents('Snd',[TIonoProbeApp]);
end;

end.
