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
// File generated on 1/26/2011 11:11:17 AM from Type Library described below.

// ************************************************************************ //
// Type Lib: C:\Proj\Published\IonoProbe\IonoProbe.tlb (1)
// IID\LCID: {D78B6B38-B90D-4E47-8A95-CD1E53A6440D}\0
// Helpfile: 
// DepndLst: 
//   (1) v2.0 stdole, (C:\Windows\system32\stdole2.tlb)
//   (2) v4.0 StdVCL, (C:\Windows\SysWow64\STDVCL40.DLL)
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

end.
