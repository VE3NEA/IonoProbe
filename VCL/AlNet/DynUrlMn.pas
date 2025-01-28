unit DynUrlMn;

interface

uses
  Windows, SysUtils, ActiveX, UrlMon;

function IsUrlMonLoaded: boolean;

var
  DynCreateURLMoniker: function (MkCtx: IMoniker; szURL: LPCWSTR;
    out mk: IMoniker): HResult; stdcall;

  DynCreateAsyncBindCtx: function (reserved: DWORD;
    pBSCb: IBindStatusCallback; pEFetc: IEnumFORMATETC;
    out ppBC: IBindCtx): HResult; stdcall;

  DynRegisterBindStatusCallback: function (pBC: IBindCtx;
    pBSCb: IBindStatusCallback; out ppBSCBPrev: IBindStatusCallback;
    dwReserved: DWORD): HResult; stdcall;



implementation

//------------------------------------------------------------------------------
//                        Load URLMON.DLL dynamically
//------------------------------------------------------------------------------
var
  HUrlMon: THandle;

procedure UnLoadUrlMon;
begin
  if HUrlMon <> 0 then try FreeLibrary(HUrlMon); except end;
  HUrlMon := 0;
end;


procedure LoadUrlMon;
begin
  try
    HUrlMon := LoadLibrary('URLMON.DLL');

    if HUrlMon = 0 then Exit;

    DynCreateURLMoniker := GetProcAddress(HUrlMon, 'CreateURLMoniker');
    DynCreateAsyncBindCtx := GetProcAddress(HUrlMon, 'CreateAsyncBindCtx');
    DynRegisterBindStatusCallback := GetProcAddress(HUrlMon, 'RegisterBindStatusCallback');

    if (@DynCreateURLMoniker = nil) or
       (@DynCreateAsyncBindCtx = nil) or
       (@DynRegisterBindStatusCallback = nil)
      then UnLoadUrlMon;
  except
  end;
end;


function IsUrlMonLoaded: boolean;
begin
  Result := HUrlMon <> 0;
end;




initialization
  LoadUrlMon;

finalization
  UnLoadUrlMon;


end.

