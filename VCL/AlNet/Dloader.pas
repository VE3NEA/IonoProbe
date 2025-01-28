unit DLoader;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ActiveX, UrlMon, DynUrlMn, WinInet, Utc;
                                
type                                                        
  TMonikerObj = class;

  TDownloadedFile = record
    Url: AnsiString;
    FileName: TFileName;
    FileTime: TDateTime;
    FileSize: DWORD;
    ByteCount: DWORD;
    StartPos: integer;
    Data: AnsiString;
    ErrorText: string;
    ErrorCode: integer;
    Purpose: integer;
    end;
    

  TDownloader = class (TComponent)
  private
    FMonikerObj: TMonikerObj;
    FPostData: AnsiString;
    FHeaders: AnsiString;

    FOnError: TNotifyEvent;
    FOnSuccess: TNotifyEvent;
    FOnData: TNotifyEvent;

    procedure Err(const Txt: string; RC: integer);
    procedure DoDownload(Url: AnsiString; NewerThan: TDateTime; StartPos: integer);
  public
    Client: Pointer;
    DownloadedFile: TDownloadedFile;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Download(Url: AnsiString; NewerThan: TDateTime = 0; StartPos: integer = 0);
    procedure Post(Url, Data: AnsiString; NewerThan: TDateTime = 0; StartPos: integer = 0); overload;
    procedure Post(Url, Data, Headers: AnsiString); overload;

    procedure AbortDownload;
    function IsBusy: boolean;

  published
    property OnData: TNotifyEvent read FOnData write FOnData;
    property OnSuccess: TNotifyEvent read FOnSuccess write FOnSuccess;
    property OnError: TNotifyEvent read FOnError write FOnError;
  end;


  TMonikerObj = class(TInterfacedObject, IBindStatusCallback, IHttpNegotiate)
  private
    Papa: TDownloader;

    FMoniker: IMoniker;
    FBindCtx: IBindCtx;
    FOldCallback: IBindStatusCallback;
    FBinding: IBinding;
    FStream: IStream;

    //IBindStatusCallback
    function OnStartBinding(dwReserved: DWORD; pib: IBinding): HResult; stdcall;
    function GetPriority(out nPriority): HResult; stdcall;
    function OnLowResource(reserved: DWORD): HResult; stdcall;
    function OnProgress(ulProgress, ulProgressMax, ulStatusCode: ULONG;
      szStatusText: LPCWSTR): HResult; stdcall;
    function OnStopBinding(hresult: HResult; szError: LPCWSTR): HResult; stdcall;
    function GetBindInfo(out grfBINDF: DWORD; var bindinfo: TBindInfo): HResult; stdcall;
    function OnDataAvailable(grfBSCF: DWORD; dwSize: DWORD; formatetc: PFormatEtc;
      stgmed: PStgMedium): HResult; stdcall;
    function OnObjectAvailable(const iid: TGUID; punk: IUnknown): HResult; stdcall;
    //IHttpNegotiate
    function BeginningTransaction(szURL, szHeaders: LPCWSTR; dwReserved: DWORD;
      out szAdditionalHeaders: LPWSTR): HResult; stdcall;
    function OnResponse(dwResponseCode: DWORD; szResponseHeaders, szRequestHeaders: LPCWSTR;
      out szAdditionalRequestHeaders: LPWSTR): HResult; stdcall;
  public
    constructor Create(APapa: TDownloader);
    procedure Start;
    procedure Stop;
  end;



function FileNameFromUrl(AUrl: AnsiString): TFileName;


procedure Register;




implementation


procedure Register;
begin
  RegisterComponents('AL', [TDownloader]);
end;


function FileNameFromUrl(AUrl: AnsiString): TFileName;
var
  p: integer;
begin
  p := LastDelimiter('/', AUrl);
  if (p = 0)
    then Result := ''
    else Result := Copy(AUrl, p+1, MAXINT);
end;


//------------------------------------------------------------------------------
//                             TDownloader
//------------------------------------------------------------------------------

constructor TDownloader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMonikerObj := TMonikerObj.Create(Self);
  FMonikerObj._AddRef;
end;


destructor TDownloader.Destroy;
begin
  AbortDownload;

  FMonikerObj._Release;
  FMonikerObj := nil;
  inherited Destroy;
end;


procedure TDownloader.Err(const Txt: string; RC: integer);
begin
  DownloadedFile.ErrorText := Format(Txt, [RC]);
  DownloadedFile.ErrorCode := RC;
  AbortDownload;

  if Assigned(FOnError) then
    try FOnError(Self); except Application.HandleException(Self); end;
end;


procedure TDownloader.AbortDownload;
begin
  FMonikerObj.Stop;
end;


procedure TDownloader.Download(Url: AnsiString; NewerThan: TDateTime; StartPos: integer);
begin
  FPostData := '';
  FHeaders := '';
  DoDownload(Url, NewerThan, StartPos);
end;


procedure TDownloader.Post(Url, Data: AnsiString; NewerThan: TDateTime; StartPos: integer);
begin
  FPostData := Data;
  FHeaders := '';
  DoDownload(Url, NewerThan, StartPos);
end;


procedure TDownloader.Post(Url, Data, Headers: AnsiString);
begin
  FPostData := Data;
  FHeaders := Headers;
  DoDownload(Url, 0, 0);
end;


var
  BlankDlFile: TDownloadedFile;

procedure TDownloader.DoDownload(Url: AnsiString; NewerThan: TDateTime; StartPos: integer);
begin
  DownloadedFile := BlankDlFile;

  DownloadedFile.Url := Url;
  DownloadedFile.FileName := FileNameFromUrl(Url);
  DownloadedFile.FileTime := NewerThan;
  DownloadedFile.StartPos := StartPos;

  if not IsUrlMonLoaded then begin Err('IE not found', -1); Exit; end;

  FMonikerObj.Start;
end;



function TDownloader.IsBusy: boolean;
begin
  Result := FMonikerObj.FBinding <> nil;
end;



//------------------------------------------------------------------------------
//                             TMonikerObj
//------------------------------------------------------------------------------

constructor TMonikerObj.Create(APapa: TDownloader);
begin
  inherited Create;
  Papa := APapa;
end;


procedure TMonikerObj.Stop;
begin
  if FBinding <> nil then FBinding.Abort;

  FMoniker := nil;
  FBindCtx := nil;
  FOldCallback := nil;
  FBinding := nil;
  FStream := nil;
end;


procedure TMonikerObj.Start;
var
  rc: HResult;
  Storage: IUnknown;
  FWideUrl: WideString;
begin
  FWideUrl := Papa.DownloadedFile.Url;

  rc := DynCreateURLMoniker(nil, PWideChar(FWideUrl), FMoniker);
  if not SUCCEEDED(rc) then begin Papa.Err('CreateURLMoniker error %x', rc); Exit; end;

  rc := DynCreateAsyncBindCtx(0, Self as IBindStatusCallback, nil, FBindCtx);
  if not SUCCEEDED(rc) then begin Papa.Err('CreateAsyncBindCtx error %x', rc); Exit; end;

  rc := DynRegisterBindStatusCallback(FBindCtx, Self as IBindStatusCallback, FOldCallback, 0);
  if not SUCCEEDED(rc) then begin Papa.Err('RegisterBindStatusCallback error %x', rc); Exit; end;

  rc := FMoniker.BindToStorage(FBindCtx, nil, IStream, Storage);
  Storage := nil; //release
  if not SUCCEEDED(rc) then begin Papa.Err('BindToStorage error %x', rc); Exit; end;
end;







//------------------------------------------------------------------------------
//                   TMonikerObj  IBindStatusCallback
//------------------------------------------------------------------------------

function TMonikerObj.GetBindInfo(out grfBINDF: DWORD;
  var bindinfo: TBindInfo): HResult;
const
 BINDF_PREFERDEFAULTHANDLER = $00400000;
 BINDF_FROMURLMON = $00100000;
var
  Size: integer;
  P: Pointer;
begin
  if (@bindInfo = nil) or (bindInfo.cbSize = 0) or (@grfBINDF = nil)
    then begin Result := E_INVALIDARG; Exit; end;

  grfBINDF := BINDF_ASYNCHRONOUS or     //BindToStorage will return asynchronously
              BINDF_ASYNCSTORAGE or     //IStream will be non-blocking
              BINDF_GETNEWESTVERSION or // =  WinInet INTERNET_FLAG_RELOAD
              BINDF_NOWRITECACHE  or BINDF_PULLDATA or //no temp files
              BINDF_IGNORESECURITYPROBLEM or
              BINDF_RESYNCHRONIZE or    //download only if newer
              BINDF_NO_UI or
              BINDF_SILENTOPERATION or
              BINDF_PRAGMA_NO_CACHE or  //do not write to cache
              BINDF_FROMURLMON or
              BINDF_PREFERDEFAULTHANDLER; //do not use custom spacename handlers

  Size := bindinfo.cbSize;
  FillChar(bindinfo, Size, 0);
  bindinfo.cbSize := Size;

  Size := Length(Papa.FPostData);
  if Size = 0
    then
      bindInfo.dwBindVerb := BINDVERB_GET
    else
      begin
      bindInfo.dwBindVerb := BINDVERB_POST;
      bindInfo.stgmedData.hGlobal  := GlobalAlloc(GMEM_SHARE,  Size);
      P := GlobalLock(bindinfo.stgmedData.hGlobal);
      if P <> nil then Move(Papa.FPostData[1],  P^,  Size);
      GlobalUnlock(bindinfo.stgmedData.hGlobal);
      bindinfo.stgmedData.tymed := TYMED_HGLOBAL;
      bindinfo.cbstgmedData := Size;
      end;

  Result := S_OK;
end;


function TMonikerObj.OnStartBinding(dwReserved: DWORD; pib: IBinding): HResult;
begin
  FBinding := pib;
  Result := S_OK;
end;


function TMonikerObj.OnProgress(ulProgress, ulProgressMax, ulStatusCode: ULONG;
  szStatusText: LPCWSTR): HResult;
const
  QRY_SIZE = HTTP_QUERY_CONTENT_LENGTH;
  QRY_TIME = HTTP_QUERY_LAST_MODIFIED or HTTP_QUERY_FLAG_SYSTEMTIME;
var
  HInfo: IWinInetHttpInfo;
  StrBuf: string;
  TimeBuf: TSystemTime;
  Len, Flags, Reserved: DWORD;
begin
  if ulStatusCode = BINDSTATUS_BEGINDOWNLOADDATA then
    try
      HInfo := FBinding as IWinInetHttpInfo;

      //file size
      Len := 1024; Flags := 0; Reserved := 0;
      SetLength(StrBuf, Len);
      if S_OK = HInfo.QueryInfo(QRY_SIZE, PChar(StrBuf), Len, Flags, Reserved)
        then Papa.DownloadedFile.FileSize := StrToIntDef(Copy(StrBuf, 1, Len), 0)
        else Papa.DownloadedFile.FileSize := 0;
      //file time
      Len := SizeOf(TSystemTime); Flags := 0; Reserved := 0;
      if S_OK = HInfo.QueryInfo(QRY_TIME, @TimeBuf, Len, Flags, Reserved)
        then Papa.DownloadedFile.FileTime := SystemTimeToDateTime(TimeBuf)
        else Papa.DownloadedFile.FileTime := 0;
    except end;


  Result := S_OK;
end;


function TMonikerObj.OnDataAvailable(grfBSCF, dwSize: DWORD;
  formatetc: PFormatEtc; stgmed: PStgMedium): HResult;
var
  NewData: AnsiString;
  LenAv, LenRd: integer;
  rc: HResult;
begin
  NewData := '';
  Result := S_OK;

  //store intf to stream
  if ((BSCF_FIRSTDATANOTIFICATION and grfBSCF) <> 0) and
     (FStream = nil) and (stgmed.tymed = TYMED_ISTREAM)
    then FStream := IStream(stgmed.stm);

  //get data
  rc := 0;
  LenAv := {Integer}(dwSize) - Papa.DownloadedFile.ByteCount;
  if (FStream <> nil) and (LenAv > 0) then
    repeat
      NewData := StringOfChar(' ', LenAv);
      //SetLength(NewData, LenAv);
      rc := FStream.Read(@NewData[1], LenAv, @LenRd);
      SetLength(NewData, LenRd);
      Papa.DownloadedFile.Data := Papa.DownloadedFile.Data + NewData;
      Inc(Papa.DownloadedFile.ByteCount, LenRd);
    until
      //E_PENDING - downloading, S_FALSE - end of data
      (rc = HResult(E_PENDING)) or (rc = S_FALSE) or not (SUCCEEDED(rc));

  //data end, release intf to stream
  if (BSCF_LASTDATANOTIFICATION and grfBSCF) <> 0 then FStream := nil;

  //use data
  if Assigned(Papa.FOnData) then
    try Papa.FOnData(Papa); except Application.HandleException(Self); end;

  if (rc <> HResult(E_PENDING)) and not (SUCCEEDED(rc))
    then Papa.Err('Connection Error %x', rc);
end;



function TMonikerObj.OnStopBinding(hresult: HResult; szError: LPCWSTR): HResult;
begin
  Result := S_OK;

  case hresult of
    E_ABORT: ;

    S_OK:
      begin
      Stop;
      Papa.DownloadedFile.ErrorText := 'OK';
      Papa.DownloadedFile.ErrorCode := 0;
      if Assigned(Papa.FOnSuccess) then
        try Papa.FOnSuccess(Papa); except Application.HandleException(Self); end;
      end;

    else
      Papa.Err('Moniker error %x', hresult);
  end;
end;



function TMonikerObj.GetPriority(out nPriority): HResult;
  begin Result := E_NOTIMPL; end;

function TMonikerObj.OnLowResource(reserved: DWORD): HResult;
  begin Result := E_NOTIMPL; end;

function TMonikerObj.OnObjectAvailable(const iid: TGUID; punk: IUnknown): HResult;
  begin Result := E_NOTIMPL; end;





//------------------------------------------------------------------------------
//                   TMonikerObj  IHttpNegotiate
//------------------------------------------------------------------------------

function TMonikerObj.BeginningTransaction(szURL, szHeaders: LPCWSTR;
  dwReserved: DWORD; out szAdditionalHeaders: LPWSTR): HResult;
const
  MoreHeaders = 'Pragma: no-cache' + #13#10 + 'Cache-Control: no-cache' + #13#10 + 'Accept-Encoding: gzip, deflate'#13#10;
var
  p, Len: integer;
  ExtraHeaders: WideString;
begin
  ExtraHeaders := Papa.FHeaders + MoreHeaders;

  //restart at pos
  p := Papa.DownloadedFile.StartPos;
  if p < 0 then
    ExtraHeaders := ExtraHeaders + 'Range: bytes=' + IntToStr(p) + #13#10
  else if p > 0 then
    ExtraHeaders := ExtraHeaders + 'Range: bytes=0' + IntToStr(p) + '-' + #13#10;

  //only if changed
  if Papa.DownloadedFile.FileTime <> 0 then
  ExtraHeaders := ExtraHeaders + 'If-Modified-Since: '
    + HttpDateTimeToStr(Papa.DownloadedFile.FileTime) + #13#10;

  //allocate memory and copy headers
  szAdditionalheaders := nil;
  Len := Length(ExtraHeaders);
  if Len > 0 then
    //I would never guess the use of CoTaskMemAlloc was necessary
    //but in fact it is: http://msdn.microsoft.com/en-us/library/ms686638
    begin
    szAdditionalheaders := CoTaskMemAlloc((Len+1) * SizeOf(WideChar));
   if Len > 0 then Move(ExtraHeaders[1], szAdditionalheaders^, Len * SizeOf(WideChar));
    szAdditionalheaders[Len] := #0;
    end;

  Result := S_OK;
end;


function TMonikerObj.OnResponse(dwResponseCode: DWORD; szResponseHeaders,
  szRequestHeaders: LPCWSTR;
  out szAdditionalRequestHeaders: LPWSTR): HResult;
var
  p: integer;
  S: string;
begin
  case dwResponseCode of
    200, 206:
      begin
      //extract real file name
      p := Pos('filename="', szResponseHeaders);
      if p > 0 then
        begin
        S := Copy(szResponseHeaders, p+10, MAXINT);
        p := Pos('"', S);
        if p > 0 then Papa.DownloadedFile.FileName := Copy(S, 1, p-1);;
        end;
      end;

    304:
      begin
      Papa.DownloadedFile.Data := '';
      Papa.DownloadedFile.ErrorText := 'Not Modified';
      Papa.DownloadedFile.ErrorCode := 0;
      Stop;
      if Assigned(Papa.FOnError) then
        try Papa.FOnError(Papa); except Application.HandleException(Self); end;
      end;

    else Papa.Err('Server reply code is %d', dwResponseCode);
    end;

  szAdditionalRequestHeaders := nil;
  Result := S_OK;
end;




end.

