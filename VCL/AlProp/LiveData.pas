unit LiveData;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  SolArrs, DLoader, DlSched, SolArray, SsneArr, RefMgr;

type


  TLiveData = class(TSolarArrays)
  private
    FDownloaders: TDownloadScheduler;

    FOnDlSuccess: TFileEvent;
    FOnDlError: TFileEvent;
    FOnDlFinish: TNotifyEvent;
    FOnDlStart: TNotifyEvent;

    procedure GetFileEvent(ASender: TObject; const AFile: TDownloadedFile);
    procedure ErrorEvent(ASender: TObject; const AFile: TDownloadedFile);
    procedure FinishEvent(Sender: TObject);
    procedure StartEvent(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Tick;
    property Downloaders: TDownloadScheduler read FDownloaders write FDownloaders;
  published
    property OnDlStart: TNotifyEvent read FOnDlStart write FOnDlStart;
    property OnDlSuccess: TFileEvent read FOnDlSuccess write FOnDlSuccess;
    property OnDlError: TFileEvent read FOnDlError write FOnDlError;
    property OnDlFinish: TNotifyEvent read FOnDlFinish write FOnDlFinish;
  end;

procedure Register;

implementation


procedure Register;
begin
  RegisterComponents('GIS', [TLiveData]);
end;




{ TLiveData }

constructor TLiveData.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDownloaders := TDownloadScheduler.Create;
  FDownloaders.OnGetFile := GetFileEvent;
  FDownloaders.OnError := ErrorEvent;
  FDownloaders.OnFinish := FinishEvent;
  FDownloaders.OnStart := StartEvent;
  FDownloaders.LoadSchedule(GetIniFolder + 'Schedule.ini');
end;


destructor TLiveData.Destroy;
begin
  FDownloaders.Free;
  inherited Destroy;
end;


procedure TLiveData.Tick;
begin
  Update;
  FDownloaders.Tick;
end;




//------------------------------------------------------------------------------
//                         downloader events
//------------------------------------------------------------------------------
procedure TLiveData.StartEvent(Sender: TObject);
begin
  if Assigned(FOnDlStart) then FOnDlStart(Self);
end;


procedure TLiveData.GetFileEvent(ASender: TObject; const AFile: TDownloadedFile);
begin
  Update;

  //if UpperCase(ExtractFileExt(AFile.FileName)) = '.GZ'
  case AFile.Purpose of
    0: //fpIndex
      Import(AFile.Data);
    1: //fpIonosonde
      (Arrays[dkSsne] as TSsneArray).ImportTgz(AFile.Data);
    //fpText is not processed
    end;

  if Assigned(FOnDlSuccess) then FOnDlSuccess(Self, AFile);
end;


procedure TLiveData.ErrorEvent(ASender: TObject; const AFile: TDownloadedFile);
begin
  if Assigned(FOnDlError) then FOnDlError(Self, AFile);
end;


procedure TLiveData.FinishEvent(Sender: TObject);
begin
  if Assigned(FOnDlFinish) then FOnDlFinish(Self);
end;





end.

