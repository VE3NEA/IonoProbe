unit SolArrs;

interface

uses
  Windows, SysUtils, Classes, SolArray, FileCtrl, SsneArr;

type
  TSolarArrays = class(TComponent)
  private
    FGeoAlerts: TGeoAlerts;
    function GetArray(Idx: TDataKind): TSolarArray;
  protected
    FArrays: array [TDataKind] of TSolarArray;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Update;

    procedure Import(Str: string);
    procedure ImportFile(FileName: string);
    procedure LoadFromFiles(ADir: string);
    procedure SaveToFiles(ADir: string);
    property Arrays[index: TDataKind]: TSolarArray  read GetArray; default;
    property Alerts: TGeoAlerts read FGeoAlerts;
  end;


procedure Register;



implementation


procedure Register;
begin
  RegisterComponents('GIS', [TSolarArrays]);
end;


constructor TSolarArrays.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FArrays[dkSsn] := TSsnArray.Create;
  FArrays[dkSfi] := TSfiArray.Create;
  FArrays[dkXRayBg] := TXRayBgArray.Create;
  FArrays[dkProtonBg] := TProtonBgArray.Create;
  FArrays[dkAp] := TApArray.Create;
  FArrays[dkKp3] := TKp3Array.Create;
  FArrays[dkKp1] := TKp1Array.Create;
//  FArrays[dkCostello] := TCostelloArray.Create;
  FArrays[dkIntAp] := TIntApArray.Create;
  FArrays[dkAp1] := TAp1Array.Create;
  FArrays[dkAurora] := TAuroraArray.Create;
  FArrays[dkProtons] := TProtonArray.Create;
  FArrays[dkXrays] := TXRayArray.Create;
  FArrays[dkSsne] := TSsneArray.Create;


  FGeoAlerts := TGeoAlerts.Create;
end;


destructor TSolarArrays.Destroy;
var
  Idx: TDataKind;
begin
  FGeoAlerts.Free;
  for Idx:=Low(TDataKind) to High(TDataKind) do FArrays[Idx].Free;
  inherited Destroy;
end;


function TSolarArrays.GetArray(Idx: TDataKind): TSolarArray;
begin
  Result := FArrays[Idx];
end;


procedure TSolarArrays.LoadFromFiles(ADir: string);
var
  Arr: TDataKind;
begin
  ForceDirectories(ADir);

  for Arr:=Low(TDataKind) to High(TDataKind) do
    if FArrays[Arr] <> nil then
      FArrays[Arr].LoadFromFile(Format('%sData%.2d.dat', [ADir, Integer(Arr)]));

  FGeoAlerts.LoadFromFile(ADir + 'DataAL.dat');
end;


procedure TSolarArrays.SaveToFiles(ADir: string);
var
  Arr: TDataKind;
begin
  if not DirectoryExists(ADir) then Exit;

  for Arr:=Low(TDataKind) to High(TDataKind) do
    if FArrays[Arr] <> nil then
      FArrays[Arr].SaveToFile(Format('%sData%.2d.dat', [ADir, Integer(Arr)]));

  FGeoAlerts.SaveToFile(ADir + 'DataAL.dat');
end;


procedure TSolarArrays.Import(Str: string);
var
  L: TStringList;
  Idx: TDataKind;
begin
  //import GZip
  if Copy(Str, 1, 2) = '' then
    begin

    Exit;
    end;

  //import text
  L := TStringList.Create;
  try
    L.Text := AdjustLineBreaks(Str);
    if L.Count = 0 then Exit;
    for Idx:=Low(TDataKind) to High(TDataKind) do
      if FArrays[Idx] <> nil then FArrays[Idx].ImportText(L);
    FGeoAlerts.Import(L);
  finally
    L.Free;
  end;
end;


procedure TSolarArrays.Update;
var
  Idx: TDataKind;
begin
  for Idx:=Low(TDataKind) to High(TDataKind) do
    if FArrays[Idx] <> nil then FArrays[Idx].Update;
end;




procedure TSolarArrays.ImportFile(FileName: string);
var
  Fs: TFileStream;
begin
  Fs := TFileStream.Create(FileName, fmOpenRead);
  try
    with TStringStream.Create('') do
      try
        CopyFrom(Fs, 0);
        Import(DataString);
      finally Free; end;
  finally Fs.Free; end;

end;

end.

