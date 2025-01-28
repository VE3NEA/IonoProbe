unit FramLoad;

interface

uses
  Windows, SysUtils, Controls, FramIntf, Registry;


procedure UnloadFrame;
function LoadFrame(AParent: TWinControl): boolean;

var
  Frm: ILivePlotFrame;



implementation

var
  FrmPkg: THandle;


function LoadFrame(AParent: TWinControl): boolean;
type
  TFunMakeFrame = function: ILivePlotFrame;
const
  RegKey = '\CLSID\{E08045D3-A708-4AF6-B6EB-4246D9E5CF46}\LocalServer32';
var
  Path: string;
  funMakeFrame: TFunMakeFrame;
begin
  Result := false;
  if Frm <> nil then Exit;

  //path to the package
  with TRegistry.Create do
    try
      RootKey := HKEY_CLASSES_ROOT;
      if not KeyExists(RegKey) then Exit;
      OpenKeyReadOnly(RegKey);
      Path := ReadString('');
    finally
      Free;
    end;
  Path := ExtractFilePath(Path) + 'LivePlot.bpl';

  //load package
  FrmPkg := LoadPackage(Path);
  if FrmPkg = 0 then Exit;

  funMakeFrame := GetProcAddress(FrmPkg, 'MakeFrame');
  if @funMakeFrame = nil
    then
      begin
      UnLoadPackage(FrmPkg);
      FrmPkg := 0;
      Exit;
      end;

  //create frame
  Frm := funMakeFrame;
  Frm.Frame.Parent := AParent;
  Frm.Frame.Align := alClient;

  Result := true;
end;


procedure UnloadFrame;
begin
  if Frm <> nil then
    begin
    Frm.Frame.Parent := nil;
    Frm.Frame.Free;
    Frm := nil;
    end;

  if FrmPkg <> 0 then
    UnLoadPackage(FrmPkg);
end;



end.

