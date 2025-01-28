unit AutoStrt;

interface

uses
  SysUtils, Windows, Registry;

function IsAutoStart: boolean;
procedure SetAutoStart(Value: boolean);

implementation

const
  AutoStartKey = '\Software\Microsoft\Windows\CurrentVersion\Run';


function IsAutoStart: boolean;
var
  FileName: string;
  KeyValue: string;
begin
  FileName := ExtractFileName(ParamStr(0));

  with TRegistry.Create do
    try
      OpenKey(AutoStartKey, false);
      KeyValue := ReadString(FileName);
    finally Free; end;

  Result := KeyValue <> '';
end;


procedure SetAutoStart(Value: boolean);
var
  FilePath: string;
  FileName: string;
begin
  FilePath := ParamStr(0);
  FileName := ExtractFileName(ParamStr(0));

  with TRegistry.Create do
    try
      OpenKey(AutoStartKey, true);
      if Value
        then WriteString(FileName, FilePath)
        else DeleteValue(FileName);
    finally Free; end;
end;


end.

