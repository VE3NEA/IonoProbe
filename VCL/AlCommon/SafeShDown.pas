//------------------------------------------------------------------------------
//This Source Code Form is subject to the terms of the Mozilla Public
//License, v. 2.0. If a copy of the MPL was not distributed with this
//file, You can obtain one at http://mozilla.org/MPL/2.0/.
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
//                               Omni-Rig
//
//               Copyright (c) 2003 Alex Shovkoplyas, VE3NEA
//
//                           ve3nea@dxatlas.com
//------------------------------------------------------------------------------

unit SafeShDown;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

{
ADD TO THE MAIN FORM:

procedure WMQueryEndSession(var Msg: TMessage); message WM_QUERYENDSESSION;

procedure TMainForm.WMQueryEndSession(var Msg: TMessage);
begin
  SafeShutdown1.ShuttingDown := true;
  if ComServer <> nil then ComServer.UIInteractive := false;
  inherited;
end;
}



type
  TSafeShutdown = class(TComponent)
  private
    FShuttingDown: boolean;
    FOnShutdown: TNotifyEvent;
    function ShutdownHook(var Msg: TMessage): boolean;
    procedure SetShuttingDown(const Value: boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ShuttingDown: boolean read FShuttingDown write SetShuttingDown;
  published
    property OnShutdown: TNotifyEvent read FOnShutdown write FOnShutdown;
  end;

procedure LogShDown(AFileName, AText: AnsiString);


procedure Register;





implementation


procedure Register;
begin
  RegisterComponents('Al', [TSafeShutdown]);
end;




constructor TSafeShutdown.Create(AOwner: TComponent);
begin
  inherited;
  if not (csDesigning in ComponentState) then
    Application.HookMainWindow(ShutdownHook);
end;



destructor TSafeShutdown.Destroy;
begin
  if not (csDesigning in ComponentState) then
    Application.UnhookMainWindow(ShutdownHook);

  inherited;
end;


procedure TSafeShutdown.SetShuttingDown(const Value: boolean);
begin
  FShuttingDown := Value;
end;


function TSafeShutdown.ShutdownHook(var Msg: TMessage): boolean;
begin
{
  if Msg.Msg = WM_QUERYENDSESSION then
    begin
    FShuttingDown := true;
    if Assigned(FOnShutdownQuery) then FOnShutdownQuery(Self);
    Msg.Result := UINT(true);
    Result := false; //allow further processing of the message
    Exit;
    end;
}

  Result := (Msg.Msg = WM_ENDSESSION) and WordBool(Msg.wParam);
  if Result then
    begin
    if Assigned(FOnShutdown) then FOnShutdown(Self);
    Halt;
    end;
end;




procedure LogShDown(AFileName, AText: AnsiString);
var
  Fs: TFileStream;
begin
  Exit;


  

  AText := FormatDateTime('yyyy-mm-dd hh:nn ', Now) + AText + #13;
  if FileExists(AFileName)
    then Fs := TFileStream.Create(AFileName, fmOpenReadWrite)
    else Fs := TFileStream.Create(AFileName, fmCreate);   
  try
    Fs.Seek(0, soFromEnd);
    Fs.WriteBuffer(AText[1], Length(AText));
  finally Fs.Free; end;
end;


end.
 
