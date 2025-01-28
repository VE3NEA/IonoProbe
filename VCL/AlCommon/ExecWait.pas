(*

Original function designed by
Brad Choate
Choate Consulting
choate@cswnet.com
http://www.cswnet.com/~choate

Modified by Alex Shovkoplyas alshovk@dxatlas.com

*)


unit ExecWait;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

function WinExecAndWait(Path: string; Visibility: word): dword;


implementation

function WinExecAndWait(Path: string; Visibility: word): dword;
var
  Msg: TMsg;
  pi: TProcessInformation;
  si: TStartupInfo;
  iExit: DWORD;
  bExit: boolean;
begin
  FillMemory( @si, sizeof( TStartupInfo ), 0 );

  with si do
    begin
    cb := sizeof( TStartupInfo );
    dwFlags := STARTF_USESHOWWINDOW;
    wShowWindow := Visibility;
    end;

  CreateProcess
    (
    nil,  // address of module name
    PChar(Path), // address of command line
    nil,  // address of process security attributes
    nil,  // address of thread security attributes
    false,  // new process inherits handles
    NORMAL_PRIORITY_CLASS,  // creation flags
    nil,  // address of new environment block
    nil,  // address of current directory name
    si, // address of STARTUPINFO
    pi  // address of PROCESS_INFORMATION
    );

    repeat
      while ( PeekMessage( Msg, 0, 0, 0, PM_REMOVE ) ) do
        begin
        if ( Msg.Message = WM_QUIT ) then
          Halt( Msg.wParam );
        TranslateMessage( Msg );
        DispatchMessage( Msg );
        end;

      GetExitCodeProcess( pi.hProcess, iExit );
      bExit := iExit <> STILL_ACTIVE;
    until bExit;

  CloseHandle( pi.hProcess );
  CloseHandle( pi.hThread );
  Result := iExit;
end;



end.       
