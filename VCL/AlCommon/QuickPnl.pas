unit QuickPnl;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls;

type
  TQuickPanel = class(TPanel)
  private
    FHRedraw: boolean;
    FVRedraw: boolean;
    FEraseBG: boolean;

    procedure WmEraseBkGnd(var Message: TMessage); message WM_ERASEBKGND;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    { Public declarations }
  published
    property HRedraw: boolean read FHRedraw write FHRedraw;
    property VRedraw: boolean read FVRedraw write FVRedraw;
    property EraseBG: boolean read FEraseBG write FEraseBG;
    property DoubleBuffered;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Al', [TQuickPanel]);
end;

{ TDirtyPanel }

procedure TQuickPanel.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    begin
    if FHRedraw then WindowClass.style := WindowClass.style or CS_HREDRAW;
    if FVRedraw then WindowClass.style := WindowClass.style or CS_VREDRAW;
    end;
end;
                                     

procedure TQuickPanel.WmEraseBkGnd(var Message: TMessage);
begin
  if FEraseBG
    then inherited
    else Message.Result := 1;
end;



end.
