unit GrafFram;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls;

type
  TFrame1 = class(TFrame)
    Panel1: TPanel;
    Image1: TImage;
    Panel2: TPanel;
    Shape1: TShape;
    Label1: TLabel;
    PaintBox1: TPaintBox;
    procedure PaintBox1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.DFM}

procedure TFrame1.PaintBox1Click(Sender: TObject);
var
  R: TRect;
begin
  with PaintBox1 do R := RECT(0,0, Width, Height);

  with PaintBox1.Canvas do
    begin
    Brush.Color := clBlack;
    Pen.Color := clBlack;
    FillRect(R);
    Dec(R.Right, 5);
    Dec(R.Bottom, 5);
    Pen.Color := clGray;
    Pen.Style := psDot;
    FrameRect(R);
    end;
end;

end.

