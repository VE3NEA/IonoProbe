unit Fram;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Tabs, ComCtrls, ExtCtrls, Buttons, Math, PermHint,
  IniFiles, SolArray, SolArrs, QuickPnl, Utc, Help, GeomFun, Grids, Menus,
  SsneArr;

const
  HighTab = 2; //Tab1,2

type
  TLivePlotFrame = class(TFrame)
    WeekPanel: TQuickPanel;
    Panel26: TQuickPanel;
    Image12: TImage;
    Panel28: TQuickPanel;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    Panel29: TQuickPanel;
    Image13: TImage;
    Panel31: TQuickPanel;
    Image14: TImage;
    Panel33: TQuickPanel;
    Image15: TImage;
    Shape7: TShape;
    TabSet1: TTabSet;
    YearPanel: TQuickPanel;
    Panel3: TQuickPanel;
    Image1: TImage;
    Panel5: TQuickPanel;
    Image2: TImage;
    Panel7: TQuickPanel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    Panel8: TQuickPanel;
    Image3: TImage;
    Panel10: TQuickPanel;
    Image4: TImage;
    Panel12: TQuickPanel;
    Image5: TImage;
    WSliderPaintBox: TPaintBox;
    QSliderPaintBox: TPaintBox;
    Timer1: TTimer;
    PaintBox4: TPaintBox;
    Label27: TLabel;
    Label28: TLabel;
    PaintBox3: TPaintBox;
    Label25: TLabel;
    Label26: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    PaintBox2: TPaintBox;
    Label29: TLabel;
    Label30: TLabel;
    PaintBox1: TPaintBox;
    Label1: TLabel;
    Label2: TLabel;
    PaintBox5: TPaintBox;
    Label9: TLabel;
    Label10: TLabel;
    PaintBox6: TPaintBox;
    Label3: TLabel;
    Label4: TLabel;
    PaintBox7: TPaintBox;
    Label5: TLabel;
    Label6: TLabel;
    PaintBox8: TPaintBox;
    Label7: TLabel;
    Label8: TLabel;
    PaintBox9: TPaintBox;
    DayPanel: TQuickPanel;
    SummaryPanel: TQuickPanel;
    AlertsPanel: TQuickPanel;
    Shape6: TShape;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Last24Label1: TLabel;
    Next24Label1: TLabel;
    Last24Label2: TLabel;
    Next24Label2: TLabel;
    Label18: TLabel;
    Panel43: TQuickPanel;
    PaintBox0: TPaintBox;
    Label19: TLabel;
    Label20: TLabel;
    Image6: TImage;
    ReportPanel: TQuickPanel;
    Panel2: TPanel;
    ComboBox1: TComboBox;
    VersionPanel: TQuickPanel;
    RichEdit1: TRichEdit;
    QuickPanel2: TQuickPanel;
    Label39: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    Label44: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    QuickPanel3: TQuickPanel;
    Label21: TLabel;
    Label22: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Shape1: TShape;
    PopupMenu1: TPopupMenu;
    SelectAll1: TMenuItem;
    Copy1: TMenuItem;
    Scale100Image: TImage;
    Scale400Image: TImage;
    procedure TabSet1Change(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure DragMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DragMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure DragMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure SliderPaintBoxPaint(Sender: TObject);
    procedure DecBtnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure IncBtnMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure Timer1Timer(Sender: TObject);
    procedure IncDecBtnMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SliderPaintBoxMouseMove(Sender: TObject; Shift: TShiftState;
      X, Y: Integer);
    procedure FrameResize(Sender: TObject);
    procedure PlotCurve(Sender: TObject);
    procedure PlotKp(Sender: TObject);
    procedure PlotBars(Sender: TObject);
    procedure AlertsPanelResize(Sender: TObject);
    procedure WhatsThisClick(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure SelectAll1Click(Sender: TObject);
  private
    ShiftX: array[0..HighTab] of integer;
    T0: array[0..HighTab] of TDateTime;
    FHintWin: TPermanentHintWindow;
    FDraggedControl: TControl;
    FCursorBeforeDrag: TCursor;
    Panels: array[0..12] of TQuickPanel;
    FSlideStart: TDateTime;
    FOldTick: TDateTime;
    FSlideOrg: integer;
    FArrays: TSolarArrays;
    GraphR: TRect;

    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure PaintBG(Box: TPaintBox);
    procedure PaintGrid(Box: TPaintBox);
    function TtoX(T: TDateTime; TabNo: integer): integer;
    function XtoT(X: integer; TabNo: integer): TDateTime;
    procedure SetT0;
    procedure ListPanels;
    procedure ValidateShift;
    procedure RepaintGraphs;
    procedure DoPlotBars(Box: TPaintBox; Arr: TSolarArray; dX: integer);
    procedure PlotKp1(Box: TPaintBox);
    procedure AlignAlerts;
    procedure ShowAlerts;
    procedure ShovValues;

    function ValueAsText(Tg, X: integer): string;
    procedure PlotThreshold(Box: TPaintBox);
    function CurrentValueString(Tg: integer): string;
    procedure AlignSummary;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetNearestHeight(H: integer): integer;
    procedure LoadPanelOrder(const IniFile: TFileName);
    procedure SavePanelOrder(const IniFile: TFileName);
    function GetTabIndex: integer;
    procedure SetTabIndex(Value: integer);
    procedure SetArrays(Arrays: TSolarArrays);
    procedure DataChanged;
    function Average90d(Arr: TSolarArray; AUtc: TDateTime): integer;

    property TabIndex: integer read GetTabIndex write SetTabIndex;
  end;



  TGraphParams = record
    TabNo: integer;
    Dk: TDataKind;
    DarkColor: TColor;
    LightColor: TColor;
    LineCnt: integer;
    end;


const
  StepT: array[0..HighTab] of TDateTime = (1, 1/24/4, 1/4);           //6h, 15 min
  LeftLen: array[0..HighTab] of integer = (1, 7*24*4 + 1, 366*4 + 1); //92+1d, 7d
  RightLen: array[0..HighTab] of integer = (1, 3*24*4, 45*4);         //45d, 3d

  GraphParams: array[0..9] of TGraphParams =(
    (TabNo: 1; Dk: dkSsne; DarkColor: clOLive; LightColor: clYellow; LineCnt: 3),
    (TabNo: 1; Dk: dkAurora; DarkColor: clPurple; LightColor: clFuchsia; LineCnt: 4),
    (TabNo: 1; Dk: dkKp3; DarkColor: clTeal; LightColor: clAqua; LineCnt: 2),
    (TabNo: 1; Dk: dkProtons; DarkColor: clNavy; LightColor: clBlue; LineCnt: 2),
    (TabNo: 1; Dk: dkXrays; DarkColor: clWhite; LightColor: clWhite; LineCnt: 4),
    (TabNo: 2; Dk: dkSsn; DarkColor: clYellow; LightColor: clYellow; LineCnt: 3),
    (TabNo: 2; Dk: dkSfi; DarkColor: clGreen; LightColor: clLime; LineCnt: 2),
    (TabNo: 2; Dk: dkAp; DarkColor: clTeal; LightColor: clAqua; LineCnt: 2),
    (TabNo: 2; Dk: dkProtonBg; DarkColor: clNavy; LightColor: clBlue; LineCnt: 2),
    (TabNo: 2; Dk: dkXRayBg; DarkColor: clWhite; LightColor: clWhite; LineCnt: 4));



implementation

{$R *.DFM}


function UtcNow: TDateTime;
var
  St: TSystemTime;
begin
  GetSystemTime(St);
  Result := SystemTimeToDateTime(St);
end;





{ TLivePlotFrame }

//------------------------------------------------------------------------------
//                                Init
//------------------------------------------------------------------------------

constructor TLivePlotFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FSlideStart := MAXINT;
  FHintWin := TPermanentHintWindow.Create(Self);

  ShiftX[0]  := 0; //D
  ShiftX[1]  := 270; //W
  ShiftX[2] := 165; //Y

  SetT0;
  GraphR.Top := 16;
  GraphR.Bottom := PaintBox6.Height - 7;

  DayPanel.Align := alClient;
  WeekPanel.Align := alClient;
  YearPanel.Align := alClient;

  DoubleBuffered := true;
  TabSet1.DoubleBuffered := true;


  with WSliderPaintBox do ControlStyle := ControlStyle + [csCaptureMouse];
  with QSliderPaintBox do ControlStyle := ControlStyle + [csCaptureMouse];

  ListPanels;
end;


procedure TLivePlotFrame.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params.WindowClass do style := style or CS_HREDRAW;
end;





//------------------------------------------------------------------------------
//                                Helper funs
//------------------------------------------------------------------------------

procedure TLivePlotFrame.SetT0;
begin
  T0[0] := XtoT(TtoX(UtcNow, 0), 0);
  T0[1] := XtoT(TtoX(UtcNow, 1), 1);
  T0[2] := XtoT(TtoX(UtcNow, 2), 2);
end;


function TLivePlotFrame.TtoX(T: TDateTime; TabNo: integer): integer;
begin
  Result := GraphR.Right + ShiftX[TabNo] - RightLen[TabNo]
    + Floor((T-T0[TabNo] - SEC1)/StepT[TabNo]);
end;


function TLivePlotFrame.XtoT(X: integer; TabNo: integer): TDateTime;
begin
  Result := T0[TabNo] + (X - GraphR.Right - ShiftX[TabNo]
    + RightLen[TabNo]) * StepT[TabNo];
end;


procedure TLivePlotFrame.ValidateShift;
var
  i: integer;
begin
  for i:=0 to HighTab do
    begin
    if ShiftX[i] > (LeftLen[i] + RightLen[i] - GraphR.Right)
      then ShiftX[i] := LeftLen[i] + RightLen[i] - GraphR.Right;
    if ShiftX[i] <= 0 then ShiftX[i] := 0;
    end;

  RepaintGraphs;
end;


procedure TLivePlotFrame.ListPanels;
begin
  Panels[0] := AlertsPanel;
  Panels[1] := SummaryPanel;
  Panels[2] := ReportPanel;
  Panels[3] := Panel43;
  Panels[4] := Panel26;
  Panels[5] := Panel33;
  Panels[6] := Panel29;
  Panels[7] := Panel31;
  Panels[8] := Panel3;
  Panels[9] := Panel12;
  Panels[10] := Panel5;
  Panels[11] := Panel8;
  Panels[12] := Panel10;
end;






//------------------------------------------------------------------------------
//                             ILivePlotFrame
//------------------------------------------------------------------------------

function TLivePlotFrame.GetNearestHeight(H: integer): integer;
var
  MinHeight: integer;
begin
  case TabSet1.TabIndex of
    0: //day
      begin
      MinHeight := Height - DayPanel.Height + VersionPanel.Height;
      Result := (H - MinHeight + AlertsPanel.Height div 2) div AlertsPanel.Height;
      Result := Max(1, Min(5, Result));
      Result := Result * AlertsPanel.Height + MinHeight;
      end;
    1: //week
      begin
      MinHeight := Height - WeekPanel.Height + Panel28.Height;
      Result := (H - MinHeight + Panel43.Height div 2) div Panel43.Height;
      Result := Max(1, Min(5, Result));
      Result := Result * Panel43.Height + MinHeight;
      end;
    else //year
      begin
      MinHeight := Height - YearPanel.Height + Panel7.Height;
      Result := (H - MinHeight + Panel3.Height div 2) div Panel3.Height;
      Result := Max(1, Min(5, Result));
      Result := Result * Panel3.Height + MinHeight;
      end;
    end;
end;



const SEC_ORD = 'Panel order';

procedure TLivePlotFrame.LoadPanelOrder(const IniFile: TFileName);
var
  p, i: integer;
  L: TStringList;
  H: integer;
begin
  H := AlertsPanel.Height;

  L := TStringList.Create;
  try
    //read
    with TIniFile.Create(IniFile) do
      try ReadSectionValues(SEC_ORD, L); finally Free; end;
    //apply
    for p:=0 to 4 do //position
      for i:=0 to L.Count-1 do //panel at this position
        if StrToIntDef(L.Values[L.Names[i]], -1) = p
          then Panels[i].Top := H * p - 10;
  finally
    L.Free;
  end;
end;


procedure TLivePlotFrame.SavePanelOrder(const IniFile: TFileName);
var
  i: integer;
  H: integer;
begin
  H := AlertsPanel.Height;

  with TIniFile.Create(IniFile) do
    try
      for i:=0 to High(Panels) do
        with Panels[i] do
          WriteInteger(SEC_ORD, Format('Panel%.2d', [i]), (Top+10) div H);
    finally
      Free;
    end;
end;


function TLivePlotFrame.GetTabIndex: integer;
begin
  Result := TabSet1.TabIndex;
end;


procedure TLivePlotFrame.SetTabIndex(Value: integer);
begin
  TabSet1.TabIndex := Value;

  DayPanel.Visible := Value = 0;
  WeekPanel.Visible := Value = 1;
  YearPanel.Visible := Value = 2;
end;


procedure TLivePlotFrame.SetArrays(Arrays: TSolarArrays);
begin
  FArrays := Arrays;
end;


procedure TLivePlotFrame.DataChanged;
begin
  RepaintGraphs;

  //SSNe scale
  with FArrays[dkSsne] as TSsneArray do
    if GetMax > 100
    then
      begin
      FScale := 400;
      Image6.Picture.Bitmap := Scale400Image.Picture.Bitmap;
      end
    else
      begin
      FScale := 100;
      Image6.Picture.Bitmap := Scale100Image.Picture.Bitmap;
      end;
end;





//------------------------------------------------------------------------------
//                               Events
//------------------------------------------------------------------------------

procedure TLivePlotFrame.TabSet1Change(Sender: TObject; NewTab: Integer;
  var AllowChange: Boolean);
begin
  DayPanel.Visible := NewTab = 0;
  WeekPanel.Visible := NewTab = 1;
  YearPanel.Visible := NewTab = 2;
end;


procedure TLivePlotFrame.FrameResize(Sender: TObject);
begin
  GraphR.Right := Width - Image15.Width - 10 - 7;
  ValidateShift;
end;


function TLivePlotFrame.ValueAsText(Tg, X: integer): string;
var
  Arr: TSolarArray;
  Idx: integer;
begin
  Arr := FArrays[GraphParams[Tg].Dk];
  Idx := Arr.TimeToIdx( XtoT(X, GraphParams[Tg].TabNo) );

  if (Idx < 0) or (Idx > High(Arr.Kind)) or (Arr.Kind[Idx] = vkNone)
    //no data
    then Result := '<no data>'
  else
    begin
    //text
    case GraphParams[Tg].Dk of
      dkSsne:
        if GetKeyState(VK_CONTROL) < 0
          then
            with Arr as TSsneArray do
              Result := Format('SSNe = %d'#13'foF2 error = %.1f%%'#13'%d observations',
                [Round(Data[Idx]), FErr[Idx], FCnt[Idx]])
          else Result := IntToStr(Round((Arr.Data[Idx])));

      dkSsn, dkSfi, dkAp, dkKp3, dkAurora:
        Result := IntToStr(Round((Arr.Data[Idx])));

      dkXRayBg, dkXrays:
        Result := EncodeXRAY(Arr.Data[Idx], true);

      dkProtonBg,dkProtons:
        Result := FormatExp(Arr.Data[Idx]) + '/cm2/s/sr';
      end;

    //estimated
    if Arr.Kind[Idx] = vkEstimated then Result := Result + ' (estimated)';
    //Kp3 -> Kp1
    if (Arr.Kind[Idx] = vkEstimated) and (GraphParams[Tg].Dk = dkKp3) then
      begin
      Arr := FArrays[dkKp1];
      Idx := Arr.TimeToIdx( XtoT(X, GraphParams[Tg].TabNo) );
      if (Idx >= 0) and (Idx <= High(Arr.Kind)) and (Arr.Kind[Idx] = vkMeasured)
        then Result := SafeDecimalFormat('%1.2f', [Arr.Data[Idx]]);
      end;
    end;
end;


procedure TLivePlotFrame.PaintBoxMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  Px, Py: TPoint;
  TabNo: integer;
  Txt: string;
  Box: TPaintBox;
begin
  Box := Sender as TPaintBox;
  if not PtInRect(GraphR, POINT(X,Y)) then
    begin
    FHintWin.HideHint;
    Box.Cursor := crDefault;
    Exit;
    end;

  Box.Cursor := crCross;

  //pos
  GetCursorPos(Px);
  Py := Box.ClientToScreen(POINT(0, Box.Height));
  //text
  TabNo := GraphParams[Box.Tag].TabNo;
  Txt := ValueAsText(Box.Tag, X) + #13#10;
  Txt := Txt + FormatDateTime('yyyy-mm-dd hh:nn', XtoT(X, TabNo)) + ' UTC';
  //show
  FHintWin.ShowHintAt(Txt, Px.x, Py.y);
end;


procedure TLivePlotFrame.CMMouseLeave(var Message: TMessage);
begin
  FHintWin.HideHint;
end;


type THackControl = class (TControl) end;
procedure TLivePlotFrame.Timer1Timer(Sender: TObject);
var
  T: TDateTime;
begin
  T := Now;

  //advance arrays
  if Trunc(T * 24*4) > Trunc(FOldTick * 24*4) then
    begin
    SetT0;
    FArrays.Update;
    RepaintGraphs;
    end;

  //advance slider
  if T > FSlideStart then
    begin
    if THackControl(SpeedButton3).MouseCapture
      or THackControl(SpeedButton1).MouseCapture
        then Inc(ShiftX[TabSet1.TabIndex], 8)
    else if THackControl(SpeedButton4).MouseCapture
      or THackControl(SpeedButton2).MouseCapture
        then Inc(ShiftX[TabSet1.TabIndex], -8);
    ValidateShift;
    end;

  FOldTick := T;
end;




//------------------------------------------------------------------------------
//                            Panel dragging
//------------------------------------------------------------------------------
//start dragging panel
procedure TLivePlotFrame.DragMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  NewCursor: TCursor;
begin
  FHintWin.HideHint;
  if Button <> mbLeft then Exit;

  SetCapture(Handle);

  //remember what we are dragging
  FDraggedControl := Sender as TControl;

  //SummaryPanel has panel-in-panel layout, go one step up
  if FDraggedControl.Parent = QuickPanel2 then FDraggedControl := QuickPanel2
  else if FDraggedControl.Parent = QuickPanel3 then FDraggedControl := QuickPanel3;


  if (FDraggedControl.Parent = AlertsPanel) or
     (FDraggedControl.Parent = SummaryPanel)
    then
      begin
      NewCursor := crSizeNS;
      //disable hor. sliding
      FSlideOrg := MAXINT;
      end
    else
      begin
      NewCursor := crSizeAll;
      //store the origin of hor. sliding
      FSlideOrg := ShiftX[TabSet1.TabIndex]
        - ScreenToClient(FDraggedControl.ClientToScreen(POINT(X,Y))).x;
      end;

  //if dragging the slider, don't allow reordering panels
  if FDraggedControl.Tag = 99 then begin FDraggedControl := nil; Exit; end;

  //change cursor
  FCursorBeforeDrag := FDraggedControl.Cursor;
  FDraggedControl.Cursor := NewCursor;
  Screen.Cursor := NewCursor;
  Screen.Cursor := crDefault;
end;


//end dragging panel
procedure TLivePlotFrame.DragMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FDraggedControl = nil then Exit;

  ReleaseCapture;
  FDraggedControl.Cursor := FCursorBeforeDrag;
  FDraggedControl := nil;
end;


//panel dragging in progress
procedure TLivePlotFrame.DragMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  NewTop: integer;
begin
  if not (ssLeft in Shift) then Exit;

  //slide horizontally

  if FSlideOrg <> MAXINT then
    begin
    ShiftX[TabSet1.TabIndex] := FSlideOrg + X;
    ValidateShift;
    end;

  //reorder vertically
  if FDraggedControl <> nil then
    with (FDraggedControl.Parent as TPanel) do
      begin
      NewTop := Min(4, Max(0, Y div Height)) * Height;
      if NewTop < Top then Top := Top - Height - 1
      else if NewTop > Top then Top := Top + Height + 1;
      end;
end;






//------------------------------------------------------------------------------
//                                Slider
//------------------------------------------------------------------------------


//slider repainting

procedure TLivePlotFrame.SliderPaintBoxPaint(Sender: TObject);
const
  MonthNames: array[1..12] of string =
    ('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec');
var
  R: TRect;
  Box: TPaintBox;
  X: integer;
  TabNo: integer;
  T: TDateTime;
  y,m,d: WORD;
  S: string;

begin
  Box := Sender as TPaintBox;
  TabNo := TabSet1.TabIndex;

  with Box.Canvas do
    begin
    //BG
    Brush.Color := clInfoBk;
    Pen.Color := clInfoBk;
    R := RECT(0,0, Box.Width, Box.Height);
    FillRect(R);

    Pen.Color := clGray;
    //Pen.Color := clBlack;

    case TabNo of
      1:
        begin
        T := Trunc(XtoT(GraphR.Right, TabNo)) + 1/24/60;
        repeat
          X := TtoX(T, TabNo) + Image15.Width;
          MoveTo(X, 1); LineTo(X, Box.Height);
          DecodeDate(T, y,m,d);
          S := Format('%s %d', [MonthNames[m], d]);
          TextOut(X + 48 - (TextWidth(S) div 2), 0, S);
          T := T - 1;
        until X < 0;
        end;
      2:
        begin
        DecodeDate(XtoT(GraphR.Right, TabNo), y,m,d);
        T := EncodeDate(y, m, 1) + 1/24/60;
        repeat
          X := TtoX(T, TabNo) + Image15.Width;
          MoveTo(X, 1); LineTo(X, Box.Height);
          DecodeDate(T, y,m,d);
          S := Format('%s %d', [MonthNames[m], y]);
          TextOut(X + 60 - (TextWidth(S) div 2), 0, S);
          T := IncMonth(T, -1);
        until X < 0;
        end;
      end;
    end;
end;



//slider buttons

procedure TLivePlotFrame.DecBtnMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Inc(ShiftX[TabSet1.TabIndex], 8);
  ValidateShift;
  FSlideStart := Now + 0.5/86400;
end;


procedure TLivePlotFrame.IncBtnMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  Inc(ShiftX[TabSet1.TabIndex], -8);
  ValidateShift;
  FSlideStart := Now + 0.5/86400;
end;


procedure TLivePlotFrame.IncDecBtnMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FSlideStart := MAXINT;
end;


//slider dragging

procedure TLivePlotFrame.SliderPaintBoxMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
end;








//------------------------------------------------------------------------------
//                               Paint
//------------------------------------------------------------------------------

procedure TLivePlotFrame.RepaintGraphs;
begin
  case TabSet1.TabIndex of
    1:
      begin
      WSliderPaintBox.Repaint;
      PaintBox0.Repaint;
      PaintBox1.Repaint;
      PaintBox2.Repaint;
      PaintBox3.Repaint;
      PaintBox4.Repaint;
      end;
    2:
      begin
      QSliderPaintBox.Repaint;
      PaintBox5.Repaint;
      PaintBox6.Repaint;
      PaintBox7.Repaint;
      PaintBox8.Repaint;
      PaintBox9.Repaint;
      end;
    end;

  ShowAlerts;
  ShovValues;
end;


procedure TLivePlotFrame.PaintBG(Box: TPaintBox);
var
  R: TRect;
begin
  with Box.Canvas do
    begin
    //black BG
    Brush.Color := clBlack;
    Pen.Color := clBlack;
    R := RECT(0,0, Width, Height);
    FillRect(R);
    //gray frame
    Pen.Color := clGray;
    Pen.Style := psDot;
    MoveTo(GraphR.Right, GraphR.Top); LineTo(0, GraphR.Top);
    MoveTo(GraphR.Right, GraphR.Bottom); LineTo(0, GraphR.Bottom);
    MoveTo(0, GraphR.Bottom); LineTo(0, GraphR.Top);
    MoveTo(GraphR.Right, GraphR.Top); LineTo(GraphR.Right, GraphR.Bottom);
    end;
end;


procedure TLivePlotFrame.PaintGrid(Box: TPaintBox);
var
  X,Y, i: integer;
  TabNo: integer;
  T: TDateTime;
  yy,mm,dd: WORD;
begin
  Box.Canvas.Pen.Style := psDot;
  Box.Canvas.Pen.Color := clGray;

  //vert. lines
  TabNo := GraphParams[Box.Tag].TabNo;
  case TabNo of
    1:
      begin
      T := Trunc(XtoT(GraphR.Right, TabNo)) + 1/24/60;
      repeat
        X := TtoX(T, TabNo);
        Box.Canvas.MoveTo(X, GraphR.Top+2);
        Box.Canvas.LineTo(X, GraphR.Bottom);
        T := T - 1;
      until X < 0;
      end;
    2:
      begin
      DecodeDate(XtoT(GraphR.Right, TabNo), yy,mm,dd);
      T := EncodeDate(yy, mm, 1) + 1/24/60;
      repeat
        X := TtoX(T, TabNo);
        Box.Canvas.MoveTo(X, GraphR.Top+2);
        Box.Canvas.LineTo(X, GraphR.Bottom);
        T := IncMonth(T, -1);
      until X < 0;
      end;
    end;

  //hor. lines
  for i:=1 to GraphParams[Box.Tag].LineCnt do
    with GraphR do
      begin
      Y := Bottom - i * (Bottom-Top) div (GraphParams[Box.Tag].LineCnt+1);
      Box.Canvas.MoveTo(Right, Y);
      Box.Canvas.LineTo(Left, Y);
      end;

  //Now line
  Box.Canvas.Pen.Style := psSolid;
  Box.Canvas.Pen.Color := clRed;
  X := TtoX(UtcNow, TabNo);
  Box.Canvas.MoveTo(X, GraphR.Top+2);
  Box.Canvas.LineTo(X, GraphR.Bottom);
end;


procedure TLivePlotFrame.PlotThreshold(Box: TPaintBox);
var
  Y: integer;
begin
  with GraphParams[Box.Tag] do
    if FArrays[Dk].StormThreshold <> -1 then
      begin
{
      Box.Canvas.Pen.Style := psDot;//psSolid;
      Box.Canvas.Pen.Color := clRed;//clMaroon;
      Box.Canvas.Brush.Color := clBlack;
}
      Box.Canvas.Pen.Style := psSolid;
      Box.Canvas.Pen.Color := clMaroon;

      Y := GraphR.Top + Round ((GraphR.Bottom-GraphR.Top)
           * (1 - FArrays[Dk].ScaleValue(FArrays[Dk].StormThreshold)));
      Box.Canvas.MoveTo(GraphR.Right, Y);
      Box.Canvas.LineTo(GraphR.Left, Y);
      end;
end;



procedure TLivePlotFrame.DoPlotBars(Box: TPaintBox; Arr: TSolarArray; dX: integer);
var
  TabNo: integer;
  x, i: integer;
  T: TDateTime;
  H: integer;
  R: TRect;
begin
  TabNo := GraphParams[Box.Tag].TabNo;

  R.Bottom := GraphR.Bottom + 1;
  H := GraphR.Bottom-GraphR.Top;
  Box.Canvas.Pen.Style := psSolid;

  T := XtoT(GraphR.Right, TabNo);
  T := Trunc(T / (StepT[TabNo]*dX)) * StepT[TabNo] * dX;
  x := TtoX(T, TabNo) + 1{Kludge};
  i := Arr.TimeToIdx(T);

  with Box.Canvas do
    while x > (GraphR.Left - dX) do
      try
        if (i < 0) then Break;
        if i > High(Arr.Data) then Continue;

        case Arr.Kind[i] of
          vkNone: Continue;
          vkEstimated: Pen.Color := GraphParams[Box.Tag].DarkColor;
          vkMeasured:  Pen.Color := GraphParams[Box.Tag].LightColor;
          end;
        Brush.Color := Pen.Color;

        R.Right := Min(x + dX, GraphR.Right);
        R.Left := Min(x+1, GraphR.Right);

        R.Top := GraphR.Top + Round(H * (1 - Arr.ScaledData(i)));
        R.Top := Min(GraphR.Bottom, Max(GraphR.Top, R.Top));

        FillRect(R);
      finally Dec(x, dX); Dec(i); end;
end;


procedure TLivePlotFrame.PlotCurve(Sender: TObject);
var
  Box: TPaintBox;
  Arr: TSolarArray;
  TabNo: integer;
  x, y, i: integer;
  H: integer;
  LastAvailable: integer;
begin
  Box := Sender as TPaintBox;
  Arr := FArrays[GraphParams[Box.Tag].Dk];
  TabNo := GraphParams[Box.Tag].TabNo;

  PaintBG(Box);
  PaintGrid(Box);

  H := GraphR.Bottom-GraphR.Top;

  Box.Canvas.Pen.Style := psSolid;
  Box.Canvas.Pen.Color := GraphParams[Box.Tag].LightColor;

  x :=GraphR.Right;
  i := Arr.TimeToIdx(XtoT(x, TabNo));
  LastAvailable := MAXINT;


  with Box.Canvas do
    while x >= GraphR.Left do
      try
        if (i < 0) then Break
        else if (i > High(Arr.Data)) or (Arr.Kind[i] = vkNone) then Continue;

        y := GraphR.Bottom - Round(H * Min(1, Arr.ScaledData(i)));
        y := Min(GraphR.Bottom, Max(GraphR.Top, y));

        if LastAvailable > (x + 4) then MoveTo(x, y) else LineTo(x, y);
        LastAvailable := x;
      finally Dec(x); Dec(i); end;

  PlotThreshold(Box);
end;


procedure TLivePlotFrame.PlotKp1(Box: TPaintBox);
const
  dX = 4;
var
  TabNo: integer;
  x, i: integer;
  T: TDateTime;
  H: integer;
  R: TRect;
begin
  TabNo := GraphParams[Box.Tag].TabNo;

  R.Bottom := GraphR.Bottom + 1;
  H := GraphR.Bottom-GraphR.Top;
  Box.Canvas.Pen.Style := psSolid;

  T := XtoT(GraphR.Right, TabNo);
  T := Trunc(T / (StepT[TabNo]*dX)) * StepT[TabNo] * dX;
  x := TtoX(T, TabNo) + 1{Kludge};
  i := FArrays[dkKp1].TimeToIdx(T);

  with Box.Canvas do
    while x > (GraphR.Left - dX) do
      try
        if (i < 0) then Break;
        if i > High(FArrays[dkKp1].Data) then Continue;

        T := XtoT(x, TabNo);
        with FArrays[dkKp3] do
          if Kind[TimeToIdx(T)] = vkMeasured then Continue;

        if FArrays[dkKp1].Kind[i] = vkNone then Continue;

        Pen.Color := GraphParams[Box.Tag].LightColor;
        Brush.Color := Pen.Color;

        R.Right := Min(x + dX, GraphR.Right);
        R.Left := Min(x+1, GraphR.Right);

        R.Top := GraphR.Top + Round(H * (1 - FArrays[dkKp1].ScaledData(i)));
        R.Top := Min(GraphR.Bottom, Max(GraphR.Top, R.Top));

        FillRect(R);
      finally Dec(x, dX); Dec(i); end;
end;


procedure TLivePlotFrame.PlotKp(Sender: TObject);
var
  Box: TPaintBox;
begin
  Box := Sender as TPaintBox;
  PaintBG(Box);
  PaintGrid(Box);
  DoPlotBars(Box, FArrays[dkKp3], 12);
  PlotKp1(Box);
  PlotThreshold(Box);
end;

procedure TLivePlotFrame.PlotBars(Sender: TObject);
var
  Box: TPaintBox;
begin
  Box := Sender as TPaintBox;

  PaintBG(Box);
  PaintGrid(Box);
  DoPlotBars(Box, FArrays[GraphParams[Box.Tag].Dk], 4);
  PlotThreshold(Box);
end;


function TLivePlotFrame.CurrentValueString(Tg: integer): string;
var
  Arr: TSolarArray;
  Idx, i: integer;
  Depth: integer;
begin
  Result := '';
  Arr := FArrays[GraphParams[Tg].Dk];
  Idx := Arr.TimeToIdx(UtcNow);

  if GraphParams[Tg].Dk = dkAurora then Depth := 10 else Depth := 4;

  for i:= Min(Idx, High(Arr.Data)) downto Max(Idx-Depth, 0) do
    if Arr.Kind[i] = vkMeasured then
      begin
      case GraphParams[Tg].Dk of
        dkSsn, dkSfi, dkAp, dkKp3, dkAurora, dkSsne:
          Result := IntToStr(Round((Arr.Data[i])));

        dkXRayBg, dkXrays:
          Result := EncodeXRAY(Arr.Data[i], false);

        dkProtonBg,dkProtons:
          Result := FormatExp(Arr.Data[i]);
        end;
      Exit;
      end;
      
        
{    //Kp3 -> Kp1
    if (Arr.Kind[Idx] = vkEstimated) and (GraphParams[Tg].Dk = dkKp3) then
      begin
      Arr := FArrays[dkKp1];
      Idx := Arr.TimeToIdx( XtoT(X, GraphParams[Tg].TabNo) );
      if (Idx >= 0) and (Idx <= High(Arr.Kind)) and (Arr.Kind[Idx] = vkMeasured)
        then Result := SafeDecimalFormat('%1.2f', [Arr.Data[Idx]]);
      end;
}
end;


function TLivePlotFrame.Average90d(Arr: TSolarArray; AUtc: TDateTime): integer;
var
  Idx, i: integer;
  Sum: Single;
  Cnt: integer;
begin
  Sum := 0; Cnt := 0;
  Idx := Arr.TimeToIdx(AUtc);
  for i:=Max(0, Idx-89) to Min(High(Arr.Kind), Idx) do
    if Arr.Kind[i] = vkMeasured then
      begin
      Sum := Sum + Arr.Data[i];
      Inc(Cnt);
      end;

  if Cnt > 0 //at least some data required
    then Result := Round(Sum / Cnt)
    else Result := -999;
end;


procedure TLivePlotFrame.ShovValues;
var
  Avg: integer;
begin
  Label20.Caption := CurrentValueString(0);
  Label24.Caption := CurrentValueString(1);
  Label30.Caption := CurrentValueString(2);
  Label26.Caption := CurrentValueString(3);
  Label28.Caption := CurrentValueString(4);
  Label2.Caption := CurrentValueString(5);
  Label10.Caption := CurrentValueString(6);
  Label4.Caption := CurrentValueString(7);
  Label6.Caption := CurrentValueString(8);
  Label8.Caption := CurrentValueString(9);


  Label36.Caption := Label20.Caption;
  Label46.Caption := Label24.Caption;
  Label45.Caption := Label30.Caption;
  Label47.Caption := Label26.Caption;
  Label48.Caption := Label28.Caption;
  Label34.Caption := Label2.Caption;
  Label37.Caption := Label10.Caption;
  Label44.Caption := Label4.Caption;

  Avg := Average90d(FArrays[dkSsn], UtcNow);
  if Avg >= 0 then Label35.Caption := IntToStr(Avg) else Label35.Caption := '';

  Avg := Average90d(FArrays[dkSfi], UtcNow);
  if Avg >= 0 then Label38.Caption := IntToStr(Avg) else Label38.Caption := '';
end;


//------------------------------------------------------------------------------
//                                 Alerts
//------------------------------------------------------------------------------
procedure TLivePlotFrame.ShowAlerts;
const
  LedChars: array[TAlertKind] of Char = ('G','S','R','G','S','R');
  LColors: array[-1..5] of TColor = (clGray, clGreen, clGreen, clOlive, clOlive, clMaroon, clMaroon);
  NColors: array[-1..5] of TColor = (clGray, clLime, clLime, clYellow, clYellow, clRed, clRed);
  Summary: array[0..5] of string = ('No', 'Minor', 'Moderate', 'Strong', 'Severe', 'Extreme');
  LedHints: array[TAlertKind] of string =
   (' geomagnetic storms occurred',
    ' solar radiation storms occurred',
    ' radio blackouts occurred',
   ' geomagnetic storms expected',
    ' solar radiation storms expected',
    ' radio blackouts expected');


var
  Leds: array[TAlertKind] of TLabel;
  a: TAlertKind;
  V: integer;
  Mx: integer;
begin
  Leds[alPastG] := Label13;
  Leds[alPastS] := Label11;
  Leds[alPastR] := Label12;
  Leds[alNextG] := Label17;
  Leds[alNextS] := Label15;
  Leds[alNextR] := Label16;

  if FArrays.Alerts.Issued < (UtcNow - 1)
    //disabled LED's
    then
      begin
      Label18.Caption := ''; //issue time
      for a:=Low(TAlertKind) to High(TAlertKind) do
        begin
        Leds[a].Color := clGray;
        Leds[a].Font.Color := clBlack;
        Leds[a].Caption := LedChars[a] + ' --';
        Leds[a].Hint := 'No data';
        end;
      Last24Label2.Caption := '<no data>';
      Next24Label2.Caption := '<no data>';
      end

    //LED data
    else
      begin
      //issued time
      V := Max(0, Round(24*(UtcNow -FArrays.Alerts.Issued)));
      if V in[1, 21] 
        then Label18.Caption := 'Issued ' + IntToStr(V) + ' hour ago'
        else Label18.Caption := 'Issued ' + IntToStr(V) + ' hours ago';

      for a:=Low(TAlertKind) to High(TAlertKind) do
        begin
        V := Max(-1, Min(5, FArrays.Alerts[a]));
        //colors
        if a < alNextG
          then Leds[a].Color := LColors[V] else Leds[a].Color := NColors[V];
        if a < alNextG
          then Leds[a].Font.Color := clWhite else Leds[a].Font.Color := clBlack;
        //caption and hint
        if FArrays.Alerts[a] < 0
          then
            begin
            Leds[a].Caption := LedChars[a] + ' --';
            Leds[a].Hint := 'No data';
            end
          else
            begin
            Leds[a].Caption := LedChars[a] + IntToStr(V);
            Leds[a].Hint := Summary[V] + LedHints[a];
            end;
        end;

      //summary
      Mx := 0;
      for a:=alPastG to alPastR do
        if FArrays.Alerts[a] > Mx then Mx := FArrays.Alerts[a];
      Last24Label2.Caption := Summary[Max(0, Min(5, Mx))] + ' activity';

      for a:=alNextG to alNextR do
        if FArrays.Alerts[a] > Mx then Mx := FArrays.Alerts[a];
      Next24Label2.Caption := Summary[Max(0, Min(5, Mx))] + ' activity';
      end;

  AlignAlerts;
end;


procedure TLivePlotFrame.AlignAlerts;

  function LabelFits(Lb: TLabel): boolean;
    begin Result := (Lb.Left + Lb.Width) < Shape6.Width; end;

begin
  Last24Label1.Visible := LabelFits(Last24Label1) and LabelFits(Next24Label1);
  Next24Label1.Visible := Last24Label1.Visible;

  Last24Label2.Visible := LabelFits(Last24Label2) and LabelFits(Next24Label2);
  Next24Label2.Visible := Last24Label2.Visible;

  Label18.Visible := LabelFits(Label18);
end;


procedure TLivePlotFrame.AlignSummary;
var
  W: integer;
begin
  W := SummaryPanel.ClientWidth - 2 * SummaryPanel.BorderWidth;
  //QuickPanel3.Width := Max(W div 2, Label34.Left + Label34.Width);
  QuickPanel2.Visible := (W - QuickPanel3.Width) > (Label44.Left + Label44.Width);
end;


procedure TLivePlotFrame.AlertsPanelResize(Sender: TObject);
begin
  AlignAlerts;
  AlignSummary;
end;


procedure TLivePlotFrame.WhatsThisClick(Sender: TObject);
begin
  ShowHHelpPopup((Sender as TLabel).Tag);
end;





procedure TLivePlotFrame.Copy1Click(Sender: TObject);
begin
  RichEdit1.CopyToClipboard;
end;

procedure TLivePlotFrame.SelectAll1Click(Sender: TObject);
begin
  RichEdit1.SelStart := 0;
  RichEdit1.SelLength := MAXINT;
end;

end.

