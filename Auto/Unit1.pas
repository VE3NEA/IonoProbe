unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Buttons, Spin, ComObj, IonoProbe_TLB;

type
  TForm1 = class(TForm)
    SpinEdit1: TSpinEdit;
    SpeedButton1: TSpeedButton;
    DateTimePicker1: TDateTimePicker;
    Label1: TLabel;
    DateTimePicker2: TDateTimePicker;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    CheckBox1: TCheckBox;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
  private
    procedure DataArrived(Sender: TObject);
    procedure ExitEvent(Sender: TObject);
    { Private declarations }
  public
    Probe: TIonoProbeApp;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.FormCreate(Sender: TObject);
begin
  //Probe := CreateOleObject('IonoProbe.IonoProbeApp');
end;


procedure TForm1.ExitEvent(Sender: TObject);
begin
  FreeAndNil(Probe);
  SpeedButton2.Down := false;
  SpeedButton3.Down := false;
end;


procedure TForm1.DataArrived(Sender: TObject);
begin
  Label1.Caption := 'New Data';
  Beep;
end;




procedure TForm1.SpeedButton1Click(Sender: TObject);
var
  T: TDateTime;
  V: Single;
begin
  T := Trunc(DateTimePicker1.DateTime) + Frac(DateTimePicker2.DateTime);
  V := Probe.GetValue(SpinEdit1.Value, T);
  Label1.Caption := Format('%.4f', [V]);
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
begin
  if SpeedButton2.Down
    then
      begin
      Probe := TIonoProbeApp.Create(Self);
      Probe.OnExitRequest := ExitEvent;
      Probe.OnDataArrived := DataArrived;

      CheckBox1.Checked := Probe.AutoDL;
      Label1.Caption := Format('%d.%d', [HiWord(Probe.Version), LoWord(Probe.Version)]);

      if Probe.Version < $0010001 then FreeAndNil(Probe);
      end
    else
      FreeAndNil(Probe);

  SpeedButton2.Down := Probe <> nil;
  SpeedButton3.Down := false;
end;



procedure TForm1.SpeedButton3Click(Sender: TObject);
begin
  Probe.Visible := SpeedButton3.Down;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  Probe.AutoDL := CheckBox1.Checked;
end;

procedure TForm1.SpeedButton4Click(Sender: TObject);
begin
  Probe.DownloadLatest;
end;

procedure TForm1.SpeedButton5Click(Sender: TObject);
begin
  Probe.DownloadRecent;
end;

procedure TForm1.SpeedButton6Click(Sender: TObject);
begin
  Probe.DownloadOld;
end;

end.

