unit SetDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Ini, AutoStrt, Buttons, FramLoad;

type
  TSettingsDialog = class(TForm)
    OkBtn: TButton;
    CancelBtn: TButton;
    GroupBox1: TGroupBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    Edit1: TEdit;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    OpenDialog1: TOpenDialog;
    CheckBox6: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure OkBtnClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure CheckBox5Click(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure CheckBox4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SettingsDialog: TSettingsDialog;

implementation

uses Main;

{$R *.DFM}

procedure TSettingsDialog.FormCreate(Sender: TObject);
begin
  CheckBox1.Checked := Ini.InTray;
  CheckBox2.Checked := IsAutoStart;
  CheckBox3.Checked := MainForm.FormStyle = fsStayOnTop;
  CheckBox4.Checked := MainForm.GetAutoDL;
  CheckBox5.Checked := MainForm.FPlaySound;
  CheckBox6.Checked := MainForm.LiveData1.Downloaders.IncludeReports;
  Edit1.Text := MainForm.FSoundFile;

  //enable WAV box
  CheckBox5Click(nil);
  //enable IncludingReports checkbox
  CheckBox4Click(nil);
end;

procedure TSettingsDialog.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Release;
end;

procedure TSettingsDialog.OkBtnClick(Sender: TObject);
begin
  Ini.InTray := CheckBox1.Checked;
  SetAutoStart(CheckBox2.Checked);
  if CheckBox3.Checked
    then MainForm.FormStyle := fsStayOnTop
    else MainForm.FormStyle := fsNormal;
  MainForm.SetAutoDL(CheckBox4.Checked);

  MainForm.FPlaySound := Checkbox5.Checked;
  MainForm.LiveData1.Downloaders.IncludeReports := CheckBox6.Checked;
  MainForm.FSoundFile := Edit1.text;
end;

procedure TSettingsDialog.FormShow(Sender: TObject);
var
  X, Y: integer;
begin
  X := MainForm.Left + (MainForm.Width - Width) div 2;
  Y := MainForm.Top + (MainForm.Height - Height) div 2;
  SetBounds(X, Y, Width, Height);
  DlgInWorkArea(Self);
end;

procedure TSettingsDialog.CheckBox5Click(Sender: TObject);
begin
  SpeedButton1.Enabled := CheckBox5.Checked;
  SpeedButton2.Enabled := CheckBox5.Checked and FileExists(Edit1.Text);

  Edit1.Enabled := CheckBox5.Checked;
  if CheckBox5.Checked
    then Edit1.Color := clWindow
    else Edit1.Color := clBtnFace;
end;

procedure TSettingsDialog.Edit1Change(Sender: TObject);
begin
  SpeedButton2.Enabled := CheckBox5.Checked and FileExists(Edit1.Text);
end;

procedure TSettingsDialog.SpeedButton1Click(Sender: TObject);
begin
  with OpenDialog1 do
    begin
    FileName := Edit1.Text;
    if Execute then Edit1.Text := FileName;
    end;
end;

procedure TSettingsDialog.SpeedButton2Click(Sender: TObject);
begin
  MainForm.PlayWavFile(Edit1.Text);    
end;

procedure TSettingsDialog.CheckBox4Click(Sender: TObject);
begin
  Checkbox6.Enabled := Checkbox4.Checked;
end;

end.

