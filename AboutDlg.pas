unit AboutDlg;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Ini;

type
  TAboutDialog = class(TForm)
    Bevel1: TBevel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label7: TLabel;
    Label6: TLabel;
    Label8: TLabel;
    Button1: TButton;
    Image1: TImage;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Label5Click(Sender: TObject);
    procedure Label6Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AboutDialog: TAboutDialog;

implementation

uses Main;

{$R *.DFM}

procedure TAboutDialog.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Release;
end;

procedure TAboutDialog.Label5Click(Sender: TObject);
begin
  MainForm.EmailMNU.Click;
end;

procedure TAboutDialog.Label6Click(Sender: TObject);
begin
  MainForm.WebsiteMNU.Click;
end;


procedure TAboutDialog.FormShow(Sender: TObject);
var
  X, Y: integer;
begin
  Image1.Picture.Icon := Application.Icon;
  Label1.Caption := Application.Title;
  Label2.Caption := Application.Title;

  X := MainForm.Left + (MainForm.Width - Width) div 2;
  Y := MainForm.Top + (MainForm.Height - Height) div 2;
  SetBounds(X, Y, Width, Height);
  DlgInWorkArea(Self);
end;

end.

