object AboutDialog: TAboutDialog
  Left = 394
  Top = 103
  BorderStyle = bsDialog
  Caption = 'About IonoProbe'
  ClientHeight = 142
  ClientWidth = 348
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Bevel1: TBevel
    Left = 8
    Top = 8
    Width = 330
    Height = 125
    Anchors = [akLeft, akTop, akRight]
    Shape = bsFrame
  end
  object Label1: TLabel
    Left = 67
    Top = 16
    Width = 253
    Height = 35
    Caption = 'VersionNumber'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clAqua
    Font.Height = -29
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
  end
  object Label2: TLabel
    Left = 68
    Top = 17
    Width = 253
    Height = 35
    Caption = 'VersionNumber'
    Color = clBtnFace
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -29
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
    Transparent = True
  end
  object Label3: TLabel
    Left = 24
    Top = 66
    Width = 213
    Height = 13
    Caption = 'Copyright © 2002, 2003 Afreet Software, Inc.'
  end
  object Label4: TLabel
    Left = 24
    Top = 86
    Width = 37
    Height = 13
    Caption = 'Author: '
  end
  object Label5: TLabel
    Left = 76
    Top = 86
    Width = 132
    Height = 13
    Cursor = crHandPoint
    Hint = 'support@dxatlas.com'
    Caption = 'Alex Shovkoplyas, VE3NEA'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    OnClick = Label5Click
  end
  object Label7: TLabel
    Left = 24
    Top = 106
    Width = 45
    Height = 13
    Caption = 'Web site:'
  end
  object Label6: TLabel
    Left = 76
    Top = 106
    Width = 168
    Height = 13
    Cursor = crHandPoint
    Hint = 'http://www.dxatlas.com/IonoProbe'
    Caption = 'http://www.dxatlas.com/IonoProbe'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsUnderline]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    OnClick = Label6Click
  end
  object Label8: TLabel
    Left = 248
    Top = 56
    Width = 75
    Height = 13
    Caption = 'Internal build'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    Visible = False
  end
  object Image1: TImage
    Left = 25
    Top = 22
    Width = 32
    Height = 32
  end
  object Button1: TButton
    Left = 253
    Top = 96
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Close'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
end
