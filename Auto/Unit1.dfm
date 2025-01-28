object Form1: TForm1
  Left = 272
  Top = 107
  Width = 255
  Height = 247
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object SpeedButton1: TSpeedButton
    Left = 20
    Top = 152
    Width = 69
    Height = 22
    Caption = 'Get value'
    OnClick = SpeedButton1Click
  end
  object Label1: TLabel
    Left = 24
    Top = 188
    Width = 32
    Height = 13
    Caption = 'Label1'
  end
  object SpeedButton2: TSpeedButton
    Left = 168
    Top = 16
    Width = 61
    Height = 22
    AllowAllUp = True
    GroupIndex = 1
    Caption = 'Connect'
    OnClick = SpeedButton2Click
  end
  object SpeedButton3: TSpeedButton
    Left = 168
    Top = 40
    Width = 61
    Height = 22
    AllowAllUp = True
    GroupIndex = 2
    Caption = 'Show'
    OnClick = SpeedButton3Click
  end
  object SpeedButton4: TSpeedButton
    Left = 168
    Top = 120
    Width = 61
    Height = 22
    Caption = 'DL Latest'
    OnClick = SpeedButton4Click
  end
  object SpeedButton5: TSpeedButton
    Left = 168
    Top = 148
    Width = 61
    Height = 22
    Caption = 'DL Recent'
    OnClick = SpeedButton5Click
  end
  object SpeedButton6: TSpeedButton
    Left = 168
    Top = 176
    Width = 61
    Height = 22
    Caption = 'DL Old'
    OnClick = SpeedButton6Click
  end
  object Label2: TLabel
    Left = 20
    Top = 8
    Width = 21
    Height = 13
    Caption = 'Kind'
  end
  object Label3: TLabel
    Left = 20
    Top = 56
    Width = 23
    Height = 13
    Caption = 'Date'
  end
  object Label4: TLabel
    Left = 20
    Top = 100
    Width = 23
    Height = 13
    Caption = 'Time'
  end
  object SpinEdit1: TSpinEdit
    Left = 20
    Top = 28
    Width = 121
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 0
    Value = 0
    OnChange = SpeedButton1Click
  end
  object DateTimePicker1: TDateTimePicker
    Left = 20
    Top = 72
    Width = 121
    Height = 21
    CalAlignment = dtaLeft
    Date = 37676.7629539005
    Time = 37676.7629539005
    DateFormat = dfShort
    DateMode = dmUpDown
    Kind = dtkDate
    ParseInput = False
    TabOrder = 1
    OnChange = SpeedButton1Click
  end
  object DateTimePicker2: TDateTimePicker
    Left = 20
    Top = 116
    Width = 121
    Height = 21
    CalAlignment = dtaLeft
    Date = 37676.7629539005
    Time = 37676.7629539005
    DateFormat = dfShort
    DateMode = dmUpDown
    Kind = dtkTime
    ParseInput = False
    TabOrder = 2
    OnChange = SpeedButton1Click
  end
  object CheckBox1: TCheckBox
    Left = 172
    Top = 92
    Width = 58
    Height = 17
    Caption = 'AutoDL'
    TabOrder = 3
    OnClick = CheckBox1Click
  end
end
