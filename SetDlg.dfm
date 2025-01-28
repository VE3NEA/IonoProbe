object SettingsDialog: TSettingsDialog
  Left = 403
  Top = 109
  BorderStyle = bsDialog
  Caption = 'IonoProbe Settings'
  ClientHeight = 242
  ClientWidth = 286
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefaultPosOnly
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object OkBtn: TButton
    Left = 121
    Top = 211
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
    OnClick = OkBtnClick
  end
  object CancelBtn: TButton
    Left = 201
    Top = 211
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object GroupBox1: TGroupBox
    Left = 9
    Top = 5
    Width = 266
    Height = 194
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = ' Settings '
    TabOrder = 2
    object SpeedButton1: TSpeedButton
      Left = 208
      Top = 150
      Width = 23
      Height = 22
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00555555555555
        5555555555555555555555555555555555555555555555555555555555555555
        555555555555555555555555555555555555555FFFFFFFFFF555550000000000
        55555577777777775F55500B8B8B8B8B05555775F555555575F550F0B8B8B8B8
        B05557F75F555555575F50BF0B8B8B8B8B0557F575FFFFFFFF7F50FBF0000000
        000557F557777777777550BFBFBFBFB0555557F555555557F55550FBFBFBFBF0
        555557F555555FF7555550BFBFBF00055555575F555577755555550BFBF05555
        55555575FFF75555555555700007555555555557777555555555555555555555
        5555555555555555555555555555555555555555555555555555}
      NumGlyphs = 2
      OnClick = SpeedButton1Click
    end
    object SpeedButton2: TSpeedButton
      Left = 232
      Top = 150
      Width = 23
      Height = 22
      Glyph.Data = {
        76010000424D7601000000000000760000002800000020000000100000000100
        04000000000000010000120B0000120B00001000000000000000000000000000
        800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        33333333333FFF3333F333333300033339333333337773F33733333330008033
        933333333737F7F37333333307078733333933337337373F3337333077088803
        33933337F37F337F3373333077088803393333F7337FF37F3733300777008803
        9333377F33773F7F733307088808087333337F7F337F7F7F3FFF070777080873
        99997F7F337F7F7F77770808880808733333737F337F737F3F33300888008803
        93333773F377337F73F333308807880339333337F37F337F373F333088077803
        339333373F73F37333733333087777333339333373F7F7F33F37333330807033
        933333333737F73373F333333300033339333333337773333733}
      Margin = 1
      NumGlyphs = 2
      OnClick = SpeedButton2Click
    end
    object CheckBox1: TCheckBox
      Left = 8
      Top = 38
      Width = 206
      Height = 17
      Caption = 'Start in the System Tray'
      TabOrder = 0
    end
    object CheckBox2: TCheckBox
      Left = 8
      Top = 16
      Width = 241
      Height = 17
      Caption = 'Run this program when Windows starts'
      TabOrder = 1
    end
    object CheckBox3: TCheckBox
      Left = 8
      Top = 60
      Width = 153
      Height = 17
      Caption = 'Always stay on top'
      TabOrder = 2
    end
    object CheckBox4: TCheckBox
      Left = 8
      Top = 82
      Width = 177
      Height = 17
      Caption = 'Auto download data'
      TabOrder = 3
      OnClick = CheckBox4Click
    end
    object CheckBox5: TCheckBox
      Left = 8
      Top = 130
      Width = 189
      Height = 17
      Caption = 'Play this sound when a storm starts:'
      TabOrder = 4
      OnClick = CheckBox5Click
    end
    object Edit1: TEdit
      Left = 24
      Top = 150
      Width = 181
      Height = 21
      TabOrder = 5
      OnChange = Edit1Change
    end
    object CheckBox6: TCheckBox
      Left = 32
      Top = 104
      Width = 125
      Height = 17
      Caption = 'Including reports'
      TabOrder = 6
    end
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'wav'
    Filter = 'Wave files (*.wav)|*.wav|All files (*.*)|*.*'
    Options = [ofFileMustExist, ofEnableSizing]
    Title = 'Select sound file'
    Left = 228
    Top = 104
  end
end
