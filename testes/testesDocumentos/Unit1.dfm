object Form1: TForm1
  Left = 192
  Top = 114
  Width = 696
  Height = 480
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 48
    Top = 32
    Width = 20
    Height = 13
    Caption = 'CPF'
  end
  object Label2: TLabel
    Left = 56
    Top = 104
    Width = 32
    Height = 13
    Caption = 'Label2'
  end
  object Edit1: TEdit
    Left = 48
    Top = 48
    Width = 121
    Height = 21
    TabOrder = 0
    Text = 'Edit1'
  end
  object Button1: TButton
    Left = 40
    Top = 72
    Width = 75
    Height = 25
    Caption = 'CalcDV'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 120
    Top = 72
    Width = 75
    Height = 25
    Caption = 'Check'
    TabOrder = 2
    OnClick = Button2Click
  end
end
