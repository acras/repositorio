object Form1: TForm1
  Left = 192
  Top = 122
  Width = 689
  Height = 270
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
    Left = 32
    Top = 16
    Width = 22
    Height = 13
    Caption = 'XTR'
  end
  object Label2: TLabel
    Left = 32
    Top = 40
    Width = 22
    Height = 13
    Caption = 'XML'
  end
  object Label3: TLabel
    Left = 40
    Top = 144
    Width = 58
    Height = 22
    Caption = 'Label3'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object Label4: TLabel
    Left = 40
    Top = 184
    Width = 58
    Height = 22
    Caption = 'Label3'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
  end
  object Edit1: TEdit
    Left = 80
    Top = 16
    Width = 361
    Height = 21
    TabOrder = 0
    Text = 'c:\tissao\TISS_RetornoProtocolo.xtr'
  end
  object Edit2: TEdit
    Left = 80
    Top = 40
    Width = 361
    Height = 21
    TabOrder = 1
    Text = 'c:\tissao\teste.xml'
  end
  object Button1: TButton
    Left = 360
    Top = 72
    Width = 75
    Height = 25
    Caption = 'Vai'
    TabOrder = 2
    OnClick = Button1Click
  end
end
