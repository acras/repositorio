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
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 688
    Height = 446
    ActivePage = TabSheet2
    Align = alClient
    TabOrder = 0
    object TabSheet1: TTabSheet
      Caption = 'TabSheet1'
      DesignSize = (
        680
        418)
      object Label5: TLabel
        Left = 192
        Top = 24
        Width = 78
        Height = 13
        Caption = 'Tipo de Arquivo:'
      end
      object Label6: TLabel
        Left = 328
        Top = 24
        Width = 32
        Height = 13
        Caption = 'Label6'
      end
      object Label1: TLabel
        Left = 192
        Top = 48
        Width = 95
        Height = 13
        Caption = 'Data/Hora Gera'#231#227'o'
      end
      object Label2: TLabel
        Left = 192
        Top = 72
        Width = 107
        Height = 13
        Caption = 'Sequencial do Arquivo'
      end
      object Label3: TLabel
        Left = 328
        Top = 48
        Width = 32
        Height = 13
        Caption = 'Label3'
      end
      object Label4: TLabel
        Left = 328
        Top = 72
        Width = 32
        Height = 13
        Caption = 'Label4'
      end
      object Button1: TButton
        Left = 24
        Top = 32
        Width = 75
        Height = 25
        Caption = 'Button1'
        TabOrder = 0
        OnClick = Button1Click
      end
      object DBGrid1: TDBGrid
        Left = 7
        Top = 232
        Width = 673
        Height = 120
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'TabSheet2'
      ImageIndex = 1
      object Button2: TButton
        Left = 144
        Top = 232
        Width = 115
        Height = 25
        Caption = 'Adicionar Boleto'
        TabOrder = 0
        OnClick = Button2Click
      end
      object DBGrid2: TDBGrid
        Left = 0
        Top = 264
        Width = 680
        Height = 154
        Align = alBottom
        DataSource = DataSource2
        TabOrder = 1
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
      end
      object Edit1: TEdit
        Left = 144
        Top = 32
        Width = 121
        Height = 21
        TabOrder = 2
        Text = 'Edit1'
      end
      object Edit2: TEdit
        Left = 144
        Top = 56
        Width = 121
        Height = 21
        TabOrder = 3
        Text = 'Edit2'
      end
      object DateTimePicker1: TDateTimePicker
        Left = 144
        Top = 88
        Width = 186
        Height = 21
        Date = 38644.918123553240000000
        Time = 38644.918123553240000000
        TabOrder = 4
      end
      object Edit3: TEdit
        Left = 144
        Top = 112
        Width = 121
        Height = 21
        TabOrder = 5
        Text = 'Edit3'
      end
      object Edit4: TEdit
        Left = 144
        Top = 136
        Width = 121
        Height = 21
        TabOrder = 6
        Text = 'Edit4'
      end
      object Edit5: TEdit
        Left = 144
        Top = 160
        Width = 121
        Height = 21
        TabOrder = 7
        Text = 'Edit5'
      end
      object Edit6: TEdit
        Left = 144
        Top = 184
        Width = 121
        Height = 21
        TabOrder = 8
        Text = 'Edit6'
      end
      object Edit7: TEdit
        Left = 144
        Top = 208
        Width = 121
        Height = 21
        TabOrder = 9
        Text = 'Edit7'
      end
      object Button3: TButton
        Left = 504
        Top = 232
        Width = 75
        Height = 25
        Caption = 'Button3'
        TabOrder = 10
        OnClick = Button3Click
      end
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 600
    Top = 24
  end
  object DataSource1: TDataSource
    Left = 568
    Top = 24
  end
  object DataSource2: TDataSource
    Left = 572
    Top = 64
  end
end
