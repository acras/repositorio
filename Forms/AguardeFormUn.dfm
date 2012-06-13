object AguardeForm: TAguardeForm
  Left = 883
  Top = 603
  BorderIcons = []
  BorderStyle = bsToolWindow
  Caption = 'Aguarde...'
  ClientHeight = 66
  ClientWidth = 227
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object lblAviso: TLabel
    Left = 8
    Top = 8
    Width = 213
    Height = 26
    Caption = 
      'Gerando os Dados de Exporta'#231#227'o...'#13#10'Esta opera'#231#227'o pode demorar al' +
      'guns minutos.'
  end
  object ProgressBar: TProgressBar
    Left = 0
    Top = 49
    Width = 227
    Height = 17
    Align = alBottom
    TabOrder = 0
    Visible = False
  end
end
