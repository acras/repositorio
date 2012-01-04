object ShowMessageForm: TShowMessageForm
  Left = 268
  Top = 166
  ActiveControl = btnOK
  BorderIcons = []
  BorderStyle = bsDialog
  Caption = 'Mensagem'
  ClientHeight = 115
  ClientWidth = 308
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object edtMensagem: TLabel
    Left = 8
    Top = 8
    Width = 279
    Height = 69
    Caption = 
      'Bla bla bla bla bla bla bla bla bla bla bla bla bla bla bla bla ' +
      'bla bla bla bla bla bla bla bla bla bla bla'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    WordWrap = True
  end
  object lblFechar: TLabel
    Left = 8
    Top = 88
    Width = 268
    Height = 19
    Caption = 'Este aviso ir'#225' fechar em 10 segundos.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object btnOK: TButton
    Left = 216
    Top = 84
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 0
    OnClick = btnOKClick
  end
  object Timer: TTimer
    Enabled = False
    OnTimer = TimerTimer
    Left = 224
    Top = 24
  end
end
