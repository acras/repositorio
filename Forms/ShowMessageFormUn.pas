unit ShowMessageFormUn;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TShowMessageForm = class(TForm)
    edtMensagem: TLabel;
    Timer: TTimer;
    lblFechar: TLabel;
    btnOK: TButton;
    procedure TimerTimer(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    remaning: integer;
    waitOK: boolean;
    procedure updateLabel;
  public
    { Public declarations }
    class procedure showMessage(msg: string; time: integer = 0);
  end;

var
  ShowMessageForm: TShowMessageForm;

implementation

{$R *.dfm}

{ TForm2 }

class procedure TShowMessageForm.showMessage(msg: string; time: integer = 0);
begin
  with TShowMessageForm.Create(nil) do
  begin
    waitOK := time = 0;
    edtMensagem.Caption := msg;
    lblFechar.Visible := not waitOK;
    btnOk.visible := waitOK;
    if time > 0 then
    begin
      remaning := time;
      updateLabel;
      Timer.Enabled := true;
      application.processMessages;
      show();
    end
    else
    begin
      showModal;
    end;
  end;
end;

procedure TShowMessageForm.updateLabel;
begin
  lblFechar.Caption := 'Este aviso irá fechar em ' + IntToStr(remaning) + ' segundo';
  if remaning > 1 then
    lblFechar.Caption := lblFechar.Caption + 's';
  application.processMessages;    
end;

procedure TShowMessageForm.TimerTimer(Sender: TObject);
begin
  remaning := remaning - 1;
  if remaning > 0 then
    updateLabel
  else
  begin
    Timer.Enabled := false;
    close;
  end;
end;

procedure TShowMessageForm.btnOkClick(Sender: TObject);
begin
  close;
end;

procedure TShowMessageForm.FormShow(Sender: TObject);
begin
  SetForegroundWindow(self.Handle);
end;

end.
