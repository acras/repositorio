unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, xmldom, Provider, Xmlxform, StdCtrls;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    Label3: TLabel;
    Label4: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses ProtocoloLoteGuiasUn;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  with TProtocoloLoteGuias.Create(nil) do
  begin
    xtrFileName := edit1.Text;
    xmlFileName := edit2.Text;

    label3.Caption := 'Número do Lote: ' + numeroLote;
    label4.Caption := 'Número do Protocolo de Recebimento: ' + numeroProtocoloRecebimento;
    free;
  end;
end;

end.
