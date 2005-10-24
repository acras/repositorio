unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Label1: TLabel;
    Button1: TButton;
    Label2: TLabel;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses ValidaDocumentos;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  label2.Caption := CalcDV_CPF(edit1.Text);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if check_CPF(edit1.Text) then
    ShowMessage('valido')
  else
    ShowMessage('nao é valido');
end;

end.
