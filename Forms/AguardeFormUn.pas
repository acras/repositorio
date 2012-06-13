unit AguardeFormUn;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TAguardeForm = class(TForm)
    lblAviso: TLabel;
    ProgressBar: TProgressBar;
  private
    Ftotal: integer;
    Fcurrent: integer;
    procedure Settotal(const Value: integer);
    procedure Setcurrent(const Value: integer);
  public
    property total: integer read Ftotal write Settotal;
    property current: integer read Fcurrent write Setcurrent;
    procedure mostrar(aviso: TCaption; height: integer=0;
      width:integer = 0; mostrarProgress: boolean = false);
    procedure esconder;
  end;

var
  AguardeForm: TAguardeForm;

implementation

{$R *.dfm}

{-------------------------------------------------------------------------
 Objetivo   > Esconde a janela de aguarde
 Parâmetros > Conforme documentação
 Retorno    >
 Criação    > Ricardo N. Acras
 Observações>
 Atualização>
 ------------------------------------------------------------------------}
procedure TAguardeForm.esconder;
begin
  screen.Cursor := crArrow;
  Hide;
end; //TAguardeForm.esconder

{-------------------------------------------------------------------------
 Objetivo   > Mostra a janela de aguarde. Muda o desenho do cursos, apresentando
              a ampulheta.
 Parâmetros > mostrarProgress: boolean que diz se a barra deve ser exibida ou nao
 Retorno    >
 Criação    > Ricardo N. Acras
 Observações>
 Atualização>
 ------------------------------------------------------------------------}
procedure TAguardeForm.mostrar(aviso: TCaption; height: integer = 0;
  width:integer = 0; mostrarProgress: boolean = false);
begin
  self.caption := 'Aguarde...';
  Screen.Cursor := crHourGlass;
  if height<>0 then
    self.Height := height
  else
    self.height := 74;
  if width<>0 then
    self.Width := width;
  self.lblAviso.Caption := aviso;

  ProgressBar.visible := mostrarProgress;

  AguardeForm.show;
  aguardeForm.Repaint;
end; //TAguardeForm.mostrar

{-------------------------------------------------------------------------
 Objetivo   > Transforma um valor em moeda
 Parâmetros > Value: Integer com o valor a ser transformado em moeda
 Retorno    >
 Criação    > Ricardo N. Acras
 Observações>
 Atualização>
 ------------------------------------------------------------------------}
procedure TAguardeForm.Setcurrent(const Value: integer);
begin
  Fcurrent := Value;
  ProgressBar.Position := value;
  if total <> 0 then
    self.caption := 'Aguarde... ('+ formatFloat('0.00%',(value/total)*100) +')';
  Application.ProcessMessages;
end; //TAguardeForm.Setcurrent

{-------------------------------------------------------------------------
 Objetivo   >
 Parâmetros > Value: Integer com o valor a ser mostrado
 Retorno    >
 Criação    > Ricardo N. Acras
 Observações>
 Atualização>
 ------------------------------------------------------------------------}
procedure TAguardeForm.Settotal(const Value: integer);
begin
  Ftotal := Value;
  ProgressBar.Max := value;
end; //TAguardeForm.Settotal

end.
