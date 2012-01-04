unit acBematechUtils;

interface

uses
  Bematech;

function temGerencialAberto: boolean;
function temNaoFiscalAberto: boolean;
procedure fechaComprovantes;


implementation

function temGerencialAberto: boolean;
var
  i: integer;
begin
  Bematech_FI_StatusEstendidoMFD(i);
  result := (i and 4) = 4;
end;

function temNaoFiscalAberto: boolean;
var
  i: integer;
begin
  Bematech_FI_StatusEstendidoMFD(i);
  result := (i and 2) = 2;
end;

procedure fechaComprovantes;
begin
  //if temNaoFiscalAberto then
    Bematech_FI_FechaComprovanteNaoFiscalVinculado;
  //if temGerencialAberto then
    Bematech_FI_FechaRelatorioGerencial;
end;


end.
 