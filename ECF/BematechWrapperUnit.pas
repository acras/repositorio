unit BematechWrapperUnit;

interface

uses
  BematechIntfUnit;

type
  TBematechWrapper = class(TInterfacedObject, IBematech)
  public
    function ProgramaAliquota(var Aliquota: string; Vinculo: Integer): Integer;

    function AbreCupom(CNPJCPF: string): Integer;
    function CancelaCupom: Integer;

    function VendeItem(Codigo, Descricao, AliquotaICMS, TipoQuantidade,
        Quantidade: string; QtdeDecimais: Integer; ValorUnitario, TipoDesconto,
        Desconto: string): Integer;
    function VendeItemDepartamento(Codigo, Descricao, Aliquota, ValorUnitario,
        Quantidade, Acrescimo, Desconto, IndiceDepartamento, UnidadeMedida: string): Integer;
    function CancelaItemGenerico(Numero: string): Integer;
    function AumentaDescricaoItem(Descricao: string): Integer;

    function IniciaFechamentoCupom(AcrescimoOuDesconto, TipoAcrescimoOuDesconto,
        ValorAcrescimoOuDesconto: string): Integer;
    function EfetuaFormaPagamentoDescricaoForma(Forma, Valor, Descricao: string): Integer;
    function EfetuaFormaPagamento(Forma, Valor: string): Integer;
    function TerminaFechamentoCupom(Mensagem: string): Integer;

    function LeituraX: Integer;
    function ReducaoZ(Data, Hora: string): Integer;

    function AberturaDoDia(var Valor, FormaPagamento: string): Integer;
    function FechamentoDoDia: Integer;
    function RetornoImpressora(var Ack, St1, St2: Integer): Integer;
    function VerificaImpressoraLigada: Integer;

    function DataHoraReducao(var DataReducao, HoraReducao: string): Integer;
    function ImprimeConfiguracoesImpressora: Integer;
    function RetornoAliquotas(var Aliquotas: string): Integer;

    function FlagsFiscais(var flag: integer): integer;
    function AbrePortaSerial: integer;

    function NumeroSerie(var num: string): integer;
    function numeroCupom(var numeroCupom: string): integer;

    function LeituraMemoriaFiscalData(dtInicial: string; dtFinal: string): integer;
    function DadosUltimaReducaoMFD(DadosReducao: string): Integer;
    function LeituraMemoriaFiscalDataMFD(DataInicial, DataFinal,
      FlagLeitura: string): Integer;
    function LeituraMemoriaFiscalSerialDataMFD(DataInicial, DataFinal,
      FlagLeitura: string): Integer;
    function LeituraMemoriaFiscalSerialDataPAFECF(DataInicial, DataFinal,
      FlagLeitura, chavePublica, chavePrivada: string): Integer;
    function LeituraMemoriaFiscalReducaoMFD(ReducaoInicial, ReducaoFinal,
      FlagLeitura: string): Integer;
    function LeituraMemoriaFiscalSerialReducaoMFD(ReducaoInicial, ReducaoFinal,
      FlagLeitura: string): Integer;
    function LeituraMemoriaFiscalSerialReducaoPAFECF(ReducaoInicial, ReducaoFinal,
      FlagLeitura, chavePublica, chavePrivada: string): Integer;
    function HabilitaDesabilitaRetornoEstendidoMFD(flag: string): integer;
  end;

implementation

uses
  Bematech;

{ TBematech }

{ TBematechWrapper }

function TBematechWrapper.AberturaDoDia(var Valor,
  FormaPagamento: string): Integer;
begin
  Result := Bematech_FI_AberturaDoDia(Valor, FormaPagamento);
end;

function TBematechWrapper.AbreCupom(CNPJCPF: string): Integer;
begin
  Result := Bematech_FI_AbreCupom(CNPJCPF);
end;

function TBematechWrapper.AumentaDescricaoItem(Descricao: string): Integer;
begin
  Result := Bematech_FI_AumentaDescricaoItem(Descricao);
end;

function TBematechWrapper.AbrePortaSerial: integer;
begin
  result := Bematech_FI_AbrePortaSerial;
end;

function TBematechWrapper.CancelaCupom: Integer;
begin
  Result := Bematech_FI_CancelaCupom;
end;

function TBematechWrapper.CancelaItemGenerico(Numero: string): Integer;
begin
  Result := Bematech_FI_CancelaItemGenerico(Numero);
end;

function TBematechWrapper.DataHoraReducao(var DataReducao,
  HoraReducao: string): Integer;
begin
  Result := Bematech_FI_DataHoraReducao(DataReducao, HoraReducao);
end;

function TBematechWrapper.EfetuaFormaPagamentoDescricaoForma(Forma, Valor,
  Descricao: string): Integer;
begin
  Result := Bematech_FI_EfetuaFormaPagamentoDescricaoForma(Forma, Valor, Descricao);
end;

function TBematechWrapper.EfetuaFormaPagamento(Forma, Valor: string): Integer;
begin
  Result := Bematech_FI_EfetuaFormaPagamento(PChar(Forma), PChar(Valor));
end;

function TBematechWrapper.FechamentoDoDia: Integer;
begin
  Result := Bematech_FI_FechamentoDoDia;
end;

function TBematechWrapper.FlagsFiscais(var flag: integer): integer;
begin
  result := Bematech_FI_FlagsFiscais(flag);
end;

function TBematechWrapper.ImprimeConfiguracoesImpressora: Integer;
begin
  Result := Bematech_FI_ImprimeConfiguracoesImpressora;
end;

function TBematechWrapper.IniciaFechamentoCupom(AcrescimoOuDesconto,
  TipoAcrescimoOuDesconto, ValorAcrescimoOuDesconto: string): Integer;
begin
  Result := Bematech_FI_IniciaFechamentoCupom(AcrescimoOuDesconto, TipoAcrescimoOuDesconto,
      ValorAcrescimoOuDesconto);
end;

function TBematechWrapper.LeituraX: Integer;
begin
  Result := Bematech_FI_LeituraX;
end;

function TBematechWrapper.ProgramaAliquota(var Aliquota: string;
  Vinculo: Integer): Integer;
begin
  Result := Bematech_FI_ProgramaAliquota(Aliquota, Vinculo);
end;

function TBematechWrapper.ReducaoZ(Data, Hora: string): Integer;
begin
  Result := Bematech_FI_ReducaoZ(Data, Hora);
end;

function TBematechWrapper.RetornoAliquotas(var Aliquotas: string): Integer;
begin
  Result := Bematech_FI_RetornoAliquotas(Aliquotas);
end;

function TBematechWrapper.RetornoImpressora(var Ack, St1,
  St2: Integer): Integer;
begin
  Result := Bematech_FI_RetornoImpressora(Ack, St1, St2);
end;

function TBematechWrapper.TerminaFechamentoCupom(
  Mensagem: string): Integer;
begin
  Result := Bematech_FI_TerminaFechamentoCupom(Mensagem);
end;

function TBematechWrapper.VendeItem(Codigo, Descricao, AliquotaICMS,
  TipoQuantidade, Quantidade: string; QtdeDecimais: Integer;
  ValorUnitario, TipoDesconto, Desconto: string): Integer;
begin
  Result := Bematech_FI_VendeItem(Codigo, Descricao, AliquotaICMS, TipoQuantidade, Quantidade,
      QtdeDecimais, ValorUnitario, TipoDesconto, Desconto);
end;

function TBematechWrapper.VendeItemDepartamento(Codigo, Descricao,
  Aliquota, ValorUnitario, Quantidade, Acrescimo, Desconto,
  IndiceDepartamento, UnidadeMedida: string): Integer;
begin
  Result := Bematech_FI_VendeItemDepartamento(Codigo, Descricao, Aliquota, ValorUnitario,
      Quantidade, Acrescimo, Desconto, IndiceDepartamento, UnidadeMedida);
end;

function TBematechWrapper.VerificaImpressoraLigada: Integer;
begin
  Result := Bematech_FI_VerificaImpressoraLigada;
end;

function TBematechWrapper.NumeroSerie(var num: string): Integer;
begin
  result := Bematech_FI_NumeroSerie(num);
end;

function TBematechWrapper.numeroCupom(var numeroCupom: string): integer;
begin
  result := Bematech_FI_NumeroCupom(numeroCupom);
end;

function TBematechWrapper.LeituraMemoriaFiscalData(dtInicial,
  dtFinal: string): integer;
begin
  result := Bematech_FI_LeituraMemoriaFiscalData(dtInicial, dtFinal);
end;

function TBematechWrapper.DadosUltimaReducaoMFD(DadosReducao : string): Integer;
begin
  result := Bematech_FI_DadosUltimaReducaoMFD(dadosReducao);
end;

function TBematechWrapper.LeituraMemoriaFiscalDataMFD(
  DataInicial, DataFinal, FlagLeitura : string): Integer;
begin
  result := Bematech_FI_LeituraMemoriaFiscalDataMFD(Pchar(dataInicial), PChar(dataFinal), PChar(FlagLeitura));
end;

function TBematechWrapper.LeituraMemoriaFiscalReducaoMFD(
  ReducaoInicial, ReducaoFinal, FlagLeitura : string): Integer;
begin
  result := Bematech_FI_LeituraMemoriaFiscalReducaoMFD(Pchar(ReducaoInicial), PChar(ReducaoFinal), PChar(FlagLeitura));
end;

function TBematechWrapper.LeituraMemoriaFiscalSerialDataMFD(
  DataInicial, DataFinal, FlagLeitura : string): Integer;
begin
  result := Bematech_FI_LeituraMemoriaFiscalSerialDataMFD(Pchar(dataInicial), PChar(dataFinal), PChar(FlagLeitura));
end;

function TBematechWrapper.LeituraMemoriaFiscalSerialReducaoMFD(
  ReducaoInicial, ReducaoFinal, FlagLeitura : string): Integer;
begin
  result := Bematech_FI_LeituraMemoriaFiscalSerialReducaoMFD(Pchar(ReducaoInicial), PChar(ReducaoFinal), PChar(FlagLeitura));
end;

function TBematechWrapper.HabilitaDesabilitaRetornoEstendidoMFD(flag: string): integer;
begin
  result := Bematech_FI_HabilitaDesabilitaRetornoEstendidoMFD(flag);
end;

function TBematechWrapper.LeituraMemoriaFiscalSerialDataPAFECF(DataInicial,
  DataFinal, FlagLeitura, chavePublica, chavePrivada: string): Integer;
begin
  result := Bematech_FI_LeituraMemoriaFiscalSerialDataPAFECF(Pchar(dataInicial), PChar(dataFinal), PChar(FlagLeitura), PCHar(chavePublica), PChar(chavePrivada));
end;

function TBematechWrapper.LeituraMemoriaFiscalSerialReducaoPAFECF(
  ReducaoInicial, ReducaoFinal, FlagLeitura, chavePublica,
  chavePrivada: string): Integer;
begin
  result := Bematech_FI_LeituraMemoriaFiscalSerialReducaoPAFECF(Pchar(ReducaoInicial), PChar(ReducaoFinal), PChar(FlagLeitura), PCHar(chavePublica), PChar(chavePrivada));
end;

end.
