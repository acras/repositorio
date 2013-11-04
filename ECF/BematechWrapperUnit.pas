unit BematechWrapperUnit;

interface

uses
  BematechIntfUnit, SysUtils, dialogs;

type
  TBematechWrapper = class(TInterfacedObject, IBematech)
  public
    function ProgramaAliquota(var Aliquota: AnsiString; Vinculo: Integer): Integer;

    function AbreCupom(CNPJCPF: AnsiString): Integer;
    function AbreCupomMFD(CGC: AnsiString; Nome: AnsiString; Endereco : AnsiString): Integer;
    function CancelaCupom: Integer;

    function VendeItem(Codigo, Descricao, AliquotaICMS, TipoQuantidade,
        Quantidade: AnsiString; QtdeDecimais: Integer; ValorUnitario, TipoDesconto,
        Desconto: AnsiString): Integer;
    function VendeItemDepartamento(Codigo, Descricao, Aliquota, ValorUnitario,
        Quantidade, Acrescimo, Desconto, IndiceDepartamento, UnidadeMedida: AnsiString): Integer;
    function CancelaItemGenerico(Numero: AnsiString): Integer;
    function AumentaDescricaoItem(Descricao: AnsiString): Integer;

    function IniciaFechamentoCupom(AcrescimoOuDesconto, TipoAcrescimoOuDesconto,
        ValorAcrescimoOuDesconto: AnsiString): Integer;
    function EfetuaFormaPagamentoDescricaoForma(Forma, Valor, Descricao: AnsiString): Integer;
    function EfetuaFormaPagamento(Forma, Valor: AnsiString): Integer;
    function TerminaFechamentoCupom(Mensagem: AnsiString): Integer;

    function LeituraX: Integer;
    function ReducaoZ(Data, Hora: AnsiString): Integer;

    function AberturaDoDia(var Valor, FormaPagamento: AnsiString): Integer;
    function FechamentoDoDia: Integer;
    function RetornoImpressora(var Ack, St1, St2: Integer): Integer;
    function RetornoImpressoraMFD(var Ack, St1, St2, St3: Integer): Integer;
    function VerificaImpressoraLigada: Integer;

    function DataHoraReducao(var DataReducao, HoraReducao: AnsiString): Integer;
    function DataMovimento(var DataMovimento: AnsiString): Integer;
    function DataHoraImpressora(var Data, Hora: AnsiString): Integer;
    function ImprimeConfiguracoesImpressora: Integer;
    function RetornoAliquotas(var Aliquotas: AnsiString): Integer;

    function FlagsFiscais(var flag: integer): integer;
    function AbrePortaSerial: integer;

    function NumeroSerie(var num: AnsiString): integer;
    function numeroCupom(var numeroCupom: AnsiString): integer;

    function LeituraMemoriaFiscalData(dtInicial: AnsiString; dtFinal: AnsiString): integer;
    function DadosUltimaReducaoMFD(DadosReducao: AnsiString): Integer;
    function LeituraMemoriaFiscalDataMFD(DataInicial, DataFinal,
      FlagLeitura: AnsiString): Integer;
    function LeituraMemoriaFiscalSerialDataMFD(DataInicial, DataFinal,
      FlagLeitura: AnsiString): Integer;
    function LeituraMemoriaFiscalSerialDataPAFECF(DataInicial, DataFinal,
      FlagLeitura, chavePublica, chavePrivada: AnsiString): Integer;
    function LeituraMemoriaFiscalReducaoMFD(ReducaoInicial, ReducaoFinal,
      FlagLeitura: AnsiString): Integer;
    function LeituraMemoriaFiscalSerialReducaoMFD(ReducaoInicial, ReducaoFinal,
      FlagLeitura: AnsiString): Integer;
    function LeituraMemoriaFiscalSerialReducaoPAFECF(ReducaoInicial, ReducaoFinal,
      FlagLeitura, chavePublica, chavePrivada: AnsiString): Integer;
    function ArquivoMFD(ArquivoOrigem, DataOuCOOInicial, DataOuCOOFinal, TipoDownload,
      Usuario: AnsiString; TipoGeracao: integer; ChavePublica, ChavePrivada: AnsiString; UnicoArquivo: integer): integer;
    function EspelhoMFD(NomeArquivo, DataOuCOOInicial, DataOuCOOFinal,
      TipoDownload, Usuario, ChavePublica, ChavePrivada: AnsiString): integer;
    function DownloadMF( Arquivo: AnsiString ): Integer;
    function DownloadMFD( Arquivo: AnsiString; TipoDownload: AnsiString; ParametroInicial: AnsiString; ParametroFinal: AnsiString; UsuarioECF: AnsiString ): Integer;
    function FormatoDadosMFD( ArquivoOrigem: AnsiString; ArquivoDestino: AnsiString; TipoFormato: AnsiString; TipoDownload: AnsiString; ParametroInicial: AnsiString; ParametroFinal: AnsiString; UsuarioECF: AnsiString ): Integer;
    function SubTotal(var SubTotal: AnsiString): Integer;
    function LeituraXSerial: Integer;
    function VersaoFirmware(var VersaoFirmware: AnsiString): Integer;
    function VersaoFirmwareMFD(var VersaoFirmware: AnsiString): Integer;
    function CGC_IE(var CGC: AnsiString; var IE: AnsiString): Integer;
    function GrandeTotal(var GrandeTotal: AnsiString): Integer;
    function DataHoraGravacaoUsuarioSWBasicoMFAdicional(var DataHoraUsuario, DataHoraSWBasico, MFAdicional: AnsiString): integer;
    function Sangria(Valor: AnsiString): Integer;
    function Suprimento(Valor, FormaPagamento: AnsiString): Integer;
    function DataHoraUltimoDocumentoMFD(var dataHora: AnsiString): integer;
    function ContadorRelatoriosGerenciaisMFD(var contador: AnsiString): integer;
    function NumeroOperacoesNaoFiscais(var numOper: AnsiString): integer;
    function ContadorComprovantesCreditoMFD(var contador: AnsiString): integer;
    function ContadorCupomFiscalMFD(var contador: AnsiString): integer;
    function AcionaGaveta: integer;
    function habilitaDesabilitaRetornoEstendidoMFD(flag: AnsiString): integer;
  end;

implementation

uses
  Bematech;

{ TBematech }

{ TBematechWrapper }

function TBematechWrapper.AberturaDoDia(var Valor,
  FormaPagamento: AnsiString): Integer;
begin
  Result := Bematech_FI_AberturaDoDia(Valor, FormaPagamento);
end;

function TBematechWrapper.AbreCupom(CNPJCPF: AnsiString): Integer;
begin
  Result := Bematech_FI_AbreCupom(CNPJCPF);
end;

function TBematechWrapper.AbreCupomMFD(CGC, Nome,
  Endereco: AnsiString): Integer;
begin
  Result := Bematech_FI_AbreCupomMFD(CGC, Nome, Endereco);
end;

function TBematechWrapper.AumentaDescricaoItem(Descricao: AnsiString): Integer;
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

function TBematechWrapper.CancelaItemGenerico(Numero: AnsiString): Integer;
begin
  Result := Bematech_FI_CancelaItemGenerico(Numero);
end;

function TBematechWrapper.DataHoraReducao(var DataReducao,
  HoraReducao: AnsiString): Integer;
begin
  Result := Bematech_FI_DataHoraReducao(DataReducao, HoraReducao);
end;

function TBematechWrapper.DataHoraImpressora(var Data, Hora: AnsiString): Integer;
var
  d, h: AnsiString;
begin
  SetLength(d, 6);
  SetLength(h, 6);
  Result := Bematech_FI_DataHoraImpressora(d, h);
  Data := d;
  Hora := h;
end;

function TBematechWrapper.GrandeTotal(var GrandeTotal: AnsiString): Integer;
var
  s: AnsiString;
begin
  SetLength(s, 18);
  result := Bematech_FI_GrandeTotal(s);
  GrandeTotal := s;
end;


function TBematechWrapper.EfetuaFormaPagamentoDescricaoForma(Forma, Valor,
  Descricao: AnsiString): Integer;
begin
  Result := Bematech_FI_EfetuaFormaPagamentoDescricaoForma(Forma, Valor, Descricao);
end;

function TBematechWrapper.EfetuaFormaPagamento(Forma, Valor: AnsiString): Integer;
begin
  Result := Bematech_FI_EfetuaFormaPagamento(Forma, Valor);
end;

function TBematechWrapper.FechamentoDoDia: Integer;
begin
  Result := Bematech_FI_FechamentoDoDia;
end;

function TBematechWrapper.Sangria( Valor: AnsiString ): Integer;
begin
  result := Bematech_FI_Sangria(valor);
end;

function TBematechWrapper.Suprimento( Valor: AnsiString; FormaPagamento: AnsiString ): Integer;
begin
  result := Bematech_FI_Suprimento(Valor, FormaPagamento);
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
  TipoAcrescimoOuDesconto, ValorAcrescimoOuDesconto: AnsiString): Integer;
begin
  Result := Bematech_FI_IniciaFechamentoCupom(AcrescimoOuDesconto, TipoAcrescimoOuDesconto,
      ValorAcrescimoOuDesconto);
end;

function TBematechWrapper.LeituraX: Integer;
begin
  Result := Bematech_FI_LeituraX;
end;

function TBematechWrapper.ProgramaAliquota(var Aliquota: AnsiString;
  Vinculo: Integer): Integer;
begin
  Result := Bematech_FI_ProgramaAliquota(Aliquota, Vinculo);
end;

function TBematechWrapper.ReducaoZ(Data, Hora: AnsiString): Integer;
begin
  Result := Bematech_FI_ReducaoZ(Data, Hora);
end;

function TBematechWrapper.RetornoAliquotas(var Aliquotas: AnsiString): Integer;
begin
  Result := Bematech_FI_RetornoAliquotas(Aliquotas);
end;

function TBematechWrapper.RetornoImpressora(var Ack, St1,
  St2: Integer): Integer;
begin
  Result := Bematech_FI_RetornoImpressora(Ack, St1, St2);
end;

function TBematechWrapper.TerminaFechamentoCupom(
  Mensagem: AnsiString): Integer;
begin
  Result := Bematech_FI_TerminaFechamentoCupom(Mensagem);
end;

function TBematechWrapper.VendeItem(Codigo, Descricao, AliquotaICMS,
  TipoQuantidade, Quantidade: AnsiString; QtdeDecimais: Integer;
  ValorUnitario, TipoDesconto, Desconto: AnsiString): Integer;
begin
  Result := Bematech_FI_VendeItem(Codigo, Descricao, AliquotaICMS, TipoQuantidade, Quantidade,
      QtdeDecimais, ValorUnitario, TipoDesconto, Desconto);
end;

function TBematechWrapper.VendeItemDepartamento(Codigo, Descricao,
  Aliquota, ValorUnitario, Quantidade, Acrescimo, Desconto,
  IndiceDepartamento, UnidadeMedida: AnsiString): Integer;
begin
  Result := Bematech_FI_VendeItemDepartamento(Codigo, Descricao, Aliquota, ValorUnitario,
      Quantidade, Acrescimo, Desconto, IndiceDepartamento, UnidadeMedida);
end;

function TBematechWrapper.VerificaImpressoraLigada: Integer;
begin
  Result := Bematech_FI_VerificaImpressoraLigada;
end;

function TBematechWrapper.NumeroSerie(var num: AnsiString): Integer;
begin
  result := Bematech_FI_NumeroSerieMFD(num);
end;

function TBematechWrapper.numeroCupom(var numeroCupom: AnsiString): integer;
begin
  result := Bematech_FI_NumeroCupom(numeroCupom);
end;

function TBematechWrapper.LeituraMemoriaFiscalData(dtInicial,
  dtFinal: AnsiString): integer;
begin
  result := Bematech_FI_LeituraMemoriaFiscalData(dtInicial, dtFinal);
end;

function TBematechWrapper.DadosUltimaReducaoMFD(DadosReducao : AnsiString): Integer;
begin
  result := Bematech_FI_DadosUltimaReducaoMFD(dadosReducao);
end;

function TBematechWrapper.LeituraMemoriaFiscalDataMFD(
  DataInicial, DataFinal, FlagLeitura : AnsiString): Integer;
begin
  result := Bematech_FI_LeituraMemoriaFiscalDataMFD(Pchar(dataInicial), PChar(dataFinal), PChar(FlagLeitura));
end;

function TBematechWrapper.LeituraMemoriaFiscalReducaoMFD(
  ReducaoInicial, ReducaoFinal, FlagLeitura : AnsiString): Integer;
begin
  result := Bematech_FI_LeituraMemoriaFiscalReducaoMFD(Pchar(ReducaoInicial), PChar(ReducaoFinal), PChar(FlagLeitura));
end;

function TBematechWrapper.LeituraMemoriaFiscalSerialDataMFD(
  DataInicial, DataFinal, FlagLeitura : AnsiString): Integer;
begin
  result := Bematech_FI_LeituraMemoriaFiscalSerialDataMFD(Pchar(dataInicial), PChar(dataFinal), PChar(FlagLeitura));
end;

function TBematechWrapper.LeituraMemoriaFiscalSerialReducaoMFD(
  ReducaoInicial, ReducaoFinal, FlagLeitura : AnsiString): Integer;
begin
  result := Bematech_FI_LeituraMemoriaFiscalSerialReducaoMFD(Pchar(ReducaoInicial), PChar(ReducaoFinal), PChar(FlagLeitura));
end;

function TBematechWrapper.LeituraMemoriaFiscalSerialDataPAFECF(DataInicial,
  DataFinal, FlagLeitura, chavePublica, chavePrivada: AnsiString): Integer;
begin
  result := Bematech_FI_LeituraMemoriaFiscalSerialDataPAFECF(Pchar(dataInicial), PChar(dataFinal), PChar(FlagLeitura), PCHar(chavePublica), PChar(chavePrivada));
end;

function TBematechWrapper.LeituraMemoriaFiscalSerialReducaoPAFECF(
  ReducaoInicial, ReducaoFinal, FlagLeitura, chavePublica,
  chavePrivada: AnsiString): Integer;
begin
  result := Bematech_FI_LeituraMemoriaFiscalSerialReducaoPAFECF(Pchar(ReducaoInicial),
    PChar(ReducaoFinal), PChar(FlagLeitura), PCHar(chavePublica), PChar(chavePrivada));
end;

function TBematechWrapper.ArquivoMFD(ArquivoOrigem, DataOuCOOInicial, DataOuCOOFinal,
  TipoDownload, Usuario: AnsiString; TipoGeracao: integer; ChavePublica, ChavePrivada: AnsiString;
  UnicoArquivo: integer): integer;
begin
  result := Bematech_FI_ArquivoMFD(PChar(ArquivoOrigem), PChar(DataOuCOOInicial),
    PChar(DataOuCOOFinal), PChar(TipoDownload), PChar(Usuario), TipoGeracao,
    PChar(ChavePublica), PChar(ChavePrivada), UnicoArquivo);
end;

function TBematechWrapper.EspelhoMFD(NomeArquivo, DataOuCOOInicial, DataOuCOOFinal,
  TipoDownload, Usuario, ChavePublica, ChavePrivada: AnsiString): integer;
begin
  result := Bematech_FI_EspelhoMFD(NomeArquivo, dataOuCOOInicial, dataOuCOOFinal,
    TipoDownload, Usuario, ChavePublica, ChavePrivada);
end;

function TBematechWrapper.DownloadMF(Arquivo: AnsiString): Integer;
begin
  result := Bematech_FI_DownloadMF(PChar(Arquivo));
end;

function TBematechWrapper.DownloadMFD(Arquivo, TipoDownload,
  ParametroInicial, ParametroFinal, UsuarioECF: AnsiString): Integer;
begin
  result := Bematech_FI_DownloadMFD(PChar(Arquivo), PChar(TipoDownload), PChar(ParametroInicial),
    PChar(ParametroFinal), PChar(UsuarioECF));
end;

function TBematechWrapper.FormatoDadosMFD(ArquivoOrigem, ArquivoDestino,
  TipoFormato, TipoDownload, ParametroInicial, ParametroFinal,
  UsuarioECF: AnsiString): Integer;
begin
  result := Bematech_FI_FormatoDadosMFD(PChar(ArquivoOrigem), PChar(ArquivoDestino), PChar(TipoFormato),
    PChar(TipoDownload), PChar(ParametroInicial), PChar(ParametroFinal), PChar(UsuarioECF));
end;

function TBematechWrapper.CGC_IE(var CGC, IE: AnsiString): Integer;
begin
  result := Bematech_FI_CGC_IE(CGC, IE);
end;


function TBematechWrapper.LeituraXSerial: Integer;
begin
  result := Bematech_FI_LeituraXSerial;
end;

function TBematechWrapper.SubTotal(var SubTotal: AnsiString): Integer;
var
  s: AnsiString;
begin
  SetLength(s, 14);
  result := Bematech_FI_SubTotal(s);
  SubTotal := s;
end;

function TBematechWrapper.VersaoFirmware(var VersaoFirmware: AnsiString): Integer;
var
  v: AnsiString;
begin
  SetLength(v, 4);
  result := Bematech_FI_VersaoFirmware(VersaoFirmware);
  VersaoFirmware := v;
end;

function TBematechWrapper.VersaoFirmwareMFD(
  var VersaoFirmware: AnsiString): Integer;
var
  v: AnsiString;
begin
  SetLength(v, 6);
  result := Bematech_FI_VersaoFirmwareMFD(v);
  VersaoFirmware := v;
end;


function TBematechWrapper.DataHoraGravacaoUsuarioSWBasicoMFAdicional(
  var DataHoraUsuario, DataHoraSWBasico, MFAdicional: AnsiString): integer;
var
  du, ds, mf: AnsiString;
begin
  setLength(du, 20);
  setLength(ds, 20);
  setLength(mf, 2);
  result := Bematech_FI_DataHoraGravacaoUsuarioSWBasicoMFAdicional(du, ds, mf);
  dataHoraUsuario := du;
  dataHoraSWBasico := ds;
  MFAdicional := mf;
end;


function TBematechWrapper.DataHoraUltimoDocumentoMFD(var dataHora: AnsiString): integer;
var
  r: AnsiString;
begin
  setLength(r, 12);
  result := Bematech_FI_DataHoraUltimoDocumentoMFD(r);
  dataHora := r;
end;

function TBematechWrapper.ContadorRelatoriosGerenciaisMFD(var contador: AnsiString): integer;
var
  r: AnsiString;
begin
  setLength(r, 7);
  result := Bematech_FI_ContadorRelatoriosGerenciaisMFD(r);
  contador := r;
end;

function TBematechWrapper.NumeroOperacoesNaoFiscais(var numOper: AnsiString): integer;
var
  r: AnsiString;
begin
  setLength(r, 6);
  result := Bematech_FI_NumeroOperacoesNaoFiscais(r);
  numOper := r;
end;

function TBematechWrapper.ContadorComprovantesCreditoMFD(var contador: AnsiString): integer;
var
  r: AnsiString;
begin
  setLength(r, 4);
  result := Bematech_FI_ContadorComprovantesCreditoMFD(r);
  contador := r;
end;

function TBematechWrapper.ContadorCupomFiscalMFD(var contador: AnsiString): integer;
var
  r: AnsiString;
begin
  setLength(r, 6);
  result := Bematech_FI_ContadorCupomFiscalMFD(r);
  contador := r;
end;

function TBematechWrapper.DataMovimento(
  var DataMovimento: AnsiString): Integer;
begin
  Result := Bematech_FI_DataMovimento(DataMovimento);
end;

function TBematechWrapper.AcionaGaveta: integer;
begin
  Result := Bematech_FI_AcionaGaveta;
end;

function TBematechWrapper.RetornoImpressoraMFD(var Ack, St1, St2,
  St3: Integer): Integer;
begin
  Result := Bematech_FI_RetornoImpressoraMFD(Ack, St1, St2, St3);
end;

function TBematechWrapper.habilitaDesabilitaRetornoEstendidoMFD(
  flag: AnsiString): integer;
begin
  result := Bematech_FI_HabilitaDesabilitaRetornoEstendidoMFD(pchar(flag));
end;

end.
