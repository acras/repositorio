unit BematechWrapperUnit;

interface

uses
  BematechIntfUnit, SysUtils, dialogs;

type
  TBematechWrapper = class(TInterfacedObject, IBematech)
  public
    function ProgramaAliquota(var Aliquota: string; Vinculo: Integer): Integer;

    function AbreCupom(CNPJCPF: string): Integer;
    function AbreCupomMFD(CGC: string; Nome: string; Endereco : string): Integer;
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
    function DataHoraImpressora(var Data, Hora: string): Integer;
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
    function ArquivoMFD(ArquivoOrigem, DataOuCOOInicial, DataOuCOOFinal, TipoDownload,
      Usuario: string; TipoGeracao: integer; ChavePublica, ChavePrivada: string; UnicoArquivo: integer): integer;
    function EspelhoMFD(NomeArquivo, DataOuCOOInicial, DataOuCOOFinal,
      TipoDownload, Usuario, ChavePublica, ChavePrivada: string): integer;
    function DownloadMF( Arquivo: String ): Integer;
    function DownloadMFD( Arquivo: String; TipoDownload: String; ParametroInicial: String; ParametroFinal: String; UsuarioECF: String ): Integer;
    function FormatoDadosMFD( ArquivoOrigem: String; ArquivoDestino: String; TipoFormato: String; TipoDownload: String; ParametroInicial: String; ParametroFinal: String; UsuarioECF: String ): Integer;
    function SubTotal(var SubTotal: String): Integer;
    function LeituraXSerial: Integer;
    function VersaoFirmware(var VersaoFirmware: String): Integer;
    function VersaoFirmwareMFD(var VersaoFirmware: String): Integer;
    function CGC_IE(var CGC: String; var IE: String): Integer;
    function GrandeTotal(var GrandeTotal: String): Integer;
    function DataHoraGravacaoUsuarioSWBasicoMFAdicional(var DataHoraUsuario, DataHoraSWBasico, MFAdicional: string): integer;
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

function TBematechWrapper.AbreCupomMFD(CGC, Nome,
  Endereco: string): Integer;
begin
  Result := Bematech_FI_AbreCupomMFD(CGC, Nome, Endereco);
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

function TBematechWrapper.DataHoraImpressora(var Data, Hora: string): Integer;
var
  d, h: string;
begin
  SetLength(d, 6);
  SetLength(h, 6);
  Result := Bematech_FI_DataHoraImpressora(d, h);
  Data := d;
  Hora := h;
end;

function TBematechWrapper.GrandeTotal(var GrandeTotal: String): Integer;
var
  s: String;
begin
  SetLength(s, 18);
  result := Bematech_FI_GrandeTotal(s);
  GrandeTotal := s;
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
  result := Bematech_FI_LeituraMemoriaFiscalSerialReducaoPAFECF(Pchar(ReducaoInicial),
    PChar(ReducaoFinal), PChar(FlagLeitura), PCHar(chavePublica), PChar(chavePrivada));
end;

function TBematechWrapper.ArquivoMFD(ArquivoOrigem, DataOuCOOInicial, DataOuCOOFinal,
  TipoDownload, Usuario: string; TipoGeracao: integer; ChavePublica, ChavePrivada: string;
  UnicoArquivo: integer): integer;
begin
  result := Bematech_FI_ArquivoMFD(PChar(ArquivoOrigem), PChar(DataOuCOOInicial),
    PChar(DataOuCOOFinal), PChar(TipoDownload), PChar(Usuario), TipoGeracao,
    PChar(ChavePublica), PChar(ChavePrivada), UnicoArquivo);
end;

function TBematechWrapper.EspelhoMFD(NomeArquivo, DataOuCOOInicial, DataOuCOOFinal,
  TipoDownload, Usuario, ChavePublica, ChavePrivada: string): integer;
begin
  result := Bematech_FI_EspelhoMFD(PChar(NomeArquivo), PChar(dataOuCOOInicial), PChar(dataOuCOOFinal),
    PChar(TipoDownload), PChar(Usuario), PChar(ChavePublica), PChar(ChavePrivada));
end;

function TBematechWrapper.DownloadMF(Arquivo: String): Integer;
begin
  result := Bematech_FI_DownloadMF(PChar(Arquivo));
end;

function TBematechWrapper.DownloadMFD(Arquivo, TipoDownload,
  ParametroInicial, ParametroFinal, UsuarioECF: String): Integer;
begin
  result := Bematech_FI_DownloadMFD(PChar(Arquivo), PChar(TipoDownload), PChar(ParametroInicial),
    PChar(ParametroFinal), PChar(UsuarioECF));
end;

function TBematechWrapper.FormatoDadosMFD(ArquivoOrigem, ArquivoDestino,
  TipoFormato, TipoDownload, ParametroInicial, ParametroFinal,
  UsuarioECF: String): Integer;
begin
  result := Bematech_FI_FormatoDadosMFD(PChar(ArquivoOrigem), PChar(ArquivoDestino), PChar(TipoFormato),
    PChar(TipoDownload), PChar(ParametroInicial), PChar(ParametroFinal), PChar(UsuarioECF));
end;

function TBematechWrapper.CGC_IE(var CGC, IE: String): Integer;
begin
  result := Bematech_FI_CGC_IE(CGC, IE);
end;


function TBematechWrapper.LeituraXSerial: Integer;
begin
  result := Bematech_FI_LeituraXSerial;
end;

function TBematechWrapper.SubTotal(var SubTotal: String): Integer;
var
  s: string;
begin
  SetLength(s, 14);
  result := Bematech_FI_SubTotal(s);
  SubTotal := s;
end;

function TBematechWrapper.VersaoFirmware(var VersaoFirmware: String): Integer;
var
  v: string;
begin
  SetLength(v, 4);
  result := Bematech_FI_VersaoFirmware(VersaoFirmware);
  VersaoFirmware := v;
end;

function TBematechWrapper.VersaoFirmwareMFD(
  var VersaoFirmware: String): Integer;
var
  v: string;
begin
  SetLength(v, 6);
  result := Bematech_FI_VersaoFirmwareMFD(v);
  VersaoFirmware := v;
end;


function TBematechWrapper.DataHoraGravacaoUsuarioSWBasicoMFAdicional(
  var DataHoraUsuario, DataHoraSWBasico, MFAdicional: string): integer;
var
  du, ds, mf: string;
begin
  setLength(du, 20);
  setLength(ds, 20);
  setLength(mf, 2);
  result := Bematech_FI_DataHoraGravacaoUsuarioSWBasicoMFAdicional(du, ds, mf);
  dataHoraUsuario := du;
  dataHoraSWBasico := ds;
  MFAdicional := mf;
end;

end.
