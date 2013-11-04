unit BematechPrinterUnit;

interface

uses
  SysUtils, BematechIntfUnit, PDVIntfUnit, PDVPrinterIntfUnit, Dialogs;

type
  TModoReducaoZ = (tmrzManual, tmrzAutomatica);
  TInfoReducaoZRaw = record
    modo: AnsiString;
    contReinicioOperacao: AnsiString;
    contReducaoZ: AnsiString;
    contOrdemOperacao: AnsiString;
    contGeralOperacoesNaoFiscais: AnsiString;
    contCupomFiscal: AnsiString;
    contGeralRelatoriosGerenciais: AnsiString;
    contFitaDetalheEmitida: AnsiString;
    contOperacoesNaoFiscaisCanceladas: AnsiString;
    contCupomFiscalCancelados: AnsiString;
    contOperacaoesNaoFiscais: AnsiString;
    contEspecificosRelatoriosGerenciais: AnsiString;
    contComprovanteDebitoCredito: AnsiString;
    contComprovanteDebitoCreditoNaoEmitido: AnsiString;
    contComprovanteDebitoCreditoCancelados: AnsiString;
    totalizadorGeral: AnsiString;
    totalizadoresParciaisTributados: AnsiString;
    totalizadorIsencaoICMS: AnsiString;
    totalizadoresNaoIncidenciaICMS: AnsiString;
    totalizadorSubstituicaoTributariaICMS: AnsiString;
    totalizadorIsencaoISSQN: AnsiString;
    totalizadorNaoIncidenciaISSQN: AnsiString;
    totalizadorSubstituicaotributariaISSQN: AnsiString;
    totalizadorDescontosICMS: AnsiString;
    totalizadorDescontosISSQN: AnsiString;
    totalizadorAcrescimosICMS: AnsiString;
    totalizadorAcrescimosISSQN: AnsiString;
    totalizadorCancelamentosICMS: AnsiString;
    totalizadorCancelamentosISSQN: AnsiString;
    totalizadoresParceiaisNaoSujeitosICMS: AnsiString;
    totalizadorSangria: AnsiString;
    totalizadorSuprimento: AnsiString;
    totalizadorCancelamentosNaoFiscais: AnsiString;
    totalizadorDescontosNaoFiscais: AnsiString;
    totalizadorAcrescimosNaoFiscais: AnsiString;
    aliquotasTributarias: AnsiString;
    dataMovimento: AnsiString;
  end;

  TInfoReducaoZ = record
    modo: AnsiString;
    dataMovimento: TDateTime;
    dataHoraReducao: TDateTime;
    contReinicioOperacao: AnsiString;
    contReducaoZ: AnsiString;
    contOrdemOperacao: AnsiString;
    contGeralOperacoesNaoFiscais: AnsiString;
    contCupomFiscal: AnsiString;
    contGeralRelatoriosGerenciais: AnsiString;
    contFitaDetalheEmitida: AnsiString;
    contOperacoesNaoFiscaisCanceladas: AnsiString;
    contCupomFiscalCancelados: AnsiString;
    contOperacaoesNaoFiscais: AnsiString;
    contEspecificosRelatoriosGerenciais: AnsiString;
    contComprovanteDebitoCredito: AnsiString;
    contComprovanteDebitoCreditoNaoEmitido: AnsiString;
    contComprovanteDebitoCreditoCancelados: AnsiString;
    totalizadorGeral: AnsiString;
    totalizadoresParciaisTributados: AnsiString;
    totalizadorIsencaoICMS: AnsiString;
    totalizadoresNaoIncidenciaICMS: AnsiString;
    totalizadorSubstituicaoTributariaICMS: AnsiString;
    totalizadorIsencaoISSQN: AnsiString;
    totalizadorNaoIncidenciaISSQN: AnsiString;
    totalizadorSubstituicaotributariaISSQN: AnsiString;
    totalizadorDescontosICMS: AnsiString;
    totalizadorDescontosISSQN: AnsiString;
    totalizadorAcrescimosICMS: AnsiString;
    totalizadorAcrescimosISSQN: AnsiString;
    totalizadorCancelamentosICMS: AnsiString;
    totalizadorCancelamentosISSQN: AnsiString;
    totalizadoresParceiaisNaoSujeitosICMS: AnsiString;
    totalizadorSangria: AnsiString;
    totalizadorSuprimento: AnsiString;
    totalizadorCancelamentosNaoFiscais: AnsiString;
    totalizadorDescontosNaoFiscais: AnsiString;
    totalizadorAcrescimosNaoFiscais: AnsiString;
    aliquotasTributarias: AnsiString;
  end;

  EBematechPrinter = class(Exception)
  end;

  TBematechPrinter = class(TInterfacedObject, IPDVPrinter)
  private
    FBematech: IBematech;
    function GetMessageFromRetVal(RetVal: Integer): AnsiString;
    function GetMessageFromAckByte(Ack: Integer): AnsiString;
    function GetMessageFromStatusBytes(St1, St2, st3: Integer): AnsiString;
  public
    erros, alertas: AnsiString;
    constructor Create(const ABematech: IBematech);

    function mensagemUltimoErro: AnsiString;

    { IPDVPrinter }
    procedure AbrirDia;
    procedure FecharDia;
    procedure EfetuarReducaoZ(DateTime: TDateTime = 0);
    procedure EfetuarLeituraX;
    function dataUltimoMovimento: TDateTime;

    procedure ImprimirConfiguracoes;
    function VerifyDataUltimaReducaoZ(out DateTime: TDateTime): Boolean;

    function IsAtiva: Boolean;

    procedure ProgramarAliquotaICMS(Aliquota: Currency);
    function GetAliquotaList: IAliquotaList;

    procedure CheckStatus(RetVal: Integer);

    function FlagsFiscais: TFlagsFiscais;
    procedure AbrePortaSerial;

    function getNumSerie: AnsiString;


    { IPDV }
    function CriarOperacao(VendedorId, ClienteId,
        TipoOperacaoId: Integer;
        const NomeCliente, Documento, Endereco: AnsiString): IPDVTransactionState;
    function CancelarOperacao(const OperacaoPDV: IOperacaoPDV): IPDVTransactionState;

    function IniciarFechamento(const OperacaoPDV: IOperacaoPDV; ValorDesconto,
        PorcentualDesconto: Currency; const NomeSupervisor,
        SenhaSupervisor: AnsiString): IPDVTransactionState;
    procedure EfetuarPagamento(forma: AnsiString; valor: currency);
    function TerminarFechamento(const OperacaoPDV: IOperacaoPDV; mensagem: AnsiString = ''): IPDVTransactionState;

    function InserirItem(const OperacaoPDV: IOperacaoPDV; MercadoriaId: Integer;
        const Codigo, Descricao, Unidade, tipoTributacao: AnsiString; AliquotaICMS, Quantidade, PrecoUnitario,
        Desconto: Currency): IPDVTransactionState;
    function RemoverItem(const Item: IItemPDV; const NomeSupervisor,
        SenhaSupervisor: AnsiString): IPDVTransactionState;
    function RemoverItemPeloNumero(numero: integer): IPDVTransactionState;
    function numeroUltimoCupom: integer;

    procedure LeituraMemoriaFiscalData(dtInicial, dtFinal: TDateTime);
    function DadosUltimaReducaoMFD: TInfoReducaoZRaw;
    function LeituraMemoriaFiscalDataMFD(DataInicial, DataFinal: TDateTime;
      FlagLeitura: AnsiString): Integer;
    function LeituraMemoriaFiscalReducaoMFD(ReducaoInicial, ReducaoFinal: integer;
      FlagLeitura: AnsiString): Integer;
    function LeituraMemoriaFiscalSerialDataMFD(DataInicial, DataFinal: TDateTime;
      FlagLeitura: AnsiString): Integer;
    function LeituraMemoriaFiscalSerialDataPAFECF(DataInicial, DataFinal: TDateTime;
      FlagLeitura: AnsiString): Integer;
    function LeituraMemoriaFiscalSerialReducaoMFD(ReducaoInicial, ReducaoFinal: integer;
      FlagLeitura: AnsiString): Integer;
    function LeituraMemoriaFiscalSerialReducaoPAFECF(ReducaoInicial, ReducaoFinal: integer;
      FlagLeitura: AnsiString): Integer;
    function ArquivoMFD(ArquivoOrigem, DadoInicial, DadoFinal, TipoDownload, Usuario: AnsiString;
      TipoGeracao, UnicoArquivo: integer): integer;
    function EspelhoMFD(NomeArquivo, DataOuCOOInicial,
      DataOuCOOFinal,  TipoDownload, Usuario: AnsiString): integer;
    function DownloadMF( Arquivo: AnsiString ): Integer;
    function DownloadMFD( Arquivo: AnsiString; TipoDownload: AnsiString; ParametroInicial: AnsiString; ParametroFinal: AnsiString; UsuarioECF: AnsiString ): Integer;
    function FormatoDadosMFD( ArquivoOrigem: AnsiString; ArquivoDestino: AnsiString; TipoFormato: AnsiString; TipoDownload: AnsiString; ParametroInicial: AnsiString; ParametroFinal: AnsiString; UsuarioECF: AnsiString ): Integer;
    function dataHoraImpressora: TDateTime;
    function SubTotal: double;
    procedure LeituraXSerial;
    function VersaoFirmware: AnsiString;
    function VersaoFirmwareMFD: AnsiString;
    procedure CGC_IE(var CGC, IE: AnsiString);
    function GrandeTotal: Double;
    procedure DataHoraGravacaoUsuarioSWBasicoMFAdicional(var DataHoraUsuario, DataHoraSWBasico, MFAdicional: AnsiString);
    function Sangria(Valor: Currency): Integer;
    function Suprimento(Valor: Currency; FormaPagamento: AnsiString): Integer;
    function DataHoraUltimoDocumentoMFD: TDateTime;
    function ContadorRelatoriosGerenciaisMFD: integer;
    function ContadorCupomFiscalMFD: integer;
    function NumeroOperacoesNaoFiscais: integer;
    function ContadorComprovantesCreditoMFD: integer;
    procedure acionaGaveta;
    procedure habilitaRetornoEstendido;
    procedure desabilitaRetornoEstendido;    
  end;

  TBematechAliquotaList = class(TInterfacedObject, IAliquotaList)
  private
    FCount: Integer;
    FItems: array of Currency;
    function GetStr(const SourceStr: AnsiString; var State: Integer; out Dest: AnsiString): Boolean;
  public
    constructor Create(const AliquotaListStr: AnsiString);

    { IAliquotaList }
    function Count: Integer;
    function GetItem(Index: Integer): Currency;
  end;

implementation

uses
  StrUtils, Classes, DateUtils, {DConfigGeral, DConfigSistema,}
  acBematechUtils, statusUnit, sglConsts;

const
  MsgSt1: array[0..7] of AnsiString = (
    'Número de parâmetro de CMD inválido',
    'Cupom fiscal aberto',
    'Comando inexistente',
    'Primeiro dado de CMD não foi ESC',
    'Impressora em erro',
    'Erro no relógio',
    'Pouco papel',
    'Fim do papel');

  MsgSt2: array[0..7] of AnsiString = (
    'Comando não executado',
    'CNPJ/IE do proprietário não programados',
    'Cancelamento não permitido',
    'Capacidade de alíquotas esgotada',
    'Alíquota não programada',
    'Erro na memória CMOS não volátil',
    'Memória fiscal lotada',
    'Tipo de parâmetro de CMD inválido');

{ TBematechPrinter }


procedure TBematechPrinter.AbrirDia;
var
  Valor,
  FormaPagamento: AnsiString;
begin
  Valor := '0';
  FormaPagamento := '';
  CheckStatus(FBematech.AberturaDoDia(Valor, FormaPagamento));
end;

function TBematechPrinter.CancelarOperacao(
  const OperacaoPDV: IOperacaoPDV): IPDVTransactionState;
begin
  CheckStatus(FBematech.CancelaCupom);
end;

procedure TBematechPrinter.CheckStatus(RetVal: Integer);
var
  Ack, St1, St2, st3: Integer;
  v: Integer;
begin
  erros := '';
  alertas := '';
  if RetVal <> 1 then
    erros := erros + GetMessageFromRetVal(RetVal)
  else
  begin
    Ack := 0;
    St1 := 0;
    St2 := 0;
    st3 := 0;
    v := FBematech.RetornoImpressoraMFD(Ack, St1, St2, st3);
    if v <> 1 then
      erros := erros + GetMessageFromRetVal(v);
    if Ack <> 6 then
      erros := erros + GetMessageFromAckByte(Ack);

    //se for somente o aviso de pouco papel
    if (St1 = 64) and (St2 = 0) then
      alertas := alertas + 'Pouco papel'
    else
      if not ((St1 = 0) and (St2 = 0) and (st3 = 0)) then
        erros := erros + GetMessageFromStatusBytes(St1, St2, st3);
  end;
  if erros <> '' then
    raise exception.Create(erros);
end;

constructor TBematechPrinter.Create(const ABematech: IBematech);
begin
  FBematech := ABematech;
end;

function TBematechPrinter.CriarOperacao(VendedorId, ClienteId,
  TipoOperacaoId: Integer; const NomeCliente, Documento, Endereco: AnsiString): IPDVTransactionState;
begin
  CheckStatus(FBematech.AbreCupomMFD(Documento, NomeCliente, Endereco));
end;

procedure TBematechPrinter.EfetuarPagamento(forma: AnsiString; valor: currency);
begin
  CheckStatus(
    FBematech.EfetuaFormaPagamento(forma, PChar(formatFloat(',0.00', valor))));
end;

//procedure TBematechPrinter.AbreComprovanteNaoFiscalVinculado((forma: AnsiString; valor: currency);
//begin
//  CheckStatus(FBematech.EfetuaFormaPagamento(forma, PChar(formatFloat(',0.00', valor))));
//end;

procedure TBematechPrinter.EfetuarReducaoZ(DateTime: TDateTime);
var
  StrData, StrHora: AnsiString;
  FormatSettings: TFormatSettings;
begin
  GetLocaleFormatSettings(1033, FormatSettings);
  if DateTime = 0 then
    DateTime := Now;
  StrData := FormatDateTime('dd/mm/yyyy', DateTime, FormatSettings);
  StrHora := FormatDateTime('hh:nn:ss', DateTime, FormatSettings);
  CheckStatus(FBematech.ReducaoZ(StrData, StrHora));
end;

procedure TBematechPrinter.EfetuarLeituraX;
begin
  CheckStatus(FBematech.LeituraX);
end;

procedure TBematechPrinter.FecharDia;
begin
  CheckStatus(FBematech.FechamentoDoDia);
end;

function TBematechPrinter.GetAliquotaList: IAliquotaList;
var
  s: AnsiString;
begin
  SetLength(s, 79);
  CheckStatus(FBematech.RetornoAliquotas(s));
  Result := TBematechAliquotaList.Create(s);
end;

function TBematechPrinter.GetMessageFromAckByte(Ack: Integer): AnsiString;
begin
  if Ack = 6 then
    Result := 'Função recebida corretamente'
  else
    Result := Format('Função não executada (ack = %d)', [Ack]);
end;

function TBematechPrinter.GetMessageFromRetVal(RetVal: Integer): AnsiString;
begin
  case RetVal of
    0:
      Result := 'Erro de comunicação';
    1:
      Result := 'OK';
    -2:
      Result := 'Parâmetro inválido na função';
    -3:
      Result := 'Alíquota não programada';
    -4:
      Result := 'O arquivo de inicialização BemaFI32.ini não foi encontrado';
    -5:
      Result := 'Erro ao abrir a porta de comunicação';
    -6:
      Result := 'Impressora desligada ou cabo de comunicação desconectado';
    -8:
      Result := 'Erro ao criar ou gravar no arquivo STATUS.TXT ou RETORNO.TXT';
    -27:
      Result := 'Status da impressora diferente de 6,0,0';
    -30:
      Result := 'Função não compatível com a impressora YANCO';
    else
      Result := Format('Erro %d (sem mensagem)', [RetVal]);
  end;
end;

function TBematechPrinter.GetMessageFromStatusBytes(St1,
  St2, st3: Integer): AnsiString;
  function GetMessagesFromSingleByte(St: Integer; MsgList: array of AnsiString): AnsiString;
  var
    i: Integer;
  begin
    for i := 0 to 7 do
    begin
      if (St and 1) <> 0 then
        Result := Result + MsgList[i] + '; ';
      St := St shr 1;
    end;
  end;
begin
  Result := GetMessagesFromSingleByte(St1, MsgSt1) + GetMessagesFromSingleByte(St2, MsgSt2);
  SetLength(Result, Length(Result) - 2);
  if st3 <> 0 then
  begin
    if st3 = 63 then
      result := 'ECF Bloqueado por que já foi realizada uma Redução Z.';
  end;
end;

procedure TBematechPrinter.ImprimirConfiguracoes;
begin
  CheckStatus(FBematech.ImprimeConfiguracoesImpressora);
end;

function TBematechPrinter.IniciarFechamento(
  const OperacaoPDV: IOperacaoPDV; ValorDesconto, PorcentualDesconto: Currency;
  const NomeSupervisor, SenhaSupervisor: AnsiString): IPDVTransactionState;
var
  Sinal: AnsiString;
begin
  if ValorDesconto < 0 then
  begin
    Sinal := 'A';
    ValorDesconto := - ValorDesconto;
  end
  else
    Sinal := 'D';

  CheckStatus(FBematech.IniciaFechamentoCupom(Sinal, '$', Format('%.2f', [ValorDesconto])));
end;

function TBematechPrinter.InserirItem(const OperacaoPDV: IOperacaoPDV;
  MercadoriaId: Integer; const Codigo, Descricao, Unidade, tipoTributacao: AnsiString;
  AliquotaICMS, Quantidade, PrecoUnitario,
  Desconto: Currency): IPDVTransactionState;
var
  DescImpressao, strAliquota: AnsiString;
  Acrescimo: Currency;
begin
  Acrescimo := 0;
  DescImpressao := Copy(Descricao, 1, 201);

  if Desconto < 0 then
  begin
    Acrescimo := - Desconto;
    Desconto := 0;
  end;
  
  strAliquota := 'NN';

  if tipoTributacao = 'I' then
    strAliquota := 'II';
  if tipoTributacao = 'N' then
    strAliquota := 'NN';
  if tipoTributacao = 'F' then
    strAliquota := 'FF';
  if tipoTributacao = 'T' then
    strAliquota := FormatFloat('0000', AliquotaICMS * 100);

  CheckStatus(
      FBematech.VendeItemDepartamento(Codigo, DescImpressao,
          strAliquota, Format('%.3f', [PrecoUnitario]),
          Format('%.3f', [Quantidade]), Format('%.2f', [Acrescimo]),
          Format('%.2f', [Desconto]), '00', Copy(Unidade, 1, 2)));
end;

function TBematechPrinter.IsAtiva: Boolean;
var
  i: Integer;
begin
  i := FBematech.VerificaImpressoraLigada;
  if i = -6 then
    Result := False
  else
  begin
    CheckStatus(i);
    Result := True;
  end;
end;

procedure TBematechPrinter.ProgramarAliquotaICMS(Aliquota: Currency);
var
  AliquotaStr: AnsiString;
begin
  AliquotaStr := StringReplace(Format('%5.2f', [Aliquota]), ' ', '0', [rfReplaceAll]);
  CheckStatus(FBematech.ProgramaAliquota(AliquotaStr, 0));
end;

function TBematechPrinter.RemoverItem(const Item: IItemPDV;
  const NomeSupervisor, SenhaSupervisor: AnsiString): IPDVTransactionState;
begin
  CheckStatus(FBematech.CancelaItemGenerico(IntToStr(Item.Index)));
end;

function TBematechPrinter.RemoverItemPeloNumero(numero: integer): IPDVTransactionState;
begin
  CheckStatus(FBematech.CancelaItemGenerico(IntToStr(numero)));
end;

function TBematechPrinter.TerminarFechamento(
  const OperacaoPDV: IOperacaoPDV; mensagem: AnsiString = ''): IPDVTransactionState;
begin
  CheckStatus(FBematech.TerminaFechamentoCupom(PChar(mensagem)));
end;

function TBematechPrinter.VerifyDataUltimaReducaoZ(
  out DateTime: TDateTime): Boolean;
  procedure DecodeStr(const S: AnsiString; out I1, I2, I3: Integer);
  var
    j: Integer;
    Values: array [0..2] of Integer;
  begin
    for j := 0 to 2 do
      Values[j] := StrToInt(Copy(S, j * 2 + 1, 2));

    I1 := Values[0];
    I2 := Values[1];
    I3 := Values[2];
  end;
var
  Data,
  Hora: AnsiString;
  Year, Month, Day, Hour, Minute, Second: Integer;
begin
  SetLength(Data, 6);
  SetLength(Hora, 6);
  CheckStatus(FBematech.DataHoraReducao(Data, Hora));
  Result := (Data <> '000000') and (Hora <> '000000');
  if Result then
  begin
    DecodeStr(Data, Day, Month, Year);
    DecodeStr(Hora, Hour, Minute, Second);
    DateTime := EncodeDateTime(2000 + Year, Month, Day, Hour, Minute, Second, 0);
  end
  else
    DateTime := 0;
end;

function TBematechPrinter.FlagsFiscais: TFlagsFiscais;
var
  d: integer;
begin
  d := 0;
  CheckStatus(FBematech.FlagsFiscais(d));
  result.CupomFiscalAberto := (d and 1) > 0;
  result.FechamentoFormasPagamentoIniciado := (d and 2) > 0;
  result.HorarioVeraoAtivo := (d and 4) > 0;
  result.JaFezReducaoZ := (d and 8) > 0;
  result.PermiteCancelarCupomFiscal := (d and 32) > 0;
  result.MemoriaFiscalSemEspaco := (d and 128) > 0;
end;

procedure TBematechPrinter.AbrePortaSerial;
begin
  CheckStatus(FBematech.AbrePortaSerial);
end;

function TBematechPrinter.getNumSerie: AnsiString;
var
  num: AnsiString;
begin
  SetLength(num, 20);
  checkStatus(FBematech.numeroSerie(num));
  result := num;
end;

function TBematechPrinter.numeroUltimoCupom: integer;
var
  num: AnsiString;
begin
  SetLength(num, 6);
  checkStatus(FBematech.numeroCupom(num));
  result := strToInt(num);
end;

procedure TBematechPrinter.LeituraMemoriaFiscalData(dtInicial,
  dtFinal: TDateTime);
begin
  checkStatus(FBematech.LeituraMemoriaFiscalData(FormatDateTime('ddmmyy', dtInicial),
    FormatDateTime('ddmmyy', dtFinal)));
end;

function TBematechPrinter.DadosUltimaReducaoMFD: TInfoReducaoZRaw;
const
  indices: Array[0..36] of integer =
    (2,4,4,6,6,6,6,6,4,4,120,120,4,4,4,18,224,14,14,14,14,14,14,14,14,14,14,14,14,392,14,14,14,14,14,64,6);
var
  i, posAtual: integer;
  dadosReducao: AnsiString;
  dadosReducaoArr: array[0..36] of AnsiString;
begin
  for i := 1 to 1278 do dadosReducao := dadosReducao + ' ';
  CheckStatus(FBematech.DadosUltimaReducaoMFD(DadosReducao));
  posAtual := 1;
  for i := low(indices) to high(indices) do
  begin
    dadosReducaoArr[i] := copy(dadosReducao, posAtual, indices[i]);
    posAtual := posAtual + indices[i] + 1;
  end;

  result.modo := dadosReducaoArr[0];
  result.contReinicioOperacao := dadosReducaoArr[1];
  result.contReducaoZ := dadosReducaoArr[2];
  result.contOrdemOperacao := dadosReducaoArr[3];
  result.contGeralOperacoesNaoFiscais := dadosReducaoArr[4];
  result.contCupomFiscal := dadosReducaoArr[5];
  result.contGeralRelatoriosGerenciais := dadosReducaoArr[6];
  result.contFitaDetalheEmitida := dadosReducaoArr[7];
  result.contOperacoesNaoFiscaisCanceladas := dadosReducaoArr[8];
  result.contCupomFiscalCancelados := dadosReducaoArr[9];
  result.contOperacaoesNaoFiscais := dadosReducaoArr[10];
  result.contEspecificosRelatoriosGerenciais := dadosReducaoArr[11];
  result.contComprovanteDebitoCredito := dadosReducaoArr[12];
  result.contComprovanteDebitoCreditoNaoEmitido := dadosReducaoArr[13];
  result.contComprovanteDebitoCreditoCancelados := dadosReducaoArr[14];
  result.totalizadorGeral := dadosReducaoArr[15];
  result.totalizadoresParciaisTributados := dadosReducaoArr[16];
  result.totalizadorIsencaoICMS := dadosReducaoArr[17];
  result.totalizadoresNaoIncidenciaICMS := dadosReducaoArr[18];
  result.totalizadorSubstituicaoTributariaICMS := dadosReducaoArr[19];
  result.totalizadorIsencaoISSQN := dadosReducaoArr[20];
  result.totalizadorNaoIncidenciaISSQN := dadosReducaoArr[21];
  result.totalizadorSubstituicaotributariaISSQN := dadosReducaoArr[22];
  result.totalizadorDescontosICMS := dadosReducaoArr[23];
  result.totalizadorDescontosISSQN := dadosReducaoArr[24];
  result.totalizadorAcrescimosICMS := dadosReducaoArr[25];
  result.totalizadorAcrescimosISSQN := dadosReducaoArr[26];
  result.totalizadorCancelamentosICMS := dadosReducaoArr[27];
  result.totalizadorCancelamentosISSQN := dadosReducaoArr[28];
  result.totalizadoresParceiaisNaoSujeitosICMS := dadosReducaoArr[29];
  result.totalizadorSangria := dadosReducaoArr[30];
  result.totalizadorSuprimento := dadosReducaoArr[31];
  result.totalizadorCancelamentosNaoFiscais := dadosReducaoArr[32];
  result.totalizadorDescontosNaoFiscais := dadosReducaoArr[33];
  result.totalizadorAcrescimosNaoFiscais := dadosReducaoArr[34];
  result.aliquotasTributarias := dadosReducaoArr[35];
  result.dataMovimento := dadosReducaoArr[36];
end;

function TBematechPrinter.LeituraMemoriaFiscalDataMFD(DataInicial,
  DataFinal: TDateTime; FlagLeitura: AnsiString): Integer;
begin
  CheckStatus(FBematech.LeituraMemoriaFiscalDataMFD(
    formatDateTime('ddmmyyyy', DataInicial),
    formatDateTime('ddmmyyyy', DataFinal),
    PChar(FlagLeitura)
  ));
end;

function TBematechPrinter.LeituraMemoriaFiscalSerialDataPAFECF(DataInicial,
  DataFinal: TDateTime; FlagLeitura: AnsiString): Integer;
begin
  CheckStatus(FBematech.LeituraMemoriaFiscalSerialDataPAFECF(
    formatDateTime('ddmmyyyy', DataInicial),
    formatDateTime('ddmmyyyy', DataFinal),
    PChar(FlagLeitura),
    PChar(_paf_cpb), PChar(_paf_cpv)
  ));
end;

function TBematechPrinter.LeituraMemoriaFiscalReducaoMFD(ReducaoInicial,
  ReducaoFinal: integer; FlagLeitura: AnsiString): Integer;
begin
  CheckStatus(FBematech.LeituraMemoriaFiscalReducaoMFD(
    PChar(intToStr(reducaoInicial)),
    PChar(intToStr(reducaoFinal)),
    PChar(FlagLeitura)
  ));
end;

function TBematechPrinter.LeituraMemoriaFiscalSerialReducaoPAFECF(
  ReducaoInicial, ReducaoFinal: integer; FlagLeitura: AnsiString): Integer;
begin
  CheckStatus(FBematech.LeituraMemoriaFiscalSerialReducaoPAFECF(
    intToStr(reducaoInicial),
    intToStr(reducaoFinal),
    FlagLeitura, _paf_cpv, _paf_cpb
  ));
end;

function TBematechPrinter.LeituraMemoriaFiscalSerialDataMFD(DataInicial,
  DataFinal: TDateTime; FlagLeitura: AnsiString): Integer;
begin
  CheckStatus(FBematech.LeituraMemoriaFiscalDataMFD(
    formatDateTime('ddmmyyyy', DataInicial),
    formatDateTime('ddmmyyyy', DataFinal),
    PChar(FlagLeitura)
  ));
end;

function TBematechPrinter.LeituraMemoriaFiscalSerialReducaoMFD(ReducaoInicial,
  ReducaoFinal: integer; FlagLeitura: AnsiString): Integer;
begin
  CheckStatus(FBematech.LeituraMemoriaFiscalSerialReducaoMFD(
    PChar(intToStr(reducaoInicial)),
    PChar(intToStr(reducaoFinal)),
    PChar(FlagLeitura)
  ));
end;

function TBematechPrinter.ArquivoMFD(ArquivoOrigem, DadoInicial, DadoFinal,
  TipoDownload, Usuario: AnsiString; TipoGeracao, UnicoArquivo: integer): integer;
begin
  CheckStatus(FBematech.ArquivoMFD(ArquivoOrigem, DadoInicial, DadoFinal,
    TipoDownload, Usuario, TipoGeracao, _paf_cpv, _paf_cpb,
    UnicoArquivo));
end;

function TBematechPrinter.EspelhoMFD(NomeArquivo, DataOuCOOInicial,
  DataOuCOOFinal, TipoDownload, Usuario: AnsiString): integer;
begin
  CheckStatus(FBematech.EspelhoMFD(NomeArquivo, DataOuCOOInicial, DataOuCOOFinal,
    TipoDownload, Usuario, _paf_cpv, _paf_cpb));
end;

function TBematechPrinter.DownloadMF(Arquivo: AnsiString): Integer;
begin
  CheckStatus(FBematech.DownloadMF(Arquivo));
end;

function TBematechPrinter.DownloadMFD(Arquivo, TipoDownload,
  ParametroInicial, ParametroFinal, UsuarioECF: AnsiString): Integer;
begin
  CheckStatus(Fbematech.DownloadMFD(Arquivo, TipoDownload, ParametroInicial, ParametroFinal, UsuarioECF));
end;

function TBematechPrinter.FormatoDadosMFD(ArquivoOrigem, ArquivoDestino,
  TipoFormato, TipoDownload, ParametroInicial, ParametroFinal,
  UsuarioECF: AnsiString): Integer;
begin
  CheckStatus(FBematech.FormatoDadosMFD(ArquivoOrigem, ArquivoDestino, TipoFormato,
    TipoDownload, ParametroInicial, ParametroFinal, UsuarioECF));
end;

function TBematechPrinter.dataHoraImpressora: TDateTime;
var
  d, h: AnsiString;
begin
  CheckStatus(FBematech.DataHoraImpressora(d, h));
  result := EncodeDateTime(strToInt('20' + copy(d, 5, 2)),
                 StrToInt(copy(d, 3, 2)),
                 StrToInt(copy(d, 1, 2)),
                 StrToInt(copy(h, 1, 2)),
                 StrToInt(copy(h, 3, 2)),
                 StrToInt(copy(h, 5, 2)), 0);
end;

function TBematechPrinter.GrandeTotal: Double;
var
  gt: AnsiString;
begin
  CheckStatus(FBematech.GrandeTotal(gt));
  result := StrToFloat(gt);
  result := Result / 100;
end;


procedure TBematechPrinter.CGC_IE(var CGC, IE: AnsiString);
var
  pCgc, pIE: AnsiString;
begin
  SetLength(pCgc, 18);
  setLength(pIE, 15);
  CheckStatus(FBematech.CGC_IE(pCGC, pIE));
  CGC := pCGC;
  IE := pIE;
end;


procedure TBematechPrinter.LeituraXSerial;
begin
  CheckStatus(FBematech.LeituraXSerial);
end;

function TBematechPrinter.SubTotal: double;
var
  sub: AnsiString;
begin
  CheckStatus(FBematech.SubTotal(sub));
  result := StrToFloat(sub);
  result := result / 100;
end;

function TBematechPrinter.VersaoFirmware: AnsiString;
var
  v: AnsiString;
begin
  CheckStatus(FBematech.VersaoFirmware(v));
  result := v;
end;

function TBematechPrinter.VersaoFirmwareMFD: AnsiString;
var
  v: AnsiString;
begin
  CheckStatus(FBematech.VersaoFirmwareMFD(v));
  result := v;
end;

procedure TBematechPrinter.DataHoraGravacaoUsuarioSWBasicoMFAdicional(
  var DataHoraUsuario, DataHoraSWBasico, MFAdicional: AnsiString);
begin
  checkStatus(FBematech.DataHoraGravacaoUsuarioSWBasicoMFAdicional(DataHoraUsuario, DataHoraSWBasico, MFAdicional));
end;

function TBematechPrinter.Sangria(Valor: Currency): Integer;
var
  valorStr: AnsiString;
begin
  valorStr := FormatFloat('000', Valor * 100);
  checkStatus(FBematech.Sangria(valorStr));
end;

function TBematechPrinter.Suprimento(Valor: Currency;
  FormaPagamento: AnsiString): Integer;
var
  valorStr: AnsiString;
begin
  valorStr := FormatFloat('000', Valor * 100);
  CheckStatus(FBematech.Suprimento(valorStr, FormaPagamento));
end;

function TBematechPrinter.ContadorComprovantesCreditoMFD: integer;
var
  r: AnsiString;
begin
  CheckStatus(FBematech.ContadorComprovantesCreditoMFD(r));
  result := StrToInt(r);
end;

function TBematechPrinter.ContadorRelatoriosGerenciaisMFD: integer;
var
  r: AnsiString;
begin
  CheckStatus(FBematech.ContadorRelatoriosGerenciaisMFD(r));
  result := StrToInt(r);
end;

function TBematechPrinter.ContadorCupomFiscalMFD: integer;
var
  r: AnsiString;
begin
  CheckStatus(FBematech.ContadorCupomFiscalMFD(r));
  result := StrToInt(r);
end;

function TBematechPrinter.DataHoraUltimoDocumentoMFD: TDateTime;
var
  r: AnsiString;
  a, m, d, h, n, s: word;
begin
  CheckStatus(FBematech.DataHoraUltimoDocumentoMFD(r));
  a := StrToInt('20' + copy(r, 5, 2));
  m := StrToInt(copy(r, 3, 2));
  d := StrToInt(copy(r, 1, 2));
  h := StrToInt(copy(r, 7, 2));
  n := StrToInt(copy(r, 9, 2));
  s := StrToInt(copy(r, 11, 2));
  result := EncodeDateTime(a, m, d, h, n, s, 0);
end;

function TBematechPrinter.NumeroOperacoesNaoFiscais: integer;
var
  r: AnsiString;
begin
  CheckStatus(FBematech.NumeroOperacoesNaoFiscais(r));
  result := StrToInt(r);
end;

function TBematechPrinter.dataUltimoMovimento: TDateTime;
var
  r: AnsiString;
  a, m, d: word;
begin
  CheckStatus(FBematech.DataHoraUltimoDocumentoMFD(r));
  a := StrToInt('20' + copy(r, 5, 2));
  m := StrToInt(copy(r, 3, 2));
  d := StrToInt(copy(r, 1, 2));
  result := EncodeDate(a, m, d);
end;

procedure TBematechPrinter.acionaGaveta;
begin
  CheckStatus(FBematech.AcionaGaveta);
end;

procedure TBematechPrinter.desabilitaRetornoEstendido;
var
  flag: AnsiString;
begin
  flag := '0';
  if FBematech.habilitaDesabilitaRetornoEstendidoMFD(flag) <> 1 then
    MessageDlg('Não foi possível habilitar o retorno estendido da impressora fiscal.'+#13+#10+'Contate o suporte técnico.', mtError, [mbOK], 0);
end;

procedure TBematechPrinter.habilitaRetornoEstendido;
var
  flag: AnsiString;
begin
  flag := '1';
  if FBematech.habilitaDesabilitaRetornoEstendidoMFD(flag) <> 1 then
    MessageDlg('Não foi possível habilitar o retorno estendido da impressora fiscal.'+#13+#10+'Contate o suporte técnico.', mtError, [mbOK], 0);
end;

function TBematechPrinter.mensagemUltimoErro: AnsiString;
begin
  
end;

{ TBematechAliquotaList }

function TBematechAliquotaList.Count: Integer;
begin
  Result := FCount;
end;

constructor TBematechAliquotaList.Create(const AliquotaListStr: AnsiString);
var
  i: Integer;
  State: Integer;
  s: AnsiString;
  Value: Integer;
  LastNonZero: Integer;
begin
  SetLength(FItems, 16);

  State := 1;
  LastNonZero := -1;
  for i := Low(FItems) to High(FItems) do
  begin
    if not GetStr(AliquotaListStr, State, s) then
      break;

    if TryStrToInt(s, Value) then
    begin
      FItems[i] := Value / 100.0;
      if FItems[i] <> 0 then
        LastNonZero := i;
    end;
  end;
  FCount := LastNonZero + 1;
end;

function TBematechAliquotaList.GetItem(Index: Integer): Currency;
begin
  if (Index < 0) or (Index >= FCount) then
    raise EListError.CreateFmt('List index out of bounds (%d)', [Index]);
  Result := FItems[Index];
end;

function TBematechAliquotaList.GetStr(const SourceStr: AnsiString;
  var State: Integer; out Dest: AnsiString): Boolean;
var
  p, i, Count: Integer;
begin
  if (State < 1) or (State > Length(SourceStr)) then
  begin
    Result := False;
    Exit;
  end;

  i := State;
  p := PosEx(',', SourceStr, State);
  if p = 0 then
  begin
    Count := Length(SourceStr) + 1 - i;
    State := 0;
  end
  else
  begin
    Count := p - i;
    State := p + 1;
  end;

  Dest := Copy(SourceStr, i, Count);
  Result := True;
end;

end.
