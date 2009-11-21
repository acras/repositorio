unit BematechPrinterUnit;

interface

uses
  SysUtils, BematechIntfUnit, PDVIntfUnit, PDVPrinterIntfUnit;

type
  EBematechPrinter = class(Exception)
  end;

  TBematechPrinter = class(TInterfacedObject, IPDV, IPDVPrinter)
  private
    FBematech: IBematech;
    function GetMessageFromRetVal(RetVal: Integer): string;
    function GetMessageFromAckByte(Ack: Integer): string;
    function GetMessageFromStatusBytes(St1, St2: Integer): string;
  public
    constructor Create(const ABematech: IBematech);

    { IPDVPrinter }
    procedure AbrirDia;
    procedure FecharDia;
    procedure EfetuarReducaoZ(DateTime: TDateTime = 0);
    procedure EfetuarLeituraX;

    procedure ImprimirConfiguracoes;
    function VerifyDataUltimaReducaoZ(out DateTime: TDateTime): Boolean;

    function IsAtiva: Boolean;

    procedure ProgramarAliquotaICMS(Aliquota: Currency);
    function GetAliquotaList: IAliquotaList;

    procedure CheckStatus(RetVal: Integer);

    function FlagsFiscais: TFlagsFiscais;
    procedure AbrePortaSerial;

    function getNumSerie: string;


    { IPDV }
    function CriarOperacao(VendedorId, ClienteId,
        TipoOperacaoId: Integer; const NomeCliente, Documento: string): IPDVTransactionState;
    function CancelarOperacao(const OperacaoPDV: IOperacaoPDV): IPDVTransactionState;

    function IniciarFechamento(const OperacaoPDV: IOperacaoPDV; ValorDesconto,
        PorcentualDesconto: Currency; const NomeSupervisor,
        SenhaSupervisor: string): IPDVTransactionState;
    function EfetuarPagamento(const OperacaoPDV: IOperacaoPDV;
        const Pagamento: IPagamentoPDV): IPDVTransactionState;
    function TerminarFechamento(const OperacaoPDV: IOperacaoPDV): IPDVTransactionState;

    function InserirItem(const OperacaoPDV: IOperacaoPDV; MercadoriaId: Integer;
        const Codigo, Descricao, Unidade: string; AliquotaICMS, Quantidade, PrecoUnitario,
        Desconto: Currency): IPDVTransactionState;
    function RemoverItem(const Item: IItemPDV; const NomeSupervisor,
        SenhaSupervisor: string): IPDVTransactionState;
    function RemoverItemPeloNumero(numero: integer): IPDVTransactionState;
    function numeroUltimoCupom: integer;
  end;

  TBematechAliquotaList = class(TInterfacedObject, IAliquotaList)
  private
    FCount: Integer;
    FItems: array of Currency;
    function GetStr(const SourceStr: string; var State: Integer; out Dest: string): Boolean;
  public
    constructor Create(const AliquotaListStr: string);

    { IAliquotaList }
    function Count: Integer;
    function GetItem(Index: Integer): Currency;
  end;

implementation

uses
  StrUtils, Classes, DateUtils, DConfigGeral, DConfigSistema,
  BematechUtils;

const
  MsgSt1: array[0..7] of string = (
    'Número de parâmetro de CMD inválido',
    'Cupom fiscal aberto',
    'Comando inexistente',
    'Primeiro dado de CMD não foi ESC',
    'Impressora em erro',
    'Erro no relógio',
    'Pouco papel',
    'Fim do papel');

  MsgSt2: array[0..7] of string = (
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
  FormaPagamento: string;
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
  Ack, St1, St2: Integer;
  v: Integer;
begin
  if RetVal <> 1 then
    raise EBematechPrinter.Create(GetMessageFromRetVal(RetVal));

  Ack := 0;
  St1 := 0;
  St2 := 0;
  v := FBematech.RetornoImpressora(Ack, St1, St2);
  if v <> 1 then
    raise EBematechPrinter.Create(GetMessageFromRetVal(v));
  if Ack <> 6 then
    raise EBematechPrinter.Create(GetMessageFromAckByte(Ack));

  if not ((St1 = 0) and (St2 = 0)) then
    raise EBematechPrinter.Create(GetMessageFromStatusBytes(St1, St2));
end;

constructor TBematechPrinter.Create(const ABematech: IBematech);
begin
  FBematech := ABematech;
end;

function TBematechPrinter.CriarOperacao(VendedorId, ClienteId,
  TipoOperacaoId: Integer; const NomeCliente, Documento: string): IPDVTransactionState;
begin
  CheckStatus(FBematech.AbreCupom(Documento));
end;

function TBematechPrinter.EfetuarPagamento(const OperacaoPDV: IOperacaoPDV;
  const Pagamento: IPagamentoPDV): IPDVTransactionState;
begin
  CheckStatus(Pagamento.EfetuarPagamentoPrinter(OperacaoPDV, FBematech));
end;

procedure TBematechPrinter.EfetuarReducaoZ(DateTime: TDateTime);
var
  StrData, StrHora: string;
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
  s: string;
begin
  SetLength(s, 79);
  CheckStatus(FBematech.RetornoAliquotas(s));
  Result := TBematechAliquotaList.Create(s);
end;

function TBematechPrinter.GetMessageFromAckByte(Ack: Integer): string;
begin
  if Ack = 6 then
    Result := 'Função recebida corretamente'
  else
    Result := Format('Função não executada (ack = %d)', [Ack]);
end;

function TBematechPrinter.GetMessageFromRetVal(RetVal: Integer): string;
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
  St2: Integer): string;
  function GetMessagesFromSingleByte(St: Integer; MsgList: array of string): string;
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
end;

procedure TBematechPrinter.ImprimirConfiguracoes;
begin
  CheckStatus(FBematech.ImprimeConfiguracoesImpressora);
end;

function TBematechPrinter.IniciarFechamento(
  const OperacaoPDV: IOperacaoPDV; ValorDesconto, PorcentualDesconto: Currency;
  const NomeSupervisor, SenhaSupervisor: string): IPDVTransactionState;
var
  Sinal: string;
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
  MercadoriaId: Integer; const Codigo, Descricao, Unidade: string;
  AliquotaICMS, Quantidade, PrecoUnitario,
  Desconto: Currency): IPDVTransactionState;
var
  DescImpressao: string;
  Acrescimo: Currency;
begin
  Acrescimo := 0;
  DescImpressao := Copy(Descricao, 1, 201);

  if Desconto < 0 then
  begin
    Acrescimo := - Desconto;
    Desconto := 0;
  end;

  CheckStatus(
      FBematech.VendeItemDepartamento(Codigo, DescImpressao,
          'NN', Format('%.3f', [PrecoUnitario]),
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
  AliquotaStr: string;
begin
  AliquotaStr := StringReplace(Format('%5.2f', [Aliquota]), ' ', '0', [rfReplaceAll]);
  CheckStatus(FBematech.ProgramaAliquota(AliquotaStr, 0));
end;

function TBematechPrinter.RemoverItem(const Item: IItemPDV;
  const NomeSupervisor, SenhaSupervisor: string): IPDVTransactionState;
begin
  CheckStatus(FBematech.CancelaItemGenerico(IntToStr(Item.Index)));
end;

function TBematechPrinter.RemoverItemPeloNumero(numero: integer): IPDVTransactionState;
begin
  CheckStatus(FBematech.CancelaItemGenerico(IntToStr(numero)));
end;

function TBematechPrinter.TerminarFechamento(
  const OperacaoPDV: IOperacaoPDV): IPDVTransactionState;
begin
  CheckStatus(FBematech.TerminaFechamentoCupom(''));
end;

function TBematechPrinter.VerifyDataUltimaReducaoZ(
  out DateTime: TDateTime): Boolean;
  procedure DecodeStr(const S: string; out I1, I2, I3: Integer);
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
  Hora: string;
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

function TBematechPrinter.getNumSerie: string;
var
  num: string;
begin
  SetLength(num, 20);
  checkStatus(FBematech.numeroSerie(num));
  result := num;
end;

function TBematechPrinter.numeroUltimoCupom: integer;
var
  num: string;
begin
  SetLength(num, 6);
  checkStatus(FBematech.numeroCupom(num));
  result := strToInt(num);
end;

{ TBematechAliquotaList }

function TBematechAliquotaList.Count: Integer;
begin
  Result := FCount;
end;

constructor TBematechAliquotaList.Create(const AliquotaListStr: string);
var
  i: Integer;
  State: Integer;
  s: string;
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

function TBematechAliquotaList.GetStr(const SourceStr: string;
  var State: Integer; out Dest: string): Boolean;
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
