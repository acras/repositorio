{* Unit CNAB400.pas
 *
 * Criação: Ricardo Nastás Acras (ricardo@acras.net)
 * Data: 17/10/2005
 *
 * Objetivo: Tratar arquivos CNAB 400 posições.
}
unit CNAB400;

interface

uses
  Dialogs, classes, SysUtils, DBClient, DB, DateUtils;
type
  TTipoArquivo = (taRemessa, taRetorno);

  TCNAB400 = class
  private
    FClientDataSetTitulos: TClientDataSet;
    FRazaoSocial: string;
    FCodigoEmpresa: integer;
    FSequencialArquivo: integer;
    FAgenciaEmpresa: integer;
    FContaEmpresa: integer;
    FTipoArquivo: TTipoArquivo;
    FdataGeracaoArquivo: TDateTime;
    FSeqRegistro: integer;
    FValorBoleto: currency;
    FDataHoraGeracao: TDateTime;
    FClientDataSetRetorno: TClientDataSet;
    function getCodigoEmpresa: String;
    function getRazaoSocial: String;
    function getCodigoBanco: String;
    function getNomebanco: string;
    function getSequencialArquivo: string;
    function getStrDataGeracao: string;
    function getItentificacaoEmpresa: string;
    function getTextoSequencialBoleto: string;
    function getNumDocumentoBoleto: string;
    function getTextoDatavencimentoBoleto: string;
    function getTextoValorBoleto: string;
    function getInstrucaoProtesto: string;
    function getStrCPFCNPJ: string;
    function getTextoEnderecoCompleto: string;
    function getTextoNomeSacado: string;
    function getTextoPrimeiraMensagem: string;
    function getTextoSegundaMensagem: string;
    function getNumSeqRegistro: string;
    function getTrailer: string;
    function getUltimoSequencial: string;
    function getLinhaBoleto: string;
    function getLinhaMensagem: string;
    function getTextoValorMultaDia: string;
    procedure criarDataSetRetorno;
  protected
    function getHeader: String;
  public
    procedure abrirArquivo(sNomeArquivo: string);
    procedure adicionarBoleto(sequencial: integer;
                              numDocumento: string;
                              datavencimento: TDateTime;
                              valor: Currency;
                              valorDiaAtraso: currency;
                              CPFCNPJ: string;
                              nomeSacado: string;
                              enderecoCompleto: string;
                              CEP: string;
                              mensagem1: string;
                              mensagem2: string;
                              mensagem3: string;
                              mensagem4: string
                              );
    property RazaoSocial: string read FRazaoSocial write FRazaoSocial;
    property CodigoEmpresa: integer read FCodigoEmpresa write FCodigoEmpresa;
    property SequencialArquivo: integer read FSequencialArquivo
      write FSequencialArquivo;
    property tipoArquivo: TTipoArquivo read FTipoArquivo;
    property dataGeracaoArquivo: TDateTime read FdataGeracaoArquivo write FdataGeracaoArquivo;
    property dataSet: TClientDataSet read FClientDataSetTitulos;
    property dataSetRetorno: TClientDataSet read FClientDataSetRetorno;

    //as propriedades carteira, agencia e conta são geradas para cada boleto
    //mas como nos casos detectados era sempre o mesmo ficaram como dados
    //da classe, se preisar diferente, mude.
    //carteira sempre é 009 (até agora) então ficou como const
    property AgenciaEmpresa: integer read FAgenciaEmpresa write FAgenciaEmpresa;
    property ContaEmpresa: integer read FContaEmpresa write FContaEmpresa;

    property ValorBoleto: currency read FValorBoleto write FValorBoleto;

    property dataHoraGeracao: TDateTime read FDataHoraGeracao write FDataHoraGeracao;

    destructor Destroy; override;
    function SalvarArquivo(nomeArquivo: string): boolean;


    procedure testar;
  public
    constructor Create;
  end;

const
  FNomeBanco = 'Bradesco';
  FCodigoBanco = '237';
  FCarteira = '009';

  //constantes para os tipos de operação
  toEntradaConfirmada = 2;
  toLiquidacaoNormal  = 6;
  toEntradaRejeitada  = 3;  

implementation




uses TypInfo;





{ TCNAB400 }

constructor TCNAB400.Create;
begin
  FValorBoleto := 0;
  FTipoArquivo := taRemessa;
  FSeqRegistro := 1;
  FClientDataSetTitulos := TClientDataSet.Create(nil);

  with TIntegerField.Create(FClientDataSetTitulos) do
  begin
    FieldName := 'Sequencial';
    DataSet   := FClientDataSetTitulos;
  end;

  with TDateField.Create(FClientDataSetTitulos) do
  begin
    FieldName := 'DataVencimento';
    DataSet   := FClientDataSetTitulos;
  end;

  with TStringField.Create(FClientDataSetTitulos) do
  begin
    FieldName := 'NumDocumento';
    DataSet   := FClientDataSetTitulos;
  end;

  with TFloatField.Create(FClientDataSetTitulos) do
  begin
    FieldName := 'Valor';
    currency := True;
    DataSet   := FClientDataSetTitulos;
  end;

  with TStringField.Create(FClientDataSetTitulos) do
  begin
    FieldName := 'CPFCNPJ';
    DataSet   := FClientDataSetTitulos;
  end;

  with TStringField.Create(FClientDataSetTitulos) do
  begin
    FieldName := 'NomeSacado';
    DataSet   := FClientDataSetTitulos;
  end;

  with TStringField.Create(FClientDataSetTitulos) do
  begin
    FieldName := 'EnderecoCompleto';
    DataSet   := FClientDataSetTitulos;
  end;

  with TStringField.Create(FClientDataSetTitulos) do
  begin
    FieldName := 'CEP';
    DataSet   := FClientDataSetTitulos;
  end;

  with TStringField.Create(FClientDataSetTitulos) do
  begin
    FieldName := 'Mensagem1';
    Size := 80;
    DataSet   := FClientDataSetTitulos;
  end;

  with TStringField.Create(FClientDataSetTitulos) do
  begin
    FieldName := 'Mensagem2';
    Size := 80;
    DataSet   := FClientDataSetTitulos;
  end;

  with TStringField.Create(FClientDataSetTitulos) do
  begin
    FieldName := 'Mensagem3';
    Size := 80;
    DataSet   := FClientDataSetTitulos;
  end;

  with TStringField.Create(FClientDataSetTitulos) do
  begin
    FieldName := 'Mensagem4';
    Size := 80;
    DataSet   := FClientDataSetTitulos;
  end;


  with TCurrencyField.Create(FClientDataSetTitulos) do
  begin
    FieldName := 'ValorDiaAtraso';
    DataSet := FClientDataSetTitulos;
  end;

  FClientDataSetTitulos.CreateDataSet;
end;

destructor TCNAB400.Destroy;
begin
  FClientDataSetTitulos.Free;
  if FClientDataSetRetorno<>nil then
    FreeAndNil(FClientDataSetRetorno);
end;

procedure TCNAB400.criarDataSetRetorno;
begin
  FClientDataSetRetorno := TClientDataSet.Create(nil);

  with TIntegerField.Create(FClientDataSetRetorno) do
  begin
    FieldName := 'Sequencial';
    DataSet   := FClientDataSetRetorno;
  end;

  with TIntegerField.Create(FClientDataSetRetorno) do
  begin
    FieldName := 'TipoOcorrencia';
    DataSet   := FClientDataSetRetorno;
  end;

  with TCurrencyField.Create(FClientDataSetRetorno) do
  begin
    FieldName := 'ValorBoleto';
    DataSet   := FClientDataSetRetorno;
  end;

  with TCurrencyField.Create(FClientDataSetRetorno) do
  begin
    FieldName := 'ValorPago';
    DataSet   := FClientDataSetRetorno;
  end;

  with TStringField.Create(FClientDataSetRetorno) do
  begin
    FieldName := 'MotivoRecusa';
    DataSet   := FClientDataSetRetorno;
  end;

  with TDateTimeField.Create(FClientDataSetRetorno) do
  begin
    FieldName := 'DataPagamento';
    DataSet   := FClientDataSetRetorno;
  end;

  FClientDataSetRetorno.CreateDataSet;
end;

procedure TCNAB400.abrirArquivo(sNomeArquivo: string);
var
  conteudo: TStringList;
  sHeader, linhaAtual: string;
  iLinhaAtual: integer;
begin
  conteudo := TStringList.Create;
  try
    conteudo.LoadFromFile(sNomeArquivo);

    //Interpretar a primeira linha do arquivo
    //A primeira linha do arquivo é o Header de Arquivo
    //Adicionados apenas campos com uso prático detectado, caso necessite
    //mais campos basta consultar a documentação do CNAB40 e adiciona-los
    sHeader := conteudo[0];

    if copy(sHeader, 2,1) = '1' then
      FTipoArquivo := taRemessa
    else
    begin
      FTipoArquivo := taRetorno;
      criarDataSetRetorno;
    end;

    FDataHoraGeracao := EncodeDateTime(
      2000+StrToInt(copy(sHeader,99,2)),
      StrToInt(copy(sHeader,97,2)),
      StrToInt(copy(sHeader,95,2)),
      0,
      0,
      0,
      0
      );

    //pulando o header do lote por que não foi encontrado uso prático para
    //o arquivo de retorno, caso encontre pode adicionar aqui o código de
    //interpretação desta linha

    //interpretar as linhas do boleto, cada linha é um boleto
    iLinhaAtual := 1;
    while iLinhaAtual < conteudo.Count do
    begin
      linhaAtual := conteudo[iLinhaAtual];
      if copy(linhaAtual,1,1) = '1' then
      begin
        FClientDataSetRetorno.Append;
        FClientDataSetRetorno.FieldByName('Sequencial').AsInteger :=
          strToInt(copy(linhaAtual,38,25));
        FClientDataSetRetorno.FieldByName('TipoOcorrencia').AsInteger :=
          strToInt(copy(linhaAtual,109,2));
        FClientDataSetRetorno.FieldByName('ValorBoleto').AsCurrency :=
          StrToFloat(copy(linhaAtual, 176, 11)+','+copy(linhaAtual, 187, 2));
        FClientDataSetRetorno.FieldByName('ValorPago').AsCurrency :=
          StrToFloat(copy(linhaAtual, 254, 11)+','+copy(linhaAtual, 265, 2));
        FClientDataSetRetorno.FieldByName('MotivoRecusa').AsInteger :=
          strToInt(copy(linhaAtual,319,10));
        FClientDataSetRetorno.FieldByName('DataPagamento').AsDateTime :=
          encodedate(
            strToInt(copy(linhaAtual, 111, 2)),
            strToInt(copy(linhaAtual, 113, 2)),
            strToInt(copy(linhaAtual, 115, 2))
          );
          strToInt(copy(linhaAtual,319,10));
        FClientDataSetRetorno.Post;
      end;
      inc(iLinhaAtual);
    end;
  finally
    FreeAndNil(conteudo);
  end;
end;


function TCNAB400.getCodigoEmpresa: String;
begin
  result := IntToStr(FCodigoEmpresa);
  result := stringOfChar('0', 20-length(result)) + result;
end;

function TCNAB400.getCodigoBanco: String;
begin
  result := FCodigobanco;
end;

function TCNAB400.getRazaoSocial: String;
begin
  result := FRazaoSocial + stringOfChar(' ', 30-length(FRazaoSocial));
end;

function TCNAB400.getHeader: String;
begin
  result := '01REMESSA01COBRANCA       '; //inicial fixo do header
  result := result + getCodigoEmpresa + getRazaoSocial + getCodigoBanco +
    getNomebanco + getStrDataGeracao;
  result := Result + StringOfChar(' ', 8); //8 brancos
  result := Result + 'MX'; //quando é micro a micro deve ser MX, quando não é
                           //será desconsiderado. Por isso fica sempre MX
  Result := Result + getSequencialArquivo;
  result := Result + StringOfChar(' ', 277); //277 brancos
  result := Result + getNumSeqRegistro;
end;

function TCNAB400.SalvarArquivo(nomeArquivo: string): boolean;
var
  conteudoArquivo: TStringList;
begin
  result := false;
  FSeqRegistro := 1;
  with TOpenDialog.Create(nil) do
  begin
    FileName := nomeArquivo;
    if Execute then
    begin
      result := true;
      conteudoArquivo := TStringList.Create;
      try
        conteudoArquivo.Add(getHeader);
        FClientDataSetTitulos.First;
        while not FClientDataSetTitulos.Eof do
        begin
          conteudoArquivo.add(getLinhaBoleto);
          conteudoArquivo.add(getLinhaMensagem);
          FClientDataSetTitulos.Next;
        end;
        conteudoArquivo.add(getTrailer);
        conteudoArquivo.SaveToFile(FileName);
      finally
        FreeAndNil(conteudoArquivo);
      end;
    end;
    free;
  end;
end;

procedure TCNAB400.testar;
begin
  with TStringList.Create do
  begin
    add(getHeader);
    FClientDataSetTitulos.First;
    while not FClientDataSetTitulos.Eof do
    begin
      FClientDataSetTitulos.Next;
    end;
    add(getTrailer);
    SaveToFile('c:\testecnab400.txt');
    free
  end;
end;

function TCNAB400.getTrailer: string;
begin
  result := '9';
  result := result + stringOfChar(' ', 393);
  result := result + getNumSeqRegistro;
end;

function TCNAB400.getUltimoSequencial: string;
begin
  result := IntToStr(FSeqRegistro-1);
  result := stringOfChar('0',6-length(result));
end;

function TCNAB400.getNomebanco: string;
begin
  result := FNomeBanco + stringOfChar(' ', 15-length(FNomeBanco));
end;

function TCNAB400.getStrDataGeracao: string;
begin
  result := FormatDateTime('ddmmyy',date);
end;

function TCNAB400.getSequencialArquivo: string;
begin
  Result := IntToStr(FSequencialArquivo);
  Result := StringOfChar('0', 7-length(result)) + Result;
end;

function TCNAB400.getItentificacaoEmpresa: string;
begin
  result := FCarteira +
    FormatFloat('00000', FAgenciaEmpresa) +
    FormatFloat('00000000', FContaEmpresa)
end;

procedure TCNAB400.adicionarBoleto(
  sequencial: integer;
  numDocumento: string;
  datavencimento: TDateTime;
  valor: Currency;
  valorDiaAtraso: Currency;
  CPFCNPJ: string;
  nomeSacado: string;
  enderecoCompleto: string;
  CEP: string;
  mensagem1: string;
  mensagem2: string;
  mensagem3: string;
  mensagem4: string
  );
begin
  FClientDataSetTitulos.Append;
  FClientDataSetTitulos.FieldByName('Sequencial').AsInteger :=
    sequencial;
  FClientDataSetTitulos.FieldByName('NumDocumento').AsString :=
    numDocumento;
  FClientDataSetTitulos.FieldByName('dataVencimento').AsDateTime :=
    datavencimento;
  FClientDataSetTitulos.FieldByName('Valor').AsFloat :=
    valor;
  FClientDataSetTitulos.FieldByName('ValorDiaAtraso').AsFloat :=
    valorDiaAtraso;
  FClientDataSetTitulos.FieldByName('CPFCNPJ').AsString :=
    CPFCNPJ;
  FClientDataSetTitulos.FieldByName('NomeSacado').AsString :=
    nomeSacado;
  FClientDataSetTitulos.FieldByName('EnderecoCompleto').AsString :=
    enderecoCompleto;
  FClientDataSetTitulos.FieldByName('CEP').AsString :=
    CEP;
  FClientDataSetTitulos.FieldByName('Mensagem1').AsString :=
    Mensagem1;
  FClientDataSetTitulos.FieldByName('Mensagem2').AsString :=
    Mensagem2;
  FClientDataSetTitulos.FieldByName('Mensagem3').AsString :=
    Mensagem3;
  FClientDataSetTitulos.FieldByName('Mensagem4').AsString :=
    Mensagem4;
  FClientDataSetTitulos.Post;
end;

function TCNAB400.getTextoNomeSacado: string;
begin
  result := FClientDataSetTitulos.fieldByName('NomeSacado').AsString;
  result := result + stringOfChar(' ', 40-length(result));
end;

function TCNAB400.getTextoEnderecoCompleto: string;
begin
  result := FClientDataSetTitulos.fieldByName('EnderecoCompleto').AsString;
  result := result + stringOfChar(' ', 40-length(result));
end;

function TCNAB400.getTextoSequencialBoleto: string;
begin
  result := FClientDataSetTitulos.fieldByName('Sequencial').AsString;
  result := stringOfChar('0',25-length(result)) + result;
end;

function TCNAB400.getNumDocumentoBoleto: string;
begin
  Result := FClientDataSetTitulos.fieldByName('NumDocumento').AsString;
  result := stringOfChar(' ',10-length(result)) + result;
end;

function TCNAB400.getTextoDatavencimentoBoleto: string;
begin
  Result := FormatDateTime('ddmmyy',
    FClientDataSetTitulos.fieldByName('DataVencimento').AsDateTime);
  result := stringOfChar('0',6-length(result)) + result;
end;

function TCNAB400.getTextoValorBoleto: string;
var
  valInteiro: integer;
  valFrac: integer;
  valFloat: double;
begin
  valFloat := FClientDataSetTitulos.fieldByName('Valor').AsFloat + FValorBoleto;
  Result := FormatFloat('0000000000000',valFloat*100);
end;

function TCNAB400.getTextoValorMultaDia: string;
var
  valInteiro: integer;
  valFrac: integer;
  valFloat: double;
begin
  valFloat := FClientDataSetTitulos.fieldByName('ValorDiaAtraso').AsFloat;
  valInteiro := trunc(valFloat);
  valFrac := trunc((valFloat - trunc(valFloat))*100);
  Result := FormatFloat('00000000000',valInteiro) +
    FormatFloat('00',valFrac);
end;

function TCNAB400.getInstrucaoProtesto: string;
begin
  result := '0000'; //não protesta, está fixo
end;

function TCNAB400.getStrCPFCNPJ: string;
begin
  result := '01'; //CPF
  result := result+ '000' + FClientDataSetTitulos.FieldByName('CPFCNPJ').AsString;
            //o 000 acima é por que é CPF
end;

function TCNAB400.getTextoPrimeiraMensagem: string;
begin
  result := stringOfChar(' ', 12);
end;

function TCNAB400.getTextoSegundaMensagem: string;
begin
  result := '';
  result := result + stringOfChar(' ', 60-length(result));
end;

function TCNAB400.getNumSeqRegistro: string;
begin
  result := IntToStr(FSeqRegistro);
  inc(FSeqRegistro);
  result := stringOfChar('0', 6-length(result)) + result;
end;

function TCNAB400.getLinhaBoleto: string;
begin
  result := '1';
  result := result + stringOfChar('0', 19); //19 zeros. esta parte deverá ser
                        //preenchida caso se queira contemplar o débito em conta
  result := Result + '0' + getItentificacaoEmpresa;
  result := result + getTextoSequencialBoleto;
  result := result + '000';
  result := result + stringOfChar('0', 5); //5 zeros, fixo
  result := result + stringOfChar('0', 12); //12 zeros, assim a papeleta do
                        //boleto é emitida pelo banco
  result := result + stringOfChar('0', 10); //zero de desconto, se quiser tem
                        //que mudar
  result := result + '1'; //banco emite a papeleta, mudar caso precise que o
                        //cliente emita. Mudar pra algo não fixo, claro.
  result := result + 'N'; //não entra no débito automático
  result := result + stringOfChar(' ', 10); //10 brancos, fixo.
                        //o campo significa: Identificação da operação do Banco
  result := Result + ' '; //um branco por que não participa do rateio
  result := Result + '2'; //não emite aviso de débito automático
  result := result + stringOfChar(' ', 2); //2 brancos, fixo
  result := result + '01'; //indica remessa
  result := result + getNumDocumentoBoleto + getTextoDatavencimentoBoleto +
    getTextoValorBoleto;
  result := result + '000'; //banco encarregado
  result := result + '00000'; //agencia depositária
  result := result + '01'; //espécie do título = 99 -> DM
  result := result + 'N';  //o que significa Aceito?
  result := result + FormatDateTime('ddmmyy', date); //data da emissão do título
  result := result + getInstrucaoProtesto; //instrucoes de protesto
  result := result + getTextoValorMultaDia;
  result := result + stringOfChar('0',19); //32 zeros fixos
                     //aqui estão os valores dos descontos
  result := result + stringOfChar('0',13); //13 zeros. IOF, só deve ser
                     //preenchido quando a cedente for administradora de seguros
  result := result + stringOfChar('0',13); //13 zeros. abatimento
  result := result + getStrCPFCNPJ;
  result := result + getTextoNomeSacado;
  result := result + getTextoEnderecoCompleto;
  result := result + getTextoPrimeiraMensagem;
  result := result + FClientDataSetTitulos.fieldByName('CEP').AsString;
  result := result + getTextoSegundaMensagem;
  result := result + getNumSeqRegistro;
end;


function TCNAB400.getLinhaMensagem: string;
begin
  result := '2';
  result := result + trim(FClientDataSetTitulos.fieldByName('Mensagem1').AsString) + StringOfChar(' ',80-length(trim(FClientDataSetTitulos.fieldByName('Mensagem1').AsString)));
  result := result + trim(FClientDataSetTitulos.fieldByName('Mensagem2').AsString) + StringOfChar(' ',80-length(trim(FClientDataSetTitulos.fieldByName('Mensagem2').AsString)));
  result := result + trim(FClientDataSetTitulos.fieldByName('Mensagem3').AsString) + StringOfChar(' ',80-length(trim(FClientDataSetTitulos.fieldByName('Mensagem3').AsString)));
  result := result + trim(FClientDataSetTitulos.fieldByName('Mensagem4').AsString) + StringOfChar(' ',80-length(trim(FClientDataSetTitulos.fieldByName('Mensagem4').AsString)));
  result := result + stringOfChar(' ', 45);
  result := result + getItentificacaoEmpresa;
  result := result + stringOfChar('0',12);
  result := result + getNumSeqRegistro;
end;






end.
