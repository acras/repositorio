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
  Dialogs, classes, SysUtils, DBClient, DB;
type
  TTipoArquivo = (taRemessa, taRetorno);

  TCNAB400 = class
  private
    FRazaoSocial: string;
    FCodigoEmpresa: integer;
    FSequencialArquivo: integer;
    FAgenciaEmpresa: integer;
    FContaEmpresa: integer;
    FTipoArquivo: TTipoArquivo;
    function getCodigoEmpresa: String;
    function getRazaoSocial: String;
    function getCodigoBanco: String;
    function getNomebanco: string;
    function getSequencialArquivo: string;
    function getStrDataGeracao: string;
    function getItentificacaoEmpresa: string;
    function getLinhaBoleto: string;
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
    procedure abrirArquivo(sNomeArquivo: string);
  protected
    function getHeader: String;
  public
    FClientDataSetTitulos: TClientDataSet;
    procedure adicionarBoleto(sequencial: integer;
                              numDocumento: string;
                              datavencimento: TDateTime;
                              valor: Currency;
                              CPFCNPJ: string;
                              nomeSacado: string;
                              enderecoCompleto: string;
                              CEP: string);
    property RazaoSocial: string read FRazaoSocial write FRazaoSocial;
    property CodigoEmpresa: integer read FCodigoEmpresa write FCodigoEmpresa;
    property AgenciaEmpresa: integer read FAgenciaEmpresa write FAgenciaEmpresa;
    property ContaEmpresa: integer read FContaEmpresa write FContaEmpresa;
    property SequencialArquivo: integer read FSequencialArquivo
      write FSequencialArquivo;
    property tipoArquivo: TTipoArquivo read FTipoArquivo;
    constructor Create;
    destructor Destroy; override;
    procedure SalvarArquivo;


    procedure testar;
  end;

implementation


uses TypInfo;

const
  FNomeBanco = 'Bradesco';
  FCodigoBanco = '237';
  FCarteira = '009';

{ TCNAB400 }

constructor TCNAB400.Create;
begin
  FTipoArquivo := taRemessa;

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

  with TCurrencyField.Create(FClientDataSetTitulos) do
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

  FClientDataSetTitulos.CreateDataSet;
end;

destructor TCNAB400.Destroy;
begin
  FClientDataSetTitulos.Free;
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
  result := Result + '000001'; //seq do registro está fixo até desvendar
end;

procedure TCNAB400.SalvarArquivo;
var
  conteudoArquivo: TStringList;
begin
  with TOpenDialog.Create(nil) do
  begin
    if Execute then
    begin
      conteudoArquivo := TStringList.Create;
      try
        conteudoArquivo.Add(getHeader);
        FClientDataSetTitulos.First;
        while not FClientDataSetTitulos.Eof do
        begin
          FClientDataSetTitulos.Next;
        end;
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
      add(getLinhaBoleto);
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
  result := result + getUltimoSequencial;
end;

function TCNAB400.getUltimoSequencial: string;
begin
  result := IntToStr(FClientDataSetTitulos.fieldByName('Sequencial').asInteger);
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

  result := '0' + FCarteira +
    FormatFloat('00000', FAgenciaEmpresa) +
    FormatFloat('00000000', FContaEmpresa)
end;

procedure TCNAB400.adicionarBoleto(
  sequencial: integer;
  numDocumento: string;
  datavencimento: TDateTime;
  valor: Currency;
  CPFCNPJ: string;
  nomeSacado: string;
  enderecoCompleto: string;
  CEP: string
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
  FClientDataSetTitulos.FieldByName('CPFCNPJ').AsString :=
    CPFCNPJ;
  FClientDataSetTitulos.FieldByName('NomeSacado').AsString :=
    nomeSacado;
  FClientDataSetTitulos.FieldByName('EnderecoCompleto').AsString :=
    enderecoCompleto;
  FClientDataSetTitulos.FieldByName('CEP').AsString :=
    CEP;
  FClientDataSetTitulos.Post;
end;

function TCNAB400.getTextoNomeSacado: string;
begin
  result := FClientDataSetTitulos.fieldByName('NomeSacado').AsString;
  result := stringOfChar(' ', 40-length(result)) + result;
end;

function TCNAB400.getTextoEnderecoCompleto: string;
begin
  result := FClientDataSetTitulos.fieldByName('EnderecoCompleto').AsString;
  result := stringOfChar(' ', 40-length(result)) + result;
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
  valFloat := FClientDataSetTitulos.fieldByName('Valor').AsFloat;
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
  result := 'PRIMEIRAMENS';
end;

function TCNAB400.getTextoSegundaMensagem: string;
begin
  result := 'mensagem a ser impressa no boleto';
  result := result + stringOfChar(' ', 60-length(result));
end;

function TCNAB400.getNumSeqRegistro: string;
begin
  result := IntToStr(FClientDataSetTitulos.RecNo);
  result := stringOfChar('0', 6-length(result)) + result;
end;

function TCNAB400.getLinhaBoleto: string;
begin
  result := '1';
  result := result + stringOfChar(' ', 19); //19 espaços. esta parte deverá ser
                        //preenchida caso se queira contemplar o débito em conta
  result := Result + getItentificacaoEmpresa;
  result := result + getTextoSequencialBoleto;
  result := result + FCodigoBanco;
  result := result + stringOfChar('0', 5); //5 zeros, fixo
  result := result + stringOfChar('0', 12); //12 zeros, assim a papeleta do
                        //boleto é emitida pelo banco
  result := result + stringOfChar(' ', 10); //zero de desconto, se quiser tem
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
  result := result + '99'; //espécie do título = 99 -> outros
  result := result + 'N';  //o que significa Aceito?
  result := result + FormatDateTime('ddmmyy', date); //data da emissão do título
  result := result + getInstrucaoProtesto; //instrucoes de protesto
  result := result + stringOfChar('0',32); //32 zeros fixos
                     //aqui está a multa mora dia e os valores dos descontos
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

procedure TCNAB400.abrirArquivo(sNomeArquivo: string);
var
  conteudo: TStringList;
  sHeader, sSegmentoT, sSegmentoU: string;
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
    
  finally
    FreeAndNil(conteudo);
  end;
end;

end.
