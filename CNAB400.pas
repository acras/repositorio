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
    function SalvarArquivo(var nomeArquivo: string): boolean;


    procedure testar;
  public
    constructor Create;
  end;

const
  FNomeBanco = 'Bradesco';
  FCodigoBanco = '237';
  FCarteira = '009';

  //constantes para os tipos de operação
  toEntradaConfirmada                       = 2;
  toEntradaRejeitada                        = 3;
  toLiquidacaoNormal                        = 6;
  toBaixadoAutomaticamenteViaArquivo        = 9;
  toBaixadoConformeInstrucaoAgencia         = 10;
  toArquivoTitulosPendentes                 = 11;
  toAbatimentoConncedido                    = 12;
  toAbatimentoCancelado                     = 13;
  toVencimentoAlterado                      = 14;
  toLiquidacaoCartorio                      = 15;
  toTituloPagoCheque                        = 16;
  toLiquidacaoAposBaixaOuNaoRegistrado      = 17;
  toAcertoDepositaria                       = 18;
  toConfirmacaoRecebInstProtesto            = 19;
  toConfirmacaoRecebInstSustacaoCheque      = 20;
  toAcertoControleParticipante              = 21;
  toTituloPagamentoCancelado                = 22;
  toEntradaTituloCartorio                   = 23;
  toEntradaRejeitadaCEPIrregular            = 24;
  toBaixaRejeitada                          = 27;
  toDebitoTarifasCustas                     = 28;
  toAlteracaoOutrosDadosRejeitados          = 30;
  toInstrucaoRejeitada                      = 32;
  toConfirmacaoPedidoAlteracaoOutorsDados   = 33;
  toRetiradoCartorioManutencaoCarteira      = 34;
  toDesagendamentoDebitoAutomatico          = 35;
  toAcertoDadosRateioCredito                = 68;
  toCancelamentoDadosRateio                 = 69;

implementation




uses TypInfo, acStrUtils;





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
    size := 40;
  end;

  with TStringField.Create(FClientDataSetTitulos) do
  begin
    FieldName := 'EnderecoCompleto';
    Size := 40;
    DisplayWidth := 40;
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

  with TCurrencyField.Create(FClientDataSetRetorno) do
  begin
    FieldName := 'Mora';
    DataSet   := FClientDataSetRetorno;
  end;

  with TStringField.Create(FClientDataSetRetorno) do
  begin
    FieldName := 'MotivoRecusa';
    Size := 2000;
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
  iLinhaAtual, i, codMotivo, tipoOperacao: integer;
  mensagem, statusProtesto: string;
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

    //processar campos de interesse no header
    linhaAtual := conteudo[0];
    FSequencialArquivo := StrToInt(copy(linhaAtual,109,5));

    //interpretar as linhas do boleto, cada linha é um boleto
    iLinhaAtual := 1;
    while iLinhaAtual < conteudo.Count do
    begin
      linhaAtual := conteudo[iLinhaAtual];
      //se é linha de retonro e o sequencial não é zero
      if (copy(linhaAtual,1,1) = '1') AND (strToInt(copy(linhaAtual,38,25))<>0) then
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
        FClientDataSetRetorno.FieldByName('Mora').AsCurrency :=
          StrToFloat(copy(linhaAtual, 267, 11)+','+copy(linhaAtual, 278, 2));
        FClientDataSetRetorno.FieldByName('DataPagamento').AsDateTime :=
          encodedate(
            2000+strToInt(copy(linhaAtual, 115, 2)),
            strToInt(copy(linhaAtual, 113, 2)),
            strToInt(copy(linhaAtual, 111, 2))
          );

        tipoOperacao :=
          FClientDataSetRetorno.FieldByName('TipoOcorrencia').AsInteger;

        mensagem := '';
        case tipoOperacao of
        //Entrada Confirmada
        02 :
        begin
          for i := 0 to 4 do
          begin
            codMotivo := strToInt(copy(linhaAtual,319+i*2,2));
            if codMotivo <> 0 then
            case codMotivo of
              00: mensagem := mensagem + 'Ocorrência aceita. ';
              01: mensagem := mensagem + 'Código do Banco inválido.';
              17: mensagem := mensagem + 'Data de vencimento anterior a data de emissão.';
              21: mensagem := mensagem + 'Espécie do Título inválido.';
              24: mensagem := mensagem + 'Data da emissão inválida.';
              38: mensagem := mensagem + 'Prazo para protesto inválido.';
              39: mensagem := mensagem + 'Pedido para protesto não permitido para título.';
              43: mensagem := mensagem + 'Prazo para baixa e devolução inválido.';
              45: mensagem := mensagem + 'Nome do Sacado inválido.';
              46: mensagem := mensagem + 'Tipo/num. de inscrição do Sacado inválidos.';
              47: mensagem := mensagem + 'Endereço do Sacado não informado.';
              48: mensagem := mensagem + 'CEP irregular.';
              50: mensagem := mensagem + 'CEP referente a Banco correspondente.';
              53: mensagem := mensagem + 'Nº de inscrição do Sacador/avalista inválidos (CPF/CNPJ).';
              54: mensagem := mensagem + 'Sacador/avalista não informado.';
              67: mensagem := mensagem + 'Débito automático agendado';
              68: mensagem := mensagem + 'Débito não agendado - erro nos dados de remessa.';
              69: mensagem := mensagem + 'Débito não agendado - Sacado não consta no cadastro de autorizante.';
              70: mensagem := mensagem + 'Débito não agendado - Cedente não autorizado pelo Sacado.';
              71: mensagem := mensagem + 'Débito não agendado - Cedente não participa da modalidade de déb.automático.';
              72: mensagem := mensagem + 'Débito não agendado - Código de moeda diferente de R$.';
              73: mensagem := mensagem + 'Débito não agendado - Data de vencimento inválida.';
              75: mensagem := mensagem + 'Débito não agendado - Tipo do número de inscrição do sacado debitado inválido.';
              86: mensagem := mensagem + 'Seu número do documento inválido.';
            end;
          end;
        end;

        //Entrada Rejeitada
        03 :
        begin
          for i := 0 to 4 do
          begin
            codMotivo := strToInt(copy(linhaAtual,319+i*2,2));
            if codMotivo <> 0 then
            case codMotivo of
              02: mensagem := mensagem + 'Código do registro detalhe inválido.';
              03: mensagem := mensagem + 'Código da ocorrência inválida.';
              04: mensagem := mensagem + 'Código de ocorrência não permitida para a carteira.';
              05: mensagem := mensagem + 'Código de ocorrência não numérico.';
              07: mensagem := mensagem + 'Agência/conta/Digito - |Inválido.';
              08: mensagem := mensagem + 'Nosso número inválido.';
              09: mensagem := mensagem + 'Nosso número duplicado.';
              10: mensagem := mensagem + 'Carteira inválida.';
              16: mensagem := mensagem + 'Data de vencimento inválida.';
              18: mensagem := mensagem + 'Vencimento fora do prazo de operação.';
              20: mensagem := mensagem + 'Valor do Título inválido.';
              21: mensagem := mensagem + 'Espécie do Título inválida.';
              22: mensagem := mensagem + 'Espécie não permitida para a carteira.';
              24: mensagem := mensagem + 'Data de emissão inválida.';
              38: mensagem := mensagem + 'Prazo para protesto inválido.';
              44: mensagem := mensagem + 'Agência Cedente não prevista.';
              50: mensagem := mensagem + 'CEP irregular - Banco Correspondente.';
              63: mensagem := mensagem + 'Entrada para Título já cadastrado.';
              68: mensagem := mensagem + 'Débito não agendado - erro nos dados de remessa.';
              69: mensagem := mensagem + 'Débito não agendado - Sacado não consta no cadastro de autorizante.';
              70: mensagem := mensagem + 'Débito não agendado - Cedente não autorizado pelo Sacado.';
              71: mensagem := mensagem + 'Débito não agendado - Cedente não participa do débito Automático.';
              72: mensagem := mensagem + 'Débito não agendado - Código de moeda diferente de R$.';
              73: mensagem := mensagem + 'Débito não agendado - Data de vencimento inválida.';
              74: mensagem := mensagem + 'Débito não agendado - Conforme seu pedido, Título não registrado.';
              75: mensagem := mensagem + 'Débito não agendado – Tipo de número de inscrição do debitado inválido.';
            end;
          end;
        end;

        //Liquidação
        06 :
        begin
          for i := 0 to 4 do
          begin
            codMotivo := strToInt(copy(linhaAtual,319+i*2,2));
            if codMotivo <> 0 then
            case codMotivo of
              00: mensagem := mensagem + 'Título pago com dinheiro ( Novo ).';
              15: mensagem := mensagem + 'Título pago com cheque.';
            end;
          end;
        end;

        //Baixado Automaticamente via arquivo
        09 :
        begin
          for i := 0 to 4 do
          begin
            codMotivo := strToInt(copy(linhaAtual,319+i*2,2));
            if codMotivo <> 0 then
            case codMotivo of
              10: mensagem := mensagem + 'Baixa Comandada pelo Cliente. ';
            end;
          end;
        end;

        //Baixado pelo banco
        10 :
        begin
          for i := 0 to 4 do
          begin
            codMotivo := strToInt(copy(linhaAtual,319+i*2,2));
            if codMotivo <> 0 then
            case codMotivo of

              00: mensagem := mensagem + 'Baixado Conforme Instruções da Agência.';
              14: mensagem := mensagem + 'Título Protestado.';
              15: mensagem := mensagem + 'Título excluído.';
              16: mensagem := mensagem + 'Título Baixado pelo Banco por decurso Prazo.';
              20: mensagem := mensagem + 'Título Baixado e transferido para desconto.';
            end;
          end;
        end;

        //Liquidacao em cartório
        15 :
        begin
          for i := 0 to 4 do
          begin
            codMotivo := strToInt(copy(linhaAtual,319+i*2,2));
            if codMotivo <> 0 then
            case codMotivo of
              00: mensagem := mensagem + 'Titulo pago com dinehiro. ';
              15: mensagem := mensagem + 'Titulo pago com cheque. ';
            end;
          end;
        end;

        //Liquidação após baixa ou título não registrado
        17 :
        begin
          for i := 0 to 4 do
          begin
            codMotivo := strToInt(copy(linhaAtual,319+i*2,2));
            if codMotivo <> 0 then
            case codMotivo of
              00: mensagem := mensagem + 'Título pago com dinheiro.';
              15: mensagem := mensagem + 'Título pago com cheque.';
            end;
          end;
        end;

        //Confirmação de Recebimento Instrução de Protesto
        19 :
        begin
            statusProtesto := copy(linhaAtual,295,1);
            if statusProtesto <> '' then
            if statusProtesto = 'A' then
              mensagem := mensagem + 'Protesto aceito.';
            if statusProtesto = 'D' then
              mensagem := mensagem + 'Protesto desprezado.';
        end;

        //Entrada rejeitada por CEP irregular
        24 :
        begin
          for i := 0 to 4 do
          begin
            codMotivo := strToInt(copy(linhaAtual,319+i*2,2));
            if codMotivo <> 0 then
            case codMotivo of
              48: mensagem := mensagem + 'CEP Inválido. ';
            end;
          end;
        end;

        //Baixa Rejeitada
        27 :
        begin
          for i := 0 to 4 do
          begin
            codMotivo := strToInt(copy(linhaAtual,319+i*2,2));
            if codMotivo <> 0 then
            case codMotivo of
              04: mensagem := mensagem + 'Código de ocorrência não permitido para a carteira.';
              07: mensagem := mensagem + 'Agência/Conta/dígito inválidos.';
              08: mensagem := mensagem + 'Nosso número inválido.';
              10: mensagem := mensagem + 'Carteira inválida.';
              15: mensagem := mensagem + 'Carteira/Agência/Conta/nosso número inválidos.';
              40: mensagem := mensagem + 'Título com ordem de protesto emitido.';
              42: mensagem := mensagem + 'Código para baixa/devolução via Telebradesco inválido.';
              60: mensagem := mensagem + 'Movimento para Título não cadastrado.';
              77: mensagem := mensagem + 'Transferência para desconto não permitido para a carteira.';
              85: mensagem := mensagem + 'Título com pagamento vinculado.';
            end;
          end;
        end;

        //Débito de tarifas/Custas
        28 :
        begin
          for i := 0 to 4 do
          begin
            codMotivo := strToInt(copy(linhaAtual,319+i*2,2));
            if codMotivo <> 0 then
            case codMotivo of
              03: mensagem := mensagem + 'Tarifa de sustação.';
              04: mensagem := mensagem + 'Tarifa de protesto.';
              08: mensagem := mensagem + 'Custas de protesto.';
            end;
          end;
        end;

        //Alteração de outros dados rejeitados
        30 :
        begin
          for i := 0 to 4 do
          begin
            codMotivo := strToInt(copy(linhaAtual,319+i*2,2));
            if codMotivo <> 0 then
            case codMotivo of
              01: mensagem := mensagem + 'Código do Banco inválido.';
              04: mensagem := mensagem + 'Código de ocorrência não permitido para a carteira.';
              05: mensagem := mensagem + 'Código da ocorrência não numérico.';
              08: mensagem := mensagem + 'Nosso número inválido.';
              15: mensagem := mensagem + 'Característica da cobrança imcopátivel.';
              16: mensagem := mensagem + '.Data de vencimento inválido.';
              17: mensagem := mensagem + 'Data de vencimento anterior a data de emissão.';
              18: mensagem := mensagem + 'Vencimento fora do prazo de operação.';
              24: mensagem := mensagem + 'Data de emissão Inválida.';
              29: mensagem := mensagem + 'Valor do desconto maior/igual ao valor do Título.';
              30: mensagem := mensagem + 'Desconto a conceder não confere.';
              31: mensagem := mensagem + 'Concessão de desconto já existente ( Desconto anterior ).';
              33: mensagem := mensagem + 'Valor do abatimento inválido.';
              34: mensagem := mensagem + 'Valor do abatimento maior/igual ao valor do Título.';
              38: mensagem := mensagem + 'Prazo para protesto inválido.';
              39: mensagem := mensagem + 'Pedido de protesto não permitido para o Título.';
              40: mensagem := mensagem + 'Título com ordem de protesto emitido.';
              42: mensagem := mensagem + 'Código para baixa/devolução inválido.';
              60: mensagem := mensagem + 'Movimento para Título não cadastrado.';
              85: mensagem := mensagem + 'Título com Pagamento Vinculado.';
            end;
          end;
        end;

        //Instrução rejeitada
        32 :
        begin
          for i := 0 to 4 do
          begin
            codMotivo := strToInt(copy(linhaAtual,319+i*2,2));
            if codMotivo <> 0 then
            case codMotivo of
              01: mensagem := mensagem + '.Código do Banco inválido.';
              02: mensagem := mensagem + 'Código do registro detalhe inválido.';
              04: mensagem := mensagem + 'Código de ocorrência não permitido para a carteira.';
              05: mensagem := mensagem + 'Código de ocorrência não numérico.';
              07: mensagem := mensagem + 'Agência/Conta/dígito inválidos.';
              08: mensagem := mensagem + 'Nosso número inválido.';
              10: mensagem := mensagem + 'Carteira inválida.';
              15: mensagem := mensagem + 'Características da cobrança incompatíveis.';
              16: mensagem := mensagem + 'Data de vencimento inválida.';
              17: mensagem := mensagem + 'Data de vencimento anterior a data de emissão.';
              18: mensagem := mensagem + 'Vencimento fora do prazo de operação.';
              20: mensagem := mensagem + 'Valor do título inválido.';
              21: mensagem := mensagem + 'Espécie do Título inválida.';
              22: mensagem := mensagem + '.Espécie não permitida para a carteira.';
              24: mensagem := mensagem + 'Data de emissão inválida.';
              28: mensagem := mensagem + 'Código de desconto via Telebradesco inválido.';
              29: mensagem := mensagem + 'Valor do desconto maior/igual ao valor do Título.';
              30: mensagem := mensagem + 'Desconto a conceder não confere.';
              31: mensagem := mensagem + 'Concessão de desconto - Já existe desconto anterior.';
              33: mensagem := mensagem + 'Valor do abatimento inválido.';
              34: mensagem := mensagem + 'Valor do abatimento maior/igual ao valor do Título.';
              36: mensagem := mensagem + 'Concessão abatimento - Já existe abatimento anterior.';
              38: mensagem := mensagem + 'Prazo para protesto inválido.';
              39: mensagem := mensagem + 'Pedido de protesto não permitido para o Título.';
              40: mensagem := mensagem + 'Título com ordem de protesto emitido.';
              41: mensagem := mensagem + 'Pedido cancelamento/sustação para Título sem instrução de protesto.';
              42: mensagem := mensagem + 'Código para baixa/devolução inválido.';
              45: mensagem := mensagem + 'Nome do Sacado não informado.';
              46: mensagem := mensagem + 'Tipo/número de inscrição do Sacado inválidos.';
              47: mensagem := mensagem + 'Endereço do Sacado não informado.';
              48: mensagem := mensagem + 'CEP Inválido.';
              50: mensagem := mensagem + 'CEP referente a um Banco correspondente.';
              53: mensagem := mensagem + 'Tipo de inscrição do sacador avalista inválidos.';
              60: mensagem := mensagem + 'Movimento para Título não cadastrado.';
              85: mensagem := mensagem + 'Título com pagamento vinculado.';
              86: mensagem := mensagem + 'Seu número inválido.';
            end;
          end;
        end;

        //Desagendamento do débito automático
        35 :
        begin
          for i := 0 to 4 do
          begin
            codMotivo := strToInt(copy(linhaAtual,319+i*2,2));
            if codMotivo <> 0 then
            case codMotivo of
              81: mensagem := mensagem + 'Tentativas esgotadas, baixado.';
              82: mensagem := mensagem + 'Tentativas esgotadas, pendente.';
            end;
          end;
        end;

        end;
        FClientDataSetRetorno.fieldByName('MotivoRecusa').AsString :=
          mensagem;
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

function TCNAB400.SalvarArquivo(var nomeArquivo: string): boolean;
var
  conteudoArquivo: TStringList;
begin
  result := false;
  FSeqRegistro := 1;
  with TSaveDialog.Create(nil) do
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
        nomeArquivo := FileName;
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
    RemoveAcento(nomeSacado);
  FClientDataSetTitulos.FieldByName('EnderecoCompleto').AsString :=
    RemoveAcento(enderecoCompleto);
  FClientDataSetTitulos.FieldByName('CEP').AsString :=
    CEP;
  FClientDataSetTitulos.FieldByName('Mensagem1').AsString :=
    RemoveAcento(Mensagem1);
  FClientDataSetTitulos.FieldByName('Mensagem2').AsString :=
    RemoveAcento(Mensagem2);
  FClientDataSetTitulos.FieldByName('Mensagem3').AsString :=
    RemoveAcento(Mensagem3);
  FClientDataSetTitulos.FieldByName('Mensagem4').AsString :=
    RemoveAcento(Mensagem4);
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
