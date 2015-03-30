unit Bematech;

interface

uses Windows, SysUtils;

//As funções abaixo estão comentadas pois foi feito um esquema para load dinâmico das funções
//  somente quando necessário. Se precisar de alguma destas funções deve-se escrevê-la no
//  esquema em que as outras estão escritas mais abaixo

{// Funções de Inicialização
function Bematech_FI_AlteraSimboloMoeda( SimboloMoeda: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ProgramaHorarioVerao: Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_NomeiaDepartamento( Indice: Integer; Departamento: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_NomeiaTotalizadorNaoSujeitoIcms( Indice: Integer; Totalizador: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ProgramaArredondamento: Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ProgramaTruncamento: Integer; StdCall; External 'BEMAFI32.DLL' Name 'Bematech_FI_ProgramaTruncamento';
function Bematech_FI_LinhasEntreCupons( Linhas: Integer ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_EspacoEntreLinhas( Dots: Integer ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ForcaImpactoAgulhas( ForcaImpacto: Integer ): Integer; StdCall; External 'BEMAFI32.DLL';

// Funções do Cupom Fiscal
function Bematech_FI_CancelaItemAnterior: Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_FechaCupomResumido( FormaPagamento: AnsiString; Mensagem: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_FechaCupom( FormaPagamento: AnsiString; AcrescimoDesconto: AnsiString; TipoAcrescimoDesconto: AnsiString; ValorAcrescimoDesconto: AnsiString; ValorPago: AnsiString; Mensagem: AnsiString): Integer; StdCall; External 'BEMAFI32.DLL';

function Bematech_FI_EstornoFormasPagamento( FormaOrigem: AnsiString; FormaDestino: AnsiString; Valor: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_UsaUnidadeMedida( UnidadeMedida: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';

// Funções dos Relatórios Fiscais
function Bematech_FI_LeituraMemoriaFiscalData( DataInicial: AnsiString; DataFinal: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_LeituraMemoriaFiscalReducao( ReducaoInicial: AnsiString; ReducaoFinal: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_LeituraMemoriaFiscalSerialData( DataInicial: AnsiString; DataFinal: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
}
// Funções das Operações Não Fiscais
//function Bematech_FI_RecebimentoNaoFiscal( IndiceTotalizador: AnsiString; Valor: AnsiString; FormaPagamento: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
//function Bematech_FI_AbreComprovanteNaoFiscalVinculado( FormaPagamento: AnsiString; Valor: AnsiString; NumeroCupom: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
//function Bematech_FI_UsaComprovanteNaoFiscalVinculado( Texto: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
//function Bematech_FI_FechaComprovanteNaoFiscalVinculado: Integer; StdCall; External 'BEMAFI32.DLL';
{
// Funções de Informações da Impressora
function Bematech_FI_ModeloImpressora( Modelo: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_LeituraXSerial: Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_VersaoFirmware( VersaoFirmware: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_CGC_IE( CGC: AnsiString; IE: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_GrandeTotal( GrandeTotal: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_Cancelamentos( ValorCancelamentos: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_Descontos( ValorDescontos: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_NumeroCuponsCancelados( NumeroCancelamentos: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_NumeroIntervencoes( NumeroIntervencoes: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_NumeroReducoes( NumeroReducoes: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_NumeroSubstituicoesProprietario( NumeroSubstituicoes: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ClicheProprietario( Cliche: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_NumeroCaixa( NumeroCaixa: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_NumeroLoja( NumeroLoja: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_SimboloMoeda( SimboloMoeda: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_MinutosLigada( Minutos: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_MinutosImprimindo( Minutos: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_VerificaModoOperacao( Modo: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_VerificaEpromConectada( Flag: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ValorPagoUltimoCupom( ValorCupom: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ContadoresTotalizadoresNaoFiscais( Contadores: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_VerificaTotalizadoresNaoFiscais( Totalizadores: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_VerificaTruncamento( Flag: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_Acrescimos( ValorAcrescimos: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ContadorBilhetePassagem( ContadorPassagem: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';

function Bematech_FI_VerificaRecebimentoNaoFiscal( Recebimentos: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_VerificaDepartamentos( Departamentos: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_VerificaTipoImpressora( Var TipoImpressora: Integer ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_VerificaTotalizadoresParciais( Totalizadores: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_DadosUltimaReducao( DadosReducao: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_MonitoramentoPapel( Var Linhas: Integer): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_VerificaIndiceAliquotasIss( Flag: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ValorFormaPagamento( FormaPagamento: AnsiString; Valor: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ValorTotalizadorNaoFiscal( Totalizador: AnsiString; Valor: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';

// Funções de Autenticação e Gaveta de Dinheiro
function Bematech_FI_Autenticacao:Integer; StdCall; External 'BEMAFI32.DLL' Name 'Bematech_FI_Autenticacao';
function Bematech_FI_ProgramaCaracterAutenticacao( Parametros: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_VerificaEstadoGaveta( Var EstadoGaveta: Integer ): Integer; StdCall; External 'BEMAFI32.DLL';

// Funções para a Impressora Restaurante
function Bematech_FIR_RegistraVenda( Mesa: AnsiString; Codigo: AnsiString; Descricao: AnsiString; Aliquota: AnsiString; Quantidade: AnsiString; ValorUnitario: AnsiString; FlagAcrescimoDesconto: AnsiString; ValorAcrescimoDesconto: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FIR_CancelaVenda( Mesa: AnsiString; Codigo: AnsiString; Descricao: AnsiString; Aliquota: AnsiString; Quantidade: AnsiString; ValorUnitario: AnsiString; FlagAcrescimoDesconto: AnsiString; ValorAcrescimoDesconto: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FIR_FechaCupomContaDividida( NumeroCupons: AnsiString; FlagAcrescimoDesconto: AnsiString; TipoAcrescimoDesconto: AnsiString; ValorAcrescimoDesconto: AnsiString; FormasPagamento: AnsiString; ValorFormasPagamento: AnsiString; ValorPagoCliente: AnsiString; CGC_CPF: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FIR_TransferenciaItem( MesaOrigem: AnsiString; Codigo: AnsiString; Descricao: AnsiString; Aliquota: AnsiString; Quantidade: AnsiString; ValorUnitario: AnsiString; FlagAcrescimoDesconto: AnsiString; ValorAcrescimoDesconto: AnsiString; MesaDestino: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FIR_RegistroVendaSerial( Mesa: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FIR_VerificaMemoriaLivre( Bytes: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';

// Funções de Impressão de Cheques
function Bematech_FI_ProgramaMoedaSingular( MoedaSingular: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ProgramaMoedaPlural( MoedaPlural: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_CancelaImpressaoCheque: Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_VerificaStatusCheque( Var StatusCheque: Integer ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ImprimeCheque( Banco: AnsiString; Valor: AnsiString; Favorecido: AnsiString; Cidade: AnsiString; Data: AnsiString; Mensagem: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_IncluiCidadeFavorecido( Cidade: AnsiString; Favorecido: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ImprimeCopiaCheque: Integer; StdCall; External 'BEMAFI32.DLL' Name 'Bematech_FI_ImprimeCopiaCheque';

// Outras Funções
function Bematech_FI_FechaPortaSerial: Integer; StdCall; External 'BEMAFI32.DLL' Name 'Bematech_FI_FechaPortaSerial';
function Bematech_FI_MapaResumo:Integer; StdCall; External 'BEMAFI32.DLL' Name 'Bematech_FI_MapaResumo';
function Bematech_FI_ImprimeDepartamentos: Integer; StdCall; External 'BEMAFI32.DLL' Name 'Bematech_FI_ImprimeDepartamentos';
function Bematech_FI_RelatorioTipo60Analitico: Integer; StdCall; External 'BEMAFI32.DLL' Name 'Bematech_FI_RelatorioTipo60Analitico';
function Bematech_FI_RelatorioTipo60Mestre: Integer; StdCall; External 'BEMAFI32.DLL' Name 'Bematech_FI_RelatorioTipo60Mestre';
function Bematech_FI_ImpressaoCarne( Titulo, Percelas: AnsiString; Datas, Quantidade: integer; Texto, Cliente, RG_CPF, Cupom: AnsiString; Vias, Assina: integer ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_InfoBalanca( Porta: AnsiString; Modelo: integer; Peso, PrecoKilo, Total: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_DadosSintegra( DataInicio: AnsiString; DataFinal: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_VersaoDll( Versao: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_RegistrosTipo60: Integer; StdCall; External 'BEMAFI32.DLL' Name 'Bematech_FI_RegistrosTipo60';

// Funções da Impressora Fiscal MFD
function Bematech_FI_CancelaCupomMFD(CGC, Nome, Endereco: AnsiString): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ProgramaFormaPagamentoMFD(FormaPagto, OperacaoTef: AnsiString) : Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_EfetuaFormaPagamentoMFD(FormaPagamento, ValorFormaPagamento, Parcelas, DescricaoFormaPagto: AnsiString): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_CupomAdicionalMFD(): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_AcrescimoDescontoItemMFD (Item, AcrescimoDesconto,TipoAcrescimoDesconto, ValorAcrescimoDesconto: AnsiString): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_NomeiaRelatorioGerencialMFD (Indice, Descricao : AnsiString): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_AutenticacaoMFD(Linhas, Texto : AnsiString) : Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_AbreComprovanteNaoFiscalVinculadoMFD(FormaPagamento, Valor, NumeroCupom, CGC, nome, Endereco : AnsiString): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ReimpressaoNaoFiscalVinculadoMFD() : Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_AbreRecebimentoNaoFiscalMFD(CGC, Nome, Endereco : AnsiString): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_EfetuaRecebimentoNaoFiscalMFD(IndiceTotalizador, ValorRecebimento : AnsiString): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_IniciaFechamentoRecebimentoNaoFiscalMFD(AcrescimoDesconto,TipoAcrescimoDesconto, ValorAcrescimo, ValorDesconto : AnsiString): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_FechaRecebimentoNaoFiscalMFD(Mensagem : AnsiString): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_CancelaRecebimentoNaoFiscalMFD(CGC, Nome, Endereco : AnsiString): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_AbreRelatorioGerencialMFD(Indice : AnsiString): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_UsaRelatorioGerencialMFD(Texto : AnsiString): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_UsaRelatorioGerencialMFDTEF(Texto : AnsiString): Integer; StdCall; External 'BEMAFI32.DLL';

function Bematech_FI_VersaoFirmwareMFD(VersaoFirmware : AnsiString): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_CNPJMFD(CNPJ : AnsiString): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_InscricaoEstadualMFD(InscricaoEstadual : AnsiString): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_InscricaoMunicipalMFD(InscricaoMunicipal : AnsiString): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_TempoOperacionalMFD(TempoOperacional : AnsiString): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_MinutosEmitindoDocumentosFiscaisMFD(Minutos : AnsiString): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ContadoresTotalizadoresNaoFiscaisMFD(Contadores : AnsiString): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_VerificaTotalizadoresNaoFiscaisMFD(Totalizadores : AnsiString): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_VerificaFormasPagamentoMFD(FormasPagamento : AnsiString): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_VerificaRecebimentoNaoFiscalMFD(Recebimentos : AnsiString): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_VerificaRelatorioGerencialMFD(Relatorios : AnsiString): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ContadorOperacoesNaoFiscaisCanceladasMFD(OperacoesCanceladas : AnsiString): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ContadorFitaDetalheMFD(ContadorFita : AnsiString): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ComprovantesNaoFiscaisNaoEmitidosMFD(Comprovantes : AnsiString): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_NumeroSerieMemoriaMFD(NumeroSerieMFD : AnsiString): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_MarcaModeloTipoImpressoraMFD(Marca, Modelo, Tipo : AnsiString): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ReducoesRestantesMFD(Reducoes : AnsiString): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_VerificaTotalizadoresParciaisMFD(Totalizadores : AnsiString): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_LeituraChequeMFD(CodigoCMC7 : AnsiString): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ImprimeChequeMFD(NumeroBanco, Valor, Favorecido, Cidade, Data, Mensagem, ImpressaoVerso, Linhas : AnsiString): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_AbreBilhetePassagemMFD(Embarque, Destino, Linha, Agencia, Data, Hora, Poltrona, Plataforma, TipoPassagem: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_CancelaAcrescimoDescontoItemMFD( cFlag, cItem: AnsiString ): integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_SubTotalizaCupomMFD: integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_SubTotalizaRecebimentoMFD: integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_TotalLivreMFD( cMemoriaLivre: AnsiString ): integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_TamanhoTotalMFD( cTamanhoMFD: AnsiString ): integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_AcrescimoDescontoSubtotalRecebimentoMFD( cFlag, cTipo, cValor: AnsiString ): integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_AcrescimoDescontoSubtotalMFD( cFlag, cTipo, cValor: AnsiString): integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_CancelaAcrescimoDescontoSubtotalMFD( cFlag: AnsiString): integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_CancelaAcrescimoDescontoSubtotalRecebimentoMFD( cFlag: AnsiString ): integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_TotalizaCupomMFD: integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_TotalizaRecebimentoMFD: integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_PercentualLivreMFD( cMemoriaLivre: AnsiString ): integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_MapaResumoMFD:Integer; StdCall; External 'BEMAFI32.DLL' Name 'Bematech_FI_MapaResumoMFD';
function Bematech_FI_RelatorioTipo60AnaliticoMFD: Integer; StdCall; External 'BEMAFI32.DLL' Name 'Bematech_FI_RelatorioTipo60AnaliticoMFD';
function Bematech_FI_ValorFormaPagamentoMFD( FormaPagamento: AnsiString; Valor: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ValorTotalizadorNaoFiscalMFD( Totalizador: AnsiString; Valor: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_IniciaFechamentoCupomMFD( AcrescimoDesconto: AnsiString; TipoAcrescimoDesconto: AnsiString; ValorAcrescimo: AnsiString; ValorDesconto: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_CancelaItemNaoFiscalMFD( NumeroItem: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_AcrescimoItemNaoFiscalMFD( NumeroItem: AnsiString; AcrescimoDesconto: AnsiString; TipoAcrescimoDesconto: AnsiString; ValorAcrescimoDesconto: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_CancelaAcrescimoNaoFiscalMFD( NumeroItem: AnsiString; AcrescimoDesconto: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ImprimeClicheMFD:Integer; StdCall; External 'BEMAFI32.DLL' Name 'Bematech_FI_ImprimeClicheMFD';
function Bematech_FI_ImprimeInformacaoChequeMFD( Posicao: Integer; Linhas: Integer; Mensagem: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';

// Função para Configuração dos Códigos de Barras
function Bematech_FI_ConfiguraCodigoBarrasMFD( Altura: Integer; Largura: Integer; PosicaoCaracteres: Integer; Fonte: Integer; Margem: Integer): Integer; StdCall; External 'BEMAFI32.DLL';

// Funções para Impressão dos Códigos de Barras
function Bematech_FI_CodigoBarrasUPCAMFD( Codigo: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_CodigoBarrasUPCEMFD( Codigo: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_CodigoBarrasEAN13MFD( Codigo: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_CodigoBarrasEAN8MFD( Codigo: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_CodigoBarrasCODE39MFD( Codigo: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_CodigoBarrasCODE93MFD( Codigo: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_CodigoBarrasCODE128MFD( Codigo: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_CodigoBarrasITFMFD( Codigo: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_CodigoBarrasCODABARMFD( Codigo: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_CodigoBarrasISBNMFD( Codigo: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_CodigoBarrasMSIMFD( Codigo: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_CodigoBarrasPLESSEYMFD( Codigo: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_CodigoBarrasPDF417MFD( NivelCorrecaoErros: Integer; Altura: Integer; Largura: Integer; Colunas: Integer; Codigo: AnsiString ): Integer; StdCall; External 'BEMAFI32.DLL';
}


procedure loadAllBematechFunctions;
procedure unloadBematechFunctions;

type
  TBematech_FI_ProgramaAliquota = function( Aliquota: AnsiString; ICMS_ISS: Integer ): integer; stdcall;
  TBematech_FI_AbreCupom = function( CGC_CPF: AnsiString ): Integer; stdcall;
  TBematech_FI_AbreCupomMFD = function(CGC: AnsiString; Nome: AnsiString; Endereco : AnsiString): Integer; StdCall;
  TBematech_FI_VendeItem = function( Codigo: AnsiString; Descricao: AnsiString; Aliquota: AnsiString; TipoQuantidade: AnsiString; Quantidade: AnsiString; CasasDecimais: Integer; ValorUnitario: AnsiString; TipoDesconto: AnsiString; Desconto: AnsiString): Integer; stdcall;
  TBematech_FI_VendeItemDepartamento = function( Codigo: AnsiString; Descricao: AnsiString; Aliquota: AnsiString; ValorUnitario: AnsiString; Quantidade: AnsiString; Acrescimo: AnsiString; Desconto: AnsiString; IndiceDepartamento: AnsiString; UnidadeMedida: AnsiString): Integer; stdcall;
  TBematech_FI_CancelaItemGenerico = function( NumeroItem: AnsiString ): Integer; StdCall;
  TBematech_FI_CancelaCupom = function: Integer; StdCall;
  TBematech_FI_IniciaFechamentoCupom = function( AcrescimoDesconto: AnsiString; TipoAcrescimoDesconto: AnsiString; ValorAcrescimoDesconto: AnsiString ): Integer; StdCall;
  TBematech_FI_EfetuaFormaPagamento = function( FormaPagamento: AnsiString; ValorFormaPagamento: AnsiString ): Integer; StdCall;
  TBematech_FI_EfetuaFormaPagamentoDescricaoForma = function( FormaPagamento: AnsiString; ValorFormaPagamento: AnsiString; DescricaoFormaPagto: AnsiString ): integer; StdCall;
  TBematech_FI_TerminaFechamentoCupom = function( Mensagem: AnsiString): Integer; StdCall;
  TBematech_FI_AumentaDescricaoItem = function( Descricao: AnsiString ): Integer; StdCall;
  TBematech_FI_LeituraX = function: Integer; StdCall;
  TBematech_FI_ReducaoZ = function( Data: AnsiString; Hora: AnsiString ): Integer; StdCall;
  TBematech_FI_NumeroSerie = function( NumeroSerie: AnsiString ): Integer; StdCall;
  TBematech_FI_NumeroSerieMFD = function(NumeroSerie : AnsiString): Integer; StdCall;
  TBematech_FI_NumeroCupom = function( NumeroCupom: AnsiString ): Integer; StdCall;
  TBematech_FI_FlagsFiscais = function( Var Flag: Integer ): Integer; StdCall;
  TBematech_FI_DataHoraReducao = function( Data: AnsiString; Hora: AnsiString ): Integer; StdCall;
  TBematech_FI_DataMovimento = function(Data: AnsiString): Integer; StdCall;
  TBematech_FI_RetornoAliquotas = function( Aliquotas: AnsiString ): Integer; StdCall;
  TBematech_FI_AbrePortaSerial = function: Integer; StdCall;
  TBematech_FI_RetornoImpressora = function( Var ACK: Integer; Var ST1: Integer; Var ST2: Integer ): Integer; StdCall;
  TBematech_FI_AberturaDoDia = function( ValorCompra: AnsiString; FormaPagamento: AnsiString ): Integer; StdCall;
  TBematech_FI_FechamentoDoDia = function: Integer; StdCall;
  TBematech_FI_ImprimeConfiguracoesImpressora = function:Integer; StdCall;
  TBematech_FI_VerificaImpressoraLigada = function: Integer; StdCall;
  TBematech_FI_ResetaImpressora = function : Integer; StdCall;
  TBematech_FI_LeituraMemoriaFiscalData = function( DataInicial: AnsiString; DataFinal: AnsiString ): Integer; StdCall;
  TBematech_FI_IniciaModoTEF = function : Integer; StdCall;
  TBematech_FI_AbreComprovanteNaoFiscalVinculado = function( FormaPagamento: AnsiString; Valor: AnsiString; NumeroCupom: AnsiString ): Integer; StdCall;
  TBematech_FI_UsaComprovanteNaoFiscalVinculadoTEF = function( Texto: AnsiString ): Integer; StdCall;
  TBematech_FI_FechaComprovanteNaoFiscalVinculado = function : Integer; StdCall;
  TBematech_FI_RelatorioGerencialTEF = function ( Texto: AnsiString ): Integer; StdCall;
  TBematech_FI_FinalizaModoTEF = function : Integer; StdCall;
  TBematech_FI_SegundaViaNaoFiscalVinculadoMFD = function : Integer; StdCall;
  TBematech_FI_EstornoNaoFiscalVinculadoMFD = function (CGC, Nome, Endereco : AnsiString): Integer; StdCall;
  TBematech_FI_RelatorioGerencial = function( Texto: AnsiString ): Integer; StdCall;
  TBematech_FI_FechaRelatorioGerencial = function: Integer; StdCall;
  TBematech_FI_StatusEstendidoMFD = function(var iStatus: integer): integer; StdCall;
  TBematech_FI_LeituraMemoriaFiscalSerialReducao = function(ReducaoInicial: AnsiString; ReducaoFinal: AnsiString): Integer; StdCall;
  TBematech_FI_RetornoImpressoraMFD = function(Var ACK: Integer; Var ST1: Integer; Var ST2: Integer; Var ST3: Integer ): Integer; StdCall;
  TBematech_FI_RelatorioSintegraMFD = function( iRelatorios : Integer;
                                           cArquivo    : AnsiString;
                                           cMes        : AnsiString;
                                           cAno        : AnsiString;
                                           cRazaoSocial: AnsiString;
                                           cEndereco   : AnsiString;
                                           cNumero     : AnsiString;
                                           cComplemento: AnsiString;
                                           cBairro     : AnsiString;
                                           cCidade     : AnsiString;
                                           cCEP        : AnsiString;
                                           cTelefone   : AnsiString;
                                           cFax        : AnsiString;
                                           cContato    : AnsiString ): Integer; StdCall;
  TBematech_FI_DadosUltimaReducaoMFD = function(DadosReducao : AnsiString): Integer; StdCall;
  TBematech_FI_LeituraMemoriaFiscalDataMFD = function(DataInicial, DataFinal, FlagLeitura : AnsiString): Integer; StdCall;
  TBematech_FI_LeituraMemoriaFiscalSerialDataMFD = function(DataInicial, DataFinal, FlagLeitura : AnsiString): Integer; StdCall;
  TBematech_FI_LeituraMemoriaFiscalSerialDataPAFECF = function(DataInicial, DataFinal, FlagLeitura, chavePublica, chavePrivada : AnsiString): Integer; StdCall;
  TBematech_FI_LeituraMemoriaFiscalReducaoMFD = function(ReducaoInicial, ReducaoFinal, FlagLeitura : AnsiString): Integer; StdCall;
  TBematech_FI_LeituraMemoriaFiscalSerialReducaoMFD = function(ReducaoInicial, ReducaoFinal, FlagLeitura : AnsiString): Integer; StdCall;
  TBematech_FI_LeituraMemoriaFiscalSerialReducaoPAFECF = function(ReducaoInicial, ReducaoFinal, FlagLeitura, chavePublica, chavePrivada : AnsiString): Integer; StdCall;
  TBematech_FI_HabilitaDesabilitaRetornoEstendidoMFD = function(FlagRetorno : AnsiString): Integer; StdCall;
  TBematech_FI_ArquivoMFD = function(ArquivoOrigem, DadoInicial, DadoFinal, TipoDownload, Usuario: AnsiString; TipoGeracao: integer; ChavePublica, ChavePrivada: AnsiString; UnicoArquivo: integer): integer; StdCall;
  TBematech_FI_EspelhoMFD = function(NomeArquivo, DataInicial, DataFinal, TipoDownload, Usuario, ChavePublica, ChavePrivada: AnsiString): integer; StdCall;
  TBematech_FI_DownloadMF = function ( Arquivo: AnsiString ): Integer; StdCall;
  TBematech_FI_DownloadMFD = function( Arquivo: AnsiString; TipoDownload: AnsiString; ParametroInicial: AnsiString; ParametroFinal: AnsiString; UsuarioECF: AnsiString ): Integer; StdCall;
  TBematech_FI_FormatoDadosMFD = function ( ArquivoOrigem: AnsiString; ArquivoDestino: AnsiString; TipoFormato: AnsiString; TipoDownload: AnsiString; ParametroInicial: AnsiString; ParametroFinal: AnsiString; UsuarioECF: AnsiString ): Integer; StdCall;
  TBematech_FI_DataHoraImpressora = function(Data: AnsiString; Hora: AnsiString): Integer; StdCall;
  TBematech_FI_GrandeTotal = function(GrandeTotal: AnsiString): Integer; StdCall;
  TBematech_FI_SubTotal = function(SubTotal: AnsiString ): Integer; StdCall;
  TBematech_FI_LeituraXSerial = function: Integer; StdCall;
  TBematech_FI_VersaoFirmware = function(VersaoFirmware: AnsiString ): Integer; StdCall;
  TBematech_FI_VersaoFirmwareMFD = function(VersaoFirmware: AnsiString ): Integer; StdCall;
  TBematech_FI_CGC_IE = function(CGC: AnsiString; IE: AnsiString ): Integer; StdCall;
  TBematech_FI_DataHoraGravacaoUsuarioSWBasicoMFAdicional = function(DataHoraUsuario, DataHoraSWBasico, MFAdicional: AnsiString): integer; StdCall;
  TBematech_FI_Sangria = function( Valor: AnsiString ): Integer; StdCall;
  TBematech_FI_Suprimento = function( Valor: AnsiString; FormaPagamento: AnsiString ): Integer; StdCall;
  TBematech_FI_DataHoraUltimoDocumentoMFD = function(cDataHora: AnsiString): integer; StdCall;
  TBematech_FI_ContadorRelatoriosGerenciaisMFD = function(Relatorios : AnsiString): Integer; StdCall;
  TBematech_FI_NumeroOperacoesNaoFiscais = function(NumeroOperacoes: AnsiString): Integer; StdCall;
  TBematech_FI_ContadorComprovantesCreditoMFD = function(Comprovantes : AnsiString): Integer; StdCall;
  TBematech_FI_VerificaAliquotasIss = function(Flag: AnsiString): Integer; StdCall;
  TBematech_FI_ContadorCupomFiscalMFD = function (CuponsEmitidos : AnsiString): Integer; StdCall;
  TBematech_FI_VerificaEstadoImpressora = function ( Var ACK: Integer; Var ST1: Integer; Var ST2: Integer ): Integer; StdCall;
  TBematech_FI_VerificaEstadoImpressoraMFD = function ( Var ACK: Integer; Var ST1: Integer; Var ST2: Integer; Var ST3: Integer ): Integer; StdCall;
  TBematech_FI_AcionaGaveta = function : Integer; StdCall;
  TBematech_FI_VerificaFormasPagamento = function(Formas: AnsiString): Integer; StdCall;
  TBematech_FI_UltimoItemVendido = function( NumeroItem: AnsiString ): Integer; StdCall;



var
  Bematech_FI_ProgramaAliquota: TBematech_FI_ProgramaAliquota;
  Bematech_FI_AbreCupom: TBematech_FI_AbreCupom;
  Bematech_FI_AbreCupomMFD: TBematech_FI_AbreCupomMFD;
  Bematech_FI_VendeItem: TBematech_FI_VendeItem;
  Bematech_FI_VendeItemDepartamento: TBematech_FI_VendeItemDepartamento;
  Bematech_FI_CancelaItemGenerico: TBematech_FI_CancelaItemGenerico;
  Bematech_FI_CancelaCupom: TBematech_FI_CancelaCupom;
  Bematech_FI_IniciaFechamentoCupom: TBematech_FI_IniciaFechamentoCupom;
  Bematech_FI_EfetuaFormaPagamento: TBematech_FI_EfetuaFormaPagamento;
  Bematech_FI_EfetuaFormaPagamentoDescricaoForma: TBematech_FI_EfetuaFormaPagamentoDescricaoForma;
  Bematech_FI_TerminaFechamentoCupom: TBematech_FI_TerminaFechamentoCupom;
  Bematech_FI_AumentaDescricaoItem: TBematech_FI_AumentaDescricaoItem;
  Bematech_FI_LeituraX: TBematech_FI_LeituraX;
  Bematech_FI_ReducaoZ: TBematech_FI_ReducaoZ;
  Bematech_FI_NumeroSerie: TBematech_FI_NumeroSerie;
  Bematech_FI_NumeroSerieMFD: TBematech_FI_NumeroSerieMFD;
  Bematech_FI_NumeroCupom: TBematech_FI_NumeroCupom;
  Bematech_FI_FlagsFiscais: TBematech_FI_FlagsFiscais;
  Bematech_FI_DataHoraReducao: TBematech_FI_DataHoraReducao;
  Bematech_FI_DataMovimento: TBematech_FI_DataMovimento;
  Bematech_FI_RetornoAliquotas: TBematech_FI_RetornoAliquotas;
  Bematech_FI_AbrePortaSerial: TBematech_FI_AbrePortaSerial;
  Bematech_FI_RetornoImpressora: TBematech_FI_RetornoImpressora;
  Bematech_FI_AberturaDoDia: TBematech_FI_AberturaDoDia;
  Bematech_FI_FechamentoDoDia: TBematech_FI_FechamentoDoDia;
  Bematech_FI_ImprimeConfiguracoesImpressora: TBematech_FI_ImprimeConfiguracoesImpressora;
  Bematech_FI_VerificaImpressoraLigada: TBematech_FI_VerificaImpressoraLigada;
  Bematech_FI_ResetaImpressora: TBematech_FI_ResetaImpressora;
  Bematech_FI_LeituraMemoriaFiscalData: TBematech_FI_LeituraMemoriaFiscalData;
  Bematech_FI_IniciaModoTEF: TBematech_FI_IniciaModoTEF;
  Bematech_FI_AbreComprovanteNaoFiscalVinculado: TBematech_FI_AbreComprovanteNaoFiscalVinculado;
  Bematech_FI_UsaComprovanteNaoFiscalVinculadoTEF: TBematech_FI_UsaComprovanteNaoFiscalVinculadoTEF;
  Bematech_FI_FechaComprovanteNaoFiscalVinculado: TBematech_FI_FechaComprovanteNaoFiscalVinculado;
  Bematech_FI_RelatorioGerencialTEF: TBematech_FI_RelatorioGerencialTEF;
  Bematech_FI_FinalizaModoTEF: TBematech_FI_FinalizaModoTEF;
  Bematech_FI_SegundaViaNaoFiscalVinculadoMFD: TBematech_FI_SegundaViaNaoFiscalVinculadoMFD;
  Bematech_FI_EstornoNaoFiscalVinculadoMFD: TBematech_FI_EstornoNaoFiscalVinculadoMFD;
  Bematech_FI_RelatorioGerencial: TBematech_FI_RelatorioGerencial;
  Bematech_FI_FechaRelatorioGerencial: TBematech_FI_FechaRelatorioGerencial;
  Bematech_FI_StatusEstendidoMFD: TBematech_FI_StatusEstendidoMFD;
  Bematech_FI_LeituraMemoriaFiscalSerialReducao: TBematech_FI_LeituraMemoriaFiscalSerialReducao;
  Bematech_FI_RetornoImpressoraMFD: TBematech_FI_RetornoImpressoraMFD;
  Bematech_FI_RelatorioSintegraMFD: TBematech_FI_RelatorioSintegraMFD;
  Bematech_FI_DadosUltimaReducaoMFD: TBematech_FI_DadosUltimaReducaoMFD;
  Bematech_FI_LeituraMemoriaFiscalDataMFD: TBematech_FI_LeituraMemoriaFiscalDataMFD;
  Bematech_FI_LeituraMemoriaFiscalSerialDataMFD: TBematech_FI_LeituraMemoriaFiscalDataMFD;
  Bematech_FI_LeituraMemoriaFiscalSerialDataPAFECF: TBematech_FI_LeituraMemoriaFiscalSerialDataPAFECF;
  Bematech_FI_LeituraMemoriaFiscalReducaoMFD: TBematech_FI_LeituraMemoriaFiscalReducaoMFD;
  Bematech_FI_LeituraMemoriaFiscalSerialReducaoMFD: TBematech_FI_LeituraMemoriaFiscalReducaoMFD;
  Bematech_FI_LeituraMemoriaFiscalSerialReducaoPAFECF: TBematech_FI_LeituraMemoriaFiscalSerialReducaoPAFECF;
  Bematech_FI_HabilitaDesabilitaRetornoEstendidoMFD: TBematech_FI_HabilitaDesabilitaRetornoEstendidoMFD;
  Bematech_FI_ArquivoMFD: TBematech_FI_ArquivoMFD;
  Bematech_FI_EspelhoMFD: TBematech_FI_EspelhoMFD;
  Bematech_FI_DownloadMF: TBematech_FI_DownloadMF;
  Bematech_FI_DownloadMFD: TBematech_FI_DownloadMFD;
  Bematech_FI_FormatoDadosMFD: TBematech_FI_FormatoDadosMFD;
  Bematech_FI_DataHoraImpressora: TBematech_FI_DataHoraImpressora;
  Bematech_FI_SubTotal: TBematech_FI_SubTotal;
  Bematech_FI_LeituraXSerial: TBematech_FI_LeituraXSerial;
  Bematech_FI_VersaoFirmware: TBematech_FI_VersaoFirmware;
  Bematech_FI_VersaoFirmwareMFD: TBematech_FI_VersaoFirmwareMFD;
  Bematech_FI_CGC_IE: TBematech_FI_CGC_IE;
  Bematech_FI_GrandeTotal: TBematech_FI_GrandeTotal;
  Bematech_FI_DataHoraGravacaoUsuarioSWBasicoMFAdicional: TBematech_FI_DataHoraGravacaoUsuarioSWBasicoMFAdicional;
  Bematech_FI_Sangria: TBematech_FI_Sangria;
  Bematech_FI_Suprimento: TBematech_FI_Suprimento;
  Bematech_FI_DataHoraUltimoDocumentoMFD: TBematech_FI_DataHoraUltimoDocumentoMFD;
  Bematech_FI_ContadorRelatoriosGerenciaisMFD: TBematech_FI_ContadorRelatoriosGerenciaisMFD;
  Bematech_FI_NumeroOperacoesNaoFiscais: TBematech_FI_NumeroOperacoesNaoFiscais;
  Bematech_FI_ContadorComprovantesCreditoMFD: TBematech_FI_ContadorComprovantesCreditoMFD;
  Bematech_FI_VerificaAliquotasIss: TBematech_FI_VerificaAliquotasIss;
  Bematech_FI_ContadorCupomFiscalMFD: TBematech_FI_ContadorCupomFiscalMFD;
  Bematech_FI_VerificaEstadoImpressora: TBematech_FI_VerificaEstadoImpressora;
  Bematech_FI_VerificaEstadoImpressoraMFD: TBematech_FI_VerificaEstadoImpressoraMFD;
  Bematech_FI_AcionaGaveta: TBematech_FI_AcionaGaveta;
  Bematech_FI_VerificaFormasPagamento: TBematech_FI_VerificaFormasPagamento;
  Bematech_FI_UltimoItemVendido: TBematech_FI_UltimoItemVendido;

var
  DLLHandle: THandle;

implementation

procedure loadAllBematechFunctions;
begin
  DLLHandle := LoadLibrary('BEMAFI32.DLL');
  if DLLHandle = 0 then
    raise Exception.create('Não foi possível carregar a DLL da impressora fiscal Bematech' + #13#10 + 'Entre em contato com o suporte.');
  @Bematech_FI_ProgramaAliquota := GetProcAddress(DLLHandle, 'Bematech_FI_ProgramaAliquota');
  @Bematech_FI_AbreCupom := GetProcAddress(DLLHandle, 'Bematech_FI_AbreCupom');
  @Bematech_FI_AbreCupomMFD := GetProcAddress(DLLHandle, 'Bematech_FI_AbreCupomMFD');
  @Bematech_FI_VendeItem := GetProcAddress(DLLHandle, 'Bematech_FI_VendeItem');
  @Bematech_FI_VendeItemDepartamento := GetProcAddress(DLLHandle, 'Bematech_FI_VendeItemDepartamento');
  @Bematech_FI_CancelaItemGenerico := GetProcAddress(DLLHandle, 'Bematech_FI_CancelaItemGenerico');
  @Bematech_FI_CancelaCupom := GetProcAddress(DLLHandle, 'Bematech_FI_CancelaCupom');
  @Bematech_FI_IniciaFechamentoCupom := GetProcAddress(DLLHandle, 'Bematech_FI_IniciaFechamentoCupom');
  @Bematech_FI_EfetuaFormaPagamento := GetProcAddress(DLLHandle, 'Bematech_FI_EfetuaFormaPagamento');
  @Bematech_FI_EfetuaFormaPagamentoDescricaoForma := GetProcAddress(DLLHandle, 'Bematech_FI_EfetuaFormaPagamentoDescricaoForma');
  @Bematech_FI_TerminaFechamentoCupom := GetProcAddress(DLLHandle, 'Bematech_FI_TerminaFechamentoCupom');
  @Bematech_FI_AumentaDescricaoItem := GetProcAddress(DLLHandle, 'Bematech_FI_AumentaDescricaoItem');
  @Bematech_FI_LeituraX := GetProcAddress(DLLHandle, 'Bematech_FI_LeituraX');
  @Bematech_FI_ReducaoZ := GetProcAddress(DLLHandle, 'Bematech_FI_ReducaoZ');
  @Bematech_FI_NumeroSerie := GetProcAddress(DLLHandle, 'Bematech_FI_NumeroSerie');
  @Bematech_FI_NumeroSerieMFD := GetProcAddress(DLLHandle, 'Bematech_FI_NumeroSerieMFD');
  @Bematech_FI_NumeroCupom := GetProcAddress(DLLHandle, 'Bematech_FI_NumeroCupom');
  @Bematech_FI_FlagsFiscais := GetProcAddress(DLLHandle, 'Bematech_FI_FlagsFiscais');
  @Bematech_FI_DataHoraReducao := GetProcAddress(DLLHandle, 'Bematech_FI_DataHoraReducao');
  @Bematech_FI_DataMovimento := GetProcAddress(DLLHandle, 'Bematech_FI_DataMovimento');
  @Bematech_FI_RetornoAliquotas := GetProcAddress(DLLHandle, 'Bematech_FI_RetornoAliquotas');
  @Bematech_FI_AbrePortaSerial := GetProcAddress(DLLHandle, 'Bematech_FI_AbrePortaSerial');
  @Bematech_FI_RetornoImpressora := GetProcAddress(DLLHandle, 'Bematech_FI_RetornoImpressora');
  @Bematech_FI_AberturaDoDia := GetProcAddress(DLLHandle, 'Bematech_FI_AberturaDoDia');
  @Bematech_FI_FechamentoDoDia := GetProcAddress(DLLHandle, 'Bematech_FI_FechamentoDoDia');
  @Bematech_FI_ImprimeConfiguracoesImpressora := GetProcAddress(DLLHandle, 'Bematech_FI_ImprimeConfiguracoesImpressora');
  @Bematech_FI_VerificaImpressoraLigada := GetProcAddress(DLLHandle, 'Bematech_FI_VerificaImpressoraLigada');
  @Bematech_FI_ResetaImpressora := GetProcAddress(DLLHandle, 'Bematech_FI_ResetaImpressora');
  @Bematech_FI_LeituraMemoriaFiscalData := GetProcAddress(DLLHandle, 'Bematech_FI_LeituraMemoriaFiscalData');
  @Bematech_FI_IniciaModoTEF := GetProcAddress(DLLHandle, 'Bematech_FI_IniciaModoTEF');
  @Bematech_FI_AbreComprovanteNaoFiscalVinculado := GetProcAddress(DLLHandle, 'Bematech_FI_AbreComprovanteNaoFiscalVinculado');
  @Bematech_FI_UsaComprovanteNaoFiscalVinculadoTEF := GetProcAddress(DLLHandle, 'Bematech_FI_UsaComprovanteNaoFiscalVinculadoTEF');
  @Bematech_FI_FechaComprovanteNaoFiscalVinculado := GetProcAddress(DLLHandle, 'Bematech_FI_FechaComprovanteNaoFiscalVinculado');
  @Bematech_FI_RelatorioGerencialTEF := GetProcAddress(DLLHandle, 'Bematech_FI_RelatorioGerencialTEF');
  @Bematech_FI_FinalizaModoTEF := GetProcAddress(DLLHandle, 'Bematech_FI_FinalizaModoTEF');
  @Bematech_FI_SegundaViaNaoFiscalVinculadoMFD := GetProcAddress(DLLHandle, 'Bematech_FI_SegundaViaNaoFiscalVinculadoMFD');
  @Bematech_FI_EstornoNaoFiscalVinculadoMFD :=  GetProcAddress(DLLHandle, 'Bematech_FI_EstornoNaoFiscalVinculadoMFD');
  @Bematech_FI_RelatorioGerencial :=  GetProcAddress(DLLHandle, 'Bematech_FI_RelatorioGerencial');
  @Bematech_FI_FechaRelatorioGerencial := GetProcAddress(DLLHandle, 'Bematech_FI_FechaRelatorioGerencial');
  @Bematech_FI_StatusEstendidoMFD := GetProcAddress(DLLHandle, 'Bematech_FI_StatusEstendidoMFD');
  @Bematech_FI_LeituraMemoriaFiscalSerialReducao := GetProcAddress(DLLHandle, 'Bematech_FI_LeituraMemoriaFiscalSerialReducao');
  @Bematech_FI_RetornoImpressoraMFD := GetProcAddress(DLLHandle, 'Bematech_FI_RetornoImpressoraMFD');
  @Bematech_FI_RelatorioSintegraMFD := GetProcAddress(DLLHandle, 'Bematech_FI_RelatorioSintegraMFD');
  @Bematech_FI_DadosUltimaReducaoMFD := GetProcAddress(DLLHandle, 'Bematech_FI_DadosUltimaReducaoMFD');
  @Bematech_FI_LeituraMemoriaFiscalDataMFD := GetProcAddress(DLLHandle, 'Bematech_FI_LeituraMemoriaFiscalDataMFD');
  @Bematech_FI_LeituraMemoriaFiscalSerialDataMFD := GetProcAddress(DLLHandle, 'Bematech_FI_LeituraMemoriaFiscalSerialDataMFD');
  @Bematech_FI_LeituraMemoriaFiscalSerialDataPAFECF := GetProcAddress(DLLHandle, 'Bematech_FI_LeituraMemoriaFiscalSerialDataPAFECF');
  @Bematech_FI_LeituraMemoriaFiscalReducaoMFD := GetProcAddress(DLLHandle, 'Bematech_FI_LeituraMemoriaFiscalReducaoMFD');
  @Bematech_FI_LeituraMemoriaFiscalSerialReducaoMFD := GetProcAddress(DLLHandle, 'Bematech_FI_LeituraMemoriaFiscalSerialReducaoMFD');
  @Bematech_FI_LeituraMemoriaFiscalSerialReducaoPAFECF := GetProcAddress(DLLHandle, 'Bematech_FI_LeituraMemoriaFiscalSerialReducaoPAFECF');
  @Bematech_FI_HabilitaDesabilitaRetornoEstendidoMFD := GetProcAddress(DLLHandle, 'Bematech_FI_HabilitaDesabilitaRetornoEstendidoMFD');
  @Bematech_FI_ArquivoMFD := GetProcAddress(DLLHandle, 'Bematech_FI_ArquivoMFD');
  @Bematech_FI_EspelhoMFD := GetProcAddress(DLLHandle, 'Bematech_FI_EspelhoMFD');
  @Bematech_FI_RetornoImpressoraMFD := GetProcAddress(DLLHandle, 'Bematech_FI_RetornoImpressoraMFD');
  @Bematech_FI_DownloadMF := GetProcAddress(DLLHandle, 'Bematech_FI_DownloadMF');
  @Bematech_FI_DownloadMFD := GetProcAddress(DLLHandle, 'Bematech_FI_DownloadMFD');
  @Bematech_FI_FormatoDadosMFD := GetProcAddress(DLLHandle, 'Bematech_FI_FormatoDadosMFD');
  @Bematech_FI_DataHoraImpressora := GetProcAddress(DLLHandle, 'Bematech_FI_DataHoraImpressora');
  @Bematech_FI_SubTotal := GetProcAddress(DLLHandle, 'Bematech_FI_SubTotal');
  @Bematech_FI_LeituraXSerial := GetProcAddress(DLLHandle, 'Bematech_FI_LeituraXSerial');
  @Bematech_FI_VersaoFirmware := GetProcAddress(DLLHandle, 'Bematech_FI_VersaoFirmware');
  @Bematech_FI_VersaoFirmwareMFD := GetProcAddress(DLLHandle, 'Bematech_FI_VersaoFirmwareMFD');
  @Bematech_FI_CGC_IE := GetProcAddress(DLLHandle, 'Bematech_FI_CGC_IE');
  @Bematech_FI_GrandeTotal := GetProcAddress(DLLHandle, 'Bematech_FI_GrandeTotal');
  @Bematech_FI_DataHoraGravacaoUsuarioSWBasicoMFAdicional := GetProcAddress(DLLHandle, 'Bematech_FI_DataHoraGravacaoUsuarioSWBasicoMFAdicional');
  @Bematech_FI_Sangria := GetProcAddress(DLLHandle, 'Bematech_FI_Sangria');
  @Bematech_FI_Suprimento := GetProcAddress(DLLHandle, 'Bematech_FI_Suprimento');
  @Bematech_FI_DataHoraUltimoDocumentoMFD := GetProcAddress(DLLHandle, 'Bematech_FI_DataHoraUltimoDocumentoMFD');
  @Bematech_FI_ContadorRelatoriosGerenciaisMFD := GetProcAddress(DLLHandle, 'Bematech_FI_ContadorRelatoriosGerenciaisMFD');
  @Bematech_FI_NumeroOperacoesNaoFiscais := GetProcAddress(DLLHandle, 'Bematech_FI_NumeroOperacoesNaoFiscais');
  @Bematech_FI_ContadorComprovantesCreditoMFD := GetProcAddress(DLLHandle, 'Bematech_FI_ContadorComprovantesCreditoMFD');
  @Bematech_FI_VerificaAliquotasIss := GetProcAddress(DLLHandle, 'Bematech_FI_VerificaAliquotasIss');
  @Bematech_FI_ContadorCupomFiscalMFD := GetProcAddress(DLLHandle, 'Bematech_FI_ContadorCupomFiscalMFD');
  @Bematech_FI_VerificaEstadoImpressora := GetProcAddress(DLLHandle, 'Bematech_FI_VerificaEstadoImpressora');
  @Bematech_FI_VerificaEstadoImpressoraMFD := GetProcAddress(DLLHandle, 'Bematech_FI_VerificaEstadoImpressoraMFD');
  @Bematech_FI_AcionaGaveta := GetProcAddress(DLLHandle, 'Bematech_FI_AcionaGaveta');
  @Bematech_FI_VerificaFormasPagamento := GetProcAddress(DLLHandle, 'Bematech_FI_VerificaFormasPagamento');
  @Bematech_FI_UltimoItemVendido := GetProcAddress(DLLHandle, 'Bematech_FI_UltimoItemVendido');
end;

procedure unloadBematechFunctions;
begin
  FreeLibrary(DLLHandle);
end;

end.
