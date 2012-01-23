unit Bematech;

interface

uses Windows, SysUtils;

//As funções abaixo estão comentadas pois foi feito um esquema para load dinâmico das funções
//  somente quando necessário. Se precisar de alguma destas funções deve-se escrevê-la no
//  esquema em que as outras estão escritas mais abaixo

{// Funções de Inicialização
function Bematech_FI_AlteraSimboloMoeda( SimboloMoeda: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ProgramaHorarioVerao: Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_NomeiaDepartamento( Indice: Integer; Departamento: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_NomeiaTotalizadorNaoSujeitoIcms( Indice: Integer; Totalizador: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ProgramaArredondamento: Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ProgramaTruncamento: Integer; StdCall; External 'BEMAFI32.DLL' Name 'Bematech_FI_ProgramaTruncamento';
function Bematech_FI_LinhasEntreCupons( Linhas: Integer ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_EspacoEntreLinhas( Dots: Integer ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ForcaImpactoAgulhas( ForcaImpacto: Integer ): Integer; StdCall; External 'BEMAFI32.DLL';

// Funções do Cupom Fiscal
function Bematech_FI_CancelaItemAnterior: Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_FechaCupomResumido( FormaPagamento: String; Mensagem: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_FechaCupom( FormaPagamento: String; AcrescimoDesconto: String; TipoAcrescimoDesconto: String; ValorAcrescimoDesconto: String; ValorPago: String; Mensagem: String): Integer; StdCall; External 'BEMAFI32.DLL';

function Bematech_FI_EstornoFormasPagamento( FormaOrigem: String; FormaDestino: String; Valor: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_UsaUnidadeMedida( UnidadeMedida: String ): Integer; StdCall; External 'BEMAFI32.DLL';

// Funções dos Relatórios Fiscais
function Bematech_FI_LeituraMemoriaFiscalData( DataInicial: String; DataFinal: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_LeituraMemoriaFiscalReducao( ReducaoInicial: String; ReducaoFinal: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_LeituraMemoriaFiscalSerialData( DataInicial: String; DataFinal: String ): Integer; StdCall; External 'BEMAFI32.DLL';
}
// Funções das Operações Não Fiscais
//function Bematech_FI_RecebimentoNaoFiscal( IndiceTotalizador: String; Valor: String; FormaPagamento: String ): Integer; StdCall; External 'BEMAFI32.DLL';
//function Bematech_FI_AbreComprovanteNaoFiscalVinculado( FormaPagamento: String; Valor: String; NumeroCupom: String ): Integer; StdCall; External 'BEMAFI32.DLL';
//function Bematech_FI_UsaComprovanteNaoFiscalVinculado( Texto: String ): Integer; StdCall; External 'BEMAFI32.DLL';
//function Bematech_FI_FechaComprovanteNaoFiscalVinculado: Integer; StdCall; External 'BEMAFI32.DLL';
//function Bematech_FI_Sangria( Valor: String ): Integer; StdCall; External 'BEMAFI32.DLL';
//function Bematech_FI_Suprimento( Valor: String; FormaPagamento: String ): Integer; StdCall; External 'BEMAFI32.DLL';
{
// Funções de Informações da Impressora
function Bematech_FI_ModeloImpressora( Modelo: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_SubTotal( SubTotal: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_LeituraXSerial: Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_VersaoFirmware( VersaoFirmware: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_CGC_IE( CGC: String; IE: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_GrandeTotal( GrandeTotal: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_Cancelamentos( ValorCancelamentos: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_Descontos( ValorDescontos: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_NumeroOperacoesNaoFiscais( NumeroOperacoes: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_NumeroCuponsCancelados( NumeroCancelamentos: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_NumeroIntervencoes( NumeroIntervencoes: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_NumeroReducoes( NumeroReducoes: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_NumeroSubstituicoesProprietario( NumeroSubstituicoes: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_UltimoItemVendido( NumeroItem: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ClicheProprietario( Cliche: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_NumeroCaixa( NumeroCaixa: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_NumeroLoja( NumeroLoja: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_SimboloMoeda( SimboloMoeda: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_MinutosLigada( Minutos: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_MinutosImprimindo( Minutos: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_VerificaModoOperacao( Modo: string ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_VerificaEpromConectada( Flag: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ValorPagoUltimoCupom( ValorCupom: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_DataHoraImpressora( Data: String; Hora: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ContadoresTotalizadoresNaoFiscais( Contadores: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_VerificaTotalizadoresNaoFiscais( Totalizadores: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_DataMovimento( Data: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_VerificaTruncamento( Flag: string ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_Acrescimos( ValorAcrescimos: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ContadorBilhetePassagem( ContadorPassagem: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_VerificaAliquotasIss( Flag: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_VerificaFormasPagamento( Formas: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_VerificaRecebimentoNaoFiscal( Recebimentos: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_VerificaDepartamentos( Departamentos: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_VerificaTipoImpressora( Var TipoImpressora: Integer ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_VerificaTotalizadoresParciais( Totalizadores: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_VerificaEstadoImpressora( Var ACK: Integer; Var ST1: Integer; Var ST2: Integer ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_DadosUltimaReducao( DadosReducao: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_MonitoramentoPapel( Var Linhas: Integer): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_VerificaIndiceAliquotasIss( Flag: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ValorFormaPagamento( FormaPagamento: String; Valor: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ValorTotalizadorNaoFiscal( Totalizador: String; Valor: String ): Integer; StdCall; External 'BEMAFI32.DLL';

// Funções de Autenticação e Gaveta de Dinheiro
function Bematech_FI_Autenticacao:Integer; StdCall; External 'BEMAFI32.DLL' Name 'Bematech_FI_Autenticacao';
function Bematech_FI_ProgramaCaracterAutenticacao( Parametros: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_AcionaGaveta:Integer; StdCall; External 'BEMAFI32.DLL' Name 'Bematech_FI_AcionaGaveta';
function Bematech_FI_VerificaEstadoGaveta( Var EstadoGaveta: Integer ): Integer; StdCall; External 'BEMAFI32.DLL';

// Funções para a Impressora Restaurante
function Bematech_FIR_AbreCupomRestaurante( Mesa: String; CGC_CPF: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FIR_RegistraVenda( Mesa: String; Codigo: String; Descricao: String; Aliquota: String; Quantidade: String; ValorUnitario: String; FlagAcrescimoDesconto: String; ValorAcrescimoDesconto: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FIR_CancelaVenda( Mesa: String; Codigo: String; Descricao: String; Aliquota: String; Quantidade: String; ValorUnitario: String; FlagAcrescimoDesconto: String; ValorAcrescimoDesconto: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FIR_ConferenciaMesa( Mesa: String; FlagAcrescimoDesconto: String; TipoAcrescimoDesconto: String; ValorAcrescimoDesconto: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FIR_AbreConferenciaMesa( Mesa: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FIR_FechaConferenciaMesa( FlagAcrescimoDesconto: String; TipoAcrescimoDesconto: String; ValorAcrescimoDesconto: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FIR_TransferenciaMesa( MesaOrigem: String; MesaDestino: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FIR_ContaDividida( NumeroCupons: String; ValorPago: String; CGC_CPF: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FIR_FechaCupomContaDividida( NumeroCupons: String; FlagAcrescimoDesconto: String; TipoAcrescimoDesconto: String; ValorAcrescimoDesconto: String; FormasPagamento: String; ValorFormasPagamento: String; ValorPagoCliente: String; CGC_CPF: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FIR_TransferenciaItem( MesaOrigem: String; Codigo: String; Descricao: String; Aliquota: String; Quantidade: String; ValorUnitario: String; FlagAcrescimoDesconto: String; ValorAcrescimoDesconto: String; MesaDestino: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FIR_RelatorioMesasAbertas( TipoRelatorio: Integer ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FIR_ImprimeCardapio: Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FIR_RelatorioMesasAbertasSerial: Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FIR_CardapioPelaSerial: Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FIR_RegistroVendaSerial( Mesa: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FIR_VerificaMemoriaLivre( Bytes: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FIR_FechaCupomRestaurante( FormaPagamento: String; FlagAcrescimoDesconto: String; TipoAcrescimoDesconto: String; ValorAcrescimoDesconto: String; ValorFormaPagto: String; Mensagem: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FIR_FechaCupomResumidoRestaurante( FormaPagamento: String; Mensagem: String ): Integer; StdCall; External 'BEMAFI32.DLL';

// Função para a Impressora Bilhete de Passagem
function Bematech_FI_AbreBilhetePassagem( ImprimeValorFinal: string; ImprimeEnfatizado: string; Embarque: string; Destino: string; Linha: string; Prefixo: string; Agente: string; Agencia: string; Data: string; Hora: string; Poltrona: string; Plataforma: string ): Integer; StdCall; External 'BEMAFI32.DLL';

// Funções de Impressão de Cheques
function Bematech_FI_ProgramaMoedaSingular( MoedaSingular: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ProgramaMoedaPlural( MoedaPlural: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_CancelaImpressaoCheque: Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_VerificaStatusCheque( Var StatusCheque: Integer ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ImprimeCheque( Banco: String; Valor: String; Favorecido: String; Cidade: String; Data: String; Mensagem: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_IncluiCidadeFavorecido( Cidade: String; Favorecido: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ImprimeCopiaCheque: Integer; StdCall; External 'BEMAFI32.DLL' Name 'Bematech_FI_ImprimeCopiaCheque';

// Outras Funções
function Bematech_FI_FechaPortaSerial: Integer; StdCall; External 'BEMAFI32.DLL' Name 'Bematech_FI_FechaPortaSerial';
function Bematech_FI_MapaResumo:Integer; StdCall; External 'BEMAFI32.DLL' Name 'Bematech_FI_MapaResumo';
function Bematech_FI_ImprimeDepartamentos: Integer; StdCall; External 'BEMAFI32.DLL' Name 'Bematech_FI_ImprimeDepartamentos';
function Bematech_FI_RelatorioTipo60Analitico: Integer; StdCall; External 'BEMAFI32.DLL' Name 'Bematech_FI_RelatorioTipo60Analitico';
function Bematech_FI_RelatorioTipo60Mestre: Integer; StdCall; External 'BEMAFI32.DLL' Name 'Bematech_FI_RelatorioTipo60Mestre';
function Bematech_FI_ImpressaoCarne( Titulo, Percelas: string; Datas, Quantidade: integer; Texto, Cliente, RG_CPF, Cupom: string; Vias, Assina: integer ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_InfoBalanca( Porta: string; Modelo: integer; Peso, PrecoKilo, Total: string ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_DadosSintegra( DataInicio: string; DataFinal: string ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_VersaoDll( Versao: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_RegistrosTipo60: Integer; StdCall; External 'BEMAFI32.DLL' Name 'Bematech_FI_RegistrosTipo60';

// Funções da Impressora Fiscal MFD
function Bematech_FI_AbreCupomMFD(CGC: string; Nome: string; Endereco : string): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_CancelaCupomMFD(CGC, Nome, Endereco: string): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ProgramaFormaPagamentoMFD(FormaPagto, OperacaoTef: String) : Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_EfetuaFormaPagamentoMFD(FormaPagamento, ValorFormaPagamento, Parcelas, DescricaoFormaPagto: string): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_CupomAdicionalMFD(): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_AcrescimoDescontoItemMFD (Item, AcrescimoDesconto,TipoAcrescimoDesconto, ValorAcrescimoDesconto: string): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_NomeiaRelatorioGerencialMFD (Indice, Descricao : string): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_AutenticacaoMFD(Linhas, Texto : string) : Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_AbreComprovanteNaoFiscalVinculadoMFD(FormaPagamento, Valor, NumeroCupom, CGC, nome, Endereco : string): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ReimpressaoNaoFiscalVinculadoMFD() : Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_AbreRecebimentoNaoFiscalMFD(CGC, Nome, Endereco : string): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_EfetuaRecebimentoNaoFiscalMFD(IndiceTotalizador, ValorRecebimento : string): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_IniciaFechamentoRecebimentoNaoFiscalMFD(AcrescimoDesconto,TipoAcrescimoDesconto, ValorAcrescimo, ValorDesconto : string): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_FechaRecebimentoNaoFiscalMFD(Mensagem : string): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_CancelaRecebimentoNaoFiscalMFD(CGC, Nome, Endereco : string): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_AbreRelatorioGerencialMFD(Indice : string): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_UsaRelatorioGerencialMFD(Texto : string): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_UsaRelatorioGerencialMFDTEF(Texto : string): Integer; StdCall; External 'BEMAFI32.DLL';


function Bematech_FI_NumeroSerieMFD(NumeroSerie : string): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_VersaoFirmwareMFD(VersaoFirmware : string): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_CNPJMFD(CNPJ : string): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_InscricaoEstadualMFD(InscricaoEstadual : string): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_InscricaoMunicipalMFD(InscricaoMunicipal : string): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_TempoOperacionalMFD(TempoOperacional : string): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_MinutosEmitindoDocumentosFiscaisMFD(Minutos : string): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ContadoresTotalizadoresNaoFiscaisMFD(Contadores : string): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_VerificaTotalizadoresNaoFiscaisMFD(Totalizadores : string): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_VerificaFormasPagamentoMFD(FormasPagamento : string): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_VerificaRecebimentoNaoFiscalMFD(Recebimentos : string): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_VerificaRelatorioGerencialMFD(Relatorios : string): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ContadorComprovantesCreditoMFD(Comprovantes : string): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ContadorOperacoesNaoFiscaisCanceladasMFD(OperacoesCanceladas : string): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ContadorRelatoriosGerenciaisMFD (Relatorios : String): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ContadorCupomFiscalMFD(CuponsEmitidos : string): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ContadorFitaDetalheMFD(ContadorFita : string): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ComprovantesNaoFiscaisNaoEmitidosMFD(Comprovantes : string): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_NumeroSerieMemoriaMFD(NumeroSerieMFD : string): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_MarcaModeloTipoImpressoraMFD(Marca, Modelo, Tipo : string): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ReducoesRestantesMFD(Reducoes : string): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_VerificaTotalizadoresParciaisMFD(Totalizadores : string): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_LeituraChequeMFD(CodigoCMC7 : string): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ImprimeChequeMFD(NumeroBanco, Valor, Favorecido, Cidade, Data, Mensagem, ImpressaoVerso, Linhas : string): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_AbreBilhetePassagemMFD(Embarque, Destino, Linha, Agencia, Data, Hora, Poltrona, Plataforma, TipoPassagem: string ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_CancelaAcrescimoDescontoItemMFD( cFlag, cItem: string ): integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_SubTotalizaCupomMFD: integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_SubTotalizaRecebimentoMFD: integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_TotalLivreMFD( cMemoriaLivre: string ): integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_TamanhoTotalMFD( cTamanhoMFD: string ): integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_AcrescimoDescontoSubtotalRecebimentoMFD( cFlag, cTipo, cValor: string ): integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_AcrescimoDescontoSubtotalMFD( cFlag, cTipo, cValor: string): integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_CancelaAcrescimoDescontoSubtotalMFD( cFlag: string): integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_CancelaAcrescimoDescontoSubtotalRecebimentoMFD( cFlag: string ): integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_TotalizaCupomMFD: integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_TotalizaRecebimentoMFD: integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_PercentualLivreMFD( cMemoriaLivre: string ): integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_DataHoraUltimoDocumentoMFD( cDataHora: string ): integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_MapaResumoMFD:Integer; StdCall; External 'BEMAFI32.DLL' Name 'Bematech_FI_MapaResumoMFD';
function Bematech_FI_RelatorioTipo60AnaliticoMFD: Integer; StdCall; External 'BEMAFI32.DLL' Name 'Bematech_FI_RelatorioTipo60AnaliticoMFD';
function Bematech_FI_ValorFormaPagamentoMFD( FormaPagamento: String; Valor: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ValorTotalizadorNaoFiscalMFD( Totalizador: String; Valor: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_VerificaEstadoImpressoraMFD( Var ACK: Integer; Var ST1: Integer; Var ST2: Integer; Var ST3: Integer ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_IniciaFechamentoCupomMFD( AcrescimoDesconto: String; TipoAcrescimoDesconto: String; ValorAcrescimo: String; ValorDesconto: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_CancelaItemNaoFiscalMFD( NumeroItem: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_AcrescimoItemNaoFiscalMFD( NumeroItem: String; AcrescimoDesconto: String; TipoAcrescimoDesconto: String; ValorAcrescimoDesconto: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_CancelaAcrescimoNaoFiscalMFD( NumeroItem: String; AcrescimoDesconto: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_ImprimeClicheMFD:Integer; StdCall; External 'BEMAFI32.DLL' Name 'Bematech_FI_ImprimeClicheMFD';
function Bematech_FI_ImprimeInformacaoChequeMFD( Posicao: Integer; Linhas: Integer; Mensagem: String ): Integer; StdCall; External 'BEMAFI32.DLL';

// Função para Configuração dos Códigos de Barras
function Bematech_FI_ConfiguraCodigoBarrasMFD( Altura: Integer; Largura: Integer; PosicaoCaracteres: Integer; Fonte: Integer; Margem: Integer): Integer; StdCall; External 'BEMAFI32.DLL';

// Funções para Impressão dos Códigos de Barras
function Bematech_FI_CodigoBarrasUPCAMFD( Codigo: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_CodigoBarrasUPCEMFD( Codigo: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_CodigoBarrasEAN13MFD( Codigo: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_CodigoBarrasEAN8MFD( Codigo: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_CodigoBarrasCODE39MFD( Codigo: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_CodigoBarrasCODE93MFD( Codigo: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_CodigoBarrasCODE128MFD( Codigo: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_CodigoBarrasITFMFD( Codigo: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_CodigoBarrasCODABARMFD( Codigo: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_CodigoBarrasISBNMFD( Codigo: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_CodigoBarrasMSIMFD( Codigo: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_CodigoBarrasPLESSEYMFD( Codigo: String ): Integer; StdCall; External 'BEMAFI32.DLL';
function Bematech_FI_CodigoBarrasPDF417MFD( NivelCorrecaoErros: Integer; Altura: Integer; Largura: Integer; Colunas: Integer; Codigo: String ): Integer; StdCall; External 'BEMAFI32.DLL';
}


procedure loadAllBematechFunctions;

type
  TBematech_FI_ProgramaAliquota = function( Aliquota: String; ICMS_ISS: Integer ): integer; stdcall;
  TBematech_FI_AbreCupom = function( CGC_CPF: String ): Integer; stdcall;
  TBematech_FI_VendeItem = function( Codigo: String; Descricao: String; Aliquota: String; TipoQuantidade: String; Quantidade: String; CasasDecimais: Integer; ValorUnitario: String; TipoDesconto: String; Desconto: String): Integer; stdcall;
  TBematech_FI_VendeItemDepartamento = function( Codigo: String; Descricao: String; Aliquota: String; ValorUnitario: String; Quantidade: String; Acrescimo: String; Desconto: String; IndiceDepartamento: String; UnidadeMedida: String): Integer; stdcall;
  TBematech_FI_CancelaItemGenerico = function( NumeroItem: String ): Integer; StdCall;
  TBematech_FI_CancelaCupom = function: Integer; StdCall;
  TBematech_FI_IniciaFechamentoCupom = function( AcrescimoDesconto: String; TipoAcrescimoDesconto: String; ValorAcrescimoDesconto: String ): Integer; StdCall;
  TBematech_FI_EfetuaFormaPagamento = function( FormaPagamento: String; ValorFormaPagamento: String ): Integer; StdCall;
  TBematech_FI_EfetuaFormaPagamentoDescricaoForma = function( FormaPagamento: string; ValorFormaPagamento: string; DescricaoFormaPagto: string ): integer; StdCall;
  TBematech_FI_TerminaFechamentoCupom = function( Mensagem: String): Integer; StdCall;
  TBematech_FI_AumentaDescricaoItem = function( Descricao: String ): Integer; StdCall;
  TBematech_FI_LeituraX = function: Integer; StdCall;
  TBematech_FI_ReducaoZ = function( Data: String; Hora: String ): Integer; StdCall;
  TBematech_FI_NumeroSerie = function( NumeroSerie: String ): Integer; StdCall;
  TBematech_FI_NumeroCupom = function( NumeroCupom: String ): Integer; StdCall;
  TBematech_FI_FlagsFiscais = function( Var Flag: Integer ): Integer; StdCall;
  TBematech_FI_DataHoraReducao = function( Data: String; Hora: String ): Integer; StdCall;
  TBematech_FI_RetornoAliquotas = function( Aliquotas: String ): Integer; StdCall;
  TBematech_FI_AbrePortaSerial = function: Integer; StdCall;
  TBematech_FI_RetornoImpressora = function( Var ACK: Integer; Var ST1: Integer; Var ST2: Integer ): Integer; StdCall;
  TBematech_FI_AberturaDoDia = function( ValorCompra: string; FormaPagamento: string ): Integer; StdCall;
  TBematech_FI_FechamentoDoDia = function: Integer; StdCall;
  TBematech_FI_ImprimeConfiguracoesImpressora = function:Integer; StdCall;
  TBematech_FI_VerificaImpressoraLigada = function: Integer; StdCall;
  TBematech_FI_ResetaImpressora = function : Integer; StdCall;
  TBematech_FI_LeituraMemoriaFiscalData = function( DataInicial: String; DataFinal: String ): Integer; StdCall;
  TBematech_FI_IniciaModoTEF = function : Integer; StdCall;
  TBematech_FI_AbreComprovanteNaoFiscalVinculado = function( FormaPagamento: String; Valor: String; NumeroCupom: String ): Integer; StdCall;
  TBematech_FI_UsaComprovanteNaoFiscalVinculadoTEF = function( Texto: String ): Integer; StdCall;
  TBematech_FI_FechaComprovanteNaoFiscalVinculado = function : Integer; StdCall;
  TBematech_FI_RelatorioGerencialTEF = function ( Texto: String ): Integer; StdCall;
  TBematech_FI_FinalizaModoTEF = function : Integer; StdCall;
  TBematech_FI_SegundaViaNaoFiscalVinculadoMFD = function : Integer; StdCall;
  TBematech_FI_EstornoNaoFiscalVinculadoMFD = function (CGC, Nome, Endereco : string): Integer; StdCall;
  TBematech_FI_RelatorioGerencial = function( Texto: String ): Integer; StdCall;
  TBematech_FI_FechaRelatorioGerencial = function: Integer; StdCall;
  TBematech_FI_StatusEstendidoMFD = function(var iStatus: integer): integer; StdCall;
  TBematech_FI_LeituraMemoriaFiscalSerialReducao = function(ReducaoInicial: String; ReducaoFinal: String): Integer; StdCall;
  TBematech_FI_RetornoImpressoraMFD = function(Var ACK: Integer; Var ST1: Integer; Var ST2: Integer; Var ST3: Integer ): Integer; StdCall;
  TBematech_FI_RelatorioSintegraMFD = function( iRelatorios : Integer;
                                           cArquivo    : String;
                                           cMes        : String;
                                           cAno        : String;
                                           cRazaoSocial: String;
                                           cEndereco   : String;
                                           cNumero     : String;
                                           cComplemento: String;
                                           cBairro     : String;
                                           cCidade     : String;
                                           cCEP        : String;
                                           cTelefone   : String;
                                           cFax        : String;
                                           cContato    : String ): Integer; StdCall;
  TBematech_FI_DadosUltimaReducaoMFD = function(DadosReducao : string): Integer; StdCall;
  TBematech_FI_LeituraMemoriaFiscalDataMFD = function(DataInicial, DataFinal, FlagLeitura : string): Integer; StdCall;
  TBematech_FI_LeituraMemoriaFiscalSerialDataMFD = function(DataInicial, DataFinal, FlagLeitura : string): Integer; StdCall;
  TBematech_FI_LeituraMemoriaFiscalSerialDataPAFECF = function(DataInicial, DataFinal, FlagLeitura, chavePublica, chavePrivada : string): Integer; StdCall;
  TBematech_FI_LeituraMemoriaFiscalReducaoMFD = function(ReducaoInicial, ReducaoFinal, FlagLeitura : string): Integer; StdCall;
  TBematech_FI_LeituraMemoriaFiscalSerialReducaoMFD = function(ReducaoInicial, ReducaoFinal, FlagLeitura : string): Integer; StdCall;
  TBematech_FI_LeituraMemoriaFiscalSerialReducaoPAFECF = function(ReducaoInicial, ReducaoFinal, FlagLeitura, chavePublica, chavePrivada : string): Integer; StdCall;
  TBematech_FI_HabilitaDesabilitaRetornoEstendidoMFD = function(FlagRetorno : string): Integer;

var
  Bematech_FI_ProgramaAliquota: TBematech_FI_ProgramaAliquota;
  Bematech_FI_AbreCupom: TBematech_FI_AbreCupom;
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
  Bematech_FI_NumeroCupom: TBematech_FI_NumeroCupom;
  Bematech_FI_FlagsFiscais: TBematech_FI_FlagsFiscais;
  Bematech_FI_DataHoraReducao: TBematech_FI_DataHoraReducao;
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

implementation

procedure loadAllBematechFunctions;
var
  DLLHandle: THandle;
begin
  DLLHandle := LoadLibrary('BEMAFI32.DLL');
  if DLLHandle = 0 then
    raise Exception.create('Não foi possível carregar a DLL da impressora fiscal Bematech' + #13#10 + 'Entre em contato com o suporte.');
  @Bematech_FI_ProgramaAliquota := GetProcAddress(DLLHandle, 'Bematech_FI_ProgramaAliquota');
  @Bematech_FI_AbreCupom := GetProcAddress(DLLHandle, 'Bematech_FI_AbreCupom');
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
  @Bematech_FI_NumeroCupom := GetProcAddress(DLLHandle, 'Bematech_FI_NumeroCupom');
  @Bematech_FI_FlagsFiscais := GetProcAddress(DLLHandle, 'Bematech_FI_FlagsFiscais');
  @Bematech_FI_DataHoraReducao := GetProcAddress(DLLHandle, 'Bematech_FI_DataHoraReducao');
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
  @Bematech_FI_HabilitaDesabilitaRetornoEstendidoMFD := GetProcAddress(DLLHandle, 'Bematech_FI_LeituraMemoriaFiscalSerialReducaoMFD');
end;

end.
