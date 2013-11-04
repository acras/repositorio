unit BematechIntfUnit;

interface

type
  IBematech = interface(IInterface)
  ['{1B6E8261-E8DC-4D9A-86CD-EDD97C384B99}']
    // Inicialização
    function ProgramaAliquota(var Aliquota: AnsiString; Vinculo: Integer): Integer;

    // Funções do cupom fiscal
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

    // Funções dos Relatórios Fiscais
    function LeituraX: Integer;
    function ReducaoZ(Data, Hora: AnsiString): Integer;
    function LeituraMemoriaFiscalData(dataInicial, dataFinal: AnsiString): integer;
    function DadosUltimaReducaoMFD(DadosReducao: AnsiString): Integer;
    function LeituraMemoriaFiscalDataMFD(DataInicial, DataFinal, FlagLeitura: AnsiString): Integer;
    function LeituraMemoriaFiscalSerialDataMFD(DataInicial, DataFinal, FlagLeitura: AnsiString): Integer;
    function LeituraMemoriaFiscalSerialDataPAFECF(DataInicial, DataFinal, FlagLeitura, chavePublica, chavePrivada: AnsiString): Integer;
    function LeituraMemoriaFiscalReducaoMFD(ReducaoInicial, ReducaoFinal, FlagLeitura: AnsiString): Integer;
    function LeituraMemoriaFiscalSerialReducaoMFD(ReducaoInicial, ReducaoFinal, FlagLeitura: AnsiString): Integer;
    function LeituraMemoriaFiscalSerialReducaoPAFECF(ReducaoInicial, ReducaoFinal, FlagLeitura, chavePublica, chavePrivada: AnsiString): Integer;
    function ArquivoMFD(ArquivoOrigem, DadoInicial, DadoFinal, TipoDownload, Usuario: AnsiString;
      TipoGeracao: integer; ChavePublica, ChavePrivada: AnsiString; UnicoArquivo: integer): integer;
    function EspelhoMFD(NomeArquivo, DataOuCOOInicial, DataOuCOOFinal,
      TipoDownload, Usuario, ChavePublica, ChavePrivada: AnsiString): integer;
    function DownloadMF( Arquivo: AnsiString ): Integer;
    function DownloadMFD( Arquivo: AnsiString; TipoDownload: AnsiString; ParametroInicial: AnsiString; ParametroFinal: AnsiString; UsuarioECF: AnsiString ): Integer;
    function FormatoDadosMFD( ArquivoOrigem: AnsiString; ArquivoDestino: AnsiString; TipoFormato: AnsiString; TipoDownload: AnsiString; ParametroInicial: AnsiString; ParametroFinal: AnsiString; UsuarioECF: AnsiString ): Integer;


    // Funções de informações da impressora
    function DataHoraReducao(var DataReducao, HoraReducao: AnsiString): Integer;
    function DataMovimento(var dataMovimento: AnsiString): Integer;
    function DataHoraImpressora(var Data, hora: AnsiString): Integer;
    function ImprimeConfiguracoesImpressora: Integer;
    function RetornoAliquotas(var Aliquotas: AnsiString): Integer;

    function FlagsFiscais(var flag: integer): integer;
    function AbrePortaSerial: integer;

    function numeroSerie(var num: AnsiString): integer;
    function numeroCupom(var num: AnsiString): integer;

    function habilitaDesabilitaRetornoEstendidoMFD(flag: AnsiString): integer;

    // Outras funções
    function AberturaDoDia(var Valor, FormaPagamento: AnsiString): Integer;
    function FechamentoDoDia: Integer;
    function Sangria(Valor: AnsiString): Integer;
    function Suprimento(Valor, FormaPagamento: AnsiString): Integer;

    function DataHoraUltimoDocumentoMFD(var dataHora: AnsiString): integer;
    function ContadorRelatoriosGerenciaisMFD(var contador: AnsiString): integer;
    function NumeroOperacoesNaoFiscais(var numOper: AnsiString): integer;
    function ContadorComprovantesCreditoMFD(var contador: AnsiString): integer;
    function ContadorCupomFiscalMFD(var contador: AnsiString): integer;

    function RetornoImpressora(var Ack, St1, St2: Integer): Integer;
    function RetornoImpressoraMFD(var Ack, St1, St2, st3: Integer): Integer;
    function VerificaImpressoraLigada: Integer;
    function AcionaGaveta: Integer;

    function SubTotal(var SubTotal: AnsiString): Integer;
    function LeituraXSerial: Integer;
    function VersaoFirmware(var VersaoFirmware: AnsiString): Integer;
    function VersaoFirmwareMFD(var VersaoFirmware: AnsiString): Integer;
    function CGC_IE(var CGC: AnsiString; var IE: AnsiString): Integer;
    function GrandeTotal(var GrandeTotal: AnsiString): Integer;
    function DataHoraGravacaoUsuarioSWBasicoMFAdicional(var DataHoraUsuario, DataHoraSWBasico, MFAdicional: AnsiString): integer;
  end;

implementation

end.
