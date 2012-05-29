unit BematechIntfUnit;

interface

type
  IBematech = interface(IInterface)
  ['{1B6E8261-E8DC-4D9A-86CD-EDD97C384B99}']
    // Inicialização
    function ProgramaAliquota(var Aliquota: string; Vinculo: Integer): Integer;

    // Funções do cupom fiscal
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

    // Funções dos Relatórios Fiscais
    function LeituraX: Integer;
    function ReducaoZ(Data, Hora: String): Integer;
    function LeituraMemoriaFiscalData(dataInicial, dataFinal: string): integer;
    function DadosUltimaReducaoMFD(DadosReducao: string): Integer;
    function LeituraMemoriaFiscalDataMFD(DataInicial, DataFinal, FlagLeitura: string): Integer;
    function LeituraMemoriaFiscalSerialDataMFD(DataInicial, DataFinal, FlagLeitura: string): Integer;
    function LeituraMemoriaFiscalSerialDataPAFECF(DataInicial, DataFinal, FlagLeitura, chavePublica, chavePrivada: string): Integer;
    function LeituraMemoriaFiscalReducaoMFD(ReducaoInicial, ReducaoFinal, FlagLeitura: string): Integer;
    function LeituraMemoriaFiscalSerialReducaoMFD(ReducaoInicial, ReducaoFinal, FlagLeitura: string): Integer;
    function LeituraMemoriaFiscalSerialReducaoPAFECF(ReducaoInicial, ReducaoFinal, FlagLeitura, chavePublica, chavePrivada: string): Integer;
    function HabilitaDesabilitaRetornoEstendidoMFD(flag: string): integer;
    function ArquivoMFD(ArquivoOrigem, DadoInicial, DadoFinal, TipoDownload, Usuario: string;
      TipoGeracao: integer; ChavePublica, ChavePrivada: string; UnicoArquivo: integer): integer;
    function EspelhoMFD(NomeArquivo, DataOuCOOInicial, DataOuCOOFinal,
      TipoDownload, Usuario, ChavePublica, ChavePrivada: string): integer;
    function DownloadMF( Arquivo: String ): Integer;
    function DownloadMFD( Arquivo: String; TipoDownload: String; ParametroInicial: String; ParametroFinal: String; UsuarioECF: String ): Integer;
    function FormatoDadosMFD( ArquivoOrigem: String; ArquivoDestino: String; TipoFormato: String; TipoDownload: String; ParametroInicial: String; ParametroFinal: String; UsuarioECF: String ): Integer;


    // Funções de informações da impressora
    function DataHoraReducao(var DataReducao, HoraReducao: string): Integer;
    function DataMovimento(var dataMovimento: string): Integer;
    function DataHoraImpressora(var Data, hora: string): Integer;
    function ImprimeConfiguracoesImpressora: Integer;
    function RetornoAliquotas(var Aliquotas: string): Integer;

    function FlagsFiscais(var flag: integer): integer;
    function AbrePortaSerial: integer;

    function numeroSerie(var num: string): integer;
    function numeroCupom(var num: string): integer;

    // Outras funções
    function AberturaDoDia(var Valor, FormaPagamento: string): Integer;
    function FechamentoDoDia: Integer;
    function Sangria(Valor: String): Integer;
    function Suprimento(Valor, FormaPagamento: String): Integer;

    function DataHoraUltimoDocumentoMFD(var dataHora: string): integer;
    function ContadorRelatoriosGerenciaisMFD(var contador: string): integer;
    function NumeroOperacoesNaoFiscais(var numOper: string): integer;
    function ContadorComprovantesCreditoMFD(var contador: string): integer;

    function RetornoImpressora(var Ack, St1, St2: Integer): Integer;
    function VerificaImpressoraLigada: Integer;

    function SubTotal(var SubTotal: String): Integer;
    function LeituraXSerial: Integer;
    function VersaoFirmware(var VersaoFirmware: String): Integer;
    function VersaoFirmwareMFD(var VersaoFirmware: String): Integer;
    function CGC_IE(var CGC: String; var IE: String): Integer;
    function GrandeTotal(var GrandeTotal: String): Integer;
    function DataHoraGravacaoUsuarioSWBasicoMFAdicional(var DataHoraUsuario, DataHoraSWBasico, MFAdicional: string): integer;
  end;

implementation

end.
