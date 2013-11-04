unit PDVPrinterIntfUnit;

interface

type
  TFlagsFiscais = record
    CupomFiscalAberto: boolean;
    FechamentoFormasPagamentoIniciado: boolean;
    HorarioVeraoAtivo: boolean;
    JaFezReducaoZ: boolean;
    PermiteCancelarCupomFiscal: boolean;
    MemoriaFiscalSemEspaco: boolean;
  end;
  
  IAliquotaList = interface(IInterface)
  ['{C4BDFD52-3A7A-472D-ACB8-99E373CB7367}']
    function Count: Integer;
    function GetItem(Index: Integer): Currency;
    
    property Items[Index: Integer]: Currency read GetItem; default;
  end;

  IPDVPrinter = interface(IInterface)
  ['{95CDD575-353B-444A-BE5E-F11BDB7E6F94}']
    procedure AbrirDia;
    procedure FecharDia;
    procedure EfetuarReducaoZ(DateTime: TDateTime = 0);
    function Sangria(Valor: Currency): Integer;
    function Suprimento(Valor: Currency; FormaPagamento: AnsiString): Integer;

    procedure ImprimirConfiguracoes;
    function FlagsFiscais: TFlagsFiscais; 
    function VerifyDataUltimaReducaoZ(out DateTime: TDateTime): Boolean;

    function IsAtiva: Boolean;

    procedure ProgramarAliquotaICMS(Aliquota: Currency);
    function GetAliquotaList: IAliquotaList;
    function getNumSerie: AnsiString;

    function dataHoraImpressora: TDateTime;
    function dataUltimoMovimento: TDateTime;
    procedure acionaGaveta;
    procedure habilitaRetornoEstendido;
    procedure desabilitaRetornoEstendido;

    function SubTotal: double;
    procedure LeituraXSerial;
    function VersaoFirmware: AnsiString;
    function VersaoFirmwareMFD: AnsiString;
    procedure CGC_IE(var CGC, IE: AnsiString);
    function GrandeTotal: Double;
    procedure DataHoraGravacaoUsuarioSWBasicoMFAdicional(var DataHoraUsuario, DataHoraSWBasico, MFAdicional: AnsiString);

    function DataHoraUltimoDocumentoMFD: TDateTime;
    function ContadorRelatoriosGerenciaisMFD: integer;
    function NumeroOperacoesNaoFiscais: integer;
    function ContadorComprovantesCreditoMFD: integer;


    // Esse método será usado apenas internamente pelo controller, mas é muito conveniente para
    // os test-cases ele ser público
    procedure CheckStatus(RetVal: Integer);
  end;

implementation

end.
