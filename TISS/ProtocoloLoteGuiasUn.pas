unit ProtocoloLoteGuiasUn;

interface

uses
  Xmlxform, Forms, DBClient;

type

  TProtocoloLoteGuias = class
  private
    procedure SetxtrFileName(const Value: string);
    function GetxtrFileName: string;
    procedure SetxmlFileName(const Value: string);
    function GetxmlFileName: string;
    function GetNumeroLote: string;
    function GetNumeroProtocoloRecebimento: string;

  public
    constructor Create(owner: TObject);
    destructor destroy();
    property xtrFileName: string read GetxtrFileName write SetxtrFileName;
    property xmlFileName: string read GetxmlFileName write SetxmlFileName;
    property numeroLote: string read GetNumeroLote;
    property numeroProtocoloRecebimento: string
      read GetNumeroProtocoloRecebimento;
  protected
    xtr: TXMLTransformProvider;
    cdsProtocolo: TClientDataSet;
  end;

implementation

uses SysUtils;

{ TProtocoloLoteGuias }

constructor TProtocoloLoteGuias.Create(owner: TObject);
begin
  xtr := TXMLTransformProvider.Create(nil);
  xtr.name := 'xtrProtocolo';
  cdsProtocolo := TClientDataSet.Create(nil);
  cdsProtocolo.SetProvider(xtr);
end;

destructor TProtocoloLoteGuias.destroy;
begin
  FreeAndNil(cdsProtocolo);
end;

function TProtocoloLoteGuias.GetNumeroLote: string;
begin
  result := cdsProtocolo.fieldByName('numeroLote').asString;
end;

function TProtocoloLoteGuias.GetNumeroProtocoloRecebimento: string;
begin
  result := cdsProtocolo.fieldByName('numeroProtocoloRecebimento').asString;
end;

function TProtocoloLoteGuias.GetxmlFileName: string;
begin
  result := xtr.XMLDataFile;
end;

function TProtocoloLoteGuias.GetxtrFileName: string;
begin
  result := xtr.TransformRead.TransformationFile;
end;

procedure TProtocoloLoteGuias.SetxmlFileName(const Value: string);
begin
  cdsProtocolo.close;
  if xtr.TransformRead.TransformationFile = '' then
    raise Exception.Create('Arquivo XTR não foi especificado.');
  if not(FileExists(value)) then
    raise Exception.Create('Arquivo XML não existe.' + #13#10 +
      'Nome do Arquivo: ' + Value);
  xtr.XMLDataFile := Value;
  cdsProtocolo.SetProvider(xtr);
  cdsProtocolo.Open;
  //cdsProtocolo.SaveToFile('c:\tissao\teste.cds');
end;

procedure TProtocoloLoteGuias.SetxtrFileName(const Value: string);
begin
  if not(FileExists(Value)) then
    raise exception.Create('Arquivo de configuração XTR não existe.' + #13#10 +
      'Nome do arquivo: ' + Value);
  xtr.TransformRead.TransformationFile := Value;
end;

end.
