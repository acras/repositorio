{* Unit CNAB240.pas
 *
 * Criação: Ricardo Nastás Acras (ricardo@acras.net)
 * Data: 15/10/2005
 *
 * Objetivo: Tratar arquivos CNAB 240 posições.
}
unit CNAB240;

interface

uses
  Classes, SysUtils, DBClient, DB;

type
  TTipoArquivo = (taRemessa, taRetorno);

  TCNAB240 = class
  private
    FDataHoraGeracao: TDateTime;
    FSequencialArquivo: integer;
    FTipoArquivo: TTipoArquivo;
    FClientDataSetTitulos: TClientDataSet;
  protected
  public
    constructor create;
    destructor destroy;
    property dataGeracao: TDateTime read FDataHoraGeracao write FDataHoraGeracao;
    property sequencialArquivo: integer read FSequencialArquivo write FSequencialArquivo;
    property tipoArquivo: TTipoArquivo read FTipoArquivo write FTipoArquivo;
    property dataSet: TClientDataSet read FClientDataSetTitulos;
    procedure abrirArquivo(sNomeArquivo: string);
  end;

implementation

uses TypInfo, DateUtils;

{ TCNAB240 }

procedure TCNAB240.abrirArquivo(sNomeArquivo: string);
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

    if copy(sHeader, 143,1) = '1' then
      FTipoArquivo := taRemessa
    else
      FTipoArquivo := taRetorno;

    FDataHoraGeracao := EncodeDateTime(
      StrToInt(copy(sHeader,148,4)),
      StrToInt(copy(sHeader,146,2)),
      StrToInt(copy(sHeader,144,2)),
      StrToInt(copy(sHeader,152,2)),
      StrToInt(copy(sHeader,154,2)),
      StrToInt(copy(sHeader,156,2)),
      0
      );

    FSequencialArquivo := StrToInt(copy(sHeader, 158, 6));

    //pulando o header do lote por que não foi encontrado uso prático para
    //o arquivo de retorno, caso encontre pode adicionar aqui o código de
    //interpretação desta linha

    //ler os segmentos T's e U's
    //Cada par de segmentos T's e U's correspondem a um título
    iLinhaAtual := 2;
    while iLinhaAtual < conteudo.Count do
    begin
      //se é segmento T deverá conter um segmento T e um segmento U
      if copy(conteudo[iLinhaAtual], 14, 1) = 'T' then
      begin
        sSegmentoT := conteudo[iLinhaAtual];
        sSegmentoU := conteudo[iLinhaAtual+1];
        iLinhaAtual := iLinhaAtual + 2;

        FClientDataSetTitulos.Append;
        FClientDataSetTitulos.FieldByName('NossoNumero').AsString :=
          trim(copy(sSegmentoT, 106, 25));
        FClientDataSetTitulos.FieldByName('ValorTarifa').AsFloat :=
          StrToFloat(copy(sSegmentoT, 199, 13)+','+copy(sSegmentoT, 212, 2));
        FClientDataSetTitulos.FieldByName('ValorJuros').AsFloat :=
          StrToFloat(copy(sSegmentoU, 18, 13)+','+copy(sSegmentoU, 31, 2));
        FClientDataSetTitulos.FieldByName('ValorDesconto').AsFloat :=
          StrToFloat(copy(sSegmentoU, 33, 13)+','+copy(sSegmentoU, 46, 2));
        FClientDataSetTitulos.FieldByName('ValorAbatimento').AsFloat :=
          StrToFloat(copy(sSegmentoU, 48, 13)+','+copy(sSegmentoU, 61, 2));
        FClientDataSetTitulos.FieldByName('ValorIOF').AsFloat :=
          StrToFloat(copy(sSegmentoU, 63, 13)+','+copy(sSegmentoU, 76, 2));
        FClientDataSetTitulos.FieldByName('ValorPago').AsFloat :=
          StrToFloat(copy(sSegmentoU, 78, 13)+','+copy(sSegmentoU, 91, 2));
        FClientDataSetTitulos.FieldByName('ValorLiquido').AsFloat :=
          StrToFloat(copy(sSegmentoU, 93, 13)+','+copy(sSegmentoU, 106, 2));
        FClientDataSetTitulos.FieldByName('ValorOutrasDespesas').AsFloat :=
          StrToFloat(copy(sSegmentoU, 108, 13)+','+copy(sSegmentoU, 121, 2));
        FClientDataSetTitulos.FieldByName('ValorOutrosCreditos').AsFloat :=
          StrToFloat(copy(sSegmentoU, 123, 13)+','+copy(sSegmentoU, 136, 2));
        FClientDataSetTitulos.FieldByName('DataOcorrencia').AsDateTime :=
          EncodeDate(
            StrToInt(copy(sSegmentoU,142,4)),
            StrToInt(copy(sSegmentoU,140,2)),
            StrToInt(copy(sSegmentoU,138,2))
          );
        FClientDataSetTitulos.FieldByName('DataCredito').AsDateTime :=
          EncodeDate(
            StrToInt(copy(sSegmentoU,150,4)),
            StrToInt(copy(sSegmentoU,148,2)),
            StrToInt(copy(sSegmentoU,146,2))
          );
        FClientDataSetTitulos.FieldByName('DataTarifa').AsDateTime :=
          EncodeDate(
            StrToInt(copy(sSegmentoU,158,4)),
            StrToInt(copy(sSegmentoU,156,2)),
            StrToInt(copy(sSegmentoU,154,2))
          );
        FClientDataSetTitulos.Post;
      end
      else
        inc(iLinhaAtual);
    end;
  finally
    FreeAndNil(conteudo);
  end;

end;

constructor TCNAB240.create;
begin
  inherited;
  FClientDataSetTitulos := TClientDataSet.Create(nil);

  //criar os campos para o ClientDataSet
  with TStringField.create(FClientDataSetTitulos) do
  begin
    FieldName := 'NossoNumero';
    DataSet := FClientDataSetTitulos;
  end;

  with TFloatField.Create(FClientDataSetTitulos) do
  begin
    FieldName := 'ValorTarifa';
    currency := true;
    DataSet := FClientDataSetTitulos;
  end;

  with TFloatField.Create(FClientDataSetTitulos) do
  begin
    FieldName := 'ValorJuros';
    currency := true;
    DataSet := FClientDataSetTitulos;
  end;

  with TFloatField.Create(FClientDataSetTitulos) do
  begin
    FieldName := 'ValorDesconto';
    currency := true;
    DataSet := FClientDataSetTitulos;
  end;

  with TFloatField.Create(FClientDataSetTitulos) do
  begin
    FieldName := 'ValorAbatimento';
    currency := true;
    DataSet := FClientDataSetTitulos;
  end;

  with TFloatField.Create(FClientDataSetTitulos) do
  begin
    FieldName := 'ValorIOF';
    currency := true;
    DataSet := FClientDataSetTitulos;
  end;

  with TFloatField.Create(FClientDataSetTitulos) do
  begin
    FieldName := 'ValorPago';
    currency := true;
    DataSet := FClientDataSetTitulos;
  end;

  with TFloatField.Create(FClientDataSetTitulos) do
  begin
    FieldName := 'ValorLiquido';
    currency := true;
    DataSet := FClientDataSetTitulos;
  end;

  with TFloatField.Create(FClientDataSetTitulos) do
  begin
    FieldName := 'ValorOutrasDespesas';
    currency := true;
    DataSet := FClientDataSetTitulos;
  end;

  with TFloatField.Create(FClientDataSetTitulos) do
  begin
    FieldName := 'ValorOutrosCreditos';
    currency := true;
    DataSet := FClientDataSetTitulos;
  end;

  with TDateTimeField.Create(FClientDataSetTitulos) do
  begin
    FieldName := 'DataOcorrencia';
    DataSet := FClientDataSetTitulos;
  end;

  with TDateTimeField.Create(FClientDataSetTitulos) do
  begin
    FieldName := 'DataCredito';
    DataSet := FClientDataSetTitulos;
  end;

  with TDateTimeField.Create(FClientDataSetTitulos) do
  begin
    FieldName := 'DataTarifa';
    DataSet := FClientDataSetTitulos;
  end;

  FClientDataSetTitulos.CreateDataSet;
end;

destructor TCNAB240.destroy;
begin
  FClientDataSetTitulos.Free;
end;

end.
