unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DB, Grids, DBGrids, ComCtrls,  CNAB240, CNAB400;

type
  TForm1 = class(TForm)
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Label5: TLabel;
    Label6: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Button1: TButton;
    DBGrid1: TDBGrid;
    OpenDialog1: TOpenDialog;
    Button2: TButton;
    DBGrid2: TDBGrid;
    DataSource1: TDataSource;
    DataSource2: TDataSource;
    Edit1: TEdit;
    Edit2: TEdit;
    DateTimePicker1: TDateTimePicker;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Button3: TButton;
    TabSheet3: TTabSheet;
    Button4: TButton;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    DataSource3: TDataSource;
    DBGrid3: TDBGrid;
    TabSheet4: TTabSheet;
    Button5: TButton;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    DBGrid4: TDBGrid;
    dsRetorno400: TDataSource;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
  private
    { Private declarations }
    FCNAB400: TCNAB400;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation


{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  c: tcnab240;
begin
  c := TCNAB240.Create;
  with OpenDialog1 do
  begin
    if Execute then
    begin
      c.abrirArquivo(FileName);
      if c.tipoArquivo=CNAB240.taRemessa then
        label6.Caption := 'Remessa'
      else
        label6.Caption := 'Retorno';
      label3.Caption := FormatDateTime('dd/mm/yyyy hh:nn:ss', c.dataGeracao);
      label4.Caption := IntToStr(c.sequencialArquivo);

      DataSource1.DataSet := c.dataSet;
    end;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  with FCNAB400 do
  begin
    RazaoSocial := 'ABMH Curitiba';
    CodigoEmpresa := 123456;
    SequencialArquivo := 287;
//    adicionarBoleto(strToInt(edit1.text), edit2.text, DateTimePicker1.Date,
//      StrToCurr(edit3.Text), StrToCurr(edit4.Text), edit5.Text, edit6.Text, edit7.Text);
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FCNAB400 := TCNAB400.Create;
  DataSource2.DataSet := FCNAB400.dataSet;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  FCNAB400.AgenciaEmpresa := 3451;
  FCNAB400.ContaEmpresa := 1158007;
  FCNAB400.testar
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  c: tcnab400;
begin
  c := TCNAB400.Create;
  with OpenDialog1 do
  begin
    if Execute then
    begin
      c.abrirArquivo(FileName);
      if c.tipoArquivo=CNAB400.taRemessa then
        label8.Caption := 'Remessa'
      else
        label8.Caption := 'Retorno';
      label11.Caption := FormatDateTime('dd/mm/yyyy hh:nn:ss', c.dataGeracaoArquivo);
      label12.Caption := IntToStr(c.SequencialArquivo);

      DataSource3.DataSet := c.dataSet;
    end;
  end;

end;

procedure TForm1.Button5Click(Sender: TObject);
var
  c: tcnab400;
begin
  c := TCNAB400.Create;
  with OpenDialog1 do
  begin
    if Execute then
    begin
      c.abrirArquivo(FileName);
      if c.tipoArquivo=CNAB400.taRemessa then
        label8.Caption := 'Remessa'
      else
        label8.Caption := 'Retorno';
      label11.Caption := FormatDateTime('dd/mm/yyyy hh:nn:ss', c.dataGeracaoArquivo);
      label12.Caption := IntToStr(c.SequencialArquivo);

      dsRetorno400.DataSet := c.dataSetRetorno;
    end;
  end;

end;

end.
