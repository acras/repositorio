unit PAFFileUnit;

interface
  uses Classes, SysUtils;

type

  TPAFFile = class
  public
    constructor Create;
    destructor Destroy; override;
    procedure saveContents(nomeArq: String);
  protected
    contents: TStringList;
    function addN1Contents: string;
    function addN2Contents: string;
    function addN3Contents: string;
    function addN9Contents: string;
  end;

const
  CNPJ = '07504505000132';
  IE = '              ';
  IM = '010704940940  ';
  razaoSocial = 'ACRAS TECNOLOGIA DA INFORMAÇÃO LTDA - ME          ';

  nomePAF = 'FOCUS LOJAS                                       ';
  versao = '6         ';

  nomeExecutavel = 'FOCUSLOJAS.EXE';

var
  totalRegistros: integer;

implementation

uses sglRegistry, acCryptoUtils, signBema, sglConsts, TypInfo, UtilsEnne,
  acStrUtils, Dialogs;

{ TPAFFile }

constructor TPAFFile.Create;
begin
  contents := TStringList.create;
  totalRegistros := 0;
end;

destructor TPAFFile.Destroy;
begin
  contents.free;
end;

function TPAFFile.addN1Contents: string;
begin
  contents.Add('N1' + cnpj + ie + im + razaoSocial);
end;

procedure TPAFFile.saveContents(nomeArq: string);
var
  flagTipoSalvamento: integer;
  ead: String;
  nomeArqLocal: string;
  hash: string;
  arqHash: TextFile;
  _paf_cpb_local, _paf_cpv_local: string;
begin
  addN1Contents;
  addN2Contents;
  addN3Contents;
  addN9Contents;
  flagTipoSalvamento := 1;
  contents.SaveToFile(nomeArq);
  nomeArqLocal := nomeArq;
  setlength(ead, 256);
  _paf_cpv_local := _paf_cpv;
  _paf_cpb_local := _paf_cpb;
  generateEAD(nomeArqLocal, _paf_cpb_local, _paf_cpv_local, ead, flagTipoSalvamento);
  hash := MD5(nomeArq);
  AssignFile(arqHash, getFileNameHashPAF);
  Rewrite(arqHash);
  Write(arqHash, simpleCrypt(hash));
  CloseFile(arqHash);
end;

function TPAFFile.addN2Contents: string;
begin
  contents.add('N2' + _paf_laudo + nomePAF + versao);
end;

function TPAFFile.addN3Contents: string;
var
  f: TSearchRec;
  i: integer;
begin
  totalRegistros := 1;
  contents.add('N3' + rtoTxtField(nomeExecutavel, 50) + MD5(getSGLPath + 'produto\focuslojas.exe'));
  i := FindFirst(getSGLPath + 'produto\DLLs\*.dll', faAnyFile, f);
  if (i = 0) then repeat
    totalRegistros := totalRegistros + 1;
    contents.add('N3' + rtoTxtField(f.Name, 50) + MD5(getSGLPath + 'produto\DLLs\' + f.Name));
  until (FindNext(f) <> 0);
end;

function TPAFFile.addN9Contents: string;
begin
  contents.Add('N9' + CNPJ + IE + FormatFloat('000000', totalRegistros));
end;

end.
