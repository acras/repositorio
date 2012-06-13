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
  IE = '00000000000000';
  IM = '00000000000000';
  razaoSocial = 'ACRAS TECNOLOGIA DA INFORMAÇÃO LTDA               ';

  numLaudo = '0000000000';
  nomePAF = 'FOCUS LOJAS                                       ';
  versao = '4         ';

  nomeExecutavel = 'FOCUSLOJAS.EXE                                           ';

implementation

uses sglRegistry, acCryptoUtils, signBema, sglConsts, TypInfo, UtilsEnne,
  acStrUtils, Dialogs;

{ TPAFFile }

constructor TPAFFile.Create;
begin
  contents := TStringList.create;
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
  generateEAD(nomeArqLocal, _paf_cpb_local, _paf_cpv_local, PChar(ead), flagTipoSalvamento);
  hash := MD5(nomeArq);
  AssignFile(arqHash, getFileNameHashPAF);
  Rewrite(arqHash);
  Write(arqHash, simpleCrypt(hash));
  CloseFile(arqHash);
end;

function TPAFFile.addN2Contents: string;
begin
  contents.add('N2' + numLaudo + nomePAF + versao);
end;

function TPAFFile.addN3Contents: string;
var
  f: TSearchRec;
  i: integer;
begin
  contents.add('N3' + nomeExecutavel + MD5(getSGLPath + 'produto\focuslojas.exe'));
  i := FindFirst(getSGLPath + 'produto\DLLs\*.dll', faAnyFile, f);
  if (i = 0) then repeat
    contents.add('N3' + f.Name + MD5(getSGLPath + 'produto\DLLs\' + f.Name));
  until (FindNext(f) <> 0);
end;

function TPAFFile.addN9Contents: string;
begin
  contents.Add('N9' + CNPJ + IE + '000001');
end;

end.
