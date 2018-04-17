unit acDLLUtils;

interface

uses
  System.SysUtils, Dialogs, Math, Winapi.Windows, ShellAPI;

type
  TFileVersion = array of integer;
  TDLLChecker = class
  private
    FRootPath: string;
    FRootUrl: string;
    procedure downloadDLL(name: string);
  public
    handle: HWND;
    exePath: string;
    property rootUrl: string read FRootUrl write FRootUrl;
    property rootPath: string read FRootPath write FRootPath;
    constructor create; virtual;
    function DLLNeedUpdate(name: string; version: TFileVersion): boolean;
    procedure ensureDLLVersion(name: string; version: TFileVersion); overload;
    procedure ensureDLLVersion(name: string; version: string); overload;
    class function compareVersion(a, b: string): integer; overload;
    class function compareVersion(a, b: TFileVersion): integer; overload;
    class function compareVersion(a: string; b: TFileVersion): integer; overload;
    function locateDLLEmpirically(name: string): string;
    function locateDLLAPI(name: string): string;
    class function getFileVersion(const fn: string): TFileVersion;
    class function getFileVersionAsString(const fn: string): string;
    class function parseVersion(v: string): TFileVersion;
    class function fileVersion2String(version: TFileVersion): string; static;
  end;

implementation

{ TDLLChecker }

uses DLog, acSysUtils, Utils;

procedure TDLLChecker.downloadDLL(name: string);
var
  dest, tempFileName: string;
begin
  tempFileName := getWindowsTempFileName(name);
  //dest será o diretório raiz + nome da DLL
  dest := IncludeTrailingPathDelimiter(rootPath) + name;
  downloadArquivo(rootUrl + name, tempFileName);
  if FileExists(dest) then
  begin
    DataLog.log('Arquivo já existe, tentar apagar.','DLL');
    RenameFile(dest, dest + '_ASOF_' + FormatDateTime('yyyymmddhhnnss', now));
    if FileExists(dest) then
    begin
      DataLog.log('Não consegui apagar.','DLL');
      exit;
    end;
  end;
  if RenameFile(tempFileName, dest) then
  begin
    MessageDlg('Uma biblioteca necessária para o funcionamento do Focus Lojas foi atualizada.'#13#10+
      'Seu PDV será reiniciado.', mtInformation, [mbOK], 0);
    ShellExecute(handle, nil, PChar(exePath), nil, nil, SW_SHOWNORMAL);
    ExitProcess(0);
    DataLog.log('Arquivo salvo com sucesso', 'DLL');
  end
  else
    DataLog.log('ERRO ao tentar salvar arquivo', 'DLL');
end;

procedure TDLLChecker.ensureDLLVersion(name, version: string);
begin
  ensureDLLVersion(name, parseVersion(version));
end;

class function TDLLChecker.parseVersion(v: string): TFileVersion;
var
  rest, nStr: string;
  tam: integer;
begin
  rest := v;
  tam := 0;
  while Pos('.', rest) > 0 do
  begin
    nStr := copy(rest, 1, Pos('.', rest)-1);
    rest := copy(rest, Pos('.', rest)+1, 9999);
    tam := tam + 1;
    SetLength(result, tam);
    result[tam-1] := strToInt(nStr);
  end;
  tam := tam + 1;
  SetLength(result, tam);
  result[tam-1] := strToInt(rest);
end;

class function TDLLChecker.compareVersion(a, b: string): integer;
var
  versionA, versionB: TFileVersion;
begin
  versionA := parseVersion(a);
  versionB := parseVersion(b);
  result := compareVersion(versionA, versionB);
end;

class function TDLLChecker.compareVersion(a, b: TFileVersion): integer;
var
  n, i: integer;
begin
  result := 0;
  n := min(length(a), length(b));
  for i := 0 to n-1 do
  begin
    if a[i] > b[i] then
      result := -1;
    if b[i] > a[i] then
      result := 1;
    if result <> 0 then break;
  end;
end;

class function TDLLChecker.compareVersion(a: string; b: TFileVersion): integer;
var
  versionA: TFileVersion;
begin
  versionA := parseVersion(a);
  result := compareVersion(versionA, b);
end;

constructor TDLLChecker.create;
begin
  rootPath := GetCurrentDir;
end;

class function TDLLChecker.fileVersion2String(version: TFileVersion): string;
var
  i: integer;
begin
  result := '';
  for i := low(version) to high(version) do
  begin
    result := result + IntToStr(version[i]);
    if i <> high(version) then
      result := result + '.';
  end;
end;

function TDLLChecker.locateDLLAPI(name: string): string;
var
  fn: PWideChar;
  h: THandle;
  p: array[0..MAX_PATH-1] of char;
begin
  result := '';
  fn := PWideChar(name);
  h := LoadLibrary(fn);
  if h <> 0 then
  begin
    GetModuleFileName(h, p, MAX_PATH);
    result := p;
  end;
end;

function TDLLChecker.locateDLLEmpirically(name: string): string;
var
  path: string;
begin
  path := GetEnvironmentVariable('path');
  result := FileSearch(name, path);
  if result <> '' then
    if pos('\', result) = 0 then
      result := IncludeTrailingPathDelimiter(rootPath) + name;
  DataLog.log('Resultado da busca: ' + result, 'DLL');
end;

function TDLLChecker.DLLNeedUpdate(name: string; version: TFileVersion): boolean;
var
  fn: string;
  foundVersion: TFileVersion;
begin
  result := false;
  DataLog.log('Procurando DLL: ' + name + ' - v: ' + fileVersion2String(version), 'DLL');
  fn := locateDLLAPI(name);
  if fn <> '' then
  begin
    foundVersion := getFileVersion(fn);
    DataLog.log('Versão encontrada: ' + fileVersion2String(foundVersion), 'DLL');
  end;
  if (fn='') or (compareVersion(foundVersion, version) > 0) then
  begin
    DataLog.log('Necessário atualizar', 'DLL');
    result := true;
  end;
end;

procedure TDLLChecker.ensureDLLVersion(name: string; version: TFileVersion);
begin
  if DLLNeedUpdate(name, version) then
    downloadDLL(name);
  if DLLNeedUpdate(name, version) then
    DataLog.log('Atualização automática não funcionou, necessita intervenção manual.','DLL');
end;

class function TDLLChecker.getFileVersion(const fn: string): TFileVersion;
var
  infoSize: DWORD;
  verBuf: pointer;
  verSize: UINT;
  wnd: UINT;
  FixedFileInfo: PVSFixedFileInfo;
  r: string;
begin
  infoSize := GetFileVersioninfoSize(PChar(fn), wnd);

  r := '';

  setLength(result, 0);

  if infoSize <> 0 then
  begin
    GetMem(verBuf, infoSize);
    try
      if GetFileVersionInfo(PChar(fn), wnd, infoSize, verBuf) then
      begin
        VerQueryValue(verBuf, '\', Pointer(FixedFileInfo), verSize);

        r := IntToStr(FixedFileInfo.dwFileVersionMS div $10000) + '.' +
                  IntToStr(FixedFileInfo.dwFileVersionMS and $0FFFF) + '.' +
                  IntToStr(FixedFileInfo.dwFileVersionLS div $10000) + '.' +
                  IntToStr(FixedFileInfo.dwFileVersionLS and $0FFFF);
      end;
    finally
      FreeMem(verBuf);
    end;
  end;
  try
    result := parseVersion(r);
  except
    result := [0];
  end;
end;


class function TDLLChecker.getFileVersionAsString(
  const fn: string): string;
begin
  result := fileVersion2String(getFileVersion(fn));
end;

end.
