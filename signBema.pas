unit signBema;

interface

uses Windows, SysUtils;

procedure loadAllSignBemaFunctions;

type
  Tgenkkey = function (ChavePublica: AnsiString; ChavePrivada: AnsiString): integer; stdcall;
  TsetLibType = function(Tipo: integer): integer; stdcall;
  TgenerateEAD = function(NomeArq: AnsiString; ChavePublica: AnsiString; ChavePrivada: AnsiString; EAD: AnsiString; Sign: Integer): integer; stdcall;
  TvalidateFile = function(NomeArq: AnsiString; ChavePublica: AnsiString; ChavePrivada: AnsiString): integer; stdcall;
  Tmd5FromFile = function (nomeArquivo: AnsiString; md5: AnsiString): integer; stdcall;

var
  genkkey: Tgenkkey;
  setLibType: TsetLibType;
  generateEAD: TgenerateEAD;
  validateFile: TvalidateFile;
  md5FromFile: Tmd5FromFile;

implementation

procedure loadAllSignBemaFunctions;
var
  DLLHandle: THandle;
begin
  DLLHandle := LoadLibrary('sign_bema.dll');
  if DLLHandle = 0 then
    raise Exception.create('Não foi possível carregar a DLL sign Bema' + #13#10 + 'Entre em contato com o suporte.');
  @genkkey := GetProcAddress(DLLHandle, 'genkkey');
  @setLibType := GetProcAddress(DLLHandle, 'setLibType');
  @generateEAD := GetProcAddress(DLLHandle, 'generateEAD');
  @validateFile := GetProcAddress(DLLHandle, 'validateFile');
  @md5fromfile := GetProcAddress(DLLHandle, 'md5FromFile');
end;

end.
