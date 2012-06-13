unit signBema;

interface

uses Windows, SysUtils;

procedure loadAllSignBemaFunctions;

type
  Tgenkkey = function (ChavePublica: String; ChavePrivada: String): integer; stdcall;
  TsetLibType = function(Tipo: integer): integer; stdcall;
  TgenerateEAD = function(NomeArq: String; ChavePublica: String; ChavePrivada: String; EAD:String; Sign: Integer): integer; stdcall;
  TvalidateFile = function(NomeArq: String; ChavePublica: String; ChavePrivada: String): integer; stdcall; 
  Tmd5FromFile = function (nomeArquivo: String; md5: String): integer; stdcall;

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
