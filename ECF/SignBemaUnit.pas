unit SignBemaUnit;

interface

  function genkkey( cChavePublica: AnsiString; cChavePrivada: AnsiString ): integer; stdcall; external 'sign_bema.dll';
  function setLibType( iTipo: integer ): integer; stdcall; external 'sign_bema.dll';
  function generateEAD( cNomeArquivo: AnsiString; cChavePublica: AnsiString; cChavePrivada: AnsiString; cEAD:AnsiString; iSign: Integer): integer; stdcall; external 'sign_bema.dll';
  function validateFile( cNomeArquivo: AnsiString; cChavePublica: AnsiString; cChavePrivada: AnsiString ): integer; stdcall; external 'sign_bema.dll';
  function md5FromFile( NomeArq: AnsiString; MD5: AnsiString ): integer; stdcall; external 'sign_bema.dll';

implementation

end.
