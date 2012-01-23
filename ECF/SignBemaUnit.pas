unit SignBemaUnit;

interface

  function genkkey( cChavePublica: String; cChavePrivada: String ): integer; stdcall; external 'sign_bema.dll';
  function setLibType( iTipo: integer ): integer; stdcall; external 'sign_bema.dll';
  function generateEAD( cNomeArquivo: String; cChavePublica: String; cChavePrivada: String; cEAD:String; iSign: Integer): integer; stdcall; external 'sign_bema.dll';
  function validateFile( cNomeArquivo: String; cChavePublica: String; cChavePrivada: String ): integer; stdcall; external 'sign_bema.dll';
  function md5FromFile( NomeArq: String; MD5: String ): integer; stdcall; external 'sign_bema.dll';

implementation

end.
