unit U_Cipher;

interface

uses
  StdCtrls, MD5, IDEA, DCPcrypt, Forms,
  Classes, SysUtils, Dialogs;

type
  TBuffer = Packed Array [1..255] of Char;

  TChave = Record
    Senha       : String[50];
    DataGeracao : String[50];
    Software    : String[50];
    Dias        : String[50];
    Usuarios    : String[50];
    Comentario  : String[100];
  end;

var
  MD5_Hash : TDCP_MD5;
  IDEA_Buffer : TDCP_Idea;

Function GetHexa (Frase: String):String;
Function GetDecimal (Frase : String) : String;
Function MD5Hash(Buffer : String):String;
Function Cifra(Buffer : String; KeyString : String) : String;
Function Decifra(Buffer : String; KeyString : String):String;
procedure DecifraArquivo(ArquivoIn, ArquivoOut : String;Senha:String);
procedure CifraArquivo(ArquivoIn, ArquivoOut : String;Senha:String);

implementation

Function Cifra(Buffer : String; KeyString : String) : String;
Var Temp : String;
begin
  IDEA_Buffer := TDCP_Idea.Create(Application);
  Temp:=MD5Hash(KeyString);
  IDEA_Buffer.InitStr(Temp);
  Temp:=IDEA_Buffer.EncryptString(Buffer);
  Result:=GetHexa(Temp);
  IDEA_Buffer.Burn;
  IDEA_Buffer.Free;
end;

Function Decifra(Buffer : String; KeyString : String):String;
Var Temp : String;
begin
  Temp:=GetDecimal(Buffer);
  IDEA_Buffer := TDCP_Idea.Create(Application);
  IDEA_Buffer.InitStr(MD5Hash(KeyString));
  Result:=IDEA_Buffer.DecryptString(Temp);
  IDEA_Buffer.Burn;
  IDEA_Buffer.Free;
end;

Function GetDecimal (Frase : String) : String;
Var j     : integer;
    c     : Byte;
    s,b   : String;
begin
  b:='';
  for j:= 1 to Length(Frase) do
  begin
    s:= copy(Frase,1,2);
    try
      Delete(Frase,1,2);
    except end;
    If length(s)<>2 then continue;
    s:='$'+s;
    c:=StrToInt(s);
    b:=b+Chr(c);
  end;
  Result:=b;
end;

Function GetHexa (Frase: String):String;
Var j : integer;
    s : String;
    c : Byte;
begin
  s:='';
  For j:=1 to length(Frase) do
  begin
    c:=Ord(Frase[j]);
    s:=s+IntToHex(c,2);
  end;
  Result:=s;
end;

Function MD5Hash(Buffer : String):String;
Var Hash : Array[0..15] of byte;  //31
    Temp : String;
    f    : Byte;
begin
  For f:=0 to 15 do Hash[f]:=0;
  Buffer:=Trim(Buffer);
  MD5_Hash:=TDCP_MD5.Create(NIL);
  MD5_Hash.Init;
  MD5_Hash.UpdateStr(Buffer);
  MD5_Hash.Final(Hash);
  For f:=0 to 15 do Temp:=Temp+IntToHex(Hash[f],2);
  Result:=Copy(Temp,1,32);
  MD5_Hash.Burn;
  MD5_Hash.Free;
end;

procedure CifraArquivo(ArquivoIn, ArquivoOut : String;Senha:String);
var
  Cipher: TDCP_IDEA;
  Input, Output: TFileStream;
  Progress, Done: longint;
  Header: packed record     { This is the header of the encrypted file }
    Id: longint;
    Check1, Check2: longint;
  end;
begin
  if not FileExists(ArquivoIn) then Exit;
  Cipher:= TDCP_IDEA.Create(Application);
  try
    Input:= TFileStream.Create(ArquivoIn,fmOpenRead); { Open the input file }
  except
    Cipher.Free;
    Exit;
  end;
  try
    Output:= TFileStream.Create(ArquivoOut,fmCreate); { Open the output file }
  except
    Cipher.Free;
    Input.Free;
    Exit;
  end;
  Progress:= 0;     { Progress contains the total bytes done }
  Header.Id:= Cipher.Id;  { Store the cipher id so we know what algorithm to use
                            on decrypting }
  Header.Check1:= Random($FFFF) or (Random($FFFF) shl 16);
  Header.Check2:= Header.Check1;
    { To check that we have the right passphrase on decryption if we store a
      random number twice then when we decrypt we know that the two numbers
      should match }
  Cipher.InitStr(Senha); { Initialize the cipher using the passphrase }
  if Cipher is TDCP_blockcipher then
    TDCP_blockcipher(Cipher).CipherMode:= cmCFBblock; { Set the appropriate mode if a block cipher }
  Cipher.Encrypt(Header.Check1,Header.Check1,8); { Encrypt the last 8 bytes of header - NOT the id }
  Output.Write(Header,Sizeof(Header));
  repeat
    Done:= Cipher.EncryptStream(Input,Output,32768);  { Encrypt the file 32768 bytes at a time }
    Inc(Progress,Done);
  until Done<> 32768;
  Input.Free;   { Free all resources used }
  Output.Free;
  Cipher.Burn;  { Remember to Burn after use }
  Cipher.Free;
  FillChar(Header,Sizeof(Header),0);
end;

procedure DecifraArquivo(ArquivoIn, ArquivoOut : String;Senha:String);
var
  Cipher: TDCP_IDEA;
  Input, Output: TFileStream;
  Progress, Done: longint;
  Header : packed record
     Id  : longint;
     Check1, Check2: longint;
  end;
begin
  if not FileExists(ArquivoIn) then Exit;
  try
    Input:= TFileStream.Create(ArquivoIn,fmOpenRead);
  except
    Exit;
  end;
  try
    Output:= TFileStream.Create(ArquivoOut,fmCreate);
  except
    Input.Free;
    Exit;
  end;
  Input.Read(Header,Sizeof(Header));
  Cipher:= TDCP_IDEA.Create(Application);
  Progress:= 0;
  Cipher.InitStr(Senha); { Initialize the cipher with the passphrase }
  if Cipher is TDCP_blockcipher then
    TDCP_blockcipher(Cipher).CipherMode:= cmCFBblock; { Set the mode }
  Cipher.Decrypt(Header.Check1,Header.Check1,8); { Decrypt the header and check that the
                                                   two numbers we stored are the same }
  if Header.Check1<> Header.Check2 then
  begin
    Input.Free;
    Output.Free;
    DeleteFile(ArquivoOut);
    Cipher.Burn;
    Cipher.Free;
    Exit;
  end;
  repeat
    Done:= Cipher.DecryptStream(Input,Output,32768);  { Decrypt the file }
    Inc(Progress,Done);
//    DecProgressBar.Position:= (Progress*100) div Input.Size;
  until Done<> 32768;
  Input.Free;
  Output.Free;
  Cipher.Burn;  { Remember to Burn }
  Cipher.Free;
  FillChar(Header,Sizeof(Header),0);
end;

end.
