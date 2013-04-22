unit acCryptoUtils;

interface

uses IdHashMessageDigest, idHash, Classes, SysUtils;

function MD5(const fileName : string) : string;
function getMD5checksum(s: TStream): string;

implementation

function MD5(const fileName : string) : string;
var
  s: TFileStream;
begin
  s := TFileStream.Create(fileName, fmOpenRead);
  try
    result := getMD5checksum(s);
  finally
    FreeAndNil(s);
  end;
end;

function getMD5checksum(s: TStream): string;
var
  md5: TIdHashMessageDigest5;
  hash : T4x4LongWordRecord;
begin
  md5 := TIdHashMessageDigest5.Create;
  s.Seek(0,0);
  {$IFDEF VER150}
    hash := md5.HashValue(s);
    result := TIdHashMessageDigest5.AsHex(hash);
  {$ELSE}
    result := md5.HashStreamAsHex(s);
  {$ENDIF}
end;

end.
