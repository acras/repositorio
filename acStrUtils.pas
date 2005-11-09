unit acStrUtils;

interface

uses
  classes, StrUtils, SysUtils;

function getStrField(str: string; delimiter: char; index: integer): string;

implementation

function getStrField(str: string; delimiter: char; index: integer): string;
var
  strL: TStringList;
begin
  strL := TStringList.Create;
  try
    str := '"' + AnsiReplaceStr(str, '|', '"|"') + '"';
    strL.Delimiter := delimiter;
    strL.DelimitedText := str;
    result := strL[index-1];
  finally
    FreeAndNil(strL);
  end;
end;


end.
