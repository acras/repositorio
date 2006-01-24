unit acStrUtils;

interface


uses
  classes, StrUtils, SysUtils;

Type
  TInfoPalavra = class
    public
      frequencia: integer;
  end;

function getStrField(str: string; delimiter: char; index: integer): string;
function retiraEspacos(str: String): String;
function RemoveAcento(str: String): String;
function RemoveSimbolo(str: String): String;
function extraiPalavras(texto: string): TStringList;
function achaPalavra(texto, palavra: string): integer;

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

function retiraEspacos(str: string): String;
var
  i: integer;
  adicionouEspaco: boolean;
begin
  result := '';
  adicionouEspaco := false;
  for i := 1 to length(str) do
  begin
    if (str[i]<>' ') then
    begin
      result := result + str[i];
      adicionouEspaco := False;
    end
    else
    begin
      if not adicionouEspaco then
      begin
        adicionouEspaco := true;
        result := result + ' ';
      end;
    end;
  end;
end;

function RemoveAcento(Str:String): String;
Const
  ComAcento = '‡‚ÍÙ˚„ı·ÈÌÛ˙Á¸‰¿¬ ‘€√’¡…Õ”⁄«‹ƒ';
  SemAcento = 'aaeouaoaeioucuaAAEOUAOAEIOUCUA';
Var
  x : Integer;
Begin
  For x := 1 to Length(Str) do
    if Pos(Str[x],ComAcento)<>0 Then
      Str[x] := SemAcento[Pos(Str[x],ComAcento)];
  Result := Str;
end;

function RemoveSimbolo(str: string): String;
const
  simbolo = '''"!@#$%.®&*()_-+=ß¨π¥`~^;:/\|<>][{}?!';
var
  i: integer;
begin
  for i:= 1 to length(str) do
  begin
     if Pos(str[i],simbolo)=0 Then
      result := result + str[i];
     if Pos(str[i],simbolo)<>0 Then
       result := result + '';
  end;
end;

function achaPalavra(texto, palavra: string): integer;
begin
  result := pos(palavra, texto);
end;

function extraiPalavras(texto: string): TStringList;
var
  strL: TStringList;
  i, posicao: integer;
  palavra: string;
  info: TInfoPalavra;
begin
  palavra := '';
  strL := TStringList.Create;
  for i := 1 to length(texto) do
  begin
    //se È uma palavra nova e n„o foi inserida
    if texto[i]=' ' then
    begin
      posicao := strL.add(palavra);
      info := TInfoPalavra.Create;
      info.frequencia := 1;
      strL.Objects[posicao] := info;
      palavra := '';
    end
    else
      palavra := palavra+texto[i];
   end;
  //se sobrou uma ultima palavra adiciona
  if palavra <> '' then
    strL.add(palavra);
  result := strL;
end;
end.

