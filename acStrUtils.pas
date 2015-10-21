unit acStrUtils;

interface


uses
  classes, StrUtils, SysUtils, ComCTrls

  {$IFDEF VER150}
  , fastString
  {$ENDIF}
  ;

Type
  TInfoPalavra = class
    public
      frequencia: integer;
  end;

function getStrField(str: string; delimiter: char; index: integer): string;
function getStrField2(str: string; delimiter: char; index: integer): string;
function getintField2(str: string; delimiter: char; index: integer): integer;
function retiraEspacos(str: String): String;
function tiraPontos(str: String): String;
function removeAcento(str: String): String;
function RemoveSimbolo(str: String): String;
function extraiPalavras(texto: string): TStringList;
function achaPalavra(texto, palavra: string): integer;
procedure RTFsubstText(rtf: TRichEdit; fromText, toText: string);
//procedure wwRTFsubstText(rtf: TwwDBRichEdit; fromText, toText: string);
function strip(value: string; delimiter: string = ' '): TStringList;
function numeroIncluidoNaRegra(numero: integer; regra: string): boolean;
function strVezes(str: string; vezes: integer): string;
procedure stringListSubstText(strl: TStringList; fromText, toText: string);
function simpleCrypt(str: string): string;
function simpleDecrypt(str: string): string;
function rToTxtField(value: string; size: integer): string;
function toTxtField(value: string; size: integer): string; overload;
function toTxtField(value: integer; size: integer): string; overload;
function currencyToTxtField(value: double; size: integer): string;
function encodeFileToUTF8(fileName: string): string;
function substituiCaracterInvalido(texto: string; strConst: char = '_'): string;
function prepareIdsForIn(strIds: string): TStringList;
function EnsureTrailingSlash(const path: string): string;
function dasherize(const str: string): string;
function underscorize(const str: string): string;
procedure writeTextFile(fileName, text: string);
function readTextFile(fileName: string): string;

implementation

function strVezes(str: string; vezes: integer): string;
var
  i: integer;
begin
  result := '';
  for i := 1 to vezes do
  begin
    result := result + str;
  end;
end;

function getStrField(str: string; delimiter: char; index: integer): string;
var
  strL: TStringList;
begin
  strL := TStringList.Create;
  try
    str := AnsiReplaceStr(str, ';', '"|"');
    str := '"' + AnsiReplaceStr(str, '|', '"|"') + '"';
    strL.Delimiter := delimiter;
    strL.DelimitedText := str;
    if index>strL.Count then
      result := ''
    else
      result := strL[index-1];
  finally
    FreeAndNil(strL);
  end;
end;

function getIntField2(str: string; delimiter: char; index: integer): integer;
var
  strValue: string;
begin
  strValue := getStrfield2(str, delimiter, index);
  try
    result := strToInt(strValue);
  except
    result := 0;
  end;
end;

function getStrField2(str: string; delimiter: char; index: integer): string;
var
  ultimoIndice: integer;
  strl: array of string;
  arrLength: integer;
begin
  arrLength := 0;
  ultimoIndice := pos(delimiter, str);
  while (ultimoIndice > 0) do
  begin
    inc(arrLength);
    SetLength(strl, arrLength);
    strl[arrLength-1] := copy(str, 0, ultimoIndice-1);
    str := copy(str, ultimoIndice+1, length(str));
    ultimoIndice := pos(delimiter, str);
    if (ultimoIndice = 0) and (str <> '') then
    begin
      inc(arrLength);
      SetLength(strl, arrLength);
      strl[arrLength-1] := str;
    end;
  end;

  if (index > 0) and (index <= arrLength) then
    result := strl[index-1]
  else
    result := '';
end;


function tiraPontos(str: String): String;
var
  i: integer;
begin
  result := '';
  for i := 1 to length(str) do
    if (str[i]<>'.') then
      result := result + str[i];
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
    if (str[i]<>' ') and (str[i]<> #13) and (str[i]<> #10) then
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
  result := '';
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

procedure RTFsubstText(rtf: TRichEdit; fromText, toText: string);
var
  index: integer;
begin
  index := rtf.FindText(fromText, 0, length(rtf.Lines.GetText), []);
  while index<>-1 do
  begin
    rtf.SelStart := index;
    rtf.SelLength := length(fromText);
    rtf.SelText := toText;

    index := rtf.FindText(fromText, index+1, length(rtf.Lines.GetText), []);
  end;
end;

{procedure wwRTFsubstText(rtf: TwwDBRichEdit; fromText, toText: string);
var
  index: integer;
begin
  index := rtf.FindText(fromText, 0, length(rtf.Lines.GetText), []);
  while index<>-1 do
  begin
    rtf.SelStart := index;
    rtf.SelLength := length(fromText);
    rtf.SelText := toText;

    index := rtf.FindText(fromText, index+1, length(rtf.Lines.GetText), []);
  end;
end;}

procedure stringListSubstText(strl: TStringList; fromText, toText: string);
begin
//  strl.Text := FastReplace(strl.Text, fromText, toText);
end;

function strip(value: string; delimiter: string = ' '): TStringList;
var
  tmpStrList: TStringList;
begin
  value := '"' + AnsiReplaceStr(value, delimiter, '"' + delimiter + '"') + '"';
  tmpStrList := TStringList.Create;
  tmpStrList.Delimiter := delimiter[1];
  tmpStrList.DelimitedText := value;
  result := tmpStrList;
end;

function numeroIncluidoNaRegra(numero: integer; regra: string): boolean;
var
  faixas, intervalos: TStringList;
  j: integer;
  baseFaixa, topoFaixa: integer;
begin
  result := false;
  faixas := strip(regra, ',');
  for j := 0 to faixas.Count - 1 do
  begin
    if Pos('-', faixas[j]) <> 0 then
    begin
      intervalos := strip(faixas[j], '-');
      baseFaixa := StrToInt(intervalos[0]);
      topoFaixa := StrToInt(intervalos[1]);
      if ((numero >= baseFaixa) or (numero <= topoFaixa)) then
        result := true;
    end
    else
    begin
      baseFaixa := strToInt(faixas[j]);
      if numero = baseFaixa then
        result := true;
    end;
  end;
end;

//simpleCrypt e deCrypt sÛ devem ser chamados para strings
//simples e pequenas e È uma criptografia extremamente fraca
//apenas para proteger informaÁıes n„o muito sensÌveis e de usu·rios leigos
function simpleCrypt(str: string): string;
var
  i: integer;
begin
  result := '';
  for i := 1 to length(str) do
    result := result + FormatFloat('0000', ord(str[i]) + i);
end;

function simpleDecrypt(str: string): string;
var
  i, num, posInicial: integer;
begin
  result := '';
  for i := 1 to (length(str) div 4) do
  begin
    posInicial := (i-1)*4+1;
    num := strToInt(copy(str, posInicial, 4));
    num := num - i;
    result := result + char(num);
  end;
end;

function toTxtField(value: string; size: integer): string;
var
  tam: integer;
begin
  tam := length(value);
  if tam < size then
    result := strVezes(' ', size - tam) + value;
  if tam >= size then
    result := copy(value, 1, size);
end;

function rToTxtField(value: string; size: integer): string;
var
  tam: integer;
begin
  tam := length(value);
  if tam < size then
    result := value + strVezes(' ', size - tam);
  if tam >= size then
    result := copy(value, 1, size);
end;

function toTxtField(value: integer; size: integer): string;
begin
  result := FormatFloat(strVezes('0', size), value);
end;

function currencyToTxtField(value: double; size: integer): string;
begin
  result := FormatFloat(strVezes('0', size), value*100);
end;

function encodeFileToUTF8(fileName: string): string;
var
  fc: TStringList;
begin
  try
    fc := TstringList.Create;
    fc.LoadFromFile(fileName);
    fc.Text := UTF8Encode(fc.Text);
    fc.SaveToFile(fileName);
    result := fileName;
  finally
    FreeAndNil(fc);
  end;
end;

function substituiCaracterInvalido(texto: string; strConst: Char = '_'): string;
const
  CaracteresInvalidos = ' \ / : * ? " < > | #';
var
  i: Integer;
  str: string;
begin
  for i:=1 to Length(texto) do
  begin
    if Pos(texto[i], CaracteresInvalidos) > 0 then
      str := str + strConst
    else
      str := str + texto[i];
  end;
  Result := str;
end;

function prepareIdsForIn(strIds: string): TStringList;
var
  IDsStringList: TStringList;
  listaIds: string;
  contador: Integer;
begin
  try
    listaIds := '-1';
    IDsStringList := TStringList.Create;
    IDsStringList.Duplicates := dupIgnore;
    IDsStringList.Sorted := True;
    ExtractStrings([','], [' '], PChar(strIds), IDsStringList);

    Result := TStringList.Create;
    for contador := 1 to (IDsStringList.Count) do
    begin
      listaIds := listaIds + ',' + IDsStringList.Strings[contador - 1];
      if ((contador mod 1495) = 0) or (contador = IDsStringList.Count) then
      begin
        Result.Add(listaIds);
        listaIds := '-1';
      end;
    end;
  finally
    FreeAndNil(IDsStringList);
  end;
end;

function EnsureTrailingSlash(const path: string): string;
begin
  result := path;
  if result[Length(result)] <> '/' then
    result := result + '/';
end;

function dasherize(const str: string): string;
begin
  {$IFDEF VER150}
    result := FastReplace(str, '_', '-');
  {$ELSE}
    result := ReplaceStr(str, '_', '-');
  {$ENDIF}
end;

function underscorize(const str: string): string;
begin
  {$IFDEF VER150}
    result := FastReplace(str, '-', '_');
  {$ELSE}
    result := ReplaceStr(str, '-', '_');
  {$ENDIF}
end;

procedure writeTextFile(fileName, text: string);
var
  tf: TextFile;
begin
  AssignFile(tf, fileName);
  Rewrite(tf);
  Writeln(tf, text);
  CloseFile(tf);
end;

function readTextFile(fileName: string): string;
var
  tf: TextFile;
  text: string;
begin
  if FileExists(fileName) then
  begin
    AssignFile(tf, fileName);
    Reset(tf);
    Readln(tf, text);
    CloseFile(tf);
    Result := text;
  end;
end;

end.


