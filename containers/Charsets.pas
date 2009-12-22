{== Charsets ==========================================================}
{: This unit collects a few useful sets of character.
@author Dr. Peter Below
@desc   Version 1.0 created way, way back<br/>
        Last modified       2008-10-05 for Delphi 2009.   }
{======================================================================}
unit Charsets;
interface

uses CharactersU;

type
  TCharSet = set of AnsiChar;
const
  Signs: TCharset = ['-', '+'];
  Numerals: TCharset = ['0'..'9'];
  HexNumerals: TCharset = ['A'..'F', 'a'..'f', '0'..'9'];
  IntegerChars: TCharset = ['0'..'9', '-', '+'];
  IdentifierChars: TCharset = ['a'..'z', 'A'..'Z', '0'..'9', '_'];
  SqlWildcards: TCharset = ['_', '%'];
  DosWildcards: TCharset = ['?', '*'];
  EditOperations: TCharset = [^C, ^V, ^X, Backspace];
  TextChars: TCharSet = [Tab, LF, CR, Space..#255];
var
  Digits, Letters, LowerCaseLetters, UpperCaseLetters: TCharSet;
  FloatChars, SciFloatChars: TCharset;
  AlphaNum, NonAlphaNum: TCharset;

{ Need to call this again when locale changes.  }
procedure SetupCharsets;

{: Returns true if the passed string S contains any characters from
the set CSet, false if not.}
function StringContainsChars(const S: string; const CSet: TCharset): Boolean;

implementation

uses Windows, Sysutils, UnicodeHelpersU;

const
  Latin1Codepage = 1252;
var
  locale: DWORD = 0;

procedure SetupCharsets;
{$IFDEF UNICODE}
type
  Latin1String = type Ansistring(Latin1Codepage);
{$ENDIF}
var
  ch: AnsiChar;
{$IFDEF UNICODE}
  S: Latin1String;
  WS: Unicodestring;
  I: Integer;
  c: char;
{$ENDIF}
begin
  if locale = GetThreadLocale then
    Exit
  else
    Locale := GetThreadLocale;
  LowerCaseLetters := [];
  UpperCaseLetters := [];
  AlphaNum := [];
  NonAlphaNum := [];
  Digits := Numerals;

{$IFDEF UNICODE}
  SetLength(S, 255);
  for ch := #1 to #255 do
    S[Ord(ch)] := ch;
  WS := String(S);
  for I := 1 to Length(WS) do begin
    c:= WS[I];
    if IsCharAlpha(c) then
      if IsCharUpper(c) then
        Include(UpperCaseLetters, S[I])
      else
        Include(LowerCaseLetters, S[I]);
    if IsCharAlphanumeric(c) then
      Include(AlphaNum, S[I])
    else
      Include(NonAlphaNum, S[I]);
  end; {for}
{$ELSE}
  for ch := Low(ch) to High(ch) do begin
    if IsCharAlpha(ch) then
      if IsCharUpper(ch) then
        Include(UpperCaseLetters, ch)
      else
        Include(LowerCaseLetters, ch);
    if IsCharAlphanumeric(ch) then
      Include(AlphaNum, ch)
    else
      Include(NonAlphaNum, ch);
  end; { For }
{$ENDIF}
  Letters := LowerCaseLetters + UpperCaseLetters;
  FloatChars := IntegerChars;
  Include(FloatChars, AnsiChar(DecimalSeparator));
  SciFloatChars := FloatChars + ['e', 'E'];
end; { SetupCharsets }

function StringContainsChars(const S: string; const CSet: TCharset): Boolean;
var
  I: Integer;
begin
  Result := false;
  for I := 1 to Length(S) do
     if CharInSet(S[I], CSet) then begin
       Result := true;
       Break;
     end; {if}
end;

initialization
  SetupCharsets;
end { Unit Charsets }.
