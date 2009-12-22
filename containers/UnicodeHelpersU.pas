{== UnicodeHelpersU ===================================================}
{: This unit collects some helper routines for working with Unicodestring.
@author Dr. Peter Below
@desc   Version 1.0 created 2008-03-16<br/>
        Last modified       2008-10-05<p>
This is a compatibility helper unit for Delphi version that do not have
a native Unicodestring type and thus also no standard functions that
operate on such strings. For versions that do the implemented functions
will map to the RTL functions.   }
{======================================================================}
{$BOOLEVAL OFF}{Unit depends on shortcut boolean evaluation}
unit UnicodeHelpersU;

{$I JEDI.INC}

interface

uses CommonTypesU;

function UnicodeUpperCase(const S: UnicodeString): UnicodeString;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function UnicodeSameStr(const S1, S2: UnicodeString): Boolean;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function UnicodeSameText(const S1, S2: UnicodeString): Boolean;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function UnicodeCompareStr(const S1, S2: UnicodeString): Integer;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function UnicodeCompareText(const S1, S2: UnicodeString): Integer;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function UnicodeFormat(const Fmt: UnicodeString; const A: array of const): UnicodeString;
  {function cannot be inlined due to open array parameter. }
function UnicodeIsWhitespace(const Ch: Unicodechar): Boolean;

function PointerToHex(P: Pointer): UnicodeString;

{$IFNDEF UNICODE}
type
  TSysCharset = set of char;
function CharInSet(const Ch: AnsiChar; const aSet: TSysCharset): Boolean;
  {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
{$ENDIF}

implementation

uses Windows, SysUtils, Classes;

function UnicodeUpperCase(const S: UnicodeString): UnicodeString;
begin
  {$IFDEF UNICODE}
  Result := Uppercase(S);
  {$ELSE}
  Result := WideUppercase(S);
  {$ENDIF}
end;

function UnicodeSameStr(const S1, S2: UnicodeString): Boolean;
begin
  {$IFDEF UNICODE}
  Result := SameStr(S1, S2);
  {$ELSE}
  Result := WideCompareStr(S1, S2) = 0;
  {Note: using WideSameStr here causes a bogus warning in D2007 since
   WideSameStr is inline itself. }
  {$ENDIF}
end;

function UnicodeSameText(const S1, S2: UnicodeString): Boolean;
begin
  {$IFDEF UNICODE}
  Result := SameText(S1, S2);
  {$ELSE}
  Result := WideCompareText(S1, S2) = 0;
  {$ENDIF}
end;

function UnicodeCompareStr(const S1, S2: UnicodeString): Integer;
begin
  {$IFDEF UNICODE}
  Result := CompareStr(S1, S2);
  {$ELSE}
  Result := WideCompareStr(S1, S2);
  {$ENDIF}
end;

function UnicodeCompareText(const S1, S2: UnicodeString): Integer;
begin
  {$IFDEF UNICODE}
  Result := CompareText(S1, S2);
  {$ELSE}
  Result := WideCompareText(S1, S2);
  {$ENDIF}
end;

function UnicodeFormat(const Fmt: UnicodeString; const A: array of const): UnicodeString;
begin
  {$IFDEF UNICODE}
  Result := Format(Fmt, A);
  {$ELSE}
  Result := WideFormat(Fmt, A);
  {$ENDIF}
end;

function UnicodeIsWhitespace(const Ch: Unicodechar): Boolean;
var
  Res: Word;
begin
  Result :=
    GetStringTypeExW(LOCALE_USER_DEFAULT, CT_CTYPE1, @Ch, 1, Res);
  if Result then
    Result := (Res and (C1_SPACE or C1_CNTRL or C1_BLANK)) <> 0;
end;

function PointerToHex(P: Pointer): UnicodeString;
var
  Buffer: array [0..Sizeof(Pointer)*2] of AnsiChar;
begin
  BinToHex(@P, Buffer, Sizeof(Pointer));
  Buffer[High(Buffer)] := #0;
  {$IFDEF UNICODE}
  Result := String(Buffer);
  {$ELSE}
  Result := Buffer;
  {$ENDIF}
end;

{$IFNDEF UNICODE}
function CharInSet(const Ch: AnsiChar; const aSet: TSysCharset): Boolean;
begin
  Result := Ch IN aSet;
end;
{$ENDIF}


end.
