{== Utils =============================================================}
{: This Unit collects some general purpose routines.
@author Dr. Peter Below
@desc   Version 1.0 created a long time ago<BR>
        Current revision 1.18<BR>
        Last modified    2008-03-13<P>
1996-04-05: added TextfileSize, HasWhitespace, IsWhitespace<BR>
1997-05-19: added AsserTrailingBackslash<BR>
1997-05-21: added EnumDirectories<BR>
1997-05-27: added CopyFile, UpdateFile, AssertIsNotReadOnly<BR>
1997-06-03: added StringOfChar for Win16<BR>
1997-06-06: added ExtractFileDir for Win16<BR>
1997-06-30: added CreateBackupfile<BR>
1997-07-08: added SplitString<BR>
1997-07-22: added DirIsWriteable<BR>
1998-05-23: added FreeObject<BR>
2000-05-21: added PadLeft and Padright<BR>
2000-07-23: added RemoveFileExt<BR>
2000-08-11: added ConcatSubstrings, ConcatQuotedSubstrings<BR>
2001-03-23: added LoadStringFromFile, SaveStringToFile<BR>
2001-03-18: added LoadStringsFromArray<BR>
2001-11-09: added CountOfChar<BR>
2001-12-12: added TryStrToFloat, StrToFloatDef<BR>
2001-02-05: added CompareInt, CompareFloat<BR>
2001-03-22: added TryStringToBoolean<BR>
2002-06-20: changed all TStringlist parameters to TStrings,
            added StringIn<BR>
2002-08-19: Added JEDI.INC to handle compiler versions better <br>
2002-10-21: Added FindSwitch <br>
2002-11-07: Added ValueInArray <br>
2003-01-24: Added MixedCase<br>
2003-06-05: Added Split<br>
2003-10-30: Added ExtractStringElement and CountOfElements<br>
2004-05-05: Added SameMethod<br>
2005-06-05: Added GetfirstNonBlankString, SplitStringEx<br>
2005-11-29: Added FileExists version that uses FindFirst<br>
2006-02-06: Added RemoveWhitespace, RemoveCharsInSet<br>
2008-01-23: Added CompareInt64 and added inlining for several functions.<br>
2008-03-18: Added ByteInRange<br>
2008-10-05: Added support for Delphi 2009.
   }
{======================================================================}
unit Utils;
{$I JEDI.INC}

interface
uses Classes, Sysutils, Charsets;

{$IFDEF DELPHI1}
const
  MAX_PATH = 255;
{$ENDIF}
const
  SingleQuote = '''';
  DoubleQuote = '"';
  Space       = ' ';

{: Return the size of a file in bytes. }
function TextfileSize(const name: string): LongInt;

{: Return true if the passed string contains whitespace anywhere. }
function HasWhitespace(const S: string): Boolean;

{: Return true if the passed character is whitespace. Whitespace is
   currently defined as any characters <= #32.  }
function IsWhitespace(ch: Char): Boolean;

{: Remove whitespace from the right side of the passed string.
   Has been superseeded by Sysutils.TrimRight. }
function RTrim(const S: string): string;

{: Remove whitespace from the left side of the passed string.
   Has been superseeded by Sysutils.TrimLeft. }
function LTrim(const S: string): string;

{: Return the position of the first instance of ch in S, or 0 if
   ch was not found. }
function Scan(ch: Char; const S: string): Integer;

{: Return the position of the first instance of ch in S after
   position fromPos, or 0 if ch was not found. }
function IScan(ch: Char; const S: string; fromPos: Integer): Integer;

{: Return the position of the last instance of ch in S, or 0 if
   ch was not found. }
function RScan(ch: Char; const S: string): Integer;

{: Return the position of the last instance of ch in S before fromPos,
   or 0 if ch was not found. }
function RIScan(ch: Char; const S: string; fromPos: Integer): Integer;

{: Copy the passed string to a PChar buffer. The caller has to
   deallocate the returned buffer with StrDispose! }
function StrToPChar(const S: string): Pchar;

{: Try to allocate a buffer of nBufSize characters. If this fails the
   routine will half the requested size repeatedly until the allocation
   succeeds or the size drops below minAcceptedSize. nBufSize returns
   the size actually allocated if the routine returns <> nil. The
   caller has to deallocate the returned buffer with StrDispose.
   Note for Delphi 2009: Buffer size is measured in characters, not
   bytes! Cannot change to use PByte since that would change the sematics
   of use. }
function AllocBuffer(var nBufSize: LongInt; minAcceptedSize: Integer):
  PChar;

{: Counts the set bits in a variable. size is the size of the
   variable in bytes. }
function BitCount(var aValue; size: Integer): Integer;

{: Returns the position of the himost set bit in a variable. Size is
   the size of the variable in bytes, the LSB is position 0. }
function HiBitPosition(var aValue; size: Integer): Integer;

{: Adds a trailing backslash to the passed string if it does not
   already end in one. }
procedure AssertTrailingBackslash(var S: string);

{: If the passed file exists the routine will remove the read-only
   flag if it is set. Of course this will not work if the medium
   itself is read-only! }
procedure AssertIsNotReadonly(const fname: string);

{: Enumerate all subdirectories directly under the directory
   specified in path. The list is not cleared first, so repeated
   calls using the same list are accumulative. }
procedure EnumDirectories(path: string; list: TStrings);

{: Copy a file. Has been superseeded by the Windows.CopyFile routine. }
function CopyFile(const source, target: string): Boolean;

{: Copy the source to the target file, but only if the target
   does not exist or is older than the source. Returns true if
   the file was copied, false if not. }
function UpdateFile(const source, target: string): Boolean;

{: Copies the file with the passed filename to the new extension,
   deletes any previous file with the new name. }
procedure CreateBackupfile(const filename: string;
  const newExt: string);

{: Split the passed string into substrings at the position of the
   separator character and add the substrings to the passed list.
   The list is not cleared first! Does not deal with separators in
   quoted substrings!}
procedure SplitString(const S: string; separator: Char;
  substrings: TStrings);

{: Split the passed string into substrings at the position of the
   separator string and add the substrings to the passed list.
   The list is not cleared first! Does not deal with separators in
   quoted substrings! }
procedure SplitString2(S: string; const separator: string;
  substrings: TStrings);

{: Concatenate the items of the passed list into a single string,
   separating the items with the separator string. }
function ConcatSubstrings(const separator: string;
  substrings: TStrings): string;

{: Concatenate the items of the passed list into a single string,
   separating the items with the separator string and enclosing
   each item with the quote characters. Instances of the quote
   character inside an item will be doubled. }
function ConcatQuotedSubstrings(separator, quote: Char;
  substrings: TStrings): string;

{: Checks if the passed directory can be written to. }
function DirIsWriteable(S: string): Boolean;

{$IFDEF DELPHI1}
{ Compatibility functions for Delphi 1. }
function Trim(const S: string): string;
function StringOfChar(ch: Char; count: Byte): string;
function ExtractFileDir(const fname: string): string;
procedure SetLength(var S: string; len: Integer);
{$ENDIF}

{: Return the highest value in an array }
function MaxValue(const values: array of Extended): Extended;

{: Return the lowest value in an array }
function MinValue(const values: array of Extended): Extended;

{: Free the passed object and set the variable to Nil. Superseeded
   by Sysutils.FreeAndNil. }
procedure FreeObject(var anObject: TObject);

{: Pad a string to a given length by adding characters on the left. }
function PadLeft(const S: string; toLength: Integer;
  withChar: Char): string;

{: Pad a string to a given length by adding characters on the right. }
function PadRight(const S: string; toLength: Integer;
  withChar: Char): string;

{: Remove a files extension including the dot. }
function RemoveFileExt(const pathname: string): string;

{: Load a file into a string. For Delphi 1 the length of the
   string would be limited to 255!  }
function LoadStringFromFile(const filename: string): string;

{: Store the passed string into a file }
procedure SaveStringToFile(const S, filename: string);

{: Load an array of strings into a TStrings descendent }
procedure LoadStringsFromArray(list: TStrings;
  const A: array of string; clearlist: Boolean = true);

{: Count the number of occurence of ch in S }
function CountOfChar(ch: Char; const S: string): Integer;

{: Extract the Index-th element from a string of elements separated by
  a separator character. Index is a zero-based index. If the Index
  is >= the number of elements the function returns an empty string. }
function ExtractStringElement( const Value: string; Index: Integer;
  Separator: Char = #9 ): string;

{: Count the elements present in the Value string. Elements are
substrings separated by the Separator character, they must not
contain the separator themselves. }
function CountOfElements( const Value: string;
  Separator: Char = #9 ): Integer;


{$IFNDEF COMPILER6_UP}
{ Delphi 6 added these functions to Sysutils }
{: Try to convert a string to a floating point number, return true
   if successful, false if not. The function is locale-sensitive. }
function TryStrToFloat(const S: string; var value: Double): Boolean;
  overload;
function TryStrToFloat(const S: string; var value: Single): Boolean;
  overload;
function TryStrToFloat(const S: string; var value: Extended): Boolean;
  overload;
{$ENDIF}

{: Try to convert a string to a floating point number, if successful
   return the number, else return the passed default. }
function StrToFloatDef(const S: string; const default: Extended):
  Extended;

{: Compare two integers using the same semantics as CompareStr.
  @returns -1 if i1 < i2, 1 if i1 > i2, 0 if i1 = i2   }
function CompareInt(i1, i2: Integer): Integer;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

{: Compare two integers using the same semantics as CompareStr.
  @returns -1 if i1 < i2, 1 if i1 > i2, 0 if i1 = i2   }
function CompareInt64(i1, i2: Int64): Integer;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}


{: Compare two floats using the same semantics as CompareStr.
  @returns -1 if f1 < f2, 1 if f1 > f2, 0 if f1 = f2. Note
    that two values will be considered equal if their difference
    is < Epsilon. The default for Epsilon is 1.0E-10. }
function CompareFloat(f1, f2: Extended): Integer;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

{: Try to convert a string to a boolean value
  @param value is the string to convert
  @param b will return the converted value, if successful
  @returns true if the value could be converted, false if not. }
function TryStringToBoolean(const value: string; var b: Boolean):
  Boolean;

{: Tests if aString is in the passed array of strings
@Param aString is the string to test for
@Param A is an array of strings to test against
@Param caseSensitive determines whether the test is case-sensitive
  or not
@Returns true if aString is one of the strings in the array, false if
  not. }
function StringIn(const aString: string;
  const A: array of string;
  caseSensitive: Boolean = true): Boolean;

{: Find a commandline switch and return its value
@Param switchchar is the character identifying the switch, usually a
  letter.
@Param value returns the switches value, if it has one. A colon
  following the switchchar is removed, if the result is enclosed
  in double quotes it is unquoted.
@returns True if the switch was found, false if not. }
function FindSwitch(switchchar: Char; var value: string): Boolean;

{: Test if a value is contained in an array.
@Param value is the value to look for
@Param A is the array to look in
@Returns true if a match for value is found in A, false if not.  }
function ValueInArray(value: Integer; const A: array of Integer):
  Boolean;

{: Convert a string to all lower case, with the first character in
   upper case.
@Param S is the string to convert
@Returns the converted string, with any whitespace at start or end
  removed.   }
function MixedCase(const S: string): string;

{: Split a string on the first occurence of a separator substring.
@Param S is the string to split
@Param firstpart takes the part before the separator
@Param secondpart takes the part after the separator
@Param separator is the separator substring
@Desc The search for the substring is case-sensitive. If the substring
does not appear in the passed string S firstpart will return a copy of S
and secondpart will be empty.}
procedure Split(const S: string; var firstpart, secondpart: string;
  const separator: string);

{: Compare two method pointers, consider them the same if both code
  and data parts are the same }
function SameMethod( M1, M2: TMethod ): Boolean;
{$IFDEF SUPPORTS_INLINE}
inline;
{$ENDIF}

type
  ESplitStringError = class(Exception);

{: Split a string on a separator, handling quoted items that may contain
  the separator.
 @param S is the string to split.
 @param List takes the found items.
 @param Separator is the character separating items, a tab by default.
 @param QuoteChar is the character used to quote items, a double quote
   by default.
 @param Clearlist determines whether List will be cleared before new items
   are added, true by default.
 @precondition List <> nil.
 @raises ESplitStringError if the input string is not formatted correctly.
   This will typically happen if quote characters are not properly
   balanced.
 @desc Any quoted items found will be returned unquoted. A literal quote
   character inside a quoted string has to be doubled in the input
   string to be processed correctly. A sequence of two separators is
   interpreted as an empty item, as is a separator at the end of the
   input string. }
procedure SplitStringEx(const S: String; List: TStrings;
  Separator: char = #9; QuoteChar: char = '"'; ClearList: Boolean = true );

{: Returns the first non-empty string in Strings, or an empty string if
all strings are empty. Will remove leading and trailing whitespace from
the input strings for this test, but the returned string is NOT trimmed! }
function GetFirstNonBlankString( const Strings: array of string ): string;

{: Check if the passed file exists. Other than the Sysutils function of the
same name this one uses FindFirst, which is more reliable for network drives.}
function FileExists(const Filename: String): Boolean;

{: Remove all characters considered whitespace from the passed string
  and returns the resulting string}
function RemoveWhitespace(const S: string):string;

{: Remove all characters in aSet from the passed string
  and returns the resulting string}
function RemoveCharsInSet(const S: string; const aSet: TCharset):string;

{: Check if a byte value is in the range LowBound..HiBound,
 including the bounds.
 @precondition LowBound &lt;= HiBound }
function ByteInRange(const Value, LowBound, HiBound: Byte): Boolean;
{$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

var
  {: Set of characters considered whitespace in parsing strings. }
  {$IFDEF UNICODE}
    Whitespace: TSysCharset;
  {$ELSE}
    Whitespace: set of Char;
  {$ENDIF}
  {: Floating point numbers with a difference less than Epsilon
     are considered equal. }
  Epsilon: Extended = 1.0E-10;

implementation

uses
  Windows
  , UnicodeHelpersU;

{-- TextfileSize ------------------------------------------------------}
{: Returns the size, in bytes, of the passed file.
}{ Created 5.4.1996 by P. Below
-----------------------------------------------------------------------}

function TextfileSize(const name: string): LongInt;
var
  SRec: TSearchRec;
begin
  if FindFirst(name, faAnyfile, SRec) = 0 then begin
    Result := SRec.Size;
    Sysutils.FindClose(SRec);
  end
  else
    Result := 0;
end; { TextfileSize }

{-- HasWhitespace -----------------------------------------------------}
{: Check a string for whitespace
@Param S is the String to examine
@Returns True if the string contains a control character or a space
 anywhere, False otherwise.
}{ Created 5.4.1996 by P. Below
-----------------------------------------------------------------------}

function HasWhitespace(const S: string): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 1 to Length(S) do begin
    if CharInSet(S[i], Whitespace) then begin
      Result := True;
      Break;
    end; { If }
  end; { For }
end; { HasWhitespace }

{: Returns true if the passed character is a space or control character,
   false if not. }

function IsWhitespace(ch: Char): Boolean;
begin
  Result := CharInSet(ch, Whitespace);
end;

{-- AllocBuffer -------------------------------------------------------}
{: Tries to allocate a buffer of the requested size
@Param nBufSize is the requested size of the buffer, also returns the
   actually allocated size.
@Param minAcceptedSize If the buffer could not be allocted to at least
  this size the function returns Nil. Note that the minimum accepted for
  this parameter is 8, if the passed value is smaller it will be set
  to 8.
@Returns Pointer to the allocated buffer, or Nil, if even the minimum
 accepted size could not be allocated. Dispose of this buffer
 with StrDispose!
@Desc Tries to alloate a buffer of the requested size. If this fails
 the function reduces the size request by half and tries again.
 This is repeated until the allocation succeeds or the size
 request drops below the minAcceptedSize.
}{ Created 10.3.1996 by P. Below
-----------------------------------------------------------------------}

function AllocBuffer(var nBufSize: LongInt;
  minAcceptedSize: Integer): PChar;
begin
  if minAcceptedSize < 8 then
    minAcceptedSize := 8;
  repeat
    try
      Result := StrAlloc(nBufSize);
    except
      Result := nil
    end;
    if Result = nil then
      nBufSize := nBufSize div 2
  until (Result <> nil) or (nBufSize < minAcceptedSize);
end; { AllocBuffer }

{-- StrToPChar --------------------------------------------------------}
{: Convert a string into a PChar
@Param S is the string to convert
@Returns Pointer to the zero-terminated string generated. Dispose of
   this pointer with StrDispose!
}{ Created 19.3.1996 by P. Below
-----------------------------------------------------------------------}

function StrToPChar(const S: string): Pchar;
begin
  Result := StrAlloc(Length(S) + 1);
  StrPCopy(Result, S);
end; { StrToPChar }

{: Find the first non-whitespace character in the passed string.
   Returns the index of the found character, or Length( S ) if the
   string consists of whitespace only or is empty. }

function FindStart(const S: string): Integer;
begin
  if Length(S) > 0 then begin
    Result := 1;
    while (Result <= Length(S)) and (S[Result] <= ' ') do
      Inc(Result);
  end
  else
    Result := 0;
end;

{: Find the last non-whitespace character in the passed string.
   Returns the index of the found character, or 0 if the
   string consists of whitespace only or is empty. }

function FindEnd(const S: string): Integer;
begin
  Result := Length(S);
  while (Result > 0) and (S[Result] <= ' ') do
    Dec(Result);
end;

function RTrim(const S: string): string;
var
  n: Integer;
begin
  n := FindEnd(S);
  if n > 0 then
    Result := Copy(S, 1, n)
  else
    Result := '';
end;

function LTrim(const S: string): string;
var
  n: Integer;
begin
  n := FindStart(S);
  if (n > 0) and (n <= Length(S)) then
    Result := Copy(S, N, Maxint)
  else
    Result := EmptyStr;
end;

procedure CheckBitset(size: Integer);
begin
  if size <> 1 then
    raise Exception.Create(
      'Unit Utils. Fatal Error: the size of a Set of 0..7 is not 1 byte!');
end; { CheckBitset }

{************************************************************
 * Function BitCount
 *
 * Parameters:
 *  aValue: any kind of variable
 *  size  : the number of bytes in that variable
 * Returns:
 *  the number of bits that are set in the variable
 * Description:
 *  Loops over all bytes in the variable and counts the set
 *  bits in each by treating it as a bitset.
 * Error Conditions:
 *  Will check whether the size of a bitset is 1 byte and blow
 *  up if not, otherwise none.
 *
 *Created: 03/11/96 20:35:19 by P. Below
 ************************************************************}

function BitCount(var aValue; size: Integer): Integer;
type
{$Z-}
  Bitset = set of 0..7;
var
  proxy: array[0..High(Word) - 1] of Bitset absolute aValue;
  n: Integer;
begin
  CheckBitset(Sizeof(Bitset));
  Result := 0;
  Dec(size);
  while size >= 0 do begin
    for n := 0 to 7 do begin
      if n in proxy[size] then
        Inc(Result);
    end; { For }
    Dec(size);
  end; { While }
end; (* BitCount *)

{************************************************************
 * Function HiBitPosition
 *
 * Parameters:
 *  aValue: any kind of variable
 *  size  : the number of bytes in that variable
 * Returns:
 *  the position of the highest set bit in the variable. This
 *  position counts from 0. If no set bit is found the function
 *  will return -1!
 * Description:
 *  Loops over all bytes in the variable starting with the
 *  highest and looks for a set bit in each, also starting at
 *  the highest bit. Exits if one is found.
 * Error Conditions:
 *  Will check whether the size of a bitset is 1 byte and blow
 *  up if not, otherwise none.
 *
 *Created: 03/11/96 20:38:19 by P. Below
 ************************************************************}

function HiBitPosition(var aValue; size: Integer): Integer;
type
{$Z-}
  Bitset = set of 0..7;
var
  proxy: array[0..High(Word) - 1] of Bitset absolute aValue;
  n: Integer;
begin
  CheckBitset(Sizeof(Bitset));
  Result := -1;
  Dec(size);
  while size >= 0 do begin
    for n := 7 downto 0 do begin
      if n in proxy[size] then begin
        Result := size * 8 + n;
        Exit;
      end; { If }
    end; { For }
    Dec(size);
  end; { While }
end; (* HiBitPosition *)

{************************************************************
 * Function IScan
 *
 * Parameters:
 *  ch: Character to scan for
 *  S : String to scan
 *  fromPos: first character to scan
 * Returns:
 *  position of next occurence of character ch, or 0, if none
 *  found
 * Call method:
 *  static
 * Description:
 *  Search for next occurence of a character in a string.
 * Error Conditions:
 *  none
 *
 *Created: 11/27/96 16:57:26 by P. Below
 ************************************************************}

function IScan(ch: Char; const S: string; fromPos: Integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := fromPos to Length(S) do begin
    if S[i] = ch then begin
      Result := i;
      Break;
    end; { If }
  end; { For }
end; { IScan }

{************************************************************
 * Function Scan
 *
 * Parameters:
 *  ch: Character to scan for
 *  S : String to scan
 * Returns:
 *  position of first occurence of character ch, or 0, if none
 *  found
 * Call method:
 *  static
 * Description:
 *  Search for first occurence of a character in a string.
 * Error Conditions:
 *  none
 *
 *Created: 11/27/96 16:57:26 by P. Below
 ************************************************************}

function Scan(ch: Char; const S: string): Integer;
begin
  Result := IScan(ch, S, 1);
end; { Scan }

{************************************************************
 * Function RIScan
 *
 * Parameters:
 *  ch: Character to scan for
 *  S : String to scan
 *  fromPos: first character to scan
 * Returns:
 *  position of previous occurence of character ch, or 0, if none
 *  found
 * Call method:
 *  static
 * Description:
 *  Search for previous occurence of a character in a string.
 * Error Conditions:
 *  none
 *
 *Created: 11/27/96 16:57:26 by P. Below
 ************************************************************}

function RIScan(ch: Char; const S: string; fromPos: Integer): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := fromPos downto 1 do begin
    if S[i] = ch then begin
      Result := i;
      Break;
    end; { If }
  end; { For }
end; { RIScan }

{************************************************************
 * Function RScan
 *
 * Parameters:
 *  ch: Character to scan for
 *  S : String to scan
 * Returns:
 *  position of last occurence of character ch, or 0, if none
 *  found
 * Call method:
 *  static
 * Description:
 *  Search for last occurence of a character in a string.
 * Error Conditions:
 *  none
 *
 *Created: 11/27/96 16:57:26 by P. Below
 ************************************************************}

function RScan(ch: Char; const S: string): Integer;
begin
  Result := RIScan(ch, S, Length(S));
end; { RScan }

procedure AssertTrailingBackslash(var S: string);
begin
  if (Length(S) > 0) and (S[Length(S)] <> '\') then
    S := S + '\';
end; { AssertTrailingBackslash }

{+------------------------------------------------------------
 | Procedure EnumDirectories( path: String; list: TStrings );
 |
 | Parameters:
 |  path: name of directory to scan
 |  list: takes the names of the found directories
 | Call method:
 |  static
 | Description:
 |  searches for subdirectories in the passed path and returns
 |  their names (without path) in list. The list is not cleared
 |  first, so additions are cumulative.
 | Error Conditions:
 |  none, but passing a invalid list reference will cause an
 |  exception.
 |
 |Created: 21.05.97 by P. Below
 +------------------------------------------------------------}

procedure EnumDirectories(path: string; list: TStrings);
var
  sRec: TSearchRec;
  err: Integer;
begin
  AssertTrailingBackslash(path);
  path := path + '*.*';
  err := FindFirst(path, faDirectory, sRec);
  while err = 0 do begin
    if ((SRec.Attr and faDirectory) = faDirectory) and
      (SRec.name[1] <> '.') then
      list.Add(SRec.Name);
    err := FindNext(SRec);
  end;
  Sysutils.FindClose(SRec);
end;

procedure AssertIsNotReadonly(const fname: string);
var
  attr: Integer;
begin
  if FileExists(fname) then begin
    attr := FileGetAttr(fname);
    if (attr and faReadOnly) <> 0 then
      FileSetAttr(fname, attr xor faReadOnly);
  end; { If }
end; { AssertIsNotReadonly }

{+------------------------------------------------------------
 | Function  CopyFile(Const source, target): Boolean;
 |
 | Parameters:
 |  source: name of source file ( full path )
 |  Target: name of target file ( full path )
 | Returns:
 |  True if successful, false otherwise
 | Call method:
 |  static
 | Description:
 |  Copies the source to the target file, overwriting any
 |  existing file with the targetsname.
 | Error Conditions:
 |  Exceptions are trapped and cause the return value to be
 |  false. No message is given to the user.
 |
 |Created: 27.05.97 13:37:33 by P. Below
 +------------------------------------------------------------}
{$IFDEF VER80}{Delphi 1 version}
function CopyFile(const source, target: string): Boolean;
var
  fsource, ftarget: TFileStream;
begin
  try
    fsource := TFileStream.Create(source, fmOpenRead or
      fmShareDenyNone);
    ftarget := nil;
    try
      AssertIsNotReadonly(target);
      ftarget := TFileStream.Create(target, fmCreate);
      ftarget.CopyFrom(fSource, 0);
      FileSetDate(ftarget.Handle, FileGetDate(fSource.Handle));
    finally
      fsource.Free;
      ftarget.Free;
    end;
    Result := True;
  except
    Result := False;
  end;
end;
{$ELSE}
function CopyFile(const source, target: string): Boolean;
begin
  Result := Windows.CopyFile(PChar(Source), PChar(target), false);
end;
{$ENDIF}

{+------------------------------------------------------------
 | Function  UPdateFile(Const source, target: String): Boolean;
 |
 | Parameters:
 |  source: name of source file ( full path )
 |  Target: name of target file ( full path )
 | Returns:
 |  True if successful, false otherwise
 | Call method:
 |  static
 | Description:
 |  Copies the source to the target file, but only if the target
 |  does not exist or is older than the source.
 | Error Conditions:
 |  Exceptions are trapped and cause the return value to be
 |  false. No message is given to the user.
 |
 |Created: 27.05.97 13:37:33 by P. Below
 +------------------------------------------------------------}
{$IFDEF DELPHI10_UP}{$WARN SYMBOL_DEPRECATED OFF}{$ENDIF}
{Delphi 2006 deprecates the old version of FileAge.}
function UpdateFile(const source, target: string): Boolean;
begin
  if FileExists(target) then
    if FileAge(source) <= FileAge(target) then begin
      Result := True;
      Exit; { nothing to do }
    end;
  Result := CopyFile(source, target);
end; { UpdateFile }
{$IFDEF DELPHI10_UP}{$WARN SYMBOL_DEPRECATED ON}{$ENDIF}

{$IFDEF DELPHI1}
{************************************************************
 * Function Trim
 *
 * Parameters:
 *  S: Pascal string to trim
 * Returns:
 *  The input string minus any whitespace at start and end.
 *  The returned string may be empty.
 * Description:
 *  Finds the first and last non-whitespace characters in S
 *  and returns the characters in between.
 * Error Conditions:
 *  none
 *
 *Created: 03/10/96 14:27:29 by P. Below
 ************************************************************}

function Trim(const S: string): string;
var
  ns, ne: Integer;
begin
  ns := FindStart(S);
  ne := FindEnd(S);
  if (ns <= ne) and (ns > 0) then
    Result := Copy(S, ns, ne - ns + 1)
  else
    Result := EmptyStr;
end;

function StringOfChar(ch: Char; count: Byte): string;
begin
  Result[0] := Chr(count);
  FillChar(Result[1], count, ch);
end;

function ExtractFileDir(const fname: string): string;
var
  n: Integer;
begin
  Result := ExtractFilePath(fname);
  n := Length(Result);
  if (n > 0) and (Result[n] = '\') then
    Delete(Result, n, 1);
end;

procedure SetLength(var S: string; len: Integer);
begin
  if len > 255 then
    raise Exception.CreateFmt(
      'The requested string length of %d is too long, a Delphi 1 ' +
      'string cannot be longer than 255 characters.');
  S[0] := Chr(len);
end;
{$ENDIF}

{+------------------------------------------------------------
 | Procedure CreateBackupfile
 |
 | Parameters:
 |  filename: name of file to backup
 |  newExt  : extension to use for backup
 | Call method:
 |  static
 | Description:
 |  Copies the file with the passed filename to the new extension,
 |  deletes any previous file with the new name.
 | Error Conditions:
 |  May run out of diskspace during the copy, which would raise
 |  an exception.
 |
 |Created: 30.06.97 11:21:52 by P. Below
 +------------------------------------------------------------}

procedure CreateBackupfile(const filename: string; const newExt:
  string);
var
  bakname: string;
begin
  if FileExists(filename) then begin
    if newExt[1] = '.' then
      bakname := ChangeFileExt(filename, newExt)
    else
      bakname := ChangeFileExt(filename, '.' + newExt);
    if FileExists(bakname) then
      Sysutils.DeleteFile(bakname);
    CopyFile(filename, bakname);
  end; { If }
end; { CreateBackupfile }

{+------------------------------------------------------------
 | Procedure SplitString
 |
 | Parameters:
 |  S: String to split
 |  separator: character to use as separator between substrings
 |  substrings: list to take the substrings
 | Call method:
 |  static
 | Description:
 |  Isolates the individual substrings and copies them into the
 |  passed stringlist. Note that we only add to the list, we do
 |  not clear it first! If two separators follow each other directly
 |  an empty string will be added to the list.
 | Error Conditions:
 |  will do nothing if the stringlist is not assigned
 |
 |Created: 08.07.97 09:52:02 by P. Below
 +------------------------------------------------------------}

procedure SplitString(const S: string; separator: Char; substrings:
  TStrings);
var
  i, n: Integer;
begin
  if Assigned(substrings) and (Length(S) > 0) then begin
    i := 1;
    repeat
      n := IScan(separator, S, i);
      if n = 0 then
        n := Length(S) + 1;
      substrings.Add(Copy(S, i, n - i));
      i := n + 1;
    until i > Length(S);
  end; { If }
end; { SplitString }

procedure SplitString2(S: string; const separator: string;
  substrings: TStrings);
var
  i: Integer;
begin
  if Assigned(substrings) and (Length(S) > 0) then begin
    repeat
      i := Pos(separator, S);
      if i > 0 then begin
        substrings.Add(Copy(S, 1, i - 1));
        Delete(S, 1, i + Length(separator) - 1);
      end { If }
      else begin
        substrings.Add(S);
        Break;
      end; { Else }
    until Length(S) = 0;
  end;
end; { SplitString2 }

{+------------------------------------------------------------
 | Function DirIsWriteable
 |
 | Parameters:
 |  S: name of the directory, may contain a trailing backslash
 | Returns:
 |  True if the directory is writeable, false otherwise.
 | Call method:
 |  static
 | Description:
 |  Tries to create a testfile in the passed directory. Errors
 |  are trapped. The testfile is deleted again.
 | Error Conditions:
 |  none
 |
 |Created: 22.07.97 11:23:17 by P. Below
 +------------------------------------------------------------}

function DirIsWriteable(S: string): Boolean;
var
  olderr: Cardinal;
  fs: TFileStream;
begin
  AssertTrailingBackslash(S);
  S := S + IntToHex(GetTickCount, 8) + '.TMP';
  olderr := SetErrormode(SEM_FAILCRITICALERRORS or
    SEM_NOOPENFILEERRORBOX);
  Result := True;
  try
    fs := TFileStream.Create(S, fmCreate);
    try
      fs.WriteBuffer(olderr, Sizeof(olderr));
    finally
{$IFDEF COMPILER2_UP}
      FlushFileBuffers(fs.handle);
{$ENDIF}
      fs.Free;
{$IFDEF DELPHI1}
      asm
          mov ah, $D     { DOS reset disk, to flush cache }
          call Dos3Call
      end;
{$ENDIF}
      try
        Sysutils.DeleteFile(S);
      except
      end;
    end;
  except
    Result := False;
  end;
  SetErrormode(olderr);
end;

function MaxValue(const values: array of Extended): Extended;
var
  isFirst: Boolean;
  i: Integer;
begin
  Result := 0.0;
  isFirst := True;

  for i := Low(values) to High(values) do begin
    if isFirst then begin
      isFirst := False;
      Result := values[i];
    end { If }
    else if Result < values[i] then
      Result := values[i];
  end; { For }
end; { MaxValue }

function MinValue(const values: array of Extended): Extended;
var
  isFirst: Boolean;
  i: Integer;
begin
  Result := 0.0;
  isFirst := True;

  for i := Low(values) to High(values) do begin
    if isFirst then begin
      isFirst := False;
      Result := values[i];
    end { If }
    else if Result > values[i] then
      Result := values[i];
  end; { For }
end; { MinValue }

procedure FreeObject(var anObject: TObject);
var
  temp: TObject;
begin
  temp := anObject;
  anObject := nil;
  temp.Free;
end; { FreeObject }

{+----------------------------------------------------------------------
 | Function PadLeft
 |
 | Parameters :
 |   S        : string to pad
 |   toLength : length of string to return
 |   withChar : character to use for padding
 | Returns    : S padded to the requested length on the left with
 |              withChar
 | Error Conditions: none
 | Created: 20.5.2000 by P. Below
 +----------------------------------------------------------------------}

function PadLeft(const S: string; toLength: Integer;
  withChar: Char): string;
begin { PadLeft }
  if Length(S) < toLength then
    Result := StringOfChar(withChar, toLength - Length(S)) + S
  else
    Result := S;
end; { PadLeft }

{+----------------------------------------------------------------------
 | Function PadRight
 |
 | Parameters :
 |   S        : string to pad
 |   toLength : length of string to return
 |   withChar : character to use for padding
 | Returns    : S padded to the requested length on the right with
 |              withChar
 | Error Conditions: none
 | Created: 20.5.2000 by P. Below
 +----------------------------------------------------------------------}

function PadRight(const S: string; toLength: Integer;
  withChar: Char): string;
begin { PadRight }
  if Length(S) < toLength then
    Result := S + StringOfChar(withChar, toLength - Length(S))
  else
    Result := S;
end; { PadRight }

function RemoveFileExt(const pathname: string): string;
begin
  Result := ChangeFileExt(pathname, '');
end; { RemoveFileExt }

{ This is the inverse of SplitString, it composes a string from the
  substrings, with a separator in between }

function ConcatSubstrings(const separator: string;
  substrings: TStrings): string;
var
  i: Integer;
begin
  Result := EmptyStr;
  for i := 0 to substrings.count - 1 do begin
    if i > 0 then
      Result := Result + separator;
    Result := Result + substrings[i];
  end; { For }
end; { ConcatSubstrings }

{ Same as ConcatSubstrings, but quotes each substring. }

function ConcatQuotedSubstrings(separator, quote: Char;
  substrings: TStrings): string;
var
  i: Integer;
begin
  Result := EmptyStr;
  for i := 0 to substrings.count - 1 do begin
    if i > 0 then
      Result := Result + separator;
    Result := Result + AnsiQuotedStr(substrings[i], quote);
  end; { For }
end; { ConcatQuotedSubstrings }

function LoadStringFromFile(const filename: string): string;
var
  fs: TFilestream;
begin
  fs := TFileStream.Create(filename, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Result, fs.Size);
    fs.ReadBuffer(Result[1], Length(Result));
  finally
    fs.free
  end;
end; { LoadStringFromFile }

procedure SaveStringToFile(const S, filename: string);
var
  fs: TFilestream;
begin
  fs := TFileStream.Create(filename, fmCreate);
  try
    if Length(S) > 0 then
      fs.WriteBuffer(S[1], Length(S));
  finally
    fs.free
  end;
end; { SaveStringToFile }

procedure LoadStringsFromArray(list: TStrings; const A: array of
  string;
  clearlist: Boolean = true);
var
  i: Integer;
begin
  Assert(Assigned(list), 'LoadStringsFromArray: list is nil');
  if clearlist then
    list.Clear;
  list.BeginUpdate;
  try
    for i := Low(A) to HIgh(A) do
      list.Add(A[i]);
  finally
    list.EndUpdate;
  end; { Try finally }
end; { LoadStringsFromArray }

function CountOfChar(ch: Char; const S: string): Integer;
var
  i: Integer;
begin
  Result := 0;
  for i := 1 to Length(S) do
    if S[i] = ch then
      Inc(Result);
end;

{$IFNDEF COMPILER6_UP}

function TryStrToFloat(const S: string; var value: Double): Boolean;
begin
  try
    value := StrToFloat(s);
    Result := true;
  except
    Result := false;
  end; { except }
end;

function TryStrToFloat(const S: string; var value: Single): Boolean;
begin
  try
    value := StrToFloat(s);
    Result := true;
  except
    Result := false;
  end; { except }
end;

function TryStrToFloat(const S: string; var value: Extended): Boolean;
begin
  try
    value := StrToFloat(s);
    Result := true;
  except
    Result := false;
  end; { except }
end;

{$ENDIF}

function StrToFloatDef(const S: string; const default: Extended):
  Extended;
begin
  if not TryStrToFloat(S, Result) then
    Result := default;
end;

function CompareInt(i1, i2: Integer): Integer;
begin
  if i1 < i2 then
    Result := -1
  else if i1 > i2 then
    Result := 1
  else
    Result := 0;
end;

function CompareInt64(i1, i2: Int64): Integer;
begin
  if i1 < i2 then
    Result := -1
  else if i1 > i2 then
    Result := 1
  else
    Result := 0;
end;

function CompareFloat(f1, f2: Extended): Integer;
begin
  if Abs(f1 - f2) < Epsilon then
    Result := 0
  else if f1 < f2 then
    Result := -1
  else
    Result := 1;
end;

function TryStringToBoolean(const value: string; var b: Boolean):
  Boolean;
const
  BoolValues: array[Boolean] of string =
  ('"0","FALSE","NO","NEIN","N","FALSCH","F"',
    '"1","TRUE","YES","JA","Y","J","WAHR","W"');
var
  S: string;
begin
  S := '"' + UpperCase(value) + '"';
  Result := Pos(S, BoolValues[false]) > 0;
  if Result then
    b := False
  else begin
    Result := Pos(S, BoolValues[true]) > 0;
    if Result then
      b := true;
  end; { Else }
end;

function StringIn(const aString: string;
  const A: array of string;
  caseSensitive: Boolean = true): Boolean;
var
  i: Integer;
begin { StringIn }
  Result := false;
  for i := Low(A) to High(A) do begin
    if caseSensitive then
      Result := AnsiCompareStr(aString, A[i]) = 0
    else
      Result := AnsiCompareText(aString, A[i]) = 0;
    if Result then
      Break;
  end; { For }
end; { StringIn }

function IsCmdLineSwitch(const S: string): Boolean;
begin
  Result := (S <> '') and CharInSet(S[1], ['-', '/']);
end; {  }

function FindSwitch(switchchar: Char; var value: string): Boolean;
var
  I: Integer;
  S: string;
begin
  Result := false;
  value := '';
  for i := 1 to ParamCount do begin
    S := ParamStr(i);
    if IsCmdLineSwitch(S) and (Length(S) >= 2) and
      (UpCase(S[2]) = UpCase(switchchar)) then begin
      result := true;
      if Length(S) > 2 then begin
        value := Copy(S, 3, maxint);
        if value[1] = ':' then
          Delete(value, 1, 1);
        if (value <> '') and (value[1] = '"') then
          value := AnsiDequotedStr(value, '"');
      end; { If }
      Break;
    end; { If }
  end; { For }
end; { FindSwitch }

function ValueInArray(value: Integer; const A: array of Integer):
  Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := Low(A) to HIgh(A) do
    if A[i] = value then begin
      Result := true;
      Exit;
    end; { If }
end;

function MixedCase(const S: string): string;
begin
  Result := AnsiLowerCase(Trim(S));
  if Result <> '' then
    Result[1] := AnsiUpperCase(Result[1])[1];
end;

procedure Split(const S: string; var firstpart, secondpart: string;
  const separator: string);
var
  n: Integer;
begin { Split }
  n := Pos(separator, S);
  if n > 0 then begin
    firstpart := Copy(S, 1, n - 1);
    secondpart := Copy(S, n + Length(separator), maxint);
  end
  else begin
    firstpart := S;
    secondpart := '';
  end;
end; { Split }

function ExtractStringElement( const Value: string; Index: Integer;
  Separator: Char ): string;
var
  i, Startpos, Endpos: Integer;
begin
  Startpos := 0;
  for i:= 1 to Index do begin
    Startpos := IScan( Separator, Value, Startpos+1 );
    if Startpos = 0 then begin
      Result := '';
      Exit
    end; { if }
  end; { for }
  Inc( Startpos );
  EndPos := IScan( Separator, Value, Startpos );
  if Endpos = 0 then
    Result := Copy( Value, Startpos, Maxint )
  else
    Result := Copy( Value, Startpos, EndPos-Startpos );
end;

function CountOfElements( const Value: string;
  Separator: Char ): Integer;
begin
  Result := CountOfChar( Separator, Value ) + 1;
end;

function SameMethod( M1, M2: TMethod ): Boolean;
begin
  Result := (M1.Code = M2.Code) and (M1.Data = M2.Data);
end;

procedure SplitStringEx(const S: String; List: TStrings;
  Separator: char = #9; QuoteChar: char = '"'; ClearList: Boolean = true );
var
  ScanState : (ssStart, ssReadItem, ssReadQuotedItem);
  CurrentIndex, TargetIndex: Integer;
  Buffer: string;
  CurrentChar: Char;

  procedure MalformedItemError;
  begin
    raise ESplitStringError.CreateFmt(
      'SplitStringEx: encountered imbalanced quotes in string <%s>',
      [S]);
  end;

begin
  Assert(Assigned(List));
  if ClearList then
    List.Clear;
  if S = '' then Exit;
  SetLength(Buffer, Length(S));
  CurrentIndex := 1;
  TargetIndex := 0;
  ScanState := ssStart;
  while CurrentIndex <= Length(S) do begin
    CurrentChar := S[CurrentIndex];
    if CurrentChar =  Separator then begin
      if ScanState <> ssReadQuotedItem then begin
        List.Add(Copy(Buffer, 1, TargetIndex));
        TargetIndex := 0;
        ScanState := ssStart;
      end // if
      else begin
        Inc(TargetIndex);
        Buffer[TargetIndex] := Separator;
      end; // else
    end // if separator
    else if CurrentChar =  QuoteChar then begin
        case ScanState of
          ssStart:
            ScanState := ssReadQuotedItem;
          ssReadQuotedItem:
            if (CurrentIndex = Length(S)) or
              (S[CurrentIndex+1] = Separator)
            then
              ScanState := ssReadItem
            else begin
              Inc(CurrentIndex);
              if S[CurrentIndex] = QuoteChar then begin
                Inc(TargetIndex);
                Buffer[TargetIndex] := QuoteChar;
              end
              else
                MalformedItemError;
            end;
        else
          MalformedItemError;
        end; // case ScanState
    end  // if QuoteChar
    else begin
      if ScanState <> ssReadQuotedItem then
        ScanState := ssReadItem;
      Inc(TargetIndex);
      Buffer[TargetIndex] := CurrentChar;
    end; // else
    Inc(CurrentIndex);
  end; // while
  if ScanState = ssReadQuotedItem then
    MalformedItemError;
  List.Add(Copy(Buffer, 1, TargetIndex));
end;

function GetFirstNonBlankString( const Strings: array of string ): string;
var
  I: Integer;
begin
  Result := '';
  for I := Low(Strings) to High(Strings) do
    if Trim(Strings[I]) <> '' then begin
      Result := Strings[I];
      Break;
    end; { if }
end;

function FileExists(const Filename: string): Boolean;
var
  sRec: TSearchRec;
begin
  Result := true;  
  if FindFirst(Filename, faAnyFile, sRec) = 0 then
    Sysutils.FindClose(SRec)
  else
    Result := false;
end;

function RemoveWhitespace(const S: string):string;
begin
  Result := RemoveCharsInSet(S, Whitespace);
end;

function RemoveCharsInSet(const S: string; const aSet: TCharset):string;
var
  I: Integer;
begin
  Result := S;
  for I := Length(S) downto 1 do
    if CharInSet(S[I], aSet) then
      Delete(Result, I, 1);
end;

function ByteInRange(const Value, LowBound, HiBound: Byte): Boolean;
begin
  Assert(LowBound <= HiBound);
  Result := (Value >= LowBound) and (Value <= HiBound);
end;


initialization
  Whitespace := [#0..' '];
end.
