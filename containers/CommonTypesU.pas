{== CommonTypesU ======================================================}
{: This unit collects some common type declarations.
@author Dr. Peter Below
@desc   Version 1.0 created 2006-10-06<br/>
        Last modified       2008-03-17<p>
   }
{======================================================================}
{$BOOLEVAL OFF}{Unit depends on shortcut boolean evaluation}

unit CommonTypesU;

interface

uses Sysutils;

type
  TDynByteArray = array of Byte;
  TIntegerArray = array of Integer;
  TDoubleArray  = array of Double;
  TSingleArray  = array of Single;
  TStringArray  = array of String;
  TObjectArray  = array of TObject;
  TInterfaceArray = array of IInterface;
  {$IFNDEF UNICODE}
  UnicodeString = Widestring;
  UnicodeChar   = WideChar;
  {$ELSE}
  UnicodeChar   = Char;
  {$ENDIF}

  TDynArray = class
  public
    class function ByteArray(const A: array of Byte):TDynByteArray;
    class function IntegerArray(const A: array of Integer):TIntegerArray;
    class function DoubleArray(const A: array of Double):TDoubleArray;
    class function SingleArray(const A: array of Single):TSingleArray;
    class function StringArray(const A: array of String):TStringArray;
    class function ObjectArray(const A: array of TObject):TObjectArray;
    class function InterfaceArray(const A: array of IInterface):TInterfaceArray;
    class function StartsWith(const aSubArray, aArray: TDynByteArray):
        Boolean;
    class function ToByteArray(const Data; NumBytes: Cardinal): TDynByteArray;
  end;

implementation

class function TDynArray.ByteArray(const A: array of Byte): TDynByteArray;
begin
  SetLength(Result, Length(A));
  if Length(A) > 0 then
    Move(A[0], Result[0], Length(A)*Sizeof(A[0]));
end;

class function TDynArray.DoubleArray(
  const A: array of Double): TDoubleArray;
begin
  SetLength(Result, Length(A));
  if Length(A) > 0 then
    Move(A[0], Result[0], Length(A)*Sizeof(A[0]));
end;

class function TDynArray.IntegerArray(
  const A: array of Integer): TIntegerArray;
begin
  SetLength(Result, Length(A));
  if Length(A) > 0 then
    Move(A[0], Result[0], Length(A)*Sizeof(A[0]));
end;

class function TDynArray.InterfaceArray(
  const A: array of IInterface): TInterfaceArray;
var
  I: Integer;
begin
  SetLength(Result, Length(A));
  for I := 0 to High(A) do
     Result[I] := A[I];
end;

class function TDynArray.ObjectArray(
  const A: array of TObject): TObjectArray;
begin
  SetLength(Result, Length(A));
  if Length(A) > 0 then
    Move(A[0], Result[0], Length(A)*Sizeof(A[0]));
end;

class function TDynArray.SingleArray(
  const A: array of Single): TSingleArray;
begin
  SetLength(Result, Length(A));
  if Length(A) > 0 then
    Move(A[0], Result[0], Length(A)*Sizeof(A[0]));
end;

class function TDynArray.StartsWith(const aSubArray, aArray: TDynByteArray):
    Boolean;
begin
  Result := (Length(aArray) >= Length(aSubArray))
    and CompareMem(@aSubArray[0], @aArray[0], Length(aSubArray));
end;

class function TDynArray.StringArray(
  const A: array of String): TStringArray;
var
  I: Integer;
begin
  SetLength(Result, Length(A));
  for I := 0 to High(A) do
     Result[I] := A[I];
end;

class function TDynArray.ToByteArray(const Data; NumBytes: Cardinal):
    TDynByteArray;
begin
  SetLength(Result, NumBytes);
  Move(Data, Result[0], NumBytes);
end;

end.
