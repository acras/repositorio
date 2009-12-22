{== PrimesU ===========================================================}
{: Provides a GetNearestPrime function.
@author Dr. Peter Below
@desc   Version 1.0 created 2004-03-11<BR>
        Last modified       2006-02-28<P>    }
{======================================================================}
{$BOOLEVAL OFF}{Unit depends on shortcut boolean evaluation}
unit PrimesU;

interface

const
  MaxPrime = 512000;

function GetNearestPrime( num: Cardinal ): Cardinal;

implementation

uses Classes, Sysutils, SyncObjs;

var
  Guardian: TCriticalSection;
  Sieve: TBits;
  InitDone: Boolean = false;

procedure WaitForInitDone;
var
  i: Integer;
begin
  i := 0;
  while not InitDone and (i < 20) do
  begin
    Sleep(100);
    Inc(i);
  end;
  if not InitDone then
    raise Exception.Create(
      'PrimesU: Failed to initialize primes sieve in time');
end;

function GetNearestPrime( num: Cardinal ): Cardinal;
var
  i, prime1, prime2: Integer;
begin
  Assert( num > 0, 'GetNearestPrime: parameter cannot be 0' );
  if num >= MaxPrime then
    raise Exception.CreateFmt(
      'GetNearestPrime: Cannot handle num= %d, num has to be < %d',
      [num, MaxPrime]);

  WaitForInitDone;

  Guardian.Acquire;
  try
    prime1 := 1;
    for i:= num downto 1 do
      if Sieve.Bits[i] then begin
        prime1 := i;
        Break;
      end; { if }
    prime2 := 1;
    for i:= num to Sieve.Size-1 do
      if Sieve.Bits[i] then begin
        prime2 := i;
        Break;
      end; { if }

    i:= num;
    if Abs( i - prime1 ) >= Abs( i - prime2 ) then
      Result := prime2
    else
      Result := prime1;
  finally
    Guardian.Release;
  end; { try finally }
end;

type
  TSieveBuilderThread = class( TThread )
  protected
    procedure Execute; override;
  public
    constructor Create;
  end;

procedure InitSieve;
begin
  Guardian.Acquire;
  try
    Sieve := TBits.Create;
    Sieve.Size := MaxPrime;
    TSieveBuilderThread.Create;
  finally
    Guardian.Release;
  end; { try finally }
end;

{ TSieveBuilderThread }

constructor TSieveBuilderThread.Create;
begin
  inherited Create( true );
  FreeOnTerminate := true;
  Resume;
end;

{ Sieve of Erathostenes, implementation follows some Java code based on
  Deitel and Deitel, Java - How To Program, 2nd Ed, pg 971
  The Sieve bitset ends up with bits set only for prime numbers.
}

procedure TSieveBuilderThread.Execute;
var
  i, j, finalbit: Integer;
begin
  Guardian.Acquire;
  try
    try
      {Start by setting all bits in the sieve. We ignore the 0 slot. }
      for i:= 1 to Sieve.Size-1 do
        Sieve.Bits[i] := true;
      finalBit := Trunc(Sqrt( sieve.size ));

      for i :=  2 to finalBit do
        if  Sieve.Bits[i] then begin
          {remove all multiples of this value}
          j:= 2*i;
          while j < Sieve.Size do begin
            Sieve.Bits[j] := false;
            Inc(j, i);
          end; { while }
        end; { if }
    except
    end; { try except }
    InitDone := true;
  finally
    Guardian.Release;
  end; { try finally }
end;

initialization
  Guardian := TCriticalSection.Create;
  InitSieve;
finalization
  FreeAndNil(Sieve);
  FreeAndNil(Guardian);
end.
