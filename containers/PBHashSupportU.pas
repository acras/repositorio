{== PBHashSupportU ====================================================}
{: This unit collects types and constants used for hashing string keys,
  as well as several hash functions for strings.
@author Dr. Peter Below
@desc   Version 1.0 created 2004-03-12<BR>
        Last modified       2007-01-18<P>

The hash functions implemented in this unit are taken from Julian
Bucknalls "Tomes of Delphi - Algorithms and Data Structures", WordWare
Publishing Inc., 2001, with minor modifications. Some new hash
functions found on the Delphi newsgroups have been added later. }
{======================================================================}
{$BOOLEVAL OFF}{Unit depends on shortcut boolean evaluation}
unit PBHashSupportU;

interface

uses Sysutils;

type
  {: Type used to represent a hash value }
  THash         = Cardinal;

  {: Prototype for a hash function for string keys }
  THashFunction = function (const Key: string): THash;

  {: Interface a class has to implement to support storage in a
    IInterfaceHashTable }
  IHashable = interface
  ['{9DCD67C2-2F3B-4F16-A021-9749B9B43277}']
    function GetKey: string;

    {: Return the key to use for hashing }
    property Key: string read GetKey;
  end;

  {: This is the public interface of a hash table storing IHashable
    interface references. }
  IInterfaceHashTable = interface
  ['{6C3C94D8-797A-4010-810B-8E5BEB6FF48A}']

    {: Add an item to the hash table
     @param aItem is the item to add
     @param IgnoreDuplicates defines what to do if an item with the
       same key is already in the table. If true nothing is done in this
       case, if false an exception will be raised.
     @precondition aItem <> nil
     @raises EHashtableError if IgnoreDuplicates is false and an item
       with the same key as aItem is already in the table.  }
    procedure Add( aItem: IHashable; IgnoreDuplicates: Boolean = true );

    {: Remove all items from the hash table. All interfaces stored in the
      table will get their reference count decremented. }
    procedure Clear;

    {: Remove an item from the table and decrement its reference count.
     @param aKey is the items Key.
     @param IgnoreNotFound defines what to do if an item with the
       passed key is not found. If true nothing is done in this
       case, if false an exception will be raised.
     @precondition aKey <> ''
     @raises EHashtableError if IgnoreNotFound is false and an item
       with the passed key is not found in the table.  }
    procedure Delete( const aKey: string; IgnoreNotFound: Boolean = true );

    {: Remove an item from the table and return its reference.
     @param aKey is the items Key.
     @param aItem returns the item found, or Nil, if no item with the
       passed key was found.
     @returns true if an item was found, false if not
     @precondition aKey <> '' }
    function Extract( const aKey: string; var aItem: IHashable ): Boolean;

    {: Search for an item in the table and return its reference.
     @param aKey is the items Key.
     @param aItem returns the item found, or Nil, if no item with the
       passed key was found.
     @returns true if an item was found, false if not
     @precondition aKey <> '' }
    function Find( const aKey: string; var aItem: IHashable ): Boolean;
  end;

  {: Exception class used to report errors in hash tables }
  EHashtableError = class( EXception );

{: P. J. Weinbergers ELF hashing function, Julian Bucknalls TDPJWHashEx }
function ELFHash(const aKey: string) : THash;

{: Simple hashing function, Julian Bucknalls TDSimpleHash }
function SimpleHash(const aKey: string) : THash;

{: http://www.cse.yorku.ca/~oz/hash.html }
function Djb2(const aKey: string) : THash;

// http://www.partow.net/programming/hashfunctions/index.html#top
function APHash(const aKey: String): THash;

implementation

{$RANGECHECKS OFF}
{$OVERFLOWCHECKS OFF}


function ELFHash(const aKey: string) : THash;
var
  G : longint;
  i : integer;
  Hash : longint;
begin
  Hash := 0;
  for i := 1 to length(aKey) do begin
    Hash := (Hash shl 4) + ord(aKey[i]);
    G := Hash and $F0000000;
    if (G <> 0) then
      Hash := (Hash xor (G shr 24)) xor G;
  end;
  Result := Hash;
end;

function SimpleHash(const aKey: string) : THash;
var
  i : integer;
  Hash : longint;
begin
  Hash := 0;
  for i := 1 to length(aKey) do
    Hash := (Hash * 17) + ord(aKey[i]);
  Result := Hash;
end;

// http://www.cse.yorku.ca/~oz/hash.html
function Djb2(const aKey: string) : THash;
var i : integer;
begin
  result := 5381;
  for i := 1 to length(aKey) do
    result := (result shl 5) + result + ord(aKey[i]);
end;

// http://www.partow.net/programming/hashfunctions/index.html#top
function APHash(const aKey: String): THash;
var
 i : Cardinal;
begin
  Result := 0;
  for i := 1 to Length(aKey) do begin
   if Odd(i) then
     Result := Result xor ((Result shl 7) xor Ord(aKey[i])
       xor (Result shr 3))
   else
     Result := Result xor (not((Result shl 11) xor Ord(aKey[i])
       xor (Result shr 5)));
  end; {for}
end;

end.
