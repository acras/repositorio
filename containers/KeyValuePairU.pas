{== KeyValuePairU =====================================================}
{: This unit implements key-value pair handling using an interface-
 based design and Unicode strings.
@author Dr. Peter Below
@desc   Version 1.0 created 2008-03-13<br/>
        Last modified       2008-03-16<p>
The unit uses the UnicodeString type throughout. In Delphi versions
before Tiburon (Delphi 2007 successor) UnicodeString will be an alias
for Widestring. </p><p>
The unit offers three kinds of store for key value pairs which are
optimized for small, medium, and large lists of such pairs. The small
size version uses a simple unsorted list as storage and has to iterate
over the lists sequentially to find an item. The medium version uses
a sorted list with binary search to find an item. The large version
uses a hashtable internally and has the largest memory footprint and
processing overhead for adding and removing items. The small and medium
versions have the same memory footprint but the medium version has
a significantly higher processing overhead for adding items.</p><p>
The store can be configured to use a case-sensitive or case-insensitive
search for a key. The case-insensitive version is less efficient.   }
{======================================================================}
{$BOOLEVAL OFF}{Unit depends on shortcut boolean evaluation}
unit KeyValuePairU;

interface

uses CommonTypesU, Classes, Sysutils, EnumeratorIntfU;

type
  {: Public interface of a key-value pair. The key is
    set when the implementing object is created and read-only, the
    value can be changed.}
  IKeyValuePair = interface(IInterface)
  ['{5E808937-16B7-403A-ABB7-8B6906E35CB0}']
    function GetKey: UnicodeString;
    function GetValue: UnicodeString;
    procedure SetValue(const Value: UnicodeString);
    property Key: UnicodeString read GetKey;
    property Value: UnicodeString read GetValue write SetValue;
  end;

  {: A key value pair can implement this interface to provide a key
    to sort on. }
  ISortKeyValuePair = interface(IKeyValuePair)
  ['{2742E72D-BD85-4769-8756-7519F687659C}']
    function GetSortKey: UnicodeString;
    property SortKey: UnicodeString read GetSortKey;
  end;

  {: Public interface of an enumerator for a list of key value pairs.
   It inherits the navigation methods from IEnumerator. }
  IKeyValuePairEnum = interface(IEnumerator)
  ['{8CFEAC5B-56B9-4949-9863-113D4B9F5062}']
    function Current: IKeyValuePair;
  end;

  {: Public interface of a store for key value pairs. The store can
   be configured to be case-sensitive or not and offers methods to
   add, delete, and find key value pairs. A key can appear in the store
   only once, trying to add a duplicate will overwrite the value of the
   existing key. }
  IKeyValuePairStore = interface(IInterface)
  ['{5136BC8A-6AAD-48BD-8605-4DEE37F76064}']
    {: Add a key value pair. If the key already exists in the store
     its value will be replaced.
     @raises EKeyValuePairStoreError if aKey is empty.}
    procedure Add(const aKey, aValue: UnicodeString);

    {: Delete all entries from the store.}
    procedure Clear;

    {: Checks whether the store contains the passed key. aKey can be
      empty, this will return false since the store cannot contain an
      empty key.}
    function Contains(const aKey: UnicodeString): Boolean;

    {: Delete the passed key and its value from the store. If the key
     is not found nothing is done, this is not considered an error.}
    procedure Delete(const aKey: UnicodeString);

    {: Find a key value pair in the store.
     @param aKey is the key to look for. It can be empty.
     @param aPair returns the interface for the found key value pair,
       or nil, if the key was not found.
     @returns true if the key was found, false otherwise. }
    function Find(const aKey: UnicodeString; out aPair: IKeyValuePair):
      Boolean; overload;

    {: Find a key value pair in the store.
     @param aKey is the key to look for. It can be empty.
     @param aValue returns the value for the found key,
       or '', if the key was not found.
     @returns true if the key was found, false otherwise. }
    function Find(const aKey: UnicodeString; out aValue: UnicodeString):
      Boolean; overload;


    function GetCaseSensitive: Boolean;
    function GetEnumerator: IKeyValuePairEnum;
    function GetValues(aKey: UnicodeString): UnicodeString;
    procedure SetCaseSensitive(const Value: Boolean);
    procedure SetValues(aKey: UnicodeString; const Value: UnicodeString);

    {: Determines whether the store handles keys case sensitive or not.
     @raises EKeyValuePairStoreError if the property is set when
       the store already contains items. This could lead to problems
       with duplicate items. }
    property CaseSensitive: Boolean read GetCaseSensitive write
      SetCaseSensitive;

    {: Returns an enumerator for the items in the store. The enumerator
      operates on a deep copy of the stores internal list, the values of
      items in the store cannot be changed via the items returned by the
      enumerator! The items are not guaranteed to be returned in any
      particular order. }
    property Enumerator: IKeyValuePairEnum read GetEnumerator;

    {: Gets or sets the value for a key in the store. If the property
      is read for a key that is not in the store an empty string is
      returned. If the property is set for a key not already in the
      store a new key value pair is added.
      @precondition aKey cannot be empty. }
    property Values[aKey: UnicodeString]: UnicodeString read GetValues
      write SetValues;
  end;

  {: Defines the kind of implementations we offer for the key value
    store.
    @enum kvkSimple gives a store that uses an unsorted list as storage.
      It has low overhead but searches for a key are slower. This
      implementation is suitable for a small number of items (&lt; 10)
      or if the search speed is not critical.
    @enum kvkSorted gives a store that uses a sorted list and binary
      search to find entries. It is faster on search but slower on
      additions than kvkSimple but has similar memory requirements. This
      implementation is suitable for lists that are build once and
      searched frequently.
    @enum kvkHashed gives a store that uses a hash table for finding
      items. This gives O(0) performance but requires a lot more memory
      than the other two implementations. This implementation is suitable
      for large lists (&gt; 1000 entries), even if it changes frequently.}
  TKeyValuePairStoreKind = (kvkSimple, kvkSorted, kvkHashed);

  {: This class is used to report errors in methods of the key value
   store. }
  EKeyValuePairStoreError = class(Exception);

{: Create a key value store of the passed kind and return its interface.
 The returned store will be case sensitive by default.} 
function CreateKeyValuePairStore(
  Kind : TKeyValuePairStoreKind = kvkSimple): IKeyValuePairStore;


implementation

uses UnicodeHelpersU, PBHashSupportU, PBInterfaceHashtableU;

resourcestring
  SCannotAddEmptyKey = 'Cannot add an empty key';
{$IFDEF GERMAN}
  EEnumeratorOutOfBounds = 'Key value pair enumerator out of bounds';
  ECaseSensitiveCannotBeSet =
    'Die CaseSensitive Eigenschaft kann nur gesetzt werden, '+
    'wenn die Liste noch leer ist.';
{$ELSE}
  EEnumeratorOutOfBounds = 'Key value pair enumerator out of bounds';
  ECaseSensitiveCannotBeSet =
    'The CaseSensitive property can only be set if the list '+
    'is still empty.';
{$ENDIF}

type
  {: This store uses an unsorted list to manage the added items. As a
    consequence searches are slow on larger lists, since the search is
    sequential. Both additions and deletions are also affected since they
    search for the items to add or delete first. Performance is O(n) for
    these operations. }
  TSimpleKeyValuePairStore = class(TInterfacedObject, IKeyValuePairStore)
  private
    FCaseSensitive: Boolean;
    FList: IInterfacelist;
  protected
    procedure Add(const aKey: UnicodeString; const aValue: UnicodeString);
    procedure AddNew(const aKey, aValue: UnicodeString; Index: Integer);
      virtual;
    procedure CaseSensitiveChanging; virtual;
    procedure Clear; virtual;
    function Contains(const aKey: UnicodeString): Boolean;
    procedure Delete(const aKey: UnicodeString); virtual;
    function Find(const aKey: UnicodeString; out aPair: IKeyValuePair):
        Boolean; overload; virtual;
    function Find(const aKey: UnicodeString; out aValue: UnicodeString):
        Boolean; overload;
    function GetCaseSensitive: Boolean;
    function GetEnumerator: IKeyValuePairEnum;
    function GetValues(aKey: UnicodeString): UnicodeString;
    function InternalFind(aKey: UnicodeString; var Index: Integer; out
      Intf: IKeyValuePair): Boolean; virtual;
    procedure SetCaseSensitive(const Value: Boolean);
    procedure SetValues(aKey: UnicodeString; const Value: UnicodeString);
    property CaseSensitive: Boolean read GetCaseSensitive write
        SetCaseSensitive;
    property List: IInterfacelist read FList;
    property Values[aKey: UnicodeString]: UnicodeString read GetValues
      write SetValues;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  {: This class is used to store a key value pair. To speed up searching
   it also maintains a sort key, which will be the upper-case version
   of the key for case-insensitive searches. Only the value can be
   changed after the object has been created.}
  TKeyValuePair = class(TInterfacedObject, IKeyValuePair,
    ISortKeyValuePair)
  private
    FKey: UnicodeString;
    FSortKey: UnicodeString;
    FValue: UnicodeString;
  protected
    function GetKey: UnicodeString;
    function GetSortKey: UnicodeString;
    function GetValue: UnicodeString;
    procedure SetValue(const Value: UnicodeString);
  public
    constructor Create(const aKey, aValue: UnicodeString; CaseSensitive:
      Boolean); virtual;
    property Key: UnicodeString read GetKey;
    property SortKey: UnicodeString read GetSortKey;
    property Value: UnicodeString read GetValue write SetValue;
  end;

  {: This class implements the enumerator for key value pairs. Its
   constructor makes a deep copy of the list it is passed, the
   enumerator is thus independent of the original list. }
  TKeyValuePairEnum = class(TAbstractEnumerator, IKeyValuePairEnum)
  private
    FList: IInterfaceList;
  protected
    property List: IInterfaceList read FList;
  public
    constructor Create(aList: IInterfaceList); reintroduce;
    function Current: IKeyValuePair;
    function GetCount: Integer; override;
  end;

  {: This class uses a sorted list to manage the added items. This makes
   searching, adding and deleting faster, since we can use binary search,
   the performance is O(Log(n)). An enumerator for this class will
   enumerate the items in sort order.  }
  TSortedKeyValuePairStore = class(TSimpleKeyValuePairStore)
  protected
    procedure AddNew(const aKey, aValue: UnicodeString; Index: Integer);
      override;
    function InternalFind(aKey: UnicodeString; var Index: Integer; out
      Intf: IKeyValuePair): Boolean; override;
  end;

  {: This class uses an additional hash table to speed up searching.
    The performance for search and add is O(1) but delete is O(n), since
    we still maintain the unsorted list used by the ancestor and have
    to delete from that list as well. }
  THashedKeyValuePairStore = class(TSimpleKeyValuePairStore)
  private
    FHashtable: IInterfaceHashtable;
  protected
    procedure AddNew(const aKey, aValue: UnicodeString; Index: Integer);
      override;
    procedure Clear; override;
    procedure Delete(const aKey: UnicodeString); override;
    function InternalFind(aKey: UnicodeString; var Index: Integer; out
      Intf: IKeyValuePair): Boolean; override;
  public
    constructor Create; override;
  end;

  {: This class is used to store a key value pair and also offers a hash
   key for it. If the Delphi version used is Unicode-enabled the class
   needs no additional storage since its SortKey can be used as hash key.
   Otherwise we have to maintain an additional field storing the sort
   key as UTF-8 encoded string. }
  THashedKeyValuePair = class(TKeyValuePair, IHashable)
  {$IFDEF UNICODE}
  protected
    function IHashable.GetKey = GetSortkey;
  {$ELSE}
  private
    FHashKey: string;
  protected
    function GetHashkey: string;
    function IHashable.GetKey = GetHashKey;
  public
    constructor Create(const aKey, aValue: UnicodeString; CaseSensitive:
      Boolean); override;
  {$ENDIF}
  end;

function CreateKeyValuePairStore(Kind : TKeyValuePairStoreKind =
    kvkSimple): IKeyValuePairStore;
begin
  Result := nil;
  case Kind of
    kvkSimple:
      Result := TSimpleKeyValuePairStore.Create as IKeyValuePairStore;
    kvkSorted: 
      Result := TSortedKeyValuePairStore.Create as IKeyValuePairStore;
    kvkHashed:
      Result := THashedKeyValuePairStore.Create as IKeyValuePairStore;
  else
    Assert(false, 'CreateKeyValuePairStore: Unexpected kind');
  end;
end;

{== TKeyValuePairStore ================================================}

constructor TSimpleKeyValuePairStore.Create;
begin
  inherited Create;
  FList := TInterfacelist.Create();
  FCaseSensitive := true;
end;

destructor TSimpleKeyValuePairStore.Destroy;
begin
  FList := nil;
  inherited Destroy;
end;

procedure TSimpleKeyValuePairStore.Add(const aKey, aValue: UnicodeString);
var
  Intf: IKeyValuePair;
  Index: Integer;
begin
  if aKey = '' then
    raise EKeyValuePairStoreError.Create(SCannotAddEmptyKey);
  List.Lock;
  try
    if InternalFind(aKey, Index, Intf) then
      Intf.Value := aValue
    else
      AddNew(aKey, aValue, Index);
  finally
    List.Unlock;
  end; {finally}
end;

procedure TSimpleKeyValuePairStore.AddNew(const aKey, aValue:
  UnicodeString; Index: Integer);
begin
  List.Add(TKeyValuePair.Create(aKey, aValue, CaseSensitive) as IInterface);
end;

procedure TSimpleKeyValuePairStore.CaseSensitiveChanging;
begin
  if List.Count > 0 then
    raise EKeyValuePairStoreError.Create(ECaseSensitiveCannotBeSet);
end;

procedure TSimpleKeyValuePairStore.Clear;
begin
  List.Clear;
end;

function TSimpleKeyValuePairStore.Contains(const aKey: UnicodeString): Boolean;
var
  Intf: IKeyValuePair;
begin
  Result := Find(aKey, Intf);
end;

procedure TSimpleKeyValuePairStore.Delete(const aKey: UnicodeString);
var
  Index: Integer;
  Intf: IKeyValuePair;
begin
  List.Lock;
  try
    if InternalFind(aKey, Index, Intf) then
      List.Delete(Index);
  finally
    List.Unlock;
  end; {finally}
end;

function TSimpleKeyValuePairStore.Find(const aKey: UnicodeString; out
    aPair: IKeyValuePair): Boolean;
var
  Index: Integer;
begin
  List.Lock;
  try
    Result := InternalFind(aKey, Index, aPair);
  finally
    List.Unlock;
  end; {finally}
end;

function TSimpleKeyValuePairStore.Find(const aKey: UnicodeString; out
    aValue: UnicodeString): Boolean;
var
  Intf: IKeyValuePair;
begin
  Result := Find(aKey, Intf);
  if Result then
    aValue := Intf.Value
  else
    aValue := '';
end;

function TSimpleKeyValuePairStore.GetCaseSensitive: Boolean;
begin
  Result := FCaseSensitive;
end;

function TSimpleKeyValuePairStore.GetEnumerator: IKeyValuePairEnum;
begin
  Result := TKeyValuePairEnum.Create(List) as IKeyValuePairEnum;
end;

function TSimpleKeyValuePairStore.GetValues(aKey: UnicodeString): UnicodeString;
begin
  Find(aKey, Result);
end;

{: Uses a sequential search. If nothing is found Index will return -1
and Intf will return nil.}
function TSimpleKeyValuePairStore.InternalFind(aKey: UnicodeString; var
  Index: Integer; out Intf: IKeyValuePair): Boolean;
var
  I: Integer;
  Temp: ISortKeyValuePair;
begin
  Result := false;
  if aKey <> '' then begin
    if not CaseSensitive then
      aKey := UnicodeUpperCase(aKey);
    for I := 0 to List.Count - 1 do begin
      Temp := List[I] as ISortKeyValuePair;
      Result := UnicodeSameStr(Temp.SortKey, aKey);
      if Result then begin
        Intf := Temp as IKeyValuePair;
        Index := I;
        Break;
      end; {if}
    end; {for}
  end; {if}
  if not Result then begin
    Intf := nil;
    Index := -1;
  end; {if}
end;

procedure TSimpleKeyValuePairStore.SetCaseSensitive(const Value: Boolean);
begin
  if FCaseSensitive <> Value then begin
    CaseSensitiveChanging;
    FCaseSensitive := Value;
  end; {if}
end;

procedure TSimpleKeyValuePairStore.SetValues(aKey: UnicodeString;
  const Value: UnicodeString);
begin
  Add(aKey, Value);
end;

{== TKeyValuePair =====================================================}

constructor TKeyValuePair.Create(const aKey, aValue: UnicodeString;
  CaseSensitive: Boolean);
begin
  inherited Create;
  FKey := aKey;
  FValue := aValue;
  if CaseSensitive then
    FSortkey := FKey
  else
    FSortkey := UnicodeUpperCase(FKey);
end;

function TKeyValuePair.GetKey: UnicodeString;
begin
  Result := FKey;
end;

function TKeyValuePair.GetSortKey: UnicodeString;
begin
  Result := FSortKey;
end;

function TKeyValuePair.GetValue: UnicodeString;
begin
  Result := FValue;
end;

procedure TKeyValuePair.SetValue(const Value: UnicodeString);
begin
  FValue := Value;
end;

{== TKeyValuePairEnum =================================================}

constructor TKeyValuePairEnum.Create(aList: IInterfaceList);
var
  I: Integer;
  Intf: IKeyValuePair;
begin
  Assert(Assigned(aList));
  inherited Create;
  FList:= TInterfaceList.Create;
  aList.Lock;
  try
    FList.Lock;
    try
      for I := 0 to aList.Count - 1 do begin
        Intf:= aList[I] as IKeyValuePair;
        List.Add(TKeyValuePair.Create(Intf.Key, Intf.Value, true) as IInterface);
      end; {for}
    finally
      FList.Unlock;
    end; {finally}
  finally
    aList.Unlock;
  end; {finally}
end;

function TKeyValuePairEnum.Current: IKeyValuePair;
begin
  if IsValidIndex(CurrentIndex) then
    Result := List[CurrentIndex] as IKeyValuePair
  else
    raise EKeyValuePairStoreError.Create(EEnumeratorOutOfBounds);
end;

function TKeyValuePairEnum.GetCount: Integer;
begin
  Result := List.Count;
end;

{== TSortedKeyValuePairStore ==========================================}

procedure TSortedKeyValuePairStore.AddNew(const aKey, aValue:
  UnicodeString; Index: Integer);
begin
  Assert(Index >= 0);
  List.Insert(Index,
    TKeyValuePair.Create(aKey, aValue, CaseSensitive) as IInterface);
end;

{: Binary search algorithm implementation copied from TStringlist.Find. }
function TSortedKeyValuePairStore.InternalFind(aKey: UnicodeString; var
  Index: Integer; out Intf: IKeyValuePair): Boolean;
var
  Temp: ISortKeyValuePair;
  L, H, I, C: Integer;
begin
  Result := false;
  if aKey <> '' then begin
    if not CaseSensitive then
      aKey := UnicodeUpperCase(aKey);
    L := 0;
    H := List.Count - 1;
    while L <= H do begin
      I := (L + H) shr 1;
      Temp := List[I] as ISortKeyValuePair;
      C := UnicodeCompareStr(Temp.SortKey, aKey);
      if C < 0 then
        L := I + 1
      else begin
        H := I - 1;
        if C = 0 then begin
          Result := True;
          Intf := Temp as IKeyValuePair;
          L := I;
        end; {if}
      end; {else}
    end; {while}
    Index := L;
  end {if}
  else
    Index := -1;

  if not Result then
    Intf := nil;
end;

{== THashedKeyValuePairStore ==========================================}

constructor THashedKeyValuePairStore.Create;
begin
  inherited Create;
  FHashTable := CreateHashtable;
end;

procedure THashedKeyValuePairStore.AddNew(const aKey, aValue:
  UnicodeString; Index: Integer);
var
  Intf: IInterface;
begin
  Intf := THashedKeyValuePair.Create(aKey, aValue, CaseSensitive) as IInterface;
  List.Add(Intf);
  FHashTable.Add(Intf as IHashable);
end;

procedure THashedKeyValuePairStore.Clear;
begin
  inherited;
  FHashtable.Clear;
end;

procedure THashedKeyValuePairStore.Delete(const aKey: UnicodeString);
var
  Index: Integer;
  Intf: IKeyValuePair;
begin
  List.Lock;
  try
    if InternalFind(aKey, Index, Intf) then begin
      List.Remove(Intf as IInterface);
      FHashtable.Delete((Intf as IHashable).Key);
    end; {if}
  finally  
    List.Unlock;
  end; {finally}
end;

function THashedKeyValuePairStore.InternalFind(aKey: UnicodeString; var
  Index: Integer; out Intf: IKeyValuePair): Boolean;
var
  Found: IHashable;
  Temp: IHashable;
begin
  Index := -1;
  Temp := THashedKeyValuePair.Create(aKey, '', CaseSensitive) as IHashable;
  Result:= FHashtable.Find(Temp.Key, Found);
  if Result then
    Intf := Found as IKeyValuePair
  else
    Intf := nil;
end;

{== THashedKeyValuePair ===============================================}

{$IFNDEF UNICODE}
constructor THashedKeyValuePair.Create(const aKey, aValue:
  UnicodeString; CaseSensitive: Boolean);
begin
  inherited;
  FHashKey := Utf8Encode(FSortKey);
end;

function THashedKeyValuePair.GetHashkey: string;
begin
  Result := FHashKey;
end;
{$ENDIF}

end.
