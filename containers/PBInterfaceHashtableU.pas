{== PBInterfaceHashtableU =============================================}
{: This unit implements a hash table class for storing objects that
  implement the IHashable interface.
@author Dr. Peter Below
@desc   Version 1.0 created 2004-03-19<BR>
        Last modified       2006-05-10, removed the limit of 64K slots.}
{======================================================================}
{$BOOLEVAL OFF}{Unit depends on shortcut boolean evaluation}
unit PBInterfaceHashtableU;

interface

uses
  PBHashSupportU;

{$IFDEF TESTING}
type
  IHashTableInfo = interface
  ['{08C9B731-E39D-438A-A89A-F4C3AF364100}']
    function GetFillFactor: Double;
    function GetCollisionCount: Cardinal;
    function GetItemCount: Cardinal;
    function GetSlotCount: Cardinal;
    function GetLongestChain: Integer;
    function GetUsedSlots: Cardinal;
    property FillFactor: Double read GetFillFactor;
    property CollisionCount: Cardinal read GetCollisionCount;
    property ItemCount: Cardinal read GetItemCount;
    property SlotCount: Cardinal read GetSlotCount;
    property LongestChain: Integer read GetLongestChain;
    property UsedSlots: Cardinal read GetUsedSlots;
  end;
{$ENDIF}

{: Create an instance of the hash table and return its interface.
@param InitialSize specifies the initial number of slots in the
  hash table. If a value < 1000 is specified 1000 is used instead.
  In fact the hash table will use the closest prime number to the
  passed value. The hash table will grow automatically when it is
  about 2/3 full, but that is a slow process and should be avoided,
  if possible.
@param aHashfunction specifies the hash function to use. By default
  this will be the ElfHash function from PBHashSupportU.
@returns the hash tables interface.
}
function CreateHashtable( InitialSize: Cardinal = 1000;
  aHashfunction: THashFunction = nil ): IInterfaceHashTable;

implementation

uses
  Classes, Sysutils, Math, PBNodeStoreU, PrimesU, Windows;

resourcestring
{$IFDEF GERMAN}
  eDuplicateItem =
  'Es befindet sich bereits ein Eintrag mit dem Schlüssel "%s" in der '+
  'Hash-Tabelle.';
  eHashtableFull =
  'Die Hash-Tabelle hat die maximale Größe erreicht.';
  eItemNotFoundOnDelete =
  'Der Eintrag mit dem Schlüssel "%s" konnte nicht aus der Hash-Tabelle '+
  'gelöscht werden, da er nicht gefunden wurde.';
{$ELSE}
  eDuplicateItem =
  'There is already an item with the key "%s" in the hash table.';
  eHashtableFull =
  'The hash table has reached the maximum size.';
  eItemNotFoundOnDelete =
  'The item with key "%s" could not be deleted from the hash table '+
  'since it was not found in the table.';
{$ENDIF}


type
  PIntfListNode = ^TIntfListNode;
  TIntfListNode = record
    Next: PIntfListNode;
    Intf: IHashable;
  end;
  PSlot = ^PIntfListNode;
  TSlots = array of PIntfListNode;

  TPBInterfaceHashtable = class(TInterfacedObject, IInterfaceHashTable
    {$IFDEF TESTING} , IHashtableInfo {$ENDIF})
  private
    FCanGrow: Boolean;
    FItemCount: Cardinal;
    FNodeStore: TPBNodeStore;
    FSlotCount: Cardinal;
    FSlots: TSlots;
    FHash: THashFunction;
  protected
    function AboveLimit: Boolean;
    procedure Add(aItem: IHashable; IgnoreDuplicates: Boolean = True);
    procedure Clear;
    procedure Delete(const aKey: String; IgnoreNotFound: Boolean = True);
    function Extract(const aKey: String; var aItem: IHashable): Boolean;
    function Find(const aKey: String; var aItem: IHashable): Boolean;
    function GetCollisionCount: Cardinal;
    function GetFillFactor: Double;
    function GetItemCount: Cardinal;
    function GetLongestChain: Integer;
    function GetSlotCount: Cardinal;
    function GetUsedSlots: Cardinal;
    procedure Grow;
    function InternalFind(const Key: string; var Slot: pSlot): Boolean;
    property NodeStore: TPBNodeStore read FNodeStore;
    property Slots: TSlots read FSlots;
    property Hash: THashFunction read FHash;
  public
    constructor Create(InitialSize: Cardinal; aHashFunction: THashFunction);
    destructor Destroy; override;
    property ItemCount: Cardinal read FItemCount;
    property SlotCount: Cardinal read FSlotCount;
  end;


function CreateHashtable(InitialSize: Cardinal = 1000; aHashfunction:
  THashFunction = nil): IInterfaceHashTable;
begin
  Result := TPBInterfaceHashtable.Create(InitialSize, aHashfunction );
end;

procedure FinalizeNode( pNode: Pointer );
begin
  PIntfListNode( pNode )^.Intf := nil;
end;


{== TPBInterfaceHashtable =============================================}

function TPBInterfaceHashtable.AboveLimit: Boolean;
begin
  Result := FCanGrow and (FItemCount/FSlotCount > 0.6);
end;

procedure TPBInterfaceHashtable.Add(aItem: IHashable;
  IgnoreDuplicates: Boolean);
var
  found: Boolean;
  pInsert: PSlot;
  pItem: PIntfListNode;
begin
  Assert( Assigned( aItem ));
  found := InternalFind( aItem.Key, pInsert );
  if found then
    if not IgnoreDuplicates then
      raise EHashtableError.CreateFmt( eDuplicateItem, [aItem.Key])
    else
      // do nothing
  else begin
    pItem := Nodestore.NewNode;
    pItem^.Intf := aItem;
    pItem^.Next := pInsert^;
    pInsert^ := pItem;
    Inc(FItemCount);
    if AboveLimit then
      Grow;
  end; { else }
end;

procedure TPBInterfaceHashtable.Clear;

  procedure FreeChain(var pNode: PIntfListNode);
  var
    walker, temp: PIntfListNode;
  begin
    walker := pNode;
    if Assigned( walker ) then begin
      pNode := nil;
      while walker <> nil do begin
        temp :=  walker^.next;
        Nodestore.DisposeNode(walker);
        walker := temp;
      end; { while }
    end; { if }  
  end;

var
  i: Integer;
begin
  for i:= 0 to High(FSlots) do
    FreeChain( FSlots[i] );
  FItemCount := 0;
end;

constructor TPBInterfaceHashtable.Create(InitialSize: Cardinal;
  aHashFunction: THashFunction);
begin
  inherited Create;
  if InitialSize < 1000 then
    InitialSize := 1000;
  if not Assigned( aHashFunction ) then
    aHashFunction := ElfHash;
  FHash := aHashFunction;    
  FSlotCount := GetNearestPrime( InitialSize );
  SetLength(FSlots, FSlotCount);
  FillChar( FSlots[0], FSlotCount * Sizeof(FSlots[0]), 0 );
  FNodeStore := TPBNodeStore.Create( Sizeof(TIntfListnode), FinalizeNode );
  FItemCount := 0;
  FCanGrow := true;
end;

procedure TPBInterfaceHashtable.Delete(const aKey: String;
  IgnoreNotFound: Boolean);
var
  Item: IHashable;
begin
  if not Extract(aKey, Item) then
    if not IgnoreNotFound then
      raise EHashtableError.CreateFmt( eItemNotFoundOnDelete, [aKey] );
end;

destructor TPBInterfaceHashtable.Destroy;
begin
  FNodeStore.Free;
  inherited;
end;

function TPBInterfaceHashtable.Extract(const aKey: String;
  var aItem: IHashable): Boolean;
var
  Slot: PSlot;
  pNode: PIntfListNode;
begin
  Result := InternalFind(aKey, Slot );
  if Result then begin
    pNode := Slot^;
    Slot^ := pNode^.Next;
    aItem := pNode^.Intf;
    Nodestore.DisposeNode(pNode);
    Dec(FItemCount);  
  end { if }
  else
    aItem := nil;
end;

function TPBInterfaceHashtable.Find(const aKey: String;
  var aItem: IHashable): Boolean;
var
  Slot: PSlot;
begin
  Result := InternalFind(aKey, Slot);
  if Result then
    aItem := Slot^^.Intf
  else
    aItem := nil;  
end;

function TPBInterfaceHashtable.GetCollisionCount: Cardinal;

  function CountEntries(Root: PIntfListNode): Cardinal;
  var
    walker: PIntfListNode;
  begin
    // Note: we do not count the first node in the chain!
    Result := 0;
    walker := Root^.Next;
    while Assigned( walker ) do begin
      Inc(Result);
      walker := walker^.Next;
    end; { while }
  end;

var
  i: Integer;
begin
  Result := 0;
  for i:= 0 to High(Slots) do
    if Assigned(Slots[i]) then
      Inc(Result, CountEntries(Slots[i]));
end;

function TPBInterfaceHashtable.GetFillFactor: Double;
begin
  Result := ItemCount / SlotCount;
end;

function TPBInterfaceHashtable.GetItemCount: Cardinal;
begin
  Result := FItemCount;
end;

function TPBInterfaceHashtable.GetLongestChain: Integer;

  function ChainLength(walker: PIntfListNode): Integer;
  begin
    Result := 0;
    while Assigned( walker ) do begin
      Inc(Result);
      walker := walker^.Next;
    end;
  end;

var
  i: Integer;
begin
  Result := 0;
  for i:= 0 to High(Slots) do
    Result := Math.Max(Result, ChainLength(Slots[i]));
end;

function TPBInterfaceHashtable.GetSlotCount: Cardinal;
begin
  Result := FSlotCount;
end;

function TPBInterfaceHashtable.GetUsedSlots: Cardinal;
var
  i: Integer;
begin
  Result := 0;
  for i:= 0 to High(Slots) do
    if Assigned(Slots[i]) then
      Inc(Result);
end;

procedure TPBInterfaceHashtable.Grow;

  procedure MoveChain(pNode: PIntfListNode);
  var
    Walker: PIntfListNode;
  begin
    while Assigned(pNode) do begin
      Add( pNode^.Intf );
      walker := pNode^.Next;
      NodeStore.DisposeNode(pNode);
      pNode := walker;
    end; { while }
  end;

var
  OldSlots: TSlots;
  NewSlotcount: Cardinal;
  i: Integer;
begin
  if FSlotcount > (High(Cardinal) div 2) then
    NewSlotCount := GetNearestPrime(High(Cardinal))
  else
    NewSlotCount := GetNearestPrime(FSlotcount * 2);
  if NewSlotCount = FSlotCount then begin
    FCanGrow := false;
    raise EHashtableError.Create( eHashtableFull );
  end; { if }

  OldSlots := Copy(FSlots, 0, Length(FSlots));
  SetLength(FSlots, NewSlotCount);
  FSlotCount := NewSlotcount;
  FillChar( FSlots[0], FSlotCount * Sizeof(FSlots[0]), 0 );
  FItemCount := 0;

  for i:= 0 to High(OldSlots) do
    MoveChain( OldSlots[i] );
end;

{: Searches the hash table for a key
@param Key is the key to look for
@param Slot returns the address of the pointer variable holding the
  address of the found node, if the Key was found, else the address
  of the pointer variable where the new nodes address should be
  stored in case of an add operation.
@returns true if the key was found, false if not.  }
function TPBInterfaceHashtable.InternalFind(const Key: string; var
    Slot: pSlot): Boolean;
var
  Index: Cardinal;
  walker: PIntfListNode;
begin
  Index := Hash(Key) mod FSlotCount;
  Assert( Index < FSlotCount );
  Result := false;
  Slot := @FSlots[Index];
  walker := Slot^;
  while Assigned( walker ) do begin
    if AnsiSameStr(Key, walker^.Intf.Key) then begin
      Result := true;
      Break;
    end
    else begin
      Slot := @walker^.next;
      walker := Slot^;
    end;
  end;
end;

end.
