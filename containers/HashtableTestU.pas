{Note: Define the symbol VERBOSE to get some statistics displayed for the
 hash tables internal structure. }
unit HashtableTestU;
{ $DEFINE VERBOSE}

interface

implementation

uses
  Classes, Sysutils, TestFramework,
  PBHashSupportU, PBInterfaceHashtableU, Dialogs;

type
  TItem = class( TInterfacedObject, IHashable )
  private
    PCounter: ^Integer;
    FKey: string;
  public
    constructor Create( const aKey: string; var Counter: Integer );
    destructor Destroy; override;
    function GetKey: String;
  end;

  {$M+}
  THashtableTest = class(TTestcase)
  private
    FIDs: TStringlist;
    FObjectCount: Integer;
    procedure AddIDs(Intf: IInterfaceHashTable);
    function CreateItem(Index: Integer): IHashable;
    procedure CreateRandomIDs(NumIDs: Integer);
    procedure ReportHashtableProperties(const Routine: string; Info: 
        IHashtableInfo);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure testAdd; virtual;
    procedure testAddDuplicates; virtual;
    procedure testClear; virtual;
    procedure testCreation; virtual;
    procedure testFind; virtual;
    procedure testGrow; virtual;
    procedure testExtract; virtual;
    procedure testDelete; virtual;
  end;

procedure THashtableTest.AddIDs( Intf: IInterfaceHashTable);
var
  i: Integer;
begin
  for i:= 0 to FIDs.Count-1 do
    intf.Add( CreateItem(i), false);
end;

function THashtableTest.CreateItem(Index: Integer): IHashable;
begin
  Result := TItem.Create( FIDs[Index], FObjectCount ) as IHashable;
end;

procedure THashtableTest.CreateRandomIDs(NumIDs: Integer);
var
  ID: String;
begin
  FIDs.Clear;
  FIDs.Sorted := true;
  FIDs.Duplicates := dupIgnore;
  while FIDs.Count < NumIDs do begin
    ID := Format('HAG%8.8d',[Random(99999999)]);
    FIDs.Add(ID);
  end; { while }
end;

procedure THashtableTest.ReportHashtableProperties(const Routine: string;
    Info: IHashtableInfo);
var
  SL: TStringlist;
begin
  SL:= TStringList.Create;
  try
    SL.Add(Routine+' -------' );
    CheckNotNull(Info,'Info is nil');
    CheckEquals( FIDs.Count, Info.ItemCount, 'Item count');
    SL.Add(Format('Slot count: %d',[Info.SlotCount]));
    SL.Add(Format('Fill factor: %5.3f',[Info.FillFactor]));
    SL.Add(Format('Collisions: %d',[Info.CollisionCount]));
    SL.Add(Format('Longest chain: %d',[Info.LongestChain]));
    SL.Add(Format('Used slots: %d',[Info.UsedSlots]));
    {$IFDEF VERBOSE}
    ShowMessage(SL.Text);
    {$ENDIF}
  finally
    SL.Free;
  end; {finally}
end;

procedure THashtableTest.SetUp;
begin
  inherited;
  FIDs:= TStringlist.Create;
  FObjectCount := 0;
end;

procedure THashtableTest.TearDown;
begin
  inherited;
  FIDs.Free;
  FObjectCount := 0;
end;

procedure THashtableTest.testAdd;
var
  intf: IInterfaceHashTable;
  Info: IHashtableInfo;
begin
  intf := CreateHashtable(10000);
  CreateRandomIDs(5000);
  AddIDs(Intf);
  Info := Intf As IHashTableInfo;
  ReportHashtableProperties('testAdd', Info);
  Info := nil;
  intf := nil;
  CheckEquals(0, FObjectCount, 'Some helper objects not freed');
end;

procedure THashtableTest.testAddDuplicates;
var
  intf: IInterfaceHashTable;
  item: IHashable;
begin
  intf := CreateHashtable(1000);
  CreateRandomIDs(50);
  AddIDs(Intf);

  // this should succeed
  item := CreateItem(1);
  intf.Add(Item, true );

  // the following should fail
  try
    item := CreateItem(2);
    intf.Add(Item, false );
    Fail('Addition of duplicate succeeded where it should not');
  except
    on EHashtableError do;
  end; { try except }
  item := nil;
  intf := nil;

  // check whether the intermediate items have been disposed of properly
  CheckEquals(0, FObjectCount, 'Some helper objects not freed');
end;

procedure THashtableTest.testClear;
var
  intf: IInterfaceHashTable;
  Info: IHashTableInfo;
  i: Integer;
  Item: IHashable;
begin
  intf := CreateHashtable(3000);
  CreateRandomIDs(1000);
  AddIDs(Intf);
  Info := intf as IHashTableInfo;
  CheckEquals(FIDs.Count, Info.ItemCount);
  intf.Clear;
  CheckEquals(0, Info.ItemCount);
  for i:= 0 to FIDs.Count-1 do
    assert( not intf.Find(FIDs[i], Item));
  info:= nil;
  intf:= nil;
  CheckEquals(0, FObjectCount, 'Some helper objects not freed');
end;

procedure THashtableTest.testCreation;
var
  intf: IInterfaceHashTable;
begin
  intf := CreateHashtable(0);
  Assert( Assigned( intf ), 'Creation failed');
end;

procedure THashtableTest.testFind;
var
  intf: IInterfaceHashTable;
  i: Integer;
  Item: IHashable;
begin
  intf := CreateHashtable(10000);
  CreateRandomIDs(5000);
  AddIDs(Intf);
  for i:= 0 to FIDs.Count-1 do begin
    Assert( Intf.Find(FIDs[i], Item), 'Item '+FIDs[i]+' not found');
    CheckEquals(FIDs[i], Item.Key, 'Key mismatch');
  end;

  Assert( not Intf.Find('MFCD12345678', Item),
    'Found item that is not in the list');
  CheckNull( Item );
end;

procedure THashtableTest.testGrow;
var
  intf: IInterfaceHashTable;
  Info: IHashtableInfo;
begin
  intf := CreateHashtable(2000);
  CreateRandomIDs(5000);
  AddIDs(Intf);
  Info := Intf As IHashTableInfo;
  ReportHashtableProperties('testGrow', Info);
  Info := nil;
  intf := nil;
  CheckEquals(0, FObjectCount, 'Some helper objects not freed');
end;

procedure THashtableTest.testExtract;
var
  intf: IInterfaceHashTable;
  Info: IHashtableInfo;
  item: IHashable;
  i: Integer;
begin
  intf := CreateHashtable(2000);
  CreateRandomIDs(500);
  AddIDs(Intf);
  for i:= 300 to 499 do begin
    Assert(intf.Extract(FIDs[i], item), 'Item to extract not found: '+FIDs[i]);
    CheckNotNull(item, 'Extract failed');
    CheckEquals( FIDs[i], item.Key, 'Extracted wrong item');
  end;

  Assert(not intf.Extract(FIDs[350], item), 'Extracting nonexisting item');
  CheckNull(item);

  for i:= 300 to 499 do
    Assert( not intf.Find(FIDs[i], item));
  for i:= 0 to 299 do
    Assert( intf.Find(FIDs[i], item));

  Info := Intf As IHashTableInfo;
  CheckEquals(300, info.ItemCount);
  Info := nil;
  intf := nil;
  item := nil;
  CheckEquals(0, FObjectCount, 'Some helper objects not freed');
end;

procedure THashtableTest.testDelete;
var
  intf: IInterfaceHashTable;
  Info: IHashtableInfo;
  item: IHashable;
  i: Integer;
begin
  intf := CreateHashtable(2000);
  CreateRandomIDs(500);
  AddIDs(Intf);
  for i:= 300 to 499 do
    intf.Delete(FIDs[i], true);

  // this should fail
  try
    intf.Delete(FIDs[300], false);
    Fail('Deleting item already deleted should not be allowed');
  except
    on EHashtableError do;
  end; { try except }

  for i:= 300 to 499 do
    Assert( not intf.Find(FIDs[i], item));
  for i:= 0 to 299 do
    Assert( intf.Find(FIDs[i], item));

  Info := Intf As IHashTableInfo;
  CheckEquals(300, info.ItemCount);
  Info := nil;
  intf := nil;
  item := nil;
  CheckEquals(0, FObjectCount, 'Some helper objects not freed');
end;


{ TItem }

constructor TItem.Create(const aKey: string; var Counter: Integer);
begin
  inherited Create;
  Fkey := aKey;
  Inc(Counter);
  PCounter := @Counter;
end;

destructor TItem.Destroy;
begin
  Dec( PCounter^ );
  inherited;
end;

function TItem.GetKey: String;
begin
  Result := FKey;
end;

initialization
  Randomize;
  RegisterTest( THashtableTest.Suite);
end.
