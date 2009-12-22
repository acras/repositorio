{== PBNodeStoreU ======================================================}
{! <summary>
 This unit implements a generic node store class for the management
 of a large number of small memory blocks.</summary>
<author>Dr. Peter Below</author>
<history>
<para>Version 1.0 created 2004-03-12</para>
<para>Version 1.0 created 2008-10-05, modified for Delphi 2009</para>
<para>Last modified       2008-10-05</para>
</history>
<remarks>
I first came across the concept of a node store in Julian Bucknalls
EZDSL library and several of his articles in The Delphi Magazine.
The implementation in this unit is different from Julians, though,
since i'm not concerned with backwards compatibility back to Delphi 1.
This unit has been developed with Delphi 7 and no testing has been done
on older Delphi versions. It should be usable with Delphi 5 and up,
though. </remarks>}
{======================================================================}
{$BOOLEVAL OFF}{Unit depends on shortcut boolean evaluation}
unit PBNodeStoreU;

interface

uses Classes, Contnrs, Sysutils;

type
  {: Prototype for an optional cleanup procedure for nodes }
  TCleanupProc = procedure (pNode: Pointer);

  {: This class manages a list of small memory blocks (nodes) in a way
    that reduces memory fragmentation. User code can obtain nodes from
    the store and return them when no longer needed. Freeing the store
    automatically frees all nodes as well. <br/>
    For nodes that contain reference-counted types the creator has to
    provide a cleanup procedure that the store will call for each node,
    both when a node is returned to the store and (for every node) when
    the store itself is destroyed. The cleanup procedure has to properly
    finalize the nodes fields. The store only knows the size of a node,
    not what it contains. }
  TPBNodeStore = class
  private
    FBlocklist: TObjectList;
    FFreeList: Pointer;
    FNodesize: Cardinal;
    FNeedsCleanup: Boolean;
    FCleanupProc: TCleanupProc;
    FBlocksize: Cardinal;

  protected
    procedure PerformCleanup;
    procedure GrowBlocklist;
    function IsValidNodePtr(pNode: Pointer): Boolean;
  public
    {: Create an instance of the node store
     @param aNodesize is the nodes size. The store will at minimum
       allocate sizeof(pointer) bytes for a node, even if aNodesize is
       less.
     @param aCleanupProc is a reference to a cleanup procedure the store
       is to call for each node when it goes out of use. Can be nil to
       indicate that no cleanup is necessary for the nodes, which is the
       default.
     @param aBlockSize is the number of nodes the store should allocate
       in one of its internal blocks. The store sets this to at least 100,
       even if a smaller value is passed. The default is 1000.
     @precondition aNodesize > 0 }
    constructor Create( aNodesize: Cardinal; aCleanupProc: TCleanupProc = nil;
      aBlockSize: Cardinal = 1000 );  virtual;

    {: Destroys the node store and all its nodes, optionally calling the
      cleanup procedure for every node first. }
    destructor Destroy; override;

    {: @Returns the address of a free node.
     @desc If the node has never been used before it will have all
       bytes filled with zero. If it has been used before only the
       first sizeof(pointer) bytes are guaranteed to be 0, the rest
       may contain data from the previous use. }
    function NewNode: Pointer;

    {: Returns the passed node to the stores pool, optionally
      calling the cleanup procedure for it first.
      @param pNode is the address of the node to return to the pool
      @precondition aNode <> nil
      @desc The routine will check if the passed address is a valid
        node address if the PARANOIA symbol is defined and raise
        an ENodestoreError exception if node address is not valid }
    procedure DisposeNode(pNode: Pointer);
  end;

  {: Exception class used for node store errors }
  ENodestoreError = class( Exception );

implementation

type
  TBlockStore = class
  private
    FBlock: array of byte;
    FNodesize: Cardinal;
    FBlocksize: Cardinal;
  protected
    function ContainsNode(pNode: Pointer): Boolean;
  public
    constructor Create(aNodesize, aBlocksize: Cardinal);
    procedure SplitBlock(var aRoot: Pointer);
    procedure PerformCleanup(aCleanupProc: TCleanupProc);
  end;

{== TBlockStore =======================================================}

function TBlockstore.ContainsNode(pNode: Pointer): Boolean;
begin
  Result := (Cardinal(pNode) >= Cardinal(@FBlock[0])) and
    (Cardinal(pNode) < Cardinal(@FBlock[High(FBlock)]));
  if Result then
    Result := ((Cardinal(pNode) - Cardinal(@FBlock[0])) mod FNodesize) = 0;
end;

constructor TBlockStore.Create(aNodesize, aBlocksize: Cardinal);
begin
  inherited Create;
  FNodesize := aNodesize;
  FBlocksize := aBlocksize;
  SetLength( FBlock, aNodesize * aBlocksize );
  FillChar( FBlock[0], Length(FBlock), 0 );
end;

procedure TBlockStore.SplitBlock(var aRoot: Pointer);
var
  i: Integer;
  walker: PPointer;
begin
  walker := @FBlock[0];
  for i:= 1 to FBlocksize - 1 do begin
    {$IFDEF UNICODE}
      walker^ := PByte(walker)+FNodesize;
    {$ELSE}
      walker^ := PChar(walker)+FNodesize;
    {$ENDIF}
    walker := walker^;
  end;
  walker^ := aRoot;
  aRoot := @FBlock[0];
end;

procedure TBlockStore.PerformCleanup(aCleanupProc: TCleanupProc);
var
  i: Integer;
  walker: Pointer;
begin
  walker := @FBlock[0];
  for i:= 1 to FBlocksize do begin
    aCleanupProc( walker );
    {$IFDEF UNICODE}
    PByte(walker) := PByte(walker)+FNodesize;
    {$ELSE}
    walker := PChar(walker)+FNodesize;
    {$ENDIF}
  end;
end;

{== TPBNodeStore ======================================================}

constructor TPBNodeStore.Create(aNodesize: Cardinal; aCleanupProc:
    TCleanupProc; aBlockSize: Cardinal);
begin
  Assert( aNodesize > 0 );
  if aNodesize < sizeof(Pointer) then
    aNodesize := sizeof(Pointer);
  if aBlocksize < 100 then
    aBlocksize := 100;

  inherited Create;
  FBlocklist := TObjectlist.Create(true);
  FNodesize  := aNodesize;
  FCleanupProc:= aCleanupProc;
  FNeedsCleanup:= Assigned( aCleanupProc );
  FBlocksize:= aBlocksize;
  FFreeList := nil;
end;

destructor TPBNodeStore.Destroy;
begin
  PerformCleanup;
  FBlocklist.Free;
  inherited;
end;

procedure TPBNodeStore.PerformCleanup;
var
  i: Integer;
begin
  if FNeedsCleanup then
    for i:= 0 to FBlocklist.Count-1 do
      TBlockstore( FBlocklist[i] ).PerformCleanup(FCleanupProc);
end;

procedure TPBNodeStore.GrowBlocklist;
var
  Block: TBlockstore;
begin
  Block := TBlockStore.Create(FNodesize, FBlocksize);
  try
    Block.SplitBlock(FFreeList);
    FBlocklist.Add(Block);
  except
    Block.Free;
    raise
  end;
end;

function TPBNodeStore.NewNode: Pointer;
begin
  if not Assigned(FFreeList) then
    GrowBlocklist;
  Assert( Assigned( FFreeList ));
  Result := FFreelist;
  FFreelist := PPointer(Result)^;
  PPointer(Result)^ := nil;
end;

procedure TPBNodeStore.DisposeNode(pNode: Pointer);
begin
  Assert( Assigned( pNode ));
  {$IFDEF PARANOIA}
  if not IsValidNodePtr( pNode ) then
    raise ENodestoreError.Create('Invalid node passed to DisposeNode');
  {$ENDIF}
  if FNeedsCleanup then
    FCleanupProc(pNode);
  PPointer(pNode)^ := FFreelist;
  FFreelist := pNode;
end;

function TPBNodeStore.IsValidNodePtr(pNode: Pointer): Boolean;
var
  i: Integer;
begin
  Result := false;
  for i:= 0 to FBlocklist.Count-1 do
    if TBlockStore( FBlocklist[i] ).ContainsNode(pNode) then begin
      Result := true;
      break;
    end;
end;

end.
