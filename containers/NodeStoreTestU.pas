unit NodeStoreTestU;

interface

implementation

uses TestFramework, PBNodeStoreU, SysUtils;

type
  {$M+}
  TNodestoreTests = class( TTestcase )
  published
    procedure testNewAndDispose; virtual;
    procedure testCleanupProc; virtual;
  end;

procedure TNodestoreTests.testNewAndDispose;
type
  TTestnode = record
    anInteger: Integer;
    aDouble: Double;
    anArray: array [0..100] of Char;
  end;
  PTestnode = ^TTestnode;
const
  Testnode: TTestnode = (
    anInteger: 10;
    aDouble  : Pi;
    anArray  : 'This is a test string'#0
    );
var
  Dummy: TTestNode;
  Nodes: array [1..200] of PTestnode;
  pNode: PTestnode;
  i: Integer;
  Store: TPBNodestore;
begin
  Fillchar(Dummy, Sizeof(Dummy), 0);
  Store := TPBNodestore.Create( sizeof(TTestnode), nil, 100);
  try
    for i:= Low(Nodes) to High(Nodes) do begin
      pNode := Store.NewNode;
      Assert( Assigned( pNode ), 'NewNode failed');
      Assert( CompareMem(pNode, @Dummy, sizeof(Dummy)), 'Block not nulled out');
      pNode^:= Testnode;
      Nodes[i] := pNode;
    end; { for }
    for i:= High(Nodes) downto Low(Nodes) do begin
      Store.DisposeNode(Nodes[i]);
      CheckNotEquals(Testnode.anInteger, Nodes[i]^.anInteger ,
        'DisposeNode failed');
    end; { for }
    for i:= Low(Nodes) to High(Nodes) do begin
      pNode := Store.NewNode;
      Assert( Assigned( pNode ), 'NewNode failed');
      CheckEquals( 0, pNode^.anInteger, 'Blocks first dword not nulled out');
      Nodes[i] := pNode;
    end; { for }
  finally
    Store.Free;
  end; { try finally }
end;


type
  TTestnode = record
    anInteger: Integer;
    aDouble: Double;
    aString: string;
  end;
  PTestnode = ^TTestnode;

procedure CleanupTestnode( pNode: Pointer );
begin
  Finalize( PTestnode(pNode)^ );
end;

procedure TNodestoreTests.testCleanupProc;
const
  Testnode: TTestnode = (
    anInteger: 10;
    aDouble  : Pi;
    aString  : 'This is a test string'
    );
var
  Nodes: array [1..200] of PTestnode;
  pNode: PTestnode;
  i: Integer;
  Store: TPBNodestore;
begin
  Store := TPBNodestore.Create( sizeof(TTestnode), CleanupTestnode, 100);
  try
    for i:= Low(Nodes) to High(Nodes) do begin
      pNode := Store.NewNode;
      pNode^:= Testnode;
      Nodes[i] := pNode;
    end; { for }
    for i:= High(Nodes) downto Low(Nodes) do begin
      Store.DisposeNode(Nodes[i]);
      Assert( Nodes[i]^.aString = '', 'Cleanup failed' );
    end; { for }
  finally
    Store.Free;
  end; { try finally }
end;

initialization
  RegisterTest(TNodestoreTests.Suite);
end.
