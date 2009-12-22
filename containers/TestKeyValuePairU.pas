unit TestKeyValuePairU;
{

  Delphi DUnit Testfall
  ----------------------
  Diese Unit enthält ein Codegerüst einer Testfallklasse, das vom Testfall-Experten
  erzeugt wurde. Ändern Sie den erzeugten Code, damit die Methoden aus der
  getesteten Unit korrekt eingerichtet und aufgerufen werden.

}

interface

uses
  TestFramework, Classes, Sysutils, CommonTypesU, KeyValuePairU, EnumeratorIntfU;

implementation

uses UnicodeHelpersU;

type
  TTestEntry = record key, value: unicodestring end;
  TTestData = array of TTestEntry;
  TestSimpleStore = class(TTestCase)
  protected
    FStore: IKeyValuePairStore;
  private
    FTestData: TTestData;
  protected
    function FindKeyInTestdata(const aKey: UnicodeString): Integer;
    function Kind: TKeyValuePairStoreKind; virtual;
  public
    procedure AddTestValues(NumItems:Cardinal);
    procedure CreateTestData(NumItems:Cardinal);
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAdd;
    procedure TestAddReplace;
    procedure TestCaseInsensitive;
    procedure TestClear;
    procedure TestCreation;
    procedure TestDelete;
    procedure testEnumerator;
    procedure TestValues;
  end;
  TestSortedStore = class(TestSimpleStore)
  protected
    function Kind: TKeyValuePairStoreKind; override;
  published
    procedure TestIsSorted;
  end;

  TestHashedStore = class(TestSimpleStore)
  protected
    function Kind: TKeyValuePairStoreKind; override;
  end;

{== TestSimpleStore ===================================================}
  
procedure TestSimpleStore.AddTestValues(NumItems:Cardinal);
var
  I: Integer;
begin
  if NumItems >= 10000 then
    NumItems := 9999;
  CreateTestData(NumItems);
  for I := High(FTestData) downto 0 do
    FStore.Add(FTestData[I].Key, FTestData[I].value);
   {The testdata array is sorted, adding the entries back wards makes
    sure we do not add them in sort order, so can test sorting. }
end;

procedure TestSimpleStore.CreateTestData(NumItems:Cardinal);
var
  I: Integer;
begin
  Assert(NumItems > 0);
  SetLength(FTestData, NumItems);
  for I := 0 to NumItems - 1 do begin
    FTestData[I].Key := Format('Key%4.4d', [I]);
    FTestData[I].value := Format('Value %d', [I]);
  end; {for}
end;

function TestSimpleStore.FindKeyInTestdata(const aKey: UnicodeString):
  Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to High(FTestData) do
    if UnicodeSameStr(aKey, FTestData[I].Key) then begin
      Result := I;
      Break;
    end;
end;

function TestSimpleStore.Kind: TKeyValuePairStoreKind;
begin
  Result := kvkSimple;
end;

procedure TestSimpleStore.SetUp;
begin
  FStore := CreateKeyValuePairStore(Kind);
end;

procedure TestSimpleStore.TearDown;
begin
  FStore := nil;
  SetLength(FTestData, 0);
end;

procedure TestSimpleStore.TestAdd;
var
  Temp: UnicodeString;
  I: Integer;
begin
  AddTestValues(10);
  for I := 0 to High(FTestData) do begin
    CheckTrue(FStore.Contains(FTestData[I].Key));
    CheckTrue(FStore.Find(FTestData[I].Key, Temp));
    CheckEquals(FTestData[I].Value, Temp);
  end; {for}
end;

procedure TestSimpleStore.TestAddReplace;
var
  NewValue, Temp: UnicodeString;
  I: Integer;
begin
  AddTestValues(10);
  for I := 0 to High(FTestData) do begin
    NewValue := FTestData[I].Value + 'Foo';
    FStore.Add(FTestData[I].Key, NewValue);
    CheckTrue(FStore.Contains(FTestData[I].Key));
    CheckTrue(FStore.Find(FTestData[I].Key, Temp));
    CheckEquals(NewValue, Temp);
  end; {for}
end;

procedure TestSimpleStore.TestCaseInsensitive;
var
  NewKey, Temp: UnicodeString;
  I: Integer;
begin
  FStore.CaseSensitive := false;
  AddTestValues(25);
  for I := 0 to High(FTestData) do begin
    NewKey := UnicodeUppercase(FTestData[I].Key);
    CheckTrue(FStore.Contains(NewKey));
    CheckTrue(FStore.Find(NewKey, Temp));
    CheckEquals(FTestData[I].Value, Temp);
  end; {for}
end;

procedure TestSimpleStore.TestClear;
var
  I: Integer;
begin
  AddTestValues(28);
  FStore.Clear;
  for I := 0 to High(FTestData) do begin
    CheckFalse(FStore.Contains(FTestData[I].Key));
  end; {for}
end;

procedure TestSimpleStore.TestCreation;
begin
  CheckNotNull(FStore);
end;

procedure TestSimpleStore.TestDelete;
var
  I: Integer;
  N: Integer;
begin
  AddTestValues(8);
  for I := 0 to High(FTestData) do begin
    FStore.Delete(FTestData[I].Key);
    CheckFalse(FStore.Contains(FTestData[I].Key));
    for N := I+1 to High(FTestData) do
      CheckTrue(FStore.Contains(FTestData[N].Key));
  end; {for}
end;

procedure TestSimpleStore.testEnumerator;
var
  Enum: IKeyValuePairEnum;
  Pair: IKeyValuePair;
  N: Integer;
begin
  AddTestValues(18);
  Enum := FStore.Enumerator;
  CheckNotNull(Enum, 'Enumerator not created');
  while Enum.MoveNext do begin
    Pair:= Enum.Current;
    N := FindKeyInTestdata(Pair.Key);
    CheckTrue(N >= 0);
    CheckEquals(FTestData[N].Value, Pair.Value);
  end; {while}
end;

procedure TestSimpleStore.TestValues;
var
  I: Integer;
  Temp: UnicodeString;
  NewValue: UnicodeString;
begin
  AddTestValues(22);
  for I := 0 to High(FTestData) do begin
    Temp := FStore.Values[FTestData[I].Key];
    CheckEquals(FTestData[I].value, Temp);
    NewValue := Temp + 'Foo';
    FStore.Values[FTestData[I].Key] := NewValue;
    Temp := FStore.Values[FTestData[I].Key];
    CheckEquals(NewValue, Temp);
  end;
end;

{== TestSortedStore ===================================================}

function TestSortedStore.Kind: TKeyValuePairStoreKind;
begin
  Result := kvkSorted;
end;

procedure TestSortedStore.TestIsSorted;
var
  Enum: IKeyValuePairEnum;
  Last, Pair: ISortKeyValuePair;
begin
  AddTestValues(18);
  Enum := FStore.Enumerator;
  CheckNotNull(Enum, 'Enumerator not created');
  Last := nil;
  while Enum.MoveNext do begin
    Pair:= Enum.Current as ISortKeyValuePair;
    if Assigned(Last) then
      CheckTrue(UnicodeCompareStr(Last.Key, Pair.Key) < 0, 'Not sorted)');
    Last := Pair;
  end; {while}
end;

function TestHashedStore.Kind: TKeyValuePairStoreKind;
begin
  Result := kvkHashed;
end;

initialization
  Randomize;
  // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TestSimpleStore.Suite);
  RegisterTest(TestSortedStore.Suite);
  RegisterTest(TestHashedStore.Suite);
end.

