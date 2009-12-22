{== KeyValueStoreTestU ================================================}
{: DUnit test case for the IKeyValueStore implementation offered by
  the PBKeyValueStoreU unit.
@author Dr. Peter Below
@desc   Version 1.0 created 2007-08-15<br/>
        Last modified       2007-08-15  }
{======================================================================}
{$BOOLEVAL OFF}{Unit depends on shortcut boolean evaluation}
unit KeyValueStoreTestU;

interface

uses
  Windows, TestFramework, Classes, Sysutils, PBKeyValueStoreU;
type
  {$M+}
  TestIKeyValueStore = class(TTestCase)
  protected
    FStore: IKeyValueStore;
    FCaseSensitive : Boolean;
    function GetCaseSensitive: Boolean; virtual;
  private
    function FindKeyInTestdata(const aKey: string): Integer;
    procedure LoadTestData(First: Integer = 0; Last: Integer = 0);
    procedure VerifyTestData(First: Integer = 0; Last: Integer = 0);
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestAdd;
    procedure TestAddMultiple;
    procedure TestAssignTo;
    procedure TestClear;
    procedure TestContains;
    procedure TestDelete;
    procedure TestFind;
    procedure TestGetEnumerator;
    procedure TestGetStreamer;
    procedure TestLoadFromStrings;
    procedure TestSaveAndLoadToFile;
    procedure TestSaveAndLoadToStream;
    procedure TestSetDuplicateHandling;
  end;

  TestIKeyValueStoreCaseInsenstive = class(TestIKeyValueStore)
  protected
    function GetCaseSensitive: Boolean; override;
  end;

implementation

uses WinUtils;

type
  TKeyValue = record Key, Value, UpperKey: string end;
var
  TestData : array [1..256] of TKeyValue;

procedure InitTestData;
var
  I: Integer;
begin
  for I := Low(TestData) to High(TestData) do begin
    TestData[I].Key := Format('Key%3.3d',[I]);
    TestData[I].UpperKey := Format('KEY%3.3d',[I]);
    TestData[I].Value := Format('This is value %3.3d',[I]);
  end;
end;

function TestIKeyValueStore.FindKeyInTestdata(const aKey: string):
  Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := Low(TestData) to High(TestData) do
    if TestData[I].Key = aKey then begin
      Result := I;
      Break;
    end; {if}
end;

function TestIKeyValueStore.GetCaseSensitive: Boolean;
begin
  Result := true;
end;

procedure TestIKeyValueStore.LoadTestData(First: Integer = 0; Last:
  Integer = 0);
var
  I: Integer;
begin
  if First = 0 then
    First := Low(TestData);
  if Last = 0 then
    Last := High(TestData);
  for I := First to Last do
    FStore.Add(TestData[I].Key, TestData[I].Value, kvDupError);
end;

procedure TestIKeyValueStore.SetUp;
begin
  FCaseSensitive := GetCaseSensitive;
  FStore := CreateKeyValueStore(1000, FCaseSensitive);
end;

procedure TestIKeyValueStore.TearDown;
begin
  FStore := nil;
end;

procedure TestIKeyValueStore.TestAdd;
const
  SKey = 'Foo';
  SValue1 = 'Value1';
  SValue2 = 'Value2';
var
  aValue: string;
begin
  // check simple addition of first item
  FStore.Add(SKey, SValue1);
  Check(FStore.Find(SKey, aValue), 'Find failed');
  CheckEquals(SValue1, aValue);

  // check case-insensitive find
  if not FCaseSensitive then
    CheckTrue(FStore.Find(AnsiUpperCase(SKey), aValue))
  else
    CheckFalse(FStore.Find(AnsiUpperCase(SKey), aValue));

  // check duplicates handling
  FStore.Add(SKey, SValue1, kvDupIgnore);  // expect no exception
  Check(FStore.Find(SKey, aValue), 'Find failed');
  CheckEquals(SValue1, aValue);

  FStore.Add(SKey, sValue2, kvDupReplace);
  Check(FStore.Find(SKey, aValue), 'Find failed');
  CheckEquals(SValue2, aValue);

  FStore.DuplicateHandling := kvDupReplace;
  FStore.Add(SKey, sValue1, kvDupDefault);
  Check(FStore.Find(SKey, aValue), 'Find failed');
  CheckEquals(SValue1, aValue);

  ExpectedException := EKVDuplicateError;
  FStore.Add(SKey, sValue1, kvDupError);
  Check(false, 'Should not end up here, EKVDuplicateError not raised');
end;

procedure TestIKeyValueStore.TestAddMultiple;
var
  aValue: string;
begin
  LoadTestData;
  VerifyTestData;
  CheckFalse(FStore.Find('Foo', aValue));
end;

procedure TestIKeyValueStore.TestAssignTo;
var
  Strings: TStrings;
  I: Integer;
begin
  Strings:= TStringlist.Create;
  try
    LoadTestData;
    FStore.AssignTo(Strings);
    for I := Low(TestData) to High(TestData) do
      CheckEquals(TestData[I].Value, Strings.Values[TestData[I].Key]);
  finally
    Strings.Free;
  end; {finally}
end;

procedure TestIKeyValueStore.TestClear;
var
  I: Integer;
begin
  LoadTestData;
  FStore.Clear;
  for I := Low(TestData) to High(TestData) do
    CheckFalse(FStore.Contains(TestData[I].Key));
end;

procedure TestIKeyValueStore.TestContains;
var
  I: Integer;
begin
  LoadTestData;
  for I := Low(TestData) to High(TestData) do
    CheckTrue(FStore.Contains(TestData[I].Key));
end;

procedure TestIKeyValueStore.TestDelete;
var
  IgnoreNotFound: Boolean;
  aKey: string;
  Strings: TStringlist;
  I: Integer;
begin
  LoadTestData;
  aKey := TestData[1].Key;
  IgnoreNotFound := true;
  FStore.Delete(aKey, IgnoreNotFound);
  CheckFalse(FStore.Contains(aKey));

  // expect no exception on next statement
  FStore.Delete(aKey, IgnoreNotFound);

  // check that it was also deleted from the stores internal list,
  // not only from the hashtable
  Strings:= TStringlist.Create;
  try
    FStore.AssignTo(Strings);
    for I := Low(TestData) to High(TestData) do
      if TestData[I].Key <> aKey then
        CheckEquals(TestData[I].Value, Strings.Values[TestData[I].Key]);
  finally
    Strings.Free;
  end; {finally}

  ExpectedException := EKeyValueStoreError;
  IgnoreNotFound := false;
  FStore.Delete(aKey, IgnoreNotFound);
  Check(false, 'Should not end up here, EKeyValueStoreError not raised');
end;

procedure TestIKeyValueStore.TestFind;
begin
  // this test is actually redundant since we tested Find in the test
  // for Add, the two are not testable separately. 
  LoadTestData;
  VerifyTestData;
end;

procedure TestIKeyValueStore.TestGetEnumerator;
var
  ReturnValue: IKeyValueEnumerator;
  Results: array [Low(TestData)..High(TestData)] of Boolean;
  N: Integer;
begin
  LoadTestData;
  FillChar(Results, Sizeof(Results), 0);
  ReturnValue := FStore.GetEnumerator;
  CheckNotNull(ReturnValue);
  while ReturnValue.MoveNext do begin
    N := FindKeyInTestdata(ReturnValue.Current.Key);
    Check(N > 0, 'Enumerator returned unknown key');
    CheckFalse(Results[N], 'Enumerator returned a key twice');
    Results[N] := true;
  end; {while}
  for N := Low(Results) to HIgh(Results) do
    CheckTrue(Results[N], 'Enumerator did not return key number '+IntToStr(N));
end;

procedure TestIKeyValueStore.TestGetStreamer;
var
  ReturnValue: IKeyValueStreamer;
begin
  ReturnValue := FStore.GetStreamer;
  CheckNotNull(ReturnValue);
end;

procedure TestIKeyValueStore.TestLoadFromStrings;
var
  Append: Boolean;
  Source: TStrings;
  I: Integer;
begin
  Source := TStringList.Create;
  try
    for I := 128 to High(TestData) do
      Source.Values[TestData[I].Key] := TestData[I].Value;

    LoadTestData(0, 127);
    Append := false;
    FStore.LoadFromStrings(Source, Append, kvDupError);
    VerifyTestData(128, High(Testdata));
    CheckFalse(FStore.Contains(TestData[1].Key));
    FStore.Clear;
    LoadTestData(0, 127);
    Append := true;
    FStore.LoadFromStrings(Source, Append, kvDupError);
    VerifyTestData(0, High(Testdata));
  finally
    Source.Free;
  end; {finally}
end;

procedure TestIKeyValueStore.TestSaveAndLoadToFile;
var
  aFilename: string;
begin
  CreateTempFilename(aFilename);
  LoadTestData;
  FStore.SaveToFile(aFilename);
  try
    Check(FileExists(aFilename));
    FStore.Clear;
    FStore.LoadFromFile(aFilename);
    VerifyTestData;
  finally
    DeleteFile(aFilename);
  end; {finally}
end;

procedure TestIKeyValueStore.TestSaveAndLoadToStream;
var
  aStream: TStream;
begin
  aStream := TMemoryStream.Create;
  try
    LoadTestData;
    FStore.SaveToStream(aStream);
    FStore.Clear;
    aStream.Position := 0;
    FStore.LoadFromStream(aStream);
    VerifyTestData;
  finally
    aStream.Free;
  end; {finally}
end;

procedure TestIKeyValueStore.TestSetDuplicateHandling;
var
  Value: TKVDuplicateOptions;
begin
  for Value := Low(Value) to High(Value) do
    if Value <> kvDupDefault then begin
      FStore.SetDuplicateHandling(Value);
      Check(FStore.DuplicateHandling = Value);
    end;
  ExpectedException := EKeyValueStoreError;
  FStore.SetDuplicateHandling(kvDupDefault);
  Check(false, 'Should not end up here, exception not raised');
end;

procedure TestIKeyValueStore.VerifyTestData(First: Integer = 0; Last:
  Integer = 0);
var
  I: Integer;
  aValue: string;
begin
  if First = 0 then
    First := Low(TestData);
  if Last = 0 then
    Last := High(TestData);
  for I := First to Last do begin
    Check(FStore.Find(TestData[I].Key, aValue));
    CheckEquals(TestData[I].Value, aValue);
    // check case-insensitive find
    if not FCaseSensitive then
      CheckTrue(FStore.Find(TestData[I].UpperKey, aValue))
    else
      CheckFalse(FStore.Find(TestData[I].UpperKey, aValue));
  end;
end;

{ TestIKeyValueStoreCaseInsenstive }

function TestIKeyValueStoreCaseInsenstive.GetCaseSensitive: Boolean;
begin
  Result := false;
end;

initialization
  InitTestData;
  // Alle Testfälle beim Test-Runner registrieren
  RegisterTest(TestIKeyValueStore.Suite);
  RegisterTest(TestIKeyValueStoreCaseInsenstive.Suite);
end.

