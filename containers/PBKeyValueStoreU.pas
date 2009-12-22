{== PBKeyValueStoreU ==================================================}
{: This unit implements a hash table for key value pairs.
@author Dr. Peter Below
@desc   Version 1.0 created 2007-08-13<br/>
        Last modified       2007-08-13<p>
The class implemented here is actually just a wrapper around the
generic hash table for items that implement the IHashable interface
defined in the PBHashSupportU unit. We just have to store the key and
value strings into a small object that implements the IHashable interface.
The class also has functionality to store the list of key/value pairs
in streams or files and reload them from storage. We can also iterate
over the list (in no particular order). </p>
<p>The units exported functionality is completely based on interfaces, the
implementing classes are internal to the unit. A factory function is
provided to create an instance of the key value store, the lifetime of
all objects used is automatically managed by the interface reference
counting. An attempt has been made to make the implementation of
IKeyValueStore thread-safe, this has not been rigorously tested yet,
though.</p>
<p>The unit has been developed and tested with Delphi 2007. It will
probably compile in older versions down to Delphi 6, but that has not
been tested. The same applies to using this unit and the units it depends
on in a .NET project. </p> }
{======================================================================}
{$BOOLEVAL OFF}{Unit depends on shortcut boolean evaluation}
unit PBKeyValueStoreU;

interface

uses Sysutils, Classes, EnumeratorIntfU, PBHashSupportU;

type
  {: Public interface for an object storing a key/value pair. The key is
    read-only, the value can be changed. }
  IKeyValuePair = interface(IInterface)
  ['{D354CD15-1F5E-449B-B61A-ADD6EE14D869}']
    {: Return an interface for a deep copy of the implementor. }
    function Clone: IKeyValuePair;
    function GetKey: string;
    function GetValue: string;
    procedure SetValue(const Value: string);
    {: Returns the key part of the key/value pair. The key is read-only.}
    property Key: string read GetKey;
    {: Returns or modifies the value part of the key/value pair.}
    property Value: string read GetValue write SetValue;
  end;

  {: Public interface of an enumerator for a list of key/value pairs.
    <see interface="IEnumerator"/> for the navigation methods inherited.
    @example Typical usage follows the pattern
    <code>
     EnumVar := ..get enumerator interface from somewhere
     while EnumVar.MoveNext do begin
       Pair := EnumVar.Current;
       ...use Pair
     end;
     </code> }
  IKeyValueEnumerator = interface(IEnumerator)
  ['{36A0035D-620B-42D6-BE02-71EB4A4C426B}']
    function GetCurrent: IKeyValuePair;
    {: Returns the interface for the key/value pair at the enumerators
      current position.
     @raises EKVEnumeratorOutOfBounds if the enumerator is not on
       a valid position, e.g. if it is before the first or after the
       last item in the list or the list is empty.}
    property Current: IKeyValuePair read GetCurrent;
  end;

  {: Public interface of a key/value stream writer.
    @example Usage has to follow the pattern
     <code>
     BeginWrite...
     try
       Write..
       Write..
       ..
     finally
       EndWrite
     end;
     </code>
     Calls to Write outside the BeginWrite/EndWrite bracket will cause
     EKVStreamWriterError exceptions.     }
  IKeyValueWriter = interface(IInterface)
  ['{EC80ACB1-8A3D-487C-BD84-A068666C91E4}']
    {: Starts a series of key/value writes.
     @param Targetstream is the output stream to use. Writing starts at 
       the current stream position.
     @param Itemcount is an optional item count. If the count is 0 the
       implementor can either decide to not store the count or later
       seek back to the start position and write the accumulated count
       there. This only works with streams that support seeking to 
       arbitrary locations (not for a socket stream, for example)! If 
       a count > 0 is given the implementor should treat it as an error
       if the count of actually written key/value pairs is different.
     @precondition TargetStream not nil }
    procedure BeginWrite(TargetStream: TStream; Itemcount: Cardinal= 0);
    
    {: End a series of key/value writes.}
    procedure EndWrite;
    
    {: Write a key/value pair to the stream.
     @precondition aPair not nil.
     @raises EKVStreamWriterError if called before BeginWrite or after
       EndWrite.}
    procedure Write(aPair: IKeyValuePair); overload;
    
    {: Write a key/value pair to the stream.
     @precondition aKey not empty. 
     @raises EKVStreamWriterError if called before BeginWrite or after
       EndWrite.}
    procedure Write(const aKey, aValue: string); overload;
  end;

  {: Public interface of a key/value stream reader.
    @example Usage has to follow the pattern
     <code>
     BeginRead...
     try
       while Read(..) do
         process the read values
     finally
       EndRead
     end;
     </code>
     Calls to Read outside the BeginRead/EndRead bracket will cause
     EKVStreamReaderError exceptions.     }
  IKeyValueReader = interface(IInterface)
  ['{E48C89D7-47D1-40BC-B8FA-0283A45D0FDC}']
    {: Starts a series of key/value reads.
     @param SourceStream is the input stream to use. Reading starts at
       the current stream position.
     @param Count will return the stored count of items, or 0, if the
       implementor does not store the count.
     @param aCaseSensitive determines whether the IKeyValuePair created
       by the Read method is marked as case sensitive or not.
     @precondition SourceStream not nil }
    procedure BeginRead(SourceStream: TStream; out Count: Cardinal;
      aCaseSensitive: Boolean = true);

    {: End a series of key/value reads.}
    procedure EndRead;
    
    {: Read a key/value pair from the stream.
     @param aPair returns the interface of the read key/value pair, or
       nil if there is none to read.
     @returns true if a pair was read, false if the end of the list has
       been reached.  
     @raises EKVStreamReaderError if called before BeginRead or after
       EndRead.}
    function Read(out aPair: IKeyValuePair): Boolean; overload;
    
    {: Read a key/value pair from the stream.
     @param aKey and aValue return the read key/value pair, or
       empty strings if there is none to read.
     @returns true if a pair was read, false if the end of the list has
       been reached.  
     @raises EKVStreamReaderError if called before BeginRead or after
       EndRead.}
    function Read(out aKey, aValue: string): Boolean; overload;
  end;

  {: Public interface for a helper class implementing streaming for
    a list of key/value pairs. The implementor determines the data
    format used. }
  IKeyValueStreamer = interface(IInterface)
  ['{275D6387-BDC7-40FB-8E18-32EB670325FF}']
    {: Read accessor for the <see property="Reader"/> property}
    function GetReader: IKeyValueReader;
    {: Read accessor for the <see property="Writer"/> property}
    function GetWriter: IKeyValueWriter;
    {: Returns the interface for the stream reader. The reader expects
      a stream that was written by the matching stream writer. }
    property Reader: IKeyValueReader read GetReader;
    {: Returns the interface for the stream writer. The writer creates
      a stream that can be read by the matching stream reader. }
    property Writer: IKeyValueWriter read GetWriter;
  end;

  {: This enumeration is used to define how the key/value store should
    act if an attempt is made to add a key/value pair whos key is already
    in the store.
    @enum kvDupReplace replaces the value of the existing key entry with
      the new value. This is the default action.
    @enum kvDupIgnore simply ignores the addition request if the key is
      already in the store, even if the value is different.
    @enum kvDupError raises EKVDuplicateError if a duplicate key is added
      to the store.
    @enum kvDupDefault uses the value of the stores DuplicateHandling
      property and is itself not a valid value for this property.  }
  TKVDuplicateOptions = (kvDupReplace, kvDupIgnore, kvDupError, kvDupDefault);

  {: Public interface for a key/value store. The methods having a
    precondition will raise EKVPreconditionViolation if the precondition
    is violated.}
  IKeyValueStore = interface(IInterface)
  ['{F515DE55-FFB9-44A2-BEB5-DA15209F7B77}']
    {: Add a key/value pair to the store.
     @param aKey is the key to add.
     @param aValue is the value to associate with the key.
     @param Duplicates specifies how to react if a pair with the passed
       key is already in the store.
     @precondition aKey not empty
     @raises EKVDuplicateError if the key is already in the store and the
       duplicate handling option is kvDupError.}
    procedure Add(const aKey, aValue: string; Duplicates:
      TKVDuplicateOptions = kvDupDefault);

    {: Copy the list of key/value pairs to the passed strings instance,
      replacing any previous content.
     @precondition Strings not nil.
     @desc The format used is the standard name=value format supported by
      Names and Values property of TStrings &amp; Cie.}
    procedure AssignTo(Strings: TStrings);

    {: Remove all entries from the store. }
    procedure Clear;

    {: Check if the passed key is in the store.
     @precondition aKey not empty. }
    function Contains(const aKey: string): Boolean;

    {: Remove an item from the table.
     @param aKey is the items Key.
     @param IgnoreNotFound defines what to do if an item with the
       passed key is not found. If true nothing is done in this
       case, if false an exception will be raised.
     @precondition aKey not empty
     @raises EKeyValueStoreError if IgnoreNotFound is false and an item
       with the passed key is not found in the table.  }
    procedure Delete(const aKey: string; IgnoreNotFound: Boolean = true);

    {: Search for a key in the store and retrieve its value.
    @param aKey is the key to look for.
    @param aValue returns value associated with the key, if it is found.
      The variable passed is not changed if the search fails.
    @precondition aKey not empty
    @returns true if the key is found, false otherwise.  }
    function Find(const aKey: string; var aValue: string): Boolean;

    {: Read accessor for the DuplicateHandling property.}
    function GetDuplicateHandling: TKVDuplicateOptions;

    {: Returns an enumerator for the list of key/value pairs held by the
      store. This is a fairly expensive operation since the enumerator
      will work on a <b>copy</b> of the list for reasons of thread-safety
      and data consistency. Changing the values of items returned by
      the enumerator has no effect on the items in the store!}
    function GetEnumerator: IKeyValueEnumerator;

    {: Read accessor for the Streamer property. }
    function GetStreamer: IKeyValueStreamer;

    {: Load the list of key/value pairs from a file.
    @param aFilename is the source files name, should be a full path name.
    @precondition aFilename not empty.
    @desc The data format is determined by the stream helper used. The
     default stream reader will expect a binary storage format created
     using the VCL TWriter class.<br/>
     Any existing content of the store is lost, if you want to preserve
     it use the <see method="LoadFromStream"/> method with a TFilestream
     instance.
    @raises EFOpenError if the file does not exist. }
    procedure LoadFromFile(const aFilename: string);

    {: Load the list of key/value pairs from a stream.
    @param SourceStream is the source stream. Reading will start at the
      current stream position.
    @param Append specifies whether to clear the store first (false, the
      default) or append the stream contents to the store (true).
     @param Duplicates specifies how to react if a pair read from the
       stream has a key that is already in the store.
    @precondition SourceStream not nil.
    @desc The data format is determined by the stream helper used. The
     default stream reader will expect a binary storage format created
     using the VCL TWriter class.     }
    procedure LoadFromStream(SourceStream: TStream; Append: Boolean =
      false; Duplicates: TKVDuplicateOptions = kvDupDefault);

    {: Load the list of key/value pairs from a string list.
    @param Source is the source list. It is expected to have items in
      the standard Name=Value format. Lines with empty names will be
      ignored.
    @param Append specifies whether to clear the store first (false, the
      default) or append the source contents to the store (true).
     @param Duplicates specifies how to react if a pair read from the
       source has a key that is already in the store.
    @precondition Source not nil.}
    procedure LoadFromStrings(Source: TStrings; Append: Boolean =
      false; Duplicates: TKVDuplicateOptions = kvDupDefault);

    {: Save the list of key/value pairs to a file.
    @param aFilename is the target files name, should be a full path name.
    @precondition aFilename not empty.
    @desc The data format is determined by the stream helper used. The
     default stream writer will create a binary storage format using the
     VCLs TWriter class.}
    procedure SaveToFile(const aFilename: string);

    {: Save the list of key/value pairs to a stream.
    @param aStream is the target stream for the data.
    @precondition aStream not nil.
    @desc Starts writing at the current stream position. The data format
     is determined by the stream helper used. The default stream writer
     will create a binary storage format using the VCLs TWriter class.}
    procedure SaveToStream(aStream: TStream);

    {: Write accessor for the DuplicateHandling property.
     @raises EKeyValueStoreError if Value is kvDupDefault.}
    procedure SetDuplicateHandling(const Value: TKVDuplicateOptions);

    {: Write accessor for the Streamer property.
    @param Value is the streamer to use, can be nil to force use of the
      default streamer.}
    procedure SetStreamer(const Value: IKeyValueStreamer);

    {: Get or set the default duplicate handling option. }
    property DuplicateHandling: TKVDuplicateOptions read
      GetDuplicateHandling write SetDuplicateHandling;

    {: Get or set the interface for the helper object to use to store
      the key/value list to a stream or restore it from a stream. The
      default streamer uses a binary data format as created by the VCL
      class TWriter and expected by TReader.  }
    property Streamer: IKeyValueStreamer read GetStreamer write SetStreamer;
  end;

  {: Exception base class for errors in the key value store and 
    associated classes.}
  EKeyValueStoreError = class(Exception);
  
  {: Used to report attempts to add a key already in the store.}
  EKVDuplicateError     = class(EKeyValueStoreError);
  
  {: Raised by a method if a precondition is violated.}
  EKVPreconditionViolation = class(EKeyValueStoreError);
  
  {: Exception raised if a key value enumerator is not on a valid
    position when its GetCurrent method is called. }
  EKVEnumeratorOutOfBounds = class(EKeyValueStoreError);
  
  {: Base class for errors encountered in a key/value streamer. }
  EKVStreamerError = class(EKeyValueStoreError);

  {: Used to report errors in a key/value stream reader.}
  EKVStreamReaderError = class(EKVStreamerError);

  {: Used to report errors in a key/value stream writer.}
  EKVStreamWriterError = class(EKVStreamerError);


{: Creates a new key/value store and returns its interface.
@param InitialSize specifies the initial number of slots in the
  hash table. If a value < 1000 is specified 1000 is used instead.
  In fact the hash table will use the closest prime number to the
  passed value. The hash table will grow automatically when it is
  about 2/3 full, but that is a slow process and should be avoided,
  if possible.
@param CaseSensitive determines whether the hash generation and key
  lookup will be case sensitive or not. A case-insensitive version
  is a bit slower and needs more memory, since it stores an upper-
  case version of the key to minimize the performance hit.
@param aHashfunction specifies the hash function to use. By default
  this will be the ElfHash function from PBHashSupportU.
@returns the created objects interface. The interface controls the
  lifetime of the object through reference-counting.   }
function CreateKeyValueStore(InitialSize: Cardinal = 1000;
  CaseSensitive: Boolean = true; aHashfunction: THashFunction = nil):
  IKeyValueStore;

{: Create an instance of the default streamer and return its interface.
 The interface controls the lifetime of the streamer through reference
 counting.} 
function CreateBinaryKeyValueStreamer: IKeyValueStreamer;

implementation

uses PBInterfaceHashtableU, SyncObjs;

const
  NStreamBufferSize = 4096;

resourcestring
  SStringsParameterCannotBeNil =
    'TKeyValueStore.AssignTo: Strings parameter cannot be nil.';
  SSourceParameterCannotBeNil =
    'TKeyValueStore.LoadFromStrings: Source parameter cannot be nil.';
  SExpectedAndActualCountMismatch =
    '%s: Expected (%d) and actual (%d) count of items do not match.';
  SInvalidFilename =
    'TKeyValueStore.%s: Filename cannot be empty.';
  SInvalidStreamParameter = 
    '%s: aStream parameter cannot be nil.';
  SCannotDeleteKey =
    'TKeyValueStore.Delete: Key "%s" not found in store.';
  SEmptyKeyNotAllowed =
    'An empty key is not allowed for %s';
  SDuplicateNotAllowed =
    'The key "%s" is already in the store. Duplicates are not allowed!';
  SInvalidValueForDuplicateHandling =
    'kvDupDefault is not a valid value for the DuplicateHandling property!';
  SEnumeratorOutOfBounds =
    'The key/value enumerator is not positioned on a valid item.';

type
  TKeyValueStore = class(TInterfacedObject, IKeyValueStore)
  private
    FCaseSensitive: Boolean;
    FDuplicateHandling: TKVDuplicateOptions;
    FGuardian: TCriticalSection;
    FHashTable: IInterfaceHashtable;
    FKeyValuePairs: TInterfaceList;
    FStreamer: IKeyValueStreamer;
    function GetDuplicateHandling: TKVDuplicateOptions;
    procedure SetDuplicateHandling(const Value: TKVDuplicateOptions);
  protected
    procedure Add(const aKey, aValue: string; Duplicates:
      TKVDuplicateOptions = kvDupDefault);
    procedure AssignTo(Strings: TStrings);
    procedure CheckKey(const aKey, aProcname: string);
    procedure Clear;
    function Contains(const aKey: string): Boolean;
    procedure Delete(const aKey: string; IgnoreNotFound: Boolean = true);
    function Find(const aKey: string; var aValue: string): Boolean;
    function GetEnumerator: IKeyValueEnumerator;
    function GetStreamer: IKeyValueStreamer;
    function InternalFind(const aKey: string; out Intf: IKeyValuePair):
      Boolean;
    procedure LoadFromFile(const aFilename: string);
    procedure LoadFromStream(SourceStream: TStream; Append: Boolean =
      false; Duplicates: TKVDuplicateOptions = kvDupDefault);
    procedure LoadFromStrings(Source: TStrings; Append: Boolean = false;
      Duplicates: TKVDuplicateOptions = kvDupDefault);
    procedure Lock;
    procedure SaveToStream(aStream: TStream);
    procedure SaveToFile(const aFilename: string);
    procedure SetStreamer(const Value: IKeyValueStreamer);
    procedure Unlock;
    property CaseSensitive: Boolean read FCaseSensitive;
    property Guardian: TCriticalSection read FGuardian;
    property DuplicateHandling: TKVDuplicateOptions read
      GetDuplicateHandling write SetDuplicateHandling;
    property KeyValuePairs: TInterfaceList read FKeyValuePairs write
      FKeyValuePairs;
    property Streamer: IKeyValueStreamer read GetStreamer write SetStreamer;
  public
    constructor Create(InitialSize: Cardinal = 1000; aHashfunction:
      THashFunction = nil; aCaseSensitive: Boolean = true); reintroduce;
    destructor Destroy; override;
  end;

  TKeyValuePair = class(TInterfacedObject, IKeyValuePair, IHashable)
  private
    FCaseSensitive: Boolean;
    FKey: string;
    FUpperCaseKey: string;
    FValue: string;
    function GetOriginalKey: string;
    function GetHashKey: string;
  protected
    function IKeyValuePair.GetKey = GetOriginalKey;
    function IHashable.GetKey = GetHashKey;
    function Clone: IKeyValuePair;
    function GetValue: string;
    procedure SetValue(const Value: string);
    property CaseSensitive: Boolean read FCaseSensitive write
      FCaseSensitive;
  public
    constructor Create(const AKey, AValue: string; ACaseSensitive:
      Boolean); reintroduce;
    property Key: string read GetOriginalKey;
    property Value: string read GetValue write SetValue;
  end;

  TKeyValueEnumerator = class(TAbstractEnumerator, IKeyValueEnumerator)
  private
    FList: TInterfaceList;
  protected
    function GetCurrent: IKeyValuePair;
    function GetCount: Integer; override;
    property List: TInterfaceList read FList;
  public
    constructor Create(aList: TInterfaceList); reintroduce;
    destructor Destroy; override;
  end;

  TBinaryKeyValueReader = class(TInterfacedObject, IKeyValueReader)
  private
    FActualCount: Cardinal;
    FCaseSensitive: Boolean;
    FExpectedCount: Cardinal;
    FReader: TReader;
  protected
    procedure BeginRead(SourceStream: TStream; out Count: Cardinal;
      aCaseSensitive: Boolean);
    procedure EndRead;
    function Read(out aKey, aValue: string): Boolean; overload;
    function Read(out aPair: IKeyValuePair): Boolean; overload;
    property ActualCount: Cardinal read FActualCount;
    property CaseSensitive: Boolean read FCaseSensitive;
    property ExpectedCount: Cardinal read FExpectedCount;
    property Reader: TReader read FReader;
  public
    destructor Destroy; override;
  end;

  TBinaryKeyValueWriter = class(TInterfacedObject, IKeyValueWriter)
  private
    FActualCount: Cardinal;
    FExpectedCount: Cardinal;
    FStream: TStream;
    FStreamstart: Int64;
    FWriter: TWriter;
  protected
    procedure BeginWrite(TargetStream: TStream; Itemcount: Cardinal = 0);
    procedure EndWrite;
    procedure Write(const aKey, aValue: string); overload;
    procedure Write(aPair: IKeyValuePair); overload;
    property ActualCount: Cardinal read FActualCount;
    property ExpectedCount: Cardinal read FExpectedCount;
    property Stream: TStream read FStream;
    property Streamstart: Int64 read FStreamstart;
    property Writer: TWriter read FWriter;
  public
    destructor Destroy; override;
  end;

  TBinaryKeyValueStreamer = class(TInterfacedObject, IKeyValueStreamer)
  private
    FReader: IKeyValueReader;
    FWriter: IKeyValueWriter;
  protected
    function GetReader: IKeyValueReader;
    function GetWriter: IKeyValueWriter;
  end;

function CreateKeyValueStore(InitialSize: Cardinal = 1000;
  CaseSensitive: Boolean = true; aHashfunction: THashFunction = nil):
  IKeyValueStore;
begin
  Result := TKeyValueStore.Create(InitialSize, aHashfunction, CaseSensitive)
    as IKeyValueStore;
end;

function CreateBinaryKeyValueStreamer: IKeyValueStreamer;
begin
  Result := TBinaryKeyValueStreamer.Create as IKeyValueStreamer;
end;

{== TKeyValuePair =====================================================}

constructor TKeyValuePair.Create(const AKey, AValue: string;
  ACaseSensitive: Boolean);
begin
  inherited Create;
  FKey := AKey;
  FValue := AValue;
  FCaseSensitive := ACaseSensitive;
  if not CaseSensitive then
    FUpperCaseKey := AnsiUpperCase(aKey);
end;

function TKeyValuePair.Clone: IKeyValuePair;
begin
  Result := TKeyValuePair.Create(FKey, FValue, CaseSensitive) as IKeyValuePair;
end;

function TKeyValuePair.GetHashKey: string;
begin
  if CaseSensitive then
    Result := FKey
  else
    Result := FUpperCaseKey;
end;

function TKeyValuePair.GetOriginalKey: string;
begin
  Result := FKey
end;

function TKeyValuePair.GetValue: string;
begin
  Result := FValue;
end;

procedure TKeyValuePair.SetValue(const Value: string);
begin
  FValue := Value;
end;

{== TKeyValueEnumerator ===============================================}

{: Create a copy of the data in the passed list and store it into an
  internal list.}
constructor TKeyValueEnumerator.Create(aList: TInterfaceList);
var
  I: Integer;
  Intf: IKeyValuePair;
begin
  inherited Create;
  FList := TInterfaceList.Create();
  if Assigned(aList) then begin
    for I := 0 to  aList.Count - 1 do begin
      Intf := aList[I] as IKeyValuePair;
      List.Add(Intf.Clone as IInterface);
    end; {for}
  end; {if}
end;

destructor TKeyValueEnumerator.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

function TKeyValueEnumerator.GetCount: Integer;
begin
  Result := List.Count;
end;

function TKeyValueEnumerator.GetCurrent: IKeyValuePair;
begin
  if IsValidIndex(CurrentIndex) then
    Result := List[CurrentIndex] as IKeyValuePair
  else
    raise EKVEnumeratorOutOfBounds.Create(SEnumeratorOutOfBounds);
end;

{== TBinaryKeyValueWriter ===========================================}

destructor TBinaryKeyValueWriter.Destroy;
begin
  inherited Destroy;
  Assert(not Assigned(FWriter), 'TBinaryKeyValueWriter: EndWrite not called!');
end;

procedure TBinaryKeyValueWriter.BeginWrite(TargetStream: TStream;
  Itemcount: Cardinal);
const 
  Procname = 'TBinaryKeyValueStreamer.BeginWrite';  
begin
  if not Assigned(TargetStream) then
    raise EKVPreconditionViolation.CreateFmt(SInvalidStreamParameter,
      [Procname]);
  FStream := TargetStream;
  FWriter := TWriter.Create(Stream, NStreamBufferSize);
  FExpectedCount := ItemCount;
  FActualCount := 0;
  FStreamstart := FStream.Position;
  Writer.WriteInteger(ExpectedCount);
end;

procedure TBinaryKeyValueWriter.EndWrite;
const 
  Procname = 'TBinaryKeyValueWriter.EndWrite';
var
  CurrentPos: int64;
begin
  FWriter.FlushBuffer;
  FreeAndNil(FWriter);
  if ActualCount <> ExpectedCount then
    if ExpectedCount = 0 then begin
      CurrentPos := Stream.Position;
      Stream.Position := Streamstart;
      Stream.WriteBuffer(FActualCount, Sizeof(FActualCount));
      Stream.Position := CurrentPos;
    end {if}
    else begin
      raise EKVStreamWriterError.CreateFmt(SExpectedAndActualCountMismatch,
        [Procname, ExpectedCount, ActualCount]);
    end; {else}
end;

procedure TBinaryKeyValueWriter.Write(const aKey, aValue: string);
const
  Procname= 'TBinaryKeyValueWriter.Write';
begin
  if aKey = '' then
    raise EKVPreconditionViolation.CreateFmt(SEmptyKeyNotAllowed, 
      [Procname]);
  
  Writer.WriteString(aKey);
  Writer.WriteString(aValue);
  Inc(FActualCount);
end;

procedure TBinaryKeyValueWriter.Write(aPair: IKeyValuePair);
begin
  Write(aPair.Key, aPair.Value);
end;

{== TBinaryKeyValueReader ===========================================}

destructor TBinaryKeyValueReader.Destroy;
begin
  inherited Destroy;
  Assert(not Assigned(FReader), 'TBinaryKeyValueReader: EndRead not called!');
end;

procedure TBinaryKeyValueReader.BeginRead(SourceStream: TStream; out
  Count: Cardinal; aCaseSensitive: Boolean);
const
  Procname = 'TBinaryKeyValueReader.BeginRead';
begin
  if not Assigned(SourceStream) then
    raise EKVPreconditionViolation.CreateFmt(SInvalidStreamParameter,
      [Procname]);
  FReader := TReader.Create(SourceStream, NStreamBufferSize);
  Count := Reader.ReadInteger;
  FExpectedCount := Count;
  FActualCount := 0;
  FCaseSensitive := aCaseSensitive;
end;

procedure TBinaryKeyValueReader.EndRead;
begin
  FreeAndNil(FReader);
  {Note: we do not treat it as an error if less than the available
   number of items is read. But a read cannot be resumed from the
   resulting stream position!}
end;

function TBinaryKeyValueReader.Read(out aKey, aValue: string): Boolean;
begin
  Result := ActualCount < ExpectedCount;
  if Result then begin
    aKey := Reader.ReadString;
    aValue := Reader.ReadString;
    Inc(FActualCount);
  end {if}
  else begin
    aKey := '';
    aValue := '';
  end; {else}
end;

function TBinaryKeyValueReader.Read(out aPair: IKeyValuePair): Boolean;
var
  aKey, aValue: string;
begin
  Result := Read(aKey, aValue);
  if Result then
    aPair := TKeyValuePair.Create(aKey, aValue, CaseSensitive) as IKeyValuePair
  else
    aPair := nil;  
end;

{== TBinaryKeyValueStreamer ===========================================}

function TBinaryKeyValueStreamer.GetReader: IKeyValueReader;
begin
  if not Assigned(FReader) then
    FReader := TBinaryKeyValueReader.Create as IKeyValueReader;
  Result := FReader;
end;

function TBinaryKeyValueStreamer.GetWriter: IKeyValueWriter;
begin
  if not Assigned(FWriter) then
    FWriter := TBinaryKeyValueWriter.Create as IKeyValueWriter;
  Result := FWriter;
end;

{== TKeyValueStore ====================================================}

constructor TKeyValueStore.Create(InitialSize: Cardinal = 1000;
  aHashfunction: THashFunction = nil; aCaseSensitive: Boolean = true);
begin
  inherited Create;
  FHashTable := CreateHashtable(InitialSize, aHashfunction);
  FGuardian := TCriticalSection.Create();
  FKeyValuePairs := TInterfaceList.Create();
  FDuplicateHandling := kvDupReplace;
  FCaseSensitive := aCaseSensitive;
end;

destructor TKeyValueStore.Destroy;
begin
  FreeAndNil(FKeyValuePairs);
  FreeAndNil(FGuardian);
  inherited Destroy;
end;

procedure TKeyValueStore.Add(const aKey, aValue: string; Duplicates:
  TKVDuplicateOptions = kvDupDefault);
var
  Intf: IKeyValuePair;
const
  Procname = 'Add';
begin
  CheckKey(aKey, Procname);
  if Duplicates = kvDupDefault then
    Duplicates := DuplicateHandling;
  Lock;
  try
    if InternalFind(aKey, Intf) then begin
      case Duplicates of
        kvDupReplace: Intf.Value := aValue;
        kvDupIgnore: ; // do nothing
        kvDupError: raise EKVDuplicateError.CreateFmt(
          SDuplicateNotAllowed, [aKey]);
      else
        Assert(false, 'Unexpected value for Duplicates');
      end; {case}
    end {if}
    else begin
      Intf := TKeyValuePair.Create(aKey, aValue, CaseSensitive) as IKeyValuePair;
      FHashtable.Add(Intf as IHashable);
      KeyValuePairs.Add(Intf as IInterface);
    end; {else}
  finally
    Unlock;
  end; {finally}
end;

procedure TKeyValueStore.AssignTo(Strings: TStrings);
var
  I: Integer;
  Pair: IKeyValuePair;
begin
  if not Assigned(Strings) then
    raise EKVPreconditionViolation.Create(SStringsParameterCannotBeNil);
  Strings.Clear;
  Lock;
  try
    for I := 0 to KeyValuePairs.Count - 1 do begin
      Pair := KeyValuePairs[I] as IKeyValuePair;
      Strings.Values[Pair.Key] := Pair.Value;
    end; {for}
  finally
    Unlock;
  end; {finally}
end;

procedure TKeyValueStore.CheckKey(const aKey, aProcname: string);
const
  CFmt = '%s.%s';
begin
  if aKey = '' then
    raise EKVPreconditionViolation.CreateFmt(SEmptyKeyNotAllowed, 
      [Format(CFmt, [Classname, aProcname])]);
end;

procedure TKeyValueStore.Clear;
begin
  Lock;
  try
    FHashTable.Clear;
    KeyValuePairs.Clear;
  finally
    Unlock;
  end; {finally}
end;

function TKeyValueStore.Contains(const aKey: string): Boolean;
var
  Intf: IKeyValuePair;
const
  Procname = 'Contains';
begin
  CheckKey(aKey, Procname);
  Lock;
  try
    Result := InternalFind(aKey, Intf);
  finally
    Unlock;
  end; {finally}
end;

procedure TKeyValueStore.Delete(const aKey: string; IgnoreNotFound:
  Boolean = true);
var
  Intf: IKeyValuePair;
const
  Procname = 'Delete';
begin
  CheckKey(aKey, Procname);
  Lock;
  try
    if InternalFind(aKey, Intf) then begin
      if CaseSensitive then
        FHashTable.Delete(aKey, true)
      else
        FHashTable.Delete(AnsiUppercase(aKey), true);
        
      KeyValuePairs.Remove(Intf as IInterface);
    end
    else if not IgnoreNotFound then
      raise EKeyValueStoreError.CreateFmt(SCannotDeleteKey,[aKey]);
  finally
    Unlock;
  end; {finally}
end;

function TKeyValueStore.Find(const aKey: string; var aValue: string):
  Boolean;
var
  Intf: IKeyValuePair;
const
  Procname = 'Find';
begin
  CheckKey(aKey, Procname);
  Result := InternalFind(aKey, Intf);
  if Result then
    aValue := Intf.Value;
end;

function TKeyValueStore.GetDuplicateHandling: TKVDuplicateOptions;
begin
  Result := FDuplicateHandling;
end;

function TKeyValueStore.GetEnumerator: IKeyValueEnumerator;
begin
  Lock;
  try
    Result := TKeyValueEnumerator.Create(KeyValuePairs) as IKeyValueEnumerator;
  finally
    Unlock;
  end; {finally}
end;

function TKeyValueStore.GetStreamer: IKeyValueStreamer;
begin
  Lock;
  try
    if not Assigned(FStreamer) then
      FStreamer:= TBinaryKeyValueStreamer.Create as IKeyValueStreamer;
    Result := FStreamer;
  finally
    Unlock;
  end; {finally}
end;

function TKeyValueStore.InternalFind(const aKey: string; out Intf:
  IKeyValuePair): Boolean;
var
  Hash: IHashable;
begin
  if CaseSensitive then
    Result := FHashtable.Find(aKey, Hash)
  else
    Result := FHashtable.Find(AnsiUpperCase(aKey), Hash);
  if Result then
    Intf := Hash as IKeyValuePair;
end;

procedure TKeyValueStore.LoadFromFile(const aFilename: string);
const
  Procname = 'LoadFromFile';
var
  Stream: TFilestream;
begin
  if aFilename = '' then
    raise EKVPreconditionViolation.CreateFmt(SInvalidFilename,
      [Procname]);
  Stream:= TFilestream.Create(aFilename, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end; {finally}
end;

procedure TKeyValueStore.LoadFromStream(SourceStream: TStream; Append:
  Boolean = false; Duplicates: TKVDuplicateOptions = kvDupDefault);
const
  Procname = 'TKeyValueStore.LoadFromStream';
var
  Reader: IKeyValueReader;
  Key, Value: string;
  I: Cardinal;
begin
  if not Assigned(SourceStream) then
    raise EKVPreconditionViolation.CreateFmt(SInvalidStreamParameter,
      [Procname]);
  Lock;
  try
    if not Append then
      Clear;
    Reader := Streamer.Reader;
    Reader.BeginRead(SourceStream, I, CaseSensitive);
    try
      while Reader.Read(Key, Value) do
        Add(Key, Value, Duplicates);
    finally
      Reader.EndRead;
    end; {finally}
  finally
    Unlock;
  end; {finally}
end;

procedure TKeyValueStore.LoadFromStrings(Source: TStrings; Append:
  Boolean = false; Duplicates: TKVDuplicateOptions = kvDupDefault);
var
  I: Integer;
  Key: string;
begin
  if not Assigned(Source) then
    raise EKVPreconditionViolation.Create(SSourceParameterCannotBeNil);
  Lock;
  try
    if not Append then
      Clear;
    for I := 0 to Source.Count - 1 do begin
      Key := Source.Names[I];
      if Key <> '' then
        Add(Key, Source.ValueFromIndex[I], Duplicates);
    end; {for}
  finally
    Unlock;
  end; {finally}
end;

procedure TKeyValueStore.Lock;
begin
  Guardian.Acquire;
end;

procedure TKeyValueStore.SaveToFile(const aFilename: string);
const
  Procname = 'SaveToFile';
var
  Stream: TFilestream;
begin
  if aFilename = '' then
    raise EKVPreconditionViolation.CreateFmt(SInvalidFilename,
      [Procname]);
  Stream := TFilestream.Create(aFilename, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end; {finally}
end;

procedure TKeyValueStore.SaveToStream(aStream: TStream);
const
  Procname = 'TKeyValueStore.SaveToStream';
var
  Writer: IKeyValueWriter;
  I: Integer;
begin
  if not Assigned(aStream) then
    raise EKVPreconditionViolation.CreateFmt(SInvalidStreamParameter,
      [Procname]);
  Lock;
  try
    Writer := Streamer.Writer;
    Writer.BeginWrite(aStream, KeyValuePairs.Count);
    try
      for I := 0 to KeyValuePairs.Count - 1 do
        Writer.Write(KeyValuePairs[I] as IKeyValuePair);
    finally
      Writer.EndWrite;
    end; {finally}
  finally
    Unlock;
  end; {finally}
end;

procedure TKeyValueStore.SetDuplicateHandling(
  const Value: TKVDuplicateOptions);
begin
  if Value = kvDupDefault then
    raise EKeyValueStoreError.Create(SInvalidValueForDuplicateHandling);
  FDuplicateHandling := Value;
end;

procedure TKeyValueStore.SetStreamer(const Value: IKeyValueStreamer);
begin
  Lock;
  try
    if FStreamer <> Value then    
      FStreamer := Value;
  finally
    Unlock;
  end; {finally}
end;

procedure TKeyValueStore.Unlock;
begin
  Guardian.Release;
end;


end.
