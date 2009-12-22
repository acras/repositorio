{== EnumeratorIntfU ===================================================}
{: This interface defines a base interface for enumerators.
@author Dr. Peter Below
@desc   Version 1.0 created 2006-03-13<br/>
        Last modified       2006-05-22<p>
   }
{======================================================================}
{$BOOLEVAL OFF}{Unit depends on shortcut boolean evaluation}
unit EnumeratorIntfU;

interface

type
  {: Base type for enumerators that do not support Count and Reset }
  IBaseEnumerator = interface(IInterface)
  ['{72CA877E-5241-48E5-864F-94AA06A97911}']
    {: If there are more items to enumerate, move to the next one
      and return true, otherwise return false. }
    function MoveNext: Boolean;
  end;

  {: Base interface for an enumerator. Specific decendents need to
   add a function Current: someType; that returns the item at the
   current position of the enumerator.}
  IEnumerator = interface(IBaseEnumerator)
  ['{B22884F4-5D34-492D-B01E-34A5D33F159B}']
    {: Read accessor for the <see property="Count"/> property}
    function GetCount: Integer;

    {: Resets the enumerator to before the start of the list.}
    procedure Reset;

    {: Returns the number of items the enumerator has in its list.}
    property Count: Integer read GetCount;
  end;

  {: A class can implement this interface to indicate that it support
    an enumerator. }
  IEnumerable = interface(IInterface)
  ['{BCF96FF0-3A69-413B-AD4C-B5182B671D11}']
    {: Returns the enumerator instance to use. The instance will
     typically implement an enumerator with additional methods,
     based on IEnumerator. }
    function GetEnumerator: IEnumerator;
  end;

  {: Abstract base class for enumerators. Descendents will typically
    add a new constructor which takes a kind of list or array as
    parameter, override the GetCount method, and add a new Current
    method to return the item the enumerator points at.}
  TAbstractEnumerator = class (TInterfacedObject,
    IBaseEnumerator, IEnumerator)
  private
    FCurrentIndex: Integer;
  protected
    function GetCount: Integer; virtual; abstract;
    function IsValidIndex(Index: Integer): Boolean; virtual;
    function MoveNext: Boolean; virtual;
    procedure Reset;
    property CurrentIndex: Integer read FCurrentIndex;
  public
    constructor Create; virtual;
  end;

implementation

constructor TAbstractEnumerator.Create;
begin
  inherited;
  FCurrentIndex := -1;
end;

function TAbstractEnumerator.IsValidIndex(Index: Integer): Boolean;
begin
  Result := (Index >= 0) and (Index < GetCount);
end;

function TAbstractEnumerator.MoveNext: Boolean;
begin
  Inc(FCurrentIndex);
  Result := IsValidIndex(FCurrentIndex);
end;

procedure TAbstractEnumerator.Reset;
begin
  FCurrentIndex := -1;
end;

end.
