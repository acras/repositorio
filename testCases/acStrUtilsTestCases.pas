unit acStrUtilsTestCases;

interface

  uses testFramework;

implementation

uses acStrUtils;

  type

  TacStrUtilsTestCases = class(TTestCase)
  private
  published
    procedure testGetStrField2CamposComuns;
    procedure testGetStrField2CampoAlemDoFim;
    procedure testGetStrField2CampoAntesDoInicio;
  end;

  { TacStrUtilsTestCases }

procedure TacStrUtilsTestCases.testGetStrField2CamposComuns;
var
  str: String;
begin
  str := '1,2,3,4,5,6';
  check(getStrField2(str, ',', 1) = '1');
  check(getStrField2(str, ',', 2) = '2');
  check(getStrField2(str, ',', 3) = '3');
  check(getStrField2(str, ',', 4) = '4');
  check(getStrField2(str, ',', 5) = '5');
  check(getStrField2(str, ',', 6) = '6');
end;

procedure TacStrUtilsTestCases.testGetStrField2CampoAlemDoFim;
var
  str: String;
begin
  str := '1,2,3,4,5,6';
  check(getStrField2(str, ',', 7) = '');
  check(getStrField2(str, ',', 8) = '');
  check(getStrField2(str, ',', 999) = '');
end;

procedure TacStrUtilsTestCases.testGetStrField2CampoAntesDoInicio;
var
  str: String;
begin
  str := '1,2,3,4,5,6';
  check(getStrField2(str, ',', 0) = '');
  check(getStrField2(str, ',', -1) = '');
  check(getStrField2(str, ',', -121212) = '');
end;

initialization
    TestFramework.RegisterTest(TacStrUtilsTestCases.Suite);

end.
