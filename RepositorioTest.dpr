program RepositorioTest;

uses
  Forms,
  TestFrameWork,
  GUITestRunner,
  acStrUtilsTestCases in 'testCases\acStrUtilsTestCases.pas',
  acStrUtils in 'acStrUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  GUITestRunner.RunRegisteredTests;
end.
