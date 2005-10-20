program Project1;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  CNAB240 in '..\..\CNAB240.pas',
  CNAB400 in '..\..\CNAB400.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
