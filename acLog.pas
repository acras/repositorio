unit acLog;

interface

uses
  forms, classes;

type

  TacLog = class
    private
    FdirName: string;
    FfileName: string;
    protected
      procedure checkDir;
      function getDir: string;
      function getFile: string;
    public
      property dirName: string read FdirName write FdirName;
      property fileName: string read FfileName write FfileName;
      constructor create;
      procedure addLog(str: string);
  end;

implementation

uses SysUtils;

{ TacLog }

procedure TacLog.addLog(str: string);
var
  strL: TStringList;
begin
  checkDir;
  strL := TStringList.Create;
  try
    if FileExists(getDir + getFile) then
      strL.LoadFromFile(getDir + getFile);
    strL.Add(FormatDateTime('dd/mm/yyyy - hh:nn:ss: ', now) + str);
    strL.SaveToFile(getDir + getFile);
  finally
    FreeAndNil(strL);
  end;
end;

procedure TacLog.checkDir;
begin
  if not DirectoryExists(getDir) then
    CreateDir(getDir);
end;

constructor TacLog.create;
begin
  FdirName := '';
end;

function TacLog.getDir: string;
begin
  if FdirName='' then
    result := ExtractFilePath(Application.ExeName) + 'logs/'
  else
    result := FdirName;
end;

function TacLog.getFile: string;
begin
  if FfileName='' then
    result := FormatDateTime('ddmmyy',Date)+'.log'
  else
    result := FfileName;
end;

end.

