unit acSysUtils;

interface

uses
  Classes, Printers, SysUtils;

procedure getPrinterList(PList: TStrings);
procedure deltree(dir: string);
procedure listdirectorycontents(basedir: string; s: TStrings);
procedure listdirectories(basedir: string; s: TStrings);

implementation

procedure getPrinterList(PList: TStrings);
var
  i: integer;
  device, driver, port: array[0..50] of Char;
  PrnHandle: THandle;
  Printer: TPrinter;
begin
  printer := TPrinter.Create;
  try
  Printer.GetPrinter(Device, Driver, Port, PrnHandle);
  Plist.Add(device+' on '+port);
  Plist.AddStrings(Printer.Printers);
  for I := Plist.Count -1 downto 1 do
  begin
    If Plist[0]=Plist[i] then
    Plist.Delete(i);
  end;
  finally
    freeAndNil(printer);
  end;
end;

procedure deltree(dir: string);
var
  s: TStringList;
  i: longint;
begin
  if (dir[2]=':') and (dir[3]='\') and (length(dir)=3) then
    exit;
  s := TStringList.create;
  try
    listdirectorycontents(dir, s);
    // first we delete all the directory files
    for i := 0 to s.count - 1 do
    begin
      DeleteFile(PChar(s[i]));
    end;
    s.clear;
    listdirectories(dir, s);
    // then we can delete all the directories because are empty
    for i := s.count - 1 downto 0 do
    begin
      removedir(s[i]);
    end;
    // finally we remove the passed dir
    removedir(dir);
  finally
    s.Free;
  end;
end;

procedure listdirectorycontents(basedir: string; s: TStrings);
var
    sr: TSearchRec;
begin
  if findfirst(basedir + '/*', faAnyFile, sr) = 0 then
  begin
    if ((faDirectory and sr.Attr) = faDirectory) and (sr.Name <> '.') and (sr.name <> '..') then
    begin
      listdirectorycontents(basedir + '/' + sr.name, s);
    end
    else
      if (sr.Name <> '.') and (sr.name <> '..') then s.Add(basedir + '/' + sr.Name);
    while findnext(sr) = 0 do
    begin
      if ((faDirectory and sr.Attr) = faDirectory) and (sr.Name <> '.') and (sr.name <> '..') then
      begin
        listdirectorycontents(basedir + '/' + sr.name, s);
      end
      else
        if (sr.Name <> '.') and (sr.name <> '..') then s.Add(basedir + '/' + sr.Name);
    end;
    findclose(sr);
  end;
end;

procedure listdirectories(basedir: string; s: TStrings);
var
  sr: TSearchRec;
begin
  if findfirst(basedir + '/*', faAnyFile, sr) = 0 then
  begin
    if ((faDirectory and sr.Attr) = faDirectory) and (sr.Name <> '.') and (sr.name <> '..') then
    begin
      s.add(basedir + '/' + sr.name);
      listdirectories(basedir + '/' + sr.name, s);
    end;
    while findnext(sr) = 0 do
    begin
      if ((faDirectory and sr.Attr) = faDirectory) and (sr.Name <>'.') and (sr.name <> '..') then
      begin
        s.add(basedir + '/' + sr.name);
        listdirectories(basedir + '/' + sr.name, s);
      end;
    end;
    findclose(sr);
  end;
end;


end.
