unit acSysUtils;

interface

uses
  Classes, Printers, SysUtils, Windows, ShlObj, ActiveX, Forms;

procedure getPrinterList(PList: TStrings);
procedure deltree(dir: string);
procedure listdirectorycontents(basedir: string; s: TStrings);
procedure listdirectories(basedir: string; s: TStrings);
function GetSpecialFolderLocation(HWnd:HWnd; Index:integer):string;
function GetTempDir: string;
function getWindowsTempFileName(prefix: string): string;
function getWindowsTempPath: string;
procedure blockInput;
procedure unblockInput;
function functionAvailable(dllName, funcName: string; var p: pointer): boolean;

implementation

const
  CSIDL_COMMON_APPDATA = $0023;

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
    //findclose(sr);
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
    //findclose(sr);
  end;
end;

function GetSpecialFolderLocation(HWnd:HWnd; Index:integer):string;
var
  I:PItemIDList;
  FIDesktopFolder:IShellFolder;
  StrRet:TStrRet;
  P:PChar;
  M:IMalloc;
begin
  Result := '';
  SHGetSpecialFolderLocation(HWnd, Index, I);
  if I <> nil then begin
    SetLength(Result, MAX_PATH);
    if SHGetPathFromIdList(I, PChar(Result)) then
      SetLength(Result,Strlen(PChar(Result)))
    else
      Result := '';
    SHGetMalloc(M);
    if Result='' then begin
      SHGetDesktopFolder(FIDesktopFolder);
      FIDesktopFolder.GetDisplayNameOf(I, SHGDN_FORPARSING, StrRet);
      case StrRet.uType of
        STRRET_CSTR:
          SetString(Result, StrRet.cStr, lStrLen(StrRet.cStr));
        STRRET_OFFSET:
          begin
            P := @I.mkid.abID[StrRet.uOffset - SizeOf(I.mkid.cb)];
            SetString(Result, P, I.mkid.cb - StrRet.uOffset);
          end;
        STRRET_WSTR:
          begin
            Result := WideCharToString(StrRet.pOleStr);
            if M.DidAlloc(StrRet.pOleStr)>0 then
              M.Free(StrRet.pOleStr)
          end;
      end;
    end;
    M.Free(I);
  end;
end;

function GetTempDir: string;
begin
  result := getSpecialFolderLocation(
    Application.Handle, CSIDL_COMMON_APPDATA) + '\ACRASSGL\';
end;

function getWindowsTempPath: string;
var
  lng: DWORD;
  thePath: string;
begin
  SetLength(thePath, MAX_PATH);
  lng := GetTempPath(MAX_PATH, PChar(thePath));
  SetLength(thePath, lng);
  result := thePath;
end;

function getWindowsTempFileName(prefix: string): string;
var
  lng: DWORD;
  pc: PChar;
  theFileNameWithPath: array[0..MAX_PATH] of char;
begin
  pc := @theFileNameWithPath[0];
  lng := GetTempFileName(PChar(getWindowsTempPath), PChar(prefix), 0, pc);
  Win32Check(lng <> 0);
  SetString(result, pc, strLen(pc));
  result := result;
end;

procedure blockInput;
var
  BlockInput : function(Block: BOOL): BOOL; stdcall;
begin
  if FuncAvail('USER32.DLL', 'BlockInput', @BlockInput) then
    BlockInput(true) ;
end;

procedure unblockInput;
var
  BlockInput : function(Block: BOOL): BOOL; stdcall;
begin
  if FuncAvail('USER32.DLL', 'BlockInput', @BlockInput) then
    BlockInput(false) ;
end;

function functionAvailable(dllName, funcName: string; var p: pointer): boolean;
var
  lib: THandle;
begin
  result := false;
  p := nil;
  if LoadLibrary(PChar(dllName)) = 0 then exit;
  lib := GetModuleHandle(PChar(dllName)) ;
  if lib <> 0 then
  begin
    p := GetProcAddress(lib, PChar(funcName)) ;
    if p <> nil then Result := true;
  end;
end;




end.
