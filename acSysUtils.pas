unit acSysUtils;

interface

uses
  Classes, Printers, SysUtils, Windows, ShlObj, ActiveX, Forms, dialogs;

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
function CreateProcessSimple(cmd: string; wait: boolean = false): boolean;
function getWindowsProgramDataDir(subdir: string = ''): string;
procedure bloqueiaTecladoMouse;
procedure desbloqueiaTecladoMouse;
function FuncAvail(dllName, funcName: string; var p: pointer): boolean;
function GetWindowsTempFolder: string;
function isCtrlDown: boolean;

implementation

const
  CSIDL_COMMON_APPDATA = $0023;

procedure getPrinterList(PList: TStrings);
var
  prnindex: Integer;
begin
  PList.AddStrings(Printer.Printers);
  Printer.PrinterIndex := -1;
  prnindex := Printer.PrinterIndex;
  PList.Move(prnindex,0);
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
  if functionAvailable('USER32.DLL', 'BlockInput', @BlockInput) then
    BlockInput(true) ;
end;

procedure unblockInput;
var
  BlockInput : function(Block: BOOL): BOOL; stdcall;
begin
  if functionAvailable('USER32.DLL', 'BlockInput', @BlockInput) then
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

function CreateProcessSimple(cmd: string; wait: boolean = false): boolean;
var
  SUInfo: TStartupInfo;
  ProcInfo: TProcessInformation;
begin
  FillChar(SUInfo, SizeOf(SUInfo), #0);
  SUInfo.cb      := SizeOf(SUInfo);
  SUInfo.dwFlags := STARTF_USESHOWWINDOW;
  SUInfo.wShowWindow := SW_HIDE;

  Result := CreateProcess(nil,
                          PChar(cmd),
                          nil,
                          nil,
                          false,
                          CREATE_NEW_CONSOLE or
                          NORMAL_PRIORITY_CLASS,
                          nil,
                          nil,
                          SUInfo,
                          ProcInfo);

  if (Result and wait) then
  begin
    WaitForSingleObject(ProcInfo.hProcess, INFINITE);

    CloseHandle(ProcInfo.hProcess);
    CloseHandle(ProcInfo.hThread);
  end;
end;

function getWindowsProgramDataDir(subdir: string = ''): string;
begin
  result := getSpecialFolderLocation(Application.Handle, CSIDL_COMMON_APPDATA) + '\' + subdir;
  if not DirectoryExists(result) then
    CreateDir(result);
end;

procedure bloqueiaTecladoMouse;
var
  BlockInput : function(Block: BOOL): BOOL; stdcall;
begin
  if FuncAvail('USER32.DLL', 'BlockInput', @BlockInput) then
    BlockInput(true) ;
end;


procedure desbloqueiaTecladoMouse;
var
  BlockInput : function(Block: BOOL): BOOL; stdcall;
begin
  if FuncAvail('USER32.DLL', 'BlockInput', @BlockInput) then
    BlockInput(false) ;
end;

function FuncAvail(dllName, funcName: string; var p: pointer): boolean;
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

function GetWindowsTempFolder: string;
var
  tempFolder: array[0..MAX_PATH] of Char;
begin
  GetTempPath(MAX_PATH, @tempFolder);
  result := StrPas(tempFolder);
end;

function isCtrlDown: boolean;
var
  s: TKeyboardState;
begin
  GetKeyboardState(s);
  result := ((s[VK_CONTROL] and 128)<>0);
end;


end.
