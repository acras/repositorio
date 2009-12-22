{== WinUtils ======================================================}
{! <summary>
  Collects some useful routines that need Windows API
  functions. These functions can be used with Win16 or Win32
  programs, with the exception of VerifyLength, which is
  Win32 only.</summary>
 <author>Dr. Peter Below</author>
 <history>
 <para>Version 1.0 created 1997-05-20</para>
 <para>Version 1.1: modified for Win16/Win32 support ( 20.05.97 ) </para>
 <para>Version 1.2: added IsDesignerInstance  </para>
 <para>Version 1.3: added FindServerApp, fixed bugs in FindOleServername </para>
 <para>Version 1.4: modified for use of MyRegistry unit instead of D32
   Registry unit. This allows us to set key access to KEY_READ to fix a
   problem with the Find* functions on Windows NT, which does not allow
   KEY_ALL_ACCESS for keys under HKEY_CLASSES_ROOT for non-admin processes.</para>
 <para>Version 1.5: Rewrote CreateTempFilename completely </para>
 <para>Version 1.6: Added ProcessPaintRequests</para>
 <para>Version 1.7: Added TempFilename</para>
 <para>Version 1.8: Added functions for some missing macros from windows.h</para>
 <para>Version 1.9: Added EmptyMouseQueue, EmptyKeyQueue, DiscardPendingInput</para>
 <para>Version 1.10: Added IsKeyOrMouseMessage</para>
 <para>Version 1.11: Modified for Delphi 2009 (2008-10-05).</para>
 <para>Last modified       2008-10-05</para>
 </history>
 <remarks>Depends on the correct version of the JEDI.INC file from the
 JEDI code library for symbols defines based on compiler version! </remarks>}
{======================================================================}
{$BOOLEVAL OFF}{Unit depends on shortcut boolean evaluation}
unit WinUtils;
interface
{$I JEDI.INC}
uses{$IFDEF COMPILER2_UP}Messages, Windows
{$ELSE}Wintypes, WinProcs
{$ENDIF}, Classes;

const
  WM_XBUTTONDOWN  = $020B;
  WM_XBUTTONUP    = $020C;
  WM_XBUTTONDBLCLK= $020D;

procedure FetchClipboardData(S: TStream; ctype: Integer);

procedure CreateTempFilename(var fname: string);

{$IFDEF COMPILER2_UP}
procedure VerifyLength(var S: string);
{$ENDIF}

function FindOLEServername(const OleObjName: string): string;

function FindDDECommands(const doctype, command: string;
  var serverapp, topic, service, ddecommand: string): Boolean;

function FindServerApp(const docextension: string): string;
function IsDesignerInstance(hw: HWND): Boolean;

{$IFNDEF COMPILER2_UP}
function WinExecAndWait(const Cmd: string; cmdShow: Cardinal):
  Cardinal;
{$ELSE}
{: Execute and external program.
Deprecated, use the WinExecAndWait32 from the Win32Utils unit instead,
it has better waiting behaviour. }
function WinExecAndWait(FileName: string; Visibility: integer):
  Cardinal; deprecated;
{$ENDIF}

{: Processes any paint messages in the message queue. }
procedure ProcessPaintrequests;
procedure ProcessPaintrequest; // alias used due to typo in some units

{: Construct a full pathname for a temporary file.
  @param Filepart is the name to use for the filename part, can be empty.
  @returns A full pathname for the file located in the users TEMP
    directory. If Filepart is empty the routine creates a unique name
    based on a GUID.
  @desc The routine will not create a testfile, so there is no guarantee
  that the file can be created. If Filepart is passed in <> '' the file
  may also already exist. }
function TempFilename(const Filepart: string = ''): string;

{ Locale-related macros missing from windows.pas }
function MakeLangID(p, s: Word): Word;
function PrimaryLangID(lgid: Word): Word;
function SublangID(lgID: Word): Word;
function MakeLCID(lgID: Word; srtid: Word): DWORD;
function MakeSortLCID(lgid, srtid, ver: Word): DWORD;
function LangIDFromLCID(lcid: DWORD): Word;
function SortIDFromLCID(lcid: DWORD): Word;
function SortVersionFromLCID(lcid: DWORD): Word;

{ Input queue manipulation }
procedure EmptyKeyQueue;
procedure EmptyMouseQueue;
procedure DiscardPendingInput;

function IsKeyOrMouseMessage(const Msg: TMsg): Boolean;


implementation

uses SysUtils, Clipbrd,
{$IFNDEF COMPILER5_UP}MyRegistr, {$ELSE}Registry{$ENDIF},
  Utils, ShellAPI, ActiveX, Charsets, UnicodeHelpersU;

const
{$IFDEF GERMAN}
  errCreateTempFailed =
    'CreateTempFilename: Konnte temporäre Datei %s nicht erzeugen' +
    ', das Verzeichnis oder Laufwerk ist eventuell schreibgeschützt!';

{$ELSE}
  errCreateTempFailed =
    'CreateTempFilename: Unable to create temporary file %s' +
    ', the directory or drive may be write-protected!';
{$ENDIF}

{************************************************************
 * Procedure FetchClipboardData
 *
 * Parameters:
 *      S: an open stream to write the data to. The stream position is
 *         left at the end of the data and writing starts at
 *         whatever position the stream may be at!
 *  ctype: a clipboard format
 * Call method:
 *  static
 * Description:
 *  Opens the clipboard, obtains a memory handle for its contents
 *  and copies that into the memory stream. Note that
 *   - writing starts at the current stream position, not
 *     necessarily the start of stream,
 *   - the number of bytes written is the size of the memory block
 *     received from the clipboard, which will always be a multiple
 *     of 32 bytes and in most cases larger than the actual data!
 *     We have no way to find the end of data since we have no idea
 *     what the data is.
 * Error Conditions:
 *  Nothing will be written if the clipboard does not hold
 *  data for the requested format or if access to it failed.
 *  There is no error message, however.
 *
 *Created: 02/22/97 17:32:16 by P. Below
 ************************************************************}

procedure FetchClipboardData(S: TStream; ctype: Integer);
var
  handle: THandle;
  p: PChar;
begin
  with Clipboard do begin
    Open;
    try
      handle := GetAsHandle(ctype);
      if handle <> 0 then begin
        p := GlobalLock(handle);
        if p <> nil then try
          S.Write(p^, GlobalSize(handle))
        finally
          GlobalUnlock(handle);
        end;
      end; { If }
    finally
      Close
    end;
  end; { With }
end; { FetchClipboardData }

{-- CreateTempFilename ------------------------------------------------}
{: Create a temporary filename.
@Param fname returns the filename.
@Desc The filename returned is a full pathname, typically based on the
  Windows TEMP directory, with a hex filename and extension .TMP. If
  there is not TEMP directory or it is write-protected the Windows
  directory is used.
}{ Created 16.10.2001 by P. Below, modified version of prior implementation.
-----------------------------------------------------------------------}

procedure CreateTempFilename(var fname: string);

  function FileCanBeCreated(const fname: string): Boolean;
  var
    h: THandle;
  begin
    if not FileExists(fname) then begin
      h := FileCreate(fname);
      if Integer(h) < 0 then begin
        raise Exception.CreateFmt(errCreateTempFailed, [fname]);
      end { If }
      else begin
        FileClose(h);
        SysUtils.DeleteFile(fname);
        Result := True;
      end;
    end { If }
    else
      Result := False;
  end; { FileCanBeCreated }
var
  Buf: array[0..MAX_PATH] of Char;
{$IFDEF COMPILER2_UP}
  n: int64; { Changed from dword to int64 2005-06-09 to fix range
                 check errors on WTS2000 when calling IntToHex}
{$ELSE}
  n: dword;
{$ENDIF}
  S: string;
begin { CreateTempFilename }
  Buf[0] := #0;
  GetTempPath(MAX_PATH, Buf);
  if StrLen(buf) = 0 then
    GetWindowsDirectory(Buf, Sizeof(Buf));
  fname := buf;
  AssertTrailingBackslash(fname);
  n := GetTickCount;
  repeat
    S := fname + IntToHex(n, 8) + '.TMP';
    Inc(n);
  until FileCanBeCreated(S);
  fname := S;
end; { CreateTempFilename }

{$IFDEF WIN32}
{This function fixes some problems with strings that are returned by
 OLE automation servers. For some reason they may come back with a
 length that includes the terminating #0 character, which fouls up
 a number of things when you try to use them in string expressions
 and display the result or assign it to a controls caption or text
 property. Display stops at the (now embedded) #0 character... }

procedure VerifyLength(var S: string);
begin
  if Length(S) > 0 then
    SetLength(S, StrLen(PChar(@S[1])));
end;
{$ENDIF}

{************************************************************
 * function FindOLEServername
 *
 * Parameters:
 *  OleObjName: name of the Ole object to find server for
 * Returns:
 *  pathname of the server, as registered. If there is
 *  more than one server registered the function will return
 *  the 32-bit server in preference to the 16 bit server.
 *  The returned string will be empty if an error occured.
 *  If the path contains blanks the name will be quoted.
 * Call method:
 *  static
 * Description:
 *  Looks thru the registry for the passed OLE object and
 *  returns its server path, without any parameters.
 * Error Conditions:
 *  none
 *
 *Created: 14.03.97 19:08:27 by P. Below
 ************************************************************}

function FindOLEServername(const OleObjName: string): string;
const
{$IFDEF WIN32}
  ServerKeys: array[0..3] of string =
  ('LocalServer32', 'LocalServer',
    'InProcServer32', 'InProcServer');
{$ELSE}
  ServerKeys: array[0..3] of string =
  ('LocalServer', 'LocalServer32',
    'InProcServer', 'InProcServer32');
{$ENDIF}

var
  reg: TRegistry;
  temp: string;
  i: Integer;
begin
  Result := EmptyStr;
  reg := TRegistry.Create;
  try
    with reg do begin
      RootKey := HKEY_CLASSES_ROOT;
{$IFDEF WIN32}
      Access := KEY_READ;
{$ENDIF}
      if OpenKey(OleObjName + '\CLSID', False) then begin
        temp := readString(EmptyStr);
        CloseKey;
        if OpenKey('CLSID\' + temp, false) then begin
          for i := 0 to High(ServerKeys) do
            if KeyExists(ServerKeys[i]) then begin
              if OpenKey(ServerKeys[i], False) then
                temp := ReadString(EmptyStr);
              Break;
            end; { If }
          CloseKey;
          if temp[1] <> '{' then
            if temp[1] = '"' then
              Result := Copy(temp, 1, IScan('"', temp, 2))
            else begin
              i := Scan(' ', temp);
              if i > 0 then
                Delete(temp, i, MaxInt);
              i := RScan('/', temp);
              while i > 0 do begin
                Delete(temp, i, MaxInt);
                i := RScan('/', temp);
              end;
              Result := temp;
            end; { Else }
        end; { If }
      end; { If }
    end; { With }
  finally
    reg.Free;
  end;
end; { FindOLEServername }

{+------------------------------------------------------------
 | function FindDDECommands
 |
 | Parameters:
 |  doctype: extension of document to find server for, includes dot.
 |  command: type of command to find ddemacro for. This is
 |           a subkey name under shell, e.g. open, print, new
 |  serverapp: returns pathname of server exe
 |  topic    : returns DDE topic to use
 |  service  : returns DDE service to connect to
 |  ddecommand: returns dde macro string
 | Returns:
 |  true if info found, false otherwise.
 | Call method:
 |  static
 | Description:
 |  Looks thru the registry under HKEY_CLASSES_ROOT for a shell
 |  entry for the requested extension.
 | Error Conditions:
 |
 |
 |Created: 03.06.97 16:44:56 by P. Below
 +------------------------------------------------------------}

function FindDDECommands(const doctype, command: string;
  var serverapp, topic, service, ddecommand: string): Boolean;
var
  reg: TRegistry;
  temp: string;
begin
  Result := False;
  try
    reg := TRegistry.Create;
    try
      with reg do begin
        RootKey := HKEY_CLASSES_ROOT;
{$IFDEF WIN32}
        Access := KEY_READ;
{$ENDIF}
        if OpenKey(doctype, False) then begin
          temp := readString(EmptyStr);
          CloseKey;
          if OpenKey(temp + '\shell\' + command, false) then begin
            serverapp := ReadString('command');
            ddecommand := ReadString('ddeexec');
            topic := ReadString('ddeexec\Topic');
            service := ReadString('ddeexec\Application');
            Result :=
              (Length(serverapp) > 0) and
              (Length(ddecommand) > 0) and
              (Length(topic) > 0) and
              (Length(service) > 0);
            CloseKey;
          end; { If }
        end; { If }
      end; { With }
    finally
      reg.Free;
    end;
  except
  end;
end; { FindDDECommands }

function FindServerApp(const docextension: string): string;
var
  reg: TRegistry;
  temp: string;
  i: Integer;
begin
  Result := EmptyStr;
  try
    reg := TRegistry.Create;
    try
      with reg do begin
        RootKey := HKEY_CLASSES_ROOT;
{$IFDEF WIN32}
        Access := KEY_READ;
{$ENDIF}
        if OpenKey(docextension, False) then begin
          temp := readString(EmptyStr);
          CloseKey;
          if OpenKey(temp + '\shell\open\command', false) then begin
            temp := ReadString(EmptyStr);
            i := Scan('%', temp);
            if i > 0 then
              Delete(temp, i, MaxInt);
            Result := Trim(temp);
            CloseKey;
          end { If }
        end; { If }
      end; { With }
    finally
      reg.Free;
    end;
  except
  end;
end;

{+------------------------------------------------------------
 | function IsDesignerInstance
 |
 | Parameters:
 |  hw: Window handle of window to test.
 | Returns:
 |  True if the window is owned by the same process that owns
 |  the Delphi designer window, false otherwise
 | Call method:
 |  static
 | Description:
 |  This function is an attempt to keep design time windows
 |  out of the picture. Otherwise you would not be able to
 |  run an application that uses this Unit from the Delphi
 |  IDE without first closing the design form.
 | Error Conditions:
 |  none
 |
 |Created: 02.06.97 16:40:13 by P. Below
 +------------------------------------------------------------}

function IsDesignerInstance(hw: HWND): Boolean;
var
  designer: HWND;
{$IFDEF COMPILER2_UP}
  id1, id2: DWORD;
{$ENDIF}
begin
  designer := FindWindow('TAppBuilder', nil);
  Result := designer <> 0;
  if Result then begin
{$IFDEF COMPILER2_UP}
    GetWindowThreadProcessID(hw, @id1);
    GetWindowThreadProcessID(designer, @id2);
    Result := id1 = id2;
{$ELSE}
    Result := GetWindowTask(hw) =
      GetWindowTask(designer);
{$ENDIF}
  end; { If }
end; { IsDesignerInstance }

{$IFNDEF COMPILER2_UP}

function WinExecAndWait(const Cmd: string; cmdShow: Cardinal):
  Cardinal;
var
  pBuf: PChar;
  hMod: THandle;
  Msg: TMsg;
begin
  pBuf := StrAlloc(Length(Cmd) + 1);
  try
    StrPCopy(pBuf, Cmd);
    hMod := WinExec(pBuf, cmdShow);
    if hMod < 32 then
      Result := hMod
    else
      while GetModuleUsage(hMod) > 0 do begin
        while PeekMessage(Msg, 0, 0, 0, PM_REMOVE) do begin
          TranslateMessage(Msg);
          DispatchMessage(Msg);
        end; { While }
      end; { While }
    Result := 0;
  finally
    StrDispose(pBuf);
  end;
end; { WinExecAndWait }
{$ELSE}

function WinExecAndWait(FileName: string; Visibility: integer): DWORD;
var
  zAppName: array[0..512] of char;
  zCurDir: array[0..255] of char;
  WorkDir: string;
  StartupInfo: TStartupInfo;
  ProcessInfo: TProcessInformation;
begin
  StrPCopy(zAppName, FileName);
  GetDir(0, WorkDir);
  StrPCopy(zCurDir, WorkDir);
  FillChar(StartupInfo, Sizeof(StartupInfo), #0);
  StartupInfo.cb := Sizeof(StartupInfo);

  StartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  StartupInfo.wShowWindow := Visibility;
  if not CreateProcess(nil,
    zAppName, { pointer to command line string }
    nil, { pointer to process security attributes }
    nil, { pointer to thread security attributes }
    false, { handle inheritance flag }
    CREATE_NEW_CONSOLE or { creation flags }
    NORMAL_PRIORITY_CLASS,
    nil, { pointer to new environment block }
    nil, { pointer to current directory name }
    StartupInfo, { pointer to STARTUPINFO }
    ProcessInfo) { pointer to PROCESS_INF }
    then
    Result := $FFFFFFFF
  else begin
    WaitforSingleObject(ProcessInfo.hProcess, INFINITE);
    GetExitCodeProcess(ProcessInfo.hProcess, Result);
    CloseHandle(ProcessInfo.hProcess);
    CloseHandle(ProcessInfo.hThread);
  end;
end;
{$ENDIF}

procedure ProcessPaintrequests;
var
  msg: TMsg;
begin
  while PeekMessage(msg, 0, WM_PAINT, WM_PAINT, PM_REMOVE) do
    DispatchMessage(msg);
end;

procedure ProcessPaintrequest;
begin
  ProcessPaintRequests;
end;

function TempFilename(const Filepart: string = ''): string;
var
  Buf: array[0..MAX_PATH] of Char;

  function FilepartFromGUID: string;
  var
    GUID: TGUID;
    I: Integer;
  begin
    CoCreateGUID(GUID);
    Result := GUIDToString(GUID);
    for I := Length(Result) downto 1 do
      if not CharInSet(Result[I],Charsets.HexNumerals) then
        Delete(Result, I, 1);
  end;

begin
  buf[0] := #0;
  GetTempPath(Sizeof(buf), buf);
  if Filepart <> '' then
    Result := IncludeTrailingPathDelimiter(Buf) + Filepart
  else
    Result := IncludeTrailingPathDelimiter(Buf) + FilepartFromGUID;
end;

function MakeLangID(p, s: Word): Word;
begin
  Result := (s shl 10) or p
end;

function PrimaryLangID(lgid: Word): Word;
begin
  Result := lgID and $3FF;
end;

function SublangID(lgID: Word): Word;
begin
  Result := lgid shr 10;
end;

function MakeLCID(lgID: Word; srtid: Word): DWORD;
begin
  Result := MakeLong(lgid, srtid);
end;

function MakeSortLCID(lgid, srtid, ver: Word): DWORD;
begin
  Result := MakeLCID(lgID, srtID) or (Cardinal(ver) shl 20);
end;

function LangIDFromLCID(lcid: DWORD): Word;
begin
  Result := LoWord(lcid);
end;

function SortIDFromLCID(lcid: DWORD): Word;
begin
  Result := HiWord(lcid) and $0F;
end;

function SortVersionFromLCID(lcid: DWORD): Word;
begin
  Result := LoWord(lcid shr 20) and $0F;
end;

procedure EmptyKeyQueue;
var
  Msg: TMsg;
begin
  while PeekMessage(Msg, 0, WM_KEYFIRST, WM_KEYLAST,
    PM_REMOVE or PM_NOYIELD)
    do
    ;
end;

procedure EmptyMouseQueue;
var
  Msg: TMsg;
begin
  while PeekMessage(Msg, 0, WM_MOUSEFIRST, WM_MOUSELAST,
    PM_REMOVE or PM_NOYIELD)
    do
    ;
end;

procedure DiscardPendingInput;
begin
  EmptyMouseQueue;
  EmptyKeyQueue;
end;

function IsKeyOrMouseMessage(const Msg: TMsg): Boolean;
begin
  case Msg.message of
    WM_KEYFIRST..WM_KEYLAST, WM_MOUSEFIRST..WM_MOUSELAST,
    WM_NCMOUSEMOVE..WM_NCXBUTTONDBLCLK,
    WM_XBUTTONDOWN, WM_XBUTTONUP, WM_XBUTTONDBLCLK:
      Result := true
  else
    Result := false;
  end; {case}
end;

initialization
  Randomize;
end.
