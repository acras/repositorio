unit acInetUtils;

interface

uses
  Types, wINDOWS,WinInet;

function CheckConnection:Boolean;

implementation

function CheckConnection:Boolean;
var
  Flags: DWORD;
begin
  result := False;
  try
    if InternetGetConnectedState(@Flags, 0) then
    begin

      if ((Flags and INTERNET_CONNECTION_LAN) > 0) or
         ((Flags and INTERNET_CONNECTION_MODEM) > 0) or
         ((Flags and INTERNET_CONNECTION_PROXY) > 0) then
        Result := True
      else
        //MessageDlg('No internet connection : Document update interval was reached but unable to check for update', mtWarning, [mbOK], 0);;
    end;
  except
    begin
      Result := false;
      //ShowMessage('No internet connection : Document update interval was reached but unable to check for update');
    end;
  end;
end;

end.
 