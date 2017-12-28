unit acNetUtils;

interface

uses idHTTP, SysUtils, System.Classes;

function getRemoteXmlContent(const pUrl: string; http: TIdHTTP;var erro: string): String;
function getHTTPInstance: TidHTTP;

implementation

function getHTTPInstance: TidHTTP;
var
  http: TIdHTTP;
begin
  http := TIdHTTP.Create(nil);
  http.ProtocolVersion := pv1_1;
  http.HTTPOptions := http.HTTPOptions + [hoKeepOrigProtocol];
  http.Request.Connection := 'keep-alive';
  result := http;
end;

function getRemoteXmlContent(const pUrl: string; http: TIdHTTP;var erro: string): String;
var
  criouHTTP: boolean;
  retornoStream: TStringStream;
begin
  criouHTTP := False;
  Result := EmptyStr;
  erro := EmptyStr;
  retornoStream := TStringStream.Create('', TEncoding.UTF8);
  try
    if http = nil then
    begin
      criouHTTP := true;
      http := getHTTPINstance;
    end;

    try
      http.ConnectTimeout := 30000;
      http.ReadTimeOut := 30000;
      http.Get(pUrl, retornoStream);
    except
      on E: EIdHTTPProtocolException do
        erro := E.ErrorMessage;
    end;

    Result := retornoStream.DataString;
  finally
    if criouHTTP and (http <> nil) then
      FreeAndNil(http);
    FreeAndNil(retornoStream);
  end;
end;


end.
