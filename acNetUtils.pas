unit acNetUtils;

interface

uses idHTTP, SysUtils, System.Classes, IdSSL, IdIOHandler;

function getRemoteXmlContent(pUrl: string; http: TIdHTTP = nil): String; overload
function getRemoteXmlContent(const pUrl: string; http: TIdHTTP;var erro: string): String; overload
function getHTTPInstance: TidHTTP;
procedure downloadFile(url, filename: string);

implementation

function getHTTPInstance: TidHTTP;
var
  http: TIdHTTP;
begin
  http := TIdHTTP.Create(nil);
  http.HandleRedirects := True;
  http.ProtocolVersion := pv1_1;
  http.HTTPOptions := http.HTTPOptions + [hoKeepOrigProtocol];
  http.Request.Connection := 'keep-alive';
  result := http;
end;

function getRemoteXmlContent(pUrl: string; http: TIdHTTP = nil): String;
var
  criouHTTP: boolean;
begin
  criouHttp := false;
  if http = nil then
  begin
    criouHTTP := true;
    http := getHTTPINstance;
  end;

  try
    try
      result := http.Get(pUrl);
    except
      result := '';
    end;
  finally
    if criouHTTP and (http <> nil) then
      FreeAndNil(http);
  end;
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


procedure downloadFile(url, filename: string);

var

  http: TIdHTTP;

  ms: TMemoryStream;

begin

  http := getHTTPInstance;
  ms := TMemoryStream.Create;
  try
    http.Get(url, ms);
    ms.SaveToFile(filename);
  finally
    FreeAndNil(http);
  end;
end;


end.
