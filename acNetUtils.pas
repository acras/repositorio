unit acNetUtils;

interface

uses idHTTP, SysUtils;

function getRemoteXmlContent(pUrl: string): String;

implementation

function getRemoteXmlContent(pUrl: string): String;
var
  xmlContent: string;
  http: TIDHTTP;
begin
  http := TIdHTTP.Create(nil);
  try
    http.HandleRedirects := true;
    try
      result := http.Get(pUrl);
    except
      result := '';
    end;
  finally
    FreeAndNil(http);
  end;
end;


end.
