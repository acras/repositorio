unit acSysUtils;

interface

uses
  Classes, Printers, SysUtils;

procedure getPrinterList(PList: TStrings);

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


end.
