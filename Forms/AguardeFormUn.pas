unit AguardeFormUn;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TAguardeForm = class(TForm)
    lblAviso: TLabel;
    ProgressBar: TProgressBar;
  private
    Ftotal: integer;
    Fcurrent: integer;
    procedure Settotal(const Value: integer);
    procedure Setcurrent(const Value: integer);
  private
    { Private declarations }
  public
    { Public declarations }
    property total: integer read Ftotal write Settotal;
    property current: integer read Fcurrent write Setcurrent;
    procedure mostrar(aviso: TCaption; height: integer=0;
      width:integer = 0; mostrarProgress: boolean = false);
    procedure esconder;
  end;

var
  AguardeForm: TAguardeForm;

implementation

{$R *.dfm}

procedure TAguardeForm.esconder;
begin
  screen.Cursor := crArrow;
  Hide;
end;

procedure TAguardeForm.mostrar(aviso: TCaption; height: integer = 0;
  width:integer = 0; mostrarProgress: boolean = false);
begin
  self.caption := 'Aguarde...';
  Screen.Cursor := crHourGlass;
  if height<>0 then
    self.Height := height
  else
    self.height := 74;
  if width<>0 then
    self.Width := width;
  self.lblAviso.Caption := aviso;

  ProgressBar.visible := mostrarProgress;

  AguardeForm.show;
  aguardeForm.Repaint;
end;

procedure TAguardeForm.Setcurrent(const Value: integer);
begin
  Fcurrent := Value;
  ProgressBar.Position := value;
  if total <> 0 then
    self.caption := 'Aguarde... ('+ formatFloat('0.00%',(value/total)*100) +')';
end;

procedure TAguardeForm.Settotal(const Value: integer);
begin
  Ftotal := Value;
  ProgressBar.Max := value;
end;

end.
