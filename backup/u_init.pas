unit u_init;

{$mode objfpc}{$H+}

interface

uses
  Windows,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  { TF_Init }

  TF_Init = class(TForm)
    Image1: TImage;
    Tmr1: TTimer;
    procedure FormActivate(Sender: TObject);
    procedure Tmr1Timer(Sender: TObject);
  private

  public

  end;

var
  F_Init: TF_Init;

implementation

{$R *.lfm}

{ TF_Init }

procedure TF_Init.Tmr1Timer(Sender: TObject);
begin
  TTimer(Sender).Enabled:= False;
  Self.Close;
end;

procedure TF_Init.FormActivate(Sender: TObject);
begin
  //fundo transparente
  {$IFDEF MSWINDOWS}
    //https://forum.lazarus.freepascal.org/index.php?topic=8488.0
    //https://bugs.freepascal.org/view.php?id=1804
    //by xpete
    SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_LAYERED);
    SetLayeredWindowAttributes(Handle,clWhite,255,LWA_COLORKEY);
  {$ENDIF}

  Tmr1.Enabled:= True;
end;

end.

