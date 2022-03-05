{
*****************************************************************
The MIT License (MIT)
Copyright (c) 2015 Jaromir Sukuba
*****************************************************************

Permission is hereby granted, free of charge, to any person
obtaining a copy of this software and associated documentation
files (the "Software"), to deal in the Software without
restriction, including without limitation the rights to use,
copy, modify, merge, publish, distribute, sublicense, and/or
sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following
conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY
OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT
LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

*****************************************************************
}
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

