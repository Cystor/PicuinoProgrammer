program PicuinoProgrammer;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, LazSerialPort, u_main, unit_pp, u_thread_cmd, u_init
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='Picuino_Programmer';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TF_Main, F_Main);
  Application.CreateForm(TF_Init, F_Init);
  Application.Run;
end.

