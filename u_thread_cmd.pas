unit u_thread_cmd;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process;

{ TCMDThread }

type
  TCMDThread = class(TThread)
  private
    FFilename,
    FSerial,
    FDevice,
    FHexFile,
    FSleep1,
    FSleep2,
    FVerbose1,
    FVerbose2 : String;

    procedure OnCMDTerminate(Sender: TObject);
    procedure OnDone();
  protected
    procedure Execute(); override;
  public
    FResultado: String;
    FOnDoneEvent: TNotifyEvent;
    constructor Create();
  end;

//chama processo cria thread e executa
function ExecuteCMD(Filename, Serial, Device, HexFile: String;
                    Sleep1: String = ''; Sleep2: String = '';
                    Verbose1: String = ''; Verbose2: String = '';
                    DoneEvent: TNotifyEvent = nil): Boolean;

implementation

//-------------------------------------------------
// PUBLIC - ExecuteCMD - Cria e inicializa processo
//-------------------------------------------------
function ExecuteCMD(Filename, Serial, Device, HexFile: String;
                    Sleep1: String = ''; Sleep2: String = '';
                    Verbose1: String = ''; Verbose2: String = '';
                    DoneEvent: TNotifyEvent = nil): Boolean;
var
  aThread: TCMDThread;
begin
  //proteções
  Result:= False;
  if (Filename = '') then
    Exit;
  if (Serial = '') then
    Exit;
  if (Device = '') then
    Exit;
  if (HexFile = '') then
    Exit;

  //cria objeto thread
  aThread:= TCMDThread.Create();

  //assimila variaveis
  aThread.FFilename:= Filename;
  aThread.FSerial:=   Serial;
  aThread.FDevice:=   Device;
  aThread.FHexFile:=  HexFile;

  //assimila variaveis opcionais
  aThread.FSleep1:=   Sleep1;
  aThread.FSleep2:=   Sleep2;
  aThread.FVerbose1:= Verbose1;
  aThread.FVerbose2:= Verbose2;

  //assimila evento externo
  aThread.FOnDoneEvent:= DoneEvent;

  //inicializa thread
  aThread.Resume;

  //todos processos ok
  Result:= True;
end;

//-------------------------------------------------
// Thread - Create
//-------------------------------------------------
constructor TCMDThread.Create();
begin
  //cria objeto thread
  inherited Create(True);

  //configura objeto criado
  Self.FreeOnTerminate:= True;
  Self.OnTerminate:= @OnCMDTerminate;
end;

//-------------------------------------------------
// Thread - Execute
//-------------------------------------------------
procedure TCMDThread.Execute();
var
  AStatus: Boolean;
begin
  //executa comando prompt
  AStatus:= RunCommand(FFilename, ['-c', FSerial, '-t', FDevice, FHexFile,
                       FSleep1, FSleep2, FVerbose1, FVerbose2],
                       FResultado, [poNoConsole]);

  //finaliza e libera da memória
  Self.Terminate;
end;

{
procedure TCMDThread.Execute();
var
  PlayProc: TProcess;
begin
  PlayProc := TProcess.create(nil);
  try
    PlayProc.executable := 'aplay';
    PlayProc.parameters.add(FFilename);
    PlayProc.Options := [poNoConsole];
    PlayProc.execute;
    while PlayProc.Running do begin
      if StopPlay or AppClosing then begin
        if StopPlay or FTerminateProcess then
          PlayProc.terminate(1);
        exit;
      end
      else
        sleep(1);
    end;
  finally
    PlayProc.free;
    if assigned(FOnPlayDone) and not AppClosing then
      synchronize(@Done);
  end;
end;
}

//-------------------------------------------------
// Thread - OnCMDTerminate
//-------------------------------------------------
procedure TCMDThread.OnCMDTerminate(Sender: TObject);
begin
  //finaliza e sincroniza com thread principal
  Synchronize(@OnDone);
end;

//-------------------------------------------------
// Thread - OnDone
//-------------------------------------------------
procedure TCMDThread.OnDone();
begin
  //quando acabar vai para o evento externo
  if (Assigned(FOnDoneEvent)) then
    FOnDoneEvent(Self);
end;

end.

