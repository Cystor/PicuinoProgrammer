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
unit u_main;

{$mode objfpc}{$H+}

interface

uses
  LazSerial,
  Synaser,
  LCLIntF,
  Process,
  u_thread_cmd,
  u_init,
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, ValEdit, Menus, XMLPropStorage, LCLType;

type

  { TF_Main }

  TF_Main = class(TForm)
    BtnWriteDev: TButton;
    BtnSearch: TButton;
    CBDev: TComboBox;
    CBCom: TComboBox;
    EditFilePath1: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LazSerial1: TLazSerial;
    LblStatus: TLabel;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MIArduinoIDE: TMenuItem;
    MenuItem6: TMenuItem;
    MIWriteDev: TMenuItem;
    MIShowIn: TMenuItem;
    MenuItem4: TMenuItem;
    MIShowOut: TMenuItem;
    MIVisual: TMenuItem;
    MIRun: TMenuItem;
    MIDebug0: TMenuItem;
    MenuItem5: TMenuItem;
    MIEnableTable: TMenuItem;
    MIEnableBar: TMenuItem;
    MIAlphaEffect: TMenuItem;
    MINewProg: TMenuItem;
    MenuItem2: TMenuItem;
    MITerms: TMenuItem;
    MIDebug4: TMenuItem;
    MIDebug3: TMenuItem;
    MIDebug2: TMenuItem;
    MIDebug1: TMenuItem;
    MIDebugLvl: TMenuItem;
    MIShowProgram: TMenuItem;
    MIShowCheck: TMenuItem;
    N5: TMenuItem;
    MIExternalLinks: TMenuItem;
    MILinkOriginal: TMenuItem;
    N4: TMenuItem;
    N3: TMenuItem;
    MIDelay500: TMenuItem;
    MIDelay: TMenuItem;
    MIDelayNone: TMenuItem;
    MIDelay1700: TMenuItem;
    MIDelay2000: TMenuItem;
    MIDelay2500: TMenuItem;
    MIDelay3000: TMenuItem;
    MIFile: TMenuItem;
    MIExit: TMenuItem;
    N2: TMenuItem;
    MISearch: TMenuItem;
    MIConfig: TMenuItem;
    MISerialDev: TMenuItem;
    MIPicDev: TMenuItem;
    MIMisc: TMenuItem;
    MIAbout: TMenuItem;
    OpenDialog1: TOpenDialog;
    PanelCenterLeft: TPanel;
    PanelCenter: TPanel;
    PanelDevice: TPanel;
    PanelFile: TPanel;
    PanelStatus: TPanel;
    Bar1: TProgressBar;
    Popup1: TPopupMenu;
    TaskDialog1: TTaskDialog;
    VLEDevice: TValueListEditor;
    XMLPropStorage1: TXMLPropStorage;
    procedure BtnSearchClick(Sender: TObject);
    procedure BtnWriteDevClick(Sender: TObject);
    procedure CBComDropDown(Sender: TObject);
    procedure CBDevChange(Sender: TObject);
    procedure CBDevDropDown(Sender: TObject);
    procedure CBDevSelect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure FormShow(Sender: TObject);
    procedure MIAboutClick(Sender: TObject);
    procedure MIAlphaEffectClick(Sender: TObject);
    procedure MIArduinoIDEClick(Sender: TObject);
    procedure MIEnableBarClick(Sender: TObject);
    procedure MIEnableTableClick(Sender: TObject);
    procedure MIExitClick(Sender: TObject);
    procedure MILinkOriginalClick(Sender: TObject);
    procedure MIPicDevClick(Sender: TObject);
    procedure MISearchClick(Sender: TObject);
    procedure MISerialDevClick(Sender: TObject);
    procedure MITermsClick(Sender: TObject);
    procedure MIWriteDevClick(Sender: TObject);
    procedure XMLPropStorage1RestoreProperties(Sender: TObject);
  private
    FFullcmd: String;
    function  GetExePath(): String;
    procedure WIN_GetSerialDev();
    procedure GetPicDev();
    procedure GetPicDevData();
    procedure OnAfterRunCmd(Sender: TObject);
  public

  end;

const
  WIN_HIGH_RUN_EXE = 'powershell -Command "Start-Process ' + #39 + '%s' + #39 + ' -Verb runAs"';
  PATH_PP_PROG     = 'MicroProgmeter\';
  PATH_INO_PROG    = PATH_PP_PROG;
  LBLSTATUS_0      = 'Waiting commands';
  LBLSTATUS_1      = 'Processing...';

var
  F_Main: TF_Main;

implementation

{$R *.lfm}

{ TF_Main }

//
// Funções - GetExePath
//

//obtem caminho do executável
function TF_Main.GetExePath(): String;
begin
  //obtem caminho do executável
  Result := ExtractFilePath(Application.ExeName);
  {$IFDEF DARWIN}
    Result := LeftStr(g_path, Pos(Application.ExeName + '.app', g_path)-1);
  {$ENDIF}
end;

//
// Funções - WIN_GetSerialDev
//

procedure TF_Main.WIN_GetSerialDev();
var
  aList: TStringList;
  i: Integer;
begin
  //configura em formato de lista
  aList:= TStringList.Create();
  aList.Delimiter:= ',';

  //obtem lista de dados
  aList.DelimitedText:= GetSerialPortnames();

  //limpa dados
  CBCom.Clear;

  //atualiza dados
  for i:= 0 to (aList.Count - 1) do
    CBCom.Items.Add( AList.Strings[i] );

  //seleciona o primeiro
  if (aList.Count <> 0) then
   CBCom.ItemIndex:= 0;

  //libera da memória
  FreeAndNil(aList);
end;

//
// Funções - GetPicDev
//

procedure TF_Main.GetPicDev();
var
  Apath, aLine: String;
  aFile: TextFile;
begin
  //obtem caminho do executável
  Apath := GetExePath();
  Apath:= Apath + PATH_PP_PROG + 'pp3_devices.dat';

  //segurança
  if (FileExists(Apath) = False) then
    Exit;

  //limpa dados
  CBDev.Clear;

  //obtem arquivo banco de dados
  AssignFile(aFile, Apath);
  Reset(aFile);

  //processa todo o arquivo
  while Not Eof(aFile) do
  begin
    //obtem a linha
    ReadLn(aFile, aLine);

    //pega o nome
    Apath:= Copy(aLine, 1, Pos(' ', aLine) - 1);

    //ignora comentário
    if (Pos('#', aLine) <> 0) then
      Continue;

    //ignora string vazia
    if (aLine = '') then
      Continue;

    //acrescenta item válido
    CBDev.Items.Add(UpperCase(APath));
  end;

  //seleciona primeiro item
  if (CBDev.Items.Count <> 0) then
  begin
   CBDev.ItemIndex:= 0;
   GetPicDevData();
  end;

  //fecha arquivo
  CloseFile(aFile);
end;

//
// Funções - GetPicDevData
//

procedure TF_Main.GetPicDevData();
var
  i    : Integer;
  Apath,
  aLine: String;
  aList: TStringList;
  aFile: TextFile;
begin
  //nenhum dispositivo selecionado
  if (CBDev.Text = '') then
  begin
    VLEDevice.Clear;
    Exit;
  end;

  //obtem caminho do executável
  Apath := ExtractFilePath(Application.ExeName);
  {$IFDEF DARWIN}
    Apath := LeftStr(g_path, Pos(Application.ExeName + '.app', g_path)-1);
  {$ENDIF}
  Apath:= Apath + PATH_PP_PROG + 'pp3_devices.dat';

  //segurança
  if (FileExists(Apath) = False) then
    Exit;

  //obtem arquivo banco de dados
  AssignFile(aFile, Apath);
  Reset(aFile);

  //processa todo o arquivo
  while Not Eof(aFile) do
  begin
    //obtem a linha
    ReadLn(aFile, aLine);

    //pega o nome dispositivo
    Apath:= Copy(aLine, 1, Pos(' ', aLine) - 1);

    //ignora comentário
    if (Pos('#', aLine) <> 0) then
      Continue;

    //ignora string vazia
    if (aLine = '') then
      Continue;

    //caixa alta
    APath:= UpperCase(APath);

    //ignora outro item
    if (APath <> CBDev.Text) then
      Continue;

    //fecha arquivo
    CloseFile(aFile);

    //cria objeto lista
    aList:= TStringList.Create();

    //pega os dados
    aList.Delimiter:= ' ';
    aList.DelimitedText:= aLine;

    //preenche nomes da tabela
    VLEDevice.Clear;
    VLEDevice.InsertRow('Device name', 'PIC', True);
    VLEDevice.InsertRow('Flash size',  '',    True);
    VLEDevice.InsertRow('Page size',   '',    True);
    VLEDevice.InsertRow('Device ID',   '0x',  True);
    VLEDevice.InsertRow('Mask',        '0x',  True);
    VLEDevice.InsertRow('Family type', '',    True);

    //preenche tabela de dados
    for i:= 0 to (aList.Count - 1) do
      VLEDevice.Strings.Values[VLEDevice.Strings.Names[i]]:= VLEDevice.Strings.Values[VLEDevice.Strings.Names[i]] + UpperCase(aList.Strings[i]);

    //seleciona a primeira linha
    VLEDevice.Row:=0;

    //limpa objeto
    FreeAndNil(aList);

    //sair da função
    Exit;
  end;

  //fecha arquivo
  CloseFile(aFile);
end;

//
// Funções - OnAfterRunCmd
//

//executa após processo de execução de comando prompt pp.exe
procedure TF_Main.OnAfterRunCmd(Sender: TObject);
var
  Resultado: String;
begin
  //proteções
  Resultado:= '';
  if (Sender <> nil) then
  begin
    if (Sender is TCMDThread) then
       Resultado:= TCMDThread(Sender).FResultado;
  end;

  //limpa memo
  Memo1.Clear;

  //mostra o comando do processo
  if (MIShowIn.Checked = True) then
  begin
    Memo1.Lines.Add('::: COMMAND :::');
    Memo1.Lines.Add(FFullcmd);
    Memo1.Lines.Add('');
  end;

  //mostra resultado do processo
  if (MIShowOut.Checked = True) then
  begin
    if (Resultado <> '') then
    begin
      Memo1.Lines.Add('::: OUTPUT :::');
      Memo1.Lines.Add(Resultado);
      Memo1.SelStart:= Memo1.GetTextLen;
    end;
  end;

  //indicação normal
  Bar1.Style:= pbstNormal;
  LblStatus.Caption:= LBLSTATUS_0;
end;

//
// Form - Events
//

procedure TF_Main.FormCreate(Sender: TObject);
begin

end;

procedure TF_Main.FormShow(Sender: TObject);
begin
  F_Init.ShowModal;

  //primeira vez
  if (FileExists(ExtractFilePath(Application.ExeName) + XMLPropStorage1.FileName) = False) then
  begin
    CBComDropDown(CBCom);
    CBDevDropDown(CBDev);

    //mostra informações
    MIAbout.Click;

    //mostra termos com opção
    TaskDialog1.Tag:= 1;
    MITerms.Click;
  end;
end;

procedure TF_Main.FormDropFiles(Sender: TObject;
  const FileNames: array of String);
begin
  //proteções
  if (Length(FileNames) = 0) then
    Exit;
  if (POS('.HEX', UpperCase(FileNames[0])) = 0) then
    Exit;

  EditFilePath1.Text:= FileNames[0];
  EditFilePath1.SelStart:= Length(EditFilePath1.Text);
end;

//
// Main Menu - Menu Item
//

procedure TF_Main.MIWriteDevClick(Sender: TObject);
begin
  BtnWriteDevClick(BtnWriteDev);
end;

procedure TF_Main.MISearchClick(Sender: TObject);
begin
  BtnSearchClick(BtnSearch);
end;

procedure TF_Main.MISerialDevClick(Sender: TObject);
var
  i: Integer;
  aSubItem: TMenuItem;
begin
  CBCom.DroppedDown:= True;
  {
  WIN_GetSerialDev();
  Popup1.Items.Clear;
  for i:= 0 to (CBCom.Items.Count - 1) do
  begin
    aSubItem := TMenuItem.Create(Popup1);
    aSubItem.Caption := CBCom.Items[i];
    Popup1.Items.Add(aSubItem);
  end;
  Popup1.PopUp(Mouse.CursorPos.X, Mouse.CursorPos.Y);
  }
end;

procedure TF_Main.MIPicDevClick(Sender: TObject);
begin
  CBDev.DroppedDown:= True;
end;

procedure TF_Main.MIAlphaEffectClick(Sender: TObject);
begin
  F_Main.AlphaBlend:= MIAlphaEffect.Checked;
end;

procedure TF_Main.MIArduinoIDEClick(Sender: TObject);
var
  aDummy: String;
begin
  //máquina windows
  {$IFDEF MSWINDOWS}
    //sistema 32 bits
    aDummy:= 'C:\\Program Files\Arduino\arduino.exe';
    if (FileExists(aDummy) = True) then
      RunCommand(Format(WIN_HIGH_RUN_EXE, [aDummy]), [], aDummy, [poNewConsole])

    //sistema 64 bits
    else begin
      aDummy:= 'C:\\Program Files (x86)\Arduino\arduino.exe';
      if (FileExists(aDummy) = True) then
        RunCommand(Format(WIN_HIGH_RUN_EXE, [aDummy]), [], aDummy, [poNewConsole]);
    end;
  {$ENDIF}
end;

procedure TF_Main.MIEnableBarClick(Sender: TObject);
const
  FORM_MIN_HEIGHT = 310;
begin
  Bar1.Visible:= MIEnableBar.Checked;
  if (F_Main.WindowState = TWindowState.wsNormal) then
  begin
    if (Bar1.Visible = True) then
    begin
      F_Main.Constraints.MinHeight:= FORM_MIN_HEIGHT;
      F_Main.Height:= F_Main.Height + Bar1.Height;
    end
    else
    begin
      F_Main.Constraints.MinHeight:= FORM_MIN_HEIGHT - Bar1.Height;
      F_Main.Height:= F_Main.Height - Bar1.Height;
    end;
  end;
end;

procedure TF_Main.MIEnableTableClick(Sender: TObject);
begin
  PanelCenterLeft.Visible:= MIEnableTable.Checked;
end;

procedure TF_Main.MILinkOriginalClick(Sender: TObject);
begin
  OpenURL('https://github.com/embhobbb/a-p-prog');
end;

procedure TF_Main.MIAboutClick(Sender: TObject);
begin
  TaskDialog1.MainIcon:=      TTaskDialogIcon.tdiInformation;
  TaskDialog1.Flags:=         [tfAllowDialogCancellation, tfEnableHyperlinks];
  TaskDialog1.CommonButtons:= [TTaskDialogCommonButton.tcbOk];
  TaskDialog1.DefaultButton:= TTaskDialogCommonButton.tcbOk;

  TaskDialog1.Caption:= 'About';
  TaskDialog1.Title:=   'About ' + F_Main.Caption;

  TaskDialog1.Text:=
  '<a href="#">WARNING:</a> Use at your own risk, WE DO NOT PROVIDE ' +
  'GUARANTEE of any kind of malfunction or damage of devices.' + #10 + #13 +
  #10 + #13 +
  'This open-source software was created to help to program PIC ' +
  'devices more easily from serial port through Arduino board, ' +
  'which it is low cost and very available.' + #10 + #13 +
  'Boards already tested:' + #10 + #13 +
  '- Arduino Nano;' + #10 + #13 +
  '- Arduino Uno;' + #10 + #13 +
  '- Arduino Mega 2560;' + #10 + #13 +
  '- And some "knock-off boards".' + #10 + #13 +
  #10 + #13 +
  'For now, the programmer is compatible with <a href="#">many</a> ' +
  'PIC devices which has:' + #10 + #13 +
  '- 8-bit processors;' + #10 + #13 +
  '- Flash program memory;' + #10 + #13 +
  '- Programming Voltage of 5 volts on MCLR pin.' + #10 + #13 +
  #10 + #13 +
  'Its based on a code originally written in C from jaromir-sukuba ' +
  'that can only run in prompt and console, it is Windows and Linux ' +
  'compatible.' + #10 + #13 +
  'The original code can be found at: ' +
  '<a href="#">Access external links > Original project - Micro ' +
  'Progmeter</a>' + #10 + #13 +
  #10 + #13 +
  'We would apreciate any help, suggestion and bug report.' + #10 + #13 +
  'Thanks a lot for using this solution.';

  if (TaskDialog1.Execute = True) then
  begin
  end;
end;

procedure TF_Main.MITermsClick(Sender: TObject);
begin
  TaskDialog1.MainIcon:=   TTaskDialogIcon.tdiWarning;
  TaskDialog1.FooterIcon:= TTaskDialogIcon.tdiInformation;
  TaskDialog1.Flags:=      [tfAllowDialogCancellation, tfEnableHyperlinks];

  //modo obrigatório
  if (TaskDialog1.Tag = 0) then
  begin
    TaskDialog1.CommonButtons:= [TTaskDialogCommonButton.tcbOk];
    TaskDialog1.DefaultButton:= TTaskDialogCommonButton.tcbOk;
  end
  //modo de aceitação
  else begin
    TaskDialog1.CommonButtons:= [TTaskDialogCommonButton.tcbYes, TTaskDialogCommonButton.tcbCancel];
    TaskDialog1.DefaultButton:= TTaskDialogCommonButton.tcbYes;
  end;

  TaskDialog1.Title:= 'WARNING!';
  TaskDialog1.Caption:= 'Warranties and Terms of use';

  TaskDialog1.FooterText:=
  'By clicking in "YES" to accept you have read and agreed with ' +
  'all the terms here included.';

  TaskDialog1.Text:=
  'Use at your own risk, WE DO NOT PROVIDE GUARANTEE of any kind ' +
  'of malfunction or damage of devices.' + #10 + #13 +
  #10 + #13 +
  'The MIT License (MIT)' + #10 + #13 +
  'Copyright (c) 2015 Jaromir Sukuba' + #10 + #13 +
  #10 + #13 +
  'Permission is hereby granted, free of charge, to any person ' +
  'obtaining a copy of this software and associated documentation ' +
  'files (the "Software"), to deal in the Software without ' +
  'restriction, including without limitation the rights to use, ' +
  'copy, modify, merge, publish, distribute, sublicense, and/or ' +
  'sell copies of the Software, and to permit persons to whom the ' +
  'Software is furnished to do so, subject to the following ' +
  'conditions:' +
  #10 + #13 +
  'The above copyright notice and this permission notice shall be ' +
  'included in all copies or substantial portions of the Software.' + #10 + #13 +
  #10 + #13 +
  'THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY ' +
  'OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT ' +
  'LIMITED TO THE WARRANTIES OF MERCHANTABILITY, ' +
  'FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. ' +
  'IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE ' +
  'LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, ' +
  'WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ' +
  'ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE ' +
  'OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.';

  //chama diálogo
  if (TaskDialog1.Execute = True) then
  begin
    //modo obrigatório
    if (TaskDialog1.Tag <> 0) then
    begin
      //altera para modo obrigatório
      TaskDialog1.Tag:= 0;

      //resposta negativa
      if (TaskDialog1.ModalResult <> mrYes) then
      begin
        //não permite salvar parâmetros
        XMLPropStorage1.Active:= False;

        //fecha programa
        Application.Terminate;
      end;
    end;
  end;
end;

procedure TF_Main.MIExitClick(Sender: TObject);
begin
  F_Main.Close;
end;

//
// Restore Properties
//

procedure TF_Main.XMLPropStorage1RestoreProperties(Sender: TObject);

  //filtra porta serial
  procedure CheckRestored_SerialPort();
  var
    i: Integer;
    TmpS: String;
  begin
    //backup porta serial salva
    TmpS:= CBCom.Text;

    //atualiza portas seriais disponíveis
    CBComDropDown(CBCom);

    //procura se porta salva existe
    for i:= 0 to (CBCom.Items.Count - 1) do
    begin
      if (TmpS = CBCom.Items[i]) then
      begin
        CBCom.ItemIndex:= i;
        TmpS:= '';
        Break;
      end;
    end;

    //porta não foi encontrada como disponível
    if (TmpS <> '') then
      CBCom.ItemIndex:= -1;
  end;

begin
  //filtra estado do form
  if (F_Main.WindowState = TWindowState.wsMinimized) then
    F_Main.WindowState:= TWindowState.wsNormal;

  //filtra porta serial
  CheckRestored_SerialPort();
end;

//
// ComboBox
//

procedure TF_Main.CBDevSelect(Sender: TObject);
begin
  GetPicDevData();
end;

procedure TF_Main.CBDevChange(Sender: TObject);
begin
  //GetPicDevData();
end;

procedure TF_Main.CBDevDropDown(Sender: TObject);
begin
  GetPicDev();
end;

procedure TF_Main.CBComDropDown(Sender: TObject);
begin
  {$IFDEF MSWINDOWS}
    WIN_GetSerialDev();
  {$ENDIF}
end;

//
// Buttons
//

procedure TF_Main.BtnSearchClick(Sender: TObject);
begin
  if (OpenDialog1.Execute = True) then
  begin
    EditFilePath1.Text:= OpenDialog1.FileName;
    EditFilePath1.SelStart:= Length(EditFilePath1.Text);
  end;
end;

procedure TF_Main.BtnWriteDevClick(Sender: TObject);
var
  Apath,
  Aserial,
  Adevice,
  Afile,
  Asleep1,
  Asleep2,
  Averbose1,
  Averbose2: String;
begin
  //proteções
  if (EditFilePath1.Text = '') then
    Exit;
  if (POS('.HEX', UpperCase(EditFilePath1.Text)) = 0) then
    Exit;
  if (FileExists(EditFilePath1.Text) = False) then
    Exit;

  //modo de programação - programa em Pascal
  if (MINewProg.Checked = True) then
  begin

    //------
    //TODO usar o main do código
    //------

  end
  //modo de programação - programa em C
  else begin
    //obtem caminho do executável
    Apath := GetExePath() + PATH_PP_PROG + 'pp3.exe';

    //inicio do comando
    Aserial:= CBCom.Text;
    Adevice:= LowerCase(CBDev.Text);
    Afile:=   EditFilePath1.Text;

    //tempo de delay
    Asleep1:= '-s';
    if (MIDelay500.Checked = True) then
      Asleep2:= '500'
    else
    if (MIDelay1700.Checked = True) then
      Asleep2:= '1700'
    else
    if (MIDelay2000.Checked = True) then
      Asleep2:= '2000'
    else
    if (MIDelay2500.Checked = True) then
      Asleep2:= '2500'
    else
    if (MIDelay3000.Checked = True) then
      Asleep2:= '3000'
    else
    begin
      Asleep1:= '';
      Asleep2:= '';
    end;

    //nível de debug
    Averbose1:= '-v';
    if (MIDebug1.Checked = True) then
      Averbose2:= '1'
    else
    if (MIDebug2.Checked = True) then
      Averbose2:= '2'
    else
    if (MIDebug3.Checked = True) then
      Averbose2:= '3'
    else
    if (MIDebug4.Checked = True) then
      Averbose2:= '4'
    else
    begin
      Averbose1:= '';
      Averbose2:= '';
    end;

    //obtem o comando inteiro
    FFullcmd:= '-c ' + Aserial + ' -t ' + Adevice + ' ' + Afile;
    if (Asleep1 <> '') then
      FFullcmd:= FFullcmd + ' ' + Asleep1 + ' ' + Asleep2;
    if (Averbose1 <> '') then
      FFullcmd:= FFullcmd + ' ' + Averbose1 + ' ' + Averbose2;

    //executa o programa com estes parametros
    if (ExecuteCMD(Apath, ASerial, Adevice, Afile,
                   Asleep1, Asleep2, Averbose1, Averbose2,
                   @OnAfterRunCmd) = True) then
    begin
      //indicação de progresso
      Bar1.Style:= pbstMarquee;
      LblStatus.Caption:= LBLSTATUS_1;
    end;

    //exemplo de comando:
    //pp3.exe -c COM30 -t 16f1829 file.hex -s 1000 -v 4
  end;
end;

end.

