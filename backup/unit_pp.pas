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
unit unit_pp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, StdCtrls, LazSerial;

type
  size_t = Integer;

const
  //configuração *
  PP_VERSION = '0.99';
  CF_P16F_A  = 0;
  CF_P18F_A  = 1;
  CF_P16F_B  = 2;
  CF_P18F_B  = 3;
  CF_P18F_C  = 4;
  CF_P18F_D  = 5;
  CF_P18F_E  = 6;
  CF_P16F_C  = 7;
  CF_P16F_D  = 8;
  CF_P18F_F  = 9;
  CF_P18F_G  = 10;
  CF_P18F_Q  = 11;
  //interno *
  PROGMEM_LEN   = 260000;
  CONFIG_LEN    = 32;
  HEXFILE_LEN   = 70000;
  PROGMEM_LEN_1 = PROGMEM_LEN - 1;
  CONFIG_LEN_1  = CONFIG_LEN  - 1;
  HEXFILE_LEN_1 = HEXFILE_LEN - 1;
  //alias *
  stdout = 0;
  sleep_ms: procedure(Milliseconds : Cardinal) = @SysUtils.Sleep;

var
  //configuração *
  AComport    : TLazSerial;
  AMemoOutput : TMemo;
  //interno *
  verbose,
  verify,
  aprogram, //program,
  sleep_time,
  devid_expected,
  devid_mask,
  baudRate,
  //com,
  flash_size,
  page_size,
  chip_family,
  config_size:  Integer;
  file_image:   Array[0..HEXFILE_LEN_1] of Byte;
  progmem:      Array[0..PROGMEM_LEN_1] of Char;
  config_bytes: Array[0..CONFIG_LEN_1]  of Char;

implementation

function main(aCOM: String; PicDevice: String; FileHex: String; DebugMode: Integer; aDelay: Integer; p: Boolean; n: Boolean): Boolean;

  //-------------------------------------------------
  // * Printf *
  //
  // Mostra caracteres no console
  //-------------------------------------------------
  procedure printf(const Fmt : AnsiString; const Args : Array of const); overload;
  begin
    AMemoOutput.Lines.Add(format(Fmt, Args));
  end;
  procedure printf(const Fmt : AnsiString); overload;
  begin
    printf(Fmt,[]);
  end;

  //-------------------------------------------------
  // * flsprintf *
  //
  // Não é padrão C - print genérico
  //-------------------------------------------------
  procedure flsprintf(ANone: Integer; const Fmt : AnsiString; const Args : Array of const) overload;
  begin
    printf(Fmt, Args);
  end;
  procedure flsprintf(ANone: Integer; const Fmt : AnsiString) overload;
  begin
    printf(Fmt,[]);
  end;

  //-------------------------------------------------
  // * comErr *
  //
  // Não é padrão C - print para erros
  //-------------------------------------------------
  procedure comErr(const Fmt : AnsiString; const Args : Array of const);
  begin
    printf(Fmt, [Args]);
  end;

  //-------------------------------------------------
  // * FFlush *
  //
  // Limpa buffer de saída do console
  //-------------------------------------------------
  procedure fflush(var aNone: Text); overload;
  begin
    //nada
    //Flush(aNone);
  end;
  procedure fflush(aNone: Integer); overload;
  begin
    //nada
  end;

  //-------------------------------------------------
  // * StrCmp *
  //
  // Compara duas strings, e retorna:
  // -Zero quando iguais
  // -Valor positivo quando primeiro char diferente
  //  e de valor maior for de origem da string ptr1
  // -Valor negativo quando primeiro char diferente
  //  e de valor maior for de origem da string ptr2
  //-------------------------------------------------
  function strcmp(ptr1: String; ptr2: String): Integer;
  var
    i, j, p1, p2: Integer;
  begin
    //totalmente iguais
    if (ptr1 = ptr2) then
      Result:= 0
    //strings diferentes entre si
    else begin
      //obtem o maior tamanho
      if (Length(ptr1) > Length(ptr2)) then
        j:= Length(ptr1)
      else
        j:= Length(ptr2);

      //procura o menor
      for i:= 1 to j do
      begin
        //quando diferentes
        if (ptr1[i] <> ptr2[i]) then
        begin
          //compara valores
          p1:= ord(ptr1[i]);
          p2:= ord(ptr2[i]);
          if (p1 > p2) then
            Result:= i
          else
            Result:= -i;
          Exit;
        end;
      end;

      //se terminou e ainda não encontrou
      if (j = Length(ptr1)) then
        Result:= i
      else
        Result:= -i;
    end;
  end;

  //-------------------------------------------------
  // * FFlush *
  //
  // Não é padrão C - Verifica se vetor é nulo
  //-------------------------------------------------
  function is_empty(buff: Array of Char; len: Integer): Boolean;
  var
    i: Integer;
    empty: Boolean;
  begin
    empty:= True;
    for i:= 0 to (len - 1) do
    begin
      if (buff[i] <> Chr($FF)) then
        empty:= False;
    end;
    Result:= empty;
  end;

  //-------------------------------------------------
  // * Set CPU type *
  //
  // Check the type of CPU through database file
  //-------------------------------------------------
  function setCPUtype(cpu: String): Integer;
  var
    i, read,
    name_len,
    read_id,
    read_mask,
    read_flash_size,
    read_page_size   : Integer;

    len              : size_t;

    filename,
    line             : String;

    read_cpu_type,
    read_algo_type   : Array[0..19] of Char;

    FileObj          : TextFile;
  begin
    name_len:= Length(cpu);
    cpu:= LowerCase(cpu);
    line:= '';
    filename:= 'pp3_devices.dat';

    len:= 0;
    if (verbose > 2) then
      printf('Opening filename %s \n', [filename]);

    try
      AssignFile(FileObj, filename);
      Reset(FileObj);
    except
      Result:= -1;
      if (verbose > 0) then
        printf ('Cant open database file %s\n',[filename]);
      Exit;
    end;

    if (verbose > 2) then
      printf ('File open\n');

    while Not Eof(FileObj) do
    begin
      ReadLn(FileObj, line);
      read:= Length(line);

      if (verbose > 3) then
        printf('\nRead %d chars: %s',[read,line]);
      if (line[1] <> '#') then
      begin
        //sscanf(line,'%s %d %d %x %x %s',(char*)@read_cpu_type,@ead_flash_size,@read_page_size,@read_id,@read_mask,(char*)@read_algo_type);
        sscanf(line,'%s %d %d %x %x %s',[@read_cpu_type,@read_flash_size,@read_page_size,@read_id,@read_mask,@read_algo_type]);
	if (verbose > 3) then
          printf('\n*** %s,%d,%d,%x,%x,%s',[read_cpu_type,read_flash_size,read_page_size,read_id,read_mask,read_algo_type]);
	if (strcmp(read_cpu_type,cpu) = 0) then
	begin
	  flash_size:=     read_flash_size;
	  page_size:=      read_page_size;
	  devid_expected:= read_id;
	  devid_mask:=     read_mask;

	  if (verbose > 1) then
            printf('Found database match %s,%d,%d,%x,%x,%s\n',[read_cpu_type,read_flash_size,read_page_size,read_id,read_mask,read_algo_type]);

	  if (strcmp('CF_P16F_A',read_algo_type) = 0) then
            chip_family:= CF_P16F_A;
	  if (strcmp('CF_P16F_B',read_algo_type) = 0) then
            chip_family:= CF_P16F_B;
	  if (strcmp('CF_P16F_C',read_algo_type) = 0) then
            chip_family:= CF_P16F_C;
	  if (strcmp('CF_P16F_D',read_algo_type) = 0) then
            chip_family:= CF_P16F_D;
	  if (strcmp('CF_P18F_A',read_algo_type) = 0) then
            chip_family:= CF_P18F_A;
	  if (strcmp('CF_P18F_B',read_algo_type) = 0) then
            chip_family:= CF_P18F_B;
	  if (strcmp('CF_P18F_C',read_algo_type) = 0) then
            chip_family:= CF_P18F_C;
	  if (strcmp('CF_P18F_D',read_algo_type) = 0) then
            chip_family:= CF_P18F_D;
	  if (strcmp('CF_P18F_E',read_algo_type) = 0) then
            chip_family:= CF_P18F_E;
	  if (strcmp('CF_P18F_F',read_algo_type) = 0) then
            chip_family:= CF_P18F_F;
  	  if (strcmp('CF_P18F_G',read_algo_type) = 0) then
            chip_family:= CF_P18F_G;
	  if (strcmp('CF_P18F_Q',read_algo_type) = 0) then
            chip_family:= CF_P18F_Q;
	  if (chip_family = CF_P18F_A) then
            config_size:= 16;
	  if (chip_family = CF_P18F_B) then
            config_size:= 8;
	  if (chip_family = CF_P18F_C) then
	  begin
	    config_size:= 16;
	    chip_family:= CF_P18F_B;
	  end;
	  if (chip_family = CF_P18F_D) then
            config_size:= 16;
	  if (chip_family = CF_P18F_E) then
            config_size:= 16;
	  if (chip_family = CF_P18F_F) then
            config_size:= 12;
	  if (chip_family = CF_P18F_Q) then
            config_size:= 12;
	  if (chip_family = CF_P18F_G) then
	  begin
	    config_size:= 10;
	    chip_family:= CF_P18F_F;
	  end;
	  if (verbose > 2) then
            printf('chip family:%d, config size:%d\n',[chip_family,config_size]);
	end;
      end;
    end;
    CloseFile(FileObj);
    Result:= 0;
  end;

  //-------------------------------------------------
  // * Parse Args *
  //
  // Obtem/inicializa os parametros da rotina
  //-------------------------------------------------
  function parseArgs(): Boolean;
  var
    c: Integer;
  begin
    //valores padrão
    Result:= False;
    verbose:= 1;
    verify:= 1;
    aprogram:= 1;
    sleep_time:= 0;

    //segurança
    if (aCOM = '') then
      Exit;
    if (PicDevice = '') then
      Exit;
    if (aDelay < 0) then
      Exit;

    //printHelp();

    //obtem valores
    verbose:= DebugMode;
    sleep_time:= aDelay;
    if (p = True) then
      aprogram:= 0;
    if (n = True) then
      verify:= 0;
    setCPUtype(PicDevice);

    Result:= True;
  end;

  //-------------------------------------------------
  // * Init Serial Port *
  //-------------------------------------------------
  function initSerialPort(): Boolean;
  begin
    Result:= False;

    //tenta inicializar
    try
      if (AComport = nil) then
        AComport.Create(nil)
      else if (AComport.Active = True) then
        AComport.Close;
    except
      printf('Unable to initialize port %s\n',[aCOM]);
      Exit;
    end;
    //tenta configurar
    try
      AComport.Device:=   aCOM;
      AComport.BaudRate:= TBaudRate.br_57600;
      AComport.DataBits:= TDataBits.db8bits;
      AComport.Parity:=   TParity.pNone;
      AComport.StopBits:= TStopBits.sbOne;
    except
      printf('Unable to configure port %s\n',[aCOM]);
      Exit;
    end;
    //tenta abrir
    try
      AComport.Open;
    except
      printf('Unable to open port %s\n',[aCOM]);
      Exit;
    end;

    Result:= True;
  end;

  function getByte(): Byte;
  var
    buf: Byte;
    n: Integer;
    aBuffer: String;
  begin
    //n:= read(com, &buf, 1);
    aBuffer:= AComport.ReadData();
    buf:= Ord(aBuffer[1]);
    n:= Length(aBuffer);

    if ((verbose > 3) and (n < 1)) then
      flsprintf(stdout,'RX: fail\n":"RX:  0x%02X\n', [buf and $FF]);
    if (n = 1) then
    begin
      Result:= (buf and $FF);
      Exit;
    end;
    comErr('Serial port failed to receive a byte, read returned %d\n', [n]);

    // never reached
    Result:= 255; //-1;
  end;

  //-------------------------------------------------
  // * put Byte / put Bytes *
  //-------------------------------------------------
  procedure putByte(abyte: Integer);
  var
    n: Integer;
    abuf: Byte;
    buf: String;
  begin
    abuf:= abyte and $FF;
    buf:= '' + Chr(abuf);

    if (verbose > 3) then
      flsprintf(stdout,'TX: 0x%02X\n', [abyte]);

    n:= AComport.WriteData(buf);

    if (n <> 1) then
      comErr('Serial port failed to send a byte, write returned %d\n', [n]);
  end;
  procedure putBytes (data: String; len : Integer);
  var
    i, anum: Integer;
  begin
    for i:= 1 to len do
    begin
      anum:= Ord(data[i]);
      putByte(anum);
    end;
  end;

  //-------------------------------------------------
  // * Parse Hex *
  //-------------------------------------------------
  function parse_hex(filename: String; var progmem: Array of Char; var config: Array of Char): Integer;
  var
    p16_cfg,
    i, temp, read,
    line_len, line_type,
    line_address,
    line_address_offset,
    effective_address    : Integer;
    len                  : size_t;
    line_content         : Array[0..127] of Char;
    line                 : String;
    FileObj              : TextFile;
  begin
    line:= '';
    len:= 0;

    p16_cfg:= 0;
    if (verbose > 2) then
      printf ('Opening filename %s \n', [filename]);

    //abre para ler
    try
      AssignFile(FileObj, filename);
      Reset(FileObj);
    except
      Result:= -1;
      Exit;
    end;

    line_address_offset:= 0;
    if (chip_family = CF_P16F_A) then
      p16_cfg:= 1;
    if (chip_family = CF_P16F_B) then
      p16_cfg:= 1;
    if (chip_family = CF_P16F_C) then
      p16_cfg:= 1;
    if (chip_family = CF_P16F_D) then
      p16_cfg:= 1;

    if (verbose > 2) then
      printf ('File open\n');

    while Not Eof(FileObj) do
    begin
      //obtem a linha
      ReadLn(FileObj, line);
      read:= Length(line);

      if (verbose > 2) then
        printf('\nRead %d chars: %s',[read, line]);

      if (line[1] <> ':') then
      begin
        if (verbose > 1) then
        begin
          printf('--- : invalid\n');
          Result:= -1;
          Exit;
        end;
      end;
      sscanf(line[2],'%2X',@line_len);
      sscanf(line[4],'%4X',@line_address);
      sscanf(line[8],'%2X',@line_type);
      effective_address:= line_address + (65536 * line_address_offset);
      if (verbose > 2) then
        printf('Line len %d B, type %d, address 0x%4.4x offset 0x%4.4x, EFF 0x%6.6x\n',[line_len,line_type,line_address,line_address_offset,effective_address]);
      if (line_type = 0) then
      begin
        for i:= 0 to (line_len - 1) do
        begin
          sscanf(line[10 + (i*2)], '%2X', @temp);
          line_content[i]:= Chr(temp);
        end;
        if (effective_address < flash_size) then
        begin
          if (verbose > 2) then
            printf('PM ');
          for i:=0 to (line_len - 1) do
            progmem[effective_address + i]:= line_content[i];
        end;
        if ((line_address_offset = $30) and ((chip_family = CF_P18F_A) or
           (chip_family = CF_P18F_D) or (chip_family = CF_P18F_E) or
           (chip_family = CF_P18F_F) or (chip_family = CF_P18F_Q))) then
        begin
          if (verbose > 2) then
            printf('CB ');

          for i:= 0 to (line_len - 1) do
            config[i]:= line_content[i];
        end;
        if ((chip_family = CF_P18F_B) and (effective_address = (flash_size - config_size))) then
        begin
          if (verbose > 2) then
            printf('CB ');

          for i:= 0 to (line_len - 1) do
            config[i]:= line_content[i];
        end;
        if ((line_address_offset = $01) and (p16_cfg = 1)) then
        begin
          if (verbose > 2) then
            printf('CB ');

          for i:=0 to (line_len - 1) do
            config[line_address + i - $0E]:= line_content[i];
        end;
      end;
      if (line_type = 4) then
        sscanf(line[10],'%4X',@line_address_offset);
      if (verbose > 2) then
      begin
        for i:= 0 to (line_len - 1) do
          printf('%2.2X',[line_content[i]]);
        printf('\n');
      end;
    end;

    //finaliza
    CloseFile(FileObj);
    Result:= 0;
  end;

  //-------------------------------------------------
  // * Write Page - PIC18 A *
  //-------------------------------------------------
  function p18a_write_page(data: Array of Char; address: Integer; num: Byte): Integer;
  var
     i, empty: Byte;
  begin
    empty:= 0;
    for i:= 0 to (num - 1) do
    begin
      if (data[i] <> Chr($FF)) then
        empty:= 0;
    end;
    if (empty = 1) then
    begin
      if (verbose > 3) then
        flsprintf(stdout,'~');
      Result:= 0;
      Exit;
    end;
    if (verbose > 2) then
      flsprintf(stdout,'Writing A page of %d bytes at 0x%6.6x\n', [num, address]);
    putByte($12);
    putByte(4 + num);
    putByte(num);
    putByte((address shr 16) and $FF);
    putByte((address shr 8)  and $FF);
    putByte((address shr 0)  and $FF);
    for i:=0 to (num - 1) do
      putByte(Ord(data[i]));
    getByte();
    Result:= 0;
  end;

  //-------------------------------------------------
  // * Write Page - PIC18 D *
  //-------------------------------------------------
  function p18d_write_page(data: Array of Char; address: Integer; num: Byte): Integer;
  var
     i, empty: Byte;
  begin
    empty:= 0;
    for i:=0 to (num - 1) do
    begin
      if (data[i] <> Chr($FF)) then
        empty:= 0;
    end;
    if (empty = 1) then
    begin
      if (verbose > 3) then
        flsprintf(stdout,'~');
      Result:= 0;
      Exit;
    end;
    if (verbose > 2) then
      flsprintf(stdout,'Writing D page of %d bytes at 0x%6.6x\n', [num, address]);
    putByte($31);
    putByte(4 + num);
    putByte(num);
    putByte((address shr 16) and $FF);
    putByte((address shr 8)  and $FF);
    putByte((address shr 0)  and $FF);
    for i:=0 to (num - 1) do
      putByte(Ord(data[i]));
    getByte();
    Result:= 0;
  end;

  //-------------------------------------------------
  // * Write Page - PIC18 Q *
  //-------------------------------------------------
  function p18q_write_page(data: Array of Char; address: Integer; num: Byte): Integer;
  var
     i, empty: Byte;
  begin
    address:= address div 2;
    empty:= 1;
    i:= 0;
    while (i < num) do
    begin
      if ((data[i] <> Chr($FF)) or (data[i + 1] <> Chr($FF))) then
        empty:= 0;
      i:= i + 2;
    end;
    if (verbose > 2) then
      flsprintf(stdout,'Writing A page of %d bytes at 0x%6.6x\n', [num, address]);
    if (empty = 1) then
    begin
      if (verbose > 3) then
        flsprintf(stdout,'~');
      Result:= 0;
      Exit;
    end;
    putByte($46);
    putByte(4 + num);
    putByte(num);
    putByte((address shr 16) and $FF);
    putByte((address shr 8)  and $FF);
    putByte((address shr 0)  and $FF);
    for i:=0 to (num - 1) do
      putByte(Ord(data[i]));
    getByte();
    Result:= 0;
  end;

  //-------------------------------------------------
  // * Write Page - PIC16 C *
  //-------------------------------------------------
  function p16c_write_page(data: Array of Char; address: Integer; num: Byte): Integer;
  var
     i, empty: Byte;
  begin
    address:= address div 2;
    empty:= 1;
    i:= 0;
    while (i < num) do
    begin
      if ((data[i] <> Chr($FF)) or (data[i + 1] <> Chr($FF))) then
        empty:= 0;
      i:= i + 2;
    end;
    if (verbose > 2) then
      flsprintf(stdout,'Writing A page of %d bytes at 0x%6.6x\n', [num, address]);
    if (empty = 1) then
    begin
      if (verbose > 3) then
        flsprintf(stdout,'~');
      Result:= 0;
      Exit;
    end;
    putByte($42);
    putByte(4 + num);
    putByte(num);
    putByte((address shr 16) and $FF);
    putByte((address shr 8)  and $FF);
    putByte((address shr 0)  and $FF);
    for i:=0 to (num - 1) do
      putByte(Ord(data[i]));
    getByte();
    Result:= 0;
  end;

  //-------------------------------------------------
  // * Program Page - PIC16 A *
  //-------------------------------------------------
  function p16a_program_page(ptr: Integer; num, slow: Byte): Integer;
  var
    i: Integer;
    file_image2: String;
  begin
    file_image2:= '';
    for i:= 0 to (Length(file_image) - 1) do
      file_image2:= file_image2 + Chr(file_image[ptr + i]);

    if (verbose > 2) then
      flsprintf(stdout,'Programming page of %d bytes at 0x%4.4x\n', [num, ptr]);

    putByte($08);
    putByte(num + 2);
    putByte(num);
    putByte(slow);
    putBytes(file_image2,num);
    getByte();
    Result:= 0;
  end;

  //-------------------------------------------------
  // * Reset Pointer - PIC16 A *
  //-------------------------------------------------
  function p16a_rst_pointer(): Integer;
  begin
    if (verbose > 2) then
      flsprintf(stdout,'Resetting PC\n');
    if (chip_family = CF_P16F_D) then
      putByte($09)  //operation number
    else
      putByte($03); //operation number
    putByte($00);   //number of bytes remaining
    getByte();	    //return result - no check for its value
    Result:= 0;
  end;

  //-------------------------------------------------
  // * Load Config - PIC16 A *
  //-------------------------------------------------
  function p16a_load_config(): Integer;
  begin
    if (verbose > 2) then
      flsprintf(stdout,'Load config\n');
    putByte($04);
    putByte($00);
    getByte();
    Result:= 0;
  end;

  //-------------------------------------------------
  // * Increment Pointer - PIC16 A *
  //-------------------------------------------------
  function p16a_inc_pointer(num: Byte): Integer;
  begin
    if (verbose > 2) then
      flsprintf(stdout,'Inc pointer %d\n',[num]);
    putByte($05);
    putByte($01);
    putByte(num);
    getByte();
    Result:= 0;
  end;

  //-------------------------------------------------
  // * Read Page - PIC16 A *
  //-------------------------------------------------
  function p16a_read_page(data: Array of Byte; num: Byte): Integer;
  var
    i: Byte;
  begin
    if (verbose > 2) then
      flsprintf(stdout,'Reading page of %d bytes\n', [num]);
    putByte($06);
    putByte($01);
    putByte(num div 2);
    getByte();
    for i:=0 to (num - 1) do
      data[i]:= getByte();
    Result:= 0;
  end;

  //-------------------------------------------------
  // * Get Config - PIC16 A *
  //-------------------------------------------------
  function p16a_get_config (n: Byte): Integer;
  var
    devid_lo,
    devid_hi: Byte;
    retval: Integer;
    tdat: Array[0..19] of Byte;
  begin
    p16a_rst_pointer();
    p16a_load_config();
    p16a_inc_pointer(n);
    p16a_read_page(tdat, 4);
    devid_hi := tdat[(2*0)+1];
    devid_lo := tdat[(2*0)+0];
    retval   := (devid_lo shl 0) + (devid_hi shl 8);
    if (verbose > 2) then
      flsprintf(stdout,'Getting config +%d - lo:%2.2x,hi:%2.2x = %4.4x\n', [n,devid_lo,devid_hi,retval]);
    Result:= retval;
  end;

  //-------------------------------------------------
  // * Prog get Device ID - PIC16 A *
  //-------------------------------------------------
  function p16a_get_devid(): Integer;
  var
    tdat: Array[0..19] of Byte;
    devid_lo,
    devid_hi: Byte;
    retval: Integer;
  begin
    p16a_rst_pointer();
    p16a_load_config();
    p16a_inc_pointer(6);
    p16a_read_page(tdat, 4);
    devid_hi:= tdat[(2 * 0) + 1];
    devid_lo:= tdat[(2 * 0) + 0];
    if (verbose > 2) then
      flsprintf(stdout,'Getting devid - lo:%2.2x,hi:%2.2x\n',[devid_lo, devid_hi]);
    retval:= (devid_lo shl 0) + (devid_hi shl 8);
    retval:= retval and devid_mask;
    Result:= retval;
  end;

  //-------------------------------------------------
  // * Read Page - PIC16 C *
  //-------------------------------------------------
  function p16c_read_page(data: Array of Byte; address: Integer; num: Byte): Integer;
  var
    i: Byte;
  begin
    address:= address div 2;
    if (verbose > 2) then
      flsprintf(stdout,'Reading page of %d bytes at 0x%6.6x\n', [num, address]);
    putByte($41);
    putByte($04);
    putByte(num div 2);
    putByte((address shr 16) and $FF);
    putByte((address shr 8)  and $FF);
    putByte((address shr 0)  and $FF);
    getByte();
    for i:= 0 to (num - 1) do
      data[i]:= getByte();
    Result:= 0;
  end;

  //-------------------------------------------------
  // * Prog get Device ID - PIC16 C *
  //-------------------------------------------------
  function p16c_get_devid(): Integer;
  var
    tdat: Array[0..19] of Byte;
    devid_lo,
    devid_hi: Byte;
    retval: Integer;
  begin
    p16c_read_page(tdat, $8006 * 2, 4);
    devid_hi:= tdat[(2 * 0) + 1];
    devid_lo:= tdat[(2 * 0) + 0];
    if (verbose > 2) then
      flsprintf(stdout,'Getting devid - lo:%2.2x,hi:%2.2x\n',[devid_lo,devid_hi]);
    retval:= (devid_lo shl 0) + (devid_hi shl 8);
    retval:= retval and devid_mask;
    Result:= retval;
  end;

  //-------------------------------------------------
  // * Read Page - PIC18 A *
  //-------------------------------------------------
  function p18a_read_page(data: Array of Byte; address: Integer; num: Byte): Integer;
  var
    i: Byte;
  begin
    if (verbose > 2) then
      flsprintf(stdout,'Reading page of %d bytes at 0x%6.6x\n', [num, address]);
    putByte($11);
    putByte($04);
    putByte(num div 2);
    putByte((address shr 16) and $FF);
    putByte((address shr 8)  and $FF);
    putByte((address shr 0)  and $FF);
    getByte();
    for i:=0 to (num - 1) do
      data[i]:= getByte();
    Result:= 0;
  end;

  //-------------------------------------------------
  // * Prog get Device ID *
  //-------------------------------------------------
  function prog_get_device_id(): Integer;
  var
    mem_str: Array[0..9] of Byte;
    devid: Integer;
  begin
    if (verbose > 2) then
      flsprintf(stdout,'getting ID for family %d\n',[chip_family]);

    if ((chip_family = CF_P16F_A) or (chip_family = CF_P16F_B) or (chip_family = CF_P16F_D)) then
    begin
      Result:= p16a_get_devid();
      Exit;
    end;

    if (chip_family = CF_P16F_C) then
    begin
      Result:= p16c_get_devid();
      Exit;
    end
    else
    if ((chip_family = CF_P18F_A) or (chip_family = CF_P18F_B) or (chip_family = CF_P18F_D) or (chip_family = CF_P18F_E)) then
    begin
      p18a_read_page(mem_str, $3FFFFE, 2);
      devid:= (mem_str[1] shl 8) + (mem_str[0] shl 0);
      devid:= devid and devid_mask;
      Result:= devid;
      Exit;
    end;

    if ((chip_family = CF_P18F_F) or (chip_family = CF_P18F_Q)) then
    begin
      p16c_read_page(mem_str, ($3FFFFE * 2), 2);
      devid:= (mem_str[1] shl 8) + (mem_str[0] shl 0);
      devid:= devid and devid_mask;
      Result:= devid;
      Exit;
    end;

    Result:= 0;
  end;

  //-------------------------------------------------
  // * Mass Erase - PIC18 A *
  //-------------------------------------------------
  function p18a_mass_erase(): Integer;
  begin
    if (verbose > 2) then
      flsprintf(stdout,'Mass erase\n');
    putByte($13);
    putByte($00);
    getByte();
    Result:= 0;
  end;

  //-------------------------------------------------
  // * Mass Erase - PIC18 B *
  //-------------------------------------------------
  function p18b_mass_erase(): Integer;
  begin
    if (verbose > 2) then
      flsprintf(stdout,'Mass erase\n');
    putByte($23);
    putByte($00);
    getByte();
    Result:= 0;
  end;

  //-------------------------------------------------
  // * Mass Erase Part - PIC18 D *
  //-------------------------------------------------
  function p18d_mass_erase_part(data: LongInt): Integer;
  begin
    if (verbose > 2) then
      flsprintf(stdout,'Mass erase part of 0x%6.6x\n',[data]);
    putByte($30);
    putByte($03);
    putByte((data shr 16) and $FF);
    putByte((data shr 8)  and $FF);
    putByte((data shr 0)  and $FF);
    getByte();
    Result:= 0;
  end;

  //-------------------------------------------------
  // * Mass Erase - PIC18 D *
  //-------------------------------------------------
  function p18d_mass_erase(): Integer;
  begin
    if (verbose > 2) then
      flsprintf(stdout,'Mass erase\n');
    p18d_mass_erase_part($800104);
    p18d_mass_erase_part($800204);
    p18d_mass_erase_part($800404);
    p18d_mass_erase_part($800804);
    //...
    p18d_mass_erase_part($800004);
    p18d_mass_erase_part($800005);
    p18d_mass_erase_part($800002);
    Result:= 0;
  end;

  //-------------------------------------------------
  // * Mass Erase - PIC18 E *
  //-------------------------------------------------
  function p18e_mass_erase(): Integer;
  begin
    if (verbose > 2) then
      flsprintf(stdout,'Mass erase\n');
    p18d_mass_erase_part($800104);
    p18d_mass_erase_part($800204);
    p18d_mass_erase_part($800404);
    p18d_mass_erase_part($800804);
    p18d_mass_erase_part($801004);
    p18d_mass_erase_part($802004);
    p18d_mass_erase_part($804004);
    p18d_mass_erase_part($808004);
    p18d_mass_erase_part($800004);
    p18d_mass_erase_part($800005);
    p18d_mass_erase_part($800002);
    Result:= 0;
  end;

  //-------------------------------------------------
  // * Mass Erase - PIC16 A *
  //-------------------------------------------------
  function p16a_mass_erase(): Integer;
  begin
    if (verbose > 2) then
      flsprintf(stdout,'Mass erase\n');
    putByte($07);
    putByte($00);
    getByte();
    Result:= 0;
  end;

  //-------------------------------------------------
  // * Mass Erase - PIC16 C *
  //-------------------------------------------------
  function p16c_mass_erase(): Integer;
  begin
    if (verbose > 2) then
      flsprintf(stdout,'Mass erase\n');
    putByte($43);
    putByte($00);
    getByte();
    Result:= 0;
  end;

  //-------------------------------------------------
   // * Program Config - PIC18 A *
   //-------------------------------------------------
   function p16a_program_config(): Integer;
   begin
     p16a_rst_pointer();
     p16a_load_config();
     p16a_inc_pointer(7);
     p16a_program_page(2 * $8007, 2, 1);
     p16a_program_page(2 * $8008, 2, 1);
     if ((chip_family = CF_P16F_B) or (chip_family = CF_P16F_D)) then
       p16a_program_page(2 * $8009, 2, 1);
     if (chip_family = CF_P16F_D) then
       p16a_program_page(2 * $800A, 2, 1);
     Result:= 0;
   end;

   //-------------------------------------------------
   // * Write Config - PIC18 A *
   //-------------------------------------------------
   function p18a_write_cfg(data1, data2: Byte; address: Integer): Integer;
   begin
     if (verbose > 2) then
       flsprintf(stdout,'Writing cfg 0x%2.2x 0x%2.2x at 0x%6.6x\n', [data1, data2, address]);
     putByte($14);
     putByte(6);
     putByte(0);
     putByte((address shr 16) and $FF);
     putByte((address shr 8)  and $FF);
     putByte((address shr 0)  and $FF);
     putByte(data1);
     putByte(data2);
     getByte();
     Result:= 0;
   end;

   //-------------------------------------------------
   // * Write Single Config - PIC18 C *
   //-------------------------------------------------
   function p16c_write_single_cfg (data1, data2: Byte; address: Integer): Integer;
   begin
     if (verbose > 2) then
       flsprintf(stdout,'Writing cfg 0x%2.2x 0x%2.2x at 0x%6.6x\n', [data1, data2, address]);
     putByte($44);
     putByte(6);
     putByte(0);
     putByte((address shr 16) and $FF);
     putByte((address shr 8)  and $FF);
     putByte((address shr 0)  and $FF);
     putByte(data1);
     putByte(data2);
     getByte();
     Result:= 0;
   end;

   //-------------------------------------------------
   // * Write Config - PIC18 C *
   //-------------------------------------------------
   function p16c_write_cfg (): Integer;
   begin
     p16c_write_single_cfg(Ord(config_bytes[1]), Ord(config_bytes[0]), $8007);
     p16c_write_single_cfg(Ord(config_bytes[3]), Ord(config_bytes[2]), $8008);
     p16c_write_single_cfg(Ord(config_bytes[5]), Ord(config_bytes[4]), $8009);
     p16c_write_single_cfg(Ord(config_bytes[7]), Ord(config_bytes[6]), $800A);
     p16c_write_single_cfg(Ord(config_bytes[9]), Ord(config_bytes[8]), $800B);
     Result:= 0;
   end;

   //-------------------------------------------------
   // * Write Config - PIC18 D *
   //-------------------------------------------------
   function p18d_write_cfg (data1, data2: Byte; address: Integer): Integer;
   begin
     if (verbose > 2) then
       flsprintf(stdout,'Writing cfg 0x%2.2x 0x%2.2x at 0x%6.6x\n', [data1, data2, address]);
     putByte($32);
     putByte(6);
     putByte(0);
     putByte((address shr 16) and $FF);
     putByte((address shr 8)  and $FF);
     putByte((address shr 0)  and $FF);
     putByte(data1);
     putByte(data2);
     getByte();
     Result:= 0;
   end;

   //-------------------------------------------------
   // * Write Single Config - PIC18 Q *
   //-------------------------------------------------
   function p18q_write_single_cfg (data1, data2: Byte; address: Integer): Integer;
   begin
     if (verbose > 2) then
       flsprintf(stdout,'Writing cfg 0x%2.2x 0x%2.2x at 0x%6.6x\n', [data1, data2, address]);
     putByte($45);
     putByte(6);
     putByte(0);
     putByte((address shr 16) and $FF);
     putByte((address shr 8)  and $FF);
     putByte((address shr 0)  and $FF);
     putByte(data1);
     putByte(data2);
     getByte();
     Result:= 0;
   end;

  //-------------------------------------------------
  // * Prog Enter - progmode *
  //-------------------------------------------------
  function prog_enter_progmode(): Integer;
  begin
    if (verbose > 2) then
      flsprintf(stdout,'Entering programming mode\n');

    if (chip_family = CF_P16F_A) then
      putByte($01)
    else
    if (chip_family = CF_P16F_B) then
      putByte($01)
    else
    if (chip_family = CF_P16F_D) then
      putByte($01)
    else
    if (chip_family = CF_P18F_A) then
      putByte($10)
    else
    if (chip_family = CF_P18F_B) then
      putByte($10)
    else
    if (chip_family = CF_P18F_D) then
      putByte($10)
    else
    if (chip_family = CF_P18F_E) then
      putByte($10)
    else
    if (chip_family = CF_P16F_C) then
      putByte($40)
    else
    if (chip_family = CF_P18F_F) then
      putByte($40)
    else
    if (chip_family = CF_P18F_Q) then
      putByte($40);

    putByte($00);
    getByte();
    Result:= 0;
  end;

  //-------------------------------------------------
  // * Prog Exit - progmode *
  //-------------------------------------------------
  function prog_exit_progmode(): Integer;
  begin
    if (verbose > 2) then
      flsprintf(stdout,'Exiting programming mode\n');
    putByte($02);
    putByte($00);
    getByte();
    Result:= 0;
  end;

//---------------------------------------------------
var
  i, j,
  pages_performed,
  config, econfig: Integer;
  pm_point, cm_point: ^Byte;
  tdat: Array[0..199] of Byte;
begin
  //prepara componentes
  Result:= False;
  AMemoOutput.Clear;
  parseArgs();

  //debug
  if (verbose > 0) then
    printf('PP programmer, version %s\n', [PP_VERSION]);
  if (verbose > 1) then
    printf('Opening serial port\n');

  //inicializa serial
  if (initSerialPort() = False) then
    Exit;

  if (sleep_time > 0) then
  begin
    if (verbose > 0) then
      printf('Sleeping for %d ms while Arduino bootloader expires\n', [sleep_time]);
    fflush(stdout);
    sleep_ms(sleep_time);
  end;

  //assume erased memories (0xFF)
  for i:= 0 to PROGMEM_LEN_1 do
    progmem[i]:= Chr($FF);
  for i:= 0 to CONFIG_LEN_1 do
    config_bytes[i]:= Chr($FF);

  pm_point:= @progmem;
  cm_point:= @config_bytes;

  //parse and write content of hex file into buffers
  parse_hex(FileHex, progmem, config_bytes);

  //now this is ugly kludge
  //my original programmer expected only file_image holding the image of memory to be programmed
  //for PIC18, it is divided into two regions, program memory and config. to glue those two
  //different approaches, I made this. not particulary proud of having this mess
  for i:= 0 to HEXFILE_LEN_1 do
    file_image[i]:= Ord(progmem[i]);

  for i:= 0 to 9 do
    file_image [(2 * $8007) + i]:= Ord(config_bytes[i]);

  for i:= 0 to HEXFILE_LEN_1 do
  begin
    if ((i mod 2) <> 0) then
      file_image[i]:= file_image[i] and $3F;
  end;

  //enter programming mode and probe the target
  prog_enter_progmode();

  i:= prog_get_device_id();
  if (i = devid_expected) then
  begin
    if (verbose > 0) then
      printf ('Device ID: %4.4x \n', [i]);
  end
  else
  begin
    printf('Wrong device ID: %4.4x, expected: %4.4x\n', [i, devid_expected]);
    printf('Check for connection to target MCU, exiting now\n');
    prog_exit_progmode();
    Result:= Boolean(1);
    Exit;
  end;

  //ah, I need to unify programming interfaces for PIC16 and PIC18
  if ((chip_family = CF_P18F_A) or (chip_family = CF_P18F_B) or (chip_family = CF_P18F_D) or
      (chip_family = CF_P18F_E) or (chip_family = CF_P18F_F) or (chip_family = CF_P18F_Q)) then
  begin
    if (aprogram = 1) then
    begin
      pages_performed:= 0;

      //erase whole device
      if (chip_family = CF_P18F_A) then
        p18a_mass_erase();
      if (chip_family = CF_P18F_B) then
        p18b_mass_erase();
      if (chip_family = CF_P18F_D) then
        p18d_mass_erase();
      if (chip_family = CF_P18F_E) then
        p18e_mass_erase();
      if ((chip_family = CF_P18F_F) or (chip_family = CF_P18F_Q)) then
        p16c_mass_erase();

      if (verbose > 0) then
        printf('Programming FLASH (%d B in %d pages per %d bytes): \n',[flash_size,flash_size/page_size,page_size]);
      fflush(stdout);

      i:= 0;
      while (i < flash_size) do
      begin
        if (is_empty(progmem[i], page_size) = False) then
        begin
 	  if ((chip_family = CF_P18F_D) or (chip_family = CF_P18F_E)) then
   	    p18d_write_page(progmem[i], i, page_size)
   	  else if (chip_family = CF_P18F_F) then
   	    p16c_write_page(progmem[i], i * 2, page_size)
   	  else if (chip_family = CF_P18F_Q) then
   	    p18q_write_page(progmem[i], i * 2, page_size)
   	  else
   	    p18a_write_page(progmem[i], i, page_size);
          inc(pages_performed);
          if (verbose > 1) then
   	  begin
   	    printf('#');
   	    fflush(stdout);
   	  end;
        end
        else if (verbose > 2) then
        begin
          printf('.');
          fflush(stdout);
        end;

        i:= i + page_size;
      end;

      if (verbose > 0) then
      begin
        printf('\n%d pages programmed\n',[pages_performed]);
        printf('Programming config\n');
      end;

      //write config bytes for PIC18Fxxxx and 18FxxKxx devices
      i:=0;
      while (i < config_size) do
      begin
        if (chip_family = CF_P18F_A) then
          p18a_write_cfg(Ord(config_bytes[i]), Ord(config_bytes[i + 1]), $300000 + i);
        if (chip_family = CF_P18F_D) then
          p18d_write_cfg(Ord(config_bytes[i]), Ord(config_bytes[i + 1]), $300000 + i);
        if (chip_family = CF_P18F_E) then
          p18d_write_cfg(Ord(config_bytes[i]), Ord(config_bytes[i + 1]), $300000 + i);
        if (chip_family = CF_P18F_F) then
          p16c_write_single_cfg(Ord(config_bytes[i + 1]), Ord(config_bytes[i]), $300000 + i);
        if (chip_family = CF_P18F_Q) then
          p18q_write_single_cfg(Ord(config_bytes[i + 1]), Ord(config_bytes[i]), $300000 + i);

        i:= i + 2;
      end;
      //for PIC18FxxJxx, config bytes are at the end of FLASH memory
    end;
    if (verify = 1) then
    begin
      pages_performed:= 0;

      if (verbose > 0) then
        printf('Verifying FLASH (%d B in %d pages per %d bytes): \n',[flash_size, flash_size div page_size, page_size]);

      i:= 0;
      while (i < flash_size) do
      begin
        if (is_empty(progmem[i], page_size)) then
        begin
          if (verbose > 2) then
          begin
            printf('#');
            fflush(stdout);
          end;
        end
        else
        begin
   	  if ((chip_family = CF_P18F_F) or (chip_family = CF_P18F_Q)) then
   	    p16c_read_page(tdat, i * 2, page_size)
   	  else
   	    p18a_read_page(tdat, i, page_size);
          inc(pages_performed);
          if (verbose > 3) then
            printf('Verifying page at 0x%4.4X\n',[i]);
          if (verbose > 1) then
          begin
            printf('#');
            fflush(stdout);
          end;
          for j:=0 to (page_size - 1) do
          begin
            if (progmem[i+j] <> Chr(tdat[j])) then
            begin
              printf('Error at 0x%4.4X E:0x%2.2X R:0x%2.2X\n',[i+j,progmem[i+j],tdat[j]]);
              printf('Exiting now\n');
              prog_exit_progmode();
              Exit;
            end;
          end;
        end;

        i:= i + page_size;
      end;
      if (verbose > 0) then
        printf('\n%d pages verified\n',[pages_performed]);
      if ((chip_family = CF_P18F_F) or (chip_family = CF_P18F_Q)) then
        p16c_read_page(tdat,$300000*2,page_size)
      else
        p18a_read_page(tdat,$300000,page_size);

      if (verbose > 0) then
        printf('Verifying config...');
      for i:= 0 to (config_size - 1) do
      begin
        if (config_bytes[i] <> Chr(tdat[i])) then
        begin
          printf('Error at 0x%2.2X E:0x%2.2X R:0x%2.2X\n',[i,config_bytes[i],tdat[i]]);
          printf('Exiting now\n');
          prog_exit_progmode();
          Exit;
   	end;
      end;
      if (verbose > 0) then
        printf ('OK\n');
    end;
  end
  else
  begin
    if (aprogram = 1) then
    begin
      if ((chip_family = CF_P16F_A) or (chip_family = CF_P16F_B) or (chip_family = CF_P16F_D)) then
        p16a_mass_erase();
      if (chip_family = CF_P16F_C) then
        p16c_mass_erase();
      //pointer reset is needed before every 'big' operation
      if ((chip_family = CF_P16F_A) or (chip_family = CF_P16F_B) or (chip_family = CF_P16F_D)) then
        p16a_rst_pointer();

      if (verbose > 0) then
        printf('Programming FLASH (%d B in %d pages)',[flash_size, flash_size div page_size]);
      fflush(stdout);

      i:= 0;
      while (i < flash_size) do
      begin
        if (verbose > 1) then
        begin
          printf('.');
          fflush(stdout);
        end;
        if ((chip_family = CF_P16F_A) or (chip_family = CF_P16F_B) or (chip_family = CF_P16F_D)) then
          p16a_program_page(i, page_size, 0);
        if (chip_family = CF_P16F_C) then
          p16c_write_page(progmem[i], i, page_size);

        i:= i + page_size;
      end;
      if (verbose > 0) then
        printf ('\n');
      if (verbose > 0) then
        printf('Programming config\n');
      if ((chip_family = CF_P16F_A) or (chip_family = CF_P16F_B) or (chip_family = CF_P16F_D)) then
        p16a_program_config();
      if (chip_family = CF_P16F_C) then
        p16c_write_cfg();
    end;
    if (verify = 1) then
    begin
      if (verbose > 0) then
        printf('Verifying FLASH (%d B in %d pages)', [flash_size, flash_size div page_size]);
      fflush(stdout);
      if ((chip_family = CF_P16F_A) or (chip_family = CF_P16F_B) or (chip_family = CF_P16F_D)) then
        p16a_rst_pointer();
      i:= 0;
      while (i < flash_size) do
      begin
        if (verbose > 1) then
        begin
          printf('.');
          fflush(stdout);
        end;
        if ((chip_family = CF_P16F_A) or (chip_family = CF_P16F_B) or (chip_family = CF_P16F_D)) then
          p16a_read_page(tdat,page_size);
        if ((chip_family = CF_P16F_C)) then
          p16c_read_page(tdat,i,page_size);
        for j:= 0 to (page_size - 1) do
        begin
          if (file_image[i + j] <> tdat[j]) then
          begin
            printf('Error at 0x%4.4X E:0x%2.2X R:0x%2.2X\n', [i + j, file_image[i + j], tdat[j]]);
            prog_exit_progmode();
            Exit;
          end;
        end;

        i:= i + page_size;
      end;

      if (verbose > 0) then
        printf ('\n');
      if (verbose > 0) then
        printf('Verifying config\n');

      if ((chip_family = CF_P16F_A) or (chip_family = CF_P16F_B) or (chip_family = CF_P16F_D)) then
      begin
        config:= p16a_get_config(7);
        econfig:= ((file_image[2 * $8007]) shl 0) + ((file_image[(2 * $8007) + 1]) shl 8);
        if (config = econfig) then
        begin
          if (verbose > 1) then
            printf('config 1 OK: %4.4X\n',[config]);
        end
        else
          printf('config 1 error: E:0x%4.4X R:0x%4.4X\n',[config, econfig]);
        config:= p16a_get_config(8);
        econfig:= ((file_image[2 * $8008]) shl 0) + ((file_image[2 * $8008 + 1]) shl 8);
        if (config = econfig) then
        begin
          if (verbose > 1) then
            printf('config 2 OK: %4.4X\n',[config]);
        end
        else
          printf('config 2 error: E:0x%4.4X R:0x%4.4X\n',[config, econfig]);
      end;
      if (chip_family = CF_P16F_C) then
      begin
        p16c_read_page(tdat, $8007 * 2, page_size);
        for j:= 0 to 9 do
        begin
          if (Ord(config_bytes[j]) <> tdat[j]) then
          begin
            printf('Error at 0x%4.4X E:0x%2.2X R:0x%2.2X\n',[i + j, config_bytes[j], tdat[j]]);
            prog_exit_progmode();
            Exit;
          end;
        end;
      end;
    end;
  end;

  prog_exit_progmode();
  Result:= True;
end;

end.

