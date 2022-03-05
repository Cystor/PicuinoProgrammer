# PicuinoProgrammer

## What is it?

This application was created in Lazarus on Windows (and in the future for linux distros) with the aim to help to program 8-bit PIC devices more easily from serial port through AVR (Arduino), which it is low cost and also very available. It's a user interface to the executable [Micro progmeter](https://github.com/jaromir-sukuba/micro_progmeter) and needs some files of that project to work (see more below).

![preview](https://user-images.githubusercontent.com/74105086/156858529-0c2b3ac1-662b-43c9-9e55-643fdca54aba.png)

WARNING: Use at your own risk, WE DO NOT PROVIDE GUARANTEE of any kind of malfunction or damage of devices.

Some boards already tested:
- Arduino Nano;
- Arduino Uno;
- Arduino Mega 2560;
- And some "knock-off boards".

Compatible with many PIC devices which has:
- 8-bit processors;
- Flash program memory;
- Programming Voltage of 5 volts on MCLR pin.

For more about compatibility [click here](https://github.com/jaromir-sukuba/a-p-prog/blob/master/README.md#supported-devices)

## How to use it?

Before anything, you must setup your hardware. [Click here](https://github.com/jaromir-sukuba/a-p-prog/blob/master/README.md#hardware) to see how.

Within the same folder of "PicuinoProgrammer.exe" must have a folder called "MicroProgmeter" and inside of it must have the following files:

`pp3.exe`
`pp3_devices.dat`

Now all you have to do is: search your hex file, select your target device, COM port and click in "Program PIC Device"

## Help us

We would apreciate any help, suggestion and bug report.
Thanks a lot for using this solution.
