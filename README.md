# Picuino Programmer

## What is it?

This application was created in Lazarus on Windows (and in the future for linux distros too) with the aim to help to program 8-bit PIC devices more easily from serial port through AVR (Arduino), which it is low cost and also very available. It's a user interface to the executable [a-p-prog
](https://github.com/jaromir-sukuba/a-p-prog) and needs some files of that project to work ([see more below](https://github.com/Cystor/PicuinoProgrammer#how-to-use-it)).

![preview](https://user-images.githubusercontent.com/74105086/156858529-0c2b3ac1-662b-43c9-9e55-643fdca54aba.png)

### WARNING:
Use at your own risk, WE DO NOT PROVIDE GUARANTEE of any kind of malfunction or damage of devices.

### Some boards already tested:
- Arduino Nano;
- Arduino Uno;
- Arduino Mega 2560;
- And some "knock-off boards".

### Compatible with many PIC devices which has:
- 8-bit processors;
- Flash program memory;
- Programming Voltage of 5 volts on MCLR pin.

[For more about compatibility click here.](https://github.com/jaromir-sukuba/a-p-prog/blob/master/README.md#supported-devices)

## How to use it?

Before anything, you must setup your hardware. [Click here to see how;](https://github.com/jaromir-sukuba/a-p-prog/blob/master/README.md#hardware)

You need to upload [this sketch](https://github.com/jaromir-sukuba/a-p-prog/blob/master/fw/pp/pp.ino) to Arduino;

Within the same folder of "PicuinoProgrammer.exe" must have a folder called "MicroProgmeter" and inside of it must have the following files:

- [pp3.exe](https://github.com/jaromir-sukuba/a-p-prog/blob/master/sw/pp3.exe)
- [pp3_devices.dat](https://github.com/jaromir-sukuba/a-p-prog/blob/master/sw/pp3_devices.dat)

Run "PicuinoProgrammer.exe".
Now all you have to do is: search your hex file, select your target device, COM port and click in "Program PIC Device".

## Help us

We would apreciate any help, suggestion and bug report.
Thanks a lot for using this solution.
