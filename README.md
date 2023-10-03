# A 6502toy

(this file is a stub)

## Introduction

I had a Apple II. The Furby was a 6502 'like' toy. I want make SBC minimal as my toy.

There are many "clones" [Ben Eater](https://eater.net/6502), [Mike](https://github.com/mike42/6502-computer), [Maarten](https://github.com/maarten-pennings/6502/tree/master), [Grant](http://searle.x10host.com/6502/Simple6502.html) and many others.

And a Great support from [6502.org](http://6502.org/) forum and [Wilson Mines Co](http://wilsonminesco.com/).

This is a board for testing the version for 6502 of Minimal Indirect Thread Code Forth as [IMMU](https://github.com/agsb/immu)

PS. [Ben Eater](https://www.youtube.com/watch?v=LnzuMJLZRdU) made a revival of 6502. 

## Road Map

- make all circuit plans
- make a eeprom programmer like [Ben Eater](https://github.com/beneater/eeprom-programmer), but using arduino mini and change pins used.
- make a nop circuit to test cpus, using a arduino mini and [clockvar6502](https://github.com/maarten-pennings/6502/blob/master/1clock/clockvar6502)
        

## Hardware

Use wire wrap. 

Test NMOS-6502, CMOS-65C02,

The memory map will be as in Apple II, $0000-$BFFF 48k RAM, $C000-$CFFF 4k Devices, $D000-$FFFF 12k ROM. 

The board will have one 6551 ACIA and two 6522 VIA inside, with expansion of more 4 devices.

The devices are mapped using a 3:8 74HC138 decoder, as $0 reserved, $1 ACIA, $2 VIA, $3 VIA, onboard and $4 to $7 at expansion.

A clock board by crystal of 1.8432 for VIAs and by 74HC74 of 0.9612 for CPU, with a Real Timer Click as NMI with 10ms delay using a VIA T1 timer.

No video or keyboard, using terminal at USART 19200 8N1, RS-232, vt-100.

Could use I2C and SPI protocols and devices.

Use [65SIB](http://forum.6502.org/viewtopic.php?t=1064&start=105)


## Software

Use a BIOS and Forth, 

Use USART for VT100 terminal, Tera-Term or PuTTy at computer.


## Memory 

The first RAM $0000-$03FF 1k reserved for system, and $00F0-$00FF 16 bytes at page zero. 

The Extended Wozniac Monitor uses RAM at $0200-$027F as terminal input buffer and $0024-$002B at page zero. 

The page $0300-$03FF is a list of references for devices and system (BIOS, MOS) routines.

The pages $0400-$0FFF are reserved for future expansion, programs start at $1000 till $BFFF.

The Forth uses RAM from $1000 to $BFFF and $00E0-$00EF 16 bytes at page zero.




