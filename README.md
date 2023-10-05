# A 6502toy

(this file is a stub)

## Introduction

_I seek for learn, not for speed._

I had a Apple II. The Furby was a two 6502 'like' toy. I want make a SBC minimal as my toy.

There are many "clones" [Ben Eater](https://eater.net/6502), [Mike](https://github.com/mike42/6502-computer), [Maarten](https://github.com/maarten-pennings/6502/tree/master), [Grant](http://searle.x10host.com/6502/Simple6502.html), [Álvarez](https://www.ele.uva.es/~jesus/6502copy/proto.html) and others.

And a Great support from [6502.org](http://6502.org/) forum and [Wilson Mines Co](http://wilsonminesco.com/).

This is a board for learn and testing the version for 6502 of Minimal Indirect Thread Code Forth as [IMMU](https://github.com/agsb/immu)

PS. 
        [Ben Eater](https://www.youtube.com/watch?v=LnzuMJLZRdU) made a revival of 6502. 
        
      
## Road Map

- make all circuit plans
- make a eeprom programmer like [Ben Eater](https://github.com/beneater/eeprom-programmer), but using arduino mini and change pins used.
- make a nop circuit to test cpus, using a arduino mini and [clockvar6502](https://github.com/maarten-pennings/6502/blob/master/1clock/clockvar6502)
        

## Hardware

Use wire wrap. 

Test NMOS-6502, CMOS-65C02,


The actual memory map will be 60k RAM $0000-$BFFF, 2k Devices $F000-$F7FF and $F800-$FFFF 2k ROM. 

The board will have one 6551 ACIA and two 6522 VIA inside, with expansion of more 4 devices.

The devices are mapped at $F000, $F010 to $F0F0, reserving 16 bytes for control for each device. Also using a 3:8 74HC138 decoder, with $0 reserved, $1 ACIA, $2 VIA, $3 VIA, onboard and $4 to $7 at expansion.

A clock board by crystal of 1.8432 for CIA and by 74HC74 of 0.9612 for CPU, with a Real Timer Click as NMI with 10ms delay using a VIA T1 timer.

No video or keyboard, using terminal at USART 19200 8N1, RS-232, vt-100.

Using a LCD 16x2, a keyboard with 6 keys and some led for minimal use.

Using I2C and SPI protocols and devices. 

The board have 2 8-pins slots for I2C epproms.

Use [65SIB](http://forum.6502.org/viewtopic.php?t=1064&start=105)

## Software

At boot, the bios will copy bytes from a I2C 64k eeprom into RAM at $1000 and jump to $1000.

Use a BIOS and Forth, 

Use USART for VT100 terminal, Tera-Term or PuTTy at computer.

### Bios

generic routines at bios:

    - clock tick
    - copycat page
    - copycat eeprom
    - getch
    - putch
    - getline
    - putline

## Memory 

The first 4k of RAM $0000-$0FFF is reserved for system.

The $03FF 1k reserved for system, and $E0-$FF 32 bytes at page zero. 

The Extended Wozniac Monitor uses RAM at $0200-$027F as terminal input buffer and $24-$2B at page zero. 

The page $0300-$03FF is a list of references for devices and system (BIOS, MOS) routines.

The pages $0400-$0FFF are reserved for future expansion, programs start at $1000.

The Forth uses RAM from $1000 to $EFFF and $00D0-$00DF 16 bytes at page zero.

## Ideas

- The initial memory map was to be like an Apple II, $0000-$BFFF 48k RAM, $C000-$CFFF 4k Devices, $D000-$FFFF 12k ROM.
- Use 64k RAM and take all write as RAM and reads depends of a latch.

## Links

https://6502.org

http://wilsonminesco.com/

https://www.ele.uva.es/~jesus/6502copy/proto.html


