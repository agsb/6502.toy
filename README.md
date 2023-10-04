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

The original memory map was to be like an Apple II, $0000-$BFFF 48k RAM, $C000-$CFFF 4k Devices, $D000-$FFFF 12k ROM. 

The actual memory map will be $0000-$BFFF 60k RAM, $F000-$F7FF 2k Devices, $F800-$FFFF 2k ROM. 

At boot, the bios will copy bytes from a 64k eeprom I2C into RAM and jump to $1000.

The board will have one 6551 ACIA and two 6522 VIA inside, with expansion of more 4 devices.

The devices are mapped using a 3:8 74HC138 decoder, as $0 reserved, $1 ACIA, $2 VIA, $3 VIA, onboard and $4 to $7 at expansion.

A clock board by crystal of 1.8432 for CIA and by 74HC74 of 0.9612 for CPU and VIAS, with a Real Timer Click as NMI with 10ms delay using a VIA T1 timer.

No video or keyboard, using terminal at USART 19200 8N1, RS-232, vt-100.

Could use I2C and SPI protocols and devices.

Use [65SIB](http://forum.6502.org/viewtopic.php?t=1064&start=105)


## Software

Use a BIOS and Forth, 

Use USART for VT100 terminal, Tera-Term or PuTTy at computer.


### Bios

    - clock tick
    - copycat page
    - copycat eeprom
    - getch
    - putch
    - getline
    - putline

## Memory 

The first RAM $0000-$03FF 1k reserved for system, and $00F0-$00FF 16 bytes at page zero. 

The Extended Wozniac Monitor uses RAM at $0200-$027F as terminal input buffer and $0024-$002B at page zero. 

The page $0300-$03FF is a list of references for devices and system (BIOS, MOS) routines.

The pages $0400-$0FFF are reserved for future expansion, programs start at $1000.

The Forth uses RAM from $1000 to $EFFF and $00E0-$00EF 16 bytes at page zero.

## Ideas

- A ghost EEPROM, at boot copy self to RAM, disable EEPROM and copy self back.
- Use 64k RAM and take all write as RAM and reads depends of a latch.

## Links

https://6502.org

http://wilsonminesco.com/

https://www.ele.uva.es/~jesus/6502copy/proto.html


