# A 6502 toy

(this file is a stub)

## Introduction

_I seek for learn, not for speed._

I had an Apple II. The Furby was a two 6502 'like' toy. I want make a SBC minimal as my toy.

There are many "clones" [Ben Eater](https://eater.net/6502), [Mike](https://github.com/mike42/6502-computer), [Maarten](https://github.com/maarten-pennings/6502/tree/master), [Grant](http://searle.x10host.com/6502/Simple6502.html), [Álvarez](https://www.ele.uva.es/~jesus/6502copy/proto.html) and others.

And a Great support from [6502.org](http://6502.org/) forum and [Wilson Mines Co](http://wilsonminesco.com/).

This is a board for learn and testing the version for 6502 of Minimal Indirect Thread Code Forth as [IMMU](https://github.com/agsb/immu)

PS.

- [Ben Eater](https://www.youtube.com/watch?v=LnzuMJLZRdU) made a revival of 6502. 
- using ROM, for read-only memory,
- using RAM, for random access memory, 
- using REM, for regular eeprom memory,
        
## Road Map

- make all circuit plans
- make a eeprom programmer like [Ben Eater](https://github.com/beneater/eeprom-programmer), but using arduino nano and change pins used.
- make a nop circuit to test cpus, using a arduino mini and [clockvar6502](https://github.com/maarten-pennings/6502/blob/master/1clock/clockvar6502)
        
## Hardware

### B.O.M

        - 01 x NMOS 6502 (or CMOS 65C02),
        - 01 x AT28C64
        - 01 x 62256,
        - 02 x AT24LC512, 
        - 02 x CMOS 6522
        - 01 x CMOS 6551
        - 02 x 74HC00,
        - 02 x 74HC32,
        - 01 x 74HC10,
        - 01 x 74HC74,
        - 01 x 74HC138,
        - 04 x 3k3 Ohm,
        - n x  0.1 uF,
        - 02 x 4.7 uF,
        - 01 x crystal 1.8432 MHz,
        - 01 x  TS1813
        - n  x color leds,
        - n  x small resistors,
    
The 6502's default frequency of about 1 MHz is merely illustrative, 
    compared to the 3.6 GHz of current CPUs.
   
The USART is at 19200 bps, 8-N-1, duplex. fixed, no changes.

The I2C EEPROM used as Hard Disk, 1k block, 64 bytes (screens).
        
All devices are CMOS and 5V capable.        

Use wire wrap. 

Test NMOS-6502, CMOS-65C02,

The actual memory map will be 56k SRAM $0000-$DFFF, 256b Devices $E000-$E0FF and $E100-$FFFF 8k ROM. 

The board will have one 6551 ACIA and two 6522 VIA inside, with expansion of more 4 devices.

The devices are mapped at $E000 to $E0FF, reserving 16 bytes for control for each device. 

Also using a 3:8 74HC138 decoder, with $0 reserved, $1 ACIA, $2 VIA, $3 VIA, onboard and $4 to $7 at expansion.

A clock board by crystal of 1.8432 for CIA and by 74HC74 of 0.9612 for CPU, with a Real Timer Click as NMI with 10ms delay using a VIA T1 timer.

No video or keyboard, using terminal at USART 19200 8N1, RS-232, vt-100.

Using a LCD 16x4 by I2C, a keyboard with 6 keys, a beeper, a speaker and a pool of leds, for minimal use.

Using I2C and SPI protocols and devices. 

The board have 2 x 8-pins slots for I2C epproms.

Use [UEXT](https://en.wikipedia.org/wiki/UEXT) interface for USART, I2C, SPI, 3V3

Use [65SIB](http://forum.6502.org/viewtopic.php?t=1064&start=105) for alternative interface.

## Software

                At boot BIOS does have all functions and loads a monitor to RAM, or 
                At boot, the bios will copy bytes from a I2C eeprom into RAM and jump to it.

Use a BIOS and Forth, 

Use USART for VT100 terminal, Tera-Term or PuTTy at computer.

### Bios

generic routines at bios:

    - ram2ram,        copy bytes from RAM to RAM
    - rem2ram,        copy bytes from REM to RAM
    - ram2rem,        copy bytes from RAM to REM
    
    - set_spi
    - set_i2c
    - set_sram
    
    - getc
    - putc
    - getcq
    - putcq
    
    - getp
    - putp
    
    - beep
    - tone
    - play
    
    - tick_put
    - tick_get
    - tick_run
    - tick_pause
    
    - roulette
    - seed
    - random

    
## Memory 

The first 4k of RAM $0000-$0FFF is reserved for system.

The $03FF 1k reserved for system, and $E0-$FF 32 bytes at page zero. 

The Extended Wozniac Monitor uses RAM at $0200-$027F as terminal input buffer and $24-$2B at page zero. 

The page $0300-$03FF is a list of references for devices and system (BIOS, MOS) routines.

The pages $0400-$0FFF are reserved for future expansion, programs start at $1000.

The Forth uses RAM from $1000 to $EFFF and $00D0-$00DF 16 bytes at page zero.

## Ideas

1. The initial memory map was to be like an Apple II, $0000-$BFFF 48k RAM, $C000-$CFFF 4k Devices, $D000-$FFFF 12k ROM.
2. Use 32k SRAM + 32k SRAM + 8k EEPROM (shadow)
3. Use 32k SRAM + banks x 16k SRAM + 8k SRAM + 8k EEPROM

## Links

https://6502.org

http://wilsonminesco.com/

https://www.ele.uva.es/~jesus/6502copy/proto.html

https://www.youtube.com/watch?v=LrPxM-qYNTI

