# A 6502 toy

(this file is a stub)

## Introduction

_I seek for learn, not for speed._

I had an Apple II. The Furby was a two 6502 'like' toy. 

I want make a ~SBC minimal~ as my toy.

We know the Terminator’s main CPU is a 6502.

There are many "clones" [Ben Eater](https://eater.net/6502), [Mike](https://github.com/mike42/6502-computer), [Maarten](https://github.com/maarten-pennings/6502/tree/master), [Grant](http://searle.x10host.com/6502/Simple6502.html), [Álvarez](https://www.ele.uva.es/~jesus/6502copy/proto.html) and others.

And a Great support from [6502.org](http://6502.org/) forum and [Wilson Mines Co](http://wilsonminesco.com/).

This is a board for learn and testing the version for 6502 of Minimal Indirect Thread Code Forth as [IMMU](https://github.com/agsb/immu)

PS.

- [Ben Eater](https://www.youtube.com/watch?v=LnzuMJLZRdU) made a revival of 6502. 
- using ROM, for read-only memory, never changes
- using RAM, for random access memory, ever changes
- using REM, for regular eeprom memory, can changes (I2C, SPI, etc)
        
## Road Map

- make all circuit plans (W.I.P.)
- make a eeprom programmer like [Ben Eater](https://github.com/beneater/eeprom-programmer), but using arduino nano and some change in pins used.
- make a nop circuit to test cpus, using a arduino mini and [clockvar6502](https://github.com/maarten-pennings/6502/blob/master/1clock/clockvar6502)
- wire wrap the boards
          
## Hardware

### B.O.M

        - 01 x NMOS 6502 (or CMOS 65C02),
        - 01 x AT28C64, 8kb EEPROM
        - 02 x HM62256, 32kb SRAM

        - 01 x CMOS 6522, VIA Interface
        - 01 x CMOS 6551, CIA Interface

        - 02 x 74HC00, 4x2 NAND
        - 01 x 74HC04, 6x NOT
        - 01 x 74HC27, 3x3 NOR
        - 01 x 74HC10, 3x3 NAND
        - 01 x 74HC74, 2x D-FlipFlop
        - 01 x 74HC138, 3:8 DeMux

        - 08 x 3k3 Ohm, common pull-up
        - 08 x 10k0 Ohm,  weak pull-up
        - n x  0.1 uF, decoupling
        - 02 x 4.7 uF, buffer
        - 01 x crystal 1.8432 MHz,
        - 01 x  TS1813, reset delay

        - n  x color leds,
        - n  x small resistors,

        - 02 x AT24LC512, 64kb I2C EEPROM

## Topics

The 6502's default frequency of about 1 MHz is merely illustrative compared to the 3.6 GHz of current CPUs.
   
The USART is at 9600/19200 bps, 8-N-1, duplex. 

The I2C EEPROM used as permanent storage, 1 kb blocks, 64 bytes screens.
        
All devices are CMOS and 5V capable.        

Use wire wrap. 

Test NMOS-6502, CMOS-65C02,

The actual memory map will be 56k SRAM $0000-$DFFF, 8k EEPROM $E000-$FFFF and $FE00-$FEFF 256b for devices. 

The devices are mapped at $FE00 to $FEFF, reserving 16 bytes for control for each of 16 devices.

The board will have one 6551 ACIA and one (two?) 6522 VIA inside, with expansion of more devices.

Also using a 3:8 74HC138 decoder, with $0 reserved, $1 ACIA, $2 VIA, $3 VIA reserved, onboard and $4 to $7 for expansion.

A clock board by crystal of 1.8432 MHz for CIA and by 74HC74 of 0.9612 MHz for CPU.

A beat tick as NMI with 10ms delay using VIA T1 timer.

No VGA video or 101 keyboard, using terminal at USART 9600/19200 8N1, RS-232, vt-100. 

Using a LCD 16x4 by I2C, a keyboard with 6/12 keys, a beeper, a speaker and a pool of leds, for minimal use.

Using I2C and SPI protocols and devices. 

The board have two extra 8-pins DIPs slots for I2C epproms.

Use MicroChip [AARDVARK](https://onlinedocs.microchip.com/oxy/GUID-A21CD3C1-401E-4D3B-A45B-78E60737CD44-en-US-8/GUID-92136A89-CE06-46A2-8F03-80317EDD4F89.html) connector, for interface I2C and SPI

Use [UEXT](https://en.wikipedia.org/wiki/UEXT) interface for USART, I2C, SPI, 3V3, 5V0, GND

Use [65SIB](http://forum.6502.org/viewtopic.php?t=1064&start=105) for alternative interface.

Use a 64 pin connector for expansion modules.

## Software

At boot BIOS does have all functions and loads a monitor to RAM, or 

At boot, the bios will copy bytes from a I2C eeprom into RAM and jump to it.

Use a BIOS and Forth, 

Use USART for VT100 terminal, FDDI/USB for Tera-Term or PuTTy at a computer.

### Bios

Consider ram is static random memory, rom is eeprom memory and rem is flash memory

generic routines at bios:

    - ram2ram, copy bytes from RAM to RAM
    - rem2ram, copy bytes from REM to RAM
    - ram2rem, copy bytes from RAM to REM
    
    - set_spi, prepare spi
    - set_i2c, prepare i2c
    - set_sram, prepare sram page
    
    - getc, uart get a char 
    - putc, uart put a char
    
    - getcq, verify to get
    - putcq, verify to put
    
    - getp, generic port get
    - putp, generic port put
    
    - beep, as is
    - tone, play a tone
    
    - tick_put, write into counter
    - tick_get, read from counter
    - tick_run, starts counter
    - tick_hlt, stops counter
    
    - roulette, random number from 0 to 36
    - seed, random seed from 0 to $FFFF
    - random, returns a random number from 0 to $FF

    - hex2bin, convert a hexagesimal ascii to byte ($00 to $FF)
    - bin2hex, convert a byte to hexagesimal ascii ($00 to $FF)
    
## Memory 

The first 4k of RAM $0000-$0FFF is reserved for system.

The $03FF 1k reserved for system, and $E0-$FF 32 bytes at page zero. 

The Extended Wozniac Monitor uses RAM at $0200-$027F as terminal input buffer and $24-$2B at page zero. 

The page $0300-$03FF is a list of references for devices and system (BIOS, MOS) routines.

The pages $0400-$0FFF are reserved for future expansion, programs start at $1000.

The Forth uses RAM from $1000 to $DFFF, $00D0-$00DF 16 bytes at page zero and ???? to ???? at page one.

## Ideas

The hardware is 32k SRAM-1 + 32k SRAM-0 + 8k EEPROM (shadow) and devices. Two HM62256 and an AT28C64.

Usually memory banks are maped after the first 16kb. I plan use the first 32kb, $0000-$7FFF, for memory banks and the second 32kb, $8000-$FFFF for fixed memory space. This allow multiple tasks, one per bank, instantaneous context switch, without overhead of copy and swap, and shared buffers within tasks. 

Note the fixed SRAM is named SRAM-0 and hold at 0x8000 to 0xFFFF, with 256 bytes for devices and shadow the 0xE000 to 0xFFFF of EEPROM. This alllows use the A15 line as bank SRAM selector.

As reference, the Apple II memory map, uses $0000-$BFFF 48k RAM, $C000-$CFFF 4k Devices, $D000-$FFFF 12k ROM.

### Memory banks by 6522

Use 32k SRAM-1 + banks x 32k SRAM-N over 0x0000 to 0x7FFF, mapped by PB0-PB3 of 6522, + 32k SRAM-0 fixed, eeprom shadow at last 8k. Each bank is a task and fixed RAM as shared memory; 

Maybe, a module for 8 x 32k HM62256 SRAMs, selected by a 74HC138 and some control glue logics.

1. at boot both SRAM-0 and SRAM-1 onboard must be selected;
2. at boot the expansion module is not active but could be checked if plugged;
3. the bank-0 is always active, at 0x8000, selected by A15
4. a bios call does active the default bank-1, on board and disables expansion;
6. a bios call does active another 32k memory of bank-2 to bank-9 at 0x0000;

### Memory banks illegal opcodes

Use the illegal opcode 3 and B, for select SRAM BANKs of 32kb. Take SYNC, /A15, A0, A1, /A2 as selector and A3, A4, A5, A6, A7 as pseudo A15, A16, A17, A18, A19 for 32 banks of 32k, eg. two AS64008

## Modules

### Keyboard minimal

Use a ATmega328 with RST, RX, TX, GND, PWR, with a serial conector and direct conection (lines) for 16 keys switches, in split format.

### FAT32 SD 

All support for SD FAT32 borrow from [SDCARD6502](https://github.com/gfoot/sdcard6502/tree/master)

## Links

https://6502.org

http://wilsonminesco.com/

https://www.ele.uva.es/~jesus/6502copy/proto.html

https://www.youtube.com/watch?v=LrPxM-qYNTI


