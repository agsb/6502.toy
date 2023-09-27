# 6502toy

(this file is a stub)

## introduction

I had a Apple II. Ben Eater makes a revival of 6502. Great support from 6502.org forum.

This is a board for testing the version for 6502 of Minimal Indirect Thread Code Forth as [IMMU](https://github.com/agsb/immu)



## road map

    make all circuit plans
    make a eeprom programmer like the one by Ben Eater, but using arduino mini and change pins used.
    make a noop circuit to test cpus.
        

### hardware

Use wire wrap.

Test NMOS-6502, CMOS-65C02,

The memory map will be as in Apple II, $0000-$BFFF 48k RAM, $C000-$CFFF 4k Devices, $D000-$FFFF ROM.

The board will have one 6551 ACIA and two 6522 VIAs inside, with expansion of more four devices.

The devices are mapped using a 3:8 74HC138 decoder, as $0 reserved, $1 ACIA, $2 VIA1, $3 VIA2, onboard and $4 to $7 at expansion.

No video or keyboard, using terminal at USART 19200, 8N1.

### software

Use a BIOS and Forth, 

Use USART for VT100 terminal, Tera-Term or PuTTy at computer.





