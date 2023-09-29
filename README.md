# 6502toy

(this file is a stub)

## introduction

I had a Apple II. [Ben Eater](https://www.youtube.com/watch?v=LnzuMJLZRdU) makes a revival of 6502. 

There are many "clones" [6502](https://github.com/maarten-pennings/6502/tree/master)

And a Great support from [6502.org](http://6502.org/) forum.

This is a board for testing the version for 6502 of Minimal Indirect Thread Code Forth as [IMMU](https://github.com/agsb/immu)



## road map

- make all circuit plans
- make a eeprom programmer like [Ben Eater](https://github.com/beneater/eeprom-programmer), but using arduino mini and change pins used.
- make a noop circuit to test cpus, using a arduino mini and [clockvar6502](https://github.com/maarten-pennings/6502/blob/master/1clock/clockvar6502)
        

### hardware

Use wire wrap.

Test NMOS-6502, CMOS-65C02,

The memory map will be as in Apple II, $0000-$BFFF 48k RAM, $C000-$CFFF 4k Devices, $D000-$FFFF ROM.

The board will have one 6551 ACIA and one 6522 VIA inside, with expansion of more five devices.

The devices are mapped using a 3:8 74HC138 decoder, as $0 reserved, $1 ACIA, $2 VIA, onboard and $3 to $7 at expansion.

No video or keyboard, using terminal at USART 19200, 8N1.

### software

Use a BIOS and Forth, 

Use USART for VT100 terminal, Tera-Term or PuTTy at computer.





