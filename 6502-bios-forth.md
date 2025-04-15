

# WIP not finished

## Hardware
    
## BIOS

## Forth

### Primitives

```
    inner core: 

        unnest, next, pick, nest, link, jump,
        push, pull, execute, setup, interrupt, exit
        drop, dup, swap, over, rot, -rot,
        and, or, xor, +, -, 2/, 2*, 2+, 2-, 1+, 1-,
        r>, >r, r@, sp@, rp@, sp!, rp!,
        for, next, -1, 0, 1, 2, 3, 4, 
        putc, getc, BL, CR,
        DP, here, alloc, comma, colon, semmis,
        find, tick, 
        expect, word, parse, base, number,
        docon, dovar, dodoes, pscode,

   the 'bare bones' BIOS must have:

       millis,     return milliseconds
       getch,      get a byte from USART/COMM port
       putch,      put a byte into USART/COMM port
       getchq,     verify if get a byte from USART/COMM port
       putchq,     verify if put a byte into USART/COMM port
       getio,      get a byte from a GPIO/DEVICE
       putio,      put a byte into a GPIO/DEVICE
       timer,      set, get, start, stop
    
    i2c core:
       
```

## References

   The R65F02   6502 Forth processor by Rockwell

   The history of Fig-Forth roots is at [Through The Forth
   Jungle](https://www.forth.org/svfig/kk/10-2021-Ragsdale.pdf)

    The [6502 forum](https://6502.org) 
   
