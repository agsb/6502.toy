;--------------------------------------------------------
; /*
;  *  DISCLAIMER"
;  *
;  *  Copyright © 2020, Alvaro Gomes Sobral Barcellos,
;  *
;  *  Permission is hereby granted, free of charge, to any person obtaining
;  *  a copy of this software and associated documentation files (the
;  *  "Software"), to deal in the Software without restriction, including
;  *  without limitation the rights to use, copy, modify, merge, publish,
;  *  distribute, sublicense, and/or sell copies of the Software, and to
;  *  permit per0ons to whom the Software is furnished to do so, subject to
;  *  the following conditions"
;  *
;  *  The above copyright notice and this permission notice shall be
;  *  included in all copies or substantial portions of the Software.
;  *
;  *  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;  *  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;  *  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE and
;  *  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;  *  LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;  *  OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;  *
;  */
;
;   LICENSE: http://creativecommons.org/licenses/by-nc-sa/4.0/
;
;--------------------------------------------------------
;
;	minimal bios for 6502toy
;
;--------------------------------------------------------
.IF 0

# Easy BIOS or MOS for a 6502toy

## Memory map
   
    $0000 to $EFFF, RAM
    
    $F000 to $F7FF, DEVICES

    $F800 to $FFFF, ROM 

### RAM

    $0000 to $00FF page zero, hardware registers
    
    $0100 to $01FF page one,  hardware stack
    
    $0200 to $02FF page two,  bios buffers
    
    $0300 to $03FF page tri,  list of devices and routines
   
    $0400-$0FFF  3 kb bios 

    $1000-$E7FF 56 kb user 

### DEVICES

    mapped onboard:
  
    $F000   bios exclusive hardware
    
    $F010   bios device 01, acia 6551, USART TERMINAL
    
    $F020   bios device 02,  via 6522, CLOCK, I2C, SPI, LCD, KBD
    
    $F030   bios device 03,  via 6522, user
    
    $F040   external device 04
    
    $F050   external device 05
    
    $F060   external device 06
    
    $F070   external device 07
    
    for expansion:
    
    $F080 to $F7FF, free  

### ROM
    
    $F800 to $FFFF  2k ROM 

    At boot:
    
    1. Copy the NMI/IRQ address to page $0300. 

    2. Copy I2C EEPROM to $0400 and jump ($400). 

    4. Shadow BIOS stay at $F800 to $FFFF.

### Interrupts

    the clock tick done by VIA_T1 is the only NMI of 6502toy.
    
### Best Practices

    - On return, the carry flag is 0 for Ok.

    - callee saves registers.

    - do not make recursive routines.

    - using RAM for random access memory, as static ram with read and write.

    - using ROM for read only memory, as flash eeprom parallel. NO WRITE.
    
    - using REM for regular eeprom memory, as flash eeprom with I2C protocol.

.ENDIF
;--------------------------------------------------------
;
;   enable some ca65
;
; enable listing

.list on

; identifiers

.case +

; debug

.debuginfo +

; enable features

.feature c_comments

.feature string_escapes

.feature org_per_seg

.feature dollar_is_pc

.feature pc_assignment

; enable 6502 mode

.p02

;--------------------------------------------------------
; constants
;--------------------------------------------------------
;   task states

    HALT    = 0
    IDLE    = 1
    WAIT    = 2
    BUSY    = 3

;--------------------------------------------------------
; devices
 
    DEVS = $F000

;--------------------------------------------------------
; terminal usart
    CIA       =  DEVS+$10    ; The base address of the 6551 ACIA.
    CIA_DATA  =  CIA+0   ; Its data I/O register
    CIA_RX    =  CIA+0   ; Its data I/O register
    CIA_TX    =  CIA+0   ; Its data I/O register
    CIA_STAT  =  CIA+1   ; Its  status  register
    CIA_COMM  =  CIA+2   ; Its command  register
    CIA_CTRL  =  CIA+3   ; Its control  register

;--------------------------------------------------------
; system 
    VIA        =  DEVS+$20 ; The base address of the 6522 VIA.
    VIA_PB     =  VIA+0    ; Its port B address
    VIA_PA     =  VIA+1    ; Its port A address
    VIA_DDRB   =  VIA+2    ; Its data-direction register for port B
    VIA_DDRA   =  VIA+3    ; Its data-direction register for port A
    VIA_T1CL   =  VIA+4    ; Its timer-1 counter's low  byte
    VIA_T1CH   =  VIA+5    ; Its timer-1 counter's high byte
    VIA_T1LL   =  VIA+6    ; Its timer-1 latcher's low  byte
    VIA_T1LH   =  VIA+7    ; Its timer-1 latcher's high byte
    VIA_T2CL   =  VIA+8    ; Its timer-2 counter's low  byte
    VIA_T2CH   =  VIA+9    ; Its timer-2 counter's high byte
    VIA_SR     =  VIA+10   ; The shift register
    VIA_ACR    =  VIA+11   ; The auxiliary  control register
    VIA_PCR    =  VIA+12   ; The peripheral control register
    VIA_IFR    =  VIA+13   ; The interrupt flag register
    VIA_IER    =  VIA+14   ; The interrupt enable register
    VIA_PAH    =  VIA+15   ; Its port A address no handshake

;--------------------------------------------------------
; user 
    TIA        =  DEVS+$30 ; The base address of the 6522 VIA.
    TIA_PB     =  TIA+0    ; Its port B address
    TIA_PA     =  TIA+1    ; Its port A address
    TIA_DDRB   =  TIA+2    ; Its data-direction register for port B
    TIA_DDRA   =  TIA+3    ; Its data-direction register for port A
    TIA_T1CL   =  TIA+4    ; Its timer-1 counter's low  byte
    TIA_T1CH   =  TIA+5    ; Its timer-1 counter's high byte
    TIA_T1LL   =  TIA+6    ; Its timer-1 latcher's low  byte
    TIA_T1LH   =  TIA+7    ; Its timer-1 latcher's high byte
    TIA_T2CL   =  TIA+8    ; Its timer-2 counter's low  byte
    TIA_T2CH   =  TIA+9    ; Its timer-2 counter's high byte
    TIA_SR     =  TIA+10   ; The shift register
    TIA_ACR    =  TIA+11   ; The auxiliary  control register
    TIA_PCR    =  TIA+12   ; The peripheral control register
    TIA_IFR    =  TIA+13   ; The interrupt flag register
    TIA_IER    =  TIA+14   ; The interrupt enable register
    TIA_PAH    =  TIA+15   ; Its port A address no handshake

;--------------------------------------------------------
; VIA port A

;   T1 is clock, NMI interrupt /;
;
; for USART
    USRX = (1 << 7)    
    USTX = (1 << 6)    

; for SPI
    MCS0 = (1 << 5)    
    MOSI = (1 << 4)
    MISO = (1 << 3)
    MSCL = (1 << 2)

; for I2C, with 10k pull-up drains 1 mA
    SDA  = (1 << 1)
    SCL  = (1 << 0)

;--------------------------------------------------------
; alias at page 

    buffer = $0200

    NMIVEC = $03FA

    RSTVEC = $03FC

    IRQVEC = $03FE

;--------------------------------------------------------
;   at page zero
;   $E0 to $EF bios reserved 16 bytes
;   $F0 to $FF bios reserved 16 bytes
;
* = $00E0

; clock
bios_tick:  .word $0, $0

; copycats
bios_a:  .byte $0
bios_s:  .byte $0
bios_x:  .byte $0
bios_y:  .byte $0
bios_p:  .byte $0

; extras
bios_z:  .byte $0	; keep zero
bios_r:  .byte $0
bios_w:  .byte $0

; generic
bios_f:  .byte $0
bios_g:  .byte $0
bios_h:  .byte $0
bios_t:  .byte $0

* = $00F0

bios_void: .word $0	; keep zero
; bios_tmp0 = bios_void + $0, reserved
bios_tmp1 = bios_void + $2
bios_tmp2 = bios_void + $4
bios_tmp3 = bios_void + $6
bios_tmp4 = bios_void + $8
bios_tmp5 = bios_void + $a
bios_tmp6 = bios_void + $c
bios_tmp7 = bios_void + $e

;--------------------------------------------------------

.byte $DE,$AD,$C0,$DE

;--------------------------------------------------------
; at $FFFA
.segment "VECTORS"

; hardware jumpers
.word _jump_nmi  ; fa ROM NMI vector
.word _jump_rst  ; fc ROM Reset vector
.word _jump_irq  ; fe ROM IRQ/BRK vector

;--------------------------------------------------------
;
.segment "ONCE"

_rollete:
.byte 00,32,15,19,04,21,02,25,17,34,06,27,13,36,11,30,08,23,10
.byte 05,24,16,33,01,20,14,31,09,22,18,29,07,28,12,35,03,26,00

;--------------------------------------------------------
;
; some code adapted from 6502.org forum
;
;--------------------------------------------------------
; interrups stubs, easy way
;
_jump_nmi:
    jmp (NMIVEC)

_jump_irq:
    jmp (IRQVEC)

_jump_rst:
    ; jmp (RSTVEC) never do that or hang ever boot
    jmp _rst_init

; void nmi,irq at boot

_nmi_init:

_irq_init:

    rti

;--------------------------------------------------------
; reset stub
;--------------------------------------------------------
_rst_init:

; real _init:
_init:
    ; disable interrupts
    sei

    ; no BCD math
    cld

    ; enable interrupts
    
    lda #<_bios_nmi
    sta NMIVEC+0
    lda #>_bios_nmi
    sta NMIVEC+1

    lda #<_bios_irq
    sta IRQVEC+0
    lda #>_bios_irq
    sta IRQVEC+1
    
    ; alive
    jsr beep

    ; setup acia one
    jsr _acia_init

    ; setup via one
    jsr _via_init 

    ; setup via two 
    jsr _tia_init

    ; setup clock
    jsr _clock_init

    ; stack: pull is decr, push is incr
    ldx #$FF
    txs
    
    ; alive
    jsr beep
    
    ; copy REM to RAM
    jsr _copyeep    

    ; alive
    jsr beep

    ; there we go....
    cli

    jsr _main

_main:
    ; insanity for safety
    jmp $400

;--------------------------------------------------------

.include "i2c.s"

;--------------------------------------------------------
; beep
beep:
    rts

;--------------------------------------------------------
; copy default eeprom to RAM
;
;   $FF00-$1000=$EF00
;   $EF00/$80 = $01DE
;   478 I2C pages of 128 bytes
;   Magics
;   rem_ptr = bios_tmp1
;   ram_ptr = bios_tmp2
;   len_ptr = bios_tmp3
;   device  = bios_tmp4+0
;   sense   = bios_tmp4+1
;   work    = bios_tmp5
;
;--------------------------------------------------------
_copyeep:
    ; use device 0 and sense read
    ; copy $E000 bytes from $1000 REM to $1000 RAM
    lda #$00
    sta bios_tmp1+0
    sta bios_tmp2+0
    sta bios_tmp3+0
    lda #$10
    sta bios_tmp1+1
    sta bios_tmp2+1
    lda #$E0
    sta bios_tmp3+1
    lda #%01010000	; hardcoded for 24LC512 at 000, will be << 1 
    sta bios_tmp4+0
    lda #$01
    sta bios_tmp4+1
    jsr _rem2ram
    rts

;========================================================
; NMI, counts ticks
_bios_nmi:
_bios_tick:
    cld
    bit VIA_T1CL
    inc bios_tick+0
    bne @ends
    inc bios_tick+1
    bne @ends
    inc bios_tick+2
    bne @ends
    inc bios_tick+3
@ends:
    rti

;======================================================================
; IRQ|BRK, handler
; easy minimal 
_bios_irq:
_bios_init_easy:
    cld
    sei
    sta bios_a
    stx bios_x
    sty bios_y
@easy:
    pla
    pha
    and #$10
    bne _bios_soft_easy
    beq _bios_hard_easy

;--------------------------------------------------------
_bios_ends_easy:
    ldy bios_y
    ldx bios_x
    lda bios_a
    cli
    rti

;--------------------------------------------------------
_bios_hard_easy:

@scan_via:
    bit VIA_IFR
    bpl @scan_tia
    jmp service_via
@scan_tia:
    bit TIA_IFR
    bpl @scan_cia
    jmp service_tia
@scan_cia:
    lda CIA_STAT
    bpl @scan_panic
    jmp service_cia
@scan_panic:
    jmp _bios_ends_easy

;--------------------------------------------------------
_bios_soft_easy:
    ;
    ; from a BRK, a software interrupt
    ; which always must be $00 $ZZ
    ; 
    ; the PC in stack minus one is the code $ZZ 
    ; for what break was called.
    ;
    ; do something somewhere sometime
    ;
    jmp _bios_ends_easy

;--------------------------------------------------------
;    attend interrupt 
service_via:
    pha
    lda #$7F
    sta VIA_IFR;
    jmp _bios_ends_easy

;--------------------------------------------------------
;    attend interrupt 
service_tia:
    pha
    lda #$7F
    sta TIA_IFR;
    jmp _bios_ends_easy

;--------------------------------------------------------
;    attend interrupt 
service_cia:
    jmp _bios_ends_easy

;======================================================================
;   acia_init, configures 19200,N,8,1 default 6551
;
_acia_init:
    lda #0
    sta CIA_STAT
    ; %0001 1110 =  9600 baud, external receiver, 8 bit , 1 stop bit
    ; %0001 1111 = 19200 baud, external receiver, 8 bit , 1 stop bit
    lda #$1F     
    sta CIA_CTRL
    ; %0000 1011 = no parity, normal mode, RTS low, INT disable, DTR low 
    lda #$0B     
    sta CIA_COMM
    rts

;----------------------------------------------------------------
;   verify thru 6551, no waits
;
hitc :  
@acia_ht:
; verify
    lda CIA_STAT
    and #8
    beq _nak
_ack:
    ; lda #$01
    clc
    rts
_nak:
    ; lda #$00
    sec
    rts

;----------------------------------------------------------------
;   receive a byte thru 6551, waits
;
getc :  
@acia_rx:
; verify
    lda CIA_STAT
    and #8
    ; beq @ends
    beq @acia_rx
; receive
    lda CIA_RX
    clc
    rts

;----------------------------------------------------------------
;   transmit a byte thru 6551, waits
;
putc :  
@acia_tx:
; verify
    pha
    lda CIA_STAT
    and #16
    ; beq @ends
    beq @acia_tx
; transmit
    pla                
    sta CIA_TX     
    clc
    rts

;--------------------------------------------------------
;   via_init, I2C, SPI, LCD, KBD, 
; 	pa7 utx, pa6 urx, 
;	pa5 mcs, pa4 mosi, pa3 miso, pa2 mscl, 
;	pa1 sda, pa0 scl
;
;	pb1 lcd4, pb2 lcd5, pb3 lcd6, pb4 lcd7
;	pb5 lcdR, pb6 lcdE
;
_via_init:
    lda #%01101100            
    sta VIA_DDRA
    lda #%01101100            
    sta VIA_DDRB
    rts
;--------------------------------------------------------
;
; clock tick, using VIA T1 free run 
; phi2 is 0.9216 MHz, 10ms is 9216 or $2400
;
_clock_init:
    ; store counter
    lda #$00
    sta VIA_T1CL
    lda #$24
    sta VIA_T1CH
    ; setup free-run and intrrupt at time-out
    lda VIA_ACR
    and #$7F    ;   %01111111
    ora #$40    ;   %01000000
    sta VIA_ACR

;--------------------------------------------------------
; start clock
clock_start:    
    lda #%11000000
    sta VIA_IER
    rts

;--------------------------------------------------------
; stop clock    
clock_stop:
    lda #%10000000
    sta VIA_IER
    rts

;--------------------------------------------------------
;   tia_init, extra VIA, future use
_tia_init:
    rts            

.byte $DE,$AD,$C0,$DE

;--------------------------------------------------------
