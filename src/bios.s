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
   
    $0400-$0FFF  3 kb reserved 

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
    
    $F800 to $FFFF  2k ROM ( minus 256, last page )

    At boot:
    
    1. Copy the last page $FF00 to thirth page $0300. 

    2. Reserve the pages at $0400 to $0FFF. 

    3. Copy I2C EEPROM to $1000 and jump to $1000. 

    4. The BIOS stay at $F800 to $FFFF.

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

;--------------------------------------------------------
; ascii 

    ESC_    =   27    ; ascii escape ^[
 
    XON_    =   17    ; ascii DC1 ^Q
    XOFF_   =   19    ; ascii DC3 ^S

    ACK_    =    6    ; ascii ACK ^F 
    NAK_    =   21    ; ascii NAK ^U also delete line.

    CR_     =   13    ; ascci carriage return ^M
    LF_     =   10    ; ascii line feed ^J
    
    BS_     =    8    ; ascii backspace ^H

    BL_     =   32    ; ascii space
    QT_     =   34    ; ascii double quotes \"

;--------------------------------------------------------
;   task or process states

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

    GHOSTS = $0300
    
    buffer = $0400

    SHADOWS = $FF00

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
bios_z:  .byte $0
bios_r:  .byte $0
bios_w:  .byte $0

; generic
bios_f:  .byte $0
bios_g:  .byte $0
bios_h:  .byte $0
bios_t:  .byte $0

* = $00F0

bios_void: .word $0
; bios_tmp0 = bios_void + $0, reserved
bios_tmp1 = bios_void + $2
bios_tmp2 = bios_void + $4
bios_tmp3 = bios_void + $6
bios_tmp4 = bios_void + $8
bios_tmp5 = bios_void + $a
bios_tmp6 = bios_void + $c
bios_tmp7 = bios_void + $e

;--------------------------------------------------------

.assert * > $FF, warning, "~ bios page zero over stack limit"

;--------------------------------------------------------
; at page
* = $FF00

BIOS_VECTOR:
; pointers of routines

.word blink	  ;
.word beep	  ;
.word _rem2map     ; 
.word _rem2ram     ; 
.word _ram2rem     ; 
.word _ram2ram     ; 
.word getc        ; 
.word putc        ; 
.word clock_stop  ; 
.word clock_start ; 
.word monitor     ; 

.byte $DE,$AD,$C0,$DE

;--------------------------------------------------------
.assert * > $FFFA, warning, "~ bios page last over vector limit"
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

    ; setup acia one
    jsr acia_init

    ; setup via one
    jsr via_init 

    ; setup via two 
    jsr tia_init

    ; setup clock
    jsr clock_init

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

    ; stack: pull is decr, push is incr
    ldx #$FF
    txs
    
    ; alive
    jsr beep
    
    ; copy REM to RAM
    jsr copyeep    

    ; there we go....
    cli

    jsr _main

_main:
    ; insanity for safety
    jmp $1000


;--------------------------------------------------------

.include "i2c.s"

;--------------------------------------------------------
; beep
beep:
    rts

;--------------------------------------------------------
; blink
blink:
    rts

;--------------------------------------------------------
; monitor
monitor:
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
copyeep:
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
;--------------------------------------------------------
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
;--------------------------------------------------------
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

;--------------------------------------------------------
;
; clock tick, using VIA T1 free run 
; phi2 is 0.9216 MHz, 10ms is 9216 or $2400
;
;--------------------------------------------------------
clock_init:
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
;   acia_init, configures 19200,N,8,1 default 6551
;
acia_init:
    pha            ; Push A to stack
    lda #0
    sta CIA_STAT
    ; %0001 1110 =  9600 baud, external receiver, 8 bit , 1 stop bit
    ; %0001 1111 = 19200 baud, external receiver, 8 bit , 1 stop bit
    lda #$1F     
    sta CIA_CTRL
    ; %0000 1011 = no parity, normal mode, RTS low, INT disable, DTR low 
    lda #$0B     
    sta CIA_COMM
    pla             ; Restore A
    clc
    rts

;----------------------------------------------------------------
;   acia_pull, receive a byte thru 6551, no waits
;
getc :  
acia_rx:
; verify
    sec
    lda CIA_STAT
    and #8
    beq @ends
; receive
    lda CIA_RX
    clc
@ends:
    rts

;----------------------------------------------------------------
;   acia_push, transmit a byte thru 6551, no waits
;
putc :  
acia_tx:
; verify
    pha
    lda CIA_STAT
    and #16
    beq @ends
; transmit
    pla                
    sta CIA_TX     
    clc
    rts
@ends:    
    pla
    sec
    rts

;--------------------------------------------------------
;   via_init, I2C, SPI, LCD, KBD, 
via_init:
    rts

;--------------------------------------------------------
;   tia_init, extra VIA, future use
tia_init:
    rts            

.byte $DE,$AD,$C0,$DE

;--------------------------------------------------------
;--------------------------------------------------------
ACIA_EXTRAS = 0
.IF ACIA_EXTRAS

rd_ptr = bios_rd
wr_ptr = bios_wr

;--------------------------------------------------------
;   acia_rd, circular buffer, no waits
acia_rd:
    ldx rd_ptr
    lda buffer, x
    inc rd_ptr
@xon:
    jsr acia_df
    CMP #$E0
    bcs @ends
    lda #9
    sta CIA_COMM
    lda XON_
    jsr acia_tx
    bcs @xon
@ends:    
    rts

;----------------------------------------------------------------
;   acia_wd, circular buffer, no waits
;----------------------------------------------------------------
acia_wr:
    ldx wr_ptr
    sta buffer, x
    inc wr_ptr
@xoff: 
    jsr acia_df
    cmp #$F0
    bcc @ends
    lda #1
    sta CIA_COMM
    lda XOFF_
    jsr acia_tx
    bcs @xoff
@ends:    
    rts

;----------------------------------------------------------------
;   acia_df, circular buffer, no waits
;----------------------------------------------------------------
acia_df:
    lda wr_ptr
    sec
    sbc rd_ptr
    rts

;--------------------------------------------------------
; max 255 bytes
; mess with CR LF (Windows), LF (Unix) and CR (Macintosh) line break types.
;--------------------------------------------------------
gets:
    ldy #0
@loop:
    jsr getc 
    bcs @loop   
    sta (bios_into), y
; minimal 
@cr:
    cmp CR_ ; ^M
    beq @ends
@lf:    
    cmp LF_ ; ^J
    beq @ends
@nak:    
    cmp NAK_ ; ^U
    bne @esc
    beq @end
@esc:    
    cmp ESC_ ; ^[
    bne @bs
@end:    
    ldy #0
    beq @ends
@bs:    
    cmp BS_  ; ^H
    bne @ctr
    dey
    bne @loop
; invalid
@ctr:
    cmp #32 
    bmi @loop
    cmp #126
    bpl @loop
; valid
    iny
    bne @loop
; full
    dey
@ends:
; null
    lda #0
    sta (bios_into), y
    tya
    clc
    rts

;--------------------------------------------------------
; max 255 bytes
;--------------------------------------------------------
puts:
    ldy #0
@loop:
    lda (bios_from), y
    beq @ends
@trie:
    jsr putc 
    bcs @trie
    iny
    bne @loop
@ends:
    rts

;--------------------------------------------------------
.ENDIF
;--------------------------------------------------------

LOW_ROUTINES = 0
.IF LOW_ROUTINES
;--------------------------------------------------------
; convert an ASCII character to a binary number
_atoi:
@number:
    cmp #'0'
    bcc @nohex
    cmp #'9'+1
    bcs @letter
    clc
    sbc #'0'
    rts
@letter:    
    ora #32 
    cmp #'a'
    bcc @nohex
    cmp #'f'+1
    bcs @nohex
    clc
    sbc #'a'-10
    rts
@nohex:    
    sec
    rts

;--------------------------------------------------------
;   one page functions

;--------------------------------------------------------
_qcopy:
    ; x bytes to copy
    ldy #0
@loop:
    lda (bios_from), y
    sta (bios_into), y
    iny
    dex
    bne @loop
    rts

;--------------------------------------------------------
_qfill:
    ; a byte to fill
    ; x bytes to fill
    ldy #0
@loop:
    sta (bios_into), y
    iny
    dex
    bne @loop
    rts

;--------------------------------------------------------
_qskip:
    ; a byte to skip
    ldy #0
@loop:
    cmp (bios_from), y
    bne @ends
    iny
    bne @loop
@ends:    
    rts

;--------------------------------------------------------
_qscan:
    ; a byte to scan
    ldy #0
@loop:
    cmp (bios_from), y
    beq @ends
    iny
    bne @loop
@ends:    
    rts

;--------------------------------------------------------
; coarse delay loop
; will loop 255 * 255, 261900 cycles
; at 0.9216 MHz about 284 ms
;
delay:             ; 6 call
@loop:    
    txa            ; 2 Get delay loop 
@y_delay: 
    tax            ; 2 Get delay loop
@x_delay:
    dex            ; 2
    bne @x_delay   ; 2
    dey            ; 2
    bne @y_delay   ; 2
    rts            ; 6 return

;--------------------------------------------------------
; delay 25ms, 0.9216 MHz phi0
;
delay_25ms:
    ldx #75
    ldy #75
    jsr delay
    rts

;--------------------------------------------------------
.ENDIF
;--------------------------------------------------------
