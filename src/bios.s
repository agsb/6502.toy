;---------------------------------------------------------------------
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
;---------------------------------------------------------------------
.IF 0

# Easy BIOS or MOS for a 6502toy

## Memory map
   
    $0000 to $EFFF, RAM
    
    $F000 to $F7FF, DEVICES

    $F800 to $FFFF, ROM 

### RAM

    $0000 to $00FF page zero, hardware registers
    
    $0100 to $01FF page one, hardware stack
    
    $0200 to $02FF page two, bios buffers
    
    $0300 to $03FF page tri, list of devices and routines
   
    $0400-$0FFF reserved 

    $1000-$E7FF user 

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
    
    1. Copy the $FF00 page to $0300 page. 

    2. Reserve the pages at $0400 to $0FFF. 

    3. Copy I2C EEPROM to $1000 and jump to $1000. 

    4. The BIOS stay at $F080 to $FFFF.

### Interrupts

    the clock tick done by VIA_T1 is the only NMI of 6502toy.
    
### Best Practices

    - On return, the carry flag is 0 for Ok.

    - callee saves registers.

    - do not make recursive routines.

.ENDIF
;---------------------------------------------------------------------
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

;---------------------------------------------------------------------
;
;   CONSTANTS
;
;---------------------------------------------------------------------
; ASCII 

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

;---------------------------------------------------------------------
;   task or process states
    HALT    = 0
    IDLE    = 1
    WAIT    = 2
    BUSY    = 3

;---------------------------------------------------------------------
; 
    DEVS = $F000

; terminal usart
    CIA       =  DEVS+$10    ; The base address of the 6551 ACIA.
    CIA_DATA  =  CIA+0   ; Its data I/O register
    CIA_RX    =  CIA+0   ; Its data I/O register
    CIA_TX    =  CIA+0   ; Its data I/O register
    CIA_STAT  =  CIA+1   ; Its  status  register
    CIA_COMM  =  CIA+2   ; Its command  register
    CIA_CTRL  =  CIA+3   ; Its control  register

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

;---------------------------------------------------------------------
; at page 

    GHOSTS = $0300

    SHADOWS = $FF00

    NMIVEC = $03FA

    RSTVEC = $03FC

    IRQVEC = $03FE

;----------------------------------------------------------------------
; at page
* = $FF00

; references of devices, 
.repeat 8, R  
    .word DEVS+(R)*16
.endrepeat

; pointers of routines
.word delay       ; 
.word getline     ; 
.word putline     ; 
.word getch       ; 
.word putch       ; 
.word clock_stop  ; 
.word clock_start ; 
.word monitor     ; 
.word copycat     ; 

.word $DE,$AD,$C0,$DE
;---------------------------------------------------------------------
; at $FFFA
.segment "VECTORS"
;* = $FF00

; hardware jumpers
.word _jump_nmi  ; fa ROM NMI vector
.word _jump_rst  ; fc ROM Reset vector
.word _jump_irq  ; fe ROM IRQ/BRK vector

;---------------------------------------------------------------------
;   at page zero
;   $E0 to $EF bios reserved 16 bytes
;   $F0 to $FF bios reserved 16 bytes
;
* = $00E0

; copycats
bios_a:  .byte $0
bios_x:  .byte $0
bios_y:  .byte $0
bios_s:  .byte $0
bios_p:  .byte $0
bios_f:  .byte $0

; clock
bios_tick:  .word $0, $0

; generic
bios_from:  .word $0
bios_into:  .word $0
bios_work:  .word $0
bios_what:  .word $0
bios_when:  .word $0


;---------------------------------------------------------------------
;
.segment "ONCE"

.byte $DE,$AD,$C0,$DE
.byte 00,32,15,19,04,21,02,25,17,34,06,27,13,36,11,30,08,23,10
.byte 05,24,16,33,01,20,14,31,09,22,18,29,07,28,12,35,03,26,00

;---------------------------------------------------------------------
;
; some code adapted from 6502.org forum
;
;---------------------------------------------------------------------
; interrups stubs, easy way
;
; At boot, the $FF00 page is copied to $0300,
; with default values for devices and routines
; then all vectors could be changed and could
; be restored also.
;

; must be at shadow rom copied to ghost ram

_jump_nmi:
    jmp (NMIVEC)

_jump_irq:
    jmp (IRQVEC)

_jump_rst:
    ; jmp (RSTVEC) never do that or hang ever boot
    jmp _rst_init

; void nmi,irq at boot

nmi_init:

bios_init:

    rti

;---------------------------------------------------------------------
; reset stub
;---------------------------------------------------------------------
_rst_init:

; real _init:
_init:
    ; disable interrupts
    sei

    ; no BCD math
    cld

    ; copy default page
    jsr copycat

    ; setup via one
    jsr via_init 

    ; setup via two 
    jsr tia_init

    ; setup clock
    jsr clock_init

    ; setup acia one
    jsr acia_init

    ; enable interrupts
    
    lda #<_bios_init_easy
    sta IRQVEC+0
    lda #>_bios_init_easy
    sta IRQVEC+1

    lda #<_bios_init_easy
    sta NMIVEC+0
    lda #>_bios_init_easy
    sta NMIVEC+1

    ; copy eeprom I2C to $1000
    jsr eecopy

    ; stack: pull is decr, push is incr
    ldx #$FF
    txs
    
    ; there we go....
    cli
    jsr _main

_main:
    ; insanity for safety
    jmp $1000

;---------------------------------------------------------------------
; copy default page from ROM to RAM
;---------------------------------------------------------------------
copycat:
    ldy #$00
@loop:    
    lda SHADOWS, y
    sta GHOSTS, y
    iny
    bne @loop
    rts

;---------------------------------------------------------------------
; copy default eeprom to RAM
;
;   $FF00-$1000=$EF00
;   $EF00/$80 = $01DE
;   478 I2C pages of 128 bytes
;   Magics
;---------------------------------------------------------------------
eecopy:
    lda #$00
    sta bios_from+0
    lda #$10
    sta bios_from+1
    lda #$DE
    sta bios_work+0
    lda #$01
    sta bios_work+1
    rts

;---------------------------------------------------------------------
; coarse delay loop
;
; 7 * dy + 4 * dy * dx + 15
;
; will loop 255 * 255, 261900 cycles
; at 0.9216 MHz about 284 ms
;
; eg. 25.000 ms  is  75   75
;
; y = dy, x = dx
;
;---------------------------------------------------------------------
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

;---------------------------------------------------------------------
; delay 25ms, 0.9216 MHz phi0
;---------------------------------------------------------------------
delay_25ms:
    ldx #75
    ldy #75
    jsr delay
    rts
;---------------------------------------------------------------------
monitor:
    rts

;======================================================================
; real irq handler
; easy minimal 
;---------------------------------------------------------------------

_bios_init_easy:
    sta bios_a
    pla
    pha
    and #$10
    bne _bios_soft_easy
    beq _bios_hard_easy

;---------------------------------------------------------------------
_bios_ends_easy:
    lda bios_a
    rti

;---------------------------------------------------------------------
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
    jmp service_cia
@scan_panic:
    jmp _bios_ends_easy

;---------------------------------------------------------------------
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

;---------------------------------------------------------------------
;	attend interrupt 
;---------------------------------------------------------------------
service_via:
	pha
	lda #$7F
	sta VIA_IFR;
	jmp _bios_ends_easy

;---------------------------------------------------------------------
;	attend interrupt 
;---------------------------------------------------------------------
service_tia:
	pha
	lda #$7F
	sta TIA_IFR;
	jmp _bios_ends_easy

;---------------------------------------------------------------------
;	attend interrupt 
;---------------------------------------------------------------------
service_cia:
	jmp _bios_ends_easy

;---------------------------------------------------------------------
;   interrupts stubs, trampolines
;---------------------------------------------------------------------
_bios_save_registers:
    lda bios_a
    pha
    txa
    pha
    tya
    pha
    ; fake jump indirect
    lda #>_bios_load_registers
    pha
    lda #<_bios_load_registers
    pha
    rts

;---------------------------------------------------------------------
_bios_load_registers:
    pla
    tay
    pla
    tax
    pla
    sta bios_a
    rti

;======================================================================
;---------------------------------------------------------------------
getch:  ; wait in loop could hang
    jsr acia_pull
    bcs getch
    rts

;---------------------------------------------------------------------
putch:  ; wait in loop could hang
    jsr acia_push
    bcs putch
    rts

;---------------------------------------------------------------------
; max 255 bytes
; mess with CR LF (Windows), LF (Unix) and CR (Macintosh) line break types.
;---------------------------------------------------------------------
getline:
    sta bios_work+0
    stx bios_work+1
    ldy #0
@loop:
    jsr getch
    bcs @loop   
    sta (bios_work), y
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
    sta (bios_work), y
    tya
    clc
    rts

;---------------------------------------------------------------------
; max 255 bytes
;---------------------------------------------------------------------
putline:
    sta bios_work+0
    stx bios_work+1
    ldy #0
@loop:
    lda (bios_work), y
    beq @ends
@trie:
    jsr putch
    bcs @trie
    iny
    bne @loop
@ends:
    rts

;---------------------------------------------------------------------
;
; clock tick, using VIA T1 free run 
; phi2 is 0.9216 MHz, 10ms is 9216 or $2400
;
;---------------------------------------------------------------------
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

;---------------------------------------------------------------------
; start clock
;---------------------------------------------------------------------
clock_start:    
    lda #%11000000
    sta VIA_IER
    rts

;---------------------------------------------------------------------
; stop clock    
;---------------------------------------------------------------------
clock_stop:
    lda #%10000000
    sta VIA_IER
    rts

;---------------------------------------------------------------------
; counts ticks
;---------------------------------------------------------------------
clock_tick:
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

;count:
;    inc CNT+0
;    bne @ends
;    inc CNT+1
;@ends:


;---------------------------------------------------------------------
;   acia_init, configures 19200,N,8,1 FIXED
;---------------------------------------------------------------------
acia_init:
    pha			; Push A to stack
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

;-------------------------------------------------------------------------------
;   acia_pull, receive a byte thru 6551, no waits
;-------------------------------------------------------------------------------
acia_pull:
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

;-------------------------------------------------------------------------------
;   acia_push, transmit a byte thru 6551, no waits
;-------------------------------------------------------------------------------
acia_push:
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

;---------------------------------------------------------------------
;   via_init, I2C, SPI, LCD, KBD, 
;---------------------------------------------------------------------
via_init:
    rts

;---------------------------------------------------------------------
;   tia_init, extra VIA, future use
;---------------------------------------------------------------------
tia_init:
    rts			

;---------------------------------------------------------------------
; convert an ASCII character to a binary number
;---------------------------------------------------------------------
qdigit:
@number:
    cmp #'0'-1
    bcc @nohex
    cmp #'9'+1
    bcs @letter
    sbc #'0'
    clc
    rts
@letter:    
    ora #32 
    cmp #'a'-1
    bcc @nohex
    cmp #'f'+1
    bcs @nohex
    sbc #'a'-10
    clc
    rts
@nohex:    
    sec
    rts

;---------------------------------------------------------------------
;   one page functions

;---------------------------------------------------------------------
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

;---------------------------------------------------------------------
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

;---------------------------------------------------------------------
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

;---------------------------------------------------------------------
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

;---------------------------------------------------------------------


