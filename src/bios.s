;vim: filetype=asm sw=4 ts=4 autoindent expandtab shiftwidth=4 et

;----------------------------------------------------------------------
; Copyright (c) 2023, Alvaro Gomes Sobral Barcellos
; All rights reserved.
; 
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions are met:
; 
; 1. Redistributions of source code must retain the above copyright notice, this
;    list of conditions and the following disclaimer.
; 2. Redistributions in binary form must reproduce the above copyright notice,
;    this list of conditions and the following disclaimer in the documentation
;    and/or other materials provided with the distribution.
; 
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
; ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
; ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
    
    $F020   bios device 02,  via 6522, TICK, I2C, SPI, LCD, KBD
    
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

### Todo

    - a POST routine, power on self test, with beep/blink diagnostics
    - check size of RAM, ROM, in 4 kb blocks, $0000..$F000
    - check I2C, REM, LCD, 
    - responses from ACIA, VIA, TIA 	
    

.ENDIF

;--------------------------------------------------------
;
;   ca65 setup
;

; identifiers

.case +

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
; phi2 is 0.9216 MHz, 10ms is 9216 or $2400 used by VIA T1/

    tick = $2400

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

;   T1 is tick, NMI interrupt /;
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
; VIA port B

; T2 is beeper, IRQ interrupter /;

;--------------------------------------------------------
; alias at page 

    buffer = $0200

    NMIVEC = $03FA

    RSTVEC = $03FC

    IRQVEC = $03FE

;--------------------------------------------------------
; compiler

full_via_test = 1

;--------------------------------------------------------
;   at page zero
;   $E0 to $EF bios reserved 16 bytes
;   $F0 to $FF bios reserved 16 bytes
;
* = $00E0

; tick
bios_tick:  .word $0, $0

; copycats
bios_a:  .byte $0
bios_s:  .byte $0
bios_x:  .byte $0
bios_y:  .byte $0
bios_p:  .byte $0
bios_z:  .byte $0	; keep zero

; generic
bios_f:  .byte $0
bios_g:  .byte $0
bios_h:  .byte $0
yp:  .byte $0

bios_seed:  .word $02A8, $B167

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
; tunes, for T2 one-shoot, with SR as output

; StarTreck(2), C5, D5, E5, F5, G5, A5, Quindar(2)
; frequency 2000, 2500, 523, 587, 659, 698, 784, 879
tune_lo: .byte 115, 184 
          .byte 43, 157, 233, 220, 235, 131 
          .byte 75, 124
tune_hi: .byte 4, 2
          .byte 41, 10, 2, 6, 5, 8
          .byte 5, 3
shift_rs: .byte %01010101, %00110011, %00001111, %10001110

* = $FF00
.word beep
.word blink
.word hitc
.word getc
.word putc
.word _tick_start
.word _tick_stop
.word _tick_zero
.word _ram2ram
.word _rem2ram
.word _ram2rem
.word _reset

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

_reset:

; real _init:
_init:
    
    ; disable interrupts
    sei

    ;0x0 wise
    cld
    
    ; prepare stack
    ldx #$FF
    txs

    ; clear RAM
    lda #$00
    tay
@loop:
    sta $0000, y
    sta $0100, y
    sta $0200, y
    sta $0300, y
    iny
    bne @loop

    ; prepare interrupts
    
    lda #<_bios_nmi
    sta NMIVEC+0
    lda #>_bios_nmi
    sta NMIVEC+1

    lda #<_bios_irq
    sta IRQVEC+0
    lda #>_bios_irq
    sta IRQVEC+1
    
    ; post 
    jsr _post

    ; setup acia one
    jsr _acia_init

    ; setup via one
    jsr _via_init 

    ; setup via two 
    jsr _tia_init

    ; alive
    jsr beep
    
    ; copy REM to RAM
    jsr _copyeep    

    ; alive
    jsr beep

    ; enable interrupts
    cli

    ; setup tick
    jsr _tick_init

    ; there we go....
    jsr _main

_main:

    ; insanity for safety
    jmp $400

;--------------------------------------------------------

.include "i2c.s"

;--------------------------------------------------------
; BEEP_FREQ_DIVIDER = 461	; 1 KHz @ 921,6 kHz 
; 1 kHz is the censor TV classic
; Quindar tones 2525 Hz start, 2475 Hz stop
; Sequences of 1% error
; frequencies D5 523, D5 587, E5 659, F5 698, G5 784, A5 880 
; for 460,800 kHz beat on/off
; counters D5 881, D5 785, E5 699, F5 660, G5 588, A5 524 
;
; beep
beep:


    rts

;--------------------------------------------------------
blink:
    rts

;--------------------------------------------------------
; power on self test
_post:

@test_ram:
    lda #$00
    sta bios_tmp1
    lda #$10
    sta bios_tmp1
    ldx #$00
    ldy #$00
@loop:
    lda #$55
    sta (bios_tmp1), y
    lda (bios_tmp1), y
    cmp #$55
    bne @fail_ram
    lda #$AA
    sta (bios_tmp1), y
    lda (bios_tmp1), y
    cmp #$AA
    bne @fail_ram
    lda #$00
    sta (bios_tmp1), y
    lda (bios_tmp1), y
    cmp #$00
    bne @fail_ram
@next:
    clc
    lda #$10
    adc (bios_tmp1+1), y
    cmp #$F0
    bcs @ends
    sta (bios_tmp1+1), y
    bcc @loop  
@ends:    
    bcs @test_via

@fail_ram:
    sec
    rts

@test_via:
    lda VIA_IER
    cmp #%10000000  
    bne @fail_via

.if full_via_test

    lda #$FF
    sta VIA_DDRA
    sta VIA_DDRB

    lda #$55
    sta VIA_PA
    cmp VIA_PA
    bne @fail_via
    sta VIA_PB
    cmp VIA_PB
    bne @fail_via

    lda #$aa
    sta VIA_PA
    cmp VIA_PA
    bne @fail_via
    sta VIA_PB
    cmp VIA_PB
    bne @fail_via

.endif

    beq @test_tia

@fail_via:
    sec
    rts

@test_tia:
    lda TIA_IER
    cmp #%10000000  
    bne @fail_tia

.if full_via_test

    lda #$FF
    sta TIA_DDRA
    sta TIA_DDRB

    lda #$55
    sta TIA_PA
    cmp TIA_PA
    bne @fail_tia
    sta TIA_PB
    cmp TIA_PB
    bne @fail_tia

    lda #$aa
    sta TIA_PA
    cmp TIA_PA
    bne @fail_tia
    sta TIA_PB
    cmp TIA_PB
    bne @fail_tia

.endif

    beq @okey

@fail_tia:
    sec
    rts

@okey:
    jmp @post_end

@post_end:
    clc
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
    ; reset CIA
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
    and #$08
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
    and #$10
    ; beq @ends
    beq @acia_tx
; transmit
    pla                
    sta CIA_TX 
    clc
    rts

.IF 0
delay:
    ldx #$FF
@delay:
    nop
    nop
    dex
    bne @delay
    rts
.ENDIF


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
_tune_init:
    ; setup free-run and interrupt at time-out
    lda #$55
    sta VIA_SR
    lda VIA_ACR
    and #$7F    ;   %01111111
    ora #$40    ;   %00100000
    sta VIA_ACR
    rts

;--------------------------------------------------------
; also restart interrupt
_tune_tick:
    sty yp
    tay
    ; store counter
    lda tune_lo, y
    sta VIA_T2CL
    lda tune_hi, y
    sta VIA_T2CH
    ldy yp
    rts

;--------------------------------------------------------
;
; clock tick, using VIA T1 free run 
; phi2 is 0.9216 MHz, 10ms is 9216 or $2400
;
_tick_init:
    ; store counter
    lda #<tick
    sta VIA_T1CL
    lda #>tick
    sta VIA_T1CH
    ; setup free-run and interrupt at time-out
    lda VIA_ACR
    and #$7F    ;   %01111111
    ora #$40    ;   %01000000
    sta VIA_ACR

;--------------------------------------------------------
; start tick, zzzz
_tick_start:    
    lda VIA_IER
    and #$7F   ;    %01111111
    ora #$60   ;    %01100000
    sta VIA_IER
    rts

;--------------------------------------------------------
; stop tick, zzzz    
_tick_stop:
    lda #%10000000
    lda VIA_IER
    and #$7F    ;   %01111111
    ora #$80    ;   %10000000
    sta VIA_IER
    rts

;--------------------------------------------------------
; clear tick    
_tick_zero:
    jsr _tick_stop
    lda #$00
    sta bios_tick+0
    sta bios_tick+1
    sta bios_tick+2
    sta bios_tick+3
    jsr _tick_start
    rts

;--------------------------------------------------------
;   tia_init, extra VIA, future use
_tia_init:
    rts            

.byte $DE,$AD,$C0,$DE

;----------------------------------------------------------------------
;
;   ok  ( -- w)
;
;     ©2000-2021 by Gerhard Schmidt,
;      http://www.avr-asm-tutorial.net/avr_en/apps/random_tn13/random_calc_tn13.html
;
;    seed ~ 0x02A8
;
;     also good seeds
;
;.word  $02A8
;.word  $B167, $4A3C, $9879, $B61E, $7B26
;.word  $A858, $1F88, $50D5, $419D, $5537
;.word  $0224, $0527, $5EB6, $1E6D, $BCDC
;.word  $92FF, $C206, $0ECD, $9361, $2823
;.word  $BE0B, $B303, $6462, $0E4C, $3D24
;
random:

	lda bios_seed+1
	eor bios_seed+0
	adc bios_seed+0
	ldy bios_seed+1
	sta bios_seed+1
	sty bios_seed+0

.if 0

	lda bios_seed+2
	eor bios_seed+1
	adc bios_seed+1
	ldy bios_seed+2
	sta bios_seed+2
	sty bios_seed+1

	lda bios_seed+3
	eor bios_seed+2
	adc bios_seed+2
	ldy bios_seed+3
	sta bios_seed+3
	sty bios_seed+2

	lda bios_seed+4
	eor bios_seed+3
	adc bios_seed+3
	ldy bios_seed+4
	sta bios_seed+4
	sty bios_seed+3

	rts

.endif 

;----------------------------------------------------------------------
rolette:
    jsr random
    and #%00111111
    adc #10
    ror 
    tay
    lda _roulette, y
    rts
    
;0-32-15-19-04-21-02-25-17-34-06-27-13-36-11-30-08-23-10
; -05-24-16-33-01-20-14-31-09-22-18-29-07-28-12-35-03-26

_roulette:
.byte 32,15,19,04,21,02, 25,17,34,06,27,13, 36,11,30,08,23,10
.byte 05,24,16,33,01,20, 14,31,09,22,18,29, 07,28,12,35,03,26
.byte 00

;----------------------------------------------------------------------

.end

 LDA  VIA_DDRB

 ORA #$80

 STA  VIA_DDRB   ; Set the high bit in DDRB, to make PB7 an output.

 LDA  VIA_ACR    ; Set the two high bits in the ACR to get the

 ORA #$C0        ; square-wave output on PB7.  (Don't enable the

 STA  VIA_ACR    ; T1 interrupt in the IER though.)

 LDA #255        ; Set the T1 timeout period. LOWER THIS TO SOMETHING LIKE 77 FOR 1MHZ CPU CLOCK

 STA $6004       ; VIA_T1CL   ;USE 255 if the Φ2 rate is 5MHz.  

 LDA #100        ; Beep Tone, 0 highest, 255 lowest

 STA $6005       ; VIA_T1CH    ;We are now beeping

 It will not stop until the system is reset or you turn it off.

 BEEP_Off:

 STZ $600b       ;VIA_ACR ;No more beep! 
