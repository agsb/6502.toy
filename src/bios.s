; vim: filetype=asm sw=8 ts=8 autoindent expandtab shiftwidth=8 et
;-----------------------------------------------------------------------
; Copyright (c) 2023, Alvaro Gomes Sobral Barcellos
; All rights reserved.
; 
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions
; are met:
; 
; 1. Redistributions of source code must retain the above copyright 
;    notice, this list of conditions and the following disclaimer.
;
; 2. Redistributions in binary form must reproduce the above copyright
;    notice, this list of conditions and the following disclaimer in 
;    the documentation and/or other materials provided with the 
;    distribution.
; 
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT 
; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS 
; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE 
; COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES, LOSS
; OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED 
; AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT 
; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN 
; ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
; POSSIBILITY OF SUCH DAMAGE.
;----------------------------------------------------------------------

;--------------------------------------------------------
;
;	minimal bios for 6502.toy
;
;       reserved RAM:
;       $40 bytes at $000 page zero
;       $40 bytes at $100 page one
;       $100 bytes at $400 
;       $1000 free for users
;       
;--------------------------------------------------------
;
;# Easy BIOS or MOS for a 6502toy
;
;       64bit = +/- 9223372036854775807
;
;## Memory map
;   
;       56k     $0000 to $DFFF, RAM
; 
;        8k     $E000 to $FFFF, ROM 
;
;       256     $E000 to $E0FF, DEVICES
;
;### RAM
;
;        $0000 to $00FF page zero, hardware registers
;            reserved $00-$3F for extras
;            reserved $FF-
;
;        $0100 to $01FF page one, hardware stack
;            reserved $100 to $13F
;
;        $0200 page two, free 
;        
;        $0300 page tri, free
;   
;        $0400 page qua, bios reserved
;
;        $0E00
;
;        $1000-$E7FF 56 kb user
;
;#### exclusive ROM
;
;        $E000-$E0FF Devices
;
;        $E100-$FFFF BOOT ROM
;
;### DEVICES
;
;        mapped onboard:
;  
;        $E000   bios exclusive hardware
;        
;        $E010   bios device 01, acia 6551, USART TERMINAL
;        
;        $E020   bios device 02, via 6522, TICK, I2C, SPI, LCD, KBD
;        
;        $E030   bios device 03, via 6522, user
;        
;        $E040   external device 04
;        
;        $E050   external device 05
;        
;        $E060   external device 06
;        
;        $E070   external device 07
;        
;        for expansion:
;        
;        $E080 to $E0FF, free  
;
;### ROM
;        
;        $E100 to $FFFF  7.75k ROM 
;
;        At boot:
;        
;        1. Copy the NMI/IRQ address to page $0400. 
;
;        2. Copy I2C EEPROM to $1000 and jump ($1000). 
;
;        4. Shadow BIOS stay at $F100 to $FFFF.
;
;### Interrupts
;
;        the clock tick done by VIA T1 is the only NMI of 6502
;        
;### Best Practices
;
;        - On return, the carry flag is 0 for Ok.
;
;        - callee saves registers.
;
;        - do not make recursive routines.
;
;        - using RAM for random access memory, as static ram with read and write.
;
;        - using ROM for read only memory, as flash eeprom parallel. NO WRITE.
;        
;        - using REM for regular eeprom memory, as flash eeprom with I2C protocol.
;
;### Todo
;
;        - a POST routine, power on self test, with beep/blink diagnostics
;
;        - check size of RAM, ROM, in 4 kb blocks, $0000..$F000
;        
;        - check I2C, SPI, REM, LCD, 
;        
;        - responses from ACIA, VIA, TIA 	
;
;.ENDIF
;
;--------------------------------------------------------
; constants
;--------------------------------------------------------
;   task states

        HALT    = 0
        IDLE    = 1
        WAIT    = 2
        BUSY    = 3

;--------------------------------------------------------
; using same clock of 6551 1.8432, by half a phi0
;
; phi2 is 0.9216 MHz, 10ms is 9216 or $2400 
;
; used by VIA T1 which depends directly of phi2
;
;--------------------------------------------------------
; devices
 
        DEVS = $E000

;--------------------------------------------------------
; terminal usart
        ; The base address of the 6551 ACIA.
        CIA       =  DEVS + $10    
        CIA_DATA  =  CIA+0   ; Its data I/O register
        CIA_RX    =  CIA+0   ; Its data I/O register
        CIA_TX    =  CIA+0   ; Its data I/O register
        CIA_STAT  =  CIA+1   ; Its status  register
        CIA_COMM  =  CIA+2   ; Its command  register
        CIA_CTRL  =  CIA+3   ; Its control  register

;--------------------------------------------------------
; system via
        ; The base address of the 6522 VIA.
        VIA        =  DEVS + $20 
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
; user via
        ; The base address of the 6522 VIA.
        TIA        =  DEVS + $30 
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
        URX = (1 << 7)    
        UTX = (1 << 6)    

; for SPI
        MCSS = (1 << 5)    
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
; hold the references for interrupt routines

        JMP_VEC = $04F8

        NMI_VEC = $04FA

        RST_VEC = $04FC ; fake

        IRQ_VEC = $04FE

;--------------------------------------------------------
;   at page zero
;
.segment "ZERO"

;--------------------------------------------------------
; $000 to $03F, reserved for bios
* = $000

; reserved for extras, eg. monitors, sweet-16, etc
.res $30, $00

; reserved for bios, 16 bytes

; bios_tick, must be bytes :)
; ~49,7 days in milliseconds
bios_tick:      .byte $0, $0, $0, $0 

; bios service interrupt saves, BRK
bios_f:         .byte $0

; bios service save space
bios_s:         .byte $0
bios_a:         .byte $0
bios_p:         .byte $0
bios_y:         .byte $0
bios_x:         .byte $0

; for I2C, SPI, RAM, REM, ROM
bios_from:       .word $0
bios_into:       .word $0
bios_many:       .word $0

;
;       $040 to $0FF for user
;

;----------------------------------------------------------------------
; $0100 to $013F, reserved for bios
; about 32 address
* = $0100
.res $40, $00   ; bios_stack

;
;       $140 to $1FF    for user
;

;----------------------------------------------------------------------
.segment "CODE"


; reserved for bios 256 bytes
* = $0400

; functions vectors
; misc
.addr init

.addr void
.addr beep
.addr tone_play ; lsb and msb ????
.addr blink     ; lsb and msb ????


;.addr seed      ; change seed
;.addr rand      ; return $0000 to $FFFF
.addr roulette  ; ireturn 0 to 37

; usart
.addr setch     ; init tx rx 19200-8-N-1
.addr getcq     ; test rx ready
.addr putcq     ; text tx ready
.addr getch     ; get a char
.addr putch     ; put a char

; clock
.addr tick_start        ; start counter
.addr tick_stop         ; stop counter
.addr tick_zero         ; reset counter

; ram
.addr ram2ram

; i2c
.addr rem2ram_i2c
.addr ram2rem_i2c

; spi
.addr rem2ram_spi
.addr ram2rem_spi

; tia init
.addr bios_tia_init
.addr bios_tia_service

; variables

bios_void:      .word $0, $0
bios_seed:      .byte $0, $0, $0, $0

bios_tone:      .byte $0, $0 
bios_beat:      .byte $0, $0 
bios_acia:      .byte $0, $0

bios_tmp0:      .word $0
bios_tmp1:      .word $0
bios_tmp2:      .word $0
bios_tmp3:      .word $0
bios_tmp4:      .word $0
bios_tmp5:      .word $0
bios_tmp6:      .word $0
bios_tmp7:      .word $0

;--------------------------------------------------------
* = $047F 
bios_buffer:    .byte $0


;--------------------------------------------------------
;
.segment "ONCE"

.byte $DE,$AD,$C0,$DE

;--------------------------------------------------------
; tones, for T2 one-shoot, with SR as output

; StarTreck(2), C5, D5, E5, F5, G5, A5, Quindar(2)
; frequency 2000, 2500, 523, 587, 659, 698, 784, 879

tone_lsb: 
        .byte 115, 184, 43, 157, 233, 220, 235, 131, 75, 124

tone_msb: 
        .byte 4, 2, 41, 10, 2, 6, 5, 8, 5, 3

shift_rs: 
        .byte %01010101, %00110011, %00001111, %10001110

;--------------------------------------------------------
;
; some code adapted from 6502.org forum
;
;--------------------------------------------------------
; $F000 to $F03F reserved for devices
* = $E000

;--------------------------------------------------------
; $F100 to $FFFF bios
;       nop for align
* = $E100

 jump_nmi:
        nop    
        jmp (NMI_VEC)

 jump_irq:
        nop
        jmp (IRQ_VEC)

 jump_rst:
        nop
        jmp (rst_init)

; void nmi, irq at boot

 jump_void:

 nmi_init:

 irq_init:

        rti

; i2c

rem2ram_i2c:

ram2rem_i2c:

; spi

rem2ram_spi:

ram2rem_spi:

; copy

ram2ram:
        rts

;--------------------------------------------------------
; reset 
;--------------------------------------------------------
rst_init:

; real  init:
init:
        
        ; disable interrupts
        sei

        ;0x0 wise
        cld
        
        ; prepare bios stack
        ldx #$40
        txs

        ; alive
        jsr beep

        ; post 
        jsr post

        ; %0001 1111 = 19200 baud, external receiver, 8 bit, 1 stop bit
        lda #$1F     
        sta bios_acia + 0
        
        ; %0000 1011 = no parity, normal mode, RTS low, INT disable, DTR low 
        lda #$0B     
        sta bios_acia + 1

        ; setup acia one
        jsr acia_init

        ; setup via one
        jsr via_init  

        ; setup via two 
        jsr tia_init

        ; setup tone
        jsr tone_init

        ; alive
        jsr beep

        ; copy REM to RAM
        jsr copy_eep     

        ; alive
        jsr beep

        ; interrupt vectors
        
        lda #<bios_nmi
        sta NMI_VEC+0
        lda #>bios_nmi
        sta NMI_VEC+1

        lda #<bios_irq
        sta IRQ_VEC+0
        lda #>bios_irq
        sta IRQ_VEC+1
        
        ; seed 

        lda #$A8
        sta bios_seed + 0
        lda #$02
        sta bios_seed + 1
        lda #$67
        sta bios_seed + 2
        lda #$B1
        sta bios_seed + 3

        ; phi2 is 0.9216 MHz, 10ms is 9216 or $2400

        lda #$00
        sta bios_beat + 0
        lda #$24
        sta bios_beat + 1

        ; setup tick
        jsr tick_init

        ; enable interrupts

        cli

        ; insanity for safety
        jmp $1000

;--------------------------------------------------------
; BEEP FREQ DIVIDER = 461	; 1 KHz @ 921,6 kHz 
; 1 kHz is the censor TV classic
;
; Quindar tones 2525 Hz start, 2475 Hz stop
; Sequences of 1% error
; frequencies D5 523, D5 587, E5 659, F5 698, G5 784, A5 880 
; for 460, 800 kHz beat on/off
; counters D5 881, D5 785, E5 699, F5 660, G5 588, A5 524 
;
; zzzz
void:

;       beep audio
beep:

;--------------------------------------------------------
;       LED blinks
blink:

bios_tia_init:

bios_tia_service:

        rts

;--------------------------------------------------------
; power on self test
 post:
        clc
@test_ram:
        lda #$00
        tay
        tax
        sta bios_tmp1 + 0
        lda #$20 
        sta bios_tmp1 + 1
@loop:
        lda #$55
        sta bios_tmp1, y
        cmp bios_tmp1, y
        bne @fail_ram 
        lda #$AA
        sta bios_tmp1, y
        cmp bios_tmp1, y
        bne @fail_ram
        lda #$00
        sta bios_tmp1, y
        cmp bios_tmp1, y
        bne @fail_ram
        iny
        bne @loop
        clc
        lda bios_tmp1 + 1
        adc #$10
        sta bios_tmp1 + 1
        cmp #$00
        bne @loop  
        beq @test_via

@fail_ram:
        ldx #$01
        bne @fail

@test_via:
        lda VIA_IER
        cmp #%10000000  
        beq @test_tia 

@fail_via:
        ldx #$02
        bne @fail

@test_tia:
        lda TIA_IER
        cmp #%10000000  
        beq @okey

@fail_tia:
        ldx #$04
        bne @fail

@fail:
@okey:
        rts

;--------------------------------------------------------
; copy default eeprom to RAM
;
;   $FF00-$1000=$EF00
;   $EF00/$80 = $01DE
;   478 I2C pages of 128 bytes
;   Magics
;   rem ptr = bios tmp1
;   ram ptr = bios tmp2
;   len ptr = bios tmp3
;   device  = bios tmp4+0
;   sense   = bios tmp4+1
;   work    = bios tmp5
;
;--------------------------------------------------------
 copy_eep:
        ; use device 0 and sense read
        ; copy $E000 bytes from $1000 REM to $1000 RAM
        lda #$00
        sta bios_from + 0
        sta bios_into + 0
        sta bios_many + 0
        lda #$10
        sta bios_from + 1
        sta bios_into + 1
        lda #$E0
        sta bios_many + 1
        lda #%01010000	; hardcoded for 24LC512 at 000, will be << 1 
        sta bios_tmp4 + 0
        lda #$01
        sta bios_tmp4 + 1
        jsr rem2ram_i2c
        rts

;========================================================
; NMI, counts ticks
;       bios_tick in zero page
 bios_nmi:
 bios_clock:
        ; for safe
        sei 
        cld                
        bit VIA_T1CL            ; clear bit
        ; cascate counter
        inc bios_tick+0
        bne @ends
        inc bios_tick+1
        bne @ends
        inc bios_tick+2
        bne @ends
        inc bios_tick+3
@ends:
        cli
        rti

;======================================================================
; IRQ|BRK, handler
; easy minimal 
;--------------------------------------------------------
 bios_soft_ends:
        ldy bios_y
        ldx bios_x
 bios_hard_ends:
        lda bios_a
        cli
        rti

;--------------------------------------------------------
 bios_irq:
 bios_init_easy:
        sei
        cld
        sta bios_a
@easy:
        pla
        pha
        and #$10
        beq bios_hard_easy
        ; fall througth
        ; bne bios_soft_easy

;--------------------------------------------------------
 bios_soft_easy:
        ;
        ; from a BRK, a software interrupt
        ; which always called as:
        ;
        ;       ...
        ;       brk
        ;       .byte 00 to FF bios function code

        ; ; (optional)
        ;       parameter
        ;       ...
        ;       parameter
        ;       rts
        ;
        ;
        ; the PC in stack minus one is the code $ZZ 
        ; for what break was called.
        ;
        ; do something somewhere sometime
        ;
        ; general software interrupt service


        stx bios_x
        sty bios_y

        jmp bios_soft_ends

;--------------------------------------------------------
 bios_hard_easy:

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
        ; forgot voids
        pha
        jmp bios_hard_ends

;--------------------------------------------------------
;    attend interrupt 
service_via:
        pha
        lda #$7F
        sta VIA_IFR;
        jmp bios_hard_ends

;--------------------------------------------------------
;    attend interrupt 
service_tia:
        pha
        lda #$7F
        sta TIA_IFR;
        jmp bios_hard_ends

;--------------------------------------------------------
;    attend interrupt 
service_cia:
        pha
        jmp bios_hard_ends

;======================================================================
;   acia init, configures 19200,N,8,1 default 6551
;
setch:
acia_init:
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
;   verify RX thru 6551, no waits
;   zero on not
getcq:  
acia_rxq:
        ; verify
        lda CIA_STAT
        and #8
        rts

;----------------------------------------------------------------
;   verify TX thru 6551, no waits
;   zero on not
putcq:  
acia_txq:
        ; verify
        lda CIA_STAT
        and #10
        rts

;----------------------------------------------------------------
;   receive a byte thru 6551, waits
;
getch:  
@acia_rx:
        ; verify
        lda CIA_STAT
        and #$08
        beq @acia_rx
        ; receive
        lda CIA_RX
        clc
        rts

;----------------------------------------------------------------
;   transmit a byte thru 6551, waits
;
putch:  
@acia_tx:
        ; verify
        pha
        lda CIA_STAT
        and #$10
        beq @acia_tx
        ; transmit
        pla                
        sta CIA_TX 
        clc
        rts

;--------------------------------------------------------
; internal delay
delay:
        txa
        ldx #$FF
@delay:
        nop
        nop
        dex
        bne @delay
        tax
        rts

;--------------------------------------------------------
;   via init, I2C, SPI, LCD, KBD, 
;
; 	pa7 utx, pa6 urx, 
;
;	pa5 mscs, pa4 mosi, pa3 miso, pa2 msck, 
;
;	pa1 sda, pa0 scl
;
;	pb1 lcd4, pb2 lcd5, pb3 lcd6, 
;       pb4 lcd7, pb5 lcdR, pb6 lcdE
;
 via_init:
        lda #%01101100            
        sta VIA_DDRA
        lda #%01101100            
        sta VIA_DDRB
        ; fall throught

;--------------------------------------------------------
tone_init:
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
 tone_play:
        sty bios_y
        tay
        ; store counter
        lda tone_lsb, y
        sta VIA_T2CL
        lda tone_msb, y
        sta VIA_T2CH
        ldy bios_y
        rts

;--------------------------------------------------------
;
; clock beat, using VIA T1 free run 
; phi2 is 0.9216 MHz, 10ms is 9216 or $2400
;
 tick_init:
        ; store counter
        lda bios_beat + 0
        sta VIA_T1CL
        lda bios_beat + 1
        sta VIA_T1CH

        ; setup free-run and interrupt at time-out
        lda VIA_ACR
        and #$7F    ;   %01111111
        ora #$40    ;   %01000000
        sta VIA_ACR
        ; fall throught

;--------------------------------------------------------
; start tick, zzzz
 tick_start:    
        lda VIA_IER
        and #$7F   ;    %01111111
        ora #$60   ;    %01100000
        sta VIA_IER
        rts

;--------------------------------------------------------
; stop tick, zzzz    
 tick_stop:
        ; lda #%10000000
        lda VIA_IER
        and #$7F    ;   %01111111
        ora #$80    ;   %10000000
        sta VIA_IER
        rts

;--------------------------------------------------------
; clear tick    
tick_zero:
        jsr tick_stop
        lda #$00
        sta bios_tick+0
        sta bios_tick+1
        sta bios_tick+2
        sta bios_tick+3
        jsr tick_start
        rts

;--------------------------------------------------------
;   tia init, extra VIA, future use
tia_init:
        ; jmp (tia_init_user)
        rts            

;----------------------------------------------------------------------

.include "i2c.s"

;----------------------------------------------------------------------

.byte $DE,$AD,$C0,$DE

;----------------------------------------------------------------------
;
;   ok  ( -- w)
;
;     ©2000-2021 by Gerhard Schmidt,
;      http://www.avr-asm-tutorial.net/avr en/apps/random tn13/random calc tn13.html
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
        
        pha
	
        lda bios_seed + 1
        pha
	eor bios_seed + 0
	adc bios_seed + 0
	sta bios_seed + 1
        pla
	sta bios_seed + 0

	lda bios_seed + 2
        pha
	eor bios_seed + 1
	adc bios_seed + 1
	sta bios_seed + 2
        pla
	sta bios_seed + 1

	lda bios_seed + 3
        pha
	eor bios_seed + 2
	adc bios_seed + 2
	sta bios_seed + 3
        pla
	sta bios_seed + 2

	lda bios_seed + 0
        pha
	eor bios_seed + 3
	adc bios_seed + 3
	sta bios_seed + 0
        pla
	sta bios_seed + 3

        pla
	rts

;----------------------------------------------------------------------
;0-32-15-19-04-21-02-25-17-34-06-27-13-36-11-30-08-23-10
; -05-24-16-33-01-20-14-31-09-22-18-29-07-28-12-35-03-26
roulette:
        pha
        tya
        pha
@loop:
        jsr random
        lda bios_seed + 0
        cmp #$93        ; valid 0 to 147, 148/256 repeat ~42%
        bpl @loop
        ror 
        ror 
        tay
        lda roulette_wheel, y
        sta bios_seed + 0
        pla
        tay
        pla
        rts
        
roulette_wheel:

.byte 00
.byte 32, 15, 19, 04, 21, 02, 25, 17, 34, 06, 27, 13 
.byte 36, 11, 30, 08, 23, 10, 05, 24, 16, 33, 01, 20
.byte 14, 31, 09, 22, 18, 29, 07, 28, 12, 35, 03, 26

;--------------------------------------------------------
; at $FFFA
.segment "VECTORS"

; hardware jumpers
.word jump_nmi   ; fa ROM NMI vector
.word jump_rst   ; fc ROM Reset vector
.word jump_irq   ; fe ROM IRQ/BRK vector

;----------------------------------------------------------------------
; ends list 
;

end_of_code:
.end

;-----------------------------------------------------------------------
; extras for 6502
; vide eorBookV1.0.1

; set overflow bit
setovr :
        bit @ends
@ends:
        rts

; Z flag is zero in NMOS6502
nmos :
        sed
        clc
        lda #$99
        adc #$01
        cld
        rts

;----------------------------------------------------------------------
; just extras

 LDA  VIA DDRB

 ORA #$80

 STA  VIA DDRB   ; Set the high bit in DDRB, to make PB7 an output.

 LDA  VIA ACR    ; Set the two high bits in the ACR to get the

 ORA #$C0        ; square-wave output on PB7.  (Don't enable the

 STA  VIA ACR    ; T1 interrupt in the IER though.)

 LDA #255        ; Set the T1 timeout period. LOWER THIS TO SOMETHING LIKE 77 FOR 1MHZ CPU CLOCK

 STA $6004       ; VIA T1CL   ;USE 255 if the Φ2 rate is 5MHz.  

 LDA #100        ; Beep Tone, 0 highest, 255 lowest

 STA $6005       ; VIA T1CH    ;We are now beeping

 It will not stop until the system is reset or you turn it off.

 BEEP Off:

 STZ $600b       ;VIA ACR ;No more beep! 
