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
;	not so minimal bios for 6502.toy
;
;       reserved RAM:
;       $40 bytes at $0000 page zero
;       $40 bytes at $0100 page one
;       $1000 free for users
;       
;--------------------------------------------------------
;
;# Easy BIOS for a 6502toy
;
;       64bit = +/- 9223372036854775807
;
;## Memory map
;   
;       56k     $0000 to $DFFF, RAM
; 
;        8k     $E000 to $FFFF, ROM 
;
;       256     $FE00 to $FEFF, DEVICES
;
;### RAM
;
;        $0000 to $00FF page zero, hardware registers
;
;        $0100 to $01FF page one, hardware stack
;
;        $0200 page two, bios jump vectors 
;        
;        $0300 page tri, bios variables, pointers, buffers
;   
;        $0400 page qua, free
;         
;        $1000-$DFFF 56 kb user
;
;#### exclusive ROM
;
;        $E000-$FFF9 EEPROM
;
;        $FE00-$FEFF DEVICES
;
;### DEVICES
;
;        mapped onboard from dev0 to dev07 ($FE00 to $FE70):
;  
;        dev00  $FE00 bios exclusive hardware reserved
;        
;        dev01  $FE10 bios device 01, acia 6551, USART RS-232 TERMINAL
;        
;        dev02  $FE20 bios device 02, via 6522, beat, I2C, SPI, LCD, KBD
;        
;        dev03  $FE30 bios device 03, via 6522, user
;        
;        dev04 to dev07 ($FE40 to $FE70) for user
;        
;        for expansion board, dev08 to dev15 ($FE80 to $FEFF)
;
;### ROM
;        
;       $E000 to $FFFF  7.75k ROM 
;
;       At boot:
;        
;       1. Copy the NMI/IRQ address to page $0200.
;
;       2. Post
;
;       3. Initialize CIA, VIA, TIA
;
;       4. Load Forth and prompt
;
;       Extras ?
;
;       5. Copy I2C EEPROM to $1000 and jump ($1000). 
;
;       6. Shadow BIOS to SRAM.
;
;### Interrupts
;
;        the clock beat done by VIA T1 is the only NMI of 6502
;        
;### Best 
;
;        - On return, the carry flag is 0 for Ok.
;
;        - callee saves registers.
;
;        - try do not make recursive routines.
;
;        - try do not make self modify routines.
;
;        - using RAM for random access memory, as static ram with read and write.
;
;        - using ROM for read only memory, as flash eeprom parallel, just read.
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

; default reset boot address
        
        NMI_BOOT = $FFFA
        RST_BOOT = $FFFC
        IRQ_BOOT = $FFFE

;--------------------------------------------------------
; using same clock of 6551 1.8432, by half a phi0
;
; phi2 is 0.9216 MHz, 10ms is 9216 or $2400 
;
; used by VIA T1 which depends directly of phi2
;
;--------------------------------------------------------
; devices address
 
        DEVS = $FE00
        
;--------------------------------------------------------
; reserved bios interrupt

        FBI  = DEVS + $00

;--------------------------------------------------------
; sytem usart (CIA)
        ; The base address of the 6551 ACIA.
        CIA       =  DEVS + $10    
        CIA_DATA  =  CIA+0   ; Its data I/O register
        CIA_RX    =  CIA+0   ; Its data I/O register
        CIA_TX    =  CIA+0   ; Its data I/O register
        CIA_STAT  =  CIA+1   ; Its status  register
        CIA_COMM  =  CIA+2   ; Its command  register
        CIA_CTRL  =  CIA+3   ; Its control  register

;--------------------------------------------------------
; system via (VIA)
        ; The base address of the primary 6522 VIA.
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
; user via (TIA)
        ; The base address of the secondary 6522 VIA.
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

;   T1 is beat, NMI interrupt /;

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
;   at page zero
;
.segment "ZERO"

;--------------------------------------------------------
; reserved for bios
* = $000

; for safe and sake
bios_void:      .word $0

; bios service interrupt saves, BRK
bios_flag:      .word $0

; bios_beat, must be bytes ; ~49,7 days in milliseconds
bios_beat:      .byte $0, $0, $0, $0 

; bios services save space, 
bios_a:         .byte $0
bios_b:         .byte $0
bios_y:         .byte $0
bios_x:         .byte $0
bios_s:         .byte $0
bios_p:         .byte $0

; for memory moves, lda/sta (indirect), y
bios_from:      .word $0
bios_into:      .word $0
bios_many:      .word $0
bios_with:      .word $0

;
page_zero_used:

;----------------------------------------------------------------------
; $0100 to $013F, reserved for bios
* = $0100
.res $40, $00   ; bios_stack

;
;       $140 to $1FF    for user
;
page_one_used:

;----------------------------------------------------------------------
.segment "CODE"

;----------------------------------------------------------------------
; bios pointers
* = $0200

bios_list:

.res $FF, 00

page_two_used:

;----------------------------------------------------------------------
* = $0300
; bios variables

bios_boot:      .word $0
bios_seed:      .byte $0, $0, $0, $0
bios_tick:      .word $0, $0, $0, $0 

bios_tone:      .word $0, $0 
bios_acia:      .word $0, $0

bios_tmp0:      .word $0
bios_tmp1:      .word $0
bios_tmp2:      .word $0
bios_tmp3:      .word $0
bios_tmp4:      .word $0
bios_tmp5:      .word $0
bios_tmp6:      .word $0
bios_tmp7:      .word $0

bios_buffer:
    .res 128

page_tri_used:

;--------------------------------------------------------
;
.segment "ONCE"

;--------------------------------------------------------
;   generic bios code, EEPROM
* = $E000

.byte $DE,$AD,$C0,$DE

;---------------------------------------------------------------------
; jump indirect from a list, all address absolute
bios_jump:
        lda bios_list + 1, x
        pha
        lda bios_list + 0, x
        pha
        php
        rti


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
; never use before reset !
;
post:
@test_ram:
        ldy #02 
        sty bios_into + 1
        ldy #00
        sta bios_into + 0
@loop:
        lda #$00
        sta (bios_into), y
        cmp (bios_into), y
        bne @fail_ram
        lda #$55
        sta (bios_into), y
        cmp (bios_into), y
        bne @fail_ram 
        lda #$AA
        sta (bios_into), y
        cmp (bios_into), y
        bne @fail_ram
        iny
        bne @loop
        ldy bios_into + 1
        cpy #00
        beq @test_via
        iny
        sty bios_into + 1
        bne @loop  

@fail_ram:
        ldx #01
        bne @fail

@test_via:
        lda VIA_IER
        cmp #%10000000  
        beq @test_tia 

@fail_via:
        ldx #02
        bne @fail

@test_tia:
        lda TIA_IER
        cmp #%10000000  
        beq @okey

@fail_tia:
        ldx #04
        bne @fail

@okey:
        ldx #00
        rts

@fail:
        txa
        tay
@loops:
        jsr beep
        dey
        bne @loops

        clc
        bcc @fail

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
; NMI, counts beats
;       bios_beat in zero page
; at 1ms about 8 years
bios_nmi:
bios_clock:
        ; for safe
        sei 
        bit VIA_T1CL            ; clear bit
        ; cascate counter
        inc bios_beat+0
        bne @ends
        inc bios_beat+1
        bne @ends
        inc bios_beat+2
        bne @ends
        inc bios_beat+3
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
        ; bne bios_soft_endsasy

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
;       fall throught

bios_hard_easy:
        pha

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
        jmp bios_hard_ends

;--------------------------------------------------------
;    attend interrupt 
service_via:
        lda #$7F
        sta VIA_IFR;
        jmp bios_hard_ends

;--------------------------------------------------------
;    attend interrupt 
service_tia:
        lda #$7F
        sta TIA_IFR;
        jmp bios_hard_ends

;--------------------------------------------------------
;    attend interrupt 
service_cia:
        jmp bios_hard_ends

;======================================================================
;   acia init, configures 19200,N,8,1 default 6551
;
setch:
acia_init:
        ; reset CIA
        lda #0
        sta CIA_STAT
        ; %0001 1111 =  9600 baud, external receiver, 8 bit , 1 stop bit
        ; %0001 1111 = 19200 baud, external receiver, 8 bit , 1 stop bit
        lda bios_acia + 0
        sta CIA_CTRL
        ; %0000 1011 = no parity, normal mode, RTS low, INT disable, DTR low 
        lda bios_acia + 1
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
acia_rx:
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
acia_tx:
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
; internal delay, depends on phi2
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
; zzzz
; play tone, duration
; also restart
tone_play:
        sty bios_y
        tay
        ; store counter
        lda bios_tone + 0, y
        sta VIA_T2CL
        lda bios_tone + 1, y
        sta VIA_T2CH
        ldy bios_y
        rts

;--------------------------------------------------------
;
; clock beat, using VIA T1 free run 
; phi2 is 0.9216 MHz, 10ms is 9216 or $2400
;
beat_init:
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

        rts

;--------------------------------------------------------
; start beat, zzzz
beat_start:
        pha
        lda VIA_IER
        and #$7F   ;    %01111111
        ora #$60   ;    %01100000
        sta VIA_IER
        pla
        rts

;--------------------------------------------------------
; stop beat, zzzz    
beat_stop:
        pha
        ; lda #%10000000
        lda VIA_IER
        and #$7F    ;   %01111111
        ora #$80    ;   %10000000
        sta VIA_IER
        pla
        rts

;--------------------------------------------------------
; clear beat    
beat_clear:
        pha
        jsr beat_stop
        lda #$00
        sta bios_beat+0
        sta bios_beat+1
        sta bios_beat+2
        sta bios_beat+3
        ; jsr beat_start
        pla
        rts

;--------------------------------------------------------
;   tia init, extra VIA, future use
tia_init:
        ; jmp (tia_init_user)
        rts            

;----------------------------------------------------------------------

.include "i2c.s"

; i2c

rem2ram_i2c:

ram2rem_i2c:

; spi

rem2ram_spi:

ram2rem_spi:

; copy

ram2ram:
        rts

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
randomize:
        
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
;00-32-15-19-04-21-02-25-17-34-06-27-13-36-11-30-08-23-10
;  -05-24-16-33-01-20-14-31-09-22-18-29-07-28-12-35-03-26
roulette:
        pha
        tya
        pha
@loop:
        jsr randomize
        lda bios_seed + 0
        cmp #$93        ; valid 0 to 147, 148/256 repeat ~42%
        bpl @loop

        ror 
        ror 
        tay
        lda roulette_wheel, y
        sta bios_seed + 0
@ends:
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
;   reserved for devices
* = $FE00
        .res $FF, $FF

;--------------------------------------------------------
;   boot code
* = $FF00

bios_rst:
init:
        
        ; disable interrupts
        sei

        ;0x0 wise
        cld
        
        ; prepare bios stack
        ldx #($40 -1)
        txs

        ; alive
        jsr beep

        ; power on self test 
        jsr post

        ; copy interrupt vectors and bios functions

        ldy #(bios_list_end - bios_list_rom)
@loop:
        lda bios_list_rom, y
        sta bios_list, y
        dey
        bne @loop

        ; setup acia, with external clock 11.0592 MHz

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
        ; jsr copy_eep     

        ; seed 

        lda #$A8
        sta bios_seed + 0
        lda #$02
        sta bios_seed + 1
        lda #$67
        sta bios_seed + 2
        lda #$B1
        sta bios_seed + 3

        ; phi2 is 0.9216 (1.8432/2) MHz, 10ms is 9216 or $2400

        lda #$00
        sta bios_beat + 0
        lda #$24
        sta bios_beat + 1

        ; setup beat

        lda #00
        sta bios_beat + 0 
        sta bios_beat + 1
        sta bios_beat + 2 
        sta bios_beat + 3 

        jsr beat_init

        ; enable interrupts

        cli

        ; insanity for safety

@loop:
        jsr $1000

        jmp @loop

;--------------------------------------------------------
; alias at page 

jump_nmi:
        nop
        jmp ($0202)

jump_irq:
        nop
        jmp ($0204)


;----------------------------------------------------------------------
; ends list 
;

.byte $DE,$AD,$C0,$DE

;--------------------------------------------------------
; for easy init list bios functions entry point
; for easy copy to JUMPS page
bios_list_rom:

.word $0000
.word bios_nmi        
.word bios_irq
.word bios_rst

.word acia_init
.word acia_txq
.word acia_rxq
.word acia_tx
.word acia_rx

.word via_init  
.word beat_init
.word beat_start
.word beat_stop
.word beat_clear

.word tia_init
.word tone_init


.word randomize
.word roulette
bios_list_end:

;----------------------------------------------------------------------
end_of_code:

.if 0

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
 
 .endif


