;
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


;----------------------------------------------------------------------

    PRA  = VIA+1
    DDRA = VIA+3
    
    SDA = (1 << 0)
    SCL = (1 << 1)

    SDAN = ~(SDA)
    SCLN = ~(SCL)

    TOREM = bios_from
    TORAM = bios_into
    PAGE = bios_f
    BYTE = bios_g
    DEVP = bios_h
;
;---------------------------------------------------------------------
;
;   uses TOREM as reference for address origin
;   uses TORAM as reference for address destin
;   uses PAGE as eeprom page size
;   user BYTE as byte 
;
;----------------------------------------------------------------------
;   I2C sequences for eeproms:
;
;   for write:
;
;   header: Start, Control|W, Ack, Address MSB, Ack, Address LSB, Ack, 
;   block:  Byte, (Ack), Byte, (Ack), ..., Byte, (Ack), Stop
;
;   for read:
;
;   header: Start, Control|W, Ack, Address MSB, Ack, Address LSB, Ack, 
;   ready:  Start, Control|R, Ack, 
;   block:  (Byte) Ack, (Byte) Ack, ..., (Byte) Nak, Stop
;
;   Control is byte with
;   a frame of 7 bits, 000 to 111 device
;   a low bit read/write 0 to write, 1 to read
;
;   from master view, (any) is received
;
;   do not use more than a device page in Block
;
;----------------------------------------------------------------------
	
_test_length:
	; set page size
	lda #$80	; page size of at24LC512 eeprom used in 6502toy
	sta PAGE
	lda LEN+1
	bne page2
	lda LEN+0
	cmp PAGE
	bcs page2
	sta PAGE
	; set address for REM
page2:
	; set address for RAM
	; copy a page
	; prem += page
	; pram += page
	; leng -= page
	; if len <= 0 ends
	; 
	
;----------------------------------------------------------------------
_i2c_rem2ram:
	jsr _i2c_start
	lda #$00 	; set write	
	jsr _i2c_setDevice
	jsr _i2c_setAddress
	jsr _i2c_start
	lda #$01 	; set read
	jsr _i2c_setDevice:
	; read a page
	jsr _i2c_toram
	jsr _i2c_stop
	rts
	
;---------------------------------------------------------------------
; need adjust steps
_i2c_toram: 
    ldy #$0
@loop:
    jsr _i2c_getc
    sta (TORAM),y
    iny
    cpy PAGE
    bne loop
    rts

;---------------------------------------------------------------------
; need adjust steps
_i2c_torem:    
    ldy #$0
@loop:
    lda (TOREM),y
    jsr _i2c_putc
    iny
    cpy PAGE
    bne loop
    rts

;---------------------------------------------------------------------
; move bytes
; 
_nextrem:
    
    lda PAGE
    adc TORAM+0
    sta TORAM+0
    bcc @next
    inc TORAM+1
@next:
    ldx TOREM+1
    cpx TORAM+1
    bne @cast
    ldx TOREM+0
    cpx TORAM+0
    bne @cast

@ends:

@cast:
    
    rts

;----------------------------------------------------------------------
;   a = 0 write, a = 1 read
_i2c_setDevice:
    ; select device and mask for write
    and #$01
    beq @tord
@towr:
    lda #00
@tord:    
    ora #$A0 ; all devices starts
    ora (DEV_I2C<<1)
    jsr _i2c_putc
    rts

;----------------------------------------------------------------------
_i2c_setAddress:
    lda TOREM+1
    jsr _i2c_putc
    lda TOREM+0
    jsr _i2c_putc
    rts

;----------------------------------------------------------------------
; write any byte
_i2c_putc:    
    sta BYTE
    ldx #$08
@loop:
    asl BYTE
    bcs @one
@zero:
    jsr _i2c_zero
    bcc @sent
@one:    
    jsr _i2c_one
@sent:
    dex
    bne @loop
@ack:  
    jsr recv_bit     
    ; accu 0 = Ack, 1 = Nak
    cmp #$00
    beq @end
@nak:
    ; panic !!!
@end:
    rts      

;----------------------------------------------------------------------
; read any byte
_i2c_getc:
    ldx #$08
@loop: 
    jsr _recv_bit
    ror 
    rol PAGE
    dex
    bne @loop
@end:     
    jsr _i2c_ack
    rts

;----------------------------------------------------------------------
; receive a bit
_recv_bit:    
    jsr scl_high
    ; nop ???
    lda PRA
    and #SDA
    bne @is_one
@is_zero:
    lda #$00
    bcc @ends
@is_one:    
    lda #$01
@ends:
    jsr scl_down
    rts      

;----------------------------------------------------------------------
; clear all pending
_i2c_clear:
    jsr _i2c_stop 
    jsr _i2c_start 
    jsr sda_low
    ldx #$09
@loop: 
    jsr scl_high
    jsr scl_down
    dex
    bne @loop
    jsr _i2c_start 
    jsr _i2c_stop 
    rts

;----------------------------------------------------------------------
;   marks
;   beware: order matters

;----------------------------------------------------------------------
; mark a Start, wait and take, by order  
_i2c_start:    
    jsr sda_high
    jsr scl_high
    jsr sda_down
    jsr scl_down
    rts

;----------------------------------------------------------------------
; mark a Stop,	take and give, by order
_i2c_stop:    
    jsr sda_down
    jsr scl_down
    jsr scl_high
    jsr sda_high
    rts     

;----------------------------------------------------------------------
; send a Ack
_i2c_zero:
_i2c_ack:    
    jsr sda_down
    jsr scl_high
    jsr scl_down
    rts

;----------------------------------------------------------------------
; send a Nak
_i2c_one:
_i2c_nak:    
    jsr sda_high
    jsr scl_high
    jsr scl_down
    rts

;----------------------------------------------------------------------
; send a Reset
_i2c_rst:
    jsr sda_down
    jsr scl_down
    jsr scl_high
    jsr sda_high
    jsr scl_down
    jsr scl_high
    jsr sda_down
    jsr scl_down
    rts
	
;----------------------------------------------------------------------
;   bit-bang, changes Acc, N, Z
;----------------------------------------------------------------------
sda_down:    
    lda DDRA
    ora #SDA    
    sta DDRA
    rts

;----------------------------------------------------------------------
sda_high:    
    lda #SDAN
    and DDRA
    sta DDRA
    rts     

;----------------------------------------------------------------------
scl_down:    
    lda DDRA
    ora #SCL    
    sta DDRA
    rts

;----------------------------------------------------------------------
scl_high:    
    lda #SCLN
    and DDRA
    sta DDRA
    rts

;----------------------------------------------------------------------

