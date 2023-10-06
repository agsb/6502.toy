; adapted from 
; Copyright (c) 2015, Dieter Hauer
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


    .setcpu   "6502"

    .export _i2cClear
    .export _i2cStart
    .export _i2cStop
    .export _i2cAck
    .export _i2cNak
    .export _i2cWrite
    .export _i2cRead

;----------------------------------------------------------------------

    PRA  = VIA+1
    DDRA = VIA+3
    
    SDA = (1 << 0)
    SCL = (1 << 1)

    SDAN = ~(SDA)
    SCLN = ~(SCL)

    .segment "ONCE"

    FROM = bios_from
    INTO = bios_into
    PAGE = bios_f
    BYTE = bios_w
;
;---------------------------------------------------------------------
;
;   uses FROM as reference for address origin
;   uses INTO as reference for address destin
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
;   Byte is sent, (Byte) is received
;   after each Ack device responds with Ack
;   do not use more than a device page in Block

;---------------------------------------------------------------------
; need adjust steps
_rem2ram: 
    ldy #$0
@loop:
    jsr _i2c_getc
    sta (INTO),y
    iny
    cpy PAGE
    bne loop
    rts

;---------------------------------------------------------------------
; need adjust steps
_ram2rem:    
    ldy #$0
@loop:
    lda (FROM),y
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
    adc INTO+0
    sta INTO+0
    bcc @next
    inc INTO+1
@next:
    ldx FROM+1
    cpx INTO+1
    bcs @cast
    ldx FROM+0
    cpx INTO+0
    bcs @cast
@cast:
    
@ends:
    rts

;----------------------------------------------------------------------
;   a = 0 write, a = 1 read
_i2csetDevice:
    ; select device and mask for write
    and #$01
    beq @tord
@towr:
    lda #00
@tord:    
    ora DEV_I2C
    jsr _i2cWrite
    rts

_i2csetAddress:
    lda FROM+1
    jsr _i2cWrite
    lda FROM+0
    jsr _i2cWrite
    rts

;----------------------------------------------------------------------
; write any byte
_i2c_putc:    
    sta BYTE
    ldx #$08
@loop:
    asl BYTE
    bcc @zero
@one:    
    lda #$01
    bcc @send
@zero:
    lda #$00
@send:
    jsr send_bit
    dex
    bne @loop
@ack:  
    jsr recv_bit     
    ; ack in accu 0 = success
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
    jsr _rec_bit
    ror 
    rol PAGE
    dex
    bne @loop
@end:     
    jsr _i2c_ack
    rts

;----------------------------------------------------------------------
; clear all pending
_i2cClear:
    jsr _i2c_stop 
    jsr _i2c_start 
    jsr sda_high
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
; send a Ack
_i2c_ack:    
    lda #$00
    jsr _send_bit
    rts

;----------------------------------------------------------------------
; send a Nak
_i2c_nak:    
    lda #$01
    jsr _send_bit
    rts

;----------------------------------------------------------------------
; send a bit
_send_bit:    
    cmp #$01     ;bit in accu
    beq @send_one
@send_zero:    
    jsr sda_down
    jmp @clock_out
@send_one:    
    jsr sda_high
@clock_out:    
    jsr scl_high
    jmp _i2c_ends

;----------------------------------------------------------------------
; receive a bit
_recv_bit:    
    jsr _i2c_wait
    lda PRA
    and #SDA
    bne @is_one
@is_zero:
    lda #$00
    jmp _i2c_ends
@is_one:    
    lda #$01
    jmp _i2c_ends

;----------------------------------------------------------------------
;   marks
;   beware: order matters

;----------------------------------------------------------------------
; ends a send or receive bit
_i2c_ends:
    jsr scl_down
    jsr sda_down
    rts      

;----------------------------------------------------------------------
; wait to receive bit
_i2c_wait:
    jsr sda_high
    jsr scl_high
    rts

;----------------------------------------------------------------------
; mark a Start
_i2c_start:    
    jsr sda_down
    jsr scl_down
    rts

;----------------------------------------------------------------------
; mark a Stop
_i2c_stop:    
    jsr sda_down
    jsr scl_high
    jsr sda_high
    rts     

;----------------------------------------------------------------------
;   bit-bang
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

