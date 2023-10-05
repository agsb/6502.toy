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
    .export _i2cNck
    .export _i2cWrite
    .export _i2cRead

;----------------------------------------------------------------------

    VIA1_BASE   = $8100
    PRA  = VIA_BASE+1
    DDRA = VIA_BASE+3
    
    SDA = (1 << 0)
    SCL = (1 << 1)

    .segment "CODE"

;----------------------------------------------------------------------
; clear all pending
_i2cClear:
    jsr _i2cStop 
    jsr _i2cStart 
    sda_high
    ldx #$09
@loop: 
    scl_high
    scl_down
    dex
    bne @loop
    jsr _i2cStart 
    jsr _i2cStop 
    rts

;----------------------------------------------------------------------
; write one byte
_i2cWrite:    
    sta i2c_byte
    ldx #$00
    stx PRA
    ldx #$09
@loop:
    dex
    beq @ack
    asl i2c_byte
    bcc @send_zero
@send_one:    
    lda #$01
    jsr send_bit
    jmp @loop
@send_zero:
    lda #$00
    jsr send_bit
    jmp @loop
@ack:  
    jsr rec_bit     ; ack in accu 0 = success
@end:
    clc
    rts      

;----------------------------------------------------------------------
; read one byte
_i2cRead:
    stx PRA
    ldx #$09
@loop: 
    dex
    beq @end
    jsr rec_bit
    asl i2c_byte
    ora
    jmp @loop
@end:     
    clc
    rts

;----------------------------------------------------------------------
; send a bit
send_bit:    
    cmp #$01     ;bit in accu
    beq @set_sda
@clear_sda:    
    jsr sda_down
    jmp @clock_out
@set_sda:    
    jsr sda_high
    jmp @clock_out
@clock_out:    
    jsr scl_high
    jmp _i2cEnds

;----------------------------------------------------------------------
; receive a bit
rec_bit:    
    jsr sda_high
    jsr scl_high
    lda PRA
    and #SDA
    bne @is_one
@is_zero;
    lda #$00
    jmp @end
@is_one:    
    lda #$01
@end:  
    jmp _i2cEnds

;----------------------------------------------------------------------
; send a Ack
_i2cAck:    
    lda #$00
    jsr send_bit
    rts

;----------------------------------------------------------------------
; send a Nak
_i2cNak:    
    lda #$01
    jsr send_bit
    rts

;----------------------------------------------------------------------
; ends a sed/receive bit
_i2cEnds:
    jsr scl_down
    jsr sda_down
    rts      

;----------------------------------------------------------------------
; mark a Start
_i2cStart:    
    jsr sda_down
    jsr scl_down
    rts

;----------------------------------------------------------------------
; mark a Stop
_i2cStop:    
    jsr scl_high
    jsr sda_high
    rts     

;----------------------------------------------------------------------
sda_down:    
    lda DDRA
    ora #SDA    
    sta DDRA
    rts

;----------------------------------------------------------------------
sda_high:    
    lda #SDA
    eor #$FF
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
    lda #SCL
    eor #$FF
    and DDRA
    sta DDRA
    rts

;----------------------------------------------------------------------

