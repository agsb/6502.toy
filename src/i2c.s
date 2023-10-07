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

    PRA  = VIA_PA
    DDRA = VIA_DDRA

    ; SDA = (1 << 1)
    ; SCL = (1 << 0)

;    SDAN = ~(SDA)
;    SCLN = ~(SCL)

    SDAN = $FF - (SDA)
    SCLN = $FF - (SCL)

    ; must be at zero page
    REM_PTR = bios_tmp1
    RAM_PTR = bios_tmp2
    LEN_PTR = bios_tmp3
    DEVP = bios_tmp4 + 0
    SENS = bios_tmp4 + 1
    PAGE = bios_tmp5 + 0
    BYTE = bios_tmp5 + 1
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
    
_ram2ram:
	lda #$02
	bne _moves

_rem2ram:
	lda #$01
	bne _moves

_ram2rem:
	lda #$00 
	beq _moves

_moves:
@p0:
    sta SENS
    ; set page size
    lda #$80    ; page size of at24LC512 eeprom used in 6502toy
    sta PAGE
    lda LEN_PTR+1
    bne @p1
    lda LEN_PTR+0
    cmp PAGE
    bcs @p1
    sta PAGE
@p1:
    ; copy a page
    lda SENS
    cmp #$01    ; rem2ram
    beq @toram
    cmp #$00    ; ram2rem
    beq @torem
@panic:
    sec
    rts    
@inram:
	jsr _ram2ram
    clc
    bcc @p2
@toram:
    jsr _i2c_toram
    clc
    bcc @p2
@torem:
    jsr _i2c_torem
    clc
    bcc @p2
@p2:
    ; prem += page
    clc
    lda REM_PTR+0
    adc PAGE
    sta REM_PTR+0
    bcc @p3
    inc REM_PTR+1
@p3:
    ; pram += page
    clc
    lda RAM_PTR+0
    adc PAGE
    sta RAM_PTR+0
    bcc @p4
    inc RAM_PTR+1
@p4:
    ; leng -= page
    clc
    lda LEN_PTR+0
    sbc PAGE
    sta LEN_PTR+0
    bcc @p5
    dec LEN_PTR+1
@p5:
    ; if len == 0 ends
    clc
    lda #$00
    cmp LEN_PTR+1
    bne @p7
    cmp LEN_PTR+0
    bne @p7
@p6:
    ; ends
    rts
    
@p7:
    ; loop
    jmp @p0
    
;----------------------------------------------------------------------
_i2c_inram:
    ; read a page
    ldy #$0
@loop:
    lda (REM_PTR),y
    sta (RAM_PTR),y
    iny
    cpy PAGE
    bcc @loop
	rts

;----------------------------------------------------------------------
_i2c_toram:
	
    jsr _i2c_start
    lda #$00     ; set write    
    jsr _i2c_setDevice
    jsr _i2c_setAddress

    jsr _i2c_start
    lda #$01     ; set read
    jsr _i2c_setDevice

    ; read a page
    ldy #$0
@loop:
    jsr _i2c_getc
    sta (RAM_PTR),y
    iny
    cpy PAGE
    bcs @last
    jsr _i2c_ack
    clc
    bcc @loop
@last:
    jsr _i2c_nak
    jsr _i2c_stop
    rts
    
;---------------------------------------------------------------------
; need adjust steps
_i2c_torem:    

    jsr _i2c_start
    lda #$00     ; set write    
    jsr _i2c_setDevice
    jsr _i2c_setAddress

    ; write a page
    ldy #$0
@loop:
    lda (RAM_PTR),y
    jsr _i2c_putc
	bne @loop	; wait until ready but could hang 
    iny
    cpy PAGE
    bcc @loop
@last:
    jsr _i2c_stop
    rts

;----------------------------------------------------------------------
;   a = 0 write, a = 1 read
_i2c_setDevice:
    ror
    lda DEVP
    rol
    ora #$A0 ; all devices, Microchip mask is 1010|A2|A1|A0 
    jsr _i2c_putc
    rts

;----------------------------------------------------------------------
_i2c_setAddress:
    lda REM_PTR+1
    jsr _i2c_putc
    lda REM_PTR+0
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
@qack:  
    jsr _recv_bit     
    ; accu 0 = Ack, 1 = Nak
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
    jsr sda_down
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
; mark a Stop,    take and give, by order
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

