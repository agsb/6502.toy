
ACIA_EXTRAS = 0
.IF ACIA_EXTRAS

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
