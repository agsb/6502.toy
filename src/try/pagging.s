
.segment "CODE"

;----------------------------------------------------------------------
;
;       how much for a context swap ?
;
;       moving pages zero and one into/from somewhere
;       costs about 2060 instructions and 7716 cycles
;
;
;
;

;----------------------------------------------------------------------
; copy page zero, page one, registers to a swap area
pagging:
; address in page is the destin, must be a page at $XX00
;   sta (page),y 
;   lda (page),y
;----------------------------------------------------------------------
swapinto:
@reduce:
        php
        pha
        tya
        pha
        txa
        pha
        tsx
        txa
        pha
        ldy #0
@loop_one:
        lda $0100, y
        sta (PAGE), y
        iny
        bne @loop_one
        inc PAGE+1
@loop_zero:
        lda $0000, y
        sta (PAGE), y
        iny
        bne @loop_zero
        rts

;----------------------------------------------------------------------
swapfrom:
        ; inc PAGE+1
        ldy #0
@loop_zero:
        lda (PAGE), y
        sta $0000, y
        iny
        bne @loop_zero
        dec PAGE+1
@loop_one:
        lda (PAGE), y
        sta $0100, y
        iny
        bne @loop_one
@restore:        
        pla
        tax
        txs
        pla
        tax
        pla
        tay
        plp
        rts
        
;----------------------------------------------------------------------

PAGE = $04

;----------------------------------------------------------------------

.import exit

.export _main

_main:
        
        lda #$00
        sta PAGE + 0
        lda #$02
        sta PAGE + 1

        jsr swapinto

        jsr swapfrom
        
        rts

