

;----------------------------------------------------------------------
; copy page zero, page one, registers to a swap area
pagging:
; address 02 is the destin, must be a page at $XX00
;   sta (page),y 
;   lda (page),y
;----------------------------------------------------------------------
swapinto:
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
        bne loop_one
        inc PAGE+1
@loop_zero:
        lda $0000, y
        sta (PAGE), y
        iny
        bne loop_zero
        rts

;----------------------------------------------------------------------
swapfrom:
        ; inc PAGE+1
@loop_zero:
        lda (PAGE), y
        sta $0000, y
        iny
        bne loop_zero
        dec PAGE+1
@loop_zero:
        lda (PAGE), y
        sta $0100, y
        iny
        bne loop_one
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
        
        
