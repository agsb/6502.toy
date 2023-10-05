reg  	= $dd01 ;CIA 2 PortB
datadir = $dd03 ;1 = output, 0 = input

sda_p   = %00000100 ;CIA Port b2 (UP E)
scl_p   = %00001000 ;CIA Port b3 (UP F)

;-----------------------
;--[ set data direct ]--
;-----------------------

sda_out:
		lda datadir
        ora #sda_p
        bne sda_end

sda_in:  
		lda datadir
        and #sda_p:$ff
        sta datadir
sda_end:
        rts

both_out: 
		jsr sda_out
        ;fallthrough

scl_out:  
		lda datadir
        ora #scl_p
        bne scl_end

scl_in:  
		lda datadir
        and #scl_p:$ff
        sta datadir
scl_end:
        rts


