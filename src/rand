

;----------------------------------------------------------------------
;
;   ok  ( -- w)
;
;     ©2000-2021 by Gerhard Schmidt,
;      http://www.avr-asm-tutorial.net/avr_en/apps/random_tn13/random_calc_tn13.html
;
;    seed ~ 0x02A8
;
;     also good seeds
;
.word  seed ~ 0x02A8
.word  $B167, $4A3C, $9879, $B61E, $7B26
.word  $A858, $1F88, $50D5, $419D, $5537
.word  $0224, $0527, $5EB6, $1E6D, $BCDC
.word  $92FF, $C206, $0ECD, $9361, $2823
.word  $BE0B, $B303, $6462, $0E4C, $3D24
;
random:
	lda seed_two
	eor seed_one
	adc seed_one
	ldy seed_two
	sta seed_two
	sty seed_one
	rts
	
