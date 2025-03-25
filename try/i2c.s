; ------------------------------------------------------------------------
; ------------------------------------------------------------------------
; 				I2C
;	https://www.ele.uva.es/~jesus/6502copy/nrom3/i2c.s
; ------------------------------------------------------------------------
; ------------------------------------------------------------------------

.segment "ZERO"

tmp1:	      	.res	1
tmp2:	      	.res	1
tmp3:	      	.res	1
tmp4:	      	.res	1

ptr1:	      	.addr	2
ptr2:	      	.addr	2
ptr3:	      	.addr	2
ptr4:	      	.addr	2

sp:	      	.addr  	2 	; Stack pointer

msgldx: 
        .word $0
uputs: 
        .word $0
msgs: 
        .word $0
msgI2C: 
        .word $0
LCD_data: 
        .word $0
LCD_cmd: 
        .word $0
fatbuf: 
        .word $0


SCL     = 1
SDA     = 2

IRA: 
DDRA: 
        .byte $0 

msg = 0

; ---------------------------------------------------------------------
.segment "ONCE"

msgexe:
        .asciiz "eeprom read"

; ---------------------------------------------------------------------
	.export	start
start:	lda	DDRA		
	ora	#(1<<SDA)	; SDA = L
	sta	DDRA	
	ora	#(1<<SCL)	; SCL = L
	sta	DDRA
	rts
	
	.export	stop
stop:	lda	DDRA		
	ora	#(1<<SDA)	; SDA = L
	sta	DDRA
	and	#~(1<<SCL)	; SCL = H
	sta	DDRA	
	and	#~(1<<SDA)	; SDA = H
	sta	DDRA
	rts

; send A through the I2C bus 

	.export	outbyte
outbyte:
	ldx	#8
ob1:	asl	a
	pha
	lda	DDRA
	bcc	ob2		; if Cy = 1 => SDA = 1, else SDA = 0
	and	#~(1<<SDA)
	bcs	ob3		; uncondicional (Cy is 1)
ob2:	ora	#(1<<SDA)	
ob3:	sta	DDRA
	and	#~(1<<SCL)	; SCL = H
	sta	DDRA
	nop
	ora	#(1<<SCL)	; SCL = L
	sta	DDRA
	pla
	dex
	bne	ob1
	rts

; read a byte from the I2C bus and returns it in A
; tmp2: modiffied

	.export	inbyte
inbyte:
	ldx	#8
ib1:	lda	DDRA
	and	#~(1<<SCL)	; SCL = H
	sta	DDRA
	asl	tmp2
	lda	#(1<<SDA)
	bit	IRA
	beq	ib2
	inc	tmp2
ib2:	lda	DDRA
	ora	#(1<<SCL)	; SCL = L
	sta	DDRA
	dex
	bne	ib1
	lda	tmp2
	rts

; Check the ACK bit
; returns Cy=1 if NACK

	.export	tstack
tstack:	lda	DDRA
	and	#~(1<<SDA)	; SDA = H
	sta	DDRA
	and	#~(1<<SCL)	; SCL = H
	sta	DDRA
	clc
	lda	#(1<<SDA)
	bit	IRA		; Check ACK
	beq	tsa1
	sec
tsa1:	lda	DDRA
	ora	#(1<<SCL)	; SCL = L
	sta	DDRA
	rts

; generates an ACK or NACK bit, always returns with Z=0

	.export	genack, gennack
genack: lda	DDRA
	ora	#(1<<SDA)	; SDA = L
	bne	gak1		; unconditional
gennack:
	lda	DDRA
	and	#~(1<<SDA)	; SDA = H
gak1:	sta	DDRA
	and	#~(1<<SCL)	; SCL = H
	sta	DDRA
	nop
	ora	#(1<<SCL)	; SCL = L
	sta	DDRA
	rts
	
; ------------------------------------------------------------------------
; I2C EEPROM write routine
; ptr1:	pointer to data 
; tmp1: data length
; tmp3:	EEPROM byte address 
; X: EEPROM I2C address (left aligned i.e. $A0) 
; returns CY=1 if NACK, CY=0 if OK. tmp1, X e Y modiffied

	.export	i2cwr
i2cwr:	jsr	start
	txa
	and	#$FE		; ensure write
	jsr	outbyte
	jsr	tstack
	bcs	i2sn2		; NACK -> abort
	lda	tmp3
	jsr	outbyte
	jsr	tstack
	bcs	i2sn2		; NACK -> abort
	ldy	#0
	lda	tmp1		; length=0 -> end
	beq	i2sn2
i2sn1:	;lda	(ptr1), y
	lda	ptr1, y
	iny
	jsr	outbyte
	jsr	tstack
	bcs	i2sn2
	dec	tmp1
	bne	i2sn1
i2sn3:	clc
i2sn2:	jsr	stop
i2mf2:	rts

; ------------------------------------------------------------------------
; I2C read routine
; ptr1:	pointer to data
; tmp1: data length
; X: I2C address (left aligned)
; returns CY=1 if NACK, CY=0 if OK. tmp1, X e Y modiffied

	.export	i2crd
i2crd:	jsr	start
	txa
	ora	#1		; ensure read
	jsr	outbyte
	jsr	tstack
	bcs	i2sn2		; NACK -> abort
	ldy	#0
i2r1:	lda	DDRA
	and	#~(1<<SDA)	; SDA = H
	sta	DDRA
	jsr	inbyte		; data read
	;sta	(ptr1), y
	sta	ptr1, y
	iny
	dec	tmp1
	beq	i2r2
	jsr	genack		; not last byte yet -> send ACK
	jmp	i2r1
i2r2:	jsr	gennack		; last byte -> send NACK
	bne	i2sn3		; unconditional jump

; ------------------------------------------------------------------------
; ------------------------------------------------------------------------
; bootI2C: reads the EEPROM content to the address specified in the header
; if the appropiate mark $B0,$CA is present. The loaded code is executed
; if the execution address his higher or equal than $300
; ------------------------------------------------------------------------
; ------------------------------------------------------------------------

	.export	bootI2C
bootI2C:
	lda	#<fatbuf	; set a temporary destination pointer
	sta	ptr1
	lda	#>fatbuf
	sta	ptr1+1
	lda	#0
	sta	tmp1
	sta	tmp3		; Reset the EEPROM counter
	ldx	#$A0
	jsr	i2cwr
	bcs	i2mf2
	lda	#(14+128+64)	; 14th pos. on LCD
	jsr	LCD_cmd
	lda	#'i'		; 'i' means I2C EEPROM present
	jsr	LCD_data
	ldx	#(msgI2C-msgs)	; notify also on the UART
	jsr	uputs
	lda	#6		; reading just the 6-byte header
	sta	tmp1
	ldx	#$A0
	jsr	i2crd
	bcs	i2mf
	lda	#$B0
	cmp	fatbuf
	bne	i2mf
	lda	#$CA
	cmp	fatbuf+1
	bne	i2mf
	lda	#(14+128+64)	; 14th pos. on LCD
	jsr	LCD_cmd
	lda	#'I'		; 'I' means valid mark
	jsr	LCD_data
	ldx	#(msgldx-msgs)	; notify also on the UART
	jsr	uputs
	lda	fatbuf+2	; save header pointers
	sta	ptr1
	lda	fatbuf+3
	sta	ptr1+1
	lda	fatbuf+4
	sta	ptr2
	lda	fatbuf+5
	sta	ptr2+1	
	
i3cmem:	lda	#0		; Reset the EEPROM address again
	sta	tmp1
	sta	tmp3
	ldx	#$A0
	stx	tmp4		; I2C address (increments every 256 bytes)
	jsr	i2cwr
	bcs	i2mf
	lda	#8
	sta	tmp3		; page counter (8 pages * 256 bytes = 2kb)
i2m1:	ldx	tmp4
	jsr	i2crd		; 256 byte read
	bcs	i2mf
	inc	ptr1+1
	inc	tmp4
	inc	tmp4
	dec	tmp3
	bne	i2m1
	
	lda	ptr2+1		; execute if address >= $300
	cmp	#3
	bcc	i2mf
	ldx	#(msgexe-msgs)	; notify execution on UART
	jsr	uputs
	;jmp	(ptr2)
	jmp	ptr2
i2mf:	rts		

