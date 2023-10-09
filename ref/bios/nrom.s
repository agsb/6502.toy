	VIA =$0200
	LCD =$0220
	
	IRB  = VIA
	IRA  = VIA+1
	DDRB = VIA+2
	DDRA = VIA+3
	T1CL = VIA+4
	T1CH = VIA+5
	T1LL = VIA+6
	T1LH = VIA+7
	T2CL = VIA+8
	T2CH = VIA+9
	SR   = VIA+10
	ACR  = VIA+11
	PCR  = VIA+12
	IFR  = VIA+13
	IER  = VIA+14
	IRANH= VIA+15
	
	SCL  = 1	; I2C SCL at PA1
	SDA  = 2	; I2C SDA at PA2
	
	RXD  = 3	; UART input  at PA3
	TXD  = 4	; UART output at PA4

	SND  = 5	; SOUND at PA5
	
	CS1  = 6	; SPI slave select (SD)
	CS2  = 7	; SPI slave select (ETH)

; ------------------------------------------------------------------------
; Define and export the ZP variables for the C64 runtime

	.exportzp	sp, sreg, regsave
  	.exportzp	ptr1, ptr2, ptr3, ptr4
  	.exportzp	tmp1, tmp2, tmp3, tmp4
  	.exportzp	regbank, zpspace

.zeropage

zpstart	= *
tmp1:	      	.res	1
tmp2:	      	.res	1
tmp3:	      	.res	1
tmp4:	      	.res	1
ptr1:	      	.res	2
ptr2:	      	.res	2
ptr3:	      	.res	2
ptr4:	      	.res	2
sp:	      	.res   	2 	; Stack pointer
sreg:	      	.res	2	; Secondary register/high 16 bit for longs
regsave:      	.res	2	; slot to save/restore (E)AX into
regbank:      	.res	6	; 6 byte register bank

	.exportzp	MMCcmd,sector,cluster,FAT,FATsecperclus
	.exportzp	FATnrootsec,FATrootsec,Filesize
	
MMCcmd:		.res	1	; MMC command+address(32 bits)+crc
MMCaddr3:	.res	1
MMCaddr2:	.res	1
MMCaddr1:	.res	1
MMCaddr0:	.res	1
MMCcrc:		.res	1
sector:
sector0:	.res	1	; sector: 24 bits
sector1:	.res	1
sector2:	.res	1

cluster:	.res	2	; current cluster (16 bits)
FAT:		.res	3	; first sector of the FAT (1st copy)
FATsecperclus:	.res	1	
FATnrootsec:	.res	2	; number of sectors of the root directory
FATrootsec:	.res	3	; first sector of the root directory
Filesize:	.res	4

zpspace	= * - zpstart		; Zero page space allocated

; IRQ Vectors 
	.exportzp	brkp, nmivector, irqothervector, viavector
	brkp		= $F7
	nmivector 	= $FA
	irqothervector 	= $FC
	viavector 	= $FE
; ------------------------------------------------------------------------
; ------------------------------------------------------------------------

.segment	"INIT"

; ------------------------------------------------------------------------
; ------------------------------------------------------------------------

.code
.if DEBUG
	lda	#0
	sta	DDRB
	sta	IRA	; ORA: all bits as 0
	lda	#1
	sta	DDRA	; DDRA: all bits as inputs but PA0
			; This selects RAM instead of ROM
.endif

; ------------------------------------------------------------------------
; ------------------------------------------------------------------------
; 				main program
; ------------------------------------------------------------------------
; ------------------------------------------------------------------------

; jump table with I/O routines
	jmp	str0		; $E000
	jmp	uart_putch	; $E003
	jmp	uart_getch	; $E006
	jmp	uart_gets	; $E009
	jmp	prthexuart	; $E00C
	jmp	beep1		; $E00F
	jmp	i2cwr		; $E012
	jmp	i2crd		; $E015
	jmp	spibyte		; $E018
	jmp	spiwr		; $E01B
	jmp	spird		; $E01E
	jmp	mmc_init	; $E021
	jmp	mmc_rd_sector	; $E024
	jmp	mmc_wr_sector	; $E027
	jmp	FAT_init	; $E02A
	jmp	FAT_search_dir	; $E02D
	jmp	FAT_clus2sec	; $E030
	jmp	FAT_next_cluster; $E033
fakerts:
	brk			; $E036
	brk
	rts
	
; ------------------------------------------------------------------------
; ------------------------------------------------------------------------
; 				Bitbang UART
;   This code must fit in a single page and it must be page aligned in
;   order to avoid wrong timmings due to page wrap
;
; -------------------  38400 bps (26 cycles per bit) ---------------------
; ------------------------------------------------------------------------

; ------------------------------------------------------------------------
; uart_getch:
; returns: A = input character , X,Y,tmp1 modiffied

	.export	uart_getch
uart_getch:
	lda	#(1<<RXD)
uarx1:	bit	IRA		; waiting for the start bit
	bne	uarx1		;2 cycles (not taken)

	ldx	#8		;2 cycles 
	ldy	#5		;2 cycles. First time the delay is 1.5 bits
				; 38 cyles total (ideal 39)
uarx2:	dey			;2 cycles
	bne	uarx2		;3 cycles, total: 5 x Y -1 
	
	nop			;2 cycles
uarx4:	
	ldy	#1		;2 cycles, 26 cycles total 
	
	lda	IRA		;4 cycles
	and	#(1<<RXD)	;2 cycles
	sbc	#1		;2 cycles
	
	ror	tmp1		;5 cycles
	dex			;2 cycles
	bne	uarx2		;3 cycles
	
	lda	#(1<<RXD)
uarx3:	bit	IRA		;waiting for the stop bit
	beq	uarx3	
	lda	tmp1
	rts

; ------------------------------------------------------------------------
; uart_putch
; parameters: A = data to print
; returns: A, X, Y, tmp1 modiffied

	.export	uart_putch
uart_putch:
	sta 	tmp1
	ldy 	#9
	sec			; carry is going to be the stop bit
	ldx 	#1		; 26 cycles for start bit
	lda 	#(1<<TXD)
	ora 	DDRA
	sta 	DDRA		; start bit
	bne	uatx1		; 3 cycles, always taken
uatx1:	bne	uatx4		; 3 cycles
uatx4:	nop			; 2 cycles

uatx5:	ror 	tmp1		; 5 cycles, 26 cycles/loop
	bcs 	uatx2		; 3 cycles
	lda 	#(1<<TXD)	; 2 cycles
	ora 	DDRA		; 4 cycles
	sta 	DDRA		; 4 cycles-----
	bcc 	uatx3		; 3 cycles
	
uatx2:	lda 	#~(1<<TXD)
	and 	DDRA
	sta 	DDRA
	bcs 	uatx3	
uatx3:	dey			; 2 cycles
	bne 	uatx5		; 3 cycles
	rts
	
;-----------------------------------------------------------------------
;-----------------------------------------------------------------------
; 			START
;-----------------------------------------------------------------------
;-----------------------------------------------------------------------
	
	; Some sanity for software resets
	.export	_start
_start:
str0:	sei
	cld
	ldx	#$ff		; Stack at the end of its page
	txs
	
	; VIA init
	lda	#%00000000	; CB2,CA2 inputs, negetive edge
	sta	PCR
	lda	#%00011000	;Shift out under phi2, no latch, no timed irqs
	sta	ACR

	; ISR vector init
	lda	#<defISR
	sta	nmivector
	sta	irqothervector
	sta	viavector
	lda	#>defISR
	sta	nmivector+1
	sta	irqothervector+1
	sta	viavector+1

	; clear breakpoint
	lda	#0
	sta	brkp
	sta	brkp+1
	
	jsr	beep
	
	jsr	lcdinit
	lda	#10
	jsr	uart_putch
	ldx	#0
	jsr	uputs
	ldx	#(msgcls+12-msgs)
	jsr	uputs

	jsr	bootI2C
	ldx	#(msgcls+12-msgs)
	jsr	uputs
	jsr	bootSD
		
	lda	#10
	jsr	uart_putch

;--------------------------------------------
;  Test code
;--------------------------------------------

;---------------------------------------------	
	
chos0:	clv
chos:	lda	#(1<<RXD)	; if activity at RXD
	and	IRA		; go to monitor
	bne	chos1
	jsr	fakerts		; there is a BRK there
	jmp	_start
chos1:	bvc	chos		; if activity at the 6502's S.O. pin
				; continue with bootloader
;-----------------------------------------------------------------------
;-----------------------------------------------------------------------
; Bootloader
;-----------------------------------------------------------------------
;-----------------------------------------------------------------------

	.export	bootld
bootld:	lda	#$C0		;LCD Set DDRAM addr: second line
	sta	LCD
	jsr	del5m
	ldx	#(msgcls-msgs)	;Clear line with spaces
	jsr	bputs
	lda	#$C0		;LCD Set DDRAM addr: second line
	sta	LCD
	jsr	del5m
	lda	#'.'		; '.' means waiting for block header
	sta	LCD+1
	jsr	del5m
	lda	#$C0		;LCD Set DDRAM addr: second line
	sta	LCD
	
	ldx	#16		;wait for a few edges before calibration
bt1:	clv
bt2:	bvc	bt2;
	dex
	bne	bt1;
	
	lda	#0		;Calibration
	sta	tmp1
	sta	tmp2
	ldy	#0		;256 pulses
bt3:	ldx	#0
	clv
bt4:	inx
	bvc	bt4
	txa
	clc
	adc	tmp1
	sta	tmp1
	lda	#0
	adc	tmp2
	sta	tmp2		;processing time: 26 cycles
	dey
	bne	bt3		;The average lenght of pulses remains stored at tmp2
	
	lda	#$55		;waiting for start mark ($0f)
bt7:	ldx	#0
	clv
bt8:	inx
	bvc	bt8
	cpx	tmp2
	rol	a
	cmp	#$0f
	bne	bt7
	
	lda	#'/'
	sta	LCD+1
	
	jsr	byteso		;get the Number of bytes of the block
	sta	tmp3
	sta	ptr4
	jsr	byteso
	sta	tmp4
	sta	ptr4+1

	sec			; Twos complement of byte counter
	lda	#0		; (better to INC than to DEC)
	sbc	tmp3
	sta	tmp3
	lda	#0
	sbc	tmp4
	sta	tmp4
	
	jsr	byteso
	sta	ptr1		;get the Load address
	sta	ptr3
	jsr	byteso
	sta	ptr1+1
	sta	ptr3+1
	jsr	byteso
	sta	ptr2		;get the Execution address
	jsr	byteso
	sta	ptr2+1
	
bt9:	jsr	byteso		;get the data
	ldy	#0
	sta	(ptr1),y
	inc	ptr1
	bne	bt10
	inc	ptr1+1
bt10:	inc	tmp3
	bne	bt9
	inc	tmp4
	bne	bt9		
	
;-----------------------------------------------------------------------
	lda	#0
	cmp	ptr2+1		; If (Execution address == $00xx) do not execute 
	bne	bt11
	ldx	#(msgload-msgs)	; signal the block load on LCD
	jsr	bputs
	lda	ptr3+1		; Load address to LCD
	jsr	prthexlcd
	lda	ptr3
	jsr	prthexlcd
bt22:	;jsr	del1s
	jmp	chos0
	
bt11:	lda	#1		; If (Execution address == $01xx) write data to EEPROM (2048 bytes)
	cmp	ptr2+1
	bne	bt12
	ldx	#(msgI2C-msgs)	; signal it to LCD
	jsr	bputs
	
	lda	ptr3		; writting I2C EEPROM
	sta	ptr1
	lda	ptr3+1
	sta	ptr1+1
	lda	#$A0
	sta	tmp2
	lda	#0
	sta	tmp3
bt20:	jsr	del5m
	lda	#16
	sta	tmp1
	ldx	tmp2
	jsr	i2cwr
	bcs	bt21

	lda	#16
	adc	ptr1
	sta	ptr1
	lda	#0
	adc	ptr1+1
	sta	ptr1+1
	clc
	lda	#16
	adc	tmp3
	sta	tmp3
	bne	bt20

	ldx	tmp2
	inx
	inx
	stx	tmp2
	cpx	#$B0
	bne	bt20
	
	ldx	#(msgdone-msgs)
bt23:	jsr	bputs
	jmp	chos0
	
bt21:	ldx	#(msgnack-msgs)
	jmp	bt23
		
bt12:	lda	#2		; If (Execution address == $02xx) just don't execute (same as load)
	cmp	ptr2+1
	bne	bt13
	jmp	chos0
	
bt13:	ldx	#(msgboot-msgs) ; Other addresses: Exeute the loaded code
	jsr	bputs		; signal it on LCD
	jsr	del1s
	jmp	(ptr2)		; Execute the code
;-----------------------------------------------------------------------

byteso:	ldy	#8	; byte reading through S.O. pin 
bt5:	ldx	#0
	clv
bt6:	inx
	bvc	bt6
	cpx	tmp2
	rol	a
	dey
	bne	bt5
	rts		; total processing time: 21 cycles

;-----------------------------------------------------------------------
;-----------------------------------------------------------------------
; 			     LCD Display
;
; The BUSY signal is not checked to avoid hangs if the LCD is not
; connected. We wait 5 ms after each command or data instead.
;-----------------------------------------------------------------------
;-----------------------------------------------------------------------

	.export	del1s
del1s:	ldx	#0
bl3:	jsr	del5m
	dex
	bne	bl3
	rts

	.export	del5m
del5m:	pha
	lda 	#166
bl1:	jsr	jsrnop	; 12 cycles
	jsr	jsrnop
	clc
	sbc	#0
	bne	bl1
	pla
jsrnop:	rts

	.export	lcdinit
lcdinit:		; LCD Init sequence
	ldx	#5	; 25 ms of delay for the LCD power-on reset
bl2:	jsr	del5m
	dex
	bne	bl2
	lda	#$38	; Function Set: 8-bit bus, 2 lines, font 5x8
	ldx	#3
bl5:	sta	LCD
	jsr	del5m
	dex
	bne	bl5
	lda	#$01	; Clear display
	sta	LCD
	jsr	del5m
	lda	#$06	; Entry mode: Increment, no scroll
	sta	LCD
	jsr	del5m
	lda	#$0C	; Display control: On, cursor OFF	
	sta	LCD
	jsr	del5m
	lda	#$80	; Set DDRAM addr: First line
	sta	LCD
	jsr	del5m
	
	; Wellcome message
	ldx	#0
	jsr	bputs
	rts
	
	; Print a character string to LCD
	; X: offset from 'msgs'
	; (all menssages must fit in 256 bytes)
	.export	bputs
bputs:
blb1:	lda	msgs,x
	beq	blb2
	sta	LCD+1	
	jsr	del5m
	inx
	bne	blb1
blb2:	rts		

; ------------------------------------------------------------------------
; Number printing routines
; ------------------------------------------------------------------------

lcdhexdig:
	cmp	#10
	bcc	lcdh1	; Cy=1 => A >= 10
	adc	#('A'-('9'+1)-1)	; Cy was 1
lcdh1:	;clc	; not needed Cy is always 0
	adc	#'0'
	sta	LCD+1
	jmp	del5m

; ------------------------------------------------------------------------
; Print A as an hexadecimal number to LCD
	.export	prthexlcd
prthexlcd:
	pha
	lsr	a
	lsr	a
	lsr	a
	lsr	a
	jsr	lcdhexdig
	pla
	and	#$0f
	jmp	lcdhexdig
; ------------------------------------------------------------------------

uarthexdig:
	cmp	#10
	bcc	uarth1	; Cy=1 => A >= 10
	adc	#('A'-('9'+1)-1)	; Cy was 1
uarth1:	;clc	; not needed Cy is always 0
	adc	#'0'
	jmp	uart_putch

; ------------------------------------------------------------------------
; Print A as an hexadecimal number to UART

	.export	prthexuart 
prthexuart:
	pha
	lsr	a
	lsr	a
	lsr	a
	lsr	a
	jsr	uarthexdig
	pla
	and	#$0f
	jmp	uarthexdig


;-----------------------------------------------------------------------
;-----------------------------------------------------------------------
;				SOUND
;-----------------------------------------------------------------------
;-----------------------------------------------------------------------
	.export	beep, beep1
beep:	ldy	#96		; aprox. 500 us / half cycle (1KHz)
beep1:	sty	tmp1
	ldx	#0		; 256 half cycles (128 ms)
bbb1:	lda	DDRA		; DDRA
	eor	#(1<<SND)	; toggle PA5 through DDRA
	sta	DDRA
	ldy	tmp1
bbb2:	dey
	bne	bbb2
	dex
	bne	bbb1
	ldy	tmp1	
	rts

; ------------------------------------------------------------------------
; ------------------------------------------------------------------------
; 				I2C
; ------------------------------------------------------------------------
; ------------------------------------------------------------------------
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
i2sn1:	lda	(ptr1),y
	iny
	jsr	outbyte
	jsr	tstack
	bcs	i2sn2
	dec	tmp1
	bne	i2sn1
i2sn3:	clc
i2sn2:	jsr	stop
	rts

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
	sta	(ptr1),y
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
	bcc	bi2c1
	jmp	i2mf
bi2c1:	lda	#(14+128)	; 14th pos. on LCD
	sta	LCD
	jsr	del5m
	lda	#'i'		; 'i' means I2C EEPROM present
	sta	LCD+1
	jsr	del5m
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
	lda	#(14+128)	; 14th pos. on LCD
	sta	LCD
	jsr	del5m
	lda	#'I'		; 'I' means valid mark
	sta	LCD+1
	jsr	del5m
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
	jmp	(ptr2)
i2mf:	rts		
	
; ------------------------------------------------------------------------	
; ------------------------------------------------------------------------	
; other UART related routines (not timming critical)
; ------------------------------------------------------------------------	
; ------------------------------------------------------------------------	
; uputs: prints an ASCIIZ string
; arguments: X = offset from "msgs"
; returns: A, X, Y, tmp1, tmp2 modiffied

	.export	uputs
uputs:	lda	msgs,x
	beq	upt1
	inx
	stx	tmp2
	jsr	uart_putch
	ldx	tmp2
	jmp	uputs
upt1:	rts	

; ------------------------------------------------------------------------	
; uart_gets: gets a line with some editing
; arguments: ptr1 = pointer to destination data buffer
; output: A, X, Y, tmp1, tmp2 modiffied

	.export	uart_gets
uart_gets:
	ldy 	#0
	sty	tmp2
uags1:	jsr 	uart_getch
	cmp 	#$D		; Ignore CR
	beq 	uags1
	cmp 	#$A		; End of Line
	bne 	uags2
	lda 	#0
	ldy	tmp2
	sta 	(ptr1),y
	rts
uags2:	cmp 	#$7F		; Backspace
	bne 	uags3
	lda	#0
	cmp	tmp2
	beq 	uags1
	dec	tmp2
	lda	#8		; one position back
	jsr	uart_putch
	lda	#32		; erase old character by writing a space
	jsr	uart_putch
	lda	#8		; one position back again
	jsr	uart_putch
	jmp	uags1
uags3:	ldy	tmp2
	sta 	(ptr1),y
	jsr	uart_putch	; echo
	inc	tmp2
	jmp 	uags1

; ------------------------------------------------------------------------
; ------------------------------------------------------------------------
; 				    SPI
; ------------------------------------------------------------------------
; ------------------------------------------------------------------------

; single transfer
; 14 cycles between SR write and ORB read
 
	.export	spibyte
spibyte:
	sta	SR	; 4 cycles
	jsr	delrts	; 12 cycles
	nop		; 2 cycles
	lda	IRB	; 4 cycles	
delrts:	rts		; 6 cycles

; block transfer (up to 256 bytes), writting
; ptr1 -> origin of data
; Y: number of bytes (1 minimum, 0 means 256 bytes)
; 19 us/byte (421 Kbit/s)
; A: modiffied

	.export	spiwr
spiwr:	sty	tmp1
	ldy	#0
spiwr1:	lda	(ptr1),y	; 5 cycles
	sta	SR		; 4 cycles
	nop			; 2 cycles
	iny			; 2 cycles
	cpy	tmp1		; 3 cycles
	bne	spiwr1		; 3 cycles
	rts
	
; block transfer (up to 256 bytes), reading
; ptr1 <- destination of data
; Y: number of bytes (minimum: 2 bytes, 0 means 256 bytes)
; 22 us/byte (363 Kbit/s)
; A, X, tmp1: modiffied

	.export	spird
spird:	ldx	#$ff
	stx	SR		; 4 cycles (dummy write)

	dey			; 2 cycles
	sty	tmp1		; 3 cycles
	ldy	#0		; 2 cycles
	nop			; 2 cycles
	nop			; 2 cycles
	beq	spird1		; 3 cycles
	
spird1:	lda	IRB		; 4 cycles
	stx	SR		; 4 cycles (dummy write)
	sta	(ptr1),y	; 6 cycles
	iny			; 2 cycles
	cpy	tmp1		; 3 cycles
	bne	spird1		; 3 cycles / 2 cycles if not taken
	
spird2: nop			; 2 cycles
	lda	IRB		; 4 cycles
	sta	(ptr1),y
	iny
	rts

; ------------------------------------------------------------------------
; ------------------------------------------------------------------------
; 				MMC stuff
; ------------------------------------------------------------------------
; ------------------------------------------------------------------------
; MMC_send_command: sends a 6-byte command to MMC/SD card
; arguments: MMCcmd structure
; returns: A: first byte of response (R1 according to standard)
; 	   X: modiffied

	.export	MMC_send_command
MMC_send_command:
	lda	#$ff		; dummy data
	jsr	spibyte
	ldx	#0
mmcsc1:	lda	MMCcmd,x	; command + addr + crc (6 bytes)
	jsr	spibyte
	inx
	cpx	#6
	bne	mmcsc1
	lda	#$ff
	jsr	spibyte		; dummy data
	ldx	#8
mmcsc2:	lda	#$ff		; wait for response up to 8 bytes
	jsr	spibyte
	bpl	mmcsc3
	dex
	bne	mmcsc2
mmcsc3:	rts

; Selection routines for MMC. (/CS is PA6, controlled from DDRA)

	.export	mmc_cs_l, mmc_cs_h
mmc_cs_l:
	pha
	lda	#(1<<CS1)	; /CS Low
	ora	DDRA
mmccs1:	sta	DDRA
	pla
	rts
mmc_cs_h:
	pha
	lda	#~(1<<CS1)	; /CS High
	and	DDRA
	jmp	mmccs1
	
; ------------------------------------------------------------------------
; mmc_init: performs a SD card initialization acording to the SD standard
; (more or less)

	.export	mmc_init
mmc_init:
	jsr	mmc_cs_h	; /CS High
	
	ldx	#100		; some clock cycles for the card
mmcin1:	lda	#$ff
	jsr	spibyte
	dex
	bne	mmcin1
	
	jsr	mmc_cs_l	; /CS low
	
	lda	#$40		; GO_IDLE_STATE command
	sta	MMCcmd		; (with /CS low puts the card in SPI mode)
	lda	#0
	sta	MMCaddr0
	sta	MMCaddr1
	sta	MMCaddr2
	sta	MMCaddr3
	lda	#$95		; CRC has to be correct for this command
	sta	MMCcrc
	jsr	MMC_send_command
	jsr	mmc_cs_h	; /CS en high
	cmp	#1		; correct response is 1: idle, no other error
	beq	mmcin2
mmcin0:	sec			; return with error
	rts
mmcin2:	jsr	spibyte		; 8 clock cycles
	jsr	mmc_cs_l
	lda	#$48		; SEND_IF_COND command (required for SD 2.0)
	sta	MMCcmd
	lda	#1
	sta	MMCaddr1
	lda	#$D5		; CRC has to be correct for this command
	sta	MMCcrc
	jsr	MMC_send_command
	sta	tmp1
	jsr	spibyte		; read 4 more response bytes
	jsr	spibyte
	jsr	spibyte
	sta	tmp2		; this byte contains the card's voltage 
	jsr	spibyte	
	jsr	mmc_cs_h
	lda	#4		; invalid command response if SD v 1.x
	bit	tmp1
	bne	mmcin6
	lda	#$0f
	and	tmp2
	cmp	#1		; voltage index = 1 (2.7 to 3.6 Volt)
	bne	mmcine
	
mmcin6:	lda	#0		; Now we send ACMD41 until an active response
	sta	MMCaddr1
	lda	#$ff
	sta	MMCcrc    
	ldy	#40		; number of retries
mmcin5:	lda	#$ff
	jsr	spibyte		; 8 clock cycles
	jsr	mmc_cs_l
	lda	#$77		; APP_CMD is required prior to ACMDs
	sta	MMCcmd
	jsr	MMC_send_command
	lda	#$ff
	jsr	spibyte
	lda	#$69		; ACMD41 command
	sta	MMCcmd
	lda	#$ff
	jsr	MMC_send_command
	and	#1		; retry if still in idle state
	beq	mmcin4
	ldx	#5
mmcin3:	jsr	del5m		; 25 ms delay between retries
	dex
	bne	mmcin3
	jsr	mmc_cs_h
	dey
	bne	mmcin5
mmcine:	sec			; return with error
	rts
mmcin4:	clc			; return OK
	rts

; ------------------------------------------------------------------------
; mmc_read_sector: 
; arguments: sector[0,1,2]: sector to be read
;	     ptr1: pointer to destination buffer
; returns CY=1 if error
; modiffies: A, X, Y, MMCcmd structure

	.export	mmc_rd_sector
mmc_rd_sector:
	lda	#0		; MMC address = sector * 512
	sta	MMCaddr0
	lda	sector0
	asl	a
	sta	MMCaddr1
	lda	sector1
	rol	a
	sta	MMCaddr2
	lda	sector2
	rol	a
	sta	MMCaddr3
	
	jsr	mmc_cs_l
	lda	#$51
	sta	MMCcmd		; READ_SINGLE_BLOCK command
	jsr	MMC_send_command
	ora	#0
	bne	mmcrd1
	
mmcrd2:	lda	#$ff		; wait for data tokem
	jsr	spibyte
	cmp	#$fe	
	bne	mmcrd2
	
	ldy	#0
	jsr	spird		; 256 bytes to low buffer
	inc	ptr1+1
	jsr	spird		; 256 bytes to high buffer
	dec	ptr1+1
	lda	#$ff		; ignore CRC16
	jsr	spibyte
	lda	#$ff
	jsr	spibyte
	jsr	mmc_cs_h
	clc
	rts	
	
mmcrd1:	jsr	mmc_cs_h	; error, set carry
	sec
	rts
	
; ------------------------------------------------------------------------
; mmc_write_sector: 
; arguments: sector[0,1,2]: sector to be written
;	     ptr1: pointer to source buffer
; returns CY=1 if error
; modiffies: A, X, Y, MMCcmd structure

	.export mmc_wr_sector
mmc_wr_sector:
	lda	#0		; MMC address = sector * 512
	sta	MMCaddr0
	lda	sector0
	asl	a
	sta	MMCaddr1
	lda	sector1
	rol	a
	sta	MMCaddr2
	lda	sector2
	rol	a
	sta	MMCaddr3
	
	jsr	mmc_cs_l
	lda	#$58
	sta	MMCcmd		; WRITE_SINGLE_BLOCK command
	jsr	MMC_send_command
	ora	#0
	bne	mmcwr1
	
	lda	#$FE
	jsr	spibyte		; send data tokem
	
	ldy	#0		; 256 bytes from low buffer
	jsr	spiwr
	inc	ptr1+1
	jsr	spiwr		; 256 bytes from high buffer
	dec	ptr1+1
	lda	#$ff		; send a dummy CRC-16
	jsr	spibyte
	lda	#$ff
	jsr	spibyte
	lda	#$ff
	jsr	spibyte		; response
mmcwr2:	lda	#$ff
	jsr	spibyte		; wait while busy (response==0)
	beq	mmcwr2
	jsr	mmc_cs_h
	clc
	rts

mmcwr1: jsr	mmc_cs_h
	sec
	rts
; ------------------------------------------------------------------------
; ------------------------------------------------------------------------
; 			FAT-16 Filesystem
; ------------------------------------------------------------------------
; ------------------------------------------------------------------------
; FAT_init: locates filesystem partition, read parameters and offsets
; returns CY=1 if something went wrong

	.export	FAT_init
FAT_init:
	lda	#0
	sta	sector0		; read partition table (sector 0)
	sta	sector1
	sta	sector2
	lda	#<fatbuf	; destination buffer
	sta	ptr1
	lda	#>fatbuf
	sta	ptr1+1
	jsr	mmc_rd_sector
	bcc	fati0
	jmp	fatie
	
fati0:	lda	fatbuf+$1c6	; save filesystem offset (only 24 bits)
	sta	sector0		; the MSB byte is always 0
	sta	FAT
	lda	fatbuf+$1c7
	sta	sector1
	sta	FAT+1
	lda	fatbuf+$1c8
	sta	sector2
	sta	FAT+2

fati00:	jsr	mmc_rd_sector	; read superblock
	bcs	fatie

	clc			; FAT = offset to FAT (in sectors)
	lda	fatbuf+14	; FAT = offset to partition + reserved sectors
	adc	FAT
	sta	FAT
	lda	fatbuf+15
	adc	FAT+1
	sta	FAT+1
	bcc	fati4
	inc	FAT+2
fati4:	lda	FAT
	sta	FATrootsec	; offset to root directory
	lda	FAT+1		; FATrootsec = offset to FAT + sectors/FAT*nFAT
	sta	FATrootsec+1
	lda	FAT+2
	sta	FATrootsec+2

	lda	fatbuf+22
	sta	tmp1		; tmp1,tmp2: sectors/FAT
	lda	fatbuf+23
	sta	tmp2
	lda	fatbuf+16
	sta	tmp3		; tmp3: nfat

	; multiply and accumulate
fati1:	lsr	tmp3		; nfat*sectors_per_fat
	bcc	fati2
	clc
	lda	tmp1
	adc	FATrootsec
	sta	FATrootsec
	lda	tmp2
	adc	FATrootsec+1
	sta	FATrootsec+1
	bcc	fati2
	inc	FATrootsec+2
fati2:	asl	tmp1
	rol	tmp2
	lda	tmp3
	bne	fati1
	
	lda	fatbuf+13
	sta	FATsecperclus	; Parameter: sectors / cluster

	lda	fatbuf+17	; number of root entries
	sta	FATnrootsec	; FATnrootsec: number of sectors of root
	lda	fatbuf+18	; FATnrootsec = number of root entries * 16
	sta	FATnrootsec+1
	lsr	FATnrootsec+1
	ror	FATnrootsec
	lsr	FATnrootsec+1
	ror	FATnrootsec
	lsr	FATnrootsec+1
	ror	FATnrootsec
	lsr	FATnrootsec+1
	ror	FATnrootsec	
	clc
	rts

fatie:	sec
	rts

; ------------------------------------------------------------------------
; FAT_search_dir: search for a file in the root directory
; Arguments: ptr2: pointer to a 11-character filename (name spaces ext)
; returns CY=1 if not found, else cluster and Filesize are set
; modiffies A, X, Y, tmp[1,2,3,4], ptr1, regbank[1,2,3], sector[0,1,2], MMCcmd

	.export	FAT_search_dir
FAT_search_dir:
	lda	FATrootsec
	sta	sector0
	lda	FATrootsec+1
	sta	sector1
	lda	FATrootsec+2
	sta	sector2
	lda	#<fatbuf
	sta	ptr1
	lda	#>fatbuf
	sta	ptr1+1
	lda	FATnrootsec
	sta	regbank+1
	lda	FATnrootsec+1
	sta	regbank+2
	
fatsd1:	lda	#16
	sta	regbank
	jsr	mmc_rd_sector
	lda	ptr1
	sta	tmp3
	lda	ptr1+1
	sta	tmp4
fatsd2:	ldy	#0
fatsd3:	lda	(ptr2),y	; filename comparison
	cmp	(tmp3),y
	bne	fatsd4
	iny
	cpy	#11		; up to 11 bytes
	bne	fatsd3
	jmp	fatsd10		; found
fatsd4:	
	lda	#32		; next directoy entry
	clc
	adc	tmp3
	sta	tmp3
	bcc	fatsd5
	inc	tmp4
fatsd5:	dec	regbank
	bne	fatsd2

	inc	sector0		; next sector of directory
	bne	fatsd6
	inc	sector1
	bne	fatsd6
	inc	sector2
fatsd6:	dec	regbank+1
	bne	fatsd1

	sec			; NOT FOUND
	rts
	
fatsd10: 
	ldy	#26		; Found: save file data
	lda	(tmp3),y
	sta	cluster
	iny
	lda	(tmp3),y
	sta	cluster+1
	iny
	lda	(tmp3),y
	sta	Filesize
	iny
	lda	(tmp3),y
	sta	Filesize+1
	iny
	lda	(tmp3),y
	sta	Filesize+2
	iny
	lda	(tmp3),y
	sta	Filesize+3

	clc
	rts

; ------------------------------------------------------------------------
; cluster to sector
; ------------------------------------------------------------------------
; arguments: cluster,cluster+1
; result: sector0,1,2

	.export	FAT_clus2sec
FAT_clus2sec:
	lda	cluster
	sta	tmp1
	lda	cluster+1
	sta	tmp2
	lda	tmp1	; cluster=cluster-2
	sec
	sbc	#2
	sta	tmp1
	lda	tmp2
	sbc	#0
	sta	tmp2
	lda	#0
	sta	tmp3
	
	clc
	lda	FATrootsec	; offset to root + number of root sectors
	adc	FATnrootsec
	sta	sector0
	lda	FATrootsec+1
	adc	FATnrootsec+1
	sta	sector1
	lda	FATrootsec+2
	adc	#0
	sta	sector2
	
	lda	FATsecperclus
	sta	tmp4
	; multiply and accumulate ( += sectors per cluster * cluster )
fatsd11:
	lsr	tmp4
	bcc	fatsd12
	clc
	lda	tmp1
	adc	sector0
	sta	sector0
	lda	tmp2
	adc	sector1
	sta	sector1
	lda	tmp3
	adc	sector2
	sta	sector2	
fatsd12:
	asl	tmp1
	rol	tmp2
	rol	tmp3
	lda	tmp4
	bne	fatsd11
	
	rts	; returns: sector=(cluster-2)*FATsecperclus

; ------------------------------------------------------------------------
; Search the next cluster from FAT table
; ------------------------------------------------------------------------
; arguments: cluster,cluster+1
; result: cluster,cluster+1, CY=1 if no more clusters in the current chain
; modiffies A,X,Y, ptr1, sector

	.export FAT_next_cluster
FAT_next_cluster:
	lda	#<fatbuf	; temporary buffer
	sta	ptr1
	lda	#>fatbuf
	sta	ptr1+1

	clc
	lda	FAT		; sector = FAT + cluster/256
	adc	cluster+1
	sta	sector0
	lda	FAT+1
	adc	#0
	sta	sector1
	lda	FAT+2
	adc	#0
	sta	sector2
	jsr	mmc_rd_sector
	
	lda	cluster
	asl	a		; 2 byte per cluster (FAT-16)
	tax
	bcs	fatnc1		
	lda	fatbuf,x	; First half of sector (256 bytes)
	sta	cluster
	lda	fatbuf+1,x
	sta	cluster+1
	jmp	fatnc2
fatnc1:	lda	fatbuf+$100,x	; Second half of sector (256 bytes)
	sta	cluster
	lda	fatbuf+$101,x
	sta	cluster+1
fatnc2: 
	lda	#$0f		; if cluster=$FFFx returns CY=1
	ora	cluster
	cmp	#$ff
	bne	fatnc3
	lda	cluster+1
	cmp	#$ff
	bne	fatnc3
	sec		
	rts
fatnc3:	clc
bsdff:	rts

;-----------------------------------------------------------------
;		SD bootloader
;-----------------------------------------------------------------
	.export bootSD

bootSD:	jsr	mmc_init
	bcs	bsdff
	lda	#(15+128)	; 14th pos. on LCD
	sta	LCD
	jsr	del5m
	lda	#'s'		; 's' means SD card present
	sta	LCD+1
	jsr	del5m	
	ldx	#(msgSD-msgs)	; notify on UART also
	jsr	uputs

	jsr	FAT_init
	bcs	bsdff
	
	lda	#<bootfilename
	sta	ptr2
	lda	#>bootfilename
	sta	ptr2+1
	jsr	FAT_search_dir
	bcs	bsdff
	
	jsr	FAT_clus2sec	; reading 1st sector to inspect header
	lda	#<fatbuf	
	sta	ptr1
	lda	#>fatbuf
	sta	ptr1+1
	jsr	mmc_rd_sector
	bcs	bsdff

	lda	fatbuf		; check for mark: $B0,$CA
	cmp	#$B0
	bne	bsdff
	lda	fatbuf+1
	cmp	#$CA
	bne	bsdff

	lda	#(15+128)	; 14th pos. on LCD
	sta	LCD
	jsr	del5m
	lda	#'S'		; 'S' means valid boot file
	sta	LCD+1
	jsr	del5m	
	ldx	#(msgldx-msgs)
	jsr	uputs
	
	lda	fatbuf+2	; save load address and exec. address
	sta	ptr1
	lda	fatbuf+3
	sta	ptr1+1
	lda	fatbuf+4
	sta	ptr2
	lda	fatbuf+5
	sta	ptr2+1

	; read the file to final destination
bsd1:	ldx	FATsecperclus
bsd2:	txa
	pha
	jsr	mmc_rd_sector

	inc	sector0		; next sector
	bne	bsd3
	inc	sector1
	bne	bsd3
	inc	sector2

bsd3:	inc	ptr1+1		; ptr1 += 512
	inc	ptr1+1
	
	sec			; Filesize-=512
	lda	Filesize+1
	sbc	#2
	sta	Filesize+1
	lda	Filesize+2
	sbc	#0
	sta	Filesize+2
	lda	Filesize+3
	sbc	#0
	sta	Filesize+3
	bmi	bsdd		; Filesize negative -> end
	lda	Filesize
	ora	Filesize+1
	ora	Filesize+2
	ora	Filesize+3
	beq	bsdd		; Filesize = 0 -> end

	pla
	tax
	dex
	bne	bsd2		; next sector until cluster done
	
	lda	ptr1
	pha
	lda	ptr1+1
	pha
	jsr	FAT_next_cluster ; next cluster 
	pla
	sta	ptr1+1
	pla
	sta	ptr1
	bcs	bsdf
	
	jsr	FAT_clus2sec	; convet to sector
	jmp	bsd1		

bsdd:	pla			; load done
	lda	ptr2+1		; execute if exec address >= $300
	cmp	#3
	bcc	bsdf
	ldx	#(msgexe-msgs)	; notify execution on UART
	jsr	uputs
	jmp	(ptr2)
	
bsdf:	rts
	
; ------------------------------------------------------------------------
; ------------------------------------------------------------------------
; 			IRQ & NMI routines
; ------------------------------------------------------------------------
; ------------------------------------------------------------------------

	.export _irqbrk
_irqbrk:
rql1:	pha			; save registers
	txa
	pha
	tya
	pha
	
	cld			; decimal mode off

	tsx
	inx
	inx
	inx
	inx
	lda	$100,x		; Check if BRK
	and	#$10
	bne	dobrk
	; Not BRK 
nbrk:	lda	#$20		; check for Timer2 IRQ
	bit	IFR
	bpl	irql2		; not from VIA
	bne	dotimer		; from timer2
	; setting up a stack frame for a RTI return from a software vector
	lda	#>finirq	; PCH
	pha
	lda	#<finirq	; PCL
	pha
	lda	#0		; Status reg
	pha
	jmp    	(viavector) 
irql2:	; Not BRK nor from VIA
	; setting up a stack frame for a RTI return from a software vector
	lda	#>finirq	; PCH
	pha
	lda	#<finirq	; PCL
	pha
	lda	#0		; Status reg
	pha
	jmp	(irqothervector)
	
sstep:	tsx
	inx
	inx
	inx
	inx
	lda	#~4
	and	$100,x		; Clear the IRQ mask bit on the stack
	sta	$100,x
	; new IRQ just after returning and fetching 1 op-code	
	lda	#$A0
	sta	IER		; enable TIMER2 IRQ
	lda	#20		; 21.5 cycles until irq
	sta	T2CL		; write latch
	lda	#0
	sta	T2CH		; Write counter, clear IRQ
	
finirq:	; 22 cycles until return
	pla			; restore registers
	tay
	pla
	tax
	pla
defISR:	rti

	; NMI: jump to software vector
_nmi:	jmp	(nmivector)

; ------------------------------------------------------------------------
; 			Timer IRQ code (single step)
; ------------------------------------------------------------------------

	stacktop=ptr4		; aliases for some ZP variables
	tmp5=ptr4+1
	zptop=sp

dotimer:
	lda	T2CL		; clear IRQ
	ldy	#0		; saving needed ZP variables
dotim1:	lda	0,y
	pha
	iny
	cpy	#zptop
	bne	dotim1
	
	inx
	inx
reprt:	stx	tmp3		; begining of stack frame
	stx	stacktop		; (copy)
	lda	$100,x		; ptr1 = PC
	sta	ptr1+1
	dex
	lda	$100,x
	sta	ptr1
	jmp	dobrk4
; ------------------------------------------------------------------------
; 				BRK code
; ------------------------------------------------------------------------
dobrk:	ldy	#0		; saving needed ZP variables
dobrk1:	lda	0,y
	pha
	iny
	cpy	#zptop
	bne	dobrk1
	
	inx
	inx
	stx	tmp3		; begining of stack frame
	stx	stacktop	; (copy)

	; Check for breakpoint
	
	dex
	sec
	lda	$100,x		; ptr1 = PC-2
	sbc	#2
	sta	ptr1
	inx
	lda	$100,x
	sbc	#0
	sta	ptr1+1
		
	cmp	brkp+1		; ptr1 == brkp?
	bne	dobrk3
	lda	ptr1
	cmp	brkp
	bne	dobrk3
	ldy	#0		; it was a programmed breakpoint
	lda	brkp+2		; restore the original opcode
	sta	(ptr1),y
	lda	ptr1+1		; and adjust the PC copy on the stack
	sta	$100,x
	dex
	lda	ptr1
	sta	$100,x
	inx
	jmp	dobrk4
	
	; This breackpoint wasn't programmed, leave PC as it is
dobrk3:	lda	$100,x		; ptr1 = PC
	sta	ptr1+1
	dex
	lda	$100,x
	sta	ptr1
	
	lda	ptr1		; check if PC points to the fake RTS
	cmp	#<(fakerts+2)
	bne	dobrk35
	lda	ptr1+1
	cmp	#>(fakerts+2)
	bne	dobrk35
	ldx	#(msgpause-msgs)
	jsr	uputs
	jsr	uart_getch	; pause
	jmp	dobrk4		; don't alert in this case
dobrk35:
	ldy	#192		; alert of BRK with a beep
	jsr	beep1
	
	lda	#$01		; Clear LCD
	sta	LCD
	jsr	del5m
	ldx	#(msgBRK-msgs) 	; *** BRK *** message on LCD
	jsr	bputs
	
	ldx	#(msghome-msgs)	; Go to upper-left corner of screen
	jsr	uputs
	ldx	#(msgBRK-msgs)	; *** BRK *** message on UART
	jsr	uputs
	jsr	nlinecls
	jmp	dbrk1
	
; ------------------------------------------------------------------------
;	Print state: registers
; ------------------------------------------------------------------------

dobrk4:	ldx	#(msghome-msgs)	; Go to upper-left corner of screen
	jsr	uputs
	
	; Register dump
dbrk1:	ldx	#(msgPC-msgs)
	jsr	uputs
	lda	ptr1+1		; PCH
	jsr	prthexuart
	dec	tmp3
	lda	ptr1		; PCL
	jsr	prthexuart
	dec	tmp3
	ldx	#(msgP-msgs)
	jsr	uputs
	ldx	tmp3
	lda	$100,x		; P
	ldy	#0		; Print P register bit by bit
bpreg:	asl	a
	pha
	lda	#'.'
	bcc	bpreg1
	lda	msgflags,y	
bpreg1:	sty	tmp4
	jsr	uart_putch
	ldy	tmp4
	pla
	iny
	cpy	#8
	bne	bpreg

	ldx	#(msgA-msgs)
	jsr	uputs
	dec	tmp3
	ldx	tmp3
	lda	$100,x		; A
	jsr	prthexuart	

	ldx	#(msgX-msgs)
	jsr	uputs
	dec	tmp3
	ldx	tmp3
	lda	$100,x		; X
	jsr	prthexuart	

	ldx	#(msgY-msgs)
	jsr	uputs
	dec	tmp3
	ldx	tmp3
	lda	$100,x		; Y
	jsr	prthexuart
	
	ldx	#(msgS-msgs)
	jsr	uputs
	lda	tmp3
	clc
	adc	#5		; S value before BRK
	jsr	prthexuart
	
	jsr	nlinecls
	ldx	#(msgZP-msgs)
	jsr	uputs
; ------------------------------------------------------------------------
;	Print state:
;	dissassemble 16 instrs at PC and dump Zero Page of memory
; ------------------------------------------------------------------------
	lda	tmp3
	sta	tmp5		; temporary copy of stack top
	jsr	nlinecls	; End of line
	
	jsr	dissaOP
	lda	ptr1		; save next instrucction address
	sta	ptr3
	lda	ptr1+1
	sta	ptr3+1
	ldx	#(msgtab28-msgs)
	jsr	uputs
	lda	#0
	jsr	prthexuart
	ldx	#(msgspm-msgs)
	jsr	uputs
	
pzp1:	dec	tmp5
	ldx	tmp5		; print ZP varibles from the stack
	lda	$100,x
	jsr	prthexuart
	lda	#' '
	jsr	uart_putch
	tsx
	inx
	cpx	tmp5
	bne	pzp1
	ldy	#sp		; the rest of first line from ZP
pzp2:	lda	0,y
	iny
	sty	tmp3
	jsr	prthexuart
	lda	#' '
	jsr	uart_putch
	ldy	tmp3
	cpy	#16
	bne	pzp2
	jsr	nlinecls	; End of line
	ldy	#16
	
pzp3:	tya			; rest of lines
	pha	
pzp5:	jsr	dissaOP
	ldx	#(msgtab28-msgs)
	jsr	uputs
	pla
	pha
	jsr	prthexuart
	ldx	#(msgspm-msgs)
	jsr	uputs
	lda	#16
	sta	tmp5
	pla
pzp4:	pha
	tay
	lda	0,y
	jsr	prthexuart
	lda	#' '
	jsr	uart_putch	
	pla
	tay
	iny
	beq	pzpf
	tya
	dec	tmp5
	bne	pzp4
	pha
	jsr	nlinecls
	jmp	pzp5
pzpf:	jsr	nlinecls
	jsr	nlinecls
	ldx	#(msgstack-msgs)	; print stack trace
	jsr	uputs
	lda	stacktop
	sta	tmp3
	lda	#24
	sta	tmp5
pst1:	inc	tmp3
	beq	pst2
	ldx	tmp3
	lda	$100,x
	jsr	prthexuart
	lda	#' '
	jsr	uart_putch
	dec	tmp5
	bne	pst1
pst2:	jsr	nlinecls
	
; ------------------------------------------------------------------------
; read a command from uart and execute it
; ------------------------------------------------------------------------

mon1:	ldx	#(msgmonprom-msgs) ;prompt on UART
	jsr	uputs
	jsr	uart_getch	; get character
	pha
	jsr	uart_putch	; echo
	pla
	cmp	#'s'		; -------- single step -------
	bne	mon2
	jsr	nline
	sec
	jmp	dobrk5
mon2:	cmp	#'m'		; -------- memory dump -------
	bne	mon3
	jsr	dodir		; read address into ptr1
	bcs	mon1
	jsr	hexdump		; do dump
	jmp	mon1
mon3:	cmp	#'g'		; -------- goto address (execute) -------
	bne	mon35
	jsr	dodir		; read address into ptr1
	bcs	mon1
mon31:	clc
	ldx	stacktop	; and place it on the stack
	lda	ptr1+1
	sta	$100,x
	dex
	lda	ptr1
	sta	$100,x
	jmp	dobrk5
mon35:	cmp	#'t'		; --------  trace address (execute sigle-step)
	bne	mon4
	jsr	setbrk		; read breakpoint & address
	bcc	mon31		; unconditional branch
	jmp	mon1
mon4:	cmp	#'d'		; -------- dissasemble code ------- 
	bne	mon5
	jsr	dodir		; read address into ptr1
	bcs	mon1
dissa:	ldx	#16		; number of instuctions to dissasemble
dis01:	txa
	pha
	jsr	dissaOP		; dissasemble one OP-code
	jsr	nlinecls
	pla
	tax
	dex
	bne	dis01
	jmp	mon1
mon5:	cmp	#'c'		; -------- continue -------
	bne	mon6
mon55:	jsr	nline
	clc
	jmp	dobrk5
mon6:	cmp	#'n'		; -------- execute until next -------
	bne	mon7
	ldy	#0
	lda	(ptr3),y	; save orig opcode
	sta	brkp+2
	lda	ptr3		; save address
	sta	brkp
	lda	ptr3+1
	sta	brkp+1
	lda	#0
	sta	(ptr3),y	; put a BRK at the next instr
	jsr	nline
	clc
	jmp	dobrk5
mon7:	cmp	#'b'		; -------- place a breackpoint -------	
	bne	mon8
	jsr	setbrk
	jmp	mon1
mon8:	cmp	#' '		; -------- redraw screen -------
	bne	mon9
mon85:	ldx	stacktop
	jmp	reprt
mon9:	ldx	stacktop	; get reg. position in stack
	dex			; and copy it into ptr1
	dex
	cmp	#'p'		; -------- edit P reg. -------
	bne	mon10
	stx	tmp4
	lda	#' '
	jsr	uart_putch
	ldx	#(msgflags-msgs)
	jsr	uputs
	ldx	tmp4
mon95:	stx	ptr1		; change stack value
	lda	#1
	sta	ptr1+1
	lda	#' '
	jsr	uart_putch
	jsr	hexinby		; edit byte
	jmp	mon85		; and redraw screen
mon10:	dex
	cmp	#'a'		; -------- edit A reg. -------
	beq	mon95
	dex
	cmp	#'x'		; -------- edit X reg. -------
	beq	mon95
	dex
	cmp	#'y'		; -------- edit Y reg. -------
	beq	mon95
	cmp	#'e'		; -------- edit memory -------
	bne	mon12
	jsr	dodir		; get address of memory
	bcs	monf
mon11:	lda	#' '
	jsr	uart_putch
	jsr	hexinby		; edit byte
	bcs	mon111
	inc	ptr1		; increment memory pointer
	bne	mon11
	inc	ptr1+1
	jmp	mon11
mon111:	jmp	mon1
mon12:	cmp	#'r'		;-------- execute the rest of subroutine -------
	bne	mon13
	tsx			; move stack trace 2 bytes down
	txa
	tay
	iny
	dex
	dex
	txs
	inx
mon121:	lda	$100,y
	sta	$100,x
	inx
	iny
	cpy	stacktop
	bne	mon121
	lda	$100,y		; last stack data
	sta	$100,x
	inx
	lda	#<(fakerts-1)	; place the fake return address on top of stack
	sta	$100,x
	inx
	lda	#>(fakerts-1)
	sta	$100,x
	jmp	mon55		; and continue execution
mon13:	cmp	#'h'
	bne	monf
	ldy	#0
	sty	tmp3
mon131:	ldy	tmp3
	lda	monhelp,y
	beq	mon132
	jsr	uart_putch
	inc	tmp3
	bne	mon131
mon132:	
monf:	jmp	mon1

	; restore variables 
dobrk5:	ldy	#zptop
dobrk2:	pla
	dey
	sta	0,y
	bne	dobrk2
	bcc	dobrk6
	jmp	sstep		; finish IRQ/BRK with single-step
dobrk6:	jmp	finirq		; finish IRQ/BRK

; ------------------------------------------------------------------------
; debugger-related routines
; ------------------------------------------------------------------------
setbrk:	jsr	dodir		; read address into ptr1
	bcs	sbrk2
	lda	ptr1
	sta	brkp
	lda	ptr1+1
	sta	brkp+1
	ldy	#0
	lda	(ptr1),y
	sta	brkp+2
	tya
	sta	(ptr1),y	; brk op-code
	clc
sbrk2:	rts
		
; ------------------------------------------------------------------------
; print utilities
; ------------------------------------------------------------------------
nline:	lda	#10
	jmp	uart_putch
nlinecls:
	ldx	#(msgnlcls-msgs)
	jmp	uputs
	

; ------------------------------------------------------------------------
; reads an address into ptr1. CY=1 -> read abort (ptr1 modiffied anyway)
; ------------------------------------------------------------------------

dodir:	jsr	uart_getch	; read char.
	cmp	#10		; EOL -> return
	bne	dodir3
dodir5:	lda	#10
	jsr	uart_putch
	clc
	rts
dodir3:	cmp	#' '		; space -> input hex. number
	bne	dodir
	jsr	uart_putch	
	jsr	hexin
	bcc	dodir5
	rts

; ------------------------------------------------------------------------
; dump a page of hexadecimal data
; returns: ptr1 pointing to the following page, 
;	   A, X, Y, tmp[1,2,3,4] modiffied
; ------------------------------------------------------------------------

hexdump:
	lda	#16			; 16 lines of 16 bytes
	sta	tmp4

hdump0:	lda	ptr1+1			; print hex. address
	jsr	prthexuart
	lda	ptr1
	jsr	prthexuart
	ldx	#(msgcls+14-msgs)	; print 2 spaces 
	jsr	uputs
	
	lda	#16
	sta	tmp2
	ldy	#0
hdump1:	lda	(ptr1),y		; print hex. data
	sty	tmp3
	jsr	prthexuart	
	lda	#' '			; print space
	jsr	uart_putch
	ldy	tmp3
	iny
	dec	tmp2
	bne	hdump1
	
	lda	#' '			; print space
	jsr	uart_putch
	
	lda	#16			; print ASCII data
	sta	tmp2
	ldy	#0
hdump2:	lda	(ptr1),y
	bpl	hdump3			; don't print ASCII > 127
	lda	#'.'
	bpl	hdump4
hdump3: cmp	#32			; don't print ASCII < 32
	bpl	hdump4
	lda	#'.'
hdump4:	sty	tmp3
	jsr	uart_putch
	ldy	tmp3
	iny
	dec	tmp2
	bne	hdump2
	
	lda	#10			; new line
	jsr	uart_putch

	clc				; PTR += 16
	lda	#16
	adc	ptr1
	sta	ptr1
	bcc	hdump5
	inc	ptr1+1

hdump5:	dec	tmp4
	bne	hdump0
	rts

; ------------------------------------------------------------------------
; inputs a 16-bit address into ptr1, CY=1 -> abort
; ------------------------------------------------------------------------

hexin:	lda	ptr1+1		; print current value
	jsr	prthexuart
	lda	ptr1
	jsr	prthexuart
	
hexin1:	jsr	uart_getch	; get chararecter
	cmp	#10		; EOL -> exit
	bne	hexin6
	clc
	rts
hexin6:	cmp	#27		; ESC -> abort
	bne	hexin5
	sec
	rts
hexin5:	jsr	ascii2hex
	bcs	hexin1
	ldx	#4
hexin4:	asl	ptr1		; shift value 4 bits and include new digit
	rol	ptr1+1
	dex
	bne	hexin4
	ora	ptr1
	sta	ptr1
	
	ldx	#(msgback4-msgs) ;go back 4 positions in the line
	jsr	uputs
	jmp	hexin

; ------------------------------------------------------------------------
; inputs a 8-bit address into (ptr1), CY=1 -> abort, tmp3 modiffied
; ------------------------------------------------------------------------
hexinby:
	ldy	#0
	lda	(ptr1),y
	sta	tmp3

hxb6:	jsr	prthexuart	; print current value
	
hxb1:	jsr	uart_getch	; get character
	cmp	#10		; NL: end edit
	bne	hxb2
hxb3:	lda	tmp3
	ldy	#0
	sta	(ptr1),y
	clc
	rts
hxb2:	cmp	#' '		; space: end edit
	beq	hxb3
	cmp	#27		; ESC : abort edit
	bne	hxb4
	sec
	rts
hxb4:	jsr	ascii2hex
	bcs	hxb1
	ldx	#4
hxb5:	asl	tmp3		; shift value 4 bits and include new digit
	dex
	bne	hxb5
	ora	tmp3
	sta	tmp3
	ldx	#(msgback4+2-msgs) ;go back 2 positions in the line
	jsr	uputs
	lda	tmp3
	jmp	hxb6

; ----------------------------------------------
; converts an ASCII Hex. char into a 4-bit value
; returns CY = 1 if not valid hex. digit
; ----------------------------------------------

ascii2hex:
a2x1:	cmp	#'0'		; ASCII < '0' -> ignore char.
	bmi	a2xf
	ora	#32		; to lowercase
	cmp	#'a'
	sec
	bpl	a2x2
	sbc	#'0'		; numbers (0 to 9)
	cmp	#10
	bpl	a2xf
	jmp	a2x3
a2x2:	sbc	#('a'-10)	; letters (a to f)
	cmp	#16
	bpl	a2xf
a2x3:	clc			; hex
	rts		
a2xf:	sec			; not hex
	rts

; ------------------------------------------------------------------------
; 			Disassemble a single OP-code
; ------------------------------------------------------------------------
	.export	dissaOP
dissaOP:	
	lda	ptr1+1		; print address
	jsr	prthexuart
	lda	ptr1
	jsr	prthexuart
	lda	#' '
	jsr	uart_putch
	ldy	#0
	lda	(ptr1),y	; get OP-code
	sta	tmp3
	jsr	prthexuart	; print first byte of instruction
	lda	#' '
	jsr	uart_putch
	inc	ptr1		; increment pointer
	bne	dis1
	inc	ptr1+1
dis1:	
	; search op-code in the instruction table
	lda	#<optbl
	sta	ptr2
	lda	#>optbl
	sta	ptr2+1
	ldx	#((optblf-optbl)/5)	; number of OP-code entries (151)
	
dis2:	lda	tmp3		; test this op-code
	cmp	(ptr2),y
	beq	dis3
	lda	#5		; next op-code
	clc
	adc	ptr2
	sta	ptr2
	bcc	dis4
	inc	ptr2+1
dis4:	dex
	bne	dis2
	; not found: ILLegal instruction. Print ???
	ldx	#(msgILL-msgs)
	jsr	uputs
	jmp	disfin
		
dis3:	ldy	#1		; OP-code found
	lda	(ptr2),y	; check addressing mode
	sta	tmp3
	tax
	lda	addrmodelen,x	; number of additional bytes
	beq	dis7		; if 0 don't print
	sta	tmp4
dis5:	ldy	#0		; print the additional bytes
	lda	(ptr1),y
	jsr	prthexuart
	lda	#' '
	jsr	uart_putch
	inc	ptr1
	bne	dis6
	inc	ptr1+1
dis6:	dec	tmp4
	bne	dis5
	
dis7:	lda	#9		; print a TAB character
	jsr	uart_putch
	ldy	#2
	lda	(ptr2),y	; print Mnemonic
	jsr	uart_putch
	ldy	#3
	lda	(ptr2),y
	jsr	uart_putch
	ldy	#4
	lda	(ptr2),y
	jsr	uart_putch
	lda	#' '
	jsr	uart_putch
	
	lda	tmp3		; acc = 3*tmp3 (tmp3 = addressing mode)
	asl	a
	clc
	adc	tmp3
	adc	#<jptbl1	; add jmp table offset
	sta	ptr2
	lda	#>jptbl1
	adc	#0
	sta	ptr2+1
	jmp	(ptr2)		; multiple branch
jptbl1:	jmp	admIMP
	jmp	admACC
	jmp	admIMM
	jmp	admZP
	jmp	admZPX
	jmp	admZPY
	jmp	admINDX
	jmp	admINDY
	jmp	admREL
	jmp	admABS
	jmp	admABSX
	jmp	admABSY
	jmp	admIND
	
admACC:	lda	#'A'		; ACC
	jsr	uart_putch
admIMP:	jmp	disfin
admIMM:	lda	#'#'		; IMM
	jsr	uart_putch
admZP:	jsr	op1b		; Zero Page
	jmp	disfin	
admZPX: jsr	op1b		; Zero Page , X
	ldx	#(msgcx-msgs)
	jsr	uputs	
	jmp	disfin
admZPY:	jsr	op1b		; Zero Page , Y
	ldx	#(msgcy-msgs)
	jsr	uputs	
	jmp	disfin
admINDX:lda	#'('		; (Indirect, X)
	jsr	uart_putch
	jsr	op1b
	ldx	#(msgcx-msgs)
	jsr	uputs
	lda	#')'
	jsr	uart_putch
	jmp	disfin
admINDY:lda	#'('		; (Indirect),Y
	jsr	uart_putch
	jsr	op1b
	ldx	#(msgpcy-msgs)
	jsr	uputs
	jmp	disfin
	
admREL:	lda	ptr1		; Relative branch
	bne	dis10
	dec	ptr1+1
dis10:	dec	ptr1
	ldy	#0
	lda	(ptr1),y
	sta	tmp3
	bpl	dis12		; sign extension of displacement to 16 bit
	lda	#$ff
	sta	tmp4
	jmp	dis13
dis12:	lda	#0
	sta	tmp4	
dis13:	inc	ptr1
	bne	dis11
	inc	ptr1+1
dis11:	lda	ptr1
	clc
	adc	tmp3
	sta	tmp3
	lda	ptr1+1		; print the computed jump address
	adc	tmp4	
	jsr	prthexuart
	lda	tmp3
	jsr	prthexuart
	jmp	disfin

admABS: jsr	op2b		; Absolute
	jmp	disfin
admABSX:jsr	op2b		; Absolute, X
	ldx	#(msgcx-msgs)
	jsr	uputs
	jmp	disfin
admABSY:jsr	op2b		; Absolute, Y
	ldx	#(msgcy-msgs)
	jsr	uputs
	jmp	disfin
admIND: lda	#'('		; (Indirect) (jump)
	jsr	uart_putch
	jsr	op2b
	lda	#')'
	jsr	uart_putch

disfin:	rts

;---------------------------------------------
;	print 1-byte addressing mode parameter

op1b:	lda	ptr1		; decrement pointer
	bne	op1b1
	dec	ptr1+1
op1b1:	dec	ptr1
	ldy	#0
	lda	(ptr1),y	; print hex. data
	jsr	prthexuart	
	inc	ptr1		; increment pointer
	bne	op1b2
	inc	ptr1+1
op1b2:	rts

;---------------------------------------------
;	print 2-byte addressing mode parameter

op2b:	lda	ptr1		; decrement pointer
	bne	op2b1
	dec	ptr1+1
op2b1:	dec	ptr1
	ldy	#0
	lda	(ptr1),y	; print MSB byte
	jsr	prthexuart
	
	lda	ptr1		; decrement pointer
	bne	op2b2
	dec	ptr1+1
op2b2:	dec	ptr1
	ldy	#0
	lda	(ptr1),y	; print LSB byte
	jsr	prthexuart
	
	inc	ptr1		; increment pointer
	bne	op2b3
	inc	ptr1+1
op2b3:	inc	ptr1		; increment pointer
	bne	op2b4
	inc	ptr1+1
op2b4:	rts

; ------------------------------------------------------------------------
; ------------------------------------------------------------------------
; 				DATA & BSS
; ------------------------------------------------------------------------
; ------------------------------------------------------------------------

.rodata
	.export	msgs
msgs:
msg1:		.asciiz "BENDERs BRAIN"
msgload:	.asciiz "Loaded $"
msgI2C:		.asciiz "I2C"
msgSD:		.asciiz "SD"
msgldx:		.asciiz ".load"
msgexe:		.asciiz ".exe "
msgnack:	.asciiz " NACK"
msgdone:	.asciiz "-done-"
msgboot:	.asciiz "Booting"
msgcls:		.asciiz "                "
msgBRK:		.asciiz "   *** BRK *** "
msgPC:		.asciiz "PC="
msgP:		.asciiz "  P="
msgflags:	.asciiz "NVrBDIZC"
msgA:		.asciiz "  A="
msgX:		.asciiz "  X="
msgY:		.asciiz "  Y="
msgS:		.asciiz "  S="
msgspm:		.asciiz "- "
msgZP:		.byte 27,"[48GZero Page Vars",0
msgstack:	.byte "Stack: ",0
msgmonprom:	.byte 10, "hcsnrbmdgtpaxye >",27,"[J",0
msgnlcls:	.byte 10,27,"[2K",0
msghome:	.byte 27,"[H",27,"[K",0
msgtab28:	.byte 27,"[29G",0
msgback4:	.byte	8,8,8,8,0
msgILL:		.byte 9, "???", 0
msgcx:		.asciiz ",X"
msgpcy:		.byte ")"
msgcy:		.asciiz ",Y"
msgpause:	.byte 10,"<MONpause>",0

monhelp:	.byte 10,9,"h",9,"Help",10
		.byte 9,"c",9,"Continue",10
		.byte 9,"s",9,"Single step",10
		.byte 9,"n",9,"Next instr",10
		.byte 9,"r",9,"ends Routine",10
		.byte 9,"b <adr>",9,"Break at",10
		.byte 9,"m <adr>",9,"dump Mem",10
		.byte 9,"d <adr>",9,"Dissasemble",10
		.byte 9,"g <adr>",9,"Goto at",10
		.byte 9,"t <adr>",9,"Trace at",10
		.byte 9,"p nn",9,"edit P",10
		.byte 9,"a nn",9,"edit A",10
		.byte 9,"x nn",9,"edit X",10
		.byte 9,"y nn",9,"edit Y",10
		.byte 9,"e <adr>",9,"Edit mem",10
		.byte 9,"spc",9,"redraw",10
		.byte 9,"esc",9,"abort",0

	.export	bootfilename
bootfilename:	.byte "BOOT    BIN"

; ------------------------------------------------------------------------
; ------------------------------------------------------------------------

; addressing mode types:

	IMP 	= 0
	ACC	= 1
	
	IMM	= 2
	ZP	= 3
	ZPX	= 4
	ZPY	= 5
	INDX	= 6
	INDY	= 7
	REL	= 8
		
	ABS	= 9
	ABSX	= 10
	ABSY	= 11
	IND	= 12
	
	.export	addrmodelen

; number of following bytes after OP-code:

addrmodelen:
	.byte	0,0,1,1,1,1,1,1,1,2,2,2,2


; OP-code table:

	.export	optbl
optbl:	.byte $00,	IMP,	"BRK"	;OPs 0X
	.byte $01,	INDX,	"ORA"
	.byte $05,	ZP,	"ORA"
	.byte $06,	ZP,	"ASL"
	.byte $08,	IMP,	"PHP"
	.byte $09,	IMM,	"ORA"
	.byte $0A,	ACC,	"ASL"
	.byte $0D,	ABS,	"ORA"
	.byte $0E,	ABS,	"ASL"
	
	.byte $10,	REL,	"BPL"	;OPs 1X
	.byte $11,	IND,	"ORA"
	.byte $15,	ZPX,	"ORA"
	.byte $16,	ZPX,	"ASL"
	.byte $18,	IMP,	"CLC"
	.byte $19,	ABSY,	"ORA"
	.byte $1D,	ABSX,	"ORA"
	.byte $1E,	ABSX,	"ASL"
	
	.byte $20,	ABS,	"JSR"	;OPs 2X
	.byte $21,	INDX,	"AND"
	.byte $24,	ZP,	"BIT"
	.byte $25,	ZP,	"AND"
	.byte $26,	ZP,	"ROL"
	.byte $28,	IMP,	"PLP"
	.byte $29,	IMM,	"AND"
	.byte $2A,	ACC,	"ROL"
	.byte $2C,	ABS,	"BIT"
	.byte $2D,	ABS,	"AND"
	.byte $2E,	ABS,	"ROL"
	
	.byte $30,	REL,	"BMI"	;OPs 3X
	.byte $31,	INDY,	"AND"
	.byte $35,	ZPX,	"AND"
	.byte $36,	ZPX,	"ROL"
	.byte $38,	IMP,	"SEC"
	.byte $39,	ABSY,	"AND"
	.byte $3D,	ABSX,	"AND"
	.byte $3E,	ABSX,	"ROL"
	
	.byte $40,	IMP,	"RTI"	;OPs 4X
	.byte $41,	INDX,	"EOR"
	.byte $45,	ZP,	"EOR"
	.byte $46,	ZP,	"LSR"
	.byte $48,	IMP,	"PHA"
	.byte $49,	IMM,	"EOR"
	.byte $4A,	ACC,	"LSR"
	.byte $4C,	ABS,	"JMP"
	.byte $4D,	ABS,	"EOR"
	.byte $4E,	ABS,	"LSR"
	
	.byte $50,	REL,	"BVC"	;OPs 5X
	.byte $51,	INDY,	"EOR"
	.byte $55,	ZPX,	"EOR"
	.byte $56,	ZPX,	"LSR"
	.byte $58,	IMP,	"CLI"
	.byte $59,	ABSY,	"EOR"
	.byte $5D,	ABSX,	"EOR"
	.byte $5E,	ABSX,	"LSR"
	
	.byte $60,	IMP,	"RTS"	;OPs 6X
	.byte $61,	INDX,	"ADC"
	.byte $65,	ZP,	"ADC"
	.byte $66,	ZP,	"ROR"
	.byte $68,	IMP,	"PLA"
	.byte $69,	IMM,	"ADC"
	.byte $6A,	ACC,	"ROR"
	.byte $6C,	IND,	"JMP"
	.byte $6D,	ABS,	"ADC"
	.byte $6E,	ABS,	"ROR"
	
	.byte $70,	REL,	"BVS"	;OPs 7X
	.byte $71,	INDY,	"ADC"
	.byte $75,	ZPX,	"ADC"
	.byte $76,	ZPX,	"ROR"
	.byte $78,	IMP,	"SEI"
	.byte $79,	ABSY,	"ADC"
	.byte $7D,	ABSX,	"ADC"
	.byte $7E,	ABSX,	"ROR"
	
	.byte $81,	INDX,	"STA"	;OPs 8X
	.byte $84,	ZP,	"STY"
	.byte $85,	ZP,	"STA"
	.byte $86,	ZP,	"STX"
	.byte $88,	IMP,	"DEY"
	.byte $8A,	IMP,	"TXA"
	.byte $8C,	ABS,	"STY"
	.byte $8D,	ABS,	"STA"
	.byte $8E,	ABS,	"STX"
	
	.byte $90,	REL,	"BCC"	;OPs 9X
	.byte $91,	INDY,	"STA"
	.byte $94,	ZPX,	"STY"
	.byte $95,	ZPX,	"STA"
	.byte $96,	ZPX,	"STX"
	.byte $98,	IMP,	"TYA"
	.byte $99,	ABSY,	"STA"
	.byte $9A,	IMP,	"TXS"
	.byte $9D,	ABSX,	"STA"
	
	.byte $A0,	IMM,	"LDY"	;OPs AX
	.byte $A1,	INDX,	"LDA"
	.byte $A2,	IMM,	"LDX"
	.byte $A4,	ZP,	"LDY"
	.byte $A5,	ZP,	"LDA"
	.byte $A6,	ZP,	"LDX"
	.byte $A8,	IMP,	"TAY"
	.byte $A9,	IMM,	"LDA"
	.byte $AA,	IMP,	"TAX"
	.byte $AC,	ABS,	"LDY"
	.byte $AD,	ABS,	"LDA"
	.byte $AE,	ABSY,	"LDX"
	
	.byte $B0,	REL,	"BCS"	;OPs BX
	.byte $B1,	INDY,	"LDA"
	.byte $B4,	ZPX,	"LDY"
	.byte $B5,	ZPX,	"LDA"
	.byte $B6,	ZPX,	"LDX"
	.byte $B8,	IMP,	"CLV"
	.byte $B9,	ABSY,	"LDA"
	.byte $BA,	IMP,	"TSX"
	.byte $BC,	ABSX,	"LDY"
	.byte $BD,	ABSX,	"LDA"
	.byte $BE,	ABS,	"LDX"
	
	.byte $C0,	IMM,	"CPY"	;OPs CX
	.byte $C1,	INDX,	"CMP"
	.byte $C4,	ZP,	"CPY"
	.byte $C5,	ZP,	"CMP"
	.byte $C6,	ZP,	"DEC"
	.byte $C8,	IMP,	"INY"
	.byte $C9,	IMM,	"CMP"
	.byte $CA,	IMP,	"DEX"
	.byte $CC,	ABS,	"CPY"
	.byte $CD,	ABS,	"CMP"
	.byte $CE,	ABS,	"DEC"
	
	.byte $D0,	REL,	"BNE"	;OPs DX
	.byte $D1,	INDY,	"CMP"
	.byte $D5,	ZPX,	"CMP"
	.byte $D6,	ZPX,	"DEC"
	.byte $D8,	IMP,	"CLD"
	.byte $D9,	ABSY,	"CMP"
	.byte $DD,	ABSX,	"CMP"
	.byte $DE,	ABSX,	"DEC"
	
	.byte $E0,	IMM,	"CPX"	;OPs EX
	.byte $E1,	INDX,	"SBC"
	.byte $E4,	ZP,	"CPX"
	.byte $E5,	ZP,	"SBC"
	.byte $E6,	ZP,	"INC"
	.byte $E8,	IMP,	"INX"
	.byte $E9,	IMM,	"SBC"
	.byte $EA,	IMP,	"NOP"
	.byte $EC,	ABS,	"CPX"
	.byte $ED,	ABS,	"SBC"
	.byte $EE,	ABS,	"INC"
	
	.byte $F0,	REL,	"BEQ"	;OPs FX
	.byte $F1,	INDY,	"SBC"
	.byte $F5,	ZPX,	"SBC"
	.byte $F6,	ZPX,	"INC"
	.byte $F8,	IMP,	"SED"
	.byte $F9,	ABSY,	"SBC"
	.byte $FD,	ABSX,	"SBC"
	.byte $FE,	ABSX,	"INC"
	
optblf:

.data
	.export	fatbuf
	fatbuf:	.res	512	; buffer for FAT operatios
.bss




; ------------------------------------------------------------------------
; ------------------------------------------------------------------------
; 				Vectors
; ------------------------------------------------------------------------
; ------------------------------------------------------------------------

	.segment	"VECTORS"
	.export	vectorhNMI, vectorhRES, vectorhIRQBRK
vectorhNMI:	
	.byte <_nmi	;NMI
	.byte >_nmi
vectorhRES:
	.byte <_start	;RESET 
	.byte >_start
vectorhIRQBRK:
	.byte <_irqbrk	;IRQ/BRK
	.byte >_irqbrk
