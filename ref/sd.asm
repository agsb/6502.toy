enable_softSD_debug = 0
enable_softSD_report = 0

timeout = 1000000

	.section vars
crc7			.space 1
crc16			.space 1
state			.space 1
state_expectingData_bit = 0
state_readingData_bit = 1
bufferIndex		.space 1
bitCount		.space 1
bufferPtr		.space 2
writeData		.space 1	; data to be shifted out during writes
.global softSD_card_replaced	.space 1
	.section code

; Power-up flowchart on page 40.
; CMD0 "GO_IDLE_STATE"
; CMD8 "SEND_IF_COND"
; ACMD41 "SD_SEND_OP_COND"
;   send arg = 40100000 (HCS=1, voltage = 3.3)
;   returns R3 containing HCS and supported voltages
; CMD2 "ALL_SEND_CID"
;	send arg = 00000000
;   returns R2 containing CID
; CMD3 "SEND_RELATIVE_ADDR"
;   send arg = 00000000
;   returns R6
; Now in stand-by state.

; Send CMD7 to change to transfer state
; Then CMD17 to read a block, or CMD24 to write a block
; After CMD24, it will eventually return to transfer state.
; Will need to query state (anything that returns R1) until the write has finished

; 4.7.4, page 87.  "Detailed Command Description"
; CMD7 "SELECT/DESELECT_CARD"
;	send arg bits 31-16 = RCA, 15-0 = 0
;	returns R1b
; CMD12 "STOP_TRANSMISSION"
;	send arg = 00000000
;	returns R1b
; CMD16 "SET_BLOCKLEN"
;   send arg = 512
;   returns R1
;   shouldn't be needed, but won't hurt
; CMD17 "READ_SINGLE_BLOCK"
;   send arg = block addr (since HCS=1, meaning high capacity)
;   returns R1, block on DAT0 line
; CMD24 "WRITE_BLOCK"
;   send arg = block addr, block on DAT0 line
; CMD12 "STOP_TRANSMISSION"
;   send arg = 00000000
;   returns R1b
;   needed after each block read or write?
; ACMD41 "SD_SEND_OP_COND" page 103
;	send arg = 40100000.  HCS=1 (we allow high capacity cards), Voltage = 3.3V
;	returns R3

; Responses
; R1: card status (32) 4.10.1 page 114
;	31: OUT_OF_RANGE
;	30: ADDRESS_ERROR
;	29: BLOCK_LEN_ERROR
;	28: ERASE_SEQ_ERROR
;	27: ERASE_PARAM
;	26: WP_VIOLATION
;	25: CARD_IS_LOCKED
;	24: LOCK_UNLOCK_FAILED
;	23: COM_CRC_ERROR
;	22: ILLEGAL_COMMAND
;	21: CARD_ECC_FAILED
;	20: CC_ERROR
;	19: ERROR
;	18: reserved
;	17: reserved
;	16: CSD_OVERWRITE
;	15: WP_ERASE_SKIP
;	14: CARD_ECC_DISABLED
;	13: ERASE_RESET
;	12-9: CURRENT_STATE
;		0000: idle
;		0001: ready
;		0010: ident
;		0011: stby
;		0100: tran
;		0101: data
;		0110: rcv
;		0111: prg
;		1000: dis
;	8: READY_FOR_DATA
;	7: reserved
;	6: FX_EVENT
;	5: APP_CMD
;	4: reserved
;	3: AKE_SEQ_ERROR
;	2: reserved
;	1: reserved
;	0: reserved

; R2 CID register (CMD2 or CMD10)
;	127-120: Manufacturer ID
;	119-104: OEM/Application ID
;	103-64: Product name
;	63-56: Product revision
;	55-24: Product serial number		
;	23-20: reserved
;	19-8: Manufacturing date
;	7-1: checksum.  Only covers CID bits 127-8.
;	0: stop = 1
; R2 CSD register (CMD9)

; R6 Published RCA response
;	31-16: New published RCA
;	15: card status bit 23 COM_CRC_ERROR
;	14: card status bit 22 ILLEGAL_COMMAND
;	13: card status bit 19 ERROR
;	12-9: card status bits 12-9 CURRENT_STATE
;		0000: idle
;		0001: ready
;		0010: ident
;		0011: stby
;		0100: tran
;		0101: data
;		0110: rcv
;		0111: prg
;		1000: dis
;	8: card status bit 8 READY_FOR_DATA
;	7: card status bit 7 reserved
;	6: card status bit 6 FX_EVENT
;	5: card status bit 5 APP_CMD
;	4: card status bit 4 reserved
;	3: card status bit 3 AKE_SEQ_ERROR
;	2: card status bit 2 reserved
;	1: card status bit 1 reserved
;	0: card status bit 0 reserved

	; CMD24 "WRITE_BLOCK"
; A0: buffer address
; A1: block number
.global softSD_write_block
{
	psh.l a0
	psh.l a1
	psh.l a2
	psh.l x0
	psh.l x1
	mov.l x1, a0
	ldr a0, #24
	bra.l sendCommand
	ldr x0, #4
	bra.l readResponse	; no response?  timeout?
	.if enable_softSD_debug
	;	ldr.l a0, #R1Fields
	;	bra.l decodeResponse
		; !!! command 24 should transition into rcv state
		; but it is instead staying in tran.
	.endif
	bcs error
	tbt.l a1, #19		; response was an error?
	bne error
	; two clocks before the start bit
	sti SoftSDDevice, #%01111
	sti SoftSDDevice, #%11111
	sti SoftSDDevice, #%01111
	sti SoftSDDevice, #%11111
	; send start bit
	sti SoftSDDevice, #%00011
	sti SoftSDDevice, #%10011
	ldr.w x0, #256
	; send data
	ldr a0, #0
	str.w a0, crc16
dataLoop
	ldr.w a0, 0, x1
	bra.l sendByte_DAT
	inc.l x1
	dec.w x0
	bne dataLoop
	; send CRC
	ldr.w a0, crc16
	rlb.w a0, #8
	.if enable_softSD_debug
		bra.l print_inline_string
		.byte 13, "write crc = ", char_reg_hex4+char_reg_a0, 13, 0
	.endif
	bra.l sendByte_DAT
wait_for_write_complete
	; send stop bit
	sti SoftSDDevice, #%01011
	sti SoftSDDevice, #%11011
	; card will hold DAT0 low until writing has finished (4.3.4, page 51)
	sti SoftSDDevice, #%01111
	sti SoftSDDevice, #%11111
	tbt softSDDevice, #SoftSD_DAT_bit
	beq wait_for_write_complete
	.if enable_softSD_debug
		bra.l print_inline_string
		.byte 13, 0
	.endif
	clc

	ldr.w x0, #5000
extraloop
	sti SoftSDDevice, #%01111
	sti SoftSDDevice, #%11111
	dec.w x0
	bne extraloop

	bra exit
error
	.if enable_softSD_debug
		bra.l print_inline_string
		.byte 13, "error ", 0
	.endif
exit
	pul.l x1
	pul.l x0
	pul.l a2
	pul.l a1
	pul.l a0
	rts
}

.if enable_softSD_debug
; A0: field table
; A1: response
decodeResponse
{
	php
	psh.l a0
	psh.l a2
	psh.l x0
	mov.l x0, a0
	bra.l print_inline_string
	.byte 13, 0
loop
	mov.l a0, a1
	ldr a2, 0, x0
	cmp a2, #255
	beq exit
	lsr.l a0, a2
	and.l a0, 1, x0
	inc.l x0, #2
	bra.l print_inline_string
	.byte char_reg_hex2+char_reg_a0, ": ",
	.byte char_reg_string+char_reg_x0, 13, 0
nameskip
	inc.l x0
	ldr a0, 0, x0
	bne nameskip
	inc.l x0
	bra loop
exit
	pul.l x0
	pul.l a2
	pul.l a0
	plp
	rts
}
R1Fields
	; shift amount, mask, name
	.byte 31, 1, "OUT OF RANGE", 0
	.byte 30, 1, "ADDRESS ERROR", 0
	.byte 29, 1, "BLOCK LEN ERROR", 0
	.byte 28, 1, "ERASE SEQ ERROR", 0
	.byte 27, 1, "ERASE PARAM", 0
	.byte 26, 1, "WP VIOLATION", 0
	.byte 25, 1, "CARD IS LOCKED", 0
	.byte 24, 1, "LOCK UNLOCK FAILED", 0
	.byte 23, 1, "COM CRC ERROR", 0
	.byte 22, 1, "ILLEGAL COMMAND", 0
	.byte 21, 1, "CARD ECC FAILED", 0
	.byte 20, 1, "CC ERROR", 0
	.byte 19, 1, "ERROR", 0
	.byte 16, 1, "CSD OVERWRITE", 0
	.byte 15, 1, "WP ERASE SKIP", 0
	.byte 14, 1, "CARD ECC DISABLED", 0
	.byte 13, 1, "ERASE RESET", 0
	.byte 9, 15, "CURRENT STATE", 0
	.byte 8, 1, "READY FOR DATA", 0
	.byte 6, 1, "FX EVENT", 0
	.byte 5, 1, "APP CMD", 0
	.byte 3, 1, "AKE SEQ ERROR", 0
	.byte 255
.endif

; X0 contains the number of argument bytes expected
; Returns:
;   A0: response code
;   A1: argument (low 32 bits)
;   A2: CRC
readResponse
{
	psh.l x0
	bra.l softSD_readCMD_start
	bcs exit
	psh a0
	.if enable_softSD_debug
		ldr a0, #':
		bra.l chrout
	.endif
	; read x0 bytes of response
loop
	bra.l softSD_readCMD
	bcs exit
	asl.l a1, #8
	and a0, #$ff
	ora.l a1, a0
	dec x0
	bne loop
	; read 1 byte of CRC and stop bit
	mov.l a0, a1
	.if enable_softSD_debug
		ldr a0, #';
		bra.l chrout
	.endif
	bra.l softSD_readCMD
	mov a2, a0
	.if enable_softSD_debug
		ldr a0, #13
		bra.l chrout
	.endif
	pul a0
	clc
exit
	pul.l x0
	rts
}

; add A1 bit 0 to crc7
softSD_update_crc7
	psh.l a0
	asl crc7
	ldr a0, crc7
	lsr a0, #7
	eor a0, a1
	and a0, #1
	beq softSD_update_crc7_skip
	ldr a0, crc7
	eor a0, #$89
	str a0, crc7
softSD_update_crc7_skip
	pul.l a0
	rts

; add A1 bit 0 to crc16
softSD_update_crc16
	psh.l a0
	ldr.w a0, crc16
	rlb.w a0, #1
	eor.w a0, a1
	clc
	and a0, #1
	beq softSD_update_crc16_skip
	ldr.w a0, crc16
	eor.w a0, #$8810
	str.w a0, crc16
	sec
softSD_update_crc16_skip
	rol.w crc16
	pul.l a0
	rts

sendByte_CMD
{
	psh.l a0
	.if enable_softSD_debug
		bra.l print_hex2
	.endif
	psh.l a1
	psh.l x0
	ldr x0, #8
loop
	asl a0
	rol a1
	and a1, #1
	bra.l softSD_update_crc7
	bra.l softSD_update_crc16
	ora a1, #1<<SoftSD_DAT_bit | 1<<SoftSD_DAT_dir_bit
	str a1, SoftSDDevice
	ora a1, #1<<SoftSD_DAT_bit | 1<<SoftSD_DAT_dir_bit | 1<<SoftSD_CLK_bit
	str a1, SoftSDDevice
	dec x0
	bne loop
	pul.l x0
	pul.l a1
	pul.l a0
	rts
}

; (actually a 16 bit byte, sent low 8 bits first)
sendByte_DAT
{
	psh.l a0
	.if enable_softSD_debug
		bra.l print_hex4
	.endif
	rlb.w a0,#8
	psh.l a1
	psh.l x0
	ldr x0, #16
loop
	asl.w a0
	rol.w a1
	and.w a1, #1
	bra.l softSD_update_crc7
	bra.l softSD_update_crc16
	asl a1, #SoftSD_DAT_bit
	ora a1, #1<<SoftSD_CMD_bit | 1<<SoftSD_CMD_dir_bit
	str a1, SoftSDDevice
	ora a1, #1<<SoftSD_CMD_bit | 1<<SoftSD_CMD_dir_bit | 1<<SoftSD_CLK_bit
	str a1, SoftSDDevice
	dec x0
	bne loop
	pul.l x0
	pul.l a1
	pul.l a0
	rts
}

softSD_readCMD_start
	psh.l a1
	psh.l a2
	psh.l a3
	psh.l x0
	ldr.l a2, TimerDevice_Time
softSD_readCMD_start_loop
	ldr.l a3, TimerDevice_Time
	sub.l a3, a2
	cmp.l a3, #timeout
	bge softSD_readCMD_timeout
	sti SoftSDDevice, #%01111
	bra.l softSD_checkDat0
	sti SoftSDDevice, #%11111
	ldr a1, SoftSDDevice
	and a1, #1
	bne softSD_readCMD_start_loop
	ldr x0, #7
	ldr a0, #0
	bra softSD_readCMD_loop
softSD_readCMD
	psh.l a1
	psh.l a2
	psh.l a3
	psh.l x0
	ldr x0, #8
softSD_readCMD_loop
	sti SoftSDDevice, #%01111
	bra.l softSD_checkDat0
	sti SoftSDDevice, #%11111
	ldr a1, SoftSDDevice
	lsr a1
	rol a0
	dec x0
	bne softSD_readCMD_loop
	.if enable_softSD_debug
		bra.l print_hex2
	.endif
	clc
softSD_readCMD_exit
	pul.l x0
	pul.l a3
	pul.l a2
	pul.l a1
	rts
softSD_readCMD_timeout
	sec
	bra softSD_readCMD_exit

; send a clock edge (low to high)
; repeat X0 times
idle
{
	psh.l a0
loop
	sti SoftSDDevice, #%01111
	sti SoftSDDevice, #%11111
	bra.l softSD_checkDat0
	dec x0
	bne loop
	pul.l a0
	rts
}

	; Command in a0.b
	; Argument in a1.l
sendCommand
	psh.l a0
	psh.l a1
	psh.l x0
	ldr x0, #0
	str x0, crc7
	ora a0, #$40
	bra.l sendByte_CMD
	.if enable_softSD_debug
		ldr a0, #':
		bra.l chrout
	.endif
	ldr x0, #4
sendCommand_loop
	rlb.l a1, #8
	mov a0, a1
	bra.l sendByte_CMD
	dec x0
	bne sendCommand_loop
	.if enable_softSD_debug
		ldr a0, #';
		bra.l chrout
	.endif
	ldr a0, crc7
	sec
	adc a0, a0
	bra.l sendByte_CMD
	pul.l x0
	pul.l a1
	pul.l a0
	.if enable_softSD_debug
		bra.l print_inline_string
		.byte " -> ", 0
	.endif
	rts

; ExpectingData:
; * wait for DAT to become 1, then switch to ReadingData
; ReadingData:
; * read next bit, store it in temp, add it to CRC
; * if 16 bits have been read, shuffle temp and write to buffer
; * if 257 words have been read, end ReadingData
softSD_checkDat0
	psh.l a0
	psh.l x0
	tbt state, #state_expectingData_bit
	beq softSD_checkDat0_return
	tbt state, #state_readingData_bit
	bne softSD_checkDat0_reading
; ExpectingData state.  If data isn't ready, return
	tbt SoftSDDevice, #softSD_DAT_bit
	bne softSD_checkDat0_return
; ExpectingData state.  Data is ready, so reset CRC, buffer index, bit count
	sbt state, #state_readingData_bit
	ldr a0, #0
	str.w a0, crc16
	str.w a0, bufferIndex
	ldr a0, #16
	str.w a0, bitCount
	bra softSD_checkDat0_return
softSD_checkDat0_reading
; ReadingData state.
	; get the next bit
	ldr a0, SoftSDDevice
	lsr a0, #2
	and a0, #1
	mov a1, a0
	bra.l softSD_update_crc16
	; shift it into the buffer
	ldr.w x0, bufferIndex
	add.l x0, bufferPtr
	lsr a0, #1
	rol.w 0, x0
	; finished this byte?
	dec bitCount
	bne softSD_checkDat0_return
	; yes: shuffle it
	ldr.w a0, 0, x0
	rlb.w a0, #8
	.if enable_softSD_debug
		bra.l print_hex4
	.endif
	str.w a0, 0, x0
	; then move to the next
	inc.w bufferIndex
	ldr a0, #16
	str a0, bitCount
	; finished the whole buffer? (plus CRC)
	ldr.w x0, bufferIndex
	cmp.w x0, #257
	bne softSD_checkDat0_return
	; yes: stop reading
	cbt state, #state_readingData_bit
	cbt state, #state_expectingData_bit
softSD_checkDat0_return
	pul.l x0
	pul.l a0
	rts

; Why is CMD12 not getting a response?
; Because READ_SINGLE_BLOCK doesn't need (and shouldn't have) CMD12.

; There's more happening with the DAT0 line.  It is sometimes
; used as a "busy" signal (anything that returns R1b)
; CMD7 "SELECT/DESELECT_CARD"
; CMD12 "STOP_TRANSMISSION"
; CMD20 "SPEED_CLASS_CONTROL"
; CMD28 "SET_WRITE_PROT"
; CMD29 "CLR_WRITE_PROT"
; CMD38 "ERASE"
; CMD43 "Q_MANAGEMENT"
; Timing details in 4.12.3, which this document doesn't include

; ACMD41 must be followed by continuous clocks in the range 100KHz - 400KHz
; (4.4, page 87)
; ACMD41 polling must be at less than 50ms intervals (between the end of one
; command and the start of the next)

; There must be 8 clocks after
; * a command with no response
; * the response of a command with a response
; * the end of a read data block
; * the CRC status token on a write

.global softSD_init:
	sbt softSD_card_replaced, #0
	rts

.global softSD_startup:
	psh.l a0
	psh.l a1
	psh.l a2
	psh.l x0
	psh.l y0

	ldr a0, #0
	str a0, state
	str.w a0, bufferIndex
	str.w a0, bitCount
	ldr x0, #100
	bra.l idle

	; CMD0 "GO_IDLE_STATE"
	ldr a0, #0
	ldr.l a1, #$00000000
	bra.l sendCommand

	; no response expected
	.if enable_softSD_debug
		ldr a0, #13
		bra.l chrout
	.endif
	ldr x0, #2
	bra.l idle

	; CMD8 "SEND_IF_COND" 4.3.13 page 85
	ldr a0, #8
	; bit 13: PCIe 1.2V support
	; bit 12: PCIe availability
	; bits 11-8: supply voltage (VHS).  1 = 2.7-3.6V
	; bits 7-0: pattern
	ldr.l a1, #$000001aa
	bra.l sendCommand
	ldr x0, #4
	bra.l readResponse
	bcs softSD_startup_exit
	; returns
	; %000000000000000000_A_R_VVVV_PPPPPPPPPPPP
	; A: PCIe1.2V support
	; B: PCIe response
	; VVVV: 0001 for 2.7-3.6V
	; PPPPPPPP: pattern from the command

	ldr y0, #0
softSD_wakeup_loop
	; CMD55 "APP_CMD" 4.3.9.1 page 72
	ldr a0, #55
	ldr.l a1, #$00000000
	bra.l sendCommand
	ldr x0, #4
	bra.l readResponse
	bcs softSD_startup_exit

	ldr a0, #41
	ldr.l a1, #$40100000
	bra.l sendCommand
	ldr x0, #4
	bra.l readResponse
	bcs softSD_startup_exit

	; repeat CMD55, ACMD41 until bit 31 of the response argument is 1
	and.l a1, #$80000000
	beq softSD_wakeup_loop

	; CMD2 "ALL_SEND_CID"
	ldr a0, #2
	ldr.l a1, #$00000000
	bra.l sendCommand
	ldr x0, #15
	bra.l readResponse
	bcs softSD_startup_exit

	; CMD3 "SEND_RELATIVE_ADDR"
	ldr a0, #3
	ldr.l a1, #$00010000
	bra.l sendCommand
	ldr x0, #4
	bra.l readResponse
	bcs softSD_startup_exit

	; CMD7 "SELECT_CARD" 4.7.4 page 97
	ldr a0, #7
	and.l a1, #$ffff0000	; response from CMD3 "SEND_RELATIVE_ADDR"
	bra.l sendCommand
	ldr x0, #4
	bra.l readResponse
	bcs softSD_startup_exit

softsd_startup_exit
	pul.l y0
	pul.l x0
	pul.l a2
	pul.l a1
	pul.l a0
	rts

; A0: pointer to buffer
; A1: block number
.global softSD_read_block
{
	psh.l a0
	psh.l a1
	psh.l a2
	psh.l x0
	str.l a0, bufferPtr
	.if enable_softSD_report
		bra.l print_inline_string
		.byte "read block ", 0
		mov.l a0, a1
		bra.l print_hex8
		bra.l print_return
	.endif
read_block_loop
	; CMD17 "READ_BLOCK" 4.7.4 page 97
	ldr a0, #17
	bra.l sendCommand
	sbt state, #state_expectingData_bit
	ldr x0, #4
	bra.l readResponse
	bcs exit

wait_for_read
	bra.l idle
	tbt state, #state_readingData_bit
	beq wait_for_read

	ldr a0, #0
	str.w a0, crc16
wait_for_read_finished
	bra.l idle
	tbt state, #state_readingData_bit
	bne wait_for_read_finished

	ldr x0, #8
	bra.l idle

	.if enable_softSD_debug
		ldr.w a0, crc16
		ldr.l a1, bufferPtr
		ldr.w a1, 256, a1
		bra.l print_inline_string
		.byte 13, "read crc = ", char_reg_hex4+char_reg_a0
		.byte " buffer = ", char_reg_hex4+char_reg_a1, 13, 0
	.endif

	clc
exit
	pul.l x0
	pul.l a2
	pul.l a1
	pul.l a0
	.if enable_softSD_report
		bra.l print_inline_string
		.byte " OK", 13, 0
	.endif
	rts
}

.global handleSDInterrupt
	sbt softSD_card_replaced, #0
	rts
