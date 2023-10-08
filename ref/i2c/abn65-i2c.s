;
; https://github.com/agsb
; adapted from: https://raw.githubusercontent.com/AndersBNielsen/abn6502/main/abn6502rom.s
;

i2c_start: ; Let's assume i2c addr is in A and RW bit is in C
  rol ; Move address to top bits and RW bit to bit0
  sta outb ; Save addr + rw bit

  lda #SCL_INV ; Start with SCL as INPUT HIGH
  and DDRB
  sta DDRB

  lda #SDA ; Insure SDA is output low before SCL is LOW
  ora DDRB
  sta DDRB

  lda #SDA_INV
  and PORTB
  sta PORTB

  lda #SCL_INV
  and PORTB
  sta PORTB

  lda DDRB
  ora #SCL
  sta DDRB 

  ; Set SCL LOW by setting it to OUTPUT
  ; Fall through to send address + RW bit

  ; From here on we can assume OUTPUTs are LOW and INPUTS are HIGH.
  ; Maybe some of the juggling above is not necessary but let's not assume for now
i2cbyteout:

  lda #SDA_INV ; In case this is a data byte we set SDA LOW
  and PORTB
  sta PORTB

  ldx #8
i2caddrloop: ; At start of loop SDA and SCL are both OUTPUT LOW
  asl outb ; Put MSB in carry

  lda DDRB
  ora #SCL
  sta DDRB ; Set SCL LOW by setting it to OUTPUT

  bcc seti2cbit0 ; If data bit was low

  lda DDRB       ; else set it high
  and #SDA_INV
  sta DDRB

  bcs wasone ; BRA
seti2cbit0:

  lda DDRB
  ora #SDA
  sta DDRB

wasone:

  lda DDRB
  and #SCL_INV
  sta DDRB ; Set SCL HIGH by setting it to input; Let SCL go HIGH by setting it to input

  dex
  bne i2caddrloop

  lda DDRB
  ora #SCL
  sta DDRB ; Set SCL LOW by setting it to OUTPUT ; Set SCL low after last bit

  lda DDRB ; Set SDA to INPUT
  and #SDA_INV
  sta DDRB

  lda DDRB
  and #SCL_INV
  sta DDRB ; Set SCL HIGH by setting it to input

  ; NOP here?
  lda PORTB ; Check ACK bit
  clc
  and #SDA
  bne nack
  sec ; Set carry to indicate ACK
  nack:

  lda DDRB
  ora #SCL
  sta DDRB ; Set SCL LOW by setting it to OUTPUT
  rts

i2cbytein: ; Assume SCL is LOW

  lda DDRB       ; Set SDA to input
  and #SDA_INV
  sta DDRB

  lda #0
  sta inb
  ldx #8
byteinloop:
  clc ; Clearing here for more even cycle

  lda DDRB
  and #SCL_INV
  sta DDRB ; Set SCL HIGH by setting it to input

  nop

  lda PORTB
  and #SDA
  beq got0
  sec
  got0:
  rol inb ; Shift carry into input byte
  lda DDRB
  ora #SCL
  sta DDRB ; Set SCL LOW by setting it to OUTPUT
  dex
  bne byteinloop

  lda DDRB ; Send NACK == SDA high (because we're ony fetching single bytes)
  and #SDA_INV
  sta DDRB
  lda DDRB
  and #SCL_INV
  sta DDRB ; Set SCL HIGH by setting it to input
  lda DDRB
  ora #SCL
  sta DDRB ; Set SCL LOW by setting it to OUTPUT
rts ; Input byte in inb

i2c_stop:
  lda DDRB ; SDA low
  ora #SDA
  sta DDRB
  lda DDRB
  and #SCL_INV
  sta DDRB ; Set SCL HIGH by setting it to input
  nop
  nop
  lda DDRB       ; Set SDA high after SCL == Stop condition.
  and #SDA_INV
  sta DDRB
  rts

i2c_test:
  ;lda #$77 ; Address $77 = BMP180 address
  lda I2CADDR
  clc ; Write
  jsr i2c_start
  ; Fail check here. C == 1 == ACK
  bcc failed
  ;lda #$D0 ; BMP180 register $D0 == chipID == $55
  lda I2CREG ; VGA screen EDID usually has address $50
  sta outb
  jsr i2cbyteout
  ;lda #$77 ; Address
  lda I2CADDR
  sec ; Read
  jsr i2c_start
  jsr i2cbytein
  jsr i2c_stop
  lda inb
  sec
  rts
  failed:
  lda #0
  rts

i2c_read:
  lda I2CADDR
  clc ; We "write" the address we want to read from
  jsr i2c_start
  bcc readfail
  lda #0
  sta outb
  jsr i2cbyteout
  jsr i2c_stop
  lda I2CADDR
  sec ; Now we read the register data
  jsr i2c_start
  jsr i2cbytein
  jsr i2c_stop
  lda inb
  readfail:
  rts


