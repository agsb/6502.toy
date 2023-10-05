

; This file has six sets of source code (the 6th one having been added 7/27/22):
; A. 65c02 source code for running the I2C interface as shown at
;    http://wilsonminesco.com/6502primer/potpourri.html#BITBANG_I2C in the 6502
;    primer's "potpourri" page.  I ran it with the actual circuit just enough
;    to have a degree of confidence the I'm not steering you wrong.
; B. a section on changes to make if you use different bit numbers
; C. my working Forth source code for using the 24256 32Kx8 I2C EEPROM.  Even if
;    you don't know Forth, much of it should be pretty clear as to what order
;    things need to happen in to interface to this 8-pin serial EEPROM.
;    It is profusely commented.
; D. my working Forth code for operating the MAX520 quad D/A converter.
; E. my working PIC code for running a tiny 24c00 EEPROM that was put as a
;    separate die in the PIC12CE673 we used for a product.
; F. my working PIC16 code for running a 24256 32Kx8 EEPROM that was part of
;    a semi-medical product I developed for work.  Even if you're not familiar
;    with PIC16 assembly language, the major use of macros with friendly names
;    should make it pretty clear the sequence of things that have to happen
;    to operate the 24256.

; Obviously you will need to separate out the needed parts for the applicable
; assembler or compiler.


COMMENT: MACRO          ; COMMENT and END_COMMENT here relieve us from having to
         IF 0           ; contend with so many semicolons in places where we
         ENDM           ; have many lines of comments together.  Since the IF is
 ;-------------------   ; looking for an ELSE or ENDI (either cap.s or lower
END_COMMENT: MACRO      ; case), just be sure none of the lines commented-out
        ENDI            ; start with one of these words that could cause
        ENDM            ; problems by fooling it.  If there is, that line will
 ;-------------------   ; still need a semicolon.  Also, if a line starts with a
                        ; macro name which is followed by illegal parameters for
                        ; that macro (as in a discussion of it), you will still
                        ; need the semicolon.

 COMMENT
   First:  General I2C material for 65c02, with bit numbers for the
   I2C-interfacing schematic in the 6502 primer's "potpourri" page at
   http://WilsonMinesCo.com/6502primer/potpourri.html

   Power control is on the VIA's PB7.  Holding the port bit high turns the I2C
   power off.  The bit can be used for other things as long as it is not held
   high for more that about 25ms when you want the I2C power to remain on.  This
   lets you simultaneously use PB7 for a beeper or a signal source for something
   else_ at the same time.  (The 25ms here is definitely not exact!)  This does
   mean the other function will have to store the PB7 value and put it back when
   it's done.

   The I2C clock line is on PA0, so you can form a clock pulse with only two
   instructions, INC & DEC.

   The I2C data line is on PA7 so that testing it takes fewer instructions than
   having to AND-out other bits or load the accumulator.

   In I2C, devices (and the controller) can pull the lines down, but not up.
   In most cases it's ok for the controller to pull the clock line up as well as
   down, but here we'll go with pull down only, and use the passive pull-up
   resistors for both data and clock.  The way to do that with the 6522 VIA is
   to put a "0" in the output register bit of interest, then set the
   corresponding bit in the data-direction register (DDRA in this case) to make
   the line an output and pull it down, or clear the DDRA bit to make the line
   an input and let it float up by the pull-up resistor.
 END_COMMENT


 HOF  "INT8"                    ; This tells C32 to output 8-bit Intel Hex.
 CPU  "6502.TBL"                ; Tell the assembler which processor.  (In C32,
                                ;            6502 includes 65c02 instructions.)

 ORG $000A                      ; Just one variable.  Put it wherever you like.
I2C_TEMP:  DFS  1               ; DFS in C32 is "DeFine Storage" for variables,
                                ;             and this reserves one byte for it.

VIA_BASE_ADR:  EQU  $4800       ; (Base address of the 6522 VIA.)
PB:     EQU  VIA_BASE_ADR + 0   ; Addresses of 16 registers in 6522.
PA:     EQU  VIA_BASE_ADR + 1   ; (We really only need the first four for this.)
DDRB:   EQU  VIA_BASE_ADR + 2
DDRA:   EQU  VIA_BASE_ADR + 3
T1CL:   EQU  VIA_BASE_ADR + 4
T1CH:   EQU  VIA_BASE_ADR + 5
T1LL:   EQU  VIA_BASE_ADR + 6
T1LH:   EQU  VIA_BASE_ADR + 7
T2CL:   EQU  VIA_BASE_ADR + 8
T2CH:   EQU  VIA_BASE_ADR + 9
SR:     EQU  VIA_BASE_ADR + $A
ACR:    EQU  VIA_BASE_ADR + $B
PCR:    EQU  VIA_BASE_ADR + $C
IFR:    EQU  VIA_BASE_ADR + $D
IER:    EQU  VIA_BASE_ADR + $E
PANOHS: EQU  VIA_BASE_ADR + $F


 ORG $3000    ; I stuck it at $3000.  You will probably want to change that.


I2C_DATA_UP: MACRO
             LDA   #10000000B   ; Two instructions here.  Clear bit 7 of the DDR
             TRB   DDRA         ; to make PA7 an input and let it float up.
             ENDM
 ;-----------------------

I2C_DATA_DN: MACRO
             LDA   #10000000B   ; Two instructions here.  Set bit 7 of the DDR
             TSB   DDRA         ; to make PA7 an output and pull it down since
             ENDM               ; bit 7 of the output register is a 0.
 ;-----------------------

I2C_CLK_UP:  MACRO              ; (as above)
             LDA   #1
             TRB   DDRA
             ENDM
 ;-----------------------

I2C_CLK_DN:  MACRO              ; (as above)
             LDA   #1
             TSB   DDRA
             ENDM
 ;-----------------------

I2C_START:   I2C_DATA_UP
             I2C_CLK_UP
             I2C_DATA_DN
 ist1:       INC   DDRA         ; Clk down.  We now know the bit val, so just INC.
             TRB   DDRA         ; Data up, using accum val left from I2C_DATA_DN above.
             RTS
 ;-----------------------

I2C_STOP:    I2C_DATA_DN
             I2C_CLK_UP
             I2C_DATA_UP
             BRA   ist1
 ;-----------------------

I2C_ACK:     I2C_DATA_DN        ; Acknowledge.  The ACK bit in I2C is the 9th bit of a "byte".
 ia1:        I2C_CLK_UP         ;               and acknowledging consists of pulling it down.
             INC   DDRA         ; Clk down.  We know the bit val, so just INC.
             I2C_DATA_UP
             RTS
 ;-----------------------

I2C_NAK:     I2C_DATA_UP        ; Not acknowledge.
             BRA   ia1
 ;-----------------------

I2C_ACK?:    I2C_DATA_UP        ; At end, N=0 means ACK.  N=1 means NAK.
             I2C_CLK_UP
             BIT   PA           ; Bit 7 (the data line) gets put in the N flag.
             TSB   DDRA         ; Clk down.  Accum still has 1 from I2C_CLK_UP.  Take advantage.
             RTS
 ;-----------------------

I2C_PWR_ON:  LDA   #10000000B   ; Clear bit 7 of port B.  It must first be made an output by doing INIT_I2C.
             TRB   PB
             RTS
 ;-----------------------

I2C_PWR_OFF:                    ; (Basically the same as INIT_I2C below.)
INIT_I2C:                       ; Set up the port bit directions and values.  Leaves power off, clk & data low.
        LDA     #10000000B
        TSB     PB              ; Make PB7 put out a high level (I2C power off) when made an output,
        TSB     DDRB            ; then make PB7 an output.

        INC     A               ; Put 10000001B in A for data and clock lines on port A.
        TSB     DDRA            ; Make PA0 and PA7 outputs to hold clock and data low while power is off,
        TRB     PA              ; and make the output value to be 0 for the same.
        RTS                     ; You might want to leave a delay to let the power die out so devices are really
 ;------------------            ;    cleared before turning it back on.  Then you shouldn't need CLR_I2C below.

CLR_I2C:                        ; This clears any unwanted transaction that might be in progress, by giving
        JSR     I2C_STOP        ;    enough clock pulses to finish a byte and not acknowledging it.
        JSR     I2C_START
        I2C_DATA_UP             ; Keep data line released so we don't ACK any byte sent by a device.
        LDX     #9              ; Loop 9x to send 9 clock pulses to finish any byte a device might send.
 ci2c:     DEC  DDRA            ; Like I2C_CLK_UP since we know I2C_START left clock down (DDRA bit 0 high).
           INC  DDRA            ; Like I2C_CLK_DN since we know the state from the above instruction.
           DEX
        BNE     ci2c
        JSR     I2C_START
        JMP     I2C_STOP        ; (JSR, RTS)
 ;------------------

SEND_I2C_BYTE:                  ; Start with byte in A, and clock low.  Ends with I2C_ACK?.
        STA     I2C_TEMP        ; Store the byte in a variable so we can use A with TSB & TRB for data line.
        LDA     #10000000B      ; Init A for mask for TRB & TSB below.  A does not get disturbed below.
        LDX     #8              ; We will do 8 bits.
 sIb2:     TRB  DDRA            ; Release data line.  This is like I2C_DATA_UP but saves 1 instruction.
           ASL  I2C_TEMP        ; Get next bit to send and put it in the C flag.
           BCS  sIb1
              TSB DDRA          ; If the bit was 0, pull data line down by making it an output.
 sIb1:     DEC  DDRA            ; Do a high pulse on the clock line.  Remember there's a 0 in the output
           INC  DDRA            ; register bit, and DEC'ing DDRA makes that bit an input, so it can float up.
           DEX                  ;    IOW, it's backwards from what it seems.
        BNE     sIb2
        JMP     I2C_ACK?        ; (JSR, RTS)
 ;------------------

RCV_I2C_BYTE:                   ; Start with clock low.  Ends with byte in I2C_TEMP.  Do ACK bit separately.
        I2C_DATA_UP             ; Make sure we're not holding the data line down.  Be ready to input data.
        LDX     #8              ; We will do 8 bits.  There's no need to init I2C_TEMP.
 rIb1:     DEC  DDRA            ; Set clock line high.
           ASL  I2C_TEMP        ; Get the forming byte's next bit position ready to accept the bit.
           BIT  PA              ; Read the data line value into N flag.
           BPL  rIb2            ; If the data line was high,
              INC  I2C_TEMP     ; increment the 1's place to a 1 in the forming byte.  (ASL made bit 0 = 0.)
 rIb2:     INC  DDRA            ; Put clock line back low.
           DEX
        BNE     rIb1            ; Go back for next bit if there is one.
        RTS
 ;------------------

 END

-------------------------------------------------------------------
Added 4/15/23:  Here's the same thing as the last three routines above but using my program-structure macros
at http://wilsonminesco.com/StructureMacros/ .  It assembles exactly the same thing, but the source code is
more readable.


CLR_I2C:                        ; This clears any unwanted transaction that might be in progress, by
      JSR     I2C_STOP          ;   giving enough clock pulses to finish a byte and not acknowledging it.
      JSR     I2C_START
      I2C_DATA_UP               ; Keep data line released so we don't ACK any byte sent by a device.
      FOR_X   9, DOWN_TO, 0     ; Loop 9x to send 9 clock pulses to finish any byte a device might send.
         DEC  DDRA              ; Like I2C_CLK_UP since we know I2C_START left clock down (DDRA bit 0 high).
         INC  DDRA              ; Like I2C_CLK_DN since we know the state from the above instruction.
      NEXT_X                    ; (Assembles DEX, BNE.)
      JSR     I2C_START
      JMP     I2C_STOP          ; (JSR, RTS)
 ;------------------

SEND_I2C_BYTE:                  ; Start with byte in A, and clock low.  Ends with I2C_ACK?.
      STA     I2C_TEMP          ; Store the byte in a variable so we can use A with TSB & TRB for data line.
      LDA     #10000000B        ; Init A for mask for TRB & TSB below.  A does not get disturbed below.
      FOR_X   8, DOWN_TO, 0     ; We'll do 8 bits.  FOR_X assembles LDX #8, and tells NEXT_X what to assemble.
         TRB  DDRA              ; Release data line.  This is like I2C_DATA_UP but saves 1 instruction.
         ASL  I2C_TEMP          ; Get next bit to send and put it in the C flag.
         IF_C_CLR               ; (IF_C_CLR just assembles a BCS to the line following its matching END_IF.)
            TSB DDRA            ; If the bit was 0, pull data line down by making it an output.
         END_IF                 ; (The END_IF macro just fills in the branch address for the IF above.)
         DEC  DDRA              ; Do a high pulse on the clock line.  Remember there's a 0 in the output
         INC  DDRA              ; register bit, and DEC'ing DDRA makes that bit an input, so it can float up.
      NEXT_X                    ;    IOW, it's backwards from what it seems.  (This line assembles DEX, BNE.)
      JMP     I2C_ACK?          ; (JSR, RTS)
 ;------------------

RCV_I2C_BYTE:                   ; Start with clock low.  Ends with byte in I2C_TEMP.  Do ACK bit separately.
      I2C_DATA_UP               ; Make sure we're not holding the data line down.  Be ready to input data.
      FOR_X   8, DOWN_TO, 0     ; We will do 8 bits.  There's no need to init I2C_TEMP.
         DEC  DDRA              ; Set clock line high.
         ASL  I2C_TEMP          ; Get the forming byte's next bit position ready to accept the bit.
         IF_BIT  PA, 7, IS_HIGH ; Read data line value into N flag.  (Assembles BIT <addr>, BPL.)  If high,
            INC  I2C_TEMP       ; increment the 1's place to a 1 in the forming byte.  (ASL made bit 0 = 0.)
         END_IF
         INC  DDRA              ; Put clock line back low.
      NEXT_X                    ; Go back for next bit if there is one.
      RTS
 ;------------------

 END

==============================================================================================================

 SECTION B:   things you need to modify in section A if you use different bits

If you use different bits, the TSB and TRB instructions remain the same, but you cannot use the BIT instruction
by itself to test random bits, or use INC and DEC to produce clock pulses.  BIT puts bit 7 in the N flag and
bit 6 in the V flag, so you could put the data line on bit 6 and test with BIT followed by BVC or BVS; but using
other bits would require using AND to test them, which, depending on how you do it, may require some gymnastics
in handling the value in the accumulator.  The way it is done above in SEND_I2C_BYTE above leaves the accumulator
undisturbed.  To test bit 4 for example, you could do:

        LDA  PA
        AND  #00010000B
        BNE  ...             ; (or BEQ)

or to release it in I2C so the pull-up resistor can make it float up:

        LDA  DDRA
        AND  #11101111B
        STA  DDRA

or to pull it down:

        LDA  DDRA
        ORA  #00010000B
        STA  DDRA

or for an up-down clock pulse:

        LDA  DDRA
        AND  #11101111B
        STA  DDRA
        ORA  #00010000B
        STA  DDRA

which is more instructions and bytes than

        DEC  DDRA
        INC  DDRA

like we can do with the clock on bit 0.

In the unlikely senario that you have the VIA in zero page and a Rockwell or WDC 65c02, you can use the BBS,
BBR, SMB, and RMB direct bit-manipulating instructions.

The circuit given has the master pull the clock line down but never up.  Depending on the capacitive loading
and how fast you want to run the I2C bus, you might want to modify the program to not pull the clock line down
without first verifying (by reading the clock line) that the bus capacitance has been charged up to the high
voltage.  A few devices can also hold the clock line down to indicate to the master that they need a little
more time to process a bit.  If your program waits for it to float up, it will take care of that too.  I did
not put in anything for multi-master systems and how to pass control.



==============================================================================================================

 SECTION C:   my working Forth source code for using the 24256 32Kx8 I2C EEPROM.  Even if you don't know Forth,
              much of it should be pretty clear as to what order things need to happen in to interface to this
              8-pin serial EEPROM.  You can almost just go by the comments in some cases.  In this one, I had
              the clock line fed from a totem-pole output on the master, without a passive pull-up resistor on
              the clock line.


 \ omitted: all the general I2C stuff, translated above into 6502 assembly.  Below is EERPOM-specific.

     [B]
10100000  000 2* OR  CONSTANT 24256_ADR_W       \ SGS Thomson M24256 EEPROM can only have one addr (%1010000x),
  24256_ADR_W  1 OR  CONSTANT 24256_ADR_R       \ and you tack the R/W\ bit on in bit 0.
     [H]
                \ * * * * *  Bit 3 must be a 1 when you want to address the upper 32KB of a 24LC515 64Kx8 EERPOM.


: POLL_EEPROM   ( -- f )                \ Note: per datasheet, no STOP condition is given.
   I2C_START
   24256_ADR_W   SEND_I2C_BYT       ;


: WAIT_TIL_EEPROM_RDY   ( -- )          \ Keep polling the EEPROM until it issues the ACK bit meaning that it's done carrying
   BEGIN  POLL_EEPROM  UNTIL            \ out any previous write instructions, and it's ready to for the next instruction.
   I2C_STOP                         ;   \ * * * * *  This line is new.


: SEND_EEPROM_ADR       ( addr -- )
   WAIT_TIL_EEPROM_RDY                  \ Make sure the EEPROM is finished with any previous WRITE commands.
   I2C_START                            \ Cause a START condition.
   24256_ADR_W  SEND_I2C_BYT  DROP      \ Device select.  Drop ACK flag.
   SPLIT        SEND_I2C_BYT  DROP      \ Send addr hi,         "
                SEND_I2C_BYT  DROP  ;   \ then addr lo.         "


: WR_EEPROM_BYT   ( B addr -- )
   SEND_EEPROM_ADR                      \ First see if EEPROM is ready, do a START condition, and then send address.
   SEND_I2C_BYT  DROP    I2C_STOP   ;   \ Send data, and cause a STOP condition.


: WR_EEPROM_ADR   ( addr -- )
   SEND_EEPROM_ADR       I2C_STOP   ;


: WR_EEPROM_PAGE  ( EEPROM_addr  str_addr  count -- )   \ count can be up to 64 bytes, which will all go in the same
   ROT   SEND_EEPROM_ADR                                \ EEPROM addr row, even if that means it wraps around.  A uChip ap.
   BOUNDS                                               \ note says a given location in their serial EEPROMs will last about
   DO    I C@   SEND_I2C_BYT  DROP                      \ 5x as many writes if it is written to as part of a page write; so
   LOOP              I2C_STOP       ;                   \ if you need to do more than about 15 bytes in a 64-byte page, it
                                                        \ may be best to do the whole page instead of individual bytes.

: RD_EEPROM_BYT   ( -- B )      \ Byte addr must already be set. W/ SGS Thomson, addr is automatically inc'ed aftr each read.
   \ WAIT_TIL_EEPROM_RDY        \ Don't do the polling, because it clears the address you just gave it!
   I2C_STOP
   I2C_START
      24256_ADR_R  SEND_I2C_BYT  DROP   \ Device select, telling it to output data.  (DROP the ACK flag.)
                    RCV_I2C_BYT         \ Get byte, & leave VIA bit as output.
      I2C_NAK                           \ Send a not-acknowledge bit to tell EEPROM to quit and go into standby.
   I2C_STOP             ;


: RD_EEPROM_STREAM   ( destination_mem_addr  count -- )   \ EEPROM addr must already be set.
   \ WAIT_TIL_EEPROM_RDY                \ Don't do the polling, because it clears the address you just gave it!
   I2C_STOP
   I2C_START
      24256_ADR_R  SEND_I2C_BYT  DROP   \ Device select, telling it to output data.  DROP the ACK bit result.
      BOUNDS
      DO   RCV_I2C_BYT   I C!           \ Get byte and store it in RAM.
           R@ -1 =                      \ Are we on the last one?  (See note at CMP_EEPROM.)
           IF   I2C_NAK                 \ If so, answer with not-acknowledge bit.
           ELSE I2C_ACK                 \ Else, acknowledge so the EEPROM doesn't stop yet.
           THEN
      LOOP
   I2C_STOP             ;


: WR_ARRAY    ( beg_RAM_adr  EEPROM_addr  #_of_bytes  --  )    \ As we go, the addresses will climb while the count drops.
   BEGIN   DUP                        \ Is there more to store?       (DUP the #_of_bytes for WHILE to evaluate.)
   WHILE                                 \ If so, work on the next EERPOM page.
      OVER  3F AND       0=              \ Will we start on an EEPROM page boundary,
      OVER  3F >        AND              \ and is there at least a whole page of data left to go?
      IF OVER  3PICK  40  WR_EEPROM_PAGE \ If so, put the next $40 bytes in the next EEPROM page.
         ROT 40 +                        \ Then increment the beginning  RAM   addr by $40,
         ROT 40 +                        \ then increment the beginning EEPROM addr by $40,
         ROT 40 -                        \ and  decrement the number of bytes left to store.
      ELSE
         BEGIN                           \ If not doing a whole EEPROM page, write bytes to EEPROM individually.
            1-                           \ Decrement the number of bytes left to do.
            SWAP DUP >R  1+  SWAP        \ Copy next EEPROM addr to return stack, and increment the EEPROM addr.
            ROT  DUP >R  1+  -ROT        \ Copy next  RAM   addr to return stack, and increment the  RAM   addr.
            R> C@   R>   WR_EEPROM_BYT   \ From return stack, get EEPROM addr, RAM addr, then fetch data, and store it.
            OVER 3F AND 0=               \ Now if the next byte to program would start another EERPOM page,
            OVER        0=   OR          \ or if there are no bytes left to program, then
         UNTIL                           \ leave the BEGIN...UNTIL, either because maybe the next page should be done
      THEN                               \ as a page write or because we're done writing the array.
   REPEAT  3DROP                ;        \ DROP the three parameters from the stack.


\ CMP_EEPROM below is used to see if the EEPROM contents match the CPU memory contents over a range (like for after
\ programming, to make sure I didn't forget to un-write-protect it or something like that).  There's no need to give
\ the address that fails the test.
\ Note here (and in RD_EEPROM_STREAM the use of R@ -1 = in the 9th line.  The way LMI's do and loop work, the R@ gives index
\ minus limit, so I had to do extra work.  When I write my own and fix it, this code here will probably need changing.
\ I had a note that this did over 2KB/second; but I don't remember if that was at 2MHz or 5MHz clock rate.


: CMP_EEPROM    ( EEPROM_addr  RAM_addr  count --  flag )   \ Flag is true if it checks out with no errors.
   ROT  WR_EEPROM_ADR                    \ (This ends with a STOP.)
   I2C_START
      24256_ADR_R  SEND_I2C_BYT  DROP    \ Device select, telling it to output data.
      BOUNDS
      DO RCV_I2C_BYT  I C@ <>            \ Get byte.  Is it different from RAM?
         IF  I2C_NAK  FALSE              \ If so, tell the EEPROM to quit sending, output a flag showing
             UNLOOP   EXIT               \ that the compare failed, and exit the loop and the word.
         THEN
         R@ -1 =                         \ Are we on the last one?  (See note in paragraph above.)
         IF   I2C_NAK                    \ If so, answer with a not-acknowledge bit.
         ELSE I2C_ACK                    \ Else, acknowledge so the EEPROM doesn't stop yet.
         THEN
      LOOP
   I2C_STOP  TRUE       ;

==============================================================================================================

 SECTION D:   my working Forth code for operating the MAX520 quad D/A converter.  (I only used two channels.)



                                         [B]
00001000             CONSTANT POWER_UP          \ Command byte to power-up the 520.  (not necessary?)
01010000  011 2* OR  CONSTANT D/A_ADR           \ D/A slave addr is 3 for proto board.
00010000             CONSTANT RESET_BYT         \ Command byte for resetting 520.
00000000             CONSTANT OUTPUT_0          \ Command byte for outputting on ch 0
00000010             CONSTANT OUTPUT_2   [H]    \ Command byte for outputting on ch 2


: RESET_DAC     ( -- )
   INIT_VIA
   I2C_START
      D/A_ADR    SEND_I2C_BYT  DROP             \ I'm just DROPping the ACK bit result.
      RESET_BYT  SEND_I2C_BYT  DROP
   I2C_STOP                             ;


: OUT_0         ( b -- )                        \ Send a byte to D/A output 0.
   I2C_START
      D/A_ADR   SEND_I2C_BYT  DROP
      OUTPUT_0  SEND_I2C_BYT  DROP
                SEND_I2C_BYT  DROP
   I2C_STOP                             ;


: OUT_2         ( b -- )                        \ Send a byte to D/A output 2.
   I2C_START
      D/A_ADR   SEND_I2C_BYT  DROP
      OUTPUT_2  SEND_I2C_BYT  DROP
                SEND_I2C_BYT  DROP
   I2C_STOP                             ;


==============================================================================================================

 SECTION E:   my working PIC12CE673 code for operating its EEPROM data memory


Variables used below are TEMP_W, LOOP_COUNT1, EEPROM_DATA, and EEPROM_DATA_ADR, all one byte each.  Hopefully I got 'em all.


; The PIC12CE673 has 16 bytes of EEPROM data memory, with an endurance of 1M erase/write cycles and data retention >40 years.
; The EEPROM is connected by an I2C serial interface on GP6 and GP7.  It appears to be just a 24C00 (even on a separate die with
; four bond wires!), the same as the 24256 EEPROM I've already used except that the address of a particular memory location is
; given as only one byte, not two.
;
; A few things must be kept in mind when doing successive read-modify-write (RMW) instructions on an I/O port:
;   1. Reading the port reads the pins, not the output register.  An open-drain output pulled low externally will get pulled low
;       internally after the RMW even if that pin was not supposed to get changed.
;   2. The "write" part happens in the last clock of an instruction, but the "read" happens in the first.  Without a NOP between
;       one RMW instruction and the next, the second one may read the port before the value is stable, and give a wrong result.
;   3.  * * * * *
;
; Timings that must be observed are:
; clock-low time: 1.3us min
; bus-free time:  1.3us min before next transmission
; output valid:    .9us after clock edge
; write cycle time is 4ms max.

; The code from here down to POLL_EEPROM is just standard I2C material.  After that it's particular to the EEPROM that's in the
; PIC12CE67x uC.


I2C_DATA_UP:    MACRO           ; GP6 is an open-drain line with a pull-up resistor,
        BSF     GPIO,6          ; so I2C_DATA_UP allows the line to come up but still
        NOP                     ; allows the EEPROM to pull it down.
        ENDM
 ;-------------
I2C_DATA_DN:    MACRO
        BCF     GPIO,6
        NOP			; The NOP in these is due to rule #2 above.
        ENDM
 ;-------------
I2C_CLK_UP:     MACRO           ; I2C clock line for EEPROM is on GP7, and data on GP6.
        BSF     GPIO,7
        NOP
        ENDM
 ;-------------
I2C_CLK_DN:     MACRO           ; Make sure this is followed by DATA_UP when needed because
        BCF     GPIO,7          ; of the RMW open-drain problem (rule #1 above).
        NOP
        ENDM
 ;------------------

I2C_START: I2C_DATA_UP
           I2C_CLK_UP
           I2C_DATA_DN
           I2C_CLK_DN
           I2C_DATA_UP
           RETURN
 ;-------------
I2C_STOP:  I2C_DATA_DN
           I2C_CLK_UP
           I2C_DATA_UP
           I2C_CLK_DN
           I2C_DATA_UP          ; This line is here because of rule #1 above.
           RETURN
 ;-------------
I2C_ACK:   I2C_DATA_DN
           I2C_CLK_UP
           I2C_CLK_DN
           I2C_DATA_UP
           RETURN
 ;-------------
I2C_NAK:   I2C_DATA_UP
           I2C_CLK_UP
           I2C_CLK_DN
           I2C_DATA_UP          ; This is here because of rule #1 above.
           RETURN
 ;-------------
I2C_ACK?:  I2C_DATA_UP          ; Leaves Carry flag set (not clear) if there's a true (low) acknowledge bit.
           I2C_CLK_UP
           BCF     STATUS,C     ; Init C as clear.
           BTFSS   GPIO,6       ; If I2C data is low (for acknowledge),
           BSF     STATUS,C     ; then set C.
           I2C_CLK_DN
           I2C_DATA_UP          ; This is here because of rule #1 above.
           RETURN
 ;------------------

INIT_I2C:                       ; (start, send 9 high bits, start, stop)
        CALL    I2C_STOP
        CALL    I2C_START
        MOVLW   9
        MOVWF   LOOP_COUNT1
ii2:       I2C_DATA_UP          ; (This line is from rule #1 above.)
           I2C_CLK_UP
           I2C_CLK_DN
           DECFSZ  LOOP_COUNT1,F
        GOTO    ii2
        CALL    I2C_START
        GOTO    I2C_STOP        ; (CALL, RETURN)
 ;-------------
RD_I2C_BIT:                     ;  ( EERPOM_DATA holds:  n --> 2n | 2n+1 )      Data line must already be released.
        BCF     STATUS,C        ; Init C as clear.
        BTFSC   GPIO,6          ; If I2C data is high,
        BSF     STATUS,C        ; then set C.
        RLF     EEPROM_DATA,F
        RETURN
 ;-------------
SEND_I2C_BYTE:                  ; Start with byte in W.
        MOVWF     TEMP_W
        MOVLW     8
        MOVWF     LOOP_COUNT1
sib1:      I2C_DATA_UP
           RLF    TEMP_W,F      ; Get next data bit in C.
           BTFSS  STATUS,C      ; If this one skips, it will hit the NOP in I2C_DATA_DN, which is fine.
           I2C_DATA_DN
           I2C_CLK_UP
           I2C_CLK_DN
           DECFSZ LOOP_COUNT1,F
        GOTO      sib1
        GOTO      I2C_ACK?      ; (CALL, RETURN)
 ;-------------
RCV_I2C_BYTE:
        I2C_DATA_UP
        CLRF      EEPROM_DATA           ; Init EEPROM_DATA as 0.
        MOVLW     8
        MOVWF     LOOP_COUNT1
rib1:      I2C_DATA_UP
           I2C_CLK_UP
           CALL   RD_I2C_BIT
           I2C_CLK_DN
           DECFSZ LOOP_COUNT1,F
        GOTO      rib1
        RETURN                          ; The ACK or NAK bit must be sent separately.
 ;----------------------------

EEPROM_ADR_W:   EQU     B'10100000'     ; EEPROM can only have one addr (%1010000x),
EEPROM_ADR_R:   EQU     B'10100001'     ; and you tack the R/W\ bit on in bit 0.

POLL_EEPROM:                            ; Note: per data sheet, no STOP condition is given.
        CALL    I2C_START
        MOVLW   EEPROM_ADR_W
        GOTO    SEND_I2C_BYTE           ; (CALL, RETURN)   At end, C=1 if EEPROM is ready.
 ;-------------
WAIT_TIL_EEPROM_RDY:
        CALL    POLL_EEPROM
        BCC     WAIT_TIL_EEPROM_RDY     ; Keep looping while EEPROM not ready.
        RETURN
 ;-------------
SEND_EEPROM_ADR:                        ; Start with EEPROM data byte addr in EEPROM_DATA_ADR.
        CALL    WAIT_TIL_EEPROM_RDY
        CALL    I2C_START
        MOVLW   EEPROM_ADR_W
        CALL    SEND_I2C_BYTE
        MOVF    EEPROM_DATA_ADR,W
        GOTO    SEND_I2C_BYTE           ; (CALL, RETURN)
 ;-------------
WR_EEPROM_BYTE:                         ; Start with EEPROM data byte addr in EEPROM_DATA_ADR and data in EEPROM_DATA.
        CALL    SEND_EEPROM_ADR
        MOVF    EEPROM_DATA,W
        CALL    SEND_I2C_BYTE
        GOTO    I2C_STOP                ; (CALL, RETURN)
 ;-------------
RD_EEPROM_BYTE:                         ; EEPROM's data byte addr pointer must already be correct.  End w/ data in EEPROM_DATA & W.
    ;   CALL    WAIT_TIL_EEPROM_RDY     ; Don't do the polling, because that would clear the address you just gave it!
        CALL    I2C_STOP
        CALL    I2C_START
        MOVLW   EEPROM_ADR_R
        CALL    SEND_I2C_BYTE
        CALL    RCV_I2C_BYTE
        CALL    I2C_NAK                 ; Send a not-acknowledge bit to tell EEPROM to quit and go into standby.
        CALL    I2C_STOP
        MOVF    EEPROM_DATA,W
        RETURN
 ;------------------

 END


==============================================================================================================

 SECTION F:   my working code for operating a 24256 EEPROM on a PIC16F74 for a work project in 2009.  The PIC16F74's SSP was
              already in use for SPI for something else, which is why the I2C is bit-banged here.  This one has a few things
              that are not in section E above, including extended addressing, page write and reading a continuous stream, and
              that it uses my program flow-control structure macros whose don and source code are in the applicable
              section of this website, http://wilsonminesco.com/StructureMacros/ which make the source code more clear, and
              assemble, in most cases, exactly what you would do by hand; IOW, they present no penalty at all in run speed or
              memory taken.  The 16F74 was run at 20MHz.  As usual, I hope your monitor doesn't wrap the long lines, as that
              would make the source code look a disaster!


;                    +---------------------+
;                    |   EEPROM material   |                                    PORTE<0>=clock
;                    +---------------------+                                    PORTE<1>=data


 ; The I2C material below was transferred from HSINCFI2.ASM for the PIC16CE675 for the H/S Inc battery box which had 24C00 in it,
 ; and modified for the applicable port pins and the fact that we're using PE<1:0> to emulate open-drain outputs with the output
 ; latch always having 0's.  (Later: modified also for EEPROMs that take two address bytes, like the 24256.)
 ;
 ; The densest 24-family EEPROM you can get with a single address byte is the 24C02 with 256 bytes of EEPROM and an endurance of
 ; 1M+ erase/write cycles and data retention >40 years.  After getting started, I decided I better allow more memory than that, so
 ; I went to two address bytes, ruling out the 24c02 and down.  The 24256 will always have more than enough for this project, and
 ; smaller ones may be adequate and save money.  The EEPROM is connected by an I2C serial interface on RE0 and RE1.  I've already
 ; used the 24256 on the workbench computer.
 ;
 ; A few things must be kept in mind when doing successive read-modify-write (RMW) instructions on a PIC I/O port:
 ;   1. Reading the port reads the pins, not the output register.  An open-drain output pulled low externally will get pulled low
 ;      internally after the RMW even if that pin was not supposed to get changed.  Changing the TRIS bit may work better for I2C.
 ;   2. The "write" part happens in the last clock of an instruction, but the "read" happens in the first.  Without a NOP between
 ;      one RMW instruction and the next, the second one may read the port before the value is stable, and give a wrong result.
 ;   3.  * * * * *
 ;
 ; 24256 timings that must be observed are:
 ; clock freq:           400kHz max
 ; clock-high time:        .6us min                          (3 inst cy @ 20MHz)
 ; clock-low  time:       1.3us min                          (7 inst cy)
 ; bus-free   time:       1.3us min before next transmission (7 inst cy)
 ; data input setup time: 100ns min before clock rises       (next inst is ok)
 ; data input hold time:    0us min after clock falls        (next inst is ok)
 ; output valid:           .9us max after falling clock edge
 ; write cycle time:        5ms max

 ; Initially I had this in a more logical order, with the standard I2C material first, followed by code particular to the EEPROM;
 ; but I ran out of memory and had to re-arrange it to make better use of program memory, to eliminate some routines' ending GOTOs
 ; or effective CALL/RETURN combinations.

 ; Since RMW operations on a port that has an open-drain output can corrupt those bits, we will just alter the TRIS bits instead.
 ; This means that the data in the two output bits (RE0 and RE1) must be written as 0 to init (which is done in the reset routine).
 ; TRIS registers are in RAM_BANK 1, so unfortunately we have to keep changing RAM banks.

 ; The following subroutines started out being just the content of the macros further down, then I realized that they would go too
 ; fast for I2C so I had to make them subroutines, then I realized I had forgotten to switch RAM banks for them so the added length
 ; would make them kind of long to reasonably be macros anyway.


                      ; IDU (I2C Data Up) is 31 lines down, at the end of I2C_ACK?.  ICD (I2C Clock Down) is at the end of I2C_NAK.

IDD:    RAM_BANK  1                     ; I2C Data Down         7 instruction cycles, incl CALL & RETURN
           BCF    TRISE, 1              ; Make the bit an output so the 0 in the output port pulls the line down.
idd1:   RAM_BANK  0                     ; Pulling down is faster than letting it float up, so I put the label here for IDU to
        RETURN                          ; branch to, instead of vice-versa.
 ;-------------

ICU:    RAM_BANK  1                     ; I2C Clock UP          minimum of 9 inst cy, incl CALL & RETURN
           BSF    TRISE, 0              ; I2C clock line for EEPROM is on RE0 (16F74 pin 8), and data on RE1.
        RAM_BANK  0
        BEGIN                           ; Do not go forward until the line has floated up, partly so a device can hold it down to
        UNTIL_BIT PORTE, 0, IS_HIGH     ; ask for more time.
        RETURN
 ;----------------------------

I2C_DATA_UP:    MACRO                   ; 9 instruction cycles, incl CALL & RETURN
        CALL    IDU
        ENDM
 ;-------------
I2C_DATA_DN:    MACRO                   ; 7 instruction cycles, incl CALL & RETURN
        CALL    IDD
        ENDM
 ;-------------
I2C_CLK_UP:     MACRO                   ; minimum of 9 inst cy, incl CALL & RETURN
        CALL    ICU
        ENDM
 ;-------------
I2C_CLK_DN:     MACRO                   ; 9 instruction cycles, incl CALL & RETURN
        CALL    ICD                     ; (19 lines down, at the end of I2C_NAK.)
        ENDM
 ;------------------
                                ; inst cycles, to make sure we don't outrun the EEPROM (We're just barely giving it enough time):
                                        ; / /                    2
I2C_START: I2C_DATA_UP                  ; / / / ^ / / / / /      9
           I2C_CLK_UP                   ; / / / ^ / / / / /      9
           I2C_DATA_DN                  ; / / / v / / /          7
           GOTO  ia1                    ; / / / / / v / / / / / 11      ; (lowers the clock and releases the data line)
 ;------------------
                                        ; / /                    2
I2C_ACK:   I2C_DATA_DN                  ; / / / v / / /          7
           I2C_CLK_UP                   ; / / / ^ / / / / /      9
           GOTO  ia1                    ; / / / / / v / / / / / 11      ; (lowers the clock and releases the data line)
 ;------------------

I2C_NAK:                                ; / /                    2
        I2C_DATA_UP                     ; / / / ^ / / / / /      9
        I2C_CLK_UP                      ; / / / ^ / / / / /      9      ; Then it goes right into ICD to save program memory.
ICD:    RAM_BANK  1                     ; /
        BCF   TRISE, 0                  ; /   I2C Clock Down
        GOTO  idd1                      ; / / / / / /                   ; (just goes back to RAM bank 0 and RETURNs)
 ;------------------

INIT_I2C:                               ; (start, send 9 high bits, start, stop)  TRISE must already be set up.  PORT E output
        CALL    I2C_STOP                ; latches are cleared in the reset routine.
        CALL    I2C_START               ; (Leaves data line released.)

        FOR   LOOP_CNT, 9, DOWN_TO, 0   ; We're going to do 9 pulses, and the released data line makes for NAK.
           I2C_CLK_UP
           I2C_CLK_DN
        NEXT  LOOP_CNT

        CALL    I2C_START               ; then goes right into I2C_STOP, to save program memory.

I2C_STOP:                               ; / /                    2
        I2C_DATA_DN                     ; / / / v / / /          7
        I2C_CLK_UP                      ; / / / ^ / / / / /      9
        I2C_DATA_UP                     ; / / / ^ / / / / /      9
        GOTO  ICD                       ; / / / v / / / / /      9
 ;------------------

RD_I2C_BIT:  MACRO                      ;  ( EERPOM_DATA holds:  n --> 2n | 2n+1 )      Data line must already be released.
        BCF     STATUS, C               ; Init C as clear.
        BTFSC   PORTE,  1               ; If I2C data is high,
        BSF     STATUS, C               ; then set C.
        RLF     EEPROM_DATA, F
        ENDM
 ;------------------

RCV_I2C_BYTE:
        I2C_DATA_UP
        CLRF  EEPROM_DATA               ; Init EEPROM_DATA as 0.
        FOR  LOOP_CNT, 8, DOWN_TO, 0
           I2C_CLK_UP
           RD_I2C_BIT                   ; RD_I2C_BIT collects the bits in variable EEPROM_DATA.
           I2C_CLK_DN
        NEXT  LOOP_CNT
        RETURN                          ; The ACK or NAK bit must be sent separately.           <--  <--  <--  NOTE ! !
 ;----------------------------

EEPROM_ADR_W:   EQU     B'10100000'     ; EEPROM can only have one addr (%1010000x), and you tack the R/W\ bit on in bit 0.
EEPROM_ADR_R:   EQU     B'10100001'     ; These addresses are the same across many densities of 24xx-family EEPROMs.
                                        ; If you use a Microchip one, ground pins 1, 2, and 3.


WAIT_TIL_EEPROM_RDY:  MACRO             ; This was a subroutine, but I was only using it in SEND_EEPROM_ADR, so I made it a macro.
        BEGIN
            CALL   I2C_START            ; This part between BEGIN and UNTIL had said CALL POLL_EEPROM, but this was the only place
            MOVLW  EEPROM_ADR_W         ; POLL_EEPROM was used, so I straightlined it here.  Note: per the data sheet, no STOP
            CALL   SEND_I2C_BYTE        ; condition is given in the polling.        C will be = 1 if EEPROM is ready.
        UNTIL_CARRY_SET                 ; Keep looping until EEPROM is ready.  Carry flag set means there was an acknowledge.
        ENDM                            ; If the EEPROM is defective or missing, the looping will give the power-up message
 ;------------------                    ; "No EEPROM!" time to be seen.

SEND_EEPROM_ADR:                        ; Start with EEPROM data byte addr in EEPROM_DATA_ADR.
        WAIT_TIL_EEPROM_RDY
        CALL    I2C_START
        MOVLW   EEPROM_ADR_W
        CALL    SEND_I2C_BYTE
        MOVF    EEPROM_DATA_ADR+1, W    ; These two lines were added for going from smaller EEPROMs with a single-byte
        CALL    SEND_I2C_BYTE           ; address (like 24c02) to larger ones with a two-byte address (like 24256).
        MOVF    EEPROM_DATA_ADR, W      ;    In the address variable, the address low byte is first.
                                        ; Then it goes right into SEND_I2C_BYTE (next) to save program memory.
SEND_I2C_BYTE:                          ; Start with byte in W.
        MOVWF     TEMP_W
        FOR  LOOP_CNT, 8, DOWN_TO, 0
           I2C_DATA_UP
           RLF    TEMP_W, F             ; Get next data bit in C.
           BTFSS  STATUS, C             ; If this one skips, it will hit the NOP in I2C_DATA_DN, which is fine.
           I2C_DATA_DN
           I2C_CLK_UP
           I2C_CLK_DN
        NEXT  LOOP_CNT                  ; Then goes right into I2C_ACK? (next) to save program memory.

I2C_ACK?:
        I2C_DATA_UP                     ; / / / ^ / / / / /     Leaves Carry flag set (not clear) if there's a true (low)
        I2C_CLK_UP                      ; / / / ^ / / / / /                                                        acknowledge bit.
        BCF     STATUS, C               ; /                     Init C as clear.
        BTFSS   PORTE,  1               ; /                     If I2C data is low (for acknowledge),
        BSF     STATUS, C               ; /                     then set C.
ia1:    I2C_CLK_DN                      ; / / / v / / / / /
                                        ; Then it goes into IDU (next) to save program memory.
IDU:    RAM_BANK  1                     ; I2C Data Up (release the data line)   9 instruction cycles, incl CALL & RETURN
        BSF       
