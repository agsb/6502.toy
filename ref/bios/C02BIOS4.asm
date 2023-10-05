;**************************************************************************************************
;*    C02BIOS 4.01 - Release version for Pocket SBC  (c)2013-2022 by Kevin E. Maier 27/01/2022    *
;*                                                                                                *
;* BIOS Version 4.01 supports the following 3.3V hardware specification:                          *
;*                                                                                                *
;*  - W65C02S with clock rate up to 8.0 MHz                                                       *
;*  - AS6C66256 32KB Static RAM                                                                   *
;*  - AT28BV256 32KB EEPROM ROM                                                                   *
;*  - ATF22LV10C Single Glue Logic                                                                *
;*  - NXP SC28L92 DUART for Console Port / Aux Serial Port / Timer                                *
;*  - TL7533 Reset Circuit (positive & negative Reset signals)                                    *
;*  - DS1233A Reset Circuit for NMI Panic Trigger                                                 *
;*                                                                                                *
;* Hardware map is flexible via Glue logic                                                        *
;*  - 5 I/O selects @ 32-bytes wide             $FE00 - $FE9F                                     *
;*  - 1 I/O select used by SC28L92 DUART        $FE80 - $FE9F                                     *
;*  - 4 I/O selects available on expansion bus  $FE00 - $FE7F                                     *
;*                                                                                                *
;* Additional Hardware support:                                                                   *
;*  - MicroDrive IDE PATA interface                                                               *
;*    - 16-bit upper latch for data read/write                                                    *
;*  - DS15x1W Realtime Clock/Calendar                                                             *
;*                                                                                                *
;* BIOS Functions are divided into groups as follows:                                             *
;*                                                                                                *
;* SC28L92 DUART functions:                                                                       *
;* - Full duplex interrupt-driven/buffered I/O for both DUART Channels                            *
;* - Precision timer services with 10ms accuracy                                                  *
;* - RTC based Jiffy Clock, Sec, Min, Hour, Date                                                  *
;* - Accurate delays from 10ms to ~46 hours                                                       *
;* - 10ms Benchmark Timing to 65535.99 seconds                                                    *
;*                                                                                                *
;* IDE Controller Functions supporting PATA 16-bit Data Transfers:                                *
;* - Reset IDE (recalibrate command)                                                              *
;* - Get IDE Status and Extended Error codes                                                      *
;* - Get IDE Identification Block                                                                 *
;* - Read a Block from IDE device                                                                 *
;* - Write a Block to IDE device                                                                  *
;* - Verify the last Block from IDE device                                                        *
;* - Set the LBA Block ID for Read/Write/Verify                                                   *
;* - Set the Memory Address to transfer Block data to/from                                        *
;* - Enable the Write Cache on IDE controller                                                     *
;* - Disable the Write Cache on IDE controller                                                    *
;*                                                                                                *
;* Maxim Realtime Clock functions:                                                                *
;* - Detect RTC (signature in NVRAM) and Load software RTC variables                              *
;* - Read and Write NVRAM block                                                                   *
;*                                                                                                *
;* BIOS Features:                                                                                 *
;* - Extendable BIOS structure with soft vectors                                                  *
;* - Soft config parameters for I/O devices                                                       *
;* - Monitor cold/warm start soft vectored                                                        *
;* - Panic Routine to restore Vectors and Reinitialize Console                                    *
;* - Fully relocatable code (sans page $FF)                                                       *
;* - JUMP Table at $FF00 - 32 functions                                                           *
;* - Default memory allocation of 2KB (includes I/O mapping)                                      *
;**************************************************************************************************
        PL      66      ;Page Length
        PW      132     ;Page Width (# of char/line)
        CHIP    W65C02S ;Enable WDC 65C02 instructions
        PASS1   OFF     ;Set ON when used for debug
        INCLIST ON      ;Set ON for listing Include files
;**************************************************************************************************
;
; C02BIOS Version 4.0x is based on C02BIOS Version 3.04.
;
; - Main changes are to support the NXP SC28L92 DUART.
; - Minor changes to Page Zero to include required pointers, etc. for the second serial port.
;
;**************************************************************************************************
;
; This BIOS and Monitor version also use a common source file for constants and variables used by
; both. This just simplifies keeping both code pieces in sync.
;
        INCLUDE         C02Constants4.asm
;
;**************************************************************************************************
;       - Monitor JUMP table: 32 JUMP calls are Defined, with one Call (02) currently Reserved.
;
M_COLD_MON      .EQU    $E000           ;Call 00        Monitor Cold Start
M_WARM_MON      .EQU    $E003           ;Call 01        Monitor Warm Start
;
M_RESERVE2      .EQU    $E006           ;Call 02        Reserved
;
M_MOVE_RAM      .EQU    $E009           ;Call 03        Move Memory
M_FILL_LP       .EQU    $E00C           ;Call 04        Fill Memory
M_BSOUT         .EQU    $E00F           ;Call 05        Send Backspace
M_XMDM_SAVE     .EQU    $E012           ;Call 06        Xmodem Save Entry
M_XMDM_LOAD     .EQU    $E015           ;Call 07        Xmodem Load Entry
M_BENCH         .EQU    $E018           ;Call 08        Benchmark Start
M_QUITB         .EQU    $E01B           ;Call 09        Benchmark Stop/End
M_TIME          .EQU    $E01E           ;Call 10        System Date/Time
M_PRSTAT1       .EQU    $E021           ;Call 11        CPU Status Display
M_DIS_LINE      .EQU    $E024           ;Call 12        Disassemble Line of Code
M_INCINDEX      .EQU    $E027           ;Call 13        Increment Index by 1
M_DECINDEX      .EQU    $E02A           ;Call 14        Decrement Index by 1
M_RDLINE        .EQU    $E02D           ;Call 15        Read Line from Terminal
M_RDCHAR        .EQU    $E030           ;Call 16        Read Character from Terminal
M_HEXIN2        .EQU    $E033           ;Call 17        Hex input 2 characters
M_HEXIN4        .EQU    $E036           ;Call 18        Hex input 4 characters
M_HEX2ASC       .EQU    $E039           ;Call 19        Convert Hex to ASCII
M_BIN2ASC       .EQU    $E03C           ;Call 20        Convert Binary to ASCII
M_ASC2BIN       .EQU    $E03F           ;Call 21        Convert ASCII to Binary
M_BEEP          .EQU    $E042           ;Call 22        Send BEEP to Terminal
M_DOLLAR        .EQU    $E045           ;Call 23        Send $ to Terminal
M_CROUT         .EQU    $E048           ;Call 24        Send C/R to Terminal
M_SPC           .EQU    $E04B           ;Call 25        Send ASCII Space to Terminal
M_PRBYTE        .EQU    $E04E           ;Call 26        Print Byte to Terminal
M_PRWORD        .EQU    $E051           ;Call 27        Print Word to Terminal
M_PRASC         .EQU    $E054           ;Call 28        Print ASCII to Terminal
M_PROMPT        .EQU    $E057           ;Call 29        Send Message by number to Terminal
M_PROMPTR       .EQU    $E05A           ;Call 30        Send Message by address to Terminal
M_CONTINUE      .EQU    $E05D           ;Call 31        Y/N Prompt to Continue Command
;
;**************************************************************************************************
        .ORG    $F800   ;2KB reserved for BIOS, I/O device selects (160 bytes)                    *
;**************************************************************************************************
;                               START OF BIOS CODE                                                *
;**************************************************************************************************
;C02BIOS version used here is 4.01 (updated release)
; Contains the base BIOS routines in top 2KB of EEPROM
; - Input/Feedback from "BDD" - modified CHRIN/CHROUT I/O routines - saves 12 bytes
; - $F800 - $F9FF 512 bytes for BIOS SC28L92, NMI Panic routine
; - $FA00 - $FDFF reserved for BIOS expansion (1KB)
; - $FE00 - $FE7F reserved for HW (4-I/O selects, 32 bytes wide)
; - $FE80 - $FE9F SC28L92 DUART (32 bytes wide, only 16 bytes used)
; - $FEA0 - $FEFF used for Vector and Hardware configuration data
; - $FF00 - $FFFF JMP table, CPU startup, NMI/BRK/IRQ pre-post routines, Page $03 init, BIOS msg
;
; UPDATES:
; Note: C02BIOS 3.04 is the base for 4.0x.
;
; Fixed IRQ enable for IDE controller. Also streamlined some startup routines. 11th Oct 2020
; Eliminates IDE byte swapping to ensure compatibility with standard formats. 20th January 2021
; Allocate $0400-$04FF for second UART buffer space. 5th February 2021
; Default IDE Block buffer at $0600-$07FF. 5th February 2021
; Default RTC NVRAM buffer space at $0500-$05FF. 5th February 2021
; Panic Routine changed, no longer saves multiple pages, just restores system. 5th February 2021
; New routine to Start Benchmark Counter. Allows counter Start/Stop (pause). 5th February 2021
; Some minor cleanup and fixed a few bugs in the RTC and IDE routines. 14th May 2021
; Update Init and ISR routines to support NXP SC28L92 DUART, both ports and timer. 31st August 2021
; Update Character I/O routines for Serial Ports. Supports on-chip FIFOs Rcv/Xmt. 14th October 2021
; Updates to support IDE as Hitachi MicroDrive (3.3V prototype) 9th August 2021
; Bios 4.00 and later now support multiple block transfers to/from IDE device 13th September 2021
; - note: Removed Extended Delay routine, not ever used except for testing.
;
; Note: C02BIOS 4.01 Updates 17th October, 2021
;
; Streamlined IDE Detection, Initialize and Reset routines.
; - Recalibrate Command removed from Detection, Diagnostics only are run.
; - Likewise, Diagnositcs removed from Reset command, Recalibrate only is done.
; - Removed detection for LBA support, as the Hitachi 3K8 Microdrive supports this natively.
;
; Added support for Receive/Transmit FIFOs for SC28L92 DUART.
;
;**************************************************************************************************
; The following 32 functions are provided by BIOS via the JMP Table
; $FF27 - $FF2A are Reserved for future expansion (1 available)
;
; $FF00 IDE_RESET       ;Reset IDE Controller (Recalibrate Command)
; $FF03 IDE_GET_STAT    ;Get Status and Error code
; $FF06 IDE_IDENTIFY    ;Load IDE Identity Data at $0600
; $FF09 IDE_READ_LBA    ;Read LBA into memory
; $FF0C IDE_WRITE_LBA   ;Write LBA from memory
; $FF0F IDE_VERFY_LBA   ;Verify LBA from last Read/Write
; $FF12 IDE_SET_LBA     ;Set LBA number (24-bit support only)
; $FF15 IDE_SET_ADDR    ;Set LBA transfer address (16-bit plus block count)
; $FF18 IDE_EN_CACHE    ;Enable IDE Write Cache
; $FF1B DIS_CACHE JMP   ;Disable IDE Write Cache
;
; $FF1E RTC_NVRD        ;Read NVRAM (256 bytes) from RTC to memory (16-bit)
; $FF21 RTC_NVWR        ;Write NVRAM (256 bytes) from memory to RTC (16-bit)
; $FF24 RTC_INIT        ;Initialize software RTC from hardware RTC
;
; $FF27 CHRIN2          ;Data input from aux port
; $FF2A CHROUT2         ;Data output to aux port
;
; $FF2D CNT_INIT        ;Reset Benchmark timing counters/Start 10ms benchmark timer
; $FF30 CNT_STRT        ;Start 10ms benchmark timing counter
; $FF33 CNT_STOP        ;Stop 10ms benchmark timing counter
;
; $FF36 CHRIN_NW        ;Data input from console, no waiting, clear carry if none
; $FF39 CHRIN           ;Data input from console
; $FF3C CHROUT          ;Data output to console
;
; $FF3F SET_DLY         ;Set delay value for milliseconds and 16-bit counter
; $FF42 EXE_MSDLY       ;Execute millisecond delay 1-256 * 10 milliseconds
; $FF45 EXE_LGDLY       ;Execute long delay; millisecond delay * 16-bit count

; $FF48 Reserved        ;Reserved for future expansion
;
; $FF4B INIT_VEC        ;Initialize soft vectors at $0300 from ROM
; $FF4E INIT_CFG        ;Initialize soft config values at $0320 from ROM
;
; $FF51 INIT_28L92      ;Initialize SC28L92 console 38.4K, 8-N-1 RTS/CTS
; $FF54 RESET_28L92     ;Reset SC28L92 - called before INIT_28L92
;
; $FF57 MONWARM         ;Monitor warm start - jumps to page $03
; $FF5A MONCOLD         ;Monitor cold start - jumps to page $03
; $FF5D COLDSTRT        ;System cold start - RESET vector for 65C02
;**************************************************************************************************
;               Data In and Out routines for Console I/O buffer                                   *
;**************************************************************************************************
;Data Input A routines
;CHRIN_NW uses CHRIN, returns if data is not available from the buffer with carry flag clear,
; else returns with data in A Reg and carry flag set. CHRIN waits for data to be in the buffer,
; then returns with carry flag set. Receive is IRQ driven/buffered with a size of 128 bytes.
; Note: CHRIN_NW is only on Port A, which is used for a Console.
;
CHRIN_NW        CLC                     ;Clear Carry flag for no data (2)
                LDA     ICNT_A          ;Get buffer count (4)
                BNE     GET_CH          ;Branch if buffer is not empty (2/3)
                RTS                     ;Or return to caller (6)
;
CHRIN           LDA     ICNT_A          ;Get data count (3)
                BEQ     CHRIN           ;If zero (no data, loop back) (2/3)
;
GET_CH          PHY                     ;Save Y Reg (3)
                LDY     IHEAD_A         ;Get the buffer head pointer (3)
                LDA     IBUF_A,Y        ;Get the data from the buffer (4)
                INC     IHEAD_A         ;Increment head pointer (5)
                RMB7    IHEAD_A         ;Strip off bit 7, 128 bytes only (5)
                DEC     ICNT_A          ;Decrement the buffer count (5)
;
                PLY                     ;Restore Y Reg (4)
                SEC                     ;Set Carry flag for data available (2)
                RTS                     ;Return to caller with data in A Reg (6)
;
;Data Output A routine: puts the data in the A Reg into the xmit buffer, data in
; A Reg is preserved on exit. Transmit is IRQ driven/buffered with a size of 128 bytes.
;
CHROUT          PHY                     ;Save Y Reg (3)
OUTCH           LDY     OCNT_A          ;Get data output count in buffer (3)
                BMI     OUTCH           ;Check buffer full, if yes, check Xmit on (2/3)
;
                LDY     OTAIL_A         ;Get the buffer tail pointer (3)
                STA     OBUF_A,Y        ;Place data in the buffer (5)
                INC     OTAIL_A         ;Increment Tail pointer (5)
                RMB7    OTAIL_A         ;Strip off bit 7, 128 bytes only (5)
                INC     OCNT_A          ;Increment data count (5)
;
                LDY     #%00000100      ;Get mask for xmit on (2)
                STY     UART_COMMAND_A  ;Turn on xmit (4)
;
                PLY                     ;Restore Y Reg (4)
                RTS                     ;Return to caller (6)
;
;Data Input B routine
; CHRIN waits for data to be in the buffer, then returns with data in A reg.
; Receive is IRQ driven/buffered with a size of 128 bytes.
;
CHRIN2          LDA     ICNT_B          ;Get data count (3)
                BEQ     CHRIN2          ;If zero (no data, loop back) (2/3)
;
                PHY                     ;Save Y Reg (3)
                LDY     IHEAD_A         ;Get the buffer head pointer (3)
                LDA     IBUF_B,Y        ;Get the data from the buffer (4)
                INC     IHEAD_B         ;Increment head pointer (5)
                RMB7    IHEAD_B         ;Strip off bit 7, 128 bytes only (5)
                DEC     ICNT_B          ;Decrement the buffer count (5)
;
                PLY                     ;Restore Y Reg (4)
                RTS                     ;Return to caller with data in A Reg (6)
;
;Data Output B routine: puts the data in the A Reg into the xmit buffer, data in
; A Reg is preserved on exit. Transmit is IRQ driven/buffered with a size of 128 bytes.
;
CHROUT2         PHY                     ;Save Y Reg (3)
OUTCH2          LDY     OCNT_B          ;Get data output count in buffer (3)
                BMI     OUTCH2          ;Check against limit, loop back if full (2/3)
;
                LDY     OTAIL_B         ;Get the buffer tail pointer (3)
                STA     OBUF_B,Y        ;Place data in the buffer (5)
                INC     OTAIL_B         ;Increment Tail pointer (5)
                RMB7    OTAIL_B         ;Strip off bit 7, 128 bytes only (5)
                INC     OCNT_B          ;Increment data count (5)
;
                LDY     #%00000100      ;Get mask for xmit on (2)
                STY     UART_COMMAND_B  ;Turn on xmit (4)
;
SKP_XMTB        PLY                     ;Restore Y Reg (4)
                RTS                     ;Return to caller (6)
;
;**************************************************************************************************
;Delay Routines: SET_DLY sets up the MSDELAY value and also sets the 16-bit Long Delay
; On entry, A Reg = 10-millisecond count, X Reg = High multiplier, Y Reg = Low multiplier
; these values are used by the EXE_MSDLY and EXE_LGDLY routines. Minimum delay is 10ms
; values for MSDELAY are $00-$FF ($00 = 256 times)
; values for Long Delay are $0000-$FFFF (0-65535 times MSDELAY)
; longest delay is 65,535*256*10ms = 16,776,960 * 0.01 = 167,769.60 seconds
;
;NOTE: All delay execution routines preserve registers (EXE_MSDLY, EXE_LGDLY, EXE_XLDLY)
;
SET_DLY         STA     SETMS           ;Save Millisecond count
                STY     DELLO           ;Save Low multiplier
                STX     DELHI           ;Save High multiplier
                RTS                     ;Return to caller
;
;EXE MSDELAY routine is the core delay routine. It sets the MSDELAY count value from the
; SETMS variable, enables the MATCH flag, then waits for the MATCH flag to clear.
;
EXE_MSDLY       PHA                     ;Save A Reg
                SMB7    MATCH           ;Set MATCH flag bit
                LDA     SETMS           ;Get delay seed value
                STA     MSDELAY         ;Set MS delay value
;
MATCH_LP        BBS7    MATCH,MATCH_LP  ;Test MATCH flag, loop until cleared
                PLA                     ;Restore A Reg
                RTS                     ;Return to caller
;
;EXE LONG Delay routine is the 16-bit multiplier for the MSDELAY routine.
; It loads the 16-bit count from DELLO/DELHI, then loops the MSDELAY routine
; until the 16-bit count is decremented to zero.
;
EXE_LGDLY       PHX                     ;Save X Reg
                PHY                     ;Save Y Reg
                LDX     DELHI           ;Get high byte count
                INX                     ;Increment by one (checks for $00 vs $FF)
                LDY     DELLO           ;Get low byte count
                BEQ     SKP_DLL         ;If zero, skip to high count
DO_DLL          JSR     EXE_MSDLY       ;Call millisecond delay
                DEY                     ;Decrement low count
                BNE     DO_DLL          ;Branch back until done
;
SKP_DLL         DEX                     ;Decrement high byte index
                BNE     DO_DLL          ;Loop back to D0_DLL (will run 256 times)
                PLY                     ;Restore Y Reg
                PLX                     ;Restore X Reg
                RTS                     ;Return to caller
;
;**************************************************************************************************
;COUNTER BENCHMARK TIMING ROUTINES
; To enable a level of benchmarking, three new routines have been added to C02BIOS version 2.05
; Using the existing 10ms Jiffy Clock, three bytes of Page zero are used to hold the variables;
; MS10_CNT - a 10ms count variable for 0.01 resolution of timing - resets at 100 counts (1 second)
; SECL_CNT - a low byte seconds count
; SECH_CNT - a high byte seconds count
; This provides up to 65,535.99 seconds of timing with 0.01 seconds resolution
; - the count variables reset to zero after 65,535.99 seconds!
;
; New routines in C02Monitor version 2.05:
; - Start counter (calls CNT_STRT) after prompting user
; - Stop counter (calls CNT_STOP) and displays the timing count as: XXXXX.XX Seconds.
;
; CNT_INIT has been added as a new BIOS call, effectively replacing CNT_STRT, as it does
; the same thing. CNT_STRT has been changed to only set the MATCH flag to restart the timing.
; This allows the Benchmark Timer to effectively be paused and resumed without resetting the
; the count variables. CNT_INIT has also been added to the BIOS JUMP table.
;
;CNT_INIT is used to zero the timing pointers and start the benchmark timing
;CNT_STRT is used to start the timing by setting bit 6 of the MATCH flag (new in BIOS 3.03)
;CNT_STOP is used to stop the timing by clearing bit 6 of the MATCH flag
; the interrupt handler for the DUART timer increments the timing variables when bit 6 of the
; MATCH flag is active.
;
CNT_INIT        RMB6    MATCH           ;Clear bit 6 of MATCH flag, ensure timing is disabled
                STZ     MS10_CNT        ;Zero 10ms timing count
                STZ     SECL_CNT        ;Zero low byte of seconds timing count
                STZ     SECH_CNT        ;Zero high byte of seconds timing count
;
CNT_STRT        SMB6    MATCH           ;Set bit 6 of MATCH flag to enable timing
                RTS                     ;Return to caller
;
CNT_STOP        RMB6    MATCH           ;Clear bit 6 of MATCH flag to disable timing
                RTS                     ;Return to caller
;
;**************************************************************************************************
;START of IDE Routines for Hitachi 3K8 MicroDrive                                                 *
;**************************************************************************************************
;
IDE_READ_LBA                            ;Read a Block of data from IDE device
;
; This routine requires loading the requested LBA into the appropriate registers and
; issuing the READ command 20h. The LBA count supported for the BIOS are bits 0-23,
; so bits 24-27 are always set to 0. This provides access to IDE devices up to 8GB.
;
; Once the registers/parameters are setup, the Read Block command is issued.
; This results in an interrupt being generated. The ISR handles the transfer of LBA
; data from the IDE Drive to memory.
;
; BIOS 4.00 and later supports multiple block transfers, increasing performance.
;
; The registers used are the same for read/write/verify. These are:
;
;       IDE_COMMAND = function requested (20h = READ LBA command)
;       IDE_DRV_HEAD = (Upper 4 bits) used as:
;               bit 7 = 1 per Seagate documentation
;               bit 6 = 1 for LBA mode
;               bit 5 = 1 per Seagate documentation
;               bit 4 = 0 for Drive 0
;       IDE_DRV_HEAD = LBA Address bits 27-24 (lower 4 bits) - not used, always 0000
;       IDE_CYL_HIGH = LBA Address bits 23-16
;       IDE_CYL_LOW = LBA Address bits 15-8
;       IDE_SCT_NUM = LBA Address bits 7-0
;       IDE_SCT_CNT = number of blocks to read
;
                JSR     IDE_SET_PARMS   ;Setup required parameters (6)
                LDA     #$20            ;Get Read LBA command (2)
IDENT_READ                              ;Identity Command jumps to here to complete
                SMB3    MATCH           ;Set Read LBA bit (5)
                STA     IDE_COMMAND     ;Send command to IDE Controller (4)
;
LBA_RD_CMD
                LDA     IDE_ALT_STATUS  ;Get IDE Alternate Status register (4)
                BMI     LBA_RD_CMD      ;Loop until IDE controller not Busy (2/3)
;
LBA_RD_WAIT
                LDA     IDE_ALT_STATUS  ;Get IDE Alternate Status register (4)
                CMP     #$50            ;Compare for ready (2)
                BNE     LBA_RD_ERR      ;If not, check for error condition (2/3)
LBA_RD_OK
                BBS3    MATCH,LBA_RD_OK ;Wait for Read completed via ISR (5)
                RTS                     ;Return to caller (status in A Reg) (6)
LBA_RD_ERR
                LSR     A               ;Shift error bit to carry (2)
                BCC     LBA_RD_WAIT     ;If clear, loop back and continue waiting (2/3)
;
                RMB3    MATCH           ;Reset Read LBA bit (no ISR invoked) (5)
IDE_RWV_FIN
                LDA     IDE_ALT_STATUS  ;Get IDE Alternate Status register (4)
                STA     IDE_STATUS_RAM  ;Update RAM Status Register (3)
                RTS                     ;Return to caller (6)
;
IDE_WRITE_LBA                           ;Write a block of data to LBA
;
; This routine requires loading the requested LBA into the appropriate registers and
; issuing the WRITE command 30h. The LBA count supported for the BIOS are bits 0-23,
; so bits 24-27 are always set to 0. This provides access to IDE devices up to 8GB.
;
; The registers used are the same for read/write/verify. These are:
;
;       IDE_COMMAND = function requested (30h = WRITE LBA command)
;       IDE_DRV_HEAD = (Upper 4 bits) used as:
;               bit 7 = 1 per Seagate documentation
;               bit 6 = 1 for LBA mode
;               bit 5 = 1 per Seagate documentation
;               bit 4 = 0 for Drive 0
;       IDE_DRV_HEAD = LBA Address bits 27-24 (lower 4 bits) - not used, always 0000
;       IDE_CYL_HIGH = LBA Address bits 23-16
;       IDE_CYL_LOW = LBA Address bits 15-8
;       IDE_SCT_NUM = LBA Address bits 7-0
;       IDE_SCT_CNT = number of blocks to write
;
                JSR     IDE_SET_PARMS   ;Setup required parameters (6)
;
                SMB2    MATCH           ;Set Write LBA bit (5)
                LDA     #$30            ;Get Write LBA command (2)
                STA     IDE_COMMAND     ;Send command to IDE Controller (4)
LBA_WR_CMD
                LDA     IDE_ALT_STATUS  ;Get IDE Alternate Status register (4)
                BMI     LBA_WR_CMD      ;Loop until IDE controller not Busy (2/3)
                LSR     A               ;Shift Error bit into Carry flag (2)
                BCS     IDE_WRITE_ERR   ;If Carry set, IDE error (2/3)
;
; Write Block routine integrated into IDE_WRITE_LBA
;
; - High byte needs to be loaded into the latch before the
;   low byte is loaded into the Data Register!
;
IDE_WRITE_BLK                           ;Write a block of data
                PHY                     ;Save Y reg
                LDY     #$01            ;Set offset for high byte latch
;
IDE_WRITE_LOOP
                LDA     IDE_ALT_STATUS  ;Get IDE Alternate Status register
                AND     #%00001000      ;Check for DRQ active
                BEQ     IDE_WR_FIN      ;If not active, exit (below)
IDE_WR_WBLK
                LDA     (BIOS_XFERL),Y  ;Get first byte of buffer+1
                STA     IDE_16_WRITE    ;Place into high byte latch
                LDA     (BIOS_XFERL)    ;Get first byte of buffer
                STA     IDE_DATA        ;Write buffer to IDE (writes a word)
;
; - Buffer index needs to be incremented twice
;
                INC     BIOS_XFERL      ;Increment pointers once
                BNE     IDE_WR_BLK1
                INC     BIOS_XFERH
IDE_WR_BLK1
                INC     BIOS_XFERL      ;Increment pointers again
                BNE     IDE_WRITE_LOOP
                INC     BIOS_XFERH
                BRA     IDE_WRITE_LOOP  ;Loop back for 256 words
;
IDE_WR_FIN
; when DRQ ends, 512 bytes have been sent to IDE controller. Controller then sets BUSY,
; when finished processing data, controller clears BUSY and generates an interrupt.
; So, we test for BUSY first and wait until the block is written.
;
                LDA     IDE_ALT_STATUS  ;Get IDE Alternate Status register (4)
                BMI     IDE_WR_FIN      ;Loop until BUSY bit is clear (2/3)
;
                DEC     BIOS_XFERC      ;Decrement Block Count to transfer (5)
                BNE     IDE_WRITE_LOOP  ;If not zero, branch around Match bit reset (2/3)
;
WR_WAIT
                BBS2    MATCH,WR_WAIT   ;Wait for Write completed via ISR (5)
                PLY                     ;Restore Y reg (3)
                RTS                     ;Return to caller (6)
IDE_WRITE_ERR
                RMB2    MATCH           ;Reset Write LBA bit (no ISR) (5)
                BRA     IDE_RWV_FIN     ;Branch and finish up
;
IDE_VERIFY_LBA                          ;Verify LBA after write
;
; This routine requires loading the requested LBA into the appropriate registers and
; issuing the VERIFY command 40h. The LBA count supported for the BIOS are bits 0-23,
; so bits 24-27 are always set to 0. This provides access to IDE devices up to 8GB.
;
; It is identical to a Read Block command except no Data is transferred, only verified.
; Also, no DRQ is set, but an interrupt is generated after the Verify is completed.
;
; The registers used are the same for read/write/verify. These are:
;
;       IDE_COMMAND = function requested (40h = Verify LBA command)
;       IDE_DRV_HEAD = (Upper 4 bits) used as:
;               bit 7 = 1 per Seagate documentation
;               bit 6 = 1 for LBA mode
;               bit 5 = 1 per Seagate documentation
;               bit 4 = 0 for Drive 0
;       IDE_DRV_HEAD = LBA Address bits 27-24 (lower 4 bits) - not used, always 0000
;       IDE_CYL_HIGH = LBA Address bits 23-16
;       IDE_CYL_LOW = LBA Address bits 15-8
;       IDE_SCT_NUM = LBA Address bits 7-0
;       IDE_SCT_CNT = number of blocks to verify
;
                JSR     IDE_SET_PARMS   ;Setup required parameters
;
                SMB1    MATCH           ;Set Verify LBA bit
                LDA     #$40            ;Get Verify LBA command
                STA     IDE_COMMAND     ;Send command to IDE Controller
LBA_VF_CMD
                LDA     IDE_ALT_STATUS  ;Get IDE Alternate Status register
                BMI     LBA_VF_CMD      ;Loop until IDE controller not Busy
                LSR     A               ;Shift Error bit into Carry flag
                BCC     VF_FINISH       ;If Carry clear, no error
;
                RMB1    MATCH           ;Reset Verify LBA bit (no ISR)
                BRA     IDE_RWV_FIN     ;Branch and finish up
;
VF_FINISH
                JSR     TST_IDE_RDY     ;Wait for IDE Controller ready
VF_WAIT
                BBS1    MATCH,VF_WAIT   ;Wait for Verify to complete via ISR
                RTS                     ;Return to caller
;
IDE_SET_ADDRESS                         ;Set Address for LBA (read/write)
;
; This routine uses the A,Y,X registers to setup the address in memory that a block
; will be read to or written from (16-bit address), along with the block count.
; The Register usage is as follows:
;       A Register = Memory address low byte
;       Y Register = Memory address high byte
;       X Register = Block count to transfer (device dependent)
                STA     LBA_ADDR_LOW    ;Set LBA low byte address (3)
                STY     LBA_ADDR_HIGH   ;Set LBA high byte address (3)
                STX     LBA_XFER_CNT    ;Set LBA Block count for xfer (3)
                RTS                     ;Return to caller (6)
;
IDE_SET_LBA                             ;Set LBA block for transfer (read/write)
;
; This routine sets the variables used to select the starting LBA for transfer.
; The Register usage is as follows:
;       A Register = LBA Address bits 7-0
;       Y Register = LBA Address bits 15-8
;       X Register = LBA Address bits 23-16
                STA     LBA_LOW_BYTE    ;Store Address bits 0-7 (3)
                STY     LBA_HIGH_BYTE   ;Store Address bits 8-15 (3)
                STX     LBA_EXT_BYTE    ;Store Address bits 16-23 (3)
                RTS                     ;Return to caller (6)
;
; This routine sets the LBA number used for all transfers.
; - The IDE Controller is checked first to ensure it's ready to receive parameters
; - then the requested LBA (stored in Page Zero variables) are loaded into the
; - IDE Controller registers, followed by the required Mode parameters.
; - Last, the transfer address is setup which points to the location in memory that
; - will be used to transfer Data to or from.
;
IDE_SET_PARMS                           ;Set All parameters for LBA transfers
;
                LDA     IDE_ALT_STATUS  ;Get IDE Alternate Status register (4)
                BMI     IDE_SET_PARMS   ;Loop until BUSY bit is clear (2/3)
;
;                PHX                     ;Save X register (2)
;                LDX     #$04            ;Set Index count for 4 (2)
;PARM_XFER_LP
;                LDA     LBA_XFER_CNT-1,X        ;Get the parameters (4)
;                STA     IDE_SCT_CNT-1,X ;Store to IDE registers (5)
;                DEX                     ;Decrement count (2)
;                BNE     PARM_XFER_LP    ;Loop back till done (2/3)
;                PLX                     ;Restore X register (3)
;
                LDA     LBA_EXT_BYTE    ;Set LBA bits 23-16 (3)
                STA     IDE_CYL_HIGH    ;Send to IDE (4)
                LDA     LBA_HIGH_BYTE   ;Set LBA bits 15-8 (3)
                STA     IDE_CYL_LOW     ;Send to IDE (4)
                LDA     LBA_LOW_BYTE    ;Get LBA bits 7-0 (3)
                STA     IDE_SCT_NUM     ;Send to IDE (4)
                LDA     LBA_XFER_CNT    ;Get Block count to read (3)
                STA     IDE_SCT_CNT     ;Send to IDE (4)
;
IDE_SET_PARMS2                          ;Set partial parameters (non LBA xfer commands)
;
                LDA     #%11100000      ;Set Drive 0, LBA mode, LBA bits 27-24 as 0 (2)
                STA     IDE_DRV_HEAD    ;Send to IDE controller (4)
;
                LDA     LBA_ADDR_LOW    ;Setup buffer address (3)
                STA     BIOS_XFERL      ;Store low byte (3)
                LDA     LBA_ADDR_HIGH   ;Block Buffer Address (3)
                STA     BIOS_XFERH      ;Store high byte (3)
                LDA     LBA_XFER_CNT    ;Get Block count to read (3)
                STA     BIOS_XFERC      ;Set BIOS Block count to Xfer (3)
                STZ     IDE_STATUS_RAM  ;Clear RAM Status Register, ISR updates it (3)
                RTS                     ;Return to caller (6)
;
;Test for IDE Controller Ready
; This routine tests that the IDE Controller is ready and can accept a command for execution.
; There are two bits in the status to qualify this:
; Bit 6 is for Ready and bit 4 is for Seek Complete. Both should be active to qualify the
; drive as being ready (per Hitachi Microdrive documentation).
; Note: It's also possible that bit 0 might be set, which indicates an error condition.
; If an error has occurred, we should test for this as well, and set the carry flag to
; ensure proper drive sensing.
;
;
TST_IDE_RDY
                LDA     IDE_ALT_STATUS  ;Get IDE Alternate Status register (4)
                CMP     #$51            ;Test for Ready and Error bits on (2)
                BEQ     TST_IDE_ERR     ;If yes, branch to exit (2/3)
                CMP     #$50            ;Test for Ready bits on (2)
                BNE     TST_IDE_RDY     ;If not, loop back until ready (2/3)
                CLC                     ;Safety to ensure Carry is not set (2)
                RTS                     ;Return to Caller (6)
TST_IDE_ERR     SEC                     ;Set Carry to indicate Error bit set (2)
                RTS                     ;Return to Caller (6)
;
;Test for IDE Controller Busy
; This routine loops on the Busy flag. If the IDE Controller is busy, no other
; status register flags are valid and no commands can be sent to the IDE Controller.
; Hence, this routine is key to determine if the IDE Controller is available to
; accept a command. Bit 7 is the Busy Bit. The 65C02 will set the "n" flag
; if Bit 7 is active, else clear it.
;
TST_IDE_BUSY
                LDA     IDE_ALT_STATUS  ;Get IDE Alternate Status register (4)
                BMI     TST_IDE_BUSY    ;Loop until BUSY bit is clear (2/3)
                RTS                     ;Return to Caller (6)
;
;**************************************************************************************************
;START of Realtime Clock Routines to access NVRAM                                                 *
;**************************************************************************************************
;
;RTC Routines to Read and Write Non-Volatile RAM - 256 Bytes
; To use either routine, the starting memory address needs to be
; loaded into the A/Y registers. A reg = low byte, Y reg = high byte
;
RTC_NVRD        JSR     RTC_NVSET       ;Setup for transfer
RTC_RD_LOOP     LDA     RTC_RAM_DATA    ;Get NVRAM Data
                STA     (BIOS_XFERL),Y  ;Save into RAM
                INY                     ;Increment Y Index
                BNE     RTC_RD_LOOP     ;Loop back reading NVRAM
                BRA     CLEAR_BMI       ;Clear Burst Mode, exit
;
RTC_NVWR        JSR     RTC_NVSET       ;Setup for transfer
RTC_WR_LOOP     LDA     (BIOS_XFERL),Y  ;Get RAM
                STA     RTC_RAM_DATA    ;Write to NVRAM Data
                INY                     ;Increment Y Index
                BNE     RTC_WR_LOOP     ;Loop back writing NVRAM
CLEAR_BMI       LDA     #%00100000      ;Get Burst Mode Increment bit
                TRB     RTC_CONTROL_B   ;Clear Burst Mode for NVRAM
                RTS                     ;Return to caller
;
RTC_NVSET       STA     BIOS_XFERL      ;Save Low Address location
                STY     BIOS_XFERH      ;Save High Address location
                LDA     #%00100000      ;Get Burst Mode Increment bit
                TSB     RTC_CONTROL_B   ;Set Burst Mode for NVRAM
                STZ     RTC_RAM_ADDR    ;Set NVRAM Address to Zero
                LDY     #$00            ;Zero Y reg - Index
                RTS                     ;Return to caller
;
;**************************************************************************************************
;START OF PANIC ROUTINE
;The Panic routine is for debug of system problems, i.e., a crash. The hardware design requires a
; debounced NMI trigger button which is manually operated when the system crashes or malfunctions.
;
;User presses the NMI (panic) button. The NMI vectored routine will perform the following tasks:
; 1- Save CPU registers in page zero locations
; 2- Reset the MicroDrive and disable interrupts
; 3- Clear all Console I/O buffer pointers
; 4- Call the ROM routines to init the vectors and config data (page $03)
; 5- Call the ROM routines to reset/init the DUART (SC28L92)
; 6- Enter the Monitor via the warm start vector
;
; Note: The additional hardware detection (RTC/IDE) are NOT executed with the Panic routine!
; The interrupt vectors are restored without including the additional ISR for the IDE controller.
;
; Note: no memory is cleared except the required pointers/vectors to restore the system.
;
NMI_VECTOR      SEI                     ;Disable interrupts
                STA     AREG            ;Save A Reg
                STX     XREG            ;Save X Reg
                STY     YREG            ;Save Y Reg
                PLA                     ;Get Processor Status
                STA     PREG            ;Save in PROCESSOR STATUS preset/result
                TSX                     ;Get Stack pointer
                STX     SREG            ;Save STACK POINTER
                PLA                     ;Pull RETURN address from STACK
                STA     PCL             ;Store Low byte
                PLA                     ;Pull high byte
                STA     PCH             ;Store High byte
;
                LDA     #%00000110      ;Get mask for MicroDrive Reset/IRQ diable
                STA     IDE_DEV_CTRL    ;Send to MicroDrive
;
                STZ     UART_IMR        ;Disable ALL interrupts from UART
;
                LDX     #$0C            ;Set count for 12
PAN_LP1         STZ     ICNT_A-1,X      ;Clear DUART I/O pointers
                DEX                     ;Decrement index
                BNE     PAN_LP1         ;Branch back till done
;
                JSR     INIT_PG03       ;Xfer default Vectors/HW Config to $0300
;
                LDA     #%00000010      ;Get mask for MicroDrive Reset off
                STA     IDE_DEV_CTRL    ;Send to MicroDrive
;
                JSR     INIT_IO         ;Reset and Init the UART for Console
DO_NMI0         JMP     (NMIRTVEC0)     ;Jump to NMI Return Vector (Monitor Warm Start)
;
;**************************************************************************************************
;Initializing the SC28L92 DUART as a Console.
;An anomaly in the W65C02 processor requires a different approach in programming the SC28L92
; for proper setup/operation. The SC28L92 uses three Mode Registers which are accessed at the same
; register in sequence. There is a command that Resets the Mode Register pointer (to MR0) that is
; issued first. Then MR0/1/2 are loaded in sequence. The problem with the W65C02 is a false read of
; the register when using indexed addressing (i.e., STA UART_REGISTER,X). This results in the Mode
; Register pointer being moved to the next register, so the write to next MRx never happens. While
; the indexed list works fine for all other register functions/commands, the loading of the
; Mode Registers need to be handled separately.
;
;NOTE: the W65C02 will function normally "if" a page boundary is crossed as part of the STA
; (i.e., STA $FDFF,X) where the value of the X Register is high enough to cross the page boundary.
; Programming in this manner would be confusing and require modification if the base I/O address
; is changed for a different hardware I/O map.
;
;There are two routines called to setup the 28L92 DUART:
;
;The first routine is a RESET of the DUART.
; It issues the following sequence of commands:
;  1- Reset Break Change Interrupts
;  2- Reset Receivers
;  3- Reset Transmitters
;  4- Reset All errors
;
;The second routine initializes the 28L92 DUART for operation. It uses two tables of data; one for
; the register offset and the other for the register data. The table for register offsets is
; maintained in ROM. The table for register data is copied to page $03, making it soft data. If
; needed, operating parameters can be altered and the DUART re-initialized via the ROM routine.
;
; Note: A hardware reset will reset the SC28L92 and the default ROM config will be initialized.
; Also note that the Panic routine invoked by a NMI trigger will also reset the DUART to the
; default ROM config.
;
INIT_IO         JSR     RESET_28L92     ;Reset of SC28L92 DUART (both channels)
                LDA     #DF_TICKS       ;Get divider for jiffy clock (100x10ms = 1 second)
                STA     TICKS           ;Preload TICK count
;
;This routine sets the initial operating mode of the DUART
;
INIT_28L92      SEI                     ;Disable interrupts
;
                LDX     #INIT_DUART_E-INIT_DUART ;Get the Init byte count
28L92_INT       LDA     LOAD_28L92-1,X  ;Get Data for 28L92 Register
                LDY     INIT_OFFSET-1,X ;Get Offset for 28L92 Register
                STA     SC28L92_BASE,Y  ;Store Data to selected register
                DEX                     ;Decrement count
                BNE     28L92_INT       ;Loop back until all registers are loaded
;
; Mode Registers are NOT reset to MR0 by above INIT_28L92!
; The following resets the MR pointers for both channels, then sets the MR registers
; for each channel. Note: the MR index is incremented to the next location after the write.
; NOTE: These writes can NOT be done via indexed addressing modes!
;
                LDA     #%10110000      ;Get mask for MR0 Reset
                STA     UART_COMMAND_A  ;Reset Pointer for Port A
                STA     UART_COMMAND_B  ;Reset Pointer for Port B
;
                LDX     #$03            ;Set index for 3 bytes to xfer
MR_LD_LP        LDA     LOAD_28L92+15,X ;Get MR data for Port A
                STA     UART_MODEREG_A  ;Send to 28L92 Port A
                LDA     LOAD_28L92+18,X ;Get MR data for Port B
                STA     UART_MODEREG_B  ;Send to 28L92 Port B
                DEX                     ;Decrement index to next data
                BNE     MR_LD_LP        ;Branch back till done
;
                CLI                     ;Enable interrupts
;
; Start Counter/Timer
;
                LDA     UART_START_CNT  ;Read register to start counter/timer
                RTS                     ;Return to caller
;
;This routine does a Reset of the SC28L92
;
RESET_28L92     LDX     #UART_RDATAE-UART_RDATA1 ;Get the Reset commands byte count
UART_RES1       LDA     UART_RDATA1-1,X ;Get Reset commands
                STA     UART_COMMAND_A  ;Send to UART A CR
                STA     UART_COMMAND_B  ;Send to UART B CR
                DEX                     ;Decrement the command list index
                BNE     UART_RES1       ;Loop back until all are sent
                RTS                     ;Return to caller
;
;**************************************************************************************************
;
;Core routines that are used to detect and configure additional I/O devices.
; The I/O adapter supported by the 4.0x Release of C02BIOS contains two I/O devices:
; - A Maxim DS15x1 Realtime Clock
; - An IDE Device, i.e., IBM/Hitachi MicroDrive
;
; The first routine is to detect the DS15x1 RTC. This is done by reading the NVRAM section
; and testing for the Signature at the end of NVRAM. Obviously, this is something that needs
; to be setup before it can be detected. There is a separate utility that is used to configure
; the RTC for Time/Date, NVRAM usage and the two-byte signature. This utility must be run first
; to properly configure the RTC. Once completed, the BIOS will detect the RTC and read the
; Date and Time to set the BIOS variables accordingly.
;
; The second routine is to detect the IDE device. This can be a bit tricky...
; The first step is to load the status register and check it's contents. If there is no device
; attached, the contents will be a "phantom" I/O address, which is $FE.
; If this happens, simply bypass any additional testing and setup.
; If the contents are other than the "phantom" I/O address, we take additional steps to sense
; the IDE Controller, initialize it, check for LBA support and add it's ISR into the chain.
;
;**************************************************************************************************
;
;This routine detects the DS15x1 RTC.
; While there's really no standard method to detect the RTC, the NVRAM is initialized
; during the configuration for Date, Time, etc. The last two bytes of the NVRAM will
; contain a 2-byte signature of "KM", in honor of the author ;-)
; If the signature is found, the INIT_RTC routine is called, which will setup the
; RTC variables in Page Zero, where the correct Date and Time will be maintained.
;
DETECT_RTC
                LDX     #$FE            ;Load NVRAM Offset
                STX     RTC_RAM_ADDR    ;Set index to start of signature
                LDA     RTC_RAM_DATA    ;Get NVRAM Data
                CMP     #"K"            ;Check for "K"
                BNE     SET_EPOCH       ;If not found, set EPOCH as default
                INX                     ;Increment Offset (X=$FF)
                STX     RTC_RAM_ADDR    ;Set index to start of signature
                LDA     RTC_RAM_DATA    ;Get NVRAM Data
                CMP     #"M"            ;Check for "M"
                BEQ     FOUND_RTC       ;If good, go Init RTC and exit
;
;If RTC not found, preload Date variables as EPOCH time (Thursday, 1st January, 1970)
; Note: Time variables default to zero from a cold start.
;
SET_EPOCH
                LDX     #$03            ;Get Index of 3
EPOCH_LP        LDA     EPOCH-1,X       ;Get Preload data
                STA     DAY_DATE-1,X    ;Store in RTC variables
                DEX                     ;Decrement Index
                BNE     EPOCH_LP        ;Loop back until done
                RTS                     ;Return, no RTC found
;
FOUND_RTC
                INX                     ;Increment Index ($FF -> $00)
FOUND_RTC_LP    LDA     RTC_MSG,X       ;Get BIOS init msg
                BEQ     INIT_RTC        ;If zero, msg done, go Init RTC
                JSR     CHROUT          ;Send to console
                INX                     ;Increment Index
                BRA     FOUND_RTC_LP    ;Loop back until done
;
;This routine reads the BCD registers from the DS15x1 RTC, converts BCD to Binary and loads the
; BIOS RTC variables. Note that some of the variables are shared to conserve Page Zero space.
; Note: Buffer area used for init is also used by C02 Monitor for hex input and code conversion,
; but is initialized as needed before usage, so we can clobber it here when configuring the BIOS.
;
;As there is a period of time required to get the data from the RTC, convert from BCD, then format
; into the Page Zero locations used, we need to manage the existing Timer/Counter status to ensure
; the best accuracy in time once the routine finishes. In short, the best we can manage accuracy
; is within one second, as the RTC internal update of the registers we read are halted.
;
;To help minimize the timing transfer, we first set the BIOS tick count to it's default value
; to ensure that the software RTC values are not updated during loading from the hardware RTC.
;
; The time to detect and load the current time/date is less than 5000 clock cycles. However, the
; overall accuracy can only be within one second. After the RTC values are transferred, the
; software RTC tick count will not have reached zero, as the entire timing is less than 1ms to
; to read the RTC, convert the BCD data and load the software clock variable in Page Zero. This
; results in software RTC timing starting a 1 second countdown from when the RTC is detected.
;
INIT_RTC
                LDX     #DF_TICKS       ;Get BIOS default Tick count (2)
                STX     TICKS           ;Reset the Tick count (3)
                LDX     #$00            ;Set X reg to zero for indexing below (2)
                LDA     #%10000000      ;Get TE Bit mask (2)
                TRB     RTC_CONTROL_B   ;Turn off TE Bit to disable update (6)
;
RTC_LOAD_LOOP
                LDA     RTC_SECONDS,X   ;Get Data from RTC (4)
                STA     BUFF_PG0,X      ;Store into RAM (4)
                INX                     ;Increment register count (2)
                CPX     #$08            ;Check for all 8 moved (2)
                BNE     RTC_LOAD_LOOP   :Loop back until all registers are read (2/3)
;
                LDA     #%10000000      ;Get TE Bit mask (2)
                TSB     RTC_CONTROL_B   ;Turn on TE Bit to enable update (6)
;
;Now convert BCD to Binary (X reg = $08)
;
BCD_LOOP        LDA     BUFF_PG0-1,X    ;Get BCD Data from Buffer (4)
;
                TAY                     ;Save BCD value (2)
                AND     #$F0            ;Mask for high nibble (2)
                LSR     A               ;Shift right (/2) (2)
                STA     RTC_TEMP        ;Save in temp (3)
                LSR     A               ;Shift over (/4) (2)
                LSR     A               ;Shift over (/8) -Carry is clear (2)
                ADC     RTC_TEMP        ;Add to temp (3)
                STA     RTC_TEMP        ;Store it back (3)
                TYA                     ;Get BCD back (2)
                AND     #$0F            ;Mask for low nibble (2)
                ADC     RTC_TEMP        ;Add temp (upper converted nibble) (3)
;
                STA     BUFF_PG0-1,X    ;Save Binary Data back to Buffer (4)
                DEX                     ;Decrement Index (2)
                BNE     BCD_LOOP        ;Loop back until done (2/3)
;
;Now take the binary data for the RTC, and format it for the BIOS RTC variables:
; Seconds, Minutes and Hours are fine as they are, so they simply move over.
; The Day (of the week) and the Date (of the Month) are combined into a single variable.
; Note: Day (of the week) is variable and decided by the user. In honor of my past, I'm
; declaring Saturday as the 1st day of the week, an IBM tradition.
; The Month is also kept in the upper 4 bits of the Century variable.
; Note: The Century and Year are two BCD digits each. As 4 BCD digits, these need to be
; converted to a 16-bit binary integer. Only 12-bits are needed, which frees the upper
; 4-bits to be used for the Month.
;
                LDA     BUFF_PG0+3      ;Get the Day count (3)
                ASL     A               ;Shift to upper 3 bits (2)
                ASL     A               ; (2)
                ASL     A               ; (2)
                ASL     A               ; (2)
                ASL     A               ; (2)
                ORA     BUFF_PG0+4      ;OR in the Date variable (3)
                STA     BUFF_PG0+3      ;Save to the Day variable (3)
;
                LDA     BUFF_PG0+5      ;Get the Month variable (3)
                ASL     A               ;Shift to upper 4 bits (2)
                ASL     A               ; (2)
                ASL     A               ; (2)
                ASL     A               ; (2)
                STA     BUFF_PG0+4      ;Save it (lower nibble = 0) (3)
;
; The easiest way to create the 16-bit Year is to take the high binary digit
; and multiply it by 100, then add the low binary digit. We do the multiply
; by adding it to the 16-bit variable 100 times ;-)
;
                LDX     #100            ;Get the index count for 100 (2)
                STZ     BUFF_PG0+8      ;Zero Temp low byte (3)
                STZ     BUFF_PG0+9      ;Zero Temp high byte (3)
;
RTC_ADD         LDA     BUFF_PG0+7      ;Get Year value (3)
                JSR     ADD_16          ;Go add it... (6)
                DEX                     ;Decrement count (2)
                BNE     RTC_ADD         ;Loop back until done (2/3)
;
                LDA     BUFF_PG0+6      ;Get Year lower digit (3)
                JSR     ADD_16          ;Add it into the Temp (6)
;
                LDA     BUFF_PG0+8      ;Get lower byte for year (3)
                STA     BUFF_PG0+5      ;Save it (3)
                LDA     BUFF_PG0+9      ;Get upper byte for year (3)
                ORA     BUFF_PG0+4      ;OR in Month (upper 4 bits) (3)
                STA     BUFF_PG0+4      ;Save it (3)
;
; Now transfer the buffer contents to the Page Zero RTC variables
;
                LDX     #$06            ;Set count for 6 (2)
RTC_XFER        LDA     BUFF_PG0-1,X    ;Get the buffer data (4)
                STA     SECS-1,X        ;Save to RTC variables (4)
                DEX                     ;Decrement count (2)
                BNE     RTC_XFER        ;Loop back till done (2/3)
                RTS                     ;Return to caller (6)
;
ADD_16          CLC                     ;Clear carry flag (2)
                ADC     BUFF_PG0+8      ;Add to Temp low byte (3)
                STA     BUFF_PG0+8      ;And save it (3)
                LDA     BUFF_PG0+9      ;Get high byte (3)
                ADC     #$00            ;Add carry to Temp high byte (2)
                STA     BUFF_PG0+9      ;And save it (3)
                RTS                     ;Return to caller (6)
;
;**************************************************************************************************
;
;This routine detects the IDE Port
; To detect the IDE controller, it can be a bit tricky. It might take several seconds
; from a physical Reset of the IDE Controller before the Busy flag goes off.
; During this time, any commands sent to the IDE controller will fail. As I like to say,
; "timing is everything". After quite a bit of testing, the easy way to detect the IDE
; controller present is to load the IDE_STATUS register. If an IDE Controller is not
; present, the A Reg will show a phantom address of $FE (high order IDE hardware address)
; and any initialization can be bypassed.
;
; If the IDE controller is present, the IDE_STATUS read may be invalid, so it's necessary
; to test the BUSY flag of the status register. Once the IDE Controller is no longer busy,
; the controller can be initialized. This does create an obvious pause in the startup, but
; ensures that the IDE controller can be reliably detected and initialized at boot time.
;
; A change has been made in the IDE_SETUP routine. As the interrupts were disabled on the
; IDE Controller initially, it needs to be re-enabled before the setup is completed. The short
; bit of code that does this is also called by the Reset-Diag function (JMP $FF00).
;
DETECT_IDE
                LDA     IDE_STATUS      ;Get the IDE Status
                CMP     #$FE            ;Check for an empty (phantom) address
                BNE     IDE_INIT        ;If not #$FE, try to Init the IDE controller
NO_IDE          RTS                     ;Return to caller, no IDE controller found
;
;Init the IDE controller
; First, test for the IDE controller ready. If that works, execute the IDE device
; Diagnostics and check for successful completion. If it fails, exit without linking
; the IDE controller into the IRQ chain. Else, link IRQ chain and show message that
; the IDE device is found.
;
IDE_INIT
                JSR     TST_IDE_BUSY    ;Check/wait if IDE is still busy after HW Reset
                JSR     IDE_DIAG        ;Run the self diagnostic
                CMP     #$50            ;Check for $50 on A reg (drive ready)
                BNE     NO_IDE          ;If not, no IDE present
                CPX     #$01            ;X Reg will show #$01 if diags successful
                BNE     NO_IDE          ;If failed, exit
;
; IDE Controller found and passed initial self diagnostics test.
; Send IDE Found message to terminal
;
                DEX                     ;Zero X Reg for index to msg (was #$01)
IDE_MSG_LP      LDA     IDE_MSG,X       ;Get BIOS init msg
                BEQ     IDE_SETUP       ;If zero, msg done, go setup IDE
                JSR     CHROUT          ;Send to console
                INX                     ;Increment Index
                BRA     IDE_MSG_LP      ;Loop back until done
;
;IDE Setup
; This will insert the IDE Controller ISR into the Interrupt Handler chain.
;
; First, disable interrupts, capture the current IRQ exit vector address
; and save it to the first Insert Vector. Second, load the IDE ISR routine
; address and store it to the main IRQ exit vector, then re-enable interrupts.
;
; Second, this routine will execute an Identify IDE command to load the Soft
; Config Data for the maximum LBA Count accessible by the current IDE device.
;
; Note: Two routines are below, use only one! They allow a choice to insert
; the IDE ISR before or after the main Interrupt Handler. By default, loading
; is before the main ISR. Despite the high speed of the data transfers, the
; interrupt rate is not that high.
;
IDE_SETUP       SEI                     ;Disable interrupts
;
; To load the IDE ISR Handler BEFORE the existing DUART ISR Handler:
;
                LDA     IRQVEC0         ;Get low byte of IRQ Exit
                LDY     IRQVEC0+1       ;Get high byte of IRQ Exit
                STA     VECINSRT0       ;Save low byte of IRQ Exit
                STY     VECINSRT0+1     ;Save high byte of IRQ Exit
;
                LDA     #<INTERUPT1     ;Get low byte of IDE ISR
                LDY     #>INTERUPT1     ;Get high byte of IDE ISR
                STA     IRQVEC0         ;Save low byte of IRQ Exit
                STY     IRQVEC0+1       ;Save high byte of IRQ Exit
;
; To load the IDE ISR Handler AFTER the existing DUART ISR Handler:
;
;                LDA     IRQRTVEC0       ;Get low byte of IRQ Exit
;                LDY     IRQRTVEC0+1     ;Get high byte of IRQ Exit
;                STA     VECINSRT0       ;Save low byte of IRQ Exit
;                STY     VECINSRT0+1     ;Save high byte of IRQ Exit
;
;                LDA     #<INTERUPT1     ;Get low byte of IDE ISR
;                LDY     #>INTERUPT1     ;Get high byte of IDE ISR
;                STA     IRQRTVEC0       ;Save low byte of IRQ Exit
;                STY     IRQRTVEC0+1     ;Save high byte of IRQ Exit
;
                CLI                     ;Enable interrupts
                JSR     IDE_EN_IRQ      ;Enable IDE Controller interrupt
;
; Drop into Identify Drive routine
;
IDE_IDENTIFY                            ;Identify Device
;
; This requests a 512-byte block of data that shows capabilities, CHS (not used), LBA Count, etc.
; The format is similar to Read LBA, except no LBA parameter is required. It effectively works as
; a Read Block operation and the data transferred is handled by the ISR for a Read Block.
; NOTE: The Identify Command is coded to load into LBA_BUFFER (address $0600).
;
                LDA     #<LBA_BUFFER    ;Set Address low byte
                LDY     #>LBA_BUFFER    ;Set Address high byte
                LDX     #$01            ;Set Block count to 1
                JSR     IDE_SET_ADDRESS ;Set Xfer address and block count
;
                JSR     TST_IDE_BUSY    ;Wait for IDE Controller not busy
                JSR     IDE_SET_PARMS2  ;Setup required parameters (no LBA parameter)
;
                LDA     #$EC            ;Get Identify Command
                JSR     IDENT_READ      ;Use READ_LBA routine to finish
;
; Identify data loaded in buffer. Now extract LBA count and store to Soft Config Data for usage
; by access routines (Read/Write/Verify). Four bytes are used and the format from the Identify
; Command are Low-order Word / High-order Word, where each word is in Big Endian. We will store
; the LBA count as Little Endian, Low-order Word / High-order Word.
; The offset from the buffer are Words 60-61 (decimal).
;
; A table is used to index the offset of bytes to move into consecutive soft data.
;
                LDX     #$04            ;Set count for 4 bytes
LBA_SIZE        LDY     LBA_OFFSET-1,X  ;Get Offset to LBA count
                LDA     (LBA_ADDR_LOW),Y        ;Load LBA Data
                STA     LOAD_IDE-1,X    ;Store to Soft Config Data
                DEX                     ;Decrement count
                BNE     LBA_SIZE        ;Loop back until done
;
; Enable Write Cache for Microdrive, performance improvement ;-)
IDE_EN_CACHE
                LDA     #$02            ;Get Enable Write Cache Feature
IDE_CACHE_CMD
                STA     IDE_FEATURE     ;Send to IDE controller
                LDA     #%11100000      ;Get Drive 0, LBA. mode, etc.
                STA     IDE_DRV_HEAD    ;Send to IDE controller
                LDA     #$EF            ;Get Set Features Command
                STA     IDE_COMMAND     ;Send Command to set feature
                JSR     TST_IDE_RDY     ;Test for Drive ready
                LDA     IDE_STATUS      ;Get Status
                LDX     IDE_ERROR       ;Get Error (if any)
                RTS                     ;Return to Caller
;
; Disable Write Cache for Microdrive, NO performance improvement;-(
IDE_DIS_CACHE
                LDA     #$82            ;Get Disable Write Cache Feature
                BRA     IDE_CACHE_CMD   ;Finish Command above
;
; Reset IDE Controller and run Diagnostics
; The RECAL routine disables the IRQ function, so the routine to enable the IRQ
; is called, then drops into the get status routine before returning.
;
IDE_RESET                               ;Do a Reset of IDE device
                JSR     IDE_RECAL       ;Call IDE_RESET (set LBA mode)
                JSR     IDE_EN_CACHE    ;Enable Write Cache
                JSR     TST_IDE_RDY     ;Wait until Drive ready
                JSR     IDE_EN_IRQ      ;Re-enable IDE Controller interrupt
;
; Drop into Get Status routine after Diagnostics are run
;
IDE_GET_STATUS                          ;Get Status/Error registers from the IDE controller
;
; This routine gets the current status of the IDE Controller and can be issued at any time.
; It does not rely on any interrupt capability as it's a simple read of the Status and the
; Error registers from the IDE Controller.
;
; Note: This routine should be called whenever an Error has occurred, as it returns the
; contents of the Error Register in the X Register and the contents of the Status Register
; in the A Register.
;
; Details for the Registers are:
;
; Error Register:
;Bit 7 - CRC Error or Bad Block error
;Bit 6 - Uncorrectable Data Error
;Bit 5 - 0 (not used) MC (used for Removable-Media drives)
;Bit 4 - ID Not Found
;Bit 3 - 0 (not used) MCR (used for Removable-Media drives)
;Bit 2 - Aborted Command error
;Bit 1 - Track Zero not found error
;Bit 0 - Data Address Mark Not Found
;
; Status Register:
;Bit 7 - Busy - IDE Controller is Busy (all other bits invalid)
;Bit 6 - Drive Ready (IDE Controller Ready to accept Commands)
;Bit 5 - Drive Write Fault - Write Fault error has occurred
;Bit 4 - Drive Seek Complete - is active when the drive is not seeking
;Bit 3 - Data Request - bit set when the IDE Controller has Data to transfer (R/W)
;Bit 2 - Correctable Data - bit set when bad data was found and corrected (ECC)
;Bit 1 - Index - bit toggled from 0 to 1 once per disk revolution
;Bit 0 - Error - bit set when previous command ended with some sort of error
;
                JSR     TST_IDE_BUSY    ;Wait for IDE Controller not busy
                LDA     IDE_STATUS      ;Get IDE Status Register
                LDX     IDE_ERROR       ;Get IDE Error Register
                RTS                     ;Return to Caller
;
IDE_DIAG                                ;Run internal Diagnostics on the IDE controller
;
; This is a basic self test within the IDE controller.
; This runs some internal tests for the IDE controller and returns with drive ready bits
; active ($50) and the error register as $01 if successful.
; For Diagnostics, the Error Register contains unique codes as follows:
;
;01h - No error Detected
;02h - Formatter device error
;03h - Sector Buffer error
;04h - ECC Circuitry error
;05h - Controller Microprocessor error
;8xH - Device 1 failed (not valid with a single drive system)
;       Note: 80h OR’ed in for the Slave Drive, lower bit definitions are the same!
;
; To send this command, a common routine is used to save ROM space.
;
                LDA     #$90            ;Get Diagnostic Command
                BRA     IDE_SEND_CMD    ;Branch and send Command to IDE
;
IDE_RECAL                               ;Recalibrate Command
;
;This is the Recalibrate Command ($1x). Upon issue the IDE controller will move the heads to
; Cylinder 0 and read Head 0, Sector 1. If unsuccessful, an error will be posted.
;
                LDA     #$10            ;Get Recalibrate Command
;
; Drop into Send Command routine
;
IDE_SEND_CMD                            ;Send a Command to the IDE controller
;
;Accepts a Command code via the A reg and sets up the necessary IDE Controller
; registers to accept it. It also tests to ensure the controller is ready
; to accept the command and get the Status and Error registers on return.
;
; NOTE: this routine turns off the interrupt capability as it is called
; during initial setup, where the interrupt handler has not been setup yet.
; A separate routine is called to enable the interrupt capability.
;
                SEI                     ;Disable Interrupts
                TAX                     ;Save Command to X Reg
                JSR     TST_IDE_RDY     ;Wait for IDE to be ready
                LDA     #%00001010      ;Get Mask to disable IRQ
                STA     IDE_DEV_CTRL    ;Send to control register
                LDA     #%11100000      ;Get Select Mask (LBA Mode, Drive 0)
                STA     IDE_DRV_HEAD    ;Select Drive 0
;
                STX     IDE_COMMAND     ;Send command to IDE
                JSR     TST_IDE_BUSY    ;Wait for drive Busy
;
                LDA     IDE_STATUS      ;Get IDE Status Register
                LDX     IDE_ERROR       ;Get IDE Error Register
                CLI                     ;Enable interrupts
                RTS                     ;Return to caller
;
; Enable Interrupts on the IDE Controller.
; This needs to be executed during initial setup and anytime the Reset/Diag BIOS function
; is called.
;
IDE_EN_IRQ                              ;Enable IDE Controller interrupt
                JSR     TST_IDE_RDY     ;Wait for IDE to be ready
                LDA     #%00001000      ;Get Mask to enable IRQ
                STA     IDE_DEV_CTRL    ;Send to control register
                RTS                     ;Return to caller
;
;**************************************************************************************************
;
;Interrupt 1 - This is the ISR which is responsible for servicing the IDE controller.
; The RTC does not require any ISR capabilities as no Alarm functions are being used in the BIOS.
; There are extra inserts which can be used if needed at a later date.
; The only functions that might make sense would be to add the Alarm function at a future date.
; Once the IDE controller BIOS has matured, if there's any room left in the allocated ROM area,
; I'll revisit it.
;
;The ISR for the IDE controller will handle the data transfer for LBA read/write/verify functions
; and handle any error functions. By design, the 16-bit Data Transfer feature is used for:
; Reading / Writing / Verifying of all LBA block data and the IDE Identification data.
;
;The BIOS is using the Alternate Status register to determine if DRQ (Data Request) is active.
; This works as a handshake for 16-bit data transfers without issue. Note that the normal Status
; register resets the interrupt when read, so this is only done once in the ISR per loop.
;
;Update: This ISR has been moved to the front of the ISR chain, i.e., this ISR routine gets
; serviced first, then jumps to the next ISR, which services the DUART. This makes a noticeable
; improvement in data transfer from the IDE controller. Note that overhead for this routine will
; add 33 clock cycles if it just exits (IDE controller did not generate an interrupt).
;
;To check if an interrupt has been generated by the IDE controller, the Alternate Status register
; can be read. This contains the same information as the standard Status register but will NOT
; reset the interrupt on the IDE controller. By reading the Alternate Status register first, we
; can first determine what the status of the IDE controller is and take action if required.
; Note that not all bit settings imply an interrupt was generated. Specifically, looking at the
; bit definitions below, Bits 6 and 4 are set when the IDE is ready, hence a normal condition
; where nothing requires any attention. Also, a Busy condition can imply the IDE controller is
; working on a command but may not have generated an interrupt yet. If The Busy bit (7) is set,
; then all other bits are invalid per Seagate documentation, so we trigger on that first.
;
;One annoying feature for IDE is "when" interrupts are generated. For any Read operation, once
; the command has been accepted, data is placed into the IDE buffer, followed by generating
; an interrupt to the system. Once this is done, the system will read the data. By accessing the
; Status register, the interrupt will be reset. This is normal operation. For a write operation,
; The command is sent, then DRQ goes active, which requires the data be sent to the IDE Device.
; Once the data is written, an interrupt is generated after it's completed.
; As a result, there's little function of having an ISR for servicing the write function.
; As interrupts are enabled for the IDE Controller, all generated interrupts must be handled.
;
; Status Register bits as defined as follows:
;       - Bit 7 = Busy (a Command has been accepted)
;       - Bit 6 = Ready (IDE controller is ready to accept a command)
;       - Bit 5 = Write Fault (A write command failed against the media)
;       - Bit 4 = DSC (is set when a Seek is completed)
;       - Bit 3 = Data Request (set when there is data to transfer, read or write)
;       - Bit 2 = Correction (set when a recoverable data error was corrected)
;       - Bit 1 = 0 (not used)
;       - Bit 0 = Error (set when the previous command had an unrecoverable error)
;
;       NOTE: 25 clock cycles to here if DUART ISR is second!
;
INTERUPT1                               ;Interrupt 1 (IDE)
                LDA     IDE_ALT_STATUS  ;Get Alternate Status Register (4)
                BMI     REGEXT01        ;If Busy bit active, just exit (2/3)
;
; - Check for Data Request (DRQ), as the Read LBA operation is the main function
;   of the ISR, which will handle the data transfer from the IDE controller to store the
;   data into memory. This ISR will handle single and multiple block transfers.
;
                LDA     IDE_STATUS      ;Get Status (resets IRQ) (4)
                AND     #%00001000      ;Check for DRQ (2)
                BNE     IDE_READ_BLK    ;Branch if active (2/3)
;
; - If no DRQ is sensed, the other possibilities are:
; 1- A LBA Write has occurred and the interrupt was generated after the transfer.
; 2- A Verify operation has occurred and the interrupt was generated after the verify.
; So we check for these two options and branch accordingly.
;
                BBS2    MATCH,IDE_WRIT_BLK      ;If Bit 2 set, Write operation (5)
                BBS1    MATCH,IDE_VRFY_BLK      ;If Bit 1 set, Verify operation (5)
                BRA     REGEXT01                ;Exit ISR handler (3)
;
IDE_READ_BLK                            ;IDE Read a Block of data
;
;Note: Arrival here means that the DRQ bit in the status register is active.
; This implies that:
;  1- A LBA Block Read is in progress. If so, the data transfer will be handled below.
;     This also handles multiple LBA Reads and manages the pointers and such. It also
;     clears the LBA Read bit in the MATCH Flag when completed.
;
;  2- A LBA Block Write with multilpe blocks is in progress. If so, the actual data
;     transfer is handled via the IDE WRITE Block routine. An interrupt is generated
;     at the end of each LBA transfer, so that is monitored here and the LBA Write bit
;     in the MATCH Flag is cleared when there are no more blocks to transfer.
;
;Also realize that this ISR will be executed every time the DUART generates an interrupt.
; This will happen every 10ms for the Jiffy-Clock timer and for character transmit and receive.
;
                BBR3    MATCH,REGEXT01  ;If Bit 3 clear, IDE Write (5)
;
LBA_XFER        LDA     IDE_ALT_STATUS  ;Get Status (clears IRQ) (4)
                AND     #%00001000      ;Check for DRQ (2)
                BEQ     IDE_RD_DONE     ;If not active, done, exit (2/3)
;
IDE_RD_RBLK
                LDA     IDE_DATA        ;Read low byte (high byte in latch) (4)
                STA     (BIOS_XFERL)    ;Store low byte (5)
                INC     BIOS_XFERL      ;Increment pointers (5)
                BNE     IDE_RD_BLK1     ; (2/3)
                INC     BIOS_XFERH      ; (5)
IDE_RD_BLK1
                LDA     IDE_16_READ     ;Read high byte from latch (4)
                STA     (BIOS_XFERL)    ;Store high byte (5)
                INC     BIOS_XFERL      ;Increment pointers (5)
                BNE     LBA_XFER        ;Loop back to Xfer, saves 3 clock cycles (2/3)
                INC     BIOS_XFERH      ; (5)
IDE_RD_BLK2
                BRA     LBA_XFER        ;Loop back till no more DRQs (3)
;
IDE_RD_DONE     DEC     BIOS_XFERC      ;Decrement Block Count to transfer (5)
                BNE     IDE_ALL_DONE    ;Branch around Flag Reset until all blocks moved (2/3)
                RMB3    MATCH           ;Clear Read Block flag (5)
;
IDE_ALL_DONE    LDA     IDE_ALT_STATUS  ;Get Alternate Status Register (4)
                STA     IDE_STATUS_RAM  ;Save it to RAM location (3)
REGEXT01        JMP     (VECINSRT0)     ;Exit ISR handler (6)
;
IDE_WRIT_BLK                            ;IDE Write a Block of data
                LDA     BIOS_XFERC      ;Check Block Count to transfer (3)
                BNE     IDE_ALL_DONE    ;Branch to exit if more blocks need to be moved (2/3)
                RMB2    MATCH           ;Clear Write Block flag (5)
                BRA     IDE_ALL_DONE    ;Branch and finish ISR (3)
;
IDE_VRFY_BLK                            ;IDE Verify a Block of data
                RMB1    MATCH           ;Clear Verify Block flag (5)
                BRA     IDE_ALL_DONE    ;Branch and finish ISR (3)
;
;**************************************************************************************************
;
;BRK/IRQ Interrupt service routines
;The pre-process routine located in page $FF soft-vectors to INTERUPT0/BRKINSTR0 below
;       These are the routines that handle BRK and IRQ functions
;       The BRK handler saves CPU details for register display
;       - A Monitor can provide a disassembly of the last executed instruction
;       - A Received Break is also handled here (ExtraPutty/Windows or Serial/OSX)
;
; SC28L92 handler
;       The 28L92 IRQ routine handles Transmit, Receive, Timer and Received-Break interrupts
;       - Transmit and Receive each have a 128 byte circular FIFO buffer in memory per channel
;       - Xmit IRQ is controlled (On/Off) by the handler and the CHROUT(2) routine
;
; The 28L92 Timer resolution is 10ms and used as a Jiffy Clock for RTC, delays and benchmarking
;
;**************************************************************************************************
;
;BIOS routines to handle interrupt-driven I/O for the SC28L92
;NOTE: IP0 Pin is used for RTS, which is automatically handled in the chip. As a result,
; the upper 2 bits of the ISR are not used in the handler. The Lower 5 bits are used, but
; the lower two are used to determine when to disable transmit after the buffer is empty.
;
;The DUART_ISR bits are defined as follows:

; Bit7          ;Input Change Interrupt
; Bit6          ;Change Break B Interrupt
; Bit5          ;RxRDY B Interrupt
; Bit4          ;TxRDY B Interrupt
; Bit3          ;Counter Ready Interrupt
; Bit2          ;Change Break A Interrupt
; Bit1          ;RxRDY A Interrupt
; Bit0          ;TxRDY A Interrupt
;
;**************************************************************************************************
;
; SC8L92 uses all bits in the Status Register!
; - for Receive Buffer full, we set a bit in the SC28L92 Misc. Register, one for each Port.
; Note that the Misc. Register in the SC28L92 is a free byte for storing the flags, as it's
; not used when the DUART is configured in Intel mode! Freebie Register for us to use ;-)
;
;ISR Routines for SC28L92 Port B
;
UARTB_RCV0      LDY     ICNT_B          ;Get input buffer count (3)
                BMI     BUFFUL_B        ;Check against limit ($80), branch if full (2/3)
;
UARTB_RCVLP     LDA     UART_STATUS_B   ;Get Status Register (4)
                BIT     #%00000001      ;Check RxRDY active (2)
                BEQ     UARTB_CXMT      ;If RxRDY not set, FIFO is empty, check Xmit (2/3)

                LDA     UART_RECEIVE_B  ;Else, get data from 28L92 (4)
                LDY     ITAIL_B         ;Get the tail pointer to buffer (3)
                STA     IBUF_B,Y        ;Store into buffer (5)
                INC     ITAIL_B         ;Increment tail pointer (5)
                RMB7    ITAIL_B         ;Strip off bit 7, 128 bytes only (5)
                INC     ICNT_B          ;increment data count (5)
                BPL     UARTB_RCVLP     ;If input buffer not full, check for more FIFO data (2/3)
;
UARTB_CXMT      LDA     UART_ISR        ;Get 28L92 ISR Reg (4)
                BIT     #%00010000      ;Check for Xmit B active (2)
                BEQ     REGEXT_B        ;Exit if inactive (2/3)
;
; To take advantage of the onboard FIFO, we test the TxRDY bit in the Status Register.
; If the bit is set, then there is more room in the FIFO. The ISR routine here will
; attempt to fill the FIFO from the Output Buffer. This saves processing time in the
; ISR itself.
;
UARTB_XMT0      LDA     OCNT_B          ;Get output buffer count, any data to xmit? (3)
                BEQ     NODATA_B        ;If zero, no data left, turn off xmit (2/3)
;
UARTB_XMTLP     LDA     UART_STATUS_B   ;Get Status Register (4)
                BIT     #%00000100      ;Check TxRDY active (2)
                BEQ     REGEXT_B        ;If TxRDY not set, FIFO is full, exit ISR (2/3)
;
                LDY     OHEAD_B         ;Get the head pointer to buffer (3)
                LDA     OBUF_B,Y        ;Get the next data (5)
                STA     UART_TRANSMIT_B ;Send the data to 28L92 (4)
                INC     OHEAD_B         ;Increment head pointer (5)
                RMB7    OHEAD_B         ;Strip off bit 7, 128 bytes only (5)
                DEC     OCNT_B          ;Decrement counter (5)
                BNE     UARTB_XMTLP     ;If more data, loop back to send it (2/3)
;
;No more buffer data to send, check SC28L92 TxEMT and disable transmit if empty.
; Note: If the TxEMT bit is set, then the FIFO is empty and all data has been sent.
;
NODATA_B        LDY     #%00001000      ;Else, get mask for xmit off (2)
                STY     UART_COMMAND_B  ;Turn off xmit (4)
REGEXT_B        JMP     (IRQRTVEC0)     ;Return to Exit/ROM IRQ handler (6)
;
BUFFUL_B        LDY     #%00010000      ;Get Mask for Buffer full (2)
                STY     UART_MISC       ;Save into 28L92 Misc. Register (4)
                BRA     REGEXT_B        ;Exit IRQ handler (3)
;
;ISR Routines for SC28L92 Port A
;
; The Receive Buffer is checked first to ensure there is open space in the buffer.
; By loadng the input count, bit7 will be set if it is full, which will set the "N"
; flag in the CPU status register. If this is the case, we exit to BUFFUL_A and set
; a bit the SC28L92 Misc. Register. If the buffer has space, we continue.
; 
; To take advantage of the onboard FIFO, we test the RxRDY bit in the Status Register.
; If the bit is set, the FIFO has data and the routine moves data from the FIFO into
; the Receive buffer. We loop back and contnue moving data from the FIFO to the buffer
; until the RxRDY bit is cleared (FIFO empty). If the FIFO is empty, we branch and
; check for a pending Transmit interrupt, just to save some ISR time.
;
; NOTE: the receiver is configured to use the Watchdog function. This will generate a
; receiver interrupt within 64 bit times once data is received (and the FIFO has not
; reached it's configured fill level). This provides the required operation for use
; as a console, as single character commands are common and would not fill the FIFO,
; which generates an interrupt based on the configured FIFO fill level.
;
UARTA_RCV       LDY     ICNT_A          ;Get input buffer count (3)
                BMI     BUFFUL_A        ;Check against limit ($80), branch if full (2/3)
;
UARTA_RCVLP     LDA     UART_STATUS_A   ;Get Status Register (4)
                BIT     #%00000001      ;Check RxRDY active (2)
                BEQ     UARTA_CXMT      ;If RxRDY not set, FIFO is empty, check Xmit (2/3)

                LDA     UART_RECEIVE_A  ;Else, get data from 28L92 (4)
                LDY     ITAIL_A         ;Get the tail pointer to buffer (3)
                STA     IBUF_A,Y        ;Store into buffer (5)
                INC     ITAIL_A         ;Increment tail pointer (5)
                RMB7    ITAIL_A         ;Strip off bit 7, 128 bytes only (5)
                INC     ICNT_A          ;Increment input bufffer count (5)
                BPL     UARTA_RCVLP     ;If input buffer not full, check for more FIFO data (2/3)
;
UARTA_CXMT      LDA     UART_ISR        ;Get 28L92 ISR Reg (4)
                BIT     #%00000001      ;Check for Xmit A active (2)
                BEQ     REGEXT_A        ;Exit if inactive, else drop into Xmit code (2/3)
;
;To take advantage of the onboard FIFO, we test the TxRDY bit in the Status Register.
; If the bit is set, then there is more room in the FIFO. The ISR routine here will
; attempt to fill the FIFO from the Output Buffer. This saves processing time in the
; ISR itself.
;
UARTA_XMT       LDA     OCNT_A          ;Get output buffer count, any data to xmit? (3)
                BEQ     NODATA_A        ;If zero, no data left, turn off xmit (2/3)
;
UARTA_XMTLP     LDA     UART_STATUS_A   ;Get Status Register (4)
                BIT     #%00000100      ;Check TxRDY active (2)
                BEQ     REGEXT_A        ;If TxRDY not set, FIFO is full, exit ISR (2/3)
;
                LDY     OHEAD_A         ;Get the head pointer to buffer (3)
                LDA     OBUF_A,Y        ;Get the next data (5)
                STA     UART_TRANSMIT_A ;Send the data to 28L92 (4)
                INC     OHEAD_A         ;Increment head pointer (5)
                RMB7    OHEAD_A         ;Strip off bit 7, 128 bytes only (5)
                DEC     OCNT_A          ;Decrement output buffer count (5)
                BNE     UARTA_XMTLP     ;If more data, loop back to send it (2/3)
;
;No more buffer data to send, check SC28L92 TxEMT and disable transmit if empty.
; Note: If the TxEMT bit is set, then the FIFO is empty and all data has been sent.
;
NODATA_A        LDY     #%00001000      ;Else, get mask for xmit off (2)
                STY     UART_COMMAND_A  ;Turn off xmit (4)
REGEXT_A        JMP     (IRQRTVEC0)     ;Return to Exit/ROM IRQ handler (6)
;
BUFFUL_A        LDY     #%00000001      ;Get Mask for Buffer full (2)
                STY     UART_MISC       ;Save into 28L92 Misc. Register (4)
                BRA     REGEXT_A        ;Exit IRQ handler (3)
;
;IRQ Vector defaults to here, which is the Start of Interrupt handler.
; NOTE: 25 clock cycles to get to this routine
; NOTE: If this ISR is after the IDE ISR, it will take 33 additional clock cycles
;
INTERUPT0                               ;Interrupt 0 to handle the SC28L92 DUART
                LDA     UART_ISR        ;Get the UART Interrupt Status Register (4)
                BEQ     REGEXT0         ;If no bits are set, exit handler (2/3)
;
                BIT     #%00000100      ;Test for Break on A (2)
                BNE     UARTA_BRK       ;If yes, Reset the DUART receiver (2/3)
;
                BIT     #%00000010      ;Test for RHR A having data (2)
                BNE     UARTA_RCV       ;If yes, put the data in the buffer (2/3)
;
                BIT     #%00000001      ;Test for THR A ready to receive data (2)
                BNE     UARTA_XMT       ;If yes, get data from the buffer (2/3)
;
                BIT     #%01000000      ;Test for Break on B (2)
                BNE     UARTB_BRK       ;If yes, Reset the DUART receiver (2/3)
;
                BIT     #%00100000      ;Test for RHR B having data (2)
                BNE     UARTB_RCV       ;If yes, put the data in the buffer (2/3)
;
                BIT     #%00010000      ;Test for THR B ready to receive data (2)
                BNE     UARTB_XMT       ;If yes, get data from the buffer (2/3)
;
                BIT     #%00001000      ;Test for Counter ready (RTC) (2)
                BNE     UART_RTC        ;If yes, go increment RTC variables (2/3)
;
; if none of the above bits caused the IRQ, the only bit left is the change input port.
; just save it in the temp IRT register in page zero and exit.
;
                STA     UART_IRT        ;Save the 28L92 ISR for later use (3)
REGEXT0         JMP     (IRQRTVEC0)     ;Return to Exit/ROM IRQ handler (6)
;
UARTA_BRK       JMP     UARTA_BRK0      ;Gotta JUMP to the routine (3)
UARTB_BRK       JMP     UARTB_BRK0      ;Gotta JUMP to the routine (3)
;
UARTB_RCV       JMP     UARTB_RCV0      ;Gotta JUMP to the routine (3)
UARTB_XMT       JMP     UARTB_XMT0      ;Gotta JUMP to the routine (3)
;
;NOTE: Stop timer cmd resets the interrupt flag, counter continues to generate interrupts.
; NOTE: 35 clock cycles to here from INTERUPT0 - 65 in total
;
UART_RTC        LDA     UART_STOP_CNT   ;Get Command mask for stop timer (4)
;
; Check the MATCH flag bit7 to see if a Delay is active. If yes, decrement the MSDELAY
; variable once each pass until it is zero, then clear the MATCH flag bit7
;
                BBR7    MATCH,SKIP_DLY  ;Skip Delay if bit7 is clear (5)
                DEC     MSDELAY         ;Decrement Millisecond delay variable (5)
                BNE     SKIP_DLY        ;If not zero, skip (2/3)
                RMB7    MATCH           ;Else clear MATCH flag (5)
;
; Check the MATCH flag bit6 to see if Benchmarking is active. If yes, increment the
; variables once each pass until the MATCH flag bit6 is inactive.
;
SKIP_DLY        BBR6    MATCH,SKIP_CNT  ;Skip Count if bit6 bit is clear (5)
                INC     MS10_CNT        ;Else, increment 10ms count (5)
                LDA     MS10_CNT        ;Load current value (3)
                CMP     #100            ;Compare for 1 second elapsed time (2)
                BCC     SKIP_CNT        ;If not, skip to RTC update (2/3)
                STZ     MS10_CNT        ;Else, zero 10ms count (3)
                INC     SECL_CNT        ;Increment low byte elapsed seconds (5)
                BNE     SKIP_CNT        ;If no overflow, skip to RTC update (2/3)
                INC     SECH_CNT        ;Else, increment high byte elapsed seconds (5)
;
SKIP_CNT        DEC     TICKS           ;Decrement RTC tick count (5)
                BNE     REGEXT0         ;Exit if not zero (2/3)
                LDA     #DF_TICKS       ;Get default tick count (2)
                STA     TICKS           ;Reset tick count (3)
;
                INC     SECS            ;Increment seconds (5)
                LDA     SECS            ;Load it to A reg (3)
                CMP     #60             ;Check for 60 seconds (2)
                BCC     REGEXT0         ;If not, exit (2/3)
                STZ     SECS            ;Else, reset seconds, inc Minutes (3)
;
                INC     MINS            ;Increment Minutes (5)
                LDA     MINS            ;Load it to A reg (3)
                CMP     #60             ;Check for 60 minutes (2)
                BCC     REGEXT0         ;If not, exit (2/3)
                STZ     MINS            ;Else, reset Minutes, inc Hours (3)
;
                INC     HOURS           ;Increment Hours (5)
                LDA     HOURS           ;Load it to A reg (3)
                CMP     #24             ;Check for 24 hours (2)
                BCC     REGEXT0         ;If not, exit (2/3)
                STZ     HOURS           ;Else, reset hours, inc Days (3)
;
;This is the tricky part ;-)
; One variable holds the Day of the week and the Date of the Month!
; First, update the Day of the Week, which is the upper 3 bits of the variable
; Valid days are 1 to 7. Mask off the upper 3 bits, and add #%00100000 to it,
; if the result is zero, it was #%11100000, so start over by making the Day
; #%00100000, then save it to RTC_TEMP variable.
;
;Once that's done, load the variable again, mask off the Date and increase by
; one, then check against days of the month, update as required and check for
; Leap year and February 29th stuff. When all updated, OR in the Day from the
; RTC_TEMP variable and finish updating the DAY_DATE variable.
;
                LDA     DAY_DATE        ;Get Day and Date variable (3)
                AND     #%11100000      ;Mask off for Day of Week (1-7) (2)
                CLC                     ;Clear carry for add (2)
                ADC     #%00100000      ;Add effective "1" to Day of week (2)
                CLC                     ;Clear carry to avoid extra day add (2)
                BNE     NO_DAY_ADD      ;If non-zero, don't reset to "1" (2/3)
                LDA     #%00100000      ;Else, reset Day to "1" (2)
NO_DAY_ADD
                STA     RTC_TEMP        ;Store the updated Day into temp (3)
;
;Get the Month and Year variable, move the upper 4-bits down to get the
; current Month. Xfer it the X reg, then get the Date, increment by one
; and check against the number of days in that month.
;
                LDA     MONTH_CENTURY   ;Get Month and Year variable (3)
                LSR     A               ;Shift Month to lower 4 bits (2)
                LSR     A               ; (2)
                LSR     A               ; (2)
                LSR     A               ; (2)
                TAX                     ;Move to X reg (2)
                LDA     DAY_DATE        ;Get Day and Date variable (3)
                AND     #%00011111      ;Mask off for Date of Month (1-31) (2)
                INC     A               ;Increment by one (2)
                CMP     MAX_DATE-1,X    ;Check for Max Day per Month+1 (4)
                BCS     MONTH_ADD       ;Branch if we need to update the Month (2/3)
DO_29           ORA     RTC_TEMP        ;Else OR in updated Date to updated Day (3)
                STA     DAY_DATE        ;Update Day and Date variable (3)
                BRA     REGEXT00        ;Then exit IRQ Handler (3)
;
MONTH_ADD       CPX     #$02            ;Check for Month = February (2)
                BNE     MONTH_INC       ;If not, increment Month (2/3)
                LDA     YEAR            ;Else, Get current year low byte (3)
                AND     #%00000011      ;Mask off lower 2 bits (2)
                BNE     MONTH_INC       ;If not a Leap Year, continue on (2/3)
                LDA     DAY_DATE        ;Get Day and Date variable (3)
                AND     #%00011111      :Mask off Date (2)
                INC     A               ;Increment by one (2)
                CMP     #30             ;Check for 29th+1 Day for Leap Year (2)
                BCC     DO_29           ;Save Date as 29th and exit IRQ handler (2/3)
;
MONTH_INC       LDA     RTC_TEMP        ;Get updated Day (Date is effective zero) (3)
                ORA     #%00000001      ;OR in the 1st Day of the Month (2)
                STA     DAY_DATE        ;Save updated Day and Date of the Month (3)
;
                LDA     MONTH_CENTURY   ;Get Month and Year variable (3)
                CLC                     ;Clear Carry for add (2)
                ADC     #$10            ;Add "1" to Month (upper 4 bits) (2)
                STA     RTC_TEMP        ;Save it to work temp (3)
                AND     #%11110000      ;Mask off Century (lower 4 bits) (2)
                CMP     #$D0            ;Check for "13" (December + 1) (2)
                BCS     YEAR_ADD        ;If 13 go add to YEAR (2/3)
                LDA     RTC_TEMP        ;Else, Get updated Month and Century (3)
                STA     MONTH_CENTURY   ;Save it (3)
                BRA     REGEXT00        ;Exit IRQ Handler (3)
;
YEAR_ADD        LDA     MONTH_CENTURY   ;Get Month and Century (3)
                AND     #%00001111      ;Mask off old month (2)
                ORA     #%00010000      ;OR in $10 for January (2)
                STA     MONTH_CENTURY   ;Save updated Month and existing upper 4 bits (3)
                INC     YEAR            ;Increment Year low byte (0-255) (5)
                BNE     REGEXT00        ;If no rollover, exit ISR (2/3)
                LDA     MONTH_CENTURY   ;Get Month and Year variable (3)
                TAX                     ;Save to X reg (2)
                AND     #%11110000      ;Mask off upper 4-bits for year (2)
                STA     RTC_TEMP        ;Save it in the temp area (3)
                TXA                     ;Get the Month and Year back (2)
                AND     #%00001111      ;Mask off the month (2)
                CLC                     ;Clear carry for add (2)
                ADC     #$01            ;Add 1 to upper 4 bits (2)
                ORA     RTC_TEMP        ;OR in the Month (3)
                STA     MONTH_CENTURY   ;Update Month and Century variable (3)
REGEXT00        JMP     (IRQRTVEC0)     ;If no rollover, then exit IRQ handler (6)
;
;Received Break handler for Port A
UARTA_BRK0      LDA     UART_STATUS_A   ;Get DUART Status Register (4)
                BMI     BREAKEY_A       ;If bit 7 set, received Break was detected (2/3)
;
; If a received Break was not the cause, we should reset the DUART Port as the cause
; could have been a receiver error, i.e., parity or framing
;
                LDX     #UART_RDATAE-UART_RDATA ;Get index count (2)
UARTA_RST1      LDA     UART_RDATA-1,X  ;Get Reset commands (4)
                STA     UART_COMMAND_A  ;Send to DUART CR (3)
                DEX                     ;Decrement the command list (2)
                BNE     UARTA_RST1      ;Loop back until all are sent (2/3)
                BRA     REGEXT00        ;Exit (3)
;
;Received Break handler for Port B
UARTB_BRK0      LDA     UART_STATUS_B   ;Get DUART Status Register (4)
                BMI     BREAKEY_B       ;If bit 7 set, received Break was detected (2/3)
;
; If a received Break was not the cause, we should reset the DUART Port as the cause
; could have been a receiver error, i.e., parity or framing
;
                LDX     #UART_RDATAE-UART_RDATA ;Get index count (2)
UARTB_RST1      LDA     UART_RDATA-1,X  ;Get Reset commands (4)
                STA     UART_COMMAND_B  ;Send to DUART CR (3)
                DEX                     ;Decrement the command list (2)
                BNE     UARTB_RST1      ;Loop back until all are sent (2/3)
                BRA     REGEXT00        ;Exit (3)
;
; A received Break was the cause. Just reset the receiver and return.
;
BREAKEY_B       LDA     #%01000000      ;Get Reset Received Break command (2)
                STA     UART_COMMAND_B  ;Send to DUART to reset (4)
                LDA     #%01010000      ;Get Reset Break Interrupt command (2)
                STA     UART_COMMAND_B  ;Send to DUART to reset (4)
                BRA     REGEXT00        ;Exit (3)
;
; A received Break was the cause. Reset the receiver and process the BRK routine.
;
BREAKEY_A       LDA     #%01000000      ;Get Reset Received Break command (2)
                STA     UART_COMMAND_A  ;Send to DUART to reset (4)
                LDA     #%01010000      ;Get Reset Break Interrupt command (2)
                STA     UART_COMMAND_A  ;Send to DUART to reset (4)
;
BREAKEY         CLI                     ;Enable IRQ (2)
;
; BRK Vector defaults to here
;
BRKINSTR0       PLY                     ;Restore Y Reg (4)
                PLX                     ;Restore X Reg (4)
                PLA                     ;Restore A Reg (4)
                STA     AREG            ;Save A Reg (3)
                STX     XREG            ;Save X Reg (3)
                STY     YREG            ;Save Y Reg (3)
                PLA                     ;Get Processor Status (4)
                STA     PREG            ;Save in PROCESSOR STATUS preset/result (3)
                TSX                     ;Xfer STACK pointer to X Reg (2)
                STX     SREG            ;Save STACK pointer (3)
;
                PLX                     ;Pull Low RETURN address from STACK then save it (4)
                STX     PCL             ;Store program counter Low byte (3)
                STX     INDEXL          ;Seed Indexl for DIS_LINE (3)
                PLY                     ;Pull High RETURN address from STACK then save it (4)
                STY     PCH             ;Store program counter High byte (3)
                STY     INDEXH          ;Seed Indexh for DIS_LINE (3)
                BBR4    PREG,DO_NULL    ;Check for BRK bit set (5)
;
; The following three subroutines are contained in the base C02 Monitor code. These calls
; do a register display and disassembles the line of code that caused the BRK to occur
;
                JSR     M_PRSTAT1       ;Display CPU status (6)
                JSR     M_DECINDEX      ;Decrement Index to BRK ID Byte (6)
                JSR     M_DECINDEX      ;Decrement Index to BRK instruction (6)
                JSR     M_DIS_LINE      ;Disassemble BRK instruction (6)
;
; Note: This routine only clears Port A, as it is used for the Console
;
DO_NULL         LDA     #$00            ;Clear all Processor Status Register bits (2)
                PHA                     ;Push it to Stack (3)
                PLP                     ;Pull it to Processor Status (4)
                STZ     ITAIL_A         ;Clear input buffer pointers (3)
                STZ     IHEAD_A         ; (3)
                STZ     ICNT_A          ; (3)
                JMP     (BRKRTVEC0)     ;Done BRK service process, re-enter monitor (6)
;
;**************************************************************************************************
;
; EPOCH time (Unix) starts on Thursday, 1st January, 1970.
; If no RTC is detected, preload the EPOCH Date as a start.
; Note: No time of day is preloaded, time is 00:00:00 after a cold start.
;
EPOCH           .DB     %11000001       ;Day 6 / Date 1
                .DB     %00010111       ;Month 1  High byte (nibble) of 1970
                .DB     %10110010       ;Low byte of 1970 ($B2)
;
;END OF BIOS CODE for Pages $F8 through $FD
;**************************************************************************************************
        .ORG    $FE00   ;Reserved for I/O space - do NOT put code here
;There are 5- I/O Selects, each is 32-bytes wide.
; I/O-0 = $FE00-$FE1F  Available on BUS expansion connector
; I/O-1 = $FE20-$FE3F  Available on BUS expansion connector
; I/O-2 = $FE40-$FE5F  Available on BUS expansion connector
; I/O-3 = $FE60-$FE7F  Available on BUS expansion connector - Used for RTC-IDE Adapter!
; I/O-4 = $FE80-$FE9F  SC28L92 DUART resides here (only 16 bytes used)
;
; NOTE: The above hardware selects may well change with C02 Pocket SBC 3!
;**************************************************************************************************
        .ORG    $FEA0   ;Reserved space for Soft Vector and I/O initialization data
;START OF BIOS DEFAULT VECTOR DATA AND HARDWARE CONFIGURATION DATA
;
;There are 96 bytes of ROM space remaining on page $FE from $FEA0 - $FEFF
; 64 bytes of this are copied to page $03 and used for soft vectors/hardware soft configuration.
; 32 bytes are for vectors and 32 bytes are for hardware config. The last 32 bytes are only held
; in ROM and are used for hardware configuration that should not be changed.
;
;The default location for the NMI/BRK/IRQ Vector data is at $0300. They are defined at the top of
; the source file. There are 8 defined vectors and 8 vector inserts, all are free for base config.
;
;The default location for the hardware configuration data is at $0320. It is a freeform table which
; is copied from ROM to page $03. The allocated size for the hardware config table is 32 bytes.
;
VEC_TABLE      ;Vector table data for default ROM handlers
;
                .DW     NMI_VECTOR      ;NMI Location in ROM
                .DW     BRKINSTR0       ;BRK Location in ROM
                .DW     INTERUPT0       ;IRQ Location in ROM
;
                .DW     M_WARM_MON      ;NMI return handler in ROM
                .DW     M_WARM_MON      ;BRK return handler in ROM
                .DW     IRQ_EXIT0       ;IRQ return handler in ROM
;
                .DW     M_COLD_MON      ;Monitor Cold start
                .DW     M_WARM_MON      ;Monitor Warm start
;
;Vector Inserts (total of 8)
; These can be used as required. Note that the IDE init routine will use Insert 0 if a valid
; IDE controller is found and successfully initialized.
; Also, the NMI/BRK/IRQ and the Monitor routines are vectored, so these can also be extended,
; if needed, by using reserved vector locations.
;
                .DW     $FFFF           ;Insert 0 Location (used if IDE is found)
                .DW     $FFFF           ;Insert 1 Location
                .DW     $FFFF           ;Insert 2 Location
                .DW     $FFFF           ;Insert 3 Location
                .DW     $FFFF           ;Insert 4 Location
                .DW     $FFFF           ;Insert 5 Location
                .DW     $FFFF           ;Insert 6 Location
                .DW     $FFFF           ;Insert 7 Location
;
;Configuration Data - The following tables contains the default data used for:
; - Reset of the SC28L92 (RESET_28L92 routine)
; - Init of the SC28L92 (INIT_28L92 routine)
; - Basic details for register definitions are below, consult SC28L92 DataSheet
; - Note: Output Port bits OP0/OP1 must be set for RTS to work on Ports A and B
;
;Mode Register 0 definition
; Bit7          ;Rx Watchdog Control
; Bit6          ;RX-Int(2) Select
; Bit5/4        ;Tx-Int fill level
; Bit3          ;FIFO size
; Bit2          ;Baud Rate Extended II
; Bit1          ;Test 2 (don't use)
; Bit0          ;Baud Rate Extended I
;
;Mode Register 1 definition
; Bit7          ;RxRTS Control - 1 = Yes
; Bit6          ;RX-Int(1) Select
; Bit5          ;Error Mode - 0 = Character
; Bit4/3        ;Parity Mode - 10 = No Parity
; Bit2          ;Parity Type - 0 = Even (doesn't matter)
; Bit1/0        ;Bits Per Character - 11 = 8
;
;Mode Register 2 Definition
; Bit7/6        ;Channel Mode   - 00 = Normal
; Bit5          ;TxRTS Control - 0 = Yes
; Bit4          ;TxCTS Enable - 1 = Yes
; Bit3-0        ;Stop Bits - 0111 = 1 Stop Bit
;
;Baud Rate Clock Definition (Extended Mode I)
; Upper 4 bits = Receive Baud Rate
; Lower 4 bits = Transmit Baud Rate
; for 115.2K setting is %11001100
; Also set ACR Bit7 = 1 for extended rates (115.2K)
;
;Command Register Definition
; Bit7-4        ;Special commands
; Bit3          ;Disable Transmit
; Bit2          ;Enable Transmit
; Bit1          ;Disable Receive
; Bit0          ;Enable Receive
;
;Aux Control Register Definition
; Bit7          ;BRG Set Select - 1 = Extended
; Bit6-5-4      ;Counter/Timer operating mode 110 = Counter mode from XTAL
; Bit3-2-1-0    ;Enable IP3-2-1-0 Change Of State (COS) IRQ
;
;Interrupt Mask Register Definition
; Bit7          ;Input Change Interrupt 1 = On
; Bit6          ;Change Break B Interrupt 1 = On
; Bit5          ;RxRDY B Interrupt 1 = On
; Bit4          ;TxRDY B Interrupt 1 = On
; Bit3          ;Counter Ready Interrupt 1 = On
; Bit2          ;Change Break A Interrupt 1 = On
; Bit1          ;RxRDY A Interrupt 1 = On
; Bit0          ;TxRDY A Interrupt 1 = On
;
CFG_TABLE       ;Configuration table for hardware devices
;
;Data commands are sent in reverse order from list. This list is the default initialization for
; the DUART as configured for use as a Console connected to either ExtraPutty(WIN) or Serial(OSX)
; The data here is copied to page $03 and is used to configure the DUART during boot up. The soft
; data can be changed and the core INIT_28L92 routine can be called to reconfigure the DUART.
; NOTE: the register offset data is not kept in soft config memory as the initialization
; sequence should not be changed!
;
; Both serial ports are configured at startup!
; - Port A is used as the console.
; - Port B is in idle mode.
;
INIT_DUART       ;Start of DUART Initialization Data
                .DB     %00000011       ;Enable OP0/1 for RTS control Port A/B
                .DB     %00001010       ;Disable Receiver/Disable Transmitter B
                .DB     %00001001       ;Enable Receiver/Disable Transmitter A
                .DB     %00001111       ;Interrupt Mask Register setup
                .DB     %11100000       ;Aux Register setup for Counter/Timer
                .DB     %01001000       ;Counter/Timer Upper Preset
                .DB     %00000000       ;Counter/Timer Lower Preset
                .DB     %11001100       ;Baud Rate clock for Rcv/Xmt - 115.2K B
                .DB     %11001100       ;Baud Rate clock for Rcv/Xmt - 115.2K A
                .DB     %00110000       ;Reset Transmitter B
                .DB     %00100000       ;Reset Receiver B
                .DB     %00110000       ;Reset Transmitter A
                .DB     %00100000       ;Reset Receiver A
                .DB     %00000000       ;Interrupt Mask Register setup
                .DB     %11110000       ;Command Register A - disable Power Down
INIT_DUART_E    ;End of DUART Initialization Data
;
                .DB     $FF             ;Spare byte for offset to MR data
;
;Mode Register Data is defined separately. Using the loop routine above to send this data to
; the DUART does not work properly. See the description of the problem using Indexed addressing
; to load the DUART registers above. This data is also kept in soft config memory in page $03.
; Note that this data is also in reverse order for loading into MRs!
;
MR2_DAT_A       .DB     %00010111       ;Mode Register 2 data
MR1_DAT_A       .DB     %11010011       ;Mode Register 1 Data
MR0_DAT_A       .DB     %11111001       ;Mode Register 0 Data
;
MR2_DAT_B       .DB     %00010111       ;Mode Register 2 data
MR1_DAT_B       .DB     %11010011       ;Mode Register 1 Data
MR0_DAT_B       .DB     %11000001       ;Mode Register 0 Data
;
;Reserved for additional I/O devices (10 bytes free)
;
                .DB     $FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF,$FF
;
;Reset DUART Data is listed here. The sequence and commands do not require changes for any reason.
; These are maintained in ROM only. A total of 32 bytes are available for hard configuration data.
; These are the register offsets and Reset data for the DUART
;
UART_RDATA      ;DUART Reset Data for Received Break (ExtraPutty/Serial Break)
                .DB     %00000001       ;Enable Receiver
;
UART_RDATA1     ;Smaller list for entry level Reset (RESET_28L92)
                .DB     %01000000       ;Reset All Errors
                .DB     %00110000       ;Reset Transmitter
                .DB     %00100000       ;Reset Receiver
                .DB     %01010000       ;Reset Break Change Interrupt
UART_RDATAE     ;End of DUART Reset Data
;
INIT_OFFSET     ;Start of DUART Initialization Register Offsets
                .DB     $0E             ;Set Output Port bits
                .DB     $0A             ;Command Register B
                .DB     $02             ;Command Register A
                .DB     $05             ;Interrupt Mask Register
                .DB     $04             ;Aux Command Register
                .DB     $06             ;Counter Preset Upper
                .DB     $07             ;Counter Preset Lower
                .DB     $09             ;Baud Clock Register B
                .DB     $01             ;Baud Clock Register A
                .DB     $0A             ;Command Register Port B
                .DB     $0A             ;Command Register Port B
                .DB     $02             ;Command Register Port A
                .DB     $02             ;Command Register Port A
                .DB     $05             ;Interrupt Mask Register
                .DB     $02             ;Command Register Port A
INIT_OFFSETE    ;End of DUART Initialization Register Offsets
;
; BIOS 4.0x Data space for software RTC.
;
MAX_DATE                                ;Maximum days per Month+1
                .DB     32,29,32,31,32,31,32,32,31,32,31,32
;
;END OF BIOS VECTOR DATA AND HARDWARE DEFAULT CONFIGURATION DATA
;**************************************************************************************************
;START OF TOP PAGE - DO NOT MOVE FROM THIS ADDRESS!! JUMP Table starts here.
; - BIOS calls are listed below - total of 32, Reserved calls are for future hardware support
; - "B_" JUMP Tables entries are for BIOS routines, provides isolation between Monitor and BIOS
; - Two new calls used for Benchmark Timer, calls 16/17 starting with CO2BIOS 2.02
; - One additional Benchmark Timer call to allow start/stop of timer without resetting it
; - New calls added for IDE Controller and RTC starting with C02BIOS 3.04
; - New calls added for second serial port starting with C02BIOS 4.00
; - New calls added to Enable/Disable IDE Write Cache (Hitachi 3K8 Microdrive) C02BIOS 4.00
; - EXE_XLDLY has been removed, not really used, frees a Page Zero location used for IDE now
;
; NOTE: All Jump table calls add 3 clock cycles to execution time for each BIOS function.
;
        .ORG    $FF00   ;BIOS JMP Table, Cold Init and Vector handlers
;
B_IDE_RESET     JMP     IDE_RESET       ;Call 00 $FF00
B_IDE_GET_STAT  JMP     IDE_GET_STATUS  ;Call 01 $FF03
B_IDE_IDENTIFY  JMP     IDE_IDENTIFY    ;Call 02 $FF06
B_IDE_READ_LBA  JMP     IDE_READ_LBA    ;Call 03 $FF09
B_IDE_WRITE_LBA JMP     IDE_WRITE_LBA   ;Call 04 $FF0C
B_IDE_VERFY_LBA JMP     IDE_VERIFY_LBA  ;Call 05 $FF0F
B_IDE_SET_LBA   JMP     IDE_SET_LBA     ;Call 06 $FF12
B_IDE_SET_ADDR  JMP     IDE_SET_ADDRESS ;Call 07 $FF15
B_IDE_EN_CACHE  JMP     IDE_EN_CACHE    ;Call 08 $FF18
B_IDE_DIS_CACHE JMP     IDE_DIS_CACHE   ;Call 09 $FF1B
;
B_RTC_NVRD      JMP     RTC_NVRD        ;Call 10 $FF1E
B_RTC_NVWR      JMP     RTC_NVWR        ;Call 11 $FF21
B_RTC_INIT      JMP     INIT_RTC        ;Call 12 $FF24
;
B_CHRIN2        JMP     CHRIN2          ;Call 13 $FF27
B_CHROUT2       JMP     CHROUT2         ;Call 14 $FF2A
;
B_CNT_INIT      JMP     CNT_INIT        ;Call 15 $FF2D
B_CNT_STRT      JMP     CNT_STRT        ;Call 16 $FF30
B_CNT_STOP      JMP     CNT_STOP        ;Call 17 $FF33
;
B_CHRIN_NW      JMP     CHRIN_NW        ;Call 18 $FF36
B_CHRIN         JMP     CHRIN           ;Call 19 $FF39
B_CHROUT        JMP     CHROUT          ;Call 20 $FF3C
;
B_SET_DLY       JMP     SET_DLY         ;Call 21 $FF3F
B_EXE_MSDLY     JMP     EXE_MSDLY       ;Call 22 $FF42
B_EXE_LGDLY     JMP     EXE_LGDLY       ;Call 23 $FF45
;
B_RESERVE       JMP     RESERVE         ;Call 24 $FF48
;
B_INIT_VEC      JMP     INIT_VEC        ;Call 25 $FF4B
B_INIT_CFG      JMP     INIT_CFG        ;Call 26 $FF4E
B_INIT_28L92    JMP     INIT_28L92      ;Call 27 $FF51
B_RESET_28L92   JMP     RESET_28L92     ;Call 28 $FF54
;
B_WRMMNVEC0     JMP     (WRMMNVEC0)     ;Call 29 $FF57
B_CLDMNVEC0     JMP     (CLDMNVEC0)     ;Call 30 $FF5A
;
B_COLDSTRT                              ;Call 31 $FF5D
                SEI                     ;Disable Interrupts (safety)
                CLD                     ;Clear decimal mode (safety)
                LDX     #$00            ;Index for length of page (256 bytes)
PAGE0_LP        STZ     $00,X           ;Clear Page Zero
                DEX                     ;Decrement index
                BNE     PAGE0_LP        ;Loop back till done
                DEX                     ;LDX #$FF ;-)
                TXS                     ;Set Stack Pointer
;
                JSR     INIT_PG03       ;Xfer default Vectors/HW Config to Page $03
                JSR     INIT_IO         ;Init I/O - DUART (Console/Timer)
;
; Send BIOS init msg to console - note: X Reg is zero on return from INIT_IO
;
BMSG_LP         LDA     BIOS_MSG,X      ;Get BIOS init msg
                BEQ     CHECK_IO        ;If zero, msg done, Test for extra I/O
                JSR     CHROUT          ;Send to console
                INX                     ;Increment Index
                BRA     BMSG_LP         ;Loop back until done
CHECK_IO
                JSR     DETECT_RTC      ;Detect and Init RTC
                JSR     DETECT_IDE      ;Detect and Init IDE
                JSR     RESERVE         ;Reserve one more Init routine for future use
                BRA     B_CLDMNVEC0     ;Branch to Coldstart Monitor
;
IRQ_VECTOR                              ;This is the ROM start for the BRK/IRQ handler
                PHA                     ;Save A Reg (3)
                PHX                     ;Save X Reg (3)
                PHY                     ;Save Y Reg (3)
                TSX                     ;Get Stack pointer (2)
                LDA     $0100+4,X       ;Get Status Register (4)
                AND     #$10            ;Mask for BRK bit set (2)
                BNE     DO_BRK          ;If set, handle BRK (2/3)
                JMP     (IRQVEC0)       ;Jump to Soft vectored IRQ Handler (6)
DO_BRK          JMP     (BRKVEC0)       ;Jump to Soft vectored BRK Handler (6)
NMI_ROM         JMP     (NMIVEC0)       ;Jump to Soft vectored NMI handler (6)
;
;This is the standard return for the IRQ/BRK handler routines
;
IRQ_EXIT0       PLY                     ;Restore Y Reg (4)
                PLX                     ;Restore X Reg (4)
                PLA                     ;Restore A Reg (4)
                RTI                     ;Return from IRQ/BRK routine (6)
;
INIT_PG03       JSR     INIT_VEC        ;Init the Soft Vectors first
INIT_CFG        LDY     #$40            ;Get offset to Config data
                BRA     DATA_XFER       ;Go move the Config data to page $03
INIT_VEC        LDY     #$20            ;Get offset to Vector data
;
DATA_XFER       SEI                     ;Disable Interrupts, can be called via JMP table
                LDX     #$20            ;Set count for 32 bytes
DATA_XFLP       LDA     VEC_TABLE-1,Y   ;Get ROM table data
                STA     SOFTVEC-1,Y     ;Store in Soft table location
                DEY                     ;Decrement index
                DEX                     ;Decrement count
                BNE     DATA_XFLP       ;Loop back till done
                CLI                     ;Re-enable interrupts
RESERVE         RTS                     ;Return to caller
;
RTC_MSG
;
;This is a short BIOS message that is displayed when the DS15x1 RTC is found
                .DB     "RTC found"
                .DB     $0D,$0A,$00
;
IDE_MSG
;
;This is a short BIOS message that is displayed when the IDE controller is found
                .DB     "IDE found"
                .DB     $0D,$0A,$00
;
;The offset data here is used as an index to the Identity Block of Data from the IDE controller
LBA_OFFSET      .DB     120,121,122,123 ;Offset Data for LBA Size
;
;This BIOS version does not rely on CPU clock frequency for RTC timing. Timing is based on the
; SC28L92 DUART Timer/Counter which has a fixed frequency of 3.6864MHz. Jiffy clock set at 10ms.
; Edit Displayed clock rate at CPU_CLK below as needed if running "other" than 8MHz.
;
        .ORG    $FFD0   ;Hard coded BIOS message to the top of memory (Monitor uses this)
;
;BIOS init message - sent before jumping to the monitor coldstart vector.
; CO2BIOS 2.01 and later, provides a BIOS message with more detail, fixed length/location!
;
BIOS_MSG        .DB     $0D,$0A         ;CR/LF
                .DB     "C02BIOS 4.01"  ;Updated Release Version
                .DB     $0D,$0A         ;CR/LF
                .DB     "W65C02@"       ;Display CPU type
CPU_CLK         .DB     "8MHz"          ;Displayed CPU Clock frequency
                .DB     $0D,$0A         ;CR/LF
                .DB     "27/01/2022"    ;DD/MM/YYYY
                .DB     $0D,$0A,$00     ;CR/LF and terminate string
;
        .ORG    $FFFA   ;65C02 Vectors:
                .DW     NMI_ROM         ;NMI
                .DW     B_COLDSTRT      ;RESET
                .DW     IRQ_VECTOR      ;IRQ/BRK
        .END