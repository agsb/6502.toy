;**************************************************************************************************
;*                                                                                                *
;*        Header File for writing code to be used with the C02BIOS and C02Monitor programs        *
;*                                                                                                *
;*              Equates and variables used in the C02BIOS and Monitor programs                    *
;*                                                                                                *
;*                  Updated for BIOS and Monitor versions starting at 3.00                        *
;*                                                                                                *
;*      This base of code should be at the start of any user created code. It provides            *
;*      equates to all known Page zero locations, subroutines and variables used.                 *
;*      This greatly simplifies access to any required resources in the existing code.            *
;*                                                                                                *
;**************************************************************************************************
        PL      66      ;Page Length
        PW      132     ;Page Width (# of char/line)
        CHIP    W65C02S ;Enable WDC 65C02 instructions
        PASS1   OFF     ;Set ON when used for debug
;**************************************************************************************************
; Page Zero definitions $00 to $9F reserved for user routines
; NOTES:- Locations $00 and $01 are used to zero RAM (calls CPU reset)
;       - EEPROM Byte Write routine loaded into Page Zero at $90-$A4
;
;       - Page Zero definitions $00 to $9F reserved for user routines
PGZERO_ST       .EQU    $A0             ;Start of Page Zero usage for C02 Monitor
;
BUFF_PG0        .EQU    PGZERO_ST+00    ;Default Page zero location for Monitor buffers
;
;       - 16-bit C02 Monitor variables required access for BIOS:
INDEXL          .EQU    PGZERO_ST+16    ;Index for address - multiple routines
INDEXH          .EQU    PGZERO_ST+17
;
IDE_STATUS_RAM  .EQU    $D0             ;Test status byte
;       - BIOS variables, pointers, flags located at top of Page Zero
BIOS_PG0        .EQU    $D8             ;Start of BIOS page zero use ($D8-$FF, 40 bytes total)
;
;       - BRK handler routine
PCL             .EQU    BIOS_PG0+00     ;Program Counter Low index
PCH             .EQU    BIOS_PG0+01     ;Program Counter High index
PREG            .EQU    BIOS_PG0+02     ;Temp Status Reg
SREG            .EQU    BIOS_PG0+03     ;Temp Stack ptr
YREG            .EQU    BIOS_PG0+04     ;Temp Y Reg
XREG            .EQU    BIOS_PG0+05     ;Temp X Reg
AREG            .EQU    BIOS_PG0+06     ;Temp A Reg
;
;       - 2691 IRQ handler pointers and status
ICNT            .EQU    BIOS_PG0+07     ;Input buffer count
IHEAD           .EQU    BIOS_PG0+08     ;Input buffer head pointer
ITAIL           .EQU    BIOS_PG0+09     ;Input buffer tail pointer
OCNT            .EQU    BIOS_PG0+10     ;Output buffer count
OHEAD           .EQU    BIOS_PG0+11     ;Output buffer head pointer
OTAIL           .EQU    BIOS_PG0+12     ;Output buffer tail pointer
UART_IRT        .EQU    BIOS_PG0+13     ;2691 Interrupt Status byte
UART_SRT        .EQU    BIOS_PG0+14     ;2691 Status Register byte
;
;       - Real-Time Clock variables
; These are repurposed for adding a Realtime clock chip DS1511Y
; The Ticks, Seconds, Minutes and Hours remain the same in function.
; The 16-bit Days variable is replaced however.
; - The DAY_DATE is a new variable. To minimize Page Zero usage, it has two functions
;       Bits 0-4 represent the days of the Month 1-31
;       Bits 5-7 represent the Day of the Week, 1-7 (Monday=1)
; The Months are handled by the upper 4 bits of the MONTH_YEAR variable
; The Century is handled by a the Year (0-255) and the lower 4 bits of the MONTH_YEAR variable
TICKS           .EQU    BIOS_PG0+15     ;Number of timer countdowns = 1 second (100)
SECS            .EQU    BIOS_PG0+16     ;Seconds: 0-59
MINS            .EQU    BIOS_PG0+17     ;Minutes: 0-59
HOURS           .EQU    BIOS_PG0+18     ;Hours: 0-23
DAY_DATE        .EQU    BIOS_PG0+19     ;Day: (bits 5-7) Date: (bits 0-4)
MONTH_CENTURY   .EQU    BIOS_PG0+20     ;Month: (bits 4-7) Century: (bits 0-3)
YEAR            .EQU    BIOS_PG0+21     ;Century 0-255 plus 4 bits as noted above
RTC_TEMP        .EQU    BIOS_PG0+22     ;Temp work byte for updating shared variables
;
;       - Delay Timer variables
MSDELAY         .EQU    BIOS_PG0+23     ;Timer delay countdown byte (255 > 0)
SETMS           .EQU    BIOS_PG0+24     ;Set timeout for delay routines - BIOS use only
DELLO           .EQU    BIOS_PG0+25     ;Delay value BIOS use only
DELHI           .EQU    BIOS_PG0+26     ;Delay value BIOS use only
XDL             .EQU    BIOS_PG0+27     ;XL Delay count
;
;       - Count variables for 10ms benchmark timing
10MS_CNT        .EQU    BIOS_PG0+28     ;10ms Count variable
SECL_CNT        .EQU    BIOS_PG0+29     ;Seconds Low byte count
SECH_CNT        .EQU    BIOS_PG0+30     ;Second High byte count
;
;       - Adddress and pointers for CF-Card IDE Interface
LBA_ADDR_LOW    .EQU    BIOS_PG0+31     ;LBA Transfer Address low byte
LBA_ADDR_HIGH   .EQU    BIOS_PG0+32     ;LBA Transfer Address high byte
LBA_XFER_CNT    .EQU    BIOS_PG0+33     ;LBA Transfer Count

LBA_LOW_BYTE    .EQU    BIOS_PG0+34     ;LBA Block number 0-7
LBA_HIGH_BYTE   .EQU    BIOS_PG0+35     ;LBA Block number 8-15
LBA_EXT_BYTE    .EQU    BIOS_PG0+36     ;LBA Block number 16-23
;
BIOS_XFERL      .EQU    BIOS_PG0+37     ;BIOS Move Routine low byte
BIOS_XFERH      .EQU    BIOS_PG0+38     ;BIOS Move Routine high byte
;
;       - Timer/Counter Match flag for Delay/Benchmark
MATCH           .EQU    BIOS_PG0+39     ;Bit7 used for Delay, Bit6 used for Benchmark
                                        ;Bits 4,5 used for BRG Test register status
                                        ;Bits 3,2,1 used for CF Card Interrupt Handler
;
;       - Default for RTC tick count - number of IRQs for 1 second
DF_TICKS        .EQU    #100            ;counter/timer is 10 milliseconds (100 x 10ms = 1 second)
;
;**************************************************************************************************
IBUF            .EQU    $0200           ;Console Input Buffer - 128 bytes
OBUF            .EQU    $0280           ;Console Output Buffer - 128 bytes
;**************************************************************************************************
SOFTVEC         .EQU    $0300           ;Start of soft vectors
;The Interrupt structure is vector based. During startup, Page $03 is loaded from ROM
; The soft vectors are structured to allow inserting additional routines either before
; or after the core ROM routines. This allows flexibility and changing of routine priority
;
;The main set of vectors occupy the first 16 bytes of Page $03. The ROM handler for
; NMI, BRK and IRQ jump to the first 3 vectors. The following 3 vectors are loaded with
; return addresses to the ROM handler for each. The following 2 vectors are the cold and
; warm entry points for the Monitor. After the basic initialization, the monitor is entered.
;
;The following vector set allows inserts, pre or post for NMI/BRK/IRQ. There a total of 8 inserts
; which occupy 16 bytes. They can be used as required. Currently, all of these are available.
;
NMIVEC0         .EQU    SOFTVEC+00      ;NMI Vector Entry 0
BRKVEC0         .EQU    SOFTVEC+02      ;BRK Vector Entry 0
IRQVEC0         .EQU    SOFTVEC+04      ;IRQ Vector Entry 0
;
NMIRTVEC0       .EQU    SOFTVEC+06      ;NMI Vector Return 0
BRKRTVEC0       .EQU    SOFTVEC+08      ;BRK Vector Return 0
IRQRTVEC0       .EQU    SOFTVEC+10      ;IRQ Vector Return 0
;
CLDMNVEC0       .EQU    SOFTVEC+12      ;Monitor Cold Entry Vector 0
WRMMNVEC0       .EQU    SOFTVEC+14      ;Monitor Warm Entry Vector 0
;
VECINSRT0       .EQU    SOFTVEC+16      ;1st Vector Insert
VECINSRT1       .EQU    SOFTVEC+18      ;2nd Vector Insert
VECINSRT2       .EQU    SOFTVEC+20      ;3rd Vector Insert
VECINSRT3       .EQU    SOFTVEC+22      ;4th Vector Insert
VECINSRT4       .EQU    SOFTVEC+24      ;5th Vector Insert
VECINSRT5       .EQU    SOFTVEC+26      ;6th Vector Insert
VECINSRT6       .EQU    SOFTVEC+28      ;7th Vector Insert
VECINSRT7       .EQU    SOFTVEC+30      ;8th Vector Insert
;
;**************************************************************************************************
SOFTCFG         .EQU    SOFTVEC+32      ;Start of hardware config parameters
;Soft Config values below are loaded from ROM and are the default I/O setup configuration data that
; the INIT_x routines use. As a result, you can write a routine to change the I/O configuration
; data and use the standard ROM routines to initialize the I/O without restarting or changing ROM
; A Reset (HW or coded) will reinitialize the I/O with the ROM default I/O configuration.
;There are a total of 32 Bytes configuration data reserved starting at $0320
;
LOAD_2691       .EQU    SOFTCFG+00              ;SCC2691 Soft Config Data
;
LOAD_IDE        .EQU    SOFTCFG+16              ;IDE/CF-Card Soft Config Data
;
;**************************************************************************************************
IOPAGE          .EQU    $FE00                   ;I/O Page Base Start Address
;**************************************************************************************************
SCC2691_BASE    .EQU    IOPAGE+$80              ;Beginning of Console UART address
;
UART_MODEREG    .EQU    SCC2691_BASE+$00        ;MR1/MR2 same address, sequential read/write
UART_STATUS     .EQU    SCC2691_BASE+$01        ;UART Status Register (READ)
UART_CLKSEL     .EQU    SCC2691_BASE+$01        ;UART Clock Select Register (WRITE)
UART_BRGTST     .EQU    SCC2691_BASE+$02        ;UART BRG Test Register (READ)
UART_COMMAND    .EQU    SCC2691_BASE+$02        ;UART Command Register (WRITE)
UART_RECEIVE    .EQU    SCC2691_BASE+$03        ;UART Receive Register (READ)
UART_TRANSMIT   .EQU    SCC2691_BASE+$03        ;UART Transmit Register (WRITE)
UART_CLKTEST    .EQU    SCC2691_BASE+$04        ;X1/X16 Test Register (READ)
UART_AUXCR      .EQU    SCC2691_BASE+$04        ;Aux Command Register (WRITE)
UART_ISR        .EQU    SCC2691_BASE+$05        ;Interrupt Status Register (READ)
UART_IMR        .EQU    SCC2691_BASE+$05        ;Interrupt Mask Register (WRITE)
UART_CNTU       .EQU    SCC2691_BASE+$06        ;Counter/Timer Upper Register (READ)
UART_CNTUP      .EQU    SCC2691_BASE+$06        ;Counter/Timer Upper Preset Register (WRITE)
UART_CNTL       .EQU    SCC2691_BASE+$07        ;Counter/Timer Lower Register (READ)
UART_CNTLP      .EQU    SCC2691_BASE+$07        ;Counter/Timer Lower Preset Register (WRITE)
;
;Additional Hardware
; Adding BIOS definitions for Realtime Clock chip - DS1511Y
; uses the first 16 addresses for RTC registers and basic operation
; uses two addresses for extended RAM of 256 bytes
;
; upper addresses are used for a 16-bit IDE interface (below)
; NOTE: offset $11 and $12 are unused (reserved per the datasheet).
;
RTC_IDE_BASE    .EQU    IOPAGE+$60              ;Beginning of Realtime Clock address
;
RTC_SECONDS     .EQU    RTC_IDE_BASE+$00        ;Seconds in BCD 00-59
RTC_MINUTES     .EQU    RTC_IDE_BASE+$01        ;Minutes in BCD 00-59
RTC_HOURS       .EQU    RTC_IDE_BASE+$02        ;Hours in BCD 00-23
RTC_DAY         .EQU    RTC_IDE_BASE+$03        ;Day in BCD 1-7
RTC_DATE        .EQU    RTC_IDE_BASE+$04        ;Date in BCD 1-31
RTC_MONTH       .EQU    RTC_IDE_BASE+$05        ;Month in BCD 1-12
RTC_YEAR        .EQU    RTC_IDE_BASE+$06        ;Year in BCD 00-99
RTC_CENTURY     .EQU    RTC_IDE_BASE+$07        ;Century in BCD 00-39
RTC_ALARM_SEC   .EQU    RTC_IDE_BASE+$08        ;Alarm Seconds in BCD 00-59
RTC_ALARM_MIN   .EQU    RTC_IDE_BASE+$09        ;Alarm Minutes in BCD 00-59
RTC_ALARM_HRS   .EQU    RTC_IDE_BASE+$0A        ;Alarm Hours in BCD 00-23
RTC_ALARM_DYDT  .EQU    RTC_IDE_BASE+$0B        ;Alarm Day/Date in BCD 0-7 1-31
RTC_WTCHDOG_01  .EQU    RTC_IDE_BASE+$0C        ;Watchdog 0.1 / 0.01 Seconds in BCD 00-99
RTC_WTCHDOG_10  .EQU    RTC_IDE_BASE+$0D        ;Watchdog 10 / 1 Seconds in BCD 00-99
RTC_CONTROL_A   .EQU    RTC_IDE_BASE+$0E        ;Control A
RTC_CONTROL_B   .EQU    RTC_IDE_BASE+$0F        ;Control B
RTC_RAM_ADDR    .EQU    RTC_IDE_BASE+$10        ;Extended RAM address
RTC_RAM_DATA    .EQU    RTC_IDE_BASE+$13        ;Extended RAM data
;
; Adding BIOS definitions for 16-bit IDE interface
; uses two addresses for Upper Byte Latch read / write
; uses eight addresses for Command Block Registers
; uses two addresses for Control Block Registers
;
IDE_16_READ     .EQU    RTC_IDE_BASE+$14        ;Upper byte Read address
IDE_16_WRITE    .EQU    RTC_IDE_BASE+$15        ;Upper byte Write address
;
; Adding BIOS definitions for IDE Controller (HARD DISK, Flash Module, etc.)
; Hardware Adapter provides a 16-bit IDE Port per:
;        Seagate ATA Interface Reference Manual 36111-001, Rev. C (21st May 1993)
;
; Compact Flash Adapter BIOS (here) is based on documentation from SanDisk:
;       OEM Product Manual Version 12.0 Doc # 20-10-00038m 02/2007
;
; Control Block Registers
IDE_ALT_STATUS  .EQU    RTC_IDE_BASE+$16        ;Alternate Status Register (READ)
IDE_DEV_CTRL    .EQU    RTC_IDE_BASE+$16        ;Device Control Register (WRITE)
IDE_DRV_ADDR    .EQU    RTC_IDE_BASE+$17        ;Drive Address Register (READ)
;
; Command Block Registers
IDE_DATA        .EQU    RTC_IDE_BASE+$18        ;Data Register (R/W)
IDE_ERROR       .EQU    RTC_IDE_BASE+$19        ;Error Register (READ)
IDE_FEATURE     .EQU    RTC_IDE_BASE+$19        ;Feature Register (WRITE)
IDE_SCT_CNT     .EQU    RTC_IDE_BASE+$1A        ;Sector Count Register
IDE_SCT_NUM     .EQU    RTC_IDE_BASE+$1B        ;Sector Number Register
IDE_CYL_LOW     .EQU    RTC_IDE_BASE+$1C        ;Cylinder Low Register
IDE_CYL_HIGH    .EQU    RTC_IDE_BASE+$1D        ;Cylinder High Register
IDE_DRV_HEAD    .EQU    RTC_IDE_BASE+$1E        ;Drive/Head Register
IDE_STATUS      .EQU    RTC_IDE_BASE+$1F        ;Status Register (READ)
IDE_COMMAND     .EQU    RTC_IDE_BASE+$1F        ;Command Register (WRITE)
;
;**************************************************************************************************
;Search Buffer is 16 bytes in length. Used to hold search string for text or hex data
SRCHBUFF        .EQU    $340    ;Located in Page $03 following HW config data
;
;Xmodem/CRC Loader also provides Motorola S19 Record sense and load. Designed to handle the S19
; records from the WDC Assembler/Linker package. This requires a 44 byte buffer to parse each valid
; S1 record, located just before the 132 Byte Xmodem frame buffer. Total Buffer space for the
; Xmodem/CRC Loader is 176 bytes
;
;Valid S-record headers are "S1" and "S9"/ For S1, the maximum length is "19" hex. The last S1 record
; can be less. S9 record is always the last record with no data. WDC Linker also appends a CR/LF to
; the end of each record for a total 44 bytes.
SRBUFF          .EQU    $0350   ;Start of Motorola S-record buffer, 44 bytes in length
;
;Xmodem frame buffer. The entire Xmodem frame is buffered here and then checked for proper header and
; frame number, CRC-16 on the data, then moved to user RAM.
RBUFF           .EQU    $037C   ;Xmodem temp 132 byte receive buffer
;
;Page $03 is completely allocated for Buffers, Config Data and Vector pointers.	Much of this can be
; used as temporary buffer space as needed provided the	Monitor functions that required are not
; being used concurrently.
;
;XMODEM Control Character Constants
SOH             .EQU    $01     ;Start of Block Header
EOT             .EQU    $04     ;End of Text marker
ACK             .EQU    $06     ;Good Block Acknowledge
NAK             .EQU    $15     ;Bad Block acknowledged
CAN             .EQU    $18     ;Cancel character
;
BIOS_MSG        .EQU    $FFD0   ;BIOS Startup Message is hard-coded here
;**************************************************************************************************
; RAM location used for the EEPROM Byte Write routine
; location moved from $00 to $90 to avoid conflict with EhBasic Page 0 usage
;
BURN_BYTE       .EQU    $0080   ;Location in Page 0 RAM for EEPROM BYTE write routine
;**************************************************************************************************
;The following 32 functions are provided by BIOS via the JMP Table below
; $FF21 - $FF2D are Reserved for future expansion
;
; $FF00 IDE_RES_DIAG    ;Reset IDE and Run Diagnostics
; $FF03 IDE_GET_STATUS  ;Get IDE Status and Extended Error codes
; $FF06 IDE_IDENTIFY    ;Get IDE Identification Block
; $FF09 IDE_READ_LBA    ;Read a Block from IDE device
; $FF0C IDE_WRITE_LBA   ;Write a Block to IDE device
; $FF0F IDE_VERIFY_LBA  ;Verify the last Block from IDE device
; $FF12 IDE_SET_LBA     ;Set the LBA Block ID for Read/Write/Verify
; $FF15 IDE_SET_ADDRESS ;Set the Memory Address to transfer Block data to/from 
;
; $FF18 RTC_NVRD        ;Read the NVRAM from the RTC
; $FF1B RTC_NVWR        ;Write the NVRAM to the RTC
; $FF1E RTC_INIT        ;Initialize the Software RTC from the hardware RTC
;
; $FF30 CNT_STRT        ;Reset/Start benchmark counter
; $FF33 CNT_STOP        ;Stop benchmark counter
; $FF36 CHRIN_NW        ;Data input from console, no waiting, clear carry if none
; $FF39 CHRIN           ;Data input from console, carry set if data
; $FF3C CHROUT          ;Data output to console, sent data preserved
; $FF3F SET_DLY         ;Set delay value for milliseconds and 16-bit counter
; $FF42 EXE_MSDLY       ;Execute millisecond delay 1-256 * 10 milliseconds
; $FF45 EXE_LGDLY       ;Execute long delay; millisecond delay * 16-bit count
; $FF48 EXE_XLDLY       ;Execute extra long delay; 8-bit count * long delay
; $FF4B INIT_VEC        ;Initialize soft vectors at $0300 from ROM
; $FF4E INIT_CFG        ;Initialize soft config values at $0320 from ROM
; $FF51 INIT_2691       ;Initialize SCC2691 console 38.4K, 8-N-1 RTS/CTS
; $FF54 RESET_2691      ;Reset SCC2691 - called before INIT_2691
; $FF57 MON_WARM        ;Monitor warm start - jumps to page $03
; $FF5A MON_COLD        ;Monitor cold start - jumps to page $03
; $FF5D COLDSTRT        ;System cold start - RESET vector for 65C02
;
;**************************************************************************************************
;       BIOS JUMP Table starts at $FF00
;       - BIOS calls are listed below - total of 32
;       - Reserved calls are for future hardware support
;**************************************************************************************************
;
B_IDE_RESET     .EQU    $FF00   ;Call 00
B_IDE_GET_STAT  .EQU    $FF03   ;Call 01
B_IDE_IDENTIFY  .EQU    $FF06   ;Call 02
B_IDE_READ_LBA  .EQU    $FF09   ;Call 03
B_IDE_WRITE_LBA .EQU    $FF0C   ;Call 04
B_IDE_VERFY_LBA .EQU    $FF0F   ;Call 05
B_IDE_SET_LBA   .EQU    $FF12   ;Call 06
B_IDE_SET_ADDR  .EQU    $FF15   ;Call 07
;
B_RTC_NVRD      .EQU    $FF18   ;Call 08
B_RTC_NVWR      .EQU    $FF1B   ;Call 09
B_RTC_INIT      .EQU    $FF1E   ;Call 10
;
B_Reserve11     .EQU    $FF21   ;Call 11
B_Reserve12     .EQU    $FF24   ;Call 12
B_Reserve13     .EQU    $FF27   ;Call 13
B_Reserve14     .EQU    $FF2A   ;Call 14
B_Reserve15     .EQU    $FF2D   ;Call 15
;
B_CNT_STRT      .EQU    $FF30   ;Call 16
B_CNT_STOP      .EQU    $FF33   ;Call 17
;
B_CHRIN_NW      .EQU    $FF36   ;Call 18
B_CHRIN         .EQU    $FF39   ;Call 19
B_CHROUT        .EQU    $FF3C   ;Call 20
;
B_SET_DLY       .EQU    $FF3F   ;Call 21
B_EXE_MSDLY     .EQU    $FF42   ;Call 22
B_EXE_LGDLY     .EQU    $FF45   ;Call 23
B_EXE_XLDLY     .EQU    $FF48   ;Call 24
;
B_INIT_VEC      .EQU    $FF4B   ;Call 25
B_INIT_CFG      .EQU    $FF4E   ;Call 26
B_INIT_2691     .EQU    $FF51   ;Call 27
B_RESET_2691    .EQU    $FF54   ;Call 28
;
B_WRMMNVEC0     .EQU    $FF57   ;Call 29
B_CLDMNVEC0     .EQU    $FF5A   ;Call 30
B_COLDSTRT      .EQU    $FF5D   ;Call 31
;
;**************************************************************************************************
;The following 32 functions are provided by the Monitor via the JMP Table below
; $E006 - $E00C are Reserved for future expansion
;
; $E000 COLD_Monitor    ;Monitor Cold start entry
; $E003 WARM_Monitor    ;Monitor Warm start entry
;
; $E006 RESERVE2        ;Reserved function call
; $E009 RESERVE3        ;Reserved function call
; $E00C RESERVE4        ;Reserved function call
;
; $E00F BSOUT           ;Send a Backspace to console
; $E012 XMDM_SAVE       ;Xmodem Save - entry point for external use
; $E015 XMDM_LOAD       ;Xmodem Load - entry point for external use
; $E018 BENCH           ;Start Benchmark Timer (resets count)
; $E01B QUITB           ;Quits Benchmark Timer (displays elapsed time)
; $E01E DATETIME        ;Displays SW-RTC Date/Time
; $E021 PRSTAT          ;Displays Processor Status
; $E024 DIS_LINE        ;Disassembles a line of code (Mnemonic/Operand)
; $E027 INCINDEX        ;Increments 16-bit Index pointer
; $E02A DECINDEX        ;Decrements 16-bit Index pointer
; $E02D RDLINE          ;Read a Line of Data from console
; $E030 RDCHAR          ;Read a Character from console
; $E033 HEXIN2          ;Get two Hex digits from console entry (1-byte)
; $E036 HEXIN4          ;Get four Hex digits from console entry (2-bytes)
; $E039 HEX2ASC         ;Converts Hex into ASCII (1-byte to 2-ASCII digits)
; $E03C BIN2ASC         ;Converts Binary to ASCII
; $E03F ASC2BIN         ;Converts two ASCII digits to one byte
; $E042 BEEP            ;Send a Bell character to console
; $E045 DOLLAR          ;Send a $ character to console
; $E048 CROUT           ;Send a CR/LF sequence to console
; $E04B SPC             ;Send a Space character to console
; $E04E PRBYTE          ;Print a byte as two ASCII digits to console
; $E051 PRWORD          ;Print a word as four ASCII digits to console
; $E054 PRASC           ;Print ASCII character to console
; $E057 PROMPT          ;Print a Message number to console
; $E05A PROMPTR         ;Print a String to console (via 16-bit address)
; $E05D CONTINUE        ;Prompt to continue, else pull return address from stack and return
;
;**************************************************************************************************
;       Monitor JUMP table starts at $E000
;       - Monitor calls are listed below - total of 32
;       - Reserved calls are for future functions
;**************************************************************************************************
;
M_COLD_MON      .EQU    $E000   ;Call 00
M_WARM_MON      .EQU    $E003   ;Call 01
;
M_RESERVE2      .EQU    $E006   ;Call 02
M_RESERVE3      .EQU    $E009   ;Call 03
M_RESERVE4      .EQU    $E00C   ;Call 04
;
M_BSOUT         .EQU    $E00F   ;Call 05
M_XMDM_SAVE     .EQU    $E012   ;Call 06
M_XMDM_LOAD     .EQU    $E015   ;Call 07
M_BENCH         .EQU    $E018   ;Call 08
M_QUITB         .EQU    $E01B   ;Call 09
M_UPTIME        .EQU    $E01E   ;Call 10
M_PRSTAT1       .EQU    $E021   ;Call 11
M_DIS_LINE      .EQU    $E024   ;Call 12
M_INCINDEX      .EQU    $E027   ;Call 13
M_DECINDEX      .EQU    $E02A   ;Call 14
M_RDLINE        .EQU    $E02D   ;Call 15
M_RDCHAR        .EQU    $E030   ;Call 16
M_HEXIN2        .EQU    $E033   ;Call 17
M_HEXIN4        .EQU    $E036   ;Call 18
M_HEX2ASC       .EQU    $E039   ;Call 19
M_BIN2ASC       .EQU    $E03C   ;Call 20
M_ASC2BIN       .EQU    $E03F   ;Call 21
M_BEEP          .EQU    $E042   ;Call 22
M_DOLLAR        .EQU    $E045   ;Call 23
M_CROUT         .EQU    $E048   ;Call 24
M_SPC           .EQU    $E04B   ;Call 25
M_PRBYTE        .EQU    $E04E   ;Call 26
M_PRWORD        .EQU    $E051   ;Call 27
M_PRASC         .EQU    $E054   ;Call 28
M_PROMPT        .EQU    $E057   ;Call 29
M_PROMPTR       .EQU    $E05A   ;Call 30
M_CONTINUE      .EQU    $E05D   ;Call 31
;
;**************************************************************************************************
;
;       User program code can start here. Default to $0800, can be changed as required
;
;**************************************************************************************************
;
        .ORG    $0800   ;Start of User RAM for programs
;
        .END

