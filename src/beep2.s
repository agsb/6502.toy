
No source was give. Manual disassembly for RECORDER.PLAYER
(I've documented the player portion previously.)

temp EQU $FF
WAIT EQU $FCA8
TAPEIN EQU $C060
DATA_BEG EQU $0800
DATE_END EQU $9600

ORG $300

LDY #>DATA_BEG
STY Dst+2
LDX #<DATA_BEG
STX Dst+1
STX temp

SamePage
LDY #8
Bits
PHA
LDA #4
JSR WAIT
CLC
LDA TAPEIN
EOR temp
BPL Low
EOR temp
STA temp
SEC
Low
PLA
ROL
DEY
BNE Bits
INX
Dst
STA $0800,X ; **SELF-MODIFIED**
INX
BNE SamePage
INC Dst+2
LDA Dst+2
CMP #>DATE_END
BNE SamePage
RTS 


