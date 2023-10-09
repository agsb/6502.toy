    ; Secret Sauce - - -
    ; 1-bit audio playback
    ; Michael Pohoreski, 9 de jul. de 2017,

    Speaker = $C030
    WAIT = $FCA8
    
    ORG $300
    
_beep:
    LDX #0
    STX GetNote+1 ; *** SELF-MODIFYING
    LDY #$14
    STY GetNote+2 ; *** SELF-MODIFYING
    BNE @GetNote
@SamePage:
    LDY #8 ; BitsPerByte
@SameByte:
    PHA
    LDA #8 ; Delay, Default=4
    JSR @WAIT
    PLA
    ASL
    NOP
    BCC @Delay
    STA @Speaker
    BCS @Delay
@Delay:
    NOP
    NOP
    NOP
    DEY
    BNE @SameByte
@GetNote:
    LDA $DA1A,X ; *** SELF-MODIFIED!
    INX
    BNE @SamePage
    INC GetNote+2
    LDA GetNote+2
    CMP #$6A ; End of File, Default=#$96
    BNE @SamePage
    RTS 

