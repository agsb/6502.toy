.IF 0

    ;
    ;	LCD Display codes
    ;  For Hex Code-01, the LCD command will be the clear LCD screen
    ;  For Hex Code-02, the LCD command will be returning home
    ;  For Hex Code-04, the LCD command will be decrement cursor
    ;  For Hex Code-06, the LCD command will be Increment cursor
    ;  For Hex Code-05, the LCD command will be Shift display right
    ;  For Hex Code-07, the LCD command will be Shift display left
    ;  For Hex Code-08, the LCD command will be Display off, cursor off
    ;  For Hex Code-0A, the LCD command will be cursor on and display off
    ;  For Hex Code-0C, the LCD command will be cursor off, display on
    ;  For Hex Code-0E, the LCD command will be cursor blinking, Display on
    ;  For Hex Code-0F, the LCD command will be cursor blinking, Display on
    ;  For Hex Code-10, the LCD command will be Shift cursor position to left
    ;  For Hex Code-14, the LCD command will be Shift cursor position to the right
    ;  For Hex Code-18, the LCD command will be Shift the entire display to the left
    ;  For Hex Code-1C, the LCD command will be Shift the entire display to the right
    ;  For Hex Code-80, the LCD command will be Force cursor to the beginning ( 1st line)
    ;  For Hex Code-C0, the LCD command will be Force cursor to the beginning ( 2nd line)
    ;  For Hex Code-38, the LCD command will be 2 lines and 5×7 matrix


    lda #%00111000                              ; set 8-bit mode, 2-line display, 5x8 font
    jsr LCD__send_instruction

    lda #%00001110                              ; display on, cursor on, blink off
    jsr LCD__send_instruction
    
    lda #%00000110                              ; increment and shift cursor, don't shift display
    jmp LCD__send_instruction

.ENDIF
