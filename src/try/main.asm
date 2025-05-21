; vim: filetype=asm sw=8 ts=8 autoindent expandtab shiftwidth=8 et:
;-----------------------------------------------------------------------
; Copyright (c) 2023, Alvaro Gomes Sobral Barcellos
; All rights reserved.              
; 
; Redistribution and use in source and binary forms, with or without
; modification, are permitted provided that the following conditions
; are met:
; 
; 1. Redistributions of source code must retain the above copyright 
;    notice, this list of conditions and the following disclaimer.
;
; 2. Redistributions in binary form must reproduce the above copyright
;    notice, this list of conditions and the following disclaimer in 
;    the documentation and/or other materials provided with the 
;    distribution.
; 
; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT 
; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS 
; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE 
; COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
; BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES, LOSS
; OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED 
; AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT 
; LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN 
; ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
; POSSIBILITY OF SUCH DAMAGE.
;-----------------------------------------------------------------------
;
; please vide notes.md
;
;-----------------------------------------------------------------------
;  ca65 assembler 
;----------------------------------------------------------------------

; identifiers 6502

.p02

.feature c_comments

.feature string_escapes

.feature org_per_seg

.feature dollar_is_pc

.feature pc_assignment

; .case +

;-----------------------------------------------------------------------
main:

; plugin modules

init_of:

.include "pagging.s"

exit_of:

.end

;---------------------------------------------------------------------
;  init of lib6502 emulator 
;---------------------------------------------------------------------
getc:
        lda $E000

eofs:
        cmp #$FF 
        beq byes

putc:
        sta $E000
        rts

; exit from emulator
byes:
        jmp $0000

;---------------------------------------------------------------------
;  end of lib6502 emulator 
;---------------------------------------------------------------------

;--------------------------------------------------------
; at $FFFA
;.segment "VECTORS"

* = $FFFA

; hardware jumpers
rom_nmi:
.addr jump_nmi   ; fa ROM NMI vector

rom_rst:
.addr jump_rst   ; fc ROM Reset vector

rom_irq:
.addr jump_irq   ; fe ROM IRQ/BRK vector

;----------------------------------------------------------------------

