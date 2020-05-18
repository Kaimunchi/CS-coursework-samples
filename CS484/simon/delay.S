;; Delay.S
;; Precise delays for microcontroller use
;; Brandon Ingli, Don Bindner, Jon Beck


.text
.macro nop2
    rjmp .
.endm

.global delay1us
.global delay1ms

; Delay in units of 16 cycles (microcontroller running at 16MHz)
delay1us:         ; 4 cycles for call
    nop           ; 1 cycle
    nop2          ; 2 cycles
    rjmp delay2   ; 2 cycles
delay1:           ; 12 cycles total
    nop2          ; 2 cycles
    nop2          ; ...
    nop2
    nop2
    nop2
    nop2
delay2:
    sbiw r24, 1   ; sub 1 from r24/25, 2 cycles
    brne delay1   ; branch != 0 (1 or 2 cycles)

    ret            ; 4 cycles

;; Delay n milliseconds
delay1ms:           ; 4 cycles for call
    movw r26, r24   ; 1 cycle to save ms count
    rjmp delay4     ; 2 cycles
delay3:             ; 10 cycles total
    nop2
    nop2
    nop2
    nop2
    nop2
delay4:
    ldi r24, 231     ; 1 cycle
    ldi r25, 3       ; 1 cycle
    call delay1us    ; 999us
    sbiw r26, 1      ; 2 cycles
    brne delay3      ; 1/2 cycles

    ret              ; 4 cycles