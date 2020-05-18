#define __SFR_OFFSET 0
#include <avr/io.h>
#include <avr/interrupt.h>

;; buzzerinit.S
;; Initializes the PWM for the buzzer


.text
.global initBuzzer

initBuzzer:
  ldi r18, 0b10000010 ; Fast PWM at OCR1A, non-inverting
  sts TCCR1A, r18

  ldi r18, 0b00011001 ; Fast PWM at OCR1A, clock div 1
  sts TCCR1B, r18

  mov r25, r1 ; Parameter = zero
  mov r24, r1
  call changeNote

  sbi DDRB, PB1
  
  ret