;; walkingLED.S
;; Cycles power through eight LEDs
;; Brandon Ingli

.equ DDRD, 0x0A
.equ PORTD, 0x0B
.equ DDRC, 0x07
.equ PINC, 0x06

;; Registers in use
;; r1: Zero constant
;; r5: Read input from PINC
;; r16: Bitmask of LED(s) to be on
;; r18: 0xFF constant
;; r19: storage for status register in delay
;; r28: storage for status register in main
;; r31:30 : Word for delay loop

.text
main:
  ;; Set all on DDRD to output
  ldi r18, 0xFF
  sts 0x002A, r18
  
  ;; Set PC0 to input
  cbi DDRC, 0

  ;; Set starting value of bitmask
  ldi r16, 0x00

  ;; Set Carry bit (which will rotate in first iteration)
  sec

  ;; Clear PORTD
  out PORTD, r1


loop:
  out PORTD, r16 ; Use bitmask to turn LED(s) On/Off
  rcall delay
  lds r28, 0x005F ; Save status register
  in r5, PINC ; Read PINC
  tst r5 ; Test if PINC is zero (ground)
  brne reverse ; Go in reverse if Vcc, otherwise go forward
forward: ; LED 0 to LED 7
  sts 0x005F, r28 ; Load status register back after test
  rol r16 ; Rotate Left Through Carry
  rjmp continue 
reverse: ; LED 7 to LED 0
  sts 0x005F, r28 ; Load status register back after test
  ror r16 ; Rotate Right Through Carry
continue:
  rjmp loop

;; Loop 65535 times to create a delay
delay:
    lds r19, 0x005F ; Save the status register so we can use the Carry later
    ldi r31, 0xFF   ; Together, registers r31 and r30 form a
    ldi r30, 0xFF   ; 16 bit value 65535. ATMega328P has 32 registers, but
                    ; only 3 pairs can combine to form 16 bits
delay1:
    ;; Do a bunch of nothing!
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop
    nop

    sbiw r30, 1     ; subtract 1 from r30/r31 register pair
    brne delay1     ; branch if result is not zero

    sts 0x005F, r19  ; Load the status register back
    ret             ; return
