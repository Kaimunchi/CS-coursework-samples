/**
 * Buzzer Library for ATMega328p
 * Buzzer signal on OC1A (PB1, Arduino 9)
 * Brandon Ingli 2020
 */
#include <avr/io.h>
#include <avr/interrupt.h>
#include "delay.h"

/**
 * TOP values for notes C -> C (C major)
 */
uint16_t tops[8] = {15289, 13620, 12134, 11453, 10203, 9090, 8098, 7644};

/** Turn Buzzer On */
void buzzerOn(){
  TCCR1B |= (0<<CS12) + (0<<CS11) + (1<<CS10);
  // DDRB |= (1<<PB1);
}

/** Turn Buzzer Off */
void buzzerOff(){
  TCCR1B &= ~(1<<CS10);
  // DDRB &= ~(1<<PB1);
}

/** Toggle the Buzzer */
void buzzerToggle(){
  if(DDRB & (1<<PB1)){
    buzzerOff();
  } else {
    buzzerOn();
  }
}

/**
 * Change the note to one of the 8
 * 0 - Low C
 * 1 - D
 * 2 - E
 * 3 - F
 * 4 - G
 * 5 - A
 * 6 - B
 * 7 - High C
 */
void changeNote(uint8_t i){
  uint8_t sreg = SREG;
  cli();
  ICR1 = tops[i];
  OCR1A = tops[i] >> 1;
  SREG = sreg;
}

/**
 * Allows for changing to notes not in the one octave C major scale by passing 
 * the TOP value to set.
 */
void changeNoteCustomTop(uint16_t top){
  uint8_t sreg = SREG;
  cli();
  ICR1 = top;
  OCR1A = top >> 1;
  SREG = sreg;
}

/**
 * Initialize Timer1 for Fast PWM for Buzzer and PB1 as output.
 * Default note is first in tops[]
 */
// void initBuzzer(){
//   TCCR1A = (1<<WGM11) + (0<<WGM10) + //Fast PWM at OCR1A
//            (1<<COM1A1) + (0<<COM1A0); //Non-inverting on OC1A
//   TCCR1B = (1<<WGM13) + (1<<WGM12) + //Fast PWM at OCR1A
//            (0<<CS12) + (0<<CS11) + (1<<CS10); // Clock div 1

//   EICRA |= (1<<ISC01) + (0<<ISC00); //Ex. INT0, Falling Edge Mode

//   changeNote(0);
//   DDRB |= (1<<PB1);
// }

/**
 * Play a C major Scale
 */
void cMajorScale(){
  changeNote(0);
  buzzerOn();
  for(uint8_t i = 0; i < 8; i++){
    changeNote(i);
    delay1ms(125);
  }
  buzzerOff();
}

/**
 * Play the start of the Super Mario theme
 */
void superMarioStart(){
  changeNote(2); // E
  buzzerOn();
  delay1ms(140);
  buzzerOff();
  delay1ms(27); // Eighth note with a brief rest for clarity

  buzzerOn();
  delay1ms(167); // Eighth note E

  buzzerOff();
  delay1ms(167); // Eighth Rest

  buzzerOn();
  delay1ms(167); // Eighth note E

  buzzerOff();
  delay1ms(167); // Eighth rest

  changeNote(0); // C
  buzzerOn();
  delay1ms(167); // Eighth Note

  changeNote(2); // E
  delay1ms(333); // Quarter Note

  changeNote(4); // G
  delay1ms(333); // Quarter Note

  buzzerOff();
  delay1ms(333); // Quarter Rest

  changeNoteCustomTop(20407); // Low G
  buzzerOn();
  delay1ms(333); // Quarter Note

  buzzerOff();
}

/**
 * Play the end of the Super Mario theme
 */
void superMarioEnd(){
  changeNote(7); // C
  buzzerOn();
  delay1ms(333); //quarter note
  buzzerOff();
  delay1ms(167); //eighth rest
  
  changeNote(4); // G
  buzzerOn();
  delay1ms(333); //quarter note
  buzzerOff();
  delay1ms(167); //eighth rest

  changeNote(2); // E
  buzzerOn();
  delay1ms(333); //quarter note
  
  changeNote(5); // A
  delay1ms(222); // quarter triplet

  changeNote(6); // B
  delay1ms(222); // quarter triplet

  changeNote(5); // A
  delay1ms(222); // quarter triplet

  changeNoteCustomTop(9630); // Ab
  delay1ms(222); // quarter triplet

  changeNoteCustomTop(8580); // Bb
  delay1ms(222); // quarter triplet

  changeNoteCustomTop(9630); // Ab
  delay1ms(222); // quarter triplet

  changeNote(2); // E
  delay1ms(167); // eighth note

  changeNote(1); // D
  delay1ms(167); // eighth note

  changeNote(2); // E
  delay1ms(1000); // dotted half note

  buzzerOff();


}