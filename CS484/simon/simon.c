#include <avr/io.h>
#include <avr/interrupt.h>
#include <avr/sleep.h>
#include "delay.h"
#include "buzzer.h"
#include "leds.h"
#include <stdlib.h>
#include "debug.h"
#include <string.h>

#define PLAYWAIT 500
#define AFTERTURNMULT 1
#define BUTTONCOUNTMAX 4
#define T2INTFREQ 61
#define TIMEOUTSEC 30

#define MAXSEQ 250

/**
 * A simon-like game for the Arduino Uno
 * Brandon Ingli 2020
 */

volatile uint8_t score = 0;
volatile uint8_t seq[MAXSEQ];

volatile uint8_t buttonCount = 0;
volatile uint8_t buttonPressed = 0xff;
volatile uint8_t buttonReady = 0;

volatile uint16_t timeLeft = T2INTFREQ*TIMEOUTSEC;

/**
 * A few function prototypes so the C compiler is happy with my ordering
 */
void exitGame();
void gameplay();
void resetTimeout();

/**
 * Timer 2 overflow ISR; polls buttons and enforces timeout
 */
ISR(TIMER2_OVF_vect){
  volatile uint8_t status = ~(PINB & 0b00111100);
  volatile uint8_t currButton;
  if(status & 1<<PB2){
    currButton = 0;
  } else if (status & 1<<PB3){
    currButton = 1;
  } else if (status & 1<<PB4){
    currButton = 2;
  } else if (status & 1<<PB5){
    currButton = 3;
  } else {
    currButton = 0xff;
  }

  if(currButton != 0xff && currButton == buttonPressed){
    buttonCount++;
    resetTimeout();
  } else {
    buttonPressed = currButton;
    buttonCount = 0;
  }

  timeLeft--;

  if(buttonCount >= BUTTONCOUNTMAX){
    buttonReady = 1;
  }

  if(!timeLeft){
    buttonPressed = 0xff;
    buttonReady = 1;
  }
}

/**
 * External Interrupt 0 ISR
 * Wakes the CPU from sleep, and that's it.
 */
ISR(INT0_vect){}

/**
 * Toggles the timer2 interrupt for the button polling / timeout
 */
void toggleButtonInterrupts(){
  TIMSK2 ^= (1<<TOIE2); //Interrupt on overflow
  if(TCCR2B & ((1<<CS22) + (1<<CS21) + (1<<CS20)) ){
    TCCR2B &= ~((1<<CS22) + (1<<CS21) + (1<<CS20));
  } else {
    TCCR2B |= ((1<<CS22) + (1<<CS21) + (1<<CS20)); //div1024
  }
  
}

/**
 * Toggles INT0
 */
void toggleStartButtonInterrupt(){
  EIMSK |= (1<<INT0);
}

/**
 * Resets the flag and counter associated with the button polling
 */
void resetButtons(){
  buttonCount = 0;
  buttonReady = 0;
}

/**
 * Resets the timer countdown for timeout
 */
void resetTimeout(){
  timeLeft = T2INTFREQ*TIMEOUTSEC;
}

/**
 * Puts microcontroller in PWR_DOWN mode
 * Can be awaken by INT0
 */
void powerDown(){
  set_sleep_mode(SLEEP_MODE_PWR_DOWN);
  sleep_enable();
  sleep_cpu();
  sleep_disable();
}

/**
 * Puts microcontroller in PWR_SAVE mode
 * Can be awaken by Timer 2 Overflow interrupt
 */
void powerSave(){
  set_sleep_mode(SLEEP_MODE_PWR_SAVE);
  sleep_enable();
  sleep_cpu();
  sleep_disable();
}

/**
 * Welcome animation and jingle
 */
void welcome(){
  for(uint8_t i = 0; i < 4; i++){
    LEDon(i);
  }
  superMarioStart();
  for(uint8_t i = 0; i < 4; i++){
    LEDoff(i);
  }
}

/**
 * Goodbye animation and jingle
 */
void goodbye(){
  for(uint8_t i = 0; i < 4; i++){
    LEDon(i);
  }
  superMarioEnd();
  for(uint8_t i = 0; i < 4; i++){
    LEDoff(i);
  }
}

/**
 * Count the score out on the LEDs, and send it via UART if debug enabled
 */
void countScore(){
   if(debugEnabled()){
    unsigned char message[] = "\nScore: ";
    for(uint8_t i = 0; i < strlen(message); i++){
      sendDebugValue(message[i]);
    }
    uint8_t temp = score;
    sendDebugValue(getHexDigit(temp/100));
    temp %= 100;
    sendDebugValue(getHexDigit(temp/10));
    temp %= 10;
    sendDebugValue(getHexDigit(temp));
    sendDebugValue('\n');
    sendDebugValue('\n');
  }
  while(score >= 100){
    changeNote(1);
    LEDon(1);
    buzzerOn();
    delay1ms(PLAYWAIT);
    buzzerOff();
    LEDoff(1);
    score -= 100;
    delay1ms(PLAYWAIT);
  }
  while(score >= 10){
    changeNote(2);
    LEDon(2);
    buzzerOn();  superMarioStart();

    delay1ms(PLAYWAIT);
    buzzerOff();
    LEDoff(2);
    score -= 10;
    delay1ms(PLAYWAIT);
  }
  while(score > 0){
    changeNote(3);
    LEDon(3);
    buzzerOn();
    delay1ms(PLAYWAIT);
    buzzerOff();
    LEDoff(3);
    score--;
    delay1ms(PLAYWAIT);
  }
}

/**
 * Game end procedure
 */
void exitGame(){
  for(uint8_t i = 0; i < 5; i++){
    LEDoff(i);
  }
  changeNote(7);
  buzzerOn();
  LEDoff(4);
  delay1ms(1000);
  buzzerOff();
  delay1ms(1000);
  countScore();
  delay1ms(PLAYWAIT);
  goodbye();
}

/**
 * Plays the current part of the sequence
 */
void playSequence(volatile uint8_t seq[MAXSEQ], uint8_t len){
  LEDoff(4);
  delay1ms(PLAYWAIT);
  for(uint8_t i = 0; i < len; i++){
    changeNote(seq[i]);
    buzzerOn();
    LEDon(seq[i]);
    delay1ms(PLAYWAIT);
    LEDoff(seq[i]);
    buzzerOff(seq[i]);
    delay1ms(PLAYWAIT);
  }
  LEDon(4);
}

/**
 * Main gameplay logic
 */
void gameplay(){
  score = 0;
  welcome();
  delay1ms(1000);

  uint8_t stillGood = 1;
  for (uint8_t len = 0; stillGood && len < MAXSEQ; len++){
    seq[len] = rand() % 4;
    if(debugEnabled()){
      sendDebugValue(getHexDigit(seq[len]));
    }
    playSequence(seq, len+1);
    for(uint8_t curPos = 0; stillGood && curPos <= len; curPos++){
      toggleButtonInterrupts();
      while(!buttonReady){
        powerSave();
      }
      toggleButtonInterrupts();
      resetButtons();
      if(buttonPressed != seq[curPos]){
        stillGood = 0;
        exitGame();
      } else {
        LEDon(buttonPressed);
        changeNote(buttonPressed);
        buzzerOn();
        delay1ms(PLAYWAIT);
        buzzerOff();
        LEDoff(buttonPressed);
      }
    }
    if(stillGood){score++;}
    LEDoff(4);
    for(uint8_t i = 0; i < AFTERTURNMULT; i++){
      delay1ms(PLAYWAIT);
    }
  }
  if(stillGood){ // Completed MAXSEQ rounds!
    for(uint8_t i = 0; i < 5; i++){
    LEDon(i);
    }
    superMarioEnd();
    for(uint8_t i = 0; i < 5; i++){
      LEDoff(i);
    }
    delay1ms(PLAYWAIT);
    countScore();
  }
 
}

/**
 * Seeds the Random Number Generator via the ADC reading off the pot.
 */
void seedRandADC(){
  // Free Running Mode
  ADCSRA = (1<<ADEN) +  // ADC Enable
           (1<<ADPS2) + (1<<ADPS1) + (1<<ADPS0); // 128 prescaler
  ADMUX = (0<<REFS1) + (1<<REFS0) + //AVcc
          (0<<ADLAR) + // Right justify
          (0<<MUX3) + (0<<MUX2) + (0<<MUX1) + (0<<MUX0); // Pin A0/PC0/ADC0

  ADCSRA |= (1<<ADSC); // Start Conversion
  while ((ADCSRA & (1<<ADIF)) == 0); // Wait for the conversion to finish
  // ADCSRA &= ~(1<<ADSC); // Stop Conversion
  ADCSRA |= (1<<ADIF); // Clear the Interrupt Flag
  uint16_t seed = ADCL; // Read low first
  seed += (ADCH << 8);

  srand(seed);
}

void main(){

  initLEDs(5, 6, 11, 12, 13);

  DDRB = 0x00;
  PORTB = 0xff;
  DDRD &= ~(1<<PD2);
  PORTD |= (1<<PD2);

  TCCR2A = (0<<WGM21) + (0<<WGM20); //Normal Mode, Top=0xff
  TCCR2B = (0<<WGM22);//Normal Mode, Top=0xff

  seedRandADC();

  debugInit();

  initBuzzer();

  // Power On LED and Buzzer check
  for(uint8_t i = 0; i < 5; i++){
    LEDon(i);
  }
  buzzerOn();
  delay1ms(500);
  buzzerOff();
  for(uint8_t i = 0; i < 5; i++){
    LEDoff(i);
  }

  sei();

  while(1){
    toggleStartButtonInterrupt();
    powerDown();
    toggleStartButtonInterrupt();
    // See if we should debug or not
    DDRC &= ~(1<<PC1); // Make sure PC1 is input
    if(PINC & (1<<PC1)){
      debugEnable();
    } else {
      debugDisable();
    }
    gameplay();
  };
  
}