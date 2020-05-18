/**
 * Simon LED Library for ATMega328p
 * Brandon Ingli 2020
 */
#include <avr/io.h>
#include <avr/interrupt.h>

volatile uint8_t* ports[5];
uint8_t masks[5];

/**
 * Helper function to get the PORT address of a physical pin
 */
volatile uint8_t* getPort(uint8_t pin){
  if(pin >= 2 && pin <= 6 || pin >= 11 && pin <= 13){
    return &PORTD;
  } else if (pin >= 14 && pin <= 19){
    return &PORTB;
  } else{
    return &PORTC;
  }
}

/**
 * Helper function that gets the mask for a physical pin
 */
uint8_t getMask(uint8_t pin){
  if(pin >= 2 && pin <= 6){
    return 1<<(pin-2);
  } else if (pin >= 11 && pin <= 13){
    return 1<<(pin-6);
  } else if (pin >= 14 && pin <= 19){
    return 1<<(pin-14);
  } else if (pin >= 23 && pin <= 28){
    return 1<<(pin-23);
  } else{
    return 0;
  }
}

/**
 * Helper function that sets a physical pin with a mask as an output
 */
void setOutput(uint8_t pin, uint8_t mask){
  if(pin >= 2 && pin <= 6 || pin >= 11 && pin <= 13){
    DDRD |= mask;
  } else if (pin >= 14 && pin <= 19){
    DDRB |= mask;
  } else if (pin >= 23 && pin <= 28){
    DDRC |= mask;
  }
}

/**
 * Initialized the LEDs of the Simon game
 */
void initLEDs(uint8_t red, uint8_t yel, uint8_t grn, uint8_t blu, uint8_t status) {
   ports[0] = getPort(red);
   masks[0] = getMask(red);
   setOutput(red, masks[0]);

   ports[1] = getPort(yel);
   masks[1] = getMask(yel);
   setOutput(yel, masks[1]);

   ports[2] = getPort(grn);
   masks[2] = getMask(grn);
   setOutput(grn, masks[2]);

   ports[3] = getPort(blu);
   masks[3] = getMask(blu);
   setOutput(blu, masks[3]);

   ports[4] = getPort(status);
   masks[4] = getMask(status);
   setOutput(status, masks[4]);
}

/**
 * Turn an LED on by index
 */
void LEDon(uint8_t i){
  *(ports[i]) |= masks[i];
}

/**
 * Turn an LED off by index
 */
void LEDoff(uint8_t i){
  *(ports[i]) &= ~(masks[i]);
}

/**
 * Toggle an LED by index
 */
void LEDtoggle(uint8_t i){
  if (*(ports[i]) & masks[i]) {
    LEDoff(i);
  } else {
    LEDon(i);
  }
}