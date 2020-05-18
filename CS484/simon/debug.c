#include <avr/io.h>
#include <avr/interrupt.h>
#include <avr/sleep.h>
/**
 * Library for the ATmega328p to send debug messages
 * over UART
 * Brandon Ingli 2020
 */

uint8_t enabled = 0x00;

/**
 * Gets the ASCII code for the appropriate hex digit of val
 */
uint8_t getHexDigit(uint8_t val){
  if (val <= 9){
    return val + 48;
  } else {
    return val + 55;
  }
}

/**
 * Get the debug flag
 */
uint8_t debugEnabled(){
  return enabled;
}

/**
 * Enable debug messages
 */
void debugEnable(){
  UCSR0B = (1<<TXEN0); // Enable UART transmit
  enabled = 0x01;
}

/**
 * Disable debug messages
 */
void debugDisable(){
  UCSR0B |= !(1<<TXEN0); // Disable UART transmit
  enabled = 0x00;
}

/**
 * Initialize UART at transmit 9600 baud, even parity, 8-bit
 */
void debugInit(){
  uint8_t sreg = SREG;
  cli();
  UBRR0H = 0x00; //9600 baud
  UBRR0L = 0x67;
  SREG = sreg;

  UCSR0A = 0x00; // Clear everything to init

  UCSR0C = (1<<UPM00) + // Even Parity
          (1<<UCSZ01) + (1<<UCSZ00); // 8-bit word size
}

/**
 * Send an 8-bit val over UART
 */
void sendDebugValue(uint8_t toSend){
  while(!(UCSR0A & (1 << UDRE0))){}; // Wait for output to be empty
  UDR0 = toSend;
}