#ifndef LEDS
#define LEDS

volatile uint8_t* getPort(uint8_t);
uint8_t getMask(uint8_t);
void setOutput(uint8_t, uint8_t);

void initLEDs(uint8_t, uint8_t, uint8_t, uint8_t, uint8_t);

void LEDon(uint8_t);
void LEDoff(uint8_t);
void LEDtoggle(uint8_t);

#endif