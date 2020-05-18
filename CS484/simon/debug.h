#ifndef DEBUGLIBARY
#define DEBUGLIBARY

uint8_t getHexDigit(uint8_t);
uint8_t debugEnabled();
void debugEnable();
void debugDisable();
void debugInit();
void sendDebugValue(uint8_t);

#endif