#ifndef BUZZER
#define BUZZER

void initBuzzer(); // in buzzerinit.S
void buzzerOn();
void buzzerOff();
void buzzerToggle();
void changeNote(uint8_t);
void changeNoteCustomTop(uint16_t);

void cMajorScale();
void superMarioStart();
void superMarioEnd();

#endif