## Simon Game Makefile
## Brandon Ingli

simon.hex: simon.elf
	avr-objcopy -j .text -j .data -O ihex simon.elf simon.hex

simon.elf: simon.o delay.o buzzer.o buzzerinit.o leds.o debug.o
	avr-gcc -mmcu=atmega328p simon.o delay.o buzzer.o buzzerinit.o leds.o debug.o -o simon.elf

simon.o: simon.c
	avr-gcc -mmcu=atmega328p -c simon.c -o simon.o

buzzer.o: buzzer.c buzzer.h
	avr-gcc -mmcu=atmega328p -c buzzer.c -o buzzer.o

buzzerinit.o: buzzerinit.S buzzer.h
	avr-gcc -mmcu=atmega328p -c buzzerinit.S -o buzzerinit.o

leds.o: leds.c leds.h
	avr-gcc -mmcu=atmega328p -c leds.c -o leds.o

delay.o: delay.S
	avr-gcc -mmcu=atmega328p -c delay.S -o delay.o

debug.o: debug.c debug.h
	avr-gcc -mmcu=atmega328p -c debug.c -o debug.o
install: simon.hex
	avrdude -p atmega328p -c usbtiny -U flash:w:simon.hex:i

clean:
	rm -f *.o *.elf *.hex