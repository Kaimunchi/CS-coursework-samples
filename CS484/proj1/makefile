walkingLED.hex: walkingLED.elf
					 avr-objcopy -j .text -j .data -O ihex walkingLED.elf walkingLED.hex

walkingLED.elf: walkingLED.o
					avr-ld walkingLED.o -o walkingLED.elf
					avr-strip walkingLED.elf

walkingLED.o: walkingLED.S
					avr-as -mmcu=atmega328p walkingLED.S -o walkingLED.o

walkingLED.lst: walkingLED.S
					avr-as -agls=walkingLED.lst -mmcu=atmega328p $< -o walkingLED.o

install: walkingLED.hex
					avrdude -p atmega328p -c usbtiny -U flash:w:walkingLED.hex:i

clean:
					rm -f *.o *.elf *.hex *.lst*