#!/bin/bash

WIDTH=700
HEIGHT=420

rm -rf plot
mkdir plot

for site in `ls data`
do

  mkdir "plot/${site}"
  
  gnuplot -e "set key off; set xlabel \"Time (seconds)\"; set ylabel \"Timeout (milliseconds)\"; \
		  set terminal pngcairo size ${WIDTH},${HEIGHT} enhanced font \"Times-Roman,10\"; \
	    set output \"plot/${site}/timeout.png\"; set title \"${site} Timeout\";\
	    plot 'data/${site}/timeout.dat' with linespoints"
      
  gnuplot -e "set key outside; set xlabel \"Time (seconds)\"; set ylabel \"RTT (milliseconds)\"; \
	    set terminal pngcairo size ${WIDTH},${HEIGHT} enhanced font \"Times-Roman,10\"; \
	    set output \"plot/${site}/rtt.png\"; set title \"${site} RTT\";\
	    plot 'data/${site}/samplertt.dat' with linespoints title \"Sample RTT\", \
	    'data/${site}/estrtt.dat' with linespoints title \"Est RTT\""
done