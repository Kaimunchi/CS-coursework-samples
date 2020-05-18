#!/bin/bash
# tcpsim.sh
# Brandon Ingli 2020
# Uses pings to simulate TCP RTT and calculate/plot parameters
# Args: 1 or more IP addresses or domains to ping
# Output: data/<site>/*.dat with the raw data; plot/<site>/*.png with plots
# Dependencies: ./plotit.sh to actually plot (run manually to re-plot data)

ALPHA=0.125 # Alpha value in the ESTRTT formula
BETA=0.25 # Beta value in the DEVRTT formula

declare -a SITE=() # Holds domains/IPs to ping
declare -a ESTRTT=() # Estimated RTT as calculated with TCP formula
declare -a DEVRTT=() # RTT Deviation as calculated with TCP formula
declare -a TIMEOUT=() # Timeout as calcualted with TCP formula

# Read in parameters and set up other arrays
for site in "$@"
do
	SITE+=("$site")
	ESTRTT+=(0)
	DEVRTT+=(0)
	TIMEOUT+=(1000)
done

# Get the last index of the data arrays
((MAXINDEX = ${#SITE[@]} - 1))

# Counter for time in seconds
TIME=1

# Remove previous results
rm -rf data

# Make directories for new results
mkdir data
for site in "${SITE[@]}"
do	
	mkdir "data/${site}"
done

# First round initializes everything to the sample RTT
echo "Round 1..."
for i in `seq 0 "$MAXINDEX"`
do
	echo "${SITE[$i]}"
	ESTRTT[$i]=`ping -c 1 "${SITE[$i]}" | tail -1 | awk -F '/' '{print $5}'`

	echo "$TIME ${ESTRTT[$i]}" >> "data/${SITE[$i]}/estrtt.dat"
	echo "$TIME ${ESTRTT[$i]}" >> "data/${SITE[$i]}/samplertt.dat"

done
echo -e "\n"
((TIME = TIME + 5))

# Main Loop
for j in {2..100}
do
	sleep 5s
	echo "Round $j..."
	for i in `seq 0 "$MAXINDEX"` # 0, 1, ..., MAXINDEX
	do
		echo "${SITE[$i]}"
		SAMPLERTT=`ping -c 1 "${SITE[$i]}" | tail -1 | awk -F '/' '{print $5}'` # ping, get last line, and pull out the average RTT (which for 1 ping is the ping time)
		ESTRTT[$i]=`echo "(1-$ALPHA) * ${ESTRTT[$i]} + $ALPHA * $SAMPLERTT" | bc` # Use the formula to calculate ESTRTT with `bc` (because it's floating point)
		SAMPLEDEV=`echo "$SAMPLERTT - ${ESTRTT[$i]}" | bc | tr -d -` # Calculate the sample deviation and take the absolute value (by stripping the '-'' char if present)
		DEVRTT[$i]=`echo "(1-$BETA) * ${DEVRTT[$i]} + $BETA * $SAMPLEDEV" | bc` # Use the formula to calculate DEVRTT
		TIMEOUT[$i]=`echo "${ESTRTT[$i]} + 4 * ${DEVRTT[$i]}" | bc` # Use the formula to calculate Timeout Value

		# Write Data to the data files
		echo "$TIME ${ESTRTT[$i]}" >> "data/${SITE[$i]}/estrtt.dat"
		echo "$TIME $SAMPLERTT" >> "data/${SITE[$i]}/samplertt.dat"
		echo "$TIME ${TIMEOUT[$i]}" >> "data/${SITE[$i]}/timeout.dat"

	done
	echo -e "\n"
	((TIME = TIME + 5))
done

# Plot the data
# Manually run this script with new settings to replot without fetching new data
./plotit.sh
