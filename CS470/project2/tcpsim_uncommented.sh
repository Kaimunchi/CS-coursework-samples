#!/bin/bash
ALPHA=0.125
BETA=0.25

declare -a SITE=()
declare -a ESTRTT=()
declare -a DEVRTT=()
declare -a TIMEOUT=()

for site in "$@"
do
	SITE+=("$site")
	ESTRTT+=(0)
	DEVRTT+=(0)
	TIMEOUT+=(1000)
done

((MAXINDEX = ${#SITE[@]} - 1))

TIME=1

rm -rf data

mkdir data
for site in "${SITE[@]}"
do	
	mkdir "data/${site}"
done

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

for j in {2..100}
do
	sleep 5s
	echo "Round $j..."
	for i in `seq 0 "$MAXINDEX"`
	do
		echo "${SITE[$i]}"
		SAMPLERTT=`ping -c 1 "${SITE[$i]}" | tail -1 | awk -F '/' '{print $5}'`
		ESTRTT[$i]=`echo "(1-$ALPHA) * ${ESTRTT[$i]} + $ALPHA * $SAMPLERTT" | bc`
		SAMPLEDEV=`echo "$SAMPLERTT - ${ESTRTT[$i]}" | bc | tr -d -`
		DEVRTT[$i]=`echo "(1-$BETA) * ${DEVRTT[$i]} + $BETA * $SAMPLEDEV" | bc`
		TIMEOUT[$i]=`echo "${ESTRTT[$i]} + 4 * ${DEVRTT[$i]}" | bc`
	
		echo "$TIME ${ESTRTT[$i]}" >> "data/${SITE[$i]}/estrtt.dat"
		echo "$TIME $SAMPLERTT" >> "data/${SITE[$i]}/samplertt.dat"
		echo "$TIME ${TIMEOUT[$i]}" >> "data/${SITE[$i]}/timeout.dat"
	
	done
	echo -e "\n"
	((TIME = TIME + 5))
done

./plotit.sh
