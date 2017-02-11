#! /bin/bash
TOTAL="0"
for CODE in "TODO" "FIXME"
do
	./search.sh $CODE
	echo -e "\nTOTAL $CODE: "
	FOUND=`./search.sh $CODE | wc -l`
	echo $FOUND
	TOTAL=$TOTAL"+"$FOUND
done

TOTAL=`echo $TOTAL | bc`
echo "All issues: $TOTAL"
