for FILE in *.svg
do
	OUTFILE=${FILE%.svg}
	if [ -e $OUTFILE.png ]
	then
		echo "Image skipped: $FILE" 
	else
		inkscape -D -d=360 -z --file=$FILE --export-png=$OUTFILE.png | sed "s/^/INKSCAPE: /g"
	fi
done
