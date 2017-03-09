
# Supposed to be run from within haskell. All .md's will be dropped into /bin
echo "Running the manual building script"

mkdir .bin 2> /dev/null
cd src/Assets/Manual 2> /dev/null

# Done by autoPreprocess in haskell:
# cp *.md   .bin/
cp *.tex  .bin/
cp *.svg  .bin/
cp *.png  .bin/
cp *.html .bin/ 2> /dev/null
cp *.generate  .bin/
cp *.slideshow .bin/
cd .bin
for FILE in *.svg
do
	OUTFILE=${FILE%.svg}
	if [ -e $OUTFILE.png ]
	then
		# echo "INKSCAPE: Skipping generation of $OUTFILE.png"
		echo "" >/dev/null
	else
		inkscape -D -d=360 -z --file=$FILE --export-png=$OUTFILE.png | sed "s/^/INKSCAPE: /g"
	fi
done

PANDOC_EXTENSIONS=" --tab-stop=8 -f markdown+link_attributes+grid_tables+pandoc_title_block+pipe_tables+implicit_header_references"
#for SET in *.slideshow
#do
#	SETNAME=${SET%.slideshow}
#	OUTFILE="$SETNAME-slides.md"
#	echo -n "Building slideshow $SET (dumpfile $OUTFILE) with lines: "
#	rm $OUTFILE 2> /dev/null
#	for FILE in `cat $SET`
#	do
#		cat $FILE | sed "s/\[style=terminal\]//g" >> $OUTFILE
#	done
#	cat $OUTFILE | wc -l
#	
#	echo "Running pandoc for slides on $OUTFILE"
#	cat $OUTFILE
#	rm "Slides_$SETNAME.pdf" 2> /dev/null
#	pandoc $PANDOC_EXTENSIONS --slide-level 2 --listings -t beamer $OUTFILE -o "Slides_$SETNAME.pdf"  --variable mainfont="Ubuntu"
#	cp "Slides_$SETNAME.pdf" ../Output/

#done

for SET in *.generate
do
	SETNAME=${SET%.generate}
	OUTFILE="$SETNAME-all.md"
	echo -n "$OUTFILE "
	rm $OUTFILE 2> /dev/null
	for FILE in `cat $SET`
	do
		cat $FILE >> $OUTFILE
	done
	cat $OUTFILE | wc -l
	
	# HTML OPTIONS
	pandoc $PANDOC_EXTENSIONS --self-contained $OUTFILE -o "ALGT_$SETNAME.html" | sed "s/^/PANDOC-HTML  /g"
	cp "ALGT_$SETNAME.html" ../Output/



	# LaTeX
	pandoc $PANDOC_EXTENSIONS --listings --chapters $OUTFILE -o $SETNAME.tex | sed "s/^/PANDOC-TEX /g"

	if [ $SETNAME = "Focus" ]
	then
		cat Main.tex | sed "/%NOFOCUS/d " | sed "s/§/$SETNAME/" > "ALGT_$SETNAME.tex"
	else
		cat Main.tex | sed "s/§/$SETNAME/" > "ALGT_$SETNAME.tex"
	fi
	
	echo "Running pdf latex..."
	rm "ALGT_$SETNAME.pdf"
	LATEX_OUTPUT=`pdflatex -halt-on-error "ALGT_$SETNAME.tex" | sed "s/^/§LATEX-$SETNAME: /g"`
	echo $LATEX_OUTPUT | sed "s/§/\\n/g"
	cp "ALGT_$SETNAME.pdf" ../Output/
	if [ $? -eq 0 ]
	then
		echo "Pdf-compilation successfull"
	else
		echo "NO PDF FOUND"
	fi
done


echo "MANUAL IS BUILT"


