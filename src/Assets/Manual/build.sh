
# Supposed to be run from within haskell. All .md's will be dropped into /bin
mkdir .bin
cd src/Assets/Manual
cp *.tex .bin/
cp *.svg .bin/
cp *.png .bin/
cp *.html .bin/
cd .bin
for FILE in *.svg
do
	OUTFILE=${FILE%.svg}
	if [ -e $OUTFILE.png ]
	then
		echo "INKSCAPE: Skipping generation of $OUTFILE.png"
	else
		inkscape -D -d=360 -z --file=$FILE --export-png=$OUTFILE.png | sed "s/^/INKSCAPE: /g"
	fi
done



for FILE in *.md
do
	OUTFILE=${FILE%.md}
	for FORMAT in "tex" "html"
	do
		pandoc -f markdown+link_attributes+grid_tables+pandoc_title_block+pipe_tables --tab-stop=8 $FILE -o $OUTFILE.$FORMAT | sed "s/^/PANDOC /g"
	done

done
pdflatex -halt-on-error Main.tex | sed "s/^/LATEX: /g"
cp Main.pdf ../ALGT_Manual.pdf
