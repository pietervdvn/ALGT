
# Supposed to be run from within haskell. All .md's will be dropped into /bin
cd src/Assets/Manual
cp *.tex .bin/
cp *.svg .bin/
cp *.html .bin/
cd .bin
for FILE in *.svg
do
	OUTFILE=${FILE%.svg}
	inkscape -D -z --file=$FILE --export-png=$OUTFILE.png
done



for FILE in *.md
do
	OUTFILE=${FILE%.md}
	pandoc -f markdown+link_attributes $FILE -o $OUTFILE.tex
	pandoc -f markdown+link_attributes $FILE -o $OUTFILE.html

done
pdflatex -halt-on-error Main.tex
cp Main.pdf ../../../..
