
# Supposed to be run from within haskell. All .md's will be dropped into /bin
cd src/Assets/Manual
cp *.tex bin/
cd .bin
for FILE in *.md
do
	OUTFILE=${FILE%.md}
	pandoc $FILE -o $OUTFILE.tex
done
pdflatex Main.tex
cp Main.pdf ../../../..
