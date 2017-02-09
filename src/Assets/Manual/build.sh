cp *.md bin/
cp *.tex bin/
cd bin
rm Manual.md
for FILE in *.md
do
	OUTFILE=${FILE%.md}
	pandoc $FILE -o $OUTFILE.tex
done
pdflatex Main.tex
cp Main.pdf ../
