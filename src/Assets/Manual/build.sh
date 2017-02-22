
# Supposed to be run from within haskell. All .md's will be dropped into /bin
mkdir .bin
cd src/Assets/Manual

# Done by autoPreprocess in haskell:
# cp *.md   .bin/
cp *.tex  .bin/
cp *.svg  .bin/
cp *.png  .bin/
cp *.html .bin/
cp Focus  .bin/
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

rm Manual.md
rm Focus.md

for FILE in *.md
do
	cat $FILE >> Manual.md
done


for FILE in `cat Focus`
do
	cat $FILE >> Focus.md
done




for FILE in Manual.md Focus.md
do
	OUTFILE=${FILE%.md}
	# HTML OPTIONS

	PANDOC_EXTENSIONS="-f markdown+link_attributes+grid_tables+pandoc_title_block+pipe_tables+implicit_header_references --tab-stop=8 --normalize"
	pandoc $PANDOC_EXTENSIONS --self-contained $FILE -o $OUTFILE.html | sed "s/^/PANDOC-HTML  /g"

	# LaTeX
	pandoc $PANDOC_EXTENSIONS --listings --chapters $FILE -o $OUTFILE.tex | sed "s/^/PANDOC-TEX /g"

	cat Main.tex | sed "s/§/$OUTFILE/" > "ALGT_$OUTFILE.tex"

	pdflatex -halt-on-error "ALGT_$OUTFILE.tex" | sed "s/^/LATEX-$OUTFILE: /g"
done

cp *.pdf ../



