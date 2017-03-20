#! /bin/bash

# Arguments: Inputfiles (mds) Template (no .tex) SaveAs
PANDOC_EXTENSIONS="--tab-stop=8 -f markdown+link_attributes+grid_tables+pandoc_title_block+pipe_tables+implicit_header_references --listings --chapters "

PANDOC_SLIDESHOW="--slide-level 2 --listings -t beamer"

cat $1 | xargs cat > all.md
if [ $2 == "PresentationTemplate" ]
then
	echo "Running pandoc"
	pandoc $PANDOC_EXTENSIONS $PANDOC_SLIDESHOW all.md -o Main0.tex
	cat Main0.tex | sed "s/begin{frame}/begin{frame}[fragile]/g" > Main.tex
	pdflatex $2.tex
	mv $2.pdf $3.pdf
else
	pandoc $PANDOC_EXTENSIONS all.md -o Main.tex
	pdflatex $2.tex
	mv $2.pdf $3.pdf

fi

