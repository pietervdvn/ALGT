#! /bin/bash

# Arguments: Inputfiles (mds) Template (no .tex) SaveAs
PANDOC_EXTENSIONS="--tab-stop=8 -f markdown+raw_tex+link_attributes+grid_tables+pandoc_title_block+pipe_tables+implicit_header_references --listings"

cat $1 | sed "/^#/d" | xargs cat > all.md
if [ $2 == "PresentationTemplate" ]
then
	echo "Running pandoc"
	pandoc $PANDOC_EXTENSIONS --slide-level 2 --chapters --listings -t beamer all.md -o Main0.tex
	cat Main0.tex | sed "s/begin{frame}/begin{frame}[fragile]/g" | sed "s/\[fragile\]\[fragile\]/[fragile]/" > Main.tex
else
	pandoc $PANDOC_EXTENSIONS --chapters all.md -o Main.tex

fi

cp $2.tex $3.tex
pdflatex -interaction nonstopmode -halt-on-error -file-line-error -shell-escape $3.tex
bibtex $3

mv $3.pdf  "../Output/$3.pdf"

