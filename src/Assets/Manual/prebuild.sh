mkdir .bin 2> /dev/null
cat DocumentTemplate.tex | sed "/%NOFOCUS/d" > FocusTemplate.tex
