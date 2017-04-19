all:
	make manual
	make manual
	make workshop
	make workshop
	make thesis
	make thesis


deps:
	make images-extra
	make templates

workshop:
	cp Workshop/Theme/* .bin/ && \
	cd .bin/ && ../compileDocument.sh "../workshop-files" "PresentationTemplate" "Workshop"

manual:
	# Assumption: autoPreprocess already dropped in all the .md's
	cd .bin/ && ../compileDocument.sh "../Manual-files" "DocumentTemplate" "ALGT_Manual" 


thesis:
	cd .bin && ../compileDocument.sh "../Thesis-files" "ThesisTemplate" "Thesis"

focus:
	cd .bin/ && ../compileDocument.sh "../focus-files" "FocusTemplate" "Focus"

templates:
	make focus-template
	make thesis-template
	cp *Template.tex .bin/

focus-template:
	cat DocumentTemplate.tex | sed "/NOFOCUS/d" > .bin/FocusTemplate.tex

thesis-template:
	cat DocumentTemplate.tex | sed "/NOTHESIS/d" > .bin/ThesisTemplate.tex


images:
	cd .bin; ../makeImages.sh

images-extra:
	cp */*.svg .bin/
	cp */*.png .bin/
	make images

clean:
	rm -rf .bin/*
