all:
	make manual
	make manual
	make workshop
	make workshop


deps:
	make images-extra
	make templates

workshop:
	cp Workshop/Theme/* .bin/ && \
	cd .bin/ && ../compileDocument.sh "../workshop-files" "PresentationTemplate" "../Output/Workshop"

manual:
	# Assumption: autoPreprocess already dropped in all the .md's
	cd .bin/ && ../compileDocument.sh "../Manual-files" "DocumentTemplate" "../Output/ALGT_Manual" 

focus:
	cd .bin/ && ../compileDocument.sh "../focus-files" "FocusTemplate" "../Output/Focus"

templates:
	make focus-template
	cp *Template.tex .bin/

focus-template:
	cat DocumentTemplate.tex | sed "/%NOFOCUS/d" > .bin/FocusTemplate.tex

images:
	cd .bin; ../makeImages.sh

images-extra:
	cp Manual/*.svg .bin/
	cp Manual/*.png .bin/
	make images

clean:
	rm -rf .bin/*
