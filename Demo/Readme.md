Hi! This is the demo!

Overview of the directory
=========================

- Workshop.pdf are the slides you just saw.
- ALGT_Manual.pdf contains an in-depth tutorial and overview of builtin commands. If you want to know what builtin syntax/functions/... exists, open it
- ALGT and ALGT-win.exe is the program itself
- DemoDyn.language and demodyn.demo: the simple language that is built in the first part
- Template.language: a good start to start your new language from scratch
- language.lang: syntax highlighting for `gedit` (the default notepad of Ubuntu). Drop this file into `/home/pietervdvn/.local/share/gtksourceview-3.0/language-specs/` to get it working.




Common flags to execute ALGT
=============================

As a reminder, the basic flags are:


To print a simple parsetree of each line in the demo:

	./ALGT Language-definition ExampleFile syntax -l
	./ALGT DemoDyn.language demodyn.demo expr -l

To generate parsetrees as .svg images:

	./ALGT Language-definition ExampleFile syntax -l --ptsvg Filename
	./ALGT DemoDyn.language demodyn.demo expr -l --ptsvg ParseTrees

To run a relation:

	./ALGT Language-definition ExampleFile syntax -l -r relationSymbol
	./ALGT DemoDyn.language demodyn.demo expr -l -r →

To run interactively:

	./ALGT Language-definition -i relationSymbol
	./ALGT DemoDyn.language -i →





Unicode characters
==================

On linux:
---------

1. Press `Left-CTRL + Shift + U`
2. Release all
3. Type the hexcode of the character (e.g. `2192` to get →)

On windows:
-----------

1. Hold down `Alt`
2. Type `+` (that is the plus of the numeric keypad)
3. Type the hexcode (e.g. `2192` to get →)
4. Release `Alt`

or alternatively, visit [this excellent guide](https://help.ubuntu.com/community/Installation).

On Os X
--------

1. Go to `International system preferences` → `input menu`
2. Check the box next to "Unicode hex"
3. Switch to unicode input in the menu bar
4. Hold `Alt`, enter the hexadecimal unicode value


