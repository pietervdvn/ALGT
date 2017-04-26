DECLARED=`cat .bin/DeclareUnicodeCharacter | tr "\n" "^"`
LITERATE=`cat .bin/UnicodeLiterals | tr -d "\n"`
cat DocumentTemplate.tex.pre | sed $SEDSUBS # | tr "^" "\n" | sed "s/%INPUTLITERATE/$LITERATE/g" > DocumentTemplate.tex
