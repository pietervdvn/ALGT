FILE="DocumentTemplate.tex"
rm $FILE
touch $FILE
while IFS='' read -r line || [[ -n "$line" ]]
do
	if [ "$line" == "%DeclareUnicodeCharacter" ]
	then
		cat .bin/DeclareUnicodeCharacter >> $FILE
	elif [ "$line" == "%LITERATE" ]
	then
		echo -n "  literate=" >> $FILE		
		cat .bin/UnicodeLiterals >> $FILE
	else
		echo -E "$line" >> $FILE
		echo -E "$line"
	fi
done < DocumentTemplate.tex.pre
