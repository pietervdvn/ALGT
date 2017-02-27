#! /bin/bash


./searchTodo

cd src
ls *.hs */*.hs */*/*.hs | sed /Assets.hs/d | xargs hlint

cd Assets/IntegrationTests
rm *.FAILED
cd ../..

echo "Creating real assets"
echo "createAssets False \"Assets\" \"Assets.hs\"" | ghci -fno-warn-tabs Utils/CreateAssets.hs 
cd ..

echo "Stack build"
stack build
STACKEXIT="$?"
echo "EXIT CODE: $STACKEXIT"


cd src
echo "Recreating dev assets" 
echo "createAssets True \"Assets\" \"Assets.hs\"" | ghci -fno-warn-tabs Utils/CreateAssets.hs 
cd ..

if [[ $STACKEXIT -ne 0 ]]
then
	echo "FAILED: stack did not exit properly. Quitting now"
	exit
fi



rm ALGT
rm ALGT-*

cp .stack-work/install/x86_64-linux/lts-7.15/8.0.1/bin/ALGT ALGT

VERSION=`./ALGT -v | sed "s/, .*$//"`
if ./ALGT "--test"
then
	cp ALGT "binaries/ALGT-$VERSION"
	cp ALGT "ALGT-$VERSION"
	echo "Moved new build to binaries"
else
	mv ALGT "ALGT-FAIL"
	echo "Marked build as fail"
fi


