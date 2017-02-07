#! /bin/bash


cd src
hlint *.hs */*.hs
./searchTodo.sh

cd Assets/IntegrationTests
rm *.FAILED
cd ../..

echo "createAssets False \"Assets\" \"Assets.hs\"" | ghci -fno-warn-tabs Utils/CreateAssets.hs 
cd ..

stack build

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
