#! /bin/bash


cd src
hlint *.hs */*.hs
./searchTodo.sh
echo "createAssets False \"Assets\" \"Assets.hs\"" | ghci -fno-warn-tabs Utils/CreateAssets.hs 
cd ..

stack build
cp .stack-work/install/x86_64-linux/lts-7.15/8.0.1/bin/ALGT ALGT

VERSION=`./ALGT -v | sed "s/, .*$//"`
if ./ALGT "--test"
then
	mv ALGT "binaries/ALGT-$VERSION"
	echo "Moved new build to binaries"
else
	mv ALGT "ALGT-FAIL"
	echo "Marked build as fail"
fi
