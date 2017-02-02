#! /bin/bash


cd src
hlint *.hs */*.hs
./searchTodo.sh
echo "createAssets False \"Assets\" \"Assets.hs\"" | ghci -fno-warn-tabs Utils/CreateAssets.hs 
cd ..

stack build

cp .stack-work/install/x86_64-linux/lts-7.15/8.0.1/bin/ALGT ALGT
VERSION=`./ALGT -v | sed "s/, .*$//"`
cp ALGT "binaries/ALGT-$VERSION"
./ALGT "--test"
