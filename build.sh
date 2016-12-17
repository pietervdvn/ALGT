#! /bin/bash


cd src
hlint *.hs */*.hs
./searchTodo.sh
cd ..

mkdir .bin >/dev/null

cp -r src/* .bin

cd .bin
ghc Main.hs -fno-warn-tabs
cd ..
cp .bin/Main ALGT

echo "Running default examples"
./ALGT Examples/STFL.typesystem Examples/STFL.example e -l -r "::" > Output/typings.txt 
