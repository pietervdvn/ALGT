#! /bin/bash

git pull

mkdir .bin >/dev/null

cp -r src/* .bin

cd .bin
ghc Main.hs -fno-warn-tabs
cd ..
cp .bin/Main ALGT

./ALGT Examples/STFL.typesystem Examples/STFL.example t eval --line-by-line --step 
