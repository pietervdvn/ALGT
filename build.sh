#! /bin/bash

mkdir .bin >/dev/null

cp -r src/* .bin

cd .bin
ghc Main.hs -fno-warn-tabs
cd ..
cp .bin/Main ALGT

./ALGT -h
