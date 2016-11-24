#! /bin/bash

ghc Main.hs -fno-warn-tabs

rm *.hi *.o
rm */*.hi */*.o

./Main Examples/STFL.typesystem Examples/STFL.example t eval --line-by-line --step 
