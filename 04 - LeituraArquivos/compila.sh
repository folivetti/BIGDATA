#!/bin/bash

ghc -o bin/$1 --make $1.hs -threaded -eventlog -rtsopts -i../Bibliotecas
rm *.hi *.o
