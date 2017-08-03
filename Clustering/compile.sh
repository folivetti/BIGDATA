#!/bin/bash

ghc -o bin/$1 $1.hs 
rm $1.hi $1.o
