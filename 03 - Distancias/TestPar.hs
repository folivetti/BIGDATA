{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Distances
Description : example of distances metrics
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com

Euclidean, Minkowski, Jaccard, Normalization, Standardization
-}

module Main where

import Control.Monad
import System.Random
import Data.List.Split (chunksOf)
import Formatting
import Formatting.Clock
import System.Clock
import Vector
import DistancesPar

nchunks = 1500  

-- |'main' executa programa principal
main :: IO ()
main = do
    x  <- replicateM 5000000 $ randomRIO (1.0,6.0)
    y  <- replicateM 5000000 $ randomRIO (1.0,6.0)

    let chunks  = chunksOf nchunks $ zip x y
    let chunksX = chunksOf nchunks $ x

    start <- getTime Monotonic
    
    print (minkowskiPar 1 chunks)
    print (cosinePar  chunks)
    print (jaccardPar chunks)

   
    stop <- getTime Monotonic
    fprint (timeSpecs % "\n") start stop
    return ()
