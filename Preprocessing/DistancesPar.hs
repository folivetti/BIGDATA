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
import Control.Parallel
import Control.Parallel.Strategies
import Data.List.Split (chunksOf)

nchunks = 5000

-- |'minkowski' distance
minkowski :: Double -> [Double] -> [Double] -> Double
minkowski p x y  = (sum partialSum) ** (1.0/p)
  where
    partialSum = map parSum chunks `using` parList rdeepseq
    parSum     = sum . (map pointwiseDist)
    pointwiseDist (xi,yi) = (abs (xi - yi)) ** p
    chunks = chunksOf nchunks $ zip x y

-- |'euclidean' is just a special case of minkowski
euclidean :: [Double] -> [Double] -> Double
euclidean = minkowski 2

-- |'cosine' distance of two vectors
cosine :: [Double] -> [Double] -> Double
cosine x y = (dotprod x y) / (norm' x * norm' y)
  where
    dotprod u v = sum $ (map partialSum (chunks u v) `using` parList rdeepseq)
    partialSum chunk = sum $ map (\(u',v') -> u'*v') chunk 
    norm' u  = dotprod u u
    chunks u v = chunksOf nchunks $ zip u v

-- |'standardize' a vector
standardize :: [Double] -> [Double]
standardize x = meanX `pseq` 
                  (fromMean `pseq` 
                     (stdX `pseq` toCenter))
  where
    toCenter = concat $ (map (map center) chunks `using` parList rdeepseq)
    center = \xi -> (xi - meanX)/stdX

    meanX  = sumX / (length' x)
    sumX  = sum $ (map sum chunks `using` parList rdeepseq)
    chunks = chunksOf nchunks x

    fromMean   = concat $ (map (map (\xi -> xi - meanX)) chunks
                             `using` parList rdeepseq)

    stdX  = sqrt varX
    varX  = sumToMean / (length' x)
    sumToMean = sum $ (map sumSquare chunksDev `using` parList rdeepseq)
    sumSquare cc = sum $ map (\ci -> ci*ci) cc
    chunksDev = chunksOf nchunks fromMean
  
    length' l = fromIntegral $ length l

-- |'maxminScale' scales vector to [0,1]
maxminScale :: [Double] -> [Double]
maxminScale x = minimum' `par` maximum' `pseq` parScale
  where
    parScale = concat $ (map (map scale) chunks `using` parList rdeepseq)
    scale xi = (xi - minimum') / (maximum' - minimum')
    minimum' = minimum x
    maximum' = maximum x
    chunks = chunksOf nchunks x

-- |'norm' calculates the norm vector
norm :: [Double] -> [Double]
norm x = nx `seq` concat $ (map (map (/nx)) chunks `using` parList rdeepseq)
  where
    nx = sqrt . sum $ (map parSum chunks `using` parList rdeepseq)
    parSum c = sum $ map (^2) c
    chunks = chunksOf nchunks x

-- |'main' executa programa principal
main :: IO ()
main = do
    x  <- replicateM 5000000 $ randomRIO (1.0,6.0)
    y  <- replicateM 5000000 $ randomRIO (1.0,6.0)

    let x' = x `seq` x
    let y' = y `seq` y

    print (euclidean x' y') 
    print (minkowski 1 x' y')
    print (cosine  x' y')

    print (sum $ standardize x) 
    print (sum $ maxminScale x)
    print (sum $ norm x) 
