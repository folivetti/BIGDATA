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
import Control.Parallel
import Control.Parallel.Strategies
import Data.List (foldl')
import Data.List.Split (chunksOf)
import Formatting
import Formatting.Clock
import System.Clock

nchunks = 1500

fst' (w,_,_) = w
snd' (_,w,_) = w
trd' (_,_,w) = w

-- |'minkowski' distance
minkowski :: Double -> [[(Double,Double)]] -> Double
minkowski p chunks  = (sum partialSum) ** (1.0/p)
  where
    partialSum = map parSum chunks `using` parList rdeepseq
    parSum     = sum . (map pointwiseDist)
    pointwiseDist (xi,yi) = (abs (xi - yi)) ** p

-- |'cosine' distance of two vectors
cosine :: [[(Double,Double)]] -> Double
cosine chunks = normXY `pseq` (dotprodXY  / (normx * normy))
  where
    normx     = fst' normXY
    normy     = snd' normXY
    dotprodXY = trd' normXY
    normXY = foldr sumTuple (0,0,0) $ (map partialNorm chunks
                                       `using` parList rdeepseq)

    sumTuple (x1,y1,z1) (x2,y2,z2) = (x1+x2, y1+y2, z1+z2)
    partialNorm chunk = foldl' sumTuple (0,0,0) $ map (\(u',v') -> (u'*u', v'*v',u'*v')) chunk

-- |'standardize' a vector
standardize :: [[Double]] -> [Double]
standardize chunks = meanX `pseq` 
                  (fromMean `pseq` 
                     (stdX `pseq` toCenter))
  where
    toCenter = concat $ (map (map center) chunks `using` parList rdeepseq)
    center = \xi -> (xi - meanX)/stdX

    meanX  = sumX / len
    sumX  = sum $ (map sum chunks `using` parList rdeepseq)

    fromMean   = concat $ (map (map (\xi -> xi - meanX)) chunks
                             `using` parList rdeepseq)

    stdX  = sqrt varX
    varX  = sumToMean / len
    sumToMean = sum $ (map sumSquare chunksDev `using` parList rdeepseq)
    sumSquare cc = sum $ map (\ci -> ci*ci) cc
    chunksDev = chunksOf nchunks fromMean
  
    len = fromIntegral $ sum $ map length chunks

-- |'maxminScale' scales vector to [0,1]
maxminScale :: [[Double]] -> [Double]
maxminScale chunks = maxmin `pseq` parScale
  where
    parScale = concat $ (map (map scale) chunks `using` parList rdeepseq)
    scale xi = (xi - minimum') / (maximum' - minimum')
    minimum' = minimum $ fst maxmin
    maximum' = maximum $ snd maxmin
    maxmin   = unzip $ (map (\x -> (minimum x, maximum x)) chunks
                            `using` parList rdeepseq)

-- |'norm' calculates the norm vector
norm :: [[Double]] -> [Double]
norm chunks = nx `pseq` concat $ (map (map (/nx)) chunks `using` parList rdeepseq)
  where
    nx = sqrt . sum $ (map parSum chunks `using` parList rdeepseq)
    parSum c = sum $ map (^2) c

-- |'main' executa programa principal
main :: IO ()
main = do
    x  <- replicateM 5000000 $ randomRIO (1.0,6.0)
    y  <- replicateM 5000000 $ randomRIO (1.0,6.0)

    let chunks  = chunksOf nchunks $ zip x y
    let chunksX = chunksOf nchunks $ x

    start <- getTime Monotonic
    
    print (minkowski 1 chunks)
    print (cosine  chunks)

    print (sum $ standardize chunksX) 
    print (sum $ maxminScale chunksX)
    print (sum $ norm chunksX) 
    
    stop <- getTime Monotonic
    fprint (timeSpecs % "\n") start stop
