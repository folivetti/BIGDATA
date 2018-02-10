{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Exemplos
Description : exemplos de paralelismo
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com

Exemplos dos cálculos de distancia usando estratégias paralelas.
-}

module Main where

import System.IO
import Control.Monad
import System.Random
import System.Environment
import Control.Parallel.Strategies
import Data.List (foldl1')
import Data.List.Split (chunksOf)
import Formatting
import Formatting.Clock
import System.Clock
import Vector

type ChunksOf a = [a]

minkowskiPar :: Double -> ChunksOf (Vector (Double, Double))
             ->  Double
minkowskiPar p chunks = raizP $ foldl1' (+) 
                      $ (map (minkowski p) chunks
                             `using` parList rdeepseq)
                      where
                        raizP x = x ** (1.0/p)
                             
minkowski :: Double -> Vector (Double, Double) 
             -> Double
minkowski p points  = somatoria
  where
    somatoria = sum normP
    normP     = absVal .** p
    absVal    = map abs diffVecs 
    diffVecs  = map (uncurry (-)) points

cosinePar :: ChunksOf (Vector (Double, Double))
          -> Double
cosinePar chunks = dotXY / (normX * normY)
  where
    (dotXY, normX, normY) = foldl1' soma (map prod3 chunks
                                   `using` parList rdeepseq)

    soma (x1,y1,z1) (x2,y2,z2) = (x1+x2, y1+y2, z1+z2)
    prod3 points = foldl1' soma (map (\(x,y) -> (x*y, x*x, y*y) ) 
                                           points)    
                                           
-- |'jaccard' distance
jaccard :: ChunksOf (Vector (Double, Double))
        -> Double
jaccard chunks = 1 - (prod / somatoria)
  where
    (prod, somatoria) = foldl1' soma 
                        $ (map prodSoma  chunks 
                                    `using` parList rdeepseq)
    soma (x1,y1) (x2,y2) = (x1+x2, y1+y2)
    prodSoma points = foldl1' soma $ map (\(x,y) -> (x*y, somaMin1 x y)) points
    somaMin1 x y         = (min 1.0) (x+y)                                           
    
main = do
  u  <- replicateM 5000000 $ randomRIO (1.0,6.0)
  v  <- replicateM 5000000 $ randomRIO (1.0,6.0)
  let uv = chunksOf 10000 $ zip u v
  
  start <- getTime Monotonic
  print (minkowskiPar 2 uv)
  print (cosinePar uv)
  print (jaccard uv)      
  stop <- getTime Monotonic

  fprint (timeSpecs % "\n") start stop
