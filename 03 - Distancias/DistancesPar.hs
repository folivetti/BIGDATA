{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Distances
Description : example of distances metrics
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com

Euclidean, Minkowski, Jaccard, Normalization, Standardization
-}

module DistancesPar where

import Control.Parallel
import Control.Parallel.Strategies
import Data.List (foldl1')

import Vector
import Dados

-- |'minkowski' distance
minkowskiPar :: Double -> ChunksOf [(Double, Double)]
                ->  Double
minkowskiPar p chunks = raizP 
                      $ mapReduce minkowski (+) 
                      $ chunks
  where
    raizP x = x ** (1.0/p)
    minkowski (x,y) = (abs (x-y)) ** p

cosinePar :: ChunksOf [(Double, Double)] -> Double
cosinePar chunks = dotXY / (normX * normY)
  where
    (dotXY, normX, normY) = mapReduce prod3 soma chunks
    soma (x1,y1,z1) (x2,y2,z2) = (x1+x2, y1+y2, z1+z2)
    prod3 (x,y) = (x*y, x*x, y*y)

-- |'jaccard' distance
jaccardPar :: ChunksOf [(Double, Double)] -> Double
jaccardPar chunks = 1 - (prod / somatoria)
  where
    (prod, somatoria)    = mapReduce prodSoma soma chunks
    soma (x1,y1) (x2,y2) = (x1+x2, y1+y2)
    prodSoma (x,y)       = (x*y, somaMin1 x y)
    somaMin1 x y         = (min 1.0) (x+y)


-- |'padroniza' as colunas de uma matriz de dados
padronizaPar :: ChunksOf [[Double]] -> ChunksOf [[Double]]
padronizaPar chunks = parmap padroniza' chunks
  where
    padroniza' = map (\xi -> (xi .-. media) ./. desvio)
    media      = calcMediaPar n chunks
    desvio     = calcStdPar n media chunks
    n          = sum $ map length' chunks

calcMediaPar :: Double -> ChunksOf [[Double]] -> [Double]
calcMediaPar n chunks = map (/n)
                        $ mapReduce id (.+.) chunks
                        
calcStdPar :: Double -> [Double] 
              -> ChunksOf [[Double]] -> [Double]
calcStdPar n media chunks = map (\x -> sqrt (x/n))
                          $ mapReduce dev2Mean (.+.) chunks
  where
    dev2Mean xi = xi .-. media

-- |'norm' calculates the norm vector
normalizaPar :: ChunksOf [[Double]] -> ChunksOf [[Double]]
normalizaPar chunks = parmap normaliza chunks 

normaliza :: [[Double]] -> [[Double]]
normaliza x = map normaliza' x
  where
    normaliza' xi = xi ./ (norma xi)
    norma xi      = sqrt . sum $ xi .^ 2
