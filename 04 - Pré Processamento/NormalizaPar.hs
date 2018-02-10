{-|
Module      : Distances
Description : example of distances metrics
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com

Euclidean, Minkowski, Jaccard, Normalization, Standardization
-}

module NormalizaPar where

import Data.List (foldl1')
import Data.List.Split (chunksOf)
import Control.Parallel
import Control.Parallel.Strategies
import Vector
import Dados

-- |'padroniza' as colunas de uma matriz de dados
padronizaPar :: ChunksOf [[Double]] -> ChunksOf [[Double]]
padronizaPar chunks = parmap (map (\xi -> (xi .-. media) ./. desvio)) chunks 
  where
    media      = mapReduce id (.+.) chunks
    desvio     = map (\x -> sqrt (x/n))
               $ mapReduce (\xi -> (xi .-. media).**2) (.+.) chunks
    n          = sum $ map length' chunks  
    
-- |'norm' calculates the norm vector
normalizaPar :: ChunksOf [[Double]] -> ChunksOf [[Double]]
normalizaPar chunks = parmap normaliza chunks

normaliza :: [[Double]] -> [[Double]]
normaliza x = map normaliza' x
  where
    normaliza' xi = xi ./ (norma xi)
    norma xi      = sqrt . sum $ xi .^ 2
