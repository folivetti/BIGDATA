{-|
Module      : kMeans
Description : k-Means
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com

k-Means Clustering algorithm
-}

module KMeansPar where

import Control.Parallel.Strategies
import Data.List
import Data.Ord
import Data.List.Split (chunksOf)
--import qualified Numeric.LinearAlgebra as H 

import Vector
import Dados

-- |'parseFile' parses a space separated file 
-- to a list of lists of Double
parseFile :: String -> [[Double]]
parseFile file = map parseLine (lines file)
  where
    parseLine l = map toDouble (words l)
    toDouble  w = read w :: Double

firstClusters idxs points = [ points !! i | i <- idxs ]

kmeans :: Int -> ChunksOf [[Double]] -> [[Double]] 
       -> [[Double]]
kmeans it points clusters
  | it == 0               = clusters
  | clusters' == clusters = clusters
  | otherwise             = kmeans (it-1) points clusters'
  where
    clusters' = emStep points clusters

emStep :: ChunksOf [[Double]] -> [[Double]] -> [[Double]]
emStep points clusters = map (\(idx, (xs,n)) -> xs ./ n)
                       $ mapReduceByKey (\pi -> (assign pi clusters, (pi,1.0))) soma points
  where
    soma (xs1, y1) (xs2, y2) = (xs1 .+. xs2, y1+y2)
    
assign :: [Double] -> [[Double]] -> Integer
assign point clusters = closestTo point
  where
    closestTo p = argmin $ map (euclid p) clusters
    euclid pi ci = sum $ map (^2) $ zipWith (-) pi ci
    argmin xs = fst $ head $ sortByValue $ zip [0..] xs


