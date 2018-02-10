{-|
Module      : kMeans
Description : k-Means
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com

k-Means Clustering algorithm
-}

module KMeans where

import Data.List
import Data.Ord
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

kmeans it points clusters
  | it == 0               = clusters
  | clusters' == clusters = clusters
  | otherwise             = kmeans (it-1) points clusters'
  where
    clusters' = emStep points clusters

emStep points clusters = maximizationStep $ estimationStep points clusters

estimationStep :: [[Double]] -> [[Double]] -> [[[Double]]]
estimationStep points clusters = map unTuple
                               $ groupByValue
                               $ sortByValue
                               $ zip points 
                               $ assign points clusters
  where
    unTuple = map fst

maximizationStep :: [[[Double]]] -> [[Double]]
maximizationStep candidates = map mean candidates
  where
    mean xs = (foldl' (.+.) (head xs) (tail xs)) ./ (fromIntegral $ length xs)

assign :: [[Double]] -> [[Double]] -> [Integer]
assign points clusters = map closestTo points
  where
    closestTo p = argmin $ map (euclid p) clusters
    euclid pi ci = sum $ map (^2) $ zipWith (-) pi ci
    argmin xs = fst $ head $ sortByValue $ zip [0..] xs


