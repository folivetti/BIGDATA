{-|
Module      : kMeans
Description : k-Means
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com

k-Means Clustering algorithm
-}

module Main where

import System.IO
import System.Random
import System.Environment
import Control.Parallel.Strategies
import Data.List
import Data.Ord
import Data.List.Split (chunksOf)
import qualified Numeric.LinearAlgebra as H 

-- |'parseFile' parses a space separated file 
-- to a list of lists of Double
parseFile :: String -> [[Double]]
parseFile file = map parseLine (lines file)
  where
    parseLine l = map toDouble (words l)
    toDouble  w = read w :: Double

(.+.) l1 l2 = zipWith (+) l1 l2
(./) l1 v = map (/v) l1

sortTuple (a1,b1) (a2,b2)
  | b1 < b2 = LT
  | b1 > b2 = GT
  | b1==b2  = EQ

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
                               $ groupBy compTuple
                               $ sortBy sortTuple
                               $ zip points $ assign points clusters
  where
    unTuple = map fst
    compTuple (a1,b1) (a2,b2) = b1==b2

maximizationStep :: [[[Double]]] -> [[Double]]
maximizationStep candidates = map mean candidates
  where
    mean xs = (foldl' (.+.) (head xs) (tail xs)) ./ (fromIntegral $ length xs)

assign points clusters = map closestTo points
  where
    closestTo p = argmin $ map (euclid p) clusters
    euclid pi ci = sum $ map (^2) $ zipWith (-) pi ci
    argmin xs = fst $ head $ sortBy sortTuple $ zip [0..] xs

-- |'main' executa programa principal
main :: IO ()
main = do
    args <- getArgs

    fileTrain <- readFile (args !! 0)
    let k = read (args !! 1) :: Int
    let it = read (args !! 2) :: Int
    let train = parseFile fileTrain

    g <- getStdGen
    let idxs = take k (randomRs (0, length train) g)
    let clusters  = firstClusters idxs train
    let clusters' = kmeans it train clusters
    print (assign train clusters')
