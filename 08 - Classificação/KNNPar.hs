{-|
Module      : kNN
Description : Gradient Descent
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com

Applies k-NN to a classification problem.
-}

module KNNPar where

import Control.Parallel.Strategies
import Data.List
import Data.Ord
import Vector
import Dados

type Point = ([Double], Double)

-- |'parseFile' parses a space separated file 
-- to a list of lists of Double
parseFile :: String -> [Point]
parseFile file = map parseLine (lines file)
  where
    parseLine l = splitN (doubled l) 
    splitN  l = (1 : (init l), last l)
    doubled l  = map toDouble (words l)
    toDouble  w = read w :: Double

acc :: Eq a => [a] -> [a] -> Double
acc pred real = tp/n
  where
    tp = fromIntegral $ length $ filter (id) $ zipWith (==) pred real
    n  = fromIntegral $ length pred

mostFrequent :: Ord a => [a] -> a
mostFrequent l = head . maximumBy (comparing length) . group . sort $ l

    
knnPar :: Int -> ChunksOf [Point] -> [Point] -> [Double]
knnPar k train test = map knearest test
  where
    knearest t = mostFrequent
               $ map snd
               $ takeOrdered k (distance t) train
    distance (t1,y1) (t2,y2) = (euclid t1 t2, y2)
    euclid t1 t2 = sum $ (t1 .-. t2).^2
