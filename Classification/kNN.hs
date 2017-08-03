{-|
Module      : kNN
Description : Gradient Descent
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com

Applies k-NN to a classification problem.
-}

module Main where

import System.IO
import System.Environment
import Control.Parallel.Strategies
import Data.List
import Data.Ord
import Data.List.Split (chunksOf)

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

sortTuple (a1, b1) (a2, b2)
  | a1 < a2  = LT
  | a1 > a2  = GT
  | a1 == a2 = EQ

acc :: Eq a => [a] -> [a] -> Double
acc pred real = tp/n
  where
    tp = fromIntegral $ length $ filter (id) $ zipWith (==) pred real
    n  = fromIntegral $ length pred

mostFrequent :: Ord a => [a] -> a
mostFrequent l = head . maximumBy (comparing length) . group . sort $ l

knn :: [Point] -> [Point] -> Int -> [Double]
knn train test k = map (knearest) test
  where
    knearest t = mostFrequent 
                 $ map snd 
                 $ take k 
                 $ sortBy sortTuple 
                 $ map (distance t) train
    distance (t1,y1) (t2,y2) = (euclid t1 t2, y2)
    euclid t1 t2 = sum $ zipWith sqdiff t1 t2
    sqdiff t1 t2 = (t1 - t2)^2

-- |'main' executa programa principal
main :: IO ()
main = do
    args <- getArgs
    fileTrain <- readFile (args !! 0)
    fileTest <- readFile (args !! 1)
    let k = read (args !! 2) :: Int
    let train = parseFile fileTrain
    let test = parseFile fileTest
    let pred = (knn train test k)
    print (acc pred (map snd test))
