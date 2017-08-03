{-|
Module      : NaiveBayes
Description : Gradient Descent
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com

Applies Naive Bayes to a classification problem with categorical variables.
-}

module Main where

import System.IO
import System.Environment
import Control.Parallel.Strategies
import Data.List
import Data.Ord
import qualified Data.Map as M
import Data.List.Split (chunksOf)

type Point = ([String], String)
type Dict  = [[String]]

-- |'parseFile' parses a space separated file 
-- to a list of lists of Double
parseFile :: String -> ([Point], Dict, [String])
parseFile file = (dataset, dict, klass)
  where
    dataset     = map parseLine (lines file)
    parseLine l = splitN (words l) 
    splitN  l   = (init l, last l)
    dict        = map nub $ transpose $ map fst dataset 
    klass       = nub $ map snd dataset

sortTuple (a1,b1) (a2, b2)
  | a1 > a2 = LT
  | a1 < a2 = GT
  | a1 == a2 = EQ

naiveBayes :: [Point] -> [Point] -> Dict -> [String] -> [String]
naiveBayes train test dict klass = map classify test
  where
    classify (t, label) = snd $ head $ sortBy sortTuple [(prob t k, k) | k <- klass]

    prob t k            = (likelihood t k) * (classprior k) / (predictor t)
    
    classprior k = pk M.! k
    predictor  x = foldl1 (*) $ zipWith (M.!) px x
    
    pk = M.fromListWith (+) $ map (\d -> (snd d, 1)) train
    px = [M.fromListWith (+) $ map (\xi -> (xi, 1)) x | x<-xset]
    xset    = transpose $ map fst train

    likelihood x k = foldl1 (*) $ zipWith (\xi l -> (pXK xi l)/(pk M.! k)) x (xsetK k)
    pXK xi l = fromIntegral $ length $ filter (==xi) l

    xsetK k = transpose $ map fst $ filter (\t -> snd t == k) train

acc :: Eq a => [a] -> [a] -> Double
acc pred real = tp/n
  where
    tp = fromIntegral $ length $ filter (id) $ zipWith (==) pred real
    n  = fromIntegral $ length pred

-- |'main' executa programa principal
main :: IO ()
main = do
    args <- getArgs
    fileTrain <- readFile (args !! 0)
    fileTest <- readFile (args !! 1)
    let (train, dict, klass) = parseFile fileTrain
    let (test,_,_) = parseFile fileTest
    print dict
    print klass
    let pred = naiveBayes train test dict klass
    print (acc pred (map snd test))
