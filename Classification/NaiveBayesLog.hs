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
  | b1 > b2 = LT
  | b1 < b2 = GT
  | b1 == b2 = EQ

length' l = fromIntegral $ length l

probability variables x = countX / n
  where
    countX = length' $ filter (==x) variables
    n      = length' variables

naiveBayes :: [Point] -> [Point] -> Dict -> [String] -> [String]
naiveBayes train test dict klass = map classify test
  where
    classify (t, label) = argmax [(k, logprob t k) | k <- klass]

    logprob t k = (classprior k) + (loglikelihood t k)

    classprior    k   = log $ (probability klasses k)
    loglikelihood t k = sum $ [loglike i ti k | (i, ti) <- enumerate t]

    enumerate l    = zip [0,1..] l
    loglike i ti k = log $ probability (getData i k) ti

    klasses     = map snd train
    getData i k = map (!!i) (getVars $ filterK k train)
    getVars     = map fst
    filterK k   = filter (\x -> snd x == k)

    argmax l = fst $ head $ sortBy sortTuple l


accuracy :: Eq a => [a] -> [a] -> Double
accuracy pred real = tp/n
  where
    tp = length' $ filter (id) $ zipWith (==) pred real
    n  = length' pred

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
    print (accuracy pred (map snd test))
