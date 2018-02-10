{-# LANGUAGE  NoMonomorphismRestriction #-}
{-|
Module      : NaiveBayes
Description : Gradient Descent
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com

Applies Naive Bayes to a classification problem with categorical variables.
-}

module NaiveBayesPar where

import Control.Parallel.Strategies
import Data.List
import Data.Ord
import qualified Data.Map.Strict as M
import Data.List.Split (chunksOf)

import Vector
import Dados

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
  
freqPar :: (NFData a, Ord a) => ChunksOf [a] -> [(a, Double)]
freqPar xs = mapReduceByKey (\x -> (x,1.0)) (+) xs

freqyPar :: ChunksOf [Point] -> M.Map String Double
freqyPar xys = M.fromList $ freqPar y
  where y = parmap (map snd) xys
  
freqxyPar :: ChunksOf [Point] -> M.Map (Integer, String, String) Double
freqxyPar xys = M.fromList $ freqPar $ ixy
  where
    ixy  = parmap (\xy -> concat $ map enumerate xy) xys
    enumerate (xi, yi) = zip3 [0,1..] xi (repeat yi)

naiveBayesPar :: ChunksOf [Point] -> [Point] -> [String] -> Dict -> [String]
naiveBayesPar train test klass dict = map classify test
  where
    classify (t, label) = snd $ last $ sortByKey [(prob t k, k) | k <- klass]

    prob t k            = (loglikelihood t k) + (logprior k)
        
    logprior k        = log $ (fy M.! k) / n
    loglikelihood t k = 
                    let nk = fy M.! k
                        tuplify = zip3 [0,1..] t (repeat k)
                        addOneSmooth (di, xi) = (xi+1)/(nk + di)
                     in sum 
                      $ map log
                      $ map addOneSmooth
                      $ zip ds
                      $ map (\t -> M.findWithDefault 0 t fxy) tuplify                        

    fxy = freqxyPar train
    fy  = freqyPar train    
    
    n = length' train
    ds = map length' dict

acc :: Eq a => [a] -> [a] -> Double
acc pred real = tp/n
  where
    tp = fromIntegral $ length $ filter (id) $ zipWith (==) pred real
    n  = fromIntegral $ length pred


