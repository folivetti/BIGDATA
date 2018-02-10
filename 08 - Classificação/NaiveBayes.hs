{-# LANGUAGE  NoMonomorphismRestriction #-}
{-|
Module      : NaiveBayes
Description : Gradient Descent
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com

Applies Naive Bayes to a classification problem with categorical variables.
-}

module NaiveBayes where

import Data.List
import Data.Ord
import qualified Data.Map.Strict as M

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
  
freq :: (Ord a) => [a] -> [(a, Double)]
freq xs = combine (+) $ map (\x -> (x,1.0)) xs

freqy :: [Point] -> M.Map String Double
freqy xys = M.fromList $ freq y
  where y = map snd xys
  
freqxy :: [Point] -> M.Map (Integer, String, String) Double
freqxy xys = M.fromList $ freq $ concat ixy
  where
    ixy  = map enumerate xys
    enumerate (xi, yi) = zip3 [0,1..] xi (repeat yi)

naiveBayes :: [Point] -> [Point] -> [String] -> Dict -> [String]
naiveBayes train test klass dict = map classify test
  where
    classify (t, label) = snd $ last $ sortByKey [(prob t k, k) | k <- klass]

    prob t k            = (loglikelihood t k) + (logprior k)
        
    logprior k        = log $ (fy M.! k) / n
    loglikelihood t k = 
                    let nk = fy M.! k
                        chaves = zip3 [0,1..] t (repeat k)
                        addOneSmooth (di, xi) = (xi+1)/(nk + di)
                     in sum 
                      $ map log
                      $ map addOneSmooth
                      $ zip ds
                      $ map (\t -> M.findWithDefault 0 t fxy) chaves                        

    fxy = freqxy train
    fy  = freqy train    
    
    n = length' train
    ds = map length' dict

acc :: Eq a => [a] -> [a] -> Double
acc pred real = tp/n
  where
    tp = fromIntegral $ length $ filter (id) $ zipWith (==) pred real
    n  = fromIntegral $ length pred

