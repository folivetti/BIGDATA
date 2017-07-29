{-|
Module      : ParMean2
Description : calculates the mean in parallel
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com

Calculates the mean of every line of a list in parallel.
-}

module Main where

import System.IO
import System.Environment
import Control.Parallel.Strategies
import Data.List (concat)
import Data.List.Split (chunksOf)

-- |'parseFile' parses a space separated file 
-- to a list of lists of Double
parseFile :: String -> [[Double]]
parseFile file = map parseLine (lines file)
  where
    parseLine l = map toDouble (words l)
    toDouble  w = read w :: Double

mean :: [[Double]] -> [Double]
mean l = map mean' l
  where
    mean' l' = (sum l') / (fromIntegral $ length l')

meanPar :: [[Double]] -> [Double]
meanPar l = concat lists
  where
    lists = map mean chunks `using` parList rdeepseq
    chunks = chunksOf 1000 l

-- |'main' executa programa principal
main :: IO ()
main = do
    args <- getArgs
    file <- readFile (args !! 0)
    let dataset = parseFile file
    print (meanPar dataset)
