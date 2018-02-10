{-|
Module      : ParMean
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

-- |'parseFile' parses a space separated file 
-- to a list of lists of Double
parseFile :: String -> [[Double]]
parseFile file = map parseLine (lines file)
  where
    parseLine l = map toDouble (words l)
    toDouble  w = read w :: Double

mean :: [[Double]] -> [Double]
mean l = map mean' l `using` parList rseq
  where
    mean' l' = (sum l') / (fromIntegral $ length l')

-- |'main' executa programa principal
main :: IO ()
main = do
    args <- getArgs
    file <- readFile (args !! 0)
    let dataset = parseFile file
    print (mean dataset)
