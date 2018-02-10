{-|
Module      : Readfile
Description : example of how to read and parse a file
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com

Parse a file of numbers into a list of lits.
-}

module Main where

import System.IO
import System.Environment
import Text.Read

-- |'parseFile' parses a space separated file 
-- to a list of lists of Double
parseFile :: String -> [[Double]]
parseFile file = map parseLine (lines file)
  where
    parseLine l = map toDouble (words l)
    toDouble  w = do
                    let maybeW = readMaybe w :: Maybe Double
                    case maybeW of
                         Just w' -> w'
                         Nothing -> 0.0

-- |'main' executa programa principal
main :: IO ()
main = do
    args <- getArgs
    file <- readFile (args !! 0)
    let dataset = parseFile file
    print dataset
