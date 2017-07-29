{-|
Module      : ReadfileMix
Description : example of how to read and parse a file with mixed values
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com

Parse a table with categorical, ordered and numerical features.
-}

module Main where

import System.IO
import System.Environment

-- |professions
data Ocupacao = Engenheiro | Professor | Gerente | Estudante
              deriving (Show, Read, Enum)

-- |letter grade
data Conceito = F | D | C | B | A
              deriving (Show, Read, Enum)

-- |'rank' converts the Conceito into a normalized rank value
rank :: String -> Double
rank co = (fromEnum' co') / (fromEnum' A)
  where 
    co' = read co :: Conceito
    fromEnum' = fromIntegral . fromEnum

-- |'binarize' parses a Ocupacao into a binary list
binarize :: String -> [Double]
binarize oc = [bool2double $ oc' ==  i | i <- [0..3]]
  where
    oc' = fromEnum (read oc :: Ocupacao)
    bool2double True  = 1.0
    bool2double False = 0.0

-- |'parseFile' parses a space separated file 
-- to a list of lists of Double
parseFile :: String -> [[Double]]
parseFile file = map parseLine (lines file)
  where
    parseLine l = concat $ toDouble (words l)
    toDouble [oc, co, grade] = [binarize oc, [rank co, read grade]]

-- |'main' executa programa principal
main :: IO ()
main = do
    args <- getArgs
    file <- readFile (args !! 0)
    let dataset = parseFile file
    print dataset
