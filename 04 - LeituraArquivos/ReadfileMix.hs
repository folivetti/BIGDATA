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
data Profissao = Engenheiro | Professor | Gerente | Estudante
                 deriving (Show, Read, Eq, Enum, Bounded)

-- |letter grade
data Conceito = F | D | C | B | A
                deriving (Show, Read, Enum)

type Nota = Double

type Objeto = (Profissao, Conceito, Nota)

type Objeto' = [Double]

-- |'rank' converts the Conceito into a normalized rank value
rank :: Conceito -> Double
rank co = (fromEnum' co) / (fromEnum' A)
  where 
    fromEnum' = fromIntegral . fromEnum

-- |'binariza' parses a Ocupacao into a binary list
binariza :: Profissao -> [Double]
binariza p = map bool2double [p == p' | p' <- profissoes]
  where
    profissoes = [minBound..] :: [Profissao]
    bool2double True = 1.0
    bool2double _ = 0.0

-- |'parseFile' parses a space separated file 
-- to a list of lists of Double
parseFile :: String -> [Objeto]
parseFile file = map parseLine (lines file)
  where
    parseLine l = toObj (words l)
    toObj (w1:w2:w3:[]) = (read w1 :: Profissao,
                          read w2 :: Conceito,
                          read w3 :: Nota)
    toObj _ = error "entrada incorreta"


transformData :: [Objeto] -> [Objeto']
transformData dados = map parseObj dados
  where
    parseObj (prof, conc, nota) = (binariza prof)
                                  ++ [rank conc, nota]
                                 
-- |'main' executa programa principal
main :: IO ()
main = do
    args <- getArgs
    file <- readFile (args !! 0)
    let
      dataset  = parseFile file
      dataset' = transformData dataset
    print dataset
    print dataset'
