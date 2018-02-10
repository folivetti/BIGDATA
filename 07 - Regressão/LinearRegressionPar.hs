{-# LANGUAGE UnicodeSyntax #-}

{-|
Module      : LinearRegression
Description : Gradient Descent
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com

Applies gradient descent to a linear regression problem.
-}

module LinearRegressionPar where

import System.IO
import System.Environment
import Control.Parallel.Strategies
import Data.List (foldl1')
import Data.List.Split (chunksOf)

import Vector
import Dados

-- |'parseFile' parses a space separated file 
-- to a list of lists of Double
parseFile :: String -> [(Vector Double, Double)]
parseFile file = map parseLine (lines file)
  where
    parseLine l = splitN (doubled l) 
    splitN  l = (1 : (init l), last l) -- add bias +1
    doubled l  = map toDouble (words l)
    toDouble  w = read w :: Double


gradDesc :: ChunksOf [(Vector Double, Double)] 
         -> Double -> Int
         -> Vector Double
gradDesc xy α it = gradDesc' xy w0 α it
  where
    w0 = take n [0.01,0.01..]
    n  = length' $ fst $ head $ head xy

gradDesc' :: ChunksOf [(Vector Double, Double)] 
          -> Vector Double -> Double -> Int 
          -> Vector Double
gradDesc' xy w α it
  | convergiu = w
  | otherwise = gradDesc' xy w' α (it-1)
  where
    w'        = w .+. (α *. (nabla ./ n))
    nabla     = mapReduce partial (.+.) xy
    partial (xi, yi) = ( yi - (dotprod w xi) ) *. xi
    convergiu = (it==0)
    n         = sum $ map length' xy
    
erro ::  ChunksOf [(Vector Double, Double)] -> Vector Double -> Double
erro xy w = soma / n
  where
    soma = sum
         $ (map (\xyi -> sum $ map erro' xyi) xy 
             `using` parList rdeepseq)
    erro' (xi,yi) = (yi - (dotprod w xi))^2
    n = sum $ map length' xy



