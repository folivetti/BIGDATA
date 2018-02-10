{-|
Module      : Composition
Description : example of function composition
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com

Calculates the final grade with composition.
-}

module Main where

-- |'mediaFinal' calculates the weighted
-- average of obtained points
mediaFinal :: Double -> Double -> Double
mediaFinal p1 p2 = 0.4*p1 + 0.6*p2

-- |'conceito' converts points to grade
conceito :: Double -> Char
conceito media
  | media < 5 = 'F'
  | media < 6 = 'D'
  | media < 7 = 'C'
  | media < 8 = 'B'
  | otherwise = 'A'

-- |'geraConceito' composes both functions
geraConceito :: Double -> Double -> Char
geraConceito p1 p2 = conceito $ mediaFinal p1 p2

-- |'nota' generates the final point from
-- one single test
nota :: Double -> Double
nota x = x*2

-- |'calcConceito' is the composition of 'conceito' and 'nota'
calcConceito = conceito . nota

-- |'main' executa programa principal
main :: IO ()
main = do
    print (geraConceito 10 3)
    print (calcConceito 3)
