{-|
Module      : Distances
Description : medidas de distâncias
Copyright   : (c) Fabrício Olivetti, 2018
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
-}

module Distances where

import Data.List
import Vector

minkowski :: Double -> Vector Double -> Vector Double -> Double
minkowski p x y  = somatoria ** (1.0/p)
  where
    somatoria = sum normP
    normP     = absVal .** p
    absVal    = map abs diffVecs 
    diffVecs  = x .-. y
    
manhattan  = minkowski 1
euclidiana = minkowski 2
chebyshev x y = maximum $ map abs $ x .-. y    

cosine :: Vector Double -> Vector Double -> Double
cosine x y = (dotprod x y) / (norm x * norm y)

jaccard :: Vector Integer -> Vector Integer -> Double
jaccard x y = 1.0 - (prod / soma)
  where
    prod  = fromIntegral
          $ sum
          $ x .*. y
    soma  = fromIntegral
          $ sum
          $ map (min 1) (x .+. y)
