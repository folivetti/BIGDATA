{-# LANGUAGE UnicodeSyntax #-}

{-|
Module      : LinearRegression
Description : Gradient Descent
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com

Applies gradient descent to a linear regression problem.
-}

module LogisticRegression where

import Control.Parallel.Strategies
import Data.List (foldl')
import Data.List.Split (chunksOf)

import Vector

-- |'parseFile' parses a space separated file 
-- to a list of lists of Double
parseFile :: String -> ([[Double]], [Double])
parseFile file = ((map fst dataset), (map snd dataset))
  where
    dataset = map parseLine (lines file)
    parseLine l = splitN (doubled l) 
    splitN  l = (1 : (init l), last l) -- add bias +1
    doubled l  = map toDouble (words l)
    toDouble  w = read w :: Double


gradDesc :: [[Double]] -> [Double] -> Double -> Int -> [Double]
gradDesc x y α it = gradDesc' x y w0 α it
  where
    w0 = take (length' $ head x) [0.01,0.01..]

gradDesc' :: [[Double]] -> [Double] -> [Double] -> Double -> Int -> [Double]
gradDesc' x y w α it
  | convergiu = w
  | otherwise = gradDesc' x y w' α (it-1)
  where
    w'        = w .+. (α *. nabla)
    nabla     = mediaVetor $ map (\(yi, xi) -> yi *. xi) $ zip (y .-. y') x
    y'        = map (\xi -> sigmoid $ dotprod w xi) x
    convergiu = (erro' < 1e-6) || (it==0)
    erro'     = erro x y w

sigmoid z = 1.0 / (1.0 + exp (-z))
    
erro x y w = mean $ (y .-. y') .^ 2
  where
    y' = map (dotprod w) x
    
accuracy :: [[Double]] -> [Double] -> [Double] -> Double
accuracy x y w = (length' correct) / (length' x)
  where
    correct  = filter (uncurry (==)) $ zip y yhat
    yhat     = map fhat x
    fhat xi  = round' $ sigmoid $ sum $ w .*. xi
    
    round' x = fromIntegral $ round $ x

mean :: [Double] -> Double
mean l = (sum l) / (length' l)
