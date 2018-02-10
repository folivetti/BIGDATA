{-# LANGUAGE UnicodeSyntax #-}

{-|
Module      : LinearRegression
Description : Gradient Descent
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com

Applies gradient descent to a linear regression problem.
-}

module LinearRegression where

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
    y'        = map (dotprod w) x
    convergiu = (erro' < 1e-6) || (it==0)
    erro'     = erro x y w
    
erro x y w = mean $ (y .-. y') .^ 2
  where
    y' = map (dotprod w) x    

mean :: [Double] -> Double
mean l = (sum l) / (length' l)

polyfeats :: Int -> [[Double]] -> [[Double]]
polyfeats k x = map (\xi -> poly xi !! k) x
  where
    poly x' = foldr f ([1] : repeat []) x'
    f x''   = scanl1 $ (++) . map (*x'')


