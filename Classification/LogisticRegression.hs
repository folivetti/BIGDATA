{-# LANGUAGE UnicodeSyntax #-}

{-|
Module      : LogisticRegression
Description : Gradient Descent
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com

Applies gradient descent to a logistic regression problem.
-}

module Main where

import System.IO
import System.Environment
import Control.Parallel.Strategies
import Data.List (foldl')
import Data.List.Split (chunksOf)

type Point = ([Double], Double)

-- |'parseFile' parses a space separated file 
-- to a list of lists of Double
parseFile :: String -> [Point]
parseFile file = map parseLine (lines file)
  where
    parseLine l = splitN (doubled l) 
    splitN  l = (1 : (init l), last l) -- add bias +1
    doubled l  = map toDouble (words l)
    toDouble  w = read w :: Double

-- | support operators
sum' = foldl' (+) 0
sumVecs xs = foldl' ((.+.)) (head xs) (tail xs)
(.^) xs y = map (^y) xs
(.+.) xs ys = zipWith (+) xs ys
(.-.) xs ys = zipWith (-) xs ys
(.*.) xs ys = zipWith (*) xs ys
(*.) x ys = map (*x) ys
(.*..) xs ys = zipWith (*.) xs ys

logistic z = 1.0 / (1.0 + exp (-z))

length' l = fromIntegral $ length l

gradientDesc :: [Point] -> Double -> Double -> [Double]
gradientDesc dataset α η = gradientDesc' w0 nab0
  where
    gradientDesc' w nab
      | notConverged w nab = let (w', nab') = update' w nab
                             in  gradientDesc' w' nab'
      | otherwise          = w
    
    -- | initial variables
    w0      = take n $ [1..]
    nab0    = take n $ [0..]
    n       = length' (fst $ head dataset)
    -- support functions
    update' = update dataset α η
    mse'    = mse dataset

    notConverged w nab = (mse' w > 1e-6) && (sum' (nab .^ 2) > 1e-12)


update :: [Point] -> Double -> Double -> [Double] -> [Double] -> ([Double], [Double])
update dataset α η w nab = (w''.+. nab'', nab'')
  where
    nab'' = sumVecs nab'
    nab'  = calcNabla dataset e α
    e     = err dataset w
    w'    = w  .+. (η*.nab)   -- momentum
    w''   = w' .+. (η*.nab'') -- regularization

calcNabla :: [Point] -> [Double] -> Double -> [[Double]]
calcNabla dataset e α = ((α/n) *. e) .*.. x
  where
    x = map fst dataset
    n = length' dataset

err :: [Point] -> [Double] -> [Double]
err dataset w = y .-. yhat
  where
    y       = map snd dataset
    x       = map fst dataset
    yhat    = map fhat x
    fhat xi = logistic $ sum $ w .*. xi

mse :: [Point] -> [Double] -> Double
mse dataset w = mean $ e .^ 2
  where
    e = err dataset w

accuracy :: [Point] -> [Double] -> Double
accuracy dataset w = (length' correct) / (length' dataset)
  where
    correct  = filter (uncurry (==)) $ zip y yhat
    yhat     = map fhat x
    fhat xi  = round' $ logistic $ sum $ w .*. xi
    
    round' x = fromIntegral $ round $ x

    y = map snd dataset
    x = map fst dataset

mean :: [Double] -> Double
mean l = (sum l) / (length' l)

-- |'main' executa programa principal
main :: IO ()
main = do
    args <- getArgs
    file1 <- readFile (args !! 0)
    file2 <- readFile (args !! 1)
    let 
      α = read (args !! 2) :: Double
      η = read (args !! 3) :: Double
      train = parseFile file1
      test  = parseFile file2
      w = gradientDesc train α η
    print w -- [2, 3.5, -1]
    print (accuracy test w)
