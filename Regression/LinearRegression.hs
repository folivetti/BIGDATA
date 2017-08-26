{-# LANGUAGE UnicodeSyntax #-}

{-|
Module      : LinearRegression
Description : Gradient Descent
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com

Applies gradient descent to a linear regression problem.
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
    n       = fromIntegral $ (length . fst $ head dataset)
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
    n = fromIntegral $ length dataset

err :: [Point] -> [Double] -> [Double]
err dataset w = (map snd dataset) .-. yhat
  where
    yhat = map fhat dataset
    fhat point = sum $ w .*. (fst point)

mse :: [Point] -> [Double] -> Double
mse dataset w = mean $ e .^ 2
  where
    e = err dataset w

mean :: [Double] -> Double
mean l = (sum l) / len
  where
    len = fromIntegral $ length l


polyfeats :: Int -> [[Double]] -> [[Double]]
polyfeats k x = map (\xi -> poly xi !! k) x
  where
    poly x' = foldr f ([1] : repeat []) x'
    f x''    = scanl1 $ (++) . map (*x'')

-- |'main' executa programa principal
main :: IO ()
main = do
    args <- getArgs
    file <- readFile (args !! 0)
    let α = read (args !! 1) :: Double
    let η = read (args !! 2) :: Double
    let dataset = parseFile file
    let w = gradientDesc dataset α η
    print w -- [2, 3.5, -1]
    print (mse dataset w)
    print (polyfeats 2 [[1,2,3]])
    print (polyfeats 3 [[1,2,3]])
