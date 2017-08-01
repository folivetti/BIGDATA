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
    splitN  l = (1 : (init l), last l)
    doubled l  = map toDouble (words l)
    toDouble  w = read w :: Double



gradientDesc :: [Point] -> Double -> [Double]
gradientDesc dataset alpha = head  $ (dropWhile notConverged)
                             $ iterate  (update dataset alpha) w0
  where
    w0 = take n [1..]
    n  = fromIntegral $ (length . fst $ head dataset)
    notConverged w = mse dataset w > 1e-3


update :: [Point] -> Double -> [Double] -> [Double]
update dataset alpha w = foldl' (zipWith (+)) w nab
  where
    nab = nabla dataset e alpha
    e = err dataset w

nabla :: [Point] -> [Double] -> Double -> [[Double]]
nabla dataset e alpha = zipWith step e dataset
  where
    step ei pi = map (\xi -> xi*ei*alpha/n) (fst pi)
    n = fromIntegral $ length dataset

err :: [Point] -> [Double] -> [Double]
err dataset w = zipWith (-) (map snd dataset) yhat
  where
    yhat = map fhat dataset
    fhat point = sum $ zipWith (*) w (fst point)

mse :: [Point] -> [Double] -> Double
mse dataset w = mean $ map (^2) e
  where
    e = err dataset w

mean :: [Double] -> Double
mean l = (sum l) / len
  where
    len = fromIntegral $ length l

-- |'main' executa programa principal
main :: IO ()
main = do
    args <- getArgs
    file <- readFile (args !! 0)
    let alpha = read (args !! 1) :: Double
    let dataset = parseFile file
    let w = gradientDesc dataset alpha
    print w -- [2, 3.5, -1]
    print (mse dataset w)
