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



gradientDesc :: [Point] -> Double -> Double -> [Double]
gradientDesc dataset alpha eta = fst $ head $ (dropWhile notConverged)
                             $ iterate  (update dataset alpha eta) (w0, nab0)
  where
    w0   = take n [1..]
    nab0 = take n [0..]
    n    = fromIntegral $ (length . fst $ head dataset)
    notConverged (w, nab) = (mse dataset w > 1e-6) && (foldl' (+) 0 (map (^2) nab) > 1e-12)


update :: [Point] -> Double -> Double -> ([Double], [Double]) -> ([Double], [Double])
update dataset alpha eta (w, nab0) = (zipWith (+) w'' nab', nab')
  where
    nab' = foldl' (zipWith (+)) (head nab) (tail nab)
    nab = nabla dataset e alpha
    e = err dataset w
    w' = zipWith (+) w (map (*eta) nab0)  -- momentum
    w'' = zipWith (+) w (map (*eta) nab') -- regularization

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
    let alpha = read (args !! 1) :: Double
    let eta = read (args !! 2) :: Double
    let dataset = parseFile file
    let w = gradientDesc dataset alpha eta
    print w -- [2, 3.5, -1]
    print (mse dataset w)
    print (polyfeats 2 [[1,2,3]])
    print (polyfeats 3 [[1,2,3]])
