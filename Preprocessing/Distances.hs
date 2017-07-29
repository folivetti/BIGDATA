{-|
Module      : Distances
Description : example of distances metrics
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com

Euclidean, Minkowski, Jaccard, Normalization, Standardization
-}

module Main where

-- |'minkowski' distance
minkowski :: Double -> [Double] -> [Double] -> Double
minkowski p x y  = summation ** (1.0/p)
  where
    summation = sum $ zipWith (pointwiseDist) x y
    pointwiseDist xi yi = (abs (xi - yi)) ** p

-- |'euclidean' is just a special case of minkowski
euclidean :: [Double] -> [Double] -> Double
euclidean = minkowski 2

-- |'jaccard' distance
jaccard :: [Double] -> [Double] -> Double
jaccard x y = 1 - (sumProd / sumSum)
  where
    sumProd = sum $ zipWith (*) x y
    sumSum  = sum $ zipWith (binsum) x y

-- |'binsum' is a binary addition for Jaccard
binsum :: Double -> Double -> Double
binsum 1 _ = 1
binsum _ 1 = 1
binsum _ _ = 0

-- |'cosine' distance of two vectors
cosine :: [Double] -> [Double] -> Double
cosine x y = (dotprod x y) / (norm' x * norm' y)
  where
    dotprod u v = sum $ zipWith (*) u v
    norm' u  = dotprod u u

-- |'standardize' a vector
standardize :: [Double] -> [Double]
standardize x = map toCenter x
  where
    toCenter xi = (xi - mean x) / stdX
    mean x = (sum x) / (length' x)
    stdX  = sqrt varX
    varX  = mean $ map (\xi -> (xi - mean x) ** 2) x
    length' l = fromIntegral $ length l

-- |'maxminScale' scales vector to [0,1]
maxminScale :: [Double] -> [Double]
maxminScale x = map scale x
  where
    scale xi = (xi - minimum x) / (maximum x - minimum x)

-- |'norm' calculates the norm vector
norm :: [Double] -> [Double]
norm x = map (/nx) x
  where
    nx = sqrt . sum $ map (^2) x

-- |'main' executa programa principal
main :: IO ()
main = do
    print (euclidean [1.0, 2.0, 1.0] [3.0, 2.0, 4.0]) -- 3.6
    print (minkowski 1 [1.0, 2.0, 1.0] [3.0, 2.0, 4.0]) -- 5.0
    print (cosine  [1.0, 2.0, 1.0] [3.0, 2.0, 4.0]) -- 0.06
    print (jaccard [1,0,0,1] [0,1,0,1]) -- 0.66
    print (standardize [2,0,-1]) -- [1.33, -0.26, -1.06]
    print (maxminScale [2,0,-1]) -- [1.0, 0.33, 0.0]
    print (norm [2,0,-1]) -- [0.89, 0.0, -0.44] 
