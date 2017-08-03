{-|
Module      : SVD
Description : Singular Value Decomposition
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com

Decomposes a sparse matrix into V . U
-}

module Main where

import System.IO
import System.Environment
import Control.Parallel.Strategies
import Data.List (foldl', nub)
import Data.List.Split (chunksOf)
import qualified Data.HashMap.Strict as M

data Rating = Rating { user :: Int
                     , item :: Int
                     , rating :: Double
                     } deriving (Show, Read)

type UVmtx = M.HashMap Int [Double]

-- |'parseFile' parses a space separated file 
-- to a list of lists of Double
parseFile :: String -> [Rating]
parseFile file = map toTuple (lines file)
    
toTuple l  = Rating u v r
  where
    u = read (w !! 0) :: Int
    v = read (w !! 1) :: Int
    r = read (w !! 2) :: Double
    w = words l

(.+.) :: Num a => [a] -> [a] -> [a]
(.+.) v1 v2 = zipWith (+) v1 v2

(.-.) :: Num a => [a] -> [a] -> [a]
(.-.) v1 v2 = zipWith (-) v1 v2

(.*.) :: Num a => [a] -> [a] -> [a]
(.*.) v1 v2 = zipWith (*) v1 v2

(+.) :: Num a => a -> [a] -> [a]
(+.) c v = map (+c) v

(*.) :: Num a => a -> [a] -> [a]
(*.) c v = map (*c) v

alpha = 0.1
lambda = 0.01

update :: Double -> Double -> [Double] -> [Double] -> Double -> ([Double], [Double])
update alpha lambda u v r = (u',v')
  where
    u'  = update' u v
    v'  = update' v u 
    err = r - (sum $ u .*. v)
    update' v1 v2 = v1 .+. (alpha *. ((err *. v2) .-. (lambda *. v1)))

svd :: Int -> Double -> Double -> Int -> [Rating] -> (UVmtx, UVmtx)
svd f alpha lambda it train = head $ drop it $ iterate adjust (u0, v0)
  where
    u0 = M.fromList (map (\i -> (i, replicate f 1e-1)) m)
    v0 = M.fromList (map (\i -> (i, replicate f 1e-1)) n)
    m  = nub $ map user train
    n  = nub $ map item train
    adjust (u, v) = foldl' adjustOne (u, v) train
    adjustOne (u,v) (Rating us' it' r') = (M.insert us' u' u , M.insert it' v' v)
      where
        (u', v') = update alpha lambda (u M.! us') (v M.! it') r'

rmse :: [Rating] -> UVmtx -> UVmtx -> Double
rmse train u v = sqrt $ mean $ map err train
  where
    err (Rating user' item' r') = sqdiff r' (u M.! user') (v M.! item')
    sqdiff r' u' v' = (r' - (sum $ u' .*. v'))^2
    mean l = sum l / (fromIntegral $ length l)

-- |'main' executa programa principal
main :: IO ()
main = do
    args <- getArgs
    file <- readFile (args !! 0)
    let f = read (args !! 1) :: Int
    let alpha = read (args !! 2) :: Double
    let lambda = read (args !! 3) :: Double
    let it = read (args !! 4) :: Int
    let dataset = parseFile file
    let (u,v) = svd f alpha lambda it dataset
    print (rmse dataset u v)
