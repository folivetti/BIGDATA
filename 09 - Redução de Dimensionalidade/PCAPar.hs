{-|
Module      : PCA
Description : Gradient Descent
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com

Principal Component Analysis.
-}

module Main where

import System.IO
import System.Environment
import Control.Parallel.Strategies
import Data.List
import Data.Ord
import Data.List.Split (chunksOf)
import qualified Numeric.LinearAlgebra as H 

import Vector
import Dados

-- |'parseFile' parses a space separated file 
-- to a list of lists of Double
parseFile :: String -> [[Double]]
parseFile file = map parseLine (lines file)
  where
    parseLine l = map toDouble (words l)
    toDouble  w = read w :: Double

toString :: [[Double]] -> String
toString x = intercalate "\n" $ map (\xi -> intercalate " " $ map show xi) x

center :: ChunksOf [[Double]] -> ChunksOf [[Double]]
center x = parmap (map (.-. m)) x
  where
    m = mapReduce (./n) (.+.) x
    n = sum $ parmap length' x

covar :: (NFData a, Num a) => ChunksOf [[a]] -> [[a]]
covar x = mapReduce (\xi -> outerprod xi xi) (..+..) x

multMtx :: Num a => [[a]] -> [[a]] -> [[a]]
multMtx m1 m2 = [ [dotprod ri rj | rj <- m2'] | ri <- m1 ]
  where
    m2' = transpose m2
    dotprod l1 l2 = sum $ l1 .*. l2

pca :: Int -> ChunksOf [[Double]] -> [[Double]]
pca k x = H.toLists $ eve H.?? (H.All, H.Pos idx)
  where
    idx        =  H.subVector 0 k $ H.sortIndex (-eva) 
    (eva, eve) = (fst $ H.fromComplex eva', fst $ H.fromComplex eve')
    (eva', eve') = H.eig $ H.fromLists $ covar $ center x

nchunks = 1000

-- |'main' executa programa principal
main :: IO ()
main = do
    args <- getArgs
    fileTrain <- readFile (args !! 0)
    let k = read (args !! 2) :: Int
        train = parseFile fileTrain
        chunks = chunksOf nchunks train
        train' = (multMtx train $ pca k chunks)
    writeFile (args !! 1) (toString train')
