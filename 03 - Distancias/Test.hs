{-|
Module      : Distances
Description : medidas de distâncias
Copyright   : (c) Fabrício Olivetti, 2018
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
-}

module Main where

import Data.List
import Vector
import Distances

x = [1.0, 3.0, 5.0]
y = [2.0, 2.0, 1.0]

bin1 = [1,0,0,1]
bin2 = [1,1,0,0]

main :: IO ()    
main = do
  print (euclidiana x y)
  print (cosine x y)
  print (jaccard bin1 bin2)
