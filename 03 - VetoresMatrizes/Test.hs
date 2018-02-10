{-|
Module      : Test
Description : operações com vetores
Copyright   : (c) Fabrício Olivetti, 2018
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com

Exemplifies the use of Maybe type
-}

module Main where

import Data.List (transpose)
import Vector

u = [1,3..10]
v = [2,4..12]
m = [u,v]

main :: IO()
main = do
  print u
  print v
  print m
  print (u ./. v)
  print (u .* 2)
  print (2 *. u)
  print (m ..-.. m)
  print (m ..+. v)
  print (m ..* 2)
  print (dotprod u v)
  print (outerprod u u)
  print (mapColunas (sum) m)
