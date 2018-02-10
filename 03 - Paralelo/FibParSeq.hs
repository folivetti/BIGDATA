{-|
Module      : FibPar
Description : fibonacci in parallel (only par)
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com

Calculates the fibonacci sequence in parallel (bogus).
-}

module Main where

import Control.Parallel

-- |'fib' calculates the fibonacci number n
fib :: Integer -> Integer 
fib 0 = 0
fib 1 = 1
fib n = n1 `par` n2 `pseq` (n1 + n2)
  where
    n1 = fib (n - 1)
    n2 = fib (n - 2)

-- |'main' executa programa principal
main :: IO ()
main = do
    print (fib 36)
