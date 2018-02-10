{-|
Module      : Fatorial
Description : example of recursive functions
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com

Calculates the fatorial recursively.
-}

module Main where

-- |'fatorial' calculates the fatorial
fatorial :: Integer -> Integer
fatorial 0 = 1
fatorial 1 = 1
fatorial n = n * fatorial (n-1)

-- |'fatorialT' uses tail recursion
fatorialT :: Integer -> Integer
fatorialT 0 = 1
fatorialT 1 = 1
fatorialT n = fatorial' n 1
  where
    fatorial' 1 r = r
    fatorial' n r = fatorial' (n-1) (n*r)

-- |'main' executa programa principal
main :: IO ()
main = do
    print (fatorial 10)
    print (fatorialT 10)
