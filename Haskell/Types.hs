{-|
Module      : Types
Description : Example of types operations in Haskell
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com

A sample of operations with different types.
-}

module Main where

-- |'soma' sums two integer values
soma :: Integer -> Integer -> Integer
soma x y = x + y

-- |'aurea' defines the golden number
aurea = (1 + sqrt 5) / 2.0

-- |'bissexto' returns whether 'ano' is a leap year (ugly version)
bissexto ano = (ano `rem` 400 == 0) || ((ano `rem` 4 == 0) && (ano `rem` 100 /= 0))

-- |'main' executa programa principal
main :: IO ()
main = do
    print (soma 1 3)
    print aurea
    print (bissexto 2018)
