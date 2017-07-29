{-|
Module      : Fibonacci
Description : example of lists generators
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com

Calculates the fibonacci sequence.
-}

module Main where

-- |'fib' is a generator of the fibonacci sequence
fib = 1 : 2 : prox fib
  where
    prox (x : t@(y:_)) = (x+y) : prox t

-- |'main' executa programa principal
main :: IO ()
main = do
    print (take 10 fib)
