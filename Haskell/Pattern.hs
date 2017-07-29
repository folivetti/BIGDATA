{-|
Module      : Pattern
Description : example of pattern matching
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com

Creates a mult operator with pattern matching.
-}

module Main where

mult :: (Eq a, Num a) => a -> a -> a
mult 1 y = y
mult x 1 = x
mult 0 _ = 0
mult _ 0 = 0
mult x y = x * y

-- |'main' executa programa principal
main :: IO ()
main = do
    print (mult 2 4)
    print (mult 2 1)
    print (mult 1 4)
    print (mult 2 0)
    print (mult 0 4)
