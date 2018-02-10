{-|
Module      : Hello
Description : Hello World in Haskell
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com

This is just a hello world.
-}

module Main where

-- |'main' executa programa principal
main :: IO ()
main = do
    print ("Hello World")
