{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : LinearRegression
Description : Gradient Descent
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com

Applies gradient descent to a linear regression problem.
-}

module Main where

import System.IO
import System.Environment
import Formatting
import Formatting.Clock
import System.Clock

import LinearRegression

-- |'main' executa programa principal
main :: IO ()
main = do
    args <- getArgs
    file <- readFile (args !! 0)    
    let α = read (args !! 1) :: Double
    let it = read (args !! 2) :: Int
    let (x,y) = parseFile file
    
    start <- getTime Monotonic        
    
    let w = gradDesc x y α it
    
    print w -- [2, 3.5, -1]
    print (erro x y w)
    
    stop <- getTime Monotonic
    fprint (timeSpecs % "\n") start stop
    return ()

