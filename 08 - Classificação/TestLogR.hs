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

import LogisticRegression

-- |'main' executa programa principal
main :: IO ()
main = do
    args <- getArgs
    file1 <- readFile (args !! 0)
    file2 <- readFile (args !! 1)            
    let α = read (args !! 2) :: Double
        it = read (args !! 3) :: Int
        (x,y) = parseFile file1
        (xt, yt) = parseFile file2
    
    start <- getTime Monotonic        
    
    let w = gradDesc x y α it
    
    print w -- [2, 3.5, -1]
    print (accuracy xt yt w)
    
    stop <- getTime Monotonic
    fprint (timeSpecs % "\n") start stop
    return ()

