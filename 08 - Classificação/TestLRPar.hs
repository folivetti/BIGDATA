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
import Data.List.Split (chunksOf)

import LogisticRegressionPar

nchunks = 1000

-- |'main' executa programa principal
main :: IO ()
main = do
    args <- getArgs
    file1 <- readFile (args !! 0)
    file2 <- readFile (args !! 1)                
    let α = read (args !! 2) :: Double
        it = read (args !! 3) :: Int
        xy = parseFile file1
        chunks = chunksOf nchunks xy
        xyt = parseFile file2
        chunksT = chunksOf nchunks xyt
    
    
    start <- getTime Monotonic        
    
    let w = gradDesc chunks α it
    
    print w -- [2, 3.5, -1]
    print (accuracy chunksT w)
    
    stop <- getTime Monotonic
    fprint (timeSpecs % "\n") start stop
    return ()

