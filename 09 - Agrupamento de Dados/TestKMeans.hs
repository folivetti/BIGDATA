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
import System.Random
import Formatting
import Formatting.Clock
import System.Clock

import KMeans

-- |'main' executa programa principal
main :: IO ()
main = do
    args <- getArgs

    fileTrain <- readFile (args !! 0)
    let k = read (args !! 1) :: Int
    let it = read (args !! 2) :: Int
    let train = parseFile fileTrain

    g <- getStdGen
    let idxs = take k (randomRs (0, length train) g)
    let clusters  = firstClusters idxs train
    
    start <- getTime Monotonic
    
    let clusters' = kmeans it train clusters
    print (assign train clusters')
    
    stop <- getTime Monotonic
    fprint (timeSpecs % "\n") start stop
    return ()
