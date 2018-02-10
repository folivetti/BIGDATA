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
import Data.List.Split (chunksOf)

import KMeansPar

nchunks = 1000

-- |'main' executa programa principal
main :: IO ()
main = do
    args <- getArgs
    g <- getStdGen

    fileTrain <- readFile (args !! 0)
    let k = read (args !! 1) :: Int
        it = read (args !! 2) :: Int
        train = parseFile fileTrain
        chunks    = chunksOf nchunks train
    
        idxs = take k (randomRs (0, length train) g)
        clusters  = firstClusters idxs train
        
    
    start <- getTime Monotonic
    
    let clusters' = kmeans it chunks clusters
    print (map (\p -> assign p clusters') train)
    
    stop <- getTime Monotonic
    fprint (timeSpecs % "\n") start stop
    return ()
