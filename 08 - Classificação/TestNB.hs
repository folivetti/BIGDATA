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

import NaiveBayes

-- |'main' executa programa principal
main :: IO ()
main = do
    args <- getArgs
    fileTrain <- readFile (args !! 0)
    fileTest <- readFile (args !! 1)
    let (train, dict, klass) = parseFile fileTrain
    let (test,_,_) = parseFile fileTest
    print dict
    print klass
    
    start <- getTime Monotonic 
    let pred = naiveBayes train test klass dict
    print (acc pred (map snd test))
       
    stop <- getTime Monotonic
    fprint (timeSpecs % "\n") start stop
    return ()

