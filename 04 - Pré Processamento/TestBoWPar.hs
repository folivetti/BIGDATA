{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : BoW
Description : Bag-of-Words, n-grams, k-skip, tf-idf
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com

Bag-of-Words, n-grams, k-skip-n-gram, tf, tf-idf
-}

module Main where

import BoWPar
import Formatting
import Formatting.Clock
import System.Clock
import Data.List.Split (chunksOf)

nchunks = 1000

-- |'main' executa programa principal
main :: IO ()
main = do
  text <- getContents
  let 
    tf'  = tfPar (chunksOf nchunks $ bagofwords (lines text))
    idf' = idfPar tf'
    
  start <- getTime Monotonic  
  
  print tf'
  print idf'
  
  stop <- getTime Monotonic
  fprint (timeSpecs % "\n") start stop
  return ()
