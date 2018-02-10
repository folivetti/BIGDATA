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

import BoW
import Formatting
import Formatting.Clock
import System.Clock

-- |'main' executa programa principal
main :: IO ()
main = do
  text <- getContents
  let 
    tf'  = tf (bagofwords (lines text))
    idf' = idf tf'
    ng'  = tf (getNgrams 2 (lines text))
    sk'  = tf (getSkipgrams 2 3 (lines text))
    
  start <- getTime Monotonic  
  
  print tf'
  print ng'
  print sk'
  print (tfidf tf' idf')
  
  stop <- getTime Monotonic
  fprint (timeSpecs % "\n") start stop
  return ()
