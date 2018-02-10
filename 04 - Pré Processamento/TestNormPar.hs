{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Normalizacao
Description : exemplos de normalização e padronização de dados
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com

-}

module Main where

import Control.Monad
import System.Random
import Formatting
import Formatting.Clock
import System.Clock
import Data.List.Split (chunksOf)

import NormalizaPar
import Vector
import Dados

nchunks = 1000

-- |'main' executa programa principal
main :: IO ()
main = do
    x  <- replicateM 5000000 $ randomRIO (1.0,6.0)
    let m = reshape 100 x
    let chunks = chunksOf nchunks m
    
    start <- getTime Monotonic        
    print (padronizaPar chunks) -- [1.33, -0.26, -1.06]
    print (normalizaPar chunks) -- [0.89, 0.0, -0.44] 
    
    stop <- getTime Monotonic
    fprint (timeSpecs % "\n") start stop
    return ()
