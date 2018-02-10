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

import Normaliza
import Vector

-- |'main' executa programa principal
main :: IO ()
main = do
    x  <- replicateM 5000000 $ randomRIO (1.0,6.0)
    let m = reshape 100 x
    
    start <- getTime Monotonic    
    print (padroniza m) -- [1.33, -0.26, -1.06]
    print (maxminScale m) -- [1.0, 0.33, 0.0]
    print (normaliza m) -- [0.89, 0.0, -0.44] 
    
    stop <- getTime Monotonic
    fprint (timeSpecs % "\n") start stop
    return ()
