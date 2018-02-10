{-|
Module      : Normalizacao
Description : exemplos de normalização e padronização de dados
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com

-}

module Normaliza where

import Vector
import Dados

-- |'padroniza' as colunas de uma matriz de dados
padroniza :: Matrix Double -> Matrix Double
padroniza x = mapColunas padroniza' x

padroniza' :: Vector Double -> Vector Double
padroniza' x = (x .- media) ./ sigma
  where
    media = (sum x) / n
    sigma = sqrt . (/n) $ sum $ (x .- media) .** 2
    n     = length' x    

-- |'maxminScale' scales vector to [0,1]
maxminScale :: [[Double]] -> [[Double]]
maxminScale x = mapColunas maxminScale' x

maxminScale' :: [Double] -> [Double]
maxminScale' x = map scale x
  where
    scale xi = (xi - minimum x) / (maximum x - minimum x)
    
-- |'norm' calculates the norm vector
normaliza :: [[Double]] -> [[Double]]
normaliza x = map normaliza' x
  where
    normaliza' xi = xi ./ (norma xi)
    norma xi      = sqrt . sum $ xi .^ 2
               

