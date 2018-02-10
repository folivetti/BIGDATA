{-|
Module      : BoW
Description : Bag-of-Words, n-grams, k-skip, tf-idf
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com

Bag-of-Words, n-grams, k-skip-n-gram, tf, tf-idf
-}

module BoWPar where

import qualified Data.HashMap.Strict as M
import Data.Char (toLower, isAlphaNum)
import Data.List (foldl1', nub, tails, intercalate, sortBy, groupBy)
import Data.List.Split (chunksOf)
import Data.Hashable (Hashable)
import Control.Parallel
import Control.Parallel.Strategies

import Dados
import Vector

type TF = M.HashMap String Double

type Doc = String
type Token = String
type Freq = Double

bagofwords :: [Doc] -> [[Token]]
bagofwords docs = naoVazio $ map tokeniza docs
  where
    tokeniza doc      = nub 
                        $ filter maisDe2 
                        $ map normaliza (words doc)
    normaliza palavra = map toLower 
                        $ filter isAlphaNum 
                        palavra
    naoVazio xs       = filter (not . null) xs
    maisDe2 palavra   = (length palavra) > 2

tfPar :: ChunksOf [[Token]] -> ChunksOf [[(Token, Freq)]]
tfPar chunks = parmap tf chunks

tf :: [[Token]] -> [[(Token, Freq)]]
tf docs = map normFreq docs
  where
    normFreq doc = fn $ map (\w -> (w, 1.0)) doc

fn :: [(Token, Double)] -> [(Token, Freq)]
fn tokens = mapByKey (/n) 
          $ combine (+)
          tokens
  where
    n = length' tokens
    
    
idfPar :: ChunksOf [[(Token, Freq)]] -> M.HashMap Token Freq
idfPar chunks = M.fromList
              $ mapByKey (\v -> log (n/v))
              $ mapReduceByKey (\(k,v) -> (k,1)) (+) 
              $ parmap concat chunks
              where
                n = sum $ map length' chunks
