{-|
Module      : BoW
Description : Bag-of-Words, n-grams, k-skip, tf-idf
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com

Bag-of-Words, n-grams, k-skip-n-gram, tf, tf-idf
-}

module BoW where

import qualified Data.HashMap.Strict as M
import Data.Char (toLower, isAlphaNum)
import Data.List (foldl1', nub, tails, intercalate, sortBy, groupBy)
import Data.Hashable (Hashable)
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
    
idf :: [[(Token, Freq)]] -> M.HashMap Token Freq
idf corpus = M.fromList
           $ mapByKey (\v -> log (n/v))
           $ combine (+)
           $ mapByKey (\v -> 1) 
           $ concat corpus
  where
    n = length' corpus
    
tfidf :: [[(Token, Freq)]] -> M.HashMap Token Freq -> [[(Token, Freq)]]
tfidf tf' idf' = map multIDF tf'
  where
    multIDF = map (\(k,v) -> (k, v * (idf' M.! k)) )
     
-- |generate 'ngrams' from a sequence of tokens
ngrams :: Int -> [Token] -> [Token]
ngrams n tokens = map genGram $ grams tokens
  where
    genGram       = (intercalate " ") . (take n)
    grams tokens  = sizeN $ tails tokens 
    sizeN         = filter (\l' -> length l' >= n)

-- |generate 'ngrams' for each document of a corpus
getNgrams :: Int -> [Doc] -> [[Token]]
getNgrams n corpus = map (ngrams n) $ map words corpus


-- |generate 'skipgrams' of 'k' and 'n' for a sequence of tokens
skipgrams n k tokens = map (intercalate " ") $ grams tokens
  where
    grams tokens  = sizeN $ concat $ map (takeSkips n) $ tails tokens
    takeSkips 0 ws = [[]]
    takeSkips n [] = [[]]
    takeSkips n (w:[]) = [[w]]
    takeSkips n (w:ws) = concat [zipWith (++) (repeat [w]) (takeSkips (n - 1) (drop k' ws)) | k' <- [0..k]]
    sizeN         = filter (\l' -> length l'  == n)

-- |generate 'n-grams' from tokenized text
getSkipgrams :: Int -> Int -> [Doc] -> [[Token]]
getSkipgrams n k corpus = map (skipgrams n k) $ map words corpus
