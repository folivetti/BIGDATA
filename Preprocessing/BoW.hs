{-|
Module      : BoW
Description : Bag-of-Words, n-grams, k-skip, tf-idf
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com

Bag-of-Words, n-grams, k-skip-n-gram, tf, tf-idf
-}

module Main where

import qualified Data.HashMap.Strict as M
import Data.Char (toLower, isAlphaNum)
import Data.List (foldl', nub, tails, intercalate)
import Data.Hashable (Hashable)

type TF = M.HashMap String Double

-- |'tokenize' text to BoW
tokenize :: String -> [[String]]
tokenize text = notEmpty $ map toTokens $ lines text
  where
    notEmpty       = filter (not . null)
    toTokens line   = preProcess $ words line
    preProcess     = (filter moreThanTwo) . (map normalize)
    normalize word = map toLower $ filter isAlphaNum word
    moreThanTwo l  = length l > 2

-- |generate 'ngrams' from a sequence of tokens
ngrams :: Int -> [String] -> [String]
ngrams n tokens = map genGram $ grams tokens
  where
    genGram       = (intercalate " ") . (take n)
    grams tokens  = sizeN $ tails tokens 
    sizeN         = filter (\l' -> length l' >= n)

-- |generate 'ngrams' for each document of a corpus
getNgrams :: Int -> [[String]] -> [[String]]
getNgrams n corpus = map (ngrams n) corpus


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
getSkipgrams :: Int -> Int -> [[String]] -> [[String]]
getSkipgrams n k corpus = map (skipgrams n k) corpus

-- |'binarize' the corpus of BoW
binarize :: [[String]] -> [TF]
binarize corpus = map binVec corpus
  where
    binVec line = M.fromList $ zip line [1,1..]

-- |'tf' generates the term frequency of a BoW
tf :: [[String]] -> [TF]
tf corpus = map countWords corpus
  where
    countWords doc = M.map (\v -> v / (len doc)) $ M.fromListWith (+) $ zip doc [1,1..]
    len d = fromIntegral $ length d

-- | 'df' calculates document frequency of words in a dictionary
df :: [TF] -> TF
df corpus = foldl' (M.unionWith (+)) M.empty corpus

-- | 'tfidf' of a document
tfidf :: [TF] -> TF -> [TF]
tfidf tf' df' = map calcTFIDF tf'
  where
    calcTFIDF t = M.mapWithKey calc t
    calc k v = v * n / (getDF k)
    getDF t = M.lookupDefault 0 t df'
    n = fromIntegral $ length tf'

-- |'main' executa programa principal
main :: IO ()
main = do
  text <- getContents

  let
    corpus = tokenize text
    tf' = tf corpus
    ng = tf $ getNgrams 2 corpus
    sg = tf $getSkipgrams 3 2 corpus
    df' = df $ binarize corpus
    tfidf' = tfidf tf' df'

  print (corpus !! 14)
  print (binarize corpus !! 14)
  print (tf' !! 14)
  print (ng !! 14)
  print (sg !! 14)
  print (tfidf' !! 14)
