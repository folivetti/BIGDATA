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
tokenize text = filter (not . null) $ map process (lines text)
  where
    process line   = filter moreThanTwo $ map normalize (words line)
    normalize word   = filter (/=' ') $ map lowerReplace word
    lowerReplace c = if isAlphaNum c then toLower c else ' '
    moreThanTwo l = length l > 2

-- |generate 'n-grams' from tokenized text
ngrams :: [[String]] -> Int -> [[String]]
ngrams corpus n = map ngrams' corpus
  where
    ngrams' tokens = map (intercalate " ") $ grams tokens
    grams tokens = sizeN $ map (take n) $ tails tokens 
    sizeN l = filter (\l' -> length l' == n) l

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
  let corpus = tokenize text
  let ng = ngrams corpus 2
  let uniqwords = nub $ concat corpus
  let tf' = tf corpus
  let df' = df $ binarize corpus
  let tfidf' = tfidf tf' df'
  print (uniqwords)
  print (length uniqwords)
  print (binarize corpus)
  print (tf corpus)
  print (tf ng)
  print (tfidf')
