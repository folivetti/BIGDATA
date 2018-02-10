{-|
Module      : ADT
Description : example of Maybe
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com

Exemplifies the use of Maybe type
-}

module Main where

-- |'safeDiv' returns Nothing when trying
-- to divide by 0
safeDiv :: Double -> Double -> Maybe Double
safeDiv x y
  | y==0 = Nothing
  | otherwise = Just (x / y)
  
-- |'media' safely calculates the average 
-- value of a list
media :: [Double] -> Double
media l = 
  case safeDiv (sum l) len of
    Nothing -> 1/0 -- infinity
    Just m -> m
  where
    len = fromIntegral $ length l
    

-- |'main' executa programa principal
main :: IO ()
main = do
    print (media [1,2,3])
    print (media [])
