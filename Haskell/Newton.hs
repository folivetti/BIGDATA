{-
Module          : Newton
Description     : Newton method in Haskell
Copyright       : (c) Fabricio Olivetti de Franca, 2017
License         : GPL-3
Maintainer      : fabricio.olivetti@gmail.com

A simple implementation of the Newton's method.
-}

-- | main module
module Main where

import System.Environment

-- | The function 'f' we wish to find the root
f :: Double -> Double
f  x    = x^2 - 4*x + 2

-- | The derivative of 'f'
f' :: Double -> Double
f' x    = 2*x - 4

-- | the approximation
delta :: Double -> Double
delta x = (f x) / (f' x)

-- | the update rule
update :: Double -> Double
update x = x - (delta x)

-- | check if we did converge
notConverged :: Double -> Bool
notConverged x = erro x > 1e-10
  where
    erro = abs . delta

-- | the 'newton' method
newton x0 = raiz $ iterate update x0
  where
    raiz = head . (dropWhile notConverged)

-- | The main function
main :: IO ()
main = do
  args <- getArgs
  let x0 = read (args !! 0) :: Double
  print (newton x0)
