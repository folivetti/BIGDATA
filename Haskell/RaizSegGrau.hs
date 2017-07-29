{-|
Module      : RaizSegGrau
Description : calculates the roots of a quadratic equation in Haskell
Copyright   : (c) Fabrício Olivetti, 2017
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com

The application of Baskhara
-}

module Main where

-- |'raizSegGrau' calculates the 
-- real roots of a quadratic equation
raizSegGrau :: Double -> Double -> Double -> (Double, Double)
raizSegGrau a b c
  | delta < 0  = error "Raízes negativas!"
  | delta == 0 = (x1, x1) -- don't need to calc x2
  | otherwise  = (x1, x2)
  where
    x1 = (-b - sqrt delta) / (2*a)
    x2 = (-b + sqrt delta) / (2*a)
    delta = b**2 - 4*a*c

-- |'main' executa programa principal
main :: IO ()
main = do
    print (raizSegGrau 2 4 1)
    print (raizSegGrau 4 (-2) 1)
