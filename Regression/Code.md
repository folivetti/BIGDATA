```haskell
type Point = ([Double], Double)

gradientDesc :: [Point] -> Double -> Double -> [Double]
gradientDesc dataset α η = gradientDesc' w0 nab0
  where
    gradientDesc' w nab
      | notConverged w nab = let (w', nab') = update' w nab
                             in  gradientDesc' w' nab'
      | otherwise          = w
    
    -- | initial variables
    w0      = take n $ [1..]
    nab0    = take n $ [0..]
    n       = fromIntegral $ (length . fst $ head dataset)
    -- support functions
    update' = update dataset α η
    mse'    = mse dataset

    notConverged w nab = (mse' w > 1e-6) && (sum' (nab .^ 2) > 1e-12)


update :: [Point] -> Double -> Double -> [Double] -> [Double] -> ([Double], [Double])
update dataset α η w nab = (w''.+. nab'', nab'')
  where
    nab'' = sumVecs nab'
    nab'  = calcNabla dataset e α
    e     = err dataset w
    w'    = w  .+. (η*.nab)   -- momentum
    w''   = w' .+. (η*.nab'') -- regularization

calcNabla :: [Point] -> [Double] -> Double -> [[Double]]
calcNabla dataset e α = ((α/n) *. e) .*.. x
  where
    x = map fst dataset
    n = fromIntegral $ length dataset

err :: [Point] -> [Double] -> [Double]
err dataset w = (map snd dataset) .-. yhat
  where
    yhat = map fhat dataset
    fhat point = sum $ w .*. (fst point)

mse :: [Point] -> [Double] -> Double
mse dataset w = mean $ e .^ 2
  where
    e = err dataset w

mean :: [Double] -> Double
mean l = (sum l) / len
  where
    len = fromIntegral $ length l

polyfeats :: Int -> [[Double]] -> [[Double]]
polyfeats k x = map (\xi -> poly xi !! k) x
  where
    poly x' = foldr f ([1] : repeat []) x'
    f x''    = scanl1 $ (++) . map (*x'')
```
