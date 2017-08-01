```haskell
type Point = ([Double], Double)

gradientDesc :: [Point] -> Double -> [Double]
gradientDesc dataset alpha = head  $ (dropWhile notConverged)
                             $ iterate  (update dataset alpha) w0
  where
    w0 = take n [1..]
    n  = fromIntegral $ (length . fst $ head dataset)
    notConverged w = mse dataset w > 1e-3


update :: [Point] -> Double -> [Double] -> [Double]
update dataset alpha w = foldl' (zipWith (+)) w nab
  where
    nab = nabla dataset e alpha
    e = err dataset w

nabla :: [Point] -> [Double] -> Double -> [[Double]]
nabla dataset e alpha = zipWith step e dataset
  where
    step ei pi = map (\xi -> xi*ei*alpha/n) (fst pi)
    n = fromIntegral $ length dataset

err :: [Point] -> [Double] -> [Double]
err dataset w = zipWith (-) (map snd dataset) yhat
  where
    yhat = map fhat dataset
    fhat point = sum $ zipWith (*) w (fst point)

mse :: [Point] -> [Double] -> Double
mse dataset w = mean $ map (^2) e
  where
    e = err dataset w
```
