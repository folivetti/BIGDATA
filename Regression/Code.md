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


gradientDesc :: [Point] -> Double -> Double -> [Double]
gradientDesc dataset alpha eta = fst $ head $ (dropWhile notConverged)
                             $ iterate  (update dataset alpha eta) (w0, nab0)
  where
    w0   = take n [1..]
    nab0 = take n [0..]
    n    = fromIntegral $ (length . fst $ head dataset)
    notConverged (w, nab) = (mse dataset w > 1e-6) && (foldl' (+) 0 (map (^2) nab) > 1e-12)


update :: [Point] -> Double -> Double -> ([Double], [Double]) -> ([Double], [Double])
update dataset alpha eta (w, nab0) = (zipWith (+) w'' nab', nab')
  where
    nab' = foldl' (zipWith (+)) (head nab) (tail nab)
    nab = nabla dataset e alpha
    e = err dataset w
    w' = zipWith (+) w (map (*eta) nab0)  -- momentum
    w'' = zipWith (+) w (map (*eta) nab') -- regularization

nway :: Int -> [Double] -> [Double]
nway n x 
  | n <= 1 = x
  | n == 2 = interactions x
  | otherwise = concat $ map (\xi -> map (*xi) (nway (n-1) x)) x

interaction [] = []
interactions (x:[]) = []
interactions (x:xs) = (map (*x) xs) ++ interactions xs
```
