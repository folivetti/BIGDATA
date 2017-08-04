```haskell
(.+.) l1 l2 = zipWith (+) l1 l2
(./) l1 v = map (/v) l1

sortTuple (a1,b1) (a2,b2)
  | b1 < b2 = LT
  | b1 > b2 = GT
  | b1==b2  = EQ

firstClusters idxs points = [ points !! i | i <- idxs ]

kmeans it points clusters
  | it == 0               = clusters
  | clusters' == clusters = clusters
  | otherwise             = kmeans (it-1) points clusters'
  where
    clusters' = emStep points clusters

emStep points clusters = maximizationStep $ estimationStep points clusters

estimationStep :: [[Double]] -> [[Double]] -> [[[Double]]]
estimationStep points clusters = map unTuple
                               $ groupBy compTuple
                               $ sortBy sortTuple
                               $ zip points $ assign points clusters
  where
    unTuple = map fst
    compTuple (a1,b1) (a2,b2) = b1==b2

maximizationStep :: [[[Double]]] -> [[Double]]
maximizationStep candidates = map mean candidates
  where
    mean xs = (foldl' (.+.) (head xs) (tail xs)) ./ (fromIntegral $ length xs)

assign points clusters = map closestTo points
  where
    closestTo p = argmin $ map (euclid p) clusters
    euclid pi ci = sum $ map (^2) $ zipWith (-) pi ci
    argmin xs = fst $ head $ sortBy sortTuple $ zip [0..] xs
```
