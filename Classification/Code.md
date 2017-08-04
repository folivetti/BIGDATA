```haskell
gradientDesc :: [Point] -> Double -> Double -> [Double]
gradientDesc dataset alpha eta = fst $ head $ (dropWhile notConverged)
                             $ iterate  (update dataset alpha eta) (w0, nab0)
  where
    w0   = take n [1..]
    nab0 = take n [0..]
    n    = fromIntegral $ (length . fst $ head dataset)
    notConverged (w, nab) = (mse dataset w > 1e-6) && (foldl' (+) 0 (map (^2) nab) > 1e-6)


update :: [Point] -> Double -> Double -> ([Double], [Double]) -> ([Double], [Double])
update dataset alpha eta (w, nab0) = (zipWith (+) w'' nab', nab')
  where
    nab' = foldl' (zipWith (+)) (head nab) (tail nab)
    nab = nabla dataset e alpha
    e = err dataset w
    w' = zipWith (+) w (map (*eta) nab0)  -- momentum
    w'' = zipWith (+) w (map (*eta) nab') -- regularization

nabla :: [Point] -> [Double] -> Double -> [[Double]]
nabla dataset e alpha = zipWith step e dataset
  where
    step ei pi = map (\xi -> xi*ei*alpha/n) (fst pi)
    n = fromIntegral $ length dataset


sortTuple (a1,b1) (a2, b2)
  | a1 > a2 = LT
  | a1 < a2 = GT
  | a1 == a2 = EQ

naiveBayes :: [Point] -> [Point] -> Dict -> [String] -> [String]
naiveBayes train test dict klass = map classify test
  where
    classify (t, label) = snd $ head $ sortBy sortTuple [(prob t k, k) | k <- klass]

    prob t k            = (likelihood t k) * (classprior k) / (predictor t)
    
    classprior k = pk M.! k
    predictor  x = foldl1 (*) $ zipWith (M.!) px x
    
    pk = M.fromListWith (+) $ map (\d -> (snd d, 1)) train
    px = [M.fromListWith (+) $ map (\xi -> (xi, 1)) x | x<-xset]
    xset    = transpose $ map fst train

    likelihood x k = foldl1 (*) $ zipWith (\xi l -> (pXK xi l)/(pk M.! k)) x (xsetK k)
    pXK xi l = fromIntegral $ length $ filter (==xi) l

    xsetK k = transpose $ map fst $ filter (\t -> snd t == k) train


sortTuple (a1, b1) (a2, b2)
  | a1 < a2  = LT
  | a1 > a2  = GT
  | a1 == a2 = EQ

mostFrequent :: Ord a => [a] -> a
mostFrequent l = head . maximumBy (comparing length) . group . sort $ l

knn :: [Point] -> [Point] -> Int -> [Double]
knn train test k = map (knearest) test
  where
    knearest t = mostFrequent 
                 $ map snd 
                 $ take k 
                 $ sortBy sortTuple 
                 $ map (distance t) train
    distance (t1,y1) (t2,y2) = (euclid t1 t2, y2)
    euclid t1 t2 = sum $ zipWith sqdiff t1 t2
    sqdiff t1 t2 = (t1 - t2)^2
```
