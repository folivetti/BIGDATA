```haskell
center :: [[Double]] -> [[Double]]
center x = map (\xi -> zipWith (-) xi m) x
  where
    m = map mean $ transpose x
    mean l = sum l / (fromIntegral $ length l)

covar :: Num a => [[a]] -> [[a]]
covar x = multMtx (transpose x) x

multMtx :: Num a => [[a]] -> [[a]] -> [[a]]
multMtx m1 m2 = [ [dotprod ri rj | rj <- m2'] | ri <- m1 ]
  where
    m2' = transpose m2
    dotprod l1 l2 = sum $ zipWith (*) l1 l2

pca :: Int -> [[Double]] -> [[Double]]
pca k x = H.toLists $ eve H.?? (H.All, H.Pos idx)
  where
    idx        =  H.subVector 0 k $ H.sortIndex (-eva) 
    (eva, eve) = (fst $ H.fromComplex eva', fst $ H.fromComplex eve')
    (eva', eve') = H.eig $ H.fromLists $ covar $ center x
```

```haskell
data Rating = Rating { user :: Int
                     , item :: Int
                     , rating :: Double
                     } deriving (Show, Read)

type UVmtx = M.HashMap Int [Double]

(.+.) :: Num a => [a] -> [a] -> [a]
(.+.) v1 v2 = zipWith (+) v1 v2

(.-.) :: Num a => [a] -> [a] -> [a]
(.-.) v1 v2 = zipWith (-) v1 v2

(.*.) :: Num a => [a] -> [a] -> [a]
(.*.) v1 v2 = zipWith (*) v1 v2

(+.) :: Num a => a -> [a] -> [a]
(+.) c v = map (+c) v

(*.) :: Num a => a -> [a] -> [a]
(*.) c v = map (*c) v

alpha = 0.1
lambda = 0.01

update :: Double -> Double -> [Double] -> [Double] -> Double -> ([Double], [Double])
update alpha lambda u v r = (u',v')
  where
    u'  = update' u v
    v'  = update' v u 
    err = r - (sum $ u .*. v)
    update' v1 v2 = v1 .+. (alpha *. ((err *. v2) .-. (lambda *. v1)))

svd :: Int -> Double -> Double -> Int -> [Rating] -> (UVmtx, UVmtx)
svd f alpha lambda it train = head $ drop it $ iterate adjust (u0, v0)
  where
    u0 = M.fromList (map (\i -> (i, replicate f 1e-1)) m)
    v0 = M.fromList (map (\i -> (i, replicate f 1e-1)) n)
    m  = nub $ map user train
    n  = nub $ map item train
    adjust (u, v) = foldl' adjustOne (u, v) train
    adjustOne (u,v) (Rating us' it' r') = (M.insert us' u' u , M.insert it' v' v)
      where
        (u', v') = update alpha lambda (u M.! us') (v M.! it') r'
```
