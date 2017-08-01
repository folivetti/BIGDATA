```haskell
type Op a = (a -> a -> a)    -- | Function of two inputs
type Mapper k a = k -> (k,a) -- | Function that maps a key to a key value tuple

-- |'combiner' combines a list of tuples to a HashMap using 
-- a specified operator
combiner :: (Hashable k0, Eq k0)
         => Op a0 
         -> [(k0,a0)] 
         -> M.HashMap k0 a0
combiner f m   = M.fromListWith f m

-- | 'mapper' maps a list of keys to a HashMap of key-values 
-- using a mapper function and combining with an operator
mapper :: (Hashable k0, Eq k0) 
       => Op a0 
       -> Mapper k0 a0 
       -> [k0] 
       -> M.HashMap k0 a0
mapper combfun f job = combiner combfun $ map f job

-- |'reducer' reduces the output of the 'mapper' 
-- to produce the desired output
reducer :: (Hashable k0, Eq k0) 
        => Op a0 
        -> [M.HashMap k0 a0] 
        -> M.HashMap k0 a0
reducer f m  = foldl' (M.unionWith f) M.empty m
```
