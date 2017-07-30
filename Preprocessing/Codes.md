```haskell
-- |professions
data Ocupacao = Engenheiro | Professor | Gerente | Estudante
              deriving (Show, Read, Enum)

-- |letter grade
data Conceito = F | D | C | B | A
              deriving (Show, Read, Enum)

-- |'rank' converts the Conceito into a normalized rank value
rank :: String -> Double
rank co = (fromEnum' co') / (fromEnum' A)
  where 
    co' = read co :: Conceito
    fromEnum' = fromIntegral . fromEnum

-- |'binarize' parses a Ocupacao into a binary list
binarize :: String -> [Double]
binarize oc = [bool2double $ oc' ==  i | i <- [0..3]]
  where
    oc' = fromEnum (read oc :: Ocupacao)
    bool2double True  = 1.0
    bool2double False = 0.0

-- |'parseFile' parses a space separated file 
-- to a list of lists of Double
parseFile :: String -> [[Double]]
parseFile file = map parseLine (lines file)
  where
    parseLine l = concat $ toDouble (words l)
    toDouble [oc, co, grade] = [binarize oc, [rank co, read grade]]
```

```haskell
-- |'minkowski' distance
minkowski :: Double -> [Double] -> [Double] -> Double
minkowski p x y  = summation ** (1.0/p)
  where
    summation = sum $ zipWith (pointwiseDist) x y
    pointwiseDist xi yi = (abs (xi - yi)) ** p

-- |'euclidean' is just a special case of minkowski
euclidean :: [Double] -> [Double] -> Double
euclidean = minkowski 2

-- |'jaccard' distance
jaccard :: [Double] -> [Double] -> Double
jaccard x y = 1 - (sumProd / sumSum)
  where
    sumProd = sum $ zipWith (*) x y
    sumSum  = sum $ zipWith (binsum) x y

-- |'binsum' is a binary addition for Jaccard
binsum :: Double -> Double -> Double
binsum 1 _ = 1
binsum _ 1 = 1
binsum _ _ = 0

cosine :: [Double] -> [Double] -> Double
cosine x y = (dotprod x y) / (norm' x * norm' y)
  where
    dotprod u v = sum $ zipWith (*) u v
    norm' u  = dotprod u u

-- |'standardize' a vector
standardize :: [Double] -> [Double]
standardize x = map toCenter x
  where
    toCenter xi = (xi - mean x) / stdX
    mean x = (sum x) / (length' x)
    stdX  = sqrt varX
    varX  = mean $ map (\xi -> (xi - mean x) ** 2) x
    length' l = fromIntegral $ length l

-- |'maxminScale' scales vector to [0,1]
maxminScale :: [Double] -> [Double]
maxminScale x = map scale x
  where
    scale xi = (xi - minimum x) / (maximum x - minimum x)

-- |'norm' calculates the norm vector
norm :: [Double] -> [Double]
norm x = map (/nx) x
  where
    nx = sqrt . sum $ map (^2) x
```

```haskell
type TF = M.HashMap String Double

-- |'tokenize' text to BoW
tokenize :: String -> [[String]]
tokenize text = notEmpty $ map toTokens $ lines text
  where
    notEmpty       = filter (not . null)
    toTokens line   = preProcess $ words line
    preProcess     = (filter moreThanTwo) . (map normalize)
    normalize word = map toLower $ filter isAlphaNum word
    moreThanTwo l  = length l > 2

-- |generate 'ngrams' from a sequence of tokens
ngrams :: Int -> [String] -> [String]
ngrams n tokens = map genGram $ grams tokens
  where
    genGram       = (intercalate " ") . (take n)
    grams tokens  = sizeN $ tails tokens 
    sizeN         = filter (\l' -> length l' >= n)

-- |generate 'ngrams' for each document of a corpus
getNgrams :: Int -> [[String]] -> [[String]]
getNgrams n corpus = map (ngrams n) corpus


-- |generate 'skipgrams' of 'k' and 'n' for a sequence of tokens
skipgrams n k tokens = map (intercalate " ") $ grams tokens
  where
    grams tokens  = sizeN $ concat $ map (takeSkips n) $ tails tokens
    takeSkips 0 ws = [[]]
    takeSkips n [] = [[]]
    takeSkips n (w:[]) = [[w]]
    takeSkips n (w:ws) = concat [zipWith (++) (repeat [w]) (takeSkips (n - 1) (drop k' ws)) | k' <- [0..k]]
    sizeN         = filter (\l' -> length l'  == n)

-- |generate 'n-grams' from tokenized text
getSkipgrams :: Int -> Int -> [[String]] -> [[String]]
getSkipgrams n k corpus = map (skipgrams n k) corpus

-- |'binarize' the corpus of BoW
binarize :: [[String]] -> [TF]
binarize corpus = map binVec corpus
  where
    binVec line = M.fromList $ zip line [1,1..]

-- |'tf' generates the term frequency of a BoW
tf :: [[String]] -> [TF]
tf corpus = map countWords corpus
  where
    countWords doc = M.map (\v -> v / (len doc)) $ M.fromListWith (+) $ zip doc [1,1..]
    len d = fromIntegral $ length d

-- | 'df' calculates document frequency of words in a dictionary
df :: [TF] -> TF
df corpus = foldl' (M.unionWith (+)) M.empty corpus

-- | 'tfidf' of a document
tfidf :: [TF] -> TF -> [TF]
tfidf tf' df' = map calcTFIDF tf'
  where
    calcTFIDF t = M.mapWithKey calc t
    calc k v = v * n / (getDF k)
    getDF t = M.lookupDefault 0 t df'
    n = fromIntegral $ length tf'
```
