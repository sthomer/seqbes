{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib where

import Data.Bifunctor (bimap)
import Data.Char (isAscii, isLetter, isSpace, toLower)
import qualified Data.HashMap.Lazy as M
import Data.List (intercalate, sortOn, tails, transpose)
import qualified Data.Set as S
import Numeric (showFFloat)
import Data.Hashable (Hashable)

-- Constrained Boundary Entropy Segmentation
--   Limit to first-order transitions
--   Limits to length 2 segments

newtype Token a = Token {unToken :: [a]} deriving (Eq, Hashable, Show)

newtype Context a = Context {unContext :: [Token a]} deriving (Eq, Hashable, Show)

toStrings :: String -> [String]
toStrings = map (: [])

toTokens :: [a] -> [Token a]
toTokens = map (Token . (:[]))

fromTokens :: [Token a] -> [[a]]
fromTokens = map unToken

toSplits :: Int -> [Token a] -> [(Context a, Context a)]
toSplits = toSuffixSplits

toWindows :: Int -> [Token a] -> [(Context a, Context a)]
toWindows n ts = zip (c : cs) cs
  where
    (c : cs) = (map fst . toSuffixSplits n) ts

toSplitsOf :: Int -> Int -> [Token a] -> [(Context a, Context a)]
toSplitsOf n k = map (bimap Context Context . splitAt k)
    . filter ((==) n . length)
    . transpose
    . take n
    . tails

toSuffixSplits :: Int -> [Token a] -> [(Context a, Context a)]
toSuffixSplits n = toSplitsOf (n + 1) n

toPrefixSplits :: Int -> [Token a] -> [(Context a, Context a)]
toPrefixSplits n = toSplitsOf (n + 1) 1

countPairs :: (Eq a, Hashable a) =>
 [(Context a, Context a)] ->
 M.HashMap (Context a, Context a) Integer
countPairs = M.fromListWith (+) . map (,1)

countSingles :: (Eq a, Hashable a) =>
  [(Context a, Context a)] ->
  M.HashMap (Context a) Integer
countSingles = M.fromListWith (+) . map ((,1) . fst)

toProbs :: (Eq a, Hashable a) =>
  M.HashMap (Context a) Integer ->
  M.HashMap (Context a, Context a) Integer ->
  M.HashMap (Context a, Context a) Double
toProbs singles = M.mapWithKey toProb
  where
    toProb (k, _) v = fromIntegral v / fromIntegral (singles M.! k)

toEnts :: (Eq a, Hashable a) =>
  M.HashMap (Context a, Context a) Double ->
  M.HashMap (Context a) Double
toEnts = M.fromListWith (+) . map toEnt . M.toList
  where
    infoContent 0 = 0
    infoContent 1 = 0
    infoContent p = (negate . logBase 2) p
    toEnt ((k, _), v) = (k, v * infoContent v)

toICs :: (Eq a, Hashable a) =>
  M.HashMap (Context a, Context a) Double ->
  M.HashMap (Context a) Double
toICs = M.fromList . map toIC . M.toList
  where
    infoContent 0 = 0
    infoContent 1 = 0
    infoContent p = (negate . logBase 2) p
    toIC ((Context ka, Context kb), v) = (Context (ka ++ kb), infoContent v)

toEntropy :: (Eq a, Hashable a) =>
  [(Context a, Context a)] ->
  M.HashMap (Context a) Double
toEntropy pairs = toEnts $ toProbs (countSingles pairs) (countPairs pairs)

toInfoContent :: (Eq a, Hashable a) =>
  [(Context a, Context a)] ->
  M.HashMap (Context a) Double
toInfoContent pairs = toICs $ toProbs (countSingles pairs) (countPairs pairs)

pprint :: (Show a) => M.HashMap (Context a) Double -> IO ()
pprint m = putStrLn $ intercalate "\n" s
  where
    s = map showEntry $ sortOn snd (M.toList m)
    showEntry (t, n) = show t ++ ": " ++ showFFloat (Just 3) n ""

emptyContext :: (Eq a) => Context a -> Bool
emptyContext (Context ts) = all (Token [] ==) ts

final :: Context a  -> Token a
final (Context ts) = last ts

segmentWith :: (Eq a, Hashable a) =>
  (Context a -> Context a) ->
  Int ->
  M.HashMap (Context a) Double ->
  [Token a] ->
  [Token a]
segmentWith f k m ts = map tokenize $ snd $ foldr boundary ([], []) $ toWindows k padded
  where
    padded = replicate k (Token []) ++ ts ++ [Token []]
    boundary (a, b) (seg, segs)
     | emptyContext a = ([], b : Context seg : segs) -- first
     | null seg && null segs = ([final b], []) -- last
     | isBoundary || isFull = ([final b], Context seg : segs) -- segment
     | otherwise = (final b : seg, segs) -- don't segment
      where
        isBoundary = M.lookupDefault 0 (f a) m < M.lookupDefault 0 (f b) m
        isFull = length seg > 1

rev :: [Token a] -> [Token a]
rev = reverse . map (Token . reverse . unToken)

tokenize :: Context a -> Token a
tokenize (Context ts) = Token $ concatMap unToken ts

nestedCountWith :: (Eq a, Hashable a) =>
 ([Token a] -> [Token a]) -> Int -> Int -> [Token a] -> [Token a]
nestedCountWith _ _ _ [t] = [t]
nestedCountWith _ _ 0 ts = ts
nestedCountWith f k d ts =
  nestedCountWith f k (d - 1) $
    f $ segmentWith id k entropies $ f ts
  where
    entropies = toEntropy $ toSplits k $ f ts

nestedCountSuffix :: (Eq a, Hashable a) =>
  Int -> Int -> [Token a] -> [Token a]
nestedCountSuffix = nestedCountWith id

nestedCountPrefix :: (Eq a, Hashable a) =>
  Int -> Int -> [Token a] -> [Token a]
nestedCountPrefix = nestedCountWith rev

nestedCountBoth :: (Eq a, Hashable a) => Int -> Int -> [Token a] -> [Token a]
nestedCountBoth _ _ [t] = [t]
nestedCountBoth _ 0 ts = ts
nestedCountBoth k d ts = nestedCountBoth k (d - 1) $ segmentByIndex ixs ts
  where
    suffixEntropies = toEntropy $ toSplits k ts
    prefixEntropies = toEntropy $ toSplits k $ rev ts
    suffixSegments = segmentWith id k suffixEntropies ts
    prefixSegments = rev $ segmentWith id k prefixEntropies $ rev ts
    suffixIndex = boundaryIndex suffixSegments
    prefixIndex = boundaryIndex prefixSegments
    ixs = S.toAscList $ S.union suffixIndex prefixIndex

nestedPassWith :: (Eq a, Hashable a) =>
  ([Token a] -> [Token a]) ->
  Int ->
  Int ->
  [Token a] ->
  [Token a]
nestedPassWith f k d ts = go (segmentWith (end k) k entropies) d ts
  where
    entropies = toEntropy $ toSplits k $ f ts
    go _ _ [t] = [t]
    go _ 0 ts = ts
    go prop d ts = go prop (d - 1) $ f $ prop $ f ts

nestedPassSuffix :: (Eq a, Hashable a) => Int -> Int -> [Token a] -> [Token a]
nestedPassSuffix = nestedPassWith id

nestedPassPrefix :: (Eq a, Hashable a) => Int -> Int -> [Token a] -> [Token a]
nestedPassPrefix = nestedPassWith rev

end :: Int -> Context a -> Context a
end k = Context . reverse . take k . reverse . unContext

nestedPassBoth :: (Eq a, Hashable a) =>  Int -> Int -> [Token a] -> [Token a]
nestedPassBoth k d ts = go d ts
  where
    suffixer = segmentWith (end k) k (toEntropy $ toSplits k ts)
    prefixer = segmentWith (end k) k (toEntropy $ toSplits k $ rev ts)
    go _ [t] = [t]
    go 0 ts = ts
    go d ts = go (d - 1) $ segmentByIndex ixs ts
      where
        suffixIndex = boundaryIndex $ suffixer ts
        prefixIndex = boundaryIndex $ rev $ prefixer $ rev ts
        ixs = S.toAscList $ S.union prefixIndex suffixIndex

unmarked :: String -> [Token Char]
unmarked = toTokens . map toLower . filter isLetter . filter isAscii

groundTruth :: String -> [Token Char]
groundTruth =
  map Token . words . map toLower . filter ((||) <$> isLetter <*> isSpace) . filter isAscii

tokenLength :: Token a -> Int
tokenLength (Token t) = length t

boundaryIndex :: [Token a] -> S.Set Int
boundaryIndex ts =
  S.fromDistinctDescList $
    (tail . scanr1 (+) . map tokenLength) (reverse ts)

segmentByIndex :: [Int] -> [Token a] -> [Token a]
segmentByIndex ixs ts = map tokenize $ reverse $ fst $ foldr f ([], ts) ns
  where
    ixs' = (0 : ixs) ++ [length ts]
    ns = reverse $ zipWith (-) (tail ixs') ixs'
    f n (segs, ts) = (Context (take n ts) : segs, drop n ts)

quality :: [Token a] -> [Token a] -> (Double, Double, Double)
quality source target = (p, r, f)
  where
    correct = S.size $ boundaryIndex source `S.intersection` boundaryIndex target
    found = length target - 1
    total = length source - 1
    p =
      if found == 0
        then 0
        else fromIntegral correct / fromIntegral found
    r = fromIntegral correct / fromIntegral total
    f = 2 * p * r / (p + r)

qualityAt :: Int -> Int -> String -> ((Double, Double, Double), (Double, Double, Double))
qualityAt k d source = (quality ground pass, quality ground count)
  where
    ground = groundTruth source
    strings = toTokens source
    pass = nestedPassSuffix k d strings
    count = nestedCountSuffix k d strings

qualities :: String -> IO ()
qualities filename = do
  text <- readFile filename
  putStrLn $
    intercalate
      "\t"
      ["Order", "Depth", "Count-P", "Pass-P", "Count-R", "Pass-R", "Count-F", "Pass-F"]
  putStrLn $
    unlines $ do
      k <- [1 .. 4]
      d <- [1 .. 4]
      let ((passP, passR, passF), (countP, countR, countF)) = qualityAt k d text
          vals = [countP, passP, countR, passR, countF, passF]
      return $ show k ++ "\t" ++ show d ++ "\t" ++ concatMap (\v -> showFFloat (Just 3) v "\t") vals

passFor :: Int -> Int -> String -> IO [String]
passFor k d filename = do
  text <- readFile filename
  return $ take 100 $ fromTokens $ nestedCountSuffix k d $ unmarked text
