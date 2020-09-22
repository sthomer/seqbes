module Evaluation where

import Context
import Segmentation
import ContextMap

import Data.Char (isAscii, isLetter, isSpace, toLower)
import Data.List (intercalate)
import qualified Data.Set as Set
import Data.Hashable (Hashable)
import Data.HashMap.Lazy (keys, (!))
import Numeric (showFFloat)

newtype FileName = FileName String

newtype Score = Score (Double, Double, Double)

instance Show Score where
  show (Score (p, r, f)) =
    intercalate "\t" $
      map ((\s -> s "") . showFFloat (Just 3)) [p, r, f]

--------------------------------------------------------------------------------
-- Evaluation

printUsingFile :: (String -> String) -> String -> IO ()
printUsingFile f filename = do
  text <- readFile filename
  printUsingString f text

printUsingString :: (String -> String) -> String -> IO ()
printUsingString f s = do
  print $ f s

segmentationWith :: ([Token String] -> [Token String]) -> String -> [Token String]
segmentationWith f text = f (unmarked text)

nestedEntropyText :: Depth -> Order -> String -> [Token String]
nestedEntropyText d k = segmentationWith (nestedEntropy d k)

runMultiple :: FileName -> IO ()
runMultiple (FileName s) = do
  printUsingFile qualityForDepthsOrders s

runSegmentation :: FileName -> IO ()
runSegmentation (FileName s) = do
  printUsingFile (show . take 100 . nestedEntropyText (Depth 1) (Order 1)) s


tokenizeString :: String -> [Token String]
tokenizeString = map (\c -> Token [c])

tokenizeChar :: Char -> Token String
tokenizeChar c = Token [c]

unmarked :: String -> [Token String]
unmarked = map (tokenizeChar . toLower) . filter isLetter . filter isAscii

groundTruth :: String -> [Token String]
groundTruth =
  map Token . words . map toLower . filter ((||) <$> isLetter <*> isSpace) . filter isAscii

-- Since we're comparing sets, the direction of the scan doesn't actually matter
-- Therefore, indices are counted from the end of the list to the front
boundaryIndices :: [Token [a]] -> Set.Set Int
boundaryIndices ts =
  Set.fromDistinctDescList $
    (tail . scanr1 (+) . map (\(Token t) -> (fromIntegral . length) t)) ts

quality :: [Token [a]] -> [Token [a]] -> Score
quality source target = Score (p, r, f)
  where
    correct = Set.size $ boundaryIndices source `Set.intersection` boundaryIndices target
    found = length target - 1
    total = length source - 1
    p =
      if found == 0
        then 0
        else fromIntegral correct / fromIntegral found
    r = fromIntegral correct / fromIntegral total
    f = 2 * p * r / (p + r)

qualityForDepthsOrders :: String -> String
qualityForDepthsOrders text = unlines $ do
  d <- map Depth [1 .. 3]
  k <- map Order [1 .. 3]
  let ground = groundTruth text
      segments = nestedEntropy d k (unmarked text)
  return $
    show d ++ "\t" ++ show k ++ "\t" ++ show (quality ground segments)

rmse
  :: (Eq a, Hashable a, Real b)
  => ContextMap a b
  -> ContextMap a b
  -> FrequencyMap a
  -> Double
rmse xm ym fm = sqrt $ cmExpectation fm (cmMap (^ 2) (cmIntersectWith (-) xm ym))

mae
  :: (Eq a, Hashable a, Real b)
  => ContextMap a b
  -> ContextMap a b
  -> FrequencyMap a
  -> Double
mae xm ym fm = cmExpectation fm (cmMap abs (cmIntersectWith (-) xm ym))

msd
  :: (Eq a, Hashable a, Real b)
  => ContextMap a b
  -> ContextMap a b
  -> FrequencyMap a
  -> Double
msd xm ym fm = cmExpectation fm (cmIntersectWith (-) xm ym)

compareOrdering
  :: (Eq a, Hashable a, Ord b)
  => ContextMap a b
  -> ContextMap a b
  -> Double
compareOrdering (ContextMap xm) (ContextMap ym) = totalMatching / totalPairs
  where
    pairs = [(x, y) | x <- keys xm, y <- keys xm, xm ! x < xm ! y]
    matching = [(x, y) | (x, y) <- pairs, ym ! x < ym ! y]
    totalPairs = (fromIntegral . length) pairs
    totalMatching = (fromIntegral . length) matching

