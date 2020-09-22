{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Entropy where

import Data.List (intercalate)
import Data.Ratio (Ratio, denominator, numerator, (%))
import qualified Data.Set as Set
import Context

newtype Entropy = Entropy Double
  deriving (Num, Eq, Ord, Real, Show, Fractional, Floating)

newtype InfoContent = InfoContent Double
  deriving (Num, Eq, Ord, Real, Show)

data EntropyTerm = EntropyTerm Int [Int]

data EntropySum = EntropySum (Ratio Int, [EntropyTerm]) (Ratio Int, [EntropyTerm])

instance Show EntropyTerm where
  show (EntropyTerm y []) = "H(" ++ show y ++ ")"
  show (EntropyTerm y xs) =
    "H(" ++ show y ++ "|" ++ intercalate "," (map show xs) ++ ")"
    
instance Show EntropySum where
  show (EntropySum p n) = showPair p ++ " -\n" ++ showPair n
    where
      showPair (_, []) = ""
      showPair (1, xs) = showSum xs
      showPair (sx, xs) = showScale sx ++ showSum xs
      showScale sx = "(" ++ show (numerator sx) ++ "/" ++ show (denominator sx) ++ ")"
      showSum xs = "[ " ++ intercalate " + " (map show xs) ++ " ]"

toEntropySum :: Order -> EntropyTerm -> EntropySum
toEntropySum (Order k) (EntropyTerm y xs)
  | length xs <= k = EntropySum (1, [EntropyTerm y xs]) (1, [])
  | otherwise = EntropySum (sp, ps) (sn, ns)
  where
    ks = subsets k xs
    ps = map (EntropyTerm y) ks
    ns = [EntropyTerm x k | x <- xs, k <- ks, x `notElem` k]
    n = length xs
    sp = product [1 .. max 1 (n - k)] % product [(k + 2) .. (n + 1)]
    sn = product [1 .. max 1 (n - k -1)] % product [(k + 2) .. (n + 1)]

subsets :: (Ord a) => Int -> [a] -> [[a]]
subsets k = Set.toList . Set.map Set.toList . Set.filter ((== k) . length) . Set.powerSet . Set.fromList

toAffix :: EntropyTerm -> Affix
toAffix (EntropyTerm to from) = Affix $ map (\x -> x - to) from
