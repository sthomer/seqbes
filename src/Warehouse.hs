module Warehouse where

import Lib
import qualified Data.HashMap.Lazy as M
import Data.Maybe (fromMaybe)


icSuffixSegments :: Order -> BoundaryPolicy -> InfoContentMap -> [Token] -> [Segment]
icSuffixSegments (Order k) bp im ts = go ts initial
  where
    initial = Segment $ reverse $ take k ts
    go (t : ts) (Segment s)
      | length ts < k + 1 = [Segment (reverse (t' : s))]
      | otherwise = case boundaryIC bp im tkns cxts of
        Boundary -> Segment (reverse (t' : s)) : go ts (Segment [])
        NoBoundary -> go ts $ Segment (t' : s)
      where
        t' = (t : ts) !! k
        tkns = (t, head ts)
        cxts = (Context $ take k (t : ts), Context $ take k ts)

segmentBySuffixIC :: Int -> String -> [String]
segmentBySuffixIC k s =
  map show $
    icSuffixSegments (Order k) rise (im s) (tokenizeString s)
  where
    im = infoContentMap . markovChain . suffixTransitionMap (Order k) . tokenizeString

suffixSegments :: Order -> BoundaryPolicy -> EntropyMap -> [Token] -> [Segment]
suffixSegments (Order k) bp em ts = go ts initial
  where
    initial = Segment $ reverse $ take k ts
    go (t : ts) (Segment s)
      | length ts < k + 1 = [Segment (reverse (t' : s))]
      | otherwise = case boundary bp em cxts of
        Boundary -> Segment (reverse (t' : s)) : go ts (Segment [])
        NoBoundary -> go ts $ Segment (t' : s)
      where
        t' = (t : ts) !! k
        cxts = (Context $ take k (t : ts), Context $ take k ts)

segmentBySuffixEntropy :: Int -> String -> [String]
segmentBySuffixEntropy k s =
  map show $
    suffixSegments (Order k) rise (em s) (tokenizeString s)
  where
    em = entropyMap . markovChain . suffixTransitionMap (Order k) . tokenizeString

prefixSegments :: Order -> BoundaryPolicy -> EntropyMap -> [Token] -> [Segment]
prefixSegments (Order k) bp em ts = go ts initial
  where
    initial = Segment []
    go (t : ts) (Segment s)
      | length ts < k + 1 = [Segment (reverse s ++ (t : ts))]
      | otherwise = case boundary bp em cxts of
        Boundary -> Segment (reverse s) : go ts (Segment [t])
        NoBoundary -> go ts $ Segment (t : s)
      where
        cxts = (Context $ take k (t : ts), Context $ take k ts)

segmentByPrefixEntropy :: Int -> String -> [String]
segmentByPrefixEntropy k s =
  map show $
    prefixSegments (Order k) rise (em s) (tokenizeString s)
  where
    em = entropyMap . markovChain . prefixTransitionMap (Order k) . tokenizeString

-- Note: suffixSegments is Order k ahead of prefixSegments in the list

boundarySegments :: Order -> BoundaryPolicy -> (EntropyMap, EntropyMap) -> [Token] -> [Segment]
boundarySegments (Order k) bp (pm, sm) ts = go ts initial
  where
    initial = Segment $ reverse $ take k ts
    go (t : ts) (Segment s)
      | length ts < k = [Segment (reverse s) | not (null s)]
      | otherwise = case unionBoundary prefixBoundary suffixBoundary of
        Boundary -> Segment (reverse (t' : s)) : go ts (Segment [])
        NoBoundary -> go ts $ Segment (t' : s)
      where
        t' = (t : ts) !! k
        prefixBoundary = boundary bp pm prefixContexts
        suffixBoundary = boundary bp sm suffixContexts
        prefixContexts =
          ( Context $ (take k . drop k) (t : ts),
            Context $ (take k . drop k) ts
          )
        suffixContexts =
          ( Context $ take k (t : ts),
            Context $ take k ts
          )


