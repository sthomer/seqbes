module Segmentation where

import Context
import Entropy
import ContextMap

import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import qualified Data.HashMap.Lazy as M (lookup)
import Data.List (inits)

data Boundary = Boundary | NoBoundary

newtype BoundaryPolicy a = BoundaryPolicy (a -> a -> Boundary)

newtype Segment a = Segment [Token a]

instance (Show a) => Show (Segment a) where
  show (Segment s) = concatMap show s

instance Semigroup (Segment a) where
  (Segment xs) <> (Segment ys) = Segment $ xs ++ ys

instance Monoid (Segment a) where
  mempty = Segment []

newtype Frame a = Frame (Token a, (Context a, Context a), (Context a, Context a))

newtype Depth = Depth Int

instance Show Depth where
  show (Depth d) = show d

rise :: (Ord a) => a -> a -> Boundary
rise x y
  | x < y = Boundary
  | otherwise = NoBoundary

entropyBoundary
  :: (Eq a, Hashable a)
  => BoundaryPolicy Entropy
  -> EntropyMap a
  -> (Context a, Context a)
  -> Boundary
entropyBoundary (BoundaryPolicy bp) (EntropyMap' x m) (cxtA, cxtB)
  | (cxtA, cxtB) == (Context [], Context []) = Boundary -- Hack
  | otherwise = fromMaybe NoBoundary $
    do
      entropyA <- M.lookup cxtA m
      entropyB <- M.lookup cxtB m
      pure $ entropyA `bp` entropyB

icBoundary
  :: (Eq a, Hashable a)
  => BoundaryPolicy InfoContent
  -> InfoContentMap a
  -> ((Token a, Context a), (Token a, Context a))
  -> Boundary
icBoundary (BoundaryPolicy bp) (InfoContentMap' x m) ((tknA, cxtA), (tknB, cxtB)) =
  fromMaybe NoBoundary $ do
    mA <- M.lookup cxtA m
    icA <- M.lookup tknA mA
    mB <- M.lookup cxtB m
    icB <- M.lookup tknB mB
    return $ icA `bp` icB

unionBoundary :: Boundary -> Boundary -> Boundary
unionBoundary NoBoundary NoBoundary = NoBoundary
unionBoundary _ Boundary = Boundary
unionBoundary Boundary _ = Boundary

emptyFrame :: (Monoid a) => Frame a
emptyFrame = Frame (mempty, (mempty, mempty), (mempty, mempty))

frames :: (Monoid a) => Order -> [Token a] -> [Frame a]
frames (Order k) ts =
  emptyFrame :
  map
    Frame
    ( zip3
        ts
        (starts ++ contextPairs (Order k) ts)
        (contextPairs (Order k) ts ++ ends)
    )
  where
    (x : xs) = (take (k + 1) . map Context . inits) ts
    starts = zip (x : xs) xs
    (y : ys) = (reverse . take (k + 1) . map (Context . reverse) . inits . reverse) ts
    ends = zip (y : ys) ys

contextPairs :: Order -> [Token a] -> [(Context a, Context a)]
contextPairs k (t : ts) = zip (contexts (t : ts)) (contexts ts)
  where
    contexts xs = map toContext $ windows k xs

entropyFold ::
  (Eq a, Hashable a, Monoid a) =>
  Order ->
  BoundaryPolicy Entropy ->
  (EntropyMap a, EntropyMap a) ->
  [Token a] ->
  [Segment a]
entropyFold k bp (pm, sm) ts = snd $ foldr boundaryFrame initial $ frames k ts
  where
    initial = (Segment [], [])
    boundaryFrame (Frame (t, suffixCxts, prefixCxts)) (Segment s, segments) =
      case isBoundary of
        Boundary -> (Segment [t], Segment s : segments)
        NoBoundary -> (Segment (t : s), segments)
      where
        isBoundary = unionBoundary isPrefixBoundary isSuffixBoundary
        isPrefixBoundary = entropyBoundary bp pm prefixCxts
        isSuffixBoundary = entropyBoundary bp sm suffixCxts

segmentByBoundaryEntropy
  :: (Eq a, Hashable a, Monoid a)
  => Order
  -> [Token a]
  -> [Segment a]
segmentByBoundaryEntropy k ts =
  entropyFold k (BoundaryPolicy rise) (prefixEntropyMap k ts, suffixEntropyMap k ts) ts

nestedSegmentation
  :: (Monoid a)
  => (Order -> [Token a] -> [Segment a])
  -> Depth
  -> Order
  -> [Token a]
  -> [Token a]
nestedSegmentation f (Depth d) k ts
  | d < 1 = ts
  | otherwise = nestedSegmentation f (Depth (d - 1)) k $ map tokenize $ f k ts

nestedEntropy
  :: (Eq a, Hashable a, Monoid a)
  => Depth
  -> Order
  -> [Token a]
  -> [Token a]
nestedEntropy = nestedSegmentation segmentByBoundaryEntropy

tokenize :: (Monoid a) => Segment a -> Token a
tokenize (Segment ts) = foldr1 (<>) ts
