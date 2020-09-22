{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Context where

import Data.Hashable (Hashable)

-- |
newtype Token a = Token a
  deriving (Eq, Hashable)

instance (Show a) => Show (Token a) where
  show (Token t) = show t

instance (Monoid a) => Semigroup (Token a) where
  (Token x) <> (Token y) = Token $ x <> y

instance (Monoid a) => Monoid (Token a) where
  mempty = Token mempty

newtype Context a = Context [Token a]
  deriving (Eq, Hashable)

instance (Show a) => Show (Context a) where
  show (Context ts) = concatMap show ts

instance (Monoid a) => Semigroup (Context a) where
  (Context x) <> (Context y) = Context $ x <> y

instance (Monoid a) => Monoid (Context a) where
  mempty = Context mempty

newtype Order = Order Int

instance Show Order where
  show (Order k) = show k

-- More general way of describing affixes
-- Easy for first-order. How to generalize to longer contexts?  Offset from token!
-- e.g. H(A|B) = [+1] = 1-prefix, H(B|A) = [-1] = suffix
-- e.g. H(A|BC) = [+1,+2], H(B|AC) = [-1,+1], H(C|AB) = [-2,-1]
-- e.g. H(B|ACDE) = [-1,+1,+2,+3]
newtype Affix = Affix [Int] deriving (Show)

affixOrder :: Affix -> Order
affixOrder (Affix axs) = Order $ maximum axs - minimum axs + 1

prefix :: Order -> Affix
prefix (Order k) = Affix [1 .. k]

suffix :: Order -> Affix
suffix (Order k) = Affix [(- k) .. (-1)]

forward :: Order -> Affix
forward (Order k) = Affix [- k]

backward :: Order -> Affix
backward (Order k) = Affix [k]
