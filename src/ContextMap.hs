{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}

module ContextMap where

import Context
import Entropy

import qualified Data.HashMap.Lazy as M
import Data.Hashable (Hashable)
import Data.HashMap.Lazy ((!))
import Data.List (intercalate, sortOn, tails)
import Numeric (showFFloat)
import Data.List.Index (deleteAt)

newtype Probability = Probability Double
  deriving (Num, Eq, Ord, Real, Show, Fractional)

newtype Transition = Transition Int
  deriving (Num, Eq, Ord, Real, Show)

newtype ContextMap a b = ContextMap (M.HashMap (Context a) b)

instance (Show a, RealFloat b) => Show (ContextMap a b) where
  show (ContextMap m) = intercalate "\n" s
    where
      s = map showEntry $ sortOn snd (M.toList m)
      showEntry (t, n) = show t ++ ": " ++ showFFloat (Just 3) n ""

newtype ContextTokenMap a b = ContextTokenMap (ContextMap a (M.HashMap (Token a) b))

pattern ContextTokenMap' m = ContextTokenMap (ContextMap m)

instance (Show a, Show b) => Show (ContextTokenMap a b) where
  show (ContextTokenMap' m) =
    intercalate "\n" $ map showContexts (M.toList m)
    where
      showContexts (c, ts) = show c ++ ":\n" ++ showTokens ts
      showTokens = intercalate "\n" . map showEntry . M.toList
      showEntry (t, n) = "  " ++ show t ++ ": " ++ show n

data AffixedContextMap a b = AffixedContextMap Affix (ContextMap a b)

pattern AffixedContextMap' x m = AffixedContextMap x (ContextMap m)

data AffixedContextTokenMap a b = AffixedContextTokenMap Affix (ContextTokenMap a b)

pattern AffixedContextTokenMap' x m = AffixedContextTokenMap x (ContextTokenMap' m)

-- Specific Maps

newtype EntropyMap a = EntropyMap (AffixedContextMap a Entropy)

pattern EntropyMap' x m = EntropyMap (AffixedContextMap' x m)

newtype FrequencyMap a = FrequencyMap (ContextMap a Probability)

pattern FrequencyMap' m = FrequencyMap (ContextMap m)

instance (Show a) => Show (FrequencyMap a) where
  show (FrequencyMap' m) = show m

newtype InfoContentMap a = InfoContentMap (AffixedContextTokenMap a InfoContent)

pattern InfoContentMap' x m = InfoContentMap (AffixedContextTokenMap' x m)

instance (Show a) => Show (InfoContentMap a) where
  show (InfoContentMap' x m) = show x ++ "\n" ++ show m

newtype ProbabilityMap a = ProbabilityMap (AffixedContextTokenMap a Probability)

pattern ProbabilityMap' x m = ProbabilityMap (AffixedContextTokenMap' x m)

instance (Show a) => Show (ProbabilityMap a) where
  show (ProbabilityMap' x m) = show x ++ "\n" ++ show m

newtype TransitionMap a = TransitionMap (AffixedContextTokenMap a Transition)

pattern TransitionMap' x m = TransitionMap (AffixedContextTokenMap' x m)

instance (Show a) => Show (TransitionMap a) where
  show (TransitionMap' x m) = show x ++ "\n" ++ show m

newtype Window a = Window [Token a]

instance (Show a) => Show (Window a) where
  show (Window w) = concatMap show w
  
newtype Count = Count Integer deriving (Num, Eq, Ord, Real, Show)

cmIntersectWith
  :: (Eq a, Hashable a)
  => (b -> b -> b)
  -> ContextMap a b
  -> ContextMap a b
  -> ContextMap a b
cmIntersectWith f (ContextMap xm) (ContextMap ym) = ContextMap $ M.intersectionWith f xm ym

cmUnionWith
  :: (Eq a, Hashable a)
  => (b -> b -> b)
  -> ContextMap a b
  -> ContextMap a b
  -> ContextMap a b
cmUnionWith f (ContextMap xm) (ContextMap ym) = ContextMap $ M.unionWith f xm ym

cmMap :: (b -> c) -> ContextMap a b -> ContextMap a c
cmMap f (ContextMap m) = ContextMap (M.map f m)

cmExpectation
  :: (Eq a, Hashable a, Real b)
  => FrequencyMap a
  -> ContextMap a b
  -> Double
cmExpectation (FrequencyMap fm) cm = sum $ M.elems m
  where
    (ContextMap m) = cmIntersectWith (*) fs cs
    fs = cmMap (\(Probability p) -> p) fm
    cs = cmMap realToFrac cm

prefixTransitionMap :: (Eq a, Hashable a) => Order -> [Token a] -> TransitionMap a
prefixTransitionMap k = transitionMap (prefix k) k

prefixEntropyMap :: (Eq a, Hashable a) => Order -> [Token a] -> EntropyMap a
prefixEntropyMap k = entropyMap . probabilityMap . prefixTransitionMap k

suffixTransitionMap :: (Eq a, Hashable a) => Order -> [Token a] -> TransitionMap a
suffixTransitionMap k = transitionMap (suffix k) k

suffixEntropyMap :: (Eq a, Hashable a) => Order -> [Token a] -> EntropyMap a
suffixEntropyMap k = entropyMap . probabilityMap . suffixTransitionMap k

forwardTransitionMap :: (Eq a, Hashable a) => Order -> [Token a] -> TransitionMap a
forwardTransitionMap k = transitionMap (forward k) k

forwardEntropyMap :: (Eq a, Hashable a) => Order -> [Token a] -> EntropyMap a
forwardEntropyMap k = entropyMap . probabilityMap . forwardTransitionMap k

frequencyMap :: (Eq a, Hashable a) => Order -> [Token a] -> FrequencyMap a
frequencyMap k ts = FrequencyMap' m
  where
    m = M.map (/ total) fs
    fs = (M.fromListWith (+) . map ((,1) . toContext)) (windows k ts)
    total = (fromIntegral . length) ts

probabilityMap :: TransitionMap a -> ProbabilityMap a
probabilityMap (TransitionMap' x m) = ProbabilityMap' x m'
  where
    m' = M.map asDist m
    asDist cxt = M.map ((/ total cxt) . realToFrac) cxt
    total = realToFrac . sum . M.elems

entropyMap :: ProbabilityMap a -> EntropyMap a
entropyMap (ProbabilityMap' x m) = EntropyMap' x m'
  where
    m' = M.map (entropy . M.elems) m

standardMap :: (Eq a, Hashable a) => EntropyMap a -> FrequencyMap a -> EntropyMap a
standardMap (EntropyMap' x em) (FrequencyMap' fm) = EntropyMap' x m'
  where
    m' = M.map standardize em
    fme = M.map (\(Probability x) -> Entropy x) fm -- there's gotta be a better way
    mean = sum $ M.elems $ M.unionWith (*) em fme
    sd = sqrt $ sum $ M.elems $ M.unionWith (*) fme (M.map (\e -> (e - mean) ^ 2) em)
    standardize e = (e - mean) / sd

infoContentMap :: ProbabilityMap a -> InfoContentMap a
infoContentMap (ProbabilityMap' x m) = InfoContentMap' x m'
  where
    asInfoContent = M.map infoContent
    m' = M.map asInfoContent m

infoContent :: Probability -> InfoContent
infoContent (Probability p)
  | p == 0 = InfoContent 0
  | p == 1 = InfoContent 0
  | otherwise = InfoContent $ (negate . logBase 2) p

entropy :: [Probability] -> Entropy
entropy = toEntropy . sum . map weight
  where
    weight (Probability p) = InfoContent p * infoContent (Probability p)
    toEntropy (InfoContent e) = Entropy e

toContext :: Window a -> Context a
toContext (Window w) = Context w

toWindow :: Context a -> Window a
toWindow (Context c) = Window c

--------------------------------------------------------------------------------
-- Construction

increment :: (Eq a, Hashable a) => (Token a, Context a) -> TransitionMap a -> TransitionMap a
increment (tkn, cxt) (TransitionMap' x m) = TransitionMap' x m'
  where
    m' = M.insertWith (M.unionWith (+)) cxt (M.singleton tkn 1) m

countTransitions :: (Eq a, Hashable a) => Affix -> [Window a] -> TransitionMap a
countTransitions affix =
  foldr (increment . affixSplit affix) (TransitionMap' affix M.empty)

transitionMap :: (Eq a, Hashable a) => Affix -> Order -> [Token a] -> TransitionMap a
transitionMap affix (Order k) = countTransitions affix . windows (Order (k + 1))

-- Alternatively, could go directly from affix to token/context
--  instead of going through window/split
transitionMap' :: (Eq a, Hashable a) => Affix -> [Token a] -> TransitionMap a
transitionMap' ax = countTransitions ax . windows (affixOrder ax)

-- Assume Window size is matched to Affix indices and the Token/Context
affixSplit :: Affix -> Window a -> (Token a, Context a)
affixSplit (Affix xs) (Window ts) = (token, Context context)
  where
    x = negate $ minimum (0 : xs)
    token = ts !! x
    context = deleteAt x ts

windows :: Order -> [Token a] -> [Window a]
windows (Order n) ts =
  map Window $
    (foldr (zipWith (:)) (repeat []) . take n . tails) ts


