{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE StandaloneDeriving #-}

module Lib where

--------------------------------------------------------------------------------

import Data.Char (isAscii, isLetter, isSpace, toLower)
import Data.HashMap.Lazy ((!))
import qualified Data.HashMap.Lazy as M
import Data.Hashable (Hashable)
import Data.List (inits, intercalate, permutations, sortOn, tails)
import Data.List.Index (deleteAt)
import Data.Maybe (fromMaybe)
import Data.Ratio (denominator, numerator)
import qualified Data.Set as Set
import Numeric (showFFloat)

--------------------------------------------------------------------------------
-- Types

-- Affixes and Entropy Expressions

-- More general way of describing affixes
-- Easy for first-order. How to generalize to longer contexts?  Offset from token!
-- e.g. H(A|B) = [+1] = 1-prefix, H(B|A) = [-1] = suffix
-- e.g. H(A|BC) = [+1,+2], H(B|AC) = [-1,+1], H(C|AB) = [-2,-1]
-- e.g. H(B|ACDE) = [-1,+1,+2,+3]

newtype Affix = Affix [Int] deriving (Show)

data EntropyExpr
  = Term {to :: Int, from :: [Int]}
  | Sum {scale :: Rational, positive :: [EntropyExpr], negative :: [EntropyExpr]}

-- General Maps

--data ContextMap a b where
--  ContextMap :: (Eq a, Hashable a) => M.HashMap (Context a) b -> ContextMap a b
newtype ContextMap a b = ContextMap (M.HashMap (Context a) b)

instance (Show a, RealFloat b) => Show (ContextMap a b) where
  show (ContextMap m) = intercalate "\n" s
    where
      s = map showEntry $ sortOn snd (M.toList m)
      showEntry (t, n) = show t ++ ": " ++ showFFloat (Just 3) n ""

instance Functor (ContextMap a) where
  fmap f (ContextMap m) = ContextMap $ fmap f m

instance (Eq a, Hashable a) => Semigroup (ContextMap a b) where
  (ContextMap x) <> (ContextMap y) = ContextMap $ x <> y

instance (Eq a, Hashable a) => Monoid (ContextMap a b) where
  mempty = ContextMap M.empty

newtype ContextTokenMap a b = ContextTokenMap (ContextMap a (M.HashMap (Token a) b))

pattern ContextTokenMap' m = ContextTokenMap (ContextMap m)

instance (Show a, Show b) => Show (ContextTokenMap a b) where
  show (ContextTokenMap' m) =
    intercalate "\n" $ map showContexts (M.toList m)
    where
      showContexts (c, ts) = show c ++ ":\n" ++ showTokens ts
      showTokens = intercalate "\n" . map showEntry . M.toList
      showEntry (t, n) = "  " ++ show t ++ ": " ++ show n

instance Functor (ContextTokenMap a) where
  fmap f (ContextTokenMap' m) = ContextTokenMap' $ fmap (fmap f) m

instance (Eq a, Hashable a) => Semigroup (ContextTokenMap a b) where
  (ContextTokenMap x) <> (ContextTokenMap y) = ContextTokenMap $ x <> y

instance (Eq a, Hashable a) => Monoid (ContextTokenMap a b) where
  mempty = ContextTokenMap mempty

data ACMap a b = ACMap Affix (ContextMap a b)

pattern ACMap' x m = ACMap x (ContextMap m)

instance Functor (ACMap a) where
  fmap f (ACMap x m) = ACMap x (fmap f m)

instance (Eq a, Hashable a) => Semigroup (ACMap a b) where
  -- take the left affix and ignore the right
  (ACMap ax x) <> (ACMap ay y) = ACMap ax (x <> y)

instance (Eq a, Hashable a) => Monoid (ACMap a b) where
  -- the empty affix might cause trouble with (<>)
  mempty = ACMap (Affix []) mempty

data ACTMap a b = ACTMap Affix (ContextTokenMap a b)

pattern ACTMap' x m = ACTMap x (ContextTokenMap' m)

instance (Eq a, Hashable a) => Semigroup (ACTMap a b) where
  -- take the left affix and ignore the right
  (ACTMap ax x) <> (ACTMap ay y) = ACTMap ax (x <> y)

instance (Eq a, Hashable a) => Monoid (ACTMap a b) where
  -- the empty affix might cause trouble with (<>)
  mempty = ACTMap (Affix []) mempty

instance Functor (ACTMap a) where
  fmap f (ACTMap x m) = ACTMap x (fmap f m)

-- Specific Maps

newtype EntropyMap a = EntropyMap (ACMap a Entropy)

pattern EntropyMap' x m = EntropyMap (ACMap' x m)

newtype FrequencyMap a = FrequencyMap (ContextMap a Probability)

pattern FrequencyMap' m = FrequencyMap (ContextMap m)

newtype InfoContentMap a = InfoContentMap (ACTMap a InfoContent)

pattern InfoContentMap' x m = InfoContentMap (ACTMap' x m)

newtype ProbabilityMap a = ProbabilityMap (ACTMap a Probability)

pattern ProbabilityMap' x m = ProbabilityMap (ACTMap' x m)

newtype TransitionMap a = TransitionMap (ACTMap a Transition)

pattern TransitionMap' x m = TransitionMap (ACTMap' x m)

-- Elements

newtype Token a = Token a deriving (Eq, Hashable, Ord)

instance (Monoid a) => Semigroup (Token a) where
  (Token x) <> (Token y) = Token $ x <> y

instance (Monoid a) => Monoid (Token a) where
  mempty = Token mempty

newtype Context a = Context [Token a] deriving (Eq, Hashable, Ord)

instance (Monoid a) => Semigroup (Context a) where
  (Context x) <> (Context y) = Context $ x <> y

instance (Monoid a) => Monoid (Context a) where
  mempty = Context mempty

newtype Window a = Window [Token a]

newtype Segment a = Segment [Token a]

instance Semigroup (Segment a) where
  (Segment xs) <> (Segment ys) = Segment $ xs ++ ys

instance Monoid (Segment a) where
  mempty = Segment []

data Boundary = Boundary | NoBoundary

newtype BoundaryPolicy a = BoundaryPolicy (a -> a -> Boundary)

-- Safe Aliases

newtype Order = Order Int

newtype Depth = Depth Int

newtype Count = Count Integer deriving (Num, Eq, Ord, Real, Show)

newtype Frame a = Frame (Token a, (Context a, Context a), (Context a, Context a))

newtype Score = Score (Double, Double, Double)

newtype Probability = Probability Double deriving (Num, Eq, Ord, Real, Show, Fractional)

newtype Entropy = Entropy Double deriving (Num, Eq, Ord, Real, Show, Fractional, Floating)

newtype InfoContent = InfoContent Double deriving (Num, Eq, Ord, Real, Show)

newtype Transition = Transition Int deriving (Num, Eq, Ord, Real, Show)

newtype FileName = FileName String

--------------------------------------------------------------------------------
-- Display

instance (Show a) => Show (TransitionMap a) where
  show (TransitionMap' x m) = show x ++ "\n" ++ show m

instance (Show a) => Show (ProbabilityMap a) where
  show (ProbabilityMap' x m) = show x ++ "\n" ++ show m

instance (Show a) => Show (InfoContentMap a) where
  show (InfoContentMap' x m) = show x ++ "\n" ++ show m

instance (Show a) => Show (FrequencyMap a) where
  show (FrequencyMap' m) = show m

instance (Show a) => Show (Window a) where
  show (Window w) = concatMap show w

instance (Show a) => Show (Context a) where
  show (Context ts) = concatMap show ts

instance (Show a) => Show (Segment a) where
  show (Segment s) = concatMap show s

instance (Show a) => Show (Token a) where
  show (Token t) = show t

instance Show Score where
  show (Score (p, r, f)) =
    intercalate "\t" $
      map ((\s -> s "") . showFFloat (Just 3)) [p, r, f]

instance Show Order where
  show (Order k) = show k

instance Show Depth where
  show (Depth d) = show d

-- TODO: Indentation
instance Show EntropyExpr where
  show Term {to, from} =
    "H(" ++ show to ++ "|" ++ intercalate "," (map show from) ++ ")"
  show Sum {scale, positive, negative} =
    "(" ++ show (numerator scale) ++ "/" ++ show (denominator scale) ++ ")[ "
      ++ intercalate " + " (map show positive)
      ++ concatMap ((" - " ++) . show) negative
      ++ " ]"

--------------------------------------------------------------------------------
-- Construction

toAffix :: EntropyExpr -> Affix
toAffix Term {to, from} = Affix $ map (\x -> x - to) from

mkH :: Int -> [Int] -> EntropyExpr
mkH to from = Term {to, from}

decompose :: EntropyExpr -> EntropyExpr
decompose base@Term {from = [x]} = base -- hardcoded to stop at first-order
decompose Term {to = y, from = xs} =
  Sum {scale, positive = map decompose neg, negative = map decompose neg}
  where
    scale = 1 / ((fromIntegral . length) xs + 1)
    tcs = (map (\(x : xs) -> (x, xs)) . permutations) xs
    pos = map (\(_, from) -> Term {to = y, from}) tcs
    neg = map (\(to, from) -> Term {to, from}) tcs

tokenize :: (Monoid a) => Segment a -> Token a
tokenize (Segment ts) = foldr1 (<>) ts

tokenizeString :: String -> [Token String]
tokenizeString = map (\c -> Token [c])

tokenizeChar :: Char -> Token String
tokenizeChar c = Token [c]

toContext :: Window a -> Context a
toContext (Window w) = Context w

toWindow :: Context a -> Window a
toWindow (Context c) = Window c

windows :: Order -> [Token a] -> [Window a]
windows (Order n) ts =
  map Window $
    (foldr (zipWith (:)) (repeat []) . take n . tails) ts

-- Assume Window size is matched to Affix indices and therefore the Token/Context
affixSplit :: Affix -> Window a -> (Token a, Context a)
affixSplit (Affix is) (Window ts) = (token, Context context)
  where
    i = negate $ minimum (0 : is)
    token = ts !! i
    context = deleteAt i ts

prefix :: Order -> Affix
prefix (Order k) = Affix [1 .. k]

suffix :: Order -> Affix
suffix (Order k) = Affix [(- k) .. (-1)]

forward :: Order -> Affix
forward (Order k) = Affix [- k]

increment :: (Eq a, Hashable a) => (Token a, Context a) -> TransitionMap a -> TransitionMap a
increment (tkn, cxt) (TransitionMap' x m) = TransitionMap' x m'
  where
    m' = M.insertWith (M.unionWith (+)) cxt (M.singleton tkn 1) m

countTransitions :: (Eq a, Hashable a) => Affix -> [Window a] -> TransitionMap a
countTransitions affix =
  foldr (increment . affixSplit affix) (TransitionMap' affix M.empty)

transitionMap :: (Eq a, Hashable a) => Affix -> Order -> [Token a] -> TransitionMap a
transitionMap affix (Order k) = countTransitions affix . windows (Order (k + 1))

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

--------------------------------------------------------------------------------
-- Aggregation

--------------------------------------------------------------------------------
-- Segmentation

rise :: (Ord a) => a -> a -> Boundary
rise x y
  | x < y = Boundary
  | otherwise = NoBoundary

entropyBoundary :: (Eq a, Hashable a) => BoundaryPolicy Entropy -> EntropyMap a -> (Context a, Context a) -> Boundary
entropyBoundary (BoundaryPolicy bp) (EntropyMap' x m) (cxtA, cxtB)
  | (cxtA, cxtB) == (Context [], Context []) = Boundary -- Hack
  | otherwise = fromMaybe NoBoundary $
    do
      entropyA <- M.lookup cxtA m
      entropyB <- M.lookup cxtB m
      pure $ entropyA `bp` entropyB

icBoundary :: (Eq a, Hashable a) => BoundaryPolicy InfoContent -> InfoContentMap a -> ((Token a, Context a), (Token a, Context a)) -> Boundary
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

segmentByBoundaryEntropy :: (Eq a, Hashable a, Monoid a) => Order -> [Token a] -> [Segment a]
segmentByBoundaryEntropy k ts =
  entropyFold k (BoundaryPolicy rise) (prefixEntropyMap k ts, suffixEntropyMap k ts) ts

--icFold ::
--  Order ->
--  BoundaryPolicy InfoContent ->
--  (InfoContentMap a, InfoContentMap a) ->
--  [Token a] ->
--  [Segment a]
--icFold (Order k) bp (pm, sm) ts = snd $ foldr f initial (frames (Order (k + 1)) ts)
--  where
--    initial = (Segment [], [])
--    f (Frame (t', suffixWs, prefixWs)) (Segment s, segments) = case isBoundary of
--      Boundary -> (Segment [t'], Segment s : segments)
--      NoBoundary -> (Segment (t' : s), segments)
--      where
--        (pa, pb) = prefixWs
--        (sa, sb) = suffixWs
--        prefixTknCxts = (prefixSplit (toWindow pa), prefixSplit (toWindow pb))
--        suffixTknCxts = (suffixSplit (toWindow sa), suffixSplit (toWindow sb))
--        isPrefixBoundary = icBoundary bp pm prefixTknCxts
--        isSuffixBoundary = icBoundary bp sm suffixTknCxts
--        isBoundary = unionBoundary isPrefixBoundary isSuffixBoundary

--segmentByBoundaryIC :: Order -> [Token a] -> [Segment a]
--segmentByBoundaryIC k ts = icFold k (BoundaryPolicy rise) (pm ts, sm ts) ts
--  where
--    pm = infoContentMap . probabilityMap . prefixTransitionMap k
--    sm = infoContentMap . probabilityMap . suffixTransitionMap k

nestedSegmentation :: (Monoid a) => (Order -> [Token a] -> [Segment a]) -> Depth -> Order -> [Token a] -> [Token a]
nestedSegmentation f (Depth d) k ts
  | d < 1 = ts
  | otherwise = nestedSegmentation f (Depth (d - 1)) k $ map tokenize $ f k ts

nestedEntropy :: (Eq a, Hashable a, Monoid a) => Depth -> Order -> [Token a] -> [Token a]
nestedEntropy = nestedSegmentation segmentByBoundaryEntropy

--nestedInfoContent :: Depth -> Order -> [Token a] -> [Token a]
--nestedInfoContent = nestedSegmentation segmentByBoundaryIC

--------------------------------------------------------------------------------
-- Input

---- TODO: Correct?
cmpOrd :: (Eq a, Hashable a) => EntropyMap a -> EntropyMap a -> Double
cmpOrd (EntropyMap' _ xm) (EntropyMap' _ ym) = less / less_total
  where
    total = sum [1 | xa <- M.keys xm, xb <- M.keys xm, xm ! xa /= xm ! xb]
    similar =
      sum
        [ 1 | xa <- M.keys xm, xb <- M.keys xm, (xm ! xa < xm ! xb && ym ! xa < ym ! xb)
                                                  || (xm ! xa > xm ! xb && ym ! xa > ym ! xb)
        ]
    less =
      sum
        [ 1 | xa <- M.keys xm, xb <- M.keys xm, xm ! xa < xm ! xb, ym ! xa < ym ! xb
        ]
    less_total =
      sum
        [ 1 | xa <- M.keys xm, xb <- M.keys xm, xm ! xa < xm ! xb
        ]

-- TODO: Implement LA estimate for higher-order estimation
-- TODO: Test 3rd-order estimation against Markov estimation (Higher-orders?)

difference :: FileName -> IO ()
difference (FileName filename) = do
  text <- readFile filename
  let contents = unmarked text
      (EntropyMap' _ sm) = suffixEntropyMap (Order 1) contents
      (EntropyMap' _ pm) = prefixEntropyMap (Order 1) contents
      (EntropyMap' _ fm) = forwardEntropyMap (Order 2) contents
      (FrequencyMap' cm) = frequencyMap (Order 3) contents
      (EntropyMap' _ hm) = suffixEntropyMap (Order 2) contents

      -- H(D|BC) - H(C|AB) approximately equals:
      --  = H(D|C) - H(C|B)
      --  = (1/3)*[ H(D|B) - H(C|A) + H(D|C) - H(C|B)
      --          + H(B|A) - H(C|B) + H(A|B) - H(C|B) ]
      --  = (1/3)*[ H(D|C) - H(C|B) + H(A|B) - H(B|C) ]

      hmd' =
        M.fromList
          [ ( Context (ksa ++ ksb ++ ksc),
              (1 / 3) * (fb - fa + sc - sb + sa - sb + pb - pc)
            )
            | (Context kfa, fa) <- M.toList fm,
              (Context kfb, fb) <- M.toList fm,
              (Context ksc, sc) <- M.toList sm,
              (Context ksb, sb) <- M.toList sm,
              (Context ksa, sa) <- M.toList sm,
              kfa == ksa,
              kfb == ksb,
              (Context kpb, pb) <- M.toList pm,
              ksb == kpb,
              (Context kpc, pc) <- M.toList pm
          ]
      hm'' =
        M.fromList
          [ (Context (ksa ++ ksb), sb)
            | (Context ksb, sb) <- M.toList sm,
              (Context ksa, sa) <- M.toList sm
          ]
      hmd'' =
        M.fromList
          [ ( Context (ksa ++ ksb ++ ksc),
              sc - sb
            )
            | --                            (1/2) * (sc - sb + pb - pc)) |
              --                           (1 / 3) * (sc - sb + pb - pc)) | -- replacing fa,fb with sa,sb
              (Context ksc, sc) <- M.toList sm,
              (Context ksb, sb) <- M.toList sm,
              (Context kpb, pb) <- M.toList pm,
              ksb == kpb,
              (Context kpc, pc) <- M.toList pm,
              ksc == kpc,
              (Context ksa, sa) <- M.toList sm
          ]
      hm' =
        M.fromList
          [ (Context (ksa ++ ksb), (1 / 3) * (sb + fa - pb - sa) + 2.8675)
            | (Context ksb, sb) <- M.toList sm,
              (Context kpb, pb) <- M.toList pm,
              ksb == kpb,
              (Context kfa, fa) <- M.toList fm,
              (Context ksa, sa) <- M.toList sm,
              kfa == ksa
          ]
      hm'd =
        M.fromList
          [ (Context (a : b : c), bc - ab)
            | (Context (b : c), bc) <- M.toList hm',
              (Context (a : [b']), ab) <- M.toList hm',
              b == b'
          ]
      hmd =
        M.fromList
          [ (Context (a : b : c), bc - ab)
            | (Context (b : c), bc) <- M.toList hm,
              (Context (a : [b']), ab) <- M.toList hm,
              b == b'
          ]
      dm = M.intersectionWith (-) hmd hmd'
      sem = M.map (^ 2) dm
      aem = M.map abs dm
      mse = sum $ M.elems $ M.intersectionWith (*) dm (M.map (\(Probability p) -> Entropy p) cm)
      rms = sqrt $ sum $ M.elems $ M.intersectionWith (*) sem (M.map (\(Probability p) -> Entropy p) cm)
      mae = sum $ M.elems $ M.intersectionWith (*) aem (M.map (\(Probability p) -> Entropy p) cm)
      o = cmpOrd (EntropyMap' (Affix []) hm) (EntropyMap' (Affix []) hm')
  putStrLn $ "MS Error: " ++ show mse
  putStrLn $ "MA Error: " ++ show mae
  putStrLn $ "RMS Error: " ++ show rms

--  putStrLn $ "Ordering: " ++ show o
--  putStrLn $ show $ EntropyMap dm

--inspiration :: FileName -> IO ()
--inspiration filename = do
--  text <- readFile filename
--  let contents = unmarked text
--      (EntropyMap sm) = suffixEntropyMap (Order 1) contents
--      (EntropyMap pm) = prefixEntropyMap (Order 1) contents
--      (EntropyMap fm) = forwardEntropyMap (Order 2) contents
--      (EntropyMap hm) = suffixEntropyMap (Order 2) contents
--      (FrequencyMap cm) = frequencyMap (Order 2) contents
--      -- set bias to MS error of no bias
--      -- Siddhartha: 2.8675
--      -- Moby Dick: 3.0452
--      -- Ensemble: 3.0553
--      -- Little Women: 3.0214
--      hm'' = M.fromList [(Context (ksa ++ ksb), (1/3) * (sb + fa - pb - sa) + 0) |
--                        (Context ksb, sb) <- M.toList sm,
--                        (Context kpb, pb) <- M.toList pm,
--                        ksb == kpb,
--                        (Context kfa, fa) <- M.toList fm,
--                        (Context ksa, sa) <- M.toList sm,
--                        kfa == ksa]
--      hm' = M.intersection hm'' hm
--      dm = M.intersectionWith (-) hm hm'
--      sem = M.map (^2) dm
--      aem = M.map abs dm
--      mse = sum $ M.elems $ M.intersectionWith (*) dm cm
--      rms = sqrt $ sum $ M.elems $ M.intersectionWith (*) sem cm
--      mae = sum $ M.elems $ M.intersectionWith (*) aem cm
--      sim = cmpOrd (EntropyMap hm) (EntropyMap hm'')
----  putStrLn $ "MS Error: " ++ show mse
----  putStrLn $ "RMS Error: " ++ show rms
----  putStrLn $ "MA Error: " ++ show mae
--  putStrLn $ "Ordering: " ++ show sim
----  putStrLn $ show (EntropyMap hm)
----  putStrLn ""
----  putStrLn $ show (EntropyMap hm')
----  putStrLn $ show (EntropyMap dm)

--standardMapOf :: Order -> String -> IO ()
--standardMapOf k filename = do
--  text <- readFile filename
--  let contents = unmarked text
--      fm = frequencyMap k contents
--      pm = prefixEntropyMap k contents
--      sm = suffixEntropyMap k contents
--      im = infixEntropyMap pm sm
--      standard = standardMap im fm
--  putStrLn "Prefix Entropy Map"
--  putStrLn $ show pm
--  putStrLn "Suffix Entropy Map"
--  putStrLn $ show sm
--  putStrLn "Standard Infix Entropy Map"
--  putStrLn $ show standard

printUsingFile :: (String -> String) -> String -> IO ()
printUsingFile f filename = do
  text <- readFile filename
  printUsingString f text

printUsingString :: (String -> String) -> String -> IO ()
printUsingString f s = do
  print $ f s

qualityForDepthsOrders :: String -> String
qualityForDepthsOrders text = unlines $ do
  d <- map Depth [1 .. 3]
  k <- map Order [1 .. 3]
  let ground = groundTruth text
      segments = nestedEntropy d k (unmarked text)
  return $
    show d ++ "\t" ++ show k ++ "\t" ++ show (quality ground segments)

segmentationWith :: ([Token String] -> [Token String]) -> String -> [Token String]
segmentationWith f text = f (unmarked text)

nestedEntropyText :: Depth -> Order -> String -> [Token String]
nestedEntropyText d k = segmentationWith (nestedEntropy d k)

--nestedInfoContentText :: Depth -> Order -> String -> [Token String]
--nestedInfoContentText d k = segmentationWith (nestedInfoContent d k)

runMultiple :: FileName -> IO ()
runMultiple (FileName s) = do
  printUsingFile qualityForDepthsOrders s

runSegmentation :: FileName -> IO ()
runSegmentation (FileName s) = do
  printUsingFile (show . take 100 . nestedEntropyText (Depth 1) (Order 1)) s

--------------------------------------------------------------------------------
-- Ground Truth

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
