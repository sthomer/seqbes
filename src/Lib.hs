{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}

module Lib where

--------------------------------------------------------------------------------
-- Corpora:
--   https://wortschatz.uni-leipzig.de/en/download/

--------------------------------------------------------------------------------
-- Imports

import Data.Char (isAscii, isLetter, isSpace, toLower)
import qualified Data.HashMap.Strict as M
import Data.Hashable (Hashable)
import Data.List (elemIndex, inits, intercalate, intersect, nub, sort, sortOn, tails, union, (\\), foldl', transpose, unzip4)
import Data.Maybe (fromJust, fromMaybe)
import Data.Ratio (denominator, numerator, (%))
import qualified Data.Set as S
import Numeric (showFFloat)
import System.Random
import Data.List.Index (deleteAt)

--------------------------------------------------------------------------------
-- Types

newtype FileName = FileName String

-- General Context Models

newtype Token a = Token a deriving (Eq, Hashable)

newtype Context a = Context {unContext :: [Token a]} deriving (Eq, Hashable)

newtype Order = Order Int deriving (Num)

newtype Affix = Affix {unAffix :: [Int]}
  deriving (Show, Eq, Hashable) -- e.g. H(B|ACDE) = [-1,+1,+2,+3]

type ContextMap a b = M.HashMap (Context a) b

newtype ShowContextMap a b = ShowContextMap (ContextMap a b)

type ContextTokenMap a b = ContextMap a (M.HashMap (Token a) b)

newtype ShowContextTokenMap a b = ShowContextTokenMap (ContextTokenMap a b)

-- Specific Context Models

newtype Window a = Window [Token a]

newtype Transition = Transition {unTransition :: Int}
  deriving (Num, Eq, Ord, Real, Show)

type Transitions a = ContextTokenMap a Transition

newtype Probability = Probability {unProbability :: Double}
  deriving (Num, Eq, Ord, Real, Fractional, RealFrac, Floating, RealFloat)

type Frequencies a = ContextMap a Probability

type Probabilities a = ContextTokenMap a Probability

newtype Entropy = Entropy Double
  deriving (Num, Eq, Ord, Real, Fractional, Floating, RealFrac, RealFloat)

type Entropies a = ContextMap a Entropy

type InfoContents a = ContextTokenMap a Entropy

-- Segmentation

newtype Score = Score (Double, Double, Double)

data Boundary = Boundary | NoBoundary

newtype BoundaryPolicy = BoundaryPolicy (Entropy -> Entropy -> Boundary)

newtype Segment a = Segment [Token a]

newtype Frame a = Frame (Token a, (Context a, Context a), (Context a, Context a))

newtype Depth = Depth Int

-- Linear Combination Entropy

-- 1-based index
newtype EntropyTerm = EntropyTerm (Rational, Int, [Int]) deriving (Eq, Hashable)

data EntropySum = EntropySum (Rational, [EntropyTerm]) (Rational, [EntropyTerm])

-- Joint Distribution

-- Requires finite alphabet and finite context length
type Joint a = ContextMap a Probability

--------------------------------------------------------------------------------
-- Instances

instance (Show a) => Show (Token a) where
  show (Token t) = show t

instance (Monoid a) => Semigroup (Token a) where
  (Token x) <> (Token y) = Token $ x <> y

instance (Monoid a) => Monoid (Token a) where
  mempty = Token mempty

instance (Show a, Monoid a) => Show (Context a) where
  show (Context ts) = show (foldr1 (<>) ts)

instance (Monoid a) => Semigroup (Context a) where
  (Context x) <> (Context y) = Context $ x <> y

instance (Monoid a) => Monoid (Context a) where
  mempty = Context mempty

instance Show Order where
  show (Order k) = show k

instance Show Probability where
  show (Probability p) = show p

instance Show Entropy where
  show (Entropy p) = show p

instance Show EntropyTerm where
  show (EntropyTerm (s, y, [])) = showScale s ++ "H(" ++ show y ++ ")"
  show (EntropyTerm (s, y, xs)) = showScale s ++
    "H(" ++ show y ++ "|" ++ intercalate "," (map show xs) ++ ")"

instance Show EntropySum where
  show (EntropySum p n) = showPair p ++ " -\n" ++ showPair n
    where
      showPair (_, []) = ""
      showPair (1, xs) = showSum xs
      showPair (sx, xs) = showScale sx ++ showSum xs
      showSum xs = "[ " ++ intercalate " + " (map show xs) ++ " ]"

instance (Show a) => Show (Window a) where
  show (Window w) = concatMap show w

instance Show Score where
  show (Score (p, r, f)) =
    intercalate "\t" $
      map ((\s -> s "") . showFFloat (Just 3)) [p, r, f]

instance (Show a) => Show (Segment a) where
  show (Segment s) = concatMap show s

instance Semigroup (Segment a) where
  (Segment xs) <> (Segment ys) = Segment $ xs ++ ys

instance Monoid (Segment a) where
  mempty = Segment []

instance Show Depth where
  show (Depth d) = show d

instance (Show a, Monoid a, RealFloat b) => Show (ShowContextMap a b) where
  show (ShowContextMap m) = intercalate "\n" s
    where
      s = map showEntry $ sortOn snd (M.toList m)
      showEntry (t, n) = show t ++ ": " ++ showFFloat (Just 5) n ""

instance (Show a, Monoid a, Show b) => Show (ShowContextTokenMap a b) where
  show (ShowContextTokenMap m) = intercalate "\n" $ map showContexts (M.toList m)
    where
      showContexts (c, ts) = show c ++ ":\n" ++ showTokens ts
      showTokens = intercalate "\n" . map showEntry . M.toList
      showEntry (t, n) = "  " ++ show t ++ ": " ++ show n

--------------------------------------------------------------------------------
-- Linear Combination Entropy

toEntropySum :: Order -> EntropyTerm -> EntropySum
toEntropySum (Order k) (EntropyTerm (s, y, xs))
  | length xs <= k = EntropySum (1, [EntropyTerm (s, y, xs)]) (1, [])
  | otherwise = EntropySum (s * toRational sp, ps) (s * toRational sn, ns)
  where
    xks = subsets k xs
    ps = map (\xk -> EntropyTerm (s, y, xk)) xks
    ns = [EntropyTerm (s, x', xk) | x' <- xs, xk <- xks, x' `notElem` xk]
    n = length xs
    sp = product [1 .. max 1 (n - k)] % product [(k + 2) .. (n + 1)]
    sn = product [1 .. max 1 (n - k -1)] % product [(k + 2) .. (n + 1)]

-- Assume EntropyTerm is a suffix entropy with no scale
toSegBias :: EntropyTerm -> (EntropySum, EntropySum)
toSegBias (EntropyTerm (_, y, xs)) = (seg, bias)
  where
    n = length xs
    s' = toRational $ 2 % product [n - 1 .. n + 1]
    segpos = [EntropyTerm (toRational (i - 1), y, [xs !! (i-1)]) | i <- [2..n]]
    segneg = [EntropyTerm (1, xs !! (j-1), [xs !! (i-1)]) | j <- [2..n], i <- [1..j-1]]
    biaspos = [EntropyTerm (toRational (n - i), y, [xs !! (i-1)]) | i <- [1..n-1]]
    biasneg = [EntropyTerm (1, xs !! (i-1), [xs !! (j-1)]) | j <- [2..n], i <- [1..j-1]]
    seg = EntropySum (s', segpos) (s', segneg)
    bias = EntropySum (s', biaspos) (s', biasneg)

subsets :: (Ord a) => Int -> [a] -> [[a]]
subsets k =
  S.toList . S.map S.toList . S.filter ((== k) . length) . S.powerSet . S.fromList

shorten :: EntropySum -> EntropySum
shorten (EntropySum (sp, ps) (sn, ns)) = EntropySum (sn, ps'') (sn, ns'')
  where
    ratio = sp / sn
    inflate (EntropyTerm (s, y, xs)) = EntropyTerm (ratio * s, y, xs)
    spread (EntropyTerm (s, y, xs)) = replicate ((fromIntegral . numerator) s) (mkH y xs)
    ps' = concatMap (spread . inflate . shortenTerm) ps
    ns' = concatMap (spread . shortenTerm) ns
    ps'' = combineTerms $ ps' \\ ns'
    ns'' = combineTerms ns' \\ ps'

shortenTerm :: EntropyTerm -> EntropyTerm
shortenTerm (EntropyTerm (s, y, [x]))
  | y < x = EntropyTerm (s, y, [y + 1])
  | y > x = EntropyTerm (s, y, [y - 1])

-- Assume equal scales
-- TODO: Don't assume equal scales
-- TODO: Ensure numerator is integral when replicating for spread
minus :: EntropySum -> EntropySum -> EntropySum
minus (EntropySum (xsp, xps) (xsn, xns)) (EntropySum (_, yps) (_, yns))
  = EntropySum (xsn, zps3') (xsn, zns3')
  where
    ratio = xsp / xsn
    inflate (EntropyTerm (s, y, xs)) = EntropyTerm (ratio * s, y, xs)
    spread (EntropyTerm (s, y, xs)) = replicate ((fromIntegral . numerator) s) (mkH y xs)
    zps = map inflate xps ++ yns
    zns = xns ++ map inflate yps
    zps' = concatMap spread zps
    zns' = concatMap spread zns
    zps'' = zps' \\ zns'
    zns'' = zns' \\ zps'
    zps3' = combineTerms zps''
    zns3' = combineTerms zns''

combineTerms :: [EntropyTerm] -> [EntropyTerm]
combineTerms xs = cs
  where
    groups = nub $ map (\x -> filter (\y -> to x == to y && from x == from y) xs) xs
    cs = map (\(g:gs) -> EntropyTerm (foldr ((+) . scale) 0 (g : gs), to g, from g)) groups

combine :: EntropyTerm -> EntropyTerm -> EntropyTerm
combine (EntropyTerm (sx, xto, xfrom)) (EntropyTerm (sy, yto, yfrom))
  | sx /= sy = error "Unequal scales"
  | xto `elem` yfrom = mkH yto (xfrom `union` yfrom)
  | yto `elem` xfrom = mkH xto (xfrom `union` yfrom)
  | otherwise = error "Currently unrepresentable"

affixOrder :: Affix -> Order
affixOrder (Affix axs) = Order $ maximum (0 : axs) - minimum (0 : axs)

prefix :: Order -> Affix
prefix (Order k) = Affix [1 .. k]

suffix :: Order -> Affix
suffix (Order k) = Affix [(- k) .. (-1)]

forward :: Order -> Affix
forward (Order k) = Affix [- k]

backward :: Order -> Affix
backward (Order k) = Affix [k]

toAffix :: EntropyTerm -> Affix
toAffix (EntropyTerm (_, to, from)) = Affix $ map (\x -> x - to) from

toAffixes :: EntropySum -> [Affix]
toAffixes (EntropySum (_, ps) (_, ns)) = map toAffix ps ++ map toAffix ns

termOrder :: EntropyTerm -> Order
termOrder = affixOrder . toAffix

scale :: EntropyTerm -> Rational
scale (EntropyTerm (s, _, _)) = s

to :: EntropyTerm -> Int
to (EntropyTerm (_, x, _)) = x

from :: EntropyTerm -> [Int]
from (EntropyTerm (_, _, xs)) = xs

range :: EntropySum -> [Int]
range (EntropySum (_, ps) (_, ns)) = sort $ nub $ concatMap from (ps ++ ns)

infoContent :: Probability -> Entropy
infoContent (Probability p)
  | p == 0 = Entropy 0
  | p == 1 = Entropy 0
  | otherwise = Entropy $ (negate . logBase 2) p

entropy :: [Probability] -> Entropy
entropy = sum . map weight
  where
    weight p = Entropy (unProbability p) * infoContent p

toWindow :: Context a -> Window a
toWindow (Context c) = Window c

showScale :: Rational -> String
showScale s
  | s == 1 = ""
  | numerator s /= 1 && denominator s == 1 = show (numerator s)
  | otherwise = "(" ++ show (numerator s) ++ "/" ++ show (denominator s) ++ ")"

p2e :: Probability -> Entropy
p2e (Probability p) = Entropy p

states :: Int -> Int -> [Context String]
states n k = map Context cs
  where
    xs = map (Token . (: [])) (take n ['a'..'z'])
    repeatFor k' = concat . replicate (n^(k-k'-1)) . concatMap (replicate (n^k'))
    cs = transpose [ repeatFor k' xs | k' <- [0..k-1]]

mkJoint :: Int -> Int -> Int -> Joint String
mkJoint seed n k = M.fromList $ zip contexts probs
  where
    contexts = states n k
    weights = take (length contexts) $ randoms (mkStdGen seed)
    weights' = map (1/) weights
--    weights' = replicate (length contexts) (1 / (fromIntegral n^k))
    total = sum weights'
    probs = map (Probability . (/ total)) weights'

-- TODO: make sure margin index is within list range
marginal :: (Eq a, Hashable a) => Int -> Joint a -> Joint a
marginal n joint = M.fromListWith (+) $ map removeAt $ M.toList joint
  where
    removeAt (Context k, v) = (Context $ deleteAt (n-1) k, v)

marginals :: (Eq a, Hashable a) => [Int] -> Joint a -> Joint a
marginals [] joint = joint
marginals (n:ns) joint = marginals (map pred ns) (marginal n joint)

conditional :: (Eq a, Hashable a) => Int -> [Int] -> Joint a -> Joint a
conditional y xs joint = M.fromList divs
  where
    js = [1..(length . unContext . fst . head . M.toList) joint]
    top = marginals (sort $ js \\ (y:xs)) joint
    bottom = marginals (sort $ js \\ xs) joint
    y' = length $ filter (< y) xs
    divs = [(Context ka, va / vb) |
            (Context ka, va) <- M.toList top,
            (Context kb, vb) <- M.toList bottom,
            kb == deleteAt y' ka]

condLimits :: FileName -> IO ()
condLimits (FileName output) = do
  writeFile output $ intercalate ","
    ["alphabet", "seed", "low", "mid", "high"] ++ "\n"
  appendFile output $ unlines $ do
    n <- [2..15]
    seed <- [1..100]
    let
      pABC = mkJoint seed n 3
      pAB = marginal 3 pABC
      pAC = marginal 2 pABC
      pBC = marginal 1 pABC
      pCgAB = conditional 3 [1,2] pABC
      pCgA = conditional 3 [1] pABC
      pCgB = conditional 3 [2] pABC
      pAgB = conditional 1 [2] pABC
      pBgA = conditional 2 [1] pABC
      (Entropy hCgAB) = sum [(Entropy pabc) * infoContent (pCgAB M.! context)
        | (context, Probability pabc) <- M.toList pABC]
      (Entropy hCgA) = sum [(Entropy pac) * infoContent (pCgA M.! context)
        | (context, Probability pac) <- M.toList pAC]
      (Entropy hCgB) = sum [(Entropy pbc) * infoContent (pCgB M.! context)
        | (context, Probability pbc) <- M.toList pBC]
      (Entropy hAgB) = sum [(Entropy pab) * infoContent (pAgB M.! context)
        | (context, Probability pab) <- M.toList pAB]
      (Entropy hBgA) = sum [(Entropy pab) * infoContent (pBgA M.! context)
        | (context, Probability pab) <- M.toList pAB]
      (Entropy hAB) = entropy $ M.elems pAB
      total = hCgA + hCgB - hAgB - hBgA
      low = total / 3
      high = (total + hAB)/2
    return $ intercalate "," $
      map show [n, seed] ++ map show [low, hCgAB, high]

limits :: FileName -> IO ()
limits (FileName output) = do
  writeFile output $ intercalate ","
    ["alphabet", "seed", "low", "mid", "high"] ++ "\n"
  appendFile output $ unlines $ do
    n <- [2..15]
    seed <- [1..100]
    let
      pABC = mkJoint seed n 3
      pAB = marginal 3 pABC
      pAC = marginal 2 pABC
      pBC = marginal 1 pABC
      (Entropy hABC) = entropy $ M.elems pABC
      (Entropy hAB) = entropy $ M.elems pAB
      (Entropy hAC) = entropy $ M.elems pAC
      (Entropy hBC) = entropy $ M.elems pBC
      total = hAB + hAC + hBC
    return $ intercalate "," $
      map show [n, seed] ++ map show [total/3, hABC, total/2]

agreement :: FileName -> IO ()
agreement (FileName output) = do
  writeFile output $ intercalate ","
    ["alphabet", "seed", "agree", "match"] ++ "\n"
  appendFile output $ unlines $ do
    n <- [1..30]
    seed <- [1..100]
    let
      pABC = mkJoint seed n 3
      pAB = marginal 3 pABC
      pAC = marginal 2 pABC
      pBC = marginal 1 pABC
      total = M.fromList [( Context [a,b,c],
        infoContent (pAB M.! Context [a,b]) + infoContent (pAC M.! Context [a,c]) + infoContent (pBC M.! Context [b, c]))
        | Context [a, b, c] <- M.keys pABC]
      joint = M.map infoContent pABC
      lower = M.map (Entropy (1/3) *) total
      upper = M.map (Entropy (1/2) *) total
      percent = 1 / (fromIntegral . M.size) pABC
      agree' = sum [if mid <= high then percent else 0
        | c <- M.keys pABC,
          let low = show (lower M.! c),
          let mid = show (joint M.! c),
          let high = show (upper M.! c)]
      agree = sum [if mid <= high then pABC M.! c else 0
        | c <- M.keys pABC,
          let low = show (lower M.! c),
          let mid = show (joint M.! c),
          let high = show (upper M.! c)]
    return $ intercalate "," $
      map show [n, seed] ++ map show [agree, agree']

corrPairs :: FileName -> IO ()
corrPairs (FileName output) = do
  writeFile output $ intercalate ","
    ["alphabet", "seed", "context", "frequency", "low", "mid", "high"] ++ "\n"
  appendFile output $ unlines $ do
    n <- [1..10]
    seed <- [1..100]
    let
      pABC = mkJoint seed n 3
      pAB = marginal 3 pABC
      pAC = marginal 2 pABC
      pBC = marginal 1 pABC
      total = M.fromList [( Context [a,b,c],
        infoContent (pAB M.! Context [a,b]) + infoContent (pAC M.! Context [a,c]) + infoContent (pBC M.! Context [b, c]))
        | Context [a, b, c] <- M.keys pABC]
      joint = M.map infoContent pABC
      lower = M.map (Entropy (1/3) *) total
      upper = M.map (Entropy (1/2) *) total
      es = [lower, joint, upper]
      rows = [show n : show seed : show c : show (pABC M.! c) :
        map (show . (M.! c)) es | c <- M.keys pABC]
    return $ unlines $ map (intercalate ",") rows

ent :: Probability -> Entropy
ent p = p2e p * infoContent p

checkTriangles :: FileName -> IO ()
checkTriangles (FileName output) = do
  writeFile output $ intercalate ","
    ["alphabet", "seed", "error"] ++ "\n"
  appendFile output $ unlines $ do
    n <- [2..20]
    seed <- [1..100]
    let err = checkTriangle seed n
    return $ intercalate "," $
      map show [n, seed] ++ map show [err]

checkTriangle :: Int -> Int -> Double
checkTriangle seed alphabet = mae left right joint
  where
    joint = mkJoint seed alphabet 3
    hAC = termFromJoint (mkH 1 [3]) joint
    hBA = termFromJoint (mkH 2 [1]) joint
    hCB = termFromJoint (mkH 3 [2]) joint
    hCA = termFromJoint (mkH 3 [1]) joint
    hAB = termFromJoint (mkH 1 [2]) joint
    hBC = termFromJoint (mkH 2 [3]) joint
    left = M.fromList [( Context [a,b,c],
      hBA M.! Context [a] + hCB M.! Context [b] + hAC M.! Context [c])
      | Context [a, b, c] <- M.keys joint]
    right = M.fromList [(Context [a,b,c],
      hCA M.! Context [a] + hAB M.! Context [b] + hBC M.! Context [c])
      | Context [a, b, c] <- M.keys joint]

--termFromJoint' :: (Eq a, Hashable a) => Order -> EntropyTerm -> Joint a -> Entropies a
--termFromJoint' (Order k) (EntropyTerm (s, y, xs)) joint = M.fromList e
--  where
--    cond = conditional y xs joint
--    flat = marginals ([1..k] \\ (y:xs)) joint
--    e = [ (c, Entropy p * infoContent (cond M.! c))
--        | (c, Probability p) <- M.toList flat]

termFromJoint :: (Eq a, Hashable a) => EntropyTerm -> Joint a -> Entropies a
termFromJoint (EntropyTerm (s, y, xs)) = sEntropies . conditional y xs
  where
    y' = length $ filter (< y) xs
    removeAt (Context k, v) = (Context $ deleteAt y' k, v)
    sEntropy p = fromRational s * p2e p * infoContent p
    sEntropies = M.fromListWith (+) . map removeAt . M.toList . M.map sEntropy

sumFromJoint :: (Eq a, Hashable a) => EntropySum -> Joint a -> Entropies a
sumFromJoint (EntropySum (sp, ps) (sn, ns)) joint = total
  where
    rangeXs = (nub . concatMap (\(EntropyTerm (_, _, xs)) -> xs)) (ps ++ ns)
    toEntropies = M.fromList . map (\term -> (term, termFromJoint term joint))
    translate (Context cs) (EntropyTerm (_, _, xs)) = Context [cs !! (x - 1) | x <- xs]
    totalEntropy context entropies terms = sum $ map (\term -> (entropies M.! term) M.! translate context term) terms
    posEntropies = toEntropies ps
    negEntropies = toEntropies ns
    total = M.fromList $ do
      Context xs <- M.keys joint
      let
        context = Context [xs !! (x - 1) | x <- rangeXs]
        posTotal = totalEntropy context posEntropies ps
        posScaled = Entropy (fromRational sp) * posTotal
        negTotal = totalEntropy context negEntropies ns
        negScaled = Entropy (fromRational sn) * negTotal
      return (context, posScaled - negScaled)

suffixJoints :: FileName -> IO ()
suffixJoints (FileName output) = do
  writeFile output $ intercalate ","
    ["alphabet", "order", "seed", "decomp", "markov", "seg", "bias"] ++ "\n"
  appendFile output $ unlines $ do
    n <- [2..5]
    k <- [3..5]
    seed <- [1..100]
    let (decomp, markov, seg, bias) = suffixJoint seed n k
    return $ intercalate "," $
      map show [n, k, seed] ++ map show [decomp, markov, seg, bias]

meanSuffixJoint :: Int -> Int -> Int -> (Double, Double)
meanSuffixJoint m n k = (meanDecomp, meanMarkov)
  where
    (decomp, markov, seg, bias) = unzip4 [suffixJoint seed n k | seed <- [1..m]]
    meanDecomp = sum decomp / fromIntegral m
    meanMarkov = sum markov / fromIntegral m

suffixJoint :: Int -> Int -> Int -> (Double, Double, Double, Double)
suffixJoint seed n k = (
    correlation hoEntropies decompEntropies (marginal k joint),
    correlation hoEntropies markovEntropies (marginal k joint),
    correlation hoEntropies segEntropies (marginal k joint),
    correlation hoEntropies biasEntropies (marginal k joint)
  )
  where
    joint = mkJoint seed n k
    hoTerm = mkH k [1..k-1]
    decompTerm = toEntropySum (Order 1) hoTerm
    (seg, bias) = toSegBias hoTerm
    markovTerm = mkH k [k-1]
    hoEntropies = termFromJoint hoTerm joint
    decompEntropies = sumFromJoint decompTerm joint
    segEntropies = sumFromJoint seg joint
    biasEntropies = sumFromJoint bias joint
    markovEntropies' = termFromJoint markovTerm joint
    markovEntropies = M.fromList [(Context cs, markovEntropies' M.! Context [last cs]) | Context cs <- M.keys hoEntropies]

suffixSum :: Int -> Int -> Int -> Double
suffixSum seed n k =
    correlation hoEntropies decompEntropies (marginal (k+1) joint)
--    correlation hoEntropies markovEntropies (marginal (k+1) joint))
  where
    joint = mkJoint seed n (k+1)
    hoTermP = mkH (k+1) [2..k]
    hoTermN = mkH k [1..k-1]
    hoSum = EntropySum (1, [hoTermP]) (1, [hoTermN])
    decompSum = toEntropySum (Order 1) hoTermP `minus` toEntropySum (Order 1) hoTermN
--    markovSum = EntropySum (1, [mkH (k+1) [k]]) (1, [mkH k [k-1]])
    hoEntropies = sumFromJoint hoSum joint
    decompEntropies = sumFromJoint decompSum joint
--    markovEntropies' = sumFromJoint markovSum joint
--    markovEntropies = M.fromList [(Context cs, markovEntropies' M.! Context [last cs]) | Context cs <- M.keys hoEntropies]

--------------------------------------------------------------------------------
-- Construction

-- TODO: Incrementing by iterating through windows is a bottleneck
increment :: (Eq a, Hashable a) => (Token a, Context a) -> Transitions a -> Transitions a
increment (token, context) = M.insertWith (M.unionWith (+)) context (M.singleton token 1)

countTransitions :: (Eq a, Hashable a) => Affix -> [Window a] -> Transitions a
countTransitions affix =
  -- use foldl' because HashMap is strict
  foldl' (flip (increment . affixSplit affix)) M.empty
  -- use foldr if HashMap is lazy
  -- foldlr (increment . affixSplit affix) M.empty

transitions :: (Eq a, Hashable a) => Affix -> Order -> [Token a] -> Transitions a
transitions affix (Order k) = countTransitions affix . windows (Order (k + 1))

-- Alternatively, could go directly from affix to token/context
--  instead of going through window/split
transitionsWith :: (Eq a, Hashable a) => Affix -> [Token a] -> Transitions a
transitionsWith affix = transitions affix (affixOrder affix)

frequencies :: Transitions a -> Frequencies a
frequencies transitions = M.map (/ total) counts
  where
    counts = M.map (realToFrac . sum . M.elems) transitions
    total = (realToFrac . sum . M.elems) counts

probabilities :: Transitions a -> Probabilities a
probabilities = M.map asProbabilities
  where
    asProbabilities context = M.map ((/ total context) . realToFrac) context
    total = realToFrac . sum . M.elems

entropies :: Probabilities a -> Entropies a
entropies = M.map (entropy . M.elems)

infoContents :: Probabilities a -> InfoContents a
infoContents = M.map (M.map infoContent)

-- Assume Window size is matched to Affix indices and the Token/Context
affixSplit :: Affix -> Window a -> (Token a, Context a)
affixSplit (Affix xs) (Window ts) = (token, context)
  where
    t = negate $ minimum (0 : xs)
    token = ts !! t
    context = Context [ts !! (t + x) | x <- xs]

windows :: Order -> [Token a] -> [Window a]
windows (Order n) ts =
  map Window $
    (foldr (zipWith (:)) (repeat []) . take n . tails) ts

entropiesWith :: (Eq a, Hashable a) => Affix -> [Token a] -> Entropies a
entropiesWith affix = entropies . probabilities . transitionsWith affix

frequenciesWith :: (Eq a, Hashable a) => Affix -> [Token a] -> Frequencies a
frequenciesWith affix = frequencies . transitionsWith affix

-- TODO: Only evaluate the differences actually present in the stream
--  instead of evaluating all pairs of overlapping contexts
suffixDiff :: (Eq a, Hashable a) => EntropyTerm -> [Token a] -> Entropies a
suffixDiff term tokens = diffs
  where
    entropies = M.toList $ entropiesWith (toAffix term) tokens
    diffs = M.fromList [
      (Context (head ca : cb), vb - va) |
      (Context ca, va) <- entropies,
      (Context cb, vb) <- entropies,
      tail ca == init cb]

-- TODO: Break this up; It's slow...
differences :: (Eq a, Hashable a) => EntropyTerm -> EntropyTerm -> [Token a] -> Entropies a
differences termA termB tokens = diffs
  where
    aEntropies = M.toList $ entropiesWith (toAffix termA) tokens
    bEntropies = M.toList $ entropiesWith (toAffix termB) tokens
    as = from termA
    bs = from termB
    termIntersection = sort $ as `intersect` bs
    termUnion = sort $ as `union` bs
    tokenAt x c s = c !! fromJust (x `elemIndex` s)
    pick x ca cb
      | x `elem` as = tokenAt x ca as
      | otherwise = tokenAt x cb bs
    diffs = M.fromList [
      (Context [pick x ca cb | x <- termUnion], va - vb) |
      (Context ca, va) <- aEntropies,
      (Context cb, vb) <- bEntropies,
      all (\x -> tokenAt x ca as == tokenAt x cb bs) termIntersection]

mkAt :: (Eq a, Hashable a) => EntropySum -> [Token a] -> Context a -> Entropy
mkAt (EntropySum (sp, ps) (sn, ns)) tokens = at
  where
    posEntropies = termEntropies ps tokens
    negEntropies = termEntropies ns tokens
    at context = Entropy (fromRational sp) * posTotal - Entropy (fromRational sn) * negTotal
      where
        posContexts = termContexts ps context
        negContexts = termContexts ns context
        posTotal = termsTotal posEntropies posContexts ps
        negTotal = termsTotal negEntropies negContexts ns

termsTotal' :: (Eq a, Hashable a)
  => M.HashMap EntropyTerm (Entropies a)
  -> M.HashMap EntropyTerm (Context a)
  -> [EntropyTerm]
  -> Entropy
termsTotal' entropyTerms contextTerms terms = sum [anEntropy term | term <- terms]
  where
    entropyAt term = entropyTerms M.! term
    contextAt term = contextTerms M.! term
    anEntropy term = entropyAt term M.! contextAt term

termsTotal :: (Eq a, Hashable a)
  => M.HashMap Affix (Entropies a)
  -> M.HashMap EntropyTerm (Context a)
  -> [EntropyTerm]
  -> Entropy
termsTotal entropyTerms contextTerms terms =
  sum [ Entropy (fromRational (scale term)) * anEntropy term | term <- terms]
  where
    entropyAt term = entropyTerms M.! toAffix term
    contextAt term = contextTerms M.! term
--    anEntropy term = M.lookupDefault (Entropy 0) (contextAt term) (entropyAt term)
    anEntropy term = entropyAt term M.! contextAt term

termEntropies :: (Eq a, Hashable a)
  => [EntropyTerm]
  -> [Token a]
  -> M.HashMap Affix (Entropies a)
termEntropies terms ts = M.fromList [(affix, entropiesWith affix ts) | affix <- affixes]
  where
    affixes = nub $ map toAffix terms

termContexts :: [EntropyTerm] -> Context a -> M.HashMap EntropyTerm (Context a)
termContexts terms context = M.fromList [(term, toContext term context) | term <- terms]

-- This only works if context is indexed from 1...
toContext :: EntropyTerm -> Context a -> Context a
toContext term (Context c) = Context $ [c !! (i -1) | i <- from term]

mkH :: Int -> [Int] -> EntropyTerm
mkH to from = EntropyTerm (1, to, sort from)

--------------------------------------------------------------------------------
-- Segmentation

rise :: Entropy -> Entropy -> Boundary
rise x y
  | x < y = Boundary
  | otherwise = NoBoundary

entropyBoundary ::
  (Eq a, Hashable a) =>
  BoundaryPolicy ->
  Entropies a ->
  (Context a, Context a) ->
  Boundary
entropyBoundary (BoundaryPolicy bp) entropies (contextA, contextB)
  | (contextA, contextB) == (Context [], Context []) = Boundary -- Hack
  | otherwise = fromMaybe NoBoundary $
    do
      entropyA <- M.lookup contextA entropies
      entropyB <- M.lookup contextB entropies
      pure $ entropyA `bp` entropyB

icBoundary ::
  (Eq a, Hashable a) =>
  BoundaryPolicy ->
  InfoContents a ->
  ((Token a, Context a), (Token a, Context a)) ->
  Boundary
icBoundary (BoundaryPolicy bp) infoContents ((tokenA, contextA), (tokenB, contextB)) =
  fromMaybe NoBoundary $ do
    infoContentsA <- M.lookup contextA infoContents
    infoContentA <- M.lookup tokenA infoContentsA
    infoContentsB <- M.lookup contextB infoContents
    infoContentB <- M.lookup tokenB infoContentsB
    return $ infoContentA `bp` infoContentB

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
    contexts xs = map (\(Window ts) -> Context ts) $ windows k xs

entropyFold ::
  (Eq a, Hashable a, Monoid a) =>
  Order ->
  BoundaryPolicy ->
  (Entropies a, Entropies a) ->
  [Token a] ->
  [Segment a]
entropyFold k bp (pm, sm) ts = snd $ foldr boundaryFrame initial $ frames k ts
  where
    initial = (Segment [], [])
    boundaryFrame (Frame (t, suffixContexts, prefixContexts)) (Segment s, segments) =
      case isBoundary of
        Boundary -> (Segment [t], Segment s : segments)
        NoBoundary -> (Segment (t : s), segments)
      where
        isBoundary = unionBoundary isPrefixBoundary isSuffixBoundary
        isPrefixBoundary = entropyBoundary bp pm prefixContexts
        isSuffixBoundary = entropyBoundary bp sm suffixContexts

segmentByBoundaryEntropy ::
  (Eq a, Hashable a, Monoid a) =>
  Order ->
  [Token a] ->
  [Segment a]
segmentByBoundaryEntropy k ts =
  entropyFold k (BoundaryPolicy rise) (prefixEntropies k ts, suffixEntropies k ts) ts

nestedSegmentation ::
  (Monoid a) =>
  (Order -> [Token a] -> [Segment a]) ->
  Depth ->
  Order ->
  [Token a] ->
  [Token a]
nestedSegmentation f (Depth d) k ts
  | d < 1 = ts
  | otherwise = nestedSegmentation f (Depth (d - 1)) k $ map tokenize $ f k ts

nestedEntropy ::
  (Eq a, Hashable a, Monoid a) =>
  Depth ->
  Order ->
  [Token a] ->
  [Token a]
nestedEntropy = nestedSegmentation segmentByBoundaryEntropy

tokenize :: (Monoid a) => Segment a -> Token a
tokenize (Segment ts) = foldr1 (<>) ts

prefixEntropies :: (Eq a, Hashable a) => Order -> [Token a] -> Entropies a
prefixEntropies k = entropies . probabilities . transitions (prefix k) k

suffixEntropies :: (Eq a, Hashable a) => Order -> [Token a] -> Entropies a
suffixEntropies k = entropies . probabilities . transitions (suffix k) k

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

zeroOnes :: Int -> Int -> [Token String]
zeroOnes high n = map (Token . show) $ take n $ randomRs (0, high - 1) (mkStdGen n)

groundTruth :: String -> [Token String]
groundTruth =
  map Token . words . map toLower . filter ((||) <$> isLetter <*> isSpace) . filter isAscii

-- Since we're comparing sets, the direction of the scan doesn't actually matter
-- Therefore, indices are counted from the end of the list to the front
boundaryIndices :: [Token [a]] -> S.Set Int
boundaryIndices ts =
  S.fromDistinctDescList $
    (tail . scanr1 (+) . map (\(Token t) -> (fromIntegral . length) t)) ts

quality :: [Token [a]] -> [Token [a]] -> Score
quality source target = Score (p, r, f)
  where
    correct = S.size $ boundaryIndices source `S.intersection` boundaryIndices target
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

expectation :: (Eq a, Hashable a, Real b) => ContextMap a b -> Frequencies a -> Double
expectation contextMap frequencies = sum $ M.elems weightedMap
  where
    weightedMap = M.intersectionWith (*) fs cs
    fs = M.map unProbability frequencies
    cs = M.map realToFrac contextMap

variance :: (Eq a, Hashable a, Real b, Fractional b) => ContextMap a b -> Frequencies a -> Double
variance contextMap = covariance contextMap contextMap

covariance :: (Eq a, Hashable a, Real b, Fractional b) => ContextMap a b -> ContextMap a b -> Frequencies a -> Double
covariance cmA cmB frequencies = expectation diffs frequencies
  where
    meanA =  realToFrac $ expectation cmA frequencies
    meanB = realToFrac $ expectation cmB frequencies
    diffA = M.map (\v -> v - meanA) cmA
    diffB = M.map (\v -> v - meanB) cmB
    diffs = M.intersectionWith (*) diffA diffB

correlation :: (Eq a, Hashable a, Real b, Fractional b) =>
  ContextMap a b -> ContextMap a b -> Frequencies a -> Double
correlation cmA cmB frequencies = cov / (stdA * stdB)
  where
    cov = covariance cmA cmB frequencies
    stdA = sqrt $ variance cmA frequencies
    stdB = sqrt $ variance cmB frequencies

rmse :: (Eq a, Hashable a, Real b)
  => ContextMap a b
  -> ContextMap a b
  -> Frequencies a
  -> Double
rmse xm ym = sqrt . expectation (M.map (^ 2) (M.intersectionWith (-) xm ym))

mae :: (Eq a, Hashable a, Real b)
  => ContextMap a b
  -> ContextMap a b
  -> Frequencies a
  -> Double
mae xm ym = expectation (M.map abs (M.intersectionWith (-) xm ym))

msd :: (Eq a, Hashable a, Real b)
  => ContextMap a b
  -> ContextMap a b
  -> Frequencies a
  -> Double
msd xm ym = expectation (M.intersectionWith (-) xm ym)

rsd :: (Eq a, Hashable a, Real b)
  => ContextMap a b
  -> ContextMap a b
  -> Frequencies a
  -> Double
rsd xm ym = expectation $ M.map signum $ M.intersectionWith (-) xm ym

sign :: (Eq a, Hashable a, Real b)
  => ContextMap a b
  -> ContextMap a b
  -> Frequencies a
  -> Double
sign xm ym = expectation (M.intersectionWith f xm ym)
  where
    f x y
      | x > 0 && y > 0 = 0
      | x < 0 && y < 0 = 0
      | x == 0 && y == 0 = 0
      | otherwise = 1

rank :: (Eq a, Hashable a, Ord b, Num b)
  => [Context a]
  -> ContextMap a b
  -> ContextMap a b
  -> Double
rank (c:cs) xm ym = agree / total
  where
    pairs = zip (c:cs) cs
    total = (fromIntegral . length) pairs
    agree = (fromIntegral . length) [ 1 | (x, y) <- pairs,
                         (xm M.! x > xm M.! y && ym M.! x > ym M.! y) ||
                         (xm M.! x < xm M.! y && ym M.! x < ym M.! y) ||
                         (xm M.! x == xm M.! y && ym M.! x == ym M.! y) ]

homms :: FileName -> IO ()
homms (FileName filename) = do
  text <- readFile filename
  putStrLn $ intercalate "\t"
    ["Src-n", "Tgt-k", "RMSE-M", "RMSE-D", "Corr-M", "Corr-D", "Rank-M", "Rank-D"]
  putStrLn $ unlines $ do
    n <- [2..5]
    k <- [1..n-1]
    return $ show n ++ "\t" ++ show k ++ "\t" ++
      homm (unmarked text) (mkH (n+1) [1..n]) (Order k)

-- Only use suffix entropies (for now)
-- Crossover b/w Decomp and Markov at about 4-context or 5-context
-- Shortened model almost always nearly as good as full decomposed model
homm :: (Eq a, Hashable a) => [Token a] -> EntropyTerm -> Order -> String
homm ts term order@(Order k) =
  let contexts = map (\(Window ts) -> Context ts) (windows (affixOrder (toAffix term)) ts)
      hoEntropies = entropiesWith (toAffix term) ts
      hoFrequencies = frequenciesWith (toAffix term) ts
      -- Decomposed Model
      loTerm = toEntropySum order term
      atLo = mkAt loTerm ts
      loEntropies = M.mapWithKey (\context _ -> atLo context) hoEntropies
      rmsErrorLo = rmse hoEntropies loEntropies hoFrequencies
      corrLo = correlation hoEntropies loEntropies hoFrequencies
      rankLo = rank contexts hoEntropies loEntropies
      -- Shortened Model (only works when k=1)
--      soTerm = shorten loTerm
--      atSo = mkAt soTerm ts
--      soEntropies = M.mapWithKey (\context _ -> atSo context) hoEntropies
--      rmsErrorSo = rmse hoEntropies soEntropies hoFrequencies
      -- Markov Assumption Model
      moTerm = mkH (to term) [to term - k..to term - 1] -- only makes sense for suffix entropy
      moEntropies = entropiesWith (toAffix moTerm) ts
      moEntropies' =
        M.fromList
          [ (Context c, moEntropies M.! Context (reverse (take k (reverse c))))
            | (Context c, _) <- M.toList hoEntropies
          ]
      rmsErrorMo = rmse hoEntropies moEntropies' hoFrequencies
      corrMo = correlation hoEntropies moEntropies' hoFrequencies
      rankMo = rank contexts hoEntropies moEntropies'
  in  showFFloat (Just 3) rmsErrorMo "\t"
      ++ showFFloat (Just 3) rmsErrorLo "\t"
      ++ showFFloat (Just 3) corrMo "\t"
      ++ showFFloat (Just 3) corrLo "\t"
      ++ showFFloat (Just 3) rankMo "\t"
      ++ showFFloat (Just 3) rankLo "\t"
--      ++ "\t" ++ showFFloat (Just 3) rmsErrorSo ""
--  putStrLn $ "Markov: " ++ showFFloat (Just 3) rmsError' ""
--  putStrLn $ "Decomp: " ++ showFFloat (Just 3) rmsErrorLo ""
--  putStrLn $ "Short1: " ++ showFFloat (Just 3) rmsErrorSo ""

-- Only use suffix entropies (for now)
suffixDiffs :: (Eq a, Hashable a) => [Token a] -> Order -> Order -> String
suffixDiffs contents (Order higher) order =
  let term = mkH (higher + 2) [2..higher + 1]
      term' = mkH (higher + 1) [1..higher]
      -- Higher-order model differences
      hoDiff = suffixDiff term contents
      hoFrequencies = frequenciesWith (toAffix (combine term term')) contents
      -- Decomposed model differences
      loTerm = toEntropySum order term `minus` toEntropySum order term'
      at = mkAt loTerm contents
      loDiff = M.mapWithKey (\context _ -> at context) hoDiff
      rmsErrorLo = rmse hoDiff loDiff hoFrequencies
      signErrorLo = sign hoDiff loDiff hoFrequencies
      -- Shortened Model (only works when k=1)
      soTerm = shorten loTerm
      atSo = mkAt soTerm contents
      soEntropies = M.mapWithKey (\context _ -> atSo context) hoDiff
      rmsErrorSo = rmse hoDiff soEntropies hoFrequencies
      signErrorSo = sign hoDiff loDiff hoFrequencies
      -- Markov Assumption Model
      markovTerm = mkH 3 [2]
      moDiff = suffixDiff markovTerm contents
      moDiff' = M.fromList [ (Context c, moDiff M.! Context ((\(b:a:_) -> [a,b]) (reverse c)))
                            | (Context c, _) <- M.toList hoDiff]
      rmsErrorMo = rmse hoDiff moDiff' hoFrequencies
      signErrorMo = sign hoDiff moDiff' hoFrequencies
  in  showFFloat (Just 3) rmsErrorMo "" ++ "\t" ++ showFFloat (Just 3) signErrorMo "" ++ "\t" ++
      showFFloat (Just 3) rmsErrorLo "" ++ "\t" ++ showFFloat (Just 3) signErrorLo "" ++ "\t" ++
      showFFloat (Just 3) rmsErrorSo "" ++ "\t" ++ showFFloat (Just 3) signErrorSo ""

segEval :: FileName -> IO ()
segEval (FileName filename) = do
  text <- readFile filename
  let contents = unmarked text
--      -- H(C|B) - H(B|A)
--      dHaTerm = EntropySum (1, [mkH 3 [2]]) (1, [mkH 2 [1]])
--      -- H(C|A) - H(A|B)
--      dHbTerm = EntropySum (1, [mkH 3 [1]]) (1, [mkH 1 [2]])
--      -- H(C|B) + H(C|A) - H(B|A) - H(A|B)
--      dH'Term = EntropySum (1, [mkH 3 [2], mkH 3 [1]]) (1, [mkH 2 [1], mkH 1 [2]])
--      H(C|AB)
--      fs = frequenciesWith (Affix [-2,-1]) contents
--      -- H(C|B) + H(B|C) - H(B|A) - H(C|D)
--      dHaTerm = EntropySum (1, [mkH 3 [2], mkH 2 [3]]) (1, [mkH 2 [1], mkH 3 [4]])
--      -- H(C|A) + H(B|D) - H(A|B) - H(D|C)
--      dHbTerm = EntropySum (1, [mkH 3 [1], mkH 2 [4]]) (1, [mkH 1 [2], mkH 4 [3]])
--      -- H(C|B) + H(B|C) + H(C|A) + H(B|D) - H(B|A) - H(C|D) - H(A|B) - H(D|C)
--      dH'Term = EntropySum (1, [mkH 3 [2], mkH 2 [3], mkH 3 [1], mkH 2 [4]]) (1, [mkH 2 [1], mkH 3 [4], mkH 1 [2], mkH 4 [3]])
--      -- H(C|AB) + H(B|CD)
--      fs = frequenciesWith (Affix [-4,-3,-2,-1]) contents
--      -- H*(D|ABC) -> 1
--      dHaTerm = EntropySum (1, [mkH 4 [3], mkH 4 [3], mkH 4 [2]]) (1, [mkH 2 [1], mkH 3 [2], mkH 3 [1]])
--      dHbTerm = EntropySum (1, [mkH 4 [1], mkH 4 [1], mkH 4 [2]]) (1, [mkH 1 [2], mkH 2 [3], mkH 1 [3]])
--      dH'Term = EntropySum (1, [mkH 4 [3], mkH 4 [3], mkH 4 [2], mkH 4 [1], mkH 4 [1], mkH 4 [2]]) (1, [mkH 2 [1], mkH 3 [2], mkH 3 [1], mkH 1 [2], mkH 2 [3], mkH 1 [3]])
--      fs = frequenciesWith (Affix [1,2,3]) contents
      -- H'(D|ABC)
--      dHaTerm = EntropySum (1, [mkH 4 [3], mkH 4 [2], mkH 1 [2], mkH 2 [1]]) (1, [mkH 2 [3], mkH 3 [2], mkH 3 [1], mkH 3 [2]])
--      dHbTerm = EntropySum (1, [mkH 4 [1], mkH 4 [1], mkH 4 [2], mkH 4 [3], mkH 3 [2]]) (1, [mkH 1 [2], mkH 1 [2], mkH 2 [1], mkH 2 [1], mkH 1 [3]])
--      dH'Term = EntropySum (1, [mkH 4 [3], mkH 4 [3], mkH 4 [2], mkH 4 [2], mkH 4 [1], mkH 4 [1]]) (1, [mkH 2 [1], mkH 1 [2], mkH 3 [1], mkH 1 [3], mkH 2 [3], mkH 3 [2]])
--      fs = frequenciesWith (Affix [1,2,3]) contents
--      -- H^(D|ABC)
--      dHaTerm = EntropySum (1, [mkH 4 [3], mkH 2 [1]]) (1, [mkH 3 [2], mkH 3 [2]])
--      dHbTerm = EntropySum (1, [mkH 4 [1], mkH 4 [1], mkH 4 [2], mkH 4 [2], mkH 4 [3], mkH 3 [2]]) (1, [mkH 2 [1], mkH 2 [1], mkH 1 [2], mkH 1 [3], mkH 3 [1], mkH 2 [3]])
--      dH'Term = EntropySum (1, [mkH 4 [3], mkH 4 [3], mkH 4 [2], mkH 4 [2], mkH 4 [1], mkH 4 [1]]) (1, [mkH 2 [1], mkH 1 [2], mkH 3 [1], mkH 1 [3], mkH 2 [3], mkH 3 [2]])
--      fs = frequenciesWith (Affix [1,2,3]) contents
      -- H*(D|ABC) -> 2
      dHaTerm = EntropySum (1, [mkH 4 [2,3]]) (1, [mkH 3 [1,2]])
      dHbTerm = EntropySum (1, [mkH 4 [1,2], mkH 4 [1,3]]) (1, [mkH 1 [2,3], mkH 2 [1,3]])
      dH'Term = EntropySum (1, [mkH 4 [2,3], mkH 4 [1,2], mkH 4 [1,3]]) (1, [mkH 3 [1,2], mkH 1 [2,3], mkH 2 [1,3]])
      fs = frequenciesWith (Affix [1,2,3]) contents
      -- H(E|ABCD) -> 1
--      dHaTerm = EntropySum (1, [mkH 5 [2], mkH 5 [3], mkH 5 [3], mkH 5 [4], mkH 5 [4], mkH 5 [4]]) (1, [mkH 4 [1], mkH 3 [1], mkH 4 [2], mkH 2 [1], mkH 3 [2], mkH 4 [3]])
--      dHbTerm = EntropySum (1, [mkH 5 [1], mkH 5 [1], mkH 5 [1], mkH 5 [2], mkH 5 [2], mkH 5 [3]]) (1, [mkH 1 [2], mkH 1 [3], mkH 1 [4], mkH 2 [3], mkH 2 [4], mkH 3 [4]])
--      dH'Term = EntropySum (1, [mkH 5 [2], mkH 5 [3], mkH 5 [3], mkH 5 [4], mkH 5 [4], mkH 5 [4], mkH 5 [1], mkH 5 [1], mkH 5 [1], mkH 5 [2], mkH 5 [2], mkH 5 [3]]) (1, [mkH 4 [1], mkH 3 [1], mkH 4 [2], mkH 2 [1], mkH 3 [2], mkH 4 [3], mkH 1 [2], mkH 1 [3], mkH 1 [4], mkH 2 [3], mkH 2 [4], mkH 3 [4]])
--      fs = frequenciesWith (Affix [1,2,3,4]) contents
--      term = mkH 10 [1..9]
--      (dHaTerm, dHbTerm) = toSegBias term
--      dH'Term = toEntropySum (Order 1) term
--      fs = frequenciesWith (toAffix term) contents
      atA = mkAt dHaTerm contents
      atB = mkAt dHbTerm contents
      at' = mkAt dH'Term contents
      entA = M.mapWithKey (\context _ -> atA context) fs
      entB = M.mapWithKey (\context _ -> atB context) fs
      ent' = M.mapWithKey (\context _ -> at' context) fs
      rmseAB = rmse entA entB fs
      rmseA' = rmse entA ent' fs
      rmseB' = rmse entB ent' fs
      msdAB = msd entA entB fs
      msdA' = msd entA ent' fs
      msdB' = msd entB ent' fs
      expA = expectation entA fs
      expB = expectation entB fs
      exp' = expectation ent' fs
      varA = variance entA fs
      varB = variance entB fs
      var' = variance ent' fs
      covAB = covariance entA entB fs
      covA' = covariance entA ent' fs
      covB' = covariance entB ent' fs
      corrAB = correlation entA entB fs
      corrA' = correlation entA ent' fs
      corrB' = correlation entB ent' fs
  putStrLn $ "Pair\tSquared Diff\tSigned Diff\tCovariance\tCorrelation"
  putStrLn $ "A,B\t"
    ++ showFFloat (Just 6) rmseAB "\t"
    ++ showFFloat (Just 6) msdAB "\t"
    ++ showFFloat (Just 6) covAB "\t"
    ++ showFFloat (Just 6) corrAB "\t"
  putStrLn $ "A,A+B\t"
    ++ showFFloat (Just 6) rmseA' "\t"
    ++ showFFloat (Just 6) msdA' "\t"
    ++ showFFloat (Just 6) covA' "\t"
    ++ showFFloat (Just 6) corrA' "\t"
  putStrLn $ "B,A+B\t"
    ++ showFFloat (Just 6) rmseB' "\t"
    ++ showFFloat (Just 6) msdB' "\t"
    ++ showFFloat (Just 6) covB' "\t"
    ++ showFFloat (Just 6) corrB' "\t"
  putStrLn $ "Term\tExpectation\tVariance"
  putStrLn $ "A\t"
    ++ showFFloat (Just 6) expA "\t"
    ++ showFFloat (Just 6) varA "\t"
  putStrLn $ "B\t"
    ++ showFFloat (Just 6) expB "\t"
    ++ showFFloat (Just 6) varB "\t"
  putStrLn $ "A+B\t"
    ++ showFFloat (Just 6) exp' "\t"
    ++ showFFloat (Just 6) var' "\t"

segBiasEval :: FileName -> IO ()
segBiasEval (FileName filename) = do
  text <- readFile filename
  putStrLn $ intercalate "\t"
    ["Order",
--    "Diff-ST", "Corr-BT", "Diff-BT", "Corr-BT",
--    "Corr-SH", "Corr-BH", "Corr-TH",
--    "Rank-SH", "Rank-BH", "Rank-TH"
--    "RSDE-SH", "RSDE-BH", "RSDE-TH"
      "Mean-H", "Var-H"
    ]
  putStrLn $ unlines $ do
    n <- [2..10]
    let contents = unmarked text
        contexts = map (\(Window ts) -> Context ts) (windows (affixOrder (toAffix term)) contents)
        term = mkH (n+1) [1..n]
        (seg, bias) = toSegBias term
        total = toEntropySum (Order 1) term
        fs = frequenciesWith (toAffix term) contents
        atSeg = mkAt seg contents
        atBias = mkAt bias contents
        atTotal = mkAt total contents
        entSeg = M.mapWithKey (\context _ -> atSeg context) fs
        entBias = M.mapWithKey (\context _ -> atBias context) fs
        entTotal = M.mapWithKey (\context _ -> atTotal context) fs
        entHigh = entropiesWith (toAffix term) contents
        varSeg = variance entSeg fs
        varBias = variance entBias fs
        varTotal = variance entTotal fs
        diffSegTotal = abs (varSeg - varTotal) / varTotal
        diffBiasTotal = abs (varBias - varTotal) / varTotal
        corrSegTotal = correlation entSeg entTotal fs
        corrBiasTotal = correlation entBias entTotal fs
        corrSegHigh = correlation entSeg entHigh fs
        corrBiasHigh = correlation entBias entHigh fs
        corrTotalHigh = correlation entTotal entHigh fs
        rankTotalHigh = rank contexts entHigh entTotal
        rankSegHigh = rank contexts entHigh entSeg
        rankBiasHigh = rank contexts entHigh entBias
        rsdTotalHigh = rsd entHigh entTotal fs
        rsdSegHigh = rsd entHigh entSeg fs
        rsdBiasHigh = rsd entHigh entBias fs
        expHigh = expectation entHigh fs
        varHigh = variance entHigh fs
    return $ show n ++ "\t" ++ concatMap (\v -> showFFloat (Just 3) v "\t") [
--      diffSegTotal, corrSegTotal, diffBiasTotal, corrBiasTotal,
--      corrSegHigh, corrBiasHigh, corrTotalHigh
--        rankSegHigh, rankBiasHigh, rankTotalHigh
--        rsdSegHigh, rsdBiasHigh, rsdTotalHigh
        expHigh, varHigh
      ]

suffix01s :: Int -> Int -> FileName -> IO ()
suffix01s alphabetSize contentLength (FileName output) = do
  writeFile output $ intercalate ","
    ["true", "approx", "context", "freq", "high", "decomp", "seg", "bias"]
    ++ "\n"
  appendFile output $ concat $ do
    n <- [2..6]
--    k <- [1]
    let k = 1
        contents = zeroOnes alphabetSize contentLength
        term = mkH (n+1) [1..n]
        decomp = toEntropySum (Order 1) term
        (seg, bias) = toSegBias term
--        markov = mkH (to term) [to term - k..to term - 1]
        freqs = frequenciesWith (toAffix term) contents
        atDecomp = mkAt decomp contents
        atSeg = mkAt seg contents
        atBias = mkAt bias contents
        eHigh = entropiesWith (toAffix term) contents
        eDecomp = M.mapWithKey (\context _ -> atDecomp context) freqs
        eSeg = M.mapWithKey (\context _ -> atSeg context) freqs
        eBias = M.mapWithKey (\context _ -> atBias context) freqs
--        eMarkov' = entropiesWith (toAffix markov) contents
--        eMarkov = M.fromList [
--          (Context c, eMarkov' M.! c')
--          | Context c <- M.keys freqs,
--            let c' = Context $ reverse $ take k $ reverse c]
        es = [eHigh, eDecomp, eSeg, eBias]
        rows = [show n : show k : show c : show (freqs M.! c) :
          map (show . (M.! c)) es | c <- M.keys freqs]
    return $ unlines $ map (intercalate ",") rows

suffixFile :: FileName -> FileName -> IO ()
suffixFile (FileName input) (FileName output) = do
  text <- readFile input
  writeFile output $ intercalate ","
    ["true", "approx", "context", "freq", "seg", "bias"]
    ++ "\n"
  appendFile output $ concat $ do
    n <- [2..6]
--    k <- [1]
    let k = 1
        contents = unmarked text
        term = mkH (n+1) [1..n]
--        decomp = toEntropySum (Order 1) term
        (seg, bias) = toSegBias term
--        markov = mkH (to term) [to term - k..to term - 1]
        freqs = frequenciesWith (toAffix term) contents
--        atDecomp = mkAt decomp contents
        atSeg = mkAt seg contents
        atBias = mkAt bias contents
--        eHigh = entropiesWith (toAffix term) contents
--        eDecomp = M.mapWithKey (\context _ -> atDecomp context) freqs
        eSeg = M.mapWithKey (\context _ -> atSeg context) freqs
        eBias = M.mapWithKey (\context _ -> atBias context) freqs
--        eMarkov' = entropiesWith (toAffix markov) contents
--        eMarkov = M.fromList [
--          (Context c, eMarkov' M.! c')
--          | Context c <- M.keys freqs,
--            let c' = Context $ reverse $ take k $ reverse c]
        es = [eSeg, eBias]
        rows = [show n : show k : show c : show (freqs M.! c) :
          map (show . (M.! c)) es | c <- M.keys freqs]
    return $ unlines $ map (intercalate ",") rows