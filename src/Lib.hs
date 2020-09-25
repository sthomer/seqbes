{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib where

--------------------------------------------------------------------------------
-- Imports

import Data.Char (isAscii, isLetter, isSpace, toLower)
import qualified Data.HashMap.Lazy as M
import Data.Hashable (Hashable)
import Data.List (elem, elemIndex, inits, intercalate, intersect, nub, sort, sortOn, tails, union, (\\), delete, nubBy, replicate)
import Data.Maybe (fromJust, fromMaybe)
import Data.Ratio (denominator, numerator, (%))
import qualified Data.Set as Set
import Numeric (showFFloat)

--------------------------------------------------------------------------------
-- Types

newtype Token a = Token a deriving (Eq, Hashable)

newtype Context a = Context [Token a] deriving (Eq, Hashable)

newtype Order = Order Int deriving (Num)

newtype Affix = Affix {unAffix :: [Int]}
  deriving (Show, Eq, Hashable) -- e.g. H(B|ACDE) = [-1,+1,+2,+3]

type ContextMap a b = M.HashMap (Context a) b

newtype ShowContextMap a b = ShowContextMap (ContextMap a b)

type ContextTokenMap a b = ContextMap a (M.HashMap (Token a) b)

newtype ShowContextTokenMap a b = ShowContextTokenMap (ContextTokenMap a b)

type Entropies a = ContextMap a Entropy

type InfoContents a = ContextTokenMap a Entropy

type Frequencies a = ContextMap a Probability

type Probabilities a = ContextTokenMap a Probability

type Transitions a = ContextTokenMap a Transition

newtype Entropy = Entropy Double
  deriving (Num, Eq, Ord, Real, Show, Fractional, Floating, RealFrac, RealFloat)

-- TODO: Record syntax
newtype EntropyTerm = EntropyTerm (Rational, Int, [Int]) deriving (Eq, Hashable)

data EntropySum = EntropySum (Rational, [EntropyTerm]) (Rational, [EntropyTerm])

newtype Window a = Window [Token a]

newtype Probability = Probability {unProbability :: Double}
  deriving (Num, Eq, Ord, Real, Show, Fractional, RealFrac, Floating, RealFloat)

newtype Transition = Transition {unTransition :: Int}
  deriving (Num, Eq, Ord, Real, Show)

newtype FileName = FileName String

newtype Score = Score (Double, Double, Double)

data Boundary = Boundary | NoBoundary

newtype BoundaryPolicy = BoundaryPolicy (Entropy -> Entropy -> Boundary)

newtype Segment a = Segment [Token a]

newtype Frame a = Frame (Token a, (Context a, Context a), (Context a, Context a))

newtype Depth = Depth Int

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

instance Show EntropyTerm where
  show (EntropyTerm (s, y, [])) = showScale s ++ "H(" ++ show y ++ ")"
  show (EntropyTerm (s, y, xs)) = showScale s ++
    "H(" ++ show y ++ "|" ++ intercalate "," (map show xs) ++ ")"

showScale :: Rational -> String
showScale s
  | s == 1 = ""
  | numerator s /= 1 && denominator s == 1 = show (numerator s)
  | otherwise = "(" ++ show (numerator s) ++ "/" ++ show (denominator s) ++ ")"

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
      showEntry (t, n) = show t ++ ": " ++ showFFloat (Just 3) n ""

instance (Show a, Monoid a, Show b) => Show (ShowContextTokenMap a b) where
  show (ShowContextTokenMap m) = intercalate "\n" $ map showContexts (M.toList m)
    where
      showContexts (c, ts) = show c ++ ":\n" ++ showTokens ts
      showTokens = intercalate "\n" . map showEntry . M.toList
      showEntry (t, n) = "  " ++ show t ++ ": " ++ show n

--------------------------------------------------------------------------------
-- Utilities

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

subsets :: (Ord a) => Int -> [a] -> [[a]]
subsets k =
  Set.toList . Set.map Set.toList . Set.filter ((== k) . length) . Set.powerSet . Set.fromList

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

--------------------------------------------------------------------------------
-- Construction

increment :: (Eq a, Hashable a) => (Token a, Context a) -> Transitions a -> Transitions a
increment (token, context) = M.insertWith (M.unionWith (+)) context (M.singleton token 1)

countTransitions :: (Eq a, Hashable a) => Affix -> [Window a] -> Transitions a
countTransitions affix =
  foldr (increment . affixSplit affix) M.empty

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

suffixDiff :: (Eq a, Hashable a) => EntropyTerm -> [Token a] -> Entropies a
suffixDiff term tokens = diffs
  where
    entropies = M.toList $ entropiesWith (toAffix term) tokens
    diffs = M.fromList [
      (Context (head ca : cb), va - vb) |
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

expectation :: (Eq a, Hashable a, Real b) => ContextMap a b -> Frequencies a -> Double
expectation contextMap frequencies = sum $ M.elems weightedMap
  where
    weightedMap = M.intersectionWith (*) fs cs
    fs = M.map unProbability frequencies
    cs = M.map realToFrac contextMap

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

compareOrder :: (Eq a, Hashable a, Ord b)
  => ContextMap a b
  -> ContextMap a b
  -> Double
compareOrder xm ym = totalMatching / totalPairs
  where
    pairs = [(x, y) | x <- M.keys xm, y <- M.keys xm, xm M.! x < xm M.! y]
    matching = [(x, y) | (x, y) <- pairs, ym M.! x < ym M.! y]
    totalPairs = (fromIntegral . length) pairs
    totalMatching = (fromIntegral . length) matching

-- Only use suffix entropies (for now)
-- Crossover b/w Decomp and Markov at about 4-context or 5-context
-- Shortened model almost always nearly as good as full decomposed model
homm :: FileName -> EntropyTerm -> Order -> IO ()
homm (FileName filename) term order = do
  text <- readFile filename
  let contents = unmarked text
      -- Higher-order Model
      hoEntropies = entropiesWith (toAffix term) contents
      hoFrequencies = frequenciesWith (toAffix term) contents
      -- Decomposed Model
      loTerm = toEntropySum order term
      atLo = mkAt loTerm contents
      loEntropies = M.mapWithKey (\context _ -> atLo context) hoEntropies
      rmsErrorLo = rmse hoEntropies loEntropies hoFrequencies
      -- Shortened Model
      soTerm = shorten loTerm
      atSo = mkAt soTerm contents
      soEntropies = M.mapWithKey (\context _ -> atSo context) hoEntropies
      rmsErrorSo = rmse hoEntropies soEntropies hoFrequencies
      -- Markov Assumption Model
      moTerm = mkH (to term) [to term - 1] -- only makes sense for suffix entropy
      moEntropies = entropiesWith (toAffix moTerm) contents
      moEntropies' =
        M.fromList
          [ (Context c, moEntropies M.! Context [last c])
            | (Context c, _) <- M.toList hoEntropies
          ]
      rmsError' = rmse hoEntropies moEntropies' hoFrequencies
  putStrLn $ "Markov: " ++ showFFloat (Just 3) rmsError' ""
  putStrLn $ "Decomp: " ++ showFFloat (Just 3) rmsErrorLo ""
  putStrLn $ "Short1: " ++ showFFloat (Just 3) rmsErrorSo ""

-- Only use suffix entropies (for now)
suffixDiffs :: FileName -> Order -> Order -> IO ()
suffixDiffs (FileName filename) (Order higher) order = do
  text <- readFile filename
  let contents = unmarked text
      term = mkH (higher + 2) [2..higher + 1]
      term' = mkH (higher + 1) [1..higher]
      -- Higher-order model differences
      hoDiff = suffixDiff term contents
      hoFrequencies = frequenciesWith (toAffix (combine term term')) contents
      -- Decomposed model differences
      loTerm = toEntropySum order term `minus` toEntropySum order term'
      at = mkAt loTerm contents
      loDiff = M.mapWithKey (\context _ -> at context) hoDiff
      rmsError = rmse hoDiff loDiff hoFrequencies
      signError = sign hoDiff loDiff hoFrequencies
      -- Markov assumption model differences
      markovTerm = mkH 3 [2]
      moDiff = suffixDiff markovTerm contents
      moDiff' = M.fromList [ (Context c, moDiff M.! Context ((\(b:a:_) -> [a,b]) (reverse c)))
                            | (Context c, _) <- M.toList hoDiff]
      rmsError' = rmse hoDiff moDiff' hoFrequencies
      signError' = sign hoDiff moDiff' hoFrequencies
  putStrLn $ "Markov RMSE: " ++ showFFloat (Just 3) rmsError' ""
  putStrLn $ "Markov Sign: " ++ showFFloat (Just 3) signError' ""
  putStrLn $ "Decomp RMSE: " ++ showFFloat (Just 3) rmsError ""
  putStrLn $ "Decomp Sign: " ++ showFFloat (Just 3) signError ""
