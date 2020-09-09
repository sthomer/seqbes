{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Lib where

--------------------------------------------------------------------------------

import Data.Char
import qualified Data.HashMap.Lazy as M
import Data.Hashable
import Data.List (genericLength, inits, intercalate, intersect, sort, sortOn, tails)
import Data.Maybe (fromMaybe)
import qualified Data.Set as Set
import Numeric
import System.Environment
import Data.HashMap.Lazy ((!))

--------------------------------------------------------------------------------

newtype ContextMap a = ContextMap (M.HashMap Context (M.HashMap Token a))

-- Does not handle zeroth-order models

-- newtype ContextMap a = ContextMap (M.HashMap Context a)
-- newtype TokenMap a = TokenMap (M.HashMap Token a)

class Affix a

data Prefix

instance Affix Prefix

data Suffix

instance Affix Suffix

data Infix

instance Affix Infix

data Forward

instance Affix Forward

data Reverse

instance Affix Reverse

newtype EntropyMap a = EntropyMap (M.HashMap Context Entropy)

newtype InfoContentMap a = InfoContentMap (ContextMap InfoContent)

newtype MarkovChain a = MarkovChain (ContextMap Probability)

newtype TransitionMap a = TransitionMap (ContextMap Count)

newtype FrequencyMap = FrequencyMap (M.HashMap Context Probability)

newtype Token = Token String deriving (Eq, Hashable, Ord)

newtype Context = Context [Token] deriving (Eq, Hashable, Ord)

newtype Window = Window [Token]

newtype Segment = Segment [Token]

data Boundary = Boundary | NoBoundary

newtype BoundaryPolicy = BoundaryPolicy (Entropy -> Entropy -> Boundary)

newtype Order = Order Int

newtype Depth = Depth Int

newtype Count = Count Integer deriving (Num, Eq, Ord, Real, Show)

newtype Frame = Frame (Token, (Context, Context), (Context, Context))

newtype Score = Score (Double, Double, Double)

type Probability = Double

type Entropy = Double

type InfoContent = Double

type Standard = Double

type FileName = String

--------------------------------------------------------------------------------
-- Display

instance Show (TransitionMap a) where
  show (TransitionMap tm) = show tm

instance Show (MarkovChain a) where
  show (MarkovChain mc) = show mc

instance Show (InfoContentMap a) where
  show (InfoContentMap im) = show im

instance Show (EntropyMap a) where
  show (EntropyMap em) = intercalate "\n" $ map showEntry $ sortOn snd (M.toList em)
    where
      showEntry (t, n) = show t ++ ": " ++ showFFloat (Just 6) n ""

instance Show FrequencyMap where
  show (FrequencyMap em) = intercalate "\n" $ map showEntry $ sort (M.toList em)
    where
      showEntry (t, n) = show t ++ ": " ++ showFFloat (Just 3) n ""

instance (Show a) => Show (ContextMap a) where
  show (ContextMap cm) =
    intercalate "\n" $
      map showMemories (M.toList cm)
    where
      showMemories (m, ts) = show m ++ ":\n" ++ showTokens ts
      showTokens = intercalate "\n" . map showEntry . M.toList
      showEntry (t, n) = "  " ++ show t ++ ": " ++ show n

instance Show Window where
  show (Window w) = concatMap show w

instance Show Context where
  show (Context ts) = concatMap show ts

instance Show Segment where
  show (Segment s) = concatMap show s

instance Show Token where
  show (Token t) = t

instance Show Score where
  show (Score (p, r, f)) =
    intercalate "\t" $
      map ((\s -> s "") . showFFloat (Just 3)) [p, r, f]

instance Show Order where
  show (Order k) = show k

instance Show Depth where
  show (Depth d) = show d

--------------------------------------------------------------------------------
-- Construction

tokenize :: Segment -> Token
tokenize (Segment ts) = Token $ concatMap (\(Token t) -> t) ts

tokenizeString :: String -> [Token]
tokenizeString = map (\c -> Token [c])

tokenizeChar :: Char -> Token
tokenizeChar c = Token [c]

toContext :: Window -> Context
toContext (Window w) = Context w

toWindow :: Context -> Window
toWindow (Context c) = Window c

windows :: Order -> [Token] -> [Window]
windows (Order n) ts =
  map Window $
    (foldr (zipWith (:)) (repeat []) . take n . tails) ts

increment :: (Token, Context) -> TransitionMap a -> TransitionMap a
increment (tkn, cxt) (TransitionMap (ContextMap tm)) =
  TransitionMap $
    ContextMap $
      M.insertWith (M.unionWith (+)) cxt (M.singleton tkn 1) tm

countTransitions :: (Window -> (Token, Context)) -> [Window] -> TransitionMap a
countTransitions split = foldr (increment . split) (TransitionMap $ ContextMap M.empty)

transitionMapWith :: (Window -> (Token, Context)) -> Order -> [Token] -> TransitionMap a
transitionMapWith split (Order k) = countTransitions split . windows (Order (k + 1))

suffixSplit :: Window -> (Token, Context)
suffixSplit (Window ts) = (last ts, Context (init ts))

forwardSplit :: Window -> (Token, Context)
forwardSplit (Window ts) = (last ts, Context [head ts])

reverseSplit :: Window -> (Token, Context)
reverseSplit (Window ts) = (head ts, Context [last ts])

prefixSplit :: Window -> (Token, Context)
prefixSplit (Window ts) = (head ts, Context (tail ts))

prefixTransitionMap :: Order -> [Token] -> TransitionMap Prefix
prefixTransitionMap = transitionMapWith prefixSplit

suffixTransitionMap :: Order -> [Token] -> TransitionMap Suffix
suffixTransitionMap = transitionMapWith suffixSplit

markovChain :: TransitionMap a -> MarkovChain a
markovChain (TransitionMap (ContextMap tm)) = MarkovChain $ ContextMap $ M.map asDist tm
  where
    asDist cxt = M.map ((/ total cxt) . realToFrac) cxt
    total = realToFrac . sum . M.elems

frequencyMap :: Order -> [Token] -> FrequencyMap
frequencyMap k ts = FrequencyMap $ M.map (/ total) fs
  where
    fs = (M.fromListWith (+) . map ((,1) . toContext)) (windows k ts)
    total = genericLength ts

prefixEntropyMap :: Order -> [Token] -> EntropyMap Prefix
prefixEntropyMap k = entropyMap . markovChain . transitionMapWith prefixSplit k

suffixEntropyMap :: Order -> [Token] -> EntropyMap Suffix
suffixEntropyMap k = entropyMap . markovChain . transitionMapWith suffixSplit k

forwardEntropyMap :: Order -> [Token] -> EntropyMap Forward
forwardEntropyMap k = entropyMap . markovChain . transitionMapWith forwardSplit k

reverseEntropyMap :: Order -> [Token] -> EntropyMap Reverse
reverseEntropyMap k = entropyMap . markovChain . transitionMapWith reverseSplit k

infixEntropyMap :: EntropyMap Prefix -> EntropyMap Suffix -> EntropyMap Infix
infixEntropyMap (EntropyMap pm) (EntropyMap sm) = EntropyMap $ M.unionWith (-) pm sm

entropyMap :: MarkovChain a -> EntropyMap a
entropyMap (MarkovChain (ContextMap mc)) = EntropyMap $ M.map (entropy . M.elems) mc

standardMap :: EntropyMap a -> FrequencyMap -> EntropyMap a
standardMap (EntropyMap em) (FrequencyMap fm) = EntropyMap $ M.map standardize em
  where
    mean = sum $ M.elems $ M.unionWith (*) em fm
    sd = sqrt $ sum $ M.elems $ M.unionWith (*) fm (M.map (\e -> (e - mean) ^ 2) em)
    standardize e = (e - mean) / sd

infoContentMap :: MarkovChain a -> InfoContentMap a
infoContentMap (MarkovChain (ContextMap mc)) =
  InfoContentMap $
    ContextMap $
      M.map asInfoContent mc
  where
    asInfoContent = M.map infoContent

infoContent :: Probability -> InfoContent
infoContent p
  | p == 0 = 0
  | p == 1 = 0
  | otherwise = (negate . logBase 2) p

entropy :: [Probability] -> Entropy
entropy = sum . map (\p -> p * infoContent p)

--------------------------------------------------------------------------------
-- Segmentation

rise :: Entropy -> Entropy -> Boundary
rise x y
  | x < y = Boundary
  | otherwise = NoBoundary

entropyBoundary :: BoundaryPolicy -> EntropyMap a -> (Context, Context) -> Boundary
entropyBoundary (BoundaryPolicy bp) (EntropyMap em) (cxtA, cxtB)
  | (cxtA, cxtB) == (Context [], Context []) = Boundary -- Hack
  | otherwise = fromMaybe NoBoundary $
    do
      entropyA <- M.lookup cxtA em
      entropyB <- M.lookup cxtB em
      pure $ entropyA `bp` entropyB

icBoundary :: BoundaryPolicy -> InfoContentMap a -> ((Token, Context), (Token, Context)) -> Boundary
icBoundary (BoundaryPolicy bp) (InfoContentMap (ContextMap im)) ((tknA, cxtA), (tknB, cxtB)) =
  fromMaybe NoBoundary $ do
    imA <- M.lookup cxtA im
    icA <- M.lookup tknA imA
    imB <- M.lookup cxtB im
    icB <- M.lookup tknB imB
    pure $ icA `bp` icB

unionBoundary :: Boundary -> Boundary -> Boundary
unionBoundary NoBoundary NoBoundary = NoBoundary
unionBoundary _ Boundary = Boundary
unionBoundary Boundary _ = Boundary

emptyFrame :: Frame
emptyFrame = Frame (Token [], (Context [], Context []), (Context [], Context []))

frames :: Order -> [Token] -> [Frame]
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

contextPairs :: Order -> [Token] -> [(Context, Context)]
contextPairs k (t : ts) = zip (contexts (t : ts)) (contexts ts)
  where
    contexts xs = map toContext $ windows k xs

entropyFold :: Order -> BoundaryPolicy -> (EntropyMap Prefix, EntropyMap Suffix) -> [Token] -> [Segment]
entropyFold k bp (pm, sm) ts = snd $ foldr boundaryFrame initial $ frames k ts
  where
    initial = (Segment [], [])
    boundaryFrame (Frame (t, suffixCxts, prefixCxts)) (Segment s, segments) = case isBoundary of
      Boundary -> (Segment [t], Segment s : segments)
      NoBoundary -> (Segment (t : s), segments)
      where
        isBoundary = unionBoundary isPrefixBoundary isSuffixBoundary
        isPrefixBoundary = entropyBoundary bp pm prefixCxts
        isSuffixBoundary = entropyBoundary bp sm suffixCxts

icFold :: Order -> BoundaryPolicy -> (InfoContentMap Prefix, InfoContentMap Suffix) -> [Token] -> [Segment]
icFold (Order k) bp (pm, sm) ts = snd $ foldr f initial (frames (Order (k + 1)) ts)
  where
    initial = (Segment [], [])
    f (Frame (t', suffixWs, prefixWs)) (Segment s, segments) = case isBoundary of
      Boundary -> (Segment [t'], Segment s : segments)
      NoBoundary -> (Segment (t' : s), segments)
      where
        (pa, pb) = prefixWs
        (sa, sb) = suffixWs
        prefixTknCxts = (prefixSplit (toWindow pa), prefixSplit (toWindow pb))
        suffixTknCxts = (suffixSplit (toWindow sa), suffixSplit (toWindow sb))
        isPrefixBoundary = icBoundary bp pm prefixTknCxts
        isSuffixBoundary = icBoundary bp sm suffixTknCxts
        isBoundary = unionBoundary isPrefixBoundary isSuffixBoundary

segmentByBoundaryEntropy :: Order -> [Token] -> [Segment]
segmentByBoundaryEntropy k ts =
  entropyFold k (BoundaryPolicy rise) (prefixEntropyMap k ts, suffixEntropyMap k ts) ts

segmentByBoundaryIC :: Order -> [Token] -> [Segment]
segmentByBoundaryIC k ts = icFold k (BoundaryPolicy rise) (pm ts, sm ts) ts
  where
    pm = infoContentMap . markovChain . prefixTransitionMap k
    sm = infoContentMap . markovChain . suffixTransitionMap k

nestedSegmentation :: (Order -> [Token] -> [Segment]) -> Depth -> Order -> [Token] -> [Token]
nestedSegmentation f (Depth d) k ts
  | d < 1 = ts
  | otherwise = nestedSegmentation f (Depth (d - 1)) k $ map tokenize $ f k ts

nestedEntropy :: Depth -> Order -> [Token] -> [Token]
nestedEntropy = nestedSegmentation segmentByBoundaryEntropy

nestedInfoContent :: Depth -> Order -> [Token] -> [Token]
nestedInfoContent = nestedSegmentation segmentByBoundaryIC

--------------------------------------------------------------------------------
-- Input

aggregation :: Order -> FileName -> IO ()
aggregation (Order k) filename = do
  text <- readFile filename
  let contents = unmarked text
      (EntropyMap sm1) = suffixEntropyMap (Order 1) contents
      (EntropyMap sm) = suffixEntropyMap (Order k) contents
      (EntropyMap pm1) = prefixEntropyMap (Order 1) contents
      (EntropyMap pm) = prefixEntropyMap (Order k) contents
      (EntropyMap fm) = forwardEntropyMap (Order (k+1)) contents
      (FrequencyMap cm1)= frequencyMap (Order 1) contents
      (FrequencyMap cm2)= frequencyMap (Order (k+1)) contents
      im = M.unionWith (-) sm pm

      -- 1) H(C|AB) = H(C|B)
      -- 2) H(C|AB) = H(C|B) - H(A|B) + H(B|A)
      -- 3) H(C|AB) = H(C|B) - H(A|B) + H(C|A)
      hm' = M.fromList [(Context (ka ++ kb), 0 + vb) |
                        (Context ka, va) <- M.toList fm, --sm1
                        (Context kb, vb) <- M.toList im] --sm
      (EntropyMap hm) = suffixEntropyMap (Order (k+1)) contents

      dm = M.intersectionWith (-) hm hm'
      sem = M.map (^2) dm
      aem = M.map abs dm
      rms = sqrt $ sum $ M.elems $ M.intersectionWith (*) sem cm2
      mae = sum $ M.elems $ M.intersectionWith (*) sem cm2
      dm2 = M.intersectionWith (-) sm1 fm
      sem2 = M.map (^2) dm2
      aem2 = M.map abs dm2
      rms2 = sqrt $ sum $ M.elems $ M.intersectionWith (*) sem2 cm1
      mae2 = sum $ M.elems $ M.intersectionWith (*) aem2 cm1
--  putStrLn $ "RMS Error: " ++ show rms
--  putStrLn $ "MA Error: " ++ show mae
--  putStrLn $ show (EntropyMap cm2)
  putStrLn $ show $ hm ! Context (tokenizeString "ng")
  putStrLn $ show $ hm ! Context (tokenizeString "th")
  putStrLn $ show $ hm ! Context (tokenizeString "he")
  putStrLn $ show $ hm ! Context (tokenizeString "in")
  putStrLn $ show $ hm ! Context (tokenizeString "er")
  putStrLn $ show $ hm ! Context (tokenizeString "an")
  putStrLn ""
  putStrLn $ show $ hm' ! Context (tokenizeString "ng")
  putStrLn $ show $ hm' ! Context (tokenizeString "th")
  putStrLn $ show $ hm' ! Context (tokenizeString "he")
  putStrLn $ show $ hm' ! Context (tokenizeString "in")
  putStrLn $ show $ hm' ! Context (tokenizeString "er")
  putStrLn $ show $ hm' ! Context (tokenizeString "an")

standardMapOf :: Order -> String -> IO ()
standardMapOf k filename = do
  text <- readFile filename
  let contents = unmarked text
      fm = frequencyMap k contents
      pm = prefixEntropyMap k contents
      sm = suffixEntropyMap k contents
      im = infixEntropyMap pm sm
      standard = standardMap im fm
  putStrLn "Prefix Entropy Map"
  putStrLn $ show pm
  putStrLn "Suffix Entropy Map"
  putStrLn $ show sm
  putStrLn "Standard Infix Entropy Map"
  putStrLn $ show standard

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

segmentationWith :: ([Token] -> [Token]) -> String -> [Token]
segmentationWith f text = f (unmarked text)

nestedEntropyText :: Depth -> Order -> String -> [Token]
nestedEntropyText d k = segmentationWith (nestedEntropy d k)

nestedInfoContentText :: Depth -> Order -> String -> [Token]
nestedInfoContentText d k = segmentationWith (nestedInfoContent d k)

runMultiple :: FileName -> IO ()
runMultiple s = do
  printUsingFile qualityForDepthsOrders s

runSegmentation :: FileName -> IO ()
runSegmentation s = do
  printUsingFile (show . take 100 . nestedEntropyText (Depth 1) (Order 1)) s

--------------------------------------------------------------------------------
-- Ground Truth

unmarked :: String -> [Token]
unmarked = map (tokenizeChar . toLower) . filter isLetter . filter isAscii

groundTruth :: String -> [Token]
groundTruth =
  map Token . words . map toLower . filter ((||) <$> isLetter <*> isSpace) . filter isAscii

-- Since we're comparing sets, the direction of the scan doesn't actually matter
-- Therefore, indices are counted from the end of the list to the front
boundaryIndices :: [Token] -> Set.Set Int
boundaryIndices ts =
  Set.fromDistinctDescList $
    (tail . scanr1 (+) . map (\(Token t) -> genericLength t)) ts

quality :: [Token] -> [Token] -> Score
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
