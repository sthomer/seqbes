{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

module Lib where

--------------------------------------------------------------------------------

import Data.Char
import qualified Data.HashMap.Lazy as M
import Data.Hashable
import Data.List (intercalate, tails)
import Data.Maybe (fromMaybe)
import System.Environment

--------------------------------------------------------------------------------

newtype ContextMap a = ContextMap (M.HashMap Context (M.HashMap Token a))

-- Does not handle zeroth-order models

-- newtype ContextMap a = ContextMap (M.HashMap Context a)
-- newtype TokenMap a = TokenMap (M.HashMap Token a)

newtype StandardMap = StandardMap (M.HashMap Context Standard) deriving (Show)

newtype EntropyMap = EntropyMap (M.HashMap Context Entropy) deriving (Show)

newtype InfoContentMap = InfoContentMap (ContextMap InfoContent)

newtype MarkovChain = MarkovChain (ContextMap Probability)

newtype TransitionMap = TransitionMap (ContextMap Count)

newtype Token = Token String deriving (Eq, Hashable)

newtype Context = Context [Token] deriving (Eq, Hashable)

newtype Window = Window [Token]

newtype Segment = Segment [Token]

data Boundary = Boundary | NoBoundary

newtype BoundaryPolicy = BoundaryPolicy (Entropy -> Entropy -> Boundary)

newtype Order = Order Int

type Count = Integer

type Probability = Double

type Entropy = Double

type InfoContent = Double

type Standard = Double

--------------------------------------------------------------------------------
-- Display

instance Show TransitionMap where
  show (TransitionMap tm) = show tm

instance Show MarkovChain where
  show (MarkovChain mc) = show mc

instance Show InfoContentMap where
  show (InfoContentMap im) = show im

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

--------------------------------------------------------------------------------
-- Construction

tokenize :: Segment -> Token
tokenize (Segment ts) = Token $ concatMap (\(Token t) -> t) ts

tokenizeString :: String -> [Token]
tokenizeString = map(\ c -> Token [c])

windows :: Order -> [Token] -> [Window]
windows (Order n) ts =
  map Window $
    (foldr (zipWith (:)) (repeat []) . take n . tails) ts

increment :: (Token, Context) -> TransitionMap -> TransitionMap
increment (tkn, cxt) (TransitionMap (ContextMap tm)) =
  TransitionMap $
    ContextMap $
      M.insertWith (M.unionWith (+)) cxt (M.singleton tkn 1) tm

count :: (Window -> (Token, Context)) -> [Window] -> TransitionMap
count split = foldr (increment . split) (TransitionMap $ ContextMap M.empty)

transitionMapWith :: (Window -> (Token, Context)) -> Order -> [Token] -> TransitionMap
transitionMapWith split (Order k) = count split . windows (Order (k + 1))

suffixSplit :: Window -> (Token, Context)
suffixSplit (Window ts) = (last ts, Context (init ts))

prefixSplit :: Window -> (Token, Context)
prefixSplit (Window ts) = (head ts, Context (tail ts))

prefixTransitionMap :: Order -> [Token] -> TransitionMap
prefixTransitionMap = transitionMapWith prefixSplit

suffixTransitionMap :: Order -> [Token] -> TransitionMap
suffixTransitionMap = transitionMapWith suffixSplit

markovChain :: TransitionMap -> MarkovChain
markovChain (TransitionMap (ContextMap tm)) =
  MarkovChain $
    ContextMap $
      M.map asDist tm
  where
    asDist mem = M.map ((/ total mem) . fromInteger) mem
    total = fromInteger . sum . M.elems

entropyMap :: MarkovChain -> EntropyMap
entropyMap (MarkovChain (ContextMap mc)) =
  EntropyMap $
    M.map asEntropy mc
  where
    asEntropy = entropy . M.elems

--standardMap :: EntropyMap -> StandardMap
--standardMap (EntropyMap em) = StandardMap $ M.map standard em
--  where
--    mean = sum (M.elems em) / fromIntegral (length em)
--    sdev = undefined
--    standard entropy = (entropy - mean) / sdev

infoContentMap :: MarkovChain -> InfoContentMap
infoContentMap (MarkovChain (ContextMap mc)) =
  InfoContentMap $
    ContextMap $
      M.map asInfoContent mc
  where
    asInfoContent = M.map infoContent

infoContent :: Probability -> InfoContent
infoContent p
  | p == 1 = 0
  | otherwise = (negate . logBase 2) p

entropy :: [Probability] -> Entropy
entropy = sum . map (\p -> p * infoContent p)

--------------------------------------------------------------------------------
-- Segmentation

rise :: BoundaryPolicy
rise = BoundaryPolicy (\x y -> if x < y then Boundary else NoBoundary)

boundary :: BoundaryPolicy -> EntropyMap -> (Context, Context) -> Boundary
boundary (BoundaryPolicy bp) (EntropyMap em) (cxtA, cxtB) =
  fromMaybe NoBoundary $ do
    entropyA <- M.lookup cxtA em
    entropyB <- M.lookup cxtB em
    pure $ entropyA `bp` entropyB

boundaryIC :: BoundaryPolicy -> InfoContentMap -> (Token, Token) -> (Context, Context) -> Boundary
boundaryIC (BoundaryPolicy bp) (InfoContentMap (ContextMap im)) (tknA, tknB) (cxtA, cxtB) =
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
          
-- (target, (suffix_a, suffix_b), (prefix_a, prefix_b)) 
contexts :: Order -> [Token] -> [(Token, (Context, Context), (Context, Context))] 
contexts o@(Order k) ts = zip3 $ 
  drop k ts $ contextPairs o ts $ contextPairs o $ drop k ts

contextPairs :: Order -> [Token] -> [(Context, Context)]
contextPairs (Order k) (t:ts) = zip $ windows k (t:ts) $ windows k ts

boundaryFold :: Order -> BoundaryPolicy -> (EntropyMap, EntropyMap) -> [Token] -> [Segment]
boundaryFold (Order k) bp (pm, sm) ts = foldr f initial ts
  where
    initial = undefined
    f (t', suffixContexts, prefixContexts) (current, segments) = case isBoundary of
      Boundary -> (Segment (rev))
      NoBoundary -> 
      where
        isBoundary = unionBoundary prefixBoundary suffixBoundary
        isPrefixBoundary = boundary bp prefixContexts
        isSuffixBoundary = boundary bp suffixContexts


segmentByBoundaryEntropy :: Order -> [Token] -> [Segment]
segmentByBoundaryEntropy k ts =
  boundarySegments k rise (pm ts, sm ts) ts
  where
    pm = entropyMap . markovChain . prefixTransitionMap k
    sm = entropyMap . markovChain . suffixTransitionMap k

nestedBES :: Count -> Order -> [Token] -> [Token]
nestedBES i k ts
  | i < 1 = ts
  | otherwise = nestedBES (i-1) k $ map tokenize $ segmentByBoundaryEntropy k ts

--------------------------------------------------------------------------------
-- Input

nestedBesText :: Integer -> Int -> String -> IO ()
nestedBesText depth k fileName = do
  text <- readFile fileName
  let contents = preprocessText text
      segments = nestedBES depth (Order k) contents
  print $ take 100 segments
  return ()

segmentTextWithOrder :: Int -> String -> IO ()
segmentTextWithOrder = nestedBesText 1

tokenChar :: Char -> Token
tokenChar c = Token [c]

preprocessText :: String -> [Token]
preprocessText =
  map (tokenChar . replaceDigit . toLower) . filter isAlphaNum . filter isAscii

replaceDigit :: Char -> Char
replaceDigit x
  | isDigit x = '#'
  | otherwise = x
