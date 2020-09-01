{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
tokenizeString = map (\c -> Token [c])

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
  | p == 0 = 0
  | p == 1 = 0
  | otherwise = (negate . logBase 2) p

entropy :: [Probability] -> Entropy
entropy = sum . map (\p -> p * infoContent p)

--------------------------------------------------------------------------------
-- Segmentation

rise :: BoundaryPolicy
rise = BoundaryPolicy (\x y -> if x < y then Boundary else NoBoundary)

entropyBoundary :: BoundaryPolicy -> EntropyMap -> (Context, Context) -> Boundary
entropyBoundary (BoundaryPolicy bp) (EntropyMap em) (cxtA, cxtB) =
  fromMaybe NoBoundary $ do
    entropyA <- M.lookup cxtA em
    entropyB <- M.lookup cxtB em
    pure $ entropyA `bp` entropyB

icBoundary :: BoundaryPolicy -> InfoContentMap -> (Token, Token) -> (Context, Context) -> Boundary
icBoundary (BoundaryPolicy bp) (InfoContentMap (ContextMap im)) (tknA, tknB) (cxtA, cxtB) =
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

-- (target, (suffix_a, suffix_b), (prefix_a, prefix_b))
-- TODO: Handle edges of token list
contexts :: Order -> [Token] -> [(Token, (Context, Context), (Context, Context))]
contexts (Order k) ts = zip3 (drop k ts) (contextPairs (Order k) ts) (contextPairs (Order k) $ drop k ts)

contextPairs :: Order -> [Token] -> [(Context, Context)]
contextPairs k (t : ts) = zip (map window2context (windows k (t : ts))) (map window2context (windows k ts))

window2context :: Window -> Context
window2context (Window w) = Context w

-- TODO: Handle edges of token list
entropyFold :: Order -> BoundaryPolicy -> (EntropyMap, EntropyMap) -> [Token] -> [Segment]
entropyFold (Order k) bp (pm, sm) ts = snd $ foldr f initial (contexts (Order k) ts)
  where
    initial = (Segment [], [])
    f (t', suffixCxts, prefixCxts) (Segment s, segments) = case isBoundary of
      Boundary -> (Segment [t'], Segment s : segments)
      NoBoundary -> (Segment (t' : s), segments)
      where
        isBoundary = unionBoundary isPrefixBoundary isSuffixBoundary
        isPrefixBoundary = entropyBoundary bp pm prefixCxts
        isSuffixBoundary = entropyBoundary bp sm suffixCxts

segmentByBoundaryEntropy :: Order -> [Token] -> [Segment]
segmentByBoundaryEntropy k ts =
  entropyFold k rise (pm ts, sm ts) ts
  where
    pm = entropyMap . markovChain . prefixTransitionMap k
    sm = entropyMap . markovChain . suffixTransitionMap k

nestedBES :: Count -> Order -> [Token] -> [Token]
nestedBES i k ts
  | i < 1 = ts
  | otherwise = nestedBES (i - 1) k $ map tokenize $ segmentByBoundaryEntropy k ts

--------------------------------------------------------------------------------
-- Input

nestedBesText :: Integer -> Int -> String -> IO ()
nestedBesText depth k fileName = do
  text <- readFile fileName
  let contents = preprocessText text
      segments = nestedBES depth (Order k) contents
  print $ (take 500 . drop 10000) segments
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
