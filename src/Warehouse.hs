module Warehouse where
--
--import Lib
--import qualified Data.HashMap.Lazy as M
--import Data.Maybe (fromMaybe)
--
--
--icSuffixSegments :: Order -> BoundaryPolicy -> InfoContentMap -> [Token] -> [Segment]
--icSuffixSegments (Order k) bp im ts = go ts initial
--  where
--    initial = Segment $ reverse $ take k ts
--    go (t : ts) (Segment s)
--      | length ts < k + 1 = [Segment (reverse (t' : s))]
--      | otherwise = case boundaryIC bp im tkns cxts of
--        Boundary -> Segment (reverse (t' : s)) : go ts (Segment [])
--        NoBoundary -> go ts $ Segment (t' : s)
--      where
--        t' = (t : ts) !! k
--        tkns = (t, head ts)
--        cxts = (Context $ take k (t : ts), Context $ take k ts)
--
--segmentBySuffixIC :: Int -> String -> [String]
--segmentBySuffixIC k s =
--  map show $
--    icSuffixSegments (Order k) rise (im s) (tokenizeString s)
--  where
--    im = infoContentMap . markovChain . suffixTransitionMap (Order k) . tokenizeString
--
--suffixSegments :: Order -> BoundaryPolicy -> EntropyMap -> [Token] -> [Segment]
--suffixSegments (Order k) bp em ts = go ts initial
--  where
--    initial = Segment $ reverse $ take k ts
--    go (t : ts) (Segment s)
--      | length ts < k + 1 = [Segment (reverse (t' : s))]
--      | otherwise = case boundary bp em cxts of
--        Boundary -> Segment (reverse (t' : s)) : go ts (Segment [])
--        NoBoundary -> go ts $ Segment (t' : s)
--      where
--        t' = (t : ts) !! k
--        cxts = (Context $ take k (t : ts), Context $ take k ts)
--
--segmentBySuffixEntropy :: Int -> String -> [String]
--segmentBySuffixEntropy k s =
--  map show $
--    suffixSegments (Order k) rise (em s) (tokenizeString s)
--  where
--    em = entropyMap . markovChain . suffixTransitionMap (Order k) . tokenizeString
--
--prefixSegments :: Order -> BoundaryPolicy -> EntropyMap -> [Token] -> [Segment]
--prefixSegments (Order k) bp em ts = go ts initial
--  where
--    initial = Segment []
--    go (t : ts) (Segment s)
--      | length ts < k + 1 = [Segment (reverse s ++ (t : ts))]
--      | otherwise = case boundary bp em cxts of
--        Boundary -> Segment (reverse s) : go ts (Segment [t])
--        NoBoundary -> go ts $ Segment (t : s)
--      where
--        cxts = (Context $ take k (t : ts), Context $ take k ts)
--
--segmentByPrefixEntropy :: Int -> String -> [String]
--segmentByPrefixEntropy k s =
--  map show $
--    prefixSegments (Order k) rise (em s) (tokenizeString s)
--  where
--    em = entropyMap . markovChain . prefixTransitionMap (Order k) . tokenizeString
--
---- Note: suffixSegments is Order k ahead of prefixSegments in the list
--
--boundarySegments :: Order -> BoundaryPolicy -> (EntropyMap, EntropyMap) -> [Token] -> [Segment]
--boundarySegments (Order k) bp (pm, sm) ts = go ts initial
--  where
--    initial = Segment $ reverse $ take k ts
--    go (t : ts) (Segment s)
--      | length ts < k = [Segment (reverse s) | not (null s)]
--      | otherwise = case unionBoundary prefixBoundary suffixBoundary of
--        Boundary -> Segment (reverse (t' : s)) : go ts (Segment [])
--        NoBoundary -> go ts $ Segment (t' : s)
--      where
--        t' = (t : ts) !! k
--        prefixBoundary = boundary bp pm prefixContexts
--        suffixBoundary = boundary bp sm suffixContexts
--        prefixContexts =
--          ( Context $ (take k . drop k) (t : ts),
--            Context $ (take k . drop k) ts
--          )
--        suffixContexts =
--          ( Context $ take k (t : ts),
--            Context $ take k ts
--          )
--
--

--decompose :: EntropyExpr -> EntropyExpr
--decompose base@Term {from = [_,_,_]} = base -- hardcoded to stop at first-order
--decompose Term {to = y, from = xs} =
--  Sum {scale, positive = map decompose pos, negative = map decompose neg}
--  where
--    scale = 1 / ((fromIntegral . length) xs + 1)
--    tcs = (nub . map (\(x : xs) -> (x, sort xs)) . permutations) xs
--    pos = map (\(_, from) -> Term {to = y, from}) tcs
--    neg = map (\(to, from) -> Term {to, from}) tcs
--
--flatten :: EntropyExpr -> EntropyExpr
--flatten base@Term{} = base
--flatten Sum {..} = Sum {scale = scales, positive = ps, negative = ns}
--  where
--    positives = map flatten positive
--    negatives = map flatten negative
--    ps = concatMap pos positives ++ concatMap neg negatives
--    ns = concatMap pos negatives ++ concatMap neg positives
--    pos term@Term{} = [term]
--    pos Sum{..} = positive
--    neg term@Term{} = []
--    neg Sum{..} = negative
--    sc Term{} = 1
--    sc Sum{..} = scale * sc (head positive)
--    scales = scale * sc (head positive)
--
--cancellation :: EntropyExpr -> EntropyExpr
--cancellation base@Term{} = base
--cancellation Sum {..} = Sum {scale, positive = ps, negative = ns}
--  where
--    ps = positive \\ negative
--    ns = negative \\ positive

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

--nestedInfoContent :: Depth -> Order -> [Token a] -> [Token a]
--nestedInfoContent = nestedSegmentation segmentByBoundaryIC

--nestedInfoContentText :: Depth -> Order -> String -> [Token String]
--nestedInfoContentText d k = segmentationWith (nestedInfoContent d k)

---- Currently hard-coded to first-order
--toFirstOrderSum :: EntropyTerm -> EntropySum
--toFirstOrderSum (EntropyTerm y []) = EntropySum (1, [EntropyTerm y []]) (1, [])
--toFirstOrderSum (EntropyTerm y [x]) = EntropySum (1, [EntropyTerm y [x]]) (1, [])
--toFirstOrderSum (EntropyTerm y xs) = EntropySum (sp, ps) (sn, ns)
--  where
--    ps = map (\x -> EntropyTerm y [x]) xs
--    ns = map (\(x, x') -> EntropyTerm x [x']) pairs
--    pairs = [(x, y) | x <- xs, y <- xs, x /= y]
--    n = (fromIntegral . length) xs
--    sp = 2 % ((n + 1) * n)
--    sn = 2 % ((n + 1) * n * (n - 1))

--standardMap :: (Eq a, Hashable a) => EntropyMap a -> FrequencyMap a -> EntropyMap a
--standardMap (EntropyMap' em) (FrequencyMap' fm) = EntropyMap' m'
--  where
--    m' = M.map standardize em
--    fme = M.map (\(Probability x) -> Entropy x) fm -- there's gotta be a better way
--    mean = sum $ M.elems $ M.unionWith (*) em fme
--    sd = sqrt $ sum $ M.elems $ M.unionWith (*) fme (M.map (\e -> (e - mean) ^ 2) em)
--    standardize e = (e - mean) / sd

--frequencies :: (Eq a, Hashable a) => Order -> [Token a] -> Frequencies a
--frequencies order tokens = M.map (/ total) counts
--  where
--    counts = (M.fromListWith (+) . map ((,1) . toContext)) (windows order tokens)
--    total = (fromIntegral . length) tokens
