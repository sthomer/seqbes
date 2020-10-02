module Graph where

{-

Motivation:
- Represent all Markov model orders in a single data structure
- All combinations of contexts and targets should be represented simultaneously
- Avoid duplicating context information, similar to a trie (prefix tree)
- Represent positional dependencies between elements of a context/target

Nodes lie on a plane representing an affix position.
Edges connect different planes by specifying an affix.
Therefore, an edge type corresponds to a general context,
  and an edge instance corresponds to a specific context.
-}

type Index = Int
type Count = Int
type Symbol = Char
type Alphabet = [Symbol]

data PositionSet = Position Index Alphabet
data Affix = Affix Index [Index]
data AffixSet = AffixSet Affix [Edge]
data Context = Context Symbol [Symbol]
data Edge = Edge Context Count



