-- | This module provides the core of the RASP-L language.
--
-- It is based on Listing 2 of
-- "What Algorithms Can Transformers Learn", https://arxiv.org/abs/2310.16028,
-- by Zhou et al.
module Core
  ( -- * Types

    -- | Most of these types are merely aliases.

    -- ** Tokens and Sequences
    Token,
    Sequence,
    Keys,
    Queries,
    Values,

    -- ** Predicates and Selectors
    Predicate,
    Selector,
    BoolSequence,

    -- ** Aggregation
    Aggregator,
    AggregationType (..),

    -- * Functions

    -- ** Key-Query-Value lookup
    kqv,
    maxKQV,
    minKQV,
    selectCausal,
    filledWith,
    indicesOf,

    -- ** Aggregation
    aggregate,
    aggrMax,
    aggrMean,
    aggrMin,

    -- ** Selection
    selWidth,
    fromBool,

    -- * Compatibility

    -- | These functions are provided for closer compatibility with the original Python implementation of RASP-L.
    tokMap,
    seqMap,
    full,
    indices,
    aggr,
    select,
  )
where

import Data.Int (Int8)
import Data.Maybe (fromMaybe)

-- | A `Token` in a `Sequence` is a small integer.
-- RASP-L uses `Int8` to ensure all maps of type `Token` -> `Token` are learnable.
type Token = Int8

-- | A `Sequence` is a list of `Token`s.
type Sequence = [Token]

-- | A collection of keys is a list of `Token`s.
type Keys = Sequence

-- | A collection of queries is a list of `Token`s.
type Queries = Sequence

-- | A collection of values is a list of `Token`s.
type Values = Sequence

-- | We can compare `Keys` and `Queries` to determine if they match.
type Predicate = Token -> Token -> Bool

-- | The equivalents of "attention maps" are collections of `Bool`ean sequences.
type Selector = [BoolSequence]

-- | Internally, we sometimes need to operate on collections of `Bool`s.
type BoolSequence = [Bool]

-- | Enum for the three methods for aggregating selected values
data AggregationType
  = Min
  | Mean
  | Max

-- | Performs a key-query-value lookup operation and aggregates over values.
--
-- Given a filler token, an aggregation type, two sequences (keys and queries),
-- and a predicate, it returns a processed sequence. It first selects elements
-- based on the predicate and then aggregates them.
--
-- Roughly matches the attention layer of a Transformer.
kqv ::
  -- | Filler token used in aggregation
  Token ->
  -- | Type of aggregation (Min, Mean, Max)
  AggregationType ->
  -- | Sequence of keys
  Keys ->
  -- | Sequence of queries
  Queries ->
  -- | A boolean predicate that determines whether a key and query match
  Predicate ->
  -- | Sequence of values
  Values ->
  -- | The output sequence
  Sequence
kqv filler agg keys queries predicate = aggregate agg filler $ selectCausal keys queries predicate

-- | Performs Key-Query-Value lookup with maximum aggregation of values.
maxKQV :: Keys -> Queries -> Predicate -> Values -> Sequence
maxKQV = kqv minInt8 Max
  where
    minInt8 = minBound :: Int8

-- | Performs Key-Query-Value lookup with minimum aggregation of values.
minKQV :: Keys -> Queries -> Predicate -> Values -> Sequence
minKQV = kqv maxInt8 Min
  where
    maxInt8 = maxBound :: Int8

-- | Compareis pairs of elements from sequences with a predicate subject to a causal constraint.
selectCausal :: Keys -> Queries -> Predicate -> Selector
selectCausal keys queries predicate =
  [ [ (keyIndex <= queryIndex) && predicate (keys !! keyIndex) (queries !! queryIndex)
      | keyIndex <- [0 .. length keys - 1]
    ]
    | queryIndex <- [0 .. length queries - 1]
  ]

-- | Creates a matched-length constant sequence with the provided token.
filledWith :: Sequence -> Token -> Sequence
filledWith = replicate . length

-- | Extracts the indices of the elements in a sequence.
indicesOf :: Sequence -> Sequence
indicesOf x = [0 .. (fromIntegral (length x) - 1)]

-- | Type alias for "fully-specified" aggregators that are ready to aggregate a sequence of values with a selector.
type Aggregator = Selector -> Values -> Sequence

-- | Aggregates values with some aggregation, filling in with a default token.
aggregate :: AggregationType -> Token -> Aggregator
aggregate Max = aggrMax
aggregate Mean = aggrMean
aggregate Min = aggrMin

-- | Aggregates values by selecting the largest value.
aggrMax :: Token -> Aggregator
aggrMax filler a v = map (aggrMaxByRow filler v) a

-- | Aggregates values by taking the mean.
aggrMean :: Token -> Aggregator
aggrMean filler a v = map (aggrMeanByRow filler v) a

-- | Aggregates values by selecting the smallest value.
aggrMin :: Token -> Aggregator
aggrMin filler a v = map (aggrMinByRow filler v) a

aggrMaxByRow :: Token -> Sequence -> BoolSequence -> Token
aggrMaxByRow filler v a = fromMaybe filler maybeMax
  where
    maybeMax = safeMaximum (filterByList a v)

aggrMeanByRow :: Token -> Sequence -> BoolSequence -> Token
aggrMeanByRow filler v a = fromMaybe filler maybeMean
  where
    maybeMean = safeInt8Mean (filterByList a v)

aggrMinByRow :: Token -> Sequence -> BoolSequence -> Token
aggrMinByRow filler v a = fromMaybe filler maybeMin
  where
    maybeMin = safeMinimum (filterByList a v)

filterByList :: [Bool] -> [a] -> [a]
filterByList (True : bs) (x : xs) = x : filterByList bs xs
filterByList (False : bs) (_ : xs) = filterByList bs xs
filterByList _ _ = []

safeMaximum :: (Ord a) => [a] -> Maybe a
safeMaximum [] = Nothing
safeMaximum xs = Just (maximum xs)

safeMinimum :: (Ord a) => [a] -> Maybe a
safeMinimum [] = Nothing
safeMinimum xs = Just (minimum xs)

safeInt8Mean :: Sequence -> Maybe Token
safeInt8Mean [] = Nothing
safeInt8Mean xs = Just (sum xs `div` fromIntegral (length xs))

-- | Computes the "width", or number of nonzero entries, of the rows of a `Selector`.
selWidth :: Selector -> Sequence
selWidth = map (sum . map fromBool)

fromBool :: Bool -> Token
fromBool True = 1
fromBool _ = 0

-- | Applies an elementwise operation to a sequence of tokens.
--
-- Roughly matches the MLP layer in a Transformer. Alias for `map`.
tokMap :: (Token -> Token) -> Sequence -> Sequence
tokMap = map

-- | Applies an elementwise operation for pairs of tokens on a pair of sequences.
-- Alias for `zipWith`.
seqMap :: (Token -> Token -> Token) -> Sequence -> Sequence -> Sequence
seqMap = zipWith

-- | Creates a sequence of the same length as the provided sequence filled with the provided token.
-- Alias for `filledWith`.
full :: Sequence -> Token -> Sequence
full = filledWith

-- | Extracts the indices of the elements in a sequence.
-- Alias for `indicesOf`.
indices :: Sequence -> Sequence
indices = indicesOf

-- | Creates an aggregator with a given aggregation type.
-- Alias for `aggregate`.
aggr :: AggregationType -> Token -> Aggregator
aggr = aggregate

-- | Produces a selector indicating which pairs of `Keys` and `Queries` match.
select ::
  -- | Whether to use causal selection
  Bool ->
  -- | A collection of `Keys` to check against `Queries`
  Keys ->
  -- | A collection of `Queries` to check against `Keys`
  Queries ->
  -- | A boolean predicate that determines whether a `Key` and `Query` match
  Predicate ->
  -- | A collection of boolean sequences indicating which pairs of `Keys` and `Queries` match
  Selector
select True = selectCausal
select False = selectAcausal

-- | Non-causal selection is included for some reason.
selectAcausal :: Keys -> Queries -> Predicate -> Selector
selectAcausal keys queries predicate = [[predicate keyIndex queryIndex | keyIndex <- keys] | queryIndex <- queries]
