-- | This module provides convenience functions built from the core of the RASP-L language.
--
-- It is based on Listing 3 of
-- "What Algorithms Can Transformers Learn", https://arxiv.org/abs/2310.16028,
-- by Zhou et al.
module RaskellLib
  ( -- * Logical Operations
    (?),
    shiftRight,
    toBool,
    mask,

    -- * Running Aggregations
    cumSum,
    maximum',
    minimum',
    argmax,
    argmin,

    -- * Aggregations with `Queries`
    numPrev,
    hasSeen,

    -- * Indexing with `Queries`
    firsts,
    lasts,
    indexSelect,

    -- * Token Comparisons
    leq,
    geq,
    lt,
    gt,

    -- * Sampling
    sample,

    -- * Compatibility
    where',
    sampleAutoregressive,
  )
where

import Data.Int (Int8)
import Data.Word (Word8)
import RaskellCore

-- | Use a boolean sequence to select between two sequences.
-- Also known in Python RASP-L as "where", see `where'`.
(?) :: [Bool] -> (Sequence, Sequence) -> Sequence
bs ? (xs, ys) = seqMap (\xm ym -> if xm == 0 then ym else xm) xms yms
  where
    xms = seqMap (\bt x -> if bt == 1 then x else 0) bts xs
    yms = seqMap (\bt y -> if bt == 0 then y else 0) bts ys
    bts = fromBoolSeq bs

-- | Use a boolean sequence to select between two sequences.
-- Provided for compatibility with Listing 3, but with
-- an apostrophe to avoid a name clash with the "where" keyword.
where' :: [Bool] -> Sequence -> Sequence -> Sequence
where' bs xs ys = bs ? (xs, ys)

-- | Shift a sequence to the right by a given number of elements,
-- filling the vacated positions with the provided `Token`.
shiftRight ::
  -- | Filler `Token`
  Token ->
  -- | Number of positions to shift
  Int8 ->
  -- | Input `Sequence`
  Sequence ->
  Sequence
shiftRight filler n xs = kqv filler Mean shiftedIdxs idxs (==) xs
  where
    shiftedIdxs = map (+ n) idxs
    idxs = indices xs

-- | Maps tokens onto bools using Python's "truthiness" rules.
toBool :: Token -> Bool
toBool x
  | x == 0 = False
  | otherwise = True

-- | Converts a list of bools to a sequence of tokens.
fromBoolSeq :: [Bool] -> Sequence
fromBoolSeq = map fromBool

-- | Computes the cumulative sum of a boolean sequence.
cumSum :: [Bool] -> Sequence
cumSum bs = selWidth (selectCausal bTokens bTokens first)
  where
    bTokens = fromBoolSeq bs
    first x _ = toBool x

-- | Masks a `Sequence` with a boolean sequence, using the provided `Token` as the mask.
mask :: Token -> [Bool] -> Sequence -> Sequence
mask maskT bs xs = bs ? (xs, xs `filledWith` maskT)

-- | Computes the running maximum of a `Sequence`.
maximum' :: Sequence -> Sequence
maximum' xs = maxKQV xs xs always xs
  where
    always _ _ = True

-- | Computes the running minimum of a `Sequence`.
minimum' :: Sequence -> Sequence
minimum' xs = minKQV xs xs always xs
  where
    always _ _ = True

-- | Computes the indices of the running maximum values in a `Sequence`.
argmax :: Sequence -> Sequence
argmax xs = maxKQV xs maxs (==) (indicesOf xs)
  where
    maxs = maximum' xs

-- | Computes the indices of the running minimum values in a `Sequence`.
argmin :: Sequence -> Sequence
argmin xs = maxKQV xs mins (==) (indicesOf xs)
  where
    mins = minimum' xs

-- | Computes the number of previous tokens in a `Sequence` that are equal to each `Token` from `Queries`.
numPrev :: Sequence -> Queries -> Sequence
numPrev xs queries = selWidth (selectCausal xs queries (==))

-- | Returns 1s where the `Token` from the `Queries` has been seen before in the `Sequence`.
hasSeen :: Sequence -> Queries -> Sequence
hasSeen xs queries = kqv 0 Max xs queries (==) (queries `filledWith` 1)

-- | Finds the first occurrence of each query token in a `Sequence`.
firsts :: Token -> Sequence -> Queries -> Sequence
firsts filler xs queries = kqv filler Min xs queries (==) (indicesOf xs)

-- | Finds the last occurrence of each query token in a `Sequence`.
lasts :: Token -> Sequence -> Queries -> Sequence
lasts filler xs queries = kqv filler Max xs queries (==) (indicesOf xs)

-- | Selects the tokens from a `Sequence` at the indices provided by another sequence.
indexSelect :: Token -> Sequence -> Sequence -> Sequence
indexSelect filler xs idxs = kqv filler Max (indicesOf xs) idxs (==) xs

leq :: Token -> Token -> Bool
leq = (<=)

geq :: Token -> Token -> Bool
geq = (>=)

lt :: Token -> Token -> Bool
lt = (<)

gt :: Token -> Token -> Bool
gt = (>)

-- | Greedily and autoregressively sample the output of a RASP-L program on a sequence.
sample ::
  -- | End of sequence token
  Token ->
  -- | RASP-L program to extend the sequence
  (Sequence -> Sequence) ->
  -- | Initial/prompt sequence
  Sequence ->
  -- | Number of steps to decode
  Word8 ->
  -- | Output (including prompt)
  Sequence
sample _ _ xs 0 = xs
sample endOfSequence prog xs n
  | last xs == endOfSequence = xs
  | otherwise = sample endOfSequence prog (xs ++ [last $ prog xs]) (n - 1)

-- | Greedily and autoregressively sample the output of a RASP-L program on a sequence.
--
-- Provided for compatibility with Listing 3.
sampleAutoregressive :: Token -> (Sequence -> Sequence) -> Sequence -> Word8 -> Sequence
sampleAutoregressive = sample
