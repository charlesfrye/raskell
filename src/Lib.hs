-- | This module provides convenience functions built from the core of the RASP-L language.
--
-- It is based on Listing 3 of
-- "What Algorithms Can Transformers Learn", https://arxiv.org/abs/2310.16028,
-- by Zhou et al.
module Lib
  ( -- * Logical Operations
    (?),
    where',
    shiftRight,
    toBool,
    mask,

    -- * Running Aggregation
    cumSum,
    maximum',
    minimum',
    argmax,
    argmin,

    -- * Token Comparison
    leq,
    geq,
    lt,
    gt,

    -- * Sampling
    sample,
  )
where

import Core
import Data.Int (Int8)
import Data.Word (Word8)

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
-- filling the vacated positions with the provided token.
shiftRight :: Token -> Int8 -> Sequence -> Sequence
shiftRight filler n xs = kqv filler Mean shiftedIdxs idxs (==) xs
  where
    shiftedIdxs = map (+ n) idxs
    idxs = indices xs

toBool :: Token -> Bool
toBool x
  | x == 0 = False
  | otherwise = True

fromBoolSeq :: [Bool] -> Sequence
fromBoolSeq = map fromBool

cumSum :: [Bool] -> Sequence
cumSum bs = selWidth (selectCausal bTokens bTokens first)
  where
    bTokens = fromBoolSeq bs
    first x _ = toBool x

mask :: Token -> [Bool] -> Sequence -> Sequence
mask filler bs xs = bs ? (xs, xs `filledWith` filler)

maximum' :: Sequence -> Sequence
maximum' xs = maxKQV xs xs always xs
  where
    always _ _ = True

minimum' :: Sequence -> Sequence
minimum' xs = map negate $ maximum' $ map negate xs

argmax :: Sequence -> Sequence
argmax xs = maxKQV maxs xs (==) (indicesOf xs)
  where
    maxs = maximum' xs

argmin :: Sequence -> Sequence
argmin xs = argmax $ map negate xs

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
  -- | Initial sequence
  Sequence ->
  -- | Number of steps to decode
  Word8 ->
  Sequence
sample _ _ xs 0 = xs
sample endOfSequence prog xs n
  | last xs == endOfSequence = xs
  | otherwise = sample endOfSequence prog (xs ++ [last $ prog xs]) (n - 1)
