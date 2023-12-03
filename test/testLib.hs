{-# LANGUAGE FlexibleInstances #-}

import Control.Monad (when)
import Data.Int (Int8)
import Data.List (inits)
import RaskellCore
import RaskellLib
import Test.QuickCheck (Arbitrary (arbitrary, shrink), Property, Result (..), choose, quickCheckResult, vectorOf, (===), (==>))

prop_where_allTrue_is_idLeft :: Sequence -> Property
prop_where_allTrue_is_idLeft xs = xs === allTrue ? (xs, xs `filledWith` undefined)
  where
    allTrue = replicate (length xs) True

prop_where_allFalse_is_idRight :: Sequence -> Property
prop_where_allFalse_is_idRight xs = xs === allFalse ? (xs `filledWith` undefined, xs)
  where
    allFalse = replicate (length xs) False

prop_where_alternating_alternates :: Sequence -> Property
prop_where_alternating_alternates xs = take l (cycle [1, -1]) === alternating ? (xs `filledWith` 1, xs `filledWith` (-1))
  where
    alternating = cycle [True, False]
    l = length xs

prop_shiftRight_zero_is_id :: Sequence -> Property
prop_shiftRight_zero_is_id xs = xs === shiftRight 0 0 xs

prop_shiftRight_length_matches_replicate :: Sequence -> Property
prop_shiftRight_length_matches_replicate xs = replicate (fromIntegral l) 1 === shiftRight 1 l xs
  where
    l = fromIntegral . length $ xs

prop_shiftRight_matches_rotateFill :: Token -> Int8 -> Sequence -> Property
prop_shiftRight_matches_rotateFill t n xs = n >= 0 && l > 0 ==> rotateFill xs === shiftRight t n xs
  where
    -- Uses normal list operations to shift the sequence.
    rotateFill :: Sequence -> Sequence
    rotateFill s = take l $ replicate n' t ++ take (l - n') s

    n' = fromIntegral n
    l = length xs

prop_cumSum_matches_scanl :: [Bool] -> Property
prop_cumSum_matches_scanl bs = scanl1 (+) (map fromBool bs) === cumSum bs

prop_mask_matches_zipWith :: Token -> [Bool] -> Sequence -> Property
prop_mask_matches_zipWith t bs xs = zipWith (\b x -> if b then x else t) bs xs === mask t bs xs

prop_maximum'_matches_scanl :: Sequence -> Property
prop_maximum'_matches_scanl xs = scanl1 max xs === maximum' xs

prop_minimum'_matches_scanl :: Sequence -> Property
prop_minimum'_matches_scanl xs = scanl1 min xs === minimum' xs

prop_argmax_matches_scanl :: Sequence -> Property
prop_argmax_matches_scanl xs = map fst (scanl1 argmax' (enumerate xs)) === argmax xs
  where
    argmax' :: (Token, Token) -> (Token, Token) -> (Token, Token)
    argmax' (accIdx, accVal) (idx, val)
      | val >= accVal = (idx, val)
      | otherwise = (accIdx, accVal)

    enumerate = zip [0 ..]

prop_argmin_matches_scanl :: Sequence -> Property
prop_argmin_matches_scanl xs = map fst (scanl1 argmin' (enumerate xs)) === argmin xs
  where
    argmin' :: (Token, Token) -> (Token, Token) -> (Token, Token)
    argmin' (accIdx, accVal) (idx, val)
      | val <= accVal = (idx, val)
      | otherwise = (accIdx, accVal)

    enumerate = zip [0 ..]

-- Define a newtype for a pair of lists of the same length
newtype EqualLengthSequences = EqualLengthSequences ([Token], [Token])
  deriving (Show)

-- Generate a pair of lists of the same length
instance Arbitrary EqualLengthSequences where
  arbitrary = do
    len <- choose (0, 100) -- Choose a length between 0 and 100
    list1 <- vectorOf len arbitrary
    list2 <- vectorOf len arbitrary
    return $ EqualLengthSequences (list1, list2)

  shrink (EqualLengthSequences (l1, l2)) =
    [EqualLengthSequences (l1', l2') | (l1', l2') <- zip (shrink l1) (shrink l2)]

prop_numPrev_matches_zipWith :: EqualLengthSequences -> Property
prop_numPrev_matches_zipWith (EqualLengthSequences (xs, qs)) =
  l > 0 ==> zipWith numPrev' (tail (inits xs)) qs === numPrev xs qs
  where
    numPrev' :: [Token] -> Token -> Int8
    numPrev' (x : xs) q = numPrev' xs q + fromBool (x == q)
    numPrev' [] _ = 0

    l = length qs

prop_hasSeen_matches_zipWith :: EqualLengthSequences -> Property
prop_hasSeen_matches_zipWith (EqualLengthSequences (xs, qs)) =
  l > 0 ==> zipWith hasSeen' (tail (inits xs)) qs === hasSeen xs qs
  where
    hasSeen' :: [Token] -> Token -> Int8
    hasSeen' (x : xs) q = max (hasSeen' xs q) (fromBool (x == q))
    hasSeen' [] _ = 0

    l = length qs

prop_firsts_matches_zipWith :: Token -> EqualLengthSequences -> Property
prop_firsts_matches_zipWith filler (EqualLengthSequences (xs, qs)) =
  l > 0 ==> zipWith firsts' (tail (inits (enumerate xs))) qs === firsts filler xs qs
  where
    firsts' :: [(Int8, Token)] -> Token -> Int8
    firsts' ((idx, x) : xs) q =
      if x == q then idx else firsts' xs q
    firsts' [] _ = filler

    enumerate = zip [0 ..]

    l = length qs

prop_lasts_matches_zipWith :: Token -> EqualLengthSequences -> Property
prop_lasts_matches_zipWith filler (EqualLengthSequences (xs, qs)) =
  l > 0 ==> zipWith lasts' (tail (inits (enumerate xs))) qs === lasts filler xs qs
  where
    lasts' :: [(Int8, Token)] -> Token -> Int8
    lasts' xs q = case filter (\(_, x) -> x == q) xs of
      [] -> filler
      xs' -> fst $ last xs'

    enumerate = zip [0 ..]

    l = length qs

prop_indexSelect_matches_zipWith :: Token -> EqualLengthSequences -> Property
prop_indexSelect_matches_zipWith filler (EqualLengthSequences (xs, idxs)) =
  l > 0 ==> zipWith indexSelect' (tail (inits (enumerate xs))) idxs === indexSelect filler xs idxs
  where
    indexSelect' :: [(Int8, Token)] -> Token -> Token
    indexSelect' xs q = case filter (\(idx, _) -> idx == q) xs of
      [] -> filler
      xs' -> snd $ last xs'
    enumerate = zip [0 ..]

    l = length idxs

main :: IO ()
main = do
  results <-
    sequence
      [ quickCheckResult prop_where_allTrue_is_idLeft,
        quickCheckResult prop_where_allFalse_is_idRight,
        quickCheckResult prop_where_alternating_alternates,
        quickCheckResult prop_shiftRight_zero_is_id,
        quickCheckResult prop_shiftRight_length_matches_replicate,
        quickCheckResult prop_shiftRight_matches_rotateFill,
        quickCheckResult prop_cumSum_matches_scanl,
        quickCheckResult prop_mask_matches_zipWith,
        quickCheckResult prop_maximum'_matches_scanl,
        quickCheckResult prop_minimum'_matches_scanl,
        quickCheckResult prop_argmax_matches_scanl,
        quickCheckResult prop_argmin_matches_scanl,
        quickCheckResult prop_numPrev_matches_zipWith,
        quickCheckResult prop_hasSeen_matches_zipWith,
        quickCheckResult prop_firsts_matches_zipWith,
        quickCheckResult prop_lasts_matches_zipWith
      ]
  let failed = not (all isSuccess results)
  when failed $ error "Some tests failed"

isSuccess :: Result -> Bool
isSuccess Success {} = True
isSuccess _ = False
