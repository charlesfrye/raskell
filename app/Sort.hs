module Sort where

import Core
import Lib

sort :: Sequence -> Sequence
sort xs = decode endOfSequence raspSort (xs ++ [minBound]) (fromIntegral (length xs))
  where
    endOfSequence = maximum xs
    raspSort :: Sequence -> Sequence
    -- Sort by looking for the smallest element greater than the current token
    raspSort s = minKQV s s gt s
