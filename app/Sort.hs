module Sort (main) where

import Core
import Lib (gt, sample)

-- | Θ(n^2) time, Θ(n) space sorting algorithm
sort :: Sequence -> Sequence
sort xs = sample endOfSequence raspSort (prep xs) seqLength
  where
    endOfSequence = maxBound
    seqLength = fromIntegral (length xs)
    raspSort :: Sequence -> Sequence
    -- Sort by looking for the smallest token greater than the current token
    raspSort s = minKQV s s gt s
    prep :: Sequence -> Sequence
    -- Prepare a sequence for sorting by padding with max/min values
    prep s = [maxBound] ++ s ++ [minBound]

-- | Sorts a newline-separated collection of space-separated int8 sequences
main :: IO ()
main = do
  contents <- getContents
  let sorts = map sort lists
      lists = map parse $ lines contents
  mapM_ printClean sorts

parse :: String -> [Token]
parse = map read . words

printClean :: Sequence -> IO ()
printClean = putStrLn . unwords . map show . dropPrefix
  where
    dropPrefix = drop 1 . dropWhile (> minBound)
