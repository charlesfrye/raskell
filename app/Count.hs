module Count (main) where

import RaskellCore
import RaskellLib

sos :: Token
sos = -1

eos :: Token
eos = -2

equals :: Token -> Token -> Token
equals x y
  | x == y = 1
  | otherwise = 0

-- TODO: rewrite with startCounting before withEos?
raspCount :: Sequence -> Sequence
raspCount inputs = finalCounts
  where
    finalCounts =
      -- to get the final next token prediction
      map (> 0) startCounting -- figure out where we start counting
        ? (countFroms, withEOS) -- and merge the starting number into our output there
    withEOS =
      -- add the EOS tokens in the right spots
      map (> 0) transitions ? (inputs `filledWith` eos, succs)
    succs =
      -- increment all of the tokens
      -- that's our basic prediction, the rest of this is just patching edge cases
      map (+ 1) inputs
    transitions =
      -- determine where we switch from counting to reading task tokens or vice versa
      zipWith equals inputs countTos -- it's actually just whenever we hit a number we're counting to
    countFroms =
      -- determine what number we're in the middle of counting from for each token
      maxKQV idxs (map (+ 1) lastSOS) (==) inputs
    countTos =
      -- determine what number we're in the middle of counting to for each token
      maxKQV idxs (map (+ 2) lastSOS) (==) inputs
    startCounting = zipWith equals idxs (map (+ 2) lastSOS)
    lastSOS =
      -- sequence of indices of most recent SoS token
      maxKQV inputs (inputs `filledWith` sos) (==) idxs
    idxs = indicesOf inputs

-- showSequence :: Sequence -> String
-- showSequence tokens = "[" ++ intercalate ", " (map showToken tokens) ++ "]"

showToken :: Token -> String
showToken token
  | token == sos = "SOS"
  | token == eos = "EOS"
  | otherwise = show token

-- | Counts between numbers provided in a newline-separated collection of space-separated non-negative int8 pairs.
--
-- Try it with
--   cat exampleCount.txt | raskell-count
main :: IO ()
main = do
  contents <- getContents
  let counts = map count lists
      lists = map parse $ lines contents
  mapM_ printClean counts

count :: Sequence -> Sequence
count xs = sample eos raspCount (prep xs) seqLength
  where
    seqLength = 24
    prep :: Sequence -> Sequence
    -- Prepare a sequence for counting by prepending an SOS token
    prep s = sos : s

parse :: String -> [Token]
parse = map read . words

printClean :: Sequence -> IO ()
printClean = putStrLn . unwords . map showToken . dropSuffix . dropPrefix
  where
    dropPrefix = drop 3 -- drop 1 . dropWhile (>= -1)
    dropSuffix = reverse . drop 1 . reverse
