module Count where

import Core
import Data.List (intercalate)
import Lib

sos :: Token
sos = -1

eos :: Token
eos = -2

inputs :: Sequence
inputs =
  [ sos,
    002,
    004,
    002,
    003,
    004,
    eos,
    sos,
    117,
    119,
    117,
    118,
    119
  ]

targets :: Sequence
targets =
  [ sos,
    sos,
    002,
    003,
    004,
    eos,
    sos,
    sos,
    sos,
    117,
    118,
    119,
    eos
  ]

difference :: Sequence -> Sequence -> Int
difference xs ys = length (filter (\(x, y) -> (y /= sos) && (x /= y)) (zip xs ys))

maxDifference :: Sequence -> Int
maxDifference ys = difference (ys `filledWith` sos) ys

equals :: Token -> Token -> Token
equals x y
  | x == y = 1
  | otherwise = 0

succs :: Sequence
succs = map (+ 1) inputs

idxs :: Sequence
idxs = indicesOf inputs

startIdxs :: Sequence
startIdxs = maxKQV inputs (inputs `filledWith` sos) (==) idxs

countFroms :: Sequence
countFroms = maxKQV idxs (map (+ 1) startIdxs) (==) inputs

countTos :: Sequence
countTos = maxKQV idxs (map (+ 2) startIdxs) (==) inputs

startCounting :: Sequence
startCounting = zipWith equals idxs (map (+ 2) startIdxs)

transitions :: Sequence
transitions = zipWith equals inputs countTos

mergeSuccsAndEoS :: Sequence
mergeSuccsAndEoS = map (> 0) transitions ? (inputs `filledWith` eos, succs)

nextTokens :: Sequence
nextTokens = map (> 0) startCounting ? (countFroms, mergeSuccsAndEoS)

showSequence :: Sequence -> String
showSequence tokens = "[" ++ intercalate ", " (map showToken tokens) ++ "]"

showToken :: Token -> String
showToken token
  | token == sos = ".sos"
  | token == eos = ".eos"
  | token < -100 = show token
  | token < -10 = " " ++ show token
  | token < 0 = "  " ++ show token
  | token < 10 = "   " ++ show token
  | token < 100 = "00" ++ show token
  | otherwise = "0" ++ show token

main :: IO ()
main = do
  print "> COUNT"
  print ("inputs:           " ++ showSequence inputs)
  print ("targets:          " ++ showSequence targets)
  print ("succs:            " ++ showSequence succs)

  let deltas = difference succs targets
      maxDelta = maxDifference targets
  print ("-- errors in succs: " ++ show deltas ++ " of at most " ++ show maxDelta)

  print ("startIdxs:        " ++ showSequence startIdxs)
  print ("countFroms:       " ++ showSequence countFroms)
  print ("countTos:         " ++ showSequence countTos)

  print ("startCounting:    " ++ showSequence startCounting)
  print ("transitions:      " ++ showSequence transitions)
  print ("mergeSuccsAndEoS: " ++ showSequence mergeSuccsAndEoS)
  print ("nextTokens:       " ++ showSequence nextTokens)

  let deltas = difference nextTokens targets
  print ("-- errors in nextTokens: " ++ show deltas ++ " of at most " ++ show maxDelta)
