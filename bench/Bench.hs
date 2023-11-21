import Core
import Criterion.Main
import Lib (gt, sample)

sort :: Sequence -> Sequence
sort xs = sample (maximum xs) raspSort (xs ++ [minBound]) (fromIntegral (length xs))
  where
    raspSort :: Sequence -> Sequence
    raspSort s = minKQV s s gt s

main :: IO ()
main =
  defaultMain
    [ bgroup
        "sort"
        [ bench "2" $ whnf sort (reverse [0 .. 2]),
          bench "4" $ whnf sort (reverse [0 .. 4]),
          bench "8" $ whnf sort (reverse [0 .. 8]),
          bench "16" $ whnf sort (reverse [1 .. 16]),
          bench "32" $ whnf sort (reverse [1 .. 32])
        ]
    ]
