# `raskell`: RASP-L in Haskell

Implements the RASP-L language from
[What Algorithms Can Transformers Learn](https://arxiv.org/abs/2310.16028)
by Zhou et al. in Haskell.

RASP-L is a domain-specific language that models the behavior of Transformers
on algorithmic tasks.

```haskell
-- | Θ(n^2) time, Θ(n) space sorting algorithm
sort :: [Token] -> [Token]
sort xs = decode endOfSequence raspSort (xs ++ [minBound]) (fromIntegral (length xs))
  where
    endOfSequence = maximum xs
    raspSort :: Sequence -> Sequence
    -- Sort by looking for the smallest element greater than the current token
    raspSort s = minKQV s s gt s
```
