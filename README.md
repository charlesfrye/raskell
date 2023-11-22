# `raskell`: RASP-L in Haskell

Implements the RASP-L language from
[What Algorithms Can Transformers Learn](https://arxiv.org/abs/2310.16028)
by Zhou et al. in Haskell.

Includes the entire RASP-L Core from Listing 2 (`src/Core.hs`),
some of the RASP-L Standard Library from Listing 3 (`src/Lib.hs`),
and some sample programs (in `app/`)

RASP-L is a domain-specific language that models the behavior of Transformers
on algorithmic tasks.

Here it is running just about the worst sorting algorithm imaginable:

```haskell
-- | Θ(n^2) time sorting algorithm
sort :: [Token] -> [Token]
sort xs = sample endOfSequence raspSort (xs ++ [startOfSequence]) numTokens
  where
    startOfSequence = minBound
    endOfSequence = maximum xs
    raspSort :: Sequence -> Sequence
    -- Sort by looking for the smallest element greater than the current token
    raspSort s = minKQV s s gt s
    numTokens = fromIntegral (length xs)
```
