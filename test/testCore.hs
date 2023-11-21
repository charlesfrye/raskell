import Control.Monad (when)
import Core
import Test.QuickCheck (Property, Result (..), quickCheckResult, (===), (==>))

any' :: Predicate
any' _ _ = True

prop_maxKQV_is_maximum :: Sequence -> Property
prop_maxKQV_is_maximum xs = (length xs > 1) ==> scanl1 max xs === maxKQV xs xs any' xs

prop_minKQV_is_minimum :: Sequence -> Property
prop_minKQV_is_minimum xs = (length xs > 1) ==> scanl1 min xs === minKQV xs xs any' xs

prop_selWidth_is_num_true :: Selector -> Property
prop_selWidth_is_num_true s = (length s > 1) ==> map numTrue s === selWidth s
  where
    numTrue = fromIntegral . length . filter id

main :: IO ()
main = do
  results <-
    sequence
      [ quickCheckResult prop_maxKQV_is_maximum,
        quickCheckResult prop_minKQV_is_minimum,
        quickCheckResult prop_selWidth_is_num_true
      ]
  let failed = not (all isSuccess results)
  when failed $ error "Some tests failed"

isSuccess :: Result -> Bool
isSuccess Success {} = True
isSuccess _ = False
