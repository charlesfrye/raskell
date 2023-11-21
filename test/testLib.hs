import Lib
import Test.QuickCheck (quickCheck)

constTrue :: Int -> Bool
constTrue _ = True

main :: IO ()
main = do quickCheck constTrue
