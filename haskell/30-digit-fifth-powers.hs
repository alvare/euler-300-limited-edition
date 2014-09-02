import Data.Sequence ((|>), empty, Seq(..))
import qualified Data.Foldable as F

fromNumber :: Int -> Seq Int
fromNumber 0 = empty
fromNumber x = fromNumber head |> tail
    where head = x `div` 10
          tail = x `mod` 10

digitSumsPower p = F.foldl (\b a -> b+(a^p)) 0 . fromNumber

main = putStrLn $ show [x | x <- [1000..999999], x == digitSumsPower 5 x]
