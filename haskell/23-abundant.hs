import Data.Numbers
import Data.Maybe
import Data.Set as Set hiding (filter)

isAbundant :: Integer -> Bool
isAbundant n = n < (-n + factorSum n)

abundants :: Set Integer
abundants = Set.fromList $ filter isAbundant [12..28123]

sumOfTwo :: Set Integer -> Integer -> Bool
sumOfTwo set n = loop n n
    where loop n s = case (lookupLT s set) of
              Just next -> if (n - next) `member` set then True else loop n next
              Nothing -> False

main = putStrLn . show . sum $ [1..23] ++ (filter (not . sumOfTwo abundants) [24..30000])
