import Data.Ord
import Data.List
import Data.Numbers.Primes
import System.Random

-- lol
consecutivePrimes :: Integer -> Integer -> [Integer]
consecutivePrimes a b = takeWhile (\x -> fst $ isProbablyPrime x (mkStdGen 5)) . map (quadratic a b) $ [0..]

quadratic :: (Num a) => a -> a -> a -> a
quadratic a b n = n^2 + a*n + b

consecutiveLength :: (Integer, Integer) -> Int
consecutiveLength (a, b)
    | isPrime b = length $ consecutivePrimes a b
    | otherwise = 0

n = 1000 :: Integer
pairs = [(a, b) | a <- [-n..n], b <- [-n..n]]

main = putStrLn . show . maximumBy (comparing snd) . zip pairs . map consecutiveLength $ pairs
