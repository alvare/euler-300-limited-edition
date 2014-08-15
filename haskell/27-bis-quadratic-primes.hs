import qualified Data.Set as S
import Data.Ord
import Data.List
import Data.Numbers.Primes
import System.Random

type NInt = Integer

quadratic :: (Num a) => a -> a -> a -> a
quadratic a b n = n^2 + a*n + b

semiPrime x s = x `S.member` s || isPrime x

consPrim :: NInt -> NInt -> S.Set NInt -> (S.Set NInt, NInt)
consPrim a b primes = loop a b 0 primes
    where loop x y n s = if semiPrime new s then loop x y (succ n) (S.insert new s)
                         else (s, n)
              where new = quadratic x y n

quadLen :: S.Set NInt -> [(NInt, NInt)] -> [NInt] -> [NInt]
quadLen primes [] list = list
quadLen primes ((a, b):xs) list = if semiPrime b primes then quadLen primes' xs ((n:list) `seq` (n:list))
                                  else quadLen primes xs list
                              where primes' = S.unions [primes'', primes, S.singleton b]
                                    (primes'', n) = consPrim a b primes

n = 1000 :: NInt
pairs = [(a, b) | a <- [-n..n], b <- [-n..n]]

main = putStrLn . show . maximumBy (comparing snd) . zip pairs . quadLen S.empty pairs $ []
