import qualified Data.Set as S
import Data.Ord
import Data.List
import Data.Numbers.Primes
import System.Random

type NInt = Int

quadratic :: (Num a) => a -> a -> a -> a
quadratic a b n = n^2 + a*n + b

semiPrime x s = (abs x) `S.member` s || (isPrime . fromIntegral . abs $ x)

consPrim :: NInt -> NInt -> S.Set NInt -> (S.Set NInt, NInt)
consPrim a b primes = loop a b 0 primes
    where loop x y n s = if semiPrime new s then loop x y (succ n) ((abs new) `S.insert` s)
                         else (s, n)
              where new = quadratic x y n

quadLen :: S.Set NInt -> [(NInt, NInt)] -> [NInt] -> (S.Set NInt, [NInt])
quadLen primes [] list = (primes, list)
quadLen primes ((a, b):xs) list = if semiPrime b primes then quadLen primes' xs ((n:list) `seq` (n:list))
                                  else quadLen primes xs list
                              where primes' = S.union primes'' ((abs b) `S.insert` primes)
                                    (primes'', n) = consPrim a b primes

n = 100 :: NInt
pairs = [(a, b) | a <- [-n..n], b <- [-n..n]]

main = do
    putStrLn . show . maximumBy (comparing snd) . zip pairs $ lens
    putStrLn . show $ set
    where (set, lens) = quadLen S.empty pairs []
