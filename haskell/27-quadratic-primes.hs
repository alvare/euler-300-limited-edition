import qualified Data.Set as S
import Data.Ord
import Data.List
import Data.Numbers.Primes hiding (primes)

type NInt = Int

quadratic :: NInt -> NInt -> NInt -> NInt
quadratic a b n = n^(2 :: NInt) + a*n + b

semiPrime :: NInt -> S.Set NInt -> Bool
semiPrime x s = (abs x) `S.member` s || (isPrime . fromIntegral . abs $ x)

consPrim :: NInt -> NInt -> S.Set NInt -> (S.Set NInt, NInt)
consPrim a b primes = loop a b 0 primes
    where loop x y n s = if semiPrime new s then loop x y (n + 1) ((abs new) `S.insert` s)
                         else (s, n)
              where new = quadratic x y n

quadLen :: S.Set NInt -> [(NInt, NInt)] -> [NInt] -> [NInt]
quadLen primes [] list = list
quadLen primes ((a, b):xs) list = if semiPrime b primes then quadLen primes' xs ((n:list) `seq` (n:list))
                                  else quadLen primes xs ((0:list) `seq` (0:list))
                              where primes' = (abs b) `S.insert` primes''
                                    (primes'', n) = consPrim a b primes

n = 1000 :: NInt
pairs = [(a, b) | a <- [-n..n], b <- [-n..n]]

main = do
    putStrLn $ show result
    where lens = quadLen S.empty pairs []
          result = maximumBy (comparing snd) . zip pairs $ reverse lens
