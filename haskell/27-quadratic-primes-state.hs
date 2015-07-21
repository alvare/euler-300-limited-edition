import qualified Data.Set as S
import Debug.Trace
import Data.Ord
import Data.List
import Data.Numbers.Primes hiding (primes)
import Control.Monad.Trans.State.Strict

quadratic :: Int -> (Int, Int) -> Int
quadratic n (a, b) = n^(2 :: Int) + a*n + b

semiPrime :: Int -> State (S.Set Int) Bool
semiPrime x = do
    let ax = abs x
    primes <- get
    if ax `S.member` primes
        then return True
        else if (isPrime . fromIntegral $ ax)
            then modify (S.insert ax) >> return True
            else return False

quadState :: Int -> (Int, Int) -> State (S.Set Int) Int
quadState count pair = do
    isx <- semiPrime $ fst pair
    isy <- semiPrime $ snd pair
    if not $ isx || isy
        then return 0
        else do
            let z = quadratic count pair
            isz <- semiPrime z
            if isz
                then quadState (count + 1) pair
                else return count

n = 1000 :: Int
pairs = [(a, b) | a <- [-n..n], b <- [-n..n]]

main = do
    print result
    where lens = evalState (mapM (quadState 0) pairs) S.empty
          result = maximumBy (comparing snd) $ zip pairs lens
