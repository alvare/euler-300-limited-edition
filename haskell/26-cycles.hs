import Data.List
import Data.Ord
import qualified Data.Set as S

greatMult a b = last $ filter (\x -> x*b <= a) [0..a]

divide' :: Int -> Int -> [Int] -> Bool -> S.Set Int -> Int
divide' d s q zero r
    | d == 0 = 0
    | d < s && not zero = divide' (d * 10) s q True r
    | S.member new_d r = S.size r
    | otherwise = divide' new_d s (q ++ [new_q]) False (S.insert new_d r)
    where new_q = greatMult d s
          new_d = d - s * new_q

cycleLength :: Int -> Int
cycleLength b = divide' 1 b [] False S.empty

longestCycleBelow x = maximumBy (comparing snd) $ zip ns $ map cycleLength ns
    where ns = [2..x]

main = putStrLn . show $ longestCycleBelow 1000
