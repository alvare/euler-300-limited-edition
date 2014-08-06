import Data.Maybe
import Data.Permute

allPermutations Nothing = []
allPermutations (Just perm) = perm : (allPermutations $ next perm)

permutations = allPermutations . Just . permute $ 10

main = putStrLn . show $ permutations !! 999999
