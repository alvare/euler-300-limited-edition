import Text.Regex
import Data.List
import Data.Ord
import Data.Char

mapIndexed f l = snd $ mapAccumL (\acc x -> (acc + 1, f acc x)) 0 l

wordToInt w = foldl (\acc x -> acc + ord x - 64) 0 w

processList l = mapIndexed (\i x -> wordToInt x * (i + 1)) l

main = print . sum . processList . sort . tail . init . splitRegex (mkRegex "(\",\")|(\")") =<< readFile "names.txt"
