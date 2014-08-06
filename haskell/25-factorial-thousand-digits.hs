import Data.List

fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

thousand_digits = 10^999

first_over_1000 = head $ dropWhile (<thousand_digits) fibs

main = putStrLn . show $ elemIndex first_over_1000 fibs
