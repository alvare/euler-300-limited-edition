import Data.Char
import Text.Printf
import Control.Exception
import System.CPUTime
 
-- Utility
time :: IO t -> IO t
time a = do
    start <- getCPUTime
    v <- a
    end   <- getCPUTime
    let diff = (fromIntegral (end - start)) / (10^12)
    printf "Computation time: %0.3f sec\n" (diff :: Double)
    return v

timeTest f = time $ putStrLn $ show $ f

-- Binary Tree
data Tree = Parent Int Tree Tree | Child Int deriving (Show)

add :: Tree -> Int
add (Parent n l r) = n + add l + add r
add (Child n) = n

-- Strong Password
strong :: String -> Bool
strong pswd = any isNumber pswd && any isUpper pswd && any isLower pswd && length pswd >= 15

-- 3 - primes and factors

naturalSqrt :: (Integral a) => a -> a
naturalSqrt x = floor (sqrt (fromIntegral x))

prime :: (Integral a) => a -> Bool
prime n = check n (naturalSqrt (n - 1))
    where check n b = if b > 1 then not (mod n b == 0) && check n (b - 1) else True

firstDivisor x n = if mod x n == 0 then n else firstDivisor x (n + 1)

divide x n
    | not (mod x n == 0) = divide x (firstDivisor x 2) 
    | div x n == 1 = x
    | otherwise = divide (div x n) n

-- 4 - palindromes
nList x xs
    | div x 10 == 0 = x : xs
    | otherwise = nList (div x 10) (mod x 10 : xs)

nListToInt [] _ = 0
nListToInt (x:xs) acc = (x * (10 ^ acc)) + (nListToInt xs (acc + 1))

nReverse x = nListToInt (nList x []) 0

palCheck x
    | x == nReverse x = True
    | otherwise = False

biggerWinner start end test = maximum $ filter test [x * y | x <- options, y <- options]
    where options = reverse [start..end]

-- solucion de loco
palindrome = maximum (filter (isPalindrome) [a*b | a <- [100..999], b <- [a..999]])
	where
		isPalindrome a = a == merge (makeList a)
			where
				merge [] = 0
				merge (x:xs) = x + merge (map (\x -> x*10) xs)
				makeList 0 = []
				makeList a = (makeList (div a 10)) ++ [(mod a 10)]

-- Problem 5 - Smallest multiple
divisible a b = mod a b == 0
multipliesRange x [] = True
multipliesRange x (y:ys) = divisible x y && multipliesRange x ys

smallestMultiple (x:xs) range
    | multipliesRange x range = x
    | null xs = 0
    | otherwise = smallestMultiple xs range

-- Problem 8 - Largest product in a series
n1000list = nList 7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450 []

p8 biggest list
    | length list < 5 = biggest
    | p <= biggest = p8 biggest (tail list)
    | p > biggest = p8 p (tail list)
    where p = product $ take 5 $ list

-- Sieve of Eratosthenes
primesBelow' n result numbers
    | p^2 < n = primesBelow' n (p : result) (filter (\x -> mod x p /= 0) (tail numbers))
    | otherwise = result ++ numbers
    where p = head numbers

primesBelow n = 2 : primesBelow' n [] [3,5..n]

-- 12 - Highly divisible triangular number

triangleNumbers = [sum [1..x] | x <- [1..]]
divisors n = [x | x <- [1..n], mod n x == 0]

solution12 (x:xs) limit
    | n > limit = x
    | otherwise = solution12 xs limit
    where n = length $ divisors x

-- 14 - Collatz
collatz x
    | even x = div x 2
    | otherwise = x*3+1

scollatz x
    | p > 1 = x : scollatz p
    | otherwise = x:[1]
    where p = collatz x

-- 18 - max path I
data18 = [[75],
         [95, 64],
         [17, 47, 82],
         [18, 35, 87, 10],
         [20, 04, 82, 47, 65],
         [19, 01, 23, 75, 03, 34],
         [88, 02, 77, 73, 07, 63, 67],
         [99, 65, 04, 28, 06, 16, 70, 92],
         [41, 41, 26, 56, 83, 40, 80, 70, 33],
         [41, 48, 72, 33, 47, 32, 37, 16, 94, 29],
         [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14],
         [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57],
         [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48],
         [63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31],
         [04, 62, 98, 27, 23, 09, 70, 98, 73, 93, 38, 53, 60, 04, 23]]

main = time $ putStrLn $ show $ sum $ primesBelow 2000000
