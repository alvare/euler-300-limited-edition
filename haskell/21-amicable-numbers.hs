criba :: [Int] -> [Int]
criba [] = []
criba (x:xs) = x : criba (filter noMultiploDeX xs)
    where noMultiploDeX m = mod m x /= 0

primosHasta :: Int -> [Int]
primosHasta n = 2 : criba [3,5..n]

muchosP = primosHasta 10000

divisible x y = mod x y == 0

divisores n = filter (divisible n) [1 .. n - 1]

amicable n = sum (divisores amigo) == n && n /= amigo
    where amigo = sum (divisores n)

main = sum $ filter amicable [1..10000]
