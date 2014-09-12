-- from https://andrew.neitsch.ca/publications/m496pres1.nb.pdf

change :: (Int, Int) -> Int
change (n, k) | n < 0 || k < 1 = 0
change (0, _) = 1
change (n, k) = (change (n, (k - 1))) + (change ((n - (coins !! (k - 1))), k))

coins = [1, 2, 5, 10, 20, 50, 100, 200]

main = putStrLn $ show $ change (200, length coins)
