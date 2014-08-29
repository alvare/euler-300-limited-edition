spiral :: Int -> Int
spiral 1 = 1
spiral k
    | odd k = prev + n + (n - j) + (n - 2*j) + (n - 3*j)
    | otherwise = error "No even numbers please."
    where prev = spiral (k - 2)
          n = k ^ 2
          j = k - 1

main = do
    to <- fmap read getLine :: IO Int
    putStrLn . show $ spiral to
