sumsq :: Int -> Int
sumsq 1 = 1
sumsq n = n * n + sumsq (n-1)

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

duplicates :: Eq a => [a] -> Bool
duplicates []     = False
duplicates (x:xs) = x `elem` xs || duplicates xs