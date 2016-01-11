sumsq :: Int -> Int
sumsq 1 = 1
sumsq n = n * n + sumsq (n-1)

