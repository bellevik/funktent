sumsq :: Int -> Int
sumsq 1 = 1
sumsq n = n * n + sumsq (n-1)

fib :: Int -> Int
fib 1 = 1
fib 2 = 1
fib n = fib (n-1) + fib (n-2)
