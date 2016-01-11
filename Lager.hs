-- Sumsq n returns 1*1 + 2*2 + ... + n*n

sumsq :: Int -> Int
sumsq 0 = 0
sumsq n = sumsq (abs (n)-1) + n*n

-- fib n computes the nth Fibonacci number

fib :: Int -> Int 
fib n | n `elem` [0, 1] = 1
      | otherwise       = fib (n-1) + fib (n-2)