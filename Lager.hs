-- Sumsq n returns 1*1 + 2*2 + ... + n*n

sumsq :: Int -> Int
sumsq n | n == 1    = 1
        | otherwise = sumsq (abs (n)-1) + n*n