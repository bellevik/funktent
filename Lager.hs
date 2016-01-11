import Data.List


-- Sumsq n returns 1*1 + 2*2 + ... + n*n

sumsq :: Int -> Int
sumsq 0 = 0
sumsq n = sumsq (abs (n)-1) + n*n

-- fib n computes the nth Fibonacci number

fib :: Int -> Int 
fib n | n `elem` [0, 1] = 1
      | otherwise       = fib (n-1) + fib (n-2)

-- avoiding duplicates
duplicates :: Eq a => [a] -> Bool
duplicates []     = False
duplicates (x:xs) | x `elem` xs = True
                  | otherwise   = duplicates xs

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates []     = []
removeDuplicates xs     = help xs xs
   where help []     news = news
         help (x:xs) news | x `elem` xs = help xs (newArray x news)
                          | otherwise   = help xs news
         newArray x xs = delete x xs