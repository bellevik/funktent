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

-- Occurrences in Lists
occursIn :: Eq a => a -> [a] -> Bool
occursIn x xs = (or . map (\y -> y == x)) xs

allOccursIn :: Eq a => [a] -> [a] -> Bool
allOccursIn xs ys = (and . map (\a -> occursIn a ys)) xs 

sameElements :: Eq a => [a] -> [a] -> Bool
sameElements xs ys = allOccursIn xs ys && allOccursIn ys xs

numOccurrences :: Eq a => a -> [a] -> Int
numOccurrences x xs = (length . filter (==True) . map (\y -> y == x)) xs
