import Data.Char

sumsq :: Integer -> Integer
sumsq counter = sumsq' counter 0

sumsq' :: Integer -> Integer -> Integer
sumsq' counter sums | counter == 0 = sums
sumsq' counter sums = sumsq' (counter - 1) (sums + counter * counter)

sumsq2 :: Int -> Int
sumsq2 1 = 1
sumsq2 val = val * val + sumsq2 (val - 1)

fib :: Integer -> Integer
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)

duplicates :: [Integer] -> Bool
duplicates [] = False
duplicates (x:xs) = x `elem` xs || duplicates xs

occursIn :: Eq a => a -> [a] -> Bool
occursIn val list = val `elem` list

allOccurIn :: Eq a => [a] -> [a] -> Bool
allOccurIn [] _ = True
allOccurIn (x:xs) list = occursIn x list && allOccurIn xs list

sameElements :: Eq a => [a] -> [a] -> Bool
sameElements list1 list2 | length list1 == length list2 = 
