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

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates xs = removeDuplicates' xs []

removeDuplicates' :: Eq a => [a] -> [a] -> [a]
removeDuplicates' []     ys = ys
removeDuplicates' (x:xs) ys | x `notElem` ys = removeDuplicates' xs (ys ++ [x])
                            | otherwise = removeDuplicates' xs ys

occursIn :: Eq a => a -> [a] -> Bool
occursIn x xs = x `elem` xs

allOccursIn :: Eq a => [a] -> [a] -> Bool
allOccursIn xs ys = all (`elem` ys) xs

sameElements :: Eq a => [a] -> [a] -> Bool
sameElements xs ys = allOccursIn xs ys && allOccursIn ys xs

numOccurrences :: Eq a => a -> [a] -> Int
numOccurrences x xs = length (filter (== x) xs)

bag :: Eq a => [a] -> [(a, Int)]
bag (x:xs) = undefined

testRep :: IO Int -> IO ()
testRep i = putStrLn i

repeat :: IO Bool -> IO () -> IO ()
repeat = undefined