import Data.Char
import Data.List

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

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates []     = []
removeDuplicates xs     = help xs xs
   where help []     news = news
         help (x:xs) news | x `elem` xs = help xs (newArray x news)
                          | otherwise   = help xs news
         newArray x xs = delete x xs

occursIn :: Eq a => a -> [a] -> Bool
occursIn x xs = x `elem` xs

allOccursIn :: Eq a => [a] -> [a] -> Bool
allOccursIn xs ys = and (map (`elem` ys) xs)

sameElements :: Eq a => [a] -> [a] -> Bool
sameElements xs ys = allOccursIn xs ys && allOccursIn ys xs

numOccurrences :: Eq a => a -> [a] -> Int
numOccurrences x xs = length (filter (\y -> y == x) xs)

bag :: String -> [(Char, Int)]
bag str = zip (nonDup str) (map (\x -> numOccurrences x str) (nonDup str))
            where nonDup string = removeDuplicates string


data Expr = Lit Int
          | Add Expr Expr
          | Sub Expr Expr

eval :: Expr -> Int
eval (Lit n)     = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2

showExpr :: Expr -> String
showExpr (Lit n)     = show n
showExpr (Add e1 e2) = "(" ++ showExpr e1 ++ "+" ++ showExpr e2 ++")"
showExpr (Sub e1 e2) = "(" ++ showExpr e1 ++ "-" ++ showExpr e2 ++")"


size :: Expr -> Int








