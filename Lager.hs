import Data.List
import Control.Monad
--import ExprVar

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

bag :: Eq a => [a] -> [(a, Int)]
bag []     = []
bag xs     = zip (listOfNonDup xs) (map (\x -> numOccurrences x xs) (listOfNonDup xs))
    where listOfNonDup xs = removeDuplicates xs 

readlinesInput :: IO (Int)
readlinesInput = do 
    a1 <- getLine
    return (toInt a1)
  where toInt a = (read a :: Int)

readlinesInputA :: IO (Int)
readlinesInputA = do 
    n  <- readLn
    xs <- replicateM n readLn
    return $ sum xs

readlinesInputB :: IO [Int]
readlinesInputB = helpB []

helpB :: [Int] -> IO[Int]
helpB xs = do
   x <- readLn
   if (x == 0) then 
    return $ (sort (x:xs))
   else 
    helpB (x:xs)

bubblesort :: [Int] -> [Int]
bubblesort (x:xs) = helpbubble [] xs x
  where helpbubble tested []     y = tested
        helpbubble tested (x:xs) y | x > y     = helpbubble (tested ++ [y]) xs (x)
                                   | otherwise = helpbubble (tested ++ [x]) xs (y)

repeat :: IO Bool -> IO () -> IO ()
repeat x y = undefined

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
size exp = helpsize exp 0
  where helpsize (Lit a)     n = n + 1 
        helpsize (Add e1 e2) n = helpsize e1 (n) + helpsize e2 (n) + 1
        helpsize (Sub e1 e2) n = helpsize e1 (n) + helpsize e2 (n) + 1