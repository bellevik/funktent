import Data.Maybe
import Data.List

myName :: IO ()
myName = do
    putStrLn "Lager"

-- 1ai.*****************************************************************************
maxRainfall :: Int -> Float
maxRainfall n = foldr (\x y -> max y $ rainfall x) 0 [1..n]


rainfall :: Int -> Float
rainfall f = rain !! ((f-1) `mod` 22)

rain = [ 3.0, 4.0, 123.0 , 2.0, 3.0, 5.0, 4.0, 56.0, 6.0, 4.0, 43.0, 32.0, 189.0, 65.0, 67.0, 8.0, 9.0, 189.0, 43.0, 45.0, 76.0, 200.0]

-- 1aii.*****************************************
maxRainfalla :: Int -> Float
maxRainfalla n = helpMaxR n 0
  where helpMaxR 0 maxV = maxV
        helpMaxR n maxV | rainfall n > maxV = helpMaxR (n-1) (rainfall n)
                        | otherwise         = helpMaxR (n-1) maxV       


-- 1b *******************************************
maxWeeks :: Int -> [Int]
maxWeeks n = weeks n [] (maxRainfall n)
  where weeks 0 xs a = xs
        weeks n xs a | rainfall n == a = weeks (n-1) (n:xs) a
                     | otherwise       = weeks (n-1) xs     a


maxWeekstry :: Int -> [Int]
maxWeekstry n = [i | i <- [1..n], rainfall i == maximum [ rainfall l | l <- [1..n]]]


-- 2a ******************************************************************************
--  fa :: (a -> Bool) -> (a -> Bool) -> a -> Bool
--  fa x y z = x z && y z

-- fb :: Num a => a -> Maybe a
-- fb m = Just (m+1)

-- fc :: (Num a, Ord b) => [b] -> a
-- fc (x:y:xs) | x > y = 999
-- fc _                = 0

-- fd :: Eq a => [[a]] -> [b] -> Bool
-- fd (x:xs) (y:ys) = x == ys
-- fd []     ys     = ys == []

-- fe :: Monad a => m a -> [a] -> m [a]
-- fe x y = do
--           z <- x
--           return $ z:y

-- 2b *******************************************
-- ff x y = do
--          c <- readFile x
--          d <- readFile c
--          putStr (c ++ "\n" ++ d)

-- ff x y = readFile x >>= \c -> readFile c >>= \d-> putStr ( c ++ "\n" ++ d)

-- 2c *******************************************
--fg a tb tc td
--   = case lookup a tb of
--          Nothing -> Nothing
--          Just b -> case lookup b tc of
--                         Nothing -> Nothing
--                         Just c -> case lookup c td of
--                                        Nothing -> Nothing
--                                        Just d -> Just [b,c,d]
test2c :: a -> Maybe a
test2c a = do
   x <- Just a
   Just x

--fg' a tb tc td = do
--      a <- lookup a tb
--      b <- lookup a tc
--      c <- lookup a td
--      Just [a,b,c]

-- 3 *******************************************************************************

data Bag a = EmptyBag | Node a Int (Bag a) (Bag a)
 deriving Show

emptyBag :: Bag a
emptyBag = EmptyBag

bag :: Num a => Bag a
bag = (Node 9 2 (Node 2 1 EmptyBag EmptyBag) (Node 10 1 EmptyBag EmptyBag))

-- 3a********************************************
-- När man vill göma implementationen av datatypen ( det gör att man inte exporterar konstruktorn)

-- 3b********************************************

bcount :: Ord a => a -> Bag a -> Int
bcount a EmptyBag         = 0
bcount a (Node x y left right) |  x == a = y
                               |  x > a  = bcount a left
                               |  x < a  = bcount a right

-- 3c********************************************
bagToList :: Bag a -> [a]
bagToList EmptyBag                  = []
bagToList (Node v count left right) = bagToList left ++ (replicate count v) ++ bagToList right

-- 3d********************************************
prop_Bag :: Ord a => Bag a -> Bool
prop_Bag EmptyBag                 = True
prop_Bag (Node v cunt left right) = all (<v) (bagToList left) &&
                                    all (>v) (bagToList right) &&
                                    all prop_Bag [left,right] && cunt >= 0

-- 3e********************************************
bchange :: Ord a => a -> Int -> Bag a -> Maybe (Bag a)
bchange value nbr bag | okcount >= 0 = Just $ newBag bag 
                      | otherwise = Nothing
    where okcount = bcount value bag + nbr
          newBag EmptyBag                 = (Node value nbr EmptyBag EmptyBag)
          newBag (Node v cunt left right) | v == value = (Node v okcount left right)
                                          | value < v  = (Node v cunt (newBag left) right)
                                          | value > v  = (Node v cunt left (newBag right))


-- 4 *******************************************************************************
type Pegs = Int
type NumDiscs = Int
type Disc = Int
data Hanoi = Hanoi Pegs NumDiscs [[Disc]]
  deriving (Eq,Show)

hanoitest1 :: Hanoi
hanoitest1 = (Hanoi 3 5 [[1,2,3,4,5], [] ,[]])

hanoitest2 :: Hanoi
hanoitest2 = (Hanoi 3 5 [[3,4,5], [] ,[1,2]])

hanoitest3 :: Hanoi
hanoitest3 = (Hanoi 3 5 [[1,3,4,5], [] ,[1,2]])

hanoitest4 :: Hanoi
hanoitest4 = (Hanoi 3 5 [[3,5,4], [] ,[1,2]])

-- 4a********************************************

prop_wellFormedHanoi :: Hanoi -> Bool
prop_wellFormedHanoi (Hanoi p d ps) = p == (length ps) && d == (nbrOfPegs ps) && (all (\a -> a == sort a) ps)
    where nbrOfPegs []     = 0
          nbrOfPegs (x:xs) = nbrOfPegs xs + length x

-- 4b********************************************
emptyHanoi :: Pegs -> NumDiscs -> Hanoi
emptyHanoi p nbr = Hanoi p nbr $ replicate p []

startHanoi :: Pegs -> NumDiscs -> Hanoi
startHanoi p nbr = (Hanoi p nbr start)
   where start = startPeg:startEmpty
         startPeg = [1..nbr]
         startEmpty = replicate (p-1) [] 


-- 4b********************************************

addDisc :: Disc -> Pegs -> Hanoi -> Hanoi 
addDisc d p (Hanoi peg dis xs) | isOk = Hanoi peg dis newArr
                               | otherwise = Hanoi peg dis xs
   where isOk = p < peg && arrayPosOk (xs !! p)
         arrayPosOk [] = True
         arrayPosOk as = as !! 0 > d  
         newArr = (take p xs) ++ ((d:(xs !! p)):(drop (p+1) xs))

removeDisc :: Pegs -> Hanoi -> Maybe (Disc, Hanoi)
removeDisc p (Hanoi peg dis xs) | length (xs!!p) > 0 = Just (p, newHanoi)
                                | otherwise = Nothing
   where newHanoi = Hanoi peg dis (array xs)
         array as = take p as ++ ((drop 1 (as!!p)):(drop (p+1) as ) )