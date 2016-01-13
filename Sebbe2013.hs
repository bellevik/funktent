import Data.List
import Test.QuickCheck

myName :: IO ()
myName = do
    putStrLn "Sebbe"

-- ##### Question 1 ##### --
-- a
rainfall :: Int -> Float
rainfall f = rain !! ((f-1) `mod` 22)

rain = [ 3.0, 4.0, 123.0 , 2.0, 3.0, 5.0, 4.0, 56.0, 6.0, 4.0, 43.0, 32.0, 189.0, 65.0, 67.0, 8.0, 9.0, 189.0, 43.0, 45.0, 76.0, 200.0]
--    i
maxRainfall :: Int -> Float
maxRainfall n = foldr (\a b -> max (rainfall a) b) 0 [1..n]

maxRainfall' :: Int -> Float
maxRainfall' n = maxRainfall'' 0 n

maxRainfall'' :: Float -> Int -> Float
maxRainfall'' sofar 0 = sofar
maxRainfall'' sofar n = maxRainfall'' (max sofar (rainfall n)) (n-1)

-- b
maxWeeks :: Int -> [Int]
maxWeeks n = [i | i <- [1..n], rainfall i == maximum [rainfall j | j <- [1..n]] ]

test :: Int -> [Int]
test n = [i | i <- [1..n]]

-- ##### Question 2 ##### --
-- a

{-fa :: (t -> Bool) -> (t -> Bool) -> t -> Bool
fa x y z = x y && y z

fb :: Num a => a -> Maybe a
fb m = Just (m+1)

fc :: (Num a, Ord b) => [b] -> a
fc (x:y:xs) | x > y = 999
fc _                = 0

fd :: Eq a => [[a]] -> [a] -> Bool
fd (x:xs) (y:ys) = x == ys
fd []     ys     = ys == []

fe :: Maybe m => m a -> [a] -> m [a]
fe x y = do
    z <- x
    return $ z:y-}

-- b
{-ff x y = do
    c <- readFile x
    d <- readFile c
    putStr (c ++ "\n" ++ d)

-- c
fg a tb tc td
    = case lookup a tb of
        Nothing -> Nothing
        Just b -> case lookup b tc of
            Nothing -> Nothing
            Just c -> case lookup c td of
                Nothing -> Nothing
                Just d -> Just [b,c,d]

fg' a tb tc td = do -}
    


-- ##### Question 3 ##### --
data Bag a = EmptyBag | Node a Int (Bag a) (Bag a) deriving Show

-- a
emptyBag :: Bag a
emptyBag = EmptyBag

testBag :: Num a => Bag a
testBag = (Node 9 2 EmptyBag (Node 10 1 EmptyBag EmptyBag))

-- b
bcount :: Ord a => a -> Bag a -> Int
bcount node EmptyBag         = 0
bcount node (Node n i b1 b2) | n == node = i
                             | n > node  = (bcount node b1)
                             | n < node  = (bcount node b2)

-- c
bagToList :: Bag a -> [a]
bagToList EmptyBag         = []
bagToList (Node n i b1 b2) = (bagToList b1) ++ (replicate i n) ++ (bagToList b2)

-- d
prop_Bag :: Ord a => Bag a -> Bool
prop_Bag EmptyBag = True
prop_Bag (Node n i b1 b2) = all (<n) (bagToList b1) &&
                            all (>n) (bagToList b2) &&
                            all prop_Bag [b1,b2] && i >= 0

-- e
bchange' :: Ord a => a -> Int -> Bag a -> Maybe (Bag a)
bchange' _ _ EmptyBag           = Nothing
bchange' n i (Node n2 i2 b1 b2) | n == n2 = Just (Node n (i+i2) b1 b2)
bchange' n i (Node n2 i2 b1 b2) | n < n2  = bchange' n i b1
bchange' n i (Node n2 i2 b1 b2) | n > n2  = bchange' n i b2

bchange :: Ord a => a -> Int -> Bag a -> Maybe (Bag a)
bchange n i b | newcount >= 0 = Just (bchange' b)
    where 
        newcount = bcount n b + i
        bchange' EmptyBag = Node n i EmptyBag EmptyBag
        bchange' (Node e c b1 b2) 
            | e == n = Node e newcount b1 b2
            | n < e  = Node e c (bchange' b1) b2
            | n > e  = Node e c b1 (bchange' b2)

-- ##### Question 4 ##### --
type Pegs     = Int
type NumDiscs = Int
type Disc     = Int
data Hanoi    = Hanoi Pegs NumDiscs [[Disc]] deriving (Eq, Show)

hanoitest1 :: Hanoi
hanoitest1 = (Hanoi 3 5 [[1,2,3,4,5], [] ,[]])

hanoitest2 :: Hanoi
hanoitest2 = (Hanoi 3 5 [[3,4,5], [] ,[1,2]])

hanoitest3 :: Hanoi
hanoitest3 = (Hanoi 3 5 [[1,3,4,5], [] ,[1,2]])

hanoitest4 :: Hanoi
hanoitest4 = (Hanoi 3 5 [[3,5,4], [] ,[1,2]])

-- a
prop_wellFormedHanoi :: Hanoi -> Bool
prop_wellFormedHanoi (Hanoi p d ps) = length ps == p
                                    && all (\p -> (sort p == p)) ps
                                    && sort (concat ps) == [1..d]

-- b
emptyHanoi :: Pegs -> NumDiscs -> Hanoi
emptyHanoi np nd = Hanoi np nd (replicate np [])

-- c
addDisc :: Disc -> Pegs -> Hanoi -> Hanoi
addDisc d p (Hanoi pegs nd discs) | p >= (length discs) = (Hanoi pegs nd discs)
                                  | otherwise = (Hanoi pegs nd (newDiscs d p discs))
    where
        newDiscs d1 p1 discs1 = insertAt discs1 (d1:(discs1 !! p1)) p1
        insertAt xs x n = fst (splitAt n xs) ++ [x] ++ tail (snd (splitAt n xs))

removeDisc :: Pegs -> Hanoi -> Maybe (Disc, Hanoi)
removeDisc p (Hanoi pegs nd ds) | p > pegs || (length (ds !! p) == 0) = Nothing
                                | otherwise = Just (head (ds !! p), (Hanoi pegs nd (newDiscs p ds)))
    where
        newDiscs p1 discs1 = insertAt discs1 (tail (discs1 !! p1)) p1
        insertAt xs x n = fst (splitAt n xs) ++ [x] ++ tail (snd (splitAt n xs))

-- d
{-instance Arbitrary Hanoi where
    arbitrary = do
        pegs <- elements [3..5]
        discs <- elements [2..10]
        dist <- distribute pegs [1..discs] (replicate pegs [])
        return (Hanoi pegs discs (map sort dist))-}