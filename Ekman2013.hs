import Data.List
import Data.Maybe

myName :: IO ()
myName = do
	putStrLn "Ekman"

-- ##### Question 1 ##### --
-- a

rainfall :: Int -> Float
rainfall f = rain !! ((f-1) `mod` 22)

rain = [ 3.0, 4.0, 123.0 , 2.0, 3.0, 5.0, 4.0, 56.0, 6.0, 4.0, 43.0, 32.0, 189.0, 65.0, 67.0, 8.0, 9.0, 189.0, 43.0, 45.0, 76.0, 200.0]

maxRainfall :: Int -> Float
maxRainfall n = foldr (\a b -> max (rainfall a) b ) 0 [1..n]

maxRainfall' :: Int -> Float
maxRainfall' num = maxRainfall num

maxRain :: Int -> Float -> Float
maxRain 0 val = val
maxRain num val = maxRain (num - 1) (max val (rainfall num))
-- b

maxWeeks :: Int -> [Int]
maxWeeks num = [k | k <- [1..num], (rainfall k) == maximum [rainfall x | x <- [1..num]]]


-- ##### Question 2 ##### --
-- a
--fa :: (a -> Bool) -> (a -> Bool) -> a -> Bool
--fa x y z = x z && y z

--fb :: Num a => a -> Maybe a
--fb m = Just (m+1)

--fc :: Ord a => [a] -> Int
--fc (x:y:xs) | x > y = 999
--fc _                = 0

--fd :: Eq a => [[a]] -> [a] -> Bool
--fd (x:xs) (y:ys) = x == ys
--fd []     ys     = ys == []

--fe :: Monad -> Monad
--fe x y = do 
--	z <- x
--	return $ z:y

-- b

--ff x y = readFile x >>= \c ->
--		   readFile c >>= \d ->
--		     putStr (c ++ "\n" ++ d)


-- c


-- ##### Question 3 ##### --
--data Bag a = EmptyBag | Node a Int (Bag a) (Bag a) deriving Show

--bag = (Node 9 2 (Node 2 1 EmptyBag EmptyBag) (Node 10 1 EmptyBag EmptyBag))
---- a


---- b

--bcount :: Ord a => a -> Bag a -> Int
--bcount node EmptyBag         = 0
--bcount node (Node n i b1 b2) | n == node = i
--                             | n > node  = (bcount node b1)
--                             | n < node  = (bcount node b2)

---- c
--bagToList :: Bag a -> [a]
--bagToList EmptyBag = []
--bagToList (Node n i b1 b2) = bagToList b1 ++ replicate i n ++ bagToList b2

---- d

--prop_Bag :: Ord a => Bag a -> Bool
--prop_Bag EmptyBag = True
--prop_Bag (Node a c left right) = all (<a) (bagToList left)
--                              && all (>a) (bagToList right)
--                              && all prop_Bag [left,right] && c >= 0


---- e

--bchange :: Ord a => a -> Int -> Bag a -> Maybe (Bag a)
--bchange x n b | newcount >= 0 = Just $ bc b
--              | otherwise     = Nothing
--  				where
--			        newcount = bcount x b + n
--			        bc EmptyBag = Node x n EmptyBag EmptyBag
--			        bc (Node e c left right)
--						| e == x = Node e newcount left right 
--						| x < e = Node e c (bc left) right 
--						| x > e = Node e c left (bc right)

-- ##### Question 4 ##### --
type Pegs     = Int
type NumDiscs = Int
type Disc     = Int
data Hanoi = Hanoi Pegs NumDiscs [[Disc]]
             deriving (Eq,Show)

hanoitest1 :: Hanoi
hanoitest1 = (Hanoi 3 5 [[1,2,3,4,5], [] ,[]])

hanoitest2 :: Hanoi
hanoitest2 = (Hanoi 3 5 [[3,4,5], [] ,[1,2]])


hanoitest3 :: Hanoi
hanoitest3 = (Hanoi 3 5 [[1,3,4], [] ,[1,2]])

hanoitest4 :: Hanoi
hanoitest4 = (Hanoi 3 5 [[3,5,4], [] ,[1,2]])
-- a
prop_wellFormedHanoi :: Hanoi -> Bool
prop_wellFormedHanoi (Hanoi p d ps) = p == length ps && 
									  d == length (concat ps) && 
									  all (\x -> sort x == x) ps && 
									  sort (concat ps) == [1..d]


-- b
emptyHanoi :: Pegs -> NumDiscs -> Hanoi
emptyHanoi p d = (Hanoi p d (replicate p []))

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



filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' func (x:xs) | func x = x : filter' func xs
					| otherwise = filter' func xs


lookup' :: (Eq a) => a -> [(a,b)] -> Maybe b
lookup' val arr = listToMaybe (map snd (filter (\(a,b) -> a == val)arr))

--unzip' :: [(a,b)] -> ([a], [b])
--unzip' (x:xs) = (fst (x) : first xs : [], snd (x) : second xs : [])

--first :: [(a,b)] -> [a]
--first (x:xs) = fst (x) : first ([xs] : [])

--second :: [(a,b)] -> [b]
--second (x:xs) = snd (x) : second ([xs] : [])


--fa :: Ord a => a -> a -> Maybe Bool 
--fa m n = Just (m > n)

--fb :: Num a => a -> a -> (a -> t) -> t
--fb x y z = z y + z x

--fc :: Eq a => [[a]] -> [a] -> Bool
--fc (x:xs) (y:ys) = x == ys
--fc []     ys     = null ys

--fd :: Monad m => m Int -> m [Int]
--fd x = do
--	z <- x
--    return $ replicate z z


--data T a = Leaf | Node a (T a) (T a)
--               deriving Show


--tree = Node 2 tree1 tree2
--	where tree1 = Node 1 Leaf Leaf
--		  tree2 = Node 1 

data T a = Leaf | Node a (T a) (T a)
               deriving Show

tree1 = (Node 2 (Node 1 Leaf Leaf) 
        (Node 1 (Node 1 Leaf Leaf) 
        (Node 0 Leaf Leaf)))               

parents :: Eq a => a -> T a -> [a]
parents n Leaf = []
parents n (Node m t1 t2) | hasCorrectLeaf n t1 || hasCorrectLeaf n t2 = m : parents n t1 ++ parents n t2
                         | otherwise = parents n t1 ++ parents n t2

hasCorrectLeaf :: Eq a => a -> T a -> Bool
hasCorrectLeaf x (Node m _ _) = x == m
hasCorrectLeaf x Leaf         = False











