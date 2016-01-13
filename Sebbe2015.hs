import Data.List
import Data.Maybe
import Debug.Trace

myName :: IO ()
myName = do
    putStrLn "Sebbe"

-- ##### Question 1 ##### --
-- i
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ []     = []
filter' f (x:xs) | f x       = x : filter' f xs
                 | otherwise = filter' f xs

-- ii


-- iii

test :: [(Int,Int)]
test = [(1,2),(3,4),(5,6),(7,8),(3,4)]

lookup' :: (Eq a) => a -> [(a,b)] -> Maybe b
lookup' i is = listToMaybe $ map snd $ filter (\(a,b) -> a == i) is

-- iv
sequence_' :: Monad m => [m a] -> m ()
sequence_' [] = return ()
sequence_' (i:is) = i >> sequence_' is 

-- v
unzip' :: [(a,b)] -> ([a],[b])
unzip' []          = ([],[])
unzip' ((x,y):xys) = let (xs, ys) = unzip' xys in (x:xs, y:ys)

-- Testa att skicka med fst, snd
unzip'' :: [(a,b)] -> ([a],[b])
unzip'' xys = (unzip2 xys, unzip3 xys)
    where
        unzip2 []        = []
        unzip2 ((x,y):xys) = x : (unzip2 xys)
        unzip3 []        = []
        unzip3 ((x,y):xys) = y : (unzip3 xys)

-- ##### Question 2 ##### --

{-fa :: Ord a => a -> a -> Maybe Bool
fa m n = Just (m > n)

fb :: Num b => a -> a -> (a -> b) -> b
fb x y z = z y + z x

fc :: Eq a => [[a]] -> [a] -> Bool
fc (x:xs) (y:ys) = x == ys
fc []     ys     = null ys

fd :: Monad m => m Int -> m [Int]
fd x = do
z <- x
    return $ replicate z z -}

-- ##### Question 3 ##### --
{-permu :: [a] -> [[a]]
permu []     = [[]]
permu (x:xs) = concatMap (insertAll x) $ permu xs

-- i
insertAll :: a -> [a] -> [[a]]
insertAll x []     = [[x]]
insertAll x (y:ys) = (x:y:yx) : map (y:) (insertAll x ys) -}

-- ii


-- iii


-- ##### Question 4 ##### --
data T a = Leaf | Node a (T a) (T a)
    deriving Show

-- i
tree1 = (Node 2 (Node 1 Leaf Leaf) 
        (Node 1 (Node 1 Leaf Leaf) 
        (Node 0 Leaf Leaf)))

-- ii
parents :: Eq a => a -> T a -> [a]
parents _ Leaf               = []
parents x (Node n b1 b2)     | isChild b1 x || isChild b2 x = (parents x b1) ++ [n] ++ (parents x b2)
                             | otherwise = (parents x b1) ++ (parents x b2)
    where
        isChild Leaf _           = False
        isChild (Node n1 _ _) x2 = n1 == x2

-- iii


-- iv

