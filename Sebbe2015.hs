import Data.List
import Data.Maybe

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


-- v


-- ##### Question 2 ##### --


-- ##### Question 3 ##### --
-- i


-- ii


-- iii


-- ##### Question 4 ##### --
-- i


-- ii


-- iii


-- iv

