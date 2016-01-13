myName :: IO ()
myName = do
    putStrLn "Lager"

-- 1ai.*****************************************************************************
maxRainfall :: Int -> Float
maxRainfall n = foldr (\x y -> max y $ rainfall x) 0 [1..n]


rainfall :: Int -> Float
rainfall f = rain !! ((f-1) `mod` 22)

rain = [ 3.0, 4.0, 123.0 , 2.0, 3.0, 5.0, 4.0, 56.0, 6.0, 4.0, 43.0, 32.0, 45.0, 65.0, 67.0, 8.0, 9.0, 189.0, 43.0, 45.0, 76.0, 200.0]

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

emptyBag :: Bag a
emptyBag = EmptyBag

-- 3a********************************************
-- När man vill göma implementationen av datatypen ( det gör att man inte exporterar konstruktorn)

-- 3b********************************************

bcount :: Ord a => a -> Bag a -> Int
bcount a EmptyBag         = 0
bcount a (Node x y left right) |  x == a = y
                               |  x > a  = bcount a left
                               |  x < a  = bcount a right

