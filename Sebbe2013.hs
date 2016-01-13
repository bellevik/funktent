myName :: IO ()
myName = do
    putStrLn "Sebbe"

-- ##### Question 1 ##### --
-- a
rainfall :: Int -> Float
rainfall f = fromIntegral (f) * 2.24

rain = [ 3.0, 4.0, 123.0 , 2.0, 3.0, 5.0, 4.0, 56.0, 6.0, 4.0, 43.0, 32.0, 45.0, 65.0, 67.0, 8.0, 9.0, 189.0, 43.0, 45.0, 76.0, 2.0]
--    i
maxRainfall :: Int -> Float
maxRainfall n = foldr (\a b -> max (rain !! a) b) 0 [1..n]

maxRainfall' :: Int -> Float
maxRainfall' n = maxRainfall'' [1..n] 0

maxRainfall'' :: [Int] -> Float -> Float
maxRainfall'' []     i = i
maxRainfall'' (n:ns) i = maxRainfall'' ns (max (rain !! n) i)

-- b


-- ##### Question 2 ##### --
-- a


-- b


-- c


-- ##### Question 3 ##### --
-- a


-- b


-- c


-- d


-- e


-- ##### Question 4 ##### --
-- a


-- b


-- c


-- d