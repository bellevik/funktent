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

