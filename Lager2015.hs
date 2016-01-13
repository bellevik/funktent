import Data.List

myName :: IO ()
myName = do
    putStrLn "Lager"

-- 1 *******************************************************************************
-- 1i *******************************************
filter' :: Ord t => (t -> Bool) -> [t] -> [t]
filter' f xs = helpFilter f xs []
   where helpFilter f' [] new = new
         helpFilter f' (y:ys) new | f' y      = helpFilter f' ys $ insert y new -- (new ++ [y])
                                  | otherwise = helpFilter f' ys new

-- 1i *******************************************