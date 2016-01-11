sumsq :: Integer -> Integer
sumsq counter = sumsq' counter 0

sumsq' :: Integer -> Integer -> Integer
sumsq' counter sums | counter == 0 = sums
sumsq' counter sums | sumsq' (counter - 1) (sums + counter * counter)
