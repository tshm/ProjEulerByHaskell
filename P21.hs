module P21 where
import Data.List

-- |
-- >>> d 220
-- 284
--
-- >>> d 284
-- 220
--
-- >>> isAmicalNumber 284
-- True
--
-- >>> sum $ filter isAmicalNumber [2..10000]

divisors 1 = [1]
divisors n = [k | k <- [1..n-1], n `mod` k == 0]

d n = foldr1 (+) $ divisors n

isAmicalNumber n =   n /= m && n == d m where
   m = d n


