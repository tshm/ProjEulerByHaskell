module Main where
-- |
-- >>> d 220
-- 284
--
-- >>> d 284
-- 220
--
-- >>> isAmicalNumber 284
-- True

divisors :: Int -> [Int]
divisors 1 = [1]
divisors n = [k | k <- [1..n `quot` 2], n `mod` k == 0]

d :: Int -> Int
d n = foldr1 (+) $ divisors n

isAmicalNumber :: Int -> Bool
isAmicalNumber n =   n /= m && n == d m where
   m = d n

main :: IO ()
main = print $ sum $ filter isAmicalNumber [2..10000]

