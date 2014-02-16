module P1 where
-- |
-- >>> mult [3,5] 10
-- [3,5,6,9]
--
-- >>> sum $ mult [3,5] 10
-- 23
--
-- >>> sum $ mult [3,5] 1000
-- "?"

mult xs n = [y | y <- [1..n-1], any (\x -> y `mod` x == 0) xs ]

