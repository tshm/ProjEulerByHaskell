module Main where
-- |
-- >>> xsum 5
-- 101
--
-- >>> xsum 3
-- 25

xsum :: Integer -> Integer
xsum 1 = 1
xsum n = xsum (n-2) + 4 * n*n - 6 * (n-1)

main :: IO ()
main = print $ xsum 1001

