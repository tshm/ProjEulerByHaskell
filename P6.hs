module Main where
-- |
-- >>> diffSumSqAndSqSum 10
-- 2640

-- realizing ...
-- (x + y)^2 - x^2 - y^2 = xy + yx
diffSumSqAndSqSum :: Integer -> Integer
diffSumSqAndSqSum n =
  sum [ x * y | x <- [1..n], y <- [1..n], x /= y ]

main :: IO ()
main = print $ diffSumSqAndSqSum 100
