module P6 where
-- |
-- >>> diffSumSqAndSqSum 10
-- 2640
--
-- >>> diffSumSqAndSqSum 100
--

-- realizing ...
-- (x + y)^2 - x^2 - y^2 = xy + yx
diffSumSqAndSqSum n =
  sum [ x * y | x <- [1..n], y <- [1..n], x /= y ]

