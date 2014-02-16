module P4 where
-- |
-- >>> diffSumSqAndSqSum 10
-- 2640
--
-- >>> diffSumSqAndSqSum 100
--

diffSumSqAndSqSum n = sqSum - sumSq where
  sqSum = (sum [ x | x <- [1..n] ]) ^ 2
  sumSq = sum [ x^2 | x <- [1..n] ]

