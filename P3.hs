module P3 where
-- |
-- >>> primeFactors $ 2 * 2 * 3 * 3 * 13195
-- [2,3,5,7,13,29]
--
-- >>> primeFactors 13195
-- [5,7,13,29]
--
-- >>> last $ primeFactors 600851475143 
-- 6857

primeFactors :: (Integral a) => a -> [a]
primeFactors n = fact' 2 n  where
  fact' k m
    | k > m          = []
    | m `mod` k == 0  = k : fact' (k+1) (quot' m k)
    | otherwise       = fact' (k+1) m

-- quot the same factor until it is no more possible.
-- This will ensure the 'prime'ness
quot' x y
  | (quot x y) `mod` y == 0  = quot' (quot x y) y
  | otherwise                = quot x y

