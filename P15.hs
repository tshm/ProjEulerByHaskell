module Main where
import Data.Array

-- possible route from top-left to bottom-right in 20x20 grid.
-- Where one can move either down or right.
--
-- |
-- >>> dpaths 2
-- 6
--
-- >>> mpaths 2
-- 6
--
-- >>> paths 2
-- 6
--
-- >>> paths 20 == mpaths2 20
-- True

-- generic & DP way
paths :: Integer -> Integer
paths n = r ! (n, n) where
  r = array ((0,0),(n,n)) [((x,y), paths' x y) | x<-[0..n], y<-[0..n]] 
  paths' 0 0 = 1
  paths' 0 j = r ! (0, j-1)
  paths' i 0 = r ! (i-1, 0)
  paths' i j = (r ! (i,j-1)) + (r ! (i-1, j))

-- mathematical way
-- recursive choose
mpaths :: Integer -> Integer
mpaths n = choose (2*n) n where
  choose _ 0 = 1
  choose m k
    | m == k    = 1
    | otherwise = choose (m-1) k + choose (m-1) (k-1)

-- factorial choose
mpaths2 :: Integer -> Integer
mpaths2 n = (fact (2*n) n) `quot` (fact n 1) where
  fact 1 1 = 1
  fact i j = foldr1 (*) [j+1..i]

-- straight forward but dumb way
dpaths :: Integer -> Integer
dpaths n = dpaths' n n where
  dpaths' 0 0 = 1
  dpaths' 0 j = dpaths' 0 (j-1)
  dpaths' i 0 = dpaths' (i-1) 0
  dpaths' i j = dpaths' i (j-1) + dpaths' (i-1) j

main :: IO ()
main = print $ paths 20

