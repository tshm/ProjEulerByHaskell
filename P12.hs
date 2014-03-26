module P12 where
import Data.List

-- |
-- >>> [triangleNumber n | n <- [1..10]]
--[1,3,6,10,15,21,28,36,45,55]
--
-- >>> factors $ triangleNumber 1
-- [1]
--
-- >>> factors $ triangleNumber 2
-- [1,3]
--
-- >>> factors $ triangleNumber 3
-- [1,2,3,6]
--
-- >>> factors $ triangleNumber 4
-- [1,2,5,10]
--
-- >>> factors $ triangleNumber 5
-- [1,3,5,15]
--
-- >>> factors $ triangleNumber 6
-- [1,3,7,21]
--
-- >>> factors $ triangleNumber 7
-- [1,2,4,7,14,28]
--
-- >>> find (\x -> 5 < (length $ factors x)) [triangleNumber x| x<- [2..]]
-- Just 28
--
-- >>> find (\x -> 5 < factorNumber x) [triangleNumber x| x<- [2..]]
-- Just 28
--
-- >>> find (\x -> 500 < factorNumber x) [triangleNumber x| x<- [2..]]
--

triangleNumber n = floor $ n * (n+1) / 2

-- slow way...
factors n = [ m | m <- [1..n], n `mod` m == 0 ]

-- faster...
factorNumber n = foldr (*) 1 $ map (1+) $ ranks n where
   ranks m = map length $ group $ primeFactors m

primeFactors :: (Integral a) => a -> [a]
primeFactors n = pfact 2 n where
  pfact p n
    | n < p           = []
    | n `mod` p == 0  = p : (pfact p $ n `quot` p)
    | otherwise       = pfact (p+1) n

