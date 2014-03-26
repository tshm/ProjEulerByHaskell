module P26 where
import Data.List

-- |
-- >>> quotient 7
-- [3,2,6,4,5,1]
--
-- >>> map qlen [2..9]
-- [-1,0,-1,-1,0,5,-1,0]
--
-- >>> showMaxIndex 1000
--
-- >> maximumBy (\(i,x) (j,y) -> x `compare` y ) $ zip [1..1000] (map qlen [1..1000])

showMaxIndex n = "index: " ++ show index ++ ", value: " ++ show value where
   (index, value) = maximumBy mapComp zz
   mapComp (i,x) (j,y) = x `compare` y
   zz = zip [1..n] (map qlen [1..n])

qlen n = (length $ quotient n) - 1

quotient n = qq 1 n [] where
  qq r k acc
    | rr == 0     = []
    | elem rr acc = acc
    | otherwise   = qq rr k (acc ++ [rr]) where
      rr = (r * 10) `mod` k

