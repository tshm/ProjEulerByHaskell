module Main where
import Data.List
-- |
-- >>> quotient 7
-- [3,2,6,4,5,1]
--
-- >>> map qlen [2..9]
-- [-1,0,-1,-1,0,5,-1,0]

showMaxIndex :: Integer -> String
showMaxIndex n = "index: " ++ show index ++ ", value: " ++ show value where
   (index, value) = maximumBy mapComp zz
   mapComp (_,x) (_,y) = x `compare` y
   zz = zip [1..n] (map qlen [1..n])

qlen :: Integer -> Int
qlen n = (length $ quotient n) - 1

quotient :: Integer -> [Integer]
quotient n = qq 1 n [] where
  qq r k acc
    | rr == 0     = []
    | elem rr acc = acc
    | otherwise   = qq rr k (acc ++ [rr]) where
      rr = (r * 10) `mod` k

main :: IO ()
main = print $ showMaxIndex 1000

