import Data.List

-- |
-- >>> mergesort [3,2,1,5,2,4] :: [Int]
-- [1,2,2,3,4,5]
--
mergesort :: (Ord a) => [a] -> [a]
mergesort (x:[]) = [x]
mergesort zs = merge (mergesort l) (mergesort r) where
  (l,r) = splitAt (length zs `div` 2) zs
  merge xs [] = xs
  merge [] ys = ys
  merge xs@(x:xs') ys@(y:ys') = if x < y
                                then x : merge xs' ys
                                else y : merge xs ys'

main :: IO ()
main = print $ mergesort ([3,5,2,1,5] :: [Int])

