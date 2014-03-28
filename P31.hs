module Main where
import Data.Array
-- |
-- >>> ways 5 1
-- 3
--
-- >>> ways 6 1
-- 4
--
-- >>> ways 5 2
-- 4

coins :: [Int]
coins = [1,2,5,10,20,50,100,200]

-- number of ways to make total value t by using k or less kinds.
ways :: Int -> Int -> Int
ways tot ci = r ! (tot, ci) where
  r = array ((0,0),(tot+1,ci+1)) [((t,k),w t k) | t<-[0..tot], k<-[0..ci]]
  w 0 _ = 1
  w _ 0 = 1
  w t k | t < 0      = 0
        | otherwise  = r ! (t, k-1) + w (t - coins !! k) k

main :: IO ()
main = do
  print $ ways 200 $ (length coins) -1
