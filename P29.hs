module Main where
import Data.List
-- |
-- >>> length $ powerList 5
-- 15

powerList :: Integer -> [Integer]
powerList n = nub [ a^b | a<-[2..n], b<-[2..n] ]

main :: IO ()
main = print $ length $ powerList 100

