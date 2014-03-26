module P29 where
import Data.List
-- |
-- >>> length $ powerList 5
-- 15
--
-- >>> length $ powerList 100
-- 

powerList n = nub [ a^b | a<-[2..n], b<-[2..n] ]

