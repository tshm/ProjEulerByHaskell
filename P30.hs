module P30 where
import Data.List
-- |
-- >>> isDigitNPowers 4 1634
-- True
--
-- >>> filter (isDigitNPowers 4) [10..5*9^4]
-- [1634,8208,9474]
--
-- >>> sum $ filter (isDigitNPowers 5) [10..6*9^5]
-- 

isDigitNPowers n x =  x == sum (poweredDigits n x) where
  poweredDigits n x = map (\c -> (parsedigit c)^n) $ show x
  parsedigit c = read [c] :: Integer

findMax n = find (\x -> x `quot` 10^n > 0) [ k * 9^n | k <- [1..] ]

