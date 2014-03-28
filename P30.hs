module Main where
import Data.List
-- |
-- >>> isDigitNPowers 4 1634
-- True
--
-- >>> filter (isDigitNPowers 4) [10..5*9^4]
-- [1634,8208,9474]

isDigitNPowers :: Integer -> Integer -> Bool
isDigitNPowers n x =  x == sum (poweredDigits n x) where
  poweredDigits m y = map (\c -> (parsedigit c)^m) $ show y
  parsedigit c = read [c] :: Integer

findMax :: Integer -> Maybe Integer
findMax n = find (\x -> x `quot` 10^n > 0) [ k * 9^n | k <- [1..] ]

main :: IO ()
main = print $ sum $ filter (isDigitNPowers 5) [10..6*9^(5 :: Integer)]

