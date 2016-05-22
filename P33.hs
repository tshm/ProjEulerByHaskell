module Main where
import Data.List

-- |
-- >>> isTrivial (10, 50)
-- True
-- >>> isTrivial (11, 55)
-- True
-- >>> isTrivial (49, 98)
-- False
isTrivial :: (Integer, Integer) -> Bool
isTrivial (x, y) = divisibleByTen || divisibleByEleven where
  divisibleByTen = (x `mod` 10 == 0) && (y `mod` 10 == 0)
  divisibleByEleven = (x `mod` 11 == 0) && (y `mod` 11 == 0)

-- |
-- >>> commonDigit 49 98
-- Just '9'
commonDigit :: Integer -> Integer -> Maybe Char
commonDigit x y
  | elem x0 y' = Just x0
  | elem x1 y' = Just x1
  | otherwise  = Nothing where
    (x0:x1:[]) = show x
    y' = show y

-- |
-- >>> removeCommonDigit (23, 56)
-- (23,56)
-- >>> removeCommonDigit (23, 53)
-- (2,5)
-- >>> removeCommonDigit (23, 22)
-- (3,2)
removeCommonDigit :: (Integer, Integer) -> (Integer, Integer)
removeCommonDigit (x, y)
  | Nothing == commondigit = (x, y)
  | otherwise              = (x', y') where
    commondigit = commonDigit x y
    Just c = commondigit
    (x', y') = (removeChar c x, removeChar c y)
    removeChar c n = (read reducedStr :: Integer) where
      reducedStr' = filter (/= c) (show n)
      reducedStr = if 0 < length reducedStr' then reducedStr' else [c]

-- |
-- >> isSpecialFraction (30, 50)
-- True
-- >> isSpecialFraction (49, 98)
-- False
isSpecialFraction :: (Integer, Integer) -> Bool
isSpecialFraction (x, y) = x /= x' && x * y' == x' * y where
  (x', y') = removeCommonDigit (x, y)

-- |
-- >>> length specialFractions
-- 4
specialFractions :: [(Integer, Integer)]
specialFractions = [(x, y) |
  x <- [10..99], y <- [10..99],
  (not $ isTrivial (x, y)) && (x < y) && isSpecialFraction (x, y)]

-- |
-- >>> mult (4, 8) (3, 1)
-- (3,2)
-- >>> mult (1, 3) (2, 1)
-- (2,3)
mult :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
mult (x, y) (p, q) = (x * p `quot` v, y * q `quot` v) where v = gcd (x*p) (y*q)

main :: IO ()
main = do
  --print $ specialFractions
  --print $ map removeCommonDigit specialFractions
  print $ snd $ foldl1 mult specialFractions
