module Main where
import Data.List (find)
-- |
-- >>> fact 9 == 9 * 8 * 7 * 6 * 5 * 4 * 3 * 2
-- True
fact :: Int -> Int
fact n = facts !! n

facts :: [Int]
facts = map fact' [0..9] where
  fact' 0 = 1
  fact' n = foldr1 (*) [1..n] 

-- |
-- >>> maxdigit
-- Just 8
maxdigit :: Maybe Int
maxdigit = find dig [1..] where
  dig n = n > (length . show $ n * fact 9)

-- |
-- >>> isCurious 145
-- True
isCurious :: Int -> Bool
isCurious n = n == factSum where
  factSum = sum $ map fact digits
  digits = map (\c -> read [c] :: Int) $ show n

curiousNumbers :: [Int]
curiousNumbers = [n | n <- [3..7*(fact 9)], isCurious n]

main :: IO ()
main = do
  print $ curiousNumbers
  print $ sum curiousNumbers
