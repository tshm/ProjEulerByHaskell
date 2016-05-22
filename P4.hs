module Main where
import Data.List
-- |
-- >>> largetstPalindrome 2
-- Just 9009

largetstPalindrome :: Integer -> Maybe Integer
largetstPalindrome n =
  let
    max' = 10^n - 1
    xs = [ k * m | k <- [1..max'], m <- [1..max'] ]
  in
    find isPalindrome $ reverse $ sort xs

isPalindrome :: Integer -> Bool
isPalindrome n = (str == reverse str) where
  str = show n
    
main :: IO ()
main = print $ largetstPalindrome 3

