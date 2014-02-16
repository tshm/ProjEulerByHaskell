module P4 where
import Data.List
-- |
-- >>> largetstPalindrome 2
-- Just 9009
--
-- >>> largetstPalindrome 3
--

largetstPalindrome n =
  let
    max = 10^n - 1
    xs = [ n * m | n <- [1..max], m <- [1..max] ]
  in
    find isPalindrome $ reverse $ sort xs

isPalindrome n = (str == reverse str) where
  str = show n
    

