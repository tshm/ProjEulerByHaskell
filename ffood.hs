import Data.Array

-- |
-- >>> buyable 3
-- False
--
-- >>> buyable 15
-- True
--
buyable n = r ! n where
  r = listArray (0, n) (True : map buyable' [1..n])
  buyable' i = check 6 i || check 9 i || check 20 i
  check m i = i >= m && r!(i-m)

main :: IO ()
main = print $ buyable 3

