module Main where
import Data.Array

-- A perfect number is a number for which the sum of its proper divisors is exactly equal to the number. For example, the sum of the proper divisors of 28 would be 1 + 2 + 4 + 7 + 14 = 28, which means that 28 is a perfect number.
-- A number n is called deficient if the sum of its proper divisors is less than n and it is called abundant if this sum exceeds n.
-- As 12 is the smallest abundant number, 1 + 2 + 3 + 4 + 6 = 16, the smallest number that can be written as the sum of two abundant numbers is 24. By mathematical analysis, it can be shown that all integers greater than 28123 can be written as the sum of two abundant numbers. However, this upper limit cannot be reduced any further by analysis even though it is known that the greatest number that cannot be expressed as the sum of two abundant numbers is less than this limit.
-- Find the sum of all the positive integers which cannot be written as the sum of two abundant numbers.
--
-- |
-- >>> sum $ divisors 28
-- 28
--
-- >>> head abundantNums 
-- 12
divisors :: Integer -> [Integer]
divisors n = [k | k<-[1..(n `quot` 2)], n `mod` k == 0]

isAbundant :: Integer -> Bool
isAbundant n = (sum $ divisors n) > n

abundantNums :: [Integer]
abundantNums = filter isAbundant [2..]

notSumOfAbundantNums :: [Integer]
notSumOfAbundantNums = [n | n <- [1..max'-1], False == flags ! n] where
  flags = accumArray (||) False (1,max')
    [(i+j, True) | i <- ab, j <- ab, i+j < max' ]
  ab = takeWhile (< max') abundantNums
  max' = 28123

main :: IO ()
main = print $ sum notSumOfAbundantNums
