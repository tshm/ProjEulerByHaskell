module P19 where

--     1 Jan 1900 was a Monday.
--     Thirty days has September,
--     April, June and November.
--     All the rest have thirty-one,
--     Saving February alone,
--     Which has twenty-eight, rain or shine.
--     And on leap years, twenty-nine.
--     A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
--
-- How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?

daysInMonthOfYear n = [days m | m <- [1..12]] where
  days m
    | elem m [4,6,9,11]  = 30
    | m == 2             = daysInFeb n
    | otherwise          = 31
  daysInFeb n
    | (n `mod` 4 == 0 && n `mod` 100 /= 0) || n `mod` 400 == 0 = 29
    | otherwise  = 28

sunds = length $ filter (\n -> n `mod` 7 == 0) elap where
  days = concatMap daysInMonthOfYear [1901..2000]
  elap = foldr (\n acc -> acc ++ [last acc + n]) [1] $ days

main = do
  print $ sunds

