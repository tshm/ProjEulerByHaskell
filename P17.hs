module P17 where
--import Debug.Trace

-- If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
-- If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?
-- NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.
--
-- |
--
-- >>> say 1
-- "one"
--
-- >>> say 16
-- "sixteen"
--
-- >>> say 47
-- "forty-seven"
--
-- >>> say 939
-- "nine hundred and thirty-nine"
--
-- >>> say 342
-- "three hundred and forty-two"
--
-- >>> cnt 115
-- 20
-- 
-- >>> sum [cnt n | n <- [1..1000]]

say n
  | n < 10    = base !! n
  | n < 16    = teen !! (n - 10)
  | n == 18   = "eighteen"
  | n < 20    = (say (n-10)) ++ "teen"
  | n < 100   = pref !! (t - 2) ++ "ty" ++ (if o == 0 then "" else "-" ++ say o)
  | n == 1000 = "one thousand"
  | otherwise = say h ++ " hundred" ++ (if tt == 0 then "" else " and " ++ say tt)
  where
    base = ["", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
    teen = ["ten", "eleven", "twelve", "thirteen", "fourteen", "fifteen"]
    pref = ["twen", "thir", "for", "fif", "six", "seven", "eigh", "nine"]
    h    = n `quot` 100
    tt   = n `mod` 100
    t    = (n - 100 * h) `quot` 10
    o    = n `mod` 10

cnt n = sum $ map (\x -> if elem x ['a'..'z'] then 1 else 0)  $say n
