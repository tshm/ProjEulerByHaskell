module P2 where
-- |
-- >>> take 10 $ fibSeq
--[1,2,3,5,8,13,21,34,55,89]
--
-- >>> :{
--   let
--     max = 4 * 10^6
--     iseven n = n `mod` 2 == 0
--   in
--     sum $ takeWhile (< max) [ n | n <- fibSeq, iseven n]
-- :}
-- 

fibSeq = tail fibSeq0  -- make it 'starting with 1...'
  where
    -- normal fibbonaci seq definition
    fibSeq0 = [ fib n | n <- [0..] ]
    fib 0 = 1
    fib 1 = 1
    fib n = (fibSeq0 !! (n-2)) + (fibSeq0 !! (n-1))

