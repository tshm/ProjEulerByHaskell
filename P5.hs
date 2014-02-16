module P5 where
-- |
-- >>> lcm' 10
-- 2520
--
-- >>> lcm' 20
--

lcm' 2 = 2
lcm' n = lcm n (lcm' (n-1))
    

