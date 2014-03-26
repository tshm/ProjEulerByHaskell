module P28 where
-- |
-- >>> xsum 5
-- 101
--
-- >>> xsum 3
-- 25
--
-- >>> xsum 1001
--


xsum 1 = 1
xsum n = xsum (n-2) + 4 * n^2 - 6 * (n-1)
