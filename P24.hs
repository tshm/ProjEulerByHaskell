module Main where
-- A permutation is an ordered arrangement of objects.
-- For example, 3124 is one possible permutation of
-- the digits 1, 2, 3 and 4. If all of the permutations
-- are listed numerically or alphabetically,
-- we call it lexicographic order.
-- The lexicographic permutations of 0, 1 and 2 are:
--
-- 012   021   102   120   201   210
--
-- What is the millionth lexicographic permutation of the
-- digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
--
-- |
-- >>> comb [1,2] [3,4]
-- [[1,3],[1,4],[2,3],[2,4]]
--
-- >>> pair 1 [[3],[4]]
-- [[1,3],[1,4]]
--
-- >>> perm [1,2] 1
-- [[1],[2]]
--
-- >>> perm [1,2] 2
-- [[1,2],[2,1]]
--
-- >>> flatten $ perm [0..2] 3
-- ["012","021","102","120","201","210"]

flatten :: [[Int]] -> [[Char]]
flatten xxs = map concat' xxs where
  concat' xs = foldl (\acc x -> acc ++ (show x)) "" xs

perm :: [Int] -> Int -> [[Int]]
perm _ 0 = [[]]
perm xs n = concatMap ff xs where
  ff :: Int -> [[Int]]
  ff x = pair x $ perm (exclude x xs) (n-1)
  exclude x xs' = (filter (x>) xs') ++ (filter (x<) xs')

-- dump xs = trace (" <"++show (xs)++"> ") xs

pair :: Int -> [[Int]] -> [[Int]]
pair x ys = map (\y -> x : y) ys

comb :: [Int] -> [Int] -> [[Int]]
comb [] _ = []
comb (x:xs) ys = (map (\y -> x : [y]) ys) ++ (comb xs ys)

main :: IO ()
main = print $ (flatten $ perm [0..9] 10) !! 999999

