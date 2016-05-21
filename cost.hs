import Data.List

-- | 
-- >>> parse "1\n2\n3\n1 2 3"
-- [(2,[1,2,3])]
--
-- >>> parse "2\n3\n3\n1 2 3\n5\n4\n1 2 3 4"
-- [(3,[1,2,3]),(5,[1,2,3,4])]
--
parse :: String -> [(Int, [Int])]
parse doc = parse' $ tail $ lines doc where
  parse' [] = []
  parse' (c:_:arr:ls') = (toInt c, map toInt $ words arr):(parse' ls')
  parse' _ = error "malformatted input"

toInt :: String -> Int
toInt = read

-- |
-- >>> comb 2 [1,2,3]
-- [[1,2],[1,3],[2,3]]
--
comb :: Int -> [a] -> [[a]]
comb 0 _ = [[]]
comb n xs = [ xs !! i : x
            | i <- [0..(length xs)-1], x <- comb (n-1) (drop (i+1) xs) ]

-- |
-- >>> solve (3,[1,2,3])
-- (1,2)
--
-- >>> solve (100,[55,5,45])
-- (1,3)
--
solve :: (Int, [Int]) -> (Int, Int)
solve (c,items) = head ans where
  combs = comb 2 $ zip [1..] items
  ans = [(i, j) | ((i,x):(j,y):[]) <- combs, x + y == c ]

main :: IO ()
main = print ""

