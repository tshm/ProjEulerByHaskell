import System.Environment

data Tree a = Empty | Node a (Tree a) (Tree a)
  deriving (Show, Read)

theTree :: Tree Int
theTree =
  Node 30 
    (Node 8
      (Node 3 Empty Empty)
      (Node 20
        (Node 10 Empty Empty)
        (Node 29 Empty Empty)))
    (Node 52 Empty Empty)

-- |
-- >>> pathTo 30 theTree
-- [30]
--
-- >>> pathTo 8 theTree
-- [30,8]
--
pathTo :: Int -> Tree Int -> [Int]
pathTo _ Empty = []
pathTo n (Node k l r)
  | n < k  = k : pathTo n l
  | k < n  = k : pathTo n r
  | otherwise = [k]

-- |
-- >>> commonPath [1,2,3,4] [1,2,5,6]
-- [1,2]
--
commonPath :: [Int] -> [Int] -> [Int]
commonPath _ [] = []
commonPath [] _ = []
commonPath (x:xs) (y:ys)
  | x == y     = x : commonPath xs ys
  | otherwise  = []

main :: IO ()
main = do
  [filename] <- getArgs
  strs <- lines `fmap` readFile filename
  mapM_ (print . lca . parse) strs where
    lca (x,y) = last $ commonPath xs ys where
      xs = pathTo x theTree
      ys = pathTo y theTree
    parse str = (x, y) where
      (x:y:[]) = map (\s -> read s :: Int) $ words str

