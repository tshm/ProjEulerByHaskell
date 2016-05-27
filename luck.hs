import System.Environment (getArgs)
--import Data.Function (fix)

-- | 
-- >>> m 2
-- 10
--
-- >>> m 4
-- 670
--
-- >>> m 10
-- 432457640
--
m :: Int -> Integer
m n = s where
  n' = n `div` 2
  s = sum [ l' n' i ^ (2::Integer) | i <- [0..9*n'] ]


-- l :: Int -> Int -> Integer
-- l = fix f

-- | open recursion pattern
f :: (Int -> Int -> Integer) -> Int -> Int -> Integer
f _ _ 0 = 1
f _ n 1 = toInteger n
f _ 1 k = if 0 <= k && k <= 9 then 1 else 0
f mf n k = sum [mf (n-1) j | j <- [0 `max` (k-9)..k]]

llist :: [[Integer]]
llist = map (\n -> map (f l' n) [0..]) [0..] 

l' :: Int -> Int -> Integer
l' n k = llist !! n !! k

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ proc $ lines doc where
    proc = print . m . (read :: String -> Int)

