import System.Environment (getArgs)

-- | 
-- >>> m 12
-- 670
--
m :: Int -> Integer
m n = s where
  n' = n `div` 2
  s = sum [ l n' i ^ 2 | i <- [0..9*n'] ]

-- ll :: Int -> Int -> Integer
-- ll _ 0 = 1
-- ll n 1 = toInteger n
-- ll 1 k = if 0 <= k && k <= 9 then 1 else 0
-- ll n k = sum [ll (n-1) j | j <- [0 `max` (k-9)..k]]

l :: Int -> Int -> Integer
l p q = map (\n -> map (l' n) [0..]) [0..] !! p !! q
  where
    l' _ 0 = 1
    l' n 1 = toInteger n
    l' 1 k = if 0 <= k && k <= 9 then 1 else 0
    l' n k = sum [l (n-1) j | j <- [0 `max` (k-9)..k]]

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ proc $ lines doc where
    proc = print . m . (read :: String -> Int)

