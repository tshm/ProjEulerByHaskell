import System.Environment (getArgs)
-- import Debug.Trace
-- dbg :: (Show a) => a -> a
-- dbg x = trace (show x) x

-- |
-- >>> maxBats 16 3 2 [6, 10]
-- 0
--
-- >>> maxBats 33 5 0 []
-- 5
--
-- >>> maxBats 22 2 2 [9, 11]
-- 3
--
maxBats :: Int -> Int -> Int -> [Int] -> Int
maxBats l d n xs
  | n == 0     = fill' (l - 6)
  | otherwise  = nleft + sum ns + nright
    where
      gaps = zipWith (-) (tail xs) xs
      fill len = fill' (len - d)
      ns = map fill gaps
      nleft  = fill' (head xs - 6)
      nright = fill' (l - last xs - 6)
      fill' len = if len > 0
                  then len `div` d
                  else 0

-- |
-- >>> bat "22 2 2 9 11"
-- "3"
--
-- >>> bat "33 5 0"
-- "5"
--
-- >>> bat "16 3 2 6 10"
-- "0"
--
-- >>> bat "835 125 1 113"
-- "5"
--
-- >>> bat "47 5 0"
-- "8"
--
bat :: String -> String
bat xs = show $ maxBats l d n xs' where
  (l:d:n:xs') = map (read :: String -> Int) $ words xs

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (putStrLn . bat) $ lines doc

