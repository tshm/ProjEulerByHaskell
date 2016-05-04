import System.Environment (getArgs)

-- | rept
-- >>> rept "abcabcabc"
-- 3
--
-- >>> rept "bcbcbcbcbcbcbcbcbcbcbcbcbcbc"
-- 2
--
-- >>> rept "dddddddddddddddddddd"
-- 1
--
-- >>> rept "adcdefg"
-- 7
--
rept :: String -> Int
rept str = rept' 1 str where
  nmax = length str
  rept' n xs 
    | n == nmax  = nmax
    | otherwise  = if reptp then n else rept' (n+1) xs where
      reptp = and $ zipWith (==) (drop n xs) xs

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ (print . rept) $ lines doc

