import System.Environment (getArgs)
import Data.Bits (xor,shiftR)

-- | 
-- >>> graydec "1111"
-- 10
--
graydec :: String -> Int
graydec str = n' where
  n = foldl (\s c -> s * 2 + conv c) 0 str
  conv c = if c=='1' then 1 else 0
  n' = foldl (\s i -> s `xor` (s `shiftR` i)) n [8,4,2,1]

-- |
-- >>> xlate "1111 | 1111"
-- "10 | 10"
--
xlate :: String -> String
xlate line = unwords $ map conv $ words line where
  conv "|" = "|"
  conv s = show $ graydec s

-- | entry point
main :: IO ()
main = do
  [filename] <- getArgs
  doc <- readFile filename
  mapM_ proc $ lines doc where
    proc = putStrLn . xlate

